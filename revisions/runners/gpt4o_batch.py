"""GPT-4o strain annotation via the OpenAI Batch API.

Matches Rogic et al.'s `inst/gpt.py` setup exactly:
  - model:        gpt-4o-2024-11-20
  - temperature:  0
  - top_p:        1
  - seed:         1
  - max_tokens:   1024
  - structured output via `response_format = {type:"json_schema",
                   strict:true, name:"mouse_strain"}`  (NOT tool-use)
  - submission via Batch API (50 % discount, large queued TPM)

Usage:
    revisions/.venv/bin/python revisions/gpt4o_batch.py submit \\
        --sample revisions/data/sample500.tsv \\
        --prompt revisions/strain_prompt_specificity.txt \\
        --suffix specprompt
    revisions/.venv/bin/python revisions/gpt4o_batch.py poll --suffix specprompt
    revisions/.venv/bin/python revisions/gpt4o_batch.py finalize --suffix specprompt

`submit` builds all request bodies, packs them into chunks under the
tier-2 enqueued-token cap (default 1,000,000; OpenAI's is 1,350,000 for
gpt-4o so we leave headroom), uploads each chunk, and creates one
batch per chunk. If OpenAI rejects with the enqueue-limit error the
submitter sleeps and retries until capacity returns. `poll` reports
status across all chunks. `finalize` collects per-chunk output, writes
per-GSE JSON, and aggregates summary.tsv identical in shape to the
existing Claude / open-weights summary files.

Auth resolves OPENAI_API_KEY from env, falling back to macOS Keychain
service `OPENAI_API_KEY`. Honours OPENAI_KEYCHAIN_ENTRY override."""
import argparse
import json
import os
import subprocess
import sys
import time
from pathlib import Path

from openai import OpenAI

from geo_fetch import build_input
from strain_annotate import STRAIN_LIST_PATH

MODEL = "gpt-4o-2024-11-20"
RESULTS_ROOT = "revisions/data/results"

# Rogic's strain_output schema, transliterated from R/output_schemas.R.
STRAIN_RESPONSE_FORMAT = {
    "type": "json_schema",
    "json_schema": {
        "name": "mouse_strain",
        "description": "Upon determining mouse strain used in the experiment, present "
                       "your findings with this structure. Use an empty list if you are "
                       "unable to find any strains, include all possible matches",
        "strict": True,
        "schema": {
            "type": "object",
            "additionalProperties": False,
            "required": ["strains"],
            "properties": {
                "strains": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "additionalProperties": False,
                        "required": ["value", "URI", "quote"],
                        "properties": {
                            "value": {
                                "type": "string",
                                "description": "Name of the mouse strain, must match "
                                               "value in list of strains provided",
                            },
                            "URI": {
                                "type": "string",
                                "description": "URI of the mouse strain, must match "
                                               "the URI in the list of strains provided",
                            },
                            "quote": {
                                "type": "array",
                                "description": "Verbatim quotes from the source to "
                                               "justify your decision",
                                "items": {
                                    "type": "string",
                                    "description": "A quote taken from your input",
                                },
                            },
                        },
                    },
                }
            },
        },
    },
}


def _resolve_key() -> str:
    if os.environ.get("OPENAI_API_KEY"):
        return os.environ["OPENAI_API_KEY"]
    for entry in [os.environ.get("OPENAI_KEYCHAIN_ENTRY"), "OPENAI_API_KEY", "openai", "OpenAI"]:
        if not entry:
            continue
        try:
            return subprocess.check_output(
                ["security", "find-generic-password", "-s", entry, "-w"],
                text=True, stderr=subprocess.DEVNULL,
            ).strip()
        except subprocess.CalledProcessError:
            continue
    raise SystemExit("ERROR: no OPENAI_API_KEY in env or Keychain")


def _client() -> OpenAI:
    return OpenAI(api_key=_resolve_key())


def build_system_prompt(prompt_path: str) -> str:
    with open(prompt_path) as f:
        prompt = f.read().rstrip()
    with open(STRAIN_LIST_PATH) as f:
        strain_list = json.load(f)
    return prompt + "\n" + json.dumps(strain_list)


def load_sample(path: str) -> list[dict]:
    import csv
    with open(path) as f:
        return list(csv.DictReader(f, delimiter="\t"))


def _request_body(sys_prompt: str, user_input: dict) -> dict:
    """Body matching Rogic's `inst/gpt.py::ask_gpt` exactly."""
    return {
        "model": MODEL,
        "messages": [
            {"role": "system", "content": sys_prompt},
            {"role": "user", "content": json.dumps(user_input)},
        ],
        "response_format": STRAIN_RESPONSE_FORMAT,
        "max_tokens": 1024,
        "seed": 1,
        "temperature": 0,
        "top_p": 1,
    }


def _meta_path(suffix: str) -> Path:
    tag = f"{MODEL}_{suffix}" if suffix else MODEL
    Path(f"{RESULTS_ROOT}/{tag}").mkdir(parents=True, exist_ok=True)
    return Path(f"{RESULTS_ROOT}/{tag}/_batch_meta.json")


def _estimate_tokens(body: dict) -> int:
    """Cheap token estimate: chars / 4. Used only for batch chunk sizing,
    so the bound only needs to be a moderate over-estimate."""
    return len(json.dumps(body)) // 4


def _pack_chunks(lines: list[dict], cap_tokens: int) -> list[list[dict]]:
    """Greedy bin-packing: split `lines` into chunks whose summed token
    estimate is <= cap_tokens. Each line stays intact (one per request)."""
    chunks: list[list[dict]] = []
    cur, cur_tokens = [], 0
    for line in lines:
        t = _estimate_tokens(line["body"])
        if cur and cur_tokens + t > cap_tokens:
            chunks.append(cur)
            cur, cur_tokens = [], 0
        cur.append(line)
        cur_tokens += t
    if cur:
        chunks.append(cur)
    return chunks


def cmd_submit(args):
    rows = load_sample(args.sample)
    if args.limit:
        rows = rows[: args.limit]
    sys_prompt = build_system_prompt(args.prompt)
    tag = f"{MODEL}_{args.suffix}" if args.suffix else MODEL
    results_dir = Path(f"{RESULTS_ROOT}/{tag}")
    results_dir.mkdir(parents=True, exist_ok=True)

    # Build all requests, then pack into chunks under the tier-2 enqueue cap.
    lines: list[dict] = []
    print(f"building {len(rows)} request bodies (fetching GEO inputs)…", file=sys.stderr)
    skipped_no_paper = 0
    for r in rows:
        gse = r["shortName"]
        user_input = build_input(gse)
        if args.strip_papers:
            # R1.8 paper-vs-no-paper A/B: drop the `papers` key so the model
            # sees only GEO metadata. Skip GSEs that never had a paper — they
            # contribute nothing to the A/B.
            if "papers" not in user_input:
                skipped_no_paper += 1
                continue
            user_input = {k: v for k, v in user_input.items() if k != "papers"}
        lines.append({
            "custom_id": gse,
            "method": "POST",
            "url": "/v1/chat/completions",
            "body": _request_body(sys_prompt, user_input),
        })
    if args.strip_papers and skipped_no_paper:
        print(f"  skipped {skipped_no_paper} GSEs with no paper (nothing to strip)", file=sys.stderr)

    chunks = _pack_chunks(lines, cap_tokens=args.chunk_token_cap)
    print(f"packed into {len(chunks)} chunks under {args.chunk_token_cap:,} tokens each",
          file=sys.stderr)

    client = _client()
    chunk_meta = []
    submitted = 0
    for i, chunk in enumerate(chunks):
        jsonl_path = results_dir / f"_input_chunk{i:02d}.jsonl"
        with open(jsonl_path, "w") as f:
            for line in chunk:
                f.write(json.dumps(line) + "\n")
        est = sum(_estimate_tokens(l["body"]) for l in chunk)
        # Submit with auto-retry against the enqueue-limit error: if OpenAI
        # rejects because other chunks are still draining, sleep and re-poll
        # until enqueue capacity returns.
        while True:
            try:
                up = client.files.create(file=open(jsonl_path, "rb"), purpose="batch")
                batch = client.batches.create(
                    input_file_id=up.id,
                    endpoint="/v1/chat/completions",
                    completion_window="24h",
                    metadata={"suffix": args.suffix, "chunk": str(i)},
                )
                # Some accounts mark a batch "failed" immediately for enqueue-cap
                # violations rather than rejecting the create call; poll once.
                b2 = client.batches.retrieve(batch.id)
                if b2.status == "failed":
                    err_msg = ""
                    if b2.errors and getattr(b2.errors, "data", None):
                        err_msg = " ".join(getattr(d, "message", "") or "" for d in b2.errors.data)
                    if "Enqueued token limit" in err_msg or "enqueued tokens" in err_msg:
                        wait = 60
                        print(f"chunk {i}: enqueue cap hit ({err_msg[:120]}…); sleeping {wait}s",
                              file=sys.stderr)
                        time.sleep(wait)
                        continue
                    raise RuntimeError(f"chunk {i} failed immediately: {err_msg}")
                chunk_meta.append({
                    "chunk": i,
                    "batch_id": batch.id,
                    "input_file_id": up.id,
                    "jsonl_path": str(jsonl_path),
                    "n_requests": len(chunk),
                    "est_tokens": est,
                    "status": b2.status,
                })
                submitted += len(chunk)
                print(f"chunk {i:02d}: batch_id={batch.id} n={len(chunk)} est_tokens={est:,} status={b2.status}",
                      file=sys.stderr)
                break
            except Exception as e:
                msg = str(e)
                if "Enqueued token limit" in msg or "enqueued tokens" in msg:
                    print(f"chunk {i}: enqueue cap hit at create; sleeping 60s", file=sys.stderr)
                    time.sleep(60)
                    continue
                raise

    meta = {
        "tag": tag,
        "sample": args.sample,
        "prompt": args.prompt,
        "suffix": args.suffix,
        "n_requests": len(lines),
        "limit_was": args.limit,
        "chunk_token_cap": args.chunk_token_cap,
        "chunks": chunk_meta,
        "submitted_at": time.time(),
    }
    with open(_meta_path(args.suffix), "w") as f:
        json.dump(meta, f, indent=2)
    print(json.dumps({k: v for k, v in meta.items() if k != "chunks"} | {"n_chunks": len(chunks)}, indent=2))


def _load_meta(suffix: str) -> dict:
    return json.load(open(_meta_path(suffix)))


def _save_meta(suffix: str, meta: dict) -> None:
    with open(_meta_path(suffix), "w") as f:
        json.dump(meta, f, indent=2)


def _is_enqueue_cap_error(batch) -> bool:
    if not batch.errors or not getattr(batch.errors, "data", None):
        return False
    for d in batch.errors.data:
        msg = (getattr(d, "message", "") or "")
        if "Enqueued token limit" in msg or "enqueued tokens" in msg:
            return True
    return False


def _resubmit_chunk(client, jsonl_path: Path, suffix: str, chunk_idx: int,
                   max_wait_iters: int = 60, wait_seconds: int = 30) -> dict:
    """Submit a chunk JSONL with proper validating-state polling. If the
    batch transitions to `failed` with an enqueue-cap error, sleep and
    retry the create. Returns the eventual {batch_id, status} or raises."""
    for attempt in range(max_wait_iters):
        up = client.files.create(file=open(jsonl_path, "rb"), purpose="batch")
        batch = client.batches.create(
            input_file_id=up.id,
            endpoint="/v1/chat/completions",
            completion_window="24h",
            metadata={"suffix": suffix, "chunk": str(chunk_idx), "attempt": str(attempt)},
        )
        # Poll until status leaves "validating".
        for _ in range(30):
            b = client.batches.retrieve(batch.id)
            if b.status != "validating":
                break
            time.sleep(2)
        if b.status == "failed" and _is_enqueue_cap_error(b):
            print(f"chunk {chunk_idx} attempt {attempt}: enqueue cap; sleeping {wait_seconds}s",
                  file=sys.stderr)
            time.sleep(wait_seconds)
            continue
        return {"batch_id": batch.id, "input_file_id": up.id, "status": b.status}
    raise RuntimeError(f"chunk {chunk_idx} could not be submitted after {max_wait_iters} attempts")


def cmd_recover(args):
    """Re-submit any chunks whose batches landed in `failed` with an
    enqueue-cap error. Updates meta in place."""
    client = _client()
    meta = _load_meta(args.suffix)
    changed = False
    for c in meta["chunks"]:
        b = client.batches.retrieve(c["batch_id"])
        c["status"] = b.status
        if b.status == "failed" and _is_enqueue_cap_error(b):
            print(f"chunk {c['chunk']} ({c['batch_id']}): failed → resubmitting", file=sys.stderr)
            new = _resubmit_chunk(client, Path(c["jsonl_path"]), args.suffix, c["chunk"])
            c["batch_id"] = new["batch_id"]
            c["input_file_id"] = new["input_file_id"]
            c["status"] = new["status"]
            changed = True
        elif b.status == "failed":
            print(f"chunk {c['chunk']} ({c['batch_id']}): failed for non-cap reason — manual look", file=sys.stderr)
    if changed:
        _save_meta(args.suffix, meta)
    # Report final state
    statuses = [c["status"] for c in meta["chunks"]]
    print(json.dumps({"suffix": args.suffix, "chunk_statuses": statuses}, indent=2))


def cmd_poll(args):
    client = _client()
    meta = _load_meta(args.suffix)
    tag = meta["tag"]
    summary = []
    all_done = True
    n_ok = n_fail = n_inflight = 0
    for c in meta["chunks"]:
        b = client.batches.retrieve(c["batch_id"])
        rc = b.request_counts.model_dump() if b.request_counts else {}
        summary.append({
            "chunk": c["chunk"], "batch_id": b.id, "status": b.status,
            "n_requests": c["n_requests"], "request_counts": rc,
        })
        if b.status == "completed":
            n_ok += c["n_requests"]
        elif b.status in ("failed", "cancelled", "expired"):
            n_fail += c["n_requests"]
        else:
            n_inflight += c["n_requests"]
            all_done = False
    print(json.dumps({
        "tag": tag, "n_chunks": len(meta["chunks"]),
        "all_done": all_done, "n_ok": n_ok, "n_inflight": n_inflight, "n_fail": n_fail,
        "chunks": summary,
    }, indent=2))


def cmd_finalize(args):
    client = _client()
    meta = _load_meta(args.suffix)
    tag = meta["tag"]
    results_dir = Path(f"{RESULTS_ROOT}/{tag}")

    # Collect output bytes across all chunks. Skip non-completed chunks but warn.
    n_ok = n_err = 0
    all_lines = []
    for c in meta["chunks"]:
        b = client.batches.retrieve(c["batch_id"])
        if b.status != "completed":
            print(f"WARN chunk {c['chunk']} batch {b.id} status={b.status}, skipping", file=sys.stderr)
            continue
        out_bytes = client.files.content(b.output_file_id).read()
        chunk_out = results_dir / f"_output_chunk{c['chunk']:02d}.jsonl"
        chunk_out.write_bytes(out_bytes)
        for line in out_bytes.decode().splitlines():
            all_lines.append(line)
    print(f"collected {len(all_lines)} response records from {len(meta['chunks'])} chunks",
          file=sys.stderr)

    for line in all_lines:
        rec = json.loads(line)
        gse = rec["custom_id"]
        body = rec.get("response", {}).get("body") if rec.get("response") else None
        if not body:
            n_err += 1
            with open(results_dir / f"{gse}.json", "w") as f:
                json.dump({"gse": gse, "error": rec.get("error") or "no response body"}, f)
            continue
        choices = body.get("choices", [])
        content = choices[0]["message"]["content"] if choices else None
        try:
            parsed = json.loads(content) if content else {}
        except json.JSONDecodeError:
            parsed = {}
        usage = body.get("usage", {})
        rec_out = {
            "gse": gse,
            "strains": parsed.get("strains", []),
            "usage": {
                "input_tokens": usage.get("prompt_tokens", 0),
                "output_tokens": usage.get("completion_tokens", 0),
            },
            "stop_reason": choices[0].get("finish_reason") if choices else None,
            "model": MODEL,
            "provider": "openai_batch",
            "system_fingerprint": body.get("system_fingerprint"),
        }
        with open(results_dir / f"{gse}.json", "w") as f:
            json.dump(rec_out, f, indent=2)
        n_ok += 1
    print(f"parsed {n_ok} ok / {n_err} err into {results_dir}/")

    # Aggregate summary.tsv exactly like run_open_sample.py
    from run_sample import extract_pred_uris, normalize_uri, metrics
    rows = load_sample(meta["sample"])
    if meta.get("limit_was"):
        rows = rows[: meta["limit_was"]]
    SUMMARY_FIELDS = [
        "gse", "truth", "claude_pred", "gpt4o_pred",
        "claude_tp", "claude_fp", "claude_fn",
        "claude_match", "gpt4o_match",
        "input_tokens", "output_tokens", "cache_read",
        "error",
    ]
    out_rows = []
    sum_tp = sum_fp = sum_fn = 0
    exacts = 0; n_have = 0
    for row in rows:
        gse = row["shortName"]
        p = results_dir / f"{gse}.json"
        if not p.exists():
            r = {k: "" for k in SUMMARY_FIELDS} | {"gse": gse, "error": "missing"}
            out_rows.append(r); continue
        res = json.load(open(p))
        if "error" in res and "strains" not in res:
            r = {k: "" for k in SUMMARY_FIELDS} | {"gse": gse, "error": res["error"]}
            out_rows.append(r); continue
        pred = extract_pred_uris(res.get("strains", []))
        truth = {normalize_uri(u) for u in row["gemma_uri"].split(",") if u}
        gpt = {normalize_uri(u.strip()) for u in row["gpt_uri"].split(",") if u.strip()}
        m = metrics(pred, truth)
        m_gpt = metrics(gpt, truth)
        sum_tp += m["tp"]; sum_fp += m["fp"]; sum_fn += m["fn"]
        if m["exact_match"]: exacts += 1
        n_have += 1
        out_rows.append({
            "gse": gse,
            "truth": ",".join(sorted(truth)),
            "claude_pred": ",".join(sorted(pred)),
            "gpt4o_pred": ",".join(sorted(gpt)),
            "claude_tp": m["tp"], "claude_fp": m["fp"], "claude_fn": m["fn"],
            "claude_match": m["exact_match"],
            "gpt4o_match": m_gpt["exact_match"],
            "input_tokens": res["usage"]["input_tokens"],
            "output_tokens": res["usage"]["output_tokens"],
            "cache_read": 0,
        })
    import csv
    sp = results_dir / "summary.tsv"
    with open(sp, "w") as f:
        w = csv.DictWriter(f, fieldnames=SUMMARY_FIELDS, delimiter="\t", extrasaction="ignore")
        w.writeheader()
        for r in out_rows:
            w.writerow(r)
    print(f"wrote {sp}")
    print(f"Exact-match (Rogic-faithful GPT-4o): {exacts}/{n_have} = {exacts/n_have:.1%}" if n_have else "no results")


def main():
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest="cmd", required=True)

    s = sub.add_parser("submit")
    s.add_argument("--sample", required=True)
    s.add_argument("--prompt", required=True)
    s.add_argument("--suffix", required=True)
    s.add_argument("--limit", type=int, default=None)
    s.add_argument("--chunk-token-cap", type=int, default=1_000_000,
                   help="Pack chunks under this many tokens (tier-2 cap is 1.35M).")
    s.add_argument("--strip-papers", action="store_true",
                   help="Drop the `papers` key from each input (paper-vs-no-paper A/B).")

    p = sub.add_parser("poll")
    p.add_argument("--suffix", required=True)

    f = sub.add_parser("finalize")
    f.add_argument("--suffix", required=True)

    r = sub.add_parser("recover")
    r.add_argument("--suffix", required=True)

    args = ap.parse_args()
    {"submit": cmd_submit, "poll": cmd_poll, "finalize": cmd_finalize,
     "recover": cmd_recover}[args.cmd](args)


if __name__ == "__main__":
    main()
