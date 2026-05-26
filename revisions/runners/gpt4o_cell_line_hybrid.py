"""GPT-4o cell-line Stage-2 with hybrid (dense + BM25 + RRF) retrieval.

Reuses the original Stage-1 output from
``cell_line_main_frame.tsv`` (columns ``gpt_cell_lines``,
``gpt_description``, ``gpt_quote`` — each ``|||``-separated) as the
first-pass extractions, applies our hybrid retrieval to produce a
top-50 candidate set per query, and issues GPT-4o Stage-2 calls via
the OpenAI Batch API in faithful mode (``response_format``
json_schema for ``cell_line_annotation``, seed=1, top_p=1,
max_tokens=1024, temperature=0).

The output schema matches the existing
``revisions/data/results_cl/<tag>/<gse>.json`` layout used elsewhere
in the repo so the downstream eval (`cell_line_eval.py`,
`cell_line_inherit_curator.py`) can ingest the new run directly with a
RESULTS_DIR override."""
from __future__ import annotations

# Path bootstrap so flat `from strain_annotate import X` and other
# revisions/-local imports keep working after subdir reorganisation.
import sys as _sys
from pathlib import Path as _Path
_REV = _Path(__file__).resolve().parents[1]
for _sub in ("", "annotators", "runners", "baselines", "build", "analysis", "metrics"):
    _p = str(_REV / _sub) if _sub else str(_REV)
    if _p not in _sys.path:
        _sys.path.insert(0, _p)
del _sys, _Path, _REV, _sub, _p

import argparse
import csv
import json
import os
import subprocess
import sys
import time
from pathlib import Path

from openai import OpenAI

from geo_fetch import build_input
from hybrid_retrieval import retrieve_candidates_hybrid

MODEL          = "gpt-4o-2024-11-20"
MAIN_FRAME     = "revisions/data/cell_line_main_frame.tsv"
SAMPLE         = "revisions/data/sample_cell500.tsv"
PROMPT_P2      = "revisions/cell_line_prompt_p2.txt"
RESULTS_ROOT   = "revisions/data/results_cl"
ENQUEUE_CAP    = 1_000_000  # tier-2 batch enqueue safety cap

CELL_LINE_RESPONSE_FORMAT = {
    "type": "json_schema",
    "json_schema": {
        "name": "cell_line",
        "description": "From the list of annotation terms provided, pick the one "
                       "that best matches the cell line used in the experiment. "
                       "If no term is a suitable match, return an empty array",
        "strict": True,
        "schema": {
            "type": "object",
            "additionalProperties": False,
            "required": ["cell_lines"],
            "properties": {
                "cell_lines": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "additionalProperties": False,
                        "required": ["cell_line_name", "cell_line_ID"],
                        "properties": {
                            "cell_line_name": {"type": "string", "description": "Name of the cell line."},
                            "cell_line_ID":   {"type": "string", "description": "Ontology ID of the cell line"},
                        },
                    },
                }
            },
        },
    },
}


def _resolve_key() -> str:
    if os.environ.get("OPENAI_API_KEY"): return os.environ["OPENAI_API_KEY"]
    for entry in ("OPENAI_API_KEY", "openai", "OpenAI"):
        try:
            return subprocess.check_output(
                ["security", "find-generic-password", "-s", entry, "-w"],
                text=True, stderr=subprocess.DEVNULL).strip()
        except subprocess.CalledProcessError:
            continue
    raise SystemExit("ERROR: no OPENAI_API_KEY in env or Keychain")


def _client() -> OpenAI: return OpenAI(api_key=_resolve_key())


def load_rogic_stage1() -> dict[str, list[dict]]:
    """Map gse -> list[{cell_line_name, description, quote}] using the original
    published Stage-1 columns."""
    out: dict[str, list[dict]] = {}
    for r in csv.DictReader(open(MAIN_FRAME), delimiter='\t'):
        gse = r["shortName"]
        names = (r.get("gpt_cell_lines") or "").split("|||")
        descs = (r.get("gpt_description") or "").split("|||")
        quotes = (r.get("gpt_quote") or "").split("|||")
        # Pad to same length so zip works deterministically
        n = max(len(names), len(descs), len(quotes))
        def get(L, i): return L[i] if i < len(L) else ""
        items = []
        for i in range(n):
            nm = get(names, i).strip()
            if not nm: continue
            items.append({
                "cell_line_name": nm,
                "description":    get(descs, i).strip(),
                "quote":          [q.strip() for q in get(quotes, i).split("❗") if q.strip()],
            })
        out[gse] = items
    return out


def system_prompt() -> str:
    with open(PROMPT_P2) as f: return f.read().rstrip()


def build_body(experiment: dict, first_pass: list[dict],
               candidates: dict[str, list[dict]]) -> dict:
    """Stage-2 request body. Mirrors the original ask_gpt body exactly."""
    payload = dict(experiment)
    payload["gpt_inference"] = first_pass
    payload["ontology_terms"] = candidates
    return {
        "model":           MODEL,
        "messages": [
            {"role": "system", "content": system_prompt()},
            {"role": "user",   "content": json.dumps(payload)},
        ],
        "response_format": CELL_LINE_RESPONSE_FORMAT,
        "max_tokens":      1024,
        "seed":            1,
        "temperature":     0,
        "top_p":           1,
    }


def _est_tokens(body: dict) -> int: return len(json.dumps(body)) // 4


def _pack(lines: list[dict], cap: int) -> list[list[dict]]:
    chunks, cur, t = [], [], 0
    for ln in lines:
        n = _est_tokens(ln["body"])
        if cur and t + n > cap:
            chunks.append(cur); cur, t = [], 0
        cur.append(ln); t += n
    if cur: chunks.append(cur)
    return chunks


def cmd_submit(args):
    suffix = "hybrid"
    tag    = f"{MODEL}_{suffix}"
    out_dir = Path(f"{RESULTS_ROOT}/{tag}"); out_dir.mkdir(parents=True, exist_ok=True)

    stage1 = load_rogic_stage1()
    rows = list(csv.DictReader(open(args.sample), delimiter='\t'))
    if args.limit: rows = rows[: args.limit]

    print(f"Building {len(rows)} request bodies (hybrid retrieval + GPT-4o Stage-2)…", file=sys.stderr)
    lines = []
    skipped = 0
    for r in rows:
        gse = r["shortName"]
        fp = stage1.get(gse)
        if not fp:
            skipped += 1
            continue
        experiment = build_input(gse)
        cands = retrieve_candidates_hybrid(fp)
        body = build_body(experiment, fp, cands)
        lines.append({
            "custom_id": gse,
            "method":    "POST",
            "url":       "/v1/chat/completions",
            "body":      body,
        })
    print(f"  {len(lines)} requests, {skipped} skipped (no original Stage-1)", file=sys.stderr)

    chunks = _pack(lines, ENQUEUE_CAP)
    print(f"  packed into {len(chunks)} chunks under {ENQUEUE_CAP:,} tokens each", file=sys.stderr)

    client = _client()
    chunk_meta = []
    for i, chunk in enumerate(chunks):
        jsonl = out_dir / f"_input_chunk{i:02d}.jsonl"
        with open(jsonl, "w") as f:
            for ln in chunk: f.write(json.dumps(ln) + "\n")
        while True:
            try:
                up = client.files.create(file=open(jsonl, "rb"), purpose="batch")
                b = client.batches.create(
                    input_file_id=up.id, endpoint="/v1/chat/completions",
                    completion_window="24h",
                    metadata={"suffix": suffix, "chunk": str(i)})
                b2 = client.batches.retrieve(b.id)
                if b2.status == "failed":
                    msg = " ".join((getattr(d, "message", "") or "") for d in (b2.errors.data if b2.errors else []))
                    if "Enqueued token limit" in msg or "enqueued tokens" in msg:
                        print(f"  chunk {i}: enqueue cap hit; sleeping 60s", file=sys.stderr)
                        time.sleep(60); continue
                    raise RuntimeError(f"chunk {i} failed: {msg}")
                est = sum(_est_tokens(l["body"]) for l in chunk)
                chunk_meta.append({"chunk": i, "batch_id": b.id, "input_file_id": up.id,
                                   "jsonl_path": str(jsonl), "n_requests": len(chunk), "est_tokens": est,
                                   "status": b2.status})
                print(f"  chunk {i:02d}: batch_id={b.id} n={len(chunk)} est={est:,} status={b2.status}",
                      file=sys.stderr)
                break
            except Exception as e:
                if "Enqueued" in str(e) or "enqueued tokens" in str(e):
                    print(f"  chunk {i}: enqueue cap (create-time); sleep 60s", file=sys.stderr)
                    time.sleep(60); continue
                raise

    meta = {"tag": tag, "sample": args.sample, "suffix": suffix,
            "n_requests": len(lines), "chunks": chunk_meta, "submitted_at": time.time()}
    with open(out_dir / "_batch_meta.json", "w") as f: json.dump(meta, f, indent=2)
    print(json.dumps({k: v for k, v in meta.items() if k != "chunks"} |
                     {"n_chunks": len(chunks)}, indent=2))


def cmd_poll(args):
    meta_path = Path(f"{RESULTS_ROOT}/{MODEL}_hybrid/_batch_meta.json")
    meta = json.load(open(meta_path))
    client = _client()
    n_ok = n_inflight = n_fail = 0
    items = []
    for c in meta["chunks"]:
        b = client.batches.retrieve(c["batch_id"])
        rc = b.request_counts.model_dump() if b.request_counts else {}
        items.append({"chunk": c["chunk"], "batch_id": b.id, "status": b.status, "rc": rc,
                      "n_requests": c["n_requests"]})
        if b.status == "completed": n_ok += c["n_requests"]
        elif b.status in ("failed", "cancelled", "expired"): n_fail += c["n_requests"]
        else: n_inflight += c["n_requests"]
    all_done = n_inflight == 0
    print(json.dumps({"all_done": all_done, "n_ok": n_ok, "n_inflight": n_inflight,
                      "n_fail": n_fail, "chunks": items}, indent=2))


def cmd_recover(args):
    """Resubmit failed chunks (enqueue-cap rebound) until they take."""
    meta_path = Path(f"{RESULTS_ROOT}/{MODEL}_hybrid/_batch_meta.json")
    meta = json.load(open(meta_path))
    client = _client()
    changed = False
    for c in meta["chunks"]:
        b = client.batches.retrieve(c["batch_id"])
        if b.status != "failed":
            c["status"] = b.status
            continue
        err = " ".join((getattr(d, "message", "") or "") for d in (b.errors.data if b.errors else []))
        if "Enqueued token limit" not in err and "enqueued tokens" not in err:
            print(f"chunk {c['chunk']} failed non-cap: {err[:120]}", file=sys.stderr)
            continue
        print(f"chunk {c['chunk']} ({c['batch_id']}): failed → resubmitting", file=sys.stderr)
        for attempt in range(30):
            up = client.files.create(file=open(c["jsonl_path"], "rb"), purpose="batch")
            nb = client.batches.create(input_file_id=up.id, endpoint="/v1/chat/completions",
                                       completion_window="24h",
                                       metadata={"suffix": "hybrid", "chunk": str(c["chunk"]),
                                                 "attempt": str(attempt)})
            # Wait for status to leave 'validating'
            for _ in range(30):
                b2 = client.batches.retrieve(nb.id)
                if b2.status != "validating": break
                time.sleep(2)
            if b2.status == "failed":
                emsg = " ".join((getattr(d, "message", "") or "") for d in (b2.errors.data if b2.errors else []))
                if "Enqueued token limit" in emsg or "enqueued tokens" in emsg:
                    print(f"  attempt {attempt}: enqueue cap; sleeping 30s", file=sys.stderr)
                    time.sleep(30); continue
                raise RuntimeError(f"chunk {c['chunk']} attempt {attempt} failed non-cap: {emsg[:120]}")
            c["batch_id"] = nb.id
            c["input_file_id"] = up.id
            c["status"] = b2.status
            changed = True
            break
    if changed:
        with open(meta_path, "w") as f: json.dump(meta, f, indent=2)
    print(json.dumps({"chunk_statuses": [c["status"] for c in meta["chunks"]]}, indent=2))


def cmd_finalize(args):
    meta_path = Path(f"{RESULTS_ROOT}/{MODEL}_hybrid/_batch_meta.json")
    meta = json.load(open(meta_path))
    tag  = meta["tag"]
    out_dir = Path(f"{RESULTS_ROOT}/{tag}")
    client = _client()
    n_parsed = n_err = 0
    for c in meta["chunks"]:
        b = client.batches.retrieve(c["batch_id"])
        if b.status != "completed":
            print(f"WARN chunk {c['chunk']} batch {b.id} status={b.status}, skipping", file=sys.stderr)
            continue
        data = client.files.content(b.output_file_id).read()
        (out_dir / f"_output_chunk{c['chunk']:02d}.jsonl").write_bytes(data)
        for line in data.decode().splitlines():
            rec = json.loads(line)
            gse = rec["custom_id"]
            body = rec.get("response", {}).get("body") if rec.get("response") else None
            if not body:
                n_err += 1
                with open(out_dir / f"{gse}.json", "w") as f:
                    json.dump({"gse": gse, "error": rec.get("error") or "no response body"}, f)
                continue
            content = body["choices"][0]["message"]["content"] if body.get("choices") else None
            try:
                parsed = json.loads(content) if content else {}
            except json.JSONDecodeError:
                parsed = {}
            usage = body.get("usage", {})
            with open(out_dir / f"{gse}.json", "w") as f:
                json.dump({
                    "gse":              gse,
                    "model":            MODEL,
                    "embedding_model":  "text-embedding-3-large",
                    "retrieval_method": "hybrid_rrf",
                    "first_pass":       [],  # not preserved per row; available via main_frame
                    "annotations":      parsed.get("cell_lines", []),
                    "usage": {"p1": {}, "p2": {
                        "input_tokens": usage.get("prompt_tokens", 0),
                        "output_tokens": usage.get("completion_tokens", 0),
                    }},
                    "stop_reason":          (body.get("choices") or [{}])[0].get("finish_reason"),
                    "system_fingerprint":   body.get("system_fingerprint"),
                }, f, indent=2)
            n_parsed += 1
    print(f"parsed {n_parsed} ok / {n_err} err into {out_dir}/")


def main():
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest="cmd", required=True)
    s = sub.add_parser("submit")
    s.add_argument("--sample", default=SAMPLE)
    s.add_argument("--limit",  type=int, default=None)
    sub.add_parser("poll")
    sub.add_parser("finalize")
    sub.add_parser("recover")
    args = ap.parse_args()
    {"submit": cmd_submit, "poll": cmd_poll, "finalize": cmd_finalize,
     "recover": cmd_recover}[args.cmd](args)


if __name__ == "__main__":
    main()
