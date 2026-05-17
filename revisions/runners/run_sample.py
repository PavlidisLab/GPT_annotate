"""Run Claude strain annotation on the sampled GSEs and compare to Gemma ground truth + published GPT-4o output."""
import argparse
import concurrent.futures
import csv
import json
import os
import sys
import time
from typing import Optional

import anthropic

from geo_fetch import build_input
from strain_annotate import (
    MODEL,
    PROMPT_PATH,
    STRAIN_LIST_PATH,
    STRAIN_TOOL,
    get_api_key,
)

DEFAULT_SAMPLE_PATH = "revisions/data/sample.tsv"
RESULTS_ROOT = "revisions/data/results"


def build_system_blocks() -> list[dict]:
    """System prompt with the strain list, cached for reuse across calls."""
    with open(PROMPT_PATH) as f:
        prompt = f.read().rstrip()
    with open(STRAIN_LIST_PATH) as f:
        strain_list = json.load(f)
    return [
        {"type": "text", "text": prompt + "\n" + json.dumps(strain_list),
         "cache_control": {"type": "ephemeral"}},
    ]


def annotate(client, system_blocks, gse: str, max_tokens=2048) -> dict:
    user_input = build_input(gse)
    kwargs = dict(
        model=MODEL,
        max_tokens=max_tokens,
        system=system_blocks,
        tools=[STRAIN_TOOL],
        tool_choice={"type": "tool", "name": "report_strains"},
        messages=[{"role": "user", "content": json.dumps(user_input)}],
    )
    if not MODEL.startswith("claude-opus-4-7"):
        kwargs["temperature"] = 0
    msg = client.messages.create(**kwargs)
    tool_use = next((b for b in msg.content if b.type == "tool_use"), None)
    strains = tool_use.input.get("strains", []) if tool_use else []
    return {
        "gse": gse,
        "strains": strains,
        "usage": {
            "input_tokens": msg.usage.input_tokens,
            "output_tokens": msg.usage.output_tokens,
            "cache_creation_input_tokens": getattr(msg.usage, "cache_creation_input_tokens", 0),
            "cache_read_input_tokens": getattr(msg.usage, "cache_read_input_tokens", 0),
        },
        "stop_reason": msg.stop_reason,
        "model": MODEL,
    }


def load_sample(path: str) -> list[dict]:
    rows = []
    with open(path) as f:
        reader = csv.DictReader(f, delimiter="\t")
        for r in reader:
            rows.append(r)
    return rows


def normalize_uri(u: str) -> str:
    """Collapse cosmetic URI variations so the prediction-vs-truth comparison is robust
    to namespace prefix duplication. Early runs of this pipeline used a strain list whose
    URIs were of the form http://www.ebi.ac.uk/efo/efo_EFO_xxx (because the OBO id had a
    lowercase 'efo:' prefix that got embedded into the URL). The model faithfully copied
    those URIs back; we strip the spurious prefix here so they match Gemma's clean form."""
    if not u:
        return u
    return u.replace("/efo_EFO_", "/EFO_").replace("/efo_NCBITaxon_", "/NCBITaxon_")


def extract_pred_uris(strains) -> set[str]:
    """Return the set of predicted URIs from a Claude response, robust to occasional
    malformed (non-dict) tool-call outputs."""
    out = set()
    if not isinstance(strains, list):
        return out
    for s in strains:
        if isinstance(s, dict) and isinstance(s.get("URI"), str):
            out.add(normalize_uri(s["URI"]))
    return out


def metrics(pred_uris: set[str], truth_uris: set[str]) -> dict:
    tp = len(pred_uris & truth_uris)
    fp = len(pred_uris - truth_uris)
    fn = len(truth_uris - pred_uris)
    recall = tp / (tp + fn) if (tp + fn) else None
    precision = tp / (tp + fp) if (tp + fp) else None
    if recall is None or precision is None:
        f1 = None
    elif (recall + precision) == 0:
        f1 = 0.0
    else:
        f1 = 2 * recall * precision / (recall + precision)
    return {
        "tp": tp, "fp": fp, "fn": fn,
        "recall": recall, "precision": precision, "f1": f1,
        "exact_match": (fp == 0 and fn == 0),
    }


def main(sample_path: str = DEFAULT_SAMPLE_PATH, workers: int = 4, force: bool = False,
         results_suffix: str = ""):
    tag = MODEL + (f"_{results_suffix}" if results_suffix else "")
    results_dir = f"{RESULTS_ROOT}/{tag}"
    os.makedirs(results_dir, exist_ok=True)
    rows = load_sample(sample_path)
    print(f"Loaded {len(rows)} GSEs from {sample_path} -> {results_dir}", file=sys.stderr)

    client = anthropic.Anthropic(api_key=get_api_key())
    system_blocks = build_system_blocks()

    def process(row):
        gse = row["shortName"]
        cache_path = os.path.join(results_dir, f"{gse}.json")
        if os.path.exists(cache_path) and not force:
            with open(cache_path) as f:
                return row, json.load(f)
        try:
            t0 = time.time()
            res = annotate(client, system_blocks, gse)
            res["elapsed_s"] = round(time.time() - t0, 2)
            with open(cache_path, "w") as f:
                json.dump(res, f, indent=2)
            return row, res
        except Exception as e:
            err = {"gse": gse, "error": repr(e)}
            return row, err

    results = []
    with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as ex:
        futures = [ex.submit(process, r) for r in rows]
        for f in concurrent.futures.as_completed(futures):
            row, res = f.result()
            results.append((row, res))
            gse = row["shortName"]
            if "error" in res:
                print(f"  {gse}: ERROR {res['error']}", file=sys.stderr)
            else:
                pred = extract_pred_uris(res.get("strains", []))
                truth = {normalize_uri(u) for u in row["gemma_uri"].split(",") if u}
                m = metrics(pred, truth)
                tag = "OK " if m["exact_match"] else "mis"
                print(f"  {tag} {gse}: pred={sorted(pred)} truth={sorted(truth)} tp/fp/fn={m['tp']}/{m['fp']}/{m['fn']}", file=sys.stderr)

    # aggregate
    rows_out = []
    sum_tp = sum_fp = sum_fn = 0
    recalls = []
    precisions = []
    f1s = []
    exacts = 0
    cache_read_total = 0
    cache_create_total = 0
    input_tokens_total = 0
    output_tokens_total = 0
    n_ok = 0
    SUMMARY_FIELDS = [
        "gse", "truth", "claude_pred", "gpt4o_pred",
        "claude_tp", "claude_fp", "claude_fn",
        "claude_match", "gpt4o_match",
        "input_tokens", "output_tokens", "cache_read",
        "error",
    ]
    def blank_row(gse):
        return {k: "" for k in SUMMARY_FIELDS} | {"gse": gse}

    for row, res in results:
        if "error" in res:
            r = blank_row(row["shortName"])
            r["error"] = res["error"]
            rows_out.append(r)
            continue
        n_ok += 1
        pred_set = extract_pred_uris(res.get("strains", []))
        truth_set = {normalize_uri(u) for u in row["gemma_uri"].split(",") if u}
        gpt_set = {normalize_uri(u.strip()) for u in row["gpt_uri"].split(",") if u.strip()}
        pred_uris = sorted(pred_set)
        truth_uris = sorted(truth_set)
        gpt_uris = sorted(gpt_set)
        m = metrics(pred_set, truth_set)
        m_gpt = metrics(gpt_set, truth_set)
        sum_tp += m["tp"]; sum_fp += m["fp"]; sum_fn += m["fn"]
        if m["recall"] is not None: recalls.append(m["recall"])
        if m["precision"] is not None: precisions.append(m["precision"])
        if m["f1"] is not None: f1s.append(m["f1"])
        if m["exact_match"]: exacts += 1
        cache_read_total += res["usage"].get("cache_read_input_tokens", 0) or 0
        cache_create_total += res["usage"].get("cache_creation_input_tokens", 0) or 0
        input_tokens_total += res["usage"]["input_tokens"]
        output_tokens_total += res["usage"]["output_tokens"]
        rows_out.append({
            "gse": row["shortName"],
            "truth": ",".join(truth_uris),
            "claude_pred": ",".join(pred_uris),
            "gpt4o_pred": ",".join(gpt_uris),
            "claude_tp": m["tp"], "claude_fp": m["fp"], "claude_fn": m["fn"],
            "claude_match": m["exact_match"],
            "gpt4o_match": m_gpt["exact_match"],
            "input_tokens": res["usage"]["input_tokens"],
            "output_tokens": res["usage"]["output_tokens"],
            "cache_read": res["usage"].get("cache_read_input_tokens", 0),
        })

    out_path = os.path.join(results_dir, "summary.tsv")
    if rows_out:
        with open(out_path, "w") as f:
            w = csv.DictWriter(f, fieldnames=SUMMARY_FIELDS, delimiter="\t", extrasaction="ignore")
            w.writeheader()
            for r in rows_out:
                w.writerow(r)

    def mean(xs): return sum(xs) / len(xs) if xs else float("nan")

    print()
    print("=" * 60)
    print(f"Model: {MODEL}")
    print(f"Experiments: {n_ok}")
    print(f"Exact-match rate (Claude): {exacts}/{n_ok} = {exacts/n_ok:.1%}" if n_ok else "no successful runs")
    if n_ok:
        gpt_exacts = sum(1 for r in rows_out if "gpt4o_match" in r and r["gpt4o_match"])
        print(f"Exact-match rate (published GPT-4o on same sample): {gpt_exacts}/{n_ok} = {gpt_exacts/n_ok:.1%}")
        print(f"Mean recall (Claude):    {mean(recalls):.3f}")
        print(f"Mean precision (Claude): {mean(precisions):.3f}")
        print(f"Mean F1 (Claude):        {mean(f1s):.3f}")
        print(f"Aggregate TP/FP/FN: {sum_tp}/{sum_fp}/{sum_fn}")
        print(f"Token totals: input={input_tokens_total}, cache_read={cache_read_total}, cache_creation={cache_create_total}, output={output_tokens_total}")
    print(f"Wrote {out_path}")


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--sample", default=DEFAULT_SAMPLE_PATH)
    ap.add_argument("--workers", type=int, default=4)
    ap.add_argument("--force", action="store_true")
    ap.add_argument("--results-suffix", default="",
                    help="Append to results dir name; useful for noise replicates")
    args = ap.parse_args()
    main(sample_path=args.sample, workers=args.workers, force=args.force,
         results_suffix=args.results_suffix)
