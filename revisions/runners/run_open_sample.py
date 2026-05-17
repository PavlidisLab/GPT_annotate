"""Run an open-weights strain annotator over the sampled GSEs.

Parallel runner that mirrors run_sample.py but uses strain_annotate_open.py
(OpenAI-compatible API → Together/OpenRouter/Groq/Fireworks/DeepSeek).
Caches per-GSE responses under revisions/data/results/<sanitized_model_id>/
and writes a summary.tsv identical in shape to the Claude runs."""
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
import concurrent.futures
import csv
import json
import os
import sys
import time

from strain_annotate_open import MODEL, PROVIDER, annotate, get_client
from run_sample import (
    extract_pred_uris, load_sample, metrics, normalize_uri,
    DEFAULT_SAMPLE_PATH, RESULTS_ROOT,
)


def model_tag(model: str) -> str:
    return model.replace("/", "__")


def main(sample_path: str = DEFAULT_SAMPLE_PATH, workers: int = 4, force: bool = False,
         results_suffix: str = ""):
    tag = model_tag(MODEL) + (f"_{results_suffix}" if results_suffix else "")
    results_dir = f"{RESULTS_ROOT}/{tag}"
    os.makedirs(results_dir, exist_ok=True)
    rows = load_sample(sample_path)
    print(f"Loaded {len(rows)} GSEs from {sample_path} -> {results_dir}", file=sys.stderr)
    print(f"Provider: {PROVIDER}  Model: {MODEL}", file=sys.stderr)

    client = get_client()

    def process(row):
        gse = row["shortName"]
        cache_path = os.path.join(results_dir, f"{gse}.json")
        if os.path.exists(cache_path) and not force:
            with open(cache_path) as f:
                return row, json.load(f)
        try:
            t0 = time.time()
            res = annotate(gse, client=client)
            res["elapsed_s"] = round(time.time() - t0, 2)
            with open(cache_path, "w") as f:
                json.dump(res, f, indent=2)
            return row, res
        except Exception as e:
            return row, {"gse": gse, "error": repr(e)}

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
                ok = "OK " if m["exact_match"] else "mis"
                print(f"  {ok} {gse}: pred={sorted(pred)} truth={sorted(truth)} tp/fp/fn={m['tp']}/{m['fp']}/{m['fn']}",
                      file=sys.stderr)

    SUMMARY_FIELDS = [
        "gse", "truth", "claude_pred", "gpt4o_pred",
        "claude_tp", "claude_fp", "claude_fn",
        "claude_match", "gpt4o_match",
        "input_tokens", "output_tokens", "cache_read",
        "error",
    ]
    def blank(gse):
        return {k: "" for k in SUMMARY_FIELDS} | {"gse": gse}

    rows_out = []
    sum_tp = sum_fp = sum_fn = 0
    recalls = []; precisions = []; f1s = []
    exacts = 0
    input_tokens_total = output_tokens_total = 0
    n_ok = 0
    for row, res in results:
        if "error" in res:
            r = blank(row["shortName"])
            r["error"] = res["error"]
            rows_out.append(r)
            continue
        n_ok += 1
        pred_set = extract_pred_uris(res.get("strains", []))
        truth_set = {normalize_uri(u) for u in row["gemma_uri"].split(",") if u}
        gpt_set = {normalize_uri(u.strip()) for u in row["gpt_uri"].split(",") if u.strip()}
        m = metrics(pred_set, truth_set)
        m_gpt = metrics(gpt_set, truth_set)
        sum_tp += m["tp"]; sum_fp += m["fp"]; sum_fn += m["fn"]
        if m["recall"] is not None: recalls.append(m["recall"])
        if m["precision"] is not None: precisions.append(m["precision"])
        if m["f1"] is not None: f1s.append(m["f1"])
        if m["exact_match"]: exacts += 1
        input_tokens_total += res["usage"]["input_tokens"]
        output_tokens_total += res["usage"]["output_tokens"]
        # NB: we keep the `claude_pred` / `claude_match` column names for
        # cross-script compatibility (analyze_strain_results.R reads these);
        # the values here are the open-weights model's predictions.
        rows_out.append({
            "gse": row["shortName"],
            "truth": ",".join(sorted(truth_set)),
            "claude_pred": ",".join(sorted(pred_set)),
            "gpt4o_pred": ",".join(sorted(gpt_set)),
            "claude_tp": m["tp"], "claude_fp": m["fp"], "claude_fn": m["fn"],
            "claude_match": m["exact_match"],
            "gpt4o_match": m_gpt["exact_match"],
            "input_tokens": res["usage"]["input_tokens"],
            "output_tokens": res["usage"]["output_tokens"],
            "cache_read": 0,
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
    print(f"Provider/Model: {PROVIDER} / {MODEL}")
    print(f"Experiments: {n_ok}")
    if n_ok:
        print(f"Exact-match rate (open):   {exacts}/{n_ok} = {exacts/n_ok:.1%}")
        gpt_exacts = sum(1 for r in rows_out if r.get("gpt4o_match") is True)
        print(f"Exact-match rate (GPT-4o): {gpt_exacts}/{n_ok} = {gpt_exacts/n_ok:.1%}")
        print(f"Mean recall:    {mean(recalls):.3f}")
        print(f"Mean precision: {mean(precisions):.3f}")
        print(f"Mean F1:        {mean(f1s):.3f}")
        print(f"Aggregate TP/FP/FN: {sum_tp}/{sum_fp}/{sum_fn}")
        print(f"Token totals: input={input_tokens_total}, output={output_tokens_total}")
    print(f"Wrote {out_path}")


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--sample", default=DEFAULT_SAMPLE_PATH)
    ap.add_argument("--workers", type=int, default=4)
    ap.add_argument("--force", action="store_true")
    ap.add_argument("--results-suffix", default="")
    args = ap.parse_args()
    main(sample_path=args.sample, workers=args.workers, force=args.force,
         results_suffix=args.results_suffix)
