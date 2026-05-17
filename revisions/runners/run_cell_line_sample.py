"""Parallel runner for the two-pass cell-line pipeline.

Per-GSE results are cached at revisions/data/results_cl/<model_tag>/<GSE>.json so
re-runs only re-call the API for missing experiments. Writes a summary.tsv with
per-experiment metrics + Gemma / paper-GPT-4o baselines for comparison.
"""
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

import cell_line_annotate as cla

DEFAULT_SAMPLE = "revisions/data/sample_cell20.tsv"
RESULTS_ROOT   = "revisions/data/results_cl"


KNOWN_PREFIXES = {"CLO", "EFO", "CL", "BTO", "NCBITAXON", "BFO", "OBI", "PR"}


def normalize_id(s: str) -> str:
    """Map any reasonable ontology ID form to canonical PREFIX:LOCALID.

    Accepts: full IRI, prefix:LOCAL, LOCAL with underscore (e.g. EFO_0005237), and
    mixed-case prefix variants. Examples:
        http://purl.obolibrary.org/obo/CLO_0009464  -> CLO:0009464
        http://www.ebi.ac.uk/efo/EFO_0005237        -> EFO:0005237
        efo:EFO_0005237                              -> EFO:0005237
        EFO:EFO_0005237                              -> EFO:0005237 (double-prefix recovered)
        EFO_0005237                                  -> EFO:0005237
        EFO:0005237                                  -> EFO:0005237
    """
    if not s:
        return ""
    s = s.strip()
    if s.startswith("http"):
        s = s.rsplit("/", 1)[-1]
    # If there's a colon, take the part after the last colon as the "local" — this handles
    # both "efo:EFO_0005237" (lowercase namespace prefix from the OBO file) and the
    # pathological "EFO:EFO_0005237" double-prefix form.
    local = s.split(":", 1)[1] if ":" in s else s
    if "_" in local:
        prefix, _, rest = local.partition("_")
        prefix_up = prefix.upper()
        if prefix_up in KNOWN_PREFIXES:
            return f"{prefix_up}:{rest}"
    return s


def split_multi(s: str) -> list[str]:
    if not s:
        return []
    return [p for p in s.split(",") if p.strip()]


def main(sample_path: str, workers: int, force: bool, results_suffix: str):
    tag = cla.MODEL + (f"_{results_suffix}" if results_suffix else "")
    results_dir = f"{RESULTS_ROOT}/{tag}"
    os.makedirs(results_dir, exist_ok=True)

    with open(sample_path) as f:
        rows = list(csv.DictReader(f, delimiter="\t"))
    print(f"Loaded {len(rows)} GSEs from {sample_path} -> {results_dir}", file=sys.stderr)

    def process(row):
        gse = row["shortName"]
        cache_path = f"{results_dir}/{gse}.json"
        if os.path.exists(cache_path) and not force:
            with open(cache_path) as f:
                return row, json.load(f)
        try:
            res = cla.annotate(gse)
            with open(cache_path, "w") as f:
                json.dump(res, f, indent=2)
            return row, res
        except Exception as e:
            return row, {"gse": gse, "error": repr(e)}

    results = []
    with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as ex:
        for row, res in ex.map(process, rows):
            results.append((row, res))
            gse = row["shortName"]
            if "error" in res:
                print(f"  {gse}: ERROR {res['error']}", file=sys.stderr)
            else:
                pred = sorted({normalize_id(a["cell_line_ID"]) for a in res.get("annotations", []) if a.get("cell_line_ID")})
                truth = sorted({normalize_id(u) for u in split_multi(row["gemma_uri"])})
                hit = bool(set(pred) & set(truth))
                tag2 = "OK " if set(pred) == set(truth) else ("hit" if hit else "mis")
                print(f"  {tag2} {gse}: pred={pred} truth={truth}", file=sys.stderr)

    # aggregate
    rows_out = []
    tot_p1_in = tot_p1_out = tot_p2_in = tot_p2_out = 0
    cache_read = cache_create = 0
    exact = any_overlap = n_ok = 0
    gpt4o_exact = 0
    for row, res in results:
        rec = {"gse": row["shortName"], "truth": row["gemma_uri"], "error": ""}
        if "error" in res:
            rec["error"] = res["error"]
            rows_out.append(rec); continue
        n_ok += 1
        pred_ids = sorted({normalize_id(a.get("cell_line_ID","")) for a in res.get("annotations", []) if a.get("cell_line_ID")})
        truth_ids = sorted({normalize_id(u) for u in split_multi(row["gemma_uri"])})
        gpt4o_ids = sorted({normalize_id(p) for p in split_multi(row["gpt_term_id"])})
        m_exact = set(pred_ids) == set(truth_ids)
        m_overlap = bool(set(pred_ids) & set(truth_ids))
        m_gpt4o = set(gpt4o_ids) == set(truth_ids)
        if m_exact: exact += 1
        if m_overlap: any_overlap += 1
        if m_gpt4o: gpt4o_exact += 1
        rec.update({
            "first_pass_n": len(res.get("first_pass", [])),
            "claude_pred": ",".join(pred_ids),
            "gpt4o_pred":  ",".join(gpt4o_ids),
            "claude_exact": m_exact,
            "claude_overlap": m_overlap,
            "gpt4o_exact": m_gpt4o,
        })
        u = res.get("usage", {})
        tot_p1_in  += u.get("p1", {}).get("input_tokens", 0)
        tot_p1_out += u.get("p1", {}).get("output_tokens", 0)
        tot_p2_in  += u.get("p2", {}).get("input_tokens", 0)
        tot_p2_out += u.get("p2", {}).get("output_tokens", 0)
        cache_read += u.get("p1", {}).get("cache_read", 0) + u.get("p2", {}).get("cache_read", 0)
        cache_create += u.get("p1", {}).get("cache_creation", 0) + u.get("p2", {}).get("cache_creation", 0)
        rows_out.append(rec)

    out_path = f"{results_dir}/summary.tsv"
    fields = ["gse","truth","first_pass_n","claude_pred","gpt4o_pred",
              "claude_exact","claude_overlap","gpt4o_exact","error"]
    with open(out_path, "w") as f:
        w = csv.DictWriter(f, fieldnames=fields, delimiter="\t", extrasaction="ignore")
        w.writeheader()
        for r in rows_out: w.writerow(r)

    print()
    print("=" * 60)
    print(f"Model: {cla.MODEL}  Embedding: {cla.EMB_MODEL}  Top-K: {cla.TOP_K}")
    if n_ok:
        print(f"Experiments: {n_ok}")
        print(f"Exact match (Claude): {exact}/{n_ok} = {exact/n_ok:.1%}")
        print(f"Any overlap (Claude): {any_overlap}/{n_ok} = {any_overlap/n_ok:.1%}")
        print(f"Exact match (GPT-4o, published): {gpt4o_exact}/{n_ok} = {gpt4o_exact/n_ok:.1%}")
        print(f"Tokens  p1={tot_p1_in}in/{tot_p1_out}out  p2={tot_p2_in}in/{tot_p2_out}out  cache_read={cache_read} cache_creation={cache_create}")
    print(f"Wrote {out_path}")


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--sample", default=DEFAULT_SAMPLE)
    ap.add_argument("--workers", type=int, default=4)
    ap.add_argument("--force", action="store_true")
    ap.add_argument("--results-suffix", default="")
    args = ap.parse_args()
    main(args.sample, args.workers, args.force, args.results_suffix)
