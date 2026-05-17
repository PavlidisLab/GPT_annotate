"""Run only the second pass of the cell-line pipeline with hybrid retrieval,
reusing cached first-pass extractions from the original Sonnet 4.6 run.

For each GSE in the 500-sample cell-line set:
  - load `results_cl/claude-sonnet-4-6/<gse>.json` and lift the cached
    `first_pass` field;
  - rebuild the experiment payload via `geo_fetch.build_input` (the same
    input the original run used);
  - retrieve top-50 candidates via dense + BM25 + RRF fusion;
  - call `cell_line_annotate.second_pass` with the new candidate set;
  - save the result alongside the existing run under the suffix `_hybrid`.

First-pass calls (Stage 1, Claude extraction) are NOT re-issued — we
re-use the cached extractions verbatim. This is what makes the rerun
cheap (~$15) and keeps the comparison clean (identical first-pass
behaviour; only the candidate set differs)."""
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
import concurrent.futures
import csv
import json
import os
import sys
import time
from pathlib import Path

import cell_line_annotate as cla
from geo_fetch import build_input
from hybrid_retrieval import retrieve_candidates_hybrid

DEFAULT_CACHE_DIR = "revisions/data/results_cl/claude-sonnet-4-6"
DEFAULT_OUT_DIR   = "revisions/data/results_cl/claude-sonnet-4-6_hybrid"
DEFAULT_SAMPLE    = "revisions/data/sample_cell500.tsv"

# Set by main(); used inside annotate_one() / process() so the runner can be
# parametrised by source first-pass cache and output directory without
# threading dirs through every call site.
CACHE_DIR_BASE = DEFAULT_CACHE_DIR
OUT_DIR        = DEFAULT_OUT_DIR


def load_first_pass(gse: str) -> tuple[list[dict] | None, dict | None]:
    """Return (first_pass, original_record) from the cached Stage-1 run."""
    path = Path(CACHE_DIR_BASE) / f"{gse}.json"
    if not path.exists():
        return None, None
    rec = json.load(open(path))
    return rec.get("first_pass") or [], rec


def annotate_one(gse: str) -> dict:
    fp, original = load_first_pass(gse)
    if fp is None:
        return {"gse": gse, "error": "no cached first_pass record"}
    if not fp:
        # No cell lines extracted in stage 1 → second pass is trivially empty.
        return {
            "gse": gse,
            "model": cla.MODEL,
            "embedding_model": cla.EMB_MODEL,
            "retrieval_method": "hybrid_rrf",
            "first_pass": [],
            "annotations": [],
            "usage": {"p1": {"input_tokens": 0, "output_tokens": 0, "cache_read": 0, "cache_creation": 0},
                       "p2": {"input_tokens": 0, "output_tokens": 0, "cache_read": 0, "cache_creation": 0}},
            "timings": {"retrieval_s": 0.0, "p2_s": 0.0},
        }
    experiment = build_input(gse)

    t0 = time.time()
    cands = retrieve_candidates_hybrid(fp)
    t_retr = time.time() - t0

    t0 = time.time()
    p2 = cla.second_pass(experiment, fp, cands)
    t_p2 = time.time() - t0

    return {
        "gse": gse,
        "model": cla.MODEL,
        "embedding_model": cla.EMB_MODEL,
        "retrieval_method": "hybrid_rrf",
        "first_pass": fp,
        "annotations": p2["cell_lines"],
        "usage": {
            # First-pass usage is preserved from the cached record for cost accounting.
            "p1": (original or {}).get("usage", {}).get("p1", {}),
            "p2": p2["usage"],
        },
        "timings": {"retrieval_s": round(t_retr, 2), "p2_s": round(t_p2, 2)},
    }


def main(sample_path: str = DEFAULT_SAMPLE, workers: int = 4, force: bool = False,
         cache_dir: str | None = None, out_dir: str | None = None):
    global CACHE_DIR_BASE, OUT_DIR
    if cache_dir: CACHE_DIR_BASE = cache_dir
    if out_dir:   OUT_DIR        = out_dir
    os.makedirs(OUT_DIR, exist_ok=True)
    print(f"  source first-pass cache: {CACHE_DIR_BASE}", file=sys.stderr)
    print(f"  output dir:              {OUT_DIR}", file=sys.stderr)
    print(f"  Stage-2 model:           {cla.MODEL}", file=sys.stderr)
    rows = list(csv.DictReader(open(sample_path), delimiter="\t"))
    print(f"Loaded {len(rows)} GSEs from {sample_path} -> {OUT_DIR}", file=sys.stderr)

    def process(row):
        gse = row["shortName"]
        cache_path = os.path.join(OUT_DIR, f"{gse}.json")
        if os.path.exists(cache_path) and not force:
            with open(cache_path) as f:
                return row, json.load(f)
        try:
            res = annotate_one(gse)
            with open(cache_path, "w") as f:
                json.dump(res, f, indent=2)
            return row, res
        except Exception as e:
            return row, {"gse": gse, "error": repr(e)}

    n_done = 0
    n_err = 0
    with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as ex:
        futs = [ex.submit(process, r) for r in rows]
        for f in concurrent.futures.as_completed(futs):
            row, res = f.result()
            n_done += 1
            if "error" in res and "annotations" not in res:
                n_err += 1
                print(f"  ERR {row['shortName']}: {res['error']}", file=sys.stderr)
            if n_done % 50 == 0:
                print(f"  {n_done}/{len(rows)} done ({n_err} errors)", file=sys.stderr)
    print(f"\nFinal: {n_done} done, {n_err} errors. Output in {OUT_DIR}", file=sys.stderr)


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--sample", default=DEFAULT_SAMPLE)
    ap.add_argument("--workers", type=int, default=4)
    ap.add_argument("--force", action="store_true")
    ap.add_argument("--cache-dir", default=None,
                    help=f"Source dir holding cached first-pass records. Default: {DEFAULT_CACHE_DIR}")
    ap.add_argument("--out-dir", default=None,
                    help=f"Where to write hybrid results. Default: {DEFAULT_OUT_DIR}")
    args = ap.parse_args()
    main(sample_path=args.sample, workers=args.workers, force=args.force,
         cache_dir=args.cache_dir, out_dir=args.out_dir)
