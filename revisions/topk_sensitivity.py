"""Top-K sensitivity for the cell-line two-pass pipeline.

For each GSE in the input sample, the *first-pass* Claude extractions are read
from the existing cache (`revisions/data/results_cl/claude-sonnet-4-6/<gse>.json`),
the embedded retrieval is recomputed at multiple K values, and the *second-pass*
Claude call is re-issued with each K. Results are saved to
`revisions/data/results_cl/claude-sonnet-4-6_topk<K>/<gse>.json` so they can be
evaluated with `cell_line_eval.py` without further code changes.

Cost notes (Sonnet 4.6, observed):
- second-pass at K=50 averages ~$0.05 / GSE.
- larger K → larger input payload (top-K candidate JSON dominates the prompt).
- 100 GSEs × 5 Ks ≈ $30.

Usage:
  revisions/.venv/bin/python revisions/topk_sensitivity.py \\
    --sample revisions/data/sample_cell500.tsv \\
    --n 100 \\
    --k 10 25 50 100 200 \\
    --workers 4
"""
from __future__ import annotations

import argparse
import concurrent.futures
import csv
import json
import os
import sys
import time

import numpy as np

import cell_line_annotate as cla
from geo_fetch import build_input

CACHE_FIRST_PASS = "revisions/data/results_cl/claude-sonnet-4-6"
EMB_PATH         = "revisions/data/cell_line_embeddings.npy"
IDS_PATH         = "revisions/data/cell_line_embedding_ids.json"
LIST_PATH        = "revisions/data/cell_line_list.json"


def retrieve_top_k(extractions, emb, ids, terms, top_k):
    """Identical to cla.retrieve_candidates but with parameterised k."""
    if not extractions:
        return {}
    queries = [f"{e.get('cell_line_name','')}\n{e.get('description','')}" for e in extractions]
    for attempt in range(5):
        try:
            resp = cla.openai_client().embeddings.create(model=cla.EMB_MODEL, input=queries)
            break
        except Exception:
            time.sleep(2 ** attempt)
    else:
        raise RuntimeError("OpenAI embeddings failed")
    q = np.asarray([d.embedding for d in resp.data], dtype=np.float32)
    qn = q / np.linalg.norm(q, axis=1, keepdims=True).clip(min=1e-12)
    sims = qn @ emb.T
    out = {}
    for i, ext in enumerate(extractions):
        top_idx = np.argpartition(-sims[i], min(top_k, len(ids) - 1))[:top_k]
        top_idx = top_idx[np.argsort(-sims[i, top_idx])]
        cand = []
        for j in top_idx:
            term_id = ids[j]
            t = terms.get(term_id, {})
            cand.append({
                "ID": term_id,
                "value": t.get("value", ""),
                "description": t.get("description", ""),
                "synonyms": t.get("synonyms", []),
                "cosine_similarity": float(sims[i, j]),
            })
        key = ext.get("cell_line_name") or f"_unnamed_{i}"
        out[key] = cand
    return out


def run_one(gse, k, emb, ids, terms, results_dir, force):
    out_path = f"{results_dir}/{gse}.json"
    if os.path.exists(out_path) and not force:
        return gse, "cached"

    fp_path = f"{CACHE_FIRST_PASS}/{gse}.json"
    if not os.path.exists(fp_path):
        return gse, "missing_first_pass"
    fp = json.load(open(fp_path))
    if "error" in fp or not fp.get("first_pass"):
        # nothing to do; copy the empty result through
        json.dump({**fp, "_topk": k, "_note": "no_first_pass"}, open(out_path, "w"))
        return gse, "no_first_pass"

    extractions = fp["first_pass"]
    try:
        cands = retrieve_top_k(extractions, emb, ids, terms, k)
        experiment = build_input(gse)
        p2 = cla.second_pass(experiment, extractions, cands)
    except Exception as e:
        return gse, f"error: {e!r}"

    json.dump({
        "gse": gse,
        "model": cla.MODEL,
        "embedding_model": cla.EMB_MODEL,
        "top_k": k,
        "first_pass": extractions,
        "annotations": p2["cell_lines"],
        "usage": {"p2": p2["usage"]},
    }, open(out_path, "w"), indent=2)
    return gse, "ok"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--sample", required=True)
    ap.add_argument("--n", type=int, default=100, help="subset size")
    ap.add_argument("--k", nargs="+", type=int, default=[10, 25, 50, 100, 200])
    ap.add_argument("--workers", type=int, default=4)
    ap.add_argument("--force", action="store_true")
    ap.add_argument("--seed", type=int, default=20260514)
    args = ap.parse_args()

    # Load + subsample GSEs deterministically
    rng = np.random.default_rng(args.seed)
    all_rows = list(csv.DictReader(open(args.sample), delimiter="\t"))
    all_gses = [r["shortName"] for r in all_rows]
    # only keep those with an existing first-pass cache
    have_fp = [g for g in all_gses if os.path.exists(f"{CACHE_FIRST_PASS}/{g}.json")]
    idx = rng.permutation(len(have_fp))[: args.n]
    gses = [have_fp[i] for i in sorted(idx)]
    print(f"Sub-sample: {len(gses)} GSEs (from {len(have_fp)} with first-pass cache)", file=sys.stderr)

    # Pre-load embedding index + term dictionary once
    print("Loading embedding index ...", file=sys.stderr)
    emb = np.load(EMB_PATH)
    ids = json.load(open(IDS_PATH))
    terms = json.load(open(LIST_PATH))
    print(f"  index shape: {emb.shape}", file=sys.stderr)

    for k in args.k:
        results_dir = f"revisions/data/results_cl/claude-sonnet-4-6_topk{k}"
        os.makedirs(results_dir, exist_ok=True)
        print(f"\n--- K={k} -> {results_dir} ---", file=sys.stderr)

        def fn(g): return run_one(g, k, emb, ids, terms, results_dir, args.force)

        with concurrent.futures.ThreadPoolExecutor(max_workers=args.workers) as ex:
            for i, (g, status) in enumerate(ex.map(fn, gses), 1):
                if status not in ("cached", "ok", "no_first_pass"):
                    print(f"  {g}: {status}", file=sys.stderr)
                if i % 25 == 0:
                    print(f"  {i}/{len(gses)}", file=sys.stderr)

    print("\nDone. Evaluate with cell_line_eval.py on each results dir.", file=sys.stderr)


if __name__ == "__main__":
    main()
