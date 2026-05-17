"""Validate the cell-line embedding index by recomputing top_embedding_rank_gemma for
each row of main_frame.rds and comparing to the published rank.

Method (matches analysis/cell_lines/03.compare_to_vect.R):
  - for each row's first-pass extractions, embed `name + "\n" + description` with
    text-embedding-3-large,
  - compute cosine similarity to all 46,032 ontology terms,
  - for each ground-truth gemma_uri, take the *best* rank across the row's first-pass
    embeddings,
  - report the published vs reproduced rank.
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

import json
import os
import subprocess
import sys
import time

import numpy as np
from openai import OpenAI

EMB_PATH    = "revisions/data/cell_line_embeddings.npy"
IDS_PATH    = "revisions/data/cell_line_embedding_ids.json"
LIST_PATH   = "revisions/data/cell_line_list.json"
MAIN_FRAME  = "revisions/data/cell_line_main_frame.tsv"
MODEL       = "text-embedding-3-large"


def get_openai_key() -> str:
    if "OPENAI_API_KEY" in os.environ:
        return os.environ["OPENAI_API_KEY"]
    return subprocess.check_output(
        ["security", "find-generic-password", "-s", "OPENAI_API_KEY", "-w"],
        text=True,
    ).strip()


def gemma_to_id(uri):
    """Map a Gemma URI to its CLO/EFO id used as the matrix row label."""
    if not uri:
        return None
    if "obo/CLO_" in uri:
        return "CLO:" + uri.rsplit("CLO_", 1)[1]
    if "obo/CL_" in uri:
        return "CL:" + uri.rsplit("CL_", 1)[1]
    if "efo/EFO_" in uri:
        return "EFO:" + uri.rsplit("EFO_", 1)[1]
    return None


def main(n: int = 200):
    print("Loading embedding index ...", file=sys.stderr)
    emb = np.load(EMB_PATH)             # (N, 3072), L2-normalised
    ids = json.load(open(IDS_PATH))     # list of CLO:/EFO:/CL: ids
    id_to_idx = {i: k for k, i in enumerate(ids)}
    print(f"  index: {emb.shape}", file=sys.stderr)

    rows = []
    with open(MAIN_FRAME) as f:
        header = f.readline().rstrip("\n").split("\t")
        for line in f:
            parts = line.rstrip("\n").split("\t")
            row = dict(zip(header, parts))
            rows.append(row)
    print(f"main_frame rows: {len(rows)}", file=sys.stderr)

    # Filter to rows with both a first-pass extraction and a known gemma URI.
    usable = []
    for r in rows:
        cls = r.get("gpt_cell_lines", "")
        descs = r.get("gpt_description", "")
        gemma = r.get("gemma_uri", "")
        if not cls or not gemma:
            continue
        cl_list = [c for c in cls.split("|||") if c]
        ds_list = (descs.split("|||") if descs else [""] * len(cl_list))
        if len(ds_list) < len(cl_list):
            ds_list += [""] * (len(cl_list) - len(ds_list))
        gemma_id = gemma_to_id(gemma.split(",")[0].strip())
        if gemma_id is None or gemma_id not in id_to_idx:
            continue
        usable.append({
            "shortName": r["shortName"],
            "cells": cl_list,
            "descs": ds_list[:len(cl_list)],
            "gemma_uri": gemma,
            "gemma_id": gemma_id,
            "published_rank": r.get("top_embedding_rank_gemma", ""),
        })
    print(f"usable rows (with extraction + resolvable gemma id): {len(usable)}", file=sys.stderr)
    usable = usable[:n]
    print(f"validating on {len(usable)}", file=sys.stderr)

    client = OpenAI(api_key=get_openai_key())

    diffs = []
    rep_below_50 = pub_below_50 = matched = 0
    for k, row in enumerate(usable):
        inputs = [f"{c}\n{d}" for c, d in zip(row["cells"], row["descs"])]
        if not inputs:
            continue
        for attempt in range(5):
            try:
                resp = client.embeddings.create(model=MODEL, input=inputs)
                break
            except Exception as e:
                time.sleep(2 ** attempt)
        else:
            print(f"  {row['shortName']}: embed failure, skipping", file=sys.stderr)
            continue
        q = np.asarray([d.embedding for d in resp.data], dtype=np.float32)
        qn = q / np.linalg.norm(q, axis=1, keepdims=True).clip(min=1e-12)
        sims = qn @ emb.T            # (n_queries, N)
        gemma_idx = id_to_idx[row["gemma_id"]]
        # best rank of gemma id across queries
        best_rank = None
        for j in range(sims.shape[0]):
            r = int((sims[j] > sims[j, gemma_idx]).sum() + 1)
            if best_rank is None or r < best_rank:
                best_rank = r
        try:
            published = int(row["published_rank"])
        except Exception:
            published = None
        diffs.append((row["shortName"], best_rank, published))
        if best_rank is not None and best_rank <= 50:
            rep_below_50 += 1
        if published is not None and published <= 50:
            pub_below_50 += 1
        if published is not None and best_rank == published:
            matched += 1
        if k < 10 or k % 25 == 0:
            print(f"  {row['shortName']:>10}  reproduced={best_rank:>6}  published={published}", file=sys.stderr)

    print(file=sys.stderr)
    print(f"n = {len(diffs)}", file=sys.stderr)
    print(f"exact rank match with published: {matched}", file=sys.stderr)
    print(f"in top-50 (reproduced):  {rep_below_50}", file=sys.stderr)
    print(f"in top-50 (published):   {pub_below_50}", file=sys.stderr)

    # Spearman-ish summary on rank-bucket agreement
    arr = [(r, p) for _, r, p in diffs if r is not None and p is not None]
    if arr:
        diff = np.array([abs(r - p) for r, p in arr])
        print(f"|reproduced - published|  median={int(np.median(diff))}  mean={diff.mean():.1f}  p90={int(np.percentile(diff, 90))}", file=sys.stderr)


if __name__ == "__main__":
    import argparse
    ap = argparse.ArgumentParser()
    ap.add_argument("--n", type=int, default=200)
    args = ap.parse_args()
    main(args.n)
