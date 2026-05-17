"""Embed the 46,032 cell-line ontology terms with OpenAI text-embedding-3-large.

Output:
- revisions/data/cell_line_embeddings.npy        : float32 (N, 3072) L2-normalised
- revisions/data/cell_line_embedding_ids.json    : list[str] of length N giving the ID
                                                   for each row in the matrix

Resumable: existing rows are loaded from a sidecar file and only missing IDs are sent.
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
import json
import os
import subprocess
import sys
import time
from typing import Iterable

import numpy as np
from openai import OpenAI

LIST_PATH      = "revisions/data/cell_line_list.json"
EMB_PATH       = "revisions/data/cell_line_embeddings.npy"
IDS_PATH       = "revisions/data/cell_line_embedding_ids.json"
PARTIAL_PATH   = "revisions/data/cell_line_embeddings_partial.npz"
MODEL          = "text-embedding-3-large"
DIM            = 3072  # default for text-embedding-3-large
BATCH_SIZE     = 256   # safely under per-request 300k-token cap


def get_openai_key() -> str:
    if "OPENAI_API_KEY" in os.environ:
        return os.environ["OPENAI_API_KEY"]
    return subprocess.check_output(
        ["security", "find-generic-password", "-s", "OPENAI_API_KEY", "-w"],
        text=True,
    ).strip()


def serialise_term(term: dict) -> str:
    """Match `cell_line_strings = cell_lines %>% lapply(jsonlite::toJSON)` in the original
    01.process_cell_lines.R: the embedding input is the JSON serialisation of the term object."""
    return json.dumps(term, separators=(",", ":"))


def batched(iterable: list, n: int) -> Iterable[list]:
    for i in range(0, len(iterable), n):
        yield iterable[i:i + n]


def main(force: bool = False):
    with open(LIST_PATH) as f:
        raw = json.load(f)
    # cell_line_list.json is a dict keyed by ontology ID (matching R's named list).
    # Preserve insertion order so the matrix row order is reproducible.
    if isinstance(raw, dict):
        ids = list(raw.keys())
        cell_lines = [raw[k] for k in ids]
    else:
        cell_lines = raw
        ids = [t["ID"] if isinstance(t["ID"], str) else t["ID"][0] for t in cell_lines]
    print(f"Loaded {len(cell_lines)} cell-line terms", file=sys.stderr)

    # Resume from partial output if present.
    have: dict[str, np.ndarray] = {}
    if not force and os.path.exists(PARTIAL_PATH):
        npz = np.load(PARTIAL_PATH, allow_pickle=True)
        for i, _id in enumerate(npz["ids"]):
            have[str(_id)] = npz["emb"][i]
        print(f"Loaded {len(have)} pre-computed embeddings", file=sys.stderr)

    missing_idx = [i for i, _id in enumerate(ids) if _id not in have]
    print(f"Missing: {len(missing_idx)}", file=sys.stderr)

    if missing_idx:
        client = OpenAI(api_key=get_openai_key())
        for chunk_start in range(0, len(missing_idx), BATCH_SIZE):
            chunk = missing_idx[chunk_start:chunk_start + BATCH_SIZE]
            inputs = [serialise_term(cell_lines[i]) for i in chunk]
            for attempt in range(5):
                try:
                    resp = client.embeddings.create(model=MODEL, input=inputs)
                    break
                except Exception as e:
                    print(f"  retry {attempt} after error: {e!r}", file=sys.stderr)
                    time.sleep(2 ** attempt)
            else:
                raise RuntimeError("repeated failures from OpenAI embeddings")
            for offset, item in enumerate(resp.data):
                idx = chunk[offset]
                have[ids[idx]] = np.asarray(item.embedding, dtype=np.float32)
            done = chunk_start + len(chunk)
            print(f"  {done}/{len(missing_idx)} embedded "
                  f"(total {len(have)}/{len(ids)})", file=sys.stderr)
            if done % (BATCH_SIZE * 10) == 0:
                _save_partial(have, ids)
        _save_partial(have, ids)

    # Final dense matrix in the canonical ID order.
    matrix = np.stack([have[_id] for _id in ids], axis=0)
    # L2-normalise once so retrieval is a plain dot product.
    norms = np.linalg.norm(matrix, axis=1, keepdims=True)
    norms[norms == 0] = 1.0
    matrix /= norms
    np.save(EMB_PATH, matrix.astype(np.float32))
    with open(IDS_PATH, "w") as f:
        json.dump(ids, f)
    print(f"Wrote {EMB_PATH}  shape={matrix.shape}  dtype={matrix.dtype}", file=sys.stderr)
    print(f"Wrote {IDS_PATH}  n_ids={len(ids)}", file=sys.stderr)


def _save_partial(have: dict, ids: list):
    out_ids = [i for i in ids if i in have]
    out_emb = np.stack([have[i] for i in out_ids], axis=0)
    np.savez(PARTIAL_PATH, ids=np.asarray(out_ids, dtype=object), emb=out_emb)


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--force", action="store_true")
    args = ap.parse_args()
    main(force=args.force)
