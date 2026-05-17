"""Hybrid dense + sparse retrieval for the cell-line second pass.

Drop-in replacement for `cell_line_annotate.retrieve_candidates`.

Dense channel: cached OpenAI `text-embedding-3-large` (3072-d) embeddings
in `cell_line_embeddings.npy`; query is the same `name\\ndescription`
string used in the paper.

Sparse channel: BM25 over `value + synonyms` of each ontology term,
built once by `build_bm25_index.py`. Query is the same
`name\\ndescription` string, tokenised on alphanumeric runs.

Fusion: reciprocal rank fusion with k=60 (the value Mondal et al. and
the original Cormack-2009 RRF paper both use). RRF score for a candidate
is `sum_over_retrievers (1 / (k + rank_in_retriever))`; only retrievers
that returned the candidate within their top-N contribute. We use
N = 200 per retriever before fusion so the RRF has enough overlap room
to discriminate.

Returns the same {name: [{ID, value, description, synonyms,
cosine_similarity, bm25_score, rrf_score}, ...]} shape that
`cell_line_annotate.retrieve_candidates` produces, so the downstream
second-pass code is unchanged.
"""
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

import json
import pickle
import re
from pathlib import Path

import numpy as np

EMB_PATH    = Path("revisions/data/cell_line_embeddings.npy")
IDS_PATH    = Path("revisions/data/cell_line_embedding_ids.json")
BM25_PATH   = Path("revisions/data/cell_line_bm25.pkl")
TERMS_PATH  = Path("revisions/data/cell_line_list.json")

# Tunables
PRE_FUSION_TOP_N = 200   # how many candidates each retriever contributes
RRF_K            = 60    # smoothing constant; Cormack 2009 default
DEFAULT_TOP_K    = 50    # final candidate window per query (paper parity)


_TOKEN_RE = re.compile(r"[a-z0-9]+")
def _tokenize(text: str) -> list[str]:
    return _TOKEN_RE.findall((text or "").lower())


_state: dict | None = None


def _load():
    global _state
    if _state is not None:
        return _state
    emb = np.load(EMB_PATH)
    ids = json.load(open(IDS_PATH))
    with open(BM25_PATH, "rb") as f:
        bm = pickle.load(f)
    assert bm["ids"] == ids, "BM25 ids must align with dense index ids"
    terms = json.load(open(TERMS_PATH))
    _state = {
        "emb":   emb,
        "ids":   ids,
        "bm25":  bm["bm25"],
        "terms": terms,
    }
    return _state


def hybrid_retrieve(query: str, top_k: int = DEFAULT_TOP_K) -> list[dict]:
    """Return top_k candidates for `query` under dense+BM25+RRF fusion.

    Each candidate carries `{ID, value, description, synonyms,
    cosine_similarity, bm25_score, rrf_score, dense_rank, bm25_rank}`.
    `cosine_similarity` is set even when the candidate didn't make the
    dense top-N (computed against the cached embedding) so the
    downstream second-pass can still see it; `bm25_score` similarly.
    """
    state = _load()
    emb, ids, bm25, terms = state["emb"], state["ids"], state["bm25"], state["terms"]
    n_terms = len(ids)

    # --- dense: embed the query and dot-product against the cached matrix ---
    # We avoid re-embedding by relying on the existing OpenAI client. This
    # function expects the caller to have batched query embedding already if
    # needed; for a single query call _embed_query.
    qvec = _embed_query(query)
    sims = emb @ qvec  # (N,)
    dense_top = np.argpartition(-sims, PRE_FUSION_TOP_N)[:PRE_FUSION_TOP_N]
    dense_top = dense_top[np.argsort(-sims[dense_top])]

    # --- sparse: BM25 score against the same N terms, take top-N ---
    bm_scores = bm25.get_scores(_tokenize(query))
    bm_top = np.argpartition(-bm_scores, PRE_FUSION_TOP_N)[:PRE_FUSION_TOP_N]
    bm_top = bm_top[np.argsort(-bm_scores[bm_top])]

    # --- RRF fusion ---
    rrf: dict[int, float] = {}
    dense_rank: dict[int, int] = {}
    bm_rank:    dict[int, int] = {}
    for rank, idx in enumerate(dense_top, start=1):
        rrf[idx] = rrf.get(idx, 0.0) + 1.0 / (RRF_K + rank)
        dense_rank[idx] = rank
    for rank, idx in enumerate(bm_top, start=1):
        rrf[idx] = rrf.get(idx, 0.0) + 1.0 / (RRF_K + rank)
        bm_rank[idx] = rank

    fused = sorted(rrf.items(), key=lambda kv: -kv[1])[:top_k]

    out = []
    for idx, score in fused:
        term_id = ids[idx]
        t = terms.get(term_id, {})
        out.append({
            "ID":          term_id,
            "value":       t.get("value", ""),
            "description": t.get("description", ""),
            "synonyms":    t.get("synonyms", []),
            "cosine_similarity": float(sims[idx]),
            "bm25_score":        float(bm_scores[idx]),
            "rrf_score":         float(score),
            "dense_rank":        int(dense_rank.get(idx, 0)),
            "bm25_rank":         int(bm_rank.get(idx, 0)),
        })
    return out


# --- lazy OpenAI client for query embedding -------------------------------

_oai_client = None
def _embed_query(query: str) -> np.ndarray:
    global _oai_client
    if _oai_client is None:
        import os, subprocess
        from openai import OpenAI
        key = os.environ.get("OPENAI_API_KEY")
        if not key:
            key = subprocess.check_output(
                ["security", "find-generic-password", "-s", "OPENAI_API_KEY", "-w"],
                text=True).strip()
        _oai_client = OpenAI(api_key=key)
    resp = _oai_client.embeddings.create(model="text-embedding-3-large", input=[query])
    v = np.asarray(resp.data[0].embedding, dtype=np.float32)
    v /= max(float(np.linalg.norm(v)), 1e-12)
    return v


def retrieve_candidates_hybrid(extractions: list[dict],
                               top_k: int = DEFAULT_TOP_K) -> dict[str, list[dict]]:
    """Per-extraction hybrid retrieval. Same return shape as
    cell_line_annotate.retrieve_candidates."""
    out: dict[str, list[dict]] = {}
    for i, ext in enumerate(extractions):
        query = f"{ext.get('cell_line_name','')}\n{ext.get('description','')}"
        cands = hybrid_retrieve(query, top_k=top_k)
        key = ext.get("cell_line_name") or f"_unnamed_{i}"
        out[key] = cands
    return out


if __name__ == "__main__":
    import sys
    q = sys.argv[1] if len(sys.argv) > 1 else "HEK293T\nHuman embryonic kidney cell line"
    cs = hybrid_retrieve(q, top_k=10)
    for c in cs:
        print(f"  {c['ID']:25s} rrf={c['rrf_score']:.4f}  dense_rank={c['dense_rank']:>3}  bm25_rank={c['bm25_rank']:>3}  "
              f"sim={c['cosine_similarity']:+.3f}  bm25={c['bm25_score']:6.2f}  {c['value'][:50]}")
