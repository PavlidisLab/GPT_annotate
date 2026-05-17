"""BM25 standalone baseline for the strain task.

For each GSE in the 500-sample, score all 156 mouse-strain ontology terms
against the GSE's full input document (GEO overall_design + summary +
sample characteristics + paper title/abstract/methods if available),
return the top-K terms above a score threshold, and evaluate as an
exact-match annotation against Gemma truth.

Provides the same baseline shape as the text2term and regex baselines
already in fig4. No LLM is in the loop on either side — this is a pure
sparse-retrieval baseline, parallel to text2term."""
from __future__ import annotations

import argparse
import csv
import json
import os
import re
import sys
from pathlib import Path

import numpy as np
from rank_bm25 import BM25Okapi

from geo_fetch import build_input

STRAIN_LIST = "revisions/data/strain_list.json"
OUT_DIR     = "revisions/data/results/bm25_strain"

_TOKEN_RE = re.compile(r"[a-z0-9]+")
def tokenize(text: str) -> list[str]:
    return _TOKEN_RE.findall((text or "").lower())


def build_strain_bm25():
    terms = json.load(open(STRAIN_LIST))
    docs = []
    for t in terms:
        parts = [t.get("value") or ""] + [s for s in (t.get("synonyms") or []) if s]
        docs.append(tokenize(" ".join(parts)))
    return BM25Okapi(docs), terms


def _flatten(v) -> str:
    """Coerce any GEO field value (str | list[str] | nested) to a single string."""
    if v is None:    return ""
    if isinstance(v, str):  return v
    if isinstance(v, list): return "\n".join(_flatten(x) for x in v)
    return json.dumps(v)


def document_for_gse(experiment: dict) -> str:
    """Concatenate every field of the experiment payload into a single
    BM25 document. Matches what the LLM saw."""
    parts: list[str] = [
        _flatten(experiment.get("overall_design")),
        _flatten(experiment.get("summary")),
    ]
    for s in experiment.get("samples", []) or []:
        parts.append(_flatten(s.get("title")))
        parts.append(_flatten(s.get("characteristics")))
        parts.append(_flatten(s.get("protocol")))
    for p in experiment.get("papers", []) or []:
        parts.append(_flatten(p.get("title")))
        parts.append(_flatten(p.get("abstract")))
        parts.append(_flatten(p.get("methods")))
    return "\n".join(parts)


def annotate(gse: str, bm25, terms, top_k: int = 3, rel_thresh: float = 0.7):
    """Return list of (URI, value, score) for the top BM25 matches.

    rel_thresh: keep only terms whose score is at least rel_thresh × the top
    score, to avoid dragging in irrelevant matches when the top hit is itself
    weak. Combined with a global absolute floor of 1.0 to drop near-zero hits."""
    exp = build_input(gse)
    doc = document_for_gse(exp)
    scores = bm25.get_scores(tokenize(doc))
    order = np.argsort(-scores)[:top_k]
    top_score = scores[order[0]] if len(order) else 0.0
    out = []
    for i in order:
        s = float(scores[i])
        if s < 1.0:                # absolute floor
            break
        if s < rel_thresh * top_score:
            break
        out.append({"URI": terms[i]["URI"], "value": terms[i]["value"], "score": s})
    return out


def main(sample_path: str = "revisions/data/sample500.tsv",
         top_k: int = 3, rel_thresh: float = 0.7):
    os.makedirs(OUT_DIR, exist_ok=True)
    bm25, terms = build_strain_bm25()
    print(f"BM25 over {len(terms)} strain terms", file=sys.stderr)

    rows = list(csv.DictReader(open(sample_path), delimiter='\t'))
    print(f"Annotating {len(rows)} GSEs (top-{top_k}, rel_thresh={rel_thresh})", file=sys.stderr)

    SUMMARY_FIELDS = [
        "gse", "truth", "claude_pred", "gpt4o_pred",
        "claude_tp", "claude_fp", "claude_fn",
        "claude_match", "gpt4o_match",
        "input_tokens", "output_tokens", "cache_read",
        "error",
    ]
    out_rows = []
    n_exact = 0
    for i, r in enumerate(rows, 1):
        gse = r['shortName']
        try:
            preds = annotate(gse, bm25, terms, top_k=top_k, rel_thresh=rel_thresh)
        except Exception as e:
            print(f"  ERR {gse}: {e}", file=sys.stderr)
            row = {k: "" for k in SUMMARY_FIELDS} | {"gse": gse, "error": repr(e)}
            out_rows.append(row); continue

        pred_uris = sorted({p["URI"] for p in preds})
        truth_uris = sorted({u for u in (r.get("gemma_uri") or "").split(",") if u})
        gpt_uris  = sorted({u.strip() for u in (r.get("gpt_uri") or "").split(",") if u.strip()})
        tp = len(set(pred_uris) & set(truth_uris))
        fp = len(set(pred_uris) - set(truth_uris))
        fn = len(set(truth_uris) - set(pred_uris))
        exact = (fp == 0 and fn == 0)
        n_exact += int(exact)
        out_rows.append({
            "gse": gse,
            "truth": ",".join(truth_uris),
            "claude_pred": ",".join(pred_uris),
            "gpt4o_pred":  ",".join(gpt_uris),
            "claude_tp": tp, "claude_fp": fp, "claude_fn": fn,
            "claude_match": exact,
            "gpt4o_match":  (sorted(gpt_uris) == truth_uris),
            "input_tokens": 0, "output_tokens": 0, "cache_read": 0,
        })
        # Per-GSE JSON for symmetry with other runs
        with open(f"{OUT_DIR}/{gse}.json", "w") as f:
            json.dump({"gse": gse, "preds": preds}, f)
        if i % 100 == 0:
            print(f"  {i}/{len(rows)}  exact={n_exact}/{i} = {n_exact/i:.1%}", file=sys.stderr)

    sp = f"{OUT_DIR}/summary.tsv"
    with open(sp, "w") as f:
        w = csv.DictWriter(f, fieldnames=SUMMARY_FIELDS, delimiter='\t', extrasaction='ignore')
        w.writeheader()
        for r in out_rows: w.writerow(r)
    n = sum(1 for r in out_rows if not r.get("error"))
    print()
    print(f"BM25 standalone strain baseline (top-{top_k}, rel_thresh={rel_thresh}):")
    print(f"  Exact-match rate: {n_exact}/{n} = {n_exact/n:.1%}")
    print(f"  Wrote {sp}")


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--sample", default="revisions/data/sample500.tsv")
    ap.add_argument("--top-k", type=int, default=3)
    ap.add_argument("--rel-thresh", type=float, default=0.7)
    args = ap.parse_args()
    main(sample_path=args.sample, top_k=args.top_k, rel_thresh=args.rel_thresh)
