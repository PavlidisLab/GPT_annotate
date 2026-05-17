"""Build a curator-review sheet for Sonnet 4.6's 'needs_review' cell-line cases.

Columns match the original curation.tsv format used by Rogic et al. curators,
substituting 'claude' for 'gpt' in the model-specific fields, plus two
ensemble columns appended at the end:

  Auto-populated context (read by curator):
   1  shortName
   2  claude_cell_lines             first-pass extractions (newline-separated names)
   3  claude_description            first-pass descriptions
   4  claude_quote                  first-pass quotes (joined by ❗)
   5  claude_cell_line_term         second-pass picked ontology term names
   6  claude_cell_line_term_id      second-pass picked ontology ids
   7  gemma_term                    Gemma's curated cell-line names
   8  gemma_uri                     Gemma's curated URIs
   9  top_embedding_rank_claude     best rank of Claude's pick(s) across first-pass extractions
  10  top_embedding_rank_gemma      best rank of Gemma's term(s) across first-pass extractions
  11  paper_accessible              TRUE/FALSE
  12  # guesses by Claude           count of distinct Claude picks
  13  # TRUE guesses by Claude      blank — curator fills
  14  # lines used                  blank — curator fills (or pre-filled with Gemma count)

  Curator-filled (blank initially):
  15  claude_match                  overall correct? (curator's verdict)
  16  claude_specific               all Claude picks correct? (no FP)
  17  claude_sensitive              all Gemma terms captured? (no FN)
  18  gemma_correction              did this finding correct a Gemma annotation?
  19  information_unavailable       is the experiment metadata insufficient?
  20  notes                         free text

  Ensemble context (auto):
  21  opus_cell_line_term_id        Opus 4.7's predicted URI set on this experiment
  22  ensemble_label                one of:
                                      A = Opus matches Gemma (Sonnet is the outlier)
                                      B = Opus matches Sonnet (frontier consensus ≠ Gemma)
                                      C = Opus matches GPT-4o (frontier consensus ≠ Gemma)
                                      D = all three frontier models disagree
                                      E = Opus extracted no cell line (possibly information_unavailable)

Input:
  - revisions/data/results_cl/claude-sonnet-4-6/*.json  (Sonnet outputs)
  - revisions/data/results_cl/claude-opus-4-7/*.json    (Opus outputs)
  - revisions/data/results_cl/claude-sonnet-4-6/summary_inherit.tsv (verdict buckets)
  - revisions/data/sample_cell500.tsv  (ground truth + GPT-4o predictions)
  - revisions/data/cell_line_embeddings.npy / cell_line_embedding_ids.json (index)

Output:
  - revisions/data/sonnet_curation_sheet.tsv
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

import csv
import json
import os
import subprocess
import sys
import time

import numpy as np
from openai import OpenAI

from cell_line_eval import (
    _canonical_id, CellLineXrefIndex, name_match,
    split_uris_to_ids, split_ids,
)

RESULTS_DIR  = "revisions/data/results_cl/claude-sonnet-4-6"
OPUS_DIR     = "revisions/data/results_cl/claude-opus-4-7"
INHERIT_TSV  = f"{RESULTS_DIR}/summary_inherit.tsv"
SAMPLE_TSV   = "revisions/data/sample_cell500.tsv"
EMB_PATH     = "revisions/data/cell_line_embeddings.npy"
IDS_PATH     = "revisions/data/cell_line_embedding_ids.json"
LIST_PATH    = "revisions/data/cell_line_list.json"
TERMS_PATH   = "revisions/data/cell_line_terms.json"
OUT_TSV      = "revisions/data/sonnet_curation_sheet.tsv"


def _build_label_lookup() -> dict[str, str]:
    """Return canonical-id (e.g. 'EFO:0004905') -> primary ontology label."""
    terms = json.load(open(TERMS_PATH))
    out: dict[str, str] = {}
    for cid, t in terms.items():
        ckey = _canonical_id(cid)
        names = t.get("names") or []
        label = next((n for n in names if n), "") or t.get("primary_label", "") or t.get("value", "") or t.get("label", "")
        if ckey and label:
            out[ckey] = label
    return out


def _equiv(a: set[str], b: set[str], idx) -> bool:
    if a == b: return True
    if not a and not b: return True
    if not a or not b: return False
    return name_match(a, b, idx)


def _ensemble_label(sonnet: set[str], opus: set[str], gpt: set[str],
                    truth: set[str], idx, opus_extracted: bool) -> str:
    if not opus_extracted:        return "E"
    if _equiv(opus, truth, idx):  return "A"
    if _equiv(opus, sonnet, idx): return "B"
    if _equiv(opus, gpt, idx):    return "C"
    return "D"


def _load_opus(gse: str) -> tuple[set[str], bool]:
    """Return (predicted ID set, whether Opus actually attempted extraction).

    opus_extracted = False means Opus's annotations array was empty (no cell
    line identified in the experiment metadata), which is a meaningful signal
    that the source data may lack cell-line information.
    """
    path = f"{OPUS_DIR}/{gse}.json"
    if not os.path.exists(path): return set(), False
    res = json.load(open(path))
    if "error" in res: return set(), False
    anns = res.get("annotations", [])
    if not anns: return set(), False
    ids = {_canonical_id(a.get("cell_line_ID","")) for a in anns} - {""}
    # Opus occasionally returns sentinel "UNKNOWN" instead of an empty array;
    # those canonicalise to "" and are dropped here. opus_extracted reflects
    # whether any valid id survived.
    return ids, bool(ids)

EMBED_MODEL = "text-embedding-3-large"


def get_openai_key() -> str:
    if "OPENAI_API_KEY" in os.environ:
        return os.environ["OPENAI_API_KEY"]
    return subprocess.check_output(
        ["security", "find-generic-password", "-s", "OPENAI_API_KEY", "-w"], text=True
    ).strip()


def best_rank(query_emb: np.ndarray, index_emb: np.ndarray, ids: list[str],
              id_to_idx: dict, target_ids: set[str]) -> int | None:
    """Across all first-pass queries (rows of query_emb), find the best rank
    of any target ontology id. Returns None if no target id is in the index."""
    valid_target_idx = [id_to_idx[t] for t in target_ids if t in id_to_idx]
    if not valid_target_idx:
        return None
    best = None
    for q in query_emb:
        sims = index_emb @ q
        # rank is 1-based; ties broken by index order
        for ti in valid_target_idx:
            r = int((sims > sims[ti]).sum() + 1)
            if best is None or r < best:
                best = r
    return best


def main():
    # Pull the needs_review GSE list from the inherit summary.
    with open(INHERIT_TSV) as f:
        inherit = list(csv.DictReader(f, delimiter="\t"))
    needs_review = [r["gse"] for r in inherit if r["verdict"] == "needs_review"]
    print(f"needs_review GSEs: {len(needs_review)}", file=sys.stderr)

    # Load sample (ground truth)
    sample = {r["shortName"]: r for r in csv.DictReader(open(SAMPLE_TSV), delimiter="\t")}

    # Load embedding index
    print("loading embedding index...", file=sys.stderr)
    emb = np.load(EMB_PATH)
    ids = json.load(open(IDS_PATH))
    id_to_idx = {i: k for k, i in enumerate(ids)}

    # Load term metadata for paper_accessible status
    client = OpenAI(api_key=get_openai_key())

    # Cross-walk index for ensemble equivalence
    xref_idx = CellLineXrefIndex()

    # Canonical-id -> ontology label, for the claude_cell_line_term column
    label_for = _build_label_lookup()
    def _label(cid: str) -> str:
        return label_for.get(_canonical_id(cid), "")

    rows_out = []
    for k, gse in enumerate(needs_review):
        with open(f"{RESULTS_DIR}/{gse}.json") as f:
            res = json.load(f)
        first_pass = res.get("first_pass", [])
        anns       = res.get("annotations", [])

        # Embed first-pass for rank lookups (same format as cell_line_annotate.retrieve_candidates).
        if first_pass:
            queries = [f"{e.get('cell_line_name','')}\n{e.get('description','')}" for e in first_pass]
            for attempt in range(5):
                try:
                    resp = client.embeddings.create(model=EMBED_MODEL, input=queries)
                    break
                except Exception as e:
                    time.sleep(2 ** attempt)
            else:
                print(f"  {gse}: embed failure, skipping", file=sys.stderr); continue
            q = np.asarray([d.embedding for d in resp.data], dtype=np.float32)
            q /= np.linalg.norm(q, axis=1, keepdims=True).clip(min=1e-12)
        else:
            q = np.zeros((0, emb.shape[1]), dtype=np.float32)

        claude_picks = {_canonical_id(a.get("cell_line_ID","")) for a in anns} - {""}
        gemma_picks  = split_uris_to_ids(sample.get(gse, {}).get("gemma_uri",""))
        rank_claude = best_rank(q, emb, ids, id_to_idx, claude_picks) if q.size else None
        rank_gemma  = best_rank(q, emb, ids, id_to_idx, gemma_picks)  if q.size else None

        # paper_accessible: derive from the experiment input (presence of any paper)
        try:
            from geo_fetch import build_input
            exp = build_input(gse)
            paper_accessible = bool(exp.get("papers"))
        except Exception:
            paper_accessible = ""

        # We use the literal three-pipe sequence "|||" as the within-cell array
        # separator everywhere. The sheet-upload step then replaces "|||" with
        # newlines, which line up across parallel columns (e.g. claude_cell_lines
        # row i corresponds to claude_description row i). Quotes use the same
        # "|||" separator between extractions, with "❗" between quotes within
        # a single extraction.
        SEP = "|||"
        def join_quotes(items):
            return SEP.join("❗".join(e.get("quote", [])) for e in items)

        gemma_term_str = sample.get(gse, {}).get("gemma_term", "")
        gemma_uri_str  = sample.get(gse, {}).get("gemma_uri",  "")

        opus_picks, opus_extracted = _load_opus(gse)
        gpt_picks  = split_ids(sample.get(gse, {}).get("gpt_term_id", ""))
        label      = _ensemble_label(claude_picks, opus_picks, gpt_picks,
                                     gemma_picks, xref_idx, opus_extracted)

        rows_out.append({
            "shortName":                 gse,
            "claude_cell_lines":         SEP.join(e.get("cell_line_name","") for e in first_pass),
            "claude_description":        SEP.join(e.get("description","")     for e in first_pass),
            "claude_quote":              join_quotes(first_pass),
            "claude_cell_line_term":     SEP.join(_label(a.get("cell_line_ID","")) for a in anns),
            "claude_cell_line_term_id":  SEP.join(_canonical_id(a.get("cell_line_ID","")) for a in anns),
            "gemma_term":                gemma_term_str,
            "gemma_uri":                 gemma_uri_str,
            "top_embedding_rank_claude": rank_claude if rank_claude is not None else "",
            "top_embedding_rank_gemma":  rank_gemma  if rank_gemma  is not None else "",
            "paper_accessible":          str(paper_accessible).upper() if paper_accessible != "" else "",
            "# guesses by Claude":       len(claude_picks),
            "# TRUE guesses by Claude":  "",   # curator fills
            "# lines used":              len(gemma_picks),  # pre-fill from Gemma count
            "claude_match":              "",
            "claude_specific":           "",
            "claude_sensitive":          "",
            "gemma_correction":          "",
            "information_unavailable":   "",
            "notes":                     "",
            "opus_cell_line_term_id":    SEP.join(sorted(opus_picks)),
            "ensemble_label":            label,
        })
        if (k + 1) % 25 == 0:
            print(f"  {k+1}/{len(needs_review)}", file=sys.stderr)

    fields = list(rows_out[0].keys())
    with open(OUT_TSV, "w") as f:
        w = csv.DictWriter(f, fieldnames=fields, delimiter="\t")
        w.writeheader()
        for r in rows_out: w.writerow(r)
    print(f"\nWrote {OUT_TSV}: {len(rows_out)} rows", file=sys.stderr)


if __name__ == "__main__":
    main()
