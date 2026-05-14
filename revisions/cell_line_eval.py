"""Evaluation regimes for the cell-line task.

Three metrics, all deterministic and reproducible:

1. ``exact_id``: predicted-ID set equals ground-truth-ID set. The strictest
   measure, equivalent to the paper's pre-review automated comparison.

2. ``name_match``: predicted and ground-truth IDs are equivalent under a
   case-folded name / synonym / alt-label cross-walk, plus the OBO
   cross-reference graph (``xref``, ``alt_id``, and ``property_value: seeAlso``).
   This mirrors the equivalence-expansion implemented by
   ``analysis/cell_lines/07.post_curator_evaluation.R::get_related_terms``.

3. ``inherit_verdict`` (in cell_line_inherit_curator.py): for cases where our
   prediction is equivalent to the published GPT-4o prediction, we can reuse
   the curator's verdict on the GPT-4o prediction.

The cross-walk equivalence class for an id is computed lazily and BFS-style,
using reverse indices built once from ``revisions/data/cell_line_terms.json``.
"""
from __future__ import annotations

import argparse
import csv
import functools
import glob
import json
import os
import sys
from collections import defaultdict


TERMS_PATH = "revisions/data/cell_line_terms.json"


_SENTINEL_NON_IDS = {"UNKNOWN", "NONE", "N/A", "NA", "NULL"}


def _canonical_id(s: str) -> str:
    """Canonicalise an ontology identifier to ``PREFIX:LOCAL`` form.

    Returns ``""`` for empty input or for sentinel strings that some LLMs
    emit when they cannot find a matching ontology term (e.g. Opus 4.7
    occasionally returns ``cell_line_ID = "UNKNOWN"`` instead of an empty
    annotations array). Empty returns are filtered upstream so these
    sentinels never enter a prediction set."""
    if not s: return ""
    s = s.strip()
    if s.upper() in _SENTINEL_NON_IDS:
        return ""
    if s.startswith("http"):
        s = s.rsplit("/", 1)[-1]
    local = s.split(":", 1)[1] if ":" in s else s
    if "_" in local:
        prefix, _, rest = local.partition("_")
        return f"{prefix.upper()}:{rest}"
    if ":" in s:
        prefix, _, rest = s.partition(":")
        return f"{prefix.upper()}:{rest}"
    return s


class CellLineXrefIndex:
    """Lazy equivalence-class expansion for cell-line ontology ids."""

    def __init__(self, terms_path: str = TERMS_PATH):
        with open(terms_path) as f:
            self.terms: dict[str, dict] = json.load(f)
        # Reverse index: name/syn/alt_label (lower-case) -> set of ids
        self._name_to_ids: dict[str, set[str]] = defaultdict(set)
        # Reverse index: id -> set of ids that mention it in xref/seeAlso/alt_id
        self._reverse_xref: dict[str, set[str]] = defaultdict(set)
        for cid, term in self.terms.items():
            for nm in (term.get("names", []) +
                       term.get("synonyms", []) +
                       term.get("alt_labels", [])):
                if nm: self._name_to_ids[nm].add(cid)
            for other in (term.get("xrefs", []) +
                          term.get("see_also", []) +
                          term.get("alt_ids", [])):
                if other:
                    self._reverse_xref[other].add(cid)
        # Cache for expanded equivalence classes
        self._expand_cache: dict[str, frozenset[str]] = {}

    def expand(self, cid: str) -> frozenset[str]:
        """Return the set of ids equivalent to ``cid``: closure over shared
        names/synonyms/alt-labels AND over xref/seeAlso/alt_id edges (both
        directions). Result includes ``cid`` itself."""
        if cid in self._expand_cache:
            return self._expand_cache[cid]
        seen = {cid}
        frontier = [cid]
        while frontier:
            x = frontier.pop()
            term = self.terms.get(x, {})
            # name-sharing neighbours
            for nm in (term.get("names", []) +
                       term.get("synonyms", []) +
                       term.get("alt_labels", [])):
                for y in self._name_to_ids.get(nm, ()):
                    if y not in seen:
                        seen.add(y); frontier.append(y)
            # outgoing xref / see_also / alt_id
            for y in (term.get("xrefs", []) +
                      term.get("see_also", []) +
                      term.get("alt_ids", [])):
                if y not in seen:
                    seen.add(y); frontier.append(y)
            # incoming xref / see_also / alt_id
            for y in self._reverse_xref.get(x, ()):
                if y not in seen:
                    seen.add(y); frontier.append(y)
        out = frozenset(seen)
        # Cache for every member of the class (they all have the same expansion)
        for y in out:
            self._expand_cache[y] = out
        return out


def _split_multi(s: str) -> list[str]:
    """Split a list-cell on either ``|||`` (current sample format) or ``,``
    (legacy sample format). Both separators may coexist within a single file
    if columns were exported at different times."""
    if not s: return []
    # |||  is unambiguous (does not occur in any ontology term or URI).
    parts = s.split("|||") if "|||" in s else s.split(",")
    return [p.strip() for p in parts if p.strip()]


def split_uris_to_ids(uri_str: str) -> set[str]:
    if not uri_str: return set()
    out = set()
    for u in _split_multi(uri_str):
        u = (u.replace("http://purl.obolibrary.org/obo/CLO_", "CLO:")
              .replace("http://purl.obolibrary.org/obo/CL_",  "CL:")
              .replace("http://www.ebi.ac.uk/efo/EFO_",       "EFO:"))
        out.add(_canonical_id(u))
    return out


def split_ids(s: str) -> set[str]:
    if not s: return set()
    return {_canonical_id(x) for x in _split_multi(s)}


def name_match(pred_ids: set[str], truth_ids: set[str], idx: CellLineXrefIndex) -> bool:
    """Bidirectional set-equivalence via the lazy cross-walk."""
    if not pred_ids and not truth_ids: return True
    if not pred_ids or not truth_ids: return False
    p_exp = [idx.expand(p) for p in pred_ids]
    t_exp = [idx.expand(t) for t in truth_ids]
    sensitive = all(any(p & t for p in p_exp) for t in t_exp)
    specific  = all(any(p & t for t in t_exp) for p in p_exp)
    return sensitive and specific


def main(results_dir: str, sample_path: str):
    idx = CellLineXrefIndex()
    sample = {r["shortName"]: r for r in csv.DictReader(open(sample_path), delimiter="\t")}

    n_exact = n_name = n = 0
    n_exact_gpt = n_name_gpt = 0
    rows = []
    for path in sorted(glob.glob(f"{results_dir}/*.json")):
        gse = os.path.basename(path)[:-5]
        if gse not in sample: continue
        res = json.load(open(path))
        if "error" in res: continue
        pred  = {_canonical_id(a.get("cell_line_ID","")) for a in res.get("annotations", [])} - {""}
        truth = split_uris_to_ids(sample[gse].get("gemma_uri",""))
        gpt   = split_ids(sample[gse].get("gpt_term_id",""))
        ex = pred == truth
        nm = name_match(pred, truth, idx)
        ex_g = gpt == truth
        nm_g = name_match(gpt, truth, idx)
        n += 1
        n_exact     += int(ex);    n_name     += int(nm)
        n_exact_gpt += int(ex_g);  n_name_gpt += int(nm_g)
        rows.append({"gse":gse,
                     "truth":      ",".join(sorted(truth)),
                     "claude_pred":",".join(sorted(pred)),
                     "gpt4o_pred": ",".join(sorted(gpt)),
                     "claude_exact_id": ex,  "claude_name_match": nm,
                     "gpt4o_exact_id":  ex_g,"gpt4o_name_match":  nm_g})

    out_path = f"{results_dir}/summary_eval.tsv"
    fields = list(rows[0].keys())
    with open(out_path, "w") as f:
        w = csv.DictWriter(f, fieldnames=fields, delimiter="\t")
        w.writeheader()
        for r in rows: w.writerow(r)
    print(f"n = {n}")
    print(f"Claude exact-id match:  {n_exact}/{n} = {n_exact/n:.1%}")
    print(f"Claude name match:      {n_name}/{n}  = {n_name/n:.1%}")
    print(f"GPT-4o exact-id match:  {n_exact_gpt}/{n} = {n_exact_gpt/n:.1%}")
    print(f"GPT-4o name match:      {n_name_gpt}/{n}  = {n_name_gpt/n:.1%}")
    print(f"Wrote {out_path}")


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--results", required=True)
    ap.add_argument("--sample",  required=True)
    args = ap.parse_args()
    main(args.results, args.sample)
