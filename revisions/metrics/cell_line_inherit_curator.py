"""Reuse the paper's per-experiment curator verdicts for Sonnet's predictions.

Idea: when Sonnet's predicted ID set is identical to GPT-4o's predicted ID set on
the same GSE, Sonnet inherits whatever the curator decided for that experiment
(specific=TRUE/FALSE, sensitive=TRUE/FALSE). When Sonnet's set is a name/synonym
cross-walk equivalent of GPT-4o's set, the same inheritance applies — the
curator's verdict was about the model's *semantic* choice, not the URI prefix.

This lets us extend the paper's post-curator metric to Sonnet's predictions
without re-doing 500 reviews; we then know how many residual Sonnet predictions
would still need fresh review.

Inputs:
  - revisions/data/results_cl/claude-sonnet-4-6/*.json  (our Sonnet predictions)
  - revisions/data/sample_cell500.tsv                  (published gpt_term_id + gemma_uri)
  - data-raw/cell_line_data/curation.tsv               (curator verdicts on the GPT-4o cases)
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

import csv
import glob
import json
import os
import sys

from cell_line_eval import (
    _canonical_id, CellLineXrefIndex, name_match,
    split_uris_to_ids as gemma_uri_to_ids,
    split_ids as parse_ids,
)

CURATION_TSV = "data-raw/cell_line_data/curation.tsv"
RESULTS_DIR  = "revisions/data/results_cl/claude-sonnet-4-6"
SAMPLE_TSV   = "revisions/data/sample_cell500.tsv"


def sets_equivalent(a: set[str], b: set[str], idx) -> bool:
    if a == b: return True
    return name_match(a, b, idx)


def evaluate_published_gpt4o(sample, curated, idx):
    """Apply the same evaluation pipeline to the published GPT-4o predictions
    on the same 500-GSE sample, for a side-by-side comparison.

    For GPT-4o the curator verdicts in curation.tsv attach directly; we don't
    need the equivalent-to-GPT-4o step. We do still do name-match cross-walk
    for cases the curator didn't review (i.e., not in curation.tsv).
    """
    n = exact = name = curator_perfect = curator_wrong = unreviewed_mismatch = 0
    for gse, row in sample.items():
        gpt   = parse_ids(row.get("gpt_term_id",""))
        truth = gemma_uri_to_ids(row.get("gemma_uri",""))
        n += 1
        if gpt == truth:
            exact += 1
            continue
        if name_match(gpt, truth, idx):
            name += 1
            continue
        if gse in curated:
            c = curated[gse]
            if (c.get("gpt_specific","").upper() == "TRUE"
                and c.get("gpt_sensitive","").upper() == "TRUE"):
                curator_perfect += 1
            else:
                curator_wrong += 1
        else:
            unreviewed_mismatch += 1
    total_correct = exact + name + curator_perfect
    return {
        "n": n, "exact": exact, "name_match": name,
        "curator_perfect": curator_perfect, "curator_wrong": curator_wrong,
        "unreviewed_mismatch": unreviewed_mismatch,
        "total_correct": total_correct,
        "rate": total_correct / n if n else 0.0,
    }


def main():
    idx = CellLineXrefIndex()

    sample = {r["shortName"]: r for r in csv.DictReader(open(SAMPLE_TSV), delimiter="\t")}

    # curation.tsv has one row per GSE that curators reviewed for GPT-4o.
    curated: dict[str, dict] = {}
    with open(CURATION_TSV) as f:
        for r in csv.DictReader(f, delimiter="\t"):
            curated[r["shortName"]] = r

    n = 0
    auto_match = 0          # exact-id match against Gemma -- automatic
    name_match_only = 0     # name/synonym cross-walk match against Gemma (no curator needed)
    inherited_perfect = 0   # Sonnet's set equiv to GPT-4o's set AND curator marked it perfect
    inherited_wrong = 0     # Sonnet's set equiv to GPT-4o's set AND curator marked it not perfect
    needs_review = 0        # genuinely different from GPT-4o and not auto/name match
    no_gpt_verdict = 0      # GPT-4o was an exact match (not in curation.tsv) but Sonnet differs

    rows = []
    for path in sorted(glob.glob(f"{RESULTS_DIR}/*.json")):
        gse = os.path.basename(path)[:-5]
        if gse not in sample:
            continue
        res = json.load(open(path))
        if "error" in res:
            continue
        n += 1
        pred  = {_canonical_id(a.get("cell_line_ID","")) for a in res.get("annotations", [])} - {""}
        truth = gemma_uri_to_ids(sample[gse].get("gemma_uri",""))
        gpt   = parse_ids(sample[gse].get("gpt_term_id",""))
        # Step 1: auto-exact ID match against Gemma
        if pred == truth:
            auto_match += 1
            verdict = "auto_exact"
        # Step 2: name/synonym cross-walk against Gemma
        elif name_match(pred, truth, idx):
            name_match_only += 1
            verdict = "auto_name"
        # Step 3: inherit GPT-4o curator verdict if our prediction is equivalent to GPT-4o's
        elif sets_equivalent(pred, gpt, idx):
            if gse in curated:
                c = curated[gse]
                specific = (c.get("gpt_specific") or "").upper()
                sensitive = (c.get("gpt_sensitive") or "").upper()
                if specific == "TRUE" and sensitive == "TRUE":
                    inherited_perfect += 1
                    verdict = "inherit_perfect"
                else:
                    inherited_wrong += 1
                    verdict = "inherit_wrong"
            else:
                # GPT-4o was an exact match (not in curation.tsv); but our pred differs from gemma.
                # That implies our pred is name-equivalent to GPT-4o's exact match, which means
                # it's also name-equivalent to truth -- handled by Step 2 above. This branch
                # should be rare; record it.
                no_gpt_verdict += 1
                verdict = "gpt_exact_match_we_differ_but_equiv"
        else:
            needs_review += 1
            verdict = "needs_review"

        rows.append({
            "gse": gse, "verdict": verdict,
            "truth":      ",".join(sorted(truth)),
            "claude_pred":",".join(sorted(pred)),
            "gpt4o_pred": ",".join(sorted(gpt)),
        })

    out_path = os.path.join(RESULTS_DIR, "summary_inherit.tsv")
    with open(out_path, "w") as f:
        w = csv.DictWriter(f, fieldnames=["gse","verdict","truth","claude_pred","gpt4o_pred"], delimiter="\t")
        w.writeheader()
        for r in rows: w.writerow(r)

    correct = auto_match + name_match_only + inherited_perfect
    print(f"n = {n}")
    print(f"  auto_exact_id:        {auto_match:>4}   ({auto_match/n:.1%})  -- pred set == truth set")
    print(f"  auto_name_match:      {name_match_only:>4}   ({name_match_only/n:.1%})  -- name/synonym cross-walk to truth")
    print(f"  inherit_perfect:      {inherited_perfect:>4}   ({inherited_perfect/n:.1%})  -- equivalent to GPT-4o AND curator marked it perfect")
    print(f"  inherit_wrong:        {inherited_wrong:>4}   ({inherited_wrong/n:.1%})  -- equivalent to GPT-4o AND curator marked it not perfect")
    print(f"  gpt_exact_we_differ:  {no_gpt_verdict:>4}   ({no_gpt_verdict/n:.1%})  -- (degenerate)")
    print(f"  needs_review:         {needs_review:>4}   ({needs_review/n:.1%})  -- genuinely novel, would need fresh curation")
    print()
    print(f"Reproducible perfect total:  {correct}/{n} = {correct/n:.1%}")
    print(f"Residual review burden:      {needs_review}/{n} = {needs_review/n:.1%}")
    print(f"Wrote {out_path}")

    # And the same metric applied to the published GPT-4o predictions on the same sample.
    gpt_eval = evaluate_published_gpt4o(sample, curated, idx)
    print()
    print(f"--- Same metric applied to published GPT-4o predictions ---")
    print(f"  exact_id:               {gpt_eval['exact']:>4}  ({gpt_eval['exact']/gpt_eval['n']:.1%})")
    print(f"  name_match_cross_walk:  {gpt_eval['name_match']:>4}  ({gpt_eval['name_match']/gpt_eval['n']:.1%})")
    print(f"  curator_perfect:        {gpt_eval['curator_perfect']:>4}  ({gpt_eval['curator_perfect']/gpt_eval['n']:.1%})")
    print(f"  curator_wrong:          {gpt_eval['curator_wrong']:>4}  ({gpt_eval['curator_wrong']/gpt_eval['n']:.1%})")
    print(f"  unreviewed_mismatch:    {gpt_eval['unreviewed_mismatch']:>4}  ({gpt_eval['unreviewed_mismatch']/gpt_eval['n']:.1%})")
    print(f"  TOTAL CORRECT:          {gpt_eval['total_correct']}/{gpt_eval['n']} = {gpt_eval['rate']:.1%}")


if __name__ == "__main__":
    main()
