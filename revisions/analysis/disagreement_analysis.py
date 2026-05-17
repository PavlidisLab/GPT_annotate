"""Per-GSE disagreement matrix: where do Sonnet and GPT-4o miss, separately and together?

For each GSE in the 500-sample cell-line set, classify both models' predictions
against the Gemma truth (under cross-walk equivalence), then cross-tabulate.

Output: 2x2 table of (Sonnet correct? x GPT-4o correct?) and the
proportion of errors that are "model-specific" versus shared.
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
import json
import glob
import os

from cell_line_eval import (
    _canonical_id, CellLineXrefIndex, name_match,
    split_uris_to_ids, split_ids,
)

RESULTS_DIR  = "revisions/data/results_cl/claude-sonnet-4-6"
SAMPLE_TSV   = "revisions/data/sample_cell500.tsv"

def correct(pred: set[str], truth: set[str], idx) -> bool:
    if not pred and not truth: return True
    if pred == truth: return True
    return name_match(pred, truth, idx)

def main():
    idx = CellLineXrefIndex()
    sample = {r["shortName"]: r for r in csv.DictReader(open(SAMPLE_TSV), delimiter="\t")}

    rows = []
    for path in sorted(glob.glob(f"{RESULTS_DIR}/*.json")):
        gse = os.path.basename(path)[:-5]
        if gse not in sample: continue
        res = json.load(open(path))
        if "error" in res: continue
        sonnet = {_canonical_id(a.get("cell_line_ID","")) for a in res.get("annotations", [])} - {""}
        truth  = split_uris_to_ids(sample[gse].get("gemma_uri",""))
        gpt    = split_ids(sample[gse].get("gpt_term_id",""))
        s_ok = correct(sonnet, truth, idx)
        g_ok = correct(gpt,    truth, idx)
        rows.append({"gse": gse, "sonnet_ok": s_ok, "gpt4o_ok": g_ok,
                     "sonnet_pred": ",".join(sorted(sonnet)),
                     "gpt4o_pred":  ",".join(sorted(gpt)),
                     "truth":       ",".join(sorted(truth))})

    n = len(rows)
    both_ok   = sum(1 for r in rows if r["sonnet_ok"] and r["gpt4o_ok"])
    sonnet_only = sum(1 for r in rows if r["sonnet_ok"] and not r["gpt4o_ok"])
    gpt_only    = sum(1 for r in rows if not r["sonnet_ok"] and r["gpt4o_ok"])
    both_wrong  = sum(1 for r in rows if not r["sonnet_ok"] and not r["gpt4o_ok"])

    print(f"n = {n} (cross-walk-aware scoring, no curator inheritance)")
    print()
    print(f"  Both correct:        {both_ok:>4}  ({both_ok/n:.1%})")
    print(f"  Only Sonnet correct: {sonnet_only:>4}  ({sonnet_only/n:.1%})")
    print(f"  Only GPT-4o correct: {gpt_only:>4}  ({gpt_only/n:.1%})")
    print(f"  Both wrong:          {both_wrong:>4}  ({both_wrong/n:.1%})")
    print()

    # McNemar on the disagreement cells
    b, c = sonnet_only, gpt_only  # discordant pairs
    if b + c > 0:
        # Continuity-corrected McNemar
        chi2 = (abs(b - c) - 1) ** 2 / (b + c) if (b + c) >= 1 else 0.0
        from math import erfc, sqrt
        # 1-df chi-square upper tail ≈ erfc(sqrt(chi2/2))
        p = erfc(sqrt(chi2 / 2)) if chi2 > 0 else 1.0
        print(f"  McNemar discordant: {b} (Sonnet-only) vs {c} (GPT-4o-only)")
        print(f"  chi2 = {chi2:.3f}, p ≈ {p:.3f}")
        print()

    n_errors_either = sonnet_only + gpt_only + both_wrong
    if n_errors_either:
        shared_share = both_wrong / n_errors_either
        print(f"  Of the {n_errors_either} GSEs where at least one model errs,")
        print(f"  {both_wrong} ({shared_share:.1%}) have both models wrong;")
        print(f"  {sonnet_only + gpt_only} ({(sonnet_only+gpt_only)/n_errors_either:.1%}) are model-specific errors.")

    # Decompose the both-wrong cases: do the two models agree on the (wrong) answer?
    both_wrong_rows = [r for r in rows if not r["sonnet_ok"] and not r["gpt4o_ok"]]
    agree_wrong = 0
    for r in both_wrong_rows:
        s = set(r["sonnet_pred"].split(",")) - {""}
        g = set(r["gpt4o_pred" ].split(",")) - {""}
        if s == g:
            agree_wrong += 1
        elif name_match(s, g, idx):
            agree_wrong += 1
    disagree_wrong = len(both_wrong_rows) - agree_wrong
    print()
    print(f"  Decomposing the {len(both_wrong_rows)} 'both wrong' cases:")
    print(f"    Models converge on same (wrong) prediction:  {agree_wrong}  ({agree_wrong/max(len(both_wrong_rows),1):.1%})")
    print(f"    Models pick different wrong predictions:     {disagree_wrong}  ({disagree_wrong/max(len(both_wrong_rows),1):.1%})")

    out_path = f"{RESULTS_DIR}/disagreement.tsv"
    with open(out_path, "w") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()), delimiter="\t")
        w.writeheader()
        for r in rows: w.writerow(r)
    print()
    print(f"Wrote {out_path}")

if __name__ == "__main__":
    main()
