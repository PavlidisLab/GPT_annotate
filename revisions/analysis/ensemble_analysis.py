"""Three-way ensemble analysis on the cell-line task.

For each GSE in the 500-sample, compute correctness (cross-walk-aware) for
Claude Sonnet 4.6, Claude Opus 4.7, and GPT-4o, then evaluate:

  - All pairwise disagreement matrices
  - Three-way ensemble precision (predict only when all three agree)
  - Two-of-three majority precision (predict the URI set returned by the majority)
  - Coverage of each ensemble (fraction of experiments with a verdict)

Outputs:
  revisions/data/results_cl/ensemble.tsv  -- per-GSE join with correctness flags
  stdout                                  -- summary tables
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
from math import erfc, sqrt

from cell_line_eval import (
    _canonical_id, CellLineXrefIndex, name_match,
    split_uris_to_ids, split_ids,
)

SONNET_DIR = "revisions/data/results_cl/claude-sonnet-4-6"
OPUS_DIR   = "revisions/data/results_cl/claude-opus-4-7"
SAMPLE_TSV = "revisions/data/sample_cell500.tsv"

def correct(pred: set[str], truth: set[str], idx) -> bool:
    if not pred and not truth: return True
    if pred == truth: return True
    return name_match(pred, truth, idx)

def equiv(a: set[str], b: set[str], idx) -> bool:
    if a == b: return True
    if not a and not b: return True
    if not a or not b: return False
    return name_match(a, b, idx)

def load_preds(results_dir: str) -> dict[str, set[str]]:
    out = {}
    for path in sorted(glob.glob(f"{results_dir}/*.json")):
        gse = os.path.basename(path)[:-5]
        res = json.load(open(path))
        if "error" in res: continue
        out[gse] = {_canonical_id(a.get("cell_line_ID","")) for a in res.get("annotations", [])} - {""}
    return out

def mcnemar(b: int, c: int) -> tuple[float, float]:
    if b + c == 0: return 0.0, 1.0
    chi2 = (abs(b - c) - 1) ** 2 / (b + c)
    p = erfc(sqrt(chi2 / 2)) if chi2 > 0 else 1.0
    return chi2, p

def main():
    idx = CellLineXrefIndex()
    sample = {r["shortName"]: r for r in csv.DictReader(open(SAMPLE_TSV), delimiter="\t")}
    sonnet = load_preds(SONNET_DIR)
    opus   = load_preds(OPUS_DIR)

    rows = []
    for gse, srow in sample.items():
        if gse not in sonnet or gse not in opus: continue
        truth  = split_uris_to_ids(srow.get("gemma_uri",""))
        gpt    = split_ids(srow.get("gpt_term_id",""))
        s, o, g = sonnet[gse], opus[gse], gpt
        rows.append({
            "gse": gse,
            "truth":      ",".join(sorted(truth)),
            "sonnet":     ",".join(sorted(s)),
            "opus":       ",".join(sorted(o)),
            "gpt4o":      ",".join(sorted(g)),
            "sonnet_ok":  correct(s, truth, idx),
            "opus_ok":    correct(o, truth, idx),
            "gpt4o_ok":   correct(g, truth, idx),
            "so_agree":   equiv(s, o, idx),
            "sg_agree":   equiv(s, g, idx),
            "og_agree":   equiv(o, g, idx),
            "all_agree":  equiv(s, o, idx) and equiv(o, g, idx),
        })
    n = len(rows)
    print(f"n = {n} (3-way cross-walk-aware, no curator inheritance)\n")

    # ---- marginal accuracy -----------------------------------------------------
    s_ok = sum(r["sonnet_ok"] for r in rows)
    o_ok = sum(r["opus_ok"]   for r in rows)
    g_ok = sum(r["gpt4o_ok"]  for r in rows)
    print("Marginal accuracy (cross-walk):")
    print(f"  Sonnet 4.6 : {s_ok:>4}/{n} = {s_ok/n:.1%}")
    print(f"  Opus 4.7   : {o_ok:>4}/{n} = {o_ok/n:.1%}")
    print(f"  GPT-4o     : {g_ok:>4}/{n} = {g_ok/n:.1%}\n")

    # ---- pairwise McNemar ------------------------------------------------------
    def pair_mcnemar(label_a, ok_a, label_b, ok_b):
        b = sum(1 for r in rows if r[ok_a] and not r[ok_b])
        c = sum(1 for r in rows if not r[ok_a] and r[ok_b])
        chi2, p = mcnemar(b, c)
        print(f"  {label_a} vs {label_b}: only-{label_a}={b}, only-{label_b}={c}, χ²={chi2:.2f}, p={p:.3g}")
    print("Pairwise McNemar (continuity-corrected):")
    pair_mcnemar("Sonnet", "sonnet_ok", "Opus",   "opus_ok")
    pair_mcnemar("Sonnet", "sonnet_ok", "GPT-4o", "gpt4o_ok")
    pair_mcnemar("Opus",   "opus_ok",   "GPT-4o", "gpt4o_ok")
    print()

    # ---- pairwise prediction-set agreement -------------------------------------
    def pair_agree(label_a, key_a, label_b, key_b, agree_key):
        agree = sum(1 for r in rows if r[agree_key])
        outcome = sum(1 for r in rows if (r[key_a] == r[key_b]))
        # Cohen's kappa on binary correct/wrong outcome
        po = outcome / n
        pa = sum(r[key_a] for r in rows) / n
        pb = sum(r[key_b] for r in rows) / n
        pe = pa * pb + (1 - pa) * (1 - pb)
        kappa = (po - pe) / (1 - pe) if pe < 1 else 0
        print(f"  {label_a} vs {label_b}: identical predictions {agree}/{n}={agree/n:.1%}, "
              f"outcome agreement {outcome}/{n}={outcome/n:.1%}, κ={kappa:.2f}")
    print("Pairwise model agreement (cross-walk equivalence + Cohen's κ on outcomes):")
    pair_agree("Sonnet", "sonnet_ok", "Opus",   "opus_ok",   "so_agree")
    pair_agree("Sonnet", "sonnet_ok", "GPT-4o", "gpt4o_ok",  "sg_agree")
    pair_agree("Opus",   "opus_ok",   "GPT-4o", "gpt4o_ok",  "og_agree")
    print()

    # ---- ensemble: all-three-agree --------------------------------------------
    all_agree_rows = [r for r in rows if r["all_agree"]]
    all_agree_correct = sum(1 for r in all_agree_rows if r["sonnet_ok"])
    print("Ensemble A: predict only when ALL THREE models agree.")
    print(f"  Coverage : {len(all_agree_rows):>4}/{n} = {len(all_agree_rows)/n:.1%}")
    if all_agree_rows:
        print(f"  Precision: {all_agree_correct}/{len(all_agree_rows)} = {all_agree_correct/len(all_agree_rows):.1%}")
    print()

    # ---- ensemble: two-of-three majority ---------------------------------------
    # For each GSE, find the URI set that ≥2 models picked (under cross-walk equivalence).
    # If no 2 models agree, abstain.
    maj_covered = 0
    maj_correct = 0
    for r in rows:
        # consider 3 candidate pairings
        pairs = [(r["sonnet"], r["opus"], r["so_agree"], "sonnet_ok"),
                 (r["sonnet"], r["gpt4o"], r["sg_agree"], "sonnet_ok"),
                 (r["opus"],   r["gpt4o"], r["og_agree"], "opus_ok")]
        agreeing = [p for p in pairs if p[2]]
        if not agreeing: continue
        maj_covered += 1
        # If any majority-agreed pair is correct, the majority verdict is correct
        if any(r[p[3]] for p in agreeing):
            maj_correct += 1
    print("Ensemble B: predict the URI set that >=2 models pick (cross-walk equivalence).")
    print(f"  Coverage : {maj_covered:>4}/{n} = {maj_covered/n:.1%}")
    if maj_covered:
        print(f"  Precision: {maj_correct}/{maj_covered} = {maj_correct/maj_covered:.1%}")
    print()

    # ---- write join -----------------------------------------------------------
    out_path = "revisions/data/results_cl/ensemble.tsv"
    fields = list(rows[0].keys())
    with open(out_path, "w") as f:
        w = csv.DictWriter(f, fieldnames=fields, delimiter="\t")
        w.writeheader()
        for r in rows: w.writerow(r)
    print(f"Wrote {out_path}")

if __name__ == "__main__":
    main()
