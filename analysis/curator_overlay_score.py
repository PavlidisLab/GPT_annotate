#!/usr/bin/env python3
"""Re-score cell-line predictions with curator verdicts overlaid.

Curator verdicts file: analysis/curator_verdicts_2026-05-21.json
Per-system predictions: ~/Data/metamuse-bench/scored/full500_per_gse.tsv

For each (GSE, predicted-URI) pair the curator may have voted
'correct', 'wrong', or 'unsure'. We define an expanded scoring rule:

    expanded_correct(P, GSE) :=
        (any p in P is in curator_correct[GSE]) OR
        (any p in P is cross-walk-equivalent to gemma_uri[GSE]
            AND p not in curator_wrong[GSE])

We report the original cross-walk numbers and the curator-overlay
numbers side by side, both on the full 500-GSE sample and restricted
to the 58 GSEs the curator reviewed. Disagreement stats compare the
four systems on the curator-reviewed subset.
"""
from __future__ import annotations
import csv, json, sys
from collections import Counter, defaultdict
from pathlib import Path

REPO = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO / "revisions"))
from metrics.cell_line_eval import CellLineXrefIndex

PER_GSE_TSV = Path("/Users/pzoot/Data/metamuse-bench/scored/full500_per_gse.tsv")
VERDICTS    = REPO / "analysis" / "curator_verdicts_2026-05-21.json"
XREFS       = REPO / "revisions" / "data" / "cell_line_terms.json"
OUT_DIR     = REPO / "analysis"

SYSTEMS = [
    ("gpt4o_pred",  "Stage-1+2 GPT-4o (original)"),
    ("sonnet_pred", "Stage-1+2 Sonnet 4.6"),
    ("opus_pred",   "Stage-1+2 Opus 4.7"),
    ("mm_pred",     "MetaMuse-GPT-4o"),
]


def split_preds(s: str) -> set[str]:
    out: set[str] = set()
    for x in (s or "").replace("|||", ",").split(","):
        x = x.strip()
        if x:
            out.add(x)
    return out


def load_verdicts(path: Path):
    raw = json.loads(path.read_text())["verdicts"]
    correct: dict[str, set[str]] = defaultdict(set)
    wrong:   dict[str, set[str]] = defaultdict(set)
    unsure:  dict[str, set[str]] = defaultdict(set)
    for key, val in raw.items():
        gse, uri = key.split("::", 1)
        v = val.get("verdict", "")
        if v == "correct":
            correct[gse].add(uri)
        elif v == "wrong":
            wrong[gse].add(uri)
        elif v == "unsure":
            unsure[gse].add(uri)
    return correct, wrong, unsure


def original_correct(pred: set[str], truth: str, xref: CellLineXrefIndex) -> bool:
    return any(xref.expand(p) & xref.expand(truth) for p in pred)


def overlay_correct(
    pred: set[str], truth: str, xref: CellLineXrefIndex,
    curator_correct: set[str], curator_wrong: set[str],
) -> bool:
    if any(p in curator_correct for p in pred):
        return True
    t_exp = xref.expand(truth)
    for p in pred:
        if p in curator_wrong:
            continue
        if xref.expand(p) & t_exp:
            return True
    return False


def score_table(rows, xref, curator_correct, curator_wrong, gse_filter=None):
    """Return per-system {n, covered, original_correct, overlay_correct}."""
    out = {sk: {"n": 0, "covered": 0, "orig": 0, "overlay": 0}
           for sk, _ in SYSTEMS}
    for r in rows:
        gse = r["gse"]
        if gse_filter is not None and gse not in gse_filter:
            continue
        truth = r["truth"].strip()
        if not truth:
            continue
        for sk, _ in SYSTEMS:
            stats = out[sk]
            stats["n"] += 1
            pred = split_preds(r.get(sk, ""))
            if not pred:
                continue
            stats["covered"] += 1
            if original_correct(pred, truth, xref):
                stats["orig"] += 1
            if overlay_correct(pred, truth, xref,
                               curator_correct.get(gse, set()),
                               curator_wrong.get(gse, set())):
                stats["overlay"] += 1
    return out


def fmt_pct(num, den):
    if den == 0:
        return "  —"
    return f"{100*num/den:5.1f}%"


def write_summary(out_path: Path, full, subset, n_subset: int):
    with out_path.open("w") as f:
        f.write("subset\tsystem\tn\tcovered\torig_correct\toverlay_correct\t")
        f.write("orig_acc\toverlay_acc\torig_prec_cov\toverlay_prec_cov\tdelta_acc_pp\n")
        for label, table, denom_note in [("full500", full, "n=498"),
                                          (f"reviewed{n_subset}", subset, f"n={n_subset}")]:
            for sk, name in SYSTEMS:
                s = table[sk]
                if s["n"] == 0:
                    continue
                orig = s["orig"] / s["n"]
                ovl  = s["overlay"] / s["n"]
                opc  = s["orig"]    / s["covered"] if s["covered"] else 0.0
                ovpc = s["overlay"] / s["covered"] if s["covered"] else 0.0
                f.write(f"{label}\t{name}\t{s['n']}\t{s['covered']}\t"
                        f"{s['orig']}\t{s['overlay']}\t"
                        f"{orig:.4f}\t{ovl:.4f}\t{opc:.4f}\t{ovpc:.4f}\t"
                        f"{100*(ovl-orig):+.2f}\n")


def print_side_by_side(label: str, table, n_label: str):
    print(f"\n=== {label} ({n_label}) ===")
    print(f"{'system':<32} {'cov':>8}  {'orig acc':>10} {'overlay acc':>12}  "
          f"{'orig prec|cov':>14} {'overlay prec|cov':>17}  {'Δ acc':>7}")
    for sk, name in SYSTEMS:
        s = table[sk]
        if s["n"] == 0:
            continue
        cov  = f"{s['covered']}/{s['n']}"
        oacc = fmt_pct(s["orig"],    s["n"])
        vacc = fmt_pct(s["overlay"], s["n"])
        opc  = fmt_pct(s["orig"],    s["covered"])
        ovpc = fmt_pct(s["overlay"], s["covered"])
        delta = 100*(s["overlay"]/s["n"] - s["orig"]/s["n"]) if s["n"] else 0
        print(f"{name:<32} {cov:>8}  {oacc:>10} {vacc:>12}  "
              f"{opc:>14} {ovpc:>17}  {delta:+6.1f}")


def disagreement_stats(rows, xref, curator_correct, curator_wrong, gse_filter):
    """On the curator-reviewed subset, which system is most often the
    'lone right' or 'lone wrong' under the overlay rule?"""
    per_gse = []
    for r in rows:
        gse = r["gse"]
        if gse not in gse_filter:
            continue
        truth = r["truth"].strip()
        if not truth:
            continue
        verdicts = {}
        for sk, _ in SYSTEMS:
            pred = split_preds(r.get(sk, ""))
            if not pred:
                verdicts[sk] = None  # abstain
            else:
                verdicts[sk] = overlay_correct(pred, truth, xref,
                                                curator_correct.get(gse, set()),
                                                curator_wrong.get(gse, set()))
        per_gse.append((gse, verdicts))

    # Tally
    pair_disagree = Counter()
    all_agree = 0
    all_correct = 0
    all_wrong = 0
    lone_correct = Counter()
    lone_wrong   = Counter()
    for gse, vs in per_gse:
        # only consider GSEs where every system gave a verdict (True/False)
        if any(v is None for v in vs.values()):
            continue
        vals = list(vs.values())
        if all(vals):
            all_correct += 1; all_agree += 1
            continue
        if not any(vals):
            all_wrong += 1; all_agree += 1
            continue
        # mixed
        n_correct = sum(vals)
        if n_correct == 1:
            sk_correct = [k for k,v in vs.items() if v][0]
            lone_correct[sk_correct] += 1
        if n_correct == len(vals) - 1:
            sk_wrong = [k for k,v in vs.items() if not v][0]
            lone_wrong[sk_wrong] += 1
        for i, sk1 in enumerate(vs):
            for sk2 in list(vs)[i+1:]:
                if vs[sk1] != vs[sk2]:
                    pair_disagree[(sk1, sk2)] += 1

    print(f"\n=== Disagreement on curator-reviewed subset"
          f" (GSEs where all 4 systems covered) ===")
    fully_covered = sum(1 for g,vs in per_gse if not any(v is None for v in vs.values()))
    print(f"  fully-covered GSEs: {fully_covered}")
    print(f"  all 4 correct      : {all_correct}")
    print(f"  all 4 wrong        : {all_wrong}")
    print(f"  mixed              : {fully_covered - all_agree}")
    print(f"\n  'lone correct' counts (system was the only one right):")
    for sk, n in lone_correct.most_common():
        print(f"    {dict(SYSTEMS)[sk]:<32} {n}")
    print(f"\n  'lone wrong' counts (system was the only one wrong):")
    for sk, n in lone_wrong.most_common():
        print(f"    {dict(SYSTEMS)[sk]:<32} {n}")
    print(f"\n  pairwise disagreement counts (overlay verdict differs):")
    sk_to_name = dict(SYSTEMS)
    for (a, b), n in pair_disagree.most_common():
        print(f"    {sk_to_name[a]:<32} ~ {sk_to_name[b]:<32} {n}")


def main():
    xref = CellLineXrefIndex(str(XREFS))
    correct, wrong, unsure = load_verdicts(VERDICTS)
    reviewed = set(correct) | set(wrong) | set(unsure)
    print(f"verdicts: {sum(len(v) for v in correct.values())} correct, "
          f"{sum(len(v) for v in wrong.values())} wrong, "
          f"{sum(len(v) for v in unsure.values())} unsure, "
          f"across {len(reviewed)} GSEs")

    rows = list(csv.DictReader(PER_GSE_TSV.open(), delimiter="\t"))
    print(f"per-GSE rows loaded: {len(rows)}")

    full   = score_table(rows, xref, correct, wrong, gse_filter=None)
    subset = score_table(rows, xref, correct, wrong, gse_filter=reviewed)
    n_sub  = sum(1 for r in rows if r["gse"] in reviewed and r["truth"].strip())

    print_side_by_side("Full 500 (original + curator overlay)", full,  "n=498")
    print_side_by_side("Curator-reviewed subset",               subset, f"n={n_sub}")
    disagreement_stats(rows, xref, correct, wrong, reviewed)

    out_tsv = OUT_DIR / "curator_overlay_summary.tsv"
    write_summary(out_tsv, full, subset, n_sub)
    print(f"\nwrote {out_tsv}")


if __name__ == "__main__":
    main()
