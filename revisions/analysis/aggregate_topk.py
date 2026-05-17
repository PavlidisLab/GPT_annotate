"""Aggregate cross-K name-match accuracy from each revisions/data/results_cl/claude-sonnet-4-6_topk{K} dir.

Re-uses cell_line_eval.main()'s output (summary_eval.tsv must already exist for each K).
Produces revisions/data/results_cl/topk_sensitivity_summary.tsv with one row per K.
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
import os

BASE = "revisions/data/results_cl"
KS = [10, 25, 50, 100, 200]

out_rows = []
for k in KS:
    path = os.path.join(BASE, f"claude-sonnet-4-6_topk{k}", "summary_eval.tsv")
    if not os.path.exists(path):
        print(f"missing: {path}")
        continue
    n = ex_c = nm_c = ex_g = nm_g = 0
    for r in csv.DictReader(open(path), delimiter="\t"):
        n += 1
        ex_c += (r["claude_exact_id"]   == "True")
        nm_c += (r["claude_name_match"] == "True")
        ex_g += (r["gpt4o_exact_id"]    == "True")
        nm_g += (r["gpt4o_name_match"]  == "True")
    out_rows.append({"k": k, "n": n,
                     "claude_exact": ex_c, "claude_exact_pct": f"{ex_c/n:.1%}",
                     "claude_name":  nm_c, "claude_name_pct":  f"{nm_c/n:.1%}",
                     "gpt4o_exact":  ex_g, "gpt4o_exact_pct":  f"{ex_g/n:.1%}",
                     "gpt4o_name":   nm_g, "gpt4o_name_pct":   f"{nm_g/n:.1%}"})

out_path = os.path.join(BASE, "topk_sensitivity_summary.tsv")
with open(out_path, "w") as f:
    w = csv.DictWriter(f, fieldnames=list(out_rows[0].keys()), delimiter="\t")
    w.writeheader()
    for r in out_rows: w.writerow(r)

print(f"Wrote {out_path}")
print()
print(f"{'K':>4}  {'Claude exact':>14}  {'Claude name':>14}  {'GPT-4o exact':>14}  {'GPT-4o name':>14}")
for r in out_rows:
    print(f"{r['k']:>4}  "
          f"{r['claude_exact']:>5}/{r['n']:<3} ({r['claude_exact_pct']:>5})  "
          f"{r['claude_name']:>5}/{r['n']:<3} ({r['claude_name_pct']:>5})  "
          f"{r['gpt4o_exact']:>5}/{r['n']:<3} ({r['gpt4o_exact_pct']:>5})  "
          f"{r['gpt4o_name']:>5}/{r['n']:<3} ({r['gpt4o_name_pct']:>5})")
