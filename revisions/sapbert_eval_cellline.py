"""Score SapBERT cell-line predictions with both exact-ID and cross-walk match.

Reads the per-GSE predictions written by ``sapbert_baseline.py --task cell_line``
and emits the same two-rule accuracy summary used by ``cell_line_eval.py`` for
the LLM runs, so the SapBERT and LLM numbers are directly comparable.
"""
import csv
import glob
import json
import os
import sys

sys.path.insert(0, os.path.dirname(__file__))

from cell_line_eval import (
    _canonical_id, CellLineXrefIndex, name_match,
    split_uris_to_ids, split_ids,
)

RESULTS_DIR = "revisions/data/results/sapbert_cell_line"
SAMPLE_TSV  = "revisions/data/sample_cell500.tsv"


def main():
    idx = CellLineXrefIndex()
    sample = {r["shortName"]: r for r in csv.DictReader(open(SAMPLE_TSV), delimiter="\t")}
    n = ex = nm = 0
    ex_g = nm_g = 0
    out_rows = []
    for path in sorted(glob.glob(f"{RESULTS_DIR}/per_gse/*.json")):
        gse = os.path.basename(path)[:-5]
        if gse not in sample: continue
        r = json.load(open(path))
        pred  = {_canonical_id(p) for p in r.get("pred", [])} - {""}
        truth = split_uris_to_ids(sample[gse].get("gemma_uri",""))
        gpt   = split_ids(sample[gse].get("gpt_term_id",""))
        ex_match = pred == truth
        nm_match = name_match(pred, truth, idx)
        ex_g_match = gpt == truth
        nm_g_match = name_match(gpt,  truth, idx)
        n += 1
        ex += int(ex_match); nm += int(nm_match)
        ex_g += int(ex_g_match); nm_g += int(nm_g_match)
        out_rows.append({
            "gse": gse,
            "truth":      ",".join(sorted(truth)),
            "sapbert_pred":",".join(sorted(pred)),
            "gpt4o_pred": ",".join(sorted(gpt)),
            "sapbert_exact_id":   ex_match,
            "sapbert_name_match": nm_match,
            "gpt4o_exact_id":     ex_g_match,
            "gpt4o_name_match":   nm_g_match,
        })

    print(f"n = {n}")
    print(f"SapBERT exact-id:  {ex}/{n} = {ex/n:.1%}")
    print(f"SapBERT name-match cross-walk: {nm}/{n} = {nm/n:.1%}")
    print(f"GPT-4o  exact-id:  {ex_g}/{n} = {ex_g/n:.1%}")
    print(f"GPT-4o  name-match cross-walk: {nm_g}/{n} = {nm_g/n:.1%}")

    out_path = f"{RESULTS_DIR}/summary_eval.tsv"
    with open(out_path, "w") as f:
        w = csv.DictWriter(f, fieldnames=list(out_rows[0].keys()), delimiter="\t")
        w.writeheader()
        for r in out_rows: w.writerow(r)
    print(f"Wrote {out_path}")


if __name__ == "__main__":
    main()
