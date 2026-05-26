#!/usr/bin/env python3
"""Compute curator-verdict overlay deltas on top of fig6's published
baselines.

Strategy:

* The baseline (k_exact_orig, k_xw_orig, n) for each system is pinned to the
  numbers already shown in fig6_cell_line_baselines (from make_figures.py).
  This way the unchanged "Exact-ID" and "+ cross-walk name match" bars
  preserve the published values exactly.

* Overlay deltas are computed only over the 58 curator-reviewed GSEs:
    delta_exact = (n_exact_correct_under_overlay) - (n_exact_correct_under_original)
    delta_xw    = (n_xw_correct_under_overlay)    - (n_xw_correct_under_original)
  where both sides are computed under my own scoring (CellLineXrefIndex);
  the difference is what the curator verdicts add or remove.

* Output: analysis/fig6_overlay_scores.tsv (columns: system_label, n,
  k_exact_orig, k_exact_overlay, k_xw_orig, k_xw_overlay).
"""
from __future__ import annotations
import csv, json, sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO / "revisions"))
from metrics.cell_line_eval import CellLineXrefIndex, _canonical_id  # noqa: E402

REV       = REPO / "revisions"
VERDICTS  = REPO / "analysis" / "curator_verdicts_2026-05-21.json"
TRUTH_TSV = REV  / "data"     / "sample_cell500.tsv"
XREFS     = REV  / "data"     / "cell_line_terms.json"
OUT_TSV   = REPO / "analysis" / "fig6_overlay_scores.tsv"


# Pinned baselines: match the constants in revisions/make_figures.py fig6.
PINNED = {
    "SapBERT (neural)":           (498, 61,  117),
    "Claude Sonnet 4.6":          (497, 81,  260),
    "Claude Sonnet 4.6 + hybrid": (497, 213, 243),
    "Claude Opus 4.7":            (498, 198, 256),
    "Claude Opus 4.7 + hybrid":   (498, 226, 257),
    "GPT-4o (original)":          (491, 95,  262),
    "GPT-4o + hybrid":            (491, 212, 231),
}


def canon(x: str) -> str:
    return _canonical_id(x) if x else ""


def load_truth() -> dict[str, str]:
    out = {}
    with TRUTH_TSV.open() as f:
        for r in csv.DictReader(f, delimiter="\t"):
            u = (r.get("gemma_uri") or "").strip()
            if u:
                out[r["shortName"]] = canon(u)
    return out


def load_verdicts():
    raw = json.loads(VERDICTS.read_text())["verdicts"]
    correct, wrong = {}, {}
    for key, val in raw.items():
        gse, uri = key.split("::", 1)
        cid = canon(uri)
        v = val.get("verdict", "")
        if v == "correct":
            correct.setdefault(gse, set()).add(cid)
        elif v == "wrong":
            wrong.setdefault(gse, set()).add(cid)
    return correct, wrong


def preds_from_llm_jsons(dir_path: Path) -> dict[str, set[str]]:
    out = {}
    for fn in sorted(dir_path.glob("GSE*.json")):
        d = json.loads(fn.read_text())
        ids = set(canon(a.get("cell_line_ID","")) for a in d.get("annotations",[]) or [])
        out[d.get("gse") or fn.stem] = {x for x in ids if x}
    return out


def preds_from_sapbert(dir_path: Path) -> dict[str, set[str]]:
    out = {}
    for fn in sorted(dir_path.glob("GSE*.json")):
        d = json.loads(fn.read_text())
        ids = set(canon(x) for x in (d.get("pred") or []))
        out[d.get("gse") or fn.stem] = {x for x in ids if x}
    return out


def preds_from_inherit(path: Path, col: str) -> dict[str, set[str]]:
    out = {}
    with path.open() as f:
        for r in csv.DictReader(f, delimiter="\t"):
            ids = set()
            for x in (r.get(col,"") or "").replace("|||",",").split(","):
                x = x.strip()
                if x: ids.add(canon(x))
            out[r["gse"]] = {x for x in ids if x}
    return out


def deltas(preds, truth, xref, cur_correct, cur_wrong):
    """Compute (delta_exact, delta_xw) over the curator-reviewed GSEs."""
    reviewed = set(cur_correct) | set(cur_wrong)
    de = dxw = 0
    for gse in reviewed:
        t = truth.get(gse)
        if not t: continue
        pset = preds.get(gse, set())
        if not pset: continue
        t_exp = xref.expand(t)
        cc = cur_correct.get(gse, set())
        cw = cur_wrong.get(gse, set())
        # ORIGINAL scoring (cross-walk vs gemma_uri only)
        orig_exact = (t in pset)
        orig_xw    = any(xref.expand(p) & t_exp for p in pset)
        # OVERLAY scoring
        ov_exact = (
            bool(pset & cc) or
            (orig_exact and not (pset & cw and not (pset & cc)))
        )
        ov_xw = bool(pset & cc)
        if not ov_xw:
            for p in pset:
                if p in cw: continue
                if xref.expand(p) & t_exp:
                    ov_xw = True; break
        de  += int(ov_exact) - int(orig_exact)
        dxw += int(ov_xw)    - int(orig_xw)
    return de, dxw


def main():
    truth = load_truth()
    cur_correct, cur_wrong = load_verdicts()
    xref = CellLineXrefIndex(str(XREFS))
    sonnet_inh = REV/"data"/"results_cl"/"claude-sonnet-4-6"/"summary_inherit.tsv"

    sources = {
        "SapBERT (neural)":           ("sapbert", REV/"data"/"results"/"sapbert_cell_line"/"per_gse"),
        "Claude Sonnet 4.6":          ("llm",     REV/"data"/"results_cl"/"claude-sonnet-4-6"),
        "Claude Sonnet 4.6 + hybrid": ("llm",     REV/"data"/"results_cl"/"claude-sonnet-4-6_hybrid"),
        "Claude Opus 4.7":            ("llm",     REV/"data"/"results_cl"/"claude-opus-4-7"),
        "Claude Opus 4.7 + hybrid":   ("llm",     REV/"data"/"results_cl"/"claude-opus-4-7_hybrid"),
        "GPT-4o (original)":          ("inherit", (sonnet_inh, "gpt4o_pred")),
        "GPT-4o + hybrid":            ("llm",     REV/"data"/"results_cl"/"gpt-4o-2024-11-20_hybrid"),
    }

    rows = []
    for label, (kind, src) in sources.items():
        if   kind == "sapbert": preds = preds_from_sapbert(src)
        elif kind == "llm":     preds = preds_from_llm_jsons(src)
        elif kind == "inherit": preds = preds_from_inherit(src[0], src[1])
        n, ke_o, kx_o = PINNED[label]
        de, dxw = deltas(preds, truth, xref, cur_correct, cur_wrong)
        ke_ov = max(0, min(n, ke_o + de))
        kx_ov = max(0, min(n, kx_o + dxw))
        rows.append((label, n, ke_o, ke_ov, kx_o, kx_ov, de, dxw))

    print(f"{'system':<32} {'n':>4} {'exact_o':>8} {'exact_ov':>9} {'xw_o':>6} {'xw_ov':>6}  {'Δe':>3} {'Δxw':>3}")
    for label, n, ke_o, ke_ov, kx_o, kx_ov, de, dxw in rows:
        print(f"{label:<32} {n:>4} {ke_o:>8} {ke_ov:>9} {kx_o:>6} {kx_ov:>6}  {de:>+3} {dxw:>+3}")

    with OUT_TSV.open("w") as f:
        f.write("system_label\tn\tk_exact_orig\tk_exact_overlay\tk_xw_orig\tk_xw_overlay\n")
        for label, n, ke_o, ke_ov, kx_o, kx_ov, *_ in rows:
            f.write(f"{label}\t{n}\t{ke_o}\t{ke_ov}\t{kx_o}\t{kx_ov}\n")
    print(f"\nwrote {OUT_TSV}")


if __name__ == "__main__":
    main()
