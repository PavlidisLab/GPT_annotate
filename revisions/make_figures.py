"""Generate publication-quality SVG figures for the main findings.

Run from the repo root:
    revisions/.venv/bin/python revisions/make_figures.py

Each figure is saved to revisions/figures/<name>.svg.
The first panel of each figure shows the published Rogic et al. (2026)
result for that metric (GPT-4o baseline); the second shows our
Anthropic-Claude replication on the identical sample.
"""
from __future__ import annotations

import csv
import math
import os
from pathlib import Path

import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch

ROOT      = Path(__file__).resolve().parent
ANALYSIS  = ROOT / "data" / "analysis"
FIG_DIR   = ROOT / "figures"
FIG_DIR.mkdir(exist_ok=True)

# ----------------------------------------------------------------------------
# Style — clean, flat, modern
# ----------------------------------------------------------------------------
mpl.rcParams.update({
    "font.family":        ["Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"],
    "font.size":          11,
    "axes.titlesize":     12,
    "axes.titleweight":   "semibold",
    "axes.titlepad":      14,
    "axes.labelsize":     10.5,
    "axes.labelweight":   "regular",
    "axes.spines.top":    False,
    "axes.spines.right":  False,
    "axes.linewidth":     0.8,
    "axes.edgecolor":     "#475569",
    "axes.labelcolor":    "#1f2937",
    "xtick.color":        "#475569",
    "ytick.color":        "#475569",
    "xtick.labelsize":    10,
    "ytick.labelsize":    10,
    "ytick.major.size":   0,
    "xtick.major.size":   0,
    "ytick.major.pad":    6,
    "grid.color":         "#e5e7eb",
    "grid.linewidth":     0.7,
    "axes.grid":          True,
    "axes.grid.axis":     "y",
    "figure.dpi":         120,
    "savefig.dpi":        300,
    "savefig.bbox":       "tight",
    "savefig.transparent":False,
    "svg.fonttype":       "none",   # keep text as text in the SVG
})

C = {
    "gpt4o":   "#94a3b8",   # muted gray-blue, the Rogic baseline
    "sonnet":  "#3b82f6",   # vivid blue
    "opus":    "#6366f1",   # indigo
    "haiku":   "#f59e0b",   # amber
    "patch":   "#10b981",   # emerald green (improvements)
    "ensemble":"#059669",   # darker green (consensus)
    "text2term":"#dc2626",  # red (baseline-baseline)
    "annot":   "#1f2937",   # near-black for labels
    "rogic_bg":"#f8fafc",   # very light gray panel tint
}


# ----------------------------------------------------------------------------
# Data loading
# ----------------------------------------------------------------------------
def load_tsv(path):
    return list(csv.DictReader(open(path), delimiter="\t"))


def panel_title(ax, txt, color="#0f172a"):
    ax.set_title(txt, color=color, loc="left", fontweight="semibold")


def style_axes(ax, yticks_pct=True, ymax=1.0):
    ax.set_axisbelow(True)
    ax.set_ylim(0, ymax)
    if yticks_pct:
        ax.set_yticks([i / 10 for i in range(0, int(ymax * 10) + 1, 2)])
        ax.set_yticklabels([f"{int(v*100)}%" for v in ax.get_yticks()])


def annotate_bars(ax, bars, values, color="#1f2937", offset=0.012, fmt="{v:.1%}",
                  ci_upper=None):
    """Place a label above each bar. If ci_upper is given (per-bar upper-CI
    half-width), the label clears the error-bar cap."""
    for i, (bar, v) in enumerate(zip(bars, values)):
        extra = ci_upper[i] if ci_upper else 0.0
        ax.annotate(fmt.format(v=v),
                    xy=(bar.get_x() + bar.get_width() / 2, bar.get_height() + extra + offset),
                    ha="center", va="bottom", fontsize=9.5, color=color)


def add_panel_marker(ax, letter):
    ax.text(-0.12, 1.05, letter, transform=ax.transAxes,
            fontsize=14, fontweight="bold", color="#0f172a", va="bottom", ha="left")


# ----------------------------------------------------------------------------
# Figure 1 — Strain-task accuracy
# ----------------------------------------------------------------------------
def figure_strain():
    rows = {r["model"]: r for r in load_tsv(ANALYSIS / "01_accuracy_with_ci.tsv")}
    spec = {r["setup"]: r for r in load_tsv(ANALYSIS / "06_specificity_accuracy.tsv")}

    def acc(d): return float(d["exact"])
    def ci(d): return (float(d["exact"]) - float(d["lo95"]), float(d["hi95"]) - float(d["exact"]))

    fig, (axL, axR) = plt.subplots(1, 2, figsize=(11.4, 4.6),
                                    gridspec_kw={"width_ratios": [1, 3.4], "wspace": 0.18})

    # ---- Left: Rogic et al. (2026) panel ----
    axL.set_facecolor(C["rogic_bg"])
    val = acc(rows["GPT-4o (published)"])
    lo, hi = ci(rows["GPT-4o (published)"])
    bars = axL.bar([0], [val], width=0.5, color=C["gpt4o"],
                   yerr=[[lo], [hi]], capsize=5, error_kw={"elinewidth": 1.1, "ecolor": "#475569"})
    annotate_bars(axL, bars, [val], ci_upper=[hi])
    axL.set_xticks([0])
    axL.set_xticklabels(["GPT-4o"])
    axL.set_ylabel("Exact match")
    style_axes(axL)
    panel_title(axL, "Rogic et al. (2026)")
    add_panel_marker(axL, "A")

    # ---- Right: This work panel ----
    setups = [
        ("Sonnet 4.6",              rows["Claude Sonnet 4.6"],            C["sonnet"]),
        ("Opus 4.7",                rows["Claude Opus 4.7"],              C["opus"]),
        ("Haiku 4.5",               rows["Claude Haiku 4.5"],             C["haiku"]),
        ("Sonnet 4.6\n+ spec-rule", spec["Sonnet 4.6 + specificity rule"],C["patch"]),
    ]
    xs = list(range(len(setups)))
    vals = [acc(r) for _, r, _ in setups]
    cis  = [ci(r) for _, r, _ in setups]
    colors = [c for _, _, c in setups]
    yerr = [[c[0] for c in cis], [c[1] for c in cis]]
    bars = axR.bar(xs, vals, width=0.62, color=colors,
                    yerr=yerr, capsize=4.5, error_kw={"elinewidth": 1.1, "ecolor": "#475569"})
    annotate_bars(axR, bars, vals, ci_upper=[c[1] for c in cis])

    axR.set_xticks(xs)
    axR.set_xticklabels([s for s, _, _ in setups])
    style_axes(axR)
    panel_title(axR, "This work — Anthropic Claude")
    add_panel_marker(axR, "B")

    fig.suptitle("Strain annotation accuracy, paired 500-experiment sample",
                 fontsize=13, fontweight="semibold", x=0.04, ha="left", y=1.02)
    fig.savefig(FIG_DIR / "fig1_strain_accuracy.svg")
    plt.close(fig)
    print(f"wrote {FIG_DIR/'fig1_strain_accuracy.svg'}")


# ----------------------------------------------------------------------------
# Figure 2 — Cell-line accuracy under three scoring rules
# ----------------------------------------------------------------------------
def figure_cell_line_scoring():
    # Hard-coded from cell_line_eval.py + cell_line_inherit_curator.py outputs.
    # See SENSITIVITY_ANALYSES.md Table S4 for provenance.
    rules = ["Exact-ID match", "+ cross-walk", "+ curator inheritance"]
    sonnet = [0.163, 0.523, 0.588]
    gpt4o  = [0.191, 0.525, 0.645]

    fig, (axL, axR) = plt.subplots(1, 2, figsize=(11.4, 4.6),
                                    gridspec_kw={"width_ratios": [1.0, 2.2], "wspace": 0.18})

    # Left — Rogic et al. headline number on the full 3,377 cell-line set
    axL.set_facecolor(C["rogic_bg"])
    rogic_pub = 0.59
    bars = axL.bar([0], [rogic_pub], width=0.5, color=C["gpt4o"])
    annotate_bars(axL, bars, [rogic_pub])
    axL.set_xticks([0])
    axL.set_xticklabels(["GPT-4o\n(post-curation)"])
    axL.set_ylabel("Accuracy")
    style_axes(axL)
    panel_title(axL, "Rogic et al. (2026)")
    axL.text(0, -0.18, "published headline\non full 3,377-experiment set",
             transform=axL.transAxes, fontsize=9, color="#64748b", ha="center")
    add_panel_marker(axL, "A")

    # Right — grouped bars across scoring rules, Sonnet vs GPT-4o (this work)
    x = list(range(len(rules)))
    w = 0.36
    b1 = axR.bar([xi - w/2 for xi in x], sonnet, width=w, color=C["sonnet"], label="Sonnet 4.6")
    b2 = axR.bar([xi + w/2 for xi in x], gpt4o,  width=w, color=C["gpt4o"],  label="GPT-4o (published)")
    annotate_bars(axR, b1, sonnet, offset=0.014)
    annotate_bars(axR, b2, gpt4o, offset=0.014)
    axR.set_xticks(x)
    axR.set_xticklabels(rules)
    style_axes(axR)
    axR.legend(loc="upper left", frameon=False, fontsize=10, ncol=2,
               bbox_to_anchor=(0.0, 1.0))
    panel_title(axR, "This work — same 500-experiment sample")
    add_panel_marker(axR, "B")

    fig.suptitle("Cell-line accuracy by scoring rule",
                 fontsize=13, fontweight="semibold", x=0.04, ha="left", y=1.02)
    fig.savefig(FIG_DIR / "fig2_cell_line_scoring.svg")
    plt.close(fig)
    print(f"wrote {FIG_DIR/'fig2_cell_line_scoring.svg'}")


# ----------------------------------------------------------------------------
# Figure 3 — Three-way ensemble: precision vs coverage on cell lines
# ----------------------------------------------------------------------------
def figure_ensemble():
    # From ensemble_analysis.py output (n=497).
    points = [
        ("Sonnet 4.6 alone",  1.000, 0.523, C["sonnet"]),
        ("Opus 4.7 alone",    1.000, 0.515, C["opus"]),
        ("GPT-4o alone",      1.000, 0.525, C["gpt4o"]),
        ("2-of-3 majority",   0.751, 0.694, C["ensemble"]),
        ("All three agree",   0.505, 0.869, C["patch"]),
    ]
    fig, ax = plt.subplots(figsize=(8.0, 5.6))
    fig.subplots_adjust(top=0.80, bottom=0.12, left=0.10, right=0.96)

    for label, cov, prec, col in points:
        ax.scatter(cov, prec, s=180, color=col, edgecolor="white", linewidth=1.5,
                   zorder=3)
    # Inline labels — placement chosen by hand to avoid axis & arrow collisions
    label_offsets = {
        "Sonnet 4.6 alone":  ( -0.015,  0.030, "right", "bottom"),
        "Opus 4.7 alone":    ( -0.015, -0.035, "right", "top"),
        "GPT-4o alone":      ( -0.015,  0.000, "right", "center"),
        "2-of-3 majority":   ( -0.020,  0.025, "right", "bottom"),
        "All three agree":   (  0.020,  0.025, "left",  "bottom"),
    }
    for label, cov, prec, col in points:
        dx, dy, ha, va = label_offsets[label]
        ax.annotate(label, xy=(cov, prec), xytext=(cov + dx, prec + dy),
                    fontsize=10.5, color="#1f2937", ha=ha, va=va)

    # Connector arrows: single model → 2-of-3 → all-three
    ax.annotate("", xy=(0.751, 0.694), xytext=(1.000, 0.521),
                arrowprops=dict(arrowstyle="->", color="#cbd5e1", lw=1.2, shrinkA=10, shrinkB=10))
    ax.annotate("", xy=(0.505, 0.869), xytext=(0.751, 0.694),
                arrowprops=dict(arrowstyle="->", color="#cbd5e1", lw=1.2, shrinkA=10, shrinkB=10))

    ax.set_xlim(0.42, 1.08)
    ax.set_ylim(0.42, 1.0)
    ax.set_xticks([0.5, 0.6, 0.7, 0.8, 0.9, 1.0])
    ax.set_xticklabels([f"{int(v*100)}%" for v in ax.get_xticks()])
    ax.set_yticks([0.5, 0.6, 0.7, 0.8, 0.9, 1.0])
    ax.set_yticklabels([f"{int(v*100)}%" for v in ax.get_yticks()])
    ax.set_xlabel("Coverage (fraction of experiments with a prediction)")
    ax.set_ylabel("Precision (within covered experiments)")
    ax.grid(axis="both", linewidth=0.7, color="#e5e7eb")
    ax.set_axisbelow(True)

    fig.text(0.06, 0.93, "Cell-line ensemble — precision vs coverage",
             fontsize=13, fontweight="semibold", color="#0f172a")
    fig.text(0.06, 0.87,
             "Independent frontier models converge less often on cell lines than on strains;\n"
             "intersecting their predictions trades coverage for precision.",
             fontsize=9.5, color="#64748b")

    fig.savefig(FIG_DIR / "fig3_ensemble.svg")
    plt.close(fig)
    print(f"wrote {FIG_DIR/'fig3_ensemble.svg'}")


# ----------------------------------------------------------------------------
# Figure 4 — text2term TFIDF vs LLM pipeline
# ----------------------------------------------------------------------------
def figure_baseline():
    rows = {r["model"]: r for r in load_tsv(ANALYSIS / "01_accuracy_with_ci.tsv")}
    methods = [
        ("text2term TFIDF",       0.018,                                 C["text2term"]),
        ("GPT-4o",                float(rows["GPT-4o (published)"]["exact"]),   C["gpt4o"]),
        ("Sonnet 4.6",            float(rows["Claude Sonnet 4.6"]["exact"]),    C["sonnet"]),
        ("Opus 4.7",              float(rows["Claude Opus 4.7"]["exact"]),      C["opus"]),
        ("Sonnet 4.6 + spec-rule",0.768,                                 C["patch"]),
    ]
    fig, ax = plt.subplots(figsize=(8.0, 5.0))
    fig.subplots_adjust(top=0.80, bottom=0.12, left=0.10, right=0.96)
    xs = list(range(len(methods)))
    vals = [m[1] for m in methods]
    cols = [m[2] for m in methods]
    bars = ax.bar(xs, vals, width=0.6, color=cols)
    annotate_bars(ax, bars, vals)
    ax.set_xticks(xs)
    ax.set_xticklabels([m[0] for m in methods], rotation=0, ha="center")
    for tick in ax.get_xticklabels():
        tick.set_fontsize(9.5)
    style_axes(ax)
    fig.text(0.06, 0.93, "Strain accuracy: TFIDF baseline vs frontier LLMs",
             fontsize=13, fontweight="semibold", color="#0f172a")
    fig.text(0.06, 0.87,
             "Free-text-to-ontology mapping on GEO metadata is dominated by the\n"
             "entity-extraction step, not the term-similarity step.",
             fontsize=9.5, color="#64748b")

    fig.savefig(FIG_DIR / "fig4_baseline.svg")
    plt.close(fig)
    print(f"wrote {FIG_DIR/'fig4_baseline.svg'}")


# ----------------------------------------------------------------------------
# Figure 5 — Top-K retrieval sensitivity (cell-line task)
# ----------------------------------------------------------------------------
def figure_topk():
    ks = [10, 25, 50, 100, 200]
    sonnet_name = [0.27, 0.35, 0.37, 0.40, 0.39]
    sonnet_exact= [0.19, 0.32, 0.35, 0.37, 0.36]
    fig, ax = plt.subplots(figsize=(7.4, 5.0))
    fig.subplots_adjust(top=0.85, bottom=0.13, left=0.11, right=0.96)
    ax.plot(ks, sonnet_name,  marker="o", linewidth=2.2, color=C["sonnet"],
            markersize=8, markeredgecolor="white", label="Cross-walk name match")
    ax.plot(ks, sonnet_exact, marker="o", linewidth=2.2, color=C["opus"],
            markersize=8, markeredgecolor="white", label="Exact-ID match")
    for k, v in zip(ks, sonnet_name):
        ax.annotate(f"{int(v*100)}%", xy=(k, v), xytext=(0, 9), textcoords="offset points",
                    ha="center", fontsize=9, color="#1f2937")
    ax.axvline(50, color="#94a3b8", linewidth=0.9, linestyle=(0, (3, 3)), alpha=0.7)
    ax.text(52, 0.045, "K = 50 (paper)", color="#475569", fontsize=9, ha="left", rotation=0)
    ax.set_xscale("log")
    ax.set_xticks(ks)
    ax.get_xaxis().set_major_formatter(mpl.ticker.ScalarFormatter())
    ax.set_xlabel("Retrieval window K (log scale)")
    ax.set_ylabel("Accuracy")
    style_axes(ax, ymax=0.6)
    ax.legend(loc="lower right", frameon=False, fontsize=10)
    fig.text(0.06, 0.93, "Cell-line accuracy vs retrieval window K",
             fontsize=13, fontweight="semibold", color="#0f172a")
    fig.text(0.06, 0.89, "Sonnet 4.6, 100-experiment subset",
             fontsize=9.5, color="#64748b")
    fig.savefig(FIG_DIR / "fig5_topk_sensitivity.svg")
    plt.close(fig)
    print(f"wrote {FIG_DIR/'fig5_topk_sensitivity.svg'}")


# ----------------------------------------------------------------------------
def main():
    figure_strain()
    figure_cell_line_scoring()
    figure_ensemble()
    figure_baseline()
    figure_topk()


if __name__ == "__main__":
    main()
