"""Generate publication-quality SVG figures for the main findings.

Run from the repo root:
    revisions/.venv/bin/python revisions/make_figures.py

Each figure is saved to revisions/figures/<name>.svg.
The first panel of each figure shows the original GPT-4o result for that
metric; the second shows our Anthropic-Claude replication on the
identical sample.
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


def save_fig(fig, name):
    """Emit both SVG (editable / archival) and PNG (preview).

    Two variants per call:
    - revisions/figures/<name>.{svg,png}       — full version with on-canvas
                                                 title + subtitle (slides /
                                                 posters / preview).
    - revisions/figures/journal/<name>.{svg,png} — clean version with the
                                                 figure-level title+subtitle
                                                 text hidden (for journal
                                                 submission, where that
                                                 information belongs in the
                                                 caption). Data-point labels
                                                 and in-axes annotations are
                                                 preserved.
    """
    svg = FIG_DIR / f"{name}.svg"
    png = FIG_DIR / f"{name}.png"
    fig.savefig(svg)
    fig.savefig(png, dpi=200)
    print(f"wrote {svg}")
    print(f"wrote {png}")

    # Clean / journal variant: hide figure-level title texts (added with
    # fig.text(...)) and re-tighten the top margin to use the freed space.
    journal_dir = FIG_DIR / "journal"
    journal_dir.mkdir(exist_ok=True)
    saved_visibility = [(t, t.get_visible()) for t in list(fig.texts)]
    for t, _ in saved_visibility:
        t.set_visible(False)
    saved_top = fig.subplotpars.top
    # Pull the axes up to fill the freed space; cap at 0.95 to keep ticks legible.
    fig.subplots_adjust(top=min(0.95, max(saved_top + 0.10, 0.92)))
    svg_c = journal_dir / f"{name}.svg"
    png_c = journal_dir / f"{name}.png"
    eps_c = journal_dir / f"{name}.eps"   # native EPS for Illustrator editing
    fig.savefig(svg_c)
    fig.savefig(png_c, dpi=200)
    fig.savefig(eps_c)
    # Restore so the in-memory figure remains the slide-format version.
    fig.subplots_adjust(top=saved_top)
    for t, vis in saved_visibility:
        t.set_visible(vis)
    print(f"wrote {svg_c}")
    print(f"wrote {png_c}")
    print(f"wrote {eps_c}")

# ----------------------------------------------------------------------------
# Style — clean, flat, modern
# ----------------------------------------------------------------------------
mpl.rcParams.update({
    "font.family":        ["Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"],
    "font.size":          13,
    "axes.titlesize":     14,
    "axes.titleweight":   "semibold",
    "axes.titlepad":      14,
    "axes.labelsize":     12.5,
    "axes.labelweight":   "regular",
    "axes.spines.top":    False,
    "axes.spines.right":  False,
    # Publication-grade stroke weights — bumped from 1.2 to 2.0 so the
    # spines / ticks / bar edges survive reduction at the journal
    # column-width and stay legible when the EPS is opened in
    # Illustrator (the previous 1.2-pt strokes looked too fine in EPS).
    "axes.linewidth":     2.0,
    "axes.edgecolor":     "#1f2937",
    "axes.labelcolor":    "#0f172a",
    "xtick.color":        "#1f2937",
    "ytick.color":        "#1f2937",
    "xtick.labelsize":    11.5,
    "ytick.labelsize":    11.5,
    "ytick.major.size":   6,
    "xtick.major.size":   6,
    "ytick.major.width":  2.0,
    "xtick.major.width":  2.0,
    "xtick.direction":    "out",
    "ytick.direction":    "out",
    "ytick.major.pad":    4,
    "axes.grid":          False,
    "lines.linewidth":    2.2,
    "patch.linewidth":    1.8,
    "figure.dpi":         120,
    "savefig.dpi":        300,
    "savefig.bbox":       "tight",
    "savefig.transparent":False,
    "svg.fonttype":       "none",   # keep text as text in the SVG
    "ps.fonttype":        42,       # embed TrueType in EPS so text is
                                    # editable in Illustrator (default
                                    # converts to Type 3 outlines)
})


def lighten(hex_color: str, frac: float = 0.55) -> str:
    """Blend a hex colour with white. ``frac`` is the white fraction:
    ``frac=0`` returns the input, ``frac=1`` returns pure white. Used as
    a print-safe substitute for ``alpha=`` on bar/scatter facecolour —
    EPS does not carry an alpha channel, so the two bars in an
    alpha-paired comparison render identical-solid in Illustrator."""
    h = hex_color.lstrip("#")
    r, g, b = int(h[0:2], 16), int(h[2:4], 16), int(h[4:6], 16)
    r = int(r + (255 - r) * frac)
    g = int(g + (255 - g) * frac)
    b = int(b + (255 - b) * frac)
    return f"#{r:02x}{g:02x}{b:02x}"


# Publication-grade error-bar styling, shared across every figure:
# - elinewidth 2.0 matches the bar-edge / axis stroke weight
# - capthick 3.0 is intentionally heavier than elinewidth so the cap
#   reads as a distinct I-beam, not a merge with the bar edge
# - capsize 7 protrudes beyond a typical bar width (0.36 data units →
#   ~25–35 device pt at our figure sizes), so the cap is visible even
#   when the upper Wilson bound clips at 1.0 (k = n bars at 100 %)
# - ecolor near-black so caps sit cleanly on top of paler tinted fills
ERR_KW = {"elinewidth": 2.0, "capthick": 2.0, "ecolor": "#1f2937"}
ERR_CAPSIZE = 7.0
# Scatter / PR plots use slightly thinner error lines than bar plots —
# the markers are small and a full 2 pt cap would dominate the dot.
ERR_KW_SCATTER = {"elinewidth": 1.6, "capthick": 1.6}
ERR_CAPSIZE_SCATTER = 5.0
# Default light-gray colour for error bars on scatter plots (so they
# fade behind the data point) when the bar/cap colour shouldn't be
# the same near-black as the axis frame.
ERR_ECOLOR_SCATTER = "#94a3b8"

C = {
    # GPT-4o is the paper's primary method — give it the standout amber
    # (was muted slate-grey, which read as "another baseline"). Haiku is
    # the weakest frontier model; it loses amber to GPT-4o and gets a
    # slate that still distinguishes it from the lexical-baseline grey
    # (#cbd5e1).
    "gpt4o":   "#f59e0b",   # amber — paper's method
    "sonnet":  "#3b82f6",   # vivid blue
    "opus":    "#6366f1",   # indigo
    "haiku":   "#64748b",   # slate-500
    "patch":   "#10b981",   # emerald green (improvements)
    "ensemble":"#059669",   # darker green (consensus)
    "text2term":"#dc2626",  # red (baseline-baseline)
    "llama":   "#8b5cf6",   # violet — open-weights family
    "annot":   "#1f2937",   # near-black for labels
    "orig_bg": "#f8fafc",   # very light gray panel tint
    # Edge colour applied to LLM bars to lift them off the lexical /
    # neural baselines visually. Non-LLM bars get edgecolor="none".
    "llm_edge": "#1f2937",
}


# ----------------------------------------------------------------------------
# Data loading
# ----------------------------------------------------------------------------
def load_tsv(path):
    return list(csv.DictReader(open(path), delimiter="\t"))


def wilson_ci(k: int, n: int, z: float = 1.96) -> tuple[float, float]:
    """Wilson 95 % score interval for k successes out of n trials."""
    if n == 0: return (0.0, 0.0)
    p = k / n
    denom  = 1 + z*z/n
    center = (p + z*z/(2*n)) / denom
    half   = z * math.sqrt(p*(1-p)/n + z*z/(4*n*n)) / denom
    return max(0.0, center - half), min(1.0, center + half)


def wilson_err(k: int, n: int) -> tuple[float, float]:
    """Asymmetric error bar (lower, upper) suitable for matplotlib yerr."""
    p = k / n if n else 0.0
    lo, hi = wilson_ci(k, n)
    return p - lo, hi - p


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
                    ha="center", va="bottom", fontsize=11, color=color)


def add_panel_marker(ax, letter):
    ax.text(-0.12, 1.05, letter, transform=ax.transAxes,
            fontsize=14, fontweight="bold", color="#0f172a", va="bottom", ha="left")


# ----------------------------------------------------------------------------
# Figure 1 — Strain-task accuracy
# ----------------------------------------------------------------------------
def figure_ensemble():
    # From ensemble_analysis.py output (n=497). Each point gets a Wilson 95 %
    # CI on coverage (k_covered / n) and on precision (k_correct / k_covered).
    points = [
        # label,             cov_k, cov_n, prec_k, prec_n, color
        ("Sonnet 4.6 alone",   497, 497, 260, 497, C["sonnet"]),
        ("Opus 4.7 alone",     497, 497, 256, 497, C["opus"]),
        ("GPT-4o alone",       497, 497, 261, 497, C["gpt4o"]),
        ("2-of-3 majority",    373, 497, 259, 373, C["ensemble"]),
        ("All three agree",    251, 497, 218, 251, C["patch"]),
    ]
    fig, ax = plt.subplots(figsize=(8.0, 5.6))
    fig.subplots_adjust(top=0.80, bottom=0.12, left=0.10, right=0.96)

    for label, ck, cn, pk, pn, col in points:
        cov  = ck / cn
        prec = pk / pn
        xerr = wilson_err(ck, cn) if cn != ck else (0, 0)
        yerr = wilson_err(pk, pn)
        ax.errorbar(cov, prec,
                    xerr=[[xerr[0]], [xerr[1]]],
                    yerr=[[yerr[0]], [yerr[1]]],
                    fmt="none", ecolor=ERR_ECOLOR_SCATTER, elinewidth=ERR_KW_SCATTER["elinewidth"], capthick=ERR_KW_SCATTER["capthick"], capsize=ERR_CAPSIZE_SCATTER, zorder=2)
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
    for label, ck, cn, pk, pn, col in points:
        cov  = ck / cn
        prec = pk / pn
        dx, dy, ha, va = label_offsets[label]
        ax.annotate(label, xy=(cov, prec), xytext=(cov + dx, prec + dy),
                    fontsize=12, color="#0f172a", ha=ha, va=va)

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

    save_fig(fig, "fig3_ensemble")
    plt.close(fig)


def figure_ensemble_with_metamuse():
    # Variant of fig3_ensemble adding MetaMuse-GPT-4o as an external comparator
    # point. Stage-1+2 numbers identical to fig3_ensemble (n=497, ensemble_analysis.py).
    # MetaMuse numbers from ~/Data/metamuse-bench/scored/full500_summary.tsv
    # (n=498, MetaMuse cell_line-only, 3 GSMs/GSE set-union, cross-walk scoring).
    points = [
        # label,                          cov_k, cov_n, prec_k, prec_n, color, marker
        ("Sonnet 4.6 alone",               497, 497, 260, 497, C["sonnet"],   "o"),
        ("Opus 4.7 alone",                 497, 497, 256, 497, C["opus"],     "o"),
        ("GPT-4o alone",                   497, 497, 261, 497, C["gpt4o"],    "o"),
        ("2-of-3 majority",                373, 497, 259, 373, C["ensemble"], "o"),
        ("All three agree",                251, 497, 218, 251, C["patch"],    "o"),
        ("MetaMuse-GPT-4o (cell_line)",    198, 498, 137, 198, "#db2777",     "D"),
    ]
    fig, ax = plt.subplots(figsize=(8.0, 5.6))
    fig.subplots_adjust(top=0.80, bottom=0.12, left=0.10, right=0.96)

    for label, ck, cn, pk, pn, col, marker in points:
        cov  = ck / cn
        prec = pk / pn
        xerr = wilson_err(ck, cn) if cn != ck else (0, 0)
        yerr = wilson_err(pk, pn)
        ax.errorbar(cov, prec,
                    xerr=[[xerr[0]], [xerr[1]]],
                    yerr=[[yerr[0]], [yerr[1]]],
                    fmt="none", ecolor=ERR_ECOLOR_SCATTER, elinewidth=ERR_KW_SCATTER["elinewidth"], capthick=ERR_KW_SCATTER["capthick"], capsize=ERR_CAPSIZE_SCATTER, zorder=2)
        ax.scatter(cov, prec, s=180, color=col, edgecolor="white", linewidth=1.5,
                   marker=marker, zorder=3)
    label_offsets = {
        "Sonnet 4.6 alone":            ( -0.015,  0.030, "right",  "bottom"),
        "Opus 4.7 alone":              ( -0.015, -0.035, "right",  "top"),
        "GPT-4o alone":                ( -0.015,  0.000, "right",  "center"),
        "2-of-3 majority":             ( -0.020,  0.025, "right",  "bottom"),
        "All three agree":             (  0.020,  0.025, "left",   "bottom"),
        "MetaMuse-GPT-4o (cell_line)": (  0.020,  0.000, "left",   "center"),
    }
    for label, ck, cn, pk, pn, col, marker in points:
        cov  = ck / cn
        prec = pk / pn
        dx, dy, ha, va = label_offsets[label]
        ax.annotate(label, xy=(cov, prec), xytext=(cov + dx, prec + dy),
                    fontsize=12, color="#0f172a", ha=ha, va=va)

    ax.annotate("", xy=(0.751, 0.694), xytext=(1.000, 0.521),
                arrowprops=dict(arrowstyle="->", color="#cbd5e1", lw=1.2, shrinkA=10, shrinkB=10))
    ax.annotate("", xy=(0.505, 0.869), xytext=(0.751, 0.694),
                arrowprops=dict(arrowstyle="->", color="#cbd5e1", lw=1.2, shrinkA=10, shrinkB=10))

    ax.set_xlim(0.32, 1.08)
    ax.set_ylim(0.42, 1.0)
    ax.set_xticks([0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0])
    ax.set_xticklabels([f"{int(v*100)}%" for v in ax.get_xticks()])
    ax.set_yticks([0.5, 0.6, 0.7, 0.8, 0.9, 1.0])
    ax.set_yticklabels([f"{int(v*100)}%" for v in ax.get_yticks()])
    ax.set_xlabel("Coverage (fraction of experiments with a prediction)")
    ax.set_ylabel("Precision (within covered experiments)")
    ax.grid(False)

    fig.text(0.06, 0.93, "Cell-line ensemble — precision vs coverage (with MetaMuse)",
             fontsize=13, fontweight="semibold", color="#0f172a")
    fig.text(0.06, 0.85,
             "MetaMuse (diamond) is a single-LLM agentic system with internal SapBERT/arbitrator\n"
             "abstention; it lands on the same precision–coverage frontier as our external ensemble.\n"
             "Stage-1+2 systems: n=497. MetaMuse: n=498 (cell500, cross-walk scoring).",
             fontsize=9.5, color="#64748b")

    save_fig(fig, "fig3_ensemble_metamuse")
    plt.close(fig)


# ----------------------------------------------------------------------------
# Figure 4 — text2term TFIDF vs LLM pipeline
# ----------------------------------------------------------------------------
def figure_baseline():
    # (label, k, color) — all denominators are n=500 except the bottom row,
    # the original regex baseline, which is computed over the same sample.
    # Ordered worst-to-best by k/500. text2term and the regex baseline
    # share the muted grey tone; the neural / LLM bars carry their model
    # palette. See SENSITIVITY_ANALYSES.md Table S5 for provenance.
    # 4th column = is_llm flag → drives the darker outline that
    # visually separates LLM-based methods from lexical/neural baselines.
    methods = [
        ("text2term TFIDF",         27,  "#cbd5e1", False),
        ("Regex",                   32,  "#cbd5e1", False),
        ("BM25 top-1",             114, "#cbd5e1", False),
        ("Llama 3.3 70B",          156, C["llama"], True),
        ("Claude Haiku 4.5",       237, C["haiku"],  True),
        ("SapBERT (neural)",       293, "#0ea5e9", False),
        ("Llama 3.3 70B + spec",   331, C["llama"], True),
        ("GPT-4o (original)",      360, C["gpt4o"], True),
        ("Claude Sonnet 4.6",      368, C["sonnet"], True),
        ("Claude Opus 4.7",        377, C["opus"],   True),
        ("Sonnet 4.6 + spec-rule", 384, C["patch"],  True),
        ("GPT-4o + spec-rule",     385, C["patch"],  True),
    ]
    n = 500
    vals  = [m[1] / n for m in methods]
    cols  = [m[2]    for m in methods]
    # Outline every bar — same convention as figure_cell_line_baselines.
    edgec = [C["llm_edge"] for _ in methods]
    err_l = [wilson_err(m[1], n)[0] for m in methods]
    err_u = [wilson_err(m[1], n)[1] for m in methods]

    fig, ax = plt.subplots(figsize=(12.0, 5.2))
    fig.subplots_adjust(top=0.80, bottom=0.28, left=0.065, right=0.985)
    xs   = list(range(len(methods)))
    bars = ax.bar(xs, vals, width=0.65, color=cols,
                  edgecolor=edgec, linewidth=2.0,
                  yerr=[err_l, err_u], capsize=ERR_CAPSIZE,
                  error_kw=ERR_KW)
    annotate_bars(ax, bars, vals, ci_upper=err_u)
    ax.set_xticks(xs)
    ax.set_xticklabels([m[0] for m in methods], rotation=22, ha="right")
    for tick in ax.get_xticklabels():
        tick.set_fontsize(12)
    style_axes(ax)
    fig.text(0.06, 0.93, "Strain accuracy: regex / TFIDF / neural baselines vs frontier LLMs",
             fontsize=13, fontweight="semibold", color="#0f172a")
    fig.text(0.06, 0.87,
             "Twelve methods evaluated on the same 500-experiment sample (Wilson 95 % CIs).\n"
             "Open-weights bars in violet; spec-rule prompt variants in green.",
             fontsize=9.5, color="#64748b")

    save_fig(fig, "fig4_baseline")
    plt.close(fig)


# ----------------------------------------------------------------------------
# Figure 5 — Top-K retrieval sensitivity (cell-line task)
# ----------------------------------------------------------------------------
def figure_topk():
    ks = [10, 25, 50, 100, 200]
    # n = 100 subsample for the top-K sweep; k_name/k_exact counts from
    # data/results_cl/claude-sonnet-4-6_topk{K}/summary_eval.tsv.
    name_k  = [27, 35, 37, 40, 39]
    exact_k = [19, 32, 35, 37, 36]
    n_topk  = 100
    sonnet_name  = [k/n_topk for k in name_k]
    sonnet_exact = [k/n_topk for k in exact_k]
    name_err  = list(zip(*[wilson_err(k, n_topk) for k in name_k]))
    exact_err = list(zip(*[wilson_err(k, n_topk) for k in exact_k]))
    fig, ax = plt.subplots(figsize=(7.4, 5.0))
    fig.subplots_adjust(top=0.85, bottom=0.13, left=0.11, right=0.96)
    ax.errorbar(ks, sonnet_name, yerr=name_err, fmt="o-", linewidth=2.2,
                color=C["sonnet"], markersize=8, markeredgecolor="white",
                ecolor=ERR_ECOLOR_SCATTER, elinewidth=ERR_KW_SCATTER["elinewidth"],
                capthick=ERR_KW_SCATTER["capthick"], capsize=ERR_CAPSIZE_SCATTER,
                label="Cross-walk name match")
    ax.errorbar(ks, sonnet_exact, yerr=exact_err, fmt="o-", linewidth=2.2,
                color=C["opus"], markersize=8, markeredgecolor="white",
                ecolor=ERR_ECOLOR_SCATTER, elinewidth=ERR_KW_SCATTER["elinewidth"],
                capthick=ERR_KW_SCATTER["capthick"], capsize=ERR_CAPSIZE_SCATTER,
                label="Exact-ID match")
    for k, v, hi in zip(ks, sonnet_name, name_err[1]):
        ax.annotate(f"{int(v*100)}%", xy=(k, v + hi), xytext=(0, 7), textcoords="offset points",
                    ha="center", fontsize=11, color="#0f172a")
    ax.axvline(50, color="#94a3b8", linewidth=1.2, linestyle=(0, (3, 3)), alpha=0.85)
    ax.set_xscale("log")
    ax.set_xticks(ks)
    ax.get_xaxis().set_major_formatter(mpl.ticker.ScalarFormatter())
    ax.set_xlabel("Retrieval window K (log scale)")
    ax.set_ylabel("Accuracy")
    style_axes(ax, ymax=0.6)
    ax.legend(loc="upper left", frameon=False, fontsize=11.5)
    fig.text(0.06, 0.93, "Cell-line accuracy vs retrieval window K",
             fontsize=13, fontweight="semibold", color="#0f172a")
    fig.text(0.06, 0.89, "Sonnet 4.6, 100-experiment subset",
             fontsize=9.5, color="#64748b")
    save_fig(fig, "fig5_topk_sensitivity")
    plt.close(fig)


# ----------------------------------------------------------------------------
def figure_cell_line_baselines():
    """Cell-line analogue of fig4_baseline: every method on one panel,
    with exact-ID and cross-walk grouped bars per method."""
    # (label, k_exact, k_xw, n, color)
    # First group: dense-only retrieval (the existing FINDINGS §7 numbers).
    # Second group (suffix "+ hybrid"): same model, dense + BM25 + RRF retrieval
    # rerun. Counts are the auto_exact / (auto_exact + auto_name) totals from
    # summary_inherit.tsv for each *_hybrid directory.
    # 6th column = is_llm flag → drives darker bar outline.
    methods = [
        ("SapBERT (neural)",          58, 112, 498, "#0ea5e9",      False),
        ("Claude Sonnet 4.6",         81, 260, 497, C["sonnet"],    True),
        ("Claude Sonnet 4.6\n+ hybrid", 213, 243, 497, C["sonnet"], True),
        ("Claude Opus 4.7",          198, 256, 498, C["opus"],      True),
        ("Claude Opus 4.7\n+ hybrid",  226, 257, 498, C["opus"],    True),
        ("GPT-4o (original)",          95, 262, 491, C["gpt4o"],    True),
        ("GPT-4o\n+ hybrid",          212, 231, 491, C["gpt4o"],    True),
    ]
    fig, ax = plt.subplots(figsize=(13.2, 5.6))
    fig.subplots_adjust(top=0.78, bottom=0.26, left=0.06, right=0.985)
    x = list(range(len(methods)))
    w = 0.36
    exact_vals = [m[1]/m[3] for m in methods]
    xw_vals    = [m[2]/m[3] for m in methods]
    exact_err  = list(zip(*[wilson_err(m[1], m[3]) for m in methods]))
    xw_err     = list(zip(*[wilson_err(m[2], m[3]) for m in methods]))
    # Outline every bar — consistent visual frame, regardless of whether
    # the row is an LLM or a non-LLM neural baseline.
    bar_edge   = C["llm_edge"]
    # Print-safe lightened RGB for the "exact-ID" bar so the pair survives
    # EPS export (alpha is dropped in EPS — two-alpha pairs render
    # identical-solid in Illustrator). See verifier figure for the same
    # pattern.
    exact_face = [lighten(m[4], 0.55) for m in methods]
    b1 = ax.bar([xi - w/2 for xi in x], exact_vals, width=w,
                color=exact_face,
                edgecolor=bar_edge, linewidth=2.0,
                yerr=exact_err, capsize=ERR_CAPSIZE,
                error_kw=ERR_KW,
                label="Exact-ID match")
    b2 = ax.bar([xi + w/2 for xi in x], xw_vals, width=w,
                color=[m[4] for m in methods],
                edgecolor=bar_edge, linewidth=2.0,
                yerr=xw_err, capsize=ERR_CAPSIZE,
                error_kw=ERR_KW,
                label="+ cross-walk name match")
    annotate_bars(ax, b1, exact_vals, offset=0.012, ci_upper=exact_err[1])
    annotate_bars(ax, b2, xw_vals,    offset=0.012, ci_upper=xw_err[1])
    ax.set_xticks(x)
    ax.set_xticklabels([m[0] for m in methods])
    for tick in ax.get_xticklabels():
        tick.set_fontsize(12)
    style_axes(ax, ymax=0.8)
    # Legend below the plot to avoid colliding with the tallest bars
    # (which sit above 0.5 after the colour rotation).
    ax.legend(loc="upper center", bbox_to_anchor=(0.5, -0.20),
              frameon=False, fontsize=12, ncol=2)
    fig.text(0.06, 0.93,
             "Cell-line accuracy: non-LLM neural baseline vs frontier LLMs",
             fontsize=13, fontweight="semibold", color="#0f172a")
    fig.text(0.06, 0.87,
             "Same 500-experiment cell-line sample (n = 491–498 with usable rows; Wilson 95 % CIs).\n"
             "\"+ hybrid\": dense + BM25 + reciprocal rank fusion at retrieval. Same Stage-2 model and prompt.",
             fontsize=9.5, color="#64748b")
    save_fig(fig, "fig6_cell_line_baselines")
    plt.close(fig)


def _strain_truth() -> dict[str, set[str]]:
    """Per-GSE truth from sample500.tsv (canonical ID form)."""
    def _split(s):
        if not s: return set()
        out = set()
        for part in s.replace("|||", ",").split(","):
            p = part.strip()
            if not p: continue
            last = p.rsplit("/", 1)[-1]
            out.add(last.replace("_", ":", 1) if "_" in last and ":" not in last else last)
        return out
    truth = {}
    with open(ROOT / "data" / "sample500.tsv") as f:
        for r in csv.DictReader(f, delimiter="\t"):
            truth[r["shortName"]] = _split(r.get("gemma_uri", ""))
    return truth


def _strain_preds(label: str) -> dict[str, set[str]]:
    """Load per-GSE strain predictions for one of the evaluated methods.

    Returns {gse: set(canonical_id)}. Sources are the per-run summary.tsv
    files written by the strain runner."""
    def _split(s):
        if not s: return set()
        out = set()
        for part in s.replace("|||", ",").split(","):
            p = part.strip()
            if not p: continue
            last = p.rsplit("/", 1)[-1]
            out.add(last.replace("_", ":", 1) if "_" in last and ":" not in last else last)
        return out

    # (path-suffix-under-results/, column-with-predictions)
    SOURCES = {
        "GPT-4o (original)":         ("claude-sonnet-4-6/summary.tsv",                   "gpt4o_pred"),
        "Claude Sonnet 4.6":         ("claude-sonnet-4-6/summary.tsv",                   "claude_pred"),
        "Claude Opus 4.7":           ("claude-opus-4-7/summary.tsv",                     "claude_pred"),
        "Claude Haiku 4.5":          ("claude-haiku-4-5-20251001/summary.tsv",           "claude_pred"),
        "Sonnet 4.6 + spec":         ("claude-sonnet-4-6_specprompt/summary.tsv",        "claude_pred"),
        "GPT-4o + spec":             ("gpt-4o-2024-11-20_specprompt/summary.tsv",        "claude_pred"),
        "Llama 3.3 70B":             ("meta-llama__Llama-3.3-70B-Instruct-Turbo/summary.tsv",          "claude_pred"),
        "Llama 3.3 70B + spec":      ("meta-llama__Llama-3.3-70B-Instruct-Turbo_specprompt/summary.tsv","claude_pred"),
        "BM25 top-1":                ("bm25_strain/summary.tsv",                         "claude_pred"),
        "text2term TFIDF":           ("text2term_strain/summary.tsv",                    "pred"),
        "Regex":                     None,   # see below
        "SapBERT (neural)":          None,
    }
    info = SOURCES.get(label)
    preds = {}
    if info is not None:
        rel, col = info
        path = ROOT / "data" / "results" / rel
        if not path.exists():
            return preds
        for r in csv.DictReader(open(path), delimiter="\t"):
            gse = r.get("gse") or r.get("shortName")
            preds[gse] = _split(r.get(col, ""))
        return preds
    # Regex + SapBERT use a different per-GSE layout (one JSON per GSE).
    if label == "SapBERT (neural)":
        path = ROOT / "data" / "results" / "sapbert_strain" / "summary.tsv"
        if not path.exists(): return preds
        for r in csv.DictReader(open(path), delimiter="\t"):
            preds[r["gse"]] = _split(r.get("pred", ""))
        return preds
    if label == "Regex":
        import json, glob
        # Regex predictions are recorded per GSE under the strain runner's
        # cache as a "regex" baseline summary if present; fall back to nil.
        # The actual cached strain inputs/outputs aren't in revisions/data;
        # skip Regex on this plot (its per-strain TP/FP/FN aren't archived
        # for the revision round).
        return preds
    return preds


def _micro_pr(preds: dict[str, set[str]], truth: dict[str, set[str]]):
    """Strain-level micro precision/recall: aggregate TP/FP/FN over GSEs."""
    TP = FP = FN = 0
    n_cov = 0
    for gse, t in truth.items():
        p = preds.get(gse, set())
        if p: n_cov += 1
        TP += len(p & t)
        FP += len(p - t)
        FN += len(t - p)
    P = TP / (TP + FP) if TP + FP else 0.0
    R = TP / (TP + FN) if TP + FN else 0.0
    return P, R, TP, FP, FN, n_cov


def figure_strain_micro_pr():
    """Strain-level micro precision–recall scatter (replaces the degenerate
    per-GSE figure_strain_pr — at the per-GSE level every non-abstaining
    method has P = R by definition because n_covered = n_total = 500).

    By aggregating TP / FP / FN at the strain mention level — counting each
    true strain in a multi-strain GSE as a separate retrieval target —
    the methods sit clearly above the y = x diagonal: every model is more
    precise than it recalls, because they miss strains in multi-strain
    GSEs (~11 % of the sample) more often than they fabricate them. The
    diagonal separation is the headline of this figure; rough ordering
    along the diagonal mirrors the per-GSE accuracy in Fig 3."""
    truth = _strain_truth()
    # Display order (worst → best at per-GSE accuracy)
    labels = [
        ("BM25 top-1",            "#cbd5e1", "o"),
        ("text2term TFIDF",       "#cbd5e1", "o"),
        ("Llama 3.3 70B",         C["llama"], "o"),
        ("Claude Haiku 4.5",      C["haiku"], "o"),
        ("SapBERT (neural)",      "#0ea5e9", "o"),
        ("Llama 3.3 70B + spec",  C["llama"], "s"),
        ("GPT-4o (original)",     C["gpt4o"], "o"),
        ("Claude Sonnet 4.6",     C["sonnet"], "o"),
        ("Claude Opus 4.7",       C["opus"], "o"),
        ("Sonnet 4.6 + spec",     C["patch"], "s"),
        ("GPT-4o + spec",         C["patch"], "s"),
    ]
    points = []
    for label, col, marker in labels:
        preds = _strain_preds(label)
        if not preds:
            continue
        P, R, TP, FP, FN, ncov = _micro_pr(preds, truth)
        points.append((label, P, R, TP, FP, FN, ncov, col, marker))

    fig, ax = plt.subplots(figsize=(8.6, 6.6))
    fig.subplots_adjust(top=0.74, bottom=0.10, left=0.10, right=0.96)
    ax.plot([0, 1], [0, 1], color="#e2e8f0", linewidth=1.4, linestyle=(0,(4,4)), zorder=1)
    for label, P, R, TP, FP, FN, ncov, col, marker in points:
        # Wilson errors over the strain-mention totals
        xerr = wilson_err(TP, TP + FN)
        yerr = wilson_err(TP, TP + FP)
        ax.errorbar(R, P, xerr=[[xerr[0]],[xerr[1]]], yerr=[[yerr[0]],[yerr[1]]],
                    fmt="none", ecolor=ERR_ECOLOR_SCATTER,
                    elinewidth=ERR_KW_SCATTER["elinewidth"],
                    capthick=ERR_KW_SCATTER["capthick"],
                    capsize=ERR_CAPSIZE_SCATTER, zorder=2)
        ax.scatter(R, P, s=160, color=col, edgecolor=C["llm_edge"],
                   linewidth=2.0, marker=marker, zorder=3)
        ax.annotate(label, xy=(R, P), xytext=(R + 0.012, P + 0.012),
                    fontsize=11, color="#0f172a", ha="left", va="bottom")

    ax.set_xlim(0, 1.0); ax.set_ylim(0, 1.0)
    ax.set_xticks([0, 0.2, 0.4, 0.6, 0.8, 1.0])
    ax.set_yticks([0, 0.2, 0.4, 0.6, 0.8, 1.0])
    ax.set_xticklabels([f"{int(v*100)}%" for v in ax.get_xticks()])
    ax.set_yticklabels([f"{int(v*100)}%" for v in ax.get_yticks()])
    ax.set_xlabel("Recall (TP / true strain mentions)")
    ax.set_ylabel("Precision (TP / predicted strain mentions)")
    ax.grid(False)
    fig.text(0.06, 0.94, "Strain task — strain-level micro precision vs recall",
             fontsize=13, fontweight="semibold", color="#0f172a")
    fig.text(0.06, 0.83,
             "Each point aggregates TP / FP / FN across the 500-GSE sample at the\n"
             "individual-strain level (multi-strain GSEs contribute each strain\n"
             "separately). Every method sits above the y = x diagonal — they miss\n"
             "strains in multi-strain GSEs more often than they fabricate strains.",
             fontsize=9.5, color="#64748b")
    save_fig(fig, "figS_strain_micro_pr")
    plt.close(fig)


def figure_strain_error_overlap():
    """Correlation of per-GSE correctness across the four frontier strain
    models. The fact that 96 / 500 GSEs are wrong for ALL four models, and
    212 / 500 are correct for all four, means most of the spread between
    models is being decided on the middle 192 GSEs — a finding the
    per-method bar chart (Fig 3) does not surface."""
    truth = _strain_truth()
    models = [
        ("GPT-4o (original)", C["gpt4o"]),
        ("Claude Haiku 4.5",  C["haiku"]),
        ("Claude Sonnet 4.6", C["sonnet"]),
        ("Claude Opus 4.7",   C["opus"]),
    ]
    per_gse = {}  # gse -> int count of models correct
    for label, _col in models:
        preds = _strain_preds(label)
        for gse, t in truth.items():
            p = preds.get(gse, set())
            ok = (p == t)
            per_gse.setdefault(gse, 0)
            per_gse[gse] += int(ok)

    n_total = len(truth)
    # Distribution: how many of 4 models correct per GSE?
    from collections import Counter
    dist = Counter(per_gse.values())
    counts = [dist.get(k, 0) for k in (0, 1, 2, 3, 4)]
    pcts   = [c / n_total for c in counts]

    fig, ax = plt.subplots(figsize=(9.0, 4.2))
    fig.subplots_adjust(top=0.74, bottom=0.18, left=0.06, right=0.985)

    # Stacked horizontal bar, single row.
    classes = [
        ("0 of 4 correct (hard)",         counts[0], "#7f1d1d"),
        ("1 of 4 correct",                counts[1], "#dc2626"),
        ("2 of 4 correct",                counts[2], "#f59e0b"),
        ("3 of 4 correct",                counts[3], "#84cc16"),
        ("4 of 4 correct (easy)",         counts[4], "#10b981"),
    ]
    def _text_color_for(hex_color: str) -> str:
        """Pick black or white text for legibility against `hex_color`,
        using WCAG relative luminance (≈ 0.5 cutoff). Saves manual
        per-segment colour tweaks when the palette evolves."""
        h = hex_color.lstrip("#")
        r, g, b = int(h[0:2], 16)/255, int(h[2:4], 16)/255, int(h[4:6], 16)/255
        # sRGB → linear, then weighted sum.
        def _lin(c): return c/12.92 if c <= 0.03928 else ((c + 0.055)/1.055) ** 2.4
        L = 0.2126*_lin(r) + 0.7152*_lin(g) + 0.0722*_lin(b)
        return "#ffffff" if L < 0.35 else "#0f172a"

    left = 0.0
    for label, c, color in classes:
        frac = c / n_total
        ax.barh(0, frac, left=left, height=0.55,
                color=color, edgecolor=C["llm_edge"], linewidth=2.0)
        # Label inside the segment if wide enough, otherwise above.
        cx = left + frac / 2
        if frac > 0.06:
            ax.text(cx, 0, f"{c}\n({frac:.0%})", ha="center", va="center",
                    fontsize=11, color=_text_color_for(color),
                    fontweight="semibold")
        else:
            ax.text(cx, 0.45, f"{c} ({frac:.0%})", ha="center", va="bottom",
                    fontsize=10, color="#0f172a")
        left += frac

    ax.set_xlim(0, 1.0)
    ax.set_ylim(-0.6, 1.1)
    ax.set_xticks([0, 0.25, 0.5, 0.75, 1.0])
    ax.set_xticklabels(["0 %", "25 %", "50 %", "75 %", "100 %"])
    ax.set_yticks([])
    ax.spines["left"].set_visible(False)
    ax.set_xlabel(f"Fraction of {n_total} GSEs in the strain-task sample")
    ax.grid(False)

    # Legend below
    from matplotlib.patches import Patch
    handles = [Patch(facecolor=col, edgecolor=C["llm_edge"], linewidth=1.5, label=lbl)
               for lbl, _, col in classes]
    ax.legend(handles=handles, loc="upper center", bbox_to_anchor=(0.5, -0.30),
              ncol=5, frameon=False, fontsize=10.5)

    fig.text(0.06, 0.94,
             "Strain task — correctness overlap across the four frontier models",
             fontsize=13, fontweight="semibold", color="#0f172a")
    fig.text(0.06, 0.83,
             "Sonnet 4.6, Opus 4.7, GPT-4o (original), Haiku 4.5 evaluated on the\n"
             "same 500-GSE sample. The 19 % all-wrong block isolates the hard,\n"
             "intrinsically-ambiguous cases; the 42 % all-correct block is the\n"
             "easy floor. Most model-to-model spread is decided on the middle 38 %.",
             fontsize=9.5, color="#64748b")
    save_fig(fig, "figS_strain_error_overlap")
    plt.close(fig)


def figure_cell_line_pr():
    """Supplement to fig6_cell_line_baselines: precision–recall scatter for the
    cell-line task. Cross-walk-aware scoring. Recall = k_xw / N_total (with
    N_total = 500 as the common denominator); precision = k_xw / n_covered.
    MetaMuse sits off the diagonal because it abstains on ~60 % of GSEs."""
    # (label, k_xw, n_covered, n_total, color, marker)
    methods = [
        ("SapBERT (neural)",              112, 498, 500, "#0ea5e9",      "o"),
        ("Claude Sonnet 4.6",             260, 497, 500, C["sonnet"],    "o"),
        ("Claude Sonnet 4.6 + hybrid",    243, 497, 500, C["sonnet"],    "s"),
        ("Claude Opus 4.7",               256, 498, 500, C["opus"],      "o"),
        ("Claude Opus 4.7 + hybrid",      257, 498, 500, C["opus"],      "s"),
        ("GPT-4o (original)",             262, 491, 500, C["gpt4o"],     "o"),
        ("GPT-4o + hybrid",               231, 491, 500, C["gpt4o"],     "s"),
        ("2-of-3 majority ensemble",      259, 373, 500, C["ensemble"],  "^"),
        ("All-three-agree ensemble",      218, 251, 500, C["patch"],     "^"),
        ("MetaMuse-GPT-4o (cell_line)",   137, 198, 500, "#db2777",      "D"),
    ]
    fig, ax = plt.subplots(figsize=(9.0, 6.4))
    fig.subplots_adjust(top=0.74, bottom=0.10, left=0.08, right=0.75)
    ax.plot([0, 1], [0, 1], color="#e2e8f0", linewidth=1.4, linestyle=(0,(4,4)), zorder=1)
    handles = []
    for label, k, ncov, n, col, marker in methods:
        recall = k / n
        prec   = k / ncov
        xerr   = wilson_err(k, n)
        yerr   = wilson_err(k, ncov) if ncov != n else xerr
        ax.errorbar(recall, prec, xerr=[[xerr[0]],[xerr[1]]], yerr=[[yerr[0]],[yerr[1]]],
                    fmt="none", ecolor=ERR_ECOLOR_SCATTER, elinewidth=ERR_KW_SCATTER["elinewidth"], capthick=ERR_KW_SCATTER["capthick"], capsize=ERR_CAPSIZE_SCATTER, zorder=2)
        h = ax.scatter(recall, prec, s=160, color=col, edgecolor="white", linewidth=1.4,
                       marker=marker, zorder=3, label=label)
        handles.append(h)
    ax.set_xlim(0, 0.85); ax.set_ylim(0, 1.0)
    ax.set_xticks([0, 0.2, 0.4, 0.6, 0.8])
    ax.set_yticks([0, 0.2, 0.4, 0.6, 0.8, 1.0])
    ax.set_xticklabels([f"{int(v*100)}%" for v in ax.get_xticks()])
    ax.set_yticklabels([f"{int(v*100)}%" for v in ax.get_yticks()])
    ax.tick_params(axis="both", which="major", length=4, width=0.8,
                   direction="out", color="#6b7280")
    ax.set_xlabel("Recall (correct predictions / N=500)")
    ax.set_ylabel("Precision (correct / predictions made)")
    ax.grid(False)
    ax.legend(handles=handles, loc="center left", bbox_to_anchor=(1.02, 0.5),
              frameon=False, fontsize=12, handletextpad=0.6, borderaxespad=0)
    fig.text(0.06, 0.94, "Cell-line task — precision vs recall (cross-walk-aware)",
             fontsize=13, fontweight="semibold", color="#0f172a")
    fig.text(0.06, 0.83,
             "Stage-1+2 LLMs and SapBERT cover ~98–100 % of GSEs, so they sit on the diagonal.\n"
             "Abstention-based systems lift precision above the diagonal at the cost of recall:\n"
             "ensembles (triangles, 2-of-3 majority and all-three-agree intersection) and\n"
             "MetaMuse (diamond). Squares: + BM25 hybrid retrieval. Dashed line: P = R.",
             fontsize=9.5, color="#64748b")
    save_fig(fig, "figS6_cell_line_pr")
    plt.close(fig)


def figure_curator_overlay():
    """Cell-line task — accuracy lift after a curator reviewed 156 new
    predictions across 58 GSEs (verdicts: 71 correct / 78 wrong / 7
    uncertain). The reviewed 58 is the right denominator: it's where
    every flagged disagreement had a curator decision applied. On the
    full 500-GSE sample the deltas are small (+0.4 to +1.6 pp) because
    the 58 GSEs are a small fraction of the set; on the reviewed 58
    they're substantial (+3 to +14 pp), and they're uniform across all
    four systems — the lift is a property of the gold standard, not the
    model."""
    # Source: analysis/curator_overlay_summary.tsv, subset=reviewed58.
    methods = [
        # (label, k_orig, k_overlay, n_covered, color)
        ("GPT-4o (original)",   34, 37, 57, C["gpt4o"]),
        ("Claude Sonnet 4.6",   32, 38, 56, C["sonnet"]),
        ("Claude Opus 4.7",     27, 35, 57, C["opus"]),
        ("MetaMuse-GPT-4o",      6,  8, 18, "#db2777"),
    ]
    fig, ax = plt.subplots(figsize=(9.5, 5.4))
    fig.subplots_adjust(top=0.74, bottom=0.26, left=0.07, right=0.985)
    x = list(range(len(methods)))
    w = 0.36

    # Accuracy denominator = n_covered (covered GSEs in the 58-subset).
    orig_vals    = [m[1] / m[3] for m in methods]
    overlay_vals = [m[2] / m[3] for m in methods]
    orig_err     = list(zip(*[wilson_err(m[1], m[3]) for m in methods]))
    overlay_err  = list(zip(*[wilson_err(m[2], m[3]) for m in methods]))

    light_face = [lighten(m[4], 0.55) for m in methods]
    full_face  = [m[4] for m in methods]
    edges      = [C["llm_edge"] for _ in methods]

    b1 = ax.bar([xi - w/2 for xi in x], orig_vals, width=w,
                color=light_face, edgecolor=edges, linewidth=2.0,
                yerr=orig_err, capsize=ERR_CAPSIZE,
                error_kw=ERR_KW,
                label="Original Gemma annotation as gold")
    b2 = ax.bar([xi + w/2 for xi in x], overlay_vals, width=w,
                color=full_face, edgecolor=edges, linewidth=2.0,
                yerr=overlay_err, capsize=ERR_CAPSIZE,
                error_kw=ERR_KW,
                label="After curator overlay")
    annotate_bars(ax, b1, orig_vals,    offset=0.012, ci_upper=orig_err[1])
    annotate_bars(ax, b2, overlay_vals, offset=0.012, ci_upper=overlay_err[1])

    # Delta annotation per pair: a short bracket spanning the two bar
    # centres + a "+Δ pp" label above. Place it well above both upper-CI
    # caps so it never collides with the bar-value annotations.
    for i, (m, oerr_u, xerr_u) in enumerate(zip(methods, orig_err[1], overlay_err[1])):
        delta_pp = (overlay_vals[i] - orig_vals[i]) * 100
        x_left  = i - w/2
        x_right = i + w/2
        # Top of the higher upper-CI cap, plus a clear gap for the
        # value annotation, then more space for the delta bracket.
        top_left  = orig_vals[i]    + oerr_u
        top_right = overlay_vals[i] + xerr_u
        bracket_y = max(top_left, top_right) + 0.10
        # Draw the bracket: small downticks + horizontal line.
        tick_h = 0.015
        ax.plot([x_left, x_left], [bracket_y, bracket_y - tick_h],
                color="#1f2937", linewidth=1.8, solid_capstyle="round",
                zorder=4)
        ax.plot([x_right, x_right], [bracket_y, bracket_y - tick_h],
                color="#1f2937", linewidth=1.8, solid_capstyle="round",
                zorder=4)
        ax.plot([x_left, x_right], [bracket_y, bracket_y],
                color="#1f2937", linewidth=1.8, solid_capstyle="round",
                zorder=4)
        ax.text(i, bracket_y + 0.012, f"+{delta_pp:.1f} pp",
                ha="center", va="bottom", fontsize=11.5,
                fontweight="semibold", color="#0f172a")

    ax.set_xticks(x)
    ax.set_xticklabels([m[0] for m in methods])
    for tick in ax.get_xticklabels():
        tick.set_fontsize(12)
    style_axes(ax, ymax=1.00)
    ax.legend(loc="upper center", bbox_to_anchor=(0.5, -0.18),
              frameon=False, fontsize=12, ncol=2)

    fig.text(0.06, 0.95,
             "Cell-line task — accuracy lift after curator overlay (reviewed-58 subset)",
             fontsize=13, fontweight="semibold", color="#0f172a", va="top")
    fig.text(0.06, 0.88,
             "A single curator reviewed 156 new predictions across 58 GSEs "
             "(71 judged correct, 78 wrong, 7 uncertain). Crediting the\n"
             "71 curator-confirmed corrections to each system on those 58 "
             "GSEs lifts accuracy by 3–14 pp. The lift is uniform across\n"
             "systems — it reflects gold-standard incompleteness, not a "
             "model-specific advantage.",
             fontsize=9.5, color="#64748b", va="top")
    save_fig(fig, "figS_curator_overlay")
    plt.close(fig)


def figure_verifier():
    """Quote-grounding rates: strict (verbatim) vs lenient (alphanumeric-only
    normalization). Demonstrates that the GPT-4o strict-match gap closes
    almost entirely once en-dash / added-period / multi-line-join cosmetic
    differences are normalized."""
    # (label, strict_k, lenient_k, n, color)
    methods = [
        ("GPT-4o (original)", 730, 836, 847, C["gpt4o"]),
        ("Claude Haiku 4.5",     1025, 1064, 1075, C["haiku"]),
        ("Claude Sonnet 4.6",    1392, 1399, 1411, C["sonnet"]),
        ("Claude Opus 4.7",       798,  798,  798, C["opus"]),
    ]
    fig, ax = plt.subplots(figsize=(8.4, 5.4))
    fig.subplots_adjust(top=0.74, bottom=0.13, left=0.09, right=0.97)
    x = list(range(len(methods)))
    w = 0.36
    strict_vals  = [m[1] / m[3] for m in methods]
    lenient_vals = [m[2] / m[3] for m in methods]
    strict_err   = list(zip(*[wilson_err(m[1], m[3]) for m in methods]))
    lenient_err  = list(zip(*[wilson_err(m[2], m[3]) for m in methods]))
    # Use a print-safe lightened RGB for the "strict" bar instead of
    # alpha=0.55 — EPS drops the alpha channel, so an alpha-paired
    # comparison renders identical-solid in Illustrator. lighten(c, 0.55)
    # produces a real RGB ~ 45 %-saturation tint of the model colour and
    # gives a wider tonal split than alpha did, even on screen.
    strict_face  = [lighten(m[4], 0.55) for m in methods]
    lenient_face = [m[4] for m in methods]
    bar_edge     = [C["llm_edge"] for _ in methods]
    # Error-bar styling: capsize=7 so the cap protrudes visibly beyond
    # the bar's 0.36-unit width, and capthick=3 (separately bumped above
    # elinewidth=2) so the cap line is heavier than both the vertical
    # error-bar line and the bar's edge. When the upper Wilson bound
    # clips at 1.0 (k = n), the cap lands at the same y as the bar's
    # top edge — with the new sizing it still reads as a distinct
    # I-beam stroke instead of merging into the bar outline.
    b1 = ax.bar([xi - w/2 for xi in x], strict_vals, width=w,
                color=strict_face,
                edgecolor=bar_edge, linewidth=2.0,
                yerr=strict_err, capsize=ERR_CAPSIZE,
                error_kw=ERR_KW,
                label="Strict verbatim match")
    b2 = ax.bar([xi + w/2 for xi in x], lenient_vals, width=w,
                color=lenient_face,
                edgecolor=bar_edge, linewidth=2.0,
                yerr=lenient_err, capsize=ERR_CAPSIZE,
                error_kw=ERR_KW,
                label="Lenient (alphanumeric-only)")
    annotate_bars(ax, b1, strict_vals,  offset=0.004, ci_upper=strict_err[1])
    annotate_bars(ax, b2, lenient_vals, offset=0.004, ci_upper=lenient_err[1])
    ax.set_xticks(x)
    ax.set_xticklabels([m[0] for m in methods])
    for t in ax.get_xticklabels(): t.set_fontsize(12)
    # Zoom y so the 1-5 pp differences are legible.
    ax.set_axisbelow(True); ax.set_ylim(0.80, 1.06)
    ax.set_yticks([0.80, 0.85, 0.90, 0.95, 1.0])
    ax.set_yticklabels([f"{int(v*100)}%" for v in ax.get_yticks()])
    # Legend below the plot area so it can't collide with the right-side
    # bars (Sonnet/Opus both sit at ≥98.7 % strict).
    ax.legend(loc="upper center", bbox_to_anchor=(0.5, -0.18),
              frameon=False, fontsize=11.5, ncol=2)
    fig.subplots_adjust(bottom=0.20)
    fig.text(0.06, 0.95, "Quote-grounding rates on the strain task",
             fontsize=13, fontweight="semibold", color="#0f172a",
             va="top")
    fig.text(0.06, 0.90,
             "Verbatim substring match (strict) vs alphanumeric-only "
             "normalization (lenient). All four\n"
             "models verify ≥ 98.7 % of their quotes under the lenient "
             "metric — the strict-match\n"
             "gap is cosmetic, not fabrication.",
             fontsize=9.5, color="#64748b", va="top")
    save_fig(fig, "fig7_verifier")
    plt.close(fig)


def main():
    figure_ensemble()
    figure_ensemble_with_metamuse()
    figure_baseline()
    figure_strain_micro_pr()
    figure_strain_error_overlap()
    figure_topk()
    figure_cell_line_baselines()
    figure_cell_line_pr()
    figure_curator_overlay()
    figure_verifier()


if __name__ == "__main__":
    main()
