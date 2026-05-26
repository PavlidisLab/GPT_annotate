#!/usr/bin/env Rscript
#
# ggplot2 versions of every figure produced by revisions/make_figures.py.
# Follows lab style (plotting skill): no gridlines, ticks visible (gray-500
# outward), top+right spines off, theme_classic base, Helvetica family.
#
# Outputs sit alongside the matplotlib originals in revisions/figures/ with
# the ".ggplot" suffix:
#   fig3_ensemble.ggplot.{svg,png}
#   fig3_ensemble_metamuse.ggplot.{svg,png}
#   fig4_baseline.ggplot.{svg,png}
#   fig5_topk_sensitivity.ggplot.{svg,png}
#   fig6_cell_line_baselines.ggplot.{svg,png}
#   fig7_verifier.ggplot.{svg,png}
#   figS4_strain_pr.ggplot.{svg,png}
#   figS6_cell_line_pr.ggplot.{svg,png}

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggrepel)
})

# ---- FIG_DIR resolution -------------------------------------------------
script_path <- tryCatch(
  normalizePath(sys.frame(1)$ofile, mustWork = FALSE),
  error = function(e) NULL
)
if (is.null(script_path) || !nzchar(script_path)) {
  args <- commandArgs(trailingOnly = FALSE)
  m <- regmatches(args, regexpr("(?<=^--file=).*", args, perl = TRUE))
  script_path <- if (length(m) > 0) m[1] else getwd()
}
FIG_DIR <- if (file.exists(script_path)) normalizePath(dirname(script_path)) else getwd()

save_fig <- function(p, stem, w, h) {
  ggsave(file.path(FIG_DIR, paste0(stem, ".ggplot.svg")), p, width = w, height = h, units = "in")
  ggsave(file.path(FIG_DIR, paste0(stem, ".ggplot.png")), p, width = w, height = h, units = "in", dpi = 200)
  message(sprintf("wrote %s.ggplot.{svg,png}", stem))
}

# ---- Palette (matches the matplotlib C dict) ----------------------------
C <- list(
  gpt4o    = "#94a3b8",
  sonnet   = "#3b82f6",
  opus     = "#6366f1",
  haiku    = "#f59e0b",
  patch    = "#10b981",
  ensemble = "#059669",
  text2term= "#dc2626",
  llama    = "#8b5cf6",
  annot    = "#1f2937",
  metamuse = "#f59e0b",
  sapbert  = "#0ea5e9",
  neutral  = "#cbd5e1"
)

# ---- Wilson 95 % CI -----------------------------------------------------
wilson_ci <- function(k, n, z = 1.96) {
  if (n == 0) return(c(0, 0))
  p <- k / n
  denom <- 1 + z * z / n
  center <- (p + z * z / (2 * n)) / denom
  half   <- z * sqrt(p * (1 - p) / n + z * z / (4 * n * n)) / denom
  c(max(0, center - half), min(1, center + half))
}

add_wilson <- function(df) {
  ci <- t(mapply(wilson_ci, df$k, df$n))
  df$lo <- ci[, 1]; df$hi <- ci[, 2]
  df$val <- df$k / df$n
  df
}

add_wilson_pr <- function(df) {
  rci <- t(mapply(wilson_ci, df$k, df$n_total))
  pci <- t(mapply(wilson_ci, df$k, df$n_covered))
  df$recall_lo    <- rci[, 1]; df$recall_hi    <- rci[, 2]
  df$precision_lo <- pci[, 1]; df$precision_hi <- pci[, 2]
  df$recall    <- df$k / df$n_total
  df$precision <- df$k / df$n_covered
  df
}

# ---- Lab theme ----------------------------------------------------------
theme_pavlab <- function(base_size = 11) {
  theme_classic(base_size = base_size) +
    theme(
      axis.title        = element_text(size = base_size,     color = "#1f2937"),
      axis.text         = element_text(size = base_size - 1, color = "#6b7280"),
      axis.ticks        = element_line(color = "#6b7280", linewidth = 0.4),
      axis.ticks.length = unit(3, "pt"),
      axis.line         = element_line(color = "#475569", linewidth = 0.4),
      plot.title        = element_text(size = base_size + 2, color = "#0f172a",
                                       hjust = 0, face = "bold"),
      plot.subtitle     = element_text(size = base_size - 1, color = "#64748b",
                                       hjust = 0),
      plot.caption      = element_text(size = base_size - 2, color = "#6b7280",
                                       hjust = 0),
      legend.text       = element_text(size = base_size - 1),
      legend.title      = element_blank(),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      panel.grid        = element_blank(),
      plot.margin       = margin(8, 12, 8, 8, "pt")
    )
}

pct_label <- function(x) paste0(round(x * 100), "%")
pct_label1 <- function(x) sprintf("%.1f%%", 100 * x)


# =========================================================================
# fig3 — ensemble (precision vs coverage)
# =========================================================================
make_fig3_ensemble <- function(include_metamuse = FALSE) {
  pts <- data.frame(
    label = c("Sonnet 4.6 alone", "Opus 4.7 alone", "GPT-4o alone",
              "2-of-3 majority", "All three agree"),
    cov_k = c(497, 497, 497, 373, 251),
    cov_n = c(497, 497, 497, 497, 497),
    prec_k = c(260, 256, 261, 259, 218),
    prec_n = c(497, 497, 497, 373, 251),
    color  = c(C$sonnet, C$opus, C$gpt4o, C$ensemble, C$patch),
    shape  = c(16, 16, 16, 16, 16),
    stringsAsFactors = FALSE
  )
  if (include_metamuse) {
    pts <- rbind(pts, data.frame(
      label = "MetaMuse-GPT-4o (cell_line)",
      cov_k = 198, cov_n = 498, prec_k = 137, prec_n = 198,
      color = C$metamuse, shape = 18,
      stringsAsFactors = FALSE
    ))
  }
  pts$coverage  <- pts$cov_k / pts$cov_n
  pts$precision <- pts$prec_k / pts$prec_n
  ci_cov  <- t(mapply(wilson_ci, pts$cov_k,  pts$cov_n))
  ci_prec <- t(mapply(wilson_ci, pts$prec_k, pts$prec_n))
  pts$cov_lo  <- ci_cov[, 1];  pts$cov_hi  <- ci_cov[, 2]
  pts$prec_lo <- ci_prec[, 1]; pts$prec_hi <- ci_prec[, 2]
  pts$label <- factor(pts$label, levels = pts$label)
  cols   <- setNames(pts$color, as.character(pts$label))
  shapes <- setNames(pts$shape, as.character(pts$label))

  # Connector arrows: GPT-4o-alone -> 2-of-3 -> all-three-agree
  arrows <- data.frame(
    x = c(1.000, 0.751), y = c(0.521, 0.694),
    xend = c(0.751, 0.505), yend = c(0.694, 0.869)
  )

  x_min <- if (include_metamuse) 0.32 else 0.42
  p <- ggplot(pts, aes(x = coverage, y = precision,
                       color = label, shape = label)) +
    geom_segment(data = arrows, inherit.aes = FALSE,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(length = unit(2.5, "mm"), type = "closed"),
                 color = "#cbd5e1", linewidth = 0.4) +
    geom_errorbar(aes(ymin = prec_lo, ymax = prec_hi),
                  color = "#cbd5e1", linewidth = 0.4, width = 0.012) +
    geom_errorbarh(aes(xmin = cov_lo, xmax = cov_hi),
                   color = "#cbd5e1", linewidth = 0.4, height = 0.012) +
    geom_point(size = 4, stroke = 0.7) +
    geom_text_repel(aes(label = label),
                    color = "#1f2937", size = 3.3,
                    segment.color = "#94a3b8", segment.size = 0.3,
                    min.segment.length = 0,
                    box.padding   = 0.55,
                    point.padding = 0.35,
                    force = 4, force_pull = 0.4,
                    max.overlaps = Inf, seed = 1) +
    scale_color_manual(values = cols,   guide = "none") +
    scale_shape_manual(values = shapes, guide = "none") +
    scale_x_continuous(limits = c(x_min, 1.05), breaks = seq(0.4, 1.0, 0.1),
                       labels = pct_label, expand = c(0.01, 0.01)) +
    scale_y_continuous(limits = c(0.42, 1.0), breaks = seq(0.5, 1.0, 0.1),
                       labels = pct_label, expand = c(0.01, 0.01)) +
    labs(
      title    = if (include_metamuse)
                   "Cell-line ensemble — precision vs coverage (with MetaMuse)"
                 else
                   "Cell-line ensemble — precision vs coverage",
      subtitle = if (include_metamuse)
                   paste0("MetaMuse (diamond) is a single-LLM agentic system with internal SapBERT/\n",
                          "arbitrator abstention; it lands on the same precision–coverage frontier as\n",
                          "our external ensemble. Stage-1+2 systems: n=497. MetaMuse: n=498.")
                 else
                   paste0("Independent frontier models converge less often on cell lines than on strains;\n",
                          "intersecting their predictions trades coverage for precision."),
      x = "Coverage (fraction of experiments with a prediction)",
      y = "Precision (within covered experiments)"
    ) +
    theme_pavlab(base_size = 11)

  save_fig(p, if (include_metamuse) "fig3_ensemble_metamuse" else "fig3_ensemble",
           w = 8.0, h = 5.8)
  invisible(p)
}


# =========================================================================
# fig4 — strain task baseline bars
# =========================================================================
make_fig4_baseline <- function() {
  m <- data.frame(
    label = c("text2term TFIDF","Regex (original)","BM25 top-1",
              "Llama 3.3 70B","Claude Haiku 4.5","SapBERT (neural)",
              "Llama 3.3 70B + spec","GPT-4o (original)",
              "Claude Sonnet 4.6","Claude Opus 4.7",
              "Sonnet 4.6 + spec-rule","GPT-4o + spec-rule"),
    k = c(27, 32, 103, 156, 237, 296, 331, 360, 368, 377, 384, 385),
    n = rep(500, 12),
    color = c(C$neutral, C$neutral, C$neutral, C$llama, C$haiku, C$sapbert,
              C$llama, C$gpt4o, C$sonnet, C$opus, C$patch, C$patch),
    stringsAsFactors = FALSE
  )
  m <- add_wilson(m)
  m$label <- factor(m$label, levels = m$label)
  cols <- setNames(m$color, as.character(m$label))

  p <- ggplot(m, aes(x = label, y = val, fill = label)) +
    geom_col(width = 0.65) +
    geom_errorbar(aes(ymin = lo, ymax = hi),
                  color = "#475569", linewidth = 0.4, width = 0.25) +
    geom_text(aes(y = hi, label = pct_label1(val)),
              vjust = -0.7, size = 3.2, color = "#1f2937") +
    scale_fill_manual(values = cols, guide = "none") +
    scale_y_continuous(limits = c(0, 0.92), breaks = seq(0, 0.8, 0.2),
                       labels = pct_label, expand = c(0, 0)) +
    labs(
      title    = "Strain accuracy: regex / TFIDF / neural baselines vs frontier LLMs",
      subtitle = paste0("Twelve methods evaluated on the same 500-experiment sample (Wilson 95 % CIs).\n",
                        "Open-weights bars in violet; spec-rule prompt variants in green."),
      x = NULL, y = NULL
    ) +
    theme_pavlab(base_size = 11) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))

  save_fig(p, "fig4_baseline", w = 12.0, h = 5.4)
  invisible(p)
}


# =========================================================================
# fig5 — top-K retrieval sensitivity
# =========================================================================
make_fig5_topk <- function() {
  ks <- c(10, 25, 50, 100, 200)
  n_topk <- 100
  exact_k <- c(19, 32, 35, 37, 36)
  name_k  <- c(27, 35, 37, 40, 39)
  exact_ci <- t(mapply(wilson_ci, exact_k, rep(n_topk, length(ks))))
  name_ci  <- t(mapply(wilson_ci, name_k,  rep(n_topk, length(ks))))
  df <- rbind(
    data.frame(K = ks, val = exact_k / n_topk,
               lo = exact_ci[,1], hi = exact_ci[,2],
               metric = "Exact-ID match"),
    data.frame(K = ks, val = name_k / n_topk,
               lo = name_ci[,1],  hi = name_ci[,2],
               metric = "Cross-walk name match")
  )
  df$metric <- factor(df$metric, levels = c("Cross-walk name match", "Exact-ID match"))

  p <- ggplot(df, aes(x = K, y = val, color = metric, group = metric)) +
    geom_vline(xintercept = 50, linetype = "dashed",
               color = "#94a3b8", linewidth = 0.4) +
    annotate("text", x = 52, y = 0.06, label = "K = 50 (paper)",
             color = "#475569", size = 3.0, hjust = 0) +
    geom_line(linewidth = 0.8) +
    geom_errorbar(aes(ymin = lo, ymax = hi),
                  color = "#cbd5e1", linewidth = 0.4, width = 0.06) +
    geom_point(size = 3, stroke = 0.5, shape = 16) +
    geom_text(data = subset(df, metric == "Cross-walk name match"),
              aes(label = pct_label(val), y = hi),
              vjust = -0.9, size = 3.0, color = "#1f2937", show.legend = FALSE) +
    scale_color_manual(values = c("Cross-walk name match" = C$sonnet,
                                  "Exact-ID match"        = C$opus),
                       name = NULL) +
    scale_x_log10(breaks = ks, labels = ks) +
    scale_y_continuous(limits = c(0, 0.55), breaks = seq(0, 0.5, 0.1),
                       labels = pct_label, expand = c(0, 0)) +
    labs(
      title    = "Cell-line accuracy vs retrieval window K",
      subtitle = "Sonnet 4.6, 100-experiment subset",
      x = "Retrieval window K (log scale)",
      y = "Accuracy"
    ) +
    theme_pavlab(base_size = 11) +
    theme(legend.position = c(0.97, 0.05),
          legend.justification = c(1, 0))

  save_fig(p, "fig5_topk_sensitivity", w = 7.4, h = 5.4)
  invisible(p)
}


# =========================================================================
# fig6 — cell-line task paired bars (exact + cross-walk)
# Now with a lighter-alpha "curator overlay" extension stacked on top of
# each bar — see analysis/fig6_overlay_scores.tsv. Color logic:
#   hue   = system identity (Sonnet blue, Opus indigo, GPT-4o gray, SapBERT
#           sky)
#   alpha = scoring strictness:
#               exact base    → 0.45
#               exact overlay → 0.25 (extension)
#               xw base       → 1.00
#               xw overlay    → 0.45 (extension)
# =========================================================================
make_fig6_cell_line <- function() {
  ov_path <- file.path(dirname(FIG_DIR), "..", "analysis", "fig6_overlay_scores.tsv")
  ov_path <- normalizePath(ov_path, mustWork = FALSE)
  ov <- if (file.exists(ov_path)) read.delim(ov_path, stringsAsFactors = FALSE) else NULL

  m <- data.frame(
    label_key = c("SapBERT (neural)",
                  "Claude Sonnet 4.6", "Claude Sonnet 4.6 + hybrid",
                  "Claude Opus 4.7",   "Claude Opus 4.7 + hybrid",
                  "GPT-4o (original)", "GPT-4o + hybrid"),
    label = c("SapBERT (neural)",
              "Claude Sonnet 4.6", "Claude Sonnet 4.6\n+ hybrid",
              "Claude Opus 4.7",   "Claude Opus 4.7\n+ hybrid",
              "GPT-4o (original)", "GPT-4o\n+ hybrid"),
    k_exact = c(61, 81, 213, 198, 226, 95, 212),
    k_xw    = c(117, 260, 243, 256, 257, 262, 231),
    n       = c(498, 497, 497, 498, 498, 491, 491),
    color   = c(C$sapbert, C$sonnet, C$sonnet, C$opus, C$opus, C$gpt4o, C$gpt4o),
    stringsAsFactors = FALSE
  )
  # Attach overlay numbers
  if (!is.null(ov)) {
    rownames(ov) <- ov$system_label
    m$k_exact_ov <- ov[m$label_key, "k_exact_overlay"]
    m$k_xw_ov    <- ov[m$label_key, "k_xw_overlay"]
  } else {
    m$k_exact_ov <- m$k_exact
    m$k_xw_ov    <- m$k_xw
  }
  m$exact_val      <- m$k_exact    / m$n
  m$exact_val_ov   <- m$k_exact_ov / m$n
  m$xw_val         <- m$k_xw       / m$n
  m$xw_val_ov      <- m$k_xw_ov    / m$n
  ec <- t(mapply(wilson_ci, m$k_exact, m$n))
  xc <- t(mapply(wilson_ci, m$k_xw,    m$n))
  m$exact_lo <- ec[,1]; m$exact_hi <- ec[,2]
  m$xw_lo    <- xc[,1]; m$xw_hi    <- xc[,2]
  m$label <- factor(m$label, levels = m$label)

  # Manual rect positions: 7 systems at x=1..7, paired exact (offset -0.20)
  # and cross-walk (offset +0.20). Bar half-width = 0.18.
  n_sys <- nrow(m)
  half  <- 0.18
  ex_off <- -0.20; xw_off <- +0.20

  bars <- data.frame(
    label = rep(m$label, 2),
    color = rep(m$color, 2),
    metric = factor(rep(c("Exact-ID match","+ cross-walk name match"),
                        each = n_sys),
                    levels = c("Exact-ID match","+ cross-walk name match")),
    x      = c(seq_len(n_sys) + ex_off, seq_len(n_sys) + xw_off),
    base   = c(m$exact_val,    m$xw_val),
    top_ov = c(m$exact_val_ov, m$xw_val_ov),
    lo     = c(m$exact_lo,     m$xw_lo),
    hi     = c(m$exact_hi,     m$xw_hi),
    base_alpha = c(rep(0.45, n_sys), rep(1.00, n_sys)),
    ext_alpha  = c(rep(0.25, n_sys), rep(0.45, n_sys)),
    stringsAsFactors = FALSE
  )
  bars$ext <- pmax(0, bars$top_ov - bars$base)
  cols <- setNames(m$color, as.character(m$label))

  p <- ggplot() +
    # Base segment (published value): system-color, metric-specific alpha
    geom_rect(data = bars,
              aes(xmin = x - half, xmax = x + half,
                  ymin = 0,        ymax = base,
                  fill = label, alpha = base_alpha)) +
    # Extension (curator-overlay credit): same hue, lighter alpha
    geom_rect(data = subset(bars, ext > 0),
              aes(xmin = x - half, xmax = x + half,
                  ymin = base, ymax = top_ov,
                  fill = label, alpha = ext_alpha)) +
    # Wilson CI on the published value
    geom_errorbar(data = bars,
                  aes(x = x, ymin = lo, ymax = hi),
                  color = "#475569", linewidth = 0.4, width = 0.14) +
    # Numeric labels at the overlay top
    geom_text(data = bars,
              aes(x = x, y = top_ov,
                  label = ifelse(top_ov > base,
                                 sprintf("%.1f%% (+%.0f)",
                                         100 * base,
                                         100 * (top_ov - base)),
                                 sprintf("%.1f%%", 100 * base))),
              vjust = -0.7, size = 2.9, color = "#1f2937") +
    scale_fill_manual(values = cols, guide = "none") +
    scale_alpha_identity() +
    scale_x_continuous(breaks = seq_len(n_sys),
                       labels = levels(m$label),
                       expand = expansion(add = c(0.5, 0.5))) +
    scale_y_continuous(limits = c(0, 0.84), breaks = seq(0, 0.8, 0.2),
                       labels = pct_label, expand = c(0, 0)) +
    labs(
      title    = "Cell-line accuracy: non-LLM neural baseline vs frontier LLMs",
      subtitle = paste0("Same 500-experiment cell-line sample (n = 491–498 with usable rows; Wilson 95 % CIs on published).\n",
                        "Left bar of each pair (lighter): Exact-ID match. Right bar (solid): + cross-walk name match.\n",
                        "Light-shaded extension on each bar = curator-overlay credit from 58 reviewed GSEs (delta in parens).\n",
                        "\"+ hybrid\": dense + BM25 + reciprocal rank fusion at retrieval. Same Stage-2 model and prompt."),
      x = NULL, y = NULL
    ) +
    theme_pavlab(base_size = 11) +
    theme(axis.text.x = element_text(size = 10))

  save_fig(p, "fig6_cell_line_baselines", w = 13.2, h = 6.0)
  invisible(p)
}


# =========================================================================
# fig7 — quote-grounding rates (strict vs lenient)
# =========================================================================
make_fig7_verifier <- function() {
  m <- data.frame(
    label = c("GPT-4o (original)","Claude Haiku 4.5","Claude Sonnet 4.6","Claude Opus 4.7"),
    strict_k  = c(730, 1025, 1392, 798),
    lenient_k = c(836, 1064, 1399, 798),
    n         = c(847, 1075, 1411, 798),
    color     = c(C$gpt4o, C$haiku, C$sonnet, C$opus),
    stringsAsFactors = FALSE
  )
  sc <- t(mapply(wilson_ci, m$strict_k,  m$n))
  lc <- t(mapply(wilson_ci, m$lenient_k, m$n))
  m$strict_val  <- m$strict_k  / m$n
  m$lenient_val <- m$lenient_k / m$n
  m$strict_lo   <- sc[,1]; m$strict_hi  <- sc[,2]
  m$lenient_lo  <- lc[,1]; m$lenient_hi <- lc[,2]
  m$label <- factor(m$label, levels = m$label)
  cols <- setNames(m$color, as.character(m$label))

  df <- rbind(
    data.frame(label = m$label, val = m$strict_val,  lo = m$strict_lo,  hi = m$strict_hi,
               metric = "Strict verbatim match"),
    data.frame(label = m$label, val = m$lenient_val, lo = m$lenient_lo, hi = m$lenient_hi,
               metric = "Lenient (alphanumeric-only)")
  )
  df$metric <- factor(df$metric, levels = c("Strict verbatim match", "Lenient (alphanumeric-only)"))

  p <- ggplot(df, aes(x = label, y = val, fill = label, alpha = metric,
                      group = metric)) +
    geom_col(position = position_dodge(width = 0.78), width = 0.7) +
    geom_errorbar(aes(ymin = lo, ymax = hi),
                  position = position_dodge(width = 0.78),
                  color = "#475569", linewidth = 0.4, width = 0.22) +
    geom_text(aes(y = hi, label = pct_label1(val)),
              position = position_dodge(width = 0.78),
              vjust = -0.7, size = 3.0, color = "#1f2937", show.legend = FALSE) +
    scale_fill_manual(values = cols, guide = "none") +
    scale_alpha_manual(values = c("Strict verbatim match" = 0.55,
                                  "Lenient (alphanumeric-only)" = 1.0),
                       name = NULL) +
    scale_y_continuous(limits = c(0.80, 1.02), breaks = seq(0.80, 1.0, 0.05),
                       labels = pct_label, expand = c(0, 0),
                       oob = scales::oob_keep) +
    coord_cartesian(ylim = c(0.80, 1.02)) +
    labs(
      title    = "Quote-grounding rates on the strain task",
      subtitle = paste0("Verbatim substring match (strict) vs alphanumeric-only normalisation (lenient).\n",
                        "All four models verify ≥ 98.7 % of their quotes under the lenient metric — the\n",
                        "strict-match gap is cosmetic, not fabrication."),
      x = NULL, y = NULL
    ) +
    theme_pavlab(base_size = 11) +
    theme(legend.position = c(0.98, 0.05),
          legend.justification = c(1, 0))

  save_fig(p, "fig7_verifier", w = 8.4, h = 5.4)
  invisible(p)
}


# =========================================================================
# figS4 — strain task PR scatter
# =========================================================================
make_figS4_strain_pr <- function() {
  strain <- data.frame(
    label   = c("text2term TFIDF","Regex (original)","BM25 top-1",
                "Llama 3.3 70B","Claude Haiku 4.5","SapBERT (neural)",
                "Llama 3.3 70B + spec","GPT-4o (original)",
                "Claude Sonnet 4.6","Claude Opus 4.7",
                "Sonnet 4.6 + spec","GPT-4o + spec"),
    k         = c(27, 32, 103, 156, 237, 296, 331, 360, 368, 377, 384, 385),
    n_covered = c(375, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500),
    n_total   = rep(500, 12),
    color     = c(C$neutral, C$neutral, C$neutral, C$llama, C$haiku, C$sapbert,
                  C$llama, C$gpt4o, C$sonnet, C$opus, C$patch, C$patch),
    stringsAsFactors = FALSE
  )
  strain <- add_wilson_pr(strain)
  strain$label <- factor(strain$label, levels = strain$label)
  cols <- setNames(strain$color, as.character(strain$label))

  p <- ggplot(strain, aes(x = recall, y = precision, color = label)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                color = "#e2e8f0", linewidth = 0.5) +
    geom_errorbar(aes(ymin = precision_lo, ymax = precision_hi),
                  color = "#cbd5e1", linewidth = 0.4, width = 0.012) +
    geom_errorbarh(aes(xmin = recall_lo, xmax = recall_hi),
                   color = "#cbd5e1", linewidth = 0.4, height = 0.012) +
    geom_point(size = 3.2, shape = 16) +
    geom_text_repel(aes(label = label),
                    size = 3.2, color = "#1f2937",
                    segment.color = "#cbd5e1", segment.size = 0.3,
                    max.overlaps = Inf, min.segment.length = 0.2,
                    box.padding = 0.35, point.padding = 0.25,
                    seed = 1) +
    scale_color_manual(values = cols, guide = "none") +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                       labels = pct_label, expand = c(0.01, 0.01)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                       labels = pct_label, expand = c(0.01, 0.01)) +
    labs(title    = "Strain task — precision vs recall",
         subtitle = paste0("Strain systems generally always produce a prediction, so they sit on the\n",
                           "diagonal (precision = recall = accuracy). text2term abstains ~25 % at its\n",
                           "threshold, lifting its precision above the diagonal at low recall."),
         caption  = "Dashed line: precision = recall. n=500 throughout.",
         x        = "Recall (correct predictions / N=500)",
         y        = "Precision (correct / predictions made)") +
    theme_pavlab(base_size = 11)

  save_fig(p, "figS4_strain_pr", w = 8.4, h = 6.4)
  invisible(p)
}


# =========================================================================
# figS6 — cell-line PR scatter (with ensembles + MetaMuse)
# =========================================================================
make_figS6_cell_line_pr <- function() {
  cell <- data.frame(
    label   = c("SapBERT (neural)","Claude Sonnet 4.6","Claude Sonnet 4.6 + hybrid",
                "Claude Opus 4.7","Claude Opus 4.7 + hybrid",
                "GPT-4o (original)","GPT-4o + hybrid",
                "2-of-3 majority ensemble","All-three-agree ensemble",
                "MetaMuse-GPT-4o (cell_line)"),
    k         = c(117, 260, 243, 256, 257, 262, 231, 259, 218, 137),
    n_covered = c(498, 497, 497, 498, 498, 491, 491, 373, 251, 198),
    n_total   = c(500, 500, 500, 500, 500, 500, 500, 500, 500, 500),
    color     = c(C$sapbert, C$sonnet, C$sonnet, C$opus, C$opus,
                  C$gpt4o, C$gpt4o, C$ensemble, C$patch, C$metamuse),
    shape     = c(16, 16, 15, 16, 15, 16, 15, 17, 17, 18),
    stringsAsFactors = FALSE
  )
  cell <- add_wilson_pr(cell)
  cell$label <- factor(cell$label, levels = cell$label)
  cols   <- setNames(cell$color, as.character(cell$label))
  shapes <- setNames(cell$shape, as.character(cell$label))

  p <- ggplot(cell, aes(x = recall, y = precision,
                        color = label, shape = label)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                color = "#e2e8f0", linewidth = 0.5) +
    geom_errorbar(aes(ymin = precision_lo, ymax = precision_hi),
                  color = "#cbd5e1", linewidth = 0.4, width = 0.012) +
    geom_errorbarh(aes(xmin = recall_lo, xmax = recall_hi),
                   color = "#cbd5e1", linewidth = 0.4, height = 0.012) +
    geom_point(size = 3.6, stroke = 0.8) +
    geom_text_repel(aes(label = label),
                    color = "#1f2937", size = 3.2,
                    segment.color = "#94a3b8", segment.size = 0.35,
                    min.segment.length = 0,
                    box.padding   = 0.9,
                    point.padding = 0.4,
                    force         = 8,
                    force_pull    = 0.3,
                    max.overlaps  = Inf,
                    max.iter      = 20000,
                    seed          = 7) +
    scale_color_manual(values = cols,   guide = "none") +
    scale_shape_manual(values = shapes, guide = "none") +
    scale_x_continuous(limits = c(0, 0.90), breaks = seq(0, 0.8, 0.2),
                       labels = pct_label, expand = c(0.01, 0.01)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                       labels = pct_label, expand = c(0.01, 0.01)) +
    labs(title    = "Cell-line task — precision vs recall (cross-walk-aware)",
         subtitle = paste0("Stage-1+2 LLMs and SapBERT cover ~98–100 % of GSEs, so they sit on the diagonal.\n",
                           "Abstention-based systems lift precision above the diagonal at the cost of recall:\n",
                           "ensembles (triangles) and MetaMuse (diamond)."),
         caption  = "Squares: + BM25 hybrid retrieval. Dashed line: precision = recall.",
         x        = "Recall (correct predictions / N=500)",
         y        = "Precision (correct / predictions made)") +
    theme_pavlab(base_size = 11)

  save_fig(p, "figS6_cell_line_pr", w = 9.0, h = 6.4)
  invisible(p)
}


# =========================================================================
# main
# =========================================================================
make_fig3_ensemble(include_metamuse = FALSE)
make_fig3_ensemble(include_metamuse = TRUE)
make_fig4_baseline()
make_fig5_topk()
make_fig6_cell_line()
make_fig7_verifier()
make_figS4_strain_pr()
make_figS6_cell_line_pr()
