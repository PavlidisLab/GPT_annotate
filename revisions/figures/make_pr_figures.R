#!/usr/bin/env Rscript
#
# ggplot2 versions of figS4_strain_pr and figS6_cell_line_pr.
# Follows lab style (plotting skill convention): no gridlines, ticks visible,
# theme_classic base, top+right spines off, Helvetica.
# Outputs sit alongside the matplotlib versions in revisions/figures/ with the
# ".ggplot" suffix.

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggrepel)
})

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

# ---- Wilson 95 % CI -----------------------------------------------------
wilson_ci <- function(k, n, z = 1.96) {
  if (n == 0) return(c(0, 0))
  p <- k / n
  denom <- 1 + z * z / n
  center <- (p + z * z / (2 * n)) / denom
  half   <- z * sqrt(p * (1 - p) / n + z * z / (4 * n * n)) / denom
  c(max(0, center - half), min(1, center + half))
}

# Add Wilson lo/hi columns for recall (k, n_total) and precision (k, n_covered)
add_wilson <- function(df) {
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

# ---- Strain PR (figS4) --------------------------------------------------
strain <- data.frame(
  label   = c("text2term TFIDF","Regex (original)","BM25 top-1",
              "Llama 3.3 70B","Claude Haiku 4.5","SapBERT (neural)",
              "Llama 3.3 70B + spec","GPT-4o (original)",
              "Claude Sonnet 4.6","Claude Opus 4.7",
              "Sonnet 4.6 + spec","GPT-4o + spec"),
  k         = c(27, 32, 103, 156, 237, 296, 331, 360, 368, 377, 384, 385),
  n_covered = c(375, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500),
  n_total   = rep(500, 12),
  color     = c("#cbd5e1","#cbd5e1","#cbd5e1","#8b5cf6","#f59e0b","#0ea5e9",
                "#8b5cf6","#94a3b8","#3b82f6","#6366f1","#10b981","#10b981"),
  stringsAsFactors = FALSE
)
strain <- add_wilson(strain)
strain$label <- factor(strain$label, levels = strain$label)
strain_cols <- setNames(strain$color, as.character(strain$label))

p_strain <- ggplot(strain, aes(x = recall, y = precision, color = label)) +
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
  scale_color_manual(values = strain_cols, guide = "none") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                     labels = pct_label, expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                     labels = pct_label, expand = c(0.01, 0.01)) +
  labs(title    = "Strain task — precision vs recall",
       subtitle = paste0("Strain systems generally always produce a prediction, so they sit on the diagonal\n",
                         "(precision = recall = accuracy). text2term abstains ~25 % at its threshold,\n",
                         "lifting its precision above the diagonal at low recall."),
       caption  = "Dashed line: precision = recall. n=500 throughout.",
       x        = "Recall (correct predictions / N=500)",
       y        = "Precision (correct / predictions made)") +
  theme_pavlab(base_size = 11)

ggsave(file.path(FIG_DIR, "figS4_strain_pr.ggplot.svg"),
       p_strain, width = 8.4, height = 6.4, units = "in")
ggsave(file.path(FIG_DIR, "figS4_strain_pr.ggplot.png"),
       p_strain, width = 8.4, height = 6.4, units = "in", dpi = 200)

# ---- Cell-line PR (figS6) -----------------------------------------------
cell <- data.frame(
  label   = c("SapBERT (neural)","Claude Sonnet 4.6","Claude Sonnet 4.6 + hybrid",
              "Claude Opus 4.7","Claude Opus 4.7 + hybrid",
              "GPT-4o (original)","GPT-4o + hybrid",
              "2-of-3 majority ensemble","All-three-agree ensemble",
              "MetaMuse-GPT-4o (cell_line)"),
  k         = c(117, 260, 243, 256, 257, 262, 231, 259, 218, 137),
  n_covered = c(498, 497, 497, 498, 498, 491, 491, 373, 251, 198),
  n_total   = c(500, 500, 500, 500, 500, 500, 500, 500, 500, 500),
  color     = c("#0ea5e9","#3b82f6","#3b82f6","#6366f1","#6366f1",
                "#94a3b8","#94a3b8","#059669","#10b981","#f59e0b"),
  shape     = c(16, 16, 15, 16, 15, 16, 15, 17, 17, 18),  # circle / square / triangle / diamond
  stringsAsFactors = FALSE
)
cell <- add_wilson(cell)
cell$label <- factor(cell$label, levels = cell$label)
cell_cols   <- setNames(cell$color, as.character(cell$label))
cell_shapes <- setNames(cell$shape, as.character(cell$label))

p_cell <- ggplot(cell, aes(x = recall, y = precision,
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
                  min.segment.length = 0,           # always draw a callout
                  box.padding   = 0.9,
                  point.padding = 0.4,
                  force         = 8,
                  force_pull    = 0.3,
                  max.overlaps  = Inf,
                  max.iter      = 20000,
                  seed          = 7) +
  scale_color_manual(values = cell_cols,   guide = "none") +
  scale_shape_manual(values = cell_shapes, guide = "none") +
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

ggsave(file.path(FIG_DIR, "figS6_cell_line_pr.ggplot.svg"),
       p_cell, width = 9.0, height = 6.4, units = "in")
ggsave(file.path(FIG_DIR, "figS6_cell_line_pr.ggplot.png"),
       p_cell, width = 9.0, height = 6.4, units = "in", dpi = 200)

message("wrote figS4_strain_pr.ggplot.{svg,png}")
message("wrote figS6_cell_line_pr.ggplot.{svg,png}")
