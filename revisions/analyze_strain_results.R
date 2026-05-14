# Reproducible analysis driver for the strain-annotation task.
#
# Reads per-model summary.tsv files written by run_sample.py and emits all
# tables shown in FINDINGS.md (sections 1-5), with Wilson 95% CIs on accuracy
# rates and McNemar's paired tests for pairwise comparisons. Writes machine-
# readable copies of every table into revisions/data/analysis/.
#
# Run with:   Rscript revisions/analyze_strain_results.R
#
# Inputs (must exist before running):
#   revisions/data/results/claude-sonnet-4-6/summary.tsv
#   revisions/data/results/claude-opus-4-7/summary.tsv
#   revisions/data/results/claude-haiku-4-5-20251001/summary.tsv
#   revisions/data/results/claude-sonnet-4-6_specprompt/summary.tsv
#   revisions/data/results/claude-opus-4-7_rerun/summary.tsv         (optional, for noise)

suppressPackageStartupMessages({})

OUT_DIR <- "revisions/data/analysis"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# --- helpers ---------------------------------------------------------------

read_run <- function(p) {
  if (!file.exists(p)) return(NULL)
  d <- read.delim(p, stringsAsFactors = FALSE)
  d$claude_match <- d$claude_match == "True"
  d$gpt4o_match  <- d$gpt4o_match  == "True"
  d
}

# Wilson 95% score CI for a binomial proportion (Wilson 1927; Agresti & Coull 1998).
# Preferred over the normal/Wald CI because the latter under-covers at small n and
# near 0 or 1; the Wilson interval has better coverage probability throughout.
wilson_ci <- function(k, n, conf = 0.95) {
  if (n == 0) return(list(p = NA_real_, lo = NA_real_, hi = NA_real_))
  z <- qnorm(1 - (1 - conf) / 2)
  p <- k / n
  denom <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denom
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
  list(p = p, lo = max(0, center - half), hi = min(1, center + half))
}

fmt_pct <- function(p, lo, hi) sprintf("%.1f%% [%.1f, %.1f]", 100*p, 100*lo, 100*hi)

normalize <- function(s) {
  if (is.na(s) || s == "") return(character(0))
  u <- strsplit(s, ",")[[1]]
  u <- gsub("/efo_EFO_", "/EFO_", trimws(u))
  sort(unique(u))
}
set_eq <- function(a, b) identical(normalize(a), normalize(b))

# McNemar's test for paired binary outcomes (continuity-corrected). Each pair of models
# is evaluated on the *same* GSEs, so outcomes are paired; McNemar tests marginal
# homogeneity by comparing the two off-diagonal cells of the 2x2 contingency table.
# The Edwards continuity correction is included to limit anti-conservatism of the
# chi-square approximation when discordant counts are small (relevant here for some
# comparisons with <30 discordant pairs).
mcnemar_pair <- function(a, b) {
  tab <- table(factor(a, c(FALSE, TRUE)), factor(b, c(FALSE, TRUE)))
  if (!all(dim(tab) == c(2, 2))) return(list(b1=NA, b2=NA, p=NA_real_))
  res <- mcnemar.test(tab, correct = TRUE)
  list(b1 = tab["FALSE","TRUE"], b2 = tab["TRUE","FALSE"], p = res$p.value)
}

# Cohen's kappa (Cohen 1960). Inter-rater agreement on the binary correct/wrong
# outcome, corrected for the agreement expected by chance given each rater's marginal.
# Reported in addition to raw % agreement because at ~75% accuracy, two independent
# raters agree by chance ~62% of the time (0.75^2 + 0.25^2), so percent agreement
# alone is misleading.
cohens_kappa <- function(a, b) {
  tab <- table(factor(a, c(FALSE, TRUE)), factor(b, c(FALSE, TRUE)))
  if (!all(dim(tab) == c(2, 2))) return(NA_real_)
  n <- sum(tab); po <- (tab[1, 1] + tab[2, 2]) / n
  pe <- (sum(tab[1, ]) * sum(tab[, 1]) + sum(tab[2, ]) * sum(tab[, 2])) / n^2
  (po - pe) / (1 - pe)
}

write_table <- function(df, name) {
  write.table(df, file.path(OUT_DIR, name), sep = "\t", row.names = FALSE,
              quote = FALSE, na = "")
}

# --- read inputs -----------------------------------------------------------

sonnet_base <- read_run("revisions/data/results/claude-sonnet-4-6/summary.tsv")
opus_base   <- read_run("revisions/data/results/claude-opus-4-7/summary.tsv")
haiku_base  <- read_run("revisions/data/results/claude-haiku-4-5-20251001/summary.tsv")
sonnet_spec <- read_run("revisions/data/results/claude-sonnet-4-6_specprompt/summary.tsv")
opus_rerun  <- read_run("revisions/data/results/claude-opus-4-7_rerun/summary.tsv")

stopifnot(!is.null(sonnet_base), !is.null(opus_base), !is.null(haiku_base))

# Inner-join on gse so every analysis is paired on identical experiments.
M <- merge(sonnet_base[, c("gse","truth","claude_match","gpt4o_match","claude_pred","gpt4o_pred")] |>
             setNames(c("gse","truth","sonnet_ok","gpt4o_ok","sonnet_pred","gpt4o_pred")),
           opus_base[, c("gse","claude_match","claude_pred")] |>
             setNames(c("gse","opus_ok","opus_pred")),
           by = "gse")
M <- merge(M, haiku_base[, c("gse","claude_match","claude_pred")] |>
                setNames(c("gse","haiku_ok","haiku_pred")),
           by = "gse")
M <- M[!is.na(M$sonnet_ok) & !is.na(M$opus_ok) & !is.na(M$haiku_ok) & !is.na(M$gpt4o_ok), ]
N <- nrow(M)
cat(sprintf("Paired n = %d\n\n", N))

# --- TABLE 1: headline accuracy + Wilson 95%% CI ---------------------------

tbl1 <- do.call(rbind, lapply(list(
  list(label = "Claude Opus 4.7",   ok = M$opus_ok),
  list(label = "Claude Sonnet 4.6", ok = M$sonnet_ok),
  list(label = "GPT-4o (published)",ok = M$gpt4o_ok),
  list(label = "Claude Haiku 4.5",  ok = M$haiku_ok)
), function(x) {
  ci <- wilson_ci(sum(x$ok), length(x$ok))
  data.frame(model = x$label, k = sum(x$ok), n = length(x$ok),
             exact = ci$p, lo95 = ci$lo, hi95 = ci$hi)
}))
cat("--- Table 1: Strain task accuracy on paired n =", N, " ---\n")
print(transform(tbl1, exact_match = sprintf("%.1f%% [%.1f, %.1f]", 100*exact, 100*lo95, 100*hi95))
      [, c("model","k","n","exact_match")], row.names = FALSE)
write_table(tbl1, "01_accuracy_with_ci.tsv")

# --- TABLE 2: McNemar paired tests ----------------------------------------

models <- list(opus = M$opus_ok, sonnet = M$sonnet_ok, gpt4o = M$gpt4o_ok, haiku = M$haiku_ok)
tbl2 <- do.call(rbind, lapply(combn(names(models), 2, simplify = FALSE), function(p) {
  r <- mcnemar_pair(models[[p[1]]], models[[p[2]]])
  data.frame(a = p[1], b = p[2], b1_a0 = r$b1, b0_a1 = r$b2, p_value = r$p)
}))
cat("\n--- Table 2: McNemar paired tests (correct vs wrong outcome) ---\n")
print(transform(tbl2, p_value = format.pval(p_value, digits = 3)), row.names = FALSE)
write_table(tbl2, "02_mcnemar_pairwise.tsv")

# --- TABLE 3: inter-model agreement (kappa, % identical predictions) ------

tbl3 <- do.call(rbind, lapply(combn(names(models), 2, simplify = FALSE), function(p) {
  k <- cohens_kappa(models[[p[1]]], models[[p[2]]])
  pred_a <- M[[paste0(p[1], "_pred")]]; pred_b <- M[[paste0(p[2], "_pred")]]
  same <- mapply(set_eq, pred_a, pred_b)
  outcome_agree <- mean(models[[p[1]]] == models[[p[2]]])
  data.frame(a = p[1], b = p[2],
             pct_outcome_agreement = round(outcome_agree, 3),
             kappa = round(k, 3),
             pct_identical_predictions = round(mean(same), 3))
}))
cat("\n--- Table 3: Inter-model agreement ---\n")
print(tbl3, row.names = FALSE)
write_table(tbl3, "03_agreement.tsv")

# --- TABLE 4: error overlap ------------------------------------------------

wrong_any   <- !M$opus_ok | !M$sonnet_ok | !M$gpt4o_ok | !M$haiku_ok
wrong_top3  <- !M$opus_ok & !M$sonnet_ok & !M$gpt4o_ok
wrong_all4  <- wrong_top3 & !M$haiku_ok
top3_set <- M[wrong_top3, ]
same_os <- mapply(set_eq, top3_set$opus_pred,   top3_set$sonnet_pred)
same_og <- mapply(set_eq, top3_set$opus_pred,   top3_set$gpt4o_pred)
same_sg <- mapply(set_eq, top3_set$sonnet_pred, top3_set$gpt4o_pred)
same_all <- same_os & same_og & same_sg

tbl4 <- data.frame(
  category = c("Any model wrong",
               "All 4 models wrong",
               "Top-3 wrong (Opus, Sonnet, GPT-4o)",
               " ...of which Opus & Sonnet gave the SAME wrong answer",
               " ...of which Opus & GPT-4o gave the SAME wrong answer",
               " ...of which Sonnet & GPT-4o gave the SAME wrong answer",
               " ...of which ALL THREE gave the SAME wrong answer"),
  n = c(sum(wrong_any), sum(wrong_all4), sum(wrong_top3),
        sum(same_os), sum(same_og), sum(same_sg), sum(same_all)),
  pct_of_N = c(round(mean(wrong_any),3), round(mean(wrong_all4),3), round(mean(wrong_top3),3),
               NA, NA, NA, NA)
)
cat("\n--- Table 4: Error overlap (n =", N, ") ---\n")
print(tbl4, row.names = FALSE)
write_table(tbl4, "04_error_overlap.tsv")

# --- TABLE 5: top recurring (truth, predicted) confusions ------------------

all_errors <- do.call(rbind, lapply(c("opus","sonnet","gpt4o","haiku"), function(mod) {
  ok <- M[[paste0(mod, "_ok")]]
  data.frame(model = mod, gse = M$gse,
             truth = M$truth, pred = M[[paste0(mod, "_pred")]])[!ok, ]
}))
all_errors$truth <- gsub("http://www.ebi.ac.uk/efo/", "EFO/",
                         gsub("http://gemma.msl.ubc.ca/ont/", "TGEMO/", all_errors$truth))
all_errors$pred  <- gsub("http://www.ebi.ac.uk/efo/", "EFO/",
                         gsub("http://gemma.msl.ubc.ca/ont/", "TGEMO/", all_errors$pred))
conf <- as.data.frame(table(truth = all_errors$truth, pred = all_errors$pred))
conf <- conf[conf$Freq > 0, ]
conf <- conf[order(-conf$Freq), ]
tbl5 <- head(conf, 15)
cat("\n--- Table 5: Top 15 recurring confusions across all models ---\n")
print(tbl5, row.names = FALSE)
write_table(tbl5, "05_top_confusions.tsv")

# --- TABLE 6: specificity-rule prompt patch (Sonnet only) ------------------

if (!is.null(sonnet_spec)) {
  S <- merge(sonnet_base[, c("gse","truth","claude_match","gpt4o_match")] |>
               setNames(c("gse","truth","base_ok","gpt4o_ok")),
             sonnet_spec[, c("gse","claude_match")] |> setNames(c("gse","spec_ok")),
             by = "gse")
  S <- S[!is.na(S$base_ok) & !is.na(S$spec_ok) & !is.na(S$gpt4o_ok), ]
  rows <- list(
    list(label = "Sonnet 4.6 baseline",          ok = S$base_ok),
    list(label = "Sonnet 4.6 + specificity rule", ok = S$spec_ok),
    list(label = "GPT-4o (published)",            ok = S$gpt4o_ok)
  )
  tbl6 <- do.call(rbind, lapply(rows, function(x) {
    ci <- wilson_ci(sum(x$ok), length(x$ok))
    data.frame(setup = x$label, k = sum(x$ok), n = length(x$ok),
               exact = ci$p, lo95 = ci$lo, hi95 = ci$hi)
  }))
  tbl6_mcn <- rbind(
    data.frame(a = "sonnet base", b = "sonnet+spec",
               mcnemar_pair(S$base_ok, S$spec_ok) |> as.data.frame()),
    data.frame(a = "gpt4o",       b = "sonnet+spec",
               mcnemar_pair(S$gpt4o_ok, S$spec_ok) |> as.data.frame()),
    data.frame(a = "gpt4o",       b = "sonnet base",
               mcnemar_pair(S$gpt4o_ok, S$base_ok) |> as.data.frame())
  )
  cat("\n--- Table 6: Specificity-rule prompt patch (n =", nrow(S), ") ---\n")
  print(transform(tbl6, exact_match = sprintf("%.1f%% [%.1f, %.1f]", 100*exact, 100*lo95, 100*hi95))
        [, c("setup","k","n","exact_match")], row.names = FALSE)
  cat("\nMcNemar tests:\n")
  print(transform(tbl6_mcn, p = format.pval(p, digits = 3)), row.names = FALSE)
  write_table(tbl6, "06_specificity_accuracy.tsv")
  write_table(tbl6_mcn, "07_specificity_mcnemar.tsv")
}

# --- TABLE 7: Opus run-to-run noise (n = 20) --------------------------------

if (!is.null(opus_rerun)) {
  rr <- merge(opus_base[, c("gse","truth","claude_pred","claude_match")] |>
                setNames(c("gse","truth","pred_r1","ok_r1")),
              opus_rerun[, c("gse","claude_pred","claude_match")] |>
                setNames(c("gse","pred_r2","ok_r2")),
              by = "gse")
  rr <- rr[!is.na(rr$pred_r1) & !is.na(rr$pred_r2), ]
  rr$same_pred <- mapply(set_eq, rr$pred_r1, rr$pred_r2)
  ci1 <- wilson_ci(sum(rr$ok_r1), nrow(rr))
  ci2 <- wilson_ci(sum(rr$ok_r2), nrow(rr))
  cat("\n--- Table 7: Opus 4.7 run-to-run noise on n =", nrow(rr), " ---\n")
  cat(sprintf("Run 1 exact:           %s\n", fmt_pct(ci1$p, ci1$lo, ci1$hi)))
  cat(sprintf("Run 2 exact:           %s\n", fmt_pct(ci2$p, ci2$lo, ci2$hi)))
  cat(sprintf("Identical predictions: %d / %d (%.0f%%)\n",
              sum(rr$same_pred), nrow(rr), 100*mean(rr$same_pred)))
  noise <- data.frame(
    metric = c("exact_run1","exact_run2","identical_predictions"),
    value  = c(mean(rr$ok_r1), mean(rr$ok_r2), mean(rr$same_pred)),
    k      = c(sum(rr$ok_r1), sum(rr$ok_r2), sum(rr$same_pred)),
    n      = c(nrow(rr), nrow(rr), nrow(rr))
  )
  write_table(noise, "08_opus_noise.tsv")
}

cat("\nAll tables written to", OUT_DIR, "\n")
