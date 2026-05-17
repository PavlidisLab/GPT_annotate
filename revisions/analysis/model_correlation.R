# Inter-model agreement / error overlap on the 500-GSE strain run.
#
# Superseded by analyze_strain_results.R, which produces the same outputs as
# part of the one-shot statistical-analysis driver (data/analysis/04_*.tsv).
# Kept as a readable standalone reference for the per-pair correlation computation.
suppressPackageStartupMessages({})

normalize <- function(s) {
  if (is.na(s) || s == "") return(character(0))
  u <- strsplit(s, ",")[[1]]
  u <- gsub("/efo_EFO_", "/EFO_", u)
  sort(unique(trimws(u)))
}
set_eq <- function(a, b) identical(normalize(a), normalize(b))

read_run <- function(p) {
  d <- read.delim(p, stringsAsFactors = FALSE)
  d[!is.na(d$claude_pred), c("gse","truth","claude_pred","gpt4o_pred","claude_match","gpt4o_match")]
}

opus   <- read_run("revisions/data/results/claude-opus-4-7/summary.tsv")
sonnet <- read_run("revisions/data/results/claude-sonnet-4-6/summary.tsv")
haiku  <- read_run("revisions/data/results/claude-haiku-4-5-20251001/summary.tsv")

m <- merge(opus[, c("gse","truth","claude_pred","gpt4o_pred","claude_match","gpt4o_match")] |>
             setNames(c("gse","truth","opus_pred","gpt4o_pred","opus_ok","gpt4o_ok")),
           sonnet[, c("gse","claude_pred","claude_match")] |>
             setNames(c("gse","sonnet_pred","sonnet_ok")),
           by = "gse")
m <- merge(m, haiku[, c("gse","claude_pred","claude_match")] |>
                setNames(c("gse","haiku_pred","haiku_ok")),
           by = "gse")

for (col in c("opus_ok","sonnet_ok","haiku_ok","gpt4o_ok")) m[[col]] <- m[[col]] == "True"
cat(sprintf("Paired n = %d\n\n", nrow(m)))

# 1) Pairwise agreement on EXACT-MATCH outcome (correct/incorrect)
cohens_kappa <- function(a, b) {
  tab <- table(a = a, b = b)
  if (!all(dim(tab) == c(2, 2))) return(NA_real_)
  n <- sum(tab); po <- (tab[1, 1] + tab[2, 2]) / n
  pe <- (sum(tab[1, ]) * sum(tab[, 1]) + sum(tab[2, ]) * sum(tab[, 2])) / n^2
  (po - pe) / (1 - pe)
}

cat("--- Pairwise agreement on EXACT-MATCH outcome (both correct or both wrong) ---\n")
models <- c("opus","sonnet","gpt4o","haiku")
for (i in 1:(length(models)-1)) for (j in (i+1):length(models)) {
  a <- m[[paste0(models[i],"_ok")]]; b <- m[[paste0(models[j],"_ok")]]
  agree <- mean(a == b)
  k <- cohens_kappa(a, b)
  cat(sprintf("  %-7s vs %-7s  agreement=%.3f  kappa=%.3f\n",
              models[i], models[j], agree, k))
}

# 2) Pairwise agreement on the EXACT PREDICTION (same predicted URI set)
cat("\n--- Pairwise agreement on PREDICTED URI SETS (identical predictions) ---\n")
preds <- list(
  opus   = m$opus_pred,
  sonnet = m$sonnet_pred,
  gpt4o  = m$gpt4o_pred,
  haiku  = m$haiku_pred
)
for (i in 1:(length(preds)-1)) for (j in (i+1):length(preds)) {
  same <- mapply(set_eq, preds[[i]], preds[[j]])
  cat(sprintf("  %-7s vs %-7s  identical=%.3f\n",
              names(preds)[i], names(preds)[j], mean(same)))
}

# 3) Error-overlap matrix among wrong cases
cat("\n--- How often do models make the SAME error vs DIFFERENT errors? ---\n")
cat("(restricted to experiments where at least one model is wrong)\n")
wrong_any <- !m$opus_ok | !m$sonnet_ok | !m$gpt4o_ok | !m$haiku_ok
cat(sprintf("Experiments where ≥1 model wrong: %d/%d\n", sum(wrong_any), nrow(m)))

wrong_all4 <- !m$opus_ok & !m$sonnet_ok & !m$gpt4o_ok & !m$haiku_ok
wrong_all3 <- !m$opus_ok & !m$sonnet_ok & !m$gpt4o_ok
cat(sprintf("All FOUR models wrong:                     %d  (probable curator error or genuinely ambiguous)\n", sum(wrong_all4)))
cat(sprintf("All three top models (Opus/Sonnet/GPT-4o): %d\n", sum(wrong_all3)))

# Of the experiments any model gets wrong, how often is the WRONG ANSWER the same?
wrong_subset <- m[wrong_any, ]
top3_wrong <- !wrong_subset$opus_ok & !wrong_subset$sonnet_ok & !wrong_subset$gpt4o_ok
ws3 <- wrong_subset[top3_wrong, ]
if (nrow(ws3) > 0) {
  os <- mapply(set_eq, ws3$opus_pred,   ws3$sonnet_pred)
  og <- mapply(set_eq, ws3$opus_pred,   ws3$gpt4o_pred)
  sg <- mapply(set_eq, ws3$sonnet_pred, ws3$gpt4o_pred)
  all3 <- os & og & sg
  cat(sprintf("\nAmong the %d experiments where Opus, Sonnet, AND GPT-4o were all wrong:\n", nrow(ws3)))
  cat(sprintf("  Opus & Sonnet gave SAME wrong answer:  %d (%.0f%%)\n", sum(os), 100*mean(os)))
  cat(sprintf("  Opus & GPT-4o gave SAME wrong answer:  %d (%.0f%%)\n", sum(og), 100*mean(og)))
  cat(sprintf("  Sonnet & GPT-4o gave SAME wrong answer:%d (%.0f%%)\n", sum(sg), 100*mean(sg)))
  cat(sprintf("  All three gave the SAME wrong answer:  %d (%.0f%%)\n", sum(all3), 100*mean(all3)))
}

# 4) Top recurring confusions across models
cat("\n--- Top recurring (predicted, truth) confusions across all models ---\n")
all_errors <- do.call(rbind, lapply(c("opus","sonnet","gpt4o","haiku"), function(mod) {
  ok <- m[[paste0(mod, "_ok")]]
  p  <- m[[paste0(mod, "_pred")]]
  data.frame(model = mod, gse = m$gse, truth = m$truth, pred = p,
             stringsAsFactors = FALSE)[!ok, ]
}))
all_errors$truth <- gsub("http://www.ebi.ac.uk/efo/","EFO/", all_errors$truth)
all_errors$pred  <- gsub("http://www.ebi.ac.uk/efo/","EFO/", all_errors$pred)
confusions <- as.data.frame(table(truth = all_errors$truth, pred = all_errors$pred))
confusions <- confusions[confusions$Freq > 0, ]
confusions <- confusions[order(-confusions$Freq), ]
print(head(confusions, 15), row.names = FALSE)
