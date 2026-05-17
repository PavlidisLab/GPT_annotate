# Per-GSE agreement between Opus run 1 and Opus run 2, and CI on the n=500 model comparison.
#
# Superseded by analyze_strain_results.R, which produces the same outputs as
# part of the one-shot statistical-analysis driver (data/analysis/07_noise_*.tsv).
# Kept as a readable standalone reference for the noise computation.
suppressPackageStartupMessages({})

normalize_uris <- function(s) {
  if (is.na(s) || s == "") return(character(0))
  uris <- strsplit(s, ",")[[1]]
  uris <- gsub("/efo_EFO_", "/EFO_", uris)
  sort(unique(trimws(uris)))
}

set_eq <- function(a, b) identical(normalize_uris(a), normalize_uris(b))

run1 <- read.delim("revisions/data/results/claude-opus-4-7/summary.tsv", stringsAsFactors = FALSE)
run2 <- read.delim("revisions/data/results/claude-opus-4-7_rerun/summary.tsv", stringsAsFactors = FALSE)

m <- merge(run1[, c("gse","truth","claude_pred","claude_match")],
           run2[, c("gse","claude_pred","claude_match")],
           by = "gse", suffixes = c("_r1","_r2"))
m <- m[!is.na(m$claude_pred_r1) & !is.na(m$claude_pred_r2), ]

m$same_pred  <- mapply(set_eq, m$claude_pred_r1, m$claude_pred_r2)
m$r1_correct <- m$claude_match_r1 == "True"
m$r2_correct <- m$claude_match_r2 == "True"

cat(sprintf("\n--- Opus 4.7 stability on n=%d GSEs ---\n", nrow(m)))
cat(sprintf("Identical predictions (run 1 == run 2): %d/%d (%.0f%%)\n",
            sum(m$same_pred), nrow(m), 100*mean(m$same_pred)))
cat(sprintf("Run 1 exact-match:  %d/%d (%.0f%%)\n",
            sum(m$r1_correct), nrow(m), 100*mean(m$r1_correct)))
cat(sprintf("Run 2 exact-match:  %d/%d (%.0f%%)\n",
            sum(m$r2_correct), nrow(m), 100*mean(m$r2_correct)))
cat(sprintf("Both correct:       %d\n", sum(m$r1_correct & m$r2_correct)))
cat(sprintf("Run 1 only correct: %d\n", sum(m$r1_correct & !m$r2_correct)))
cat(sprintf("Run 2 only correct: %d\n", sum(!m$r1_correct & m$r2_correct)))
cat(sprintf("Neither correct:    %d\n", sum(!m$r1_correct & !m$r2_correct)))

cat("\n--- Cases where Opus disagreed with itself ---\n")
disagree <- m[!m$same_pred, c("gse","truth","claude_pred_r1","claude_pred_r2")]
disagree$truth          <- gsub("http://www.ebi.ac.uk/efo/","EFO/", disagree$truth)
disagree$claude_pred_r1 <- gsub("http://www.ebi.ac.uk/efo/","EFO/", disagree$claude_pred_r1)
disagree$claude_pred_r2 <- gsub("http://www.ebi.ac.uk/efo/","EFO/", disagree$claude_pred_r2)
print(disagree, row.names = FALSE)

cat("\n--- n=500 model comparison: significance ---\n")
sonnet <- read.delim("revisions/data/results/claude-sonnet-4-6/summary.tsv", stringsAsFactors=FALSE)
opus500 <- read.delim("revisions/data/results/claude-opus-4-7/summary.tsv", stringsAsFactors=FALSE)
haiku  <- read.delim("revisions/data/results/claude-haiku-4-5-20251001/summary.tsv", stringsAsFactors=FALSE)

# All three were run on the same 500 GSEs; join.
all3 <- merge(
  merge(sonnet[, c("gse","truth","claude_match","gpt4o_match")] |>
          setNames(c("gse","truth","sonnet","gpt4o")),
        opus500[, c("gse","claude_match")] |> setNames(c("gse","opus")),
        by = "gse"),
  haiku[, c("gse","claude_match")] |> setNames(c("gse","haiku")),
  by = "gse"
)
to_bool <- function(x) x == "True"
all3$sonnet <- to_bool(all3$sonnet); all3$opus <- to_bool(all3$opus)
all3$haiku  <- to_bool(all3$haiku);  all3$gpt4o <- to_bool(all3$gpt4o)
all3 <- all3[!is.na(all3$sonnet) & !is.na(all3$opus) & !is.na(all3$haiku) & !is.na(all3$gpt4o), ]

cat(sprintf("Paired n = %d\n", nrow(all3)))
cat(sprintf("Opus exact:    %.1f%%\n", 100*mean(all3$opus)))
cat(sprintf("Sonnet exact:  %.1f%%\n", 100*mean(all3$sonnet)))
cat(sprintf("GPT-4o exact:  %.1f%%\n", 100*mean(all3$gpt4o)))
cat(sprintf("Haiku exact:   %.1f%%\n\n", 100*mean(all3$haiku)))

# McNemar's test for each pairwise comparison (paired binary outcomes)
mcnemar_pair <- function(a, b, label) {
  tab <- table(a = a, b = b)
  if (all(dim(tab) == c(2,2))) {
    res <- mcnemar.test(tab, correct = TRUE)
    cat(sprintf("%-25s b1=a0:%2d  b0=a1:%2d  McNemar p=%.4f\n",
                label,
                tab["FALSE","TRUE"], tab["TRUE","FALSE"], res$p.value))
  } else {
    cat(sprintf("%-25s (degenerate table)\n", label))
  }
}
mcnemar_pair(all3$opus,   all3$sonnet, "Opus vs Sonnet")
mcnemar_pair(all3$opus,   all3$gpt4o,  "Opus vs GPT-4o")
mcnemar_pair(all3$sonnet, all3$gpt4o,  "Sonnet vs GPT-4o")
mcnemar_pair(all3$opus,   all3$haiku,  "Opus vs Haiku")
mcnemar_pair(all3$sonnet, all3$haiku,  "Sonnet vs Haiku")
mcnemar_pair(all3$gpt4o,  all3$haiku,  "GPT-4o vs Haiku")
