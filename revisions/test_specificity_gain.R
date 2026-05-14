# Test whether the specificity-rule prompt gain on Sonnet 4.6 is statistically meaningful.
#
# Superseded by analyze_strain_results.R, which produces the same outputs as
# part of the one-shot statistical-analysis driver (data/analysis/06_specificity_*.tsv).
# Kept as a readable standalone reference for the McNemar test on the spec-rule patch.
suppressPackageStartupMessages({})

base <- read.delim("revisions/data/results/claude-sonnet-4-6/summary.tsv", stringsAsFactors = FALSE)
spec <- read.delim("revisions/data/results/claude-sonnet-4-6_specprompt/summary.tsv", stringsAsFactors = FALSE)

m <- merge(
  base[, c("gse","truth","claude_pred","claude_match","gpt4o_match")] |>
    setNames(c("gse","truth","base_pred","base_ok","gpt4o_ok")),
  spec[, c("gse","claude_pred","claude_match")] |> setNames(c("gse","spec_pred","spec_ok")),
  by = "gse"
)
m$base_ok <- m$base_ok == "True"
m$spec_ok <- m$spec_ok == "True"
m$gpt4o_ok <- m$gpt4o_ok == "True"
m <- m[!is.na(m$base_ok) & !is.na(m$spec_ok) & !is.na(m$gpt4o_ok), ]

cat(sprintf("Paired n = %d\n\n", nrow(m)))
cat(sprintf("Sonnet baseline:       %.1f%% (%d / %d)\n", 100*mean(m$base_ok), sum(m$base_ok), nrow(m)))
cat(sprintf("Sonnet + spec-prompt:  %.1f%% (%d / %d)\n", 100*mean(m$spec_ok), sum(m$spec_ok), nrow(m)))
cat(sprintf("GPT-4o (published):    %.1f%% (%d / %d)\n\n", 100*mean(m$gpt4o_ok), sum(m$gpt4o_ok), nrow(m)))

run_test <- function(a, b, label) {
  tab <- table(a = a, b = b)
  if (!all(dim(tab) == c(2, 2))) { cat(sprintf("%-30s (degenerate)\n", label)); return() }
  res <- mcnemar.test(tab, correct = TRUE)
  cat(sprintf("%-30s b1=a0:%3d  b0=a1:%3d  McNemar p=%.4f\n",
              label, tab["FALSE","TRUE"], tab["TRUE","FALSE"], res$p.value))
}

run_test(m$base_ok,  m$spec_ok,  "Sonnet base vs Sonnet+spec")
run_test(m$gpt4o_ok, m$spec_ok,  "GPT-4o vs Sonnet+spec")
run_test(m$gpt4o_ok, m$base_ok,  "GPT-4o vs Sonnet base (reference)")

# Look at what kinds of errors got fixed
fixed <- m[!m$base_ok & m$spec_ok, c("gse","truth","base_pred","spec_pred")]
broken <- m[m$base_ok & !m$spec_ok, c("gse","truth","base_pred","spec_pred")]
cat(sprintf("\nFixed by spec-prompt:   %d\nBroken by spec-prompt:  %d\n", nrow(fixed), nrow(broken)))

short <- function(s) gsub("http://www.ebi.ac.uk/efo/", "EFO/", gsub("http://gemma.msl.ubc.ca/ont/", "TGEMO/", s))
cat("\nSample of FIXED cases (first 12):\n")
fx <- fixed
fx$truth <- short(fx$truth); fx$base_pred <- short(fx$base_pred); fx$spec_pred <- short(fx$spec_pred)
print(head(fx, 12), row.names = FALSE)

cat("\nSample of BROKEN cases (first 12):\n")
bx <- broken
bx$truth <- short(bx$truth); bx$base_pred <- short(bx$base_pred); bx$spec_pred <- short(bx$spec_pred)
print(head(bx, 12), row.names = FALSE)
