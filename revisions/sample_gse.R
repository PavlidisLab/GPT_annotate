# Sample GSEs from main_frame.rds for evaluation. Fully random.
suppressPackageStartupMessages({
  library(magrittr)
})

args <- commandArgs(trailingOnly = TRUE)
n_total  <- if (length(args) >= 1) as.integer(args[[1]]) else 500L
out_path <- if (length(args) >= 2) args[[2]] else "revisions/data/sample500.tsv"
seed     <- if (length(args) >= 3) as.integer(args[[3]]) else 20260513L

mf <- readRDS("data-raw/strain_data/main_frame.rds")
mf$n_strains <- sapply(mf$gemma_name, length)
mf$gemma_uri_str <- sapply(mf$gemma_uri, function(x) paste0(x, collapse = ","))
mf <- mf[mf$n_strains >= 1 & !is.na(mf$gemma_uri_str) & mf$gemma_uri_str != "", ]

set.seed(seed)
picked_idx <- sample.int(nrow(mf), min(n_total, nrow(mf)))
sample_df <- mf[picked_idx, ]

out <- data.frame(
  shortName        = sample_df$shortName,
  n_strains        = sample_df$n_strains,
  paper_accessible = sample_df$paper_accessible,
  gemma_uri        = sample_df$gemma_uri_str,
  gemma_name       = sapply(sample_df$gemma_name, paste0, collapse = ","),
  gpt_uri          = sapply(sample_df$gpt_uris,   paste0, collapse = ","),
  gpt_name         = sapply(sample_df$gpt_strains, paste0, collapse = ","),
  basic_match      = sample_df$basic_match,
  stringsAsFactors = FALSE
)

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
write.table(out, out_path, sep = "\t", row.names = FALSE, quote = FALSE)
cat(sprintf("Wrote %s with %d rows (fully random from %d eligible)\n",
            out_path, nrow(out), nrow(mf)))
cat("\nDistribution:\n")
print(table(
  n_strains        = ifelse(out$n_strains == 1, "1", "2+"),
  paper_accessible = out$paper_accessible
))
