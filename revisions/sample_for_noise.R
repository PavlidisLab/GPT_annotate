# Pick 20 GSEs from the 500-sample (preserving original ground-truth columns)
# for a noise-replicate study.
suppressPackageStartupMessages({})

args <- commandArgs(trailingOnly = TRUE)
seed <- if (length(args) >= 1) as.integer(args[[1]]) else 13579L

s <- read.delim("revisions/data/sample500.tsv", stringsAsFactors = FALSE)
set.seed(seed)
picked <- s[sample.int(nrow(s), 20), ]

write.table(picked, "revisions/data/sample_noise20.tsv",
            sep = "\t", row.names = FALSE, quote = FALSE)
cat("Wrote revisions/data/sample_noise20.tsv\n")
print(picked[, c("shortName","n_strains","paper_accessible","gemma_uri")])
