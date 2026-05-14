# Uniform random sample of cell-line GSEs with a curated gemma_uri.
suppressPackageStartupMessages({})

args <- commandArgs(trailingOnly = TRUE)
n_total  <- if (length(args) >= 1) as.integer(args[[1]]) else 500L
out_path <- if (length(args) >= 2) args[[2]] else "revisions/data/sample_cell500.tsv"
seed     <- if (length(args) >= 3) as.integer(args[[3]]) else 20260514L

mf <- readRDS("data-raw/cell_line_data/main_frame.rds")
# Flatten nested list columns first (each row is a list-of-character-vectors with
# possible NAs). unlist + drop NA + collapse gives a clean comma-separated string.
flatten_col <- function(col, sep) {
  vapply(col, function(x) {
    v <- unlist(x, use.names = FALSE)
    v <- v[!is.na(v) & nzchar(v)]
    paste(v, collapse = sep)
  }, character(1))
}
mf$gemma_uri_str       <- flatten_col(mf$gemma_uri,            "|||")
mf$gemma_term_str      <- flatten_col(mf$gemma_term,           "|||")
mf$gpt_term_str        <- flatten_col(mf$gpt_cell_line_term,   "|||")
mf$gpt_id_str          <- flatten_col(mf$gpt_cell_line_term_id, "|||")
mf$gpt_cell_lines_str  <- flatten_col(mf$gpt_cell_lines,       "|||")

mf <- mf[!is.na(mf$gemma_uri_str) & mf$gemma_uri_str != "", ]
cat("eligible cell-line rows:", nrow(mf), "\n")

set.seed(seed)
idx <- sample.int(nrow(mf), min(n_total, nrow(mf)))
out <- data.frame(
  shortName        = mf$shortName[idx],
  gemma_term       = mf$gemma_term_str[idx],
  gemma_uri        = mf$gemma_uri_str[idx],
  gpt_cell_lines   = mf$gpt_cell_lines_str[idx],
  gpt_term         = mf$gpt_term_str[idx],
  gpt_term_id      = mf$gpt_id_str[idx],
  basic_match      = mf$basic_match[idx],
  specific         = mf$specific[idx],
  sensitive        = mf$sensitive[idx],
  stringsAsFactors = FALSE
)

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
write.table(out, out_path, sep = "\t", row.names = FALSE, quote = FALSE, na = "")
cat(sprintf("Wrote %s with %d rows\n", out_path, nrow(out)))
