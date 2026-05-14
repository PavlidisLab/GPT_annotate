# Export the cell-line main_frame to a TSV that Python can read robustly.
# Compresses list-columns (e.g. gpt_cell_lines, gpt_description) into pipe-separated
# strings using "|||" as the within-row separator.
suppressPackageStartupMessages({
  library(magrittr)
})

mf <- readRDS("data-raw/cell_line_data/main_frame.rds")

collapse_list <- function(x) {
  if (is.list(x)) {
    vapply(x, function(v) paste(as.character(v), collapse = "|||"), character(1))
  } else if (is.null(x)) {
    character(nrow(mf))
  } else {
    as.character(x)
  }
}

cols <- c("shortName",
          "gpt_cell_lines", "gpt_description", "gpt_quote",
          "gpt_cell_line_term", "gpt_cell_line_term_id",
          "gemma_term", "gemma_uri",
          "top_embedding_rank_gpt", "top_embedding_rank_gemma",
          "sensitive", "specific", "basic_match")
out <- as.data.frame(lapply(cols, function(c) collapse_list(mf[[c]])),
                     stringsAsFactors = FALSE)
colnames(out) <- cols
write.table(out, "revisions/data/cell_line_main_frame.tsv",
            sep = "\t", row.names = FALSE, quote = FALSE, na = "")
cat("Wrote revisions/data/cell_line_main_frame.tsv:", nrow(out), "rows\n")
