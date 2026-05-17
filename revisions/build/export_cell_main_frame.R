# Export the cell-line main_frame to a TSV that Python can read robustly.
# Collapses list-columns (e.g. gpt_cell_lines, gpt_description) into pipe-
# separated strings using "|||" as the within-row separator, and replaces
# every embedded newline / tab / carriage-return with a single space so the
# TSV row boundary is preserved even when ``gpt_quote`` or ``gpt_description``
# contains multi-line paper-text fragments. (The previous version used
# `quote = FALSE` with no in-cell newline stripping, which let ~4 % of the
# cell-line rows split across the row boundary and dropped paper-text into
# the next row's `shortName` field.)
suppressPackageStartupMessages({
  library(magrittr)
})

mf <- readRDS("data-raw/cell_line_data/main_frame.rds")

# Replace any character that would break TSV row/column parsing with a
# single space. Applied to every emitted cell as the last step.
sanitise <- function(s) {
  s <- gsub("[\r\n\t]+", " ", s)        # newlines / tabs -> space
  s <- gsub(" +", " ", s)               # collapse whitespace runs
  trimws(s)
}

collapse_list <- function(x) {
  if (is.list(x)) {
    out <- vapply(x, function(v) {
      v <- unlist(v, use.names = FALSE)
      paste(as.character(v), collapse = "|||")
    }, character(1))
  } else if (is.null(x)) {
    out <- character(nrow(mf))
  } else {
    out <- as.character(x)
  }
  sanitise(out)
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

# Sanity check before writing. Gemma shortNames are typically a GEO
# accession but may carry a version-style suffix (e.g. ``GSE1234.1`` when a
# series has been re-imported), so we accept ``GSE<digits>`` optionally
# followed by ``.<token>``. The pattern still rejects the paper-text and
# wrapped-URL spillage that triggered this check (those contain spaces,
# colons, or are far too long).
bad <- out[!grepl("^GSE[0-9]+([._-][A-Za-z0-9_.-]+)?$", out$shortName) |
           nchar(out$shortName) > 32, , drop = FALSE]
if (nrow(bad) > 0) {
  cat("ERROR: ", nrow(bad), " rows have malformed shortName; not writing.\n", sep = "")
  print(head(bad$shortName, 5))
  quit(status = 1)
}

write.table(out, "revisions/data/cell_line_main_frame.tsv",
            sep = "\t", row.names = FALSE, quote = FALSE, na = "")
cat("Wrote revisions/data/cell_line_main_frame.tsv:", nrow(out), "rows\n")
