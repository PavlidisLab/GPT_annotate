# Build the cell-line ontology list (CLO ∪ EFO descendants of CL:0000000),
# matching exactly the construction in `analysis/cell_lines/01.process_cell_lines.R`.
#
# EFO is pinned to v3.79.0 (same release used by build_strain_list.R, matching
# the original strain-task commits in mid-2025). CLO is pinned to the OBO Foundry
# release matching `clo.owl` at PURL on 2025-07; we record the SHA256 hash of the
# downloaded OWL for reproducibility. robot.jar v1.9.6 is used to convert OWL → OBO
# (same release used by the original `analysis/_downloads/download.R`).

suppressPackageStartupMessages({
  library(ontologyIndex)
  library(magrittr)
  library(jsonlite)
  library(stringr)
})

OUT_DIR  <- "revisions/data"
CLO_OWL  <- file.path(OUT_DIR, "CLO.owl")
CLO_OBO  <- file.path(OUT_DIR, "CLO.obo")
EFO_OBO  <- file.path(OUT_DIR, "efo_v3.79.0.obo")
ROBOT    <- file.path(OUT_DIR, "robot.jar")
OUT_JSON <- file.path(OUT_DIR, "cell_line_list.json")

CLO_URL   <- "https://purl.obolibrary.org/obo/clo.owl"
ROBOT_URL <- "https://github.com/ontodev/robot/releases/download/v1.9.6/robot.jar"

if (!file.exists(ROBOT)) {
  message("Downloading robot.jar v1.9.6 ...")
  utils::download.file(ROBOT_URL, ROBOT, mode = "wb")
}
if (!file.exists(CLO_OWL)) {
  message("Downloading CLO.owl ...")
  utils::download.file(CLO_URL, CLO_OWL, mode = "wb")
  message("CLO.owl SHA256: ",
          paste(tools::md5sum(CLO_OWL), collapse = ""))  # md5 for quick fingerprint
}
if (!file.exists(CLO_OBO)) {
  message("Converting CLO.owl -> CLO.obo via robot.jar ...")
  cmd <- sprintf("java -jar %s convert --input %s --check false --format obo --output %s",
                 shQuote(ROBOT), shQuote(CLO_OWL), shQuote(CLO_OBO))
  ret <- system(cmd)
  if (ret != 0) stop("robot.jar conversion failed")
}
if (!file.exists(EFO_OBO)) stop(sprintf("Missing %s -- run build_strain_list.R first", EFO_OBO))

message("Loading CLO ...")
clo <- get_ontology(CLO_OBO, extract_tags = "everything")
message("CLO term count: ", length(clo$id))

message("Loading EFO ...")
efo <- get_ontology(EFO_OBO, extract_tags = "everything")
efo_cells <- get_descendants(efo, "CL:0000000")
message("EFO 'cell' descendants: ", length(efo_cells))

strip_def <- function(d) {
  if (is.null(d) || length(d) == 0 || is.na(d)) return("")
  d <- gsub('\\[|\\]|\\"', "", d)
  trimws(d)
}
extract_syns <- function(s) {
  if (length(s) == 0) return(character(0))
  out <- unlist(str_extract_all(s, '(?<=").*?(?=")'))
  out[nzchar(out)]
}

process_term <- function(ontology, id) {
  list(
    ID          = unbox(id),
    value       = unbox(ontology$name[[id]] %||% ""),
    description = unbox(strip_def(ontology$def[[id]])),
    synonyms    = extract_syns(ontology$synonym[[id]])
  )
}

clo_list <- lapply(clo$id, function(id) process_term(clo, id))
names(clo_list) <- vapply(clo_list, function(x) unclass(x$ID), character(1))

efo_in_efo <- intersect(efo$id, efo_cells)
efo_list <- lapply(efo_in_efo, function(id) process_term(efo, id))
names(efo_list) <- vapply(efo_list, function(x) unclass(x$ID), character(1))

# Drop EFO entries whose IDs are already in CLO (mirrors the original code:
# `commons = intersect(...); efo_cells_list = efo_cells_list[!names ... %in% commons]`).
commons <- intersect(names(clo_list), names(efo_list))
message("Duplicates (CLO ∩ EFO_cells): ", length(commons))
efo_list <- efo_list[!names(efo_list) %in% commons]

cell_line_list <- c(clo_list, efo_list)
message("Final cell-line list: ", length(cell_line_list),
        " (", length(clo_list), " CLO + ", length(efo_list), " EFO-only)")

# JSON output: ordered associative array keyed by ID, matching the form fed to embeddings.
writeLines(toJSON(cell_line_list, pretty = TRUE, auto_unbox = FALSE), OUT_JSON)
message("Wrote ", OUT_JSON)
