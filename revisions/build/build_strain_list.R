# Build the mouse-strain ontology list (EFO + TGEMO descendants of NCBITaxon:10090)
# matching exactly the code path used by the original (analysis/strains/00.downloads.R).
#
# EFO is pinned to v3.79.0 (released 2025-06-16), the version contemporaneous with the
# strain-task commits in the the original repository (2025-07 "strain eval"). Older / newer
# versions differ by at most a couple of terms; the same code path on the current EFO release
# adds no new strains relevant to the original evaluation.

suppressPackageStartupMessages({
  library(ontologyIndex)
  library(magrittr)
  library(jsonlite)
  library(rdflib)
  library(stringr)
})

OUT_DIR  <- "revisions/data"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
EFO_OBO  <- file.path(OUT_DIR, "efo_v3.79.0.obo")
TGEMO_OWL <- file.path(OUT_DIR, "tgemo.owl")
OUT_JSON <- file.path(OUT_DIR, "strain_list.json")

EFO_URL   <- "https://github.com/EBISPOT/efo/releases/download/v3.79.0/efo.obo"
TGEMO_URL <- "https://raw.githubusercontent.com/PavlidisLab/TGEMO/master/TGEMO.OWL"

REMOVE_URIS <- c(
  "http://www.ebi.ac.uk/efo/EFO_0004000",  # Mus musculus strain type
  "http://www.ebi.ac.uk/efo/EFO_0003013"   # Mus musculus subspecies
)

if (!file.exists(EFO_OBO)) {
  message("Downloading EFO v3.79.0 ...")
  utils::download.file(EFO_URL, EFO_OBO, mode = "wb")
}
if (!file.exists(TGEMO_OWL)) {
  message("Downloading TGEMO ...")
  utils::download.file(TGEMO_URL, TGEMO_OWL, mode = "wb")
}

message("Loading EFO ...")
efo <- get_ontology(EFO_OBO, extract_tags = "everything")
mice <- get_descendants(efo, "NCBITaxon:10090", exclude_roots = TRUE)
mice <- mice[!grepl("NCBITaxon", mice)]
# Strip the OBO node namespace prefix (e.g. "efo:EFO_0004000" -> "EFO_0004000")
# before building the URI; v3.79.0 uses lowercase "efo:" prefixes that would
# otherwise produce malformed ".../efo_EFO_xxx" URIs and silently bypass the
# REMOVE_URIS filter below.
mice_local <- sub("^[^:]+:", "", mice)
mice_uris  <- paste0("http://www.ebi.ac.uk/efo/", mice_local)
mice <- mice[!mice_uris %in% REMOVE_URIS]
message("EFO mouse strains: ", length(mice))

extract_synonyms <- function(syns) {
  if (length(syns) == 0) return(character(0))
  s <- str_extract_all(syns, '(?<=").*?(?=")')
  s <- unlist(s)
  s[nzchar(s)]
}

strip_def <- function(d) {
  if (is.null(d) || length(d) == 0 || is.na(d)) return("")
  d <- gsub('\\[|\\]|\\"', "", d)
  trimws(d)
}

# Strip any leading namespace prefix on the OBO node id ("efo:EFO_xxx" or "EFO:0000xxx")
# before building the URI. The original repo's 00.downloads.R did a naive
# gsub(":", "_", ...) which produces http://www.ebi.ac.uk/efo/efo_EFO_xxx rather
# than .../EFO_xxx when OBO ids have a namespace prefix; EFO v3.79.0 OBO uses
# lowercase "efo:" prefixes throughout.
efo_local <- sub("^[^:]+:", "", mice)
efo_list <- lapply(seq_along(mice), function(i) {
  list(
    URI         = unbox(paste0("http://www.ebi.ac.uk/efo/", efo_local[i])),
    value       = unbox(efo$name[[mice[i]]] %||% ""),
    description = unbox(strip_def(efo$def[[mice[i]]])),
    synonyms    = extract_synonyms(efo$synonym[[mice[i]]])
  )
})

# TGEMO via rdflib (we don't run ROBOT). Match the original R/00.downloads.R logic:
# direct is_a children of NCBITaxon:10090.
message("Loading TGEMO ...")
g <- rdf_parse(TGEMO_OWL, format = "rdfxml")
sparql <- '
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX obo:  <http://purl.obolibrary.org/obo/>
PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>
PREFIX iao: <http://purl.obolibrary.org/obo/IAO_0000115>

SELECT ?cls ?label ?def
       (GROUP_CONCAT(?syn; separator = "||") AS ?syns)
WHERE {
  ?cls rdfs:subClassOf obo:NCBITaxon_10090 .
  OPTIONAL { ?cls rdfs:label ?label }
  OPTIONAL { ?cls iao: ?def }
  OPTIONAL {
    { ?cls oboInOwl:hasExactSynonym ?syn } UNION
    { ?cls oboInOwl:hasRelatedSynonym ?syn } UNION
    { ?cls oboInOwl:hasBroadSynonym ?syn } UNION
    { ?cls oboInOwl:hasNarrowSynonym ?syn }
  }
}
GROUP BY ?cls ?label ?def
'
tgemo_df <- rdf_query(g, sparql)
tgemo_df <- tgemo_df[grepl("TGEMO_", tgemo_df$cls), ]
message("TGEMO mouse strains: ", nrow(tgemo_df))

tgemo_list <- lapply(seq_len(nrow(tgemo_df)), function(i) {
  local <- sub(".*/", "", tgemo_df$cls[i])
  syns  <- if (is.na(tgemo_df$syns[i]) || tgemo_df$syns[i] == "") character(0)
           else strsplit(tgemo_df$syns[i], "\\|\\|", fixed = FALSE)[[1]]
  list(
    URI         = unbox(paste0("http://gemma.msl.ubc.ca/ont/", local)),
    value       = unbox(tgemo_df$label[i] %||% ""),
    description = unbox(strip_def(tgemo_df$def[i])),
    synonyms    = syns
  )
})

strain_list <- c(efo_list, tgemo_list)
# Dedupe by URI (paranoid; original R code didn't but doesn't hurt).
uris <- vapply(strain_list, function(x) unclass(x$URI), character(1))
strain_list <- strain_list[!duplicated(uris)]

writeLines(toJSON(strain_list, pretty = TRUE, auto_unbox = FALSE), OUT_JSON)
message(sprintf("Wrote %s: %d strains (%d EFO, %d TGEMO)",
                OUT_JSON, length(strain_list), length(efo_list), length(tgemo_list)))
