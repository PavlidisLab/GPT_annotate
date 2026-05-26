#!/usr/bin/env bash
# bundle_for_zenodo.sh — assemble the Zenodo deposit for the revision round
# of the GPT_annotate manuscript.
#
# Copies everything we'd want a third party to be able to reproduce or
# audit the revision-round analyses into a single tree, with sha256 manifest
# and a README. Source paths:
#
#   ./revisions/data/            — derived TSVs, sample manifests, dictionaries
#   ./revisions/data/results/    — per-GSE LLM outputs (strain task)
#   ./revisions/data/results_cl/ — per-GSE LLM outputs (cell-line task)
#   ./revisions/data/analysis/   — derived analysis tables
#   ./analysis/                  — curator-overlay data + scoring scripts
#   ./revisions/METHODS.md   (only protocol; not the lab-internal
#                              FINDINGS / SENSITIVITY interpretation
#                              notes — those are excluded by design)
#   ~/Documents/manuscripts/GPT-annotate-manuscript/  — figures + supp tables
#
# Excluded by design (too large, regenerable, or public): geo_cache,
# paper_cache, cache/, cell_line_embeddings.npy, robot.jar, the raw OBO
# files. Version pins for those are recorded in ONTOLOGY_VERSIONS.md inside
# the bundle.
#
# Usage:
#   ./revisions/bundle_for_zenodo.sh
#       → writes to ~/Documents/manuscripts/GPT-annotate-manuscript/zenodo_bundle/
#       and creates a tar.gz beside it.
#
#   DRY_RUN=1 ./revisions/bundle_for_zenodo.sh   # print but don't copy
#
#   OUT_DIR=/tmp/zenodo ./revisions/bundle_for_zenodo.sh   # override dest

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"
cd "$REPO_ROOT"

MANUSCRIPT_DIR="${MANUSCRIPT_DIR:-/Users/pzoot/Documents/manuscripts/GPT-annotate-manuscript}"
OUT_DIR="${OUT_DIR:-$MANUSCRIPT_DIR/zenodo_bundle}"
TARBALL="${OUT_DIR%/}.tar.gz"

# --- helpers ----------------------------------------------------------------
run() {
    if [[ "${DRY_RUN:-0}" == "1" ]]; then
        printf "  [dry-run] %s\n" "$*"
    else
        eval "$@"
    fi
}

cp_into() {
    # cp_into <src> <dst-subdir-inside-bundle>
    local src="$1"; local sub="$2"
    local dst="$OUT_DIR/$sub"
    if [ ! -e "$src" ]; then
        printf "  SKIP (missing): %s\n" "$src" >&2
        return 0
    fi
    run "mkdir -p $(printf %q "$dst")"
    run "cp -R $(printf %q "$src") $(printf %q "$dst/")"
    printf "  + %s → %s\n" "$src" "$sub"
}

# --- preflight --------------------------------------------------------------
if [[ "${DRY_RUN:-0}" != "1" ]]; then
    rm -rf "$OUT_DIR" "$TARBALL"
    mkdir -p "$OUT_DIR"
fi
echo "Bundling into: $OUT_DIR"
echo

# --- 1. data: sample manifests + derived TSVs + dictionaries ----------------
echo "[1/7] data/"
cp_into revisions/data/sample500.tsv                       data
cp_into revisions/data/sample_cell500.tsv                  data
cp_into revisions/data/sample_noise20.tsv                  data
cp_into revisions/data/sample100_for_gpt4o_sanity.tsv      data
cp_into revisions/data/strain_list.json                    data
cp_into revisions/data/cell_line_list.json                 data
cp_into revisions/data/cell_line_isa.json                  data
cp_into revisions/data/cell_line_embedding_ids.json        data
cp_into revisions/data/strain_main_frame.tsv               data
cp_into revisions/data/cell_line_main_frame.tsv            data
cp_into revisions/data/long_curation_sheet.tsv             data
cp_into revisions/data/sonnet_curation_sheet.tsv           data
cp_into revisions/data/tgemo.owl                           data
# Non-reproducible analysis inputs (LLM-touched, paid API spend):
# OpenAI text-embedding-3-large vectors over the cell-line dictionary.
# ~$10 in OpenAI API spend to regenerate; ship the actual bytes.
cp_into revisions/data/cell_line_embeddings.npy            data
# Cell-line BM25 sparse index — deterministic and small, included so
# the analysis bundle is self-contained (no rebuild needed to re-run
# the hybrid retrieval scoring).
cp_into revisions/data/cell_line_bm25.pkl                  data

# --- 2. per-GSE LLM outputs (strain task) -----------------------------------
echo
echo "[2/7] results_strain/  (per-GSE JSON + summary TSVs)"
for d in revisions/data/results/*/; do
    name=$(basename "$d")
    cp_into "$d" "results_strain/$name"
done

# --- 3. per-GSE LLM outputs (cell-line task) --------------------------------
echo
echo "[3/7] results_cell_line/  (per-GSE JSON + summary TSVs)"
for d in revisions/data/results_cl/*/; do
    name=$(basename "$d")
    cp_into "$d" "results_cell_line/$name"
done

# --- 4. derived analyses ----------------------------------------------------
echo
echo "[4/7] analysis/  (derived TSVs)"
for f in revisions/data/analysis/*.tsv; do
    cp_into "$f" analysis
done
# Curator-overlay artefacts live at the top-level analysis/ dir, not
# under revisions/. Include them too — they're load-bearing for §7.
cp_into analysis/curator_overlay_summary.tsv               analysis
cp_into analysis/curator_verdicts_2026-05-21.json          analysis
cp_into analysis/fig6_overlay_scores.tsv                   analysis

# --- 5. manuscript artefacts: figures + supplementary tables ----------------
echo
echo "[5/7] figures/ + supplementary_tables/ + manuscript drafts"
for f in "$MANUSCRIPT_DIR"/figures/*.{svg,eps,png}; do
    cp_into "$f" figures
done
for f in "$MANUSCRIPT_DIR"/supplementary_tables/*.csv; do
    cp_into "$f" supplementary_tables
done
cp_into "$MANUSCRIPT_DIR/Supplementary_Tables.xlsx"        manuscript
cp_into "$MANUSCRIPT_DIR/Figures_assembled.docx"           manuscript
cp_into "$MANUSCRIPT_DIR/ChatGPT manuscript v2.docx"       manuscript
cp_into "$MANUSCRIPT_DIR/DATABASE-2026-0041_response.PP.docx" manuscript

# --- 6. methods writeup ----------------------------------------------------
# Only METHODS.md is shipped — FINDINGS.md and SENSITIVITY_ANALYSES.md
# are lab-internal interpretation notes and are deliberately excluded
# from the deposit. The Zenodo bundle is data + protocol; the
# *interpretation* lives in the manuscript itself.
echo
echo "[6/7] writeups/"
cp_into revisions/METHODS.md                               writeups

# --- 7. build + analysis scripts --------------------------------------------
echo
echo "[7/7] scripts/  (so the bundle is reproducible from source)"
cp_into revisions/make_figures.py                          scripts
cp_into revisions/replicate.sh                             scripts/revisions
cp_into analysis/replicate.sh                              scripts/analysis
cp_into revisions/bundle_for_zenodo.sh                     scripts
# The figure / table generators that live in the manuscript folder
for f in "$MANUSCRIPT_DIR"/scripts/*.{py,sh,md}; do
    cp_into "$f" scripts/manuscript
done
# Build / analysis scripts under revisions/
for d in revisions/build revisions/analysis revisions/baselines \
         revisions/runners revisions/annotators revisions/metrics; do
    [ -d "$d" ] || continue
    cp_into "$d" scripts/$(basename "$d" | sed "s|revisions/||")
done

# --- ontology version pins (we deliberately don't ship the obo files) ------
if [[ "${DRY_RUN:-0}" != "1" ]]; then
cat > "$OUT_DIR/ONTOLOGY_VERSIONS.md" <<'EOF'
# Ontology version pins (revision round)

The Zenodo deposit deliberately does not ship the full ontology source
files (efo.obo ~84 MB, CLO.owl ~41 MB, etc.) — they're public archival
artefacts that should be obtained from their canonical sources. The
revision-round analyses used the following versions:

| Ontology | Version | Source URL | Downloaded |
|---|---|---|---|
| EFO | v3.89.0 | https://www.ebi.ac.uk/efo/efo.obo | 2026-05-13 |
| EFO (pinned for strain dictionary regression test) | v3.79.0 | https://github.com/EBISPOT/efo/releases/tag/v3.79.0 | 2026-05-14 |
| CLO | (latest at fetch time) | https://purl.obolibrary.org/obo/clo.owl | 2026-05-13 |
| TGEMO | (latest at fetch time) | https://raw.githubusercontent.com/PavlidisLab/TGEMO/master/TGEMO.OWL | 2026-05-13 |
| BTO | (used indirectly via cell-line dictionary build) | https://raw.githubusercontent.com/BRENDA-Enzymes/BTO/master/bto.obo | 2026-05-13 |
| CL | (used indirectly) | https://raw.githubusercontent.com/obophenotype/cell-ontology/master/cl-basic.obo | 2026-05-13 |

Re-fetch with `Rscript revisions/build/build_strain_list.R` /
`Rscript revisions/build/build_cell_line_list.R` from a fresh checkout
of the repo.
EOF
fi

# --- README ----------------------------------------------------------------
if [[ "${DRY_RUN:-0}" != "1" ]]; then
cat > "$OUT_DIR/README.md" <<'EOF'
# GPT_annotate — revision-round data deposit

This Zenodo deposit accompanies the revision round of the GPT_annotate
manuscript ("Application of large language models to the annotation of
cell lines and mouse strains in genomics data"). It contains all
derived data, raw per-experiment LLM outputs, figures, supplementary
tables, and writeups needed to reproduce or audit the revision-round
analyses.

## Layout

```
data/                           sample manifests, dictionaries, frames
  sample500.tsv                  500-GSE strain-task evaluation sample
  sample_cell500.tsv             500-GSE cell-line evaluation sample
  sample_noise20.tsv             20-GSE inference-noise probe
  sample100_for_gpt4o_sanity.tsv 100-GSE setup-equivalence check
  strain_list.json               156-term strain ontology dictionary
  cell_line_list.json            46,032-term cell-line ontology dictionary
  cell_line_isa.json             cell-line is_a parent/child index
  strain_main_frame.tsv          per-GSE strain results across models
  cell_line_main_frame.tsv       per-GSE cell-line results across models
  long_curation_sheet.tsv        per-(GSE, cell-line) curator-review long sheet
  sonnet_curation_sheet.tsv      per-GSE Sonnet curation sheet (original-format)

results_strain/<model>/         per-GSE JSON + summary.tsv for each strain run
results_cell_line/<model>/      per-GSE JSON + summary*.tsv for each cell-line run

analysis/                       derived tables (the inputs for the writeups)
  01_accuracy_with_ci.tsv         per-model accuracy + Wilson 95 % CI
  02_mcnemar_pairwise.tsv         every pairwise McNemar test (strain)
  03_agreement.tsv                pairwise Cohen κ + % identical (strain)
  04_error_overlap.tsv            multi-model error overlap counts
  05_top_confusions.tsv           top (truth, pred) error pairs (strain)
  06_specificity_accuracy.tsv     specificity-rule sweep
  07_specificity_mcnemar.tsv      spec-rule McNemar tests
  08_opus_noise.tsv               Opus inference-noise probe
  09_verifier_strain.tsv          quote-verifier per model
  curator_overlay_summary.tsv     post-curator overlay verdicts
  curator_verdicts_2026-05-21.json   raw curator JSON

figures/                        publication-ready figures
  Figure_3_strain_methods.{svg,eps,png}
  Figure_4_cell_line_methods.{svg,eps,png}
  Figure_5_ensemble.{svg,eps,png}
  Supp_Figure_S1_strain_PR.{svg,eps,png}
  Supp_Figure_S2_cell_line_PR.{svg,eps,png}
  Supp_Figure_S3_topk.{svg,eps,png}
  Supp_Figure_S4_verifier.{svg,eps,png}

supplementary_tables/           CSV with provenance header per file
  S1_strain_accuracy.csv          ... and 9 more.

manuscript/                     human-readable artefacts
  ChatGPT manuscript v2.docx      revision-round draft
  Figures_assembled.docx          figures + captions, one block per figure
  Supplementary_Tables.xlsx       all supplementary tables, one tab per table
  DATABASE-2026-0041_response.PP.docx   point-by-point reviewer response

writeups/                       protocol detail (data + Methods only)
  METHODS.md                      full protocol behind every analysis in
                                  the manuscript. Interpretation lives in
                                  the manuscript itself; the lab-internal
                                  FINDINGS / SENSITIVITY notes are not
                                  part of this deposit by design.

scripts/                        everything needed to reproduce the deposit
  make_figures.py                 figure generator
  revisions/replicate.sh          end-to-end revision-round driver
  analysis/replicate.sh           original-submission analysis driver
  bundle_for_zenodo.sh            this bundler
  manuscript/                     supp-table + docx + sync scripts
  build/, baselines/, analysis/,  module dirs from revisions/
  runners/, annotators/, metrics/

MANIFEST.tsv                    sha256 + size for every file in the bundle
ONTOLOGY_VERSIONS.md            EFO / CLO / TGEMO versions used (not shipped)
README.md                       this file

```

## Inclusion criterion

Everything in this deposit either (a) feeds directly into one of the
revision-round analyses or (b) cannot be cheaply regenerated from a
script. LLM-generated outputs and paid-API artefacts (OpenAI
embeddings) count as non-reproducible and are shipped as bytes.
Public archival data (GEO records, PMC papers, OBO ontology files)
and deterministic re-derivable artefacts (SapBERT embeddings, the
HuggingFace model weights, third-party tools) are excluded — every
exclusion is regenerable with one of the included scripts.

## What's deliberately *not* in the deposit

- **GEO records (~42 GB)**: re-fetch with `revisions/build/prefetch_geo.py`
  from a fresh checkout. The cached SOFT files in `revisions/data/geo_cache/`
  are public records hosted at NCBI; shipping them would bloat the deposit
  without adding archival value.
- **PMC paper text (~73 MB)**: same logic; re-fetch with the same script
  (PMC is the canonical archival source).
- **Ontology source files (efo.obo, CLO.owl, etc.)**: see
  `ONTOLOGY_VERSIONS.md` for the version pins. Re-fetch with
  `Rscript revisions/build/build_strain_list.R` /
  `revisions/build/build_cell_line_list.R`.
- **SapBERT model weights + embedding cache (~155 MB)**: deterministic;
  regenerable from the canonical HuggingFace checkpoint
  `cambridgeltl/SapBERT-from-PubMedBERT-fulltext` via
  `revisions/baselines/sapbert_baseline.py` (the model itself
  downloads on first run; the per-task dictionary embeddings are cached
  next to it).
- **robot.jar (~89 MB)**: third-party OBO conversion utility; pin from
  `https://github.com/ontodev/robot/releases/tag/v1.9.6`.

## Re-running from this deposit

```bash
# Fresh checkout of the GPT_annotate repo
git clone https://github.com/PavlidisLab/GPT_annotate.git
cd GPT_annotate

# Drop this deposit's data + analysis output into revisions/data/
cp -R <deposit>/data/*           revisions/data/
cp -R <deposit>/results_strain/* revisions/data/results/
cp -R <deposit>/results_cell_line/* revisions/data/results_cl/
cp -R <deposit>/analysis/*       revisions/data/analysis/

# Re-fetch the things we don't ship
revisions/.venv/bin/python revisions/build/prefetch_geo.py \
    --sample revisions/data/sample500.tsv
Rscript revisions/build/build_strain_list.R
Rscript revisions/build/build_cell_line_list.R

# Re-run any analysis (no LLM calls) or regenerate figures
revisions/.venv/bin/python revisions/make_figures.py
```

## Provenance

Every file in this deposit has a SHA-256 hash in `MANIFEST.tsv`.
Supplementary-table CSVs additionally carry a `#`-prefixed provenance
header naming the figure they back (see the
[supplementary-table skill](https://… your skill DOI here, if any …)).

## Citation

When citing this deposit, please cite both the manuscript and this
Zenodo DOI.

EOF
fi

# --- MANIFEST: sha256 + size for every file in the bundle ------------------
echo
echo "Building MANIFEST.tsv..."
if [[ "${DRY_RUN:-0}" != "1" ]]; then
    (
        cd "$OUT_DIR"
        printf "path\tsize_bytes\tsha256\n" > MANIFEST.tsv
        find . -type f ! -name MANIFEST.tsv \
            | sed 's|^\./||' | sort \
            | while read -r f; do
                sz=$(stat -f '%z' "$f" 2>/dev/null || stat -c '%s' "$f" 2>/dev/null)
                sh=$(shasum -a 256 "$f" | awk '{print $1}')
                printf "%s\t%s\t%s\n" "$f" "$sz" "$sh"
            done >> MANIFEST.tsv
    )
fi

# --- tarball ---------------------------------------------------------------
echo
echo "Creating tarball: $TARBALL"
if [[ "${DRY_RUN:-0}" != "1" ]]; then
    tar -czf "$TARBALL" -C "$(dirname "$OUT_DIR")" "$(basename "$OUT_DIR")"
    echo
    echo "Done."
    du -sh "$OUT_DIR" "$TARBALL"
    echo
    echo "$(wc -l < "$OUT_DIR/MANIFEST.tsv") files in MANIFEST.tsv"
fi
