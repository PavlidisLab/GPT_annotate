#!/usr/bin/env bash
# replicate.sh — single-command end-to-end replication of the *primary*
# analysis (the one reported in the original submission). This is kept
# separate from revisions/replicate.sh, which drives the revision-round
# pipeline. Run from the repository root.
#
# Usage:
#   ./analysis/replicate.sh                       # run everything
#   DRY_RUN=1 ./analysis/replicate.sh             # print commands without running
#   PHASES="downloads,strains" ./analysis/replicate.sh   # run a subset
#
# Phases (in execution order):
#   keys        Verify the OpenAI API key is reachable (Keychain → env
#               → ~/openai/access_key.txt fallback used by inst/gpt.py)
#   downloads   Shared ontology / jar downloads required by both tasks
#               (analysis/_downloads/download.R)
#   strains     Strain-task pipeline:
#                 00 download Gemma annotations + build strain dictionary
#                 01 regex baseline
#                 02 GPT-4o annotation (OpenAI Batch — can take hours)
#                 03 evaluate predictions vs curator
#                 04 post-curation cleanup
#   cells       Cell-line pipeline:
#                 00 download Gemma annotations + embed cell-line terms
#                 01 process cell-line dictionary
#                 02 GPT-4o first-pass annotation (OpenAI Batch)
#                 03 compare extractions to vector index
#                 04 build RAG inputs
#                 05 GPT-4o RAG resolution pass (OpenAI Batch)
#                 06 evaluate predictions vs curator
#                 07 post-curator evaluation
#
# Scripts 02 / 05 submit OpenAI Batch jobs. They block internally on
# batch completion via R/run_batches.R, so the bash driver does not
# need a poll loop of its own — the underlying R loop already handles
# resume-on-restart. Each batch can take up to 24 hours; typical
# turnaround is much faster.

set -euo pipefail
shopt -s expand_aliases

# ---------------------------------------------------------------------------
# Locations
# ---------------------------------------------------------------------------
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"
cd "$REPO_ROOT"
ANA="$REPO_ROOT/analysis"
RSCRIPT="${RSCRIPT:-Rscript}"

PHASES="${PHASES:-keys,downloads,strains,cells}"

# ---------------------------------------------------------------------------
# DRY_RUN: print + skip; otherwise execute
# ---------------------------------------------------------------------------
run() {
    if [[ "${DRY_RUN:-0}" == "1" ]]; then
        printf "  [dry-run] %s\n" "$*"
    else
        printf "  [run]     %s\n" "$*"
        eval "$@"
    fi
}

phase() {
    local name="$1"; shift
    if [[ ",$PHASES," == *",$name,"* ]]; then
        echo
        echo "============================================================"
        echo "PHASE: $name"
        echo "============================================================"
        "$@"
    else
        printf "(skipping phase: %s)\n" "$name"
    fi
}

# ---------------------------------------------------------------------------
# Keychain helper (per ~/.claude/CLAUDE.md): resolve a credential from the
# macOS Keychain into an environment variable, trying several plausible
# service names. Honours pre-set env vars so manual overrides still work.
# ---------------------------------------------------------------------------
keychain_export() {
    local var="$1"; shift
    if [[ -n "${!var:-}" ]]; then
        return 0
    fi
    local val=""
    for entry in "$@"; do
        [ -z "$entry" ] && continue
        if val=$(security find-generic-password -s "$entry" -w 2>/dev/null); then
            export "$var=$val"
            return 0
        fi
    done
    return 1
}

# ---------------------------------------------------------------------------
# keys: ensure the OpenAI key is reachable. inst/gpt.py reads (in order)
# ~/openai/access_key.txt, then OPENAI_API_KEY env. We resolve from
# Keychain into OPENAI_API_KEY when neither is already set.
# ---------------------------------------------------------------------------
verify_keys() {
    if [[ "${DRY_RUN:-0}" == "1" ]]; then
        printf "  [dry-run] resolve OPENAI_API_KEY (Keychain / env / ~/openai/access_key.txt)\n"
        return
    fi
    if keychain_export OPENAI_API_KEY \
            "${OPENAI_KEYCHAIN_ENTRY:-}" \
            "OPENAI_API_KEY" "openai" "OpenAI" "openai-api-key"; then
        echo "  ok: OPENAI_API_KEY resolved"
        return
    fi
    if [[ -f "$HOME/openai/access_key.txt" ]]; then
        echo "  ok: ~/openai/access_key.txt present (inst/gpt.py will read it)"
        return
    fi
    echo "ERROR: no OpenAI key found. Set OPENAI_API_KEY, add an entry to" >&2
    echo "       the macOS Keychain (override service name via" >&2
    echo "       OPENAI_KEYCHAIN_ENTRY=<name>), or place the key at" >&2
    echo "       ~/openai/access_key.txt." >&2
    exit 1
}

# ---------------------------------------------------------------------------
# downloads: shared ontology + tool downloads (robot.jar, CLO, EFO, TGEMO)
# ---------------------------------------------------------------------------
run_downloads() {
    run "$RSCRIPT $ANA/_downloads/download.R"
}

# ---------------------------------------------------------------------------
# strains: strain-task pipeline (sequential)
# ---------------------------------------------------------------------------
run_strains() {
    run "$RSCRIPT $ANA/strains/00.downloads.R"
    run "$RSCRIPT $ANA/strains/01.ask_regex.R"
    run "$RSCRIPT $ANA/strains/02.ask_gpt.R"
    run "$RSCRIPT $ANA/strains/03.evaluate.R"
    run "$RSCRIPT $ANA/strains/04.post_curation.R"
}

# ---------------------------------------------------------------------------
# cells: cell-line pipeline (sequential)
# ---------------------------------------------------------------------------
run_cells() {
    run "$RSCRIPT $ANA/cell_lines/00.downloads.R"
    run "$RSCRIPT $ANA/cell_lines/01.process_cell_lines.R"
    run "$RSCRIPT $ANA/cell_lines/02.ask_gpt.R"
    run "$RSCRIPT $ANA/cell_lines/03.compare_to_vect.R"
    run "$RSCRIPT $ANA/cell_lines/04.rag_inputs.R"
    run "$RSCRIPT $ANA/cell_lines/05.rag_gpt.R"
    run "$RSCRIPT $ANA/cell_lines/06.evaluate.R"
    run "$RSCRIPT $ANA/cell_lines/07.post_curator_evaluation.R"
}

# ---------------------------------------------------------------------------
# Run the requested phases
# ---------------------------------------------------------------------------
phase keys      verify_keys
phase downloads run_downloads
phase strains   run_strains
phase cells     run_cells

echo
echo "All requested phases finished."
