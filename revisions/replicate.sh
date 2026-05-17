#!/usr/bin/env bash
# replicate.sh — single-command end-to-end replication of the revisions/
# pipeline. Run from the repository root.
#
# Usage:
#   ./revisions/replicate.sh                # run everything
#   DRY_RUN=1 ./revisions/replicate.sh      # print commands without running
#   PHASES="build,strain" ./revisions/replicate.sh   # run a subset
#
# Phases (in execution order):
#   keys      Verify API keys present (Keychain + env)
#   build     Build ontology lists, embeddings, BM25 and is_a indices,
#             cross-walk, samples; export main_frame to TSV
#   fetch     Prefetch GEO SOFT + PMC paper text for the strain sample
#   strain    Run all strain-task annotations (5 frontier models + open
#             weights + spec-rule variants) + 3 non-LLM baselines
#   cell      Run all cell-line annotations (2 Claude + GPT-4o batch) +
#             SapBERT cell-line baseline; hybrid retrieval reruns for
#             all three frontier LLMs
#   ab        Paper-vs-no-paper A/B (GPT-4o batch, with-paper +
#             without-paper)
#   analyze   Aggregate stats, Cohen's kappa, McNemar, ensemble,
#             top-K sensitivity, quote verifier
#   curate    Build the curator-review sheets + curator-app HTML; refresh
#             live-Gemma cache for the audit GSEs
#   figures   Regenerate every SVG / PNG under revisions/figures/
#
# A few of the steps below are queue-and-wait against the OpenAI Batch
# API; the script polls every 60 s and resumes once a batch settles.
# Total wall time, end-to-end, is roughly 6-10 hours on a single
# machine. Total API spend with current pricing is roughly $200 across
# Anthropic, OpenAI, and Together. See revisions/METHODS.md for the
# parameter choices behind each step.

set -euo pipefail
shopt -s expand_aliases

# ---------------------------------------------------------------------------
# Locations
# ---------------------------------------------------------------------------
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"
cd "$REPO_ROOT"
REV="$REPO_ROOT/revisions"
PY="$REV/.venv/bin/python"
RSCRIPT="${RSCRIPT:-Rscript}"

PHASES="${PHASES:-keys,build,fetch,strain,cell,ab,analyze,curate,figures}"

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

# ---------------------------------------------------------------------------
# Helper: only execute a phase if it's in $PHASES
# ---------------------------------------------------------------------------
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
# keys: verify presence of API credentials (resolved via macOS Keychain
# fallbacks in each runner; we just probe here so failures surface early)
# ---------------------------------------------------------------------------
verify_keys() {
    local missing=0
    for entry in ANTHROPIC_API_KEY OPENAI_API_KEY TOGETHERAI_API_KEY; do
        if [[ "${DRY_RUN:-0}" == "1" ]]; then
            printf "  [dry-run] security find-generic-password -s %s -w\n" "$entry"
            continue
        fi
        if security find-generic-password -s "$entry" -w >/dev/null 2>&1; then
            printf "  ok: %s present in Keychain\n" "$entry"
        elif [[ -n "${!entry:-}" ]]; then
            printf "  ok: %s present in environment\n" "$entry"
        else
            printf "  MISSING: %s\n" "$entry"
            missing=$((missing + 1))
        fi
    done
    if (( missing > 0 )); then
        echo "Aborting: API credentials missing." >&2
        exit 1
    fi
}

# ---------------------------------------------------------------------------
# build: ontology dictionaries, embeddings, sparse + is_a indices, samples
# ---------------------------------------------------------------------------
build_indices() {
    # Strain dictionary (156 terms; EFO v3.79.0 + TGEMO)
    run "$RSCRIPT $REV/build/build_strain_list.R"
    # Cell-line dictionary (~46k terms; CLO + EFO + BTO + CL)
    run "$RSCRIPT $REV/build/build_cell_line_list.R"
    # Cross-walk equivalence index (xref + name bridge)
    run "$PY $REV/build/build_cell_line_xrefs.py"
    # OpenAI text-embedding-3-large dense index over cell-line terms
    run "$PY $REV/build/build_cell_line_index.py"
    # BM25 sparse index over cell-line terms (used by hybrid retrieval)
    run "$PY $REV/build/build_bm25_index.py"
    # is_a parent/child map across CLO + EFO (used by curator-app
    # gemma_related flag)
    run "$PY $REV/build/build_isa_index.py"

    # Sample TSVs
    run "$RSCRIPT $REV/build/sample_gse.R              500   $REV/data/sample500.tsv      20260513"
    run "$RSCRIPT $REV/build/sample_cell_lines.R       500   $REV/data/sample_cell500.tsv 20260513"
    run "$RSCRIPT $REV/build/sample_for_noise.R         20   $REV/data/sample_noise20.tsv 13579"
    # Cell-line main_frame export (Rogic published predictions + Gemma truth)
    run "$RSCRIPT $REV/build/export_cell_main_frame.R"
}

# ---------------------------------------------------------------------------
# fetch: warm the GEO SOFT + PMC caches for every GSE in the strain and
# cell-line samples. Subsequent annotators read from cache.
# ---------------------------------------------------------------------------
fetch_inputs() {
    run "$PY $REV/build/prefetch_geo.py --sample $REV/data/sample500.tsv      --workers 8"
    run "$PY $REV/build/prefetch_geo.py --sample $REV/data/sample_cell500.tsv --workers 8"
    run "$PY $REV/build/prefetch_geo.py --sample $REV/data/sample_noise20.tsv --workers 4"
}

# ---------------------------------------------------------------------------
# strain: every strain-task run reported in the manuscript
# ---------------------------------------------------------------------------
run_strain() {
    # Three Claude models, baseline prompt
    for model in claude-sonnet-4-6 claude-opus-4-7 claude-haiku-4-5-20251001; do
        run "CLAUDE_MODEL=$model $PY $REV/runners/run_sample.py --sample $REV/data/sample500.tsv --workers 8"
    done

    # Sonnet specificity-rule variant on the same 500
    run "CLAUDE_MODEL=claude-sonnet-4-6 STRAIN_PROMPT_PATH=$REV/strain_prompt_specificity.txt \
         $PY $REV/runners/run_sample.py --sample $REV/data/sample500.tsv --workers 8 --results-suffix specprompt"

    # Opus inference-time noise rerun on the 20-GSE noise sample
    run "CLAUDE_MODEL=claude-opus-4-7 $PY $REV/runners/run_sample.py \
         --sample $REV/data/sample_noise20.tsv --workers 4 --results-suffix rerun"

    # Open-weights Llama 3.3 70B (Together AI) — baseline + spec
    run "OPEN_PROVIDER=together OPEN_MODEL=meta-llama/Llama-3.3-70B-Instruct-Turbo \
         $PY $REV/runners/run_open_sample.py --sample $REV/data/sample500.tsv --workers 8"
    run "OPEN_PROVIDER=together OPEN_MODEL=meta-llama/Llama-3.3-70B-Instruct-Turbo \
         STRAIN_PROMPT_PATH=$REV/strain_prompt_specificity.txt \
         $PY $REV/runners/run_open_sample.py --sample $REV/data/sample500.tsv --workers 8 --results-suffix specprompt"

    # GPT-4o + specificity rule via OpenAI Batch API (Rogic-faithful)
    run "$PY $REV/runners/gpt4o_batch.py submit \
         --sample $REV/data/sample500.tsv --prompt $REV/strain_prompt_specificity.txt --suffix specprompt"
    wait_for_batch revisions/data/results/gpt-4o-2024-11-20_specprompt/_batch_meta.json \
                   "$PY $REV/runners/gpt4o_batch.py recover --suffix specprompt"
    run "$PY $REV/runners/gpt4o_batch.py finalize --suffix specprompt"

    # Non-LLM strain baselines (no LLM calls; fast)
    run "$PY $REV/baselines/text2term_baseline.py --sample $REV/data/sample500.tsv"
    run "$PY $REV/baselines/sapbert_baseline.py   --sample $REV/data/sample500.tsv"
    run "$PY $REV/baselines/bm25_strain_baseline.py --sample $REV/data/sample500.tsv --top-k 1"
}

# ---------------------------------------------------------------------------
# cell: cell-line task — Claude + Opus + GPT-4o + hybrid retrieval reruns
# ---------------------------------------------------------------------------
run_cell() {
    # Dense-retrieval first pass: Claude Sonnet + Opus (both stages real-time)
    run "CLAUDE_MODEL=claude-sonnet-4-6 $PY $REV/runners/run_cell_line_sample.py \
         --sample $REV/data/sample_cell500.tsv --workers 8"
    run "CLAUDE_MODEL=claude-opus-4-7   $PY $REV/runners/run_cell_line_sample.py \
         --sample $REV/data/sample_cell500.tsv --workers 8"

    # Hybrid dense + BM25 + RRF retrieval, reusing cached first-pass extractions
    run "CLAUDE_MODEL=claude-sonnet-4-6 $PY $REV/runners/run_hybrid_cell_line.py --workers 8 \
         --cache-dir $REV/data/results_cl/claude-sonnet-4-6 \
         --out-dir   $REV/data/results_cl/claude-sonnet-4-6_hybrid"
    run "CLAUDE_MODEL=claude-opus-4-7   $PY $REV/runners/run_hybrid_cell_line.py --workers 8 \
         --cache-dir $REV/data/results_cl/claude-opus-4-7 \
         --out-dir   $REV/data/results_cl/claude-opus-4-7_hybrid"

    # GPT-4o cell-line hybrid (uses Rogic's published Stage-1 columns + hybrid candidates)
    run "$PY $REV/runners/gpt4o_cell_line_hybrid.py submit"
    wait_for_batch revisions/data/results_cl/gpt-4o-2024-11-20_hybrid/_batch_meta.json \
                   "$PY $REV/runners/gpt4o_cell_line_hybrid.py recover"
    run "$PY $REV/runners/gpt4o_cell_line_hybrid.py finalize"

    # Top-K retrieval sensitivity on a 100-GSE subset
    run "$PY $REV/analysis/topk_sensitivity.py --sample $REV/data/sample_cell500.tsv --limit 100"

    # SapBERT cell-line baseline
    run "$PY $REV/baselines/sapbert_eval_cellline.py --sample $REV/data/sample_cell500.tsv"
}

# ---------------------------------------------------------------------------
# ab: paper-vs-no-paper A/B test on GPT-4o + spec
# ---------------------------------------------------------------------------
run_ab() {
    # No-paper arm (with-paper arm was already done in `strain` phase as
    # the spec rerun). The runner auto-skips GSEs that have no paper.
    run "$PY $REV/runners/gpt4o_batch.py submit \
         --sample $REV/data/sample500.tsv --prompt $REV/strain_prompt_specificity.txt \
         --suffix specprompt_nopaper --strip-papers"
    wait_for_batch revisions/data/results/gpt-4o-2024-11-20_specprompt_nopaper/_batch_meta.json \
                   "$PY $REV/runners/gpt4o_batch.py recover --suffix specprompt_nopaper"
    run "$PY $REV/runners/gpt4o_batch.py finalize --suffix specprompt_nopaper"
}

# ---------------------------------------------------------------------------
# analyze: all post-hoc stats and tables
# ---------------------------------------------------------------------------
run_analyze() {
    # Strain stats (Wilson CIs, McNemar, kappa, error tally, spec-rule effect)
    run "$RSCRIPT $REV/analysis/analyze_strain_results.R"
    # Inter-model agreement on cell-line task
    run "$PY $REV/analysis/disagreement_analysis.py"
    # Three-way ensemble + 2-of-3 majority
    run "$PY $REV/analysis/ensemble_analysis.py"
    # Top-K sensitivity table
    run "$PY $REV/analysis/aggregate_topk.py"
    # Specificity-rule test
    run "$RSCRIPT $REV/analysis/test_specificity_gain.R"
    # Quote-verifier pass (strict + lenient)
    run "$PY $REV/analysis/verify_quotes_strain.py"
    # Cross-model correlation
    run "$RSCRIPT $REV/analysis/model_correlation.R"
    # Opus noise analysis (per-GSE identical-prediction rate)
    run "$RSCRIPT $REV/analysis/noise_analysis.R"
    # Cell-line cross-walk + curator-inheritance metric
    run "$PY $REV/metrics/cell_line_inherit_curator.py"
}

# ---------------------------------------------------------------------------
# curate: build curator-review materials + refresh live Gemma cache for
# the audit GSEs
# ---------------------------------------------------------------------------
run_curate() {
    # Per-(GSE, cell-line class) long sheet for the curator UI
    run "$PY $REV/build/build_long_curation_sheet.py"
    # Per-GSE Sonnet curation sheet (Rogic-format compatible)
    run "$PY $REV/build/build_sonnet_curation_sheet.py"
    # Refresh live Gemma cache for the long-sheet GSEs (with .suffix
    # fallback for re-imported series)
    run "$PY $REV/gemma_fetch.py --force"
    # Build static curator-review web app (single-file HTML)
    run "$PY $REV/curator_app/build_app.py"
}

# ---------------------------------------------------------------------------
# figures: regenerate every SVG/PNG under revisions/figures/
# ---------------------------------------------------------------------------
run_figures() {
    run "$PY $REV/make_figures.py"
}

# ---------------------------------------------------------------------------
# Utility: poll an OpenAI Batch meta file until all_done; on failure run
# the supplied recover command and continue polling
# ---------------------------------------------------------------------------
wait_for_batch() {
    local meta="$1"; shift
    local recover_cmd="$1"
    if [[ "${DRY_RUN:-0}" == "1" ]]; then
        printf "  [dry-run] wait_for_batch %s (with recover-on-fail: %s)\n" "$meta" "$recover_cmd"
        return
    fi
    echo "  waiting for batch: $meta"
    local attempt=0
    while true; do
        attempt=$((attempt + 1))
        if [[ ! -f "$meta" ]]; then
            sleep 30; continue
        fi
        local suffix
        suffix="$(awk -F'"' '/suffix/ {print $4; exit}' "$meta")"
        local out
        if [[ "$meta" == *"results_cl"* ]]; then
            out=$("$PY" "$REV/runners/gpt4o_cell_line_hybrid.py" poll 2>/dev/null || true)
        else
            out=$("$PY" "$REV/runners/gpt4o_batch.py" poll --suffix "$suffix" 2>/dev/null || true)
        fi
        local done; done=$(echo "$out" | grep -o '"all_done": [a-z]*' | head -1 | awk '{print $2}')
        if [[ "$done" == "true" ]]; then
            local fail; fail=$(echo "$out" | grep -o '"n_fail": [0-9]*' | head -1 | awk '{print $2}')
            if [[ "${fail:-0}" -gt 0 ]]; then
                printf "  batch reports %s failed requests; running recover\n" "$fail"
                run "$recover_cmd"
                sleep 30; continue
            fi
            echo "  batch complete"
            break
        fi
        if (( attempt % 10 == 0 )); then
            printf "  ... still waiting (attempt %d)\n" "$attempt"
        fi
        sleep 60
    done
}

# ---------------------------------------------------------------------------
# Run the requested phases
# ---------------------------------------------------------------------------
phase keys     verify_keys
phase build    build_indices
phase fetch    fetch_inputs
phase strain   run_strain
phase cell     run_cell
phase ab       run_ab
phase analyze  run_analyze
phase curate   run_curate
phase figures  run_figures

echo
echo "All requested phases finished."
