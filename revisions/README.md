# revisions/

Replication of Rogic et al. (2026) "Application of large language models to
the annotation of cell lines and mouse strains in genomics data" with
Anthropic Claude models substituted for OpenAI GPT-4o. All other
methodological choices are held fixed and follow the original protocol.

## Layout

### Strain task

| Path | Purpose |
|---|---|
| `build_strain_list.R` | Build the 156-strain (140 EFO + 16 TGEMO) ontology JSON, matching `analysis/strains/00.downloads.R` exactly. EFO pinned to v3.79.0. |
| `find_efo_version.py` | One-off probe across EFO releases used to pick the version pin. |
| `geo_fetch.py` | Fetch + cache GEO SOFT family files and PMC BioC article passages with exponential backoff. Builds the per-experiment input JSON in the same shape as `R/geo_utils.R::create_input`. |
| `prefetch_geo.py` | Warm the GEO+PMC caches before running any LLM jobs. |
| `strain_annotate.py` | Annotate a single GSE with Claude using tool-use-enforced structured output. |
| `run_sample.py` | Parallel runner over the strain sample TSV; caches per-experiment JSON and writes `summary.tsv`. Model selected via `CLAUDE_MODEL` env var. |
| `sample_gse.R` | Draw a uniform random sample of GSEs from `data-raw/strain_data/main_frame.rds`. |
| `sample_for_noise.R` | 20-GSE subsample used for the Opus noise study. |
| `strain_prompt.txt` | Verbatim copy of `analysis/strains/prompt`. |
| `strain_prompt_specificity.txt` | Same prompt with one extra rule preferring the most general strain. |

### Cell-line task

| Path | Purpose |
|---|---|
| `build_cell_line_list.R` | Build the cell-line ontology JSON from EFO + CLO + BTO + a subset of CL. |
| `build_cell_line_index.py` | Embed every ontology term with OpenAI `text-embedding-3-large` and write the FAISS-equivalent NumPy index used for retrieval. |
| `build_cell_line_xrefs.py` | Walk EFO/CLO/BTO `xref` / `seeAlso` / `IAO:0000118` / synonym edges and emit a single cross-walk equivalence index used by the evaluator. |
| `cell_line_annotate.py` | Two-stage Claude annotation: open extraction → top-50 retrieval → constrained selection via `report_cell_lines` tool call. Robust to Opus 4.7 tool-input stringification. |
| `run_cell_line_sample.py` | Parallel runner for the cell-line sample; caches per-GSE JSON. |
| `sample_cell_lines.R` | Draw the 500-GSE cell-line sample. Output uses `\|\|\|` as the list-item separator (commas occur inside cell-line names). |
| `export_cell_main_frame.R` | Convert `main_frame.rds` to the TSV consumed by the runner. |
| `cell_line_prompt_p1.txt`, `cell_line_prompt_p2.txt` | Verbatim copies of `analysis/cell_lines/prompt` and `rag_prompt`. |
| `validate_index.py` | Sanity-check the retrieval index against known cell-line names. |

### Evaluation, sensitivity, ensemble

| Path | Purpose |
|---|---|
| `cell_line_eval.py` | Per-GSE evaluator: exact-ID and cross-walk-name-match accuracy against Gemma truth, plus the same metrics on the published GPT-4o predictions. |
| `cell_line_inherit_curator.py` | Inherit GPT-4o curator verdicts onto Sonnet predictions where the two models' URI sets are cross-walk equivalent. Emits `summary_inherit.tsv` with six verdict buckets (auto_exact, auto_name, inherit_perfect, inherit_wrong, gpt_exact_we_differ, needs_review). |
| `build_sonnet_curation_sheet.py` | Build the 161-row curator-review TSV for the residual `needs_review` Sonnet predictions, formatted to match the original `curation.tsv` column schema. |
| `disagreement_analysis.py` | Pairwise correctness × pairwise prediction agreement on the cell-line task; emits the both-wrong decomposition. |
| `ensemble_analysis.py` | Three-way ensemble: marginal McNemar, pairwise Cohen's κ, all-three-agree intersection, 2-of-3 majority. Joined to per-GSE TSV. |
| `topk_sensitivity.py` | Re-run the cell-line second pass at K ∈ {10, 25, 50, 100, 200} on a 100-GSE subset, reusing cached first-pass extractions. |
| `aggregate_topk.py` | Aggregate the five per-K eval summaries into one table. |
| `text2term_baseline.py` | Run text2term (TFIDF mapper) against the same 156-term strain dictionary on the full 500-GSE sample. |
| `analyze_strain_results.R` | Reproducible statistical-analysis driver for the strain task: emits the eight tables in `data/analysis/01_..._.tsv` through `08_..._.tsv`. Wilson 95 % CIs, McNemar, Cohen's κ. |
| `noise_analysis.R`, `model_correlation.R`, `test_specificity_gain.R` | Single-purpose analysis scripts; superseded by the driver above but kept as readable references. |

### Data

| Path | Purpose |
|---|---|
| `data/strain_list.json` | The committed 156-term strain ontology. |
| `data/cell_line_list.json` | The committed cell-line ontology with synonyms used for retrieval and cross-walk. |
| `data/sample500.tsv` | Strain-task random sample. |
| `data/sample_cell500.tsv` | Cell-line task random sample. |
| `data/sonnet_curation_sheet.tsv` | 161-row curator-review sheet for Sonnet `needs_review` cases. |
| `data/results/<model_id>/` | Per-GSE strain responses + per-model `summary.tsv`. |
| `data/results_cl/<model_id>[/_topk{K}]/` | Per-GSE cell-line responses (and top-K variants) + `summary.tsv`, `summary_eval.tsv`, `summary_inherit.tsv`. |
| `data/results_cl/ensemble.tsv` | Per-GSE join of Sonnet, Opus, GPT-4o predictions + correctness flags + pairwise-agreement flags. |
| `data/results_cl/topk_sensitivity_summary.tsv` | Accuracy vs K table. |
| `data/results/text2term_strain/` | text2term per-GSE predictions and aggregate summary. |
| `data/analysis/` | Machine-readable copies of every published statistical table. Regenerate with `Rscript revisions/analyze_strain_results.R`. |

Large downloads (EFO/TGEMO/CLO/BTO ontology files, GEO SOFT cache, PMC paper
cache, embedding index) land in `data/` and are excluded from git. They can
be re-fetched by re-running the corresponding `build_*` / `prefetch_*`
scripts.

## Reproduce

```bash
# 1. one-time setup
python3 -m venv revisions/.venv
revisions/.venv/bin/pip install anthropic openai obonet rdflib networkx \
    text2term pandas numpy google-auth google-api-python-client
Rscript -e 'install.packages(c("ontologyIndex","rdflib","jsonlite","magrittr","stringr"))'

# 2. ontologies + samples
Rscript revisions/build_strain_list.R          # -> data/strain_list.json
Rscript revisions/build_cell_line_list.R       # -> data/cell_line_list.json
revisions/.venv/bin/python revisions/build_cell_line_index.py
revisions/.venv/bin/python revisions/build_cell_line_xrefs.py
Rscript revisions/sample_gse.R 500 revisions/data/sample500.tsv
Rscript revisions/sample_cell_lines.R          # -> data/sample_cell500.tsv

# 3. warm caches (~10 min)
revisions/.venv/bin/python revisions/prefetch_geo.py --sample revisions/data/sample500.tsv --workers 3

# 4a. STRAIN task — annotate with each model (~20 min each)
revisions/.venv/bin/python revisions/run_sample.py --sample revisions/data/sample500.tsv --workers 6
CLAUDE_MODEL=claude-opus-4-7           revisions/.venv/bin/python revisions/run_sample.py --sample revisions/data/sample500.tsv --workers 6
CLAUDE_MODEL=claude-haiku-4-5-20251001 revisions/.venv/bin/python revisions/run_sample.py --sample revisions/data/sample500.tsv --workers 6
Rscript revisions/analyze_strain_results.R

# 4b. CELL-LINE task — annotate (~45 min Sonnet, ~60 min Opus)
revisions/.venv/bin/python revisions/run_cell_line_sample.py --sample revisions/data/sample_cell500.tsv --workers 4
CLAUDE_MODEL=claude-opus-4-7 revisions/.venv/bin/python revisions/run_cell_line_sample.py --sample revisions/data/sample_cell500.tsv --workers 4
revisions/.venv/bin/python revisions/cell_line_eval.py --results revisions/data/results_cl/claude-sonnet-4-6 --sample revisions/data/sample_cell500.tsv
revisions/.venv/bin/python revisions/cell_line_inherit_curator.py
revisions/.venv/bin/python revisions/disagreement_analysis.py
revisions/.venv/bin/python revisions/ensemble_analysis.py

# 5. sensitivity analyses
revisions/.venv/bin/python revisions/topk_sensitivity.py --sample revisions/data/sample_cell500.tsv --n 100 --k 10 25 50 100 200
revisions/.venv/bin/python revisions/aggregate_topk.py
revisions/.venv/bin/python revisions/text2term_baseline.py --sample revisions/data/sample500.tsv
```

The Anthropic API key is read from `ANTHROPIC_API_KEY` (env var) or, on macOS,
from the keychain via `security find-generic-password -s ANTHROPIC_API_KEY -w`.
The OpenAI key (for `text-embedding-3-large` on the cell-line task) is
similarly read from `OPENAI_API_KEY` or the keychain.
