# Curator review — cell-line audit sheets

Two parallel sheets are produced for the 159 GSEs in Sonnet 4.6's
`needs_review` bucket (the cell-line experiments for which Sonnet's
prediction set is not cross-walk-equivalent to either Gemma's truth set
or the published GPT-4o prediction set).

| Tab | Granularity | When to use |
|---|---|---|
| `claude-cell-line-audit` | One row per GSE | Per-experiment overview; matches the column schema of the original Rogic et al. `curation.tsv` |
| `claude-cell-line-audit-long` | One row per (GSE, cell-line concept) | Easier per-line scanning; supports the `auto_accept` short-circuit |

Both sheets cover the same 159 GSEs; the long sheet expands each
experiment into one row per cross-walk equivalence class touched by
any source (Gemma, Sonnet 4.6, Opus 4.7, or GPT-4o).

## Long-format sheet (`claude-cell-line-audit-long`)

Each row represents one cross-walk equivalence class within one
experiment. The display columns (B, C) give a canonical name and URI
for the class; the per-source columns (D–G) record which source picked
any URI in that class; columns H–K hold the actual URI each source
used; columns L–M carry verbatim supporting quotes from the LLM
first-pass extractions.

| Col | Header | Meaning |
|---|---|---|
| A | `shortName` | GEO accession (hyperlinked to the Gemma experiment page) |
| B | `canonical_label` | Display label for the cross-walk equivalence class. **Not any single model's prediction** — it is the canonical name chosen by ontology priority CLO > EFO > BTO > CL, taken from `cell_line_terms.json::names[0]`. Same equivalence class always shows the same label. |
| C | `canonical_uri` | Display URI for the equivalence class, chosen by the same CLO > EFO > BTO > CL priority. |
| D | `in_gemma` | ✓ if Gemma annotated any URI in this class on this experiment |
| E | `in_claude` | ✓ if Claude Sonnet 4.6 predicted any URI in this class |
| F | `in_opus` | ✓ if Claude Opus 4.7 predicted any URI in this class |
| G | `in_gpt4o` | ✓ if the published GPT-4o predicted any URI in this class |
| H | `gemma_uri` | The actual URI Gemma used (if any), e.g. `CLO:0003684` |
| I | `claude_uri` | The actual URI Sonnet picked (if any), e.g. `EFO:0001185` |
| J | `opus_uri` | The actual URI Opus picked (if any) |
| K | `gpt4o_uri` | The actual URI GPT-4o picked (if any) |
| L | `claude_quotes` | Verbatim quote(s) from Sonnet's first-pass extraction that mapped to this class. `❗` separates multiple quotes for the same line. Empty if Sonnet did not pick this class. |
| M | `opus_quotes` | Same, from Opus. Empty if Opus did not pick this class. |
| N | `picked_by` | Summary string, e.g. `Gemma + Claude + Opus + GPT-4o` or `Claude only` |
| O | `auto_accept` | `TRUE` if Gemma plus at least one frontier model picked this class. Rows with `auto_accept = TRUE` can be soft-accepted without review (see below). |
| P | `curator_verdict` | Blank — curator fills (TP / FP / FN / notes) |
| Q | `notes` | Blank — curator free-text |

### The `auto_accept` short-circuit

`auto_accept = TRUE` flags rows that do not need fresh curator review.
Three independent rules trigger it:

| Rule | When it fires | Why it is safe |
|---|---|---|
| **A. Gemma + model consensus** | Gemma annotated the equivalence class *and* at least one of Sonnet / Opus / GPT-4o picked a URI in the same class. | Ground truth and an independent extractor agree. |
| **B. No AI call** | No frontier model picked the row (Gemma-only annotation, or the placeholder "(no annotations)" row). | Gemma's curators already validated the annotation; the audit is evaluating the *models*, not re-validating Gemma. Sensitivity-style false negatives are still inspectable from the per-GSE sheet if needed. |
| **C. Solo GPT-4o** | Only GPT-4o picked the row (Gemma, Sonnet, and Opus are all blank). | GPT-4o's predictions were curated in the original study; their TP/FP status is recorded in `curation.tsv`. Re-reviewing them here adds no information about Sonnet 4.6 or Opus 4.7. |

Disagreeing models on a row that is otherwise auto-accepted are by
construction silent *on this row* (their `in_X` is blank); whether
they predicted something else for the same experiment is recorded on
the corresponding other row, which has its own `auto_accept`
evaluation. Silence on a row means "this source's other predictions
for the experiment do not include this exact concept", not "this
source disagrees with the concept."

In practice the three rules together skip ~60 % of long-sheet rows;
curators can filter `auto_accept = FALSE` and focus on the remainder.
The unreviewed pool is dominated by Sonnet-only, Opus-only, and
model-without-Gemma agreement rows — exactly the cases that decide
Sonnet's and Opus's precision / recall against Gemma.

### Quotes (`claude_quotes`, `opus_quotes`)

When Sonnet or Opus picked the class, the column carries the verbatim
quotes the model attached to its first-pass extraction (typically the
relevant `characteristics` line from the GSM record plus a sentence
from the study summary or paper). Multiple quotes within one
extraction are joined by `❗`. Multiple first-pass extractions that
collapse onto the same equivalence class contribute their quotes
sequentially.

Gemma and GPT-4o do not provide per-annotation quotes (the published
artifacts only record the final term), so those columns are not
available.

## Per-GSE sheet (`claude-cell-line-audit`)

One row per experiment, formatted to match the original Rogic et al.
`curation.tsv` column schema. The same `❗`/`|||`-separated quote and
list conventions apply. Two additional columns at the end
(`opus_cell_line_term_id`, `ensemble_label`) record Opus 4.7's
prediction plus a categorical agreement label:

| Label | Meaning |
|---|---|
| A | Opus matches Gemma (Sonnet is the outlier) |
| B | Opus agrees with Sonnet; both differ from Gemma |
| C | Opus agrees with GPT-4o; both differ from Gemma |
| D | All three frontier models disagree |
| E | Opus extracted no cell line — possibly `information_unavailable` |

Two cross-walk-aware overlap summaries (`claude_vs_gemma`,
`opus_vs_gemma`) report the count of Gemma annotations matched by each
model and list the matched line names.

## Workflow

1. Open the long sheet (`claude-cell-line-audit-long`) and filter
   `auto_accept = TRUE` away — those rows do not need review.
2. For each remaining row, click the `shortName` link to open the
   Gemma experiment record; cross-check the verbatim quotes in
   `claude_quotes` / `opus_quotes` against the metadata.
3. Decide whether each unreviewed line is a TP (the line is genuinely
   used in the experiment), FP (model hallucinated), or FN
   (Gemma-only line that all models missed), and fill
   `curator_verdict` / `notes`.
4. The per-GSE sheet supports the aggregated specific / sensitive /
   match verdicts when the experiment-level call is needed.

## Special cases

- **Parent-collapse rows.** When the model maps a paper-specific
  experimental cell line (e.g. an iPSC sub-line) to a parent
  ontology term, the parent appears once on the long sheet with the
  combined quotes from every source first-pass extraction. The
  ontology has no entry for the sub-line; the curator can mark
  these `parent-only — no specific ontology term available` in the
  notes.
- **Cross-walk rows.** Some rows show the same canonical concept
  with different URIs across sources (e.g. Gemma `CLO:0003684 HeLa
  cell`, Sonnet `EFO:0001185 HeLa`). The cross-walk index merges
  them into a single equivalence class; the row shows ✓ in both
  source columns and the distinct URIs in columns H and I.
- **Information-unavailable rows.** GSEs where neither model could
  extract a cell line are emitted as a single row with
  `picked_by = (no annotations)` and all check marks blank. The
  ensemble label `E` in the per-GSE sheet flags the same condition.
