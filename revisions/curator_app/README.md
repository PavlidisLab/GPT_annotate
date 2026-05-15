# Curator review web app

`index.html` is a self-contained, portable web app for casting
correct / wrong / unsure verdicts on the cell-line audit rows. It
embeds the row data inline (no network or server required) and runs
entirely in the browser; open it by double-clicking the file.

## What the curator sees

Each row of `revisions/data/long_curation_sheet.tsv` becomes one
review card. The card shows:

- The GEO accession (linked to the Gemma experiment page).
- The cross-walk-canonical cell-line concept (label + URI) for the row.
- The four prediction sources (Gemma, Sonnet 4.6, Opus 4.7, GPT-4o)
  stacked top to bottom; each shows whether the source picked a URI
  in the canonical class and which URI it used.
- Supporting evidence:
  - verbatim quotes from Claude Sonnet's first-pass extraction
    (when Sonnet picked the URI);
  - verbatim quotes from Claude Opus's first-pass extraction;
  - matching `characteristics` lines pulled from the cached GEO
    record, used as fallback evidence when no LLM produced a quote.
- Three vote buttons (Correct / Wrong / Unsure) and an optional note
  field.

Keyboard shortcuts: `←` / `→` navigate, `1` correct, `2` wrong,
`3` unsure, `s` skip.

## Filtering

By default the app shows only unreviewed rows whose `auto_accept`
flag is FALSE (i.e. the rows that actually need curator input). The
toolbar lets you switch to "all", "reviewed only", or include the
auto-accepted rows.

## Saving

- Every vote is auto-saved to the browser's `localStorage` under
  the key `claude-cell-line-audit-verdicts-v1`. Closing and reopening
  the tab in the same browser resumes where you left off.
- Click **Download verdicts JSON** to export the full verdict set
  for archiving or merging across curators. The JSON schema is:

  ```json
  {
    "version": "1.0",
    "saved_at": "2026-05-14T12:00:00Z",
    "total": 389,
    "completed": 124,
    "verdicts": {
      "GSE19466::EFO:0002786": {
        "verdict": "correct",
        "note": "",
        "timestamp": "2026-05-14T11:58:32Z"
      },
      ...
    }
  }
  ```

- **Import…** merges an existing JSON file into the current local
  verdicts (useful when reviewing on a new machine or merging the
  work of multiple curators).

## Regenerating the app

The `index.html` is built from the current long-format TSV:

```
revisions/.venv/bin/python revisions/curator_app/build_app.py
```

Re-run this whenever `revisions/data/long_curation_sheet.tsv` is
regenerated (e.g. after a model re-run or a cross-walk refresh).
