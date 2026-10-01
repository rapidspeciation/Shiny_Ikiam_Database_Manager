---
name: data-review
description: Finding and fixing problems in the Ithomiini workbook and the app's Revisión tab — checking the data or one sheet or row (check_data), "aplica las correcciones acordadas" (the fixes people accepted in Revisión), the suggested edits of Revisión → Sugerencias, and the alerts (CAM ranges running out, the 30-preserved rule). Use it when the person asks to check the data, what is wrong somewhere, to apply agreed or suggested corrections, or about CAM ranges and alerts.
---

# Data review

The app's **Revisión** tab (`https://ithomiini-ikiam.com/#/revision`) shows
the same lists as the tools:

| View | Tool |
|---|---|
| «Problemas»: cards people judge (accepted, rejected, another value) | `check_data`; the verdicts: `list_agreed_fixes` |
| «Sugerencias» | `list_suggested_edits` |
| «Resueltos»: what the sheet no longer has, with who changed it | — |
| «Alertas» | `get_alerts` |

Nothing is written without a proposal the person confirms.

## Requests

- **"¿Qué está mal?"**: `check_data` counts first, then one kind at a time.
  For an issue without a fix, look at the rows (`get_record`) and for evidence
  before asking: a correction note ("from X to Y"), the envelope (`envelope_*`
  issues of the same row), or `list_suggested_edits` source `twins`, which
  weighs the history and the envelope for the two rows of one butterfly.
- **"Aplica las correcciones acordadas"**: `list_agreed_fixes` → one
  `propose_changes` with its `issueIds` → tell the person in a few lines what
  it changes, the Drive `tasks` as a checklist (people mark them done in
  Revisión) and the `needsValue` / `stale` questions → apply on their
  confirmation. Those issues then show as applied in Revisión.
- **Suggested edits** ("propón las sugerencias seguras de tubos"):
  `list_suggested_edits` with those filters → one `propose_changes` → their
  confirmation.
- **Alerts**: a CAM range running out → say PAS or AA hand out the next one;
  a species at 30 preserved → future captures are marked and released (skill
  `monitoring`).
