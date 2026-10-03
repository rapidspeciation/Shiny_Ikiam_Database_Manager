---
name: data-review
description: Help the person work through the inconsistencies of the Ithomiini workbook (the app's Revisión tab) — first the fixes that are certain, as one proposal to review and apply, then the doubtful ones one at a time with the evidence and the options. Use it when the person asks to check the data, a sheet or a row, what is wrong somewhere, to apply the corrections agreed in Revisión ("aplica las correcciones acordadas") or the suggested ones, or about the alerts (CAM ranges running out, the 30-preserved rule).
---

# Data review

The app's **Revisión** tab (https://ithomiini-ikiam.com/#/revision) shows
what the tools give:

| Revisión view | What it holds | Tool |
|---|---|---|
| «Problemas» | inconsistencies, as cards people judge (accept the fix, reject, give another value) | `check_data`; the judged ones: `list_agreed_fixes` |
| «Sugerencias» | corrections the app computes, each with a certainty | `list_suggested_edits` |
| «Resueltos» | problems the sheet no longer has, and who solved them | — |
| «Alertas» | CAM ranges running out, the 30-preserved rule | `get_alerts` |

## The workflow

### 1. The overview

Without filters, `check_data` gives how many issues there are of each kind,
and `list_suggested_edits` gives its sources with their counts. Tell the
person in a few lines what there is, and agree where to start (a sheet, a
kind of issue, a period) when there is a lot.

### 2. The certain fixes, as one proposal

Gather the fixes whose right value is not in doubt:

- the corrections people already agreed in Revisión (`list_agreed_fixes`);
- the suggestions with certainty `certain` (only the spelling changes: a list
  value written another way, extra spaces).

Offer them as one proposal (`propose_changes`; for agreed fixes, with their
`issueIds`, so Revisión shows them as applied). Tell the person in a few
lines what it changes, then let them review it in «Cambios propuestos» and
apply it. Very many rows: one proposal per kind of fix.

### 3. The doubtful ones, one at a time

Then go through the rest: the suggestions marked `likely` or `check` and the
issues without a fix. For each one (or a group with the same cause, such as a
run of tubes), show:

- the rows involved, side by side;
- the evidence for each value:
  - the rows around them (CAMs, tubes and IDs run in sequence);
  - correction notes: when the team corrects a species or an ID they write in
    the row's notes "from X to Y";
  - the specimen photos: what the envelope in the photo says, when it
    disagrees with the row (the issues `envelope_sex` and `envelope_species`
    of that row);
  - for a wild butterfly taken to the insectary, its two rows (Collection_data
    and Insectary_data, same Insectary_ID): when they disagree on species or
    sex, the suggestion of source `twins` says which side was corrected later
    or matches the envelope;
  - who changed the cell and when (`record_history` with the row's recordId
    and the field);
  - the paper: ask for a photo of the notebook page or the envelope;
- the options, with the one the evidence favours.

The person decides; add each decision to the same proposal
(`update_proposal`) and apply when they say so.

### Not sheet changes

| Issue | Where it is solved |
|---|---|
| Drive work on the photos (`task`: rename, move) | by a person in Drive; give them as a checklist (they are marked done in Revisión) |
| A Wikiloc point without its row (`walk_doubt`) | Monitoreo → «Dudas de emparejamiento»: say which rows fit; a person pairs it |
| A formula cell (`manual`) | by hand in Google Sheets |

## Agreed fixes: what else comes back

`list_agreed_fixes` also returns `needsValue` (accepted without a value: ask
for it) and `stale` (the row changed after the verdict: show it again).

## Alerts

- A CAM range running out: PAS or AA hand out the next range; say so.
- A species at 30 preserved: further captures from those places are marked
  and released (skill `monitoring`).
