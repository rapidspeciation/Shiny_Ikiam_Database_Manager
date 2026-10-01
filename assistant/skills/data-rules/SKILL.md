---
name: data-rules
description: What a senior member of the Ikiam insectary team knows about recording data in the Ithomiini workbook — which values each kind of record gets (the NA / NOT_COLLECTED / blank template of a field collection, a monitoring capture, an emergence, a death, a preservation, a wing clip, a clutch, a failed clutch, a cross), Insectary IDs, CAM pools and tubes, counts kept as sums, notes, and how the paper notebooks, envelopes and cage cards are written. Use it before proposing new rows or corrections, when a value on a page or in the sheet looks odd, when transcribing envelopes, cage cards, crosses or field notes, and to answer "how do we record X", "what goes in column Y", "which CAM/ID/tube next".
---

# Data rules (the team's conventions)

Distilled from the workbook itself (what the rows actually hold), the meeting
notes and protocols in Drive, and the team's practice up to Sep 2026. Current
practice first; old formats only where they help to read old pages.

## Principles

- The paper (notebook, envelope, cage card) is the primary record; the sheet
  lags it by days to weeks. A blank cell usually means "not yet", not an error.
- `NA` = does not apply / was not recorded. Blank = not yet (filled at death,
  when the field envelope arrives, after photos). `NOT_COLLECTED` = a sample
  that was not taken (preservation media, unused tubes, some tissues and sexes).
  Type list values exactly (`NOT_COLLECTED`, never "NOT COLLECTED").
- Never type a formula column (grey in the app; the tools say `notWritten`).
- Never invent an ID, CAM, tube or date; take the next one from the right pool
  and check it is unused (see [reference/samples-ids.md](reference/samples-ids.md)).
- A correction of species, sex, ID, CAM or tube goes to **every** sheet that
  holds the butterfly (Collection_data and Insectary_data twins, experiment
  sheets) with a note "from X to Y"; relabelling envelopes and renaming photos
  are tasks for a person.
- Where the sources disagree (marked **Ask** in the files), say what the
  current practice is and ask; never settle it silently.
- `match_notebook`, `check_data`, `get_walk` and the app's tabs already apply
  many of these rules; the files say so where a tool does it, so don't redo it
  by hand.

## Which file

| Case | File |
|---|---|
| Field collection: the record kinds of Collection_data and their fixed values, collectors, places, species/subspecies, sex, time, weight, twins with Insectary_data | [reference/field-collections.md](reference/field-collections.md) |
| Monitoring walks, mark–release, recaptures, the 30-preserved rule, weather codes (official vs paper), SamplingDay_data | [reference/monitoring.md](reference/monitoring.md) |
| An insectary butterfly's life in Insectary_data: pre-made rows, emergence, wing clip, death, preservation, eggs/larvae (LIFESTAGE), CRISPR controls, disappearance sweeps | [reference/insectary-individuals.md](reference/insectary-individuals.md) |
| Clutches in Insectary_stocks: numbers and batches, counts as sums, dashes/NA/0, failed clutches, owners, generation, parents | [reference/clutches.md](reference/clutches.md) |
| Crosses, families, pedigree: notation, protocol steps, which sheet, what to ask | [reference/crosses.md](reference/crosses.md) |
| Insectary IDs, CAM pools, tubes and racks, tissues, preservation media, duplicates and ID corrections | [reference/samples-ids.md](reference/samples-ids.md) |
| Notes columns: format, language, what must and must never go in them, standard phrases | [reference/notes.md](reference/notes.md) |
| Reading paper: ditto marks, braces, dashes, ticks, highlights, pre-written ID rows, envelopes, cage cards, whiteboards, symbols and shorthand | [reference/reading-paper.md](reference/reading-paper.md) |

Read the file of the case before proposing; for a notebook photo the skill
**digitalizar-cuaderno** comes first and points here.

## Who knows what (sheet initials)

PAS: the workbook's owner (CAM ranges, pre-made rows, columns and formulas,
Pedigree, taxonomy). AO: notebook → sheet transfer since Sep 2026. AA: crosses
and monitoring. ABV: field collections. KG: pheromones, photos, stock census.
MJS: the previous curator (history of old rows). When you draft a question for
the team, say who is likely to know.
