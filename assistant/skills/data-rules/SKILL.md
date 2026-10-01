---
name: data-rules
description: What a senior member of the Ikiam insectary team knows about recording data in the Ithomiini workbook — which values each kind of record gets (the NA / NOT_COLLECTED / blank template of a field collection, an emergence, a death, a preservation, a wing clip, a clutch, a failed clutch, a cross), Insectary IDs, CAM pools and tubes, counts kept as sums, notes, and how the paper notebooks, envelopes and cage cards are written. Use it before proposing new rows or corrections, when a value on a page or in the sheet looks odd, when transcribing envelopes, cage cards, crosses or field notes, and to answer "how do we record X", "what goes in column Y", "which CAM/ID/tube next". Monitoring walks, marks and recaptures are in the skill monitoring.
---

# Data rules (the team's conventions)

Distilled from what the workbook's rows hold, the meeting notes and protocols
in Drive, and the team's practice up to Sep 2026. Current practice first; old
formats only where they help to read old pages.

## Principles

- `NA` = does not apply or was not recorded (a dead or disappeared butterfly
  gets `NA` wherever a cell does not apply). Blank = not yet (filled at death,
  when the field envelope arrives, after the photos). `NOT_COLLECTED` = a
  sample that was not taken. Type list values exactly (`NOT_COLLECTED`, never
  "NOT COLLECTED").
- When the team's way of filling something changed, follow the recent way.
  But a change that appeared only in Sep 2026, when two new people started,
  may be their mistake: follow the way before them unless the team decided
  the change. Old rows that differ stay as they are (no mass corrections).
- A correction of species, sex, ID, CAM or tube goes to **every** sheet that
  holds the butterfly (Collection_data and Insectary_data twins, experiment
  sheets), with a note "from X to Y"; relabelling envelopes and renaming
  photos are tasks for a person.
- Where the files say "Not settled — ask X", say what the current practice is
  and ask that person.
- `match_notebook`, `check_data`, `get_walk` and the app's tabs already apply
  many of these rules; the files say where, so don't redo it by hand.

## Which file

| Case | File |
|---|---|
| Field collection: the record kinds of Collection_data and their fixed values, collectors, places, species/subspecies, sex, time, weight, twins with Insectary_data | [reference/field-collections.md](reference/field-collections.md) |
| An insectary butterfly's life in Insectary_data: pre-made rows, emergence, wing clip, death, preservation, eggs/larvae (LIFESTAGE), CRISPR controls, disappearance sweeps | [reference/insectary-individuals.md](reference/insectary-individuals.md) |
| Clutches in Insectary_stocks: numbers and batches, counts as sums, dashes/NA/0, failed clutches, owners, generation, parents | [reference/clutches.md](reference/clutches.md) |
| Crosses, families, pedigree: notation, protocol steps, which sheet, what to ask | [reference/crosses.md](reference/crosses.md) |
| Insectary IDs, CAM pools, tubes and racks, tissues, preservation media, duplicates and ID corrections | [reference/samples-ids.md](reference/samples-ids.md) |
| Notes columns: format, what must and must never go in them, standard phrases | [reference/notes.md](reference/notes.md) |
| Reading paper: ditto marks, braces, dashes, ticks, highlights, envelopes, cage cards, whiteboards, symbols and shorthand | [reference/reading-paper.md](reference/reading-paper.md) |
| Monitoring walks, marks, recaptures, the 30-preserved rule, weather codes, SamplingDay_data | skill **monitoring** (`.claude/skills/monitoring/SKILL.md`) |

Read the file of the case before proposing; for a notebook photo the skill
**digitalizar-cuaderno** comes first and points here.

## Who knows what (sheet initials)

PAS: the workbook's owner (CAM ranges with AA, pre-made rows, columns and
formulas, Pedigree, taxonomy). AO: notebook → sheet transfer since Sep 2026.
AA: crosses, monitoring, the insectary protocols. ABV: field collections. KG:
pheromones, photos, stock census. MJS: the previous curator (history of old
rows). When you draft a question for the team, say who is likely to know.
