# Clutches (Insectary_stocks): one row per batch of eggs

The Posturas notebook ("INSECTARY STOCK", brown book, two facing pages) holds
every clutch laid in the insectary plus eggs and larvae brought from the field.
A clutch is registered when a plant with eggs leaves a stock cage (about 8–10
eggs; the plant is taped with date, species and egg count).

## Formula columns

Rows from clutch 989 lack the two formulas that count from Insectary_data
(`Earliest Emerge Date`, `Number of Adults in Insectary_data`; the pre-made
block ran out): listed in «Fórmulas que faltan» for a proposal to fill; no
values are typed there.

## Clutch numbers

- One global sequence (1016 in Sep 2026), appended in **pickup order, not
  laying order**: DATE LAID can be earlier than the previous clutch's.
- All eggs of one mating share one number. New batches follow the current
  form (clutches 994–1012, Aug–Sep 2026): the first batch plain `N`, the next
  ones `N(k)` with no space: `994`, `994(2)` … `994(8)`; `1004`, `1004(2)`.
- Older rows keep theirs (`831 (3)` with a space in 2024–25; `992(1)`).
  Insectary_data writes the clutch exactly as its Insectary_stocks row does.
- `(F1)` after the number (`994(F1)`) is the generation, not a batch;
  `994(3) F1` is batch 3 of an F1 clutch.
- Old: a number ending in `W` (`446W`) = wild-parent stock (2023).

## Counts are sums, kept as formulas

- One term per day, plant or group: `=12+15`. Single values too: `=12`.
- A minus is a loss: `=27-5` (27 larvae, 5 died). A total corrected later
  becomes a subtraction: `=31+4-34`.
- NUMBER OF LARVAE holds the larvae used (team convention since Oct 2026):
  those that died or disappeared are subtracted, preserved ones and those that
  pupated are not (20 larvae, 10 preserved, 5 pupated, 5 died → `=20-5`).
  Older rows also subtracted the pupated and preserved ones; they stay.
- The sheet keeps one date per stage, the first. Each day's counts go in
  NOTES, one dated note per event: `5/10/26 FCH: 3 larvae hatched`,
  `… 5 larvae died`, `… 2 larvae disappeared`, `… 3 pupated`,
  `… 7 eggs laid on 4/10/26` (the day it happened, when not the day written).
- `match_notebook` writes them; send the terms.
- Larvae > eggs (larvae found later), pupae > larvae and adults > pupae all
  happen: soft checks, never errors by themselves.
- Typical eggs per clutch (median 2024–26): 19 overall; proceriformis 31,
  werneri×proceriformis 32, intermedia 26, lysimnia 21, messenoides 16,
  deceptus 15, zaneka 12.

## NA, 0 and dashes

- A dash (`—`, `-`) in a date or count = `NA`: the stage never came.
- **Failed clutch (no hatch)**: HATCHING DATE `NA`, NUMBER OF LARVAE `=0`,
  then PUPA DATE, NUMBER OF PUPA, EMERGENCE DATE, NUMBER OF ADULTS and
  DISECTIONS `NA`; a note such as "no hatch" / "All eggs turn black".
- A stage that never came: its date `NA`; its count as written: a dash is
  `NA`, a written `0` stays `0` (rows with hatched larvae and no pupae hold
  `0` more often than `NA`).
- DATE LAID `NA` = eggs found or brought in, or the date unknown.
- `NUMBER OF PUPAE/LARVAE FOR DISECTIONS`: `NA` by default, else a sum
  (`=2+6`; words like "3 pupas; 1 larva" = 4) of the larvae and pupae
  preserved or dissected. The Posturas notebook's column for it is headed
  "dissections" or "# larvae preserved" (`4+3+3` → `=4+3+3`). Each group
  also gets a NOTES entry with its date and Insectary IDs, for when each was
  preserved: "Larvae preserved: 4 on 1/10/26 (D7E–E0E), 3 on 7/10/26 (T9E,
  U0E, U1E)". The dates and IDs are the larvae's Insectary_data rows
  (LIFESTAGE, Preservation_date, same CLUTCH NUMBER); a group the page
  counts that has no rows there is named as such in the note.

## Room and owner

- `INSECTARY OR LABORATORY`: `Insectary` (every clutch since 2025) or
  `Laboratory` (2022–24, "Aula 11"). A line without it takes the page's room.
- `ins/<name>` (`ins/oda`, `ins/este`, `ins ESTEBAN`, sometimes written once
  above the first line) marks clutches of students who use the insectary:
  `Insectary` plus the note "Butterflies of Oda" / "Butterflies of Esteban"
  (English; older rows say "mariposas de …": that counts as said, leave it).
  What looks like `ins/lab` is `ins/oda`. `match_notebook` does this; the
  code itself does not go in NOTES.

## Generation, species, parents

- `Generation`: `F1`, `F2`, `Backcross` when the page says so (F1)/(F2)/(BC);
  a stock clutch is `NA`.
- `SPECIES`: the **mother's** species, or the cross (hybrid names and `VS`
  forms: [crosses.md](crosses.md)). A wrong stocks species propagates to
  every sibling's SPECIES formula.
- Eggs or larvae from the field or an unknown mother: SPECIES `NA` + a note
  of where and who ("eggs collected in Muyuna", "Brought from the field, in
  5th instar", "Wild M. mes. laid the eggs"); set the species once the adults
  are identified.
- Parents live only in NOTES, female first: `U8A♀ + C8B♂` (older:
  `F1 clutch parents J7A + P5A`). Check both IDs and sexes in Insectary_data; a male
  first or two females is a misread or a swapped order: ask.
- Host plant: no column; note it when the page gives it.

## Emergence columns

- EMERGENCE DATE and NUMBER OF ADULTS were typed in 2022–24, not at all in
  2025 (the formula columns took over) and again from the notebook in 2026.
- The formula columns exist to cross-check the clutches notebook against
  Emergidos, so the typed values are a second, independent count: type what
  the page writes.
- The typed adults may differ from the Insectary_data count (backlog,
  released adults): not an error.
- Prepupa and pupa dates per individual are not typed anywhere.

## Dates and plausibility (medians 2024–26)

- Laid → hatch 6 d (zaneka 4); hatch → pupa 15 d (intermedia 18); pupa →
  adult 8 d; laid → first adult 27–31 d.
- A clear date out of order is proposed as written and pointed out (the team
  often keeps it). A month slip (laid 17 Sep, adults 14 Jul) is likely: say
  which date looks wrong.
- Impossible dates (`31/4`) and text dates (`~30/8`, `<18/11/23`): ask.

## Notes vocabulary (English)

"Some eggs with fungi", "Some eggs dry", "All eggs turn black", "no hatch",
"All larvae died in 1st instar", "1 pupa dead", "Plant with ants", "Plant with
fungi", "bad host plant", "larvae moved to another plant", "Larvae dissected
for cell culture", "1 larva for life history", "female dead → clutch to
stock", "10 butterflies release", "preserved 16/9". Notes before 2023 are
undated.
