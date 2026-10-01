# Clutches (Insectary_stocks): one row per batch of eggs

The Posturas notebook ("INSECTARY STOCK", brown book, two facing pages) holds
every clutch laid in the insectary plus eggs and larvae brought from the field.
A clutch is registered when a plant with eggs leaves a stock cage (about 8–10
eggs; the plant is taped with date, species and egg count).

## Columns

Typed: `CLUTCH NUMBER`, `Generation`, `SPECIES`, `DATE LAID `, `NUMBER OF
EGGS`, `INSECTARY OR LABORATORY`, `HATCHING DATE`, `NUMBER OF LARVAE`,
`PUPA DATE `, `NUMBER OF PUPA`, `EMERGENCE DATE `, `NUMBER OF ADULTS `,
`NUMBER OF PUPAE/LARVAE FOR DISECTIONS`, `NOTES` (some headers end in a space;
the tools handle it). Formulas, never typed: `Earliest Emerge Date`, `Number of
Adults in Insectary_data`, `Species`, `HatchingTime`, `Hatch2Pupa`,
`Pupa2Adult`, `Eggs`. New rows from clutch 989 lack the two Insectary_data
formulas (the pre-made block ran out): PAS copies them down; don't type values
there.

## Clutch numbers

- One global sequence (1016 in Sep 2026), appended in **pickup order, not laying
  order**: DATE LAID can be earlier than the previous clutch's.
- All eggs of one mating share one number. Another batch of the same couple is
  `N(k)`: `994(6)`, `1004(2)` (2026, no space); `831 (3)` (2024–25, with a
  space). The first batch is usually plain `N`, sometimes `N(1)`. Follow the
  style that clutch family already has; Insectary_data must match it exactly.
  **Ask** (PAS) which style is canonical.
- `(F1)` after the number (`994(F1)`) is the generation, not a batch; `994(3) F1`
  is batch 3 of an F1 clutch.
- Old: a number ending in `W` (`446W`) = wild-parent stock (2023).

## Counts are sums, kept as formulas

- One term per day, plant or group: `=12+15`. Single values too: `=12`. A minus
  is a loss: `=27-5` (27 larvae, 5 died). A total corrected later becomes a
  subtraction: `=31+4-34`. `match_notebook` writes them; send the terms.
- Larvae > eggs (larvae found later), pupae > larvae and adults > pupae all
  happen: soft checks, never errors by themselves.
- Typical eggs per clutch (median 2024–26): 19 overall; proceriformis 31,
  werneri×proceriformis 32, intermedia 26, lysimnia 21, messenoides 16,
  deceptus 15, zaneka 12.

## NA, 0 and dashes

- A dash (`—`, `-`) in a date or count = `NA`: the stage never came.
- **Failed clutch (no hatch)**: HATCHING DATE `NA`, NUMBER OF LARVAE `=0`, then
  PUPA DATE, NUMBER OF PUPA, EMERGENCE DATE, NUMBER OF ADULTS and DISECTIONS
  `NA`; a note such as "no hatch" / "All eggs turn black".
- A `0` written in a later stage is sent as written; the team is inconsistent
  (`0` vs `NA` for pupae that never came): **Ask** before normalising.
- DATE LAID `NA` = eggs found or brought in, or the date unknown.
- `NUMBER OF PUPAE/LARVAE FOR DISECTIONS`: `NA` by default, else a sum
  (`=2+6`; words like "3 pupas; 1 larva" = 4). The dissection dates go in NOTES
  ("2 larvae dissected on 27/4").

## Room and owner

- `INSECTARY OR LABORATORY`: `Insectary` (every clutch since 2025) or
  `Laboratory` (2022–24, "Aula 11"). A line without it takes the page's room.
- `ins/<name>` (`ins/oda`, `ins/este`, `ins ESTEBAN`, sometimes written once
  above the first line) marks clutches of students who use the insectary:
  `Insectary` plus the note "mariposas de Oda" / "mariposas de Esteban"; what
  looks like `ins/lab` is `ins/oda`. The tool does this; never put the code in
  NOTES. **Ask** (AO) whether the note should stay in Spanish.

## Generation, species, parents

- `Generation`: `F1`, `F2`, `Backcross` when the page says so (F1)/(F2)/(BC);
  a stock clutch is `NA`.
- `SPECIES`: the **mother's** species, or the cross (hybrid names and `VS`
  forms: [crosses.md](crosses.md)); an exact Lists value. A wrong stocks
  species propagates to every sibling's SPECIES formula.
- Eggs or larvae from the field or an unknown mother: SPECIES `NA` + a note of
  where and who ("eggs collected in Muyuna", "Brought from the field, in 5th
  instar", "Wild M. mes. laid the eggs"); set the species once the adults are
  identified.
- Parents live only in NOTES, female first: `U8A♀ + C8B♂` (older:
  `F1 clutch parents J7A + P5A`). Check both IDs and sexes in Insectary_data;
  a male first or two females is a misread or a swapped order: ask.
- Host plant: no column yet; note it when the page gives it (**Ask** PAS for a
  column).

## Emergence columns

EMERGENCE DATE and NUMBER OF ADULTS were typed in 2022–24, not at all in 2025
(the formula columns took over) and again from the notebook in 2026. Type what
the page writes; the typed adults may differ from the Insectary_data count
(backlog, released adults): not an error.

## Dates and plausibility (medians 2024–26)

Laid → hatch 6 d (zaneka 4); hatch → pupa 15 d (intermedia 18); pupa → adult
8 d; laid → first adult 27–31 d. A clear date out of order is proposed as
written and pointed out (the team often keeps it); a month slip (laid 17 Sep,
adults 14 Jul) is likely: say which date looks wrong. Impossible dates (`31/4`)
and text dates (`~30/8`, `<18/11/23`): ask.

## Notes vocabulary (English)

"Some eggs with fungi", "Some eggs dry", "All eggs turn black", "no hatch",
"All larvae died in 1st instar", "1 pupa dead", "Plant with ants", "Plant with
fungi", "bad host plant", "larvae moved to another plant", "Larvae dissected
for cell culture", "1 larva for life history", "female dead → clutch to
stock", "10 butterflies release", "preserved 16/9". Before 2023 notes were
undated; restating a count ("1 larva muerta") was not typed.
