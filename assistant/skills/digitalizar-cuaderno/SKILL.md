---
name: digitalizar-cuaderno
description: Digitize photos of the Ikiam insectary notebooks into the Ithomiini workbook. Use it whenever the person sends or attaches a photo (one or several) of a handwritten notebook page (Posturas/clutches, Emergidos/insectary butterflies, Muertes/daily deaths, CRISPR), of a butterfly envelope or label (CAM, Reared ID, tube), or asks to "digitalizar", "pasar", "transcribir", "revisar" or "comparar con la hoja" a page or photo. It transcribes every line, matches it with the sheet through the match_notebook tool and leaves one proposal per page in Cambios propuestos for the person to apply.
---

# Digitalizar cuaderno

You read the handwriting; `match_notebook` finds each line's row, compares
every cell with the sheet and drafts **one proposal per page** beside the
chat. Send values **as written**: the tool converts them, and its
description lists each notebook's columns.

## Workflow

1. **Identify the notebook** from its headers (below). If unsure, say so in
   one line and use the closest kind; don't stop to ask.
2. **Crop and read** (see "Crops"): transcribe **every line and every
   column**, top to bottom, crossed-out lines included (`crossedOut: true`)
   and the notes. Skip only lines holding nothing but a pre-written ID or
   number.
   - A spread of two facing pages is one page: follow each line across the
     gutter (count the ruled lines from the header on both pages; highlights
     and notes help) and give its right-hand values too.
   - Before calling the tool, check the two halves are in step: the stages
     must make sense on each line (no hatch date or 0 larvae → no pupae or
     adults; "no hatch" / "all died" sit on such lines; `ins`/`lab` is on
     almost every line). If not, the right page is shifted by a line:
     re-align it.
3. **Propose at once**: one `match_notebook` per page with
   `includeUnchanged: true` (the table then follows the whole page), and
   `year` only if the page shows it. All the envelopes/labels of a message are
   one call (`kind: "labels"`). Don't look the rows up first: the tool does
   it. Pass `photo` (the attachment's file name) and `rotate` (the turn you
   gave crops.py) so the page shows upright beside its table.
4. **Second reading, only when needed** (see "Verification").
5. **Tell the person in 3–6 short lines**: which notebook and rows
   ("Posturas, clutches 120–134"), how many cells it fills, the differences
   with the sheet (sheet → notebook), what the second reading changed (if one
   was done), and the lines not found in the sheet. A small table only when
   there are several differences.
6. **Corrections** ("la línea 5 es macho"): a few cells → `update_proposal`
   on the same proposal; many lines (a shifted block) → `match_notebook`
   again with the whole page corrected and `replaceProposalId`. Say what
   changed. A page already proposed in this chat is re-matched the same way
   (one proposal per page).

From the answer:

- `missing` (an ID not in the sheet: probably misread; see `didYouMean`),
  `ambiguous` and `duplicate` lines go in your summary.
- `overlaps` = the same rows in another pending proposal: if it is the same
  page, pass its id as `replaceProposalId` next time.

Pages that are not a plain notebook table (cage cards, crosses notebook,
field envelopes): read the skill **data-rules** first.

## Doubtful and unreadable cells

Propose everything readable; doubt is a highlight, not an omission.

- **Not sure**: your best reading, with confidence, alternatives and a
  reason, as the tool describes. Use it for characters you cannot tell apart,
  not for whole columns: a value you can read, on a line you could follow, is
  sure. A clear value that looks wrong (a date out of stage order, adults >
  pupae) is sure: send it as written and point it out.
- **Cannot read it at all**: mark it unreadable as the tool describes.
- **On the cell**: the table highlights cells, so a doubt goes on the cell it
  is about, with its other readings (`¿19/9?` → alternative `19/9`); one
  about which line a value belongs to goes on that value, its `reasons`
  naming the other line ("may be S2D's"). Words in `raw` reach the person
  only as the line's text.
- **Look-alikes**: 0/O, 1/I/7, 1/4, 1/2, 3/7, 2/7, 5/S, 8/B, 2/Z, 6/G, 4/9,
  3/8, `+`/1; ♀/♂ written small. Before reading digits, compare this hand's 1
  and 7 (and 3/8) on clear cells of the same page, zoomed. Copy IDs as
  written (`600` for `6OO`; `5OS` and `50S` are two butterflies): the tool
  looks among look-alikes.
- **Crossed out**: a crossed-out date or text is not the value: the one
  beside or above it is. Counts are different (below).

## Writing the values

- **Dates** day first, as written (`17/9`, `4-8`, `19-6-23`), without adding
  the year. `~2/7` and `29/6?` are that date, doubtful; `2/9+3/9` is the
  first day.
- **Braces and dittos first** (`"`, `ll`, `||`, `〃`, a wavy line, a brace
  `}`): before filling lines, list each run per column: its value and its
  first and last ID. The run is where the brace's ends are; its value can sit
  mid-span. Then give the value to every line of the run, or once in the
  tool's `spans`. A date written once for several lines applies to all; a
  capture day written once in a bracketed note over wild lines is each
  line's `Intro2Insectary_date`. A ditto under a blank cell repeats the last
  value written above; an arrow `↑`/`↗` under a note repeats the note.
- **Dashes and blanks**: `—` or `-` alone is `"NA"`; an empty cell: leave the
  column out.
- **Sex**: ♀ = `female`, ♂ = `male`, `NA` when written so.

**Species**, as full names:

| Written | SPECIES |
|---|---|
| `messen.`, `messenoid` | Mechanitis messenoides messenoides |
| `interm.`, `inter` | Mechanitis messenoides intermedia |
| `decept` | Mechanitis messenoides deceptus |
| `pol. p.`, `polymnia p.`, `proceriformis`, `polymnia` alone (the usual stock: sure) | Mechanitis polymnia proceriformis |
| `pol. e.`, `eurydice` | Mechanitis polymnia eurydice |
| `wer x pro`, `werpro` | Mechanitis polymnia werneri x proceriformis |
| `pro x wer`, `proxwer` | Mechanitis polymnia proceriformis x werneri |
| `lysimnia`, `lys` | Mechanitis lysimnia |
| `zaneka` | Melinaea menophilus zaneka |
| `mothone` | Melinaea mothone |
| `hibrido`, `hibrido x hibrido`, `zaneka x hibrido`, `hibrido x zaneka` | Melinaea menophilus zaneka x menophilus |
| `salapia` | Ithomia salapia salapia |
| `confusa`, `Methona` | Methona confusa psamathe |

- **Counts corrected on the page** (a number crossed out and a new one beside
  or above it, or a total after `=` that is not the sum): the first value as
  written, then each new total after `=`, in the order written:
  - `1̶2̶ 9̶ 4̶ 3̶ 2` → `12=9=4=3=2`; `16+2̶ 1` (the 2 crossed out, 1 written) →
    `16+2=17`; `24-1 = 2̶3̶ = 18` → `24-1=23=18`.
  - A lone crossed-out term stays and is subtracted: `23+3+1̶` → `23+3+1-1`.
  - A second total written below another is the final one; small raised
    terms (`8⁺¹+4`) are part of the sum. A minus is part of the count
    (`27-5`: 27 larvae, 5 died).
  - Such a cell is **sure when its final total is clear**: a confidence only
    if that total is unclear.
- **Death causes**: `unk` = Unknown, `eaten` = Eaten, `spider` = Spider,
  `ants` = Ants, `disapp` = Disappearance, `deformed` = Deformed,
  `heat shock` = Heat stroke, `preserved` = Killed_Preserved, `only wings` =
  Unknown - Only wings, `N/A` on a dead butterfly = Unknown. More paper
  words: data-rules `reference/insectary-individuals.md`.
- **Notes** in English: translate faithfully ("3 pupas muertas" → "3 pupae
  dead"), keeping IDs, codes and names. A place written short is its list
  name (Cavernas, C.T.C → Cavernas Templo de Ceremonia). More: data-rules
  `reference/notes.md`.
- **Which line a value is on**: in the Emergidos notebooks, dead dates and
  notes are often written low, on the ruling under their line: they belong to
  the line above that ruling. One note per clutch or butterfly; a bracket or
  arrow shares it between the lines it spans.

## The notebooks (kind → sheet)

### `stocks` — Posturas → Insectary_stocks

One line per clutch: *Clutch number · Species · Date laid · Number eggs ·
Hatching date · Number of larvae · Pupa date · Number of pupa · Emerge date ·
Number of adults · Insectary or lab · Notes*, often over two facing pages;
lines highlighted in colour are finished clutches.

- `CLUTCH NUMBER`: `994`, `994(7)` = batch 7 of the same couple; `994(F1)` =
  generation F1, `994(3) F1` = batch 3 of an F1.
- `INSECTARY OR LABORATORY` as written: `ins`, `lab`, `ins/oda`,
  `ins/este`, `ins ESTEBAN` (what looks like `ins/lab` is `ins/oda`).
- `NOTES`: parents female first: `U8A♀ + C8B♂`.
- The dissections column, often a sum, is its own column.

More: data-rules `reference/clutches.md`.

### `emergence` — Emergidos → Insectary_data

One line per butterfly: *# · ID · Species · Sex · # Clutch · Stock origin ·
Emerge date · Dead date · Notes*. The first `#` is a running count such as
3096: ignore it. Highlighted lines are usually dead butterflies.

- `Insectary_ID`: the ID on the wing (digit + two letters like `5VB`, `6OO`,
  or letter, digit, letter like `N4D`).
- `Stock_of_origin` as written (`interm.`, `messen.`). Only the *M.
  messenoides* stocks have one: every other line, and a dash, is `"NA"`;
  always send it.
- Emerge date → `Intro2Insectary_date`; dead date → `Death_date`. The notes
  go to `Notes_Insectary_data` with their column words ("ethanol", "wc", a
  CAM…): the tool moves those to their columns.
- **A CRISPR control** ("CRISPR #159 control" in the clutch column):
  `CLUTCH NUMBER` `"NA"`, `Wild_Reared` `Reared`, the stock, note "Comes from CRISPR
  control #159".
- **An "ethanol" / "flash frozen" bracket** over a butterfly with a CAM: it
  was killed and preserved on its emerge date: that is its `Death_date`, even
  when blank.
- **Wild-caught lines** (no clutch, "—"; the note gives the collector's
  initials, time, weather and place, e.g. "PAS 12:15 N.C C.T.C"):
  `Wild_Reared` `Wild-caught`, species, sex, `Intro2Insectary_date` (the
  capture day). Collector, time, weather and place stay out of the notes:
  they go in the butterfly's Collection_data row, in the same proposal,
  unasked. Complete the rows the tool drafts (`wildWithoutCollection`) from
  the page: `Collector` (`PAS - …`), `Identifier`,
  `Collection_location`, `Collection_time`, `Cloud_cover`, `Rainfall`,
  `Purpose` (`NA` when the page does not say). Paper codes: skill
  **monitoring** (weather) and data-rules `reference/field-collections.md`.
  Ask in your summary what the page does not say (identifier, a doubtful
  time).

### `deaths` — Muertes → Insectary_data

The daily round: *Date · ID · Species · Sex · Cause · CAM · Notes*. The notes
go to `Notes_Insectary_data` with their column words, as in Emergidos.

### `labels` — Sobres y etiquetas → Insectary_data

The envelope or label of one sampled butterfly: a CAM, the species, the sex,
"Reared ID: 1TG", a date, often a tube held beside it (read its printed
code). One line per label; ignore the notebook behind it.

- The Reared ID is the `Insectary_ID`.
- A struck CAM with a new one beside it is the correction history: the last
  uncrossed value is the CAM (report the chain). `wing clip: 28/8/24` is a
  clip date, not a death.
- Before saying the tube matches, look at the row with `get_record`: if the
  label's tube is in another tube column, or its tissue or medium disagrees
  (the envelope says wing clip, that tube is `WHOLE_ORGANISM`), say so as a
  difference for the person to decide.

More: data-rules `reference/reading-paper.md`.

### `crispr` — CRISPR → CRISPR

One line per injected egg: *CRISPR · # Eggs · CRISPR date · Guide · Specie ·
Hatch date · Pupa date · Emerge date · Mutant yes/no · CAM ID · Notes*.

- `CRISPR_No.` is the experiment (e.g. 50), `Eggs_No.` the egg (1, 2, 3…).
- `Guide` as written: `2B`, `2A-2D`, `No guide`.
- The "Specie" column is `Stock_of_origin` (e.g. `Inter`).

## Crops

One command cuts the photo for reading (Pillow). Its output goes to this
chat's own `work/<today>-<topic>/` folder.

1. `python3 .claude/skills/digitalizar-cuaderno/crops.py PHOTO --out work/<today>-<topic>`
   writes `<photo>-overview.jpg`: the photo upright (EXIF) with rulers of
   fractions (0–1) on every side. Look at it once. If the page is still
   sideways, add `--rotate 90` (clockwise; 270 if that leaves it upside down)
   to every call.
2. Read off the overview, for each page of the spread: its left and right
   edge (`x=0.11-0.50`), the top of the header row (`head=`), the top of the
   first written line and the bottom of the last one at the page's left and
   right edges (`top=0.145,0.14 bottom=0.93,0.915`), and count the written
   lines. Then:
   `python3 …/crops.py PHOTO --out DIR --lines 30 --left "x=0.11-0.50 head=0.10 top=0.145,0.14 bottom=0.93,0.915" --right "x=0.50-0.88 head=0.085 top=0.14,0.135 bottom=0.915,0.88"`
   (one page: `--page "…"`; `--enhance strong` for faint pencil).
   - It prints JSON: each strip's `path` and `lines` (e.g. left 1–10, right
     1–10).
   - The borders snap to the printed ruling (`snapped`: how many lines they
     moved; more than ~0.5 means your numbers were off: check the first
     strip).
   - Each right-page strip starts with the left page's ID column (framed in
     red) cut on the same lines, so every right-hand value sits beside its ID.
3. View several strips per reply (several Read calls in one message), e.g.
   the left and right strip of the same lines together.
4. A cell too small or crossed out: `--zoom x0,y0,x1,y1` (fractions of the
   photo, repeatable) gives an enlarged crop.

Labels, envelopes and short pages (≤ ~12 lines) can be read from the overview
or one strip per page.

## Several pages at once: reader subagents

- Read a single page yourself (splitting one page across readers was slower
  and not more accurate).
- Several pages in one message: cut every page's strips, then start one
  `notebook-reader` subagent per page (`general-purpose` if that type is
  missing), **all in one message** (several Agent calls in the same reply,
  `run_in_background: false`), each with its notebook kind, columns (from
  the `match_notebook` description), the photo path (for `--zoom`) and strip
  paths. The reading is blind: none of your readings and none of the sheet's
  values. Each answer ends with a line on how this hand writes 1/7 and 3/8:
  keep it for the reviewers.
- Check each page's lines (stages in step) and call `match_notebook` per page
  as the answers arrive.
- Without subagents (Codex): read the pages yourself, one after another.

## Verification: a second reading only when needed

Skip it when the photo is clear and nothing is doubtful: no doubtful or
unreadable cells, no `differs`, no `problems`, plausible lines. Otherwise
re-read only what is likely wrong:

1. **The cells to re-read**:
   - doubtful and unreadable cells and `problems`;
   - `differs` cells: the sheet's value was often typed from this same page,
     so each is re-read on a zoomed crop;
   - cells that fail plausibility (adults ≤ pupae ≤ larvae ≤ eggs; laid ≤
     hatch ≤ pupa ≤ emergence; counts on an "all died" / "no hatch" line; a
     line without `ins`/`lab` among lines that have it);
   - crossed-out, overwritten, faint or crowded cells and long sums (4+
     terms);
   - on a spread whose alignment you are unsure of, the right-hand page's
     lines with their IDs.
2. Tell the person in one line that the proposal is in Cambios propuestos and
   that you are checking those cells.
3. Start the `notebook-reviewer` subagents (`general-purpose` if missing)
   **all in one message** (`run_in_background: false`): one per block of
   lines, each with only its strips, the photo path (for `--zoom`), the
   proposal id, the lines (by ID) and columns to read, and the line on how
   this hand writes 1/7 and 3/8. The reading is blind: none of your
   readings and no values from the proposal (a reviewer told "ins/lab" read
   "ins/lab" where the page says "ins/oda"). Without subagents (Codex):
   re-read those cells yourself on zoomed crops.
4. Where the photo settles a disagreement (look at a zoomed crop yourself),
   correct the same proposal with `update_proposal` (only the cells that
   change). Where it does not, the cell stays doubtful. In the summary say how
   many cells were re-read and what changed.
