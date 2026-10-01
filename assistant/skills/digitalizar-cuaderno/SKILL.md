---
name: digitalizar-cuaderno
description: Digitize photos of the Ikiam insectary notebooks into the Ithomiini workbook. Use it whenever the person sends or attaches a photo (one or several) of a handwritten notebook page (Posturas/clutches, Emergidos/insectary butterflies, Muertes/daily deaths, CRISPR), of a butterfly envelope or label (CAM, Reared ID, tube), or asks to "digitalizar", "pasar", "transcribir", "revisar" or "comparar con la hoja" a page or photo. It transcribes every line, matches it with the sheet through the match_notebook tool and leaves one proposal per page in Cambios propuestos for the person to apply.
---

# Digitalizar cuaderno

A photo of a notebook page (or of envelopes/labels) becomes **one proposal per
page** in *Cambios propuestos*, the panel beside the chat. You read the
handwriting; the `match_notebook` tool of the `ithomiini` MCP server finds each
line's row, compares every cell with the sheet and drafts the proposal. Nothing
is written until the person applies it.

## Workflow

1. **Identify the notebook** from the headers (table below). If you cannot tell,
   say so in one line and use the closest kind; never stop to ask first.
2. **Crop the page with the skill's tool** (see "Crops" below): an overview
   with rulers, then strips of ~10 lines with the header repeated, straightened
   and enhanced. Read the page yourself from the strips (several per reply);
   several pages at once are read by reader subagents in parallel (see
   "Several pages at once").
   **Transcribe every line and every column**, top to bottom, including
   crossed-out lines (`crossedOut: true`) and the notes. A spread of two facing
   pages is one page: the right-hand page continues the same lines, so follow
   each line across the gutter (count the ruled lines from the header on both
   pages; coloured highlights and the notes help to keep them in step) and give
   its right-hand values too (e.g. Posturas: pupa date, pupae, emerge date,
   adults, `ins`/`lab`, notes). Skip only lines that hold nothing but a
   pre-written ID or number. Before calling the tool, check that the two halves
   are in step: the stages must make sense on each line (no hatch date or 0
   larvae → no pupae or adults; a note like "no hatch" or "all died" sits on
   such a line; `ins`/`lab` is written on almost every line). If they do not,
   the right-hand page is shifted by a line: re-align it.
3. **Propose at once: call `match_notebook` once per page** right after this
   first reading, with `kind`, the `lines` and, only if the year is written
   somewhere on the page, `year`. Several envelopes/labels photographed
   together are one call (`kind: "labels"`, one line per label). Do not look
   the rows up yourself first: the tool does it (and better: it also tries
   look-alike IDs and the order of the rows). Then tell the person in one line
   that the proposal is in *Cambios propuestos* and that you are checking it
   with a second reading (e.g. "La propuesta de Posturas 947–976 ya está en
   Cambios propuestos; la estoy verificando con una segunda lectura.").
4. **Verify and correct the same proposal** (see "Verification" below): a
   targeted second reading, its corrections applied to the same proposal
   (`update_proposal`, or `match_notebook` with `replaceProposalId`), so the
   table beside the chat improves while the person looks at it.
5. **Then tell the person in 3–6 short lines, in their language**: which
   notebook and rows (e.g. "Posturas, clutches 120–134"), how many cells to
   fill, the differences with the sheet (sheet → notebook), what the second
   reading changed, the highlighted doubtful cells to check (line, column,
   alternatives) and the lines not found in the sheet. End with: in *Cambios
   propuestos* they can set cells back to the sheet value («Valor de la hoja»)
   and press «Aplicar», or tell you "sí/está bien". Use a small table only when
   there are several differences.
6. **Apply only on explicit confirmation**: when the person's latest message
   approves it ("sí", "aplícalo", "está bien"), call `apply_proposal` with the
   `proposalId` (optionally only some `indexes`). Never say something was saved
   unless it returned `applied`. While doubtful cells are unchecked it writes
   nothing and returns them (`doubtful`: index, label, field, value,
   alternatives, reason): ask about each, then `update_proposal` with the value
   they give or `rows[].checked: [columns]` for the ones they confirm, and apply
   again. `confirmDoubtful: true` (write them as they are) only on their
   explicit word; `skipDoubtful: true` writes only the sure cells.
7. **Corrections** ("la línea 5 es macho", "el 3 es 8"): call `match_notebook`
   again with the whole page corrected (the corrected cell without confidence)
   and `replaceProposalId` = the page's pending proposal: the same table
   changes in place beside the chat. Cells the person corrected by hand in the
   table are kept; if your new reading of one differs it comes back in
   `conflicts`: say so. Say what changed.

Never guess to fill a gap, never invent IDs, CAMs, tubes or dates, and never
propose with `propose_changes` what `match_notebook` can match. The team's
conventions beyond this page (record templates, IDs and CAM pools, envelopes,
cage cards, crosses, notes) are in the skill **data-rules**
(`.claude/skills/data-rules/SKILL.md`); read its file for the case when the
page is not a plain notebook table.

## Doubtful handwriting

Propose everything readable; doubt is a highlight, never an omission.

- A cell you are not sure of: give your **best reading** in `values`, a
  `confidence` below 0.8, up to 3 `alternatives` and a few words in `reasons`
  ("1 or 7: this hand"). It goes into the proposal **highlighted** (amber, «?»)
  with its alternatives and reason; in your summary name those cells so the
  person checks them. Use it for characters you cannot tell apart, not for
  whole columns: a value you can read, on a line you could follow, is sure.
- A cell you cannot read at all: `null` (not a guess; it stays out).
- A clear value that looks wrong (a date out of stage order, adults > pupae) is
  not doubtful: send it as written and point it out; the team often keeps it.
- Look-alikes to consider: 0/O, 1/I/7, 5/S, 8/B, 2/Z, 6/G, 4/9, 3/8, `+`/1;
  ♀/♂ written small. Before reading digits, compare this hand's 1 and 7 (and
  3/8) on clear cells of the same page. Copy IDs as written (`600` for `6OO`,
  `5OS` vs `50S` are two butterflies): the tool finds the row among look-alikes
  (an ID not found comes back with `didYouMean`). It also marks as doubtful a
  clutch unlike the run of lines around it (judged by the laid dates), a CAM
  with 7 digits or far from its run, and a tube with 7 or 9 digits (the value
  becomes the reading that continues the run, yours an alternative).
- A crossed-out value in a date or text is not the value: the one written
  beside or above it is. Counts are different: see "Counts" below.

## Writing the values

Give values **as written**; the tool converts them.

- **Dates** day first, as written: `17/9`, `4-8`, `19-6-23`, `16/Oct/2024`. Do
  not add the year (the tool infers it from the sheet: laid in December, emerged
  in January is handled). Give `year` only when the page shows it. `~2/7` and
  `29/6?` are that date (doubtful); `2/9+3/9` is the first day; never copy them
  into the notes.
- **Ditto marks** (`"`, `ll`, `||`, `〃`, a wavy line down a column) and a brace
  `}` spanning lines: write the repeated value in **every** line it covers. A date
  written once for several lines applies to all of them. A ditto under a blank
  cell repeats the last value written above it; an arrow `↑`/`↗` under a note
  repeats the note.
- `—` or `-` alone means none: write `"NA"`. An empty cell: leave the column out.
- **Sex**: ♀ = `female`, ♂ = `male`, `NA` when written so.
- **Species**: the full name, from these abbreviations:
  `messen.`/`messenoid` = Mechanitis messenoides messenoides;
  `interm.`/`inter` = Mechanitis messenoides intermedia;
  `decept` = Mechanitis messenoides deceptus;
  `pol. p.`/`polymnia p.`/`proceriformis` = Mechanitis polymnia proceriformis;
  `pol. e.`/`eurydice` = Mechanitis polymnia eurydice;
  `polymnia` alone = Mechanitis polymnia proceriformis (the usual polymnia
  stock: a sure reading, not a doubtful one);
  `wer x pro`/`werpro` = Mechanitis polymnia werneri x proceriformis;
  `pro x wer`/`proxwer` = Mechanitis polymnia proceriformis x werneri;
  `lysimnia`/`lys` = Mechanitis lysimnia;
  `zaneka` = Melinaea menophilus zaneka; `mothone` = Melinaea mothone;
  `hibrido`, `hibrido x hibrido`, `zaneka x hibrido`, `hibrido x zaneka` =
  Melinaea menophilus zaneka x menophilus (the hybrid stock);
  `salapia` = Ithomia salapia salapia; `confusa`/`Methona` = Methona confusa psamathe.
  Always give the species as written: the tool keeps the SPECIES formula of
  Insectary_data (predicted from the clutch) and only types over it when what
  emerged differs from the prediction.
- **Counts** as written, sums included: `12+15`, `2+4=6+8=14`, `27-5` (27
  larvae, 5 died: a minus is part of the count). The tool proposes the formula
  `=12+15`, keeping the terms, as the team does.
- **Counts corrected on the page** (a number crossed out and a new one written
  beside or above it, or a total after `=` that is not the sum): send the first
  value as written, then **each new total after `=`**, in the order written.
  The tool keeps the first terms and turns every correction into a
  subtraction (or addition), as the team types them:
  `31+4 = 1` → `31+4=1` (→ `=31+4-34`); `1̶2̶ 9̶ 4̶ 3̶ 2` → `12=9=4=3=2`
  (→ `=12-3-5-1-1`); `16+2̶ 1` (the 2 crossed out, 1 written) → `16+2=17`
  (→ `=16+2-1`); `24-1 = 2̶3̶ = 18` → `24-1=23=18`. A lone crossed-out term
  stays and is subtracted: `23+3+1̶` → `23+3+1-1`. A second total written below
  another is the final one. Small raised terms (`8⁺¹+4`) are part of the sum.
  **Such a cell is sure when its final total is clear**: give the whole chain
  and put a `confidence` only if the final total itself is unclear, never for
  the middle terms. Never leave it out because it was corrected.
- **CAMs** (`CAM` + 6 digits) and **tubes** (2 letters + 8 digits, e.g.
  `FS50851817`, often with `wc` = wing clip): a short number under a full one
  continues it (`cam505` or `72` under `CAM076671`; `81` under `FS50851380`). You
  may write the short form as it is; the tool completes the run.
- **Death causes**: `unk` = Unknown, `eaten` = Eaten, `spider` = Spider,
  `ants`, `disapp` = Disappearance, `deformed` = Deformed, `heat shock` = Heat
  stroke, `preserved` = Killed_Preserved, `only wings` = Unknown - Only wings.
- **Notes**: the page's own notes, **in English** as the team types them
  (translate faithfully: "3 pupas muertas" → "3 pupae dead"; keep IDs, codes and
  names), in the notes column of the kind. In Emergidos and Muertes leave the
  column words in (ethanol, wc, pheromone, unk, CAMs, tubes): the tool moves
  them. Never `ins/…` codes, a restatement of a count, or your doubts (those go
  in your reply). A place written short is its list name
  (Cavernas → Cavernas Templo de Ceremonia). What must and must never be noted:
  data-rules `reference/notes.md`.
- **Right-hand-page notes** are written smaller and drift up half a line: give
  each note to the ID whose line its first word starts on; one note per clutch
  or butterfly; a bracket or arrow shares it between the lines it spans.

## The notebooks (kind → sheet, columns)

**`stocks` — Posturas → Insectary_stocks.** One line per clutch of eggs.
Headers like *Clutch number · Species · Date laid · Number eggs · Hatching date
· Number of larvae · Pupa date · Number of pupa · Emerge date · Number of adults
· Insectary or lab · Notes* (often spread over two facing pages; lines
highlighted in colour are finished clutches). Columns: `CLUTCH NUMBER` (as
written: `994`, or `994(7)` for another batch of the same couple), `SPECIES`,
`DATE LAID`, `NUMBER OF EGGS`, `INSECTARY OR LABORATORY` (`Insectary` for
`ins`, `Laboratory` for `lab`), `HATCHING DATE`, `NUMBER OF LARVAE`, `PUPA DATE`,
`NUMBER OF PUPA`, `EMERGENCE DATE`, `NUMBER OF ADULTS`, `NOTES`. A clutch not in
the sheet becomes a new row. A generation written after the species or the
clutch number ("lysimnia (F1)", `994(F1)`, "(F2)", "(BC)") goes in `Generation`
(F1, F2, Backcross; none written → the tool writes NA): `994(F1)` is clutch 994,
while `994(3) F1` is batch 3. The **dissections** column (larvae/pupae taken for
dissection, often a sum like `2+6`) goes in `NUMBER OF PUPAE/LARVAE FOR
DISECTIONS` as a sum. A dash in a date or count is `"NA"` (the stage never came).

`INSECTARY OR LABORATORY`: give it **exactly as written** (`ins`, `lab`,
`ins/oda`, `ins/este`, `ins ESTEBAN`, `in-Oda`; what looks like `ins/lab` is
`ins/oda`). The tool writes `Insectary` (or `Laboratory`) and, for
`ins/<person>`, adds the note "mariposas de Oda" / "mariposas de Esteban";
never put the code or the owner in `NOTES` yourself. A line with nothing in
that column takes the room the rest of the page says. When the sheet's sum
already holds the page's terms and more (added later), the tool keeps it
(`kept`). Parents in NOTES female first (`U8A♀ + C8B♂`); failed clutches and
other conventions: data-rules `reference/clutches.md`.

**`emergence` — Emergidos → Insectary_data.** One line per butterfly. Headers
like *# · ID · Species · Sex · # Clutch · Stock origin · Emerge date · Dead date ·
Notes* (the first `#` is a running count such as 3096: ignore it; highlighted
lines are usually dead butterflies). Columns: `Insectary_ID` (the ID written on
the wing: a digit and two letters like `5VB`, `0NX`, `6OO`, or letter, digit,
letter like `N4D`), `SPECIES`, `Sex`, `CLUTCH NUMBER` (`838`, `831(1)`),
`Stock_of_origin` (as written: `interm.`, `messen.`; the tool completes it from
the list; only the *M. messenoides* stocks have one, every other line is
`"NA"`, and so is a dash: always send it, never leave the column out),
`Intro2Insectary_date` (emerge date), `Death_date` (dead date), `Death_cause`,
`CAM_ID`, `Tube_1_id`, `Notes_Insectary_data`, `Wild_Reared`. Only existing
rows are changed. A line with a clutch is `Reared` (the tool fills it).
A **CRISPR control** ("CRISPR #159 control" in the clutch column): `CLUTCH
NUMBER` `"NA"`, `Wild_Reared` `Reared`, the stock, note "Comes from CRISPR
control #159". **Notes as written** (here and in Muertes): the tool moves the
column words out of the note into empty cells only, keeping the rest of the
note: ethanol / flash frozen → the tube's medium, wc → wing-clip tissue,
pheromone → `Research_purpose` Pheromones, preserved → Killed_Preserved, unk →
Unknown, a CAM → `CAM_ID`, a tube → `Tube_1_id` (or `Tube_2_id`). A death then
gets the not-preserved block (`NA`/`NOT_COLLECTED`) or the preserved template;
these show as `implied`. A butterfly in an "ethanol"/"flash frozen" bracket
with a CAM was killed and preserved on its emerge date: give that as its
`Death_date` even when the cell is blank. Templates: data-rules
`reference/insectary-individuals.md`.

**Wild-caught butterflies** on this page (no clutch, "—"; the note gives the
collector's initials, time, weather and place, e.g. "PAS 12:15 N.C C.T.C"):
`Wild_Reared` `Wild-caught`, species, sex, `Intro2Insectary_date` (the capture
day); the collector, time, weather and place stay **out of** the notes: they
go in the butterfly's **Collection_data row, in the same proposal**, without
being asked. `wildWithoutCollection` = `{ ids, rows, todo }`: `rows` are those
rows drafted with the live-capture template (Insectary_ID, species, sex,
date): add them with `update_proposal` `newRows`, completed from the page —
`Collector` (the list value `PAS - …`), `Identifier`, `Collection_location`
(C.T.C = Cavernas Templo de Ceremonia), `Collection_time`, `Cloud_cover`,
`Rainfall`, `Purpose` (`NA` when the page does not say); keep the template
cells and leave death and preservation empty. Paper codes and place initials:
data-rules `reference/field-collections.md` and `reference/monitoring.md`.
Ask in your summary what the page does not say (identifier, a doubtful time).

**`deaths` — Muertes → Insectary_data.** The daily round of dead butterflies:
*Date · ID · Species · Sex · Cause · CAM · Notes*. Columns: `Insectary_ID`,
`SPECIES`, `Sex`, `Death_date`, `Death_cause`, `CAM_ID`, `Tube_1_id`,
`Notes_Insectary_data`.

**`labels` — Sobres y etiquetas → Insectary_data.** The envelope or label of one
sampled butterfly (not a table): a CAM, the species, the sex, "Reared ID: 1TG",
a date, and often a tube held beside it (read its printed code). One line per
label. Columns: `Insectary_ID` (the Reared ID, copied exactly), `SPECIES`,
`Sex`, `CAM_ID`, `Tube_1_id`. Ignore the notebook behind the label. A struck
CAM with a new one beside it is the correction history: the last uncrossed
value is the CAM (report the chain); `wing clip: 28/8/24` is a clip date, not a
death. Envelopes, cage cards and crosses-notebook lines: data-rules
`reference/reading-paper.md` and `reference/crosses.md`.
Before saying the tube matches, look at the butterfly's row with `get_record`:
the sheet has four tubes (`Tube_1_id` … `Tube_4_id`), each with its tissue and
medium. If the label's tube is in another tube column, or its tissue disagrees
(the envelope says "wing clip" but that tube is `WHOLE_ORGANISM`, or the medium
differs), say it plainly as a difference for the person to decide ("el tubo del
sobre está en Tube_2_id como WHOLE_ORGANISM, pero el sobre dice wing clip"),
and don't propose tube changes on your own. Only say "coincide" when it is the
same tube in the same column.

**`crispr` — CRISPR → CRISPR.** One line per injected egg: *CRISPR · # Eggs ·
CRISPR date · Guide · Specie · Hatch date · Pupa date · Emerge date · Mutant
yes/no · CAM ID · Notes*. Columns: `CRISPR_No.` (experiment, e.g. 50),
`Eggs_No.` (1, 2, 3…), `CRISPR_date`, `Guide` (as written: `2B`, `2A-2D`,
`No guide`), `Stock_of_origin` (the "Specie" column, e.g. `Inter`),
`Hatch_date`, `Pupa_date`, `Emerge_date`, `Mutant` (Yes, No or Check),
`CAM_ID`, `Notes`. A line not in the sheet becomes a new row.

## A call

```json
{
  "kind": "emergence",
  "title": "emergidos 0VD–5VE",
  "lines": [
    { "raw": "0VD messenoides ♀ CRISPR#159 control Messenoides 6/8",
      "values": { "Insectary_ID": "0VD", "SPECIES": "Mechanitis messenoides messenoides", "Sex": "female",
                  "CLUTCH NUMBER": "NA", "Wild_Reared": "Reared", "Stock_of_origin": "Messenoides",
                  "Intro2Insectary_date": "6/8", "Notes_Insectary_data": "Comes from CRISPR control #159" } },
    { "raw": "1VD intermedia ♂ 838 intermedia 6/8 8/8 unk",
      "values": { "Insectary_ID": "1VD", "SPECIES": "Mechanitis messenoides intermedia", "Sex": "male",
                  "CLUTCH NUMBER": "838", "Stock_of_origin": "intermedia", "Intro2Insectary_date": "6/8",
                  "Death_date": "8/8", "Death_cause": "Unknown" },
      "confidence": { "Sex": 0.6 }, "alternatives": { "Sex": ["female"] }, "reasons": { "Sex": "symbol smudged" } }
  ]
}
```

## Reading the answer

Per line: `status` — `match` (row found; `message` says when it was found
through a look-alike ID, e.g. read `600`, sheet `6OO`), `new` (new row),
`missing` (the ID is not in the sheet: probably misread, say so), `ambiguous`
(several rows could be it), `duplicate` (the same ID twice on the page), `nokey`
(no readable ID; `didYouMean` lists sheet IDs one character away), `crossed`.
Cells: `fill` / `newRow` (empty in the sheet / a new row), `differs` (`sheet`
vs `notebook`: the notebook is the primary record, but point it out),
`doubtful` (`read`, `alternatives`, `confidence`, `reason`: in the proposal,
highlighted; name them in your summary; `counts.doubtful` counts them),
`implied` (written by the tool though the line does not say it: the page's
room, a death's template, a note's words), `problems` (a tube used by another
butterfly, a value outside a strict list), `notWritten` (a formula column),
`unread`, `kept` (the sheet's value stays: its sum has the page's terms and
more; mention it only if it matters). `inProposal` tells whether the line is
in the proposal; `rowError` why a line was left out. `year`/`yearSource` say which year the dates got. `overlaps` lists
other pending proposals touching the same rows (the same page matched in
another chat): mention them, and if it is the same page pass their id as
`replaceProposalId` next time.

## Several photos

One `match_notebook` call (and proposal) per page, one call for all the labels
of a message, then one short summary page by page. A page matched earlier in
the conversation is re-matched with `replaceProposalId`, never proposed twice.

## Crops

One command cuts the photo for reading (Pillow; the output goes to this chat's
`work/<today>-<topic>/`, never a folder another chat may use):

1. `python3 .claude/skills/digitalizar-cuaderno/crops.py PHOTO --out work/<today>-<topic>`
   writes `<photo>-overview.jpg`: the photo turned upright (EXIF) with rulers
   of fractions (0–1) on every side. Look at it once. If the page is still
   sideways, add `--rotate 90` (clockwise; 270 if that leaves it upside down)
   to every call.
2. Read off the overview, for each page of the spread: its left and right edge
   (`x=0.11-0.50`), the top of the header row (`head=`), the top of the first
   written line and the bottom of the last one, at the page's left and right
   edges (`top=0.145,0.14 bottom=0.93,0.915`), and count the written lines.
   Then:
   `python3 …/crops.py PHOTO --out DIR --lines 30 --left "x=0.11-0.50 head=0.10 top=0.145,0.14 bottom=0.93,0.915" --right "x=0.50-0.88 head=0.085 top=0.14,0.135 bottom=0.915,0.88"`
   (one page: `--page "…"`; `--enhance strong` for faint pencil). It prints
   JSON: each strip's `path` and `lines` (e.g. left 1–10, right 1–10). The
   borders snap to the printed ruling (`snapped`: how many lines they moved;
   more than ~0.5 means your numbers were off: check the first strip). Each
   right-page strip starts with the left page's ID column (framed in red) cut
   on the same lines, so every right-hand value sits beside its clutch/ID.
3. View several strips per reply (several Read calls in one message), e.g. the
   left and right strip of the same lines together.
4. A cell too small or crossed out: `--zoom x0,y0,x1,y1` (fractions of the
   photo, repeatable) gives an enlarged crop.

Labels, envelopes and short pages (≤ ~12 lines) can be read from the overview
or one strip per page; the tool is for tables.

## Several pages at once: reader subagents in parallel

Read a single page yourself from its strips (splitting one page across readers
was slower and not more accurate). When several pages come in one message:

1. Cut the strips of every page (above).
2. Start one reader per page — `subagent_type: "notebook-reader"` (a faster
   model that knows this skill's value rules; `general-purpose` if that type is
   not available) — **all of them in one message** (several Agent calls in the
   same reply, `run_in_background: false`, so they run at the same time). Give
   each: the notebook kind and its columns, and its page's strip paths. It
   transcribes **blind** every line and column and returns the `lines` JSON of
   `match_notebook` (with `confidence` on doubtful cells). Do not give it your
   own readings or the sheet's values.
3. Check each page's lines (the stages make sense on each line) and call
   `match_notebook` once per page as the answers arrive. Then verify.

## Verification: a targeted second reading (always, after proposing)

The proposal is already beside the chat; verification makes it right. Re-read
what is likely to be wrong, not what is clearly fine:

1. **Choose the cells to re-read** from the `match_notebook` answer and your
   reading: doubtful and unread cells and `problems`; cells that fail
   plausibility (evaluate the sums: adults ≤ pupae ≤ larvae ≤ eggs; dates in
   order laid ≤ hatch ≤ pupa ≤ emergence; a hatch date or larvae before any
   pupa/adult; lines that say "all died"/"no hatch" with counts > 0; a line
   without `ins`/`lab` when its neighbours have it); crossed-out, overwritten
   or faint cells and long sums (4+ terms); and on a two-page spread the
   alignment of the right-hand page (re-read its lines with their IDs to check
   they are in step). Clear, plausible lines get no second reading.
2. **Start every reviewer in one message** (several Agent calls in the same
   reply, `run_in_background: false`, `subagent_type: "notebook-reviewer"`, or
   `general-purpose` if that type is not available): one per block of lines, each with only
   its strips (and the photo path, for `--zoom`) and the list of lines (by ID)
   and columns to read. **Never tell
   it your readings**, never quote values or abbreviations from the proposal in
   its prompt (a reviewer told "ins/lab" read "ins/lab" where the page says
   "ins/oda"). It first transcribes those cells itself, then reads the proposal
   (`get_proposal`) and returns a table `line | column | page | proposal |
   confidence` of every disagreement, plus impossible stages it sees.
3. **Correct the same proposal**: where the photo settles a disagreement (look
   at a zoomed crop yourself), fix it with `update_proposal` (seconds: the rows
   by `index`, only the cells that change). Use `match_notebook` with the whole
   page and `replaceProposalId` only when many lines change (a shifted block):
   writing the whole page again takes more than a minute. Where it does not,
   keep the cell doubtful (it stays highlighted) and name it in the summary.
4. In the summary, say that a second reading was done, how many cells it
   checked and what it changed.

## Show every line of the page

The person checks the proposal against the page line by line: include the
lines whose values are already in the sheet as context rows
(`match_notebook` with `includeUnchanged: true`), in notebook order. Never
invent small changes (NA, notes) to make a line appear.
