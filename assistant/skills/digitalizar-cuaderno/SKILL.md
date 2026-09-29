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
2. **Transcribe every line and every column**, top to bottom, including
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
3. **Call `match_notebook` once per page** with `kind`, the `lines` and, only if
   the year is written somewhere on the page, `year`. Several envelopes/labels
   photographed together are one call (`kind: "labels"`, one line per label).
   Do not look the rows up yourself first: the tool does it (and better: it also
   tries look-alike IDs and the order of the rows).
4. **Tell the person in 3–6 short lines, in their language (Spanish or English)**: which notebook and rows
   (e.g. "Posturas, clutches 120–134"), how many cells to fill, the differences
   with the sheet (sheet → notebook), the doubtful readings with their
   alternatives, and the lines not found in the sheet. End with: the proposal is
   in *Cambios propuestos*, beside the chat; they can untick rows and press ✓, or
   tell you "sí/está bien". Use a small table only when there are several
   differences.
5. **Apply only on explicit confirmation**: when the person's latest message
   approves it ("sí", "aplícalo", "está bien"), call `apply_proposal` with the
   `proposalId` (optionally only some `indexes`). Never say something was saved
   unless it returned `applied`.
6. **Corrections** ("la línea 5 es macho", "el 3 es 8"): call `match_notebook`
   again with the whole page corrected (the corrected cell without confidence)
   and `replaceProposalId` = the page's pending proposal: the same table
   changes in place beside the chat. Cells the person corrected by hand in the
   table are kept; if your new reading of one differs it comes back in
   `conflicts`: say so. Say what changed.

Never guess to fill a gap, never invent IDs, CAMs, tubes or dates, and never
propose with `propose_changes` what `match_notebook` can match.

## Doubtful handwriting

Propose what is clear and mark the rest; do not stop to ask before proposing.

- A cell you are not sure of: give your best reading in `values`, a `confidence`
  below 0.8 and up to 3 `alternatives`. Use it for characters you cannot tell
  apart, not for whole columns: a value you can read, on a line you could
  follow, is sure. The tool leaves doubtful cells out of
  the proposal and lists them; you ask about them in your reply.
- A cell you cannot read at all: `null` (not a guess).
- Look-alikes to consider: 0/O, 1/I/7, 5/S, 8/B, 2/Z, 6/G, 4/9, 3/8; ♀/♂ written
  small. Butterfly IDs use the **letter O** in series like `6OO`, `1OP`, `5OR`,
  and `5OS` (letter) and `50S` (zero) are two different butterflies: copy what is
  written; the tool finds the right row among the look-alikes.
- A crossed-out value is not the value: the one written beside or above it is
  (`68 12̶ 11` → 11).

## Writing the values

Give values **as written**; the tool converts them.

- **Dates** day first, as written: `17/9`, `4-8`, `19-6-23`, `16/Oct/2024`. Do
  not add the year (the tool infers it from the sheet: laid in December, emerged
  in January is handled). Give `year` only when the page shows it.
- **Ditto marks** (`"`, `ll`, `||`, `〃`, a wavy line down a column) and a brace
  `}` spanning lines: write the repeated value in **every** line it covers. A date
  written once for several lines applies to all of them.
- `—` or `-` alone means none: write `"NA"`. An empty cell: leave the column out.
- **Sex**: ♀ = `female`, ♂ = `male`, `NA` when written so.
- **Species**: the full name, from these abbreviations:
  `messen.`/`messenoid` = Mechanitis messenoides messenoides;
  `interm.`/`inter` = Mechanitis messenoides intermedia;
  `decept` = Mechanitis messenoides deceptus;
  `pol. p.`/`polymnia p.`/`proceriformis` = Mechanitis polymnia proceriformis;
  `pol. e.`/`eurydice` = Mechanitis polymnia eurydice;
  `polymnia` alone = Mechanitis polymnia proceriformis (the usual polymnia
  stock; alternative Mechanitis polymnia eurydice);
  `wer x pro` = Mechanitis polymnia werneri x proceriformis;
  `pro x wer` = Mechanitis polymnia proceriformis x werneri;
  `lysimnia`/`lys` = Mechanitis lysimnia;
  `zaneka` = Melinaea menophilus zaneka; `mothone` = Melinaea mothone;
  `hibrido`, `hibrido x hibrido`, `zaneka x hibrido`, `hibrido x zaneka` =
  Melinaea menophilus zaneka x menophilus (the hybrid stock).
  Always give the species as written: the tool keeps the SPECIES formula of
  Insectary_data (predicted from the clutch) and only types over it when what
  emerged differs from the prediction.
- **Counts** as written, sums included: `12+15`, `2+4=6+8=14`, `27-5`. The team
  keeps Insectary_stocks counts as sums (one term per day or group); the tool
  proposes them as the formula `=12+15`, keeping the terms. A minus is part of
  the count: `27-5` means 27 larvae of which 5 died (23 alive), so send `27-5`.
  A number **crossed out** with a line and a new number written beside it is a
  correction, not a subtraction: send only the new number (and its sum, if it
  continues one), never the crossed-out one.
- **CAMs** (`CAM` + 6 digits) and **tubes** (2 letters + 8 digits, e.g.
  `FS50851817`, often with `wc` = wing clip): a short number under a full one
  continues it (`cam505` or `72` under `CAM076671`; `81` under `FS50851380`). You
  may write the short form as it is; the tool completes the run. Match notes to
  lines by position or by the bracket that groups them.
- **Death causes**: `unk` = Unknown, `eaten` = Eaten, `spider` = Spider,
  `ants`, `disapp` = Disappearance, `deformed` = Deformed, `heat shock` = Heat
  stroke, `preserved` = Killed_Preserved, `only wings` = Unknown - Only wings.
- Anything else in the notes column (e.g. "pupa muerta", "abit deformed",
  "emerged in cage of parents", "ethanol", "flash frozen") goes in the notes
  column of the kind, as written.

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
the sheet becomes a new row.

**`emergence` — Emergidos → Insectary_data.** One line per butterfly. Headers
like *# · ID · Species · Sex · # Clutch · Stock origin · Emerge date · Dead date ·
Notes* (the first `#` is a running count such as 3096: ignore it; highlighted
lines are usually dead butterflies). Columns: `Insectary_ID` (the ID written on
the wing: a digit and two letters like `5VB`, `0NX`, `6OO`, or letters and
digits like `H79`), `SPECIES`, `Sex`, `CLUTCH NUMBER` (`838`, `831(1)`, or the
text as written such as `CRISPR #159 control`), `Stock_of_origin` (as written:
`interm.`, `messen.`; the tool completes it from the list; a dash `—` or `-` is
`"NA"`, which is what the sheet holds for "no stock": always send it, never leave
the column out), `Intro2Insectary_date`
(emerge date), `Death_date` (dead date), `Death_cause`, `CAM_ID`, `Tube_1_id`
(the wing clip tube), `Notes_Insectary_data`. Only existing rows are changed.

**`deaths` — Muertes → Insectary_data.** The daily round of dead butterflies:
*Date · ID · Species · Sex · Cause · CAM · Notes*. Columns: `Insectary_ID`,
`SPECIES`, `Sex`, `Death_date`, `Death_cause`, `CAM_ID`, `Tube_1_id`,
`Notes_Insectary_data`.

**`labels` — Sobres y etiquetas → Insectary_data.** The envelope or label of one
sampled butterfly (not a table): a CAM, the species, the sex, "Reared ID: 1TG",
a date, and often a tube held beside it (read its printed code). One line per
label. Columns: `Insectary_ID` (the Reared ID, copied exactly), `SPECIES`,
`Sex`, `CAM_ID`, `Tube_1_id`. Ignore the notebook behind the label.
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
                  "CLUTCH NUMBER": "CRISPR #159 control", "Stock_of_origin": "Messenoides", "Intro2Insectary_date": "6/8" } },
    { "raw": "1VD intermedia ♂ 838 intermedia 6/8 8/8 unk",
      "values": { "Insectary_ID": "1VD", "SPECIES": "Mechanitis messenoides intermedia", "Sex": "male",
                  "CLUTCH NUMBER": "838", "Stock_of_origin": "intermedia", "Intro2Insectary_date": "6/8",
                  "Death_date": "8/8", "Death_cause": "Unknown" },
      "confidence": { "Sex": 0.6 }, "alternatives": { "Sex": ["female"] } }
  ]
}
```

## Reading the answer

Per line: `status` — `match` (row found; `message` says when it was found
through a look-alike ID, e.g. read `600`, sheet `6OO`), `new` (new row),
`missing` (the ID is not in the sheet: probably misread, say so), `ambiguous`
(several rows could be it), `duplicate` (the same ID twice on the page), `nokey`
(no readable ID), `crossed`. Cells: `fill` (empty in the sheet), `differs`
(`sheet` vs `notebook`: the notebook is the primary record, but point it out),
`doubtful` (left out: ask), `problems` (e.g. a tube already used by another row,
a value outside a strict list), `notWritten` (a formula column), `unread`.
`inProposal` tells whether the line is in the proposal; `rowError` why a line was
left out. `year`/`yearSource` say which year the dates got. `overlaps` lists
other pending proposals touching the same rows (the same page matched in
another chat): mention them, and if it is the same page pass their id as
`replaceProposalId` next time.

## Several photos

- Several pages in one message: one `match_notebook` call (and one proposal)
  per page, then one short summary for all of them, page by page.
- Envelopes/labels: all the labels of the message in one call.
- A page already matched earlier in the conversation: re-match it with
  `replaceProposalId` instead of making a second proposal.
