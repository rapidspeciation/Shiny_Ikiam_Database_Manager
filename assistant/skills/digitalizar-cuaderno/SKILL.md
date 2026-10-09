---
name: digitalizar-cuaderno
description: Digitize photos of the Ikiam insectary notebooks into the Ithomiini workbook. Use it whenever the person sends or attaches a photo (one or several) of a handwritten notebook page (Posturas/clutches, Emergidos/insectary butterflies, Muertes/daily deaths, CRISPR), of a butterfly envelope or label (CAM, Reared ID, tube), or asks to "digitalizar", "pasar", "transcribir", "revisar" or "comparar con la hoja" a page or photo. It transcribes every line, matches it with the sheet through the match_notebook tool and leaves one proposal per page in Cambios propuestos for the person to apply.
---

# Digitalizar cuaderno

The handwriting is read blind; `match_notebook` finds each line's row,
compares every cell with the sheet and drafts **one proposal per page**
beside the chat. Values go **as written**: the tool converts them, and its
description lists each notebook's columns.

How to read a page (the file format, doubtful and unreadable cells, how the
team writes values, each notebook's layout) is in
`.claude/agents/notebook-reader.md`, the reader subagents' instructions.

## Workflow

1. **Identify the notebook** from its headers (`stocks` Posturas, `emergence`
   Emergidos, `deaths` Muertes, `crispr` CRISPR, `labels` envelopes and
   labels). If unsure, say so in one line and use the closest kind; don't
   stop to ask.
2. **Cut the strips** (see "Crops").
3. **Read**:
   - one page: read it yourself, following
     `.claude/agents/notebook-reader.md` (steps 1–4 and "Reading a page"
     on), and give the lines to `match_notebook` directly;
   - several pages: one reader subagent per page (see "Several pages at
     once");
   - envelopes and labels: read them yourself the same way (see "Labels").
4. **Propose at once**: one `match_notebook` per page with
   `includeUnchanged: true` (the table then follows the whole page), `year`
   only if the page shows it (see "The year"), `photo` (the attachment's
   file name) and `rotate` (the turn you gave crops.py) so the page shows
   upright beside its table. All the envelopes/labels of a message are one
   call (`kind: "labels"`). Don't look the rows up first: the tool does it.
5. **Second reading** (see "Verification"): when the page has any of the
   cells listed there, it is done in the same turn, before the summary, so the
   person reviews the page once.
6. **Tell the person in 3–6 short lines**: which notebook and rows
   ("Posturas, clutches 120–134"), how many cells it fills, the differences
   with the sheet (sheet → notebook), what the second reading changed (if one
   was done), and the lines not found in the sheet. A small table only when
   there are several differences.
7. **Corrections** ("la línea 5 es macho"): a few cells → `update_proposal`
   on the same proposal; many lines (a shifted block) → `match_notebook`
   again with the whole page corrected and `replaceProposalId`. Say what
   changed. A page already proposed in this chat is re-matched the same way
   (one proposal per page). When the person moves a value to the line above
   or below, re-read the other deaths and notes of that page, and of the
   pages read with it, on zoomed crops; correct those that moved in the same
   proposals and list them.

Pages that are not a plain notebook table (cage cards, crosses notebook,
field envelopes): read the skill **data-rules** first.

## What `match_notebook` does and answers

It finds each row (look-alike IDs 0/O, 1/I, 5/S, row order), completes list
values, keeps the SPECIES formula unless what emerged differs or it gives
nothing for the clutch (then the page's species is written), writes notes as
`d/m/yy INI: text` after the existing note, turns owner codes, generations
(`(F1)`), dashes and note words (`ethanol`, `wc`, a CAM…) into their columns,
fills a death's template, and flags as doubtful the clutches, CAMs and tubes
that break the run around them. Implied values never replace a value the row
has. With `includeUnchanged`, lines already in the sheet show as grey context
rows, never written.

**The year**: `year` only when the page shows it (a header, a sticky note, a
full date). Without it the tool takes the year the sheet has for the same
dates in those rows, or the current year when the page's dates are from the
last 120 days. Otherwise it proposes nothing and asks for the year: ask the
person, then call again with `year`.

The answer: `proposalId`, `year`/`yearSource`, counts, and only the lines
that need attention (not found, crossed out, differences, doubtful,
unreadable, problems, warnings), each with its status (match, new, missing,
ambiguous, duplicate, nokey, crossed), `rowError`, `warnings` and those
cells. Lines that only fill cells or are already as in the sheet are
counted (`linesOnlyFilled`, `linesAsInSheet`); the proposal (`get_proposal`)
has every line and cell.

- `missing` (an ID not in the sheet: probably misread; see `didYouMean`),
  `ambiguous` and `duplicate` lines go in your summary, with `rowError`,
  `differs` and `warnings` (e.g. a clutch's adults unlike the butterflies
  typed in Insectary_data).
- `overlaps` = the same rows in another pending proposal (`get_proposal`
  reads it, a teammate's too). If it is your own copy of the same page, pass
  its id as `replaceProposalId` next time, or discard the older copy
  (`update_proposal` `discard`).
- A correction that looks like a typing slip (a digit missing, two swapped,
  another prefix) was often repeated in the rows typed with it: look at the
  same column in the rows around the page. The tool adds those it finds
  (`sameErrorNearby`) after the page's rows, as doubtful cells; propose
  others you see the same way, and say in the summary how many rows off the
  photo have it and where. Those with `inProposal` are in the proposal;
  `alreadyIn` names another pending proposal that writes them.
- `wildWithoutCollection`: wild-caught butterflies (Emergidos) without their
  Collection_data row, drafted for you to complete from the line's note (its
  `raw`): `Collector` (`PAS - …`), `Identifier`, `Collection_location`,
  `Collection_time`, `Cloud_cover`, `Rainfall`, `Purpose` (`NA` when the page
  does not say), in the same proposal, unasked. Paper codes: skill
  **monitoring** (weather) and data-rules `reference/field-collections.md`.
  Ask in your summary what the page does not say (identifier, a doubtful
  time).

## Labels — Sobres y etiquetas → Insectary_data

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
  `notebook-reader` subagent per page (`general-purpose` told to follow
  `.claude/agents/notebook-reader.md` if that type is missing), **all in one
  message** (`run_in_background: false`). Each prompt holds only the kind,
  the photo path and `rotate`, the strip paths with their lines, and the
  output file `work/<today>-<topic>/<page>.json`. The reading is blind: none
  of your readings and none of the sheet's values.
- Each reader saves its page to that file and answers with the path, its
  counts, anything odd and how this hand writes 1/7 and 3/8 (keep that line
  for the reviewers).
- Then `match_notebook` with `linesFile` (the path) and `includeUnchanged:
  true` for every page, several calls in one message; the file already holds
  the kind, title, photo and rotate.
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
