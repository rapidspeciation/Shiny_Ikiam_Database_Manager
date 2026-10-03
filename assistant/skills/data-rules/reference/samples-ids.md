# IDs, CAMs, tubes, tissues, preservation

## Insectary_ID (= Reared ID on envelopes)

| Rows / period | Format | Example |
|---|---|---|
| 2022–early 2023 | letter + number (Ñ used; the cycle restarted, so old duplicates) | `A7`, `M98`, `Ñ12` |
| 2023 | number + letter | `7A`…`99Z` |
| Jun 2023 – Jun 2026 | digit + 2 letters; the digit runs fastest, then the **last** letter | `0AA`…`9ZZ` (`9AZ`→`0BA`) |
| since 30 Jun 2026 | letter + digit + series letter; the digit runs fastest, then the first letter; 260 per series | `A0A`…`Z9A`, `A0B`…; series E from 29 Sep 2026 |

- The current series has **no Ñ**: `N9D` → `O0D`. The next ID is the next free
  **pre-made row** (the app's «Siguiente Insectary ID», Inicio). What follows
  `Z9` of the last series: the team decides when it comes; ask.
- Known leftovers, left as they are: the empty block `A0E`–`A8E` at rows
  13253–13261 repeats the used `A0E`–`A8E` (rows 13512–13520), so those empty
  rows are not used; the Panama STRI rows with Insectary_ID `NA` are not
  ours.
- Read an ID by position (digit vs letter, `O`/`0`, `S`/`5`, `B`/`8`, `G`/`6`,
  `I`/`1`, `Z`/`2`; transposed letters `9MN`/`9NM`); `match_notebook` checks
  look-alikes against the sheet. Letter O is common: `6OO`, `5OS` (and `50S`
  is another butterfly in the older format). A series letter that does not
  follow the current one (an E among D lines) may be a slip: ask.
- IDs are pre-written down the notebook margin and pre-made as rows: a line or
  row with only an ID is an **unused slot**, not missing data.
- The field team reserves a run of IDs for live wild butterflies after the
  day's emergences are numbered: gaps in the emergence run are expected.
- A wrong ID in the sheet is fixed by moving the data to the right pre-made row
  (Buscador → row number → «Corregir Insectary ID»), not by retyping the ID
  cell. An ID written on two butterflies: the second is `ID.1`, a third
  `ID.2` ([insectary-individuals.md](insectary-individuals.md), Duplicates).

## CAM_ID

- `CAM` + 6 digits, one per individual. Assigned at **preservation or wing
  clip**, never at emergence or to a butterfly taken alive to the insectary;
  from then on the CAM is its identifier (envelope, photos, Sanger). Each
  sample is named by its **tube**: a clipped butterfly's body goes in the next
  free `Tube_n_id` under the same CAM (`match_notebook` does this), so a
  different CAM read for a row that has one is a misreading or the wrong row.
- Pools (Lists validates them; PAS hands out ranges):
  - Collection_data, local team: the Ecuador wild block (CAM0795xx, then
    CAM079859–079999 in Sep 2026; about 65 left). Expeditions abroad use their
    own block (CAM0776xx–0794xx), not for local rows.
  - Insectary_data: `InsectaryWild&Reared_CAMid`, block CAM078000–078499 (up to
    about CAM0783xx in Sep 2026).
  - CRISPR: CAM078500–078549 (older 075801–075850). Other pools exist (wing
    dissections, genome annotation): check Lists.
- Next CAM = the last used **in that pool** + 1, skipping used ones. Before
  proposing one, check it is unused in **every** sheet and in pending
  proposals (`search_records`): when several people preserve on the same days,
  the same CAM can be taken twice. CAMs follow preservation order, not ID
  order.
- When a pool is nearly empty (< 50), say so and ask PAS or AA (they hand out
  the ranges) for the next one.
- Typical errors: an extra 0 (`CAM0770542` for `CAM077542`, often a whole
  batch), a dropped digit, a CAM typed in a tube column, a stale "next CAM"
  whiteboard. A duplicate: the **second** individual gets a new CAM; note the
  old one; the envelope is struck and rewritten; photos are renamed (tasks).
- Photo files are named by CAM (`CAM078038d.JPG`, `…v.JPG`): a CAM change
  breaks the photo link until the files are renamed.

## Tubes

- FluidX barcode: 2 letters + 8 digits (`FS50851817`); prefixes FS (most), FA,
  FD, FF (2022–23). Unique across the whole workbook. The prefix is the box
  series, **not** the sample: FS holds clips and whole bodies alike, FD and FA
  were bodies, split bodies and clips in other boxes (2026 local rows are
  almost all FS; FD in 2026 only on expedition split bodies). Tell a clip from
  a body by the tissue and the tube column, never by the prefix.
- Several racks run in parallel: flash frozen vs ethanol; crosses vs
  collection/monitoring; pheromone males. The next tube continues the **same
  rack's** most recent run. Samples taken together (a split body) get
  consecutive tubes; a body added later to a clipped butterfly takes the
  current run's next tube. No size rule: large butterflies (Methona, Tithorea,
  Thyridia, Lycorea) often go in bigger tubes, but not always; take the tube
  on the label.
- A 7-digit or 9-digit tube is a dropped or doubled digit (Sep 2026: `FS3886683`
  for `FS63886683`, `FS5848961` for `FS50848961`): suggest the form that
  continues a known run, flagged as a suggestion.
- Tubes out of order against CAM order: point it out; the order stays.
- An envelope's tube may be in another tube column (a whole body in Tube_2
  with a clip in Tube_1): compare with all four tube columns before saying it
  matches.
- Rack, manifest and location-at-Sanger columns are formulas (known only after
  the Sanger scan).

## Sanger IDs

Samples shipped to the UK get a `Specimen ID` (`SAN` + digits) and a `ToLID`
(`ilMecMess311`) in the Sanger STS, one per individual, like the CAM. In
Collection_data they are formulas looking the CAM up in the MEIER manifest
(`Not in STS` until shipped).

## Tissues (Lists ORGANISM_PART)

- `WHOLE_ORGANISM` only when the whole body, legs included, is in one tube.
- Wing clip: `**OTHER_SOMATIC_ANIMAL_TISSUE** | WING CLIP`;
  `NON-ANDROCONIA WING CLIP` only for pheromone samples.
- Split bodies: `HEAD | ABDOMEN`, `THORAX`, `THORAX | LEG`, `LEG`… per tube;
  sperm work: `**OTHER_REPRODUCTIVE_ANIMAL_TISSUE** | SPERMATOPHORE`,
  `SPERM_SEMINAL_FLUID`. Unused tube: ID `NA`, tissue `NOT_COLLECTED` (older
  rows with `NA` stay).

## Preservation medium

Values: `Flash frozen`, `Ethanol`, `DMSO`, `NOT_COLLECTED`.

- Alive at preservation, or dead less than about 1 h → `Flash frozen` (dry
  shipper or −80 °C). Dead longer → `Ethanol` (or DMSO).
- No shipper: −80 °C freezer; ethanol only when neither is available, **with a
  note saying why**. Field trips without a shipper: ethanol.
- Wing clips: `Flash frozen` (since Sep 2026; an Aug 2024 meeting had said
  ethanol). Flash frozen is the general preference: since 2025 almost
  everything is; an ethanol row without a reason note: ask.
- Pheromone males: `Flash frozen`; ethanol only with the reason in a note (KG
  knows the pheromone protocol).
- Weekends: the freezer is not reachable, so butterflies are kept alive until
  Monday (many preservations on Mondays).
- Not preserved: every medium `NOT_COLLECTED`.
- `Preserved_Dead_Alive` / `Preserved_dead_alive`: the condition **at
  preservation** (`Alive` when killed, `Dead` when found dead), not the current
  status.
- Location_body / Location_*: `Ikiam` for local samples; `NA` when not
  preserved.

## Correcting an ID, CAM, tube, sex or species

1. Find every row that holds it (Collection_data and Insectary_data twins,
   Pheromones_data, Sperm_dissections, Melinaea_eggs…; experiment sheets pull
   CAM and tube by formula from the two main sheets, so fix those first).
2. One proposal with all of them, each with the note "from X to Y" (why and on
   whose word).
3. List the tasks for people: strike and rewrite the envelope, rename or
   retake the photos, tell the Sanger contacts if the tube is already in a
   manifest.
