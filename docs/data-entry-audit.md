# How data is entered: audit and proposal (27 Sep 2026)

Sources, all read-only:

- **Main workbook snapshot:** 27 Sep 2026, all 42 sheets.
- **Drive revisions:** its 9 available revisions, 23–27 Sep 2026, compared one against the next.
- **Documents:** the insectary and crosses protocols on Drive, 139 meeting summaries, and the code of the original Shiny app.
- **This app:** the current app code.

Row numbers are sheet rows. The working scripts and the raw per-topic findings are kept outside the repository, in `~/.cache/ithomiini-test/audit/` on Franz's PC.

## 1. What actually happens, event by event (reverse-engineered)

| Event | Written where | When it is typed | What could be filled automatically |
|---|---|---|---|
| **Eggs picked up** (a plant with ≥ 8 eggs) | Insectary_stocks A–F: clutch number, species, date laid, eggs, place | Days later, from the larva notebook. The number follows the pickup, so 125 clutches have an earlier date than a lower-numbered clutch | Species from the mother; parents appear only in the NOTES text (`25/4/24 MJS: F1F2 5AA + 3AD`) and in F1/F2_MutationRate |
| **Hatch, pupation** | Insectary_stocks G–J | Weeks later; the pupa date is filled for only 46 % of clutches ≥ 900 | Expected dates (medians): lay→hatch 3–6 d, hatch→pupa 15–17 d, pupa→adult 7.5–10 d, lay→first adult 27–31 d |
| **Emergence** (or a wild butterfly entering the insectary) | Insectary_data A–H on the next pre-made ID row: Wild_Reared, CLUTCH NUMBER, Stock_of_origin, SPECIES, Sex, location, Intro2Insectary_date. The same ID is written on the wing | In batches per clutch (44 % of rows in runs of 6–20). **Currently about 5 weeks behind**: the latest adult is from 21 Aug, while clutches 981–990 should have emerged | Genus/species, stock, generation and research purpose from the clutch (96–99 %); location always "Mariposario Ikiam". Only sex and subspecies/form must be typed |
| **Mating (crosses)** | Whiteboard and crosses notebook, then F1/F2_MutationRate (couple, start/end). Stocks_Matings has not been used since 2023 | Later, by hand | Species, sex and wild/reared of both parents from their IDs |
| **Wing clip** | Insectary_data tube 1 (`OTHER_SOMATIC_ANIMAL_TISSUE \| WING CLIP`) + CAM + medium; the body later goes in tube 2 under the same CAM. **No date column**: only 79 notes give it | Median 1 day after emergence, 13 days before death. That fits the F2-pheromone protocol (clip at marking), not "after mating" | CAM and tube; the clip date |
| **Daily round of dead butterflies** | Insectary_data I–J (Death_date, Death_cause), then everything after is set to NA / NOT_COLLECTED. Notes start `d/m/yy INI:` | Median 2 days after the death; a Monday peak (weekend deaths). "Disappearance" is a cage count: 90 % fall on 28 dates (e.g. 367 rows on 6 May 2025) | Preserved_Dead_Alive from the cause (93 %); "Unknown" is the most common cause (5,065) |
| **Preservation** | Insectary_data: CAM, tubes, tissue, per-tube medium, body location | Same day as the death (98.5 %) | Preservation_date = Death_date; WHOLE_ORGANISM → the other tubes are NA / NOT_COLLECTED (followed in 2,349 of 2,386 rows); rack, manifest and photo columns are formulas |
| **Field collection day** | Collection_data, one row per butterfly: location, date, collectors, time, species, sex, fate. Live ones also get an Insectary_ID and a row in Insectary_data (typed twice) | Mostly the same week. Sent2Insectary rows lag a median 6 d, with backlogs (e.g. 305 rows from 2022 typed in 2024) | Country, Side_Andes and HABITAT from the location (formulas); date, location, collectors, weather and purpose from the day (82–96 %); CAM +1 (96 %); tube +1 inside the day's run (≈ 80 %) |
| **Monitoring walk** | Collection_data (Mark_Released / Collected_Preserved) + SamplingDay_data | Same day or the next | Everything except species/sex (see docs/monitoring.md) |
| **Later curation** | Collection_data SPECIES, ID_status, identifiers; notes `25Sep26 PAS …` | Months or years later (500 notes > 180 days after the event) | — |

### What the revisions of the last four days show

Comparing the 9 saved versions of 23–27 Sep 2026:

- **Columns first, then rows.** A field trip (rows 8133–8180) was typed in two passes. First Release_Collect and FieldMark_ID were filled down for 35 rows. Then Insectary_ID, taxonomy, identifier, sex and location for 48 rows.
- **Placeholders are made ahead.** 83 empty Insectary IDs were generated in one go (rows 13325–13407).
- **IDs within a session are consecutive.** On 24 Sep, 19 preserved field butterflies got CAM079905… and FS90415311… (+1 each). On 27 Sep, insectary preservations continued the **same FS904153xx rack** (FS90415322), so one rack is shared between Collection and Insectary.
- **Fill-down errors are real and recent.** Clutch 994(6) (rows 13383–13386) got death dates 24, 25, 26, 27 Sep, one per row, and CAMs that repeat others. On 27 Sep, CAM078276 was dragged into 12 empty rows.
- **Several people enter data.** Monitoring rows were typed the same day by the new assistant (26 Sep). Tubes from older insectary rows were catalogued by KN ("Half thorax"). The shared lab account entered the field trip. Taxonomy was re-identified by PAS.

## 2. Problems found, most urgent first

1. **Wild CAM IDs are almost used up.** CAM079915 is the last; about 84 are left in the wild pool and about 220 in the insectary block.
2. **Duplicate CAM IDs this month.** CAM078273–078275 are each used by two butterflies (U7A/C9B/E9B and N1D–N3D). CAM078276 is in 14 rows. The cause is two people using parallel runs, plus fill-down.
3. **Insectary_data is about 5 weeks behind** (emergences still only in the notebook). Meeting 137 plans training on this.
4. **Field marks restarted at B40** (Aug 2026), so 24 marks belong to two butterflies.
5. **Formula bugs in the workbook:**
   - The Wing_tissue rack lookup searches the wrong column: all 438 rows show "Not in TOL704", yet 1,925 of their tubes are in racks.
   - Its manifest lookup points to the OLD manifest.
   - The Insectary photo formula lacks an exact match, so it shows #N/A.
   - `Data_entry_order` is broken (#REF!).
   - The tube-locations sheet shows #REF! from deleted sheets.
6. **The same data is typed twice** (Collection ↔ Insectary for wild butterflies): 94 species, 122 sex and 170 location mismatches, and 17 CAM mismatches.
7. **Identifiers are messy:**
   - 84 malformed tube barcodes (7 or 9 digits).
   - 28 tubes linked to two CAMs.
   - 72 CAMs used twice; 61 of them are an old Collection ↔ Wing_tissue clash.
   - 95 duplicate and 93 ".1" Insectary IDs.
   - 18 Insectary IDs with spaces ("T 1").
   - 475 empty placeholder rows inside the used range; 45 of them are already referenced by Collection_data.
8. **Values that should be merged or split:**
   - `male ?` / `female ?` (152 rows): store the doubt as a separate flag.
   - Seven different "empty" tokens.
   - `YES or NO` left from the formula in 1,222 Pedigree rows.
   - Clutch numbers `992(3)` vs `839 (2)`.
   - Sampling day 375004 (probably 21 Sep 2026).
9. **This app's Emergidos screen writes Pedigree and CAM_ID_CollData.** Those are formula cells in most rows, so saving a hybrid clutch would be refused. The Shiny app simply overwrote the formulas. **To fix before use.**
10. **The documented protocol and the data disagree:**
    - Wing clips "in ethanol from now on" (meeting 66), but the 36 clips of 2025–26 are flash frozen. (Settled: flash frozen, a later decision; see section 5.)
    - Research_purpose should be set at emergence, but it is set at death.

## 3. How predictable each field is

These are measured on history, using only earlier rows. They set what the app can fill in and what a person must still type.

**Filled automatically, still editable (≥ 95 %):**
- Country, Side_Andes and HABITAT (from the location).
- Location of head, thorax and abdomen (copied from each other).
- Tube 2/3 tissue (from tube 1).
- Split body (from the tissue).
- Collection CAM (+1, skipping used ones, 96 %).
- Insectary_ID (next placeholder, 99 %).
- Wild_Reared.
- Location, stock and research purpose (from the clutch).
- Preservation_date (= Death_date).
- The WHOLE_ORGANISM tube rules.
- Species, sex and date of wild butterflies entering the insectary (from their Collection_data row).

**Defaults from the session or day (82–96 %):**
- Collection date, location, purpose, fate, preservation medium, collectors, rainfall/cloud, identifier.
- The day's tube run.
- Intro date.
- The clutch being registered.
- Species of reared butterflies (from siblings, 88 %).

**Ranked choices:**
- Subspecies/form, filtered by species (72 %, 90 % with location).
- Species (ranked by what the day/place usually gets).
- Death cause.
- Tube: the next free tube in each of the 3 most recent runs of that kind (81 % since 2025, 88 % in Collection).

**Must be typed:**
- Sex (52–62 %).
- Species of wild butterflies.
- Collection time, weight, flight height.
- Death date (default: today).

**ID rules (next value), with their accuracy on history:**

| ID | Rule | Accuracy |
|---|---|---|
| Collection CAM | last + 1, skipping used | 96.3 % |
| Insectary CAM | last + 1, skipping used | 57 % (75 % counting a day as one block): it needs a reservation, not a guess |
| Tube, 2nd+ in the same butterfly | previous + 1 | 80–97 % |
| Tube, first of a butterfly | last + 1 in the same run | 69 %; top-3 runs 74 % (81 % since 2025). Rack columns explain the ±6/±8 jumps |
| Rack position | not predictable from the tube; known only after the Sanger scan | — |
| Field mark | last new mark + 1 (M99 → A1) | 92 % |
| Insectary_ID | first empty placeholder after the last filled one, skipping reserved clutch blocks and IDs already referenced | 97.7 % |

## 4. Proposal

### 4.1 Principles

- **One screen per event, as in the Shiny app.** Each screen has a **header** holding what is shared by the whole batch (date, place, collectors, clutch, rack), so values are never dragged down a column. The **rows** hold only what changes per butterfly.
- **Suggestions, never silent writes.** Suggested values show in a lighter colour with their reason on hover (e.g. "next after FS90415321, same rack"). Formula cells are never written.
- **The server hands out identifiers.** CAMs, tubes, Insectary IDs and marks are **reserved** for a few minutes when a batch is prepared. Two people then never get the same next ID, which fixes the September duplicates. The save refuses any ID already used anywhere: all tube columns of every sheet, CAM pools, marks.
- **Linked sheets in one save.** For example, a field butterfly sent to the insectary creates the Collection_data row and the Insectary_data row together, so the data is not typed twice.
- **Notes carry their prefix.** The app writes `d/m/yy INI:` itself.

### 4.2 Screens, in the order of the insectary day

1. **Hoy (today)** is the start page, listing what's pending:
   - clutches whose hatch, pupa or emergence is due (from the medians above)
   - live butterflies not seen for a long time
   - placeholders referenced but empty
   - pools running low
   - the day's pending changes
2. **Ronda de muertos (daily dead round), phone-first.**
   1. Type or scan the ID read on the wing. When it is hard to read, the app proposes the **closest live IDs**: similar characters (0/O, 1/7, 5/S, 8/B), same cage or clutch, alive only.
   2. It shows each one's species, subspecies, sex, clutch, age and wing photo.
   3. The person confirms "coincide" or marks a mismatch.
   4. Cause is a big-button choice (Unknown first); the date defaults to today.
   5. "Preservar" sends the butterfly to Preservación.
   6. A separate **Conteo de jaula (cage count)** marks everyone not found as Disappearance in one step.
3. **Emergencias (emergences).**
   1. Pick the clutch (the list shows those due, with expected dates).
   2. The app fills species, stock, generation, purpose and pedigree from the clutch and takes the next free Insectary IDs.
   3. The person types only sex and subspecies/form, ranked by the siblings.
   4. Wing-ID labels print from here.
4. **Posturas y seguimiento (eggs and clutches).**
   - New clutch: the next number (with "(n)" for another batch from the same couple, written one way), the mother and father picked by ID (species from the mother), date laid, number of eggs.
   - A follow-up list gives one-tap hatch/pupa updates with the expected dates.
5. **Cruces (crosses).** Pair start/end from the whiteboard, straight into F1/F2_MutationRate. It links to the clutches that follow.
6. **Preservación / Tubos (preservation and tubes).**
   - The **active racks** are inferred from the last tubes used, per kind of work and medium (see section 5).
   - Each tube suggestion comes from its rack, with +1 inside a butterfly.
   - Checks: barcode format (2 letters + 8 digits), uniqueness everywhere, and the CAM pool (with its remaining count).
   - Phone-camera barcode scanning.
   - The WHOLE_ORGANISM and "fill blanks only" rules are kept.
7. **Wing clip.** Choose the butterfly, then clip tube, medium (the current protocol default, see question 1) and **date**. The date gets a real column or a fixed note form, so clips can be counted.
8. **Colecta de campo (field collection day).**
   - The header holds the trip: date, place from Location_data, collectors, weather.
   - Rows: species, sex, fate. Live butterflies get the next Insectary_ID and CAM; preserved ones get a CAM and tubes from the trip's rack.
   - The Insectary_data row is created in the same save.
9. **Monitoreo** is done; see docs/monitoring.md.
10. **Revisión de datos (data review)** is a nightly list, with a link to fix each problem:
    - duplicates, malformed IDs
    - fill-down patterns (dates or IDs counting up down a batch)
    - rows open too long, pools running out, vocabulary to merge
    - workbook formula faults

### 4.3 How it would be built

- **A prediction module** for the frontend (plain TypeScript, tested): `suggest(field, context)` returns ranked candidates with a reason. It works from the sheets the app already mirrors. The back-test above runs as a test, so a rule that gets worse is caught.
- **A reservation table on the server** for IDs (who, what, until when). Saving converts the reservation into the written value, and unused reservations expire.
- **Batch saves already cover several sheets at once.** Each new screen is a header plus a grid on top of that.
- **A nightly checks job** on the server, feeding "Revisión de datos" and, if wanted, a weekly message to the team.

## 5. Decisions

Answered on 27 Sep 2026:

1. **Wing clips are flash frozen.** That was decided after meeting 66, and the Tubos screen defaults to it. Until the clip date gets its own column, Tubos adds it to `Notes_Insectary_data` as `27/9/26 FCH: Wing clip 27/9/26`, the form the old notes used, so clips can still be counted.
2. **Racks run in parallel.** Three or four racks are in use at once: flash frozen and ethanol use different racks, and crosses (F1/F2) use different racks from collection and monitoring. The app infers the open runs from the last tubes used, grouped by kind of work (Cruces, Insectario, Monitoreo, Colecta, legs) and medium. Tubos offers the next free tube of each run and picks the one matching the loaded butterflies and the medium. Nobody declares racks.
3. **Formula faults** are listed at the bottom of Tablas ("Avisos del libro de Google Sheets") as reminders. The app does not edit those formulas.
4. **Production waits.** Everything is tried on the test workbook first.

Still open:

- **CAM pools:** request the next CAM range soon (about 84 wild IDs left).
- A real column for the wing-clip date (needs Patricio's OK, per File_notes).

## 6. Saving and seeing changes from Google Sheets

Measured on the test workbook, 27 Sep 2026:

| What | Before | Now |
|---|---|---|
| A change typed in the app reaches Google Sheets | when "Guardar en la hoja" was pressed, then ~7.7 s (read, write, read back to verify) | **automatically** 2.5 s after the last edit, then the same ~7.7 s |
| A change typed directly in Google Sheets shows in the app | at the next full read: every 5 min, and a full read takes ~2 min, so up to ~7 min | a few seconds with the Apps Script trigger (tools/apps-script); open pages check every 10 s |

- **Automatic saving** replaces the save button (it can be turned off in the bar at the bottom). Each save is still one atomic batch that is verified by reading it back, and it stays undoable in Historial. A save that fails because of the network retries on its own. A cell that cannot be saved (a repeated CAM, a value outside a strict list, a date outside 1990–2099, a cell someone else changed) stays pending and red with the reason, in Spanish, and everything else is saved; the bar counts those cells and "Revisar" lists them. Automatic saving sends a refused cell again only after it is edited.
- **The Apps Script trigger** sends the sheet and rows of each edit to the app, which reads only those rows again. Inserting, deleting or sorting rows makes the app read that one sheet again. Formula results that change because of another sheet still wait for the 5-minute read.
