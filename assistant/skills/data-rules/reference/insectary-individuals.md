# Insectary butterflies (Insectary_data): one row per individual

## The row's life

1. **Pre-made row.** PAS creates ID rows in blocks ahead of time (ID formula in
   column A, formulas in SPECIES, Collection_location, Pedigree, T2 medium,
   photos, racks, manifests). The ID is written on the wing (and in the
   Emergidos notebook) **before** the row is typed. The row belongs to its ID:
   a wrong ID is fixed by moving the data, not by retyping the ID cell
   ([samples-ids.md](samples-ids.md)).
2. **Emergence / entry**: type only `Wild_Reared`, `CLUTCH NUMBER`,
   `Stock_of_origin`, `Sex`, `Intro2Insectary_date` (and SPECIES only when it
   differs from the formula or the formula gives nothing). Everything else
   stays **blank** while it lives.
3. **Wing clip** (cross, pheromone and F2 parents only), while alive.
4. **Death** (the daily round): Death_date, Death_cause and, in the same edit,
   the whole not-preserved or preserved block below, and Research_purpose.
5. **Later, by formula only**: racks, manifests, photos, STS columns,
   CAM_ID_CollData.

Blank death/preservation cells on a living butterfly are pending, not errors;
the typing lags (Emergidos about 5 weeks in Sep 2026, deaths about 2 days).
A dead or disappeared butterfly gets `NA` in every cell that does not apply;
blank means it has not died yet.

## Emergence values

- `Wild_Reared`: `Reared` (it has a clutch) or `Wild-caught`.
- `CLUTCH NUMBER`: exactly as the Insectary_stocks row writes it (`831 (3)`
  with a space in 2024–25, `994(6)` without in 2026).
- `SPECIES` stays the formula from the clutch unless what emerged differs
  (deceptus stock emerging as intermedia is common; so is proceriformis →
  eurydice) or the formula gives nothing (the clutch is not in
  Insectary_stocks yet, or has no species there); then the species from the
  notebook is typed over it. A proposed species equal to the clutch's is left
  to the formula, not written. `match_notebook` does this, and `lookAt`
  (`formulaEmpty`) names the rows still left without one.
- `Stock_of_origin`: only for the *Mechanitis messenoides* stock lines:
  `messenoides`, `intermedia`, `deceptus` (lowercase) = the **clutch's**
  subspecies, even when the phenotype differs. Every other species and every
  wild-caught butterfly: `NA`. A missing stock breaks the phenotype summaries,
  so a page that leaves the stock empty or dashed still gets one: the clutch's
  subspecies, else `NA` (`match_notebook` fills it).
- `Sex`: `female`, `male`; `NA` for an adult whose sex could not be seen
  (deformed, only wings); `NOT_COLLECTED` for preserved eggs and larvae.
- `Intro2Insectary_date`: the emergence date (reared) or the capture date
  (wild-caught; = its Collection_date). `NA` for preserved eggs/larvae.
- A pupa that died ("pupa muerta", "didn't emerge"): emergence `NA`, Sex `NA`.
- A phenotype word in the species column that is not a taxon (`phasianita`):
  species from the clutch, the word in the note.

## Special origins

- **Wild-caught**: CLUTCH NUMBER `NA`, Stock_of_origin `NA`, SPECIES typed as
  the trinomial, and a twin Collection_data row (`Collected_Sent2Insectary`,
  same Insectary_ID) in the same proposal. Collection_location is a formula
  from that twin: `ERROR!` there means the Collection row is missing.
- **CRISPR controls that emerged**: `Reared`, CLUTCH NUMBER `NA` (not text
  such as "CRISPR #159 control"), Stock_of_origin = the stock written on the
  page, else `NA` for the person to confirm (about half of the rows carry the
  stock), SPECIES typed, note `Comes from CRISPR control #159`.
- **Eggs or larvae found outside / in the field**: their clutch has SPECIES
  `NA` in Insectary_stocks; the adults carry the clutch and a typed SPECIES.
- **The Panama STRI batch** (Heliconius, Insectary_ID `NA`, inserted mid-sheet
  in Jun 2026, its own Lists batch): not insectary work; leave it as it is.

## Wing clip (alive)

`CAM_ID` (insectary pool) · `Tube_1_id` · `Tube_1_tissue`
`**OTHER_SOMATIC_ANIMAL_TISSUE** | WING CLIP` · `T1_Preservation_medium`
`Flash frozen` · note `d/m/yy INI: Wing clip d/m/yy` (no date column yet).
`NON-ANDROCONIA WING CLIP` is for pheromone samples only. Such rows can hold a
CAM and a clip before the emergence data are typed: they are in use, not free.
At death the body goes to the next free tube (Tube_2) `WHOLE_ORGANISM` under
the same CAM; `match_notebook` does this.

## Death, not preserved (≈ 88 % of deaths)

Cause not `Killed_Preserved` and no CAM:

| Column | Value |
|---|---|
| Preservation_date, CAM_ID, Tube_1–4_id | `NA` |
| Tube_1–4_tissue | `NOT_COLLECTED` (many 2026 rows have `NA`: they stay) |
| T1_Preservation_medium, Preservation_medium | `NOT_COLLECTED` |
| Preserved_Dead_Alive, Location_body, Research_purpose | `NA` |

The app's Muertes tab and `match_notebook` write this block.

## Death, preserved

Preservation_date = Death_date · CAM_ID (insectary pool) · Tube_1_id
`WHOLE_ORGANISM` `Flash frozen` (Tube_2 if Tube_1 is a wing clip) · the other
tubes `NA` and their tissues `NOT_COLLECTED` · Preservation_medium
`NOT_COLLECTED` (each tube's medium is in T1_/T2_Preservation_medium) ·
Preserved_Dead_Alive `Alive` when killed (`Killed_Preserved`), `Dead` for any
other cause · Location_body `Ikiam` · Research_purpose from the project
(`F1/F2 mutation rate` for cross parents and offspring, `Pheromones` for
pheromone males, `Sperm dissections`), else `NA`; when the page does not say
the project, `F1/F2 mutation rate` (the most common) for the person to confirm.
Medium rules and ethanol exceptions: [samples-ids.md](samples-ids.md).

## Columns left to the sheet

- `Preservation_medium`: `NOT_COLLECTED` on new preserved and dead rows unless
  something else is stated; older rows keep their media (`Flash frozen`,
  `Ethanol`).
- The columns after Notes_Insectary_data (racks, manifests, the collection
  and identification block) belong to another workflow: proposals do not show
  or write them.
- Formula columns, not written: Collection_location (`Reared` → Mariposario
  Ikiam), Pedigree, T2_Preservation_medium (from Tube_2_tissue: `NA` → `NA`,
  `NOT_COLLECTED` → `NOT_COLLECTED`), Photo_dorsal and Photo_ventral (from
  CAM_ID; `NA` when the CAM is `NA`).
- CAM_ID_CollData: `NA` for a reared butterfly (it has no Collection_data
  row).

## LIFESTAGE

The butterfly's stage when its row is filled:

- `Adult`: a row with a date in Intro2Insectary_date, the day it emerged in
  the insectary or the day a wild-caught butterfly was brought in.
- A preserved egg, larva or pupa: its stage (`Egg`, `3rd instar larva`,
  `Pre-pupa`, `Pupa day 3`…), with Intro2Insectary_date `NA` (see "Eggs,
  larvae and pupae preserved").

Older rows with LIFESTAGE empty stay as they are.

## Death causes

`Unknown`, `Eaten`, `Spider`, `Ants`, `Disappearance`, `Killed_Preserved`,
`Deformed`, `Heat stroke`, `Unknown - Only wings`, `Other` (a special case
described in the note; in 2026 also a larva or egg found dead). Paper words:
unk → Unknown; eaten / body eaten → Eaten; spider, "founded by spider" (= found)
→ Spider; ants; mantis → Other + note; deformed; heat or thermal shock → Heat
stroke; disapp / desaparecido / escaped → Disappearance; preserved / killed →
Killed_Preserved; only wings → Unknown - Only wings; N/A on a dead butterfly →
Unknown. A death date with no cause written anywhere also takes `Unknown`,
for the person to confirm. Use the cause itself; the old habit of `Other` +
note "Eaten" ended in 2024.

- **Disappearance** is a cage-count outcome: butterflies no longer seen are
  closed in bulk with the sweep day as Death_date (not a real death date) and
  the note "Disappeared in census" (the Censo tab writes both). For
  a single named butterfly, ask who handled it last (it may have been taken for
  an experiment).
- Weekend deaths are dated the day they were found (often Monday).
- Partial remains: wings and legs are still preserved; cause from the note.

## Eggs, larvae and pupae preserved (since Sep 2026)

F1 eggs, larvae and prepupae get an Insectary_ID each, from the pre-made
sequence. F1 larvae are preserved at the 3rd instar (since Oct 2026; the
4th until Sep 2026); eggs and larvae that look about to die are preserved
early. The notebook (Emergidos) writes them as a run of tube IDs with
"Preserved alive · Flash frozen · Larvae · 3rd instar", or per line
"lysimnia egg → preserved → FS50849027 → CAM078279".

| Column | Value |
|---|---|
| Wild_Reared, CLUTCH NUMBER | `Reared`, the clutch (`994(3)`) |
| Intro2Insectary_date | `NA` |
| Sex | `NOT_COLLECTED` (Sanger's category for a sex not recorded) |
| LIFESTAGE | the stage when preserved: `Egg`, `1st instar larva` … `5th instar larva`, `Pre-pupa`, `Pupa day 1` … `Pupa day 12` |
| Death_date | the preservation date |
| Death_cause, Preserved_Dead_Alive | `Killed_Preserved` and `Alive`, or `Other` and `Dead` (found dead) |
| CAM_ID, Tube_1 | a CAM; one tube `WHOLE_ORGANISM` `Flash frozen` |
| Preservation_medium | `NOT_COLLECTED` |
| Research_purpose | `F1/F2 mutation rate` |
| Note | `d/m/yy INI: Preserved alive 3rd instar` |

The clutch's Insectary_stocks row gets a dated note per group, with the IDs:
`d/m/yy INI: 2 larvae preserved as 3rd instar (R0C, R1C)`. Its NUMBER OF
LARVAE keeps them: since Oct 2026 the count holds the larvae used, and only
those that died or disappeared are subtracted ([clutches.md](clutches.md)).

## Research_purpose and Pedigree

- Research_purpose: filled at death. Living butterflies have it blank,
  except hybrids, which the Emergidos tab sets to `F1/F2 mutation rate`. To set
  it on another living butterfly, ask AA.
- Pedigree is a formula giving `YES or NO` for cross purposes; PAS and the
  crosses team type `Yes`/`No` over it. A leftover `YES or NO` on a dead cross
  parent is worth mentioning only when asked about pedigrees.

## Duplicates

The same ID written on two butterflies: the first one keeps the plain ID
(`W0B`), the second becomes `W0B.1`, a third `W0B.2`, with the note "ID
duplicated". Rows follow the order of data entry (dates continuous): the
suffixed rows are new rows placed after the series where the ID was reused, in
the notebook's order. Point a new duplicate out to the person.
