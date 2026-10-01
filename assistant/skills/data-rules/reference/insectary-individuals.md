# Insectary butterflies (Insectary_data): one row per individual

## The row's life

1. **Pre-made row.** PAS creates ID rows in blocks ahead of time (ID formula in
   column A, formulas in SPECIES, Collection_location, Pedigree, T2 medium,
   photos, racks, manifests). The ID is written on the wing (and in the
   Emergidos notebook) **before** the row is typed. The row belongs to its ID:
   never retype an ID cell (see [samples-ids.md](samples-ids.md)).
2. **Emergence / entry**: type only `Wild_Reared`, `CLUTCH NUMBER`,
   `Stock_of_origin`, `Sex`, `Intro2Insectary_date` (and SPECIES only when it
   differs from the formula). Everything else stays **blank** while it lives.
3. **Wing clip** (cross, pheromone and F2 parents only), while alive.
4. **Death** (the daily round): Death_date, Death_cause and, in the same edit,
   the whole not-preserved or preserved block below, and Research_purpose.
5. **Later, by formula only**: racks, manifests, photos, STS columns,
   CAM_ID_CollData. Never type them.

Blank death/preservation cells on a living butterfly are pending, not errors;
the typing lags (Emergidos about 5 weeks in Sep 2026, deaths about 2 days).
A dead or disappeared butterfly gets `NA` in every cell that does not apply;
blank means it has not died yet.

## Emergence values

- `Wild_Reared`: `Reared` (it has a clutch) or `Wild-caught`.
- `CLUTCH NUMBER`: exactly as the Insectary_stocks row writes it (`831 (3)`
  with a space in 2024–25, `994(6)` without in 2026).
- `SPECIES` is a formula from the clutch. Keep it when the notebook's species
  is the clutch's; type over it only when what emerged differs (deceptus stock
  emerging as intermedia is common; so is proceriformis → eurydice). Never type
  the same value the formula gives. `match_notebook` does this.
- `Stock_of_origin`: only for the *Mechanitis messenoides* stock lines:
  `messenoides`, `intermedia`, `deceptus` (lowercase) = the **clutch's**
  subspecies, even when the phenotype differs. Every other species and every
  wild-caught butterfly: `NA`. A missing stock breaks the phenotype summaries.
- `Sex`: `female`, `male`; `NA` for an adult whose sex could not be seen
  (deformed, only wings) and for preserved eggs and larvae.
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
- **CRISPR controls that emerged**: `Reared`, CLUTCH NUMBER `NA` (never text
  such as "CRISPR #159 control"), Stock_of_origin = the stock, SPECIES typed,
  note `Comes from CRISPR control #159`.
- **Eggs or larvae found outside / in the field**: their clutch has SPECIES
  `NA` in Insectary_stocks; the adults carry the clutch and a typed SPECIES.
- **The Panama STRI batch** (Heliconius, Insectary_ID `NA`, inserted mid-sheet
  in Jun 2026, its own Lists batch): not insectary work; never match or edit it.

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
| Tube_1–4_tissue | `NOT_COLLECTED` (Franz's decision, 1 Oct 2026: the intended method; Jun–Sep 2026 rows typed `NA` because it is quicker: leave those) |
| T1_Preservation_medium, T2_Preservation_medium, Preservation_medium | `NOT_COLLECTED` |
| Preserved_Dead_Alive, Location_body, Research_purpose | `NA` |

The app's Muertes tab writes this block; the notebook tool checks it.

## Death, preserved

Preservation_date = Death_date · CAM_ID (insectary pool) · Tube_1_id
`WHOLE_ORGANISM` `Flash frozen` (Tube_2 if Tube_1 is a wing clip) · the other
tubes `NA`, their tissues and media `NOT_COLLECTED` · Preservation_medium
(the whole-body column) = the medium used · Preserved_Dead_Alive `Alive` when
killed (`Killed_Preserved`), `Dead` when found dead · Location_body `Ikiam` ·
Research_purpose from the project (`F1/F2 mutation rate` for cross parents and
offspring, `Pheromones` for pheromone males, `Sperm dissections`), else `NA`.
Medium rules and ethanol exceptions: [samples-ids.md](samples-ids.md).

## Death causes

`Unknown`, `Eaten`, `Spider`, `Ants`, `Disappearance`, `Killed_Preserved`,
`Deformed`, `Heat stroke`, `Unknown - Only wings`, `Other` (a special case
described in the note; in 2026 also a larva or egg found dead). Paper words:
unk → Unknown; eaten / body eaten → Eaten; spider, "founded by spider" (= found)
→ Spider; ants; mantis → Other + note; deformed; heat or thermal shock → Heat
stroke; disapp / desaparecido / escaped → Disappearance; preserved / killed →
Killed_Preserved; only wings → Unknown - Only wings. Use the cause itself; the
old habit of `Other` + note "Eaten" ended in 2024.

- **Disappearance** is a cage-count outcome: butterflies no longer seen are
  closed in bulk with the sweep day as Death_date (not a real death date). For
  a single named butterfly, ask who handled it last (it may have been taken for
  an experiment).
- Weekend deaths are dated the day they were found (often Monday).
- Partial remains: wings and legs are still preserved; cause from the note.

## Eggs and larvae preserved (since Sep 2026)

F1 eggs, larvae and prepupae get an Insectary_ID from the pre-made sequence
each: `Reared`, clutch (`994(3)`), Intro2Insectary_date `NA`, Sex `NA`
(decided; many Sep 2026 rows say `NOT_COLLECTED`), `LIFESTAGE` = `Egg`,
`1st instar larva` … `5th instar larva`, `Pre-pupa` (the column is used only for
this), Death_date = preservation date, cause `Killed_Preserved` (`Alive`) or
`Other` (found dead, `Dead`), CAM, one tube `WHOLE_ORGANISM` `Flash frozen`,
Research_purpose `F1/F2 mutation rate`, note `d/m/yy INI: Larvae 4th instar`.
The clutch's Insectary_stocks row gets the note "preserved d/m" and a
subtraction in its count. The protocol preserves F1s at the 4th instar; eggs
and younger larvae that look about to die are preserved early (hence the eggs
and 3rd instars of Sep 2026).

## Research_purpose and Pedigree

- Research_purpose: the protocol and the project lead set it at emergence; the
  2026 rows fill it only at death (all 364 living rows blank). Always fill it
  at death. On living rows it is not settled — ask AA first.
- Pedigree is a formula giving `YES or NO` for cross purposes; PAS and the
  crosses team type `Yes`/`No` over it. Don't touch it; point out a leftover
  `YES or NO` on a dead cross parent only when asked about pedigrees.

## Duplicates

IDs used twice were resolved with a suffix (`5FF.2`, `1IJ.1`) and the note
"ID duplicated". Never create such a row yourself; point the duplicate out.
