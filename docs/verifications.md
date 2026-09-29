# Checks copied from the Google Sheet

Read from the workbook on 2026-09-28 (conditional formats and data validation); kept in
`server/verifications.mjs`. Google applies them when someone types in the sheet, but not to
writes through the API, so the app applies them itself.

## Repeated IDs (pale red, as the sheet's conditional formats)

| Sheet | Columns that must not repeat |
|---|---|
| Collection_data | Insectary_ID, CAM_ID, CAM_ID_insectary |
| Insectary_data | Insectary_ID, CAM_ID, CAM_ID_CollData |
| F1/F2_MutationRate | Insectary_ID, CAM_ID |
| CRISPR, Pheromones_data, Wing_tissue, Life_History, Barcoding_DNA | CAM_ID |
| Sperm_dissections | Father_CAMid, Mother_CAMid |
| Every sheet | Tube IDs (Tube_n_id), unique across the whole workbook |

- Blank cells, `NA`, and texts without a digit (`not given`, `NOT_COLLECTED`) may repeat.
- The grids colour a repeat and say, on hover, which other rows hold it (unsaved changes included).
- Saving a value that another row already holds is refused. The grid colours it as it is typed; the cell stays
  pending and red ("CAM078274 ya está usado en Insectary_data fila 13384 (N2D)") and the other changes are saved.

## Dropdown lists (red corner, as Google marks invalid data)

`LISTS` in `server/verifications.mjs` has the lists of Collection_data, Insectary_data,
Insectary_stocks, F1/F2_MutationRate, SamplingDay_data and CRISPR. A list is either fixed or read
from Lists, Location_data, Taxonomy_v18Jun25 or Insectary_stocks.

- **Strict lists:** in the sheet, typing another value is rejected. In the app, a new value outside
  the list is refused when saving. Undo and imports put back what was there. Examples:
  - Collection_data Sex: female, male, female ?, male ?, NOT_COLLECTED
  - SPECIES from Taxonomy
  - Collection_location from Location_data
- **Other lists** (Insectary_data SPECIES, Death_cause, Pedigree; Insectary_stocks) are only marked.
- Existing values outside a list get the red corner in the grids, but are not touched.

## Found in the sheet while copying

- Insectary_data: one rule compares column N (LIFESTAGE) while colouring column O
  (CAM_ID_CollData), probably shifted when LIFESTAGE was inserted.
- Several rules are left on single cells, from rows that were copied around.
- Existing data outside strict lists:
  - Collection_data: SPECIES 88 cells, tissue locations 70, Sex 59 (`female_?`, `male?`),
    Collection_location 34.
  - Insectary_data: Collection_location 111.

## The archived Shiny app

It painted CAM_ID light red when it was not in the Lists pool, and its dropdowns were not strict.
It did not check repeated values.

## Revisión de datos (the whole workbook at once)

`server/checks.mjs` scans the local copy for inconsistencies the sheet's own rules do not catch
across rows and sheets: repeats and strict-list values (as above), a CAM given to two butterflies
(Collection_data, Insectary_data and Wing_tissue CAM_ID), Collected_Sent2Insectary rows without a
filled Insectary_data row and wild insectary butterflies without a collection row, species / sex /
copied-CAM mismatches between the two rows of one butterfly, deaths or preservations before
collection or entry, dates after today, preserved rows without CAM_ID or Tube_1_id, field
marks on two species, and Wikiloc monitoring points stored on the map without a row because
their pairing was doubtful (`walk_doubt`, with the walk, the note and the rows it could be; paired
in Monitoreo → Dudas, see monitoring.md). A fix is offered only when obvious (spelling of a list
value, the other sheet's CAM, a year typed one off). The scan is cached until the copy or the
stored walks change.

It is shown in Tablas → Revisión de datos (obvious fixes can be sent as one proposal to confirm in
Asistente) and is the assistant's `check_data` tool (`GET /api/checks?sheet=&kind=&limit=&offset=`).
