# Auditoría de datos, 28 oct 2026

## Resumen para el equipo

Revisé Collection_data, Insectary_data y Photo_links en una copia local (solo lectura). No se cambió nada en la hoja ni en la app.

- **Arreglos mecánicos listos (186 celdas):** espacios de más, 53 fechas escritas como texto («9-mrt-26»), 59 sexos dudosos escritos «female_?» en vez de «female ?», 47 nombres de especie con una letra mal o sin «(family)», 12 formas con una letra cambiada («euridice» → «eurydice»). Están preparados como Cambios propuestos; Franz decide si se envían.
- **Necesitan una persona:**
  - 3 CAM repetidos en Insectary_data, de este mes: CAM078497, 078498 y 078499 están en dos mariposas cada uno, una Methona y una Mechanitis lysimnia.
  - 95 Insectary_ID repetidos y 18 con espacio («T 7»), los mismos que en septiembre.
  - 23 fechas con día y mes probablemente cruzados.
  - 29 preservaciones con fecha anterior a la muerte.
  - 27 muertes sin causa o sin fecha.
  - 42 especies que no están en Taxonomy_v18Jun25.
  - 195 CAM con fotos en Drive pero sin fila en ninguna hoja.
  - 290 fotos llamadas «CAMO7…», con la letra O en vez de cero.
- **Methona confusa:** 22 registros de antes del 15 may 2023 son «male» por la regla de los pelos (hair pencils). Las hembras también los tienen, así que ese sexo no está comprobado. Hay un borrador para marcarlos «male ?», que debe aprobar una persona.
- La app ya muestra 8,160 avisos en Revisión de datos y siguen abiertos. Aquí van sobre todo problemas que esos avisos no cubren.

## English detail

### Source and method

- **Data.** `~/.cache/ithomiini-lab/app-workbook/app.sqlite`, `records` table, synced 28 Oct 2026 06:27. Observed rows only: Collection_data 8,102, Insectary_data 13,878, Photo_links 22,787.
- **Lab workbook, not the team's.** This is the lab app's copy of a copy of the workbook (`1bcgL4…`), not the team's workbook (`1QZj…`). Its recordIds do not exist on the live app, and rows may differ by a few days.
- **Allowed values.** Dropdowns come from `sheets.sqlite` `_columns` (19 Oct).
- **Read only.** Nothing was written to the server or to Google.
- **Script.** `audit.py` writes `findings.json`, with every row per rule, and `proposals/`.
- **The app's own checks.** `server/checks.mjs` `allIssues()` was run on a throwaway `VACUUM INTO` copy in `/tmp/audit28/`. Counts are in `app_checks_summary.json`. Rules marked "app" below are already listed in the Revisión tab and are still open there.

### Findings

| # | Rule | Count | Class | Already covered? |
|---|---|---:|---|---|
| 1 | Leading/trailing spaces in typed cells | 15 | safe | no |
| 1b | Spaces that come from the dropdown source itself (Lists: `ABV - Alejandra Barros Veliz `, `Abundance estimation `; Location_data: `Apuya Y `) | 65 | needs a person | no |
| 2 | Same CAM_ID on two rows of one sheet | 5 (3 new: CAM078497-499) | needs a person | app `repeat` |
| 3 | Duplicate Insectary_ID | 95 | needs a person | database-findings.md, still unfixed |
| 4 | Insectary_ID with a space (`T 7`) | 18 | needs a person | data-entry-audit §2.7, still unfixed |
| 5 | Text in a date column | 54 (53 fixable) | safe (53) / person (1) | app `bad_date`, open, no fix offered |
| 6 | Probable day/month swap | 23 | needs a person | no |
| 7 | Preservation_date before Death_date | 29 | needs a person | not this pair |
| 8 | Insectary adult alive > 180 days (not Disappearance) | 23 | needs a person | no |
| 9 | Disappearance dated > 180 days after intro | 501 | needs a person (rule) | no |
| 10 | Death_date without Death_cause / cause without date | 10 / 17 | needs a person | no |
| 11 | Death_cause outside the list | 1 | needs a person | no |
| 12 | Doubtful sex spelled `female_?`, `male_?`, `male?` | 59 | safe | app `list`, data-entry-audit §2.8 |
| 13 | Specimen with a species but empty Sex | 39 | needs a person | no |
| 14 | Methona confusa "male" before 15 May 2023 | 22 | needs a person | sex_label_review_20261027 |
| 15 | SPECIES typo with one obvious name, or family without `(family)` | 47 | safe | app `list` (part) |
| 16 | SPECIES not in Taxonomy_v18Jun25 | 42 | needs a person | app `list` (part) |
| 17 | Subspecies_Form case/accent variant of the common spelling | 6 (+ trims) | safe | no |
| 18 | Subspecies_Form one-letter typos | 11 (8 safe) | safe (8) / person (3) | no |
| 19 | Photo_links CAMs with no specimen row in any sheet | 195 | needs a person | partly app `photo_camid` |
| 20 | Photo files named `CAMO7…` (letter O) | 290 | safe, but a Drive rename | no |
| 21 | "Alive" in Preserved_dead_alive with a Death_date | 7,307 (5,249 Coll., 2,058 Ins.) | needs a person (rule) | database-findings.md, still unfixed |
| 22 | Preserved specimens with no photo | 3,180 | needs a person | app `photo_missing` |

Death_cause spellings are otherwise clean. Every other value is in the list: Unknown 5,253, Disappearance 3,063, Killed_Preserved 2,044, and so on. CAM IDs in Collection_data that are not `CAM+6 digits` belong to other numbering schemes (`ME22_…`, `FS…`, `LTS…`, `M584…`) or are `not given`. I did not count them as errors. The September duplicates CAM078273-078276 no longer repeat.

### Examples and proposed fixes

1. **Whitespace (15, safe).**
   - Examples: Collection r1491 FieldMark_ID `" CR117"`; Tube_1_id `" FS90415348"`; Insectary r12303 SPECIES `"Heliconius hewitsoni "`.
   - Fix: trim.
1b. **Spaces from the dropdown source (65, person).**
   - Examples: Purpose `Abundance estimation ` (r757, r776, r815); Identifier `ABV - Alejandra Barros Veliz ` (21 rows).
   - `Apuya Y ` (20 rows, r8181-) comes from Location_data row 282, which has the same trailing space.
   - Fix: trim the entry in Lists / Location_data first, because Google's dropdown would refuse the trimmed cell while the source keeps the space. Then trim the cells. Location_data also has `Apuya km 2.7` beside `Apuya 2.7km`, worth a look.
   - `BUSCAR ` in a tube cell is a placeholder, not a trim.
2. **CAM repeated within a sheet (5).**
   - CAM078497/498/499 are on Insectary r13854-13856 (Methona confusa psamathe) and again on r13895-13897 (Mechanitis lysimnia). This is the same parallel-run error as September.
   - Older ones: CAM071362 (Collection r1190/r1207), CAM070236 (Insectary r214/r280).
   - Fix: check the envelopes and tubes, then give one butterfly of each pair a new CAM.
3. **Insectary_ID repeated (95).**
   - Examples: Q1 (r157, 2022, M. messenoides deceptus / r1768, 2023, M. lysimnia), Q3, Q4.
   - These are the older formats reused in a later year. Fix: a `.1` suffix on the later row, after checking the wing label.
4. **`T 7`, `T 8`, `T 9` (r189-191), 18 rows.** Decide `T7` or keep. Renaming touches crosses that cite them.
5. **Dates as text (54).**
   - 51 are Dutch dates such as `5-mrt-26`/`9-mrt-26` in Collection Death_date (CAM079264…, r7548-7600): mrt = March, so 2026-03-05 and so on. Each converted date was checked to be on or after the Collection_date.
   - The others are `na` → `NA` and `` NA` `` → `NA`. `9/30` has no year, so it needs a person.
6. **Day/month swap (23).**
   - Rule: day ≤ 12, the date is more than 45 days from the median of the 12 neighbouring rows, and the swapped date is within 10 days of it. Disappearance rows are excluded.
   - Examples: Collection r5530 FS55706685 Collection_date 2024-08-10 (neighbours 2024-10-18; swapped 2024-10-08); Insectary r12265 Intro2Insectary_date 2025-07-11 (neighbours 2025-11-10); Insectary r1240 K93 Death_date 2023-07-03 (neighbours 2023-03-06).
   - Fix: check the notebook, then write the swapped date.
7. **Preservation before death (29).**
   - Examples: Insectary r5670 7BD preserved 2024-03-22, died 2024-04-22 (looks like a month slip); r6591 CAM075278 preserved 2024-06-24, died 2024-07-09; r3900 25L, one day apart.
8. **Long insectary life (23).**
   - Examples: Q8 r163 intro 2022-07-15, died 2023-08-11 (392 d); 59K r3834 (419 d); 33L r3908 (439 d).
   - Probably year slips.
9. **Disappearance > 180 days (501).**
   - Examples: K51 r1172 (193 d), M76 r1287, M88 r1299.
   - These are cage-count dates, not death dates. There is no row fix. Analyses should treat them as "gone by". A last-seen column would need a team decision.
10. **Death without cause (10):** I56 r1006, K56 r1177, 4DK r6259. **Cause without date (17):** L3 r105, V44 r2305, V46 r2307 (all "Unknown").
11. **Death_cause = 45471 (0FL r6787).** That is the date 2024-06-28 typed in the cause column. Move it to Death_date if that is empty, and ask for the cause.
12. **Sex spelling (59, safe).** Examples: CAM072272/072273 `female_?` → `female ?`. These are the list's own values. Turning the doubt into its own flag is a separate decision (§2.8).
13. **Empty sex (39).**
    - 36 in Collection: the LTS000000… series (r7647-), e.g. Memphis polycarmes, Zaretis sp.
    - 3 in Insectary.
    - Fix: fill from the envelope or photo; NOT_COLLECTED if unknown.
14. **Methona confusa (22).**
    - Examples: CAM070500, CAM070567, CAM070568 (2022-07-18). All 22 are in Collection_data, none with a genitalia note.
    - This matches `WingsClassificator/reports/sex_label_review_20261027/AI_REVIEW_AND_METHONA.md`.
    - Draft (not safe, Franz/PAS decide): Sex `male ?`, plus a note.
15. **Species typos (47, safe).**
    - `Riodinidae` → `Riodinidae (family)` (40), also Nymphalidae (4) and Lycaenidae (1).
    - `Perrybris pamela` → `Perrhybris pamela`; `Heliconus numata` → `Heliconius numata` (Insectary F73, r757).
16. **Not in the taxonomy (42).**
    - Eresia quintilla 21, Cithaerias aurora 14 (the taxonomy has Cithaerias pireta), Ithomia maria 2, Eresia mimas 2, Dismorphia praxinoe, Gazera heliconioides, Dysschema buckleyi.
    - Insectary hybrid and cross names were checked binomial by binomial. Apart from the Heliconus typo, all of them resolve.
    - Fix: whoever keeps Taxonomy_v18Jun25 adds the names or gives the accepted name. The same rows show `NOT_FOUND` in Family/Genus (88 formula cells).
17. **Form variants (safe).** `(no subspecies described)` → `(No subspecies described)` (CAM077439); `Hortona` → `hortona` (CAM072695); `eudaminae` → `Eudaminae`.
18. **Form typos.**
    - Safe: messnoides→messenoides (r7168), euridice→eurydice (r8276), angosia→agnosia (r2152), metonella→methonella, lorenia→loronia, leuconae→leuconoe, atergalis→atergatis, eudamine→Eudaminae.
    - Needs a person: `f. intermedia` vs `intermedia` (5 rows, a convention) and `berolla` vs `beronilla`.
19. **Photos without a specimen (195 CAMs).**
    - By folder: "Photos Kim took" 96, "Photos wings of collected butterflies" 92, CRISPR 2, RAW 5.
    - Examples: CAM036744, CAM036778, CAM036779 (Kim's). 188 of these CAMs are in the CAM pools in Lists, so they were issued but never got a row.
    - Fix: type the rows from the envelopes, or correct the CAM in the file name.
20. **`CAMO77716d.JPG` and similar (290 files).** Rename O → 0 in Drive with the assistant's `organize_photos`, as its own card. Photo_links follows on the next sync. This is not a sheet edit, so it has no file here.
21. **"Alive" with a death date (7,307).** No change until the team defines what "Alive" means here (database-findings.md).

### Proposal files (not posted)

The files are in `proposals/`. Each is one call of the assistant's `propose_changes` tool: `{"reason": …, "changes": [{"sheet", "key", "values", "note"}]}`. Nothing is written until someone presses «Aplicar».

| File | Changes | Content |
|---|---:|---|
| `01_whitespace.json` | 15 | trims |
| `03_dates_text.json` | 53 | Dutch dates → YYYY-MM-DD, `na` → NA |
| `05_sex_spelling.json` | 59 | `female_?` → `female ?` |
| `06_species_case.json` | 47 | family names, two genus typos |
| `07_subspecies_variants.json` | 12 | form case variants and one-letter typos |
| `90_methona_sex_DRAFT_needs_person.json` | 22 | **not safe**: Methona `male` → `male ?` + note; only if Franz/PAS agree |

How the rows are named:

- **No lab recordIds.** recordIds from this copy would not exist on the live app, so each change names its row with `key`. The key holds the old value of each changed cell, plus an ID column (CAM_ID, Insectary_ID, FieldMark_ID, …) when the row has one.
- **Unique in this copy.** Every key picks exactly one row here.
- **Safe on the live sheet.** On the live sheet the app refuses a key that matches no row or several rows. A cell already corrected, or changed since, therefore drops out instead of being overwritten.
- **Row numbers.** The 28 Oct copy's row number is in each note.

To post one, ask the assistant in a T3 chat to call `propose_changes` with the file's JSON as its arguments, then review the card. Formula cells were excluded from every change.

### Notes

- Light queries only; no images were read.
- The lab copy may be some days behind the team's workbook. Recheck counts in the Revisión tab before acting.
- Not checked: tube barcodes and racks (the app already flags 454 + 395 repeated tubes), the Pheromones/F1-F2 sheets, and coordinates.
