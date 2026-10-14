# Workbook performance audit (5 Oct 2026)

This audit was read-only and used the Sheets API. Workbook `1QZj6YgHAJ9NmFXFPCtu-i-1NDuDmAdMF2Wogts7S2_4` has 49 tabs, 6.64 M allocated cells, 2.44 M non-empty cells, **1.03 M formulas** and **16 pivot tables**. It has no named ranges, charts, IMPORT\*, NOW/TODAY/RAND or volatile formulas. The only volatile item is one conditional-format rule. The proposed changes are listed in `workbook-performance-changes.json`, ready to apply to a copy.

## The slowness is real

- One of my reads, a 3-cell metadata request, waited **168.7 s** at 00:13 UTC. The next requests took 0.4 s. Reads wait for a pending recalculation, so one edit cost about 3 minutes.
- The app's own full read takes 142–150 s when the workbook is quiet. Today it took **412 s, 502 s and 803 s** while people were editing.

## What actually costs

Almost every derived column is a **per-row lookup into a whole column of another sheet**. Examples are `XLOOKUP(x, Insectary_data!A:A, …)` and `VLOOKUP(x, Insectary_data!A:I, …)`. A whole-column reference makes every such cell depend on the entire column. Any edit in that column, or a new row, dirties *all* of them, and each one rescans thousands of rows.

The cost below is a model: formula cells recalculated × rows each one scans. "Realistic" counts only rows whose key is filled and assumes an average scan of 0.75 × the column length. The absolute throughput of Google's engine is unknown. The comparisons between causes, and the before/after factors, are what matter. Measure them on the copy.

What one edit triggers today:

| Edit | Formula cells dirtied (other sheets) | Model cost (cell reads) |
|---|---|---|
| New reared individual typed into Insectary_data (B…AC) | 75,000 | ~0.56 G (upper bound 1.2 G) |
| One Insectary_data CAM_ID (P) | 35,000 | ~0.27 G |
| One Insectary_data Wild_Reared (B) | 23,000 | up to 0.37 G |
| One row appended to Photo_links | 68,000 | ~0.5 G (upper bound 3 G) |
| One Collection_data Insectary_ID (D) | 27,600 | ~0.15 G |
| Insectary_stocks row | 13,000 | ~4 M |

## Top causes, ranked

| # | Cause | Size | Cost per edit (model) | Fix (identical results) | Expected |
|---|---|---|---|---|---|
| 1 | **F1/F2_MutationRate lookup block**: ~33 whole-column XLOOKUPs per row into Insectary_data A:A / P:P and Collection_data D:D | 57,000 lookup cells, 2,438 rows (2,105 used) | **~450 M** of the 556 M for a new Insectary_data row. Any edit to Insectary_data A, B, E, F, M, O, P, Q–X or AB rescans all of them | **C1**: 9 hidden helper columns per row (`XMATCH` once per key), then `INDEX` | 3–10× on Insectary_data saves. P edit 274 M → ~25 M; B/E/F/M edits → ~0 |
| 2 | **Photo link columns**: Collection_data AO/AP run 2 identical exact MATCHes per block over Photo_links (18,268 rows). Insectary_data AD/AE and CRISPR N:P also look up Photo_links | 49,000 cells | **~0.5 G per Photo_links append** | **C2**: `LET(photoRow, MATCH(…), HYPERLINK(INDEX(E,photoRow), INDEX(B,photoRow)))` | 2× on Photo_links changes |
| 3 | **Unguarded VLOOKUPs in near-empty sheets**: Stocks_Matings (110 rows used of 999) and Hybrid_Attempts (34 of 997) | 11,000 cells. ~1,850 blank pre-filled rows each run VLOOKUP("") → #N/A over Insectary_data A:F / A:I | up to ~160 M per Insectary_data A–I edit (unverified upper bound) | **C4**: `IF(k="",NA(),INDEX(col,MATCH(k,A:A,0)))`. The same #N/A results, and each cell depends on 2 columns instead of up to 10 | ~100× for these cells |
| 4 | **Insectary_stocks K/L**: `IF(COUNTIF(C:C,A3)=0,"NA",COUNTIF(C:C,A3))` computes the count twice, plus a MATCH, on 1,416 rows. 344 of those rows are blank | 2,831 cells | ~33–59 M per new individual (Insectary_data C edit) | **C3**: `LET` for one COUNTIF, plus a blank guard | ~2× |
| 5 | **Pre-filled idle rows**: Insectary_data holds data to row 13,698 but formulas fill 22 columns down to 20,846 | 150,000 idle formulas, 380,000 allocated cells | They recalculate after every Photo_links, snapshot, Location_data, Insectary_stocks or Collection_data D/Z change. They also make Insectary_data whole-column scans up to 34% longer if trailing rows are scanned | **C5**: delete rows 15,701–20,846. All their values are empty (checked). About 1,900 spare rows are kept | 113,000 fewer idle formulas; up to 25% shorter scans |
| 6 | **Collection_data → Insectary_data**: Insectary_data O is an unguarded `XLOOKUP(A, Collection_data!D:D, E:E)` on 7,878 rows, mostly misses that scan everything. G (13,524) and MutationRate AW:BB also look up Collection_data | 27,600 cells | ~150 M per Collection_data D edit | C1 removes the MutationRate share (~66 M). O has no identical per-row fix; it's a phase-2 ARRAYFORMULA candidate | ~1.8× |
| 7 | **Snapshot lookups**: Ithomiini_tube_locations_18Jun26, MEIER_manifests_23Jun26/_30Jan25_OLD, Taxonomy_v18Jun25, Lists, Location_data. Insectary_data AG:AN and AT:AX, Collection_data O:R, X:Y and AY:BW use them | ~300,000 cells | **0 on normal edits**. 0.25–1.8 G whenever PAS edits or replaces a snapshot | Phase 2 (E1): one ARRAYFORMULA per owner-protected column, if measurements show it is cheaper | unknown, measure |
| 8 | **16 pivot tables**: 10 read Collection_data, 2 Insectary_data, 2 MutationRate, 2 Insectary_stocks | ~8–13 k source rows each | 10 pivot refreshes per Collection_data edit | O2: PAS removes the unused ones (Panama/Brazil/Peru/PopGen/VisualAcuity?) | moderate |
| 9 | **Conditional formatting**: about 75 rules. These include O(n²) `COUNTIF(P:P,P1)>1` duplicate checks on whole columns, 24 copy-paste fragments on MutationRate D, stray fragments on unrelated columns, and a volatile `INDIRECT` ±2,600-row window on Insectary_data A | about 2 G comparisons if evaluated naively over every cell | **Measured cheap** on the server: effectiveFormat for P2:P20846 and A2:A20846 came back in 0.4–0.6 s | O3: merge the fragments; only matters if the browser stays slow | low |
| 10 | **Dead formulas**: Ithomiini_tube_locations L:Q (69,474 cells) look up the deleted sheets `Genus&CollectionDate_21Nov24` and `Filter_tube_locations` and are all #REF!. Sperm_dissections Z:AF (6,993 cells) have #REF! keys | 77,000 cells | ~0 (no live precedents) | O1: delete them (#REF! becomes blank, so not identical) | file size only |

Data validation is not a cost driver: rules are checked only for the edited cell and for drawing dropdowns.

The **Apps Script hook** (onEdit + onChange → `UrlFetchApp`) runs asynchronously for 2–3 s. It doesn't read the grid, block the editor or trigger recalculation. The app's re-read of the reported rows waits for the recalculation, which is why the app "hangs" along with the sheet. I could not read the bound script with these scopes; the repo copy is `tools/apps-script/SheetEditHook.gs`.

## Why the cheap-looking fixes are not enough

- **Bounding `A:A` to `A2:A20000` does not change the dependency fan-out.** Any edit inside the bounded range still dirties every dependent. Trimming the grid (C5) gets the same scan reduction with less to maintain.
- **One ARRAYFORMULA per column is the textbook fix, but it clashes with the app.** The app reads per-row formulas (`server/formula.mjs` and `formula-gives.mjs`). It makes pre-made rows by copying the last row's formulas (`server/premade.mjs`, which applies to all sheets). It lets people type over Insectary_data `SPECIES` and `Collection_location` (`TYPED_OVER_FORMULA`), and a value typed into a spill area turns the whole column into #REF!. Phase 1 therefore keeps a formula in every row. ARRAYFORMULA is only proposed (E1, E2) for **owner-protected** columns, which neither the app nor the team can write. Even there it must be measured first: Google doesn't document whether it indexes the lookup range once.
- **Helper columns are safe for the app.** It reports unknown headers as "columna nueva … (ignorada)" and carries on.

## Proposed changes (details and exact formulas in the JSON)

1. **C1, MutationRate row helpers.** Add hidden columns BD:BL to `F1/F2_MutationRate`, each `=IF($K2="","",XMATCH($K2, <range>))`:
   - D → Insectary_data A
   - D → Collection_data D (wild-caught rows only)
   - AB → Insectary_data P
   - G, J, M, P, S, V → Insectary_data A

   Then rewrite each lookup as `IF(ISNUMBER($helper),INDEX(col,$helper),"<same not-found text>")`. XMATCH matches the same way as XLOOKUP (exact, no wildcards, case-insensitive).

   Each cell is rewritten from its own formula, because the columns aren't uniform. Row 12's H looks up M12; rows 4–503 of K/N/Q/W hold #REF! keys. Those cells are left as they are.

   Effect: scans happen only when Insectary_data A or P, or Collection_data D, changes. Edits to B, E, F, M, O, Q–X or AB cost INDEX only.
2. **C2, Collection_data AO/AP.** One MATCH per photo instead of two, via LET.
3. **C3, Insectary_stocks K/L.** LET plus a blank guard. Live rows 1300–1301 show the blank case gives "NA" today.
4. **C4, Stocks_Matings, Hybrid_Attempts, Melinaea_crosses, Crosses_Lys_x_Pol, Nail_Polish.** Replace each VLOOKUP over a wide range with a blank guard and INDEX/MATCH on two columns. The #N/A results stay the same; only the tooltip text changes.
5. **C5.** Delete Insectary_data rows 15,701–20,846.
6. **Phase 2 experiments, owner-protected columns only, measure first.**
   - E1: Collection_data O:R and BJ:BK (taxonomy) as one ARRAYFORMULA each.
   - E2: the photo columns.
   - An alternative for MutationRate is to compute its derived columns in the app, which already holds every sheet in SQLite. That changes PAS's workflow, so it's his decision.
7. **Optional, results change:**
   - O1: delete the dead #REF! formulas.
   - O2: remove unused pivots and fix stale pivot ranges.
   - O3: merge conditional-format fragments.

**Combined estimate (model):**
- A typical Insectary_data save, which writes B…AC but not the ID formula: ~0.56 G → ~0.08–0.12 G cell reads (**~5×**).
- A single CAM_ID edit: **~10×**.
- Edits to Wild_Reared, SPECIES, Sex or the dates: **>20×** for the cross-sheet part.
- A Photo_links append: ~2×. More only if E2 pays off.

If the observed 2–3 minutes is mostly this work, saves should drop to roughly 20–40 s after phase 1. The copy measurement will confirm or correct that.

## Risks and checks

- **Identity check:** export every sheet as UNFORMATTED and FORMATTED values before and after each change and diff all cells. Expected differences are the new helper columns and the deleted empty rows only.
- **Points to check on the copy:**
  - INDEX of an empty cell vs XLOOKUP of an empty cell: both should be blank.
  - Error keys keep erroring.
  - The XMATCH/XLOOKUP equivalence holds for "NA" keys (Insectary_data A has 245 "NA" IDs; today `XLOOKUP("NA",…)` returns the first such row, and XMATCH does the same).
- **App impact:**
  - The app's formula evaluator doesn't know LET or XMATCH, so it shows the sheet's own value for those cells. That only affects the "what the formula will give" preview.
  - New columns raise a non-blocking notice.
  - C5 means PAS extends pre-made rows in about 6–7 months, as now.
- **Who uses it:** MutationRate feeds the F1/F2_Summary* pivots; Insectary_stocks K/L feed the app's clutch view.

## Bugs found along the way (results would change, so they're for PAS to decide)

- **Insectary_data AD/AE** test `ISERROR(MATCH(key, Photo_links!B:B))` with *approximate* MATCH, which almost never errors. Missing photos therefore show `#N/A` instead of "NOT FOUND", and the app's photo check, which looks for "not found", misses them. Rows 13690–13698 today show #N/A.
- **Pivots exclude recent rows:**
  - Collection_data pivots stop at row 7856; data reaches 8254.
  - Stocks_state stops at row 13472 and messenoides_phenotypes at 12462; Insectary_data reaches 13698.
- **Collection_data AK:AN/N** (`XLOOKUP(D, Insectary_data!A:A, …)`) have no guard. In their ~1,270 rows, a D of "NA" gets the values of the first Insectary_data row whose ID is "NA" (inferred from the formula; check it).
- **Copy-paste errors:**
  - MutationRate has keys pointing at the wrong column (e.g. H12 uses M12) and ~650 #REF! keys.
  - Insectary_stocks has #REF! in R/S/T.
  - Insectary_data AG/AK/AO/AD/AE have #REF! in rows 12165 and 13393–13561.

## Who must act

- **PAS (owner):** C1, C2, C5 and E1/E2 touch owner-protected ranges (MutationRate A…AL, Collection_data O:R/X:Y/BG:BY/AO:AP, Insectary_data AG:AJ), or need rows deleted or columns added. By team rule, C3 and C4 also go through him.
- **App side:** nothing for phase 1. Phase 2 needs a check that the app tolerates spill columns.
