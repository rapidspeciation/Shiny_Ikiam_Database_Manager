# UI wave 1 (P0 + P1 of AUDIT.md), 1 Nov 2026

Branch `ui-wave1-20261101` (from ui-overhaul-20261031), 10 commits 663e057..2f5385b. Not pushed, not deployed.
Checks: `npm run typecheck` ✓, `vitest run` 78 files / 714 tests ✓ (new: `lib/__tests__/scrollRow.test.ts`,
`pendingPlaces.test.ts`), `npm run build` ✓.

**How it was measured.** Own dev app (`tools/lab/dev-app.sh --port 8841`, LOCAL_MODE on the lab snapshot, admin `lab`, now stopped). The three
audit proposals (rack, small, chat) were copied into its database from the audit app so the rack views exist.
The audit's shoot script was copied to `~/.local/share/ui-overhaul-20261031/w1-scripts/shoot/` with three changes:
user `lab` instead of `uiaudit`, and the T3 status answered in the browser (two read-only GETs: `t3/status`, `admin/t3`). This dev app has no T3, so the
Asistente bar and the T3 update dialog show as they would with T3, and the frame itself stays empty (the lab T3 was not used).
Shots: 43 views (the ones these ids touch) × 1440/390/360 × es/en, both themes, 256 per theme:
`~/.local/share/ui-overhaul-20261031/wave1-after-light/` and `…/wave1-after-dark/` (metrics.json, summary.md).
"Before" is `audit-before/` (light) and `dark-foundation/run2/` (dark). Per-capture comparison:
`w1-scripts/compare-light-final.txt` (`w1-scripts/compare.py`).

**Totals on those 43 views (before light → after light / after dark):**
- Phone captures with main/section/a fixed dialog wider than the screen: 60 → 0 / 0, out of 172.
- Text under 4.5:1 (disabled controls not counted): 3,060 → 87 / 27. All the rest were there before wave 1
  (see "Left", below). Wave 1 added none in either theme.
- Control edges under 3:1: 1,149 → 30 / 24. What is left: the cell bar's textarea (no edge, like a formula bar) and
  the theme radios' sr-only inputs.
- Focus stops with no indicator: 150 → 0. Chromium's 1 px auto ring: 1,881 → 0.
- Controls with no name: 1,368 → 748. All date and grid-filter boxes now have names. The rest have only a placeholder
  (for example «Comentario (opcional)»); none of the wave 1 ids covers them.

| id | status | what changed | before → after (measured) | screenshots (before → after) |
|---|---|---|---|---|
| mon-phone-overflow (P0) | fixed | Monitoreo sub-tabs in `ScrollRow` (scrolls sideways, the edge with more fades out, the open tab is scrolled into sight). The «Wikiloc» tab can be tapped (probe click OK). The photo viewer's × and › are on-screen. | main 430 px → 390 (doc width = viewport at 390 and 360, es and en) | audit-before/monitoreo-fotos-especie__390-es.jpg → wave1-after-light/monitoreo-fotos-especie__360-es.jpg |
| rev-phone-overflow (P0) | fixed | Revisión tabs in ScrollRow. The search bar wraps on phones: line 1 is Filtros + search, line 2 is order + pages + refresh. The search box has a name. | 468 px → 390/360. Alertas tab, pager › and «Hasta» all on-screen | audit-before/revision-filtros__390-es.jpg → wave1-after-light/revision-filtros__390-es.jpg, revision__360-es.jpg |
| asistente-phone-overflow | fixed | The Asistente bar wraps (items keep one line each). The T3 dialog panel breaks long words. The bar wraps to 2 lines on phones. | 431 px → 390/360. The dialog's × is on-screen | audit-before/asistente-t3-version__390-es.jpg → wave1-after-light/asistente-t3-version__390-es.jpg |
| header-tabs-clipped-1440 | fixed | Header tabs `px-2` below 1536 px (`px-3` from 2xl), inside ScrollRow (fade cue when they do not fit, e.g. with a «waiting» badge or below 1280). | header nav at 1440: es 977/977 px, en 1016/1016 (scrollWidth/clientWidth). At 1280 es: 870/817, with a fade | audit-before/inicio__1440-es.jpg → wave1-after-dark/tablas-pendiente__1440-es.jpg |
| db-indicator-phone | fixed | New `DatabaseBadge`: «Copia de prueba» («Prueba»/«Test» on phones) or a Google Sheet link («Sheet» + icon on phones), at every width. On phones it sits at the end of the tab row. /propuestas shows it at its foot. | absent in all 390/360 captures → present in all of them. /propuestas: none → badge | audit-before/emergidos__390-es.jpg → wave1-after-light/tablas-pendiente__390-es.jpg, rack__390-es.jpg |
| header-focus-ring | fixed | Base `:focus-visible` = 2 px solid focus colour, offset 2 px (style.css, base layer). White inside `.app-header`, back to the focus colour in the user menu. Header tabs use an inset ring (they scroll). | auto 1 px rgb(16,16,16) on 1,881 stops → 0. Measured in the browser: EN/ES 2 px #ffffff. Body controls 2 px #2d6a4f (light) / #7cc6a0 (dark) | (not visible in still shots; w1-scripts/focuscheck.mjs) |
| grid-focus | fixed | `.tabulator-tableholder:focus-visible` gets a 2 px focus-colour ring inset by 2 px. It differs from the blue range and the green picked-row bar. Text fields get a 2 px focus edge (1 px border + 1 px ring) instead of the 25 % halo. | 148 stops with no change → 0 stops without an indicator. Grid: outline solid 2 px, offset −2 px, `:focus-visible` true after the arrow keys | — |
| enes-toggle | fixed | EN/ES 24 px tall, 44 px target on touch screens (`.hit-target::after`). The inactive language is on-brand-muted (dark merge). | 45×20 → 45×24 drawn. Contrast fails 226 → 0 (3.83 → ≥ 4.5) | audit-before/inicio__390-es.jpg → wave1-after-light/tablas-pendiente__390-es.jpg |
| menu-tap | fixed | User menu button `pointer-coarse:min-h-11`; it fits the 44 px header row, so the header does not grow. ChoiceField ▾ gets a 44 px `::after` target on touch screens. | menu 84×28 → no longer under 44 px on phones. ▾ stays 24×32 drawn; its 44 px target is a `::after` the script does not count | — |
| date-unnamed | fixed | DateField falls back to «Fecha» when it has no `<label>` and no aria-label. Named where the context is known: DateRow (its field), TabHistory/TodayChanges «Día», Censo «Día del censo», Muertes «Fecha de muerte», Tubos (clip date / Preservation_date), NewClutch «DATE LAID», EventSheet. | date boxes without a name 78 → 0 | — |
| grid-filter-inputs | fixed | Tabulator `headerFilterParams.elementAttributes` adds aria-label «Filtrar {field}» («Filter {field}»). The edge uses fg-subtle (≥ 3:1 on the header's grey and on a picked column's #d6d6d6). | unnamed 484 → 0. Edge 1.15 → 3.8 (light, on a picked column) | audit-before/tablas__1440-es.jpg → wave1-after-light/tablas__1440-es.jpg |
| pending-vs-warnings | fixed | Save bar: the colour of unsaved cells (`bg-cell-pending`), a 2 px pending edge and a pencil, and it names the rows (`lib/pendingPlaces.ts`: «Insectary_data · R6E · fila 13698: Notes_Insectary_data», the first 2 + «y n más», all of them on hover). Workbook warnings: grey sunken surface with an amber sign. | both amber-50/amber-800 → pending #fdf0c4 with edge #b45309 vs warnings #f5f5f4 (dark: #3a2c0c/#f59e0b vs #1b211e) | audit-before/tablas-pendiente__1440-es.jpg → wave1-after-dark/tablas-pendiente__1440-es.jpg, wave1-after-light/tablas-pendiente__390-es.jpg |
| rack-apply-far | fixed | ProposalGrid's Aplicar/Descartar row is `sticky bottom-0` while the proposal is pending. It stays on screen and names the apply («Aplicar 94 filas»). The exception is a phone beside the photo, where the pane is too short. | > 2 screens below the fold → always on screen | audit-before/rack__1440-es.jpg → wave1-after-light/rack__1440-es.jpg, wave1-after-dark/rack__390-es.jpg |
| rack-photo-phone | fixed | On a phone in «Revisar con la foto»: the deposit fields fold to one summary line (open when a problem is listed). The rack's 5 fields go two to a line. The divider's touch target no longer widens the dialog by 6 px. | grid cells visible in the top pane: 0 → 3 rows (A–C). Dialog 366 → 360 px wide | audit-before/rack-foto__390-es.jpg → wave1-after-dark/rack-foto__390-es.jpg |
| map-panel-phone | fixed | On phones the panel covers the map fully, with its own «Ocultar panel» at its foot (sticky). The floating button shows only when the panel is closed, so nothing overlaps the checkboxes. **Layout change:** the old 90 px strip of map beside the panel is gone. | overlap at y≈800 → none | audit-before/monitoreo-mapa__390-es.jpg → wave1-after-light/monitoreo-mapa__390-es.jpg |
| stone-400-text | fixed (by the dark merge) | stone-400 is now fg-subtle #6f6964 (light) / #959d98 (dark). | #a6a09b nodes 1,875 → 0 | — |
| field-boundary | fixed | Besides `.field-input` (already border-control after the merge): 30+ hand-styled inputs/selects with stone-200/300 edges, the rack deposit's inputs, and amber-500 edges of marked fields (→ amber-700) now meet 3:1. | edges at #d6d3d1/#e7e5e4: 1,047 → 0. Amber DateField 2.13 → ≥ 3 | audit-before/entrar__1440-es.jpg → wave1-after-light/entrar__1440-es.jpg |
| amber-badges | fixed (by the dark merge) | The Alertas count is on `bg-fill-amber` (white ≥ 4.5). | white on #fe9a00 (2.13): 20 → 0 | — |
| phone-grid (P2) | deferred | A row form instead of the grid on phones changes how people edit, so it is a functional change and was left for its own decision. | — | — |

**Left, measured but outside these ids (same before wave 1):**
- Monitoreo report: white on #2a78d6, 4.42 (48 nodes).
- Muertes keyboard-number hints (aria-hidden): 3.1 light / 4.24 dark.
- Disabled-looking links «Puntos (CSV)» / «Recorrido (GPX)» (`pointer-events-none opacity-50`, not `disabled`).
- Placeholder-only fields: «Comentario (opcional)», the Buscar boxes of the review panels.
- The cell bar's textarea has no edge.
- Taps under 24 px: 1,855, unchanged. Those are grid cells and small links; the `::after` targets added here are not counted by the script.

**Not done:** the screenshot set covers the views these ids touch, not all 74.
