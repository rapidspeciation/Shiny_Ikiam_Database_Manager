# UI wave 2 (P2 + P3 of AUDIT.md), 1 Nov 2026

Branch `ui-wave2-20261101` (from ui-overhaul-20261031 1a4c8d6), 18 commits 7c87c9b..HEAD. Not pushed, not deployed.
Checks: `npm run typecheck` ✓, `vitest run` 79 files / 717 tests ✓ (new: `lib/__tests__/disabledReason.test.ts`),
`npm run build` ✓.

**How it was measured.** Own dev app (`tools/lab/dev-app.sh --port 8851`, dir `~/.local/share/ui-overhaul-20261031/w2-app`,
now stopped), with the three audit proposals copied from w1-app: rows of `ai_proposals` **and** their two `ai_threads`
rows (the proposals list joins them; without them every /propuestas page stays on «Cargando…»), owner set to `lab`.
Shoot script: the wave 1 copy (T3 status answered in the browser, read-only), copied to `w2-scripts/shoot/`.
34 views × 1440/390/360 × es/en, both themes: `~/.local/share/ui-overhaul-20261031/wave2-after-light/` and
`…/wave2-after-dark/` (metrics.json, summary.md). "Before" = `c1-after-light/` and `c1-after-dark/`.
Comparison: `w2-scripts/compare.py` (same captures only; 195 light / 196 dark). A first run without the thread rows is
kept as `wave2-try1-*` (not used for numbers).

**Totals on those captures (before → after; light / dark):**
- Enabled text under its bar: 12 → 0 / 0 → 0. No new contrast failures in either theme.
- Disabled labels under 3:1: 12 → 0 / 48 → 0.
- Text under 12 px: 11,900 → 1,462 / 1,580. What is left: inside the rack's wells and its row and column numbers
  (9–11 px, so 96 wells fit beside the photo), and three 11 px items in ProposalsLive.vue / ProposalGrid.vue
  («T3 Code · hora», «en vivo», «Revisar con la foto»), left because another child edits those files now.
- Unnamed controls: 893 → 301 / 308.
- Page overflow on phones: 0 → 0. Focus stops without an indicator: 0 → 0.
- Taps under 24 px and "clipped" rise (2,349 → 2,433; 807 → 935) because the script counts what is drawn: the new
  44 px `::after` finger areas are not counted, and an `::after` wider than a 16 px link reads as overflow of the link
  and its header (no page overflow, nothing painted).

| id | status | change | before → after (measured) | screenshots (before → after) |
|---|---|---|---|---|
| row-number | fixed already (dark merge), re-measured | Row numbers use fg-subtle on surface-sunken. | 4.40 → 4.96 light / 5.89 dark; 0 failing nodes | c1-after-light/propuestas__390-es.jpg → wave2-after-light/propuestas__390-es.jpg |
| row-remove | fixed already, re-measured | ✕ is fg-subtle on the cell. | 2.52 → 5.41 / 6.33 | c1-after-light/rack__1440-es.jpg → wave2-after-light/rack__1440-es.jpg |
| muted-on-sunken | fixed already, re-measured | stone-500 is fg-subtle; the Clutches counts at 80 % pass. | 4.39 → 4.96 light / 5.63 dark; 0 failing nodes on asistente, revision, clutches, map | c1-after-light/asistente__1440-es.jpg → wave2-after-dark/asistente__1440-es.jpg |
| sex-tiles | fixed (contrast by the dark merge; meaning stated now) | style.css says pink and sky mean female and male, used for nothing else. | white on fill-pink / fill-sky: no node under 4.5 in either theme | wave2-after-dark/emergidos-clutch__360-en.jpg |
| disabled-low | fixed | `.btn`, `.btn-primary`, `.btn-danger` disabled at 64 % (T3's value) instead of 50 %; «Otra igual» and the Wikiloc download links too (the links: `aria-disabled` and `cursor-not-allowed` instead of `pointer-events-none`). A `title` with the reason on the disabled buttons that lacked one (copy/print with nothing to copy, «Usar» with nothing typed, «Revisar 0 casos», «Traer», «Asignar IDs», «Imprimir etiquetas», «Otra igual», site updates, reset link). **Functional (small):** a tap on a disabled button with a title shows that reason at the bottom (`lib/disabledReason.ts`; browsers send pointer events, not clicks, to disabled buttons). «Mover» already had reasons. | <3:1 disabled labels 12 → 0 (light), 48 → 0 (dark). Primary disabled dark 2.86 → 3.67; emerald «Revisar 0 casos» 2.48 → 3.14; «Otra igual» 2.87 → 4.18 | c1-after-dark/clutches-nuevo__390-es.jpg → wave2-after-dark/clutches-nuevo__390-es.jpg; wave2-after-dark/fotos-photolinks__390-en.jpg |
| font-11 | fixed (grid internals kept) | `text-[11px]`/`text-[10px]` → `text-xs` in 30 components; 11 px CSS → 12 px for the cell bar's notes, order notice, sheet banner, quick-sheet text, column chooser, formula-cost notice, deposit head, rack buttons/legend/notes; chart axes, legends and value labels 10–11 → 12. Kept: marks inside grid cells, the rack's wells, Muertes' aria-hidden key hints. The archive review's seguro/pensar/mirar columns widened to fit (w-7 → w-12). | < 12 px nodes 11,900 → 1,462 (light) | c1-after-light/clutches__390-es.jpg → wave2-after-light/clutches__390-es.jpg; wave2-after-light/monitoreo__1440-es.jpg |
| small-taps | fixed | On coarse pointers every `.btn`, `.btn-primary`, `.btn-danger`, `.btn-ghost` gets the 44 px `::after` finger area (as `.hit-target`). `hit-target` on Inicio's «Tabla», «ver más / Ver más / Cargar más», Revisión's row IDs and «ver todos / ver solo el lote», the hide-help ✕, Usuarios' row links. ColumnChooser arrows, grip and ✕ 16 → 28 px on touch. | 44 px areas are not counted by the script (see totals) | c1-after-light/propuesta-columnas__390-es.jpg → wave2-after-light/propuesta-columnas__390-es.jpg |
| placeholder-labels | fixed | aria-labels: Revisión comment, both Buscar boxes, IdPicker input (its label), the Drive folder link, Usuarios role boxes/checkboxes/reset link; the filter panels' ✕ named «Cerrar». | unnamed 893 → 301 (light) | c1-after-light/revision__1440-es.jpg → wave2-after-light/revision__1440-es.jpg |
| redundant-counts | fixed (partly) | Clutches: «Todos» chip without its number (= «En curso»), today's summary keeps only «n revisados» (the chips carry por verificar / sin revisar). Rack: the legend no longer counts «falta» and «sin fila en el libro» (the lines under it do); RackChanges hides «Aún no: …» when the deposit head says the same. The «94 filas nuevas» title and «Aplicar 94 filas» stay (ProposalGrid.vue, other child). | 27 ×4 → ×2; rack «Aún no» ×2 → ×1, 94 ×5 → ×4 | c1-after-light/clutches__1440-es.jpg → wave2-after-light/clutches__390-es.jpg; c1-after-light/rack__1440-es.jpg → wave2-after-light/rack__1440-es.jpg |
| proposal-ai-marks | deferred | Cause found: the one-row proposal changes a notes column scrolled off to the right, so no visible cell is marked while the legend shows IA. The fix (legend shows only the kinds present and names the changed columns, or the grid scrolls to the first change) is in ProposalGrid.vue, which another child is editing now. | — | c1-after-light/propuesta__1440-es.jpg |
| tablas-avisos-phone | fixed | WorkbookWarnings: on phones a short summary «Avisos del libro (n)», a chevron showing it opens/closes, list max 8 rem, a full-width «Cerrar» under it. | panel had no close → close at the bottom; summary no longer cut | c1-after-light/tablas-avisos__390-es.jpg → wave2-after-light/tablas-avisos__390-es.jpg |
| usuarios-phone | fixed | Both tables in `ScrollRow` (the cut edge fades); a line above on phones «Desliza las tablas a un lado para ver «Activa» y las acciones.»; controls named. A card layout was not done (more template, same data). | no cue → fade + line | c1-after-light/usuarios__390-es.jpg → wave2-after-light/usuarios__390-es.jpg |
| select-boundary | fixed already (wave 1 field-boundary), re-measured; offline page done | The chat picker and «+ Columna…» use border-control (no boundary under 3:1 in any capture). /prueba-offline/ select was already on --control (3.73 / 3.81). | 1.26 → ≥ 3.7; 0 bounds failures on asistente-propuestas, propuestas, rack, prueba-offline | wave2-after-dark/prueba-offline__390-es.jpg |
| /prueba-offline/ focus rings | fixed | `:focus-visible` 2 px solid --focus, offset 2 px (#2d6a4f light, #7cc6a0 dark); checkboxes/radios 20 px with accent-color. | browser 1 px (1.01:1 in dark) → 6.12 light / 9.38 dark on the canvas | wave2-after-dark/prueba-offline__390-es.jpg |
| t3-update-pulse | fixed | «Cambios propuestos» no longer `animate-pulse` when a proposal arrives: the emerald fill plus a still dot for the same 4 s. (SpeciesPhoto's pulse while a search runs is a loading state and stays.) | continuous animation → none | wave2-after-dark/asistente__1440-es.jpg (no fresh proposal in the still) |
| truncation | fixed | Species on clutch, emerged and clutch-picker cards wrap to two lines instead of «…»; «n preservadas» may hyphenate on 360 px; the clutch history dialog says «Historial» on phones; workbook-warnings summary (above). | names cut by 4–19 px → whole; clamp clips in clutches 27 → 0 | c1-after-light/clutches-historial__390-es.jpg → wave2-after-light/clutches-historial__390-es.jpg |
| empty-states | fixed (Tubos) / deferred (map) | Tubos (Tabla): on phones «Elige IDs arriba y pulsa Cargar.» sits under the ID picker (bottom copy hidden on phones). The Monitoreo map's «Aún no hay recorridos» is in MapPanel.vue (other child's file): deferred. | message y≈870 → right under the picker | c1-after-light/tubos-tabla__390-es.jpg → wave2-after-light/tubos-tabla__390-es.jpg |
| fotos-stepper | fixed | With the rename app missing, the six-step bar and the «Pulsa Open folder» line are hidden; the note and the link stay. | — | c1-after-light/fotos__1440-es.jpg → wave2-after-light/fotos__1440-es.jpg |
| chart-white-on-blue | fixed already (review round 1), re-measured | Heat table: white ink only on the three darkest blues in light, dark ink on the pale ones in dark. | 4.42 → min 4.92 light / 4.82 dark across the 7 steps; 0 failing nodes on monitoreo views | wave2-after-light/monitoreo-tabla__390-es.jpg |
| formula-selected | fixed already, re-measured | A picked formula cell's text is fg on the range blue. | 3.91 → 8.96 light / 9.66 dark | (tablas-fila not re-shot) |
| phone-grid | deferred | A row form instead of the grid on phones changes how people edit (functional), as in wave 1. | — | — |

**Review additions (wave 1 critics):**
- DatabaseBadge: «Prueba / Copia de prueba» is now outlined in neutral colours (on-brand on the header, fg on light
  bars) with a flask icon, instead of amber. Its meaning is the title, and a tap shows it at the bottom (6 s).
  Before c1-after-light/clutches__390-es.jpg → after wave2-after-light/clutches__390-es.jpg.
- SaveBar: the list of unsaved places is a button; a tap opens «Revisar» with all of them (the full list was only a
  title). Opening Revisar writes nothing. wave2-after-dark/tablas-pendiente__390-en.jpg.

**Not done / notes:**
- Files avoided because a parallel child edits them: ProposalGrid.vue, ProposalsLive.vue, PhotoReview.vue,
  MapPanel.vue, ScrollRow.vue (used, not edited), lib/theme.ts. Their 11 px texts, the map's empty state, the map's
  «Quitar filtros» reason and proposal-ai-marks wait for that work.
- Looked at 18 screenshots myself (clutches, tablas-avisos, usuarios, inicio, monitoreo, rack ×2, colecta-especie,
  propuesta, fotos-photolinks ×2, prueba-offline, tablas-pendiente, asistente, emergidos-clutch; light and dark).
  They found two regressions of this wave, both fixed and re-shot: the rack legend lost its spaces («nuevo94»), and the
  archive review's column headings ran together at 12 px.
