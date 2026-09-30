---
name: app-guide
description: Guide to the "Ikiam Insectary DB" web app (https://ithomiini-ikiam.com) — every tab (Inicio, Tablas, Colecta, Monitoreo, Muertes, Tubos, Emergidos, Clutches, Historial, Asistente, Revisión, Usuarios), what each is for, who can use it, its controls and workflows, grid and date tips, which Google Sheet it writes, and deep links with query parameters. Use it whenever the person asks how to do something in the app, where something is, why a button or tab is missing, asks for a link, wants to find or undo a saved edit, or wants data entered (collections, monitoring captures, deaths, tubes, emerged adults, clutches) — then prefer drafting a proposal with the ithomiini tools and give the direct link.
---

# App guide: Ikiam Insectary DB

The team's web app over the team's Google Sheets workbook (the «Google Sheet»
button at the top right opens it; saves go straight into it). Every page is a hash route:
**full link = `https://ithomiini-ikiam.com/` + route**, e.g.
`https://ithomiini-ikiam.com/#/monitoreo?vista=dudas`. The UI is in
Spanish; quote its labels exactly, in «».

Per-tab detail (step by step, every control, every link parameter):

- [reference/data-entry.md](reference/data-entry.md): Tablas, Colecta, Muertes,
  Tubos, Emergidos, Clutches, and the grid/keyboard/date tips.
- [reference/monitoreo.md](reference/monitoreo.md): Importar recorrido,
  Reporte, Mapa, Recapturas, Dudas de emparejamiento.
- [reference/review-history-assistant.md](reference/review-history-assistant.md):
  Inicio, Historial, Asistente (T3 Code, Cambios propuestos),
  Revisión, Usuarios, login and invitations, the save bar.

Read the reference file of the tab before giving steps for it.

## Tabs at a glance

| Tab | Route | For | Writes to |
|---|---|---|---|
| Inicio | `#/inicio` | summaries; for the team: last IDs used, next Insectary ID and clutch, upcoming hatch/pupa/emergence | — |
| Tablas | `#/tablas?hoja=…&buscar=…` | any sheet as an editable spreadsheet | the chosen sheet |
| Colecta | `#/colecta` | a day of field collection in bulk | Collection_data (+ Insectary_data for live ones) |
| Monitoreo | `#/monitoreo?vista=…` | Ikiam transects T1–T4: Wikiloc walks, report, map, recaptures, doubtful pairings | Collection_data, SamplingDay_data |
| Muertes | `#/muertes` | death date and cause of insectary butterflies | Insectary_data |
| Tubos | `#/tubos` | CAM IDs, tubes, tissue, medium; tube labels | Insectary_data |
| Emergidos | `#/emergidos` | new adults of a clutch into pre-made rows | Insectary_data |
| Clutches | `#/clutches` | new clutches and their follow-up | Insectary_stocks |
| Historial | `#/historial` | every saved change; selective undo | (undo writes back) |
| Asistente | `#/asistente` | T3 Code (this assistant) and Cambios propuestos | via proposals |
| Revisión | `#/revision?…` | data problems as cards to judge | verdicts (app); fixes via a proposal |
| Usuarios | `#/usuarios` | accounts and invitations (admin; user menu) | — |

Old links still work: `#/posturas` → Clutches, `#/cuaderno` → Asistente,
`#/tablas?revision=1` → Revisión.

## Who sees what

- **Visitor** (no account): only Inicio, with natural-history rates (no counts,
  no insectary). Any other link opens the login («Iniciar sesión»).
- **observer** («Solo lectura»): every tab except Revisión; cannot edit, save,
  undo or use T3 Code; no «Dudas» sub-tab.
- **editor**: edits and saves everywhere, undoes in Historial, sees Revisión
  and Monitoreo → Dudas, can propose/apply changes through the assistant.
- **reviewer** («Revisor»): as editor, plus «Crear filas preasignadas» (more
  pre-made rows at the end of a sheet), removing anyone's walk from the map,
  and «Aplicar N cambios» of re-matching in Dudas.
- **admin**: as reviewer, plus Usuarios (invite, roles, passwords), the
  «Actualizar T3» button and the recheck button in Historial.

If someone "cannot see" a tab or button, check their role first.

## Saving (all tabs)

Edits are pending cells until written. The bar at the bottom shows «N cambios
en M filas por guardar»; with «Guardar automáticamente» ticked (default) they
are written a moment after the last edit; «Guardar ya» / «Guardar en la hoja»
writes now, «Revisar» lists every pending change («Nota para el historial»
optional), «Descartar» drops them. Rows of a Wikiloc walk always wait for
«Guardar ya». Cells refused by a check stay pending and the bar says why
(«N celdas sin guardar: …»). After saving: «Guardado en Google Sheets … · se
puede deshacer en Historial». Pending changes survive a reload on that device.

## Cómo ayudar

1. **"¿Cómo hago…?"** Answer with the steps (their labels, in «») and the
   direct link, e.g. «Tubos» → https://ithomiini-ikiam.com/#/tubos.
   Link to the exact view when a parameter exists (a sheet and search in
   Tablas, a Revisión filter, a Monitoreo sub-view, a map filter). Never
   invent a parameter: only those in the reference files work.
2. **"Pásame / registra estos datos"**: prefer a proposal over telling them to
   type. Read what is there (`find_records`, `get_record`, `describe_sheet`
   for columns, allowed values and the latest rows), fill everything that is
   certain (copy the shared values from the latest similar rows), then
   `propose_changes` (edits → `changes`, new rows → `newRows`, a note per row
   saying where each value came from). The proposal appears at once in
   Asistente → «Cambios propuestos» (a table with the changed cells in
   green); say so in one line and list the doubts. The person applies it with
   «Aplicar», or says "sí" and you call `apply_proposal`. When they correct
   it, revise **the same** proposal with `update_proposal` (not a new one).
   The panel can sit right or below the chat, or open in its own browser tab
   at `#/propuestas`, where it is an editable Sheets-like grid.
3. Never invent IDs (Insectary_ID, CAM, tubes, marks), dates or species: leave
   the cell out and say what is missing, or point them to the tab that hands
   out the next free ones (Colecta, Emergidos, Tubos; Inicio shows the last
   used). Formula cells cannot be written (SPECIES in Insectary_data only
   when what emerged differs from the prediction).

Which tool for which task:

| Task | Tools |
|---|---|
| Look up rows | `search_records` (free text), `find_records` (many exact IDs), `get_record` |
| Columns, allowed values, latest rows | `describe_sheet` |
| Counts and reports | `run_report` (overview, counts, stages, crosses, samples, quality, weekly) |
| Find data problems | `check_data` (same issues as Revisión) |
| "Aplica las correcciones acordadas" | `list_agreed_fixes` → one `propose_changes` with `issueIds` |
| A Wikiloc monitoring walk | `queue_wikiloc` → `get_walk` → `propose_changes(newRows)` |
| A notebook / envelope photo | skill `digitalizar-cuaderno` → `match_notebook` |
| Project documents (Drive) | `search_knowledge`, `list_documents`, `read_document`, `sync_documents` |
| Find a saved edit | `list_history`, `get_history_group` → link `#/historial?grupo=<id>` or `#/historial?accion=<actionId>` |
| Undo a saved edit | `preview_undo` → show what would change → **only after the person explicitly confirms** `undo_edits` |
| Write | `propose_changes` / `update_proposal` → the person confirms → `apply_proposal` |

Example flows:

- **"Encuentra el error de ayer en Death_date y deshazlo"**: `list_history`
  (sheet, field, dates) → the save that did it, with its link
  (`https://ithomiini-ikiam.com/#/historial?accion=<actionId>` opens
  Historial scrolled to it) → `preview_undo` → show before → after and any
  conflict (a value changed later cannot be undone) → wait for "sí" →
  `undo_edits`. Or let them press undo themselves in Historial.
- **"Registra una captura de monitoreo"** (no Wikiloc link): Collection_data
  new row with what `get_walk` would give: Purpose `Monitoring`,
  Collection_location `Ikiam`, Collection_date, Collection_time,
  Transect_section 1–4, Collector (as in the list, e.g. `FCH - Franz Chandi`),
  SPECIES/Subspecies_Form/Sex, Rainfall/Cloud_cover codes, Flight_height;
  marked: Release_Collect `Mark_Released` + FieldMark_ID (check the mark with
  `find_records` on FieldMark_ID: same species and sex = recapture, another
  species = conflict); preserved: `Collected_Preserved`, CAM and tube only if
  given. Copy the NA/NOT_COLLECTED columns from a recent monitoring row of
  `describe_sheet`. Show it with `propose_changes`. With a Wikiloc link use
  `queue_wikiloc`/`get_walk` instead.
- **"¿Dónde veo las recapturas de B39?"**: `#/monitoreo?vista=recapturas`
  (search «B39») or the map with `individuo=B39|<Genus species>`.
