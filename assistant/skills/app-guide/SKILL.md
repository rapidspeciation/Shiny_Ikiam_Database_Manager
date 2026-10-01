---
name: app-guide
description: Guide to the "Ikiam Insectary DB" web app (https://ithomiini-ikiam.com) — every tab (Inicio, Buscador, Colecta, Monitoreo, Muertes, Tubos, Emergidos, Clutches, Historial, Asistente, Revisión, Usuarios), what each is for, who can use it, its controls and workflows, grid and date tips, which sheet it writes, and deep links with query parameters. Use it whenever the person asks how to do something in the app, where something is, why a button or tab is missing, or asks for a link.
---

# App guide: Ikiam Insectary DB

The team's web app over the team's Google Sheets workbook: saves go straight
into it (the «Google Sheet» button at the top right opens it).

- **Links**: every page is a hash route; full link =
  `https://ithomiini-ikiam.com/` + route, e.g.
  `https://ithomiini-ikiam.com/#/monitoreo?vista=dudas`.
- **Labels**: the interface is in English by default and in Spanish with the
  EN/ES button at the top. The labels here are the Spanish ones, in «»; for a
  person using the English interface, give the control's meaning too.

## Reference files

Read the tab's file before giving steps for it (every control, every link
parameter):

| File | Tabs |
|---|---|
| [reference/data-entry.md](reference/data-entry.md) | Buscador, Colecta, Muertes, Tubos, Emergidos, Clutches; grid, keyboard and date tips |
| [reference/monitoreo.md](reference/monitoreo.md) | Monitoreo: Importar recorrido, Reporte, Mapa, Recapturas, Dudas de emparejamiento, Datos de Wikiloc |
| [reference/review-history-assistant.md](reference/review-history-assistant.md) | Inicio, Asistente (T3 Code, Cambios propuestos, Instrucciones de la IA), Revisión, Usuarios, login and invitations, the save bar |

Historial: skill **historial**.

## Tabs at a glance

| Tab | Route | For | Writes to |
|---|---|---|---|
| Inicio | `#/inicio` | summaries; for the team: last IDs used, next Insectary ID and clutch, upcoming hatch/pupa/emergence | — |
| Buscador | `#/tablas?hoja=…&buscar=…` | any sheet as an editable spreadsheet | the chosen sheet |
| Colecta | `#/colecta` | a day of field collection in bulk | Collection_data (+ Insectary_data for live ones) |
| Monitoreo | `#/monitoreo?vista=…` | Ikiam transects T1–T4: Wikiloc walks, report, map, recaptures, doubtful pairings | Collection_data, SamplingDay_data |
| Muertes | `#/muertes` | death date and cause of insectary butterflies | Insectary_data |
| Tubos | `#/tubos` | CAM IDs, tubes, tissue, medium; tube labels | Insectary_data |
| Emergidos | `#/emergidos` | new adults of a clutch into pre-made rows | Insectary_data |
| Clutches | `#/clutches` | new clutches and their follow-up | Insectary_stocks |
| Historial | `#/historial` | every saved change; selective undo | (undo writes back) |
| Asistente | `#/asistente` | T3 Code (this assistant), Cambios propuestos; «Instrucciones de la IA» (`#/instrucciones`) | via proposals |
| Revisión | `#/revision?…` | data problems as cards to judge | verdicts (app); fixes via a proposal |
| Usuarios | `#/usuarios` | accounts and invitations (admin; user menu) | — |

Old links still work: `#/posturas` → Clutches, `#/cuaderno` → Asistente,
`#/tablas?revision=1` → Revisión.

## Who sees what

| Role | Can |
|---|---|
| Visitor (no account) | only Inicio, with natural-history rates (no counts, no insectary); any other link opens the login («Iniciar sesión») |
| observer («Solo lectura») | every tab except Revisión; cannot edit, save, undo or use T3 Code; no «Dudas» sub-tab |
| editor | edit and save everywhere, undo in Historial, Revisión and Monitoreo → Dudas, propose and apply changes through the assistant |
| reviewer («Revisor») | as editor, plus «Crear filas preasignadas» (more pre-made rows at the end of a sheet), removing anyone's walk from the map, and «Aplicar N cambios» of re-matching in Dudas |
| admin | as reviewer, plus Usuarios (invite, roles, passwords), the «Actualizar T3» button and the recheck button in Historial |

## Saving (all tabs)

- Edits are pending cells until written. The bar at the bottom shows «N
  cambios en M filas por guardar».
- With «Guardar automáticamente» ticked (default) they are written a moment
  after the last edit; «Guardar ya» / «Guardar en la hoja» writes now.
- «Revisar» lists every pending change («Nota para el historial» optional);
  «Descartar» drops them.
- Rows of a Wikiloc walk always wait for «Guardar ya».
- Cells refused by a check stay pending and the bar says why («N celdas sin
  guardar: …»).
- After saving: «Guardado en Google Sheets … · se puede deshacer en
  Historial». Pending changes survive a reload on that device.

## How to help

1. **"¿Cómo hago…?"**: the steps (their labels, in «») and the direct link,
   e.g. «Tubos» → https://ithomiini-ikiam.com/#/tubos. Link to the exact view
   when a parameter exists (a sheet and search in Buscador, a Revisión filter,
   a Monitoreo sub-view, a map filter). The link parameters that exist are the
   ones in the reference files.
2. **"Pásame / registra estos datos"**: rather than telling them to type,
   draft a proposal with everything certain (shared values can be copied from
   the latest similar rows).
3. **New IDs, CAMs, tubes and marks** are handed out by the tabs (Colecta,
   Emergidos, Tubos; Inicio shows the last used): point there when the person
   needs the next free ones.
4. **"I cannot see a tab or button"**: check their role (above) first.
