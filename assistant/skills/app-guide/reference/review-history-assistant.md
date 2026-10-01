# Inicio, Historial, Asistente, Revisión, accounts

Base URL: `https://ithomiini-ikiam.com/`.

## Inicio — `#/inicio` (also `/`)

The only page open without an account.

- Everyone: «Mariposas Ithomiini · Ikiam» facts (species, places, altitudes,
  records since) and «Historia natural» charts: life cycle days per species,
  time of day, months, weather, where to find each species. Rates and
  proportions only, never counts.
- Signed in, first: «Últimos IDs usados» (CAMID insectario, CAMID colecta,
  Marca de monitoreo, «Siguiente Insectary ID», «Siguiente clutch») and
  «Próximos días en el insectario» (Huevos por eclosionar, Larvas por pupar,
  Pupas por emerger, and late ones). Then the insectary now (Mariposas vivas,
  Clutches en curso, eggs/larvae/pupae, last 30 days), «Monitoreo en Ikiam»
  (link to the Reporte) and «Colectas».
- Signed in, when there is something to say: «Alertas» (a CAM range running
  out, a species that reached 30 preserved, species close to it), with «Ver
  rangos de CAM y la regla de los 30» → `#/revision?vista=alertas`.

## The save bar and «Revisar»

See SKILL.md "Saving". «Revisar» opens «Revisar cambios antes de guardar»:
every pending cell (old → new; the × on a cell drops that change, «quitar»
drops a new row), refused cells with the reason, and «Nota para el
historial (opcional)» (e.g. "ronda del lunes"), which appears as the note of
the save in Historial. Untick «Guardar automáticamente» to save only by hand.
Signing out keeps unsaved changes on that device for next time.

## Historial — `#/historial`

Every write to the workbook, newest first: from the app, edits detected in
Google Sheets itself, undos, the assistant and imports.

- Filters: «Buscar (ID, campo, valor o nota)» (e.g. `N4D`, `Death_date`,
  `FS0001`), «Persona», «Origen» (Aplicación, Google Sheets, Deshacer,
  Asistente, Importación), «Hoja», «Desde», «Hasta» → «Buscar»; «Cargar más».
- Each save is grouped (who, when, origin, rows, the note, status: Guardado,
  Detectado = made in Google Sheets, En curso, Sin confirmar, No guardado;
  «deshecho» when already undone). Open it to see each cell: sheet and row,
  label, column, old value struck through → new value.
- **Undo** (editors): select the saves (only «Guardado» ones), untick single
  cells if needed → «Deshacer selección» → a preview «Deshacer N cambios»
  (a cell changed again afterwards is a conflict: take it out or fix by hand)
  → «Motivo (opcional)» → «Deshacer en la hoja». The undo is itself a save
  (origin Deshacer) and can be undone.
- Admins: a recheck button re-verifies writes left «Sin confirmar».

Deep links: `#/historial?grupo=<groupId>` opens and scrolls to one group of
saves, `#/historial?accion=<actionId>` to one save. Get the ids with
`list_history` / `get_history_group`; `preview_undo` shows what an undo
would write and its conflicts; `undo_edits` undoes, **only after the person
explicitly confirms** (same as `apply_proposal`). Example:
`https://ithomiini-ikiam.com/#/historial?accion=<actionId>`.

## Asistente — `#/asistente`

T3 Code (editors): this assistant, full screen, with the person's own
workspace and the `ithomiini` tools; the app has no other chat.

- **The bar** above T3: «Cambios propuestos (N)» (show/hide the panel; it
  opens by itself and pulses when a proposal arrives), reconnect, open T3 in
  another tab; admins see the T3 version and, when a newer release exists,
  «Actualizar T3 (x → y)» (restarts T3; open chats are cut, saved chats are
  kept).
- **Cambios propuestos** (beside T3, below it on phones): each proposal as a
  table like the sheet: «Fila», the
  changed cells in green with the old value struck through, new rows marked
  «nueva», «Motivo» per row. The person selects cells and presses «Valor de
  la hoja» (back to the sheet's value, or empty in a new row: the AI value
  stays aside, dashed and struck through, not written) or «Valor de la IA»
  (the AI value again). Cells the AI is unsure of are amber and dashed with a
  «?» («N celdas dudosas por revisar» in the header; a click goes to the
  next); the cell bar shows why and the other readings to pick. Editing,
  picking a reading, «Valor de la hoja»/«de la IA» or «Marcar revisadas»
  reviews them; values the line does not write are in italics. «Aplicar N
  filas» writes what the table shows as one save (undoable in Historial);
  with unreviewed doubtful cells it asks first («Revisarlas», «Aplicar sin
  las dudosas», «Aplicar todo igualmente», «Cancelar»); a row with every cell
  set back is skipped;
  «Descartar» drops it; "sí, aplícalo" in the chat does the same through
  `apply_proposal`. «Revisados hace poco (N)» keeps the last five. The panel
  can be placed right or bottom, or opened alone in its own browser tab at
  `#/propuestas`; there it is an editable Sheets-like grid (the person can
  correct a cell before applying). When the person asks for a change to a
  proposal, revise it with `update_proposal`. It shows the proposals of the
  chat open in T3 beside it («Este chat: …»; a new chat not sent yet has none;
  «Último chat: …» when T3 does not say which is open), and «Cambios
  propuestos (N)» counts those; a selector above the tables picks another
  chat with proposals, «Fuera de los chats de T3» (Revisión, and the app's
  former simple chat) or «Todos los chats»; «N propuestas más en otros chats»
  shows all of them. Opening another chat in T3 follows it again.

## Revisión — `#/revision` (editors; last tab)

Four views along the top: «Problemas» (below), «Sugerencias», «Resueltos»
and «Alertas» (`vista=sugerencias|resueltos|alertas`).

- «Sugerencias»: corrections the app computes, read only (no apply button):
  grouped by source (Arreglos de los chequeos, Espacios de más, Fórmulas que
  faltan, Fechas imposibles, Tubos con un dígito de más o de menos, Colecta e
  insectario no coinciden, Pedigree sin decidir), each with «Seguro» /
  «Probable» / «Revisar», the row (link to Tablas), now → suggested («decidir»
  when a person must choose), the reason, «en Google Sheets» for formula cells,
  and since when. Filters: certainty, source, «Hoja», search; «CSV» downloads
  and «Copiar» copies the filtered list. Parameters: `fuente=<source id>`,
  `certeza=certain|likely|check`, `hoja=`, `q=`. To make the changes, ask the
  assistant for a proposal with the chosen ones.
- «Resueltos»: problems and suggestions the sheet no longer has, newest first:
  since when it was seen, when it was solved, who (or Google Sheets, or not in
  the history), the change (before → after) and «ver en Historial».
  Parameters: `tipo=check|suggestion`, `q=`.
- «Alertas»: the alerts, «Rangos de CAM» (per pool of Lists: range, size,
  used, highest, next, left, gaps, last use) and «Regla de los 30
  preservados» (species that reached 30, the day, those preserved after;
  species close to it).

«Problemas»: every inconsistency of the workbook and of the specimen photos as a card,
judged by people. Same kinds as `check_data` (sidebar groups «Datos de la
hoja» and «Fotos y sobres», with counts).

- Status buttons: Pendiente (default), Aceptado, Otro valor, Rechazado,
  Aplicado, Todos. Filters: kind, «Hoja», «Colector o identificador»,
  «Desde»/«Hasta», search «Buscar CAM, ID, especie…», order «Más recientes
  primero» / «Más antiguas primero» / «Por tipo y hoja». 25 cards per page.
- A card: the problem, the rows involved (row link opens it in Tablas), for
  photo issues the envelope crop («girar»), photos, «Sobre dice» / «Hoja
  dice», CAM read, envelope text, earlier curation, AI prediction, strength
  (fuerte/media/baja/dudosa). Buttons: «Aceptar arreglo» (or «Aceptar tarea»
  / «Es un problema»), «Rechazar», «Otro valor» (type the right value →
  «Guardar»), back to pending, «historial» of verdicts. Batches («Lote «…»:
  N», e.g. a day of envelopes) can be judged together or shown alone («ver
  solo el lote»).
- When fixes are accepted a green bar says «N arreglos aceptados listos para
  aplicar» (and Drive tasks): ask T3 «aplica las correcciones acordadas»
  (`list_agreed_fixes` → one `propose_changes` with `issueIds`) or press
  «Preparar propuesta aquí» (then confirm in Asistente → Cambios propuestos,
  under «Fuera de los chats de T3»).
- The download button exports the photo verdicts as training labels.

Parameters (only non-default ones appear in the link): `tipo=<kind>` (repeat,
cam_cross, list, insectary_link, link_mismatch, date_order, future_date,
bad_date, missing_sample, mark_reuse, walk_doubt, photo_camid, photo_extra,
envelope_sex, envelope_species, photo_missing, ai_species), `hoja=<sheet>`,
`persona=<name>`, `desde=YYYY-MM-DD`, `hasta=YYYY-MM-DD`,
`estado=accepted|other|rejected|applied|all` (default pending),
`lote=<batch key>`, `q=<text>`, `orden=old|kind` (default recent). Example:
`https://ithomiini-ikiam.com/#/revision?tipo=envelope_sex&hoja=Collection_data&orden=old`

## Accounts

- Login: `#/entrar` («Usuario», «Contraseña»); `?volver=<route>` returns
  there after login. The very first setup asks a «Código de configuración».
- Invitations: an admin sends one from Usuarios; the email link
  (`#/activar?t=…`, never share or invent tokens) lets the person choose
  «Usuario» (3–64 letters, numbers, dots, dashes), «Nombre» and a password of
  6–16 characters.
- Usuarios — `#/usuarios` (admin; user menu top right): invite by «Correo»,
  «Nombre», «Permiso» (Solo lectura, Editor, Revisor, Administrador) →
  «Enviar invitación»; pending invitations (copy link, send again, revoke);
  users with role, active, «Cambiar contraseña»; «Crear una cuenta sin
  correo (con contraseña inicial)».
- The user menu also has «Abrir Google Sheet» (phones) and «Cerrar sesión».
