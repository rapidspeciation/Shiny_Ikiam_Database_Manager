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

A slim bar switches «T3 Code» and «Chat simple» (remembered per device;
without T3 configured only the chat shows).

- **T3 Code** (editors): this assistant, full screen, with the person's own
  workspace and the `ithomiini` tools. Bar buttons: «Cambios propuestos (N)»
  (show/hide the panel; it opens by itself and pulses when a proposal
  arrives), reconnect, open T3 in another tab; admins see the T3 version and,
  when a newer release exists, «Actualizar T3 (x → y)» (restarts T3; open
  chats are cut, saved chats are kept).
- **Chat simple**: conversations on the left («Nueva conversación», delete),
  a box «Escribe tu pregunta o manda una foto del cuaderno», camera and
  gallery buttons (up to 6 photos, shrunk before upload; Enter sends).
  Answers show record chips (open the row in Tablas), document links (Drive),
  small result tables and proposals. `#/asistente?hilo=<threadId>` opens a
  given conversation in Chat simple.
- **Cambios propuestos** (beside T3, below it on phones; also inside the
  chat messages): each proposal as a table like the sheet: «Fila», the
  changed cells in green with the old value struck through, new rows marked
  «nueva», «Motivo» per row, a tick per row («Elegir todas»). «Aplicar N
  filas» writes the ticked rows as one save (undoable in Historial);
  «Descartar» drops it; "sí, aplícalo" in the chat does the same through
  `apply_proposal`. «Revisados hace poco (N)» keeps the last five. The panel
  can be placed right or bottom, or opened alone in its own browser tab at
  `#/propuestas`; there it is an editable Sheets-like grid (the person can
  correct a cell before applying). When the person asks for a change to a
  proposal, revise it with `update_proposal`.

## Revisión — `#/revision` (editors; last tab)

Every inconsistency of the workbook and of the specimen photos as a card,
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
  «Preparar propuesta aquí» (then confirm in Asistente → Cambios propuestos).
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
