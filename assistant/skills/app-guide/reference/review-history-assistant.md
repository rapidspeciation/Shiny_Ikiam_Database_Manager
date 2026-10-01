# Inicio, Asistente, Revisión, accounts

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

Every save, with selective undo: skill **historial**.

## Asistente — `#/asistente`

T3 Code (editors): this assistant, full screen, with the person's own
workspace and the `ithomiini` tools; the app has no other chat.

### The bar above T3

- «Instrucciones de la IA» opens `#/instrucciones` (anyone signed in): the
  brief, skills, subagents and tools this assistant follows, each with its
  change history (link one file with `?archivo=<path>`, e.g.
  `#/instrucciones?archivo=assistant/skills/monitoring/SKILL.md`).
- «Cambios propuestos (N)» shows or hides the panel; it opens by itself and
  pulses when a proposal arrives.
- Reconnect T3; open T3 in another tab.
- Admins: the T3 version and, when a newer release exists, «Actualizar T3
  (x → y)» (restarts T3; open chats are cut, saved chats are kept).

### Cambios propuestos

Beside T3 (below it on phones); placed right or bottom, or alone in its own
browser tab at `#/propuestas`, an editable Sheets-like grid where the person
can correct a cell before applying.

- **Each proposal** is a table like the sheet: «Fila», the changed cells in
  green with the old value struck through, new rows marked «nueva», «Motivo»
  per row; values the line does not write are in italics.
- **Choosing values**: select cells and press «Valor de la hoja» (back to the
  sheet's value, or empty in a new row: the AI value stays aside, dashed and
  struck through, not written) or «Valor de la IA» (the AI value again).
- **Doubtful cells** (the AI is unsure) are amber and dashed with a «?»; the
  header says «N celdas dudosas por revisar» (a click goes to the next) and
  the cell bar shows why and the other readings to pick. Editing, picking a
  reading, «Valor de la hoja» / «de la IA» or «Marcar revisadas» reviews them.
- **«Aplicar N filas»** writes what the table shows as one save (undoable in
  Historial); a row with every cell set back is skipped. With unreviewed
  doubtful cells it asks first («Revisarlas», «Aplicar sin las dudosas»,
  «Aplicar todo igualmente», «Cancelar»). «Descartar» drops the proposal;
  «Revisados hace poco (N)» keeps the last five.
- **Which proposals**: those of the chat open in T3 beside it («Este chat:
  …»; a new chat not sent yet has none; «Último chat: …» when T3 does not say
  which is open), counted in «Cambios propuestos (N)». A selector above the
  tables picks another chat with proposals, «Fuera de los chats de T3» (e.g.
  prepared in Revisión) or «Todos los chats»; «N propuestas más en otros
  chats» shows all of them. Opening another chat in T3 follows it again.

## Revisión — `#/revision` (editors; last tab)

Four views along the top: «Problemas» (default), «Sugerencias», «Resueltos»
and «Alertas» (`vista=sugerencias|resueltos|alertas`).

### Problemas

Every inconsistency of the workbook and of the specimen photos as a card,
judged by people. Same kinds as `check_data` (sidebar groups «Datos de la
hoja» and «Fotos y sobres», with counts).

- **Status buttons**: Pendiente (default), Aceptado, Otro valor, Rechazado,
  Aplicado, Todos.
- **Filters**: kind, «Hoja», «Colector o identificador», «Desde»/«Hasta»,
  search «Buscar CAM, ID, especie…», order «Más recientes primero» / «Más
  antiguas primero» / «Por tipo y hoja». 25 cards per page.
- **A card**: the problem, the rows involved (a row link opens it in Tablas);
  for photo issues the envelope crop («girar»), photos, «Sobre dice» / «Hoja
  dice», CAM read, envelope text, earlier curation, AI prediction, strength
  (fuerte/media/baja/dudosa).
- **Buttons**: «Aceptar arreglo» (or «Aceptar tarea» / «Es un problema»),
  «Rechazar», «Otro valor» (type the right value → «Guardar»), back to
  pending, «historial» of verdicts. Batches («Lote «…»: N», e.g. a day of
  envelopes) can be judged together or shown alone («ver solo el lote»).
- **Accepted fixes**: a green bar says «N arreglos aceptados listos para
  aplicar» (and Drive tasks): ask the assistant «aplica las correcciones
  acordadas», or press «Preparar propuesta aquí» (then confirm in Asistente →
  Cambios propuestos, under «Fuera de los chats de T3»).
- The download button exports the photo verdicts as training labels.

Parameters (only non-default ones appear in the link): `tipo=<kind>` (repeat,
cam_cross, list, insectary_link, link_mismatch, date_order, future_date,
bad_date, missing_sample, mark_reuse, walk_doubt, photo_camid, photo_extra,
envelope_sex, envelope_species, photo_missing, ai_species), `hoja=<sheet>`,
`persona=<name>`, `desde=YYYY-MM-DD`, `hasta=YYYY-MM-DD`,
`estado=accepted|other|rejected|applied|all` (default pending),
`lote=<batch key>`, `q=<text>`, `orden=old|kind` (default recent). Example:
`https://ithomiini-ikiam.com/#/revision?tipo=envelope_sex&hoja=Collection_data&orden=old`

### Sugerencias

Corrections the app computes; read only (no apply button: to make them, ask
the assistant for a proposal with the chosen ones).

- Grouped by source (Arreglos de los chequeos, Espacios de más, Fórmulas que
  faltan, Fechas imposibles, Tubos con un dígito de más o de menos, Colecta e
  insectario no coinciden, Pedigree sin decidir).
- Each: «Seguro» / «Probable» / «Revisar», the row (link to Tablas), now →
  suggested («decidir» when a person must choose), the reason, «en Google
  Sheets» for formula cells, and since when.
- Filters: certainty, source, «Hoja», search; «CSV» downloads and «Copiar»
  copies the filtered list.

Parameters: `fuente=<source id>`, `certeza=certain|likely|check`, `hoja=`, `q=`.

### Resueltos

Problems and suggestions the sheet no longer has, newest first: since when it
was seen, when it was solved, who (or Google Sheets, or not in the history),
the change (before → after) and «ver en Historial». Parameters:
`tipo=check|suggestion`, `q=`.

### Alertas

The alerts, «Rangos de CAM» (per pool of Lists: range, size, used, highest,
next, left, gaps, last use) and «Regla de los 30 preservados» (species that
reached 30, the day, those preserved after; species close to it).

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
