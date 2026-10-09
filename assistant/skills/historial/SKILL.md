---
name: historial
description: The app's Historial tab — every save to the workbook (from the app's tabs, typed directly in Google Sheets, the assistant, undos, imports), grouped by person, purpose and time, with selective undo. Use it when the person asks who changed something or when, or for the history of a butterfly, clutch or row, made a mistake when saving ("me equivoqué al guardar…", "¿quién cambió el sexo de 5VB?"), wants a save found, linked or undone, or asks how to undo in the app.
---

# Historial: finding and undoing a save

- The history of one butterfly, clutch or row ("¿qué cambios ha tenido
  D5D?", "¿quién cambió el sexo de 5VB?"): `row_history`.
- Otherwise:
  1. Find the save with `list_history` from what the person remembers (who,
     when, which tab, an ID or value).
  2. Go through its cells with them (`get_history_group`), with the save's
     link.
- Undo: the person can do it themselves at the save's link, or you do it
  from the chat: `preview_undo` → show it and ask → `undo_edits` on their
  yes. Either way the undo is itself a save that can be undone.

## The tab (`https://ithomiini-ikiam.com/#/historial`)

- **Links**: `#/historial?grupo=<groupId>` opens and scrolls to a group of
  saves, `#/historial?accion=<actionId>` to one save.
- **Filters**: «Buscar (ID, campo, valor o nota)», «Persona», «Origen»
  (Aplicación, Google Sheets, Deshacer, Asistente, Importación), «Hoja»,
  «Desde», «Hasta» → «Buscar»; «Cargar más».
- **A card**: who, when, origin, rows, the save's note and its status
  (Guardado, Detectado = made in Google Sheets, En curso, Sin confirmar, No
  guardado; «deshecho» once undone). Opened, each cell: sheet and row, label,
  column, old value struck through → new value.
- **Undo** (editors): select saves (only «Guardado» ones), untick single cells
  if needed → «Deshacer selección» → the preview «Deshacer N cambios» (a cell
  changed again afterwards is a conflict: take it out or fix it by hand) →
  «Motivo (opcional)» → «Deshacer en la hoja».
- **Admins**: a recheck button re-verifies writes left «Sin confirmar».
