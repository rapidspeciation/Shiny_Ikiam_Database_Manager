// Google Sheets busy (the banner, saves waiting for it) and the Emergidos and
// Clutches entries kept in the app until «Guardar en Google Sheets».
export default {
  // lib/staged.ts: the banner (components/GoogleBanner.vue)
  'Google Sheets no responde (está recalculando la hoja): los guardados se conservan aquí y se escriben cuando responda; {n} esperando':
    'The Google Sheet is busy (recalculating): saves are kept here and written when it answers; {n} waiting',
  'Google Sheets responde lento (está recalculando la hoja): los guardados se conservan aquí y se escriben en orden; {n} esperando':
    'The Google Sheet is slow (recalculating): saves are kept here and written in order; {n} waiting',
  'Escribiendo en Google Sheets {n} guardado que esperaba': 'Writing {n} save that was waiting to the Google Sheet',
  'Escribiendo en Google Sheets {n} guardados que esperaban': 'Writing {n} saves that were waiting to the Google Sheet',
  'simulado (laboratorio)': 'simulated (lab)',
  '{name}, en la app (aún no en Google Sheets)': '{name}, in the app (not in Google Sheets yet)',
  // components/SaveBar.vue, components/SheetGrid.vue
  'Google Sheets no responde (recalcula la hoja); se escriben solos, en orden, cuando responda':
    'Google Sheets is not answering (the sheet is recalculating); they are written on their own, in order, when it answers',
  'Google Sheets no responde: {n} cambio espera y se escribirá solo': 'Google Sheets is not answering: {n} change waits and will be written on its own',
  'Google Sheets no responde: {n} cambios esperan y se escribirán solos':
    'Google Sheets is not answering: {n} changes wait and will be written on their own',
  '{n} cambio guardado en la app; falta «Guardar en Google Sheets»': '{n} change saved in the app; «Save to Google Sheets» is still to do',
  '{n} cambios guardados en la app; falta «Guardar en Google Sheets»': '{n} changes saved in the app; «Save to Google Sheets» is still to do',
  '{n} esperando a Google Sheets': '{n} waiting for Google Sheets',
  'Guardado en la app ({n} cambio), visible para todo el equipo · aún no en Google Sheets':
    'Saved in the app ({n} change), visible to the whole team · not in Google Sheets yet',
  'Guardado en la app ({n} cambios), visible para todo el equipo · aún no en Google Sheets':
    'Saved in the app ({n} changes), visible to the whole team · not in Google Sheets yet',
  'Escribiéndose en Google Sheets · {who}': 'Being written to Google Sheets · {who}',
  'Aún no en Google Sheets · {who}': 'Not in Google Sheets yet · {who}',
  'Esperando a que Google Sheets responda; se escribe solo': 'Waiting for Google Sheets to answer; it is written on its own',
  'Fila nueva aún no en Google Sheets · {who}': 'New row not in Google Sheets yet · {who}',
  // components/StagedBar.vue
  '{n} cambio en la app, aún no en Google Sheets': '{n} change in the app, not in Google Sheets yet',
  '{n} cambios en la app, aún no en Google Sheets': '{n} changes in the app, not in Google Sheets yet',
  'No hay cambios por guardar': 'No changes to save',
  'Google Sheets no responde: los cambios esperan en la app y se escribirán solos cuando responda':
    'Google Sheets is not answering: the changes wait in the app and will be written on their own when it answers',
  'No se guardó': 'Not saved',
  Ver: 'Show',
  'Guardar en Google Sheets': 'Save to Google Sheets',
  'Cambio deshecho (no llegó a Google Sheets)': 'Change undone (it never reached Google Sheets)',
  'Cambios deshechos (no llegaron a Google Sheets)': 'Changes undone (they never reached Google Sheets)',
  'Guardado en Google Sheets; {n} fila necesita revisión (marcada en rojo)': 'Saved to Google Sheets; {n} row needs review (marked in red)',
  'Guardado en Google Sheets; {n} filas necesitan revisión (marcadas en rojo)': 'Saved to Google Sheets; {n} rows need review (marked in red)',
  '{n} esperando a que Google Sheets responda': '{n} waiting for Google Sheets to answer',
  'Escribiendo {n} cambio en Google Sheets…': 'Writing {n} change to Google Sheets…',
  'Escribiendo {n} cambios en Google Sheets…': 'Writing {n} changes to Google Sheets…',
  'Guardar en Google Sheets ({n} cambio)': 'Save to Google Sheets ({n} change)',
  'Guardar en Google Sheets ({n} cambios)': 'Save to Google Sheets ({n} changes)',
  'Se escribe {n} cambio de Emergidos y Clutches, de todo el equipo, en una sola vez. Si Google Sheets está ocupado, espera en la app y se escribe solo.':
    "{n} change from Emergidos and Clutches, the whole team's, is written at once. If Google Sheets is busy, it waits in the app and is written on its own.",
  'Se escriben {n} cambios de Emergidos y Clutches, de todo el equipo, en una sola vez. Si Google Sheets está ocupado, esperan en la app y se escriben solos.':
    "{n} changes from Emergidos and Clutches, the whole team's, are written at once. If Google Sheets is busy, they wait in the app and are written on their own.",
  // Emergidos and Clutches
  'Clutch {clutch} guardado en la app y revisado': 'Clutch {clutch} saved in the app and checked',
  escribiéndose: 'being written',
  'en la app': 'in the app',
  'Clutch {clutch} añadido en la app (aún no en Google Sheets)': 'Clutch {clutch} added in the app (not in Google Sheets yet)',
  'en la app, aún no en Google Sheets': 'in the app, not in Google Sheets yet',
  '{id} ya lo tiene {name} en la app (aún no en Google Sheets)': '{id} is taken by {name} in the app (not in Google Sheets yet)',
  '{value} ya lo tiene {name} en la app (aún no en Google Sheets)': '{value} is taken by {name} in the app (not in Google Sheets yet)',
  '{n} emergido guardado en la app': '{n} emerged butterfly saved in the app',
  '{n} emergidos guardados en la app': '{n} emerged butterflies saved in the app',
  '{n} emergido deshecho': '{n} emerged butterfly undone',
  '{n} emergidos deshechos': '{n} emerged butterflies undone',
  '{n} emergido guardado en la app (aún no en Google Sheets)': '{n} emerged butterfly saved in the app (not in Google Sheets yet)',
  '{n} emergidos guardados en la app (aún no en Google Sheets)': '{n} emerged butterflies saved in the app (not in Google Sheets yet)',
  // server/staged.mjs, server/outbox.mjs (plain messages)
  'Solo Emergidos y Clutches guardan en la app': 'Only Emergidos and Clutches save in the app',
  'Esa fila ya no está entre los cambios sin guardar (se guardó o se deshizo)': 'That row is no longer among the unsaved changes (it was saved or undone)',
  'Esa fila se está escribiendo en Google Sheets; cámbiala cuando esté guardada': 'That row is being written to Google Sheets; change it once it is saved',
  'Ese cambio ya no está entre los cambios sin guardar': 'That change is no longer among the unsaved changes',
  'Se está escribiendo en Google Sheets; deshazlo desde Historial cuando esté guardado': 'It is being written to Google Sheets; undo it from Historial once it is saved',
}
