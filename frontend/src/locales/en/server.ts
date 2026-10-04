// Messages that come from the server (errors, notices), shown through errorText.
// Keyed by the server's exact Spanish message (server/*.mjs). Messages with a
// variable part (e.g. «Valor no válido en Sex») come with a descriptor and
// their templates are in server-built.ts; most other server messages are
// already in English.
export default {
  // server/batch.mjs
  'Origen de escritura desconocido': 'Unknown write origin',
  'Este ID de solicitud pertenece a otra persona': 'This request ID belongs to another person',
  'El intento anterior aún se está confirmando; vuelve a intentarlo en un momento':
    'The previous attempt is still being confirmed; try again in a moment',
  'No hay nada que guardar': 'There is nothing to save',
  'No se pudo confirmar la escritura en Google Sheets': 'The write to Google Sheets could not be confirmed',
  'No se pudo verificar la escritura en Google Sheets': 'The write to Google Sheets could not be verified',
  'Algunos cambios necesitan revisión; no se guardó nada': 'Some changes need review; nothing was saved',
  // server/grid.mjs, server/index.mjs, server/schema.mjs
  'El ID inicial debe ser como CAM078277 o FS00001234': 'The first ID must look like CAM078277 or FS00001234',
  'Hoja desconocida': 'Unknown sheet',
  // server/search.mjs
  'Rango de filas no válido': 'Invalid range of rows',
  'Los valores deben ser un objeto': 'The values must be an object',
  // server/history.mjs
  'No se encontró ese guardado en el historial': 'That save was not found in the history',
  'Elige al menos un cambio para deshacer': 'Choose at least one change to undo',
  'Un guardado elegido no existe': 'A chosen save does not exist',
  'Solo se deshacen guardados confirmados en Google Sheets': 'Only saves confirmed in Google Sheets can be undone',
  'Esos cambios ya están deshechos': 'Those changes are already undone',
  // server/insectaryId.mjs
  'Escribe el Insectary ID actual y el correcto': 'Enter the current Insectary ID and the correct one',
  'El ID nuevo es igual al actual': 'The new ID is the same as the current one',
  // server/premade.mjs
  'No quedan filas con fórmulas al final de Insectary_data: pide a PAS que añada filas':
    'No rows with formulas are left at the end of Insectary_data: ask PAS to add rows',
  'Insectary_data no tiene Insectary IDs que continuar': 'Insectary_data has no Insectary IDs to continue',
  'La columna Insectary_ID está protegida para la cuenta de la app: pide a PAS que escriba los IDs':
    'The Insectary_ID column is protected for the app’s account: ask PAS to write the IDs',
  // server/knowledge.mjs, server/t3admin.mjs
  'No se pudo iniciar la sincronización con Drive': 'The sync with Drive could not be started',
  'No se pudo iniciar la actualización de T3': 'The T3 update could not be started',
} as Record<string, string>
