/** The Buscador: its phone layout, a cell's history and the sheet as it was. */
export default {
  // Toolbar and help.
  'Más herramientas': 'More tools',
  'Volver a cargar': 'Reload',
  'Descargar CSV': 'Download CSV',
  'Cómo usar la tabla': 'How to use the table',
  'Historial: los cambios de la celda elegida.': 'History: the changes of the selected cell.',
  'Ocultar la ayuda (vuelve con ⓘ)': 'Hide the help (back with ⓘ)',

  // A cell's history.
  'Historial de esta celda': 'History of this cell',
  'Historial de toda la fila': 'History of the whole row',
  'Historial de esta celda: cada cambio, quién y cuándo': 'History of this cell: every change, who and when',
  'Una fila nueva no tiene historial todavía': 'A new row has no history yet',
  'Historial de la fila': 'Row history',
  'Historial de {field}': 'History of {field}',
  Celda: 'Cell',
  Otro: 'Other',
  'Ningún cambio de esta fila en el historial.': 'No changes to this row in the history.',
  'Ningún cambio de esta celda en el historial.': 'No changes to this cell in the history.',
  'Se muestran los 500 guardados más antiguos.': 'The 500 oldest saves are shown.',
  último: 'latest',
  '({n} cambios)': '({n} changes)',
  'Ver la hoja como estaba justo antes de este cambio': 'See the sheet as it was just before this change',
  'Ver la hoja como quedó justo después de este cambio': 'See the sheet as it was just after this change',
  'Ver la hoja como estaba justo antes de este guardado': 'See the sheet as it was just before this save',
  'Hoja antes': 'Sheet before',
  'Hoja después': 'Sheet after',
  'Abrir este guardado en el Historial (allí se puede deshacer)': 'Open this save in the History (it can be undone there)',
  'En el Historial': 'In History',
  'El historial empieza el {since}; lo escrito directamente en Google Sheets se conoce desde el {sheets}, cuando la app lo leyó (sin saber quién ni los pasos intermedios).':
    'The history starts on {since}; what was typed directly in Google Sheets is known from {sheets}, when the app read it (without who or the steps in between).',

  // The sheet as it was.
  'Volver a la hoja como está ahora': 'Back to the sheet as it is now',
  'Volver a ahora': 'Back to now',
  '{sheet} como estaba el {when}': '{sheet} as on {when}',
  '(antes del guardado de {who} «{summary}»)': "(before {who}'s save «{summary}»)",
  '(después del guardado de {who} «{summary}»)': "(after {who}'s save «{summary}»)",
  'Cargando cómo estaba {sheet}…': 'Loading {sheet} as it was…',
  Antes: 'Before',
  Después: 'After',
  'Cambio anterior de {field}': 'Previous change of {field}',
  'Cambio siguiente de {field}': 'Next change of {field}',
  'cambio {at} de {total} de {field}': 'change {at} of {total} of {field}',
  'Qué se ve aquí': 'What this shows',
  '{n} celda distinta de ahora': '{n} cell differs from now',
  '{n} celdas distintas de ahora': '{n} cells differ from now',
  '{n} celda de este guardado': '{n} cell of this save',
  '{n} celdas de este guardado': '{n} cells of this save',
  '{n} fila aún no creada': '{n} row not created yet',
  '{n} filas aún no creadas': '{n} rows not created yet',
  'El historial de lo escrito en Google Sheets empieza el {date}: lo cambiado allí antes no se conoce.':
    'The history of what was typed in Google Sheets starts on {date}: changes made there before are not known.',
  'Cada celda muestra el valor que tenía en ese momento, deshaciendo todos los cambios posteriores del historial. Las fórmulas muestran su valor de hoy si eran la misma fórmula. De Google Sheets solo se conoce lo que la app leyó (cada pocos minutos, sin quién); las filas escritas directamente allí se ven como están ahora.':
    'Each cell shows the value it had at that moment, by undoing every later change in the history. Formulas show today’s value when they were the same formula. Of Google Sheets only what the app read is known (every few minutes, without who); rows typed directly there show as they are now.',
  'Ahora: {value}': 'Now: {value}',
  'Esta fila todavía no existía': 'This row did not exist yet',
  'Cómo estaba la hoja (solo lectura)': 'The sheet as it was (read-only)',

  // Server messages.
  'Falta la fila': 'The row is missing',
  'Momento no válido': 'Invalid moment',
} as Record<string, string>
