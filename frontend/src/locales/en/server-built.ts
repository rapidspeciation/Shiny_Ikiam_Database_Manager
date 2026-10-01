// Texts the server builds with values in them (server/messages.mjs: msg, msgn,
// tpl), keyed by their Spanish template. The app shows them through the
// descriptor that comes with each text (tm / tx in lib/i18n.ts). Values from the
// sheets (IDs, species, columns) fill the {placeholders} as they are.
// Words already translated elsewhere (fila {row}, sin especie, hembra…) are not repeated.
export default {
  // server/history.mjs: Historial summaries
  'Sin cambios guardados': 'No saved changes',
  '{new} y {edited}': '{new} and {edited}',
  '{n} fila nueva': '{n} new row',
  '{n} filas nuevas': '{n} new rows',
  '{n} editada': '{n} edited',
  '{n} editadas': '{n} edited',
  '{n} fila editada': '{n} row edited',
  '{n} filas editadas': '{n} rows edited',
  '{n} mariposa de colecta': '{n} collected butterfly',
  '{n} mariposas de colecta': '{n} collected butterflies',
  '{n} captura de monitoreo': '{n} monitoring capture',
  '{n} capturas de monitoreo': '{n} monitoring captures',
  '{n} emergido': '{n} emerged',
  '{n} emergidos': '{n} emerged',
  '{n} clutch nuevo': '{n} new clutch',
  '{n} clutches nuevos': '{n} new clutches',
  '{n} muerte registrada': '{n} death recorded',
  '{n} muertes registradas': '{n} deaths recorded',
  '{n} mariposa con tubos o CAM': '{n} butterfly with tubes or CAM',
  '{n} mariposas con tubos o CAM': '{n} butterflies with tubes or CAM',
  '{n} clutch actualizado': '{n} clutch updated',
  '{n} clutches actualizados': '{n} clutches updated',
  '{n} fila cambiada en Google Sheets': '{n} row changed in Google Sheets',
  '{n} filas cambiadas en Google Sheets': '{n} rows changed in Google Sheets',
  '{n} fila restaurada': '{n} row restored',
  '{n} filas restauradas': '{n} rows restored',
  '{n} fila escrita por el asistente': '{n} row written by the assistant',
  '{n} filas escritas por el asistente': '{n} rows written by the assistant',
  '{n} fila con el ID cambiado': '{n} row with its ID changed',
  '{n} filas con el ID cambiado': '{n} rows with their ID changed',
  '{head}: {items}': '{head}: {items}',
  '{head}: {items} y {more} más': '{head}: {items} and {more} more',
  // Save reasons the server gives (server/assistant.mjs, server/history.mjs)
  'Confirmado en el chat': 'Confirmed in the chat',
  'Arreglos de la Revisión de datos': 'Fixes from the data review',
  'Correcciones acordadas en Revisión': 'Corrections agreed in Review',
  'Deshecho desde el asistente': 'Undone from the assistant',

  // server/checks.mjs: Revisión problems
  '{field} tiene «{value}», que no es una fecha': '{field} has «{value}», which is not a date',
  '{field} tiene el número {value} (año {year}), que no es una fecha':
    '{field} has the number {value} (year {year}), which is not a date',
  '{field} tiene el número {value}, que no es una fecha': '{field} has the number {value}, which is not a date',
  '{field} es el texto «{value}», no una fecha': '{field} is the text «{value}», not a date',
  'fecha escrita como texto': 'date written as text',
  'día de {field}': 'day from {field}',
  'año mal escrito': 'year typed wrong',
  '{value} también está en {places}': '{value} is also in {places}',
  '{value} también está en {places} y {more} más': '{value} is also in {places} and {more} more',
  '{value} también es el CAM de {rows}': '{value} is also the CAM of {rows}',
  '{sheet} fila {row} ({label})': '{sheet} row {row} ({label})',
  'lista fija de la hoja': "the sheet's fixed list",
  '«{value}» no está en la lista de {field} ({source})': '«{value}» is not in the list of {field} ({source})',
  'Enviada al insectario sin Insectary_ID': 'Sent to the insectary without Insectary_ID',
  'La fila de {id} en Insectary_data (fila {row}) está vacía: falta registrar la mariposa':
    'The row of {id} in Insectary_data (row {row}) is empty: the butterfly still has to be recorded',
  '{id} no tiene fila en Insectary_data': '{id} has no row in Insectary_data',
  'Mariposa silvestre {id} sin fila en Collection_data': 'Wild butterfly {id} without a row in Collection_data',
  '{id}: Insectary_data dice {insectary}, Collection_data (fila {row}) dice {collection}':
    '{id}: Insectary_data says {insectary}, Collection_data (row {row}) says {collection}',
  '{id}: {field} dice {mine}, pero el CAM_ID de {sheet} (fila {row}) es {theirs}':
    '{id}: {field} says {mine}, but the CAM_ID in {sheet} (row {row}) is {theirs}',
  '{id} entró al insectario ({intro}) antes de ser colectada ({caught}, Collection_data fila {row})':
    '{id} entered the insectary ({intro}) before it was collected ({caught}, Collection_data row {row})',
  '{label} murió ({later}) antes de ser colectada ({earlier})': '{label} died ({later}) before it was collected ({earlier})',
  '{label} se preservó ({later}) antes de ser colectada ({earlier})':
    '{label} was preserved ({later}) before it was collected ({earlier})',
  '{label} murió ({later}) antes de entrar al insectario ({earlier})':
    '{label} died ({later}) before it entered the insectary ({earlier})',
  '{label} se preservó ({later}) antes de entrar al insectario ({earlier})':
    '{label} was preserved ({later}) before it entered the insectary ({earlier})',
  '{label} se preservó ({later}) antes de morir ({earlier})': '{label} was preserved ({later}) before it died ({earlier})',
  '{field} es {date}, después de hoy': '{field} is {date}, after today',
  'Preservada sin CAM_ID': 'Preserved without CAM_ID',
  'Preservada sin Tube_1_id': 'Preserved without Tube_1_id',
  '{mark} ya se usó para {species} (fila {row}, {date}); aquí es {here}':
    '{mark} was already used for {species} (row {row}, {date}); here it is {here}',
  '{mark} ya se usó para {species} (fila {row}); aquí es {here}':
    '{mark} was already used for {species} (row {row}); here it is {here}',
  // Wikiloc points without a row (walk_doubt)
  'Punto «{text}» del recorrido del {day} ({collector}) guardado sin fila: {why}. {options} Emparéjalo en Monitoreo → Dudas.':
    'Point «{text}» of the walk of {day} ({collector}) stored without a row: {why}. {options} Pair it in Monitoreo → Dudas.',
  'Punto «{text}» del recorrido del {day} ({collector}) guardado sin fila: {why} (no coincide: {conflicts}). {options} Emparéjalo en Monitoreo → Dudas.':
    'Point «{text}» of the walk of {day} ({collector}) stored without a row: {why} (does not match: {conflicts}). {options} Pair it in Monitoreo → Dudas.',
  'sin colector': 'no collector',
  'la nota no coincide del todo con su fila': 'the note does not fully match its row',
  'empate con otra fila': 'tied with another row',
  'solo por el orden del recorrido': 'only by the order of the walk',
  'ninguna fila encaja': 'no row fits',
  'No hay filas libres de ese día.': 'There are no free rows of that day.',
  'Puede ser la {a}.': 'It may be {a}.',
  'Puede ser la {a} o la {b}.': 'It may be {a} or {b}.',
  'Puede ser la {a} o la {b} o la {c}.': 'It may be {a}, {b} or {c}.',
  'fila {row} ({details})': 'row {row} ({details})',

  // server/photo-checks.mjs: from the photos
  'sin fila en la hoja': 'no row in the sheet',
  'Las fotos {files} están guardadas como {cam} ({species}), pero el sobre dice {target} ({targetSpecies})':
    'The photos {files} are filed as {cam} ({species}), but the envelope says {target} ({targetSpecies})',
  'Renombrar en Drive {files} de {from} a {to}': 'Rename {files} in Drive from {from} to {to}',
  'Renombrar en Drive {files} de {from} a {to} (después de mover las fotos que hoy ocupan ese nombre)':
    'Rename {files} in Drive from {from} to {to} (after moving the photos that now have that name)',
  'Las fotos guardadas como {cam} ({files}) muestran {target}, que ya tiene sus propias fotos':
    'The photos filed as {cam} ({files}) show {target}, which already has its own photos',
  'Unir o borrar en Drive {files}: son fotos de {target}, que ya tiene las suyas; {cam} puede no tener fotos propias':
    'Merge or delete {files} in Drive: they are photos of {target}, which already has its own; {cam} may have no photos of its own',
  'El sobre de {cam} dice {read}; la hoja dice {sheet}': 'The envelope of {cam} says {read}; the sheet says {sheet}',
  '♂ macho': '♂ male',
  '♀ hembra': '♀ female',
  'sexo del sobre': 'sex from the envelope',
  'especie del sobre': 'species from the envelope',
  'Hoja {sheet} → sobre {envelope}': 'Sheet {sheet} → envelope {envelope}',
  '{cam} preservada sin foto dorsal ni ventral en Photo_links':
    '{cam} preserved without a dorsal or ventral photo in Photo_links',
  '{cam} preservada sin foto {view} en Photo_links': '{cam} preserved without a {view} photo in Photo_links',
  '{cam}: Photo_links tiene sus fotos, pero {field} dice Not Found (¿nombre de archivo distinto?)':
    '{cam}: Photo_links has its photos, but {field} says Not Found (a different file name?)',
  '{cam}: Photo_links tiene sus fotos, pero {a} y {b} dice Not Found (¿nombre de archivo distinto?)':
    '{cam}: Photo_links has its photos, but {a} and {b} say Not Found (a different file name?)',
  'La IA de la galería ve {predicted} ({percent} %) en las fotos de {cam}; la hoja dice {sheet}':
    "The gallery's AI sees {predicted} ({percent} %) in the photos of {cam}; the sheet says {sheet}",

  // server/grid.mjs: tube racks
  'Colecta (patas)': 'Collecting (legs)',
  'Monitoreo (patas)': 'Monitoring (legs)',
  Patas: 'Legs',
  'Medio sin indicar': 'Medium not given',

  // Errors with values (server/batch.mjs, schema.mjs, verify.mjs, premade.mjs, insectaryId.mjs, grid.mjs)
  'Guarda como máximo {n} filas a la vez': 'Save at most {n} rows at a time',
  'Un guardado anterior en {sheet} aún se está confirmando; vuelve a intentarlo en un minuto':
    'An earlier save in {sheet} is still being confirmed; try again in a minute',
  'Insectary_ID {id} ya está registrado': 'Insectary_ID {id} is already recorded',
  'Hay más de una fila sin usar con el ID {id}': 'There is more than one unused row with the ID {id}',
  'Falta la columna {field} en {sheet} (Google Sheets); ese valor no se puede guardar':
    'The column {field} is missing in {sheet} (Google Sheets); that value cannot be saved',
  'Dos cambios de este guardado van a {sheet} fila {row}': 'Two changes of this save go to {sheet} row {row}',
  '{field} se calcula con una fórmula de la hoja': '{field} is computed by a formula of the sheet',
  '{field} ya da {value}; no hace falta escribirlo': '{field} already gives {value}; there is no need to type it',
  'Otra persona cambió {field} en la hoja': 'Someone else changed {field} in the sheet',
  '{field} se calcula con una fórmula en la fila nueva': '{field} is computed by a formula in the new row',
  'La fila sin usar de {id} ya no está libre; recarga y elige otro ID':
    'The unused row of {id} is no longer free; reload and choose another ID',
  '{value} ya está usado en {sheet} fila {row}': '{value} is already used in {sheet} row {row}',
  '{value} ya está usado en {sheet} fila {row} ({label})': '{value} is already used in {sheet} row {row} ({label})',
  '{value} está dos veces en este guardado': '{value} is twice in this save',
  '{field} no se puede editar': '{field} cannot be edited',
  'Valor no válido en {field}': 'Invalid value in {field}',
  'Fecha no válida en {field}: usa 14-Aug-25 o 2025-08-14, entre 1990 y 2099':
    'Invalid date in {field}: use 14-Aug-25 or 2025-08-14, between 1990 and 2099',
  '{field}: «{value}» no está en la lista de la hoja ({source})': "{field}: «{value}» is not in the sheet's list ({source})",
  'Indica entre 1 y {n} filas': 'Give between 1 and {n} rows',
  '{sheet} no tiene filas con fórmulas que copiar': '{sheet} has no rows with formulas to copy',
  'En {sheet} hay filas escritas sin fórmulas después de la última fila preasignada ({template}), hasta la {last}; revísalas en Google Sheets':
    'In {sheet} there are rows written without formulas after the last pre-made row ({template}), up to {last}; check them in Google Sheets',
  // Checks after making pre-made rows (server/premade.mjs)
  '{problem} ({n} filas)': '{problem} ({n} rows)',
  'Falta la fórmula en {column}': 'The formula is missing in {column}',
  'Quedó un valor copiado en {column}': 'A copied value was left in {column}',
  'La validación de {column} no coincide con la fila {row}': 'The validation of {column} does not match row {row}',
  'El formato de {column} no coincide con la fila {row}': 'The format of {column} does not match row {row}',
  'Fila {row}: el Insectary ID es «{id}», se esperaba {expected}':
    'Row {row}: the Insectary ID is «{id}», {expected} was expected',
  'Fila {row}: el Insectary ID es «vacío», se esperaba {expected}':
    'Row {row}: the Insectary ID is empty, {expected} was expected',
  'Fila {row}: el Insectary ID {id} ya existe': 'Row {row}: the Insectary ID {id} already exists',
  'La fila {row} no tiene un Insectary ID de la serie ({id})': 'Row {row} has no Insectary ID of the series ({id})',
  'La fila {row} no tiene un Insectary ID de la serie (vacío)': 'Row {row} has no Insectary ID of the series (empty)',
  'La serie de Insectary IDs llega a {id} y su fórmula no indica la ronda':
    'The Insectary ID series reaches {id} and its formula does not give the round',
  'La serie de Insectary IDs llega a {id}: no hay más rondas': 'The Insectary ID series reaches {id}: there are no more rounds',
  'La ronda {letter} de Insectary IDs ya está usada': 'The round {letter} of Insectary IDs is already used',
  '{id} no está registrado en Insectary_data': '{id} is not recorded in Insectary_data',
  'Hay {n} filas con {id} en Insectary_data': 'There are {n} rows with {id} in Insectary_data',
  '{id} no tiene fila preparada en Insectary_data': '{id} has no pre-made row in Insectary_data',
  '{id} no es un Insectary ID preasignado libre': '{id} is not a free pre-made Insectary ID',
  // server/notebook.mjs: why a notebook cell is doubtful, and where an implied value comes from
  'Lectura dudosa (confianza {confidence})': 'Doubtful reading (confidence {confidence})',
  '«{value}» no está en la lista de {field}': '«{value}» is not in the list of {field}',
  'Leído {read}, entre líneas del {other}: el {read} no tiene una puesta 20–90 días antes':
    'Read {read}, among lines of {other}: {read} has no clutch laid 20–90 days before',
  'Clutch {read} entre líneas del {other} (misma emergencia)': 'Clutch {read} among lines of {other} (same emergence)',
  '{value} tiene {n} cifras (son {size}): ¿falta una?': '{value} has {n} digits ({size} expected): one missing?',
  '{value} tiene {n} cifras (son {size}): ¿una de más?': '{value} has {n} digits ({size} expected): one too many?',
  'Fuera de la serie de las líneas vecinas ({from} … {to})': 'Out of the run of the lines around it ({from} … {to})',
  'De la nota: {words}': 'From the note: {words}',
  'Con CAM y muerta el día que emergió': 'With a CAM and dead the day it emerged',
  'Muerte sin preservar: como Muertes (NA / NOT_COLLECTED)': 'Death not preserved: as in Deaths (NA / NOT_COLLECTED)',
  'Individuo preservado: lo que el equipo escribe siempre': 'Preserved: what the team always writes',
  'La fecha de muerte (preservado ese día)': 'The death date (preserved that day)',
  'Killed_Preserved: preservado vivo': 'Killed_Preserved: preserved alive',
  'Lo habitual desde 2025': 'The usual since 2025',
  // server/suggestions/wikiloc-transects.mjs: corrections from the Wikiloc points
  'Transecto y hora desde Wikiloc': 'Transect and time from Wikiloc',
  'Sección del transecto (1–4) calculada con la posición del punto de Wikiloc de cada captura de monitoreo, y la hora de la nota cuando la fila tiene otra.':
    'Transect section (1–4) computed from the place of the Wikiloc point of each monitoring capture, and the note’s time when the row has another.',
  'una persona': 'a person',
  'hora, especie y sexo': 'time, species and sex',
  empate: 'tie',
  orden: 'order',
  dudoso: 'doubtful',
  '. La hora de la nota ({t}) no cabe entre los puntos vecinos ({a}–{b}): el punto pudo añadirse en otro lugar':
    '. The note’s time ({t}) does not fit between the neighbouring points ({a}–{b}): the point may have been added elsewhere',
  '. La hora de la nota ({t}) es anterior a la del punto previo ({a}): el punto pudo añadirse después, en otro lugar':
    '. The note’s time ({t}) is before the previous point’s ({a}): the point may have been added later, elsewhere',
  '. La hora de la nota ({t}) es posterior a la del punto siguiente ({b}): el punto pudo añadirse en otro lugar':
    '. The note’s time ({t}) is after the next point’s ({b}): the point may have been added elsewhere',
  'El punto está a {n} m del sendero: no se calcula el transecto': 'The point is {n} m from the trail: no transect is computed',
  '. Todas las filas de ese recorrido numeran los transectos al revés': '. Every row of that walk numbers the transects the other way round',
  'Punto de Wikiloc en T{s} ({d} m del sendero, {m} m del límite más cercano); emparejado por {how}{notes}':
    'Wikiloc point in T{s} ({d} m from the trail, {m} m from the nearest boundary); paired by {how}{notes}',
  'La fila dice T{c}; el punto de Wikiloc está en T{s} ({d} m del sendero, {m} m del límite más cercano); emparejado por {how}{notes}':
    'The row says T{c}; the Wikiloc point is in T{s} ({d} m from the trail, {m} m from the nearest boundary); paired by {how}{notes}',
  'La nota dice {t}, pero el punto está entre los de {a} y {b} del recorrido: parece {s} (hora equivocada)':
    'The note says {t}, but the point lies between those of {a} and {b} in the walk: it looks like {s} (wrong hour)',
  '; la hora de la nota sigue el orden del recorrido': '; the note’s time follows the walk’s order',
  'La nota de Wikiloc dice {noted} y la fila {typed} (otra hora){order}': 'The Wikiloc note says {noted} and the row {typed} (another hour){order}',
  'La nota de Wikiloc dice {noted} y la fila {typed}{order}': 'The Wikiloc note says {noted} and the row {typed}{order}',
  'El punto {mark} es del recorrido de {walk}, pero la fila con esa marca ese día dice {who}':
    'Point {mark} is from the walk of {walk}, but the row with that mark that day says {who}',
  'El punto {mark} es del recorrido del {walk}, pero la fila con esa marca es del {day}':
    'Point {mark} is from the walk of {walk}, but the row with that mark is from {day}',
  'Punto de Wikiloc sin fila: emparejamiento dudoso, ver Dudas': 'Wikiloc point without a row: doubtful pairing, see Doubts',
  'Punto de Wikiloc sin fila en la hoja': 'Wikiloc point without a sheet row',
  'Fila de monitoreo sin punto en el recorrido de Wikiloc de ese día': 'Monitoring row without a point in that day’s Wikiloc walk',
  '{a}{b}': '{a}{b}',
} as Record<string, string>
