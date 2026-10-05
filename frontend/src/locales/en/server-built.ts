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
  // server/batch.mjs, server/staged.mjs: identifiers held by entries kept in the app
  '{value} ya lo tiene {name} en cambios aún no guardados en Google Sheets': '{value} is taken by {name} in changes not saved to Google Sheets yet',
  '{value} ya lo tiene {name} en cambios aún no guardados en Google Sheets; el siguiente libre es {next}':
    '{value} is taken by {name} in changes not saved to Google Sheets yet; the next free one is {next}',
  'Otra persona cambió {field} en esa fila sin guardar': 'Someone else changed {field} in that unsaved row',
  '{name} cambió después la misma celda de {label}: deshaz primero ese cambio': '{name} changed the same cell of {label} later: undo that change first',
  'Registrado en la app por {people}': 'Entered in the app by {people}',
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
  'Los tubos {prefix} tienen {expected} dígitos; este tiene {digits}': '{prefix} tubes have {expected} digits; this one has {digits}',
  'Los {prefix} de {field} tienen {expected} dígitos; este tiene {digits}':
    '{prefix} IDs in {field} have {expected} digits; this one has {digits}',
  'Preservada sin CAM_ID': 'Preserved without CAM_ID',
  'Preservada sin Tube_1_id': 'Preserved without Tube_1_id',
  'Preservada ({why}) sin {field}': 'Preserved ({why}) without {field}',
  'Con fecha de entrada en Intro2Insectary_date: adulto': 'A date in Intro2Insectary_date: an adult',
  'LIFESTAGE es {stage}, pero Intro2Insectary_date tiene una fecha ({date}): con fecha de entrada es Adult':
    'LIFESTAGE is {stage}, but Intro2Insectary_date has a date ({date}): with an entry date it is Adult',
  'Death_cause dice preservada ({why}), pero CAM_ID y los tubos dicen NA (no preservada)':
    'Death_cause says preserved ({why}), but CAM_ID and the tubes say NA (not preserved)',
  'Preservada sin {field}: pregunta al equipo': 'Preserved without {field}: ask the team',
  'Death_cause dice Killed_Preserved, pero {field} es NA: pregunta al equipo':
    'Death_cause says Killed_Preserved, but {field} is NA: ask the team',
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

  // Errors with values (server/batch.mjs, schema.mjs, verify.mjs, premade.mjs, insectaryId.mjs, grid.mjs, assistant.mjs)
  'Guarda como máximo {n} filas a la vez': 'Save at most {n} rows at a time',
  'Una propuesta tiene como máximo {n} filas': 'A proposal has at most {n} rows',
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
  // An Insectary ID on two butterflies (W2B.1, W2B.2)
  'El Insectary ID {id} se calcula con una fórmula: solo se cambia añadiéndole un sufijo ({id}.1)':
    'The Insectary ID {id} is computed by a formula: it only changes by adding a suffix ({id}.1)',
  '{base} no está en Insectary_data: {id} es para una segunda mariposa con el ID {base}':
    '{base} is not in Insectary_data: {id} is for a second butterfly with the ID {base}',
  'La fila de {base} está sin usar: la mariposa va en ella, sin sufijo':
    'The row of {base} is unused: the butterfly goes in it, without a suffix',
  '{value} está en más de una fila ({rows}): corrígelo antes de añadir {id}':
    '{value} is in more than one row ({rows}): fix that before adding {id}',
  'Las filas nuevas de {base} se guardan de una en una': 'New rows of {base} are saved one at a time',
  'La fila {row} ya no es {id} en Google Sheets; recarga y vuelve a intentarlo':
    'Row {row} is no longer {id} in Google Sheets; reload and try again',
  'Debajo de {id} (fila {row}) hay otra fila de {base} en Google Sheets; recarga y vuelve a intentarlo':
    'Below {id} (row {row}) there is another row of {base} in Google Sheets; reload and try again',
  'La fila {label} tiene datos que no puso ese guardado ({field}); no se borra':
    'Row {label} holds data that save did not write ({field}); it is not deleted',
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
  'En Insectary_data hay filas escritas sin Insectary ID después del último ID (fila {row}): {rows}; revísalas en Google Sheets':
    'In Insectary_data there are rows written without an Insectary ID after the last ID (row {row}): {rows}; check them in Google Sheets',
  'El último Insectary ID está ahora en la fila {row} de Google Sheets; vuelve a intentarlo en un minuto':
    'The last Insectary ID is now in row {row} in Google Sheets; try again in a minute',
  'Quedan {n} filas al final de Insectary_data y hacen falta {count}: pide a PAS que añada filas':
    '{n} rows are left at the end of Insectary_data and {count} are needed: ask PAS to add rows',
  'No hay una fórmula de Insectary ID encima de la fila {row}': 'There is no Insectary ID formula above row {row}',
  'Google Sheets no deja a la cuenta de la app insertar la fila de {id}: pide a PAS que inserte la fila; no se guardó nada':
    'Google Sheets does not let the app’s account insert the row of {id}: ask PAS to insert the row; nothing was saved',
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
  'Leído {read}, entre líneas del {other}: la especie escrita es la del {other}': 'Read {read}, among lines of {other}: the species written is that of {other}',
  'La hoja tiene {sheet}: {a} y {b} se parecen en esta letra': 'The sheet has {sheet}: {a} and {b} look alike in this hand',
  'La hoja tiene {sheet}: un 1 de más o de menos': 'The sheet has {sheet}: a 1 more or less',
  'La página cambia el término {n} de la suma de la hoja ({sheet}); los términos nuevos se escriben al final':
    "The page changes term {n} of the sheet's sum ({sheet}); new terms are written at the end",
  '{why} (pasado de una foto del cuaderno el {date})': '{why} (typed from a notebook photo on {date})',
  'Mismo error que la línea {line} de la página ({wrong} → {right}: {what}); esta fila no está en la foto':
    'Same error as line {line} of the page ({wrong} → {right}: {what}); this row is not on the photo',
  'falta un {char}': 'a {char} missing',
  'un {char} de más': 'an extra {char}',
  'un {char} repetido': 'a doubled {char}',
  'dos cifras cambiadas de sitio': 'two digits swapped',
  '{from} en vez de {to}': '{from} instead of {to}',
  'ceros a la izquierda': 'leading zeros',
  'El clutch {clutch} es {species}: el stock es su subespecie; la página dice «{read}»':
    'Clutch {clutch} is {species}: the stock is its subspecies; the page says «{read}»',
  'Causa escrita «{written}»: Unknown': 'Cause written «{written}»: Unknown',
  'Larva o huevo encontrado muerto y preservado': 'Egg or larva found dead and preserved',
  'Huevo o larva preservado: lo que el equipo escribe': 'Preserved egg or larva: what the team writes',

  // server/suggestions/: Revisión → Sugerencias (titles, descriptions, reasons)
  'Arreglos de los chequeos': 'Fixes from the checks',
  'Los arreglos que la Revisión de datos ya calcula: un valor de lista escrito de otra forma (seguro), el CAM de la otra hoja en una columna copiada, un año escrito con uno de diferencia y una fecha escrita como texto (probables), y una fecha sacada del inicio de una nota (revisar).':
    "The fixes the data review already computes: a list value written another way (certain), the other sheet's CAM in a copied column, a year typed one off and a date written as text (likely), and a date taken from the start of a note (check).",
  '{problem} ({note})': '{problem} ({note})',
  'Espacios de más': 'Extra spaces',
  'Valores con espacios al inicio o al final que cambian lo que la hoja lee: « NA», un ID o un tubo con un espacio, o un valor de una lista que en la lista no lleva ese espacio. Las notas no se tocan. Seguro: solo se quitan los espacios.':
    'Values with spaces at the start or end that change what the sheet reads: « NA», an ID or a tube with a space, or a list value the list has without that space. Notes are left alone. Certain: only the spaces go.',
  '«{value}» lleva espacios que la hoja cuenta como parte del valor': '«{value}» has spaces the sheet counts as part of the value',
  'Fórmulas que faltan': 'Missing formulas',
  'Filas de Collection_data enviadas al insectario (Collected_Sent2Insectary) con Death_date, Preservation_date, Preservation_medium o Preserved_dead_alive vacíos donde las filas anteriores tienen la fórmula que los lee de Insectary_data. Seguro: copiar la fórmula de la fila de arriba; se muestra lo que daría hoy.':
    'Collection_data rows sent to the insectary (Collected_Sent2Insectary) with Death_date, Preservation_date, Preservation_medium or Preserved_dead_alive empty where the rows before have the formula that reads them from Insectary_data. Certain: copy the formula down from the row above; what it would give today is shown.',
  'falta la fórmula de la fila {from}; hoy daría vacío ({id} no tiene {target} en Insectary_data)':
    'the formula of row {from} is missing; today it would give nothing ({id} has no {target} in Insectary_data)',
  'falta la fórmula de la fila {from}; hoy daría {value} ({target} de {id} en Insectary_data)':
    'the formula of row {from} is missing; today it would give {value} ({target} of {id} in Insectary_data)',
  'Fechas imposibles': 'Impossible dates',
  'Fechas que no son fechas, en el futuro o antes de 2009, cuando se puede leer la fecha que se quiso escribir: un año con un dígito cambiado (2926 por 2026), un mes en otro idioma («13-mrt-26») o un día sin año («9/30»). Se sugiere la lectura más cercana a las fechas de las filas de alrededor; probable si es la única a menos de 120 días de ellas.':
    'Dates that are no date, in the future or before 2009, when the intended date can be read: a year with one digit changed (2926 for 2026), a month in another language («13-mrt-26») or a day without its year («9/30»). The reading closest to the dates of the rows around is suggested; likely when it is the only one within 120 days of them.',
  'un dígito del año cambiado': 'one digit of the year changed',
  'mes escrito en otro idioma': 'month written in another language',
  'día sin año': 'day without its year',
  '«{value}» es NA con un carácter de más': '«{value}» is NA with an extra character',
  '{field} «{value}» no se puede leer como una fecha entre 2009 y hoy': '{field} «{value}» cannot be read as a date between 2009 and today',
  '{field} «{value}»: {how}; no hay fechas cerca para comparar': '{field} «{value}»: {how}; no dates nearby to compare with',
  '{field} «{value}»: {how}; las filas de alrededor tienen {near} ({others} lecturas más)':
    '{field} «{value}»: {how}; the rows around have {near} ({others} more readings)',
  'Tubos con un dígito de más o de menos': 'Tubes with a digit too many or too few',
  'Tubos FluidX con un dígito menos (o más) que los demás de su prefijo, como FS3886683 por FS63886683. Se pone (o quita) el dígito donde el tubo queda junto a los tubos de la misma gradilla que ya están en el libro; las filas seguidas escritas el mismo día se corrigen juntas. Probable si una sola lectura toca la serie; si no, revisar la etiqueta.':
    'FluidX tubes with one digit fewer (or more) than the others of their prefix, such as FS3886683 for FS63886683. The digit is put back (or taken out) where the tube lands next to tubes of the same rack already in the workbook; consecutive rows typed the same day are corrected together. Likely when a single reading touches the run; otherwise check the label.',
  '{tube} tiene {n} dígitos (los {prefix} tienen {length}); {fixed} queda a {gap} de tubos ya usados':
    '{tube} has {n} digits ({prefix} tubes have {length}); {fixed} is {gap} away from tubes in use',
  '{tube} tiene {n} dígitos (los {prefix} tienen {length}); {fixed} queda a {gap} de tubos ya usados, y {others} lecturas más a menos de 30':
    '{tube} has {n} digits ({prefix} tubes have {length}); {fixed} is {gap} away from tubes in use, and {others} more readings within 30',
  '{tube} tiene {n} dígitos (los {prefix} tienen {length}); leídas juntas las {run} filas seguidas, {fixed} queda a {gap} de tubos ya usados':
    '{tube} has {n} digits ({prefix} tubes have {length}); reading the {run} consecutive rows together, {fixed} is {gap} away from tubes in use',
  '{tube} tiene {n} dígitos (los {prefix} tienen {length}); leídas juntas las {run} filas seguidas, {fixed} queda a {gap} de tubos ya usados, y {others} lecturas más a menos de 30':
    '{tube} has {n} digits ({prefix} tubes have {length}); reading the {run} consecutive rows together, {fixed} is {gap} away from tubes in use, and {others} more readings within 30',
  '{tube} tiene {n} dígitos (los {prefix} tienen {length}) y ninguna lectura queda junto a tubos ya usados':
    '{tube} has {n} digits ({prefix} tubes have {length}) and no reading lands next to tubes in use',
  'Especie o sexo distintos entre la fila de Collection_data y la de Insectary_data de la misma mariposa silvestre. Probable cuando un lado se corrigió después (historial) o el sobre de las fotos dice lo mismo que un lado; si no, revisar: se sugiere que Insectary_data siga a Collection_data. Se omiten los pares con fechas a más de 3 días (un ID viejo repetido: otra mariposa).':
    'Species or sex differing between the Collection_data and Insectary_data rows of one wild butterfly. Likely when one side was corrected later (history) or the envelope in the photos agrees with one side; otherwise check: Insectary_data is suggested to follow Collection_data. Pairs whose dates are more than 3 days apart are left out (an old ID used twice: another butterfly).',
  'Collection_data se corrigió el {date} ({who}): {before} → {after}': 'Collection_data was corrected on {date} ({who}): {before} → {after}',
  'Insectary_data se corrigió el {date} ({who}): {before} → {after}': 'Insectary_data was corrected on {date} ({who}): {before} → {after}',
  'el sobre de las fotos dice «{read}», como {sheet}': 'the envelope in the photos says «{read}», like {sheet}',
  '{id}: {why}': '{id}: {why}',
  '{id}: {sheet} fila {row} dice {value}; nada en el libro dice qué lado es el correcto (mirar el sobre)':
    '{id}: {sheet} row {row} says {value}; nothing in the workbook says which side is right (look at the envelope)',
  'Pedigree sin decidir': 'Pedigree not decided',
  'Mariposas muertas o preservadas de Insectary_data con Pedigree «YES or NO» (la fórmula espera que alguien escriba Yes o No). Se sugiere lo que dice el libro: si está en F1/F2_MutationRate (mismo Insectary_ID y CAM), solo con su Insectary_ID, o si su clutch es F1, F2 o Backcross. La certeza sale de cómo se decidieron las filas ya decididas con la misma evidencia; sin evidencia no se sugiere valor. Lo deciden PAS y el equipo de cruces.':
    'Dead or preserved butterflies of Insectary_data with Pedigree «YES or NO» (the formula waits for someone to type Yes or No). What the workbook shows is suggested: whether it is in F1/F2_MutationRate (same Insectary_ID and CAM), only with its Insectary_ID, or whether its clutch is F1, F2 or Backcross. The certainty comes from how the rows already decided with the same evidence were decided; without evidence no value is suggested. PAS and the crosses team decide it.',
  'F1/F2_MutationRate fila {row} es esta mariposa ({id}, {cam})': 'F1/F2_MutationRate row {row} is this butterfly ({id}, {cam})',
  'F1/F2_MutationRate fila {row} tiene su Insectary_ID {id}, con otro CAM ({cam})':
    'F1/F2_MutationRate row {row} has its Insectary_ID {id}, with another CAM ({cam})',
  'su clutch {clutch} es {generation} en Insectary_stocks': 'its clutch {clutch} is {generation} in Insectary_stocks',
  'silvestre, no está en F1/F2_MutationRate': 'wild, not in F1/F2_MutationRate',
  'no está en F1/F2_MutationRate; su clutch {clutch} es de stock (Generation NA)':
    'not in F1/F2_MutationRate; its clutch {clutch} is a stock clutch (Generation NA)',
  'no está en F1/F2_MutationRate y su clutch no tiene Generation en Insectary_stocks':
    'not in F1/F2_MutationRate and its clutch has no Generation in Insectary_stocks',
  '«{value}» es {right} escrito en otra forma': '«{value}» is {right} written another way',
  '{why}; en las filas ya decididas con lo mismo, {value} en {k} de {n}': '{why}; in the rows already decided with the same, {value} in {k} of {n}',
  '{why}; ninguna fila decidida con lo mismo': '{why}; no row decided with the same',

  // server/alerts.mjs: Revisión → Alertas and Inicio
  '{pool}: no quedan CAM en ningún rango. Pidan a PAS o AA un rango nuevo.': '{pool}: no CAMs left in any range. Ask PAS or AA for a new range.',
  '{pool}: queda {n} CAM en {first}–{last} (último {cam}, {date}). Pidan a PAS o AA un rango nuevo.':
    '{pool}: {n} CAM left in {first}–{last} (last {cam}, {date}). Ask PAS or AA for a new range.',
  '{pool}: quedan {n} CAM en {first}–{last} (último {cam}, {date}). Pidan a PAS o AA un rango nuevo.':
    '{pool}: {n} CAMs left in {first}–{last} (last {cam}, {date}). Ask PAS or AA for a new range.',
  '{species} llegó a {limit} preservadas el {date}: desde ahora se marca y libera, no se preserva.':
    '{species} reached {limit} preserved on {date}: from now on it is marked and released, not preserved.',
  '{species}: {n} preservada en los últimos 60 días después de llegar a {limit} ({total} en total).':
    '{species}: {n} preserved in the last 60 days after reaching {limit} ({total} in all).',
  '{species}: {n} preservadas en los últimos 60 días después de llegar a {limit} ({total} en total).':
    '{species}: {n} preserved in the last 60 days after reaching {limit} ({total} in all).',
  '{species}: {preserved} preservadas, falta {n} para {limit}.': '{species}: {preserved} preserved, {n} more to {limit}.',
  '{species}: {preserved} preservadas, faltan {n} para {limit}.': '{species}: {preserved} preserved, {n} more to {limit}.',
  '{id} ({species}) preservada el {date} sin CAM/tubo — pregunta al equipo': '{id} ({species}) preserved on {date} without CAM/tube — ask the team',
  '{id} ({species}) preservada el {date} sin CAM/tubo': '{id} ({species}) preserved on {date} without CAM/tube',
  '{id} ({species}): Death_cause Killed_Preserved el {date}, pero CAM_ID y los tubos dicen NA — pregunta al equipo':
    '{id} ({species}): Death_cause Killed_Preserved on {date}, but CAM_ID and the tubes say NA — ask the team',
  '{id} ({species}): Death_cause Killed_Preserved el {date}, pero CAM_ID y los tubos dicen NA':
    '{id} ({species}): Death_cause Killed_Preserved on {date}, but CAM_ID and the tubes say NA',
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
