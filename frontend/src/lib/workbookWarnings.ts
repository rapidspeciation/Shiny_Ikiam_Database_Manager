/**
 * Known faults in the Google Sheets workbook itself (formulas, lookups), found
 * in the 27 Sep 2026 audit (docs/data-entry-audit.md). The app does not change
 * formulas; these are reminders to fix in the sheet. Remove an entry once fixed.
 */
export interface WorkbookWarning {
  sheet: string
  problem: string
  fix: string
  found: string
}

export const WORKBOOK_WARNINGS: WorkbookWarning[] = [
  {
    sheet: 'Wing_tissue',
    problem:
      'Tube_n_rack busca el tubo en la columna A ("Sort") de Ithomiini_tube_locations en vez de la B: las 438 filas dicen "Not in TOL704", aunque 1.925 de sus tubos sí están en racks.',
    fix: 'Cambiar el rango de búsqueda de la fórmula a la columna B (Tube_ID).',
    found: '27-Sep-26',
  },
  {
    sheet: 'Wing_tissue',
    problem: 'Tube_n_manifest todavía busca en MEIER_manifests_30Jan25_OLD.',
    fix: 'Apuntar la fórmula a MEIER_manifests_23Jun26, como en Collection_data e Insectary_data.',
    found: '27-Sep-26',
  },
  {
    sheet: 'Insectary_data',
    problem:
      'Photo_dorsal / Photo_ventral usan MATCH sin coincidencia exacta: las fotos que faltan salen como #N/A en vez de "NOT FOUND".',
    fix: 'Añadir el 0 final a MATCH (coincidencia exacta), como en Collection_data.',
    found: '27-Sep-26',
  },
  {
    sheet: 'Collection_data',
    problem: 'Data_entry_order está roto: 876 celdas #REF!, 133 #VALUE! y 857 vacías.',
    fix: 'Rehacer la fórmula o quitar la columna; el orden de las filas ya indica el orden de ingreso.',
    found: '27-Sep-26',
  },
  {
    sheet: 'Ithomiini_tube_locations_18Jun26',
    problem:
      'Genus, DATE_OF_COLLECTION, COLLECTOR_SAMPLE_ID y FILTER muestran #REF!: apuntan a hojas borradas (Genus&CollectionDate_21Nov24, Filter_tube_locations).',
    fix: 'Rehacer esas búsquedas contra Collection_data / Insectary_data.',
    found: '27-Sep-26',
  },
  {
    sheet: 'Insectary_stocks',
    problem: '"Earliest Emerge Date" es texto, no fecha, así que no se puede ordenar ni comparar.',
    fix: 'Envolver la fórmula en DATEVALUE o devolver la fecha como número con formato de fecha.',
    found: '27-Sep-26',
  },
  {
    sheet: 'Melinaea_crosses',
    problem: 'La columna "Number of Eggs laid" contiene fechas de postura; hay 64 celdas #VALUE!.',
    fix: 'Renombrar la columna (o mover las fechas) y revisar las fórmulas con #VALUE!.',
    found: '27-Sep-26',
  },
  {
    sheet: 'Insectary_data',
    problem:
      'Pedigree muestra el texto de la fórmula "YES or NO" en 1.222 filas (971 WEST x EAST, 251 F1/F2) que debían quedar en YES.',
    fix: 'Escribir YES en esas filas (el protocolo de cruces pide Pedigree = Yes).',
    found: '27-Sep-26',
  },
]
