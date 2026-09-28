/**
 * Reading blocks of cells copied from a spreadsheet (Excel, LibreOffice,
 * Google Sheets): rows separated by new lines, cells by tabs. Used to paste
 * many butterflies at once into a list of form rows.
 */

/** The copied block, or null when the clipboard holds a single value (let the input take it). */
export function parseBlock(text: string): string[][] | null {
  const clean = text.replace(/\r\n?/g, '\n').replace(/\n$/, '')
  if (!clean.includes('\t') && !clean.includes('\n')) return null
  return clean.split('\n').map(line => line.split('\t').map(cell => cell.trim()))
}

/** The one fate or sex whose names begin with what was typed ("he" → hembra → female), if only one does. */
function byPrefix<T extends string>(typed: string, names: Record<T, string[]>): T | null {
  const hits = (Object.keys(names) as T[]).filter(key => names[key].some(name => name.startsWith(typed)))
  return hits.length === 1 ? hits[0] : null
}

/**
 * ♀ / ♂, the sheet's values, or the start of the words the team writes (he,
 * fe, ma…), as Collection_data's Sex values. A trailing "?" (female?, f ?,
 * female_?) means unsure; "?" alone or NA means not recorded (NOT_COLLECTED).
 */
export function parseSex(text: string): '' | 'female' | 'male' | 'female ?' | 'male ?' | 'NOT_COLLECTED' {
  const t = text.trim().toLowerCase()
  if (!t) return ''
  if (/^(\?|na|n\/a|not(_collected)?|unknown|desconocido)$/.test(t) || byPrefix(t, { NOT_COLLECTED: ['not_collected'] }))
    return 'NOT_COLLECTED'
  const unsure = /[\s_]*\?$/.test(t)
  const word = t.replace(/[\s_]*\?$/, '')
  const sex = word.startsWith('♀')
    ? 'female'
    : word.startsWith('♂')
      ? 'male'
      : byPrefix(word, { female: ['female', 'hembra'], male: ['male', 'macho'] })
  if (!sex) return ''
  return unsure ? `${sex} ?` : sex
}

/** Release_Collect values, the Spanish names, or the start of either (col_p, pres, i…). */
export function parseFate(text: string): 'insectario' | 'preservada' | 'liberada' | null {
  const t = text.trim().toLowerCase().replace(/^al\s+/, '')
  if (!t) return null
  if (/insect|sent2/.test(t)) return 'insectario'
  if (/preserv/.test(t)) return 'preservada'
  if (/liber|releas/.test(t)) return 'liberada'
  return byPrefix(t, {
    insectario: ['collected_sent2insectary', 'insectario'],
    preservada: ['collected_preserved', 'preservada'],
    liberada: ['released_unmarked', 'liberada'],
  })
}

/**
 * What was typed in a list cell, completed to the one option it begins (or is,
 * in other capitals): "Ithomia sal" → "Ithomia salapia". Otherwise kept as
 * typed, so a new value can still be entered.
 */
export function complete(typed: string, options: string[]): string {
  const t = typed.trim().toLowerCase()
  if (!t) return typed.trim()
  const exact = options.find(o => o.toLowerCase() === t)
  if (exact) return exact
  const starts = options.filter(o => o.toLowerCase().startsWith(t))
  return starts.length === 1 ? starts[0] : typed.trim()
}

/** "CAM079895 · FS90415305 (Flash frozen)" → the CAM ID and the tube, if present. */
export function parseCamTube(text: string): { cam: string; tube: string } {
  return {
    cam: /CAM\d{4,}/i.exec(text)?.[0].toUpperCase() ?? '',
    tube: /\b[A-Z]{2}\d{7,9}\b/.exec(text.replace(/CAM\d+/gi, ''))?.[0] ?? '',
  }
}

/** "11:02", "11.02" or "1102" as hh:mm; anything else is kept as typed. */
export function parseTime(text: string): string {
  const m = /^(\d{1,2})[:.h]?(\d{2})$/.exec(text.trim())
  return m && Number(m[1]) < 24 && Number(m[2]) < 60 ? `${m[1].padStart(2, '0')}:${m[2]}` : text.trim()
}
