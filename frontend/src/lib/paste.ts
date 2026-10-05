/**
 * Reading blocks of cells copied from a spreadsheet (Excel, LibreOffice,
 * Google Sheets): rows separated by new lines, cells by tabs. Used to paste
 * many butterflies at once into a list of form rows.
 */

/** The copied block, or null when the clipboard holds a single value (let the input take it). */
export function parseBlock(text: string): string[][] | null {
  const clean = text.replace(/\r\n?/g, '\n').replace(/\n$/, '')
  if (!clean.includes('\t') && !clean.includes('\n')) return null
  return splitCells(clean).map(line => line.map(cell => cell.trim()))
}

/**
 * Lines by new lines, cells by tabs. A cell between quotes (as spreadsheets copy
 * one with a new line or a tab in it) is read to its closing quote, "" inside
 * as one quote; a quote not closed before a tab, a new line or the end is text.
 */
function splitCells(text: string): string[][] {
  const rows: string[][] = [[]]
  let i = 0
  for (;;) {
    const close = text[i] === '"' ? closingQuote(text, i) : -1
    let end = close + 1
    if (close >= 0) rows.at(-1)!.push(text.slice(i + 1, close).replace(/""/g, '"'))
    else {
      end = i
      while (end < text.length && text[end] !== '\t' && text[end] !== '\n') end++
      rows.at(-1)!.push(text.slice(i, end))
    }
    if (end >= text.length) return rows
    if (text[end] === '\n') rows.push([])
    i = end + 1
  }
}

function closingQuote(text: string, start: number): number {
  for (let i = start + 1; i < text.length; i++) {
    if (text[i] !== '"') continue
    if (text[i + 1] === '"') {
      i++
      continue
    }
    return i + 1 === text.length || text[i + 1] === '\t' || text[i + 1] === '\n' ? i : -1
  }
  return -1
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
 * The option that typing picks, as the first suggestion of the list: the same
 * text (in other capitals), else the first option that starts with it, else
 * the first that contains it ("mess" → "Mechanitis messenoides"). Options come
 * most used first. None when nothing matches (a new value).
 */
export function pickChoice(typed: string, options: string[]): string | null {
  const t = typed.trim().toLowerCase()
  if (!t) return null
  return (
    options.find(o => o.toLowerCase() === t) ??
    options.find(o => o.toLowerCase().startsWith(t)) ??
    options.find(o => o.toLowerCase().includes(t)) ??
    null
  )
}

/** What was typed in a list cell, completed to the option it picks (Enter or Tab); otherwise kept as typed. */
export function complete(typed: string, options: string[]): string {
  return pickChoice(typed, options) ?? typed.trim()
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

/** The number at the end of an ID moved on by `step`, keeping its width: CAM079895 + 2 → CAM079897. */
export function stepId(id: string, step: number): string | null {
  const m = /^(.*?)(\d+)$/.exec(id.trim())
  if (!m) return null
  const n = Number(m[2]) + step
  return n < 0 ? null : `${m[1]}${String(n).padStart(m[2].length, '0')}`
}
