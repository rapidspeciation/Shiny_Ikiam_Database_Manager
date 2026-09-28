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

/** ♀ / ♂ / ?, or the words the team writes, as the sheet's Sex values. */
export function parseSex(text: string): '' | 'female' | 'male' | 'NA' {
  const t = text.trim().toLowerCase()
  if (!t) return ''
  if (/^(♀|f|female|hembra|h)\b/.test(t) || t === '♀') return 'female'
  if (/^(♂|m|male|macho)\b/.test(t) || t === '♂') return 'male'
  return 'NA'
}

/** "Insectario" / "Preservada" / "Liberada", their first letters, or the sheet's Release_Collect codes. */
export function parseFate(text: string): 'insectario' | 'preservada' | 'liberada' | null {
  const t = text.trim().toLowerCase().replace(/^al\s+/, '')
  if (/insect|sent2/.test(t)) return 'insectario'
  if (/preserv/.test(t)) return 'preservada'
  if (/liber|releas/.test(t)) return 'liberada'
  // Typed shortcuts: i…, p…, l…
  return t.startsWith('i') ? 'insectario' : t.startsWith('p') ? 'preservada' : t.startsWith('l') ? 'liberada' : null
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
