import { displayValue, readingValue } from './cells'
import { isNoteField } from './rowForm'
import type { CellValue, Field } from './types'

/**
 * A sheet's rows as a list for a phone (components/search/RowList.vue): one finger-sized line per row, with what
 * tells the row apart (its ID and species) and a few of its facts, so a row is found and opened without a grid of
 * small cells. In the Buscador's results each line also shows the cell where the searched text is.
 */
export interface RowListItem {
  id: string
  /** The sheet's row number (0 for a row not saved yet). */
  row: number
  /** The row's ID (its sheet's first identity column), else its first short cell; '' when it has neither. */
  title: string
  species: string
  /** A few short facts (sex, clutch, CAM…), shown on one line. */
  facts: string[]
  /** Where the searched text is, when the title and the species do not show it. */
  match: { field: string; text: string } | null
  /** Not saved yet (a row added here). */
  isNew: boolean
  /** A saved row with edits not saved yet (typed in its form). */
  dirty?: boolean
}

/** What a sheet writes when there is nothing to say: never a fact worth a line. */
const EMPTY = /^(NA|N\/A|NOT_COLLECTED|YES or NO|-|—)?$/i
const filled = (text: string) => !EMPTY.test(text.trim())
const SPECIES = /^species$/i
const SEX = /^sex$/i
const CLUTCH = /^clutch([ _]number)?$/i
const CAM = /cam_?id/i

/**
 * A cell around the searched text, cut to a line's worth: «…wing torn, found on the floor…». The whole searched
 * text always stays in. A cell without spaces (a link, a file's path) is cut shorter: it is not read, only the place
 * of the match in it.
 */
export function snippet(text: string, query: string, width = 64): string {
  const q = query.trim()
  const room = Math.max(/\s/.test(text) ? width : Math.min(width, 24), q.length + 12)
  const at = text.toLowerCase().indexOf(q.toLowerCase())
  if (text.length <= room || at < 0) return text.length > room ? `${text.slice(0, room - 1).trimEnd()}…` : text
  const from = Math.max(0, Math.min(at - Math.floor((room - q.length) / 2), text.length - room))
  const cut = text.slice(from, from + room).trim()
  return `${from > 0 ? '…' : ''}${cut}${from + room < text.length ? '…' : ''}`
}

/**
 * A text in pieces, those that are the searched text marked (any case), to show them highlighted.
 * With no text searched, one piece.
 */
export function markParts(text: string, query = ''): { text: string; hit: boolean }[] {
  const q = query.trim().toLowerCase()
  if (!q || !text) return [{ text, hit: false }]
  const out: { text: string; hit: boolean }[] = []
  const low = text.toLowerCase()
  let from = 0
  for (let at = low.indexOf(q); at >= 0; at = low.indexOf(q, from)) {
    if (at > from) out.push({ text: text.slice(from, at), hit: false })
    out.push({ text: text.slice(at, at + q.length), hit: true })
    from = at + q.length
  }
  if (from < text.length) out.push({ text: text.slice(from), hit: false })
  return out
}

/**
 * A row's line: `identity` the sheet's ID columns (the first is its title), `query` the text searched (the Buscador's
 * results), `matchColumns` the columns holding it, the likeliest first.
 */
export function rowListItem(
  row: { id: string; row: number; values: Record<string, CellValue> },
  columns: Field[],
  identity: string[] = [],
  query = '',
  matchColumns: string[] = [],
): RowListItem {
  // What a person reads (a date with the month in the interface language), and what the sheet shows (the text the
  // search looked in: a match is found and shown as the sheet has it).
  const text = (f: Field | undefined) => (f ? readingValue(row.values[f.key], f).trim() : '')
  const sheetText = (f: Field | undefined) => (f ? displayValue(row.values[f.key], f).trim() : '')
  const idField = columns.find(f => f.key === identity[0])
  const speciesField = columns.find(f => SPECIES.test(f.key))
  const title = text(idField)
  const species = filled(text(speciesField)) ? text(speciesField) : ''
  const sex = text(columns.find(f => SEX.test(f.key)))
  const clutch = text(columns.find(f => CLUTCH.test(f.key)))
  const cam = columns.map(f => (CAM.test(f.key) && f !== idField ? text(f) : '')).find(filled) ?? ''
  let facts = [sex, filled(clutch) ? `clutch ${clutch}` : '', cam].filter(filled)
  // A sheet without those columns: its first short cells after the ID say what the row is.
  if (!facts.length)
    facts = columns
      .filter(f => f !== idField && f !== speciesField && !isNoteField(f.key))
      .map(text)
      .filter(v => filled(v) && v.length <= 28)
      .slice(0, species ? 2 : 3)
  // A sheet whose rows have no ID (photo links, lists): its first fact names the row.
  const named = title || (species ? '' : (facts.shift() ?? ''))
  const q = query.trim().toLowerCase()
  const shows = (value: string) => !!q && value.toLowerCase().includes(q)
  let match: RowListItem['match'] = null
  if (q && !shows(named) && !shows(species)) {
    const order = [...matchColumns.flatMap(key => columns.find(f => f.key === key) ?? []), ...columns]
    const hit = order.find(f => shows(sheetText(f)))
    if (hit) match = { field: hit.key, text: snippet(sheetText(hit), q) }
  }
  // The fact that is the match is said once, with its column.
  const matched = match && columns.find(f => f.key === match.field)
  if (matched) facts = facts.filter(f => f !== text(matched) && !shows(f))
  return { id: row.id, row: row.row, title: named, species, facts, match, isNew: false }
}
