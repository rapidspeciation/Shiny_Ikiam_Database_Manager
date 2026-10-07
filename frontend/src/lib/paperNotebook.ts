import { sortRecorded, type RecordedOrder } from './deathsCart'
import { t } from './i18n'

/**
 * What to mark in the paper Emergidos notebook (server/paper-notebook.mjs),
 * which lists every butterfly in Insectary ID order (the sheet's rows):
 * «Actualizar el cuaderno» after a day's censuses (☺ seen, ✗ disappeared to
 * write, ▬ already dead in the database to highlight) and «Filas para
 * resaltar» in Muertes (the deaths entered since a moment, to highlight).
 */

/** A death in the sheet (with the entries kept in the app): `staged` while only in the app. */
export interface PaperDeath {
  /** A serial date, or what the cell says (NA). */
  date: number | string | null
  cause: string
  staged: boolean
}
/** One butterfly of «Actualizar el cuaderno». */
export interface NotebookLine {
  /** A sheet row, or staged:<clientId> for one emerged and not in Google Sheets yet. */
  recordId: string
  id: string
  /** Its row (= the notebook's order); null for one with no pre-made row. */
  row: number | null
  species: string
  sex: string
  /** Intro2Insectary_date. */
  entered: number | null
  wild: boolean
  /** What a census of the day found (`waiting`: its disappearances not in Google Sheets yet). */
  census: { status: 'seen' | 'disappeared' | 'excluded'; censusId: string; note: string; waiting: boolean } | null
  death: PaperDeath | null
  /** On paper: write ✗ (disappeared in the census) or highlight ▬ (any other death in the database). */
  todo: 'write' | 'highlight' | null
  /** Its disappearance was undone since (alive again). */
  undone?: boolean
  staged?: boolean
}
export interface NotebookUpdate {
  day: string
  since: string | null
  species: string[]
  censuses: { id: string; species: string; waiting: boolean }[]
  censused: string[]
  lines: NotebookLine[]
}
/** A butterfly of «Filas para resaltar». */
export interface EnteredDeath {
  recordId: string
  id: string
  row: number | null
  species: string
  sex: string
  entered: number | null
  death: PaperDeath
  /** When the death was entered (the save that made it dead). */
  enteredAt: string
  source: 'app' | 'sheets'
  by: string[]
}
export interface EnteredDeaths {
  since: string
  historyStart: string | null
  items: EnteredDeath[]
}

const EPOCH = Date.UTC(1899, 11, 30)
const dateOf = (serial: number) => new Date(EPOCH + Math.round(serial) * 86_400_000)
const MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
/** "2-Oct": a day of this season, as the list shows it. */
export function shortDay(serial: number): string {
  const d = dateOf(serial)
  return `${d.getUTCDate()}-${MONTHS[d.getUTCMonth()]}`
}
/** "2/10": a day as written in the notebook. */
export function slashDay(serial: number): string {
  const d = dateOf(serial)
  return `${d.getUTCDate()}/${d.getUTCMonth() + 1}`
}
const dayText = (date: PaperDeath['date'], form: (s: number) => string) => (typeof date === 'number' ? form(date) : (date ?? ''))

/** Lines without a pre-made row last; the notebook's order is the rows'. */
const BIG = Number.MAX_SAFE_INTEGER
/** The lines in the order chosen: the notebook's (rows) or emergence date, ↑/↓ (the same emergence by row). */
export function sortPaper<T extends { id: string; row: number | null; entered: number | null }>(
  items: T[],
  order: RecordedOrder,
): T[] {
  return sortRecorded(items, order, i => ({ id: i.id, emergence: i.entered, row: i.row ?? BIG }))
}
/** «Solo lo que hay que marcar» (✗ to write, ▬ to highlight) or «Todas». */
export const filterPaper = (lines: NotebookLine[], only: boolean) => (only ? lines.filter(l => l.todo) : lines)

/** A line as the list says it: its mark and its words. */
export function lineText(line: NotebookLine, day: number): { mark: '☺' | '✗' | '▬' | '·'; text: string } {
  if (line.todo === 'write') return { mark: '✗', text: t('Desapareció {day}', { day: shortDay(day) }) }
  if (line.todo === 'highlight') {
    const what = [dayText(line.death!.date, shortDay), line.death!.cause].filter(Boolean).join(', ')
    const seen = line.census?.status === 'seen' ? `${t('☺ vista')} · ` : ''
    return { mark: '▬', text: `${seen}${t('Ya muerta en la base: {what}', { what })}` }
  }
  if (line.census?.status === 'seen') return { mark: '☺', text: t('vista') }
  if (line.undone) return { mark: '·', text: t('viva (desaparición deshecha)') }
  if (line.census?.status === 'excluded')
    return { mark: '·', text: [t('no contada'), line.census.note].filter(Boolean).join(': ') }
  return { mark: '·', text: t('viva') }
}

/** One line of the copied text, in the notebook's words: «P4D ✗ desapareció 2/10», «P6D ▬ muerta 29/9 Unknown — resaltar». */
export function copyLine(line: NotebookLine, day: number): string {
  if (line.todo === 'write') return `${line.id} ✗ ${t('desapareció {day}', { day: slashDay(day) })}`
  if (line.todo === 'highlight')
    return `${line.id} ${deathCopy(line.death!)}${line.census?.status === 'seen' ? ` (${t('☺ vista')})` : ''}`
  return `${line.id} ${lineText(line, day).mark === '☺' ? '☺ ' : ''}${lineText(line, day).text}`
}
/** «▬ muerta 29/9 Unknown — resaltar». */
export function deathCopy(death: PaperDeath): string {
  const what = [dayText(death.date, slashDay), death.cause].filter(Boolean).join(' ')
  return `▬ ${t('muerta {what} — resaltar', { what })}`.replace(/\s+/g, ' ')
}

/** «Copiar como texto»: a title, then one line per butterfly in the order shown. */
export function notebookCopy(lines: NotebookLine[], day: number, title: string): string {
  return [title, ...lines.map(l => copyLine(l, day))].join('\n')
}
export function highlightsCopy(items: EnteredDeath[], title: string): string {
  return [title, ...items.map(i => `${i.id} ${deathCopy(i.death)}`)].join('\n')
}

/** The start of an Ecuador day (UTC−5 all year), as an instant. */
export const dayStart = (iso: string) => `${iso}T05:00:00.000Z`
/** From when «Filas para resaltar» lists: the last time this person opened it, a day chosen, or the last 7 days. */
export type SinceChoice = { kind: 'last' } | { kind: 'week' } | { kind: 'day'; day: string }
export function highlightsSince(choice: SinceChoice, lastOpened: string | null, todayIsoDay: string): string {
  if (choice.kind === 'day') return dayStart(choice.day)
  if (choice.kind === 'last' && lastOpened && !Number.isNaN(Date.parse(lastOpened))) return new Date(lastOpened).toISOString()
  const week = new Date(Date.parse(`${todayIsoDay}T00:00:00Z`) - 6 * 86_400_000).toISOString().slice(0, 10)
  return dayStart(week)
}

/** A moment as people read it in Ecuador, day first: "05/10/2026 14:12". */
export function momentLabel(iso: string): string {
  const time = Date.parse(iso)
  if (Number.isNaN(time)) return ''
  const p = Object.fromEntries(
    new Intl.DateTimeFormat('en-GB', {
      timeZone: 'America/Guayaquil',
      day: '2-digit',
      month: '2-digit',
      year: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
      hourCycle: 'h23',
    })
      .formatToParts(time)
      .map(x => [x.type, x.value]),
  )
  return `${p.day}/${p.month}/${p.year} ${p.hour}:${p.minute}`
}
