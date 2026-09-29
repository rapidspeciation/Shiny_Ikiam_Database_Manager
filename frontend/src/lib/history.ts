import type { HistoryChange } from './types'

/**
 * The purposes of saves (server/history.mjs PURPOSES): the tab or flow a save
 * came from, with the label and colours of its card in the Historial.
 */
export const PURPOSES: Record<string, { label: string; tone: string }> = {
  colecta: { label: 'Colecta', tone: 'bg-emerald-100 text-emerald-800' },
  monitoreo: { label: 'Monitoreo', tone: 'bg-teal-100 text-teal-800' },
  muertes: { label: 'Muertes', tone: 'bg-stone-200 text-stone-800' },
  emergidos: { label: 'Emergidos', tone: 'bg-amber-100 text-amber-800' },
  clutches: { label: 'Clutches', tone: 'bg-orange-100 text-orange-800' },
  tubos: { label: 'Tubos', tone: 'bg-sky-100 text-sky-800' },
  tablas: { label: 'Tablas', tone: 'bg-indigo-100 text-indigo-800' },
  revision: { label: 'Revisión', tone: 'bg-violet-100 text-violet-800' },
  cambio_id: { label: 'Cambio de ID', tone: 'bg-fuchsia-100 text-fuchsia-800' },
  asistente: { label: 'Asistente', tone: 'bg-purple-100 text-purple-800' },
  deshacer: { label: 'Deshacer', tone: 'bg-rose-100 text-rose-800' },
  sheets: { label: 'Google Sheets', tone: 'bg-green-100 text-green-800' },
  importacion: { label: 'Importación', tone: 'bg-slate-200 text-slate-800' },
}

/** The app's tabs (router paths) whose saves carry that tab as their purpose. */
const TAB_PURPOSE: Record<string, string> = {
  tablas: 'tablas',
  colecta: 'colecta',
  monitoreo: 'monitoreo',
  muertes: 'muertes',
  tubos: 'tubos',
  emergidos: 'emergidos',
  clutches: 'clutches',
  posturas: 'clutches',
  revision: 'revision',
}

/** "#/muertes?x=1" → "muertes"; undefined for a page that is no data-entry tab. */
export function purposeFromHash(hash: string): string | undefined {
  const tab = /^#?\/?([^/?#]+)/.exec(hash)?.[1]
  return tab ? TAB_PURPOSE[tab.toLowerCase()] : undefined
}

/** The purpose most of a save's changes were made with (one save may hold changes typed in two tabs). */
export function mainPurpose(purposes: (string | undefined)[]): string | undefined {
  const counts = new Map<string, number>()
  for (const p of purposes) if (p) counts.set(p, (counts.get(p) ?? 0) + 1)
  let best: string | undefined
  for (const [p, n] of counts) if (!best || n > counts.get(best)!) best = p
  return best
}

const MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
const clock = new Intl.DateTimeFormat('en-US', {
  timeZone: 'America/Guayaquil',
  year: 'numeric',
  month: 'numeric',
  day: 'numeric',
  hour: '2-digit',
  minute: '2-digit',
  hourCycle: 'h23',
})
function partsOf(iso: string) {
  const parts = Object.fromEntries(clock.formatToParts(new Date(iso)).map(p => [p.type, p.value]))
  return {
    day: `${Number(parts.day)}-${MONTHS[Number(parts.month) - 1]}-${String(parts.year).slice(-2)}`,
    time: `${parts.hour}:${parts.minute}`,
  }
}

/** "28-Sep-26 14:05", in Ecuador's time, day first like the rest of the app. */
export function formatWhen(iso: string): string {
  if (Number.isNaN(Date.parse(iso))) return ''
  const { day, time } = partsOf(iso)
  return `${day} ${time}`
}

/** "28-Sep-26 14:05–14:32", or "27-Sep-26 23:50 – 28-Sep-26 00:10" across midnight. */
export function timeRange(start: string, end: string): string {
  if (Number.isNaN(Date.parse(start)) || Number.isNaN(Date.parse(end))) return ''
  const a = partsOf(start)
  const b = partsOf(end)
  if (a.day !== b.day) return `${a.day} ${a.time} – ${b.day} ${b.time}`
  return a.time === b.time ? `${a.day} ${a.time}` : `${a.day} ${a.time}–${b.time}`
}

/** The changes of one row in a save, in the order they were made. */
export interface RowChanges {
  recordId: string
  label: string
  sheet: string
  row: number
  isNew: boolean
  changes: HistoryChange[]
}

/** Changes grouped by the row they changed (a row keeps the place of its first change). */
export function rowsOf(changes: HistoryChange[]): RowChanges[] {
  const rows = new Map<string, RowChanges>()
  for (const c of changes) {
    let row = rows.get(c.recordId)
    if (!row) {
      row = { recordId: c.recordId, label: c.label || `${c.sheet} fila ${c.row}`, sheet: c.sheet, row: c.row, isNew: false, changes: [] }
      rows.set(c.recordId, row)
    }
    row.isNew ||= !!c.isNew
    row.changes.push(c)
  }
  return [...rows.values()]
}

/** The save a link names: #/historial?grupo=<id> or ?accion=<actionId>. */
export function linkedSave(query: Record<string, unknown>): string | null {
  const pick = (v: unknown) => (Array.isArray(v) ? v[0] : v)
  const id = pick(query.grupo) ?? pick(query.accion)
  return typeof id === 'string' && id.trim() ? id.trim() : null
}
