import { displayValue } from './cells'
import type { CellValue, Field } from './types'

type FieldType = Field['type']

/**
 * "Digitalizar cuaderno": the shapes the server sends for a photographed
 * notebook page (server/notebook-jobs.mjs) and the small rules the review
 * screen follows (bands on the photo, cell colours, choices, texts).
 */
export type Kind = 'stocks' | 'emergence' | 'deaths' | 'crispr'
export const KINDS: { id: Kind | 'auto'; label: string; sheet?: string }[] = [
  { id: 'auto', label: 'Detectar por los encabezados' },
  { id: 'stocks', label: 'Posturas', sheet: 'Insectary_stocks' },
  { id: 'emergence', label: 'Emergidos', sheet: 'Insectary_data' },
  { id: 'deaths', label: 'Muertes', sheet: 'Insectary_data' },
  { id: 'crispr', label: 'CRISPR', sheet: 'CRISPR' },
]
/** The notebook a sheet's pages usually come from (the shortcut from Tablas). */
export const kindForSheet = (sheet: string): Kind | 'auto' =>
  ({ Insectary_stocks: 'stocks', Insectary_data: 'emergence', CRISPR: 'crispr' })[sheet] as Kind | undefined ?? 'auto'

export type JobStatus = 'queued' | 'reading' | 'ready' | 'error' | 'done' | 'discarded'
/** fill: empty in the sheet · conflict: the sheet says otherwise · new: a new row · keep: only the sheet has it. */
export type CellStatus = 'empty' | 'keep' | 'same' | 'fill' | 'conflict' | 'new' | 'formula' | 'error' | 'unread'
export type LineStatus = 'match' | 'new' | 'missing' | 'ambiguous' | 'duplicate' | 'nokey' | 'crossed'

export interface ReviewCell {
  value: CellValue
  before: CellValue
  status: CellStatus
  confidence: number
  doubt: boolean
  alternatives: CellValue[]
  edited: boolean
  include: boolean
  formula: boolean
  message: string | null
  write?: string
  mismatch?: boolean
}
export interface ReviewLine {
  n: number
  y: number | null
  raw: string
  crossed: boolean
  status: LineStatus
  message: string
  recordId: string | null
  row: number | null
  label: string
  cells: Record<string, ReviewCell>
  changes: number
  picked: boolean
  applied: boolean
  rowError?: string
}
export interface JobWarning {
  kind: 'photo' | 'keys' | 'reused'
  jobId: string
  at?: string
  by?: string
  status?: JobStatus
  keys?: string[]
  count?: number
}
export interface Job {
  id: string
  status: JobStatus
  requestedKind: Kind | 'auto'
  kind: Kind | null
  label: string
  sheet: string | null
  attachmentId: string
  name: string
  owner: string
  error: string | null
  createdAt: string
  updatedAt: string
  durationMs: number | null
  model: string | null
  threadId: string | null
  proposalId: string | null
  proposalStatus: string | null
  lines: number
  keys: string[]
  appliedLines: number[]
  counts?: { lines: number; rows: number; fills: number; conflicts: number; doubts: number; errors: number; created: number; same: number }
  warnings: JobWarning[]
}
export interface JobDetail extends Job {
  year: number
  yearSource: 'person' | 'page' | 'inferred'
  rotate: number
  headers: string[]
  other: string
  keyFields: string[]
  fields: string[]
  types: Record<string, Field['type']>
  options: Record<string, string[]>
  reviewLines: ReviewLine[]
}

/** The size a photo is shrunk to before upload: text stays legible and the upload small. */
export function fitSize(width: number, height: number, max = 2400) {
  const scale = Math.min(1, max / Math.max(width, height))
  return { width: Math.round(width * scale), height: Math.round(height * scale) }
}

/**
 * The band of each line on the photo (fractions of its height): from half-way
 * to the line above to half-way to the line below. A line without a position
 * gets one between its neighbours.
 */
export function bands(lines: { n: number; y: number | null }[]) {
  const ys = lines.map(l => (l.y === null || !Number.isFinite(l.y) ? null : l.y))
  const known = ys.map((y, i) => [i, y] as const).filter((p): p is readonly [number, number] => p[1] !== null)
  if (!known.length) return []
  const filled = ys.map((y, i) => {
    if (y !== null) return y
    const before = [...known].reverse().find(([j]) => j < i)
    const after = known.find(([j]) => j > i)
    if (before && after) return before[1] + ((after[1] - before[1]) * (i - before[0])) / (after[0] - before[0])
    const step = known.length > 1 ? (known.at(-1)![1] - known[0][1]) / (known.at(-1)![0] - known[0][0] || 1) : 0.03
    return before ? before[1] + step * (i - before[0]) : after![1] - step * (after![0] - i)
  })
  return lines.map((line, i) => {
    const y = filled[i]
    const up = i > 0 ? (y - filled[i - 1]) / 2 : i + 1 < filled.length ? (filled[i + 1] - y) / 2 : 0.015
    const down = i + 1 < filled.length ? (filled[i + 1] - y) / 2 : up
    const half = (v: number) => Math.min(0.04, Math.max(0.006, Math.abs(v)))
    return { n: line.n, top: Math.max(0, y - half(up)), bottom: Math.min(1, y + half(down)) }
  })
}

/** The class of a reviewed cell in the grid (colours in style.css). */
export function cellClass(cell: ReviewCell | undefined): string {
  if (!cell) return ''
  if (cell.status === 'error') return 'nb-error'
  if (cell.doubt && ['fill', 'conflict', 'new', 'unread'].includes(cell.status)) return 'nb-doubt'
  if (cell.status === 'unread') return 'nb-doubt'
  if (cell.status === 'conflict') return 'nb-conflict'
  if (cell.status === 'fill' || cell.status === 'new') return 'nb-fill'
  // A formula cell whose result differs from the notebook (e.g. =12+15 against 30): not written, pointed out.
  if (cell.status === 'formula') return cell.mismatch ? 'nb-formula nb-mismatch' : 'nb-formula'
  if (cell.status === 'keep') return 'nb-keep'
  return ''
}

const show = (value: CellValue | undefined, type: FieldType) => displayValue(value ?? null, { key: '', type })

/** What a cell's hover (and the bar under the grid) says about it. */
export function cellTitle(field: string, cell: ReviewCell | undefined, type: FieldType): string {
  if (!cell) return ''
  const book = show(cell.value, type) || 'vacío'
  const sheet = show(cell.before, type) || 'vacío'
  const parts = [
    cell.status === 'conflict'
      ? `hoja: ${sheet} → cuaderno: ${book}`
      : cell.status === 'fill'
        ? `Celda vacía en la hoja → cuaderno: ${book}`
        : cell.status === 'new'
          ? `Fila nueva: ${book}`
          : cell.status === 'keep'
            ? `El cuaderno no dice nada; la hoja tiene ${sheet}`
            : cell.status === 'same'
              ? 'Igual en la hoja y en el cuaderno'
              : cell.status === 'unread'
                ? 'No se pudo leer en la foto'
                : '',
    cell.doubt ? `Lectura dudosa (${Math.round(cell.confidence * 100)} %): confírmala o elige otra` : '',
    cell.alternatives.length ? `Otras lecturas: ${cell.alternatives.map(a => show(a, type)).join(', ')}` : '',
    cell.message ?? '',
    cell.edited ? 'Corregido a mano' : '',
  ]
  return parts.filter(Boolean).join(' · ') || field
}

/** The dropdown of a cell: its reading, the other readings, the sheet's value, then the column's list. */
export function choicesFor(cell: ReviewCell | undefined, type: FieldType, options: string[] = []): string[] {
  if (!cell) return options
  const first = [cell.value, ...cell.alternatives, cell.before].map(v => show(v, type)).filter(Boolean)
  return [...new Set([...first, ...options])]
}

/** A line's state as the grid's "Hoja" column shows it. */
export function lineState(line: ReviewLine): { text: string; tone: 'ok' | 'new' | 'bad' | 'off' | 'done' } {
  if (line.applied && !line.changes) return { text: 'aplicada', tone: 'done' }
  if (line.status === 'crossed') return { text: 'tachada', tone: 'off' }
  if (line.status === 'new') return { text: 'nueva', tone: 'new' }
  if (line.status === 'match') return { text: `fila ${line.row}`, tone: line.changes ? 'ok' : 'off' }
  return { text: { missing: 'no está', ambiguous: 'varias filas', duplicate: 'repetida', nokey: 'sin ID' }[line.status] ?? line.status, tone: 'bad' }
}

/** A job's state for the strip of pages (subiendo is a local upload, before the job exists). */
export function jobState(job: Job, now = Date.now()): { text: string; tone: 'busy' | 'ready' | 'bad' | 'done' | 'off' } {
  const seconds = Math.max(0, Math.round((now - Date.parse(job.updatedAt)) / 1000))
  if (job.status === 'queued') return { text: 'En cola', tone: 'busy' }
  if (job.status === 'reading') return { text: `Leyendo… ${seconds} s`, tone: 'busy' }
  if (job.status === 'error') return { text: 'Error al leer', tone: 'bad' }
  if (job.status === 'discarded') return { text: 'Descartada', tone: 'off' }
  if (job.status === 'done')
    return { text: job.appliedLines.length ? `Aplicada (${job.appliedLines.length} filas)` : 'Cerrada', tone: 'done' }
  const rows = job.counts?.rows
  return { text: rows === undefined ? 'Lista para revisar' : `Lista · ${rows} ${rows === 1 ? 'fila' : 'filas'} con cambios`, tone: 'ready' }
}

const day = (iso?: string) =>
  iso ? new Date(iso).toLocaleString('es-EC', { day: 'numeric', month: 'numeric', hour: '2-digit', minute: '2-digit' }) : ''

/** Why a page may be a repeat. */
export function warningText(w: JobWarning): string {
  const who = w.by ? ` por ${w.by}` : ''
  if (w.kind === 'photo') return `Esta foto ya se subió el ${day(w.at)}${who}.`
  if (w.kind === 'reused') return 'Se usó la lectura anterior de esta misma foto (no se volvió a leer).'
  const keys = (w.keys ?? []).join(', ')
  return `${w.count} ${w.count === 1 ? 'línea ya estaba' : 'líneas ya estaban'} en otra página digitalizada el ${day(w.at)}${who}: ${keys}${(w.count ?? 0) > (w.keys?.length ?? 0) ? '…' : ''}`
}

/** The next page to review after this one: the next still open in the strip, else the first. */
export function nextJob(jobs: Job[], current: string | null): string | null {
  const open = jobs.filter(j => ['queued', 'reading', 'ready', 'error'].includes(j.status))
  const at = open.findIndex(j => j.id === current)
  const after = open.slice(at + 1).find(j => j.id !== current) ?? open.find(j => j.id !== current)
  return after?.id ?? null
}

/** The first and last key of a page, for the history ("994(1) – 994(7)"). */
export const keyRange = (keys: string[]) => (keys.length ? (keys.length === 1 ? keys[0] : `${keys[0]} – ${keys.at(-1)}`) : '')

/** Merges the corrections waiting to be sent, so a paste or a fill is one request. */
export function mergeEdits(
  into: Record<number, Record<string, string | null>>,
  line: number,
  field: string,
  value: string | null,
) {
  into[line] = { ...(into[line] ?? {}), [field]: value }
  return into
}
