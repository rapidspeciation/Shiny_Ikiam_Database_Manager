/**
 * The Revisión tab: the shape of an issue as the server sends it (server/checks.mjs,
 * server/review.mjs) and the pure rules of its cards (crops, differing cells,
 * texts), kept apart from the components so they are tested without a browser.
 */
import { intlLocale, tx, type Msg } from './i18n'

export type Box = [number, number, number, number]
export interface Photos {
  dorsal: string[]
  ventral: string[]
  other?: string[]
  /** File ID → its name and the wing box (0–1 of the photo), for the thumbnails. */
  files: Record<string, { name: string; wings?: Box }>
  envelope?: { fileId: string; name: string; bbox: Box; turned: number; aspect: number }
}
export interface IssueRef {
  sheet: string
  row: number
  recordId: string
  label: string
  field?: string
  value?: unknown
}
export interface Verdict {
  verdict: 'accepted' | 'rejected' | 'other' | 'pending' | 'applied'
  status: string
  value: string | null
  comment: string | null
  user: string
  at: string
  proposalId?: string
}
export interface SideBySide {
  fields: string[]
  compare: string[]
  rows: { sheet: string; row: number; recordId: string; label: string; values: Record<string, unknown> }[]
}
export interface Issue {
  id: string
  kind: string
  sheet: string
  row: number | null
  recordId: string | null
  label: string
  field: string
  value: unknown
  problem: string
  /** Descriptors of the server's texts, for the interface language (server/messages.mjs); older verdicts lack them. */
  problemMsg?: Msg
  related?: IssueRef[]
  fix?: { recordId: string; values: Record<string, unknown> }
  fixNote?: string
  fixNoteMsg?: Msg
  link?: string
  cam?: string
  photos?: Photos
  relatedPhotos?: Partial<Photos> & { cam: string }
  envelopeText?: string
  envelopeCamid?: string
  prediction?: {
    species: [string, number][]
    genus?: [string, number]
    sex?: { sex: string; confidence: number; supported: boolean }
  }
  ocr?: { field: string; read: string; lines: string[]; sheet: string }
  ai?: { recorded: string; predicted: string; confidence: number }
  strength?: 'fuerte' | 'media' | 'baja' | 'dudosa'
  curation?: { type: string; stratum: string | null; decision: string | null; note: string | null; decidedBy?: string }
  task?: { type: string; from: string; to: string; files: string[]; text: string; textMsg?: Msg }
  group?: { key: string; label: string; labelMsg?: Msg; size: number }
  choices?: string[]
  who?: string[]
  date?: string
  verdict: Verdict | null
  table?: SideBySide | null
  /** Applied and gone from the checks: shown from what was kept with the verdict. */
  resolved?: boolean
  /** When the checks first found it (server/findings.mjs). */
  firstSeen?: string
}
export interface ReviewPage {
  checkedAt: string
  total: number
  offset: number
  limit: number
  counts: Record<string, number>
  statuses: Record<string, number>
  kinds: Record<string, string>
  taskKinds: string[]
  sheets: string[]
  people: { name: string; n: number }[]
  agreed: { fixes: number; tasks: number }
  issues: Issue[]
}

/** Status filter: the server's keys, the tab's words (Spanish, the keys of lib/i18n.ts: shown through t()). */
export const STATUSES: { key: string; label: string }[] = [
  { key: 'pending', label: 'Pendiente' },
  { key: 'accepted', label: 'Aceptado' },
  { key: 'other', label: 'Otro valor' },
  { key: 'rejected', label: 'Rechazado' },
  { key: 'applied', label: 'Aplicado' },
  { key: 'all', label: 'Todos' },
]
export const VERDICT_WORD: Record<string, string> = {
  accepted: 'Aceptado',
  rejected: 'Rechazado',
  other: 'Otro valor',
  pending: 'Pendiente',
  applied: 'Aplicado',
}
export const STRENGTH_HINT: Record<string, string> = {
  fuerte: 'Lectura clara o ya revisada',
  media: 'Probable; mírala antes de aceptar',
  baja: 'La lectura suele fallar en estos casos',
  dudosa: 'La revisión anterior no lo pudo decidir',
}

/** A cached Drive photo: 400 px for thumbnails, 1600 px for the envelope and the full view. */
export const photoUrl = (fileId: string, width: 400 | 1600 = 400) => `api/photo/${encodeURIComponent(fileId)}?w=${width}`

/**
 * CSS that shows only `box` (0–1 of the photo) of an image filling a frame:
 * the frame gets the crop's shape, the image is scaled and moved inside it.
 * `aspect` is the photo's width / height. Turned 180°: the envelope was
 * photographed upside down, so the frame is turned (a crop is symmetric).
 */
export function cropStyle(box: Box, aspect: number, turned = 0) {
  const [x0, y0, x1, y1] = box.map(n => Math.min(Math.max(n, 0), 1)) as Box
  const w = Math.max(x1 - x0, 0.01)
  const h = Math.max(y1 - y0, 0.01)
  const pct = (n: number) => `${Math.round(n * 10000) / 100}%`
  return {
    frame: {
      aspectRatio: String(Math.round(((w * aspect) / h) * 1000) / 1000),
      ...(turned === 180 ? { transform: 'rotate(180deg)' } : {}),
    },
    image: { width: pct(1 / w), height: pct(1 / h), left: pct(-x0 / w), top: pct(-y0 / h) },
  }
}

const same = (a: unknown, b: unknown) =>
  String(a ?? '')
    .trim()
    .toLowerCase() ===
  String(b ?? '')
    .trim()
    .toLowerCase()

/**
 * Cells to highlight in the rows side by side: in the compared columns, those
 * whose value is not the same in every row; with one row, its issue column.
 */
export function differingCells(table: SideBySide, issueField?: string): Set<string> {
  const out = new Set<string>()
  for (const field of table.compare) {
    const values = table.rows.map(r => r.values[field])
    const differ = table.rows.length > 1 && values.some(v => !same(v, values[0]))
    table.rows.forEach((r, i) => {
      if (differ || (table.rows.length === 1 && field === issueField)) out.add(`${i}:${field}`)
    })
  }
  return out
}

export const shown = (v: unknown) => (v === null || v === undefined || v === '' ? '—' : String(v))

/** The proposed change in words: "Sex → male", or the task (in the interface language). */
export function fixText(issue: Pick<Issue, 'fix' | 'fixNote' | 'fixNoteMsg' | 'task'>) {
  if (issue.task) return tx(issue.task.text, issue.task.textMsg)
  if (!issue.fix) return ''
  return (
    Object.entries(issue.fix.values)
      .map(([f, v]) => `${f} → ${shown(v)}`)
      .join(', ') + (issue.fixNote ? ` (${tx(issue.fixNote, issue.fixNoteMsg)})` : '')
  )
}

/** The issue's problem, and its batch's label, in the interface language. */
export const problemText = (issue: Pick<Issue, 'problem' | 'problemMsg'>) => tx(issue.problem, issue.problemMsg)
export const groupLabel = (group: { label: string; labelMsg?: Msg }) => tx(group.label, group.labelMsg)

/** The column a different value goes to ("Otro valor"). */
export const otherField = (issue: Pick<Issue, 'fix' | 'field' | 'task'>) =>
  issue.task ? 'CAM correcto' : issue.fix ? Object.keys(issue.fix.values)[0] : issue.field

/** The photos of a card in viewing order (dorsal, ventral, other), for ←/→ in the full view. */
const RAW = /\.(orf|cr2|cr3|nef|arw|dng|raw|rw2)$/i
/**
 * The specimen's photos, dorsal then ventral. A camera's raw file (.ORF, .cr2…)
 * shows the same view as its JPG, so it is listed only when there is no JPG.
 */
export function photoList(photos?: Partial<Photos>) {
  if (!photos) return []
  const all = [...(photos.dorsal ?? []), ...(photos.ventral ?? []), ...(photos.other ?? [])].map(id => ({
    id,
    name: photos.files?.[id]?.name ?? id,
    wings: photos.files?.[id]?.wings,
  }))
  const stem = (name: string) =>
    name
      .replace(/\.[^.]+$/, '')
      .replace(/\.[^.]+$/, '')
      .toLowerCase()
  const jpgs = new Set(all.filter(p => !RAW.test(p.name)).map(p => stem(p.name)))
  return all.filter(p => !RAW.test(p.name) || !jpgs.has(stem(p.name)))
}

export const percent = (n: number) => `${Math.round(n * 100)} %`

/** An ISO day (2026-09-21) as people read it here: 21/09/2026. Other values as they are. */
export const dayFirst = (value: unknown) =>
  typeof value === 'string' && /^\d{4}-\d{2}-\d{2}$/.test(value) ? value.split('-').reverse().join('/') : value
/** A cell for the suggestion and solved lists: dates day first, empty as a dash. */
export const cellText = (value: unknown) => shown(dayFirst(value))
/** A time the server stamped (ISO), as 21/09/2026 14:05 in the interface's locale (both day first). */
export const stamp = (at: string | null | undefined, withTime = true) =>
  at
    ? new Date(at).toLocaleString(intlLocale(), {
        day: '2-digit',
        month: '2-digit',
        year: 'numeric',
        ...(withTime ? { hour: '2-digit', minute: '2-digit' } : {}),
      })
    : '—'
/** The row in Tablas (its sheet, found by its label), as a link that can open in a new tab. */
export const tablesLink = (sheet: string, label: string) =>
  `#/tablas?${new URLSearchParams({ hoja: sheet, ...(label ? { buscar: label } : {}) })}`

// Revisión → Sugerencias (server/suggestions/): read-only corrections with how sure they are.
export type Certainty = 'certain' | 'likely' | 'check'
export interface Suggestion {
  key: string
  source: string
  sheet: string
  row: number
  recordId: string
  label: string
  field: string
  current: unknown
  /** null: a person has to decide the value. */
  suggested: unknown
  certainty: Certainty
  reason: string
  reasonMsg?: Msg
  related?: IssueRef[]
  group?: string
  /** Done by hand in Google Sheets (a formula cell typed over): the app's proposals cannot write it. */
  manual?: boolean
  /** `suggested` is a formula, which a proposal of the assistant writes as such. */
  formula?: boolean
  firstSeen?: string
}
export interface SuggestionSource {
  id: string
  title: string
  describe: string
  counts: Record<Certainty | 'total', number>
  /** Listed group by group (sheet · column) with a heading and its counts. */
  byGroup?: boolean
}
export interface SuggestionPage {
  computedAt: string
  ms: number
  total: number
  offset: number
  limit: number
  sources: SuggestionSource[]
  sheets: string[]
  /** Per group of a source listed by group (sheet · column): how many, by certainty. */
  groups?: Record<string, Record<Certainty | 'total', number>>
  items: Suggestion[]
}
/** Certainties, surest first: the server's keys, the tab's words (Spanish keys of lib/i18n.ts) and colours. */
export const CERTAINTIES: { key: Certainty; label: string; hint: string; tone: string }[] = [
  { key: 'certain', label: 'Seguro', hint: 'Solo cambia la forma de escribirlo', tone: 'bg-emerald-100 text-emerald-900' },
  { key: 'likely', label: 'Probable', hint: 'Evidencia fuerte; igual una persona lo mira', tone: 'bg-sky-100 text-sky-900' },
  { key: 'check', label: 'Revisar', hint: 'Una pista: lo decide alguien que sepa', tone: 'bg-amber-100 text-amber-900' },
]
export const certaintyOf = (key: string) => CERTAINTIES.find(c => c.key === key) ?? CERTAINTIES[2]

// Revisión → Resueltos (server/findings.mjs).
export interface SolvedItem {
  type: 'check' | 'suggestion'
  key: string
  kind: string
  sheet: string | null
  row: number | null
  recordId: string | null
  field: string | null
  label: string | null
  /** A problem: the cell's value then; a suggestion: { current, suggested, certainty }. */
  value: unknown
  text: string | null
  textMsg?: Msg
  firstSeen: string
  solvedAt: string
  solved: {
    at?: string
    actionId?: string
    purpose?: string | null
    user?: string | null
    field?: string
    before?: unknown
    after?: unknown
    recordId?: string
    now?: unknown
    rowGone?: boolean
  }
}
export interface SolvedPage {
  total: number
  offset: number
  limit: number
  kinds: Record<string, number>
  open: Record<string, number>
  titles: { check: Record<string, string>; suggestion: Record<string, string> }
  items: SolvedItem[]
}

// Revisión → Alertas (server/alerts.mjs).
export interface CamRange {
  first: string
  last: string
  size: number
  used: number
  highest: string | null
  next: string | null
  left: number
  gaps: number
  lastUsed: { cam: string; date: string } | null
  active: boolean
  level: 'ok' | 'low' | 'done'
}
export interface CamPool {
  pool: string
  fields: string[]
  left: number
  ranges: CamRange[]
  fullRanges: number
  current: string | null
  level: 'ok' | 'low' | 'out'
}
export interface RuleRow {
  sheet: string
  row: number
  recordId: string
  label: string
  date: string | null
}
export interface Alert {
  id: string
  level: 'warn' | 'info'
  text: string
  textMsg?: Msg
  link?: string
}
/** An insectary butterfly preserved without its CAM or tube (server/preserved.mjs), and whom to ask. */
export interface MissingSample {
  recordId: string
  sheet: string
  row: number
  id: string
  species: string
  /** Of death (else of preservation), YYYY-MM-DD. */
  date: string | null
  kind: 'missing_sample' | 'preserved_na'
  missing: string[]
}
export interface AlertsData {
  computedAt: string
  thresholds: { camLeft: number; camShare: number }
  alerts: Alert[]
  camPools: CamPool[]
  preserveRule: {
    limit: number
    near: number
    locations: string[]
    reached: {
      species: string
      preserved: number
      reachedOn: string | null
      reachedRow: RuleRow
      after: number
      afterRows: RuleRow[]
      lastPreserved: string | null
      recent: boolean
      recentAfter: number
    }[]
    close: { species: string; preserved: number; left: number; lastPreserved: string | null }[]
  }
  missingSamples?: MissingSample[]
}
