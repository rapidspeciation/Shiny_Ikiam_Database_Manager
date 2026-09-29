/**
 * The Revisión tab: the shape of an issue as the server sends it (server/checks.mjs,
 * server/review.mjs) and the pure rules of its cards (crops, differing cells,
 * texts), kept apart from the components so they are tested without a browser.
 */

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
  related?: IssueRef[]
  fix?: { recordId: string; values: Record<string, unknown> }
  fixNote?: string
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
  task?: { type: string; from: string; to: string; files: string[]; text: string }
  group?: { key: string; label: string; size: number }
  choices?: string[]
  who?: string[]
  date?: string
  verdict: Verdict | null
  table?: SideBySide | null
  /** Applied and gone from the checks: shown from what was kept with the verdict. */
  resolved?: boolean
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

/** Status filter: the server's keys, the tab's words. */
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

/** The proposed change in words: "Sex → male", or the task. */
export function fixText(issue: Pick<Issue, 'fix' | 'fixNote' | 'task'>) {
  if (issue.task) return issue.task.text
  if (!issue.fix) return ''
  return (
    Object.entries(issue.fix.values)
      .map(([f, v]) => `${f} → ${shown(v)}`)
      .join(', ') + (issue.fixNote ? ` (${issue.fixNote})` : '')
  )
}

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
  const stem = (name: string) => name.replace(/\.[^.]+$/, '').replace(/\.[^.]+$/, '').toLowerCase()
  const jpgs = new Set(all.filter(p => !RAW.test(p.name)).map(p => stem(p.name)))
  return all.filter(p => !RAW.test(p.name) || !jpgs.has(stem(p.name)))
}

export const percent = (n: number) => `${Math.round(n * 100)} %`
