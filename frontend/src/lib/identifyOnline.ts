/**
 * Identificar's «En línea (servidor)» (views/IdentifyView.vue): the photo made smaller here, read by the team's AI
 * Identifier on the server (POST api/ai/identify-photo, server/identify-photos.mjs; signed in only), and its answer
 * in the shape the tab's result already shows. The server runs the same head as the phone's models
 * (head_6fa6ab78), so the names and the subspecies leaves are the same.
 */
import { api, apiRoot, ApiError } from './api'
import { t } from './i18n'
import type { FieldSexAnswer } from './fieldSex'
import type { Candidate, Higher } from './identifyResult'

export type Mode = 'online' | 'phone'

/** The worker's lists: [name, p] pairs, most probable first (species and subspecies: 5; genus: 3). */
export interface ServerAnswer {
  species: [string, number][]
  subspecies: [string, number][]
  genus: [string, number][]
  /** The most probable tribes and subfamilies with their totals (placeholders left out); missing from an older server. */
  tribe?: [string, number][]
  subfamily?: [string, number][]
  /** Where the head records each listed species in Ecuador (null: not known on this server). */
  recorded?: Record<string, { East: boolean; West: boolean }> | null
  crop: string | null
  wingBox: [number, number, number, number] | null
  prior: string | null
  ms: number | null
  /** The general sex line (lib/fieldSex.ts): only for the six species it covers. */
  fieldSex?: FieldSexAnswer | null
}

/**
 * The mode used: online only when it can be (signed in, or a visitor when the server takes visitors' photos; with
 * signal); otherwise on the phone. With no choice kept, online when it can be (it needs no download).
 */
export function effectiveMode(choice: Mode | null, server: boolean, online: boolean): Mode {
  if (!server || !online) return 'phone'
  return choice === 'phone' ? 'phone' : 'online'
}

/**
 * The choice used when the person made none: a visitor whose phone already has a model keeps reading on the phone (their
 * photos never left it before); the team, and a visitor with nothing downloaded, start online (null). Until the page
 * has looked at the phone's models (`checked`), a visitor stays on the phone: a photo pasted meanwhile never leaves it.
 */
export function choiceOrDefault(choice: Mode | null, signedIn: boolean, phoneReady: boolean, checked = true): Mode | null {
  return choice ?? (!signedIn && (phoneReady || !checked) ? 'phone' : null)
}

/** The answer as the tab's ranking: the species, every listed genus's total, and the subspecies per species. */
export function serverRanking(a: ServerAnswer): { species: Candidate[]; genus: Map<string, number>; ssp: Map<string, Candidate[]>; higher: Higher } {
  const species = a.species.map(([name, p]) => ({ name, p }))
  const genus = new Map<string, number>(a.genus)
  // A species outside the genus list (rare: the genus list has 3) counts at least its own score.
  for (const c of species) {
    const g = c.name.split(' ')[0]
    if (!genus.has(g)) genus.set(g, species.filter(s => s.name.split(' ')[0] === g).reduce((x, s) => x + s.p, 0))
  }
  const ssp = new Map<string, Candidate[]>()
  for (const [name, p] of a.subspecies) {
    const sp = name.split(' ').slice(0, 2).join(' ')
    if (!species.some(c => c.name === sp)) continue
    ssp.set(sp, [...(ssp.get(sp) ?? []), { name, p }])
  }
  return { species, genus, ssp, higher: { tribe: new Map(a.tribe ?? []), subfamily: new Map(a.subfamily ?? []) } }
}

/** The photo at most `max` px on its longest side, upright (the browser applies the EXIF tag), as a canvas. */
export async function photoCanvas(blob: Blob, max = 1600): Promise<HTMLCanvasElement> {
  const bitmap = await createImageBitmap(blob, { imageOrientation: 'from-image' })
  const scale = Math.min(1, max / Math.max(bitmap.width, bitmap.height))
  const c = document.createElement('canvas')
  c.width = Math.max(1, Math.round(bitmap.width * scale))
  c.height = Math.max(1, Math.round(bitmap.height * scale))
  c.getContext('2d')!.drawImage(bitmap, 0, 0, c.width, c.height)
  bitmap.close?.()
  return c
}

/** A JPEG of the canvas as base64 (without the data: prefix). */
export function jpegBase64(c: HTMLCanvasElement, quality = 0.9) {
  return c.toDataURL('image/jpeg', quality).split(',')[1]
}

/**
 * The square shown as «your photo» beside the reference photos: the wings' box the server found (fractions of the
 * upright photo), padded to a square on the page's mat; the photo's centre when none was found.
 */
export function squareCrop(c: HTMLCanvasElement, box: ServerAnswer['wingBox'], size = 224) {
  const [x0, y0, x1, y1] = box ?? [0, 0, 1, 1]
  const w = (x1 - x0) * c.width
  const h = (y1 - y0) * c.height
  const side = box ? Math.max(w, h) : Math.min(c.width, c.height)
  const cx = box ? (x0 * c.width + w / 2) : c.width / 2
  const cy = box ? (y0 * c.height + h / 2) : c.height / 2
  const out = document.createElement('canvas')
  out.width = out.height = size
  const ctx = out.getContext('2d')!
  ctx.fillStyle = '#000'
  ctx.fillRect(0, 0, size, size)
  ctx.drawImage(c, cx - side / 2, cy - side / 2, side, side, 0, 0, size, size)
  return out.toDataURL('image/jpeg', 0.85)
}

/** What the person reads when the server cannot read the photo. */
export function onlineError(err: unknown): string {
  const status = err instanceof ApiError ? err.status : -1
  // 0: no answer (no signal, or the 150 s wait ran out: api() reports both the same way).
  if (status === 0) return t('Sin conexión con el servidor. Pruebe de nuevo o use «En este teléfono».')
  if (status === 401 || status === 403) return t('Su sesión terminó. Inicie sesión de nuevo o use «En este teléfono».')
  if (status === 400) return t('No se pudo leer la foto. Pruebe con otra.')
  if (status === 413) return t('La foto es demasiado grande. Pruebe con otra.')
  // A visitor's photo (identifyAsVisitor): the queue is full, or too many photos from this network.
  if (err instanceof ApiError && err.code === 'BUSY') return t('El servidor está ocupado. Pruebe de nuevo en un minuto.')
  if (status === 429) return t('Muchas fotos desde esta red. Espere unos minutos o use «En este teléfono».')
  if (status === 504) return t('El servidor tardó demasiado. Pruebe de nuevo o use «En este teléfono».')
  return t('El identificador del servidor no respondió. Pruebe de nuevo o use «En este teléfono».')
}

/** The server's reading of a photo (base64 JPEG) with the side of the Andes. */
export function identifyOnServer(photo: string, side: 'East' | 'West' | null): Promise<ServerAnswer> {
  return api<ServerAnswer>('ai/identify-photo', { method: 'POST', body: { photo, side }, signal: AbortSignal.timeout(150_000) })
}

/** A visitor's place in the server's queue (server/visitor-identify.mjs): 1 is the photo being read. */
export interface VisitorPlace {
  state: 'waiting' | 'running'
  place: number
  ahead: number
  waitS: number
}
interface VisitorTicket extends Partial<Omit<VisitorPlace, 'state'>> {
  ticket: string
  state: VisitorPlace['state'] | 'done' | 'failed'
  answer?: ServerAnswer
  status?: number
  error?: { code: string; message: string }
}

/**
 * Whether the server reads visitors' photos (ANON_SERVER_IDENTIFY, with the identifier installed): GET
 * api/visitor/identify answers { on: true, waitS, … }; off (or an older server) it answers 401 or 404.
 */
export async function visitorServer(): Promise<{ on: boolean; waitS?: number }> {
  try {
    const out = await api<{ on: boolean; waitS?: number }>('visitor/identify', { signal: AbortSignal.timeout(10_000) })
    return { on: out.on === true, waitS: out.waitS }
  } catch {
    return { on: false }
  }
}

const POLL_MS = 1500
const wait = (ms: number, signal?: AbortSignal) =>
  new Promise<void>((resolve, reject) => {
    const stop = () => (clearTimeout(timer), reject(signal!.reason))
    const timer = setTimeout(() => (signal?.removeEventListener('abort', stop), resolve()), ms)
    signal?.addEventListener('abort', stop, { once: true })
  })

/** Tickets of this page still in the queue: given up when the page closes (pagehide), so the next person moves up. */
const open = new Set<string>()
let leaving = false
if (typeof window !== 'undefined')
  window.addEventListener('pagehide', () => {
    leaving = true
    for (const id of open) fetch(`${apiRoot}api/visitor/identify/${id}`, { method: 'DELETE', keepalive: true, credentials: 'same-origin' }).catch(() => {})
    open.clear()
  })
// Back from the browser's page cache: the page lives on, and its later failures give their places up again.
if (typeof window !== 'undefined') window.addEventListener('pageshow', e => e.persisted && (leaving = false))

/**
 * A visitor's photo read by the server: in its queue (one photo at a time for everyone), its place told through
 * `onPlace` while it waits, its answer when read. `signal` gives the place up (Borrar fotos, a new photo). A ticket the
 * server dropped (the page was asleep too long) is queued once more.
 */
export async function identifyAsVisitor(photo: string, side: 'East' | 'West' | null, { onPlace, signal }: { onPlace?: (p: VisitorPlace) => void; signal?: AbortSignal } = {}): Promise<ServerAnswer> {
  let again = 1
  for (;;) {
    let t = await api<VisitorTicket>('visitor/identify', { method: 'POST', body: { photo, side }, signal })
    open.add(t.ticket)
    try {
      for (;;) {
        if (t.state === 'done' && t.answer) return t.answer
        if (t.state === 'failed') throw new ApiError(t.status ?? 503, t.error ?? { code: 'identifier_failed', message: '' })
        if (t.state === 'waiting' || t.state === 'running') onPlace?.({ state: t.state, place: t.place ?? 1, ahead: t.ahead ?? 0, waitS: t.waitS ?? 0 })
        await wait(POLL_MS, signal)
        t = await api<VisitorTicket>(`visitor/identify/${t.ticket}`, { signal })
      }
    } catch (err) {
      const gone = err instanceof ApiError && err.status === 404
      if (!gone || signal?.aborted || again-- <= 0) {
        if (!gone && !leaving) fetch(`${apiRoot}api/visitor/identify/${t.ticket}`, { method: 'DELETE', credentials: 'same-origin' }).catch(() => {})
        throw err
      }
    } finally {
      open.delete(t.ticket)
    }
  }
}
