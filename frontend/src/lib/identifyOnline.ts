/**
 * Identificar's «En línea (servidor)» (views/IdentifyView.vue): the photo made smaller here, read by the team's AI
 * Identifier on the server (POST api/ai/identify-photo, server/identify-photos.mjs; signed in only), and its answer
 * in the shape the tab's result already shows. The server runs the same head as the phone's models
 * (head_b8127dbf), so the names and the subspecies leaves are the same.
 */
import { api, ApiError } from './api'
import { t } from './i18n'
import type { Candidate } from './identifyResult'

export type Mode = 'online' | 'phone'

/** The worker's lists: [name, p] pairs, most probable first (species and subspecies: 5; genus: 3). */
export interface ServerAnswer {
  species: [string, number][]
  subspecies: [string, number][]
  genus: [string, number][]
  crop: string | null
  wingBox: [number, number, number, number] | null
  prior: string | null
  ms: number | null
}

/**
 * The mode used: online only when it can be (signed in, with signal); otherwise on the phone. With no choice kept,
 * online when it can be (it needs no download).
 */
export function effectiveMode(choice: Mode | null, signedIn: boolean, online: boolean): Mode {
  if (!signedIn || !online) return 'phone'
  return choice === 'phone' ? 'phone' : 'online'
}

/** The answer as the tab's ranking: the species, every listed genus's total, and the subspecies per species. */
export function serverRanking(a: ServerAnswer): { species: Candidate[]; genus: Map<string, number>; ssp: Map<string, Candidate[]> } {
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
  return { species, genus, ssp }
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
  return t('El identificador del servidor no respondió. Pruebe de nuevo o use «En este teléfono».')
}

/** The server's reading of a photo (base64 JPEG) with the side of the Andes. */
export function identifyOnServer(photo: string, side: 'East' | 'West' | null): Promise<ServerAnswer> {
  return api<ServerAnswer>('ai/identify-photo', { method: 'POST', body: { photo, side }, signal: AbortSignal.timeout(150_000) })
}
