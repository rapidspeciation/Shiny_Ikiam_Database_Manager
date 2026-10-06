import type { ClutchEvent } from './clutches'

/**
 * Photos of clutches (only in the app: server/clutch-photos.mjs). The phone
 * makes them small before sending: at most 2560 px on the longest side, JPEG
 * quality 85, turned as the camera held it (the browser applies the EXIF
 * orientation when it decodes; the canvas writes no EXIF, so the GPS and the
 * rest stay on the phone), plus a 480 px thumbnail. Both travel as one upload,
 * the thumbnail first, in chunks the server acknowledges one by one.
 */
export const PHOTO_EDGE = 2560
export const PHOTO_QUALITY = 0.85
export const THUMB_EDGE = 480
export const THUMB_QUALITY = 0.8
/** One request's bytes: small enough to get through a weak signal, resumed from the last one acknowledged. */
export const CHUNK = 256 * 1024

export interface ClutchPhoto {
  id: string
  recordId: string
  clutch: string | null
  day: string
  eventId: string | null
  note: string | null
  actor: string
  username: string | null
  name: string | null
  width: number
  height: number
  bytes: number
  thumbBytes: number
  createdAt: string
}

/** Captions the team gives a photo (English, as the notes): quick buttons. */
export const PHOTO_CAPTIONS = ['Dead larva', 'Sick larva', 'Eggs', 'Larvae', 'Pupa', 'Plant', 'Fungi on eggs', 'Missing larvae']

/** A size fitted within `edge` on its longest side, never enlarged. */
export function fitWithin(width: number, height: number, edge: number): { width: number; height: number } {
  const scale = Math.min(1, edge / Math.max(width, height, 1))
  return { width: Math.max(1, Math.round(width * scale)), height: Math.max(1, Math.round(height * scale)) }
}

/** Where each chunk of an upload starts and ends: [from, to) pieces of `size` bytes from `offset`. */
export function chunksFrom(offset: number, total: number, size = CHUNK): [number, number][] {
  const out: [number, number][] = []
  for (let at = Math.max(0, offset); at < total; at += size) out.push([at, Math.min(total, at + size)])
  return out
}

/** How long to wait before trying again after `attempt` failures: 2 s, 5 s, 10 s, 20 s, then every 30 s. */
export const retryDelay = (attempt: number) => [2000, 5000, 10_000, 20_000][attempt - 1] ?? 30_000

/** The photo's address: its thumbnail or the whole photo. */
export const photoUrl = (id: string, size: 'thumb' | 'full') => `api/clutches/photos/${encodeURIComponent(id)}?size=${size}`

/** "1.2 MB", "340 kB": a photo's weight. */
export function sizeText(bytes: number): string {
  return bytes >= 1024 * 1024 ? `${(bytes / 1024 / 1024).toFixed(1)} MB` : `${Math.max(1, Math.round(bytes / 1024))} kB`
}

/** What a photo is linked to, in short: its event ("−1 disappeared") or the day. */
export function linkedEvent(photo: Pick<ClutchPhoto, 'eventId'>, events: ClutchEvent[]): ClutchEvent | null {
  return photo.eventId ? (events.find(e => e.id === photo.eventId) ?? null) : null
}

/** Draws a decoded image on a canvas of the size given and encodes it as JPEG. */
function encode(source: CanvasImageSource, width: number, height: number, quality: number): Promise<Blob> {
  const canvas = document.createElement('canvas')
  canvas.width = width
  canvas.height = height
  const ctx = canvas.getContext('2d')
  if (!ctx) return Promise.reject(new Error('canvas'))
  ctx.imageSmoothingQuality = 'high'
  ctx.drawImage(source, 0, 0, width, height)
  return new Promise((resolve, reject) => canvas.toBlob(b => (b ? resolve(b) : reject(new Error('encode'))), 'image/jpeg', quality))
}

/** The picture decoded upright (the EXIF turn applied), as a bitmap or, on older browsers, an image. */
async function decode(file: Blob): Promise<{ source: CanvasImageSource; width: number; height: number; close: () => void }> {
  if (typeof createImageBitmap === 'function') {
    try {
      const bitmap = await createImageBitmap(file, { imageOrientation: 'from-image' })
      return { source: bitmap, width: bitmap.width, height: bitmap.height, close: () => bitmap.close() }
    } catch {
      /* the image element below */
    }
  }
  const url = URL.createObjectURL(file)
  const img = new Image()
  img.decoding = 'async'
  img.src = url
  await img.decode()
  return { source: img, width: img.naturalWidth, height: img.naturalHeight, close: () => URL.revokeObjectURL(url) }
}

/** A photo from the camera or the gallery made ready to send: the photo and its thumbnail, both JPEG, upright, without EXIF. */
export async function preparePhoto(file: Blob): Promise<{ full: Blob; thumb: Blob; width: number; height: number }> {
  const image = await decode(file)
  try {
    const size = fitWithin(image.width, image.height, PHOTO_EDGE)
    const small = fitWithin(image.width, image.height, THUMB_EDGE)
    const full = await encode(image.source, size.width, size.height, PHOTO_QUALITY)
    const thumb = await encode(image.source, small.width, small.height, THUMB_QUALITY)
    return { full, thumb, width: size.width, height: size.height }
  } finally {
    image.close()
  }
}
