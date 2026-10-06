import { reactive, ref } from 'vue'
import { api, ApiError, csrfToken, pageId, requestId } from '../lib/api'
import { chunksFrom, preparePhoto, retryDelay, type ClutchPhoto } from '../lib/clutchPhotos'
import { t } from '../lib/i18n'

/**
 * The clutch photos being sent, for the whole page (they keep going when the
 * clutch or the tab changes): one at a time, so a weak signal carries one
 * photo well rather than several badly. Each is made small on the phone, sent
 * in chunks with its progress, and on a dropped connection tried again by
 * itself (2 s, 5 s, 10 s… and as soon as the phone is online again) from the
 * last chunk the server acknowledged; after eight tries it waits for Retry.
 */
export interface PhotoUpload {
  id: string
  recordId: string
  clutch: string
  day: string
  eventId: string | null
  note: string | null
  /** The thumbnail, shown while it goes. */
  preview: string
  status: 'preparing' | 'sending' | 'waiting' | 'failed' | 'done'
  sent: number
  total: number
  attempts: number
  error: string
}
interface Payload {
  data: Blob
  thumbBytes: number
}
const MAX_ATTEMPTS = 8

const uploads = reactive<PhotoUpload[]>([])
/** Bumped when a photo is stored: the timelines load again. */
const stored = ref(0)
const payloads = new Map<string, Payload>()
let running = false
let timer: ReturnType<typeof setTimeout> | undefined

if (typeof window !== 'undefined') {
  window.addEventListener('online', () => {
    for (const u of uploads) if (u.status === 'waiting') u.attempts = Math.max(0, u.attempts - 1)
    void run()
  })
  // Leaving the page with photos still going loses them: the browser asks first.
  window.addEventListener('beforeunload', e => {
    if (uploads.some(u => u.status !== 'done' && u.status !== 'failed')) e.preventDefault()
  })
}

/** One chunk, with its progress (XHR: fetch cannot tell how much of a body has gone). */
function putChunk(u: PhotoUpload, from: number, body: Blob): Promise<{ status: number; received: number | null; done?: boolean }> {
  return new Promise((resolve, reject) => {
    const xhr = new XMLHttpRequest()
    xhr.open('PUT', `api/clutches/photo-uploads/${encodeURIComponent(u.id)}?offset=${from}`)
    xhr.setRequestHeader('content-type', 'application/octet-stream')
    xhr.setRequestHeader('x-ithomiini-page', pageId)
    const csrf = csrfToken()
    if (csrf) xhr.setRequestHeader('x-csrf-token', csrf)
    xhr.withCredentials = true
    xhr.timeout = 120_000
    xhr.upload.onprogress = e => {
      if (e.lengthComputable) u.sent = Math.min(u.total, from + e.loaded)
    }
    xhr.onload = () => {
      let data: { received?: number | null; done?: boolean; error?: { code: string; message: string } } = {}
      try {
        data = JSON.parse(xhr.responseText || '{}')
      } catch {
        /* not JSON: a proxy's page */
      }
      if (xhr.status === 200 || xhr.status === 409) return resolve({ status: xhr.status, received: data.received ?? null, done: data.done })
      reject(new ApiError(xhr.status, data.error ?? { code: 'SERVER_ERROR', message: xhr.statusText }))
    }
    const offline = () => reject(new ApiError(0, { code: 'OFFLINE', message: t('Sin conexión con el servidor') }))
    xhr.onerror = offline
    xhr.ontimeout = offline
    xhr.send(body)
  })
}

/** Sends one photo from where the server stands, then stores it. */
async function send(u: PhotoUpload) {
  let payload = payloads.get(u.id)
  if (!payload) {
    u.status = 'failed'
    u.error = t('La foto ya no está en este teléfono: vuelve a elegirla')
    return
  }
  u.status = 'sending'
  // Where the server stands (a chunk may have arrived before the connection dropped).
  const state = await api<{ received: number | null; photo?: ClutchPhoto }>(`clutches/photo-uploads/${encodeURIComponent(u.id)}`)
  let at = state.photo ? u.total : (state.received ?? 0)
  // Chunk after chunk; a 409 says where the server stands, and the next one starts there.
  while (!state.photo && at < u.total) {
    const [from, to] = chunksFrom(at, u.total)[0]
    const out = await putChunk(u, from, payload.data.slice(from, to))
    if (out.done) break
    at = out.received ?? to
    u.sent = at
  }
  u.sent = u.total
  await api<{ photo: ClutchPhoto }>('clutches/photos', {
    method: 'POST',
    body: { requestId: u.id, recordId: u.recordId, day: u.day, eventId: u.eventId, note: u.note, thumbBytes: payload.thumbBytes, totalBytes: u.total },
  })
  payload = undefined
  payloads.delete(u.id)
  u.status = 'done'
  stored.value++
  setTimeout(() => {
    const i = uploads.indexOf(u)
    if (i >= 0 && u.status === 'done') {
      URL.revokeObjectURL(u.preview)
      uploads.splice(i, 1)
    }
  }, 4000)
}

async function run() {
  if (running) return
  running = true
  clearTimeout(timer)
  try {
    for (;;) {
      const next = uploads.find(u => u.status === 'waiting' && payloads.has(u.id))
      if (!next) break
      try {
        await send(next)
      } catch (e) {
        const code = e instanceof ApiError ? e.code : ''
        const status = e instanceof ApiError ? e.status : 0
        next.attempts++
        next.error = e instanceof Error ? e.message : String(e)
        // Refused for good (a bad photo, a clutch not found): no point trying again.
        const final = status >= 400 && status < 500 && !['UPLOAD_INCOMPLETE', 'RATE_LIMITED'].includes(code) && status !== 408
        if (final || next.attempts >= MAX_ATTEMPTS) next.status = 'failed'
        else {
          next.status = 'waiting'
          timer = setTimeout(() => void run(), retryDelay(next.attempts))
          break
        }
      }
    }
  } finally {
    running = false
  }
}

/** Photos chosen for a clutch: made small here, then sent one after another. */
async function add(files: File[], meta: { recordId: string; clutch: string; day: string; eventId: string | null; note: string | null }) {
  const items: PhotoUpload[] = files.map(() =>
    reactive({ id: requestId(), ...meta, preview: '', status: 'preparing' as const, sent: 0, total: 0, attempts: 0, error: '' }),
  )
  uploads.push(...items)
  for (const [i, file] of files.entries()) {
    const u = uploads.find(x => x.id === items[i].id)!
    try {
      const { full, thumb } = await preparePhoto(file)
      payloads.set(u.id, { data: new Blob([thumb, full], { type: 'application/octet-stream' }), thumbBytes: thumb.size })
      u.preview = URL.createObjectURL(thumb)
      u.total = thumb.size + full.size
      u.status = 'waiting'
      void run()
    } catch {
      u.status = 'failed'
      u.error = t('No se pudo leer la foto (¿es una imagen JPEG o PNG?)')
    }
  }
}
function retry(id: string) {
  const u = uploads.find(x => x.id === id)
  if (!u || u.status !== 'failed' || !payloads.has(id)) return
  u.status = 'waiting'
  u.attempts = 0
  u.error = ''
  void run()
}
function discard(id: string) {
  const i = uploads.findIndex(x => x.id === id)
  if (i < 0) return
  if (uploads[i].preview) URL.revokeObjectURL(uploads[i].preview)
  payloads.delete(id)
  uploads.splice(i, 1)
}

export function usePhotoUploads() {
  return { uploads, stored, add, retry, discard }
}
