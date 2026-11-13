/**
 * «Mis identificaciones», the part that lets an item load again (views/IdentifyView.vue): each result's photo (a JPEG
 * of at most 1600 px, as the server is sent it) and the result itself, kept on this phone only, in IndexedDB
 * (`ithomiini-identify-history`, store `results`, key `at`). The list's lines stay in localStorage
 * (lib/identifyLog.ts), each with the model versions that read it, so «Reprocesar» shows only when a newer model is in
 * use now. Nothing here is sent anywhere; sending a photo for training goes through the contributions outbox.
 * Without IndexedDB (some private modes) the records live in memory for the page's life.
 */
import { LOG_MAX } from './identifyLog'

/** The models that read a photo: the identifier (its head's file, or the head's SHA-256) and the in-hand sex model. */
export interface Versions {
  id: string | null
  sex: string | null
}

export interface HistoryRecord {
  at: string
  /** The photo as kept (JPEG, at most 1600 px; its EXIF dropped by the canvas). */
  photo: Blob
  /** The result as the page keeps it for a tab switch (its `feature` as base64), JSON. */
  result: string
  compared: string | null
  pick: string | null
  /** The day the photo was taken (from its EXIF, read before the canvas dropped it); the place is never kept. */
  takenAt: string | null
  country: string | null
  versions: Versions
}

export interface HistoryStore {
  get(at: string): Promise<HistoryRecord | null>
  put(rec: HistoryRecord): Promise<void>
  delete(at: string): Promise<void>
  keys(): Promise<string[]>
  clear(): Promise<void>
}

/** A version's day when its name carries one («B 2026-11-08», «small-SB-20261111»), as YYYYMMDD; null otherwise. */
export function versionDay(v: string): number | null {
  const m = /(20\d{2})-?(0[1-9]|1[0-2])-?(0[1-9]|[12]\d|3[01])(?!\d)/.exec(v)
  return m ? Number(m[1] + m[2] + m[3]) : null
}

/**
 * When this phone first saw each model version in use (localStorage `identify:models-seen`, version → ISO time). The
 * identifier's version is its head file (a hash, no date): which of two is newer is the order they came to this phone.
 */
const SEEN = 'identify:models-seen'
export function readSeen(): Record<string, string> {
  try {
    const v = JSON.parse(localStorage.getItem(SEEN) || '{}')
    return v && typeof v === 'object' && !Array.isArray(v) ? v : {}
  } catch {
    return {}
  }
}
/** Records the versions in use now (the first time each is seen). */
export function noteSeen(v: Partial<Versions>, at = new Date().toISOString()): void {
  const seen = readSeen()
  let changed = false
  for (const x of [v.id, v.sex]) if (x && !seen[x]) ((seen[x] = at), (changed = true))
  if (!changed) return
  try {
    localStorage.setItem(SEEN, JSON.stringify(seen))
  } catch {
    /* private mode: no order known */
  }
}

/**
 * `a` is known to be older than `b`: both named, different, and either both carry a day (a's earlier) or this phone
 * saw `a` first. Unknown (no version kept, never seen) or newer: false.
 */
export function olderThan(a: string | null | undefined, b: string | null | undefined, seen: Record<string, string>): boolean {
  if (!a || !b || a === b) return false
  const da = versionDay(a)
  const db = versionDay(b)
  if (da != null && db != null) return da < db
  return !!seen[a] && !!seen[b] && seen[a] < seen[b]
}

/**
 * «Reprocesar»: a newer identifier or sex model reads photos now than the one that read this item. Only when known
 * (a server rolled back to an older model, or a version never recorded, shows no button).
 */
export function outdated(saved: Partial<Versions> | null | undefined, now: Versions, seen: Record<string, string> = readSeen()): boolean {
  return olderThan(saved?.id, now.id, seen) || olderThan(saved?.sex, now.sex, seen)
}

export function memoryHistory(): HistoryStore & { items: Map<string, HistoryRecord> } {
  const items = new Map<string, HistoryRecord>()
  return {
    items,
    get: async at => items.get(at) ?? null,
    put: async rec => void items.set(rec.at, rec),
    delete: async at => void items.delete(at),
    keys: async () => [...items.keys()],
    clear: async () => items.clear(),
  }
}

const DB = 'ithomiini-identify-history'
const STORE = 'results'
const done = <T>(r: IDBRequest<T>) =>
  new Promise<T>((ok, no) => {
    r.onsuccess = () => ok(r.result)
    r.onerror = () => no(r.error)
  })
function open(): Promise<IDBDatabase> {
  const r = indexedDB.open(DB, 1)
  r.onupgradeneeded = () => {
    if (!r.result.objectStoreNames.contains(STORE)) r.result.createObjectStore(STORE, { keyPath: 'at' })
  }
  return done(r)
}

let shared: Promise<HistoryStore> | null = null
/** The phone's store (one per page). */
export function historyStore(): Promise<HistoryStore> {
  shared ??= (async () => {
    let db: IDBDatabase
    try {
      if (typeof indexedDB === 'undefined') throw new Error('no IndexedDB')
      db = await open()
    } catch {
      return memoryHistory()
    }
    const run = async <T>(mode: IDBTransactionMode, op: (s: IDBObjectStore) => IDBRequest<T>): Promise<T> => {
      try {
        return await done(op(db.transaction(STORE, mode).objectStore(STORE)))
      } catch {
        db = await open()
        return done(op(db.transaction(STORE, mode).objectStore(STORE)))
      }
    }
    return {
      get: async at => ((await run('readonly', s => s.get(at))) as HistoryRecord | undefined) ?? null,
      put: async rec => void (await run('readwrite', s => s.put(rec))),
      delete: async at => void (await run('readwrite', s => s.delete(at))),
      keys: async () => (await run('readonly', s => s.getAllKeys())).map(String),
      clear: async () => void (await run('readwrite', s => s.clear())),
    }
  })()
  return shared
}

/** Records whose line left the list (the list keeps LOG_MAX): their photos go too. */
export async function prune(store: HistoryStore, keep: string[]): Promise<void> {
  const wanted = new Set(keep.slice(0, LOG_MAX))
  for (const at of await store.keys()) if (!wanted.has(at)) await store.delete(at)
}

/** A canvas as a JPEG Blob. */
export function canvasJpeg(c: HTMLCanvasElement, quality = 0.9): Promise<Blob | null> {
  return new Promise(ok => c.toBlob(b => ok(b), 'image/jpeg', quality))
}

/** A base64 JPEG (no data: prefix) as a Blob. */
export function base64Jpeg(b64: string): Blob {
  return new Blob([Uint8Array.from(atob(b64), c => c.charCodeAt(0))], { type: 'image/jpeg' })
}

/** «Copiar la foto»: the clipboard takes PNG everywhere, so the photo is drawn and copied as one. */
export async function copyPhoto(blob: Blob): Promise<boolean> {
  try {
    const bitmap = await createImageBitmap(blob)
    const c = document.createElement('canvas')
    c.width = bitmap.width
    c.height = bitmap.height
    c.getContext('2d')!.drawImage(bitmap, 0, 0)
    bitmap.close?.()
    const png = await new Promise<Blob | null>(ok => c.toBlob(b => ok(b), 'image/png'))
    if (!png || typeof ClipboardItem === 'undefined') return false
    await navigator.clipboard.write([new ClipboardItem({ 'image/png': png })])
    return true
  } catch {
    return false
  }
}

/** «Descargar la foto»: the kept JPEG, named by the species and the time. */
export function downloadPhoto(blob: Blob, name: string, at: string) {
  const a = document.createElement('a')
  const url = URL.createObjectURL(blob)
  a.href = url
  a.download = `${name.replace(/[^\p{L}\d]+/gu, '_')}_${at.slice(0, 16).replace(/[-:T]/g, '')}.jpg`
  document.body.append(a)
  a.click()
  a.remove()
  setTimeout(() => URL.revokeObjectURL(url), 10_000)
}
