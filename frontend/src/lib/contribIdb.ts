/**
 * The outbox's storage in the app (lib/contribOutbox.ts OutboxStore): IndexedDB database `ithomiini-identify`, store
 * `outbox`, key `id`. Apart from the models' Cache Storage (lib/offlineEngine.ts), so neither cleans up the other.
 * Without IndexedDB (an old browser, some private modes) the items live in memory: they are sent while the page is
 * open, and `persistent` says so.
 */
import { memoryStore, type Item, type OutboxStore } from './contribOutbox'

const DB = 'ithomiini-identify'
const STORE = 'outbox'

const done = <T>(r: IDBRequest<T>) =>
  new Promise<T>((ok, no) => {
    r.onsuccess = () => ok(r.result)
    r.onerror = () => no(r.error)
  })

function open(): Promise<IDBDatabase> {
  const r = indexedDB.open(DB, 1)
  r.onupgradeneeded = () => {
    if (!r.result.objectStoreNames.contains(STORE)) r.result.createObjectStore(STORE, { keyPath: 'id' })
  }
  return done(r)
}

export async function idbStore(): Promise<OutboxStore & { persistent: boolean }> {
  let db: IDBDatabase
  try {
    if (typeof indexedDB === 'undefined') throw new Error('no IndexedDB')
    db = await open()
  } catch {
    return { ...memoryStore(), persistent: false }
  }
  const tx = (mode: IDBTransactionMode) => db.transaction(STORE, mode).objectStore(STORE)
  return {
    persistent: true,
    all: () => done(tx('readonly').getAll() as IDBRequest<Item[]>),
    put: async item => void (await done(tx('readwrite').put(item))),
    delete: async id => void (await done(tx('readwrite').delete(id))),
  }
}

/** Asked once, on the first item kept: the browser then does not clear the outbox when short of space. */
export function askPersistence() {
  navigator.storage?.persist?.().catch(() => null)
}

/** One sender at a time across this phone's tabs (navigator.locks, where there is one); a tab that finds it held skips. */
export function browserLock<T>(fn: () => Promise<T>): Promise<T | null> {
  const locks = (navigator as Navigator & { locks?: LockManager }).locks
  if (!locks?.request) return fn()
  return locks.request('identify-outbox', { ifAvailable: true }, lock => (lock ? fn() : null)) as Promise<T | null>
}
