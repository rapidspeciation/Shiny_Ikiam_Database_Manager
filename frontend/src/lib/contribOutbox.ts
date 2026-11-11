/**
 * The phone's outbox for photos sent to train the AI (DESIGN.md §3, server/contributions.mjs). An item is kept on
 * the phone from Enviar until the server has it, and sent with the id made once at Enviar, so a retry never makes a
 * second copy. Without signal it waits; it is sent at the page's start, when the signal comes back, when the page is
 * shown again, right after Enviar, and on a timer that backs off (30 s → 1 h).
 *
 * The answers (the server's status codes):
 * - 201, or 200 `already`: sent. The photo is dropped from the phone; a small thumbnail and the status stay. After
 *   `already` with an `id` (the same photo sent before under another id) that id is kept: it is the one to delete.
 * - 401 / 403: paused until the person signs in again.
 * - 400 / 409 / 413 / 422: failed for good, with the reason; the person can delete it.
 * - 410: deleted on the server before: dropped from the phone.
 * - 429: waits for Retry-After. 0 (no signal), 5xx, 507 (the server is full): backs off and tries again.
 *
 * «Borrar» deletes an item on the phone and on the server: one never sent goes at once; one that may have reached the
 * server is kept as `deleting` (without its photo) until the server has deleted it, signal or not.
 * Only the signed-in person's items are sent or listed; another person on the same phone never sees them.
 * The storage is behind `OutboxStore` (IndexedDB in the app, lib/contribIdb.ts; memory in tests).
 */
import { MAX_PENDING, MAX_SENT_KEPT } from './contribConfig'
import { apiRoot, csrfToken } from './api'

export type ItemState = 'pending' | 'sending' | 'sent' | 'failed' | 'deleting'

/** What goes to POST /api/identify/contributions besides id, photo, thumb and sha256. */
export interface Payload {
  createdAt: string
  takenAt: string | null
  labels: {
    sex: 'female' | 'male' | null
    photoKind: 'hand' | 'live' | 'collection' | null
    species: { name: string; rank: 'genus' | 'species' | 'subspecies'; confirmed: boolean; freeText: boolean } | null
  }
  prediction: Record<string, unknown>
  gps: { lat: number; lon: number } | null
  consent: { version: string }
}

export interface Item {
  id: string
  userId: string
  createdAt: string
  state: ItemState
  attempts: number
  /** ms: not sent again before (backoff, Retry-After). */
  nextAt: number
  lastError: string | null
  /** The id the server holds it under, when it differs (`already` with another id). */
  serverId: string | null
  /** Base64 JPEG (at most 1600 px) and its SHA-256, and the 224 px square: only until sent. */
  photo: string | null
  sha256: string | null
  thumb: string | null
  /** A 72 px data: URL for «Mis envíos». */
  mini: string
  payload: Payload
  /** The server's status once listed (received, accepted, …). */
  serverStatus: string | null
}

export interface OutboxStore {
  all(): Promise<Item[]>
  put(item: Item): Promise<void>
  delete(id: string): Promise<void>
}

/** A request's answer: `status` 0 when there was no answer (no signal). */
export interface Answer {
  status: number
  body: Record<string, unknown> | null
  retryAfter: number | null
}
export type Send = (method: 'POST' | 'DELETE' | 'GET', path: string, body?: unknown) => Promise<Answer>

/** The app's API with the session's cookie and token; keeps the status and Retry-After that api() does not give. */
export const httpSend: Send = async (method, path, body) => {
  const headers: Record<string, string> = {}
  if (body !== undefined) headers['content-type'] = 'application/json'
  const csrf = csrfToken()
  if (method !== 'GET' && csrf) headers['x-csrf-token'] = csrf
  let r: Response
  try {
    r = await fetch(`${apiRoot}api/${path}`, { method, headers, credentials: 'same-origin', body: body === undefined ? undefined : JSON.stringify(body), signal: AbortSignal.timeout(120_000) })
  } catch {
    return { status: 0, body: null, retryAfter: null }
  }
  let data: Record<string, unknown> | null = null
  try {
    data = await r.json()
  } catch {
    data = null
  }
  const retry = Number(r.headers.get('retry-after'))
  // A service worker's offline answer, or a proxy's page: no answer from the app.
  if (!data && r.status >= 500) return { status: 0, body: null, retryAfter: null }
  return { status: r.status, body: data, retryAfter: Number.isFinite(retry) && retry > 0 ? retry : null }
}

export interface FlushResult {
  sent: number
  /** Why it stopped before the end: no session, the limit, no signal or the server. */
  stopped: 'auth' | 'rate' | 'offline' | null
}

/** 30 s, 1 min, 2 min, 5 min, 15 min, then every hour, ±20 %. */
export const BACKOFF = [30_000, 60_000, 120_000, 300_000, 900_000, 3_600_000]
export const backoff = (attempts: number, rand = Math.random) => {
  const base = BACKOFF[Math.min(Math.max(attempts, 1), BACKOFF.length) - 1]
  return Math.round(base * (0.8 + 0.4 * rand()))
}

export function memoryStore(): OutboxStore & { items: Map<string, Item> } {
  const items = new Map<string, Item>()
  const copy = (i: Item) => structuredClone(i)
  return {
    items,
    all: async () => [...items.values()].map(copy),
    put: async i => void items.set(i.id, copy(i)),
    delete: async id => void items.delete(id),
  }
}

export interface Draft {
  photo: string
  sha256: string
  thumb: string | null
  mini: string
  payload: Payload
}

export class OutboxFull extends Error {}

export function createOutbox({
  store,
  send,
  now = Date.now,
  rand = Math.random,
  uuid = () => crypto.randomUUID(),
  lock,
}: {
  store: OutboxStore
  send: Send
  now?: () => number
  rand?: () => number
  uuid?: () => string
  /** One sender at a time across the phone's tabs (navigator.locks); in this page a flag does the same. */
  lock?: <T>(fn: () => Promise<T>) => Promise<T | null>
}) {
  let running: Promise<FlushResult> | null = null
  const mine = async (userId: string) => (await store.all()).filter(i => i.userId === userId).sort((a, b) => a.createdAt.localeCompare(b.createdAt))
  const waiting = (i: Item) => i.state === 'pending' || i.state === 'sending'

  /** A new item for `userId` from Enviar; refused (OutboxFull) while MAX_PENDING wait for signal. */
  async function add(userId: string, draft: Draft): Promise<Item> {
    const items = await mine(userId)
    if (items.filter(waiting).length >= MAX_PENDING) throw new OutboxFull('full')
    const item: Item = {
      id: uuid(),
      userId,
      createdAt: draft.payload.createdAt,
      state: 'pending',
      attempts: 0,
      nextAt: 0,
      lastError: null,
      serverId: null,
      photo: draft.photo,
      sha256: draft.sha256,
      thumb: draft.thumb,
      mini: draft.mini,
      payload: draft.payload,
      serverStatus: null,
    }
    await store.put(item)
    // Sent ones beyond MAX_SENT_KEPT leave the list (oldest first); they stay on the server.
    const sent = items.filter(i => i.state === 'sent')
    for (const old of sent.slice(0, Math.max(0, sent.length + 1 - MAX_SENT_KEPT))) await store.delete(old.id)
    return item
  }

  const errorOf = (a: Answer) => {
    const e = a.body?.error as { message?: string } | undefined
    return e?.message ?? `HTTP ${a.status}`
  }

  async function sendOne(item: Item, t: number): Promise<FlushResult['stopped'] | 'sent' | 'next'> {
    if (item.state === 'deleting') {
      const a = await send('DELETE', `identify/contributions/${encodeURIComponent(item.serverId ?? item.id)}`)
      if (a.status === 200 || a.status === 400 || a.status === 404) {
        await store.delete(item.id)
        return 'next'
      }
      if (a.status === 401 || a.status === 403) return 'auth'
      await store.put({ ...item, attempts: item.attempts + 1, nextAt: t + backoff(item.attempts + 1, rand) })
      return a.status === 429 ? 'rate' : 'offline'
    }
    await store.put({ ...item, state: 'sending' })
    const { payload } = item
    const a = await send('POST', 'identify/contributions', { id: item.id, photo: item.photo, thumb: item.thumb, sha256: item.sha256, ...payload })
    // Deleted while it was on its way: the delete follows (the server knows the id now, or refuses it when it comes).
    const now2 = (await store.all()).find(i => i.id === item.id)
    if (!now2) return 'next'
    if (now2.state === 'deleting') {
      const serverId = a.status === 200 && typeof a.body?.id === 'string' ? a.body.id : now2.serverId
      await store.put({ ...now2, serverId })
      return 'next'
    }
    if (a.status === 201 || a.status === 200) {
      const contribution = a.body?.contribution as { status?: string } | undefined
      await store.put({
        ...item,
        state: 'sent',
        serverId: typeof a.body?.id === 'string' && a.body.id !== item.id ? a.body.id : null,
        photo: null,
        sha256: null,
        thumb: null,
        lastError: null,
        serverStatus: contribution?.status ?? 'received',
      })
      return 'sent'
    }
    if (a.status === 410) {
      await store.delete(item.id)
      return 'next'
    }
    if (a.status === 401 || a.status === 403) {
      await store.put({ ...item, state: 'pending' })
      return 'auth'
    }
    if ([400, 409, 413, 422].includes(a.status)) {
      await store.put({ ...item, state: 'failed', photo: null, thumb: null, lastError: errorOf(a) })
      return 'next'
    }
    const attempts = item.attempts + 1
    const wait = a.status === 429 && a.retryAfter ? a.retryAfter * 1000 : backoff(attempts, rand)
    await store.put({ ...item, state: 'pending', attempts, nextAt: t + wait, lastError: a.status ? errorOf(a) : null })
    return a.status === 429 ? 'rate' : 'offline'
  }

  /**
   * Sends what waits for `userId`, oldest first. `force` (the signal back, the page shown, Enviar) does not wait for
   * the backoff. One run at a time: a call during a run waits for it.
   */
  async function flush(userId: string, { force = false } = {}): Promise<FlushResult> {
    if (running) return running
    const run = async (): Promise<FlushResult> => {
      const out: FlushResult = { sent: 0, stopped: null }
      for (const { id } of await mine(userId)) {
        // Read again: «Borrar» may have changed it while the one before was on its way.
        const item = (await store.all()).find(i => i.id === id)
        if (!item || !(waiting(item) || item.state === 'deleting')) continue
        const t = now()
        if (!force && item.nextAt > t) continue
        const r = await sendOne(item, t)
        if (r === 'sent') out.sent++
        else if (r !== 'next') {
          out.stopped = r
          break
        }
      }
      return out
    }
    running = (async () => {
      try {
        return (lock ? await lock(run) : await run()) ?? { sent: 0, stopped: null }
      } finally {
        running = null
      }
    })()
    return running
  }

  /** «Borrar» or «Deshacer»: gone from the phone; from the server too (now, or once there is signal). */
  async function remove(id: string): Promise<void> {
    const item = (await store.all()).find(i => i.id === id)
    if (!item) return
    if (item.state === 'pending' && item.attempts === 0 && !running) return store.delete(id)
    if (item.state === 'failed' && item.lastError) return store.delete(id)
    await store.put({ ...item, state: 'deleting', photo: null, thumb: null, attempts: 0, nextAt: 0 })
  }

  /** The person's items for «Mis envíos», newest first (deleting ones are already gone for the person). */
  async function list(userId: string): Promise<Item[]> {
    return (await mine(userId)).filter(i => i.state !== 'deleting').reverse()
  }

  /** The statuses the server gives (GET …/mine), on the items sent from this phone. */
  async function refresh(userId: string): Promise<void> {
    const a = await send('GET', 'identify/contributions/mine')
    if (a.status !== 200) return
    const status = new Map((a.body?.contributions as { id: string; status: string }[] | undefined)?.map(c => [c.id, c.status]) ?? [])
    for (const item of await mine(userId)) {
      const s = status.get(item.serverId ?? item.id)
      if (item.state === 'sent' && s && s !== item.serverStatus) await store.put({ ...item, serverStatus: s })
    }
  }

  /** When the next timed send is due (ms), or null. */
  async function nextAt(userId: string): Promise<number | null> {
    const due = (await mine(userId)).filter(i => waiting(i) || i.state === 'deleting').map(i => i.nextAt)
    return due.length ? Math.min(...due) : null
  }

  /** How many wait for signal (the page's «N envíos pendientes»). */
  async function pending(userId: string): Promise<number> {
    return (await mine(userId)).filter(waiting).length
  }

  return { add, flush, remove, list, refresh, nextAt, pending }
}
export type Outbox = ReturnType<typeof createOutbox>
