/**
 * What every download of Identificar → Descargas shares (the models: lib/offlineEngine.ts; the photo packs:
 * lib/photoPacks.ts) to outlast a dropped signal (12 Nov 2026: Franz switched wifi networks during the fast model's
 * update and had to reload the page to download it again).
 *
 * - One request at a time is watched (`request`): no byte for STALL ms (a connection left hanging by a network switch
 *   answers nothing and never fails) or the browser's `offline` event ends it at once, as a network error.
 * - A network error is never the end of a download (`retry`): the next try waits for a backoff (1 s, 2 s, 4 s … up
 *   to 30 s), the browser's `online` event or the page coming back in front, whichever comes first; meanwhile the
 *   caller hears `waiting` (the page says «Esperando conexión…»). The server's own errors (5xx) are tried a few times;
 *   a refusal (4xx), a changed file or a bad hash is the caller's to handle (`DownloadError`).
 * - The bytes that came are kept by the caller, which asks only for the rest with a Range request (206) the next time.
 */

/** Why a request did not give its bytes. `network`: tried again for as long as it takes. */
export type FailKind = 'network' | 'server' | 'refused' | 'changed' | 'corrupt' | 'cancelled'
export class DownloadError extends Error {
  constructor(
    readonly kind: FailKind,
    message: string = kind,
  ) {
    super(message)
  }
}
/** A full phone: never tried again (the caller says so). */
export const isQuota = (err: unknown) => /quota/i.test(`${(err as Error)?.name} ${(err as Error)?.message}`)

/** The timings (tests make them short). */
export const timing = {
  /** No byte for this long: the connection is taken as cut. */
  stall: 20_000,
  /** The first wait before a new try, doubled each time up to `max`. */
  base: 1000,
  max: 30_000,
  /** How many times a server error (5xx, 429) is tried before it is the caller's. */
  serverTries: 6,
}

const hasWindow = () => typeof addEventListener === 'function'
const offlineNow = () => typeof navigator !== 'undefined' && navigator.onLine === false

/**
 * Waits before try number `attempt` (1, 2 …): the backoff, cut short by the `online` event or the page back in front
 * with the browser online. While the browser says it is offline only those (or a minute) end the wait.
 */
export function nextTry(attempt: number, signal?: AbortSignal): Promise<void> {
  const backoff = Math.min(timing.max, timing.base * 2 ** Math.max(0, attempt - 1))
  return new Promise(resolve => {
    let timer: ReturnType<typeof setTimeout> | undefined
    const finish = () => {
      clearTimeout(timer)
      signal?.removeEventListener('abort', finish)
      if (hasWindow()) {
        removeEventListener('online', finish)
        if (typeof document !== 'undefined') document.removeEventListener('visibilitychange', onVisible)
      }
      resolve()
    }
    const onVisible = () => {
      if (document.visibilityState === 'visible' && !offlineNow()) finish()
    }
    signal?.addEventListener('abort', finish)
    if (hasWindow()) {
      addEventListener('online', finish)
      if (typeof document !== 'undefined') document.addEventListener('visibilitychange', onVisible)
    }
    // Offline: until the `online` event, or a minute (the browser's flag can be wrong: a try then), else the backoff.
    timer = setTimeout(finish, offlineNow() ? Math.max(backoff, 60_000) : backoff)
  })
}

/**
 * One fetch watched for a stall and for the `offline` event. `body` reads the response (each chunk through `read`,
 * which also restarts the stall clock); the request is aborted on a stall, on `offline`, on `signal`, or when `body`
 * throws. Only the network's failures (the fetch, a read) become DownloadError('network'); anything else `body`
 * throws (a storage error) is the caller's. `flowing` hears of the first bytes that come.
 */
export async function request<T>(
  url: string,
  init: RequestInit,
  body: (r: Response, read: () => Promise<ReadableStreamReadResult<Uint8Array>>) => Promise<T>,
  signal?: AbortSignal,
  flowing: () => void = () => {},
): Promise<T> {
  const ctl = new AbortController()
  const cancel = () => ctl.abort()
  signal?.addEventListener('abort', cancel)
  let timer: ReturnType<typeof setTimeout> | undefined
  let stalled = false
  const arm = () => {
    clearTimeout(timer)
    timer = setTimeout(() => ((stalled = true), ctl.abort()), timing.stall)
  }
  const cut = () => ctl.abort()
  /** A failure of the network itself (or of the request given up: a stall, offline, cancelled). */
  const net = (err: unknown) =>
    signal?.aborted ? new DownloadError('cancelled') : new DownloadError('network', stalled ? 'stalled' : String((err as Error)?.message ?? err))
  if (hasWindow()) addEventListener('offline', cut)
  try {
    if (signal?.aborted) throw new DownloadError('cancelled')
    arm()
    const r = await fetch(url, { ...init, signal: ctl.signal }).catch(err => Promise.reject(net(err)))
    arm()
    const reader = r.body?.getReader()
    let whole: Uint8Array | null = null
    let first = true
    const read = async (): Promise<ReadableStreamReadResult<Uint8Array>> => {
      let out: ReadableStreamReadResult<Uint8Array>
      try {
        if (!reader) {
          // A response without a stream (tests, old engines): its bytes at once.
          if (whole) return { done: true, value: undefined }
          whole = new Uint8Array(await r.arrayBuffer())
          out = { done: false, value: whole }
        } else out = await reader.read()
      } catch (err) {
        throw net(err)
      }
      arm()
      if (first && !out.done && out.value.length) {
        first = false
        flowing()
      }
      return out
    }
    try {
      return await body(r, read)
    } finally {
      reader?.cancel().catch(() => {})
    }
  } finally {
    clearTimeout(timer)
    ctl.abort()
    signal?.removeEventListener('abort', cancel)
    if (hasWindow()) removeEventListener('offline', cut)
  }
}

/** An HTTP status as a DownloadError (null: fine). */
export function statusError(status: number, ok: number[]): DownloadError | null {
  if (ok.includes(status)) return null
  if (status >= 500 || status === 408 || status === 429) return new DownloadError('server', `HTTP ${status}`)
  return new DownloadError('refused', `HTTP ${status}`)
}

/**
 * Runs `step` until it returns: a network error waits (nextTry) and tries again for as long as it takes, a server
 * error up to timing.serverTries times; anything else is thrown. `waiting(true)` when a wait starts, `waiting(false)`
 * once bytes flow again (the step calls `flowing`, which `request` takes); bytes that flow also start the backoff over.
 * `step` keeps what it got (its next try asks only the rest); `signal` ends it (DownloadError('cancelled')).
 */
export async function retry<T>(step: (flowing: () => void) => Promise<T>, waiting: (on: boolean) => void = () => {}, signal?: AbortSignal): Promise<T> {
  let server = 0
  let attempt = 0
  let waitingNow = false
  const flowing = () => {
    attempt = 0
    if (waitingNow) {
      waitingNow = false
      waiting(false)
    }
  }
  for (;;) {
    if (signal?.aborted) throw new DownloadError('cancelled')
    try {
      const out = await step(flowing)
      flowing()
      return out
    } catch (err) {
      const again = err instanceof DownloadError && (err.kind === 'network' || (err.kind === 'server' && ++server < timing.serverTries)) && !signal?.aborted
      if (!again) {
        // Over (done with an error): the waiting it said ends too.
        if (waitingNow) waiting(false)
        throw signal?.aborted && !(err instanceof DownloadError && err.kind === 'cancelled') ? new DownloadError('cancelled') : err
      }
      if (!waitingNow) {
        waitingNow = true
        waiting(true)
      }
      await nextTry(++attempt, signal)
    }
  }
}

/** A `bytes a-b/total` Content-Range: its numbers (null: none or another form). */
export function contentRange(v: string | null): { start: number; end: number; total: number | null } | null {
  const m = v && /^bytes (\d+)-(\d+)\/(\d+|\*)$/.exec(v.trim())
  return m ? { start: Number(m[1]), end: Number(m[2]), total: m[3] === '*' ? null : Number(m[3]) } : null
}
