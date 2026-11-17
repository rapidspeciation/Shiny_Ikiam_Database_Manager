// A visitor's photo on the server (identifyOnline.ts identifyAsVisitor, server/visitor-identify.mjs): the places told
// while it waits, the answer, a ticket the server dropped queued once more, and the place given up on failure.
import { afterEach, describe, expect, it, vi } from 'vitest'
import { ApiError } from '../api'
import { choiceOrDefault, identifyAsVisitor, onlineError, visitorServer, type VisitorPlace } from '../identifyOnline'

const answer = { species: [['Oleria onega', 0.9]], subspecies: [], genus: [['Oleria', 0.95]], crop: 'success', wingBox: null, prior: null, ms: 400 }
const json = (status: number, body: unknown) => new Response(JSON.stringify(body), { status, headers: { 'content-type': 'application/json' } })

function serve(replies: ((url: string, init?: RequestInit) => Response)[]) {
  const calls: { url: string; method: string }[] = []
  vi.stubGlobal(
    'fetch',
    vi.fn(async (url: string, init?: RequestInit) => {
      calls.push({ url: String(url), method: init?.method ?? 'GET' })
      const reply = replies.shift()
      if (!reply) throw new Error(`unexpected ${url}`)
      return reply(String(url), init)
    }),
  )
  return calls
}

afterEach(() => {
  vi.useRealTimers()
  vi.unstubAllGlobals()
})

describe('identifyAsVisitor', () => {
  it('tells the places while it waits, then gives the answer', async () => {
    vi.useFakeTimers()
    const t = '11111111-1111-1111-1111-111111111111'
    const calls = serve([
      () => json(202, { ticket: t, state: 'waiting', place: 3, ahead: 2, waitS: 4 }),
      () => json(200, { ticket: t, state: 'waiting', place: 2, ahead: 1, waitS: 3 }),
      () => json(200, { ticket: t, state: 'running', place: 1, ahead: 0, waitS: 1 }),
      () => json(200, { ticket: t, state: 'done', answer }),
    ])
    const places: VisitorPlace[] = []
    const out = identifyAsVisitor('/9j/AAAA', 'East', { onPlace: p => places.push(p) })
    await vi.runAllTimersAsync()
    expect((await out).species[0]).toEqual(['Oleria onega', 0.9])
    expect(places.map(p => p.place)).toEqual([3, 2, 1])
    expect(calls[0]).toMatchObject({ method: 'POST' })
    expect(calls.at(-1)!.url).toContain(`visitor/identify/${t}`)
  })

  it('a ticket the server dropped (the page asleep) is queued once more', async () => {
    vi.useFakeTimers()
    const calls = serve([
      () => json(202, { ticket: 'a', state: 'waiting', place: 2, ahead: 1, waitS: 2 }),
      () => json(404, { error: { code: 'NOT_FOUND', message: 'gone' } }),
      () => json(202, { ticket: 'b', state: 'running', place: 1, ahead: 0, waitS: 1 }),
      () => json(200, { ticket: 'b', state: 'done', answer }),
    ])
    const out = identifyAsVisitor('/9j/AAAA', null)
    await vi.runAllTimersAsync()
    expect((await out).ms).toBe(400)
    expect(calls.filter(c => c.method === 'POST')).toHaveLength(2)
  })

  it('a failed reading is an error the page can word, and its place is given up', async () => {
    vi.useFakeTimers()
    const calls = serve([
      () => json(202, { ticket: 'c', state: 'waiting', place: 1, ahead: 0, waitS: 1 }),
      () => json(200, { ticket: 'c', state: 'failed', status: 504, error: { code: 'timeout', message: 'late' } }),
      () => json(200, { ok: true }),
    ])
    const out = identifyAsVisitor('/9j/AAAA', null).catch(e => e)
    await vi.runAllTimersAsync()
    const err = await out
    expect(err).toBeInstanceOf(ApiError)
    expect(err.status).toBe(504)
    expect(calls.at(-1)).toMatchObject({ method: 'DELETE' })
  })

  it('given up (the page left, Borrar fotos) while waiting: the ticket is DELETEd and polling stops', async () => {
    vi.useFakeTimers()
    const calls = serve([() => json(202, { ticket: 'd', state: 'waiting', place: 2, ahead: 1, waitS: 2 }), () => json(200, { ok: true })])
    const ctl = new AbortController()
    const out = identifyAsVisitor('/9j/AAAA', null, { signal: ctl.signal }).catch(e => e)
    await vi.advanceTimersByTimeAsync(100)
    ctl.abort()
    await vi.runAllTimersAsync()
    await out
    expect(calls.map(c => c.method)).toEqual(['POST', 'DELETE'])
    expect(calls[1].url).toContain('visitor/identify/d')
  })

  it('a full queue says so plainly', () => {
    expect(onlineError(new ApiError(503, { code: 'BUSY', message: 'busy' }))).toMatch(/ocupado|busy/i)
    expect(onlineError(new ApiError(429, { code: 'RATE_LIMITED', message: 'x' }))).toMatch(/red|network/i)
  })
})

describe('visitorServer', () => {
  it('on only when the server says so; off on 401 (the flag off) or no signal', async () => {
    serve([() => json(200, { on: true, waitS: 2 })])
    expect(await visitorServer()).toEqual({ on: true, waitS: 2 })
    serve([() => json(401, { error: { code: 'AUTH_REQUIRED', message: 'Sign in required' } })])
    expect((await visitorServer()).on).toBe(false)
    serve([
      () => {
        throw new TypeError('offline')
      },
    ])
    expect((await visitorServer()).on).toBe(false)
  })
})

describe('choiceOrDefault', () => {
  it('a visitor with a model on the phone stays on the phone; otherwise online; a kept choice wins', () => {
    expect(choiceOrDefault(null, false, true)).toBe('phone')
    expect(choiceOrDefault(null, false, false)).toBe(null)
    expect(choiceOrDefault(null, true, true)).toBe(null)
    expect(choiceOrDefault('online', false, true)).toBe('online')
  })
})
