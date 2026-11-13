import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { ApiError } from '../api'
import { noteLimit, photoFailed, probePhoto, PROBES, resetVisitorLimit, visitorLimited } from '../visitorLimit'

// The signed-out limits (server/public-reads.mjs): 429 past a network's share, said in one line; 503 BUSY when every
// visitor's Drive budget is spent for a moment, the photo tried once more after Retry-After.
beforeEach(() => resetVisitorLimit())
afterEach(() => vi.useRealTimers())

const answer = (status: number, headers: Record<string, string> = {}) => new Response('', { status, headers })

describe('the visitors’ limits', () => {
  it('a list refused with 429 says it; other errors do not', () => {
    noteLimit(new ApiError(503, { code: 'BUSY', message: 'busy' }))
    noteLimit(new Error('offline'))
    expect(visitorLimited.value).toBe(false)
    noteLimit(new ApiError(429, { code: 'RATE_LIMITED', message: 'Too many' }))
    expect(visitorLimited.value).toBe(true)
  })

  it('only the app’s photos are asked again, at most PROBES per batch', async () => {
    const fetch = vi.fn(async () => answer(404))
    vi.stubGlobal('fetch', fetch)
    expect(await probePhoto('https://inaturalist-open-data.s3.amazonaws.com/photos/1/small.jpg', 'a')).toEqual({ kind: 'skipped' })
    expect(await probePhoto('/identificar/field/1.jpg', 'a')).toEqual({ kind: 'skipped' })
    for (let i = 0; i < PROBES + 2; i++) await probePhoto(`/api/photo/x${i}?w=400`, 'a')
    expect(fetch).toHaveBeenCalledTimes(PROBES)
  })

  it('a 429 after earlier unrelated 404s: another batch has its own allowance', async () => {
    const statuses = [404, 404, 404, 429]
    vi.stubGlobal('fetch', vi.fn(async () => answer(statuses.shift() ?? 404)))
    for (const id of ['a', 'b', 'c']) await probePhoto(`/api/photo/${id}?w=400`, 'search:Oleria onega')
    expect(visitorLimited.value).toBe(false)
    expect(await probePhoto('/api/identify/ref-photo?u=x', 'search:Mechanitis polymnia')).toEqual({ kind: 'limited' })
    expect(visitorLimited.value).toBe(true)
  })

  it('a photo refused while the server is busy comes back once after Retry-After, then never loops', async () => {
    vi.useFakeTimers()
    vi.stubGlobal('fetch', vi.fn(async () => answer(503, { 'retry-after': '2' })))
    const broken = new Set<string>()
    const hooks = { drop: (s: string) => broken.add(s), restore: (s: string) => broken.delete(s) }
    photoFailed('/api/photo/abc?w=400', 'b', hooks)
    expect(broken.has('/api/photo/abc?w=400')).toBe(true)
    await vi.advanceTimersByTimeAsync(1900)
    expect(broken.has('/api/photo/abc?w=400')).toBe(true)
    await vi.advanceTimersByTimeAsync(200)
    // shown again: the browser asks for it again
    expect(broken.has('/api/photo/abc?w=400')).toBe(false)
    // still busy: dropped, and not tried a third time
    photoFailed('/api/photo/abc?w=400', 'b', hooks)
    await vi.advanceTimersByTimeAsync(10_000)
    expect(broken.has('/api/photo/abc?w=400')).toBe(true)
  })

  it('a 404 is dropped for good', async () => {
    vi.useFakeTimers()
    vi.stubGlobal('fetch', vi.fn(async () => answer(404)))
    const broken = new Set<string>()
    photoFailed('/api/photo/gone?w=400', 'c', { drop: s => broken.add(s), restore: s => broken.delete(s) })
    await vi.advanceTimersByTimeAsync(10_000)
    expect(broken.has('/api/photo/gone?w=400')).toBe(true)
  })
})
