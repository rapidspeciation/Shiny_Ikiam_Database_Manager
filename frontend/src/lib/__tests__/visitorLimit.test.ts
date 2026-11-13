import { beforeEach, describe, expect, it, vi } from 'vitest'
import { ApiError } from '../api'
import { noteLimit, probePhoto, resetVisitorLimit, visitorLimited } from '../visitorLimit'

// The signed-out limit said in one line (server/public-reads.mjs answers 429 past a network's share).
beforeEach(() => resetVisitorLimit())

describe('the visitors’ limit', () => {
  it('a list refused with 429 says it; other errors do not', () => {
    noteLimit(new ApiError(503, { code: 'BUSY', message: 'busy' }))
    noteLimit(new Error('offline'))
    expect(visitorLimited.value).toBe(false)
    noteLimit(new ApiError(429, { code: 'RATE_LIMITED', message: 'Too many' }))
    expect(visitorLimited.value).toBe(true)
  })

  it('a photo of the app that failed is asked once more, at most three times a page; others never', async () => {
    const statuses = [404, 404, 404, 429]
    const fetch = vi.fn(async () => new Response('', { status: statuses.shift() }))
    vi.stubGlobal('fetch', fetch)
    probePhoto('https://inaturalist-open-data.s3.amazonaws.com/photos/1/small.jpg')
    probePhoto('/identificar/field/1.jpg')
    for (const src of ['/api/photo/abc?w=400', '/api/identify/ref-photo?u=x', '/api/photo/def?w=400', '/api/photo/ghi?w=400']) probePhoto(src)
    await new Promise(r => setTimeout(r, 0))
    expect(fetch).toHaveBeenCalledTimes(3)
    expect(visitorLimited.value).toBe(false)
    resetVisitorLimit()
    statuses.splice(0, statuses.length, 429)
    probePhoto('/api/photo/abc?w=400')
    await new Promise(r => setTimeout(r, 0))
    expect(visitorLimited.value).toBe(true)
  })
})
