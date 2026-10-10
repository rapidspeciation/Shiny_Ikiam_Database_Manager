import { afterEach, describe, expect, it, vi } from 'vitest'
import { askEarly, early } from '../api'

const answer = (value: unknown) => new Response(JSON.stringify(value), { status: 200 })

describe('askEarly / early', () => {
  afterEach(() => {
    vi.unstubAllGlobals()
    vi.useRealTimers()
  })

  it('the page takes the answer asked early once; later it asks again', async () => {
    const fetch = vi.fn(async () => answer({ n: fetch.mock.calls.length }))
    vi.stubGlobal('fetch', fetch)
    askEarly('summary')
    expect(fetch).toHaveBeenCalledTimes(1)
    expect(await early('summary')).toEqual({ n: 1 })
    expect(fetch).toHaveBeenCalledTimes(1)
    expect(await early('summary')).toEqual({ n: 2 })
    expect(fetch).toHaveBeenCalledTimes(2)
  })

  it('an answer older than a minute is not used', async () => {
    vi.useFakeTimers()
    const fetch = vi.fn(async () => answer({ n: fetch.mock.calls.length }))
    vi.stubGlobal('fetch', fetch)
    askEarly('summary')
    vi.advanceTimersByTime(61_000)
    expect(await early('summary')).toEqual({ n: 2 })
  })

  it('a refused early answer reaches the page as its own request would', async () => {
    vi.stubGlobal('fetch', vi.fn(async () => new Response(JSON.stringify({ error: { code: 'X', message: 'no' } }), { status: 500 })))
    askEarly('summary')
    await expect(early('summary')).rejects.toMatchObject({ status: 500, code: 'X' })
  })
})
