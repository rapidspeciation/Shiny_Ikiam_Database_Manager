import { describe, expect, it } from 'vitest'
import { CHUNK, chunksFrom, fitWithin, linkedEvent, photoUrl, retryDelay, sizeText } from '../clutchPhotos'
import type { ClutchEvent } from '../clutches'

// Clutch photos on the phone: made small, sent in chunks that resume, tried again by themselves.

describe('clutch photos on the phone', () => {
  it('fitted within 2560 px on the longest side (a 12 MP photo, upright or not), never enlarged', () => {
    expect(fitWithin(4080, 3060, 2560)).toEqual({ width: 2560, height: 1920 })
    expect(fitWithin(3060, 4080, 2560)).toEqual({ width: 1920, height: 2560 })
    expect(fitWithin(1600, 1200, 2560)).toEqual({ width: 1600, height: 1200 })
    expect(fitWithin(4080, 3060, 480)).toEqual({ width: 480, height: 360 })
  })
  it('chunks from where the server stands, the last one shorter', () => {
    expect(chunksFrom(0, 600_000)).toEqual([
      [0, CHUNK],
      [CHUNK, 2 * CHUNK],
      [2 * CHUNK, 600_000],
    ])
    // Resumed after a dropped connection: only what is missing.
    expect(chunksFrom(CHUNK + 10, 600_000)[0]).toEqual([CHUNK + 10, 2 * CHUNK + 10])
    expect(chunksFrom(600_000, 600_000)).toEqual([])
  })
  it('tried again sooner first, then every 30 s', () => {
    expect([1, 2, 3, 4, 5, 9].map(retryDelay)).toEqual([2000, 5000, 10_000, 20_000, 30_000, 30_000])
  })
  it('addresses, weights and the event a photo shows', () => {
    expect(photoUrl('a b', 'thumb')).toBe('api/clutches/photos/a%20b?size=thumb')
    expect(sizeText(650_000)).toBe('635 kB')
    expect(sizeText(1_300_000)).toBe('1.2 MB')
    const events = [{ id: 'e1' }] as ClutchEvent[]
    expect(linkedEvent({ eventId: 'e1' }, events)?.id).toBe('e1')
    expect(linkedEvent({ eventId: null }, events)).toBe(null)
  })
})
