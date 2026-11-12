import { describe, expect, it } from 'vitest'
import { photoMeta, stripJpegMeta } from '../exifMeta'

/**
 * A JPEG with an APP1 EXIF block as cameras write it: IFD0 with DateTime, an Exif IFD with DateTimeOriginal, a GPS
 * IFD (lat/lon as degrees, minutes, seconds rationals, with their N/S, E/W). `little`: Intel byte order.
 */
function jpeg({ little = false, lat = [0, 57, 4.5], latRef = 'S', lon = [77, 51, 40.2], lonRef = 'W', original = '2026:11:10 08:31:00', gps = true } = {}) {
  const parts: number[] = []
  const u16 = (v: number) => (little ? [v & 255, v >> 8] : [v >> 8, v & 255])
  const u32 = (v: number) => (little ? [v & 255, (v >> 8) & 255, (v >> 16) & 255, v >>> 24] : [v >>> 24, (v >> 16) & 255, (v >> 8) & 255, v & 255])
  // Layout (offsets from the TIFF start): header 8, IFD0 at 8 (3 entries), Exif IFD, GPS IFD, then the data.
  const ifd0 = 8, ifd0Len = 2 + 3 * 12 + 4
  const exif = ifd0 + ifd0Len, exifLen = 2 + 12 + 4
  const gpsAt = exif + exifLen, gpsLen = 2 + 4 * 12 + 4
  let data = gpsAt + gpsLen
  const blobs: number[][] = []
  const put = (bytes: number[]) => {
    const at = data
    blobs.push(bytes)
    data += bytes.length
    return at
  }
  const str = (s: string) => [...s].map(c => c.charCodeAt(0)).concat(0)
  const rat = (v: number) => {
    const d = 100
    return [...u32(Math.round(v * d)), ...u32(d)]
  }
  const dateAt = put(str('2020:01:01 00:00:00'))
  const origAt = put(str(original))
  const latAt = put([...rat(lat[0]), ...rat(lat[1]), ...rat(lat[2])])
  const lonAt = put([...rat(lon[0]), ...rat(lon[1]), ...rat(lon[2])])
  const entry = (tag: number, type: number, count: number, value: number[]) => [...u16(tag), ...u16(type), ...u32(count), ...value]
  parts.push(...(little ? [0x49, 0x49] : [0x4d, 0x4d]), ...u16(42), ...u32(ifd0))
  parts.push(...u16(3), ...entry(0x0132, 2, 20, u32(dateAt)), ...entry(0x8769, 4, 1, u32(exif)), ...entry(0x8825, 4, 1, u32(gps ? gpsAt : 0)), ...u32(0))
  parts.push(...u16(1), ...entry(0x9003, 2, original.length + 1, u32(origAt)), ...u32(0))
  const ref = (c: string) => [c.charCodeAt(0), 0, 0, 0]
  parts.push(...u16(4), ...entry(1, 2, 2, ref(latRef)), ...entry(2, 5, 3, u32(latAt)), ...entry(3, 2, 2, ref(lonRef)), ...entry(4, 5, 3, u32(lonAt)), ...u32(0))
  for (const b of blobs) parts.push(...b)
  const app1 = [0x45, 0x78, 0x69, 0x66, 0, 0, ...parts]
  const bytes = [0xff, 0xd8, 0xff, 0xe0, 0, 16, 0x4a, 0x46, 0x49, 0x46, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0xff, 0xe1, (app1.length + 2) >> 8, (app1.length + 2) & 255, ...app1, 0xff, 0xda, 0, 2, 1, 2, 3, 0xff, 0xd9]
  return new Uint8Array(bytes).buffer
}

describe('exifMeta', () => {
  it('reads the location (S and W are negative) and the day taken, both byte orders', () => {
    for (const little of [false, true]) {
      const m = photoMeta(jpeg({ little }))
      expect(m.takenAt).toBe('2026-11-10')
      expect(m.gps!.lat).toBeCloseTo(-(57 / 60 + 4.5 / 3600), 6)
      expect(m.gps!.lon).toBeCloseTo(-(77 + 51 / 60 + 40.2 / 3600), 6)
    }
    expect(photoMeta(jpeg({ latRef: 'N', lonRef: 'E' })).gps!.lat).toBeGreaterThan(0)
  })

  it('no GPS block, 0,0 (no fix), not a JPEG, a cut file: nulls', () => {
    expect(photoMeta(jpeg({ gps: false })).gps).toBeNull()
    expect(photoMeta(jpeg({ lat: [0, 0, 0], lon: [0, 0, 0] })).gps).toBeNull()
    expect(photoMeta(new Uint8Array([0x89, 0x50, 0x4e, 0x47]).buffer)).toEqual({ gps: null, takenAt: null })
    expect(photoMeta(jpeg().slice(0, 60))).toEqual({ gps: null, takenAt: null })
    expect(photoMeta(jpeg({ original: '0000:00:00 00:00:00' })).takenAt).toBeNull()
  })

  it('stripJpegMeta: a camera JPEG with GPS comes out without EXIF (no location, no day), the image data untouched', () => {
    const before = new Uint8Array(jpeg())
    expect(photoMeta(before.buffer).gps).not.toBeNull()
    const after = stripJpegMeta(before)!
    expect(after.length).toBeLessThan(before.length)
    expect(photoMeta(after.slice().buffer)).toEqual({ gps: null, takenAt: null })
    // No APP1 (0xffe1) left; the JFIF header and the scan kept.
    const text = [...after].map(b => b.toString(16).padStart(2, '0')).join('')
    expect(text).not.toMatch(/ffe1/)
    expect(text).toMatch(/^ffd8ffe0/)
    expect(text.endsWith('ffda0002010203ffd9')).toBe(true)
    // Nothing to take out: the same bytes. Not a JPEG or a cut one: null.
    expect(stripJpegMeta(after)).toBe(after)
    expect(stripJpegMeta(new Uint8Array([0x89, 0x50, 0x4e, 0x47]))).toBeNull()
    expect(stripJpegMeta(before.slice(0, 60))).toBeNull()
  })
})
