/**
 * Which country's photos come first (Franz, 11 Nov 2026: «photos of the same species from another country sometimes
 * look like different species»). A photo's country is where it was taken (the photo packs' `country`, from the
 * iNaturalist observation; null when not known). The person's country: the GPS position in the photo identified when
 * it has one (exifGps + countryAt, on the phone, without signal), else the settings (the side of the Andes means
 * Ecuador; else the countries of the photo pack chosen in Descargas). Then the neighbouring countries, then the rest,
 * then the photos whose country is not known. A subspecies' range (the countries it is recorded in), when one is named
 * or searched, comes before all of that.
 */
import { appRoot } from './appRoot'

/** Land borders (and the nearest islands, for the Caribbean) of the photo packs' 49 Neotropical countries. */
const BORDERS: Record<string, string[]> = {
  MX: ['GT', 'BZ'],
  GT: ['BZ', 'SV', 'HN'],
  SV: ['HN'],
  HN: ['NI'],
  NI: ['CR'],
  CR: ['PA'],
  PA: ['CO'],
  CO: ['VE', 'BR', 'PE', 'EC'],
  VE: ['BR', 'GY', 'TT', 'AW', 'CW', 'BQ'],
  GY: ['BR', 'SR'],
  SR: ['BR', 'GF'],
  GF: ['BR'],
  EC: ['PE'],
  PE: ['BR', 'BO', 'CL'],
  BO: ['BR', 'PY', 'AR', 'CL'],
  BR: ['PY', 'AR', 'UY'],
  PY: ['AR'],
  UY: ['AR'],
  AR: ['CL'],
  TT: ['GD'],
  CU: ['JM', 'BS', 'HT', 'KY'],
  JM: ['HT', 'KY'],
  HT: ['DO', 'TC'],
  DO: ['PR'],
  PR: ['VI', 'VG'],
  BS: ['TC'],
  VG: ['VI', 'AI'],
  AI: ['MF', 'SX'],
  MF: ['SX', 'BL'],
  SX: ['BL'],
  KN: ['AG', 'MS'],
  AG: ['MS', 'GP'],
  MS: ['GP'],
  GP: ['DM'],
  DM: ['MQ'],
  MQ: ['LC'],
  LC: ['VC', 'BB'],
  VC: ['GD', 'BB'],
  AW: ['CW'],
  CW: ['BQ'],
}
const NEAR = new Map<string, Set<string>>()
for (const [a, list] of Object.entries(BORDERS))
  for (const b of list) {
    if (!NEAR.has(a)) NEAR.set(a, new Set())
    if (!NEAR.has(b)) NEAR.set(b, new Set())
    NEAR.get(a)!.add(b)
    NEAR.get(b)!.add(a)
  }
/** A country's neighbours (ISO2). */
export const neighboursOf = (iso: string): string[] => [...(NEAR.get(iso) ?? [])].sort()

/** Where the person is, for the order of photos: their countries (most likely first) and a subspecies' range. */
export interface Place {
  home: string[]
  /** The countries of the subspecies named or searched, when known: its photos' countries come first. */
  range?: string[] | null
}
export const NO_PLACE: Place = { home: [] }

/**
 * A photo's place in the order (lower first): without a subspecies' range, home 0, a neighbour 1, another country 2,
 * not known 3; with one, the range first (home inside it before the rest of it), then the same four after it.
 * Without any home or range every photo is 0 (the list's own order).
 */
export function countryRank(country: string | null | undefined, place: Place): number {
  const range = place.range?.length ? place.range : null
  if (!place.home.length && !range) return 0
  if (range && country && range.includes(country)) return place.home.includes(country) ? 0 : 1
  const shift = range ? 2 : 0
  if (!country) return shift + 3
  if (place.home.includes(country)) return shift
  if (place.home.some(h => NEAR.get(h)?.has(country))) return shift + 1
  return shift + 2
}

/** `items` in the order of their countries for `place`, stable inside the same rank (`tie` breaks first). */
export function byPlace<T>(items: T[], place: Place, countryOf: (x: T) => string | null | undefined, tie?: (a: T, b: T) => number): T[] {
  const rank = new Map(items.map(x => [x, countryRank(countryOf(x), place)]))
  return items
    .map((x, i) => ({ x, i }))
    .sort((a, b) => rank.get(a.x)! - rank.get(b.x)! || (tie ? tie(a.x, b.x) : 0) || a.i - b.i)
    .map(({ x }) => x)
}

/**
 * The photos of one taxon a pack keeps, at most `k`: one from the region chosen, one from a neighbouring country and
 * one from elsewhere (or not known) when there are, then the rest in that order; the list's own order inside each.
 * No region (Neotropics): the list's first `k`.
 */
export function choosePhotos<T extends { country?: string | null }>(photos: T[], region: string[] | null, k: number): T[] {
  if (photos.length <= k && !region) return photos
  if (!region?.length) return photos.slice(0, k)
  const tier = (p: T) => {
    const r = countryRank(p.country, { home: region })
    return r >= 2 ? 2 : r
  }
  const firsts: T[] = []
  for (const t of [0, 1, 2]) {
    const p = photos.find(x => tier(x) === t)
    if (p) firsts.push(p)
  }
  const rest = byPlace(
    photos.filter(p => !firsts.includes(p)),
    { home: region },
    p => p.country,
  )
  return [...firsts, ...rest].slice(0, k)
}

// ---------------------------------------------------------------- the GPS position in a photo

/** The GPS position in a JPEG's Exif block, or null (none, not a JPEG, cut short, 0/0). */
export function exifGps(bytes: ArrayBuffer): { lat: number; lon: number } | null {
  try {
    const v = new DataView(bytes)
    if (v.byteLength < 4 || v.getUint16(0) !== 0xffd8) return null
    let at = 2
    while (at + 4 <= v.byteLength) {
      if (v.getUint8(at) !== 0xff) return null
      const marker = v.getUint8(at + 1)
      const len = v.getUint16(at + 2)
      if (marker === 0xda || marker === 0xd9) return null // the image data: no Exif before it
      // APP1 «Exif\0\0»
      if (marker === 0xe1 && at + 10 <= v.byteLength && v.getUint32(at + 4) === 0x45786966 && v.getUint16(at + 8) === 0) return tiffGps(v, at + 10, at + 2 + len)
      at += 2 + len
    }
  } catch {
    /* a broken file: no position */
  }
  return null
}

function tiffGps(v: DataView, tiff: number, end: number): { lat: number; lon: number } | null {
  const little = v.getUint16(tiff) === 0x4949
  const u16 = (o: number) => v.getUint16(tiff + o, little)
  const u32 = (o: number) => v.getUint32(tiff + o, little)
  if (u16(2) !== 42) return null
  const ifd = (o: number) => {
    const n = u16(o)
    const tags = new Map<number, { type: number; count: number; value: number }>()
    for (let i = 0; i < n; i++) {
      const e = o + 2 + i * 12
      if (tiff + e + 12 > end) break
      tags.set(u16(e), { type: u16(e + 2), count: u32(e + 4), value: e + 8 })
    }
    return tags
  }
  const gpsAt = ifd(u32(4)).get(0x8825)
  if (!gpsAt) return null
  const gps = ifd(u32(gpsAt.value))
  const ref = (tag: number) => {
    const t = gps.get(tag)
    return t ? String.fromCharCode(v.getUint8(tiff + t.value)) : ''
  }
  const dms = (tag: number) => {
    const t = gps.get(tag)
    if (!t || t.type !== 5 || t.count < 3) return null
    const o = u32(t.value)
    const r = (i: number) => {
      const den = u32(o + i * 8 + 4)
      return den ? u32(o + i * 8) / den : 0
    }
    return r(0) + r(1) / 60 + r(2) / 3600
  }
  const lat = dms(2)
  const lon = dms(4)
  if (lat == null || lon == null || (lat === 0 && lon === 0) || lat > 90 || lon > 180) return null
  return { lat: ref(1) === 'S' ? -lat : lat, lon: ref(3) === 'W' ? -lon : lon }
}

// ---------------------------------------------------------------- the country of a position

/** identificar/countries.json (scripts/country-shapes.py): each country's rings, flat lon/lat in 1/scale degrees. */
export interface CountryShapes {
  schema: 'ithomiini-countries/1'
  scale: number
  countries: Record<string, { box: [number, number, number, number]; rings: number[][] }>
}
export const SHAPES_URL = () => new URL('identificar/countries.json', appRoot()).href
let shapesMemo: Promise<CountryShapes> | null = null
/** The outlines (kept by the service worker once read: see warmShapes). */
export function countryShapes(): Promise<CountryShapes> {
  shapesMemo ??= fetch(SHAPES_URL(), { credentials: 'same-origin' })
    .then(r => (r.ok ? (r.json() as Promise<CountryShapes>) : Promise.reject(new Error(`HTTP ${r.status}`))))
    .catch(err => {
      shapesMemo = null
      throw err
    })
  return shapesMemo
}

const inside = (ring: number[], x: number, y: number) => {
  let odd = false
  for (let i = 0, j = ring.length - 2; i < ring.length; j = i, i += 2) {
    const xi = ring[i], yi = ring[i + 1], xj = ring[j], yj = ring[j + 1]
    if (yi > y !== yj > y && x < ((xj - xi) * (y - yi)) / (yj - yi) + xi) odd = !odd
  }
  return odd
}
/**
 * The Neotropical country at a position (ISO2), or null outside them. A point just off a simplified coast (a beach, a
 * small island) takes the country with the nearest outline point within about 25 km.
 */
export function countryIn(shapes: CountryShapes, lat: number, lon: number): string | null {
  const x = lon * shapes.scale, y = lat * shapes.scale
  const near = 0.25 * shapes.scale
  let best: string | null = null
  let bestD = near * near
  for (const [iso, c] of Object.entries(shapes.countries)) {
    const [x0, y0, x1, y1] = c.box
    if (x < x0 - near || x > x1 + near || y < y0 - near || y > y1 + near) continue
    let odd = false
    for (const ring of c.rings) if (inside(ring, x, y)) odd = !odd
    if (odd) return iso
    for (const ring of c.rings)
      for (let i = 0; i < ring.length; i += 2) {
        const d = (ring[i] - x) ** 2 + (ring[i + 1] - y) ** 2
        if (d < bestD) {
          bestD = d
          best = iso
        }
      }
  }
  return best
}

/** The country a photo was taken in, from its GPS position (null: no position, or outside the Neotropics). */
export async function photoCountry(bytes: ArrayBuffer): Promise<string | null> {
  const at = exifGps(bytes)
  if (!at) return null
  const shapes = await countryShapes().catch(() => null)
  return shapes ? countryIn(shapes, at.lat, at.lon) : null
}
/** Read the outlines once with signal, so the service worker has them for a photo taken without. */
export const warmShapes = () => void countryShapes().catch(() => null)
