import { readFileSync } from 'node:fs'
import { resolve } from 'node:path'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { byPlace, choosePhotos, comparePlace, countryIn, countryRank, exifGps, neighboursOf, type CountryShapes } from '../photoCountry'
import type { PackIndex, PackPhoto, PackTaxon } from '../photoPacks'

// Photos by country (Franz, 11 Nov 2026: a Mechanitis lysimnia photo from Ecuador compared with Brazilian photos of
// M. polymnia): the person's country first, then the neighbours, then the rest; a subspecies' range before all; the
// photo's GPS position read on the phone; a pack keeps one photo from the region, one from a neighbour, one from
// elsewhere; a preserved specimen fits the same record.

const shapes = JSON.parse(readFileSync(resolve(import.meta.dirname, '../../../public/identificar/countries.json'), 'utf8')) as CountryShapes

describe('the order of countries', () => {
  it('home, neighbours, others, not known; a range first', () => {
    const ec = { home: ['EC'] }
    expect(['EC', 'CO', 'PE', 'BR', null].map(c => countryRank(c, ec))).toEqual([0, 1, 1, 2, 3])
    expect(neighboursOf('EC')).toEqual(['CO', 'PE'])
    expect(neighboursOf('CO')).toEqual(['BR', 'EC', 'PA', 'PE', 'VE'])
    // No place: the list's own order.
    expect(countryRank('BR', { home: [] })).toBe(0)
    // A subspecies of Brazil, a person in Ecuador: Brazil's photos first, then Ecuador's.
    const casabranca = { home: ['EC'], range: ['BR'] }
    expect(['BR', 'EC', 'CO', 'PE', 'VE', null].map(c => countryRank(c, casabranca))).toEqual([1, 2, 3, 3, 4, 5])
    // Home inside the range comes before the rest of the range.
    expect(['EC', 'CO'].map(c => countryRank(c, { home: ['EC'], range: ['CO', 'EC'] }))).toEqual([0, 1])
  })

  it('Fable r4-F4: Compare takes a named subspecies’ range only when the person’s country is in it', () => {
    // M. p. casabranca (Brazil) named for a photo from Ecuador: Ecuador first, not Brazil.
    expect(comparePlace(['EC'], ['BR'])).toEqual({ home: ['EC'], range: null })
    expect(byPlace(['BR', 'EC', 'CO'], comparePlace(['EC'], ['BR']), c => c)).toEqual(['EC', 'CO', 'BR'])
    // kayei (Ecuador, Trinidad) and a Colombian GPS position: Colombia first.
    expect(comparePlace(['CO'], ['EC', 'TT']).range).toBeNull()
    // The range holds the country: kept. No country known: the range.
    expect(comparePlace(['EC'], ['EC', 'TT']).range).toEqual(['EC', 'TT'])
    expect(comparePlace([], ['BR']).range).toEqual(['BR'])
  })

  it('sorts stably inside a rank', () => {
    const items = [
      { id: 'br', c: 'BR' },
      { id: 'x', c: null },
      { id: 'pe', c: 'PE' },
      { id: 'ec1', c: 'EC' },
      { id: 'co', c: 'CO' },
      { id: 'ec2', c: 'EC' },
    ]
    expect(byPlace(items, { home: ['EC'] }, x => x.c).map(x => x.id)).toEqual(['ec1', 'ec2', 'pe', 'co', 'br', 'x'])
  })

  it('a pack keeps one from the region, one from a neighbour, one from elsewhere', () => {
    const p = (file: string, country: string | null) => ({ file, country })
    const all = [p('default', 'BR'), p('ec', 'EC'), p('co', 'CO'), p('pe', 'PE')]
    expect(choosePhotos(all, ['EC'], 3).map(x => x.file)).toEqual(['ec', 'co', 'default'])
    // No Ecuador photo: a neighbour, another, then the next neighbour.
    expect(choosePhotos([p('default', null), p('co', 'CO'), p('pe', 'PE')], ['EC'], 3).map(x => x.file)).toEqual(['co', 'default', 'pe'])
    // A Peru pack: Peru first, then a neighbour (Brazil's comes first in the list).
    expect(choosePhotos(all, ['PE'], 2).map(x => x.file)).toEqual(['pe', 'default'])
    // Neotropics: the builder's first ones.
    expect(choosePhotos(all, null, 3).map(x => x.file)).toEqual(['default', 'ec', 'co'])
  })

  it('Fable r4-F5: the curated taxon photo is kept second, after the region’s own, before a neighbour’s', () => {
    const p = (file: string, country: string | null, curated = false) => ({ file, country, ...(curated ? { curated } : {}) })
    const all = [p('taxon', 'BR', true), p('ec', 'EC'), p('co', 'CO'), p('pe', 'PE')]
    expect(choosePhotos(all, ['EC'], 2).map(x => x.file)).toEqual(['ec', 'taxon'])
    expect(choosePhotos(all, ['EC'], 3).map(x => x.file)).toEqual(['ec', 'taxon', 'co'])
    // No Ecuador photo: the taxon photo, then a neighbour's.
    expect(choosePhotos([p('taxon', null, true), p('co', 'CO')], ['EC'], 2).map(x => x.file)).toEqual(['taxon', 'co'])
    // The taxon photo is Ecuador's itself: then a neighbour's.
    expect(choosePhotos([p('taxon', 'EC', true), p('co', 'CO'), p('pe', 'PE')], ['EC'], 2).map(x => x.file)).toEqual(['taxon', 'co'])
  })
})

/** A JPEG holding only an Exif block with a GPS position (big-endian TIFF), then its end. */
function jpegWithGps(lat: number, lon: number): ArrayBuffer {
  const tiff = new DataView(new ArrayBuffer(128))
  tiff.setUint16(0, 0x4d4d)
  tiff.setUint16(2, 42)
  tiff.setUint32(4, 8)
  // IFD0: one entry, the GPS IFD at 26.
  tiff.setUint16(8, 1)
  tiff.setUint16(10, 0x8825)
  tiff.setUint16(12, 4)
  tiff.setUint32(14, 1)
  tiff.setUint32(18, 26)
  tiff.setUint32(22, 0)
  // GPS IFD: refs and the two positions (degrees, minutes, seconds as rationals at 80 and 104).
  tiff.setUint16(26, 4)
  const entry = (i: number, tag: number, type: number, count: number, value: (o: number) => void) => {
    const o = 28 + i * 12
    tiff.setUint16(o, tag)
    tiff.setUint16(o + 2, type)
    tiff.setUint32(o + 4, count)
    value(o + 8)
  }
  entry(0, 1, 2, 2, o => tiff.setUint8(o, (lat < 0 ? 'S' : 'N').charCodeAt(0)))
  entry(1, 2, 5, 3, o => tiff.setUint32(o, 80))
  entry(2, 3, 2, 2, o => tiff.setUint8(o, (lon < 0 ? 'W' : 'E').charCodeAt(0)))
  entry(3, 4, 5, 3, o => tiff.setUint32(o, 104))
  const dms = (at: number, v: number) => {
    const a = Math.abs(v)
    const d = Math.floor(a), m = Math.floor((a - d) * 60), s = Math.round(((a - d) * 60 - m) * 60 * 100)
    ;[
      [d, 1],
      [m, 1],
      [s, 100],
    ].forEach(([n, den], i) => {
      tiff.setUint32(at + i * 8, n)
      tiff.setUint32(at + i * 8 + 4, den)
    })
  }
  dms(80, lat)
  dms(104, lon)
  const app1 = new Uint8Array(2 + 2 + 6 + 128)
  const v = new DataView(app1.buffer)
  v.setUint16(0, 0xffe1)
  v.setUint16(2, 2 + 6 + 128)
  app1.set([0x45, 0x78, 0x69, 0x66, 0, 0], 4)
  app1.set(new Uint8Array(tiff.buffer), 10)
  const out = new Uint8Array(2 + app1.length + 2)
  out.set([0xff, 0xd8])
  out.set(app1, 2)
  out.set([0xff, 0xd9], 2 + app1.length)
  return out.buffer
}

describe("the photo's GPS position and its country", () => {
  it('reads the position from the Exif block, none without one', () => {
    const at = exifGps(jpegWithGps(-0.22, -78.52))!
    expect(at.lat).toBeCloseTo(-0.22, 3)
    expect(at.lon).toBeCloseTo(-78.52, 3)
    expect(exifGps(new Uint8Array([0xff, 0xd8, 0xff, 0xd9]).buffer)).toBeNull()
    expect(exifGps(new Uint8Array([0x89, 0x50, 0x4e, 0x47]).buffer)).toBeNull()
    expect(exifGps(new ArrayBuffer(0))).toBeNull()
  })

  it('finds the country, on both sides of a border, off a simplified coast, and none outside the Neotropics', () => {
    expect(countryIn(shapes, -0.22, -78.52)).toBe('EC') // Quito
    expect(countryIn(shapes, -0.99, -77.81)).toBe('EC') // Tena
    expect(countryIn(shapes, 4.71, -74.07)).toBe('CO') // Bogotá
    expect(countryIn(shapes, -12.05, -77.04)).toBe('PE') // Lima, on the coast
    expect(countryIn(shapes, 0.81, -77.72)).toBe('EC') // Tulcán
    expect(countryIn(shapes, 0.83, -77.64)).toBe('CO') // Ipiales
    expect(countryIn(shapes, -0.74, -90.31)).toBe('EC') // Puerto Ayora, Galápagos
    expect(countryIn(shapes, 4.93, -52.33)).toBe('GF') // Cayenne
    expect(countryIn(shapes, 40.42, -3.7)).toBeNull() // Madrid
    expect(Object.keys(shapes.countries)).toHaveLength(49)
  })

  it('photoCountry: the GPS position to a country, with the outlines read once', async () => {
    vi.resetModules()
    const fetchMock = vi.fn(async () => new Response(JSON.stringify(shapes)))
    vi.stubGlobal('fetch', fetchMock)
    const { photoCountry } = await import('../photoCountry')
    expect(await photoCountry(jpegWithGps(-0.99, -77.81))).toBe('EC')
    expect(await photoCountry(jpegWithGps(4.71, -74.07))).toBe('CO')
    expect(await photoCountry(new Uint8Array([0xff, 0xd8, 0xff, 0xd9]).buffer)).toBeNull()
    expect(fetchMock).toHaveBeenCalledTimes(1)
  })
})

// ---------------------------------------------------------------- packs with several photos per taxon

const photo = (file: string, country: string | null, bytes = 100, extra: Partial<PackPhoto> = {}): PackPhoto => ({
  file,
  attribution: `(c) Ana ${file}, some rights reserved (CC BY)`,
  license: 'CC-BY',
  taxonUrl: null,
  source: `https://www.inaturalist.org/photos/${file.replace(/\D/g, '') || 1}`,
  bytes,
  country,
  ...extra,
})
const taxon = (name: string, countries: string[], photos: PackPhoto[], group: PackTaxon['group'] = 'ithomiini'): PackTaxon => {
  const p = name.split(' ')
  return { name, rank: p.length > 2 ? 'subspecies' : 'species', species: p.slice(0, 2).join(' '), genus: p[0], family: 'Nymphalidae', group, countries, photos }
}
const multi: PackIndex = {
  schema: 'ithomiini-photo-packs/2',
  version: 'm1',
  built: '2026-11-11',
  taxa: [
    taxon('Mechanitis polymnia', ['BR', 'CO', 'EC', 'PE'], [photo('10.webp', 'BR'), photo('11.webp', 'EC'), photo('12.webp', 'CO'), photo('13.webp', 'PE')]),
    taxon('Mechanitis polymnia casabranca', ['BR'], [photo('20.webp', 'BR')]),
    taxon('Mechanitis polymnia proceriformis', ['CO', 'EC', 'PE'], [photo('30.webp', null), photo('31.webp', 'EC')]),
    taxon('Mechanitis lysimnia', ['BR', 'EC'], [photo('40.webp', 'BR')]),
    // No live photo: a preserved specimen with its ID.
    taxon('Hyposcada kena', ['EC'], [photo('p1.webp', 'EC', 100, { kind: 'preserved', specimen: 'CAM040123', source: null })]),
  ],
}

function fakeCaches() {
  const store = new Map<string, Response>()
  const cache = {
    match: async (u: string) => store.get(String(u))?.clone(),
    put: async (u: string, r: Response) => void store.set(String(u), r),
    keys: async () => [...store.keys()].map(url => ({ url })),
    delete: async (r: string | { url: string }) => store.delete(typeof r === 'string' ? r : r.url),
  }
  vi.stubGlobal('caches', { open: async () => cache })
  return store
}
let online = true
beforeEach(() => {
  vi.resetModules()
  localStorage.clear()
  online = true
  Object.defineProperty(navigator, 'onLine', { configurable: true, get: () => online })
  vi.stubGlobal(
    'fetch',
    vi.fn(async (u: string) => {
      if (String(u).endsWith('/packs/index.json')) return new Response(JSON.stringify(multi))
      if (String(u).endsWith('/countries.json')) return new Response(JSON.stringify(shapes))
      if (String(u).endsWith('.json')) return new Response('{}', { status: 404 })
      return new Response(new Uint8Array(100), { headers: { 'content-type': 'image/webp' } })
    }),
  )
})

describe('a pack of several photos per taxon', () => {
  it('an Ecuador pack keeps the Ecuador photo, then a neighbour; sizes count those', async () => {
    const { chosenPhotos, scopeSize, countrySizes, PER_TAXON } = await import('../photoPacks')
    expect(PER_TAXON).toBe(2)
    const mp = multi.taxa[0]
    expect(chosenPhotos(mp, { region: ['EC'] }).map(p => p.file)).toEqual(['11.webp', '12.webp'])
    // Brazil: its own, then a neighbour (Colombia).
    expect(chosenPhotos(mp, { region: ['BR'] }).map(p => p.file)).toEqual(['10.webp', '12.webp'])
    // Neotropics: the builder's first two.
    expect(chosenPhotos(mp, { region: 'neotropics' }).map(p => p.file)).toEqual(['10.webp', '11.webp'])
    // Ecuador with subspecies: 2 + 2 (proceriformis) + 1 (lysimnia) + 1 (preserved) photos.
    expect(scopeSize(multi, { taxa: 'ithomiini', region: ['EC'], ssp: true })).toMatchObject({ photos: 6, bytes: 600 })
    expect(countrySizes(multi, { taxa: 'ithomiini', ssp: true }).get('EC')).toMatchObject({ photos: 6 })
  })

  it('installs each taxon with the photos it keeps, and a list of the first schema still reads', async () => {
    const store = fakeCaches()
    const { installPack, installedPack, normIndex } = await import('../photoPacks')
    const out = await installPack(multi, { taxa: 'ithomiini', region: ['EC'], ssp: false }, () => {})
    expect(out.kept).toBe(false)
    const kept = await installedPack()
    expect(kept?.index.taxa.find(t => t.name === 'Mechanitis polymnia')?.photos.map(p => p.file)).toEqual(['11.webp', '12.webp'])
    expect([...store.keys()].filter(u => u.includes('/photos/')).map(u => u.split('/').pop()).sort()).toEqual(['11.webp', '12.webp', '40.webp', 'p1.webp'])
    // The countries' outlines are kept with the photos (a GPS position read without signal).
    expect([...store.keys()].some(u => u.endsWith('/identificar/countries.json'))).toBe(true)
    // Schema 1 (one photo and its bytes per taxon), as a phone may keep it: one photo, country not known.
    const old = normIndex({ taxa: [{ name: 'Aeria elara', rank: 'species', species: 'Aeria elara', genus: 'Aeria', family: 'Nymphalidae', group: 'ithomiini', countries: ['EC'], photo: { file: 'a.webp', attribution: '(c) A', license: 'CC-BY', taxonUrl: null, source: null }, bytes: 900 }] })
    expect(old.taxa[0].photos).toEqual([{ file: 'a.webp', attribution: '(c) A', license: 'CC-BY', taxonUrl: null, source: null, bytes: 900, country: null }])
  })

  it('Compare offline from the pack: Mechanitis polymnia with Ecuador first, each photo with its country', async () => {
    fakeCaches()
    const { installPack } = await import('../photoPacks')
    await installPack(multi, { taxa: 'ithomiini', region: ['EC'], ssp: true }, () => {})
    online = false
    const { savedPhotos, orderRefs } = await import('../identifyRefs')
    const refs = (await savedPhotos(['Mechanitis polymnia']))['Mechanitis polymnia']
    const ordered = orderRefs(refs.photos, { home: ['EC'] })
    expect(ordered.map(p => [p.src.split('/').pop(), p.country, p.level])).toEqual([
      ['11.webp', 'EC', 'species'],
      ['31.webp', 'EC', 'subspecies'],
      ['12.webp', 'CO', 'species'],
      ['30.webp', null, 'subspecies'],
    ])
    // A subspecies of Colombia named: its range first.
    expect(orderRefs(refs.photos, { home: ['EC'], range: ['CO'] })[0].src).toMatch(/12\.webp$/)
  })

  it('online, the server list: the same order beyond the pack', async () => {
    fakeCaches()
    const { referencePhotos, orderRefs } = await import('../identifyRefs')
    const got = await referencePhotos(['Mechanitis polymnia'], { everySource: true })
    const ordered = orderRefs(got.refs['Mechanitis polymnia'].photos, { home: ['EC'] })
    expect(ordered.slice(0, 2).map(p => p.country)).toEqual(['EC', 'EC'])
    expect(ordered.map(p => p.country)).toContain('PE')
  })

  it('r5-fable-F1: a species and its subspecies sharing a file each keep their photo (Napeogenes lycora, offline)', async () => {
    fakeCaches()
    const shared: PackIndex = {
      ...multi,
      taxa: [
        taxon('Napeogenes lycora', ['EC'], [photo('70.webp', 'EC')]),
        taxon('Napeogenes lycora lycora', ['EC'], [photo('70.webp', 'EC')]),
        taxon('Napeogenes lycora apulia', ['EC'], [photo('71.webp', 'EC')]),
      ],
    }
    const { installPack } = await import('../photoPacks')
    await installPack(shared, { taxa: 'ithomiini', region: ['EC'], ssp: true }, () => {})
    online = false
    const { savedPhotos } = await import('../identifyRefs')
    const got = (await savedPhotos(['Napeogenes lycora']))['Napeogenes lycora'].photos
    expect(got.map(p => [p.level, p.taxon ?? null, p.src.split('/').pop()])).toEqual([
      ['species', null, '70.webp'],
      ['subspecies', 'Napeogenes lycora apulia', '71.webp'],
      ['subspecies', 'Napeogenes lycora lycora', '70.webp'],
    ])
  })

  it('a preserved specimen keeps its kind and ID to the screen', async () => {
    fakeCaches()
    const { referencePhotos } = await import('../identifyRefs')
    const got = await referencePhotos(['Hyposcada kena'], { everySource: true })
    expect(got.refs['Hyposcada kena'].photos[0]).toMatchObject({ kind: 'preserved', specimen: 'CAM040123', country: 'EC', level: 'species' })
  })
})
