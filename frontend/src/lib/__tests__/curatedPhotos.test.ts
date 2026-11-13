import { readFileSync } from 'node:fs'
import { resolve } from 'node:path'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { curatedIndex, curatedPhotos, orderCurated, photoId, resetCurated, SIMILAR, wingPhoto, type CuratedIndex, type CuratedTaxon } from '../curatedPhotos'
import type { RefPhoto } from '../identifyRefs'

// The Search hero (Franz, 13 Nov 2026: «why the first photo that appeared for polymnia was chosen one with a bad
// angle ... is it because we prefer showing photos of Ecuador?»): the curated order puts the clearest photo first, the
// person's country first only among photos of similar quality, and gives the species' Sanger wing crop.

const pack = (id: string, country: string | null, extra: Partial<RefPhoto> = {}): RefPhoto => ({
  src: `https://x.test/identificar/packs/photos/${id}-0a1b2c3d.webp`,
  credit: '© A',
  license: 'CC-BY',
  level: 'species',
  source: `https://www.inaturalist.org/photos/${id}`,
  country,
  ...extra,
})
// Mechanitis polymnia as the live pack lists it: the taxon photo (country not known), then the best-faved photo of
// Ecuador, Colombia and Peru; with the scores the review gave (the Ecuador photo: oblique, small, judged poor).
const polymnia = [pack('59300165', null), pack('137194978', 'EC'), pack('581501869', 'CO'), pack('477327466', 'PE')]
const entry: CuratedTaxon = {
  field: [
    { id: '59300165', s: [4, 5, 5, 3], hero: 'yes' },
    { id: '137194978', s: [3, 3, 4, 2], hero: 'no' },
    { id: '581501869', s: [4, 4, 5, 3], hero: 'no' },
    { id: '477327466', s: [5, 5, 5, 4], hero: 'yes' },
  ],
  wing: { file: 'CAM074396d-9d202dcb.webp', specimen: 'CAM074396', view: 'dorsal' },
}
const ids = (ps: RefPhoto[]) => ps.map(p => photoId(p))
const EC = { home: ['EC'] }

afterEach(() => {
  vi.unstubAllGlobals()
  resetCurated()
})

describe('the curated order', () => {
  it('M. polymnia: the poor Ecuador photo no longer leads', () => {
    // Before: the country order alone put the Ecuador photo first.
    expect(ids(orderCurated(polymnia, null, EC))).toEqual(['137194978', '581501869', '477327466', '59300165'])
    // Now: the photos judged good first (Peru's 19 points, then the taxon photo's 17, within SIMILAR: Peru is a
    // neighbour, the taxon photo's country not known), then those judged poor (Colombia's 16 before Ecuador's 12).
    expect(ids(orderCurated(polymnia, entry, EC))).toEqual(['477327466', '59300165', '581501869', '137194978'])
  })

  it('the person’s country first only among photos of similar quality', () => {
    const e: CuratedTaxon = { field: [{ id: '1', s: [5, 5, 5, 5], hero: 'yes' }, { id: '2', s: [5, 5, 4, 4], hero: 'yes' }, { id: '3', s: [4, 4, 4, 3], hero: 'yes' }] }
    const ps = [pack('1', 'BR'), pack('2', 'EC'), pack('3', 'EC')]
    expect(SIMILAR).toBe(2)
    // 18 is within 2 of 20: Ecuador's first. 15 is not within 2 of Brazil's 20: after it.
    expect(ids(orderCurated(ps, e, EC))).toEqual(['2', '1', '3'])
    // Nobody's country known: by quality alone.
    expect(ids(orderCurated(ps, e, { home: [] }))).toEqual(['1', '2', '3'])
  })

  it('photos the review did not see come after those judged good or unsure, before those judged poor', () => {
    const e: CuratedTaxon = { field: [{ id: '1', s: [2, 2, 2, 2], hero: 'no' }, { id: '2', s: [3, 3, 3, 3], hero: 'unsure' }, { id: '3', s: [5, 5, 5, 5], hero: 'yes' }] }
    const ssp = pack('9', 'EC', { level: 'subspecies', taxon: 'A b c' })
    const server = { ...pack('8', 'PE'), src: '/api/identify/ref-photo?u=' + encodeURIComponent('https://inaturalist-open-data.s3.amazonaws.com/photos/8/medium.jpg'), source: null }
    const specimen: RefPhoto = { src: '/api/photo/CAM1d.JPG?w=400', credit: 'CAM1', license: '', level: 'specimen' }
    expect(ids(orderCurated([pack('1', 'EC'), pack('2', 'EC'), pack('3', 'BR'), server, ssp, specimen], e, EC))).toEqual(['3', '2', '9', '8', null, '1'])
  })

  it('a species without curation keeps the country order', () => {
    expect(ids(orderCurated(polymnia, {}, EC))).toEqual(ids(orderCurated(polymnia, null, EC)))
  })

  it('reads the photo id wherever the photo comes from', () => {
    expect(photoId({ src: 'https://a/identificar/packs/photos/59300165-be13e026.webp', source: 'https://www.inaturalist.org/observations/1' })).toBe('59300165')
    expect(photoId({ src: 'https://a/identificar/field/100974887.jpg' })).toBe('100974887')
    expect(photoId({ src: '/api/identify/ref-photo?u=' + encodeURIComponent('https://static.inaturalist.org/photos/42/small.jpg') })).toBe('42')
    expect(photoId({ src: 'blob:x', source: 'https://www.inaturalist.org/photos/7' })).toBe('7')
    expect(photoId({ src: '/api/photo/CAM1d.JPG' })).toBeNull()
  })

  it('the wing crop is a preserved specimen with its ID and the Sanger credit', () => {
    const w = wingPhoto(entry)!
    expect(w.src).toMatch(/\/identificar\/wings\/CAM074396d-9d202dcb\.webp$/)
    expect(w).toMatchObject({ kind: 'preserved', specimen: 'CAM074396', credit: 'Ikiam / Wellcome Sanger Institute', license: '' })
    expect(wingPhoto({ field: entry.field })).toBeNull()
  })
})

describe('curatedPhotos (for the Identify panel)', () => {
  it('orders, gives the hero and the wing; a subspecies uses its species', async () => {
    const doc: CuratedIndex = { schema: 'ithomiini-curated/1', taxa: { 'Mechanitis polymnia': entry } }
    vi.stubGlobal('fetch', vi.fn(async () => new Response(JSON.stringify(doc))))
    const got = await curatedPhotos('Mechanitis polymnia casabranca', polymnia, EC)
    expect(photoId(got.hero!)).toBe('477327466')
    expect(got.wing?.specimen).toBe('CAM074396')
    expect(ids(got.photos)[0]).toBe('477327466')
  })

  it('without the list: the country order, no wing; asked again next time', async () => {
    const f = vi.fn(async () => new Response('nope', { status: 404 }))
    vi.stubGlobal('fetch', f)
    const got = await curatedPhotos('Mechanitis polymnia', polymnia, EC)
    expect(photoId(got.hero!)).toBe('137194978')
    expect(got.wing).toBeNull()
    await curatedIndex()
    expect(f).toHaveBeenCalledTimes(2)
  })

  it('the published list is well formed: every species named, scores 1 to 5, the wing files exist', () => {
    const dir = resolve(import.meta.dirname, '../../../public/identificar')
    const doc = JSON.parse(readFileSync(resolve(dir, 'curated.json'), 'utf8')) as CuratedIndex
    expect(doc.schema).toBe('ithomiini-curated/1')
    const names = Object.keys(doc.taxa)
    expect(names.length).toBeGreaterThan(150)
    for (const [name, t] of Object.entries(doc.taxa)) {
      expect(name.split(' ')).toHaveLength(2)
      for (const f of t.field ?? []) {
        expect(f.id).toMatch(/^\d+$/)
        if (f.s) expect(f.s.every(v => v >= 1 && v <= 5)).toBe(true)
      }
      if (t.wing) expect(readFileSync(resolve(dir, 'wings', t.wing.file)).length).toBeGreaterThan(1000)
    }
    expect(doc.taxa['Mechanitis polymnia'].field?.find(f => f.id === '137194978')?.hero).toBe('no')
  })
})
