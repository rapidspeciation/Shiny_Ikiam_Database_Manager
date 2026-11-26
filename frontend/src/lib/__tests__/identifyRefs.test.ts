import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import type { RefPhoto } from '../identifyRefs'

// «Fotos para el campo»: the choice of what to save, its size, the save itself (a fake Cache Storage) and the
// subspecies photos after their species' photo.
const index = {
  species: [
    { name: 'Mechanitis polymnia', file: '1.jpg', attribution: '(c) Ana, some rights reserved (CC BY)', license: 'CC-BY', taxonUrl: null, ec: true, bytes: 1000 },
    { name: 'Mechanitis lysimnia', file: '2.jpg', attribution: '(c) Bo', license: 'CC-BY-NC', taxonUrl: null, ec: false, bytes: 2000 },
    { name: 'Melinaea menophilus', file: '2.jpg', attribution: '(c) Bo', license: 'CC-BY-NC', taxonUrl: null, ec: true, bytes: 2000 },
  ],
  subspecies: [
    { name: 'Mechanitis polymnia proceriformis', file: 's10.jpg', attribution: '(c) Cy', license: 'CC0', taxonUrl: null, ec: true, bytes: 300 },
    { name: 'Mechanitis polymnia casabranca', file: 's11.jpg', attribution: '(c) Di', license: 'CC-BY', taxonUrl: null, ec: false, bytes: 400 },
  ],
}

function fakeCaches() {
  const store = new Map<string, Response>()
  const cache = {
    match: async (u: string) => store.get(u)?.clone(),
    put: async (u: string, r: Response) => void store.set(u, r),
    keys: async () => [...store.keys()].map(url => ({ url })),
    delete: async (r: { url: string }) => store.delete(r.url),
  }
  vi.stubGlobal('caches', { open: async () => cache })
  return store
}

beforeEach(() => {
  vi.resetModules()
  localStorage.clear()
  vi.stubGlobal(
    'fetch',
    vi.fn(async (u: string) =>
      String(u).endsWith('index.json')
        ? new Response(JSON.stringify(index))
        : // the server past its limit for visitors (server/public-reads.mjs): the photos the phone can reach instead
          String(u).includes('api/species-photos')
          ? new Response(JSON.stringify({ error: { code: 'RATE_LIMITED', message: 'Too many requests' } }), { status: 429 })
          : new Response(new Uint8Array(String(u).includes('/s1') ? 300 : 1000)),
    ),
  )
})

// The lib tests share a worker (isolate: false): the stubs go with the file.
afterEach(() => vi.unstubAllGlobals())

describe('field photos', () => {
  it('chooses by scope and counts a shared photo once', async () => {
    const { inScope, sizeOf } = await import('../identifyRefs')
    const all = inScope(index, { species: 'all', ssp: 'none' })
    expect(all.species).toHaveLength(3)
    expect(all.subspecies).toHaveLength(0)
    expect(sizeOf(all.species)).toEqual({ photos: 3, bytes: 3000 })
    const ec = inScope(index, { species: 'ec', ssp: 'ec' })
    expect(ec.species.map(e => e.name)).toEqual(['Mechanitis polymnia', 'Melinaea menophilus'])
    expect(ec.subspecies.map(e => e.name)).toEqual(['Mechanitis polymnia proceriformis'])
  })

  it('defaults to every species and Ecuador’s subspecies, saved after the download', async () => {
    const { fieldScope, setFieldScope } = await import('../identifyRefs')
    expect(fieldScope()).toEqual({ species: 'all', ssp: 'ec', auto: true })
    setFieldScope({ species: 'ec', ssp: 'none', auto: false })
    expect(fieldScope()).toEqual({ species: 'ec', ssp: 'none', auto: false })
    localStorage.setItem('identify:field-scope', '{"species":"x","ssp":"y"}')
    expect(fieldScope()).toEqual({ species: 'all', ssp: 'ec', auto: true })
  })

  it('saves the chosen photos and removes those no longer chosen', async () => {
    const store = fakeCaches()
    const { saveFieldPhotos, fieldSet } = await import('../identifyRefs')
    const first = await saveFieldPhotos({ species: 'all', ssp: 'all' }, () => {})
    expect(first).toMatchObject({ count: 5, ssp: 2, failed: 0, total: 4 })
    const files = () => [...store.keys()].filter(u => u.includes('/identificar/field/')).map(u => u.split('/').pop()).sort()
    expect(files()).toEqual(['1.jpg', '2.jpg', 's10.jpg', 's11.jpg'])
    const second = await saveFieldPhotos({ species: 'ec', ssp: 'ec' }, () => {})
    expect(second).toMatchObject({ count: 3, ssp: 1, scope: { species: 'ec', ssp: 'ec' } })
    expect(files()).toEqual(['1.jpg', '2.jpg', 's10.jpg'])
    expect(fieldSet()?.scope).toEqual({ species: 'ec', ssp: 'ec' })
  })

  it('signed out and online, asks the server: iNaturalist photos through the proxy and the project’s specimens', async () => {
    fakeCaches()
    const asked: string[] = []
    vi.stubGlobal(
      'fetch',
      vi.fn(async (u: string, init?: RequestInit) => {
        if (String(u).endsWith('index.json')) return new Response(JSON.stringify(index))
        if (!String(u).includes('api/species-photos')) return new Response(new Uint8Array(10))
        asked.push(String(init?.body))
        const thumb = 'https://inaturalist-open-data.s3.amazonaws.com/photos/77/small.jpg'
        return new Response(
          JSON.stringify({
            species: {
              'Mechanitis lysimnia': {
                inat: { level: 'species', taxon: { url: 'https://www.inaturalist.org/taxa/1' }, photos: [{ thumb, attribution: '(c) Eva, some rights reserved', license: 'CC-BY' }] },
                pending: false,
                specimens: [{ cam: 'CAM000001', sex: 'female', dorsal: '1abcdefghijklmnopqrstuvwxyz0123' }],
              },
            },
          }),
          { headers: { 'content-type': 'application/json' } },
        )
      }),
    )
    const { referencePhotos } = await import('../identifyRefs')
    const { refs } = await referencePhotos(['Mechanitis lysimnia'])
    expect(JSON.parse(asked[0])).toEqual({ names: ['Mechanitis lysimnia'] })
    const photos = refs['Mechanitis lysimnia'].photos
    expect(photos[0]).toMatchObject({ level: 'species', credit: '© Eva', license: 'CC-BY', source: 'https://www.inaturalist.org/photos/77' })
    expect(photos[0].src).toContain('api/identify/ref-photo?u=')
    expect(photos.find(p => p.level === 'specimen')).toMatchObject({ credit: 'CAM000001', sex: 'female', src: expect.stringContaining('api/photo/1abcdefghijklmnopqrstuvwxyz0123?w=400') })
  })

  it('shows a species’ subspecies photos after its own, signed out, when the server does not answer', async () => {
    fakeCaches()
    const { referencePhotos } = await import('../identifyRefs')
    const { visitorLimited } = await import('../visitorLimit')
    const { refs } = await referencePhotos(['Mechanitis polymnia', 'Mechanitis lysimnia'])
    // the refusal was the visitors' limit: the page says so
    expect(visitorLimited.value).toBe(true)
    expect(refs['Mechanitis polymnia'].photos.map(p => [p.level, p.taxon ?? null])).toEqual([
      ['species', null],
      ['subspecies', 'Mechanitis polymnia proceriformis'],
      ['subspecies', 'Mechanitis polymnia casabranca'],
    ])
    expect(refs['Mechanitis lysimnia'].photos).toHaveLength(1)
  })
})

// The subspecies the model names, first among a species' reference photos (Franz's phone photo of 26 Nov 2026: a
// rayed Napo Heliconius melpomene read as H. m. schunkei showed a red-band postman first).

const SCHUNKEI = 'Heliconius melpomene schunkei'
const sp = (id: number, country: string | null = 'EC'): RefPhoto => ({
  src: `https://x.test/sp${id}.jpg`,
  credit: `© S${id}`,
  license: 'CC-BY',
  level: 'species',
  source: `https://www.inaturalist.org/photos/${id}`,
  country,
})
const ssp = (id: number, taxon: string): RefPhoto => ({
  src: `https://x.test/ssp${id}.jpg`,
  credit: `© P${id}`,
  license: 'CC-BY',
  level: 'subspecies',
  taxon,
  source: `https://www.inaturalist.org/photos/${id}`,
  country: 'PE',
})
const specimen: RefPhoto = { src: 'https://x.test/api/photo/abc?w=400', credit: 'CAM000001', license: '', level: 'specimen', sex: 'male' }
const wing: RefPhoto = { src: 'https://x.test/wing.webp', credit: 'Sanger', license: '', level: 'species', kind: 'preserved', specimen: 'CAM1' }

describe('subspeciesFirst', () => {
  it('puts the named subspecies ahead of the curated hero and every species photo, the rest in their order', async () => {
    const { subspeciesFirst } = await import('../identifyRefs')
    // As refsShown gets them from the curation: the hero (a red-band postman), then the rest.
    const curated = [sp(1), sp(2, 'BR'), ssp(10, 'Heliconius melpomene malleti'), ssp(11, SCHUNKEI), specimen, wing]
    const extra = [ssp(20, SCHUNKEI), ssp(21, SCHUNKEI)]
    const out = subspeciesFirst(curated, SCHUNKEI, extra)
    expect(out.map(p => p.src)).toEqual([
      'https://x.test/ssp11.jpg',
      'https://x.test/ssp20.jpg',
      'https://x.test/ssp21.jpg',
      'https://x.test/sp1.jpg',
      'https://x.test/sp2.jpg',
      'https://x.test/ssp10.jpg',
      specimen.src,
      wing.src,
    ])
    expect(out.slice(0, 3).every(p => p.level === 'subspecies' && p.taxon === SCHUNKEI)).toBe(true)
  })

  it('shows each photo once: the same iNaturalist photo as the species and as the subspecies goes first, once', async () => {
    const { subspeciesFirst } = await import('../identifyRefs')
    const sameAsSpecies = { ...ssp(1, SCHUNKEI), src: 'https://x.test/other-size.jpg' }
    const out = subspeciesFirst([sp(1), sp(2)], SCHUNKEI, [sameAsSpecies, sameAsSpecies])
    expect(out.map(p => p.src)).toEqual(['https://x.test/other-size.jpg', 'https://x.test/sp2.jpg'])
  })

  it('matches the name in any case and spacing', async () => {
    const { subspeciesFirst } = await import('../identifyRefs')
    const out = subspeciesFirst([sp(1), ssp(5, 'heliconius  melpomene SCHUNKEI')], SCHUNKEI)
    expect(out[0].src).toBe('https://x.test/ssp5.jpg')
  })

  it('without a subspecies, or with no photo of it, leaves the order exactly as it was', async () => {
    const { subspeciesFirst } = await import('../identifyRefs')
    const photos = [sp(1), sp(2), ssp(10, 'Heliconius melpomene malleti'), specimen, wing]
    expect(subspeciesFirst(photos, null)).toBe(photos)
    expect(subspeciesFirst(photos, undefined, [ssp(20, SCHUNKEI)])).toBe(photos)
    expect(subspeciesFirst(photos, SCHUNKEI)).toBe(photos)
    expect(subspeciesFirst(photos, SCHUNKEI, [ssp(30, 'Heliconius melpomene rosina')])).toBe(photos)
  })
})

describe('subspeciesRefPhotos', () => {
  const thumb = 'https://inaturalist-open-data.s3.amazonaws.com/photos/88/small.jpg'
  const answer = (level: string) => ({
    species: {
      [SCHUNKEI]: {
        inat: { level, taxon: { url: 'https://www.inaturalist.org/taxa/9' }, photos: [{ thumb, attribution: '(c) Ana, some rights reserved', license: 'CC-BY', country: 'PE' }] },
        pending: false,
        specimens: [{ cam: 'CAM000002', sex: 'female', dorsal: '1abcdefghijklmnopqrstuvwxyz0123' }],
      },
    },
  })
  const server = (body: unknown, asked: string[] = []) =>
    vi.stubGlobal(
      'fetch',
      vi.fn(async (u: string, init?: RequestInit) => {
        if (!String(u).includes('api/species-photos')) return new Response('{}', { status: 404 })
        asked.push(String(init?.body))
        return new Response(JSON.stringify(body), { headers: { 'content-type': 'application/json' } })
      }),
    )

  it('takes iNaturalist’s photos of the subspecies itself, labelled as it, without the specimens', async () => {
    fakeCaches()
    const asked: string[] = []
    server(answer('subspecies'), asked)
    const { subspeciesRefPhotos } = await import('../identifyRefs')
    const got = await subspeciesRefPhotos(SCHUNKEI)
    expect(JSON.parse(asked[0])).toEqual({ names: [SCHUNKEI] })
    expect(got.pending).toBe(false)
    expect(got.photos).toHaveLength(1)
    expect(got.photos[0]).toMatchObject({ level: 'subspecies', taxon: SCHUNKEI, credit: '© Ana', country: 'PE', source: 'https://www.inaturalist.org/photos/88' })
    expect(got.photos[0].src).toContain('api/identify/ref-photo?u=')
  })

  it('a subspecies iNaturalist has no photos of (the server fell back to the species) adds nothing', async () => {
    fakeCaches()
    server(answer('species'))
    const { subspeciesRefPhotos } = await import('../identifyRefs')
    expect(await subspeciesRefPhotos(SCHUNKEI)).toEqual({ photos: [], pending: false })
  })

  it('a binomial is not asked; without signal, what the phone kept', async () => {
    fakeCaches()
    const asked: string[] = []
    server(answer('subspecies'), asked)
    const { subspeciesRefPhotos } = await import('../identifyRefs')
    expect(await subspeciesRefPhotos('Heliconius melpomene')).toEqual({ photos: [], pending: false })
    expect(asked).toHaveLength(0)
    await subspeciesRefPhotos(SCHUNKEI) // kept on the phone
    vi.stubGlobal('navigator', { ...navigator, onLine: false })
    const offline = await subspeciesRefPhotos(SCHUNKEI)
    expect(offline.photos.map(p => p.taxon)).toEqual([SCHUNKEI])
    expect(asked).toHaveLength(1)
  })
})
