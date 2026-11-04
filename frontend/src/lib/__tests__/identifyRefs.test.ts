import { beforeEach, describe, expect, it, vi } from 'vitest'

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
        : new Response(new Uint8Array(String(u).includes('/s1') ? 300 : 1000)),
    ),
  )
})

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

  it('shows a species’ subspecies photos after its own, signed out', async () => {
    fakeCaches()
    const { referencePhotos } = await import('../identifyRefs')
    const { refs } = await referencePhotos(['Mechanitis polymnia', 'Mechanitis lysimnia'], { signedIn: false })
    expect(refs['Mechanitis polymnia'].photos.map(p => [p.level, p.taxon ?? null])).toEqual([
      ['species', null],
      ['subspecies', 'Mechanitis polymnia proceriformis'],
      ['subspecies', 'Mechanitis polymnia casabranca'],
    ])
    expect(refs['Mechanitis lysimnia'].photos).toHaveLength(1)
  })
})
