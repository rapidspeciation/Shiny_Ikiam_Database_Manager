import { beforeEach, describe, expect, it, vi } from 'vitest'
import type { PackIndex, PackTaxon } from '../photoPacks'

// Photo packs (Identificar → Descargas): the list's shape, scope x region sizes, the install (a fake Cache Storage)
// with update and removal, and Compare's photos from the pack offline with the server's online fallback.

const tx = (name: string, group: PackTaxon['group'], countries: string[], file: string | null, bytes = 1000, family = 'Nymphalidae'): PackTaxon => {
  const parts = name.split(' ')
  return {
    name,
    rank: parts.length > 2 ? 'subspecies' : 'species',
    species: parts.slice(0, 2).join(' '),
    genus: parts[0],
    family,
    group,
    countries,
    photo: file ? { file, attribution: `(c) Ana ${file}, some rights reserved (CC BY)`, license: 'CC-BY', taxonUrl: null, source: `https://www.inaturalist.org/photos/${file.replace(/\D/g, '') || 1}` } : null,
    bytes: file ? bytes : 0,
  }
}
const index: PackIndex = {
  schema: 'ithomiini-photo-packs/1',
  version: 'v1',
  built: '2026-11-11',
  taxa: [
    tx('Mechanitis polymnia', 'ithomiini', ['EC', 'PE'], '1.webp', 1000),
    tx('Mechanitis polymnia casabranca', 'ithomiini', ['BR'], '2.webp', 300),
    tx('Mechanitis polymnia proceriformis', 'ithomiini', ['EC'], '3.webp', 400),
    tx('Mechanitis lysimnia', 'ithomiini', ['BR'], '4.webp', 2000),
    tx('Melinaea menophilus', 'ithomiini', ['EC'], '4.webp', 2000), // shares a file with M. lysimnia
    tx('Heliconius erato', 'nymphalidae', ['EC', 'CO'], '6.webp', 5000),
    tx('Morpho helenor', 'nymphalidae', [], '7.webp', 6000),
    tx('Papilio thoas', 'butterfly', ['CO'], '8.webp', 7000, 'Papilionidae'),
    tx('Urania leilus', 'moth', ['EC'], '9.webp', 8000, 'Uraniidae'),
    tx('Oleria onega', 'ithomiini', ['EC'], null),
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
let served: PackIndex = index
const fetched: string[] = []
beforeEach(() => {
  vi.resetModules()
  localStorage.clear()
  served = index
  fetched.length = 0
  vi.stubGlobal(
    'fetch',
    vi.fn(async (u: string) => {
      fetched.push(String(u))
      if (String(u).endsWith('/packs/index.json')) return new Response(JSON.stringify(served))
      if (String(u).endsWith('.json')) return new Response('{}', { status: 404 })
      return new Response(new Uint8Array(100), { headers: { 'content-type': 'image/webp' } })
    }),
  )
  Object.defineProperty(navigator, 'onLine', { configurable: true, get: () => true })
})

describe('pack list and scope', () => {
  it('takes the groups of each scope, the region and the subspecies', async () => {
    const { packTaxa } = await import('../photoPacks')
    const names = (s: Parameters<typeof packTaxa>[1]) => packTaxa(index, s).map(t => t.name)
    expect(names({ taxa: 'ithomiini', region: ['EC'], ssp: false })).toEqual(['Mechanitis polymnia', 'Melinaea menophilus'])
    expect(names({ taxa: 'ithomiini', region: ['EC'], ssp: true })).toEqual(['Mechanitis polymnia', 'Mechanitis polymnia proceriformis', 'Melinaea menophilus'])
    expect(names({ taxa: 'nymphalidae', region: ['EC'], ssp: false })).toContain('Heliconius erato')
    expect(names({ taxa: 'butterflies', region: ['CO'], ssp: false })).toEqual(['Heliconius erato', 'Papilio thoas'])
    expect(names({ taxa: 'all', region: ['EC'], ssp: false })).toContain('Urania leilus')
    expect(names({ taxa: 'butterflies', region: ['EC'], ssp: false })).not.toContain('Urania leilus')
    // A set of countries: any of them. No countries known: only the Neotropics.
    expect(names({ taxa: 'ithomiini', region: ['BR', 'PE'], ssp: true })).toEqual(['Mechanitis polymnia', 'Mechanitis polymnia casabranca', 'Mechanitis lysimnia'])
    expect(names({ taxa: 'nymphalidae', region: 'neotropics', ssp: false })).toContain('Morpho helenor')
    expect(names({ taxa: 'all', region: ['EC', 'CO', 'BR', 'PE'], ssp: true })).not.toContain('Morpho helenor')
    // No photo: never in a pack.
    expect(names({ taxa: 'all', region: 'neotropics', ssp: true })).not.toContain('Oleria onega')
  })

  it('sizes each scope x region, a shared file once', async () => {
    const { scopeSize, TAXA_SCOPES } = await import('../photoPacks')
    expect(scopeSize(index, { taxa: 'ithomiini', region: 'neotropics', ssp: false })).toEqual({ photos: 2, bytes: 3000 })
    expect(scopeSize(index, { taxa: 'ithomiini', region: ['EC'], ssp: true })).toEqual({ photos: 3, bytes: 3400 })
    const ec = TAXA_SCOPES.map(taxa => scopeSize(index, { taxa, region: ['EC'], ssp: true }))
    expect(ec.map(s => s.photos)).toEqual([3, 4, 4, 5])
    expect(ec.map(s => s.bytes)).toEqual([3400, 8400, 8400, 16400])
    // Each larger scope takes at least what the smaller one does, for every region.
    for (const region of [['EC'], ['BR'], ['CO', 'PE'], 'neotropics'] as const) {
      const sizes = TAXA_SCOPES.map(taxa => scopeSize(index, { taxa, region: region === 'neotropics' ? region : [...region], ssp: true }).bytes)
      expect([...sizes].sort((a, b) => a - b)).toEqual(sizes)
    }
  })

  it('reads a stored scope back safely, Ecuador by default', async () => {
    const { cleanScope, packScope, setPackScope, DEFAULT_PACK } = await import('../photoPacks')
    expect(packScope()).toEqual(DEFAULT_PACK)
    expect(DEFAULT_PACK.region).toEqual(['EC'])
    setPackScope({ taxa: 'all', region: ['CO', 'EC'], ssp: false })
    expect(packScope()).toEqual({ taxa: 'all', region: ['CO', 'EC'], ssp: false })
    expect(cleanScope({ taxa: 'x', region: ['ecuador'], ssp: 'yes' })).toEqual(DEFAULT_PACK)
    expect(cleanScope({ region: 'neotropics' }).region).toBe('neotropics')
  })

  it('lists the countries, Ecuador first', async () => {
    const { packCountries } = await import('../photoPacks')
    expect(packCountries(index)).toEqual(['EC', 'BR', 'CO', 'PE'])
  })
})

describe('install, update and removal', () => {
  it('saves the photos of a scope, replaces them with another and removes them', async () => {
    const store = fakeCaches()
    const { installPack, installedPack, removePack, updateAvailable, INSTALLED_KEY } = await import('../photoPacks')
    const photos = () => [...store.keys()].filter(u => /\/packs\/photos\//.test(u)).map(u => u.split('/').pop()).sort()
    const steps: number[] = []
    const first = await installPack(index, { taxa: 'ithomiini', region: ['EC'], ssp: true }, done => steps.push(done))
    expect(photos()).toEqual(['1.webp', '3.webp', '4.webp'])
    expect(first.installed).toMatchObject({ photos: 3, bytes: 300, failed: 0, version: 'v1' })
    expect(steps.at(-1)).toBe(3)
    expect(store.has(INSTALLED_KEY())).toBe(true)
    // A wider one downloads only what is new.
    fetched.length = 0
    await installPack(index, { taxa: 'all', region: ['EC'], ssp: true }, () => {})
    expect(fetched.map(u => u.split('/').pop()).sort()).toEqual(['6.webp', '9.webp'])
    // A narrower one removes what it no longer takes.
    await installPack(index, { taxa: 'ithomiini', region: ['EC'], ssp: false }, () => {})
    expect(photos()).toEqual(['1.webp', '4.webp'])
    const kept = await installedPack()
    expect(kept?.index.taxa.map(t => t.name)).toEqual(['Mechanitis polymnia', 'Melinaea menophilus'])
    expect(updateAvailable(kept?.installed, { version: 'v1' })).toBe(false)
    expect(updateAvailable(kept?.installed, { version: 'v2' })).toBe(true)
    await removePack()
    expect(photos()).toEqual([])
    expect(await installedPack()).toBeNull()
  })

  it('counts photos that failed, for the next update', async () => {
    fakeCaches()
    vi.stubGlobal('fetch', vi.fn(async (u: string) => (String(u).endsWith('3.webp') ? new Response('', { status: 500 }) : new Response(new Uint8Array(10), { headers: { 'content-type': 'image/webp' } }))))
    const { installPack } = await import('../photoPacks')
    const out = await installPack(index, { taxa: 'ithomiini', region: ['EC'], ssp: true }, () => {})
    expect(out.installed).toMatchObject({ photos: 2, failed: 1 })
    expect(out.index.taxa.map(t => t.name)).not.toContain('Mechanitis polymnia proceriformis')
  })
})

describe('Compare from the pack', () => {
  it('offline: the installed pack for any taxon of the head, the species then its subspecies', async () => {
    fakeCaches()
    const { installPack } = await import('../photoPacks')
    await installPack(index, { taxa: 'all', region: 'neotropics', ssp: true }, () => {})
    Object.defineProperty(navigator, 'onLine', { configurable: true, get: () => false })
    fetched.length = 0
    const { referencePhotos } = await import('../identifyRefs')
    const { refs } = await referencePhotos(['Mechanitis polymnia', 'Urania leilus', 'Papilio thoas', 'Oleria onega'], { signedIn: false })
    expect(refs['Mechanitis polymnia'].photos.map(p => [p.level, p.taxon ?? null])).toEqual([
      ['species', null],
      ['subspecies', 'Mechanitis polymnia casabranca'],
      ['subspecies', 'Mechanitis polymnia proceriformis'],
    ])
    expect(refs['Urania leilus'].photos[0]).toMatchObject({ level: 'species', credit: '© Ana 9.webp', license: 'CC-BY' })
    expect(refs['Papilio thoas'].photos).toHaveLength(1)
    expect(refs['Oleria onega']).toBeUndefined()
    // Nothing asked to the network without signal.
    expect(fetched.filter(u => !u.includes('/field/'))).toEqual([])
  })

  it('online without a pack photo: the server’s photos as before, the pack’s first when it has one', async () => {
    fakeCaches()
    const { installPack } = await import('../photoPacks')
    await installPack(index, { taxa: 'ithomiini', region: ['EC'], ssp: false }, () => {})
    vi.doMock('../api', () => ({
      api: async () => ({
        species: {
          'Papilio thoas': { pending: false, inat: { level: 'species', taxon: { url: 'https://www.inaturalist.org/taxa/1' }, photos: [{ thumb: 'https://inaturalist-open-data.s3.amazonaws.com/photos/50/small.jpg', attribution: '(c) Bo', license: 'CC-BY-NC' }] } },
          'Mechanitis polymnia': { pending: false, inat: { level: 'species', photos: [{ thumb: 'https://inaturalist-open-data.s3.amazonaws.com/photos/1/small.jpg', attribution: '(c) Ana', license: 'CC-BY' }, { thumb: 'https://inaturalist-open-data.s3.amazonaws.com/photos/77/small.jpg', attribution: '(c) Cy', license: 'CC0' }] } },
        },
      }),
    }))
    // The server's list is not reachable here: only the installed pack and the team's photos.
    served = { ...index, taxa: [] }
    const { referencePhotos } = await import('../identifyRefs')
    const { refs } = await referencePhotos(['Papilio thoas', 'Mechanitis polymnia'], { signedIn: true })
    expect(refs['Papilio thoas'].photos.map(p => p.credit)).toEqual(['© Bo'])
    // The pack's photo first; the server's copy of the same iNaturalist photo (photos/1) not twice.
    expect(refs['Mechanitis polymnia'].photos.map(p => p.credit)).toEqual(['© Ana 1.webp', '© Cy'])
    vi.doUnmock('../api')
  })

  it('signed out and online: the server’s whole list beyond the pack', async () => {
    fakeCaches()
    const { referencePhotos } = await import('../identifyRefs')
    const { refs } = await referencePhotos(['Heliconius erato'], { signedIn: false })
    expect(refs['Heliconius erato'].photos[0].src).toMatch(/\/identificar\/packs\/photos\/6\.webp$/)
  })
})
