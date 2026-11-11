import { createHash } from 'node:crypto'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import type { PackIndex, PackPhoto, PackTaxon } from '../photoPacks'

// Franz, 11 Nov 2026 (live, «4,002 of 4,852 photos», slow): «is it downloading one by one? Wouldn't it be faster to
// download something like how we download the models compressed in one file». A pack's photos come in a few range
// requests on bundles (scripts/photo-packs.py make_bundles), each cut into its photos and kept under the photo's own
// address; a dropped range is tried again alone; progress counts photos. And: a list the server replaced reaches an
// open page (his phone still showed «≈ 131 MB» for a choice the server had made exact).

/** A photo's bytes and its content-addressed name (<id>-<sha256, 8 hex>.webp), as the builder makes them. */
const bytesOf = (i: number) => new Uint8Array(Array.from({ length: 100 + i * 7 }, (_, k) => (k * 31 + i) & 255))
const nameOf = (i: number) => `${i}-${createHash('sha256').update(bytesOf(i)).digest('hex').slice(0, 8)}.webp`

/** 30 photos in 2 bundles (15 each), back to back. */
function build(): { index: PackIndex; bundles: Uint8Array[] } {
  const taxa: PackTaxon[] = []
  const bundles: Uint8Array[] = [new Uint8Array(0), new Uint8Array(0)]
  for (let i = 0; i < 30; i++) {
    const b = i < 15 ? 0 : 1
    const body = bytesOf(i)
    const at: [number, number] = [b, bundles[b].length]
    const grown = new Uint8Array(bundles[b].length + body.length)
    grown.set(bundles[b])
    grown.set(body, bundles[b].length)
    bundles[b] = grown
    const photo: PackPhoto = { file: nameOf(i), attribution: '(c) A', license: 'CC-BY', taxonUrl: null, source: null, bytes: body.length, country: 'EC', at }
    taxa.push({ name: `Genus sp${i}`, rank: 'species', species: `Genus sp${i}`, genus: 'Genus', family: 'Nymphalidae', group: 'ithomiini', countries: ['EC'], photos: [photo] })
  }
  return {
    index: { schema: 'ithomiini-photo-packs/2', version: 'b1', built: '2026-11-11', taxa, bundles: bundles.map((b, i) => ({ file: `bundles/b00${i}-x.bin`, bytes: b.length })) },
    bundles,
  }
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

let built = build()
let requests: { url: string; range: string | null }[] = []
/** A server: bundles by range (or whole when `noRanges`), single photos, failures on demand. */
function server({ noRanges = false, failFirst = 0, corrupt = false, corruptSingle = false, shortSingle = false } = {}) {
  let failures = failFirst
  vi.stubGlobal(
    'fetch',
    vi.fn(async (u: string, init?: RequestInit) => {
      const url = String(u)
      const range = (init?.headers as Record<string, string> | undefined)?.range ?? null
      requests.push({ url, range })
      if (url.endsWith('.json')) return new Response('{}', { status: 404 })
      const m = /bundles\/b00(\d)-x\.bin$/.exec(url)
      if (m) {
        if (failures-- > 0) throw new TypeError('network')
        const all = built.bundles[Number(m[1])].slice()
        if (corrupt) all[3] ^= 255
        if (noRanges || !range) return new Response(all, { status: 200 })
        const [s, e] = /bytes=(\d+)-(\d+)/.exec(range)!.slice(1).map(Number)
        return new Response(all.slice(s, e + 1), { status: 206, headers: { 'content-range': `bytes ${s}-${e}/${all.length}` } })
      }
      const i = Number(/\/(\d+)-[0-9a-f]{8}\.webp$/.exec(url)?.[1] ?? -1)
      if (i < 0) return new Response('', { status: 404 })
      const body = bytesOf(i).slice(0, shortSingle ? 50 : undefined)
      if (corruptSingle) body[5] ^= 255
      return new Response(body, { headers: { 'content-type': 'image/webp' } })
    }),
  )
}

beforeEach(() => {
  vi.resetModules()
  localStorage.clear()
  built = build()
  requests = []
  Object.defineProperty(navigator, 'onLine', { configurable: true, get: () => true })
})

const scope = { taxa: 'ithomiini' as const, region: ['EC'], ssp: true }
const photoBytes = async (store: Map<string, Response>, i: number) => {
  const r = [...store].find(([u]) => u.endsWith(`/${nameOf(i)}`))?.[1]
  return r ? new Uint8Array(await r.clone().arrayBuffer()) : null
}

describe('a pack saved from bundles', () => {
  it('two range requests for 30 photos, each kept under its own address with its own bytes; progress in photos', async () => {
    const store = fakeCaches()
    server()
    const { installPack } = await import('../photoPacks')
    const steps: [number, number][] = []
    const out = await installPack(built.index, scope, (d, t) => steps.push([d, t]))
    expect(out).toMatchObject({ kept: false, failed: 0, total: 30 })
    const photoRequests = requests.filter(r => !r.url.endsWith('.json'))
    expect(photoRequests.map(r => [r.url.split('/').pop(), r.range])).toEqual([
      ['b000-x.bin', `bytes=0-${built.bundles[0].length - 1}`],
      ['b001-x.bin', `bytes=0-${built.bundles[1].length - 1}`],
    ])
    for (const i of [0, 14, 15, 29]) expect(await photoBytes(store, i)).toEqual(bytesOf(i))
    expect(steps[0]).toEqual([0, 30])
    expect(steps.at(-1)).toEqual([30, 30])
    expect(out.pack?.installed).toMatchObject({ photos: 30 })
  })

  it('only what the phone lacks: separate runs become separate ranges, close ones one range', async () => {
    const store = fakeCaches()
    server()
    const { installPack, photoSrc, INDEX_URL } = await import('../photoPacks')
    // Photos 0-4 and 20-29 are on the phone already (a save cut short).
    for (const i of [0, 1, 2, 3, 4, ...Array.from({ length: 10 }, (_, k) => 20 + k)]) store.set(photoSrc(built.index, INDEX_URL(), nameOf(i)), new Response(bytesOf(i)))
    await installPack(built.index, scope, () => {})
    const ranges = requests.filter(r => r.range).map(r => [r.url.split('/').pop(), r.range])
    const at = (i: number) => built.index.taxa[i].photos[0].at![1]
    expect(ranges).toEqual([
      ['b000-x.bin', `bytes=${at(5)}-${built.bundles[0].length - 1}`],
      ['b001-x.bin', `bytes=0-${at(20) - 1}`],
    ])
    for (const i of [5, 19]) expect(await photoBytes(store, i)).toEqual(bytesOf(i))
  })

  it('a dropped range is asked again alone; a server without ranges sends whole bundles, still right', async () => {
    let store = fakeCaches()
    server({ failFirst: 1 })
    let { installPack } = await import('../photoPacks')
    expect(await installPack(built.index, scope, () => {})).toMatchObject({ failed: 0, total: 30 })
    expect(requests.filter(r => r.url.includes('/bundles/')).length).toBe(3)
    expect(await photoBytes(store, 7)).toEqual(bytesOf(7))

    vi.resetModules()
    requests = []
    store = fakeCaches()
    server({ noRanges: true })
    ;({ installPack } = await import('../photoPacks'))
    expect(await installPack(built.index, scope, () => {})).toMatchObject({ failed: 0, total: 30 })
    expect(await photoBytes(store, 22)).toEqual(bytesOf(22))
  })

  it('bytes that do not match a photo’s name: the bundle is refused and its photos come one by one', async () => {
    const store = fakeCaches()
    server({ corrupt: true })
    const { installPack } = await import('../photoPacks')
    expect(await installPack(built.index, scope, () => {})).toMatchObject({ failed: 0, total: 30 })
    expect(requests.filter(r => r.url.includes('/bundles/')).length).toBe(6) // 2 ranges x 3 tries
    expect(requests.filter(r => r.url.endsWith('.webp')).length).toBe(30)
    expect(await photoBytes(store, 0)).toEqual(bytesOf(0))
  }, 15_000)

  it('r4-sol-F3: bundle and single photo both wrong (bytes, or cut short): nothing kept, every photo counted failed', async () => {
    for (const bad of [{ corruptSingle: true }, { shortSingle: true }]) {
      vi.resetModules()
      requests = []
      const store = fakeCaches()
      server({ corrupt: true, ...bad })
      const { installPack } = await import('../photoPacks')
      const out = await installPack(built.index, scope, () => {})
      expect(out).toMatchObject({ failed: 30, total: 30, pack: null })
      expect([...store.keys()].filter(u => u.endsWith('.webp'))).toEqual([])
    }
  }, 30_000)

  it('a list without bundles: one request per photo, as before', async () => {
    fakeCaches()
    server()
    const { installPack } = await import('../photoPacks')
    const { bundles: _b, ...plain } = built.index
    expect(await installPack({ ...plain, taxa: plain.taxa.map(t => ({ ...t, photos: t.photos.map(({ at: _a, ...p }) => p) })) }, scope, () => {})).toMatchObject({ failed: 0, total: 30 })
    expect(requests.filter(r => r.url.endsWith('.webp')).length).toBe(30)
    expect(requests.some(r => r.url.includes('/bundles/'))).toBe(false)
  })
})

describe('a list the server replaced reaches an open page (B)', () => {
  const list = (version: string, bytes: number): PackIndex => ({
    schema: 'ithomiini-photo-packs/2',
    version,
    built: '2026-11-11',
    taxa: [{ name: 'Urania leilus', rank: 'species', species: 'Urania leilus', genus: 'Urania', family: 'Uraniidae', group: 'moth', countries: ['EC'], photos: [{ file: 'u.webp', attribution: '', license: 'CC-BY', taxonUrl: null, source: null, bytes, ...(version === 'old' ? { missing: true } : {}) }] }],
  })
  let served = list('old', 1000)
  let etag = '"1"'
  beforeEach(() => {
    served = list('old', 1000)
    etag = '"1"'
    requests = []
    vi.stubGlobal(
      'fetch',
      vi.fn(async (u: string, init?: RequestInit) => {
        requests.push({ url: String(u), range: String(init?.cache) })
        return new Response(JSON.stringify(served), { headers: { etag } })
      }),
    )
  })

  it('revalidateIndex: the same validator keeps the list read; a new one replaces it', async () => {
    const { onlineIndex, revalidateIndex } = await import('../photoPacks')
    const first = await onlineIndex()
    expect(await revalidateIndex()).toBe(first)
    served = list('new', 900)
    etag = '"2"'
    const fresh = await revalidateIndex()
    expect(fresh.version).toBe('new')
    expect(await onlineIndex()).toBe(fresh)
    // Every check asks the server (no-cache), never the browser's copy alone.
    expect(requests.every(r => r.range === 'no-cache')).toBe(true)
  })

  it('Descargas opened again: «≈» goes once the server has the photos', async () => {
    fakeCaches()
    const { usePacks } = await import('../usePacks')
    const { scopeSize } = await import('../photoPacks')
    const packs = usePacks()
    await packs.open()
    const all = { taxa: 'all' as const, region: ['EC'], ssp: true }
    expect(scopeSize(packs.index.value!, all)).toMatchObject({ missing: 1, bytes: 1000 })
    served = list('new', 900)
    etag = '"2"'
    await packs.refreshIndex(0)
    expect(packs.index.value?.version).toBe('new')
    expect(scopeSize(packs.index.value!, all)).toMatchObject({ missing: 0, bytes: 900 })
  })
})
