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

beforeEach(async () => {
  vi.resetModules()
  // Short waits between tries (lib/resumable.ts): a server error is tried again a few times.
  Object.assign((await import('../resumable')).timing, { base: 5, max: 20, stall: 2000 })
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

// 12 Nov 2026 (Franz switched wifi networks during a download and had to reload): a range cut midway goes on from the
// last byte that came, each photo is kept as soon as its bytes are in, no signal is waited for («Esperando
// conexión…») instead of counting photos as failed, and after a reload Descargas offers «Continuar».
describe('a pack save that outlasts a dropped connection', () => {
  /** Bundles by range, as a stream; `cut(n)` drops the n-th bundle answer after that many bytes; `down()`: no signal. */
  function streaming({ cut = (_n: number): number | null => null, down = (): boolean => false, etag = (_n: number): string => '"b"' } = {}) {
    let n = 0
    vi.stubGlobal(
      'fetch',
      vi.fn(async (u: string, init?: RequestInit) => {
        const url = String(u)
        const range = (init?.headers as Record<string, string> | undefined)?.range ?? null
        requests.push({ url, range })
        if (url.endsWith('.json')) return new Response('{}', { status: 404 })
        if (down()) throw new TypeError('Failed to fetch')
        const m = /bundles\/b00(\d)-x\.bin$/.exec(url)
        if (!m) return new Response('', { status: 404 })
        const all = built.bundles[Number(m[1])]
        const [s, e] = /bytes=(\d+)-(\d+)/.exec(range!)!.slice(1).map(Number)
        const body = all.slice(s, e + 1)
        const k = ++n
        const at = cut(k)
        let sent = 0
        const stream = new ReadableStream<Uint8Array>({
          pull(c) {
            if (at != null && sent >= at) return c.error(new TypeError('network changed'))
            const step = Math.min(64, body.length - sent, at != null ? at - sent : Infinity)
            if (step <= 0) return c.close()
            c.enqueue(body.slice(sent, sent + step))
            sent += step
          },
        })
        return new Response(stream, { status: 206, headers: { 'content-range': `bytes ${s}-${e}/${all.length}`, etag: etag(k) } })
      }),
    )
  }
  const offset = (i: number) => built.index.taxa[i].photos[0].at![1]

  it('a range cut midway: the next request asks from the last byte that came; the photos before it are kept already', async () => {
    const store = fakeCaches()
    // The first bundle's answer drops 500 bytes in (inside photo 4: photos 0-3 are whole).
    streaming({ cut: k => (k === 1 ? 500 : null) })
    const { installPack } = await import('../photoPacks')
    const steps: [number, number, string | undefined][] = []
    const out = await installPack(built.index, scope, (d, t, s) => steps.push([d, t, s]))
    expect(out).toMatchObject({ kept: false, failed: 0, total: 30 })
    const b0 = requests.filter(r => r.url.includes('b000'))
    expect(b0.map(r => r.range)).toEqual([`bytes=0-${built.bundles[0].length - 1}`, `bytes=500-${built.bundles[0].length - 1}`])
    expect(requests.some(r => r.url.endsWith('.webp'))).toBe(false) // nothing one by one
    expect(steps.some(s => s[2] === 'waiting')).toBe(true)
    expect(steps.every((s, i) => i === 0 || s[0] >= steps[i - 1][0])).toBe(true) // the count never goes back
    for (const i of [0, 3, 14, 29]) expect(await photoBytes(store, i)).toEqual(bytesOf(i))
    expect(offset(4)).toBeLessThan(500)
    expect(offset(5)).toBeGreaterThan(500)
  })

  it('no signal for a while: it waits («waiting»), counts nothing as failed, and finishes when the network is back', async () => {
    const store = fakeCaches()
    let down = false
    streaming({ cut: k => (k === 1 ? ((down = true), 300) : null), down: () => down })
    const { installPack } = await import('../photoPacks')
    let last: string | undefined
    const run = installPack(built.index, scope, (_d, _t, s) => (last = s))
    await vi.waitFor(() => expect(last).toBe('waiting'))
    await new Promise(r => setTimeout(r, 100)) // many tries meanwhile
    // The photos that came before the cut are on the phone already.
    expect(await photoBytes(store, 0)).toEqual(bytesOf(0))
    down = false
    const out = await run
    expect(out).toMatchObject({ kept: false, failed: 0, total: 30 })
    expect(last).toBeUndefined()
  })

  it('the bundle changed on the server meanwhile (another ETag): it goes on from the first photo not saved', async () => {
    const store = fakeCaches()
    streaming({ cut: k => (k === 1 ? 500 : null), etag: k => (k === 1 ? '"old"' : '"new"') })
    const { installPack } = await import('../photoPacks')
    expect(await installPack(built.index, scope, () => {})).toMatchObject({ failed: 0, total: 30 })
    const b0 = requests.filter(r => r.url.includes('b000')).map(r => r.range)
    // Cut at 500 (in photo 4); asked from 500, another ETag: asked again from photo 4's first byte.
    expect(b0).toEqual([`bytes=0-${built.bundles[0].length - 1}`, `bytes=500-${built.bundles[0].length - 1}`, `bytes=${offset(4)}-${built.bundles[0].length - 1}`])
    expect(await photoBytes(store, 4)).toEqual(bytesOf(4))
  })

  it('after a reload: «Continuar» with what is left; it downloads only the photos not saved', async () => {
    const store = fakeCaches()
    // A save cut off by a reload: its choice kept on the device, ten of its photos saved.
    const { photoSrc, INDEX_URL } = await import('../photoPacks')
    for (let i = 0; i < 10; i++) store.set(photoSrc(built.index, INDEX_URL(), nameOf(i)), new Response(bytesOf(i)))
    localStorage.setItem('identify:pack-saving', JSON.stringify(scope))
    vi.stubGlobal('fetch', vi.fn(async (u: string) => (String(u).endsWith('index.json') ? new Response(JSON.stringify(built.index), { headers: { etag: '"1"' } }) : new Response('', { status: 404 }))))
    const { usePacks } = await import('../usePacks')
    const packs = usePacks()
    await packs.load(true)
    const saved = Array.from({ length: 10 }, (_, i) => bytesOf(i).length).reduce((a, b) => a + b, 0)
    const all = Array.from({ length: 30 }, (_, i) => bytesOf(i).length).reduce((a, b) => a + b, 0)
    expect(packs.unfinished.value).toEqual({ scope, saved, left: all - saved })
    streaming()
    requests = []
    await packs.install(packs.unfinished.value!.scope)
    expect(requests.filter(r => r.url.includes('b000')).map(r => r.range)).toEqual([`bytes=${offset(10)}-${built.bundles[0].length - 1}`])
    expect(packs.pack.value?.installed.photos).toBe(30)
    expect(localStorage.getItem('identify:pack-saving')).toBeNull()
    await packs.load()
    expect(packs.unfinished.value).toBeNull()
  })
})
