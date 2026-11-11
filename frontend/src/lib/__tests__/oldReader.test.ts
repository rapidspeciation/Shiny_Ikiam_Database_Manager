import { spawnSync } from 'node:child_process'
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join, resolve } from 'node:path'
import { gzipSync } from 'node:zlib'
import { afterAll, beforeEach, describe, expect, it, vi } from 'vitest'
import type { PackIndex } from '../photoPacks'

// Review r4: (F1) a page of release 1 (642d9251, fixtures/photoPacks-642d9251.ts, its code unchanged), still open in a
// tab or offline on a phone, reads the list the server serves after this change and the installed record the new code
// writes, and loses no photo; (F2) a taxon the phone has saved still gets the server list's other photos, so the
// person's country comes first.

const python = process.env.PYTHON ?? 'python3'
const hasPillow = spawnSync(python, ['-I', '-c', 'import PIL'], { encoding: 'utf8' }).status === 0
const SCRIPT = resolve(import.meta.dirname, '../../../../scripts/photo-packs.py')
const dir = mkdtempSync(join(tmpdir(), 'old-reader-'))
afterAll(() => rmSync(dir, { recursive: true, force: true }))

/** The list as the server will serve it: scripts/photo-packs.py taxa, with a country photo and bundles. */
function buildList(): string {
  const img = (n: string) => join(dir, `${n}.png`)
  const made = spawnSync(python, ['-I', '-c', 'import sys\nfrom PIL import Image\nfor i, p in enumerate(sys.argv[1:]): Image.new("RGB", (500, 300), (50 * i, 120, 90)).save(p)', ...['br', 'ec', 'co', 'pe', 'b'].map(img)], { encoding: 'utf8' })
  expect(made.status, made.stderr).toBe(0)
  const t = (name: string, inatId: number, photo: object | null, countries = ['EC']) => ({ name, rank: 'species', genus: name.split(' ')[0], family: 'Nymphalidae', group: 'ithomiini', countries, inatId, photo })
  writeFileSync(
    join(dir, 'taxa.json'),
    JSON.stringify([
      t('Mechanitis polymnia', 148675, { url: img('br'), inatPhotoId: 59300165, attribution: '(c) A, some rights reserved (CC BY)', license: 'CC-BY', taxonUrl: null, source: 'https://www.inaturalist.org/photos/59300165' }, ['BR', 'EC']),
      t('Melinaea menophilus', 222, { url: img('b'), inatPhotoId: 7, attribution: '(c) B', license: 'CC-BY', taxonUrl: null, source: 'https://www.inaturalist.org/photos/7' }),
    ]),
  )
  // One observation photo of M. polymnia in each of Ecuador, Colombia and Peru.
  const obsArgs: string[] = []
  ;['ec', 'co', 'pe'].forEach((c, i) => {
    const obs = { id: i + 1, faves: 3, taxon: { id: 148675, name: 'Mechanitis polymnia', rank: 'species', anc: [] }, photos: [{ id: 102 + i, url: img(c), license: 'cc-by-nc', attribution: `(c) obs ${c}` }] }
    writeFileSync(join(dir, `obs-${c}.jsonl.gz`), gzipSync(JSON.stringify(obs) + '\n'))
    obsArgs.push('--obs', `${c.toUpperCase()}=${join(dir, `obs-${c}.jsonl.gz`)}`)
  })
  const out = join(dir, 'packs')
  const run = spawnSync(python, ['-I', SCRIPT, 'taxa', '--taxa', join(dir, 'taxa.json'), '--out', out, '--cell', 'EC:all', '--pause', '0', ...obsArgs], { encoding: 'utf8', timeout: 120_000 })
  expect(run.status, run.stderr + run.stdout).toBe(0)
  return out
}
const out = hasPillow ? buildList() : ''
const served = () => JSON.parse(readFileSync(join(out, 'index.json'), 'utf8')) as PackIndex & { taxa: { photo?: { file: string } | null; bytes?: number }[] }
/** The live list today (schema 1): the same taxa, one photo each (the taxon photo), as release 1 installed it. */
const schema1 = () => {
  const v = served()
  return { schema: 'ithomiini-photo-packs/1', version: 'live-1', built: v.built, base: v.base, taxa: v.taxa.map(({ photos: _p, ...x }) => x) }
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
let list: () => object = served
beforeEach(() => {
  vi.resetModules()
  localStorage.clear()
  list = served
  Object.defineProperty(navigator, 'onLine', { configurable: true, get: () => true })
  // The server: the list, and every file under packs/ (a bundle by range).
  vi.stubGlobal(
    'fetch',
    vi.fn(async (u: string, init?: RequestInit) => {
      const path = new URL(String(u)).pathname
      if (path.endsWith('/packs/index.json')) return new Response(JSON.stringify(list()), { headers: { etag: `"${(list() as PackIndex).version}"` } })
      const rel = /\/identificar\/packs\/(.+)$/.exec(path)?.[1]
      if (!rel) return new Response('', { status: 404 })
      let body: Uint8Array
      try {
        body = new Uint8Array(readFileSync(join(out, rel)))
      } catch {
        return new Response('', { status: 404 })
      }
      const range = (init?.headers as Record<string, string> | undefined)?.range
      const m = range && /bytes=(\d+)-(\d+)/.exec(range)
      if (m) return new Response(body.slice(Number(m[1]), Number(m[2]) + 1) as Uint8Array<ArrayBuffer>, { status: 206 })
      return new Response(body as Uint8Array<ArrayBuffer>, { headers: { 'content-type': rel.endsWith('.webp') ? 'image/webp' : 'application/octet-stream' } })
    }),
  )
})
const ecPack = { taxa: 'all' as const, region: ['EC'], ssp: true }
const cachedPhotos = (store: Map<string, Response>) => [...store.keys()].filter(k => k.includes('/packs/photos/')).map(k => k.split('/').pop()!).sort()

describe.skipIf(!hasPillow)('r4-sol-F1: a page of release 1 and the lists after this change', () => {
  it('the old installer on the new list keeps the pack it had and records the photos it has', async () => {
    const store = fakeCaches()
    // The phone's pack from today's live list, saved by release 1.
    list = schema1
    const old1 = await import('./fixtures/photoPacks-642d9251')
    await old1.installPack(schema1() as never, ecPack, () => {})
    const before = cachedPhotos(store)
    expect(before.length).toBe(2)
    // The server now serves the new list; a tab of release 1 saves again.
    vi.resetModules()
    list = served
    const old2 = await import('./fixtures/photoPacks-642d9251')
    const out = await old2.installPack(await old2.onlineIndex(), ecPack, () => {})
    expect(out).toMatchObject({ failed: 0, total: 2, kept: false })
    expect(out.pack?.installed.photos).toBe(2)
    // No photo lost: every photo the phone had is still there, and every photo the record names is on the phone.
    expect(cachedPhotos(store)).toEqual(before)
    for (const t of out.pack!.index.taxa) expect(store.has(new URL(t.photo!.file, out.pack!.index.base).href)).toBe(true)
  })

  it('a record the new code writes (two photos a taxon, from bundles) is read by release 1 with its photos', async () => {
    const store = fakeCaches()
    const fresh = await import('../photoPacks')
    const mine = await fresh.installPack(await fresh.onlineIndex(), ecPack, () => {})
    expect(mine.pack?.index.taxa.find(t => t.name === 'Mechanitis polymnia')?.photos.map(p => p.country)).toEqual(['EC', 'CO'])
    vi.resetModules()
    const old = await import('./fixtures/photoPacks-642d9251')
    const pack = await old.installedPack()
    expect(pack?.installed.photos).toBe(3)
    const l = old.lookup(pack!.index, old.INSTALLED_KEY())
    for (const name of ['Mechanitis polymnia', 'Melinaea menophilus']) {
      const p = old.taxonPhoto(l, l.byName.get(name)!)
      expect(p, name).not.toBeNull()
      expect(store.has(p!.src), name).toBe(true)
    }
    // And release 1 saving the same choice from the new list keeps a full pack (its record readable by both).
    const again = await old.installPack(await old.onlineIndex(), ecPack, () => {})
    expect(again).toMatchObject({ failed: 0, kept: false })
    expect(again.pack?.installed.photos).toBe(2)
  })
})

describe.skipIf(!hasPillow)('r4-sol-F2: the server list beside a saved pack', () => {
  it('a pack of release 1 (Brazilian photo, country not known) and the new list: with an Ecuador GPS photo, Ecuador first', async () => {
    fakeCaches()
    list = schema1
    const old = await import('./fixtures/photoPacks-642d9251')
    await old.installPack(schema1() as never, ecPack, () => {})
    vi.resetModules()
    list = served
    const { referencePhotos, orderRefs } = await import('../identifyRefs')
    const got = (await referencePhotos(['Mechanitis polymnia'], { signedIn: false })).refs['Mechanitis polymnia']
    expect(got.photos).toHaveLength(4)
    const ordered = orderRefs(got.photos, { home: ['EC'] })
    expect(ordered.map(p => p.country)).toEqual(['EC', 'CO', 'PE', null])
    expect(ordered[0].source).toBe('https://www.inaturalist.org/photos/102')
  })

  it('a two-photo pack: the server list adds the taxon’s other photos, each once', async () => {
    fakeCaches()
    const fresh = await import('../photoPacks')
    // The Ecuador pack keeps two of its four photos (Ecuador's and Colombia's).
    await fresh.installPack(await fresh.onlineIndex(), ecPack, () => {})
    const kept = (await fresh.installedPack())!.index.taxa.find(t => t.name === 'Mechanitis polymnia')!.photos
    vi.resetModules()
    const { referencePhotos } = await import('../identifyRefs')
    const got = (await referencePhotos(['Mechanitis polymnia'], { signedIn: false })).refs['Mechanitis polymnia']
    const all = served().taxa.find(t => (t as { name?: string }).name === 'Mechanitis polymnia') as unknown as { photos: { source: string }[] }
    expect([kept.length, all.photos.length]).toEqual([2, 4])
    expect(got.photos.map(p => p.source).sort()).toEqual(all.photos.map(p => p.source).sort())
  })
})
