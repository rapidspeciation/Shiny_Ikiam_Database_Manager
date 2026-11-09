import { createHash } from 'node:crypto'
import { afterEach, describe, expect, it, vi } from 'vitest'

// The sex model's download beside an identifier model's (offlineEngine.ts downloadRoles): two downloads that overlap
// must not drop each other's files from stored.json, and only the pinned sex file is taken.

const SEX_BYTES = new Uint8Array([1, 2, 3, 4, 5, 6, 7, 8])
const sha = (b: Uint8Array) => createHash('sha256').update(b).digest('hex')
vi.mock('../sexModel', async orig => ({
  ...(await orig<typeof import('../sexModel')>()),
  SEX_FILE: { name: 'sex_test.onnx', role: 'sex', bytes: 8, sha256: 'x', parts: [sha(new Uint8Array([1, 2, 3, 4, 5, 6, 7, 8]))] },
}))
// The lib tests share loaded modules in a worker (vite.config.ts, isolate: false): offlineEngine loaded by another file
// there (publicModels.test.ts) holds the real SEX_FILE, so it is loaded again here, with the mock.
vi.resetModules()
const { createEngine } = await import('../offlineEngine')

function memoryCaches() {
  const store = new Map<string, { body: ArrayBuffer; headers: Headers }>()
  const cache = {
    match: async (k: string) => (store.has(k) ? new Response(store.get(k)!.body, { headers: store.get(k)!.headers }) : undefined),
    put: async (k: string, r: Response) => void store.set(k, { body: await r.arrayBuffer(), headers: r.headers }),
    delete: async (k: string) => store.delete(k),
    keys: async () => [...store.keys()].map(url => ({ url })),
  }
  vi.stubGlobal('self', { crossOriginIsolated: false })
  return { store, caches: { open: async () => cache, delete: async () => true } }
}

afterEach(() => vi.unstubAllGlobals())

describe('sex model download', () => {
  const files = [
    { name: 'wing_seg_s2.onnx', bytes: 4, etag: '"d"', role: 'detector' as const },
    { name: 'student.onnx', bytes: 4, etag: '"s"', role: 'student' as const },
    { name: 'head.json', bytes: 4, etag: '"m"', role: 'head-meta' as const },
    { name: 'head.bin', bytes: 4, etag: '"h"', role: 'head' as const },
    { name: 'sex_test.onnx', bytes: 8, etag: '"x"', role: 'sex' as const },
  ]
  const body = (url: string) => (url.includes('sex_test') ? SEX_BYTES : new Uint8Array([9, 9, 9, 9]))

  it('overlapping downloads keep both models in stored.json', async () => {
    const { store, caches } = memoryCaches()
    vi.stubGlobal('caches', caches)
    // The identifier's parts wait until the sex model is done, so the identifier's download writes stored.json last
    // from a snapshot taken before the sex model existed.
    let release!: () => void
    const gate = new Promise<void>(r => (release = r))
    vi.stubGlobal('fetch', async (url: string) => {
      const u = String(url)
      if (u.includes('/ort/')) return new Response('')
      if (u.includes('student')) await gate
      const f = files.find(x => u.includes(x.name))!
      return new Response(body(u), { status: 200, headers: { etag: `W/${f.etag.slice(0, -1)}-p0"`, 'x-part-bytes': String(f.bytes) } })
    })
    const engine = createEngine('https://x.test/prueba-offline/', () => [], { signedIn: () => false })
    const small = engine.download('small', files, () => {})
    await new Promise(r => setTimeout(r, 20))
    await engine.downloadSex(files, () => {})
    release()
    await small
    const stored = JSON.parse(new TextDecoder().decode(store.get('https://x.test/prueba-offline/models/stored.json')!.body))
    expect(Object.keys(stored.files).sort()).toEqual(['head.bin', 'head.json', 'sex_test.onnx', 'student.onnx', 'wing_seg_s2.onnx'])
    expect(await engine.sexReady(files)).toBe(true)
  })

  it('a listed sex file other than the pinned one is not taken', async () => {
    const { caches } = memoryCaches()
    vi.stubGlobal('caches', caches)
    vi.stubGlobal('fetch', async () => new Response(''))
    const engine = createEngine('https://x.test/prueba-offline/', () => [], { signedIn: () => false })
    const other = files.map(f => (f.role === 'sex' ? { ...f, bytes: 9 } : f))
    await expect(engine.downloadSex(other, () => {})).rejects.toThrow('missing-on-server')
    expect(engine.sexToDownload(other, null).total).toBe(4) // the detector only
  })
})
