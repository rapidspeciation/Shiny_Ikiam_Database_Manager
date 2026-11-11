import { createHash } from 'node:crypto'
import { afterEach, describe, expect, it, vi } from 'vitest'

// The phone's model downloads (offlineEngine.ts downloadRoles) with versions: two downloads that overlap must not drop
// each other's files from stored.json; the sex model the server lists is taken only with its SHA-256 and calibration;
// an update is offered when the phone has another version (or none recorded), and it keeps the installed version
// working until the new one is downloaded and checked, then swaps. The server is a stand-in (fetch); no AI.

const SEX_BYTES = new Uint8Array([1, 2, 3, 4, 5, 6, 7, 8])
const sha = (b: Uint8Array) => createHash('sha256').update(b).digest('hex')
vi.mock('../sexModel', async orig => {
  const b = new Uint8Array([1, 2, 3, 4, 5, 6, 7, 8])
  const h = createHash('sha256').update(b).digest('hex')
  return { ...(await orig<typeof import('../sexModel')>()), SEX_FILE: { name: 'sex_test.onnx', role: 'sex', bytes: 8, sha256: h, parts: [h] } }
})
// The lib tests share loaded modules in a worker (vite.config.ts, isolate: false): offlineEngine loaded by another file
// there (publicModels.test.ts) holds the real SEX_FILE, so it is loaded again here, with the mock.
vi.resetModules()
const { createEngine } = await import('../offlineEngine')
const { SEX_DEFAULT } = await import('../sexModel')
const { studentSizeOf } = await import('../offlineId')

const BASE = 'https://x.test/prueba-offline/'
function memoryCaches() {
  const store = new Map<string, { body: ArrayBuffer; headers: Headers }>()
  const cache = {
    match: async (k: string) => (store.has(k) ? new Response(store.get(k)!.body, { headers: store.get(k)!.headers }) : undefined),
    put: async (k: string, r: Response) => void store.set(k, { body: await r.arrayBuffer(), headers: r.headers }),
    delete: async (k: string | { url: string }) => store.delete(typeof k === 'string' ? k : k.url),
    keys: async () => [...store.keys()].map(url => ({ url })),
  }
  vi.stubGlobal('self', { crossOriginIsolated: false })
  return { store, caches: { open: async () => cache, delete: async () => true } }
}
const storedJson = (store: Map<string, { body: ArrayBuffer }>) => JSON.parse(new TextDecoder().decode(store.get(`${BASE}models/stored.json`)!.body))
const partKeys = (store: Map<string, unknown>) => [...store.keys()].filter(k => k.includes('?part=')).map(k => k.slice(BASE.length)).sort()

afterEach(() => vi.unstubAllGlobals())

const CAL_C = { cut: { t: 0.5, lo: -0.5, hi: 1.5 }, display: { a: 1, b: 0 }, genera: ['Mechanitis', 'Oleria'] }
const C_BYTES = new Uint8Array([7, 7, 7, 7, 7, 7, 7, 7, 7, 7])
const det = { name: 'wing_seg_s2.onnx', bytes: 4, etag: '"d"', role: 'detector' as const, version: 'sha256-det', sha256: sha(new Uint8Array([9, 9, 9, 9])) }
const files = [
  det,
  { name: 'student.onnx', bytes: 4, etag: '"s"', role: 'student' as const },
  { name: 'head.json', bytes: 4, etag: '"m"', role: 'head-meta' as const },
  { name: 'head.bin', bytes: 4, etag: '"h"', role: 'head' as const },
  { name: 'sex_test.onnx', bytes: 8, etag: '"x"', role: 'sex' as const, version: 'B 2026-11-08', sha256: sha(SEX_BYTES), calibration: SEX_DEFAULT },
]
/** The list once model C replaces model B on the server (a new file name, version, SHA-256 and calibration). */
const withC = (o: Record<string, unknown> = {}) => [
  ...files.filter(f => f.role !== 'sex'),
  { name: 'sex_c.onnx', bytes: C_BYTES.length, etag: '"c"', role: 'sex' as const, version: 'C 2026-11-20', sha256: sha(C_BYTES), calibration: CAL_C, ...o },
]
const bodyOf = (url: string) => (url.includes('sex_test') ? SEX_BYTES : url.includes('sex_c') ? C_BYTES : new Uint8Array([9, 9, 9, 9]))

/** A stand-in for the server's parts: `fail(url)` true answers a cut connection; `body` changes the bytes sent. */
function server(list: { name: string; etag: string; bytes: number }[], { fail = () => false, body = bodyOf }: { fail?: (u: string) => boolean; body?: (u: string) => Uint8Array<ArrayBuffer> } = {}) {
  const asked: string[] = []
  vi.stubGlobal('fetch', async (url: string) => {
    const u = String(url)
    asked.push(u)
    if (u.includes('/ort/')) return new Response('')
    if (fail(u)) throw new TypeError('Failed to fetch')
    const f = list.find(x => u.includes(`/models/${x.name}?`))!
    return new Response(body(u), { status: 200, headers: { etag: `W/${f.etag.slice(0, -1)}-p0"`, 'x-part-bytes': String(f.bytes) } })
  })
  return asked
}

/** A phone with model B and the detector kept before versions existed (as Franz's phone, downloaded 10 Nov). */
async function phoneWithOldB(store: Map<string, { body: ArrayBuffer; headers: Headers }>, etag = '"x"') {
  const put = (k: string, b: Uint8Array, e: string) => store.set(`${BASE}${k}`, { body: b.slice().buffer, headers: new Headers({ 'x-etag': e, 'x-bytes': String(b.length) }) })
  put('models/sex_test.onnx?part=0', SEX_BYTES, etag)
  put('models/wing_seg_s2.onnx?part=0', new Uint8Array([9, 9, 9, 9]), '"d"')
  const s = {
    at: '2026-11-10T12:00:00Z',
    files: { 'wing_seg_s2.onnx': { etag: '"d"', bytes: 4, role: 'detector' }, 'sex_test.onnx': { etag, bytes: 8, role: 'sex', parts: [sha(SEX_BYTES)] } },
  }
  store.set(`${BASE}models/stored.json`, { body: new TextEncoder().encode(JSON.stringify(s)).buffer, headers: new Headers() })
}

describe('sex model download', () => {
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
      return new Response(bodyOf(u), { status: 200, headers: { etag: `W/${f.etag.slice(0, -1)}-p0"`, 'x-part-bytes': String(f.bytes) } })
    })
    const engine = createEngine(BASE, () => [], { signedIn: () => false })
    const small = engine.download('small', files, () => {})
    await new Promise(r => setTimeout(r, 20))
    await engine.downloadSex(files, () => {})
    release()
    await small
    const stored = storedJson(store)
    expect(Object.keys(stored.files).sort()).toEqual(['head.bin', 'head.json', 'sex_test.onnx', 'student.onnx', 'wing_seg_s2.onnx'])
    expect(stored.files['sex_test.onnx']).toMatchObject({ version: 'B 2026-11-08', sha256: sha(SEX_BYTES), calibration: SEX_DEFAULT })
    expect(await engine.sexReady()).toBe(true)
  })

  it('a listed sex file without its SHA-256 or a valid calibration is not taken', async () => {
    const { caches } = memoryCaches()
    vi.stubGlobal('caches', caches)
    vi.stubGlobal('fetch', async () => new Response(''))
    const engine = createEngine(BASE, () => [], { signedIn: () => false })
    for (const change of [{ sha256: undefined }, { calibration: undefined }, { calibration: { ...CAL_C, cut: { t: 1, lo: 2, hi: 3 } } }]) {
      const other = withC(change)
      await expect(engine.downloadSex(other, () => {})).rejects.toThrow('missing-on-server')
      expect(engine.sexToDownload(other, null).total).toBe(4) // the detector only
      expect(engine.offer('sex', other, null).state).toBe('none')
    }
  })
})

describe('versions: what Descargas offers', () => {
  it('no stored version: an update (Franz’s phone with model B when model C is listed)', async () => {
    const { store, caches } = memoryCaches()
    vi.stubGlobal('caches', caches)
    await phoneWithOldB(store)
    const engine = createEngine(BASE, () => [], { signedIn: () => true })
    await engine.ready(withC())
    const o = engine.offer('sex', withC(), await engine.stored())
    expect(o).toMatchObject({ state: 'update', from: null, to: 'C 2026-11-20', plain: C_BYTES.length })
    // The same file name with other bytes (another ETag and SHA-256, no part hashes in common): older too.
    const sameName = files.map(f => (f.role === 'sex' ? { ...f, etag: '"other"', version: 'B2', sha256: sha(C_BYTES) } : f))
    expect(engine.offer('sex', sameName, await engine.stored())).toMatchObject({ state: 'update', from: null, to: 'B2' })
    // The installed one keeps working meanwhile.
    expect(await engine.sexReady()).toBe(true)
    expect((await engine.sexInfo())?.calibration).toEqual(SEX_DEFAULT)
  })

  it('no stored version but provably the listed bytes: the version is recorded, nothing offered, nothing downloaded', async () => {
    const { store, caches } = memoryCaches()
    vi.stubGlobal('caches', caches)
    await phoneWithOldB(store)
    const asked = server(files)
    const engine = createEngine(BASE, () => [], { signedIn: () => true })
    await engine.ready(files)
    expect(storedJson(store).files['sex_test.onnx']).toMatchObject({ version: 'B 2026-11-08', sha256: sha(SEX_BYTES), calibration: SEX_DEFAULT })
    expect(engine.offer('sex', files, await engine.stored())).toMatchObject({ state: 'current', from: 'B 2026-11-08', to: 'B 2026-11-08' })
    expect(asked).toEqual([])
  })

  it('the same version: nothing offered', async () => {
    const { store, caches } = memoryCaches()
    vi.stubGlobal('caches', caches)
    server(withC())
    const engine = createEngine(BASE, () => [], { signedIn: () => false })
    await engine.downloadSex(withC(), () => {})
    expect(engine.offer('sex', withC(), await engine.stored())).toMatchObject({ state: 'current', plain: 0 })
    // The same version from another place (another ETag, the same SHA-256): still nothing.
    expect(engine.offer('sex', withC({ etag: '"elsewhere"' }), await engine.stored()).state).toBe('current')
    expect(storedJson(store).files['sex_c.onnx'].version).toBe('C 2026-11-20')
  })

  it('nothing on the phone: a first download, not an update', async () => {
    const { caches } = memoryCaches()
    vi.stubGlobal('caches', caches)
    const engine = createEngine(BASE, () => [], { signedIn: () => true })
    expect(engine.offer('sex', withC(), null).state).toBe('missing')
    expect(engine.offer('small', withC(), null).state).toBe('missing')
  })
})

describe('versions: the update', () => {
  it('downloads C beside B, checks it, then swaps: stored.json names C with its version and calibration; B’s parts go', async () => {
    const { store, caches } = memoryCaches()
    vi.stubGlobal('caches', caches)
    await phoneWithOldB(store)
    server(withC())
    const engine = createEngine(BASE, () => [], { signedIn: () => false })
    const seen: string[][] = []
    const totals = new Set<number>()
    await engine.downloadSex(withC(), (_done, total) => (totals.add(total), seen.push(Object.keys(storedJson(store).files).sort())))
    // The progress counts what is fetched (C), not the detector the phone has already.
    expect([...totals]).toEqual([C_BYTES.length])
    // While it downloaded, stored.json still named model B (the phone kept using it).
    expect(seen.length).toBeGreaterThan(0)
    for (const names of seen) expect(names).toEqual(['sex_test.onnx', 'wing_seg_s2.onnx'])
    const s = storedJson(store)
    expect(Object.keys(s.files).sort()).toEqual(['sex_c.onnx', 'wing_seg_s2.onnx'])
    expect(s.files['sex_c.onnx']).toMatchObject({ version: 'C 2026-11-20', sha256: sha(C_BYTES), calibration: CAL_C, bytes: C_BYTES.length })
    expect(partKeys(store)).toEqual(['models/sex_c.onnx?part=0', 'models/wing_seg_s2.onnx?part=0'])
    expect(await engine.sexInfo()).toMatchObject({ name: 'sex_c.onnx', version: 'C 2026-11-20', calibration: CAL_C })
    expect(engine.offer('sex', withC(), await engine.stored()).state).toBe('current')
  })

  it('an interrupted update keeps the old model working; the next try resumes and swaps', async () => {
    const { store, caches } = memoryCaches()
    vi.stubGlobal('caches', caches)
    await phoneWithOldB(store)
    vi.useFakeTimers({ toFake: ['setTimeout'] })
    try {
      // The connection drops for every part of C (4 tries, then the download gives up).
      server(withC(), { fail: u => u.includes('sex_c') })
      const engine = createEngine(BASE, () => [], { signedIn: () => false })
      const run = engine.downloadSex(withC(), () => {})
      const done = run.catch(e => e)
      await vi.runAllTimersAsync()
      expect(String(await done)).toMatch(/Failed to fetch/)
      const s = storedJson(store)
      expect(Object.keys(s.files).sort()).toEqual(['sex_test.onnx', 'wing_seg_s2.onnx'])
      expect(new Uint8Array(store.get(`${BASE}models/sex_test.onnx?part=0`)!.body)).toEqual(SEX_BYTES)
      expect(await engine.sexReady()).toBe(true)
      expect((await engine.sexInfo())?.name).toBe('sex_test.onnx')
      expect(engine.offer('sex', withC(), await engine.stored()).state).toBe('update')
      // Signal back: the update finishes.
      server(withC())
      await engine.downloadSex(withC(), () => {})
      expect(Object.keys(storedJson(store).files).sort()).toEqual(['sex_c.onnx', 'wing_seg_s2.onnx'])
    } finally {
      vi.useRealTimers()
    }
  })

  it('a download that is not the listed SHA-256 is thrown away; the old model stays', async () => {
    const { store, caches } = memoryCaches()
    vi.stubGlobal('caches', caches)
    await phoneWithOldB(store)
    server(withC(), { body: u => (u.includes('sex_c') ? new Uint8Array(C_BYTES.length).fill(1) : bodyOf(u)) })
    const engine = createEngine(BASE, () => [], { signedIn: () => false })
    await expect(engine.downloadSex(withC(), () => {})).rejects.toThrow('corrupt')
    expect(Object.keys(storedJson(store).files).sort()).toEqual(['sex_test.onnx', 'wing_seg_s2.onnx'])
    expect(partKeys(store)).toEqual(['models/sex_test.onnx?part=0', 'models/wing_seg_s2.onnx?part=0'])
    expect(await engine.sexReady()).toBe(true)
  })

  it('the identifier: a new version under the same file name is kept under its own keys until it replaces the old', async () => {
    const { store, caches } = memoryCaches()
    vi.stubGlobal('caches', caches)
    server(files)
    const engine = createEngine(BASE, () => [], { signedIn: () => false })
    await engine.download('small', files, () => {})
    expect(engine.offer('small', files, await engine.stored()).state).toBe('current')
    // The server's small model changes in place (same name, new bytes, version and SHA-256).
    const NEW = new Uint8Array([5, 5, 5, 5])
    const changed = files.map(f => (f.role === 'student' ? { ...f, etag: '"s2"', version: 'sha256-new', sha256: sha(NEW) } : f))
    expect(engine.offer('small', changed, await engine.stored())).toMatchObject({ state: 'update', plain: 4 })
    expect(await engine.ready(changed)).toEqual({ small: true, full: false }) // still usable meanwhile
    const body = (u: string) => (u.includes('student') ? NEW : bodyOf(u))
    // Interrupted: the old bytes stay under the old keys.
    vi.useFakeTimers({ toFake: ['setTimeout'] })
    try {
      server(changed, { fail: u => u.includes('student'), body })
      const done = engine.download('small', changed, () => {}).catch(e => e)
      await vi.runAllTimersAsync()
      expect(await done).toBeInstanceOf(Error)
    } finally {
      vi.useRealTimers()
    }
    expect(new Uint8Array(store.get(`${BASE}models/student.onnx?part=0`)!.body)).toEqual(new Uint8Array([9, 9, 9, 9]))
    expect(storedJson(store).files['student.onnx'].slot).toBeUndefined()
    server(changed, { body })
    await engine.download('small', changed, () => {})
    const entry = storedJson(store).files['student.onnx']
    expect(entry).toMatchObject({ version: 'sha256-new', sha256: sha(NEW), slot: sha(NEW).slice(0, 16) })
    expect(new Uint8Array(store.get(`${BASE}models/student.onnx?part=0&v=${entry.slot}`)!.body)).toEqual(NEW)
    expect(store.has(`${BASE}models/student.onnx?part=0`)).toBe(false) // the old version's parts went after the swap
    expect(engine.offer('small', changed, await engine.stored()).state).toBe('current')
  })
})

describe('a newer small model at another input size (182 px) replacing L1 (192 px)', () => {
  const L1 = new Uint8Array([9, 9, 9, 9])
  const NEW = new Uint8Array([4, 4, 4, 4, 4, 4])
  /** The list with a newer small model (another file name, version, SHA-256 and input size). */
  const withNew = (o: Record<string, unknown> = {}) =>
    files.map(f => (f.role === 'student' ? { name: 'student_new_int8.onnx', bytes: NEW.length, etag: '"new"', role: 'student' as const, version: 'small-NEW-20261111', sha256: sha(NEW), input: { size: 182, values: 'clip' }, ...o } : f))
  const l1List = files.map(f => (f.role === 'student' ? { ...f, version: 'small-L1-20261031', sha256: sha(L1), input: { size: 192, values: 'clip' } } : f))
  const body = (u: string) => (u.includes('student_new') ? NEW : bodyOf(u))

  it('L1 kept without a version: an update to the newer model; the model and its input size swap together', async () => {
    const { store, caches } = memoryCaches()
    vi.stubGlobal('caches', caches)
    server(files)
    const engine = createEngine(BASE, () => [], { signedIn: () => false })
    await engine.download('small', files, () => {}) // as before versions: no version, no size stored
    const before = storedJson(store).files['student.onnx']
    expect(before.version).toBeUndefined()
    expect(studentSizeOf(before)).toBe(192)
    expect(engine.offer('small', withNew(), await engine.stored())).toMatchObject({ state: 'update', from: null, to: 'small-NEW-20261111', toBytes: NEW.length })
    server(withNew(), { body })
    await engine.download('small', withNew(), () => {})
    const s = storedJson(store)
    expect(s.files['student.onnx']).toBeUndefined()
    expect(s.files['student_new_int8.onnx']).toMatchObject({ version: 'small-NEW-20261111', sha256: sha(NEW), input: { size: 182 } })
    expect(studentSizeOf(s.files['student_new_int8.onnx'])).toBe(182)
    expect(partKeys(store).filter(k => k.includes('student'))).toEqual(['models/student_new_int8.onnx?part=0'])
    expect(engine.offer('small', withNew(), await engine.stored()).state).toBe('current')
  })

  it('L1 with its version: the same version shows nothing; the list from an older server (no size) still means 192', async () => {
    const { store, caches } = memoryCaches()
    vi.stubGlobal('caches', caches)
    server(l1List)
    const engine = createEngine(BASE, () => [], { signedIn: () => false })
    await engine.download('small', l1List, () => {})
    expect(storedJson(store).files['student.onnx']).toMatchObject({ version: 'small-L1-20261031', input: { size: 192 } })
    expect(engine.offer('small', l1List, await engine.stored()).state).toBe('current')
    expect(studentSizeOf({})).toBe(192)
    expect(studentSizeOf({ input: { size: 0 } })).toBe(192)
    expect(studentSizeOf({ input: { size: 182 } })).toBe(182)
  })

  it('an interrupted update keeps L1 and its 192 px', async () => {
    const { store, caches } = memoryCaches()
    vi.stubGlobal('caches', caches)
    server(l1List)
    const engine = createEngine(BASE, () => [], { signedIn: () => false })
    await engine.download('small', l1List, () => {})
    vi.useFakeTimers({ toFake: ['setTimeout'] })
    try {
      server(withNew(), { body, fail: u => u.includes('student_new') })
      const done = engine.download('small', withNew(), () => {}).catch(e => e)
      await vi.runAllTimersAsync()
      expect(await done).toBeInstanceOf(Error)
    } finally {
      vi.useRealTimers()
    }
    const s = storedJson(store)
    expect(s.files['student_new_int8.onnx']).toBeUndefined()
    expect(s.files['student.onnx']).toMatchObject({ version: 'small-L1-20261031', input: { size: 192 } })
    expect(new Uint8Array(store.get(`${BASE}models/student.onnx?part=0`)!.body)).toEqual(L1)
    expect(await engine.ready(withNew())).toEqual({ small: true, full: false })
    expect(engine.offer('small', withNew(), await engine.stored()).state).toBe('update')
  })
})

describe('review r1: rollback paths and what a cut download leaves', () => {
  it('model C back to B (the manifest removed on the server): a phone holding C is offered B and gets it, with B\'s calibration', async () => {
    const { store, caches } = memoryCaches()
    vi.stubGlobal('caches', caches)
    server(withC())
    const engine = createEngine(BASE, () => [], { signedIn: () => true })
    await engine.downloadSex(withC(), () => {})
    expect((await engine.sexInfo())?.version).toBe('C 2026-11-20')
    // The server lists B again.
    expect(engine.offer('sex', files, await engine.stored())).toMatchObject({ state: 'update', from: 'C 2026-11-20', to: 'B 2026-11-08' })
    server(files)
    await engine.downloadSex(files, () => {})
    const s = storedJson(store)
    expect(Object.keys(s.files).sort()).toEqual(['sex_test.onnx', 'wing_seg_s2.onnx'])
    expect(await engine.sexInfo()).toMatchObject({ name: 'sex_test.onnx', version: 'B 2026-11-08', calibration: SEX_DEFAULT })
    expect(partKeys(store).filter(k => k.includes('sex'))).toEqual(['models/sex_test.onnx?part=0'])
    expect(engine.offer('sex', files, await engine.stored()).state).toBe('current')
  })

  it('sign-out (forgetSex) also removes the parts of a sex model whose download was cut (never named in stored.json)', async () => {
    const { store, caches } = memoryCaches()
    vi.stubGlobal('caches', caches)
    await phoneWithOldB(store)
    // An update to C cut after its first part: C's part is cached under its own name, stored.json still names B.
    store.set(`${BASE}models/sex_c.onnx?part=0`, { body: C_BYTES.slice().buffer, headers: new Headers({ 'x-etag': '"c"', 'x-bytes': '10' }) })
    const engine = createEngine(BASE, () => [], { signedIn: () => false })
    await engine.forgetSex()
    expect(partKeys(store)).toEqual(['models/wing_seg_s2.onnx?part=0'])
    expect(Object.keys(storedJson(store).files)).toEqual(['wing_seg_s2.onnx'])
    // Nothing stored at all, only a cut first download: removed too.
    store.set(`${BASE}models/sex_c.onnx?part=0`, { body: C_BYTES.slice().buffer, headers: new Headers() })
    store.delete(`${BASE}models/stored.json`)
    await engine.forgetSex()
    expect(partKeys(store)).toEqual(['models/wing_seg_s2.onnx?part=0'])
  })

  it('«Borrar del teléfono» on the small model also removes a cut update\'s parts, and keeps what the other model needs', async () => {
    const { store, caches } = memoryCaches()
    vi.stubGlobal('caches', caches)
    server(files)
    const engine = createEngine(BASE, () => [], { signedIn: () => false })
    await engine.download('small', files, () => {})
    store.set(`${BASE}models/student_new_int8.onnx?part=0`, { body: new Uint8Array([1]).buffer, headers: new Headers() })
    await engine.removeModel('small')
    expect(partKeys(store).some(k => k.includes('student'))).toBe(false)
    expect(Object.keys(storedJson(store).files)).toEqual([])
  })

  it('S_B on a phone, offline, against the rollback build (the server offers L1 again): still runs at 182; online, L1 is offered', async () => {
    const { store, caches } = memoryCaches()
    vi.stubGlobal('caches', caches)
    const SB = new Uint8Array([4, 4, 4, 4, 4, 4])
    const sbList = files.map(f => (f.role === 'student' ? { name: 'student_SB_int8.onnx', bytes: SB.length, etag: '"sb"', role: 'student' as const, version: 'small-SB-20261111', sha256: sha(SB), input: { size: 182, values: 'clip' } } : f))
    server(sbList, { body: u => (u.includes('student_SB') ? SB : bodyOf(u)) })
    const engine = createEngine(BASE, () => [], { signedIn: () => true })
    await engine.download('small', sbList, () => {})
    const s = storedJson(store)
    // Offline (no list): usable, at the stored size.
    expect(await engine.ready(null)).toEqual({ small: true, full: false })
    expect(studentSizeOf(s.files['student_SB_int8.onnx'], 'student_SB_int8.onnx')).toBe(182)
    // A copy kept without its size (e.g. through an older page): its name still gives 182, L1's 192.
    expect(studentSizeOf({}, 'student_SB_int8.onnx')).toBe(182)
    expect(studentSizeOf({}, 'student_L1_int8.onnx')).toBe(192)
    expect(studentSizeOf({})).toBe(192)
    // Online with the rollback build's list (L1 at 192): an update to L1, S_B kept usable until it is in.
    const l1List = files.map(f => (f.role === 'student' ? { ...f, name: 'student_L1_int8.onnx', version: 'small-L1-20261031', sha256: sha(new Uint8Array([9, 9, 9, 9])), input: { size: 192, values: 'clip' } } : f))
    expect(engine.offer('small', l1List, await engine.stored())).toMatchObject({ state: 'update', from: 'small-SB-20261111', to: 'small-L1-20261031' })
    expect(await engine.ready(l1List)).toEqual({ small: true, full: false })
  })
})
