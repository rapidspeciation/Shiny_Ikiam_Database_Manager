import { createHash } from 'node:crypto'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'

// A model download that outlasts a dropped signal (12 Nov 2026: Franz changed wifi networks during the fast model's
// update and had to reload): it waits for the network instead of failing, goes on from the last byte that came (a
// Range request for the rest), keeps 4 MB pieces on the phone so a reload goes on from there («Continuar»), starts
// over cleanly when the file changed on the server, and downloads a part again when its hash is wrong. The servers
// (the app's parts and Hugging Face) are stand-ins (fetch); no AI, no network.

vi.resetModules()
const { createEngine, PART, PIECE, pieceKey } = await import('../offlineEngine')
const { timing } = await import('../resumable')

const sha = (b: Uint8Array) => createHash('sha256').update(b).digest('hex')
const bytesOf = (n: number, seed: number) => {
  const b = new Uint8Array(n)
  for (let i = 0; i < n; i++) b[i] = (i * 31 + seed * 7 + (i >> 11)) & 255
  return b
}
const MB = 1024 * 1024
/** Byte arrays joined (never spread: 40 MB as a JS array is slow and huge). */
const join = (...parts: Uint8Array[]) => {
  const out = new Uint8Array(parts.reduce((a, p) => a + p.length, 0))
  let o = 0
  for (const p of parts) (out.set(p, o), (o += p.length))
  return out
}

let n = 0
/** Each test its own folder: the downloads running on a page are kept by folder (offlineEngine.ts inflight). */
const newBase = () => `https://x${++n}.test/prueba-offline/`

function memoryCaches() {
  const store = new Map<string, { body: ArrayBuffer; headers: Headers }>()
  const cache = {
    match: async (k: string) => (store.has(k) ? new Response(store.get(k)!.body, { headers: store.get(k)!.headers }) : undefined),
    put: async (k: string, r: Response) => void store.set(k, { body: await r.arrayBuffer(), headers: r.headers }),
    delete: async (k: string | { url: string }) => store.delete(typeof k === 'string' ? k : k.url),
    keys: async () => [...store.keys()].map(url => ({ url })),
  }
  vi.stubGlobal('caches', { open: async () => cache, delete: async () => true })
  return store
}

interface Served {
  name: string
  role: 'detector' | 'student' | 'head-meta' | 'head'
  body: Uint8Array
  etag: string
  /** Hugging Face's way: one URL, ranges of the whole file. */
  hf?: boolean
}
interface Asked {
  url: string
  range: string | null
}
/**
 * The app's model route and Hugging Face. `cut(url, from)`: after how many bytes of this answer the connection drops
 * (null: none); `down()`: no connection at all; `hang(url)`: the answer never comes (a reload meanwhile).
 */
function servers(
  base: string,
  served: Served[] | (() => Served[]),
  opts: {
    cut?: (a: Asked, i: number) => number | null
    down?: () => boolean
    hang?: (a: Asked) => boolean
    refuse?: () => boolean
    tamper?: (a: Asked, b: Uint8Array) => Uint8Array
    before?: (a: Asked) => void
  } = {},
) {
  const asked: Asked[] = []
  const now = () => (typeof served === 'function' ? served() : served)
  vi.stubGlobal('fetch', async (input: string, init: RequestInit = {}) => {
    const url = String(input)
    const range = (init.headers as Record<string, string> | undefined)?.range ?? null
    const a = { url, range }
    asked.push(a)
    opts.before?.(a)
    if (opts.refuse?.()) return new Response('', { status: 403 })
    if (opts.down?.()) throw new TypeError('Failed to fetch')
    // No answer until the request is given up (the browser's abort).
    if (opts.hang?.(a)) return new Promise<Response>((_, reject) => init.signal?.addEventListener('abort', () => reject(new DOMException('aborted', 'AbortError'))))
    if (url.includes('/ort/')) return new Response('runtime')
    if (url.includes('list.json')) return Response.json({ files: listing(now()), partBytes: PART })
    const f = now().find(x => url.includes(`/${x.name}`))
    if (!f) return new Response('', { status: 404 })
    let start: number, end: number, total: number, status: number
    const headers: Record<string, string> = {}
    if (f.hf) {
      const m = /^bytes=(\d+)-(\d+)$/.exec(range!)!
      ;[start, end, total, status] = [Number(m[1]), Math.min(Number(m[2]), f.body.length - 1), f.body.length, 206]
      headers.etag = `"hf-${f.etag.slice(1)}`
      headers['content-range'] = `bytes ${start}-${end}/${total}`
    } else {
      const i = Number(new URL(url).searchParams.get('part'))
      const p0 = i * PART, len = Math.min(f.body.length, p0 + PART) - p0
      const m = range && /^bytes=(\d+)-$/.exec(range)
      const from = m ? Number(m[1]) : 0
      ;[start, end, status] = [p0 + from, p0 + len - 1, m ? 206 : 200]
      headers.etag = `W/${f.etag.slice(0, -1)}-p${i}"`
      headers['x-part-bytes'] = String(len)
      if (m) headers['content-range'] = `bytes ${from}-${len - 1}/${len}`
    }
    let body = f.body.slice(start, end + 1)
    if (opts.tamper) body = opts.tamper(a, body) as Uint8Array<ArrayBuffer>
    const cutAt = opts.cut?.(a, asked.length) ?? null
    let sent = 0
    const stream = new ReadableStream<Uint8Array>({
      pull(c) {
        if (cutAt != null && sent >= cutAt) return c.error(new TypeError('network changed'))
        const step = Math.min(MB, body.length - sent, cutAt != null ? cutAt - sent : Infinity)
        if (step <= 0) return c.close()
        c.enqueue(body.slice(sent, sent + step))
        sent += step
      },
    })
    return new Response(stream, { status, headers })
  })
  return asked
}
const listing = (files: Served[]) =>
  files.map(f => ({
    name: f.name,
    role: f.role,
    bytes: f.body.length,
    etag: f.etag,
    sha256: sha(f.body),
    version: `v-${f.etag.slice(1, -1)}`,
    ...(f.hf ? { url: `https://hf.test/${f.name}` } : {}),
    // Part hashes as both lists give them (one per 32 MB part).
    parts: Array.from({ length: Math.ceil(f.body.length / PART) }, (_, i) => sha(f.body.slice(i * PART, (i + 1) * PART))),
  }))

/** The small model's files: the student is the big one (two parts, the second short), the rest tiny. */
function smallModel({ hf = false, student = bytesOf(PART + 10 * MB, 1), etag = '"s1"' } = {}): Served[] {
  return [
    { name: 'wing_seg_s2.onnx', role: 'detector', body: bytesOf(1000, 2), etag: '"d"', hf },
    { name: 'student_SB_int8.onnx', role: 'student', body: student, etag, hf },
    { name: 'head.json', role: 'head-meta', body: bytesOf(300, 3), etag: '"m"', hf },
    { name: 'head.bin', role: 'head', body: bytesOf(5000, 4), etag: '"h"', hf },
  ]
}
const partOf = (store: Map<string, { body: ArrayBuffer }>, base: string, name: string, i: number, slot?: string) => {
  const e = store.get(`${base}models/${name}?part=${i}${slot ? `&v=${slot}` : ''}`)
  return e ? new Uint8Array(e.body) : null
}
const storedJson = (store: Map<string, { body: ArrayBuffer }>, base: string) => JSON.parse(new TextDecoder().decode(store.get(`${base}models/stored.json`)!.body))
/** Every progress call, to check the bar never goes back while the network is down. */
function recorder() {
  const calls: { done: number; total: number; state?: string }[] = []
  return { calls, progress: (done: number, total: number, state?: string) => void calls.push({ done, total, state }) }
}
const monotonic = (calls: { done: number }[]) => calls.every((c, i) => i === 0 || c.done >= calls[i - 1].done)

beforeEach(() => {
  vi.stubGlobal('self', { crossOriginIsolated: false })
  vi.stubGlobal('navigator', { onLine: true })
  // Real time, made short: a minute of no signal is a fraction of a second here (fake timers race the Blob reads).
  Object.assign(timing, { stall: 300, base: 5, max: 40 })
})
afterEach(() => {
  vi.useRealTimers()
  vi.unstubAllGlobals()
})

describe('a model download resumes after a dropped connection', () => {
  it('the server\'s parts: a cut mid-part goes on with a Range for the rest; the bar never goes back', async () => {
    const base = newBase()
    const store = memoryCaches()
    const files = smallModel()
    // The student's first part drops at 9 MB; its retry asks for bytes=9437184- of that part (206).
    const asked = servers(base, files, { cut: (a, k) => (a.url.includes('student') && !a.range && a.url.includes('part=0') && k < 20 ? 9 * MB : null) })
    const engine = createEngine(base, () => [], { signedIn: () => false })
    const rec = recorder()
    await engine.download('small', listing(files), rec.progress)
    const student = asked.filter(a => a.url.includes('student'))
    expect(student.map(a => [new URL(a.url).searchParams.get('part'), a.range])).toEqual([
      ['0', null],
      ['0', `bytes=${9 * MB}-`],
      ['1', null],
    ])
    expect(rec.calls.some(c => c.state === 'waiting')).toBe(true)
    expect(monotonic(rec.calls)).toBe(true)
    expect(rec.calls.at(-1)).toMatchObject({ done: rec.calls.at(-1)!.total })
    const body = files[1].body
    expect(sha(join(partOf(store, base, files[1].name, 0)!, partOf(store, base, files[1].name, 1)!))).toBe(sha(body))
    // The pieces kept on the way went once the part was whole.
    expect([...store.keys()].filter(k => k.includes('&piece=')).length).toBe(0)
    expect(storedJson(store, base).files[files[1].name]).toMatchObject({ sha256: sha(body) })
  })

  it('Hugging Face: ranges of the file; after a cut, the range from the last byte that came', async () => {
    const base = newBase()
    memoryCaches()
    const files = smallModel({ hf: true })
    const first = `bytes=0-${PART - 1}`
    const asked = servers(base, files, { cut: (a, k) => (a.url.includes('student') && a.range === first && k < 20 ? 5 * MB + 123 : null) })
    const engine = createEngine(base, () => [], { signedIn: () => false })
    await engine.download('small', listing(files), () => {})
    expect(asked.filter(a => a.url.includes('student')).map(a => a.range)).toEqual([first, `bytes=${5 * MB + 123}-${PART - 1}`, `bytes=${PART}-${PART + 10 * MB - 1}`])
    expect((await engine.ready(listing(files))).small).toBe(true)
  })

  it('no signal for a while: «waiting», then it goes on by itself when the network is back (no reload)', async () => {
    const base = newBase()
    const store = memoryCaches()
    const files = smallModel()
    let down = false
    let cutOnce = true
    servers(base, files, {
      down: () => down,
      cut: a => {
        if (a.url.includes('student') && cutOnce) {
          cutOnce = false
          down = true
          return 13 * MB
        }
        return null
      },
    })
    const engine = createEngine(base, () => [], { signedIn: () => false })
    const rec = recorder()
    const run = engine.download('small', listing(files), rec.progress)
    await vi.waitFor(() => expect(rec.calls.at(-1)?.state).toBe('waiting'))
    await new Promise(r => setTimeout(r, 300)) // many tries meanwhile, all failing
    const last = rec.calls.at(-1)!
    expect(last.state).toBe('waiting')
    expect(last.done).toBeGreaterThanOrEqual(13 * MB) // the bar stays where it was
    // Three pieces (12 MB) are on the phone already.
    expect([...store.keys()].filter(k => k.includes('&piece=')).length).toBe(3)
    down = false
    await run
    expect(rec.calls.at(-1)!.state).toBeUndefined()
    expect(monotonic(rec.calls)).toBe(true)
    expect(storedJson(store, base).files[files[1].name].sha256).toBe(sha(files[1].body))
  })

  it('a stalled connection (no byte, no error, as after a network switch) is cut and tried again', async () => {
    const base = newBase()
    memoryCaches()
    const files = smallModel()
    let hung = false
    const asked = servers(base, files, { hang: a => a.url.includes('student') && !hung && (hung = true) })
    const engine = createEngine(base, () => [], { signedIn: () => false })
    await engine.download('small', listing(files), () => {})
    expect(asked.filter(a => a.url.includes('student') && a.url.includes('part=0')).length).toBe(2)
  })
})

/**
 * A download cut 10 MB into the student's first part, then the page closed (the old page's download ends: its next
 * try is refused). Two pieces (8 MB) stay on the phone.
 */
async function closedAt10MB(base: string, files: Served[]) {
  let down = false, closed = false
  servers(base, files, { cut: a => (a.url.includes('student') && !down ? ((down = true), 10 * MB) : null), down: () => down, refuse: () => closed })
  const engine = createEngine(base, () => [], { signedIn: () => false })
  const rec = recorder()
  const run = engine.download('small', listing(files), rec.progress).catch(e => e)
  await vi.waitFor(() => expect(rec.calls.at(-1)?.state).toBe('waiting'))
  closed = true
  expect(String(await run)).toMatch(/403/)
  return true
}

describe('review r1: what else can hold a download', () => {
  it('sol-F1: a runtime file that hangs while the download is prepared: «waiting», then it goes on (no endless «Preparando»)', async () => {
    const base = newBase()
    const store = memoryCaches()
    const files = smallModel()
    let hung = 0
    servers(base, files, { hang: a => a.url.includes('/ort/') && hung++ < 2 })
    const engine = createEngine(base, () => [], { signedIn: () => false })
    const rec = recorder()
    await engine.download('small', listing(files), rec.progress)
    expect(rec.calls.some(c => c.state === 'waiting')).toBe(true)
    expect(storedJson(store, base).files[files[1].name].sha256).toBe(sha(files[1].body))
    // The runtime kept for airplane mode is the bytes that came.
    expect(new TextDecoder().decode(store.get(`${base}ort/ort.wasm.min.mjs`)!.body)).toBe('runtime')
  })

  it('fable-F1: «waiting» stays on through tries that get nothing, and goes once bytes flow again', async () => {
    const base = newBase()
    memoryCaches()
    const files = smallModel()
    let hung = 0
    servers(base, files, { hang: a => a.url.includes('student') && hung++ < 3 })
    const engine = createEngine(base, () => [], { signedIn: () => false })
    const rec = recorder()
    await engine.download('small', listing(files), rec.progress)
    // Three tries hang (each given up after the stall): one «waiting» stretch, no flicker between the tries, and it
    // ends with the first bytes (the next call counts them).
    const ends = rec.calls.flatMap((c, i) => (i && rec.calls[i - 1].state === 'waiting' && c.state !== 'waiting' ? [i] : []))
    expect(ends).toHaveLength(1)
    expect(rec.calls[ends[0] + 1].done).toBeGreaterThan(rec.calls[ends[0]].done)
  })

  it('fable-F3: a storage error that is not the network ends the download (no endless «waiting»)', async () => {
    const base = newBase()
    const store = memoryCaches()
    const files = smallModel()
    servers(base, files)
    const cache = await caches.open('x')
    const put = cache.put
    cache.put = async (k: string, r: Response) => (String(k).includes('&piece=') ? Promise.reject(new Error('disk broke')) : put(k, r))
    const engine = createEngine(base, () => [], { signedIn: () => false })
    await expect(engine.download('small', listing(files), () => {})).rejects.toThrow('disk broke')
    expect(store.has(`${base}models/stored.json`)).toBe(false)
  })
})

describe('after a reload: «Continuar» from the pieces kept', () => {
  it('a reload mid-part: saved() counts the pieces, and the next download asks only for the rest', async () => {
    const base = newBase()
    const store = memoryCaches()
    const files = smallModel()
    // The page is closed while the student's first part comes: 10 MB in, two pieces (8 MB) kept, then nothing more.
    const closed = await closedAt10MB(base, files)
    expect([...store.keys()].filter(k => k.includes('&piece=')).sort()).toEqual([
      `${base}${pieceKey(files[1].name, 0, undefined, 0)}`,
      `${base}${pieceKey(files[1].name, 0, undefined, 1)}`,
    ])
    // The page again: a new engine (a fresh module, nothing in memory).
    vi.resetModules()
    const fresh = await import('../offlineEngine')
    const asked = servers(base, files)
    const again = fresh.createEngine(base, () => [], { signedIn: () => false })
    const kept = await again.saved('small', listing(files))
    // The detector came whole before the student (it is listed first).
    // stored.json names nothing yet (it is written once all is in): every file counts in the total.
    expect(kept).toEqual({ done: 1000 + 2 * PIECE, total: files.reduce((a, f) => a + f.body.length, 0) })
    // Only the model begun says so: the other model shares the detector, which came whole, but was never started.
    expect(await again.saved('full', listing(files))).toEqual({ done: 0, total: 0 })
    const rec2 = recorder()
    await again.download('small', listing(files), rec2.progress)
    expect(asked.filter(a => a.url.includes('student')).map(a => a.range)).toEqual([`bytes=${2 * PIECE}-`, null])
    expect(rec2.calls.find(c => c.done > 1000)!.done).toBeGreaterThanOrEqual(2 * PIECE) // the bar starts where it was
    expect(storedJson(store, base).files[files[1].name].sha256).toBe(sha(files[1].body))
    expect(await again.saved('small', listing(files))).toEqual({ done: 0, total: 0 })
  })

  it('a kept piece that is corrupt: the part\'s hash fails, the part comes again whole, the file is right', async () => {
    const base = newBase()
    const store = memoryCaches()
    const files = smallModel()
    const closed = await closedAt10MB(base, files)
    // A kept piece goes bad on the phone (storage, or a proxy that mangled it).
    const k = `${base}${pieceKey(files[1].name, 0, undefined, 1)}`
    const bad = new Uint8Array(store.get(k)!.body)
    bad[1234] ^= 0xff
    store.set(k, { ...store.get(k)!, body: bad.buffer })
    vi.resetModules()
    const fresh = await import('../offlineEngine')
    const asked = servers(base, files)
    const again = fresh.createEngine(base, () => [], { signedIn: () => false })
    await again.download('small', listing(files), () => {})
    expect(asked.filter(a => a.url.includes('student')).map(a => [new URL(a.url).searchParams.get('part'), a.range])).toEqual([
      ['0', `bytes=${2 * PIECE}-`],
      ['0', null],
      ['1', null],
    ])
    expect(storedJson(store, base).files[files[1].name].sha256).toBe(sha(files[1].body))
  })

  it('bytes that come wrong (a part\'s hash): that part is downloaded again; three times wrong, the download stops', async () => {
    const base = newBase()
    const store = memoryCaches()
    const files = smallModel({ hf: true })
    let bad = 1
    servers(base, files, { tamper: (a, b) => (a.url.includes('student') && a.range?.startsWith('bytes=0-') && bad-- > 0 ? b.map((x, i) => (i === 77 ? x ^ 1 : x)) : b) })
    const engine = createEngine(base, () => [], { signedIn: () => false })
    await engine.download('small', listing(files), () => {})
    expect(storedJson(store, base).files[files[1].name].sha256).toBe(sha(files[1].body))
    // Always wrong: the phone keeps what it had, and says so.
    const base2 = newBase()
    memoryCaches()
    servers(base2, files, { tamper: (a, b) => (a.url.includes('student') ? b.map((x, i) => (i === 77 ? x ^ 1 : x)) : b) })
    await expect(createEngine(base2, () => [], { signedIn: () => false }).download('small', listing(files), () => {})).rejects.toThrow('corrupt')
  })
})

describe('a file that changed on the server meanwhile starts over cleanly', () => {
  it('the server\'s part ETag changes mid-download: what came of the old file goes, the new list is read, the new file is taken', async () => {
    const base = newBase()
    const store = memoryCaches()
    const old = smallModel()
    const now = smallModel({ student: bytesOf(PART + 3 * MB, 9), etag: '"s2"' })
    let served = old
    // The student is replaced on the server once its first part is in.
    const asked = servers(base, () => served, { before: a => void (a.url.includes('student') && a.url.includes('part=1') && (served = now)) })
    const engine = createEngine(base, () => [], { signedIn: () => true })
    await engine.download('small', listing(old), () => {})
    // The list was read again and the new student taken from its start; nothing of the old one stays.
    expect(asked.some(a => a.url.includes('list.json'))).toBe(true)
    expect(asked.filter(a => a.url.includes('student')).map(a => new URL(a.url).searchParams.get('part'))).toEqual(['0', '1', '0', '1'])
    const entry = storedJson(store, base).files['student_SB_int8.onnx']
    expect(entry).toMatchObject({ etag: '"s2"', sha256: sha(now[1].body) })
    expect(sha(join(partOf(store, base, 'student_SB_int8.onnx', 0)!, partOf(store, base, 'student_SB_int8.onnx', 1)!))).toBe(sha(now[1].body))
    expect([...store.keys()].filter(k => k.includes('&piece=')).length).toBe(0)
  })

  it('Hugging Face\'s ETag changes between two ranges of a part: that part starts again from its first byte', async () => {
    const base = newBase()
    const store = memoryCaches()
    const files = smallModel({ hf: true })
    let first = true
    // The first range of the student drops at 6 MB; the next answer comes with another ETag (another copy of it).
    const asked = servers(base, () => files, {
      cut: a => (a.url.includes('student') && first ? ((first = false), 6 * MB) : null),
      before: a => void (a.url.includes('student') && !first && (files[1] = { ...files[1], etag: '"s1b"' })),
    })
    const engine = createEngine(base, () => [], { signedIn: () => false })
    const rec = recorder()
    await engine.download('small', listing(smallModel({ hf: true })), rec.progress)
    expect(asked.filter(a => a.url.includes('student')).map(a => a.range)).toEqual([`bytes=0-${PART - 1}`, `bytes=${6 * MB}-${PART - 1}`, `bytes=0-${PART - 1}`, `bytes=${PART}-${PART + 10 * MB - 1}`])
    expect(storedJson(store, base).files['student_SB_int8.onnx'].sha256).toBe(sha(files[1].body))
  })
})

describe('the installed model keeps working until the new one is checked', () => {
  it('an update cut and waiting: the old version is what the phone uses; «Borrar del teléfono» stops it', async () => {
    const base = newBase()
    const store = memoryCaches()
    const v1 = smallModel({ student: bytesOf(6 * MB, 5) })
    servers(base, v1)
    const engine = createEngine(base, () => [], { signedIn: () => false })
    await engine.download('small', listing(v1), () => {})
    const v2 = smallModel({ student: bytesOf(PART + MB, 6), etag: '"s2"' })
    let down = false
    servers(base, v2, { down: () => down, cut: a => (a.url.includes('student') ? ((down = true), 5 * MB) : null) })
    const rec = recorder()
    const run = engine.download('small', listing(v2), rec.progress).catch(e => e)
    await vi.waitFor(() => expect(rec.calls.at(-1)?.state).toBe('waiting'))
    expect(storedJson(store, base).files['student_SB_int8.onnx'].etag).toBe('"s1"')
    expect((await engine.ready(null)).small).toBe(true)
    expect((await engine.saved('small', listing(v2))).done).toBe(PIECE)
    await engine.removeModel('small')
    expect(String(await run)).toMatch(/cancelled/)
    expect([...store.keys()].some(k => k.includes('student'))).toBe(false)
  })
})
