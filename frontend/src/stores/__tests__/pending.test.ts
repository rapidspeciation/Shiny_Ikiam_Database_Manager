import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { effectScope, markRaw } from 'vue'
import { createPinia, disposePinia, getActivePinia, setActivePinia } from 'pinia'
import type { Field, TableRow } from '../../lib/types'
import { setStagedSaving } from '../../lib/stagedSwitch'
import { verificationsFor } from '../../lib/verifications'
import { isStaged, usePending } from '../pending'
import { useSession } from '../session'
import { useTables } from '../tables'

// Pending changes on their way to the sheet: checked first, written (POST /api/records/batch), kept in
// the app for everyone (Emergidos and Clutches, POST /api/staged), or kept by the server while Google
// cannot take them (status queued) and settled when it answers.

const columns: Field[] = [
  { key: 'Insectary_ID', label: 'Insectary ID', type: 'text' },
  { key: 'Sex', label: 'Sex', type: 'text' },
  { key: 'CAM_ID', label: 'CAM ID', type: 'text' },
  { key: 'Tube_1_id', label: 'Tube 1 id', type: 'text' },
  { key: 'Death_date', label: 'Death date', type: 'date' },
  { key: 'Death_cause', label: 'Death cause', type: 'text' },
  { key: 'Preserved_Dead_Alive', label: 'Preserved Dead Alive', type: 'text' },
]
const row = (id: string, n: number, values: TableRow['values']): TableRow => ({ id, row: n, version: 1, observed: true, values, formulas: [] })
const rows = [
  row('r1', 13384, { Insectary_ID: 'N2D', Sex: 'female', CAM_ID: 'CAM078274', Tube_1_id: 'FS90415320' }),
  row('r2', 13385, { Insectary_ID: 'N3D', Sex: 'male' }),
  row('r3', 13386, { Insectary_ID: 'N4D' }),
]

/** What the app sent (url and body; GETs have none), and the server's answer per URL prefix. */
let calls: { url: string; body: Record<string, unknown> | null }[] = []
let answers: Record<string, () => unknown> = {}
const sent = () => calls.filter(c => c.body).map(c => c.body as { edits: unknown[]; partial?: boolean; purpose?: string })

beforeEach(async () => {
  setActivePinia(createPinia())
  // The store's check and automatic-save timers are the module's, one of each: the last test's would still fire on
  // its own store, and a store's action makes its pinia the active one again, so a usePending() after it (late on a
  // busy machine) got the last test's changes. Scheduling them on the new store replaces them first.
  const fresh = usePending()
  fresh.scheduleCheck()
  fresh.scheduleAutoSave()
  localStorage.clear()
  calls = []
  answers = {}
  vi.stubGlobal(
    'fetch',
    vi.fn(async (url: string, init?: RequestInit) => {
      if (url.startsWith('api/verifications'))
        return new Response(
          JSON.stringify({
            unique: ['Insectary_ID', 'CAM_ID', 'Tube_1_id'],
            lists: { Preserved_Dead_Alive: { strict: true, source: 'x', values: ['Dead', 'Alive', 'NA'] } },
          }),
        )
      calls.push({ url, body: init?.body ? JSON.parse(String(init.body)) : null })
      const answer = Object.entries(answers).find(([prefix]) => url.startsWith(prefix))?.[1]
      return new Response(JSON.stringify(answer ? answer() : { records: [], skipped: [], created: [] }))
    }),
  )
  useTables().tables.Insectary_data = markRaw({ module: 'Insectary_data', revision: '1', columns, rows, headerProblems: [] })
  verificationsFor('Insectary_data')
  await new Promise(resolve => setTimeout(resolve, 0))
})
afterEach(() => {
  location.hash = ''
})

describe('saving pending changes', () => {
  it('saves every other change and keeps a repeated CAM and a broken date pending, with the reason', async () => {
    const pending = usePending()
    pending.setAutoSave(false)
    pending.setCell('Insectary_data', rows[1], 'N3D', 'CAM_ID', 'CAM078274')
    pending.setCell('Insectary_data', rows[1], 'N3D', 'Death_date', 46292)
    pending.setCell('Insectary_data', rows[2], 'N4D', 'Death_date', 32_902_000)
    pending.setCell('Insectary_data', rows[2], 'N4D', 'Death_cause', 'Unknown')
    const result = await pending.save('')
    expect(sent()).toHaveLength(1)
    expect(sent()[0].partial).toBe(true)
    expect(sent()[0].edits).toEqual([
      { id: 'r2', values: { Death_date: 46292 }, expected: { Death_date: null } },
      { id: 'r3', values: { Death_cause: 'Unknown' }, expected: { Death_cause: null } },
    ])
    expect(result).toEqual({ saved: 2, left: 2 })
    expect(Object.keys(pending.edits.r2.values)).toEqual(['CAM_ID'])
    expect(Object.keys(pending.edits.r3.values)).toEqual(['Death_date'])
    expect(pending.issues['r2:CAM_ID']).toBe('CAM078274 ya está usado en Insectary_data fila 13384 (N2D)')
    expect(pending.issues['r3:Death_date']).toMatch(/Fecha no válida/)
  })

  it('keeps what the server left out red, and automatic saving does not send it again until it is edited', async () => {
    const pending = usePending()
    pending.setAutoSave(false)
    answers['api/records/batch'] = () => ({
      records: [],
      created: [],
      skipped: [{ id: 'r2', field: 'CAM_ID', code: 'DUPLICATE_ID', message: 'CAM079001 ya está usado en Collection_data fila 9000' }],
    })
    pending.setCell('Insectary_data', rows[1], 'N3D', 'CAM_ID', 'CAM079001')
    pending.setCell('Insectary_data', rows[2], 'N4D', 'Death_cause', 'Unknown')
    expect(await pending.save('')).toEqual({ saved: 1, left: 1 })
    expect(pending.errors['r2:CAM_ID']).toMatch(/Collection_data fila 9000/)
    expect(pending.edits.r3).toBeUndefined()
    // Another change: automatic saving sends it, not the refused CAM.
    pending.setCell('Insectary_data', rows[2], 'N4D', 'Sex', 'male')
    delete answers['api/records/batch']
    await pending.save('', { retryRefused: false })
    expect(sent()[1].edits).toEqual([{ id: 'r3', values: { Sex: 'male' }, expected: { Sex: null } }])
    expect(pending.errors['r2:CAM_ID']).toBeDefined()
    // Nothing else to send: no request at all.
    expect(await pending.save('', { retryRefused: false })).toEqual({ saved: 0, left: 1 })
    expect(sent()).toHaveLength(2)
  })

  it('sends the tab most changes were typed in, as the purpose of the save (Historial)', async () => {
    const pending = usePending()
    pending.setAutoSave(false)
    location.hash = '#/tubos'
    pending.setCell('Insectary_data', rows[2], 'N4D', 'Sex', 'male')
    location.hash = '#/muertes'
    pending.setCell('Insectary_data', rows[1], 'N3D', 'Death_cause', 'Unknown')
    pending.setCell('Insectary_data', rows[2], 'N4D', 'Death_cause', 'Unknown')
    location.hash = '#/historial'
    await pending.save('')
    expect(sent()[0].purpose).toBe('muertes')
  })

  it("leaves a walk's new rows for Guardar: automatic saving sends only the other changes", async () => {
    const pending = usePending()
    pending.setAutoSave(false)
    pending.addCreate('Insectary_data', 'captura', { Insectary_ID: 'Z9Z' }, { manual: true })
    pending.setCell('Insectary_data', rows[2], 'N4D', 'Death_cause', 'Unknown')
    await pending.save('', { retryRefused: false, auto: true })
    expect(sent()[0].edits).toEqual([{ id: 'r3', values: { Death_cause: 'Unknown' }, expected: { Death_cause: null } }])
    expect(JSON.stringify(sent()[0])).not.toContain('Z9Z')
    expect(pending.creates).toHaveLength(1)
    // Guardar sends it.
    await pending.save('')
    expect(JSON.stringify(sent()[1])).toContain('Z9Z')
  })
})

describe('where pending changes go', () => {
  it('a change typed in Emergidos is kept in the app; one typed in another tab is written', async () => {
    const pending = usePending()
    pending.setAutoSave(false)
    location.hash = '#/emergidos'
    pending.setCell('Insectary_data', rows[0], 'N2D', 'Sex', 'male')
    location.hash = '#/tablas'
    pending.setCell('Insectary_data', rows[1], 'N3D', 'Sex', 'female')
    answers['api/staged'] = () => ({ status: 'staged', entryId: 'e1', records: [], created: [], skipped: [] })
    const out = await pending.save('')
    const posts = calls.filter(c => c.body)
    expect(posts.map(c => c.url)).toEqual(['api/staged', 'api/records/batch'])
    expect(posts[0].body).toMatchObject({ purpose: 'emergidos', edits: [{ id: 'r1', values: { Sex: 'male' } }] })
    expect(posts[1].body).toMatchObject({ purpose: 'tablas', edits: [{ id: 'r2', values: { Sex: 'female' } }] })
    expect(out).toMatchObject({ saved: 1, staged: 1, stagedEntry: 'e1', left: 0 })
    expect(pending.lastSaved?.where).toBe('sheet')
  })

  it('the staged-saving switch off: Emergidos and Clutches go straight to the sheet, except rows entered in the app', () => {
    try {
      expect(isStaged('emergidos')).toBe(true)
      expect(isStaged('clutches')).toBe(true)
      expect(isStaged('muertes')).toBe(false)
      setStagedSaving(false)
      expect(isStaged('emergidos')).toBe(false)
      expect(isStaged('clutches')).toBe(false)
      // A row entered in the app before the switch was turned off still goes through the app.
      expect(isStaged('emergidos', 'staged:abc')).toBe(true)
    } finally {
      setStagedSaving(true)
    }
  })

  it('a save Google cannot take now stays pending, marked as waiting, is not sent twice, and leaves once written', async () => {
    const pending = usePending()
    pending.setAutoSave(false)
    pending.setCell('Insectary_data', rows[1], 'N3D', 'Sex', 'female')
    answers['api/records/batch'] = () => ({ status: 'queued', outboxId: 'o1', records: [], created: [], skipped: [] })
    expect(await pending.save('')).toMatchObject({ saved: 0, queued: 1, left: 1 })
    expect(pending.isQueued('r2', 'Sex')).toBe(true)
    expect(pending.queuedCount).toBe(1)
    // Not sent again while it waits.
    expect(await pending.save('')).toEqual({ saved: 0, left: 1 })
    expect(sent()).toHaveLength(1)
    // Still waiting: nothing changes.
    answers['api/outbox/o1'] = () => ({ outbox: { status: 'queued' } })
    await pending.resolveQueued()
    expect(pending.queuedCount).toBe(1)
    // Written.
    answers['api/outbox/o1'] = () => ({ outbox: { status: 'done' }, status: 'verified', records: [], created: [], skipped: [], action: { id: 'a1' } })
    await pending.resolveQueued()
    expect(pending.queuedCount).toBe(0)
    expect(pending.edits.r2).toBeUndefined()
  })

  it('a waiting save the sheet refused keeps its cells, red with why', async () => {
    const pending = usePending()
    pending.setAutoSave(false)
    pending.setCell('Insectary_data', rows[1], 'N3D', 'Sex', 'female')
    answers['api/records/batch'] = () => ({ status: 'queued', outboxId: 'o2' })
    await pending.save('')
    answers['api/outbox/o2'] = () => ({
      outbox: { status: 'conflict' },
      error: { code: 'BATCH_CONFLICT', message: 'x', details: { items: [{ id: 'r2', field: 'Sex', code: 'EXTERNAL_CONFLICT', message: 'Otra persona cambió Sex en la hoja' }] } },
    })
    await pending.resolveQueued()
    expect(pending.queuedCount).toBe(0)
    expect(pending.edits.r2.values.Sex).toBe('female')
    expect(pending.issues['r2:Sex']).toBe('Otra persona cambió Sex en la hoja')
  })
})

describe('the check and automatic-save timers after a sign-out or a disposed store', () => {
  afterEach(() => {
    vi.useRealTimers()
  })
  /** A change typed with automatic saving on: it schedules the check (200 ms) and the save (2.5 s). */
  const typed = (pending: ReturnType<typeof usePending>) => {
    pending.setCell('Insectary_data', rows[1], 'N3D', 'Sex', 'female')
    expect(vi.getTimerCount()).toBe(2)
  }
  const actions = (pending: ReturnType<typeof usePending>) => {
    const names: string[] = []
    pending.$onAction(({ name }) => void names.push(name))
    return names
  }

  it('signing out stops both, the change is not saved, and it stays on this device for that person', async () => {
    vi.useFakeTimers()
    useSession().user = { username: 'ana' } as never
    const pending = usePending()
    pending.restore()
    typed(pending)
    const ran = actions(pending)
    // AppHeader's logout: stopTimers() before auth/logout, clear() after it.
    pending.stopTimers()
    expect(vi.getTimerCount()).toBe(0)
    useSession().user = null
    pending.clear()
    await vi.advanceTimersByTimeAsync(120_000)
    expect(ran).not.toContain('check')
    expect(ran).not.toContain('runAutoSave')
    expect(sent()).toEqual([])
    expect(JSON.parse(localStorage.getItem('ithomiini:pending:ana')!).edits.r2.values).toEqual({ Sex: 'female' })
    // Signing in again brings the change back (and the check), and the next edit saves it.
    useSession().user = { username: 'ana' } as never
    pending.restore()
    expect(pending.changeCount).toBe(1)
  })

  it('clear() (sign-out) stops both on its own', async () => {
    vi.useFakeTimers()
    const pending = usePending()
    typed(pending)
    pending.clear()
    expect(vi.getTimerCount()).toBe(0)
    await vi.advanceTimersByTimeAsync(10_000)
    expect(sent()).toEqual([])
  })

  it('an automatic save on its way when the person signs out schedules no retry', async () => {
    vi.useFakeTimers()
    let answer: (r: Response) => void = () => {}
    vi.stubGlobal(
      'fetch',
      vi.fn((url: string) => {
        calls.push({ url, body: {} })
        return new Promise<Response>(resolve => (answer = resolve))
      }),
    )
    const pending = usePending()
    typed(pending)
    await vi.advanceTimersByTimeAsync(2_500)
    expect(calls.map(c => c.url)).toEqual(['api/records/batch'])
    pending.stopTimers()
    answer(new Response(JSON.stringify({ error: { code: 'SERVER_ERROR', message: 'x' } }), { status: 500 }))
    await vi.advanceTimersByTimeAsync(0)
    expect(vi.getTimerCount()).toBe(0)
    expect(pending.autoBlocked).toBe('')
    await vi.advanceTimersByTimeAsync(120_000)
    expect(calls).toHaveLength(1)
  })

  it('the same failed save still retries when nobody signed out', async () => {
    vi.useFakeTimers()
    let answer: (r: Response) => void = () => {}
    vi.stubGlobal(
      'fetch',
      vi.fn((url: string) => {
        calls.push({ url, body: {} })
        return new Promise<Response>(resolve => (answer = resolve))
      }),
    )
    const pending = usePending()
    typed(pending)
    await vi.advanceTimersByTimeAsync(2_500)
    answer(new Response(JSON.stringify({ error: { code: 'SERVER_ERROR', message: 'x' } }), { status: 500 }))
    await vi.advanceTimersByTimeAsync(0)
    expect(vi.getTimerCount()).toBe(1)
    expect(pending.autoBlocked).toBe('No se pudo guardar; se reintentará')
  })

  for (const how of ['store.$dispose()', 'disposePinia()'] as const)
    it(`after ${how}, the old store's timers do not touch the new store`, async () => {
      vi.useFakeTimers()
      const old = usePending()
      const oldPinia = getActivePinia()!
      typed(old)
      const ran = actions(old)
      if (how === 'store.$dispose()') old.$dispose()
      else disposePinia(oldPinia)
      expect(vi.getTimerCount()).toBe(0)
      const pinia = createPinia()
      setActivePinia(pinia)
      useTables().tables.Insectary_data = markRaw({ module: 'Insectary_data', revision: '1', columns, rows, headerProblems: [] })
      const fresh = usePending()
      await vi.advanceTimersByTimeAsync(120_000)
      expect(ran).toEqual([])
      expect(getActivePinia()).toBe(pinia)
      expect(fresh.problems).toEqual({})
      expect(sent()).toEqual([])
    })

  it('$reset() run in another scope does not tie the timers to that scope', () => {
    vi.useFakeTimers()
    const pending = usePending()
    const scope = effectScope()
    scope.run(() => pending.$reset())
    typed(pending)
    scope.stop()
    expect(vi.getTimerCount()).toBe(2)
    pending.clear()
  })
})

describe('a save on its way, or of unclear outcome', () => {
  /** The mocked fetch, with the next records/batch answer held until `release()`, or answered with `status`. */
  function hold({ status = 200 } = {}) {
    const mocked = globalThis.fetch
    let release: () => void = () => {}
    const gate = new Promise<void>(resolve => (release = resolve))
    let reached = false
    vi.stubGlobal('fetch', async (url: string, init?: RequestInit) => {
      if (!url.startsWith('api/records/batch')) return mocked(url, init)
      reached = true
      await gate
      const out = await mocked(url, init)
      vi.stubGlobal('fetch', mocked)
      return status === 200 ? out : new Response(JSON.stringify({ error: { code: 'WRITE_UNCERTAIN', message: 'Google did not answer' } }), { status })
    })
    return { release, reached: () => reached }
  }
  const tick = async (until: () => boolean) => {
    for (let i = 0; i < 50 && !until(); i++) await new Promise(resolve => setTimeout(resolve, 0))
  }

  it('keeps a cell typed again while its save was on its way; the next save expects the value saved', async () => {
    const pending = usePending()
    pending.setAutoSave(false)
    pending.setCell('Insectary_data', rows[2], 'N4D', 'Death_cause', 'Unknown')
    const gate = hold()
    const first = pending.save('')
    await tick(gate.reached)
    pending.setCell('Insectary_data', rows[2], 'N4D', 'Death_cause', 'Fungus')
    gate.release()
    await first
    expect(pending.edits.r3?.values).toEqual({ Death_cause: 'Fungus' })
    await pending.save('')
    expect(sent()[1].edits).toEqual([{ id: 'r3', values: { Death_cause: 'Fungus' }, expected: { Death_cause: 'Unknown' } }])
  })

  it('a save of unclear outcome is sent again with the same request ID after a reload (never written twice)', async () => {
    const pending = usePending()
    pending.setAutoSave(false)
    pending.setCell('Insectary_data', rows[2], 'N4D', 'Death_cause', 'Unknown')
    const gate = hold({ status: 503 })
    gate.release()
    await expect(pending.save('')).rejects.toMatchObject({ code: 'WRITE_UNCERTAIN' })
    // The page reloaded: a new store reads what this device kept.
    setActivePinia(createPinia())
    const again = usePending()
    again.setAutoSave(false)
    again.restore()
    await again.save('')
    const [a, b] = calls.filter(c => c.url.startsWith('api/records/batch')).map(c => c.body as { requestId: string; edits: unknown })
    expect(b.requestId).toBe(a.requestId)
    expect(b.edits).toEqual(a.edits)
  })
})
