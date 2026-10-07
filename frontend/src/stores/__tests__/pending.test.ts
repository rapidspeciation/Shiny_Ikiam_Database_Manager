import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { markRaw } from 'vue'
import { createPinia, setActivePinia } from 'pinia'
import type { Field, TableRow } from '../../lib/types'
import { setStagedSaving } from '../../lib/stagedSwitch'
import { verificationsFor } from '../../lib/verifications'
import { isStaged, usePending } from '../pending'
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
