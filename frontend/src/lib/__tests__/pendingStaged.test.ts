import { beforeEach, describe, expect, it, vi } from 'vitest'
import { markRaw } from 'vue'
import { createPinia, setActivePinia } from 'pinia'
import type { Field, TableRow } from '../types'
import { usePending } from '../../stores/pending'
import { useTables } from '../../stores/tables'

// Emergidos and Clutches keep their changes in the app for everyone (POST /api/staged); a save
// Google cannot take now is kept by the server (status queued) and settled when it answers.
const columns: Field[] = [
  { key: 'Insectary_ID', label: 'Insectary ID', type: 'text' },
  { key: 'Sex', label: 'Sex', type: 'text' },
]
const row = (id: string, n: number, values: TableRow['values']): TableRow => ({ id, row: n, version: 1, observed: true, values, formulas: [] })
const rows = [row('r1', 10, { Insectary_ID: 'N2D', Sex: 'female' }), row('r2', 11, { Insectary_ID: 'N3D', Sex: 'male' })]

describe('where pending changes go', () => {
  let calls: { url: string; body: Record<string, unknown> | null }[] = []
  let answers: Record<string, () => unknown> = {}
  beforeEach(() => {
    setActivePinia(createPinia())
    localStorage.clear()
    calls = []
    answers = {}
    vi.stubGlobal(
      'fetch',
      vi.fn(async (url: string, init?: RequestInit) => {
        if (url.startsWith('api/verifications')) return new Response(JSON.stringify({ unique: [], lists: {} }))
        calls.push({ url, body: init?.body ? JSON.parse(String(init.body)) : null })
        const answer = Object.entries(answers).find(([prefix]) => url.startsWith(prefix))?.[1]
        return new Response(JSON.stringify(answer ? answer() : { records: [], skipped: [], created: [] }))
      }),
    )
    useTables().tables.Insectary_data = markRaw({ module: 'Insectary_data', revision: '1', columns, rows, headerProblems: [] })
  })

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
    location.hash = ''
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
    expect(calls.filter(c => c.body)).toHaveLength(1)
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
