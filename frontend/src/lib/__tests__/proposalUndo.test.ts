import { describe, expect, it } from 'vitest'
import { cellId, cellOf, rowKey, withLocal, type LocalCell, type Proposal, type ProposalChange } from '../proposals'
import { StepBuilder, UndoHistory, historyFor, restoreOps, snapCell } from '../proposalUndo'
import type { CellValue } from '../types'

const created = (key: string, values: ProposalChange['values'], extra: Partial<ProposalChange> = {}): ProposalChange => ({
  index: 0,
  key,
  clientId: key,
  recordId: null,
  sheet: 'Insectary_data',
  row: null,
  label: key,
  create: true,
  values,
  ...extra,
})
const proposal = (changes: ProposalChange[]): Proposal => ({
  id: 'p1',
  reason: 'Cuaderno',
  status: 'pending',
  fields: [],
  types: {},
  applied: null,
  changes: changes.map((c, index) => ({ ...c, index })),
})

/**
 * The table as ProposalGrid keeps it: the server's copy with the person's edits
 * and checks laid over it (withLocal), and the edits going in as the table sends them.
 */
function table(server: Proposal) {
  const local = new Map<string, LocalCell>()
  const checks = new Map<string, boolean>()
  const t = {
    server,
    shown: () => withLocal(t.server, local, checks),
    find: (key: string) => t.shown().changes.find(c => rowKey(c) === key),
    edit(cells: { key: string; field: string; value: CellValue; use?: 'sheet' | 'ai' }[]) {
      for (const c of cells) local.set(cellId(c.key, c.field), c.use ? { value: c.value, use: c.use } : { value: c.value })
    },
    check(cells: { key: string; field: string; checked: boolean }[]) {
      for (const c of cells) checks.set(cellId(c.key, c.field), c.checked)
    },
    /** One step as the table records it: the cells as they were, then the edits. */
    step(history: UndoHistory, cells: { key: string; field: string; value: CellValue; use?: 'sheet' | 'ai' }[]) {
      const b = new StepBuilder()
      for (const c of cells) b.add(t.find(c.key)!, c.field)
      history.record(b.take())
      t.edit(cells)
    },
    run(out: ReturnType<UndoHistory['undo']>) {
      t.edit(out!.edits)
      t.check(out!.checks)
      expect(out!.sheets).toEqual([])
      return out!
    },
    look: (key: string, field: string) => {
      const c = cellOf(t.find(key)!, field)
      return [c.kind, c.value]
    },
  }
  return t
}

const FIELDS = ['Death_date', 'Death_cause', 'CAM_ID', 'Tube_1_id']
const AI = { Death_date: 46000, Death_cause: 'preserved', CAM_ID: 'CAM079895', Tube_1_id: 'T0123' }

describe('undoing the person’s edits in a proposal’s table', () => {
  it('moves four cells one row up (a cut pasted) and back as one step, then redoes it', () => {
    const t = table(proposal([created('up', {}), created('row', { ...AI })]))
    const h = new UndoHistory()
    // The cut pasted: the cells written in the row above, the row they came from emptied, in one step.
    t.step(h, [
      ...FIELDS.map(field => ({ key: 'up', field, value: AI[field as keyof typeof AI] })),
      ...FIELDS.map(field => ({ key: 'row', field, value: null })),
    ])
    expect(FIELDS.map(f => t.look('up', f))).toEqual(FIELDS.map(f => ['person', AI[f as keyof typeof AI]]))
    // Emptied in a new row: the AI's value aside, not applied.
    expect(FIELDS.map(f => t.look('row', f)[0])).toEqual(['reverted', 'reverted', 'reverted', 'reverted'])

    const undone = t.run(h.undo(t.find))
    expect(undone.revised + undone.gone).toBe(0)
    // The row above empty again, the AI's values back in theirs (the AI's, not the person's).
    expect(FIELDS.map(f => t.look('up', f))).toEqual(FIELDS.map(() => ['empty', null]))
    expect(FIELDS.map(f => t.look('row', f))).toEqual(FIELDS.map(f => ['proposed', AI[f as keyof typeof AI]]))
    expect(t.find('row')!.personEdits).toBeUndefined()
    expect(t.find('up')!.personEdits).toBeUndefined()
    expect([h.done.length, h.undone.length]).toEqual([0, 1])

    t.run(h.redo(t.find))
    expect(FIELDS.map(f => t.look('up', f))).toEqual(FIELDS.map(f => ['person', AI[f as keyof typeof AI]]))
    expect(FIELDS.map(f => t.look('row', f)[0])).toEqual(['reverted', 'reverted', 'reverted', 'reverted'])
    expect([h.done.length, h.undone.length]).toEqual([1, 0])
  })

  it('puts back a value typed over another typed one, a cell set back to the sheet, and the checked mark «Valor de la IA» gave', () => {
    const t = table(
      proposal([
        created('r', { Sex: 'female', Notes: 'mine' }, {
          personEdits: { Sex: { ai: 'male' }, Notes: {} },
          doubts: { Sex: { confidence: 0.4 } },
        }),
      ]),
    )
    const h = new UndoHistory()
    t.step(h, [{ key: 'r', field: 'Notes', value: 'mine, again' }])
    t.step(h, [{ key: 'r', field: 'Sex', value: null, use: 'sheet' }])
    expect(t.look('r', 'Sex')).toEqual(['reverted', null])
    t.run(h.undo(t.find))
    expect(t.look('r', 'Sex')).toEqual(['person', 'female'])
    t.run(h.undo(t.find))
    expect(t.look('r', 'Notes')).toEqual(['person', 'mine'])
    // «Valor de la IA»: the AI's value again, which the server marks as checked; undone, unchecked again.
    const server = proposal([created('r', { Sex: 'male' }, { doubts: { Sex: { confidence: 0.4, checked: { how: 'ai-value' } } } })])
    const before = snapCell(
      created('r', { Sex: 'female' }, { personEdits: { Sex: { ai: 'male' } }, doubts: { Sex: { confidence: 0.4 } } }),
      'Sex',
    )
    expect(restoreOps(before, snapCell(server.changes[0], 'Sex'))).toEqual({
      edit: { key: 'r', field: 'Sex', value: 'female', before: 'male' },
      check: { key: 'r', field: 'Sex', checked: false },
    })
    // A cell as it was: nothing to send.
    expect(restoreOps(before, before)).toEqual({})
  })

  it('undoes «Marcar revisadas» by unmarking the cells', () => {
    const t = table(proposal([created('r', { Sex: 'male' }, { doubts: { Sex: { confidence: 0.4 } } })]))
    const h = new UndoHistory()
    const b = new StepBuilder()
    b.add(t.find('r')!, 'Sex')
    h.record(b.take())
    t.check([{ key: 'r', field: 'Sex', checked: true }])
    expect(cellOf(t.find('r')!, 'Sex').doubtful).toBe(false)
    const out = t.run(h.undo(t.find))
    expect(out.edits).toEqual([])
    expect(out.checks).toEqual([{ key: 'r', field: 'Sex', checked: false }])
    expect(cellOf(t.find('r')!, 'Sex').doubtful).toBe(true)
  })

  it('keeps the sheet’s value again in a cell edited in the sheet, once a value typed there is undone', () => {
    const edited = (values: ProposalChange['values'], extra: Partial<ProposalChange>): ProposalChange => ({
      index: 0,
      key: 'r',
      recordId: 'r',
      sheet: 'Insectary_data',
      row: 12,
      label: 'A0E',
      values,
      current: { Sex: 'male' },
      ...extra,
    })
    const read = { read: null, now: 'male', by: 'AA' }
    const kept = snapCell(edited({ Sex: 'female' }, { sheetChanged: { Sex: read } }), 'Sex')
    expect(kept.sheetUse).toBe('sheet')
    // Typed over: the person's value, written over the sheet's (the proposal's chosen).
    const typed = snapCell(
      edited({ Sex: 'unknown' }, { personEdits: { Sex: { ai: 'female' } }, sheetChanged: { Sex: { ...read, use: 'proposal' } } }),
      'Sex',
    )
    expect(restoreOps(kept, typed)).toEqual({
      edit: { key: 'r', field: 'Sex', value: 'female', before: 'unknown' },
      sheet: { key: 'r', field: 'Sex', use: 'sheet' },
    })
    // Only the choice beside the cell: only the choice back.
    const chosen = snapCell(edited({ Sex: 'female' }, { sheetChanged: { Sex: { ...read, use: 'proposal' } } }), 'Sex')
    expect(restoreOps(kept, chosen)).toEqual({ sheet: { key: 'r', field: 'Sex', use: 'sheet' } })
  })

  it('leaves the cells the AI revised since, and says how many', () => {
    const t = table(proposal([created('r', { Sex: 'male', CAM_ID: 'CAM079895' })]))
    const h = new UndoHistory()
    t.step(h, [
      { key: 'r', field: 'Sex', value: 'female' },
      { key: 'r', field: 'Notes', value: 'note' },
    ])
    // The AI revises the proposal meanwhile: it now proposes another Sex (the person's mark keeps it aside).
    t.server = proposal([created('r', { Sex: 'female', CAM_ID: 'CAM079895' }, { personEdits: { Sex: { ai: 'unknown' } } })])
    const out = t.run(h.undo(t.find))
    expect(out.revised).toBe(1)
    expect(out.edits.map(e => e.field)).toEqual(['Notes'])
    expect(t.look('r', 'Notes')).toEqual(['empty', null])
    expect(t.look('r', 'Sex')).toEqual(['person', 'female'])
    // Redo puts back only what was undone.
    const again = t.run(h.redo(t.find))
    expect(again.edits.map(e => [e.field, e.value])).toEqual([['Notes', 'note']])
    // A row no longer in the proposal: its cells are left, counted apart.
    t.server = proposal([])
    expect(h.undo(t.find)).toMatchObject({ gone: 1, edits: [] })
  })

  it('keeps up to its limit, and a new edit drops what could be redone', () => {
    const t = table(proposal([created('r', {})]))
    const h = new UndoHistory(3)
    for (const v of ['a', 'b', 'c', 'd']) t.step(h, [{ key: 'r', field: 'Notes', value: v }])
    expect(h.done).toHaveLength(3)
    t.run(h.undo(t.find))
    expect(t.look('r', 'Notes')).toEqual(['person', 'c'])
    expect(h.undone).toHaveLength(1)
    t.step(h, [{ key: 'r', field: 'Notes', value: 'e' }])
    expect(h.undone).toHaveLength(0)
    expect(h.redo(t.find)).toBeNull()
    // The steps of each table stay while the page is open.
    expect(historyFor('p1:Insectary_data')).toBe(historyFor('p1:Insectary_data'))
    expect(historyFor('p1:Insectary_data')).not.toBe(historyFor('p2:Insectary_data'))
  })
})
