import { describe, expect, it } from 'vitest'
import { cellId, cellOf, withLocal, type LocalCell, type Proposal, type ProposalChange } from '../proposals'
import { deathNote, planMove, rowsText, type MovePlan, type MoveRefusal } from '../proposalMove'

/** An existing row of Insectary_data: what the proposal sets (`values`) over what the sheet has (`sheet`). */
const row = (label: string, values: ProposalChange['values'], sheet: ProposalChange['values'] = {}, extra: Partial<ProposalChange> = {}): ProposalChange => ({
  index: 0,
  key: label,
  recordId: label,
  sheet: 'Insectary_data',
  row: 10,
  label,
  values,
  rowValues: sheet,
  ...extra,
})
const move = (rows: (ProposalChange | null)[], top: number, bottom: number, fields: string[], dir: 'up' | 'down') =>
  planMove({ rows, top, bottom, fields, dir, sheet: 'Insectary_data' })
const plan = (out: MovePlan | MoveRefusal) => {
  if (!out.ok) throw new Error(`refused: ${out.why}`)
  return out
}
/** The edits as { 'key field': value or 'sheet' }. */
const byCell = (out: MovePlan) => Object.fromEntries(out.edits.map(e => [`${e.key} ${e.field}`, e.use === 'sheet' ? 'sheet' : e.value]))

describe('planMove', () => {
  it('swaps one row’s cells with the row above, nothing lost', () => {
    const rows = [row('L3C', { Sex: 'male' }), row('L4C', { Sex: 'female' })]
    const out = plan(move(rows, 1, 1, ['Sex'], 'up'))
    expect(byCell(out)).toEqual({ 'L3C Sex': 'female', 'L4C Sex': 'male' })
    expect(out.edits.find(e => e.key === 'L3C')!.before).toBe('male')
    expect([out.from, out.to, out.target, out.death]).toEqual([['L4C'], ['L3C'], ['L3C'], false])
  })

  it('a value moved off a row leaves the sheet’s there; one moved onto a row with nothing proposed is typed', () => {
    const rows = [row('L3C', {}, { Sex: 'male' }), row('L4C', { Sex: 'female' }, { Sex: 'male' })]
    const out = plan(move(rows, 1, 1, ['Sex'], 'up'))
    expect(out.edits).toEqual([
      { key: 'L3C', field: 'Sex', value: 'female', before: null },
      { key: 'L4C', field: 'Sex', value: null, before: 'female', use: 'sheet' },
    ])
  })

  it('moves a block of rows down: the row below goes to the block’s top', () => {
    const rows = [row('A', { Sex: 'a' }), row('B', { Sex: 'b' }), row('C', { Sex: 'c' }), row('D', { Sex: 'd' })]
    const out = plan(move(rows, 0, 1, ['Sex'], 'down'))
    expect(byCell(out)).toEqual({ 'A Sex': 'c', 'B Sex': 'a', 'C Sex': 'b' })
    expect([out.to, out.target]).toEqual([['B', 'C'], ['B', 'C']])
    expect(rowsText(out.from)).toBe('A–B')
  })

  it('moves a block up: the row above goes to the block’s bottom', () => {
    const rows = [row('A', { Sex: 'a' }), row('B', { Sex: 'b' }), row('C', { Sex: 'c' }), row('D', { Sex: 'd' })]
    const out = plan(move(rows, 1, 2, ['Sex'], 'up'))
    expect(byCell(out)).toEqual({ 'A Sex': 'b', 'B Sex': 'c', 'C Sex': 'a' })
    expect(out.target).toEqual(['A', 'B'])
  })

  it('skips the slim rows between rows, inside the selection too', () => {
    const rows = [row('A', { Sex: 'a' }), null, row('B', { Sex: 'b' }), null, row('C', { Sex: 'c' })]
    expect(byCell(plan(move(rows, 2, 2, ['Sex'], 'up')))).toEqual({ 'A Sex': 'b', 'B Sex': 'a' })
    expect(byCell(plan(move(rows, 0, 2, ['Sex'], 'down')))).toEqual({ 'A Sex': 'c', 'B Sex': 'a', 'C Sex': 'b' })
    expect(move(rows, 1, 1, ['Sex'], 'up')).toEqual({ ok: false, why: 'nothing' })
  })

  it('refuses at the edge, over a row that cannot be edited, on a formula cell, and with nothing proposed', () => {
    const rows = [row('A', {}, {}, { context: true, index: -1 }), row('B', { Sex: 'b' }), row('C', { Sex: 'c' }, {}, { formulas: ['SPECIES'] })]
    expect(move(rows, 1, 1, ['Sex'], 'up')).toEqual({ ok: false, why: 'readonly', label: 'A' })
    expect(move(rows, 2, 2, ['Sex'], 'down')).toEqual({ ok: false, why: 'edge' })
    expect(move(rows, 1, 1, ['SPECIES'], 'down')).toEqual({ ok: false, why: 'locked', label: 'C', field: 'SPECIES' })
    expect(move([row('A', {}), row('B', {})], 1, 1, ['Sex'], 'up')).toEqual({ ok: false, why: 'empty' })
    expect(move(rows, 1, 1, [], 'down')).toEqual({ ok: false, why: 'nothing' })
  })

  it('a death moves whole: date, cause, its template where the sheet has none, and its note', () => {
    const death = {
      Death_date: 46300,
      Death_cause: 'Unknown',
      CAM_ID: 'NA',
      Tube_1_id: 'NA',
      Tube_1_tissue: 'NOT_COLLECTED',
      Preserved_Dead_Alive: 'NA',
      Research_purpose: 'NA',
      Notes_Insectary_data: 'Emerged deformed | 6/10/26 FC: eaten by ants',
    }
    const rows = [
      // L3C has a wing clip in the sheet (its CAM) and a note of its own.
      row('L3C', { Sex: 'male' }, { CAM_ID: 'CAM045123', Notes_Insectary_data: 'Wing clip' }),
      row('L4C', death, { Notes_Insectary_data: 'Emerged deformed' }),
    ]
    const out = plan(move(rows, 1, 1, ['Death_date'], 'up'))
    expect(byCell(out)).toEqual({
      'L3C Death_date': 46300,
      'L3C Death_cause': 'Unknown',
      'L3C Tube_1_id': 'NA',
      'L3C Tube_1_tissue': 'NOT_COLLECTED',
      'L3C Preserved_Dead_Alive': 'NA',
      'L3C Research_purpose': 'NA',
      'L3C Notes_Insectary_data': 'Wing clip | 6/10/26 FC: eaten by ants',
      'L4C Death_date': 'sheet',
      'L4C Death_cause': 'sheet',
      'L4C CAM_ID': 'sheet',
      'L4C Tube_1_id': 'sheet',
      'L4C Tube_1_tissue': 'sheet',
      'L4C Preserved_Dead_Alive': 'sheet',
      'L4C Research_purpose': 'sheet',
      'L4C Notes_Insectary_data': 'sheet',
    })
    // The template's NA does not go over L3C's CAM; Sex was not part of it.
    expect([out.death, out.note, out.kept]).toEqual([true, true, 1])
  })

  it('two deaths a line off swap whole, notes included', () => {
    const rows = [
      row('A', { Death_date: 1, Death_cause: 'Unknown', Notes_Insectary_data: 'a note' }),
      row('B', { Death_date: 2, Death_cause: 'Eaten', Notes_Insectary_data: 'old | b note' }, { Notes_Insectary_data: 'old' }),
    ]
    const out = plan(move(rows, 0, 0, ['Death_cause'], 'down'))
    expect(byCell(out)).toEqual({
      'A Death_date': 2,
      'A Death_cause': 'Eaten',
      'A Notes_Insectary_data': 'b note',
      'B Death_date': 1,
      'B Death_cause': 'Unknown',
      'B Notes_Insectary_data': 'old | a note',
    })
  })

  it('leaves notes not proposed with the death, and notes that are not the sheet’s plus more', () => {
    expect(deathNote(row('A', { Notes_Insectary_data: 'x' }))).toBeNull()
    expect(deathNote(row('A', { Death_date: 1, Notes_Insectary_data: 'rewritten' }, { Notes_Insectary_data: 'old' }))).toBeNull()
    expect(deathNote(row('A', { Death_date: 1, Notes_Insectary_data: 'old | new' }, { Notes_Insectary_data: 'old' }))).toBe('new')
    const rows = [row('A', {}), row('B', { Death_date: 1, Notes_Insectary_data: 'rewritten' }, { Notes_Insectary_data: 'old' })]
    const out = plan(move(rows, 1, 1, ['Death_date'], 'up'))
    expect(byCell(out)).toEqual({ 'A Death_date': 1, 'B Death_date': 'sheet' })
    expect(out.note).toBe(false)
  })

  it('only Insectary_data deaths extend; other columns move alone', () => {
    const rows = [row('A', {}), row('B', { Death_date: 1, Death_cause: 'Unknown' })]
    const out = plan(planMove({ rows, top: 1, bottom: 1, fields: ['Death_date'], dir: 'up', sheet: 'Collection_data' }))
    expect(byCell(out)).toEqual({ 'A Death_date': 1, 'B Death_date': 'sheet' })
  })

  it('once saved, the cells read as a cut-paste would leave them: the person’s value, the AI’s set aside', () => {
    const rows = [row('L3C', {}, { Death_date: null }), row('L4C', { Death_date: 46300 }, { Death_date: null })]
    const p: Proposal = { id: 'p', reason: '', status: 'pending', fields: [], types: {}, applied: null, changes: rows }
    const local = new Map<string, LocalCell>()
    for (const e of plan(move(rows, 1, 1, ['Death_date'], 'up')).edits)
      local.set(cellId(e.key, e.field), e.use ? { value: e.value, use: e.use } : { value: e.value })
    const [l3, l4] = withLocal(p, local).changes
    expect([cellOf(l3, 'Death_date').kind, cellOf(l3, 'Death_date').value]).toEqual(['person', 46300])
    expect([cellOf(l4, 'Death_date').kind, cellOf(l4, 'Death_date').ai]).toEqual(['reverted', 46300])
  })
})
