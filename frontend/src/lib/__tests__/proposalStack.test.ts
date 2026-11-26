import { describe, expect, it } from 'vitest'
import { stackRows, stackToDecide } from '../proposalStack'
import type { ProposalChange } from '../proposals'

// A proposal's rows for a phone: each row's changed cells one under the other, the sheet's value over the new one.
const edited = (key: string, values: ProposalChange['values'], extra: Partial<ProposalChange> = {}): ProposalChange => ({
  index: 0,
  key,
  recordId: key,
  sheet: 'Insectary_data',
  row: 13687,
  label: 'Q5E',
  values,
  rowValues: { Death_date: 46300, Death_cause: 'Killed_Preserved', Sex: 'male', SPECIES: 'Mechanitis lysimnia' },
  formulas: ['SPECIES'],
  ...extra,
})
const FIELDS = ['Death_date', 'Death_cause', 'Sex', 'SPECIES', 'CAM_ID']

describe('the stacked list of a proposal', () => {
  it('lists every cell the proposal changes, with what the sheet has now, and leaves the others out', () => {
    const [row] = stackRows([edited('a', { Death_date: 46347, Death_cause: 'Natural_death', Sex: 'male' }, { note: 'Cuaderno p. 41' })], FIELDS)
    expect(row).toMatchObject({ key: 'a', label: 'Q5E', row: 13687, create: false, note: 'Cuaderno p. 41', refused: false, written: false })
    // Sex is proposed as the sheet already has it: nothing to review. SPECIES and CAM_ID are not touched.
    expect(row.fields.map(f => f.field)).toEqual(['Death_date', 'Death_cause'])
    expect(row.fields[0]).toMatchObject({ state: 'ai', was: 46300, value: 46347, needsTable: false })
    expect(row.fields[1]).toMatchObject({ state: 'ai', was: 'Killed_Preserved', value: 'Natural_death' })
  })
  it('keeps the columns in the order given (the table\'s)', () => {
    const [row] = stackRows([edited('a', { Death_date: 46347, Death_cause: 'Natural_death' })], ['Death_cause', 'Death_date'])
    expect(row.fields.map(f => f.field)).toEqual(['Death_cause', 'Death_date'])
  })
  it('a new row shows only what it writes, with no sheet value', () => {
    const created: ProposalChange = { index: 1, key: 'n1', clientId: 'n1', recordId: null, sheet: 'Insectary_data', row: null, label: 'S2E', create: true, values: { Sex: 'female', CAM_ID: '' } }
    const [row] = stackRows([created], FIELDS)
    expect(row).toMatchObject({ create: true, row: null })
    expect(row.fields).toHaveLength(1)
    expect(row.fields[0]).toMatchObject({ field: 'Sex', state: 'ai', value: 'female' })
    expect(row.fields[0].was).toBeUndefined()
  })
  it('says whose value it is and which cells wait for a decision in the table', () => {
    const rows = stackRows(
      [
        edited('typed', { Sex: 'female' }, { personEdits: { Sex: {} } }),
        edited('doubt', { Sex: 'female' }, { doubts: { Sex: { reason: 'tinta corrida' } } }),
        edited('checked', { Sex: 'female' }, { doubts: { Sex: { reason: 'x', checked: { by: 'Lab' } } } }),
        edited('unread', {}, { unreadable: { Sex: { reason: 'mancha de agua' } } }),
        edited('kept', { Sex: 'female' }, { sheetChanged: { Sex: { read: 'male', now: 'unknown' } } }),
        edited('over', { Sex: 'female' }, { sheetChanged: { Sex: { read: 'male', now: 'unknown', use: 'proposal' } } }),
        edited('back', {}, { personEdits: { Sex: { ai: 'female' } } }),
      ],
      FIELDS,
    )
    const of = (key: string) => rows.find(r => r.key === key)!.fields[0]
    expect(of('typed')).toMatchObject({ state: 'person', value: 'female', needsTable: false })
    expect(of('doubt')).toMatchObject({ state: 'doubtful', why: 'tinta corrida', needsTable: true })
    // Reviewed already: the assistant's value, like any other.
    expect(of('checked')).toMatchObject({ state: 'ai', needsTable: false })
    expect(of('unread')).toMatchObject({ state: 'unreadable', why: 'mancha de agua', needsTable: true })
    // Edited in the sheet after the proposal: the sheet's value stays, the proposal's is set aside, until someone chooses.
    expect(of('kept')).toMatchObject({ state: 'kept', value: 'unknown', aside: 'female', needsTable: true })
    // The person chose the proposal's value: written over what the sheet has now.
    expect(of('over')).toMatchObject({ state: 'ai', was: 'unknown', value: 'female', needsTable: false })
    // Set back to the sheet's value: the assistant's is set aside, not written.
    expect(of('back')).toMatchObject({ state: 'reverted', value: 'male', aside: 'female', needsTable: false })
    expect(stackToDecide(rows)).toBe(3)
  })
  it('leaves the lines shown only to follow the page to the table; a refused line stays, with why', () => {
    const rows = stackRows(
      [
        edited('context', {}, { context: true }),
        edited('gap', {}, { context: true, gap: true }),
        edited('refused', { Sex: 'x' }, { context: true, note: 'Línea 4', page: { photo: 0, line: 4, error: 'Sex no acepta «x»' } as ProposalChange['page'] }),
        edited('same', { Sex: 'male' }),
      ],
      FIELDS,
    )
    expect(rows.map(r => r.key)).toEqual(['refused'])
    expect(rows[0]).toMatchObject({ refused: true, fields: [] })
    expect(rows[0].note).toContain('Sex no acepta «x»')
  })
  it('marks a butterfly already dead in the sheet and a row written by an earlier apply', () => {
    const [dead, written] = stackRows(
      [
        edited('dead', { Death_cause: 'Natural_death' }, { sheetDeath: { date: 46300 } as ProposalChange['sheetDeath'] }),
        edited('written', { Death_cause: 'Natural_death' }, { applied: 1 }),
      ],
      FIELDS,
    )
    expect(dead.dead).toBe(true)
    expect(written.written).toBe(true)
  })
  it('a preserved butterfly left without its CAM is flagged, though nothing is written there', () => {
    const [row] = stackRows([edited('a', { Death_cause: 'Killed_Preserved' }, { rowValues: { Death_cause: 'Natural_death' }, warnings: { CAM_ID: { text: 'Falta el CAM' } } })], FIELDS)
    expect(row.fields.find(f => f.field === 'CAM_ID')).toMatchObject({ state: 'warned', why: 'Falta el CAM' })
  })
})
