import { describe, expect, it } from 'vitest'
import { sharedFold, sharedNote, stackRows, stackToDecide } from '../proposalStack'
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
  it('a new row whose pre-made row was used meanwhile is not written, and says so instead of showing its values', () => {
    const taken: ProposalChange = {
      index: 3,
      key: 't',
      clientId: 't',
      recordId: null,
      sheet: 'Insectary_data',
      row: null,
      label: 'S2E',
      create: true,
      values: { Sex: 'female', Death_cause: 'Natural_death' },
      note: 'Cuaderno p. 41',
      rowTaken: { row: 13699 } as ProposalChange['rowTaken'],
    }
    const [row] = stackRows([taken], FIELDS)
    expect(row).toMatchObject({ key: 't', refused: true, look: false, fields: [] })
    expect(row.note).toBe('Su fila sin usar (13699) ya se usó en la hoja: esta fila no se escribe · Cuaderno p. 41')
  })
  it('a page line with no row to write to stays, to look at, with why; a crossed-out one does not', () => {
    const line = (key: string, status: string): ProposalChange =>
      edited(key, {}, { context: true, placeholder: true, row: null, label: key, page: { photo: 0, line: 2, status, raw: 'Q9X ♀' } as ProposalChange['page'] })
    const rows = stackRows([line('missing', 'missing'), line('ambiguous', 'ambiguous'), line('crossed', 'crossed')], FIELDS)
    expect(rows.map(r => r.key)).toEqual(['missing', 'ambiguous'])
    expect(rows[0]).toMatchObject({ look: true, refused: false, fields: [] })
    expect(rows[0].note).toContain('No está en la hoja')
    expect(rows[1].note).toContain('Varias filas de la hoja podrían ser esta')
  })
  it('nothing «Aplicar» writes is left out: the template\'s columns are there, flagged to fold', () => {
    const created: ProposalChange = {
      index: 1,
      key: 'n1',
      clientId: 'n1',
      recordId: null,
      sheet: 'Insectary_data',
      row: null,
      label: 'S2E',
      create: true,
      values: { Sex: 'female', Stock_of_origin: 'NA', Bait: 'NOT_COLLECTED' },
    }
    const [row] = stackRows([created], ['Sex', 'Stock_of_origin', 'Bait'], [], ['Stock_of_origin', 'Bait'])
    expect(row.fields.map(f => [f.field, !!f.template, f.value])).toEqual([
      ['Sex', false, 'female'],
      ['Stock_of_origin', true, 'NA'],
      ['Bait', true, 'NOT_COLLECTED'],
    ])
  })
  it('only a plain write folds: a template column\'s cell that waits for a decision always shows', () => {
    const rows = stackRows(
      [
        edited('kept', { Stock_of_origin: 'NA' }, { rowValues: { Stock_of_origin: '' }, sheetChanged: { Stock_of_origin: { read: '', now: 'Ikiam' } } }),
        edited('doubt', { Stock_of_origin: 'NA' }, { rowValues: { Stock_of_origin: '' }, doubts: { Stock_of_origin: { reason: 'borroso' } } }),
        edited('plain', { Stock_of_origin: 'NA' }, { rowValues: { Stock_of_origin: '' } }),
      ],
      ['Stock_of_origin'],
      [],
      ['Stock_of_origin'],
    )
    expect(rows.map(r => [r.key, r.fields[0].state, !!r.fields[0].template])).toEqual([
      ['kept', 'kept', false],
      ['doubt', 'doubtful', false],
      ['plain', 'ai', true],
    ])
  })
  it('a fold every row shares is counted once; rows that differ count their own', () => {
    const created = (key: string, values: ProposalChange['values']): ProposalChange => ({ index: 1, key, clientId: key, recordId: null, sheet: 'Insectary_data', row: null, label: key, create: true, values })
    const same = stackRows([created('a', { Sex: 'female', Bait: 'NA' }), created('b', { Sex: 'male', Bait: 'NA' })], ['Sex', 'Bait'], [], ['Bait'])
    expect(sharedFold(same)).toBe(1)
    const differ = stackRows([created('a', { Sex: 'female', Bait: 'NA' }), created('b', { Sex: 'male' })], ['Sex', 'Bait'], [], ['Bait'])
    expect(sharedFold(differ)).toBe(0)
    expect(sharedFold(same.slice(0, 1))).toBe(0)
    expect(sharedFold(stackRows([created('a', { Sex: 'female' }), created('b', { Sex: 'male' })], ['Sex'], [], []))).toBe(0)
  })
  it('keeps the table\'s marks: a value not written on the line, a row the assistant flags', () => {
    const [row] = stackRows([edited('a', { Death_cause: 'Natural_death', Sex: 'female' }, { inferred: ['Death_cause'], highlight: true })], FIELDS)
    expect(row.marked).toBe(true)
    expect(row.fields.find(f => f.field === 'Death_cause')!.inferred).toBe(true)
    expect(row.fields.find(f => f.field === 'Sex')!.inferred).toBe(false)
  })
  it('a note every row shares is said once', () => {
    const same = stackRows([edited('a', { Sex: 'female' }, { note: 'Cuaderno p. 41' }), edited('b', { Sex: 'female' }, { note: 'Cuaderno p. 41' })], FIELDS)
    expect(sharedNote(same)).toBe('Cuaderno p. 41')
    const mixed = stackRows([edited('a', { Sex: 'female' }, { note: 'Cuaderno p. 41' }), edited('b', { Sex: 'female' }, { note: 'Foto 2' })], FIELDS)
    expect(sharedNote(mixed)).toBe('')
    // One row keeps its own note; rows without notes share nothing.
    expect(sharedNote(same.slice(0, 1))).toBe('')
    expect(sharedNote(stackRows([edited('a', { Sex: 'female' }), edited('b', { Sex: 'female' })], FIELDS))).toBe('')
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
