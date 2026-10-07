import { describe, expect, it } from 'vitest'
import {
  censusIndex,
  disappearanceEdits,
  findingsOf,
  markFinder,
  notSeen,
  progressOf,
  type CensusMark,
  type RosterEntry,
} from '../census'
import type { TableRow } from '../types'

const POLY = 'Mechanitis polymnia proceriformis'
const entry = (recordId: string, id: string, row: number | null, extra: Partial<RosterEntry> = {}): RosterEntry => ({
  recordId,
  id,
  row,
  species: POLY,
  sex: 'female',
  clutch: '990',
  entered: 46280,
  wild: false,
  ...extra,
})
let n = 0
const mark = (recordId: string | null, insectaryId: string, extra: Partial<CensusMark> = {}): CensusMark => ({
  id: `m${++n}`,
  recordId,
  insectaryId,
  kind: 'seen',
  species: POLY,
  doubt: null,
  note: null,
  actor: 'ana',
  actorName: 'Ana',
  createdAt: '2026-10-05T14:00:00Z',
  updatedAt: '2026-10-05T14:00:00Z',
  ...extra,
})
const roster = [
  entry('r1', 'A1B', 2),
  entry('r2', 'A2B', 3),
  entry('r3', 'A3B', 4),
  entry('staged:x', 'A9B', 10, { staged: true }),
]

describe('census progress and the butterflies not seen', () => {
  it('counts seen and left out; a mark made while the butterfly was in the app is found by its ID once written', () => {
    const marks = [mark('r1', 'A1B'), mark('r3', 'A3B', { kind: 'excluded', note: 'Other cage' }), mark('staged:x', 'A9B')]
    expect(progressOf(roster, marks)).toEqual({ seen: 2, excluded: 1, left: 1, total: 4 })
    expect(notSeen(roster, marks).map(b => b.id)).toEqual(['A2B'])
    // A9B written to its row since: the same mark.
    const written = [...roster.slice(0, 3), entry('r9', 'A9B', 10)]
    expect(markFinder(marks)(written[3])?.insectaryId).toBe('A9B')
    expect(notSeen(written, marks).map(b => b.id)).toEqual(['A2B'])
  })

  it('findings: doubts, another species, seen but not on the list, IDs in no row', () => {
    const marks = [
      mark('r1', 'A1B', { doubt: 'sex', note: 'Looks male' }),
      mark('r2', 'A2B'),
      mark('r7', 'A7B', { species: 'Mechanitis lysimnia' }),
      mark('r4', 'A4B'),
      mark(null, 'Q9Z', { kind: 'unknown' }),
      mark('r3', 'A3B', { kind: 'excluded' }),
    ]
    expect(findingsOf(' mechanitis polymnia  proceriformis', roster, marks).map(f => [f.mark.insectaryId, f.kind])).toEqual([
      ['A1B', 'doubt'],
      ['A7B', 'otherSpecies'],
      ['A4B', 'offList'],
      ['Q9Z', 'unknown'],
    ])
  })
})

describe('disappearances', () => {
  const row = (id: string, values: TableRow['values'], formulas: string[] = []): TableRow => ({
    id,
    row: 2,
    version: 1,
    observed: true,
    values: { Insectary_ID: id, ...values },
    formulas,
  })
  it('the cells Muertes writes for a death not preserved, on the census day, with what each cell holds now', () => {
    const rows = new Map([
      ['r1', row('r1', {})],
      // A wing clip: it keeps its CAM and tube, only the date and cause.
      ['r2', row('r2', { CAM_ID: 'CAM000500', Tube_1_id: 'FS00000500', Tube_1_tissue: 'WINGS' })],
      // NA already in some cells, and a formula cell never written.
      ['r3', row('r3', { Tube_4_id: 'NA', Location_body: '' }, ['Preservation_date'])],
    ])
    const { edits, absent } = disappearanceEdits(
      [entry('r1', 'A1B', 2), entry('r2', 'A2B', 3), entry('r3', 'A3B', 4), entry('r4', 'A4B', 5)],
      rows,
      46300,
    )
    expect(absent).toEqual(['A4B'])
    const [a1, a2, a3] = edits
    expect(a1.values).toEqual({
      Death_date: 46300,
      Death_cause: 'Disappearance',
      Preserved_Dead_Alive: 'NA',
      CAM_ID: 'NA',
      Tube_1_id: 'NA',
      Tube_1_tissue: 'NOT_COLLECTED',
      T1_Preservation_medium: 'NOT_COLLECTED',
      Tube_2_id: 'NA',
      Tube_2_tissue: 'NOT_COLLECTED',
      T2_Preservation_medium: 'NOT_COLLECTED',
      Tube_3_id: 'NA',
      Tube_3_tissue: 'NOT_COLLECTED',
      Tube_4_id: 'NA',
      Tube_4_tissue: 'NOT_COLLECTED',
      Preservation_medium: 'NOT_COLLECTED',
      Preservation_date: 'NA',
      Location_body: 'NA',
    })
    expect(Object.values(a1.expected).every(v => v === null)).toBe(true)
    expect(a2).toEqual({
      id: 'r2',
      values: { Death_date: 46300, Death_cause: 'Disappearance' },
      expected: { Death_date: null, Death_cause: null },
    })
    // NA counts as empty (deathCells writes over blank cells only): Tube_4_id stays out; the formula cell too.
    expect(a3.values).not.toHaveProperty('Preservation_date')
    expect(a3.values.Location_body).toBe('NA')
    expect(a3.expected.Location_body).toBe('')
  })
  it('each disappearance gets the note «Disappeared in census», after the notes it has', () => {
    const rows = new Map([
      ['r1', row('r1', {})],
      ['r2', row('r2', { Notes_Insectary_data: '1/9/26 MJS: Wing clip 30/8/26' })],
    ])
    const sign = { today: 46300, initials: 'FCH' }
    const { edits } = disappearanceEdits([entry('r1', 'A1B', 2), entry('r2', 'A2B', 3)], rows, 46300, sign)
    expect(edits[0].values.Notes_Insectary_data).toBe('5/10/26 FCH: Disappeared in census')
    expect(edits[1].values.Notes_Insectary_data).toBe('1/9/26 MJS: Wing clip 30/8/26 | 5/10/26 FCH: Disappeared in census')
    expect(edits[1].expected.Notes_Insectary_data).toBe('1/9/26 MJS: Wing clip 30/8/26')
    // A census finished another day says which census.
    const later = disappearanceEdits([entry('r1', 'A1B', 2)], rows, 46300, { today: 46301, initials: 'FCH' })
    expect(later.edits[0].values.Notes_Insectary_data).toBe('6/10/26 FCH: Disappeared in census of 5/10/26')
  })
})

describe('the matcher index', () => {
  it('keeps repeated IDs (an old dead B9D and a living one) and leaves pre-made rows out', () => {
    const rows: TableRow[] = [
      { id: 'a', row: 2, version: 1, observed: true, values: { Insectary_ID: 'B9D', Death_date: 44000 }, formulas: [] },
      { id: 'b', row: 900, version: 1, observed: true, values: { Insectary_ID: 'b9d ' }, formulas: [] },
      { id: 'c', row: 901, version: 1, observed: false, values: { Insectary_ID: 'C0D' }, formulas: [] },
    ]
    expect(censusIndex(rows).map(e => [e.row.id, e.key, e.order])).toEqual([
      ['a', 'B9D', 2],
      ['b', 'B9D', 900],
    ])
  })
})
