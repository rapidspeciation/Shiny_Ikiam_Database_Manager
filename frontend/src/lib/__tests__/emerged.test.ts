import { describe, expect, it } from 'vitest'
import { readCount } from '../clutches'
import { isoToSerial } from '../dates'
import {
  dayDoubt,
  draftValues,
  idProblem,
  nextId,
  siblingSpecies,
  skippedIds,
  stockOrigin,
  stockPlan,
  tallies,
  youngNote,
  type Draft,
  type RowContext,
} from '../emerged'
import type { CellValue } from '../types'

const draft = (over: Partial<Draft> = {}): Draft => ({
  key: 'k1',
  id: 'E4E',
  clutch: '994(6)',
  date: '2026-10-02',
  kind: 'adult',
  sex: 'female',
  fate: 'alive',
  species: '',
  stage: '',
  foundDead: false,
  note: '',
  cam: '',
  tube: '',
  ...over,
})
const DAY = isoToSerial('2026-10-02')
// The pre-made row's formulas (as in the sheet in Oct 2026).
const FORMULAS = ['Insectary_ID', 'SPECIES', 'Collection_location', 'Pedigree', 'T2_Preservation_medium', 'Tube_1_rack']
const ctx = (over: Partial<RowContext> = {}): RowContext => ({
  clutchValue: '994(6)',
  clutchSpecies: 'Mechanitis messenoides deceptus',
  generation: 'F1',
  formulas: FORMULAS,
  today: isoToSerial('2026-10-03'),
  initials: 'FCH',
  medium: 'Flash frozen',
  ...over,
})

describe('species and stock', () => {
  it('stock only for the messenoides lines, the clutch subspecies', () => {
    expect(stockOrigin('Mechanitis messenoides deceptus')).toBe('deceptus')
    expect(stockOrigin('Mechanitis messenoides intermedia')).toBe('intermedia')
    expect(stockOrigin('Mechanitis polymnia proceriformis')).toBe('NA')
    expect(stockOrigin('')).toBe('NA')
  })
  it('the other subspecies of the clutch species, hybrids only for a hybrid clutch', () => {
    const known = ['Mechanitis polymnia eurydice', 'Mechanitis polymnia proceriformis', 'Mechanitis polymnia proceriformis x werneri', 'Ithomia salapia salapia']
    expect(siblingSpecies('Mechanitis polymnia proceriformis', known)).toEqual(['Mechanitis polymnia proceriformis', 'Mechanitis polymnia eurydice'])
    expect(siblingSpecies('', known)).toEqual([])
  })
})

describe('Insectary IDs', () => {
  const order = ['A0D', 'H0E', 'H1E', 'H2E', 'H3E', 'H4E']
  it('the next free ID after the last one the cards hold, else the first after the last row used', () => {
    expect(nextId(order, 'H0E', [])).toBe('H0E')
    expect(nextId(order, 'H0E', ['H0E', 'H1E'])).toBe('H2E')
    // A card changed to an earlier empty row (the wing says A0D) is followed by the next in the sheet.
    expect(nextId(order, 'H0E', ['A0D'])).toBe('H0E')
    // The last card removed: its ID is given again.
    expect(nextId(order, 'H0E', ['H0E'])).toBe('H1E')
    expect(nextId(order, 'H0E', ['H4E'])).toBeNull()
  })
  it('free IDs skipped between the cards', () => {
    expect(skippedIds(order, ['H0E', 'H3E'])).toEqual(['H1E', 'H2E'])
    expect(skippedIds(order, ['H0E', 'H1E'])).toEqual([])
  })
  it('an ID must be a free pre-made row, on one card only; a suffixed duplicate goes to the server', () => {
    const free = new Set(['H0E', 'H1E'])
    const used = new Set(['W0B'])
    expect(idProblem('h0e', [], free, used)).toBe('')
    expect(idProblem('H0E', ['H0E'], free, used)).toBe('repeated')
    expect(idProblem('W0B', [], free, used)).toBe('used')
    expect(idProblem('Z9Z', [], free, used)).toBe('not-free')
    expect(idProblem('W0B.1', [], free, used)).toBe('')
    expect(idProblem(' ', [], free, used)).toBe('empty')
  })
})

describe('the row a card writes', () => {
  it('an adult alive: the emergence values only, SPECIES left to the formula', () => {
    expect(draftValues(draft(), ctx())).toEqual({
      Insectary_ID: 'E4E',
      Wild_Reared: 'Reared',
      'CLUTCH NUMBER': '994(6)',
      Stock_of_origin: 'deceptus',
      Sex: 'female',
      Intro2Insectary_date: DAY,
    })
  })
  it('another subspecies is typed over the formula; the stock stays the clutch subspecies', () => {
    const v = draftValues(draft({ species: 'Mechanitis messenoides intermedia' }), ctx())
    expect(v.SPECIES).toBe('Mechanitis messenoides intermedia')
    expect(v.Stock_of_origin).toBe('deceptus')
    // The clutch's own species chosen: nothing typed.
    expect(draftValues(draft({ species: 'Mechanitis messenoides deceptus' }), ctx()).SPECIES).toBeUndefined()
  })
  it('a hybrid alive gets its Research_purpose', () => {
    expect(draftValues(draft(), ctx({ clutchSpecies: 'Mechanitis polymnia proceriformis x werneri' })).Research_purpose).toBe('F1/F2 mutation rate')
  })
  it('deformed on its emergence day: died that day, not preserved', () => {
    const v = draftValues(draft({ fate: 'deformed', sex: 'NA', note: "Deformed wings, can't fly" }), ctx())
    expect(v).toMatchObject({
      Sex: 'NA',
      Intro2Insectary_date: DAY,
      Death_date: DAY,
      Death_cause: 'Deformed',
      Research_purpose: 'NA',
      CAM_ID: 'NA',
      Tube_1_id: 'NA',
      Tube_1_tissue: 'NOT_COLLECTED',
      Preservation_date: 'NA',
      Location_body: 'NA',
      Notes_Insectary_data: "3/10/26 FCH: Deformed wings, can't fly",
    })
    // A formula of the pre-made row is never written.
    expect('T2_Preservation_medium' in v).toBe(false)
  })
  it('killed and preserved on its emergence day: CAM, tube, whole organism', () => {
    const v = draftValues(draft({ fate: 'preserved', cam: 'cam078400', tube: 'fs90415500' }), ctx())
    expect(v).toMatchObject({
      Death_date: DAY,
      Death_cause: 'Killed_Preserved',
      CAM_ID: 'CAM078400',
      Tube_1_id: 'FS90415500',
      Tube_1_tissue: 'WHOLE_ORGANISM',
      T1_Preservation_medium: 'Flash frozen',
      Preservation_date: DAY,
      Preserved_Dead_Alive: 'Alive',
      Location_body: 'Ikiam',
      Research_purpose: 'F1/F2 mutation rate',
    })
  })
  it('a larva preserved: NOT_COLLECTED, emergence NA, its stage, a default note', () => {
    const v = draftValues(draft({ kind: 'young', stage: '3rd instar larva', cam: 'CAM1', tube: 'FS1' }), ctx())
    expect(v).toMatchObject({
      Sex: 'NOT_COLLECTED',
      Intro2Insectary_date: 'NA',
      LIFESTAGE: '3rd instar larva',
      Death_date: DAY,
      Death_cause: 'Killed_Preserved',
      Preserved_Dead_Alive: 'Alive',
      Research_purpose: 'F1/F2 mutation rate',
      Notes_Insectary_data: '3/10/26 FCH: Preserved alive 3rd instar',
    })
    const dead = draftValues(draft({ kind: 'young', stage: 'Egg', foundDead: true, cam: 'CAM1', tube: 'FS1' }), ctx())
    expect(dead).toMatchObject({ Death_cause: 'Other', Preserved_Dead_Alive: 'Dead' })
    expect(youngNote('Pre-pupa', false)).toBe('Preserved alive prepupa')
  })
})

describe("the clutch's row", () => {
  const counts: Record<string, CellValue> = { 'NUMBER OF PUPA': '=10', 'NUMBER OF ADULTS': '=2+2', 'NUMBER OF LARVAE': '=9+3', 'NUMBER OF EGGS': '=12' }
  const plan = (drafts: Draft[], values: Record<string, CellValue> = {}) =>
    stockPlan(tallies(drafts)[0], f => readCount(counts[f] ?? null), f => (f in values ? values[f] : (counts[f] ?? null)), { today: isoToSerial('2026-10-03'), initials: 'FCH' })
  it("each day's adults as one more term, the first emergence date when empty", () => {
    const p = plan([draft(), draft({ key: 'k2', id: 'E5E' }), draft({ key: 'k3', id: 'E6E', date: '2026-10-03', sex: 'male' })])
    expect(p.cells).toEqual([
      { field: 'NUMBER OF ADULTS', value: '=2+2+2+1', before: '=2+2' },
      { field: 'EMERGENCE DATE', value: DAY, before: null },
    ])
    // A first emergence date already there stays.
    expect(plan([draft()], { 'EMERGENCE DATE': DAY - 3 }).cells.map(c => c.field)).toEqual(['NUMBER OF ADULTS'])
  })
  it('larvae and eggs preserved taken off their counts, with a note', () => {
    const p = plan([draft({ kind: 'young', stage: '3rd instar larva' }), draft({ key: 'k2', kind: 'young', stage: '4th instar larva' }), draft({ key: 'k3', kind: 'young', stage: 'Egg' })])
    expect(p.cells).toEqual([
      { field: 'NUMBER OF LARVAE', value: '=9+3-2', before: '=9+3' },
      { field: 'NUMBER OF EGGS', value: '=12-1', before: '=12' },
      { field: 'NOTES', value: '3/10/26 FCH: 2 larvae and 1 egg preserved 2/10', before: null },
    ])
  })
  it('a count that would go below 0 is left as it is', () => {
    const p = stockPlan(tallies([draft({ kind: 'young', stage: 'Egg' })])[0], () => readCount(null), () => null, { today: 1, initials: 'X' })
    expect(p.skipped).toEqual(['NUMBER OF EGGS'])
  })
})

describe('the day', () => {
  it('flags a day in the future, before laying or before the first pupa', () => {
    expect(dayDoubt(10, 9, null, null)).toBe('future')
    expect(dayDoubt(5, 9, 6, null)).toBe('before-laid')
    expect(dayDoubt(7, 9, 1, 8)).toBe('before-pupa')
    expect(dayDoubt(9, 9, 1, 8)).toBe('')
  })
})
