import { describe, expect, it } from 'vitest'
import { deathCells, type Getter } from '../deaths'
import {
  WHOLE,
  WING_CLIP,
  assign,
  choiceFor,
  formProblem,
  freeSlots,
  localRun,
  nextAfter,
  problemsOf,
  setChoice,
  sharedChoice,
  tubeCells,
  type TubeChoice,
} from '../tubes'
import type { CellValue, TableRow } from '../types'

let next = 1
function row(values: Record<string, CellValue>, extra: Partial<TableRow> = {}): TableRow {
  const n = next++
  return { id: `r${n}`, row: 100 + n, version: 1, observed: true, values, formulas: [], ...extra }
}
const saved: Getter = (r, f) => r.values[f] ?? null
const asObject = (cells: { field: string; value: CellValue }[]) => Object.fromEntries(cells.map(c => [c.field, c.value]))
const DAY = 46296 // 1-Oct-26
const ISO = '2026-10-01'
const whole: TubeChoice = { kind: 'whole', parts: [], medium: 'Flash frozen', date: ISO, closeRest: true }
const opts = { today: DAY, initials: 'FCH' }

describe('what a card writes', () => {
  it('a whole body: as the team types a preservation (and as Muertes writes a preserved body)', () => {
    const r = row({ Insectary_ID: 'D2E', T2_Preservation_medium: null })
    const cells = asObject(tubeCells(r, saved, whole, { cam: 'cam078323', tubes: ['FS90415432'] }, opts))
    expect(cells).toEqual({
      CAM_ID: 'CAM078323',
      Tube_1_id: 'FS90415432',
      Tube_1_tissue: WHOLE,
      T1_Preservation_medium: 'Flash frozen',
      Preservation_date: DAY,
      Death_date: DAY,
      Death_cause: 'Killed_Preserved',
      Preserved_Dead_Alive: 'Alive',
      Location_body: 'Ikiam',
      Tube_2_id: 'NA',
      Tube_2_tissue: 'NOT_COLLECTED',
      T2_Preservation_medium: 'NOT_COLLECTED',
      Tube_3_id: 'NA',
      Tube_3_tissue: 'NOT_COLLECTED',
      Tube_4_id: 'NA',
      Tube_4_tissue: 'NOT_COLLECTED',
    })
    const death = deathCells(r, saved, { serial: DAY, cause: '', notPreserved: false, preserve: { cam: 'CAM078323', tube: 'FS90415432', medium: 'Flash frozen' } })
    expect(asObject(death)).toEqual(cells)
  })

  it('a body found dead keeps its death date and cause; the unused tubes stay empty when asked', () => {
    const r = row({ Insectary_ID: 'C3E', Death_date: DAY - 1, Death_cause: 'Unknown' })
    const cells = asObject(tubeCells(r, saved, { ...whole, medium: 'Ethanol', closeRest: false }, { cam: 'CAM078400', tubes: ['FS90415500'] }, opts))
    expect(cells.Death_date).toBeUndefined()
    expect(cells.Death_cause).toBeUndefined()
    expect(cells.Preserved_Dead_Alive).toBe('Dead')
    expect(cells.T1_Preservation_medium).toBe('Ethanol')
    expect(cells.Tube_2_id).toBeUndefined()
  })

  it('a wing-clipped butterfly that dies keeps its CAM; its body goes in the next tube', () => {
    const r = row({ Insectary_ID: 'O8D', CAM_ID: 'CAM078322', Tube_1_id: 'FS90415337', Tube_1_tissue: WING_CLIP })
    expect(freeSlots(f => saved(r, f))).toEqual([2, 3, 4])
    const cells = asObject(tubeCells(r, saved, whole, { cam: 'CAM999999', tubes: ['FS90415493'] }, opts))
    expect(cells.CAM_ID).toBeUndefined()
    expect(cells.Tube_2_id).toBe('FS90415493')
    expect(cells.Tube_2_tissue).toBe(WHOLE)
    expect(cells.T2_Preservation_medium).toBe('Flash frozen')
    expect(cells.Tube_3_id).toBe('NA')
  })

  it('a wing clip: tube, tissue, medium and the dated note after the notes there', () => {
    const r = row({ Insectary_ID: 'B1E', Notes_Insectary_data: 'Small' })
    const clip: TubeChoice = { ...whole, kind: 'clip', date: '2026-09-27' }
    const cells = asObject(tubeCells(r, saved, clip, { cam: 'CAM078401', tubes: ['FS90415338'] }, opts))
    expect(cells).toEqual({
      CAM_ID: 'CAM078401',
      Tube_1_id: 'FS90415338',
      Tube_1_tissue: WING_CLIP,
      T1_Preservation_medium: 'Flash frozen',
      Notes_Insectary_data: 'Small | 1/10/26 FCH: Wing clip 27/9/26',
    })
  })

  it('a split body: one tube per part, in the free columns', () => {
    const r = row({ Insectary_ID: 'X1E' })
    const parts: TubeChoice = { ...whole, kind: 'parts', parts: ['HEAD | ABDOMEN', 'THORAX'] }
    const cells = asObject(tubeCells(r, saved, parts, { cam: 'CAM078402', tubes: ['FS1', 'FS2'] }, opts))
    expect([cells.Tube_1_id, cells.Tube_1_tissue, cells.Tube_2_id, cells.Tube_2_tissue]).toEqual(['FS1', 'HEAD | ABDOMEN', 'FS2', 'THORAX'])
    expect(cells.T2_Preservation_medium).toBe('Flash frozen')
    expect(cells.Tube_3_id).toBe('NA')
  })

  it('not preserved: the NA / NOT_COLLECTED block, only on rows without CAM or tube', () => {
    const none: TubeChoice = { ...whole, kind: 'none' }
    const cells = asObject(tubeCells(row({ Insectary_ID: 'D1E' }), saved, none, { cam: '', tubes: [] }, opts))
    expect(cells.CAM_ID).toBe('NA')
    expect(cells.Tube_1_tissue).toBe('NOT_COLLECTED')
    expect(tubeCells(row({ Insectary_ID: 'D1E', CAM_ID: 'CAM1' }), saved, none, { cam: '', tubes: [] }, opts)).toEqual([])
  })

  it('formula cells are never written', () => {
    const r = row({ Insectary_ID: 'D3E' }, { formulas: ['T2_Preservation_medium'] })
    expect(tubeCells(r, saved, whole, { cam: 'CAM1', tubes: ['FS90415433'] }, opts).some(c => c.field === 'T2_Preservation_medium')).toBe(false)
  })
})

describe('the form of a tube or CAM', () => {
  it('two letters and eight digits; a dropped or doubled digit offers the reading next to the run', () => {
    expect(formProblem('tube', 'fs90415474')).toBeNull()
    expect(formProblem('tube', 'FS5848994', ['FS50848990'])).toEqual({ problem: 'digits', fix: 'FS50848994' })
    expect(formProblem('tube', 'FS3886683', ['FS63886682'])).toEqual({ problem: 'digits', fix: 'FS63886683' })
    expect(formProblem('tube', 'FS904154744', ['FS90415474'])).toEqual({ problem: 'digits', fix: 'FS90415474' })
    expect(formProblem('tube', 'FS5848994')).toEqual({ problem: 'digits', fix: undefined })
    expect(formProblem('tube', 'CAM078300')?.problem).toBe('cam')
    expect(formProblem('tube', 'hello')?.problem).toBe('format')
    expect(formProblem('tube', 'NA')).toBeNull()
  })
  it('CAM + six digits; an extra 0 is offered back', () => {
    expect(formProblem('cam', 'CAM078300')).toBeNull()
    expect(formProblem('cam', 'CAM0770542', ['CAM077540'])).toEqual({ problem: 'digits', fix: 'CAM077542' })
    expect(formProblem('cam', 'FS90415474')?.problem).toBe('tube')
  })
})

describe('the next free CAMs and tubes', () => {
  const used = new Set(['FS90415495'])
  const run = (start: string, count: number) => localRun(start, count, id => used.has(id))
  it('in the cards order, skipping used ones, and the IDs typed on any card', () => {
    expect(nextAfter('FS90415499')).toBe('FS90415500')
    const needs = [
      { id: 'A', cam: true, tubes: 1 },
      { id: 'B', cam: false, tubes: 1 },
      { id: 'C', cam: true, tubes: 1 },
      { id: 'D', cam: true, tubes: 1 },
    ]
    const out = assign(needs, { D: { tubes: ['FS90415494'] } }, { camStart: 'CAM078366', tubeStart: 'FS90415493', run })
    expect(out.A).toEqual({ cam: { value: 'CAM078366', auto: true }, tubes: [{ value: 'FS90415493', auto: true }] })
    expect(out.B.cam).toBeNull()
    // FS90415494 is typed on D, FS90415495 used: C gets 496.
    expect(out.B.tubes[0].value).toBe('FS90415496')
    expect(out.C.tubes[0].value).toBe('FS90415497')
    expect(out.C.cam?.value).toBe('CAM078367')
    expect(out.D.tubes[0]).toEqual({ value: 'FS90415494', auto: false })
  })
  it('after a tube scanned on a card, the next cards go on from it (the rack is there)', () => {
    const needs = ['A', 'B', 'C'].map(id => ({ id, cam: false, tubes: 1 }))
    const out = assign(needs, { A: { tubes: ['FS90415600'] } }, { camStart: '', tubeStart: 'FS90415493', run })
    expect(needs.map(n => out[n.id].tubes[0].value)).toEqual(['FS90415600', 'FS90415601', 'FS90415602'])
    // A box emptied on purpose stays empty; a badly typed tube does not move the run.
    const kept = assign(needs, { A: { tubes: [''] }, B: { tubes: ['FS9041560'] } }, { camStart: '', tubeStart: 'FS90415493', run })
    expect(needs.map(n => kept[n.id].tubes[0].value)).toEqual(['', 'FS9041560', 'FS90415493'])
  })
  it('a split body takes consecutive tubes', () => {
    const out = assign([{ id: 'A', cam: true, tubes: 2 }, { id: 'B', cam: true, tubes: 1 }], {}, { camStart: 'CAM1', tubeStart: 'FS10', run })
    expect(out.A.tubes.map(t => t.value)).toEqual(['FS10', 'FS11'])
    expect(out.B.tubes[0].value).toBe('FS12')
  })
})

describe('what blocks Save', () => {
  it('missing, misread, repeated on two cards, or used already', () => {
    const card = (id: string, cam: string, tube: string, extra: Partial<TubeChoice> = {}) => ({
      id,
      choice: { ...whole, ...extra },
      free: 4,
      needsCam: true,
      assigned: { cam: { value: cam, auto: false }, tubes: [{ value: tube, auto: false }] },
    })
    const problems = problemsOf(
      [card('A', 'CAM078300', 'FS90415493'), card('B', 'CAM078300', 'FS9041549'), card('C', '', 'FS90415400'), card('D', 'CAM078301', 'FS90415494', { date: '' })],
      { used: new Map([['FS90415400', 'Collection_data fila 9']]), near: ['FS90415493'] },
    )
    expect(problems.get('A')).toEqual([])
    expect(problems.get('B')!.map(p => p.kind)).toEqual(['repeated', 'form'])
    expect(problems.get('C')!.map(p => p.kind)).toEqual(['missing', 'used'])
    expect(problems.get('D')!.map(p => p.kind)).toEqual(['date'])
    // A tube read as written on its label is accepted.
    const ok = problemsOf([card('B', 'CAM078300', 'FS9041549')], { used: new Map(), accepted: new Set(['FS9041549']) })
    expect(ok.get('B')).toEqual([])
    const full = problemsOf([{ ...card('E', 'CAM1', 'FS1'), free: 0 }], { used: new Map() })
    expect(full.get('E')![0]).toEqual({ kind: 'slots', need: 1, free: 0 })
  })
})

describe('own choices of selected cards', () => {
  it('set for the selected cards only, or for all', () => {
    let state = setChoice(whole, {}, ['A'], 'kind', 'clip')
    expect(choiceFor(state.all, state.own, 'A').kind).toBe('clip')
    expect(choiceFor(state.all, state.own, 'B').kind).toBe('whole')
    expect(sharedChoice(state.all, state.own, ['A', 'B'], 'kind')).toBeUndefined()
    state = setChoice(state.all, state.own, [], 'kind', 'whole')
    expect(state.own).toEqual({})
    state = setChoice(state.all, state.own, ['A'], 'parts', ['THORAX'])
    expect(state.own.A.parts).toEqual(['THORAX'])
  })
})
