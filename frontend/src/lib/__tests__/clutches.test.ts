import { describe, expect, it } from 'vitest'
import {
  appendNote,
  appendTerm,
  changeText,
  clutchNumber,
  clutchState,
  countedToday,
  dayText,
  effectLabel,
  formulaOf,
  hasClutch,
  nextBatch,
  nextClutch,
  notesOf,
  parentsOf,
  parentsText,
  readCount,
  removeLast,
  sameMating,
  termLabels,
  totalOf,
  typedTotal,
  type CountField,
} from '../clutches'
import { formatSerial } from '../dates'
import type { CellValue } from '../types'

const TODAY = 46296 // 1-Oct-26

describe('counts kept as sums', () => {
  it('reads the history of a count: formulas, plain numbers, NA and other text', () => {
    expect(readCount('=3+5-2')).toEqual({ terms: [3, 5, -2], na: false, text: null })
    expect(readCount('= 12 + 15')).toEqual({ terms: [12, 15], na: false, text: null })
    expect(readCount(6)).toEqual({ terms: [6], na: false, text: null })
    expect(readCount('=0')).toEqual({ terms: [0], na: false, text: null })
    expect(readCount('7')).toEqual({ terms: [7], na: false, text: null })
    expect(readCount(null)).toEqual({ terms: [], na: false, text: null })
    expect(readCount('NA')).toEqual({ terms: [], na: true, text: null })
    expect(readCount('3 pupas; 1 larva')).toEqual({ terms: [], na: false, text: '3 pupas; 1 larva' })
    expect(readCount('=A2+1').text).toBe('=A2+1')
  })
  it('writes the terms back as the team types them, and shows them as chips', () => {
    expect(formulaOf([3, 5, -2])).toBe('=3+5-2')
    expect(formulaOf([6])).toBe('=6')
    expect(formulaOf([])).toBeNull()
    expect(termLabels([3, 5, -2])).toEqual(['3', '+5', '−2'])
    expect(totalOf([3, 5, -2])).toBe(6)
  })
  it('+N and −N add a term; a lone 0 is replaced; never below 0, never a loss first', () => {
    expect(appendTerm([3, 5, -2], 4)).toEqual({ ok: true, terms: [3, 5, -2, 4] })
    expect(appendTerm([3, 5, -2], -3)).toEqual({ ok: true, terms: [3, 5, -2, -3] })
    expect(appendTerm([], 3)).toEqual({ ok: true, terms: [3] })
    expect(appendTerm([0], 3)).toEqual({ ok: true, terms: [3] })
    expect(appendTerm([3], -4)).toEqual({ ok: false, reason: 'negative' })
    expect(appendTerm([], -1)).toEqual({ ok: false, reason: 'first' })
    expect(appendTerm([3], 0)).toEqual({ ok: false, reason: 'empty' })
  })
  it('"Counted today" adds the difference to the total, or nothing when it is the same', () => {
    // =3+5-2 is 6; 3 counted today → −3.
    expect(countedToday([3, 5, -2], 3)).toEqual({ ok: true, terms: [3, 5, -2, -3] })
    expect(countedToday([3, 5, -2], 8)).toEqual({ ok: true, terms: [3, 5, -2, 2] })
    expect(countedToday([3, 5, -2], 6)).toEqual({ ok: false, reason: 'unchanged' })
    expect(countedToday([], 3)).toEqual({ ok: true, terms: [3] })
    expect(countedToday([0], 5)).toEqual({ ok: true, terms: [5] })
    expect(countedToday([3, 5, -2], 0)).toEqual({ ok: true, terms: [3, 5, -2, -6] })
    expect(countedToday([4], -1)).toEqual({ ok: false, reason: 'empty' })
  })
  it('typing a new total over the total adds the difference to the sum, as "Counted today"', () => {
    // =14+12+6 is 32: typing 30 appends −2, 35 appends +3, 32 changes nothing.
    const sum = [14, 12, 6]
    const down = typedTotal(sum, '30')
    expect(down).toEqual({ ok: true, terms: [14, 12, 6, -2] })
    expect(down.ok && formulaOf(down.terms)).toBe('=14+12+6-2')
    expect(effectLabel(sum, down)).toBe('−2')
    const up = typedTotal(sum, ' 35 ')
    expect(up).toEqual({ ok: true, terms: [14, 12, 6, 3] })
    expect(effectLabel(sum, up)).toBe('+3')
    expect(typedTotal(sum, '32')).toEqual({ ok: false, reason: 'unchanged' })
    expect(effectLabel(sum, typedTotal(sum, '32'))).toBe('')
    expect(typedTotal(sum, '0')).toEqual({ ok: true, terms: [14, 12, 6, -32] })
    // An empty count (or a lone 0) starts at the number typed.
    expect(typedTotal([], '12')).toEqual({ ok: true, terms: [12] })
    expect(effectLabel([], typedTotal([], '12'))).toBe('= 12')
    expect(effectLabel([0], typedTotal([0], '4'))).toBe('= 4')
    for (const bad of ['', '-2', '3.5', 'abc', '12345'])
      expect(typedTotal(sum, bad), bad).toEqual({ ok: false, reason: 'empty' })
  })
  it('"remove last term" takes back yesterday\'s −3 when the 3 turn up again', () => {
    const yesterday = countedToday([3, 5, -2], 3)
    expect(yesterday.ok && formulaOf(yesterday.terms)).toBe('=3+5-2-3')
    expect(formulaOf(removeLast([3, 5, -2, -3]))).toBe('=3+5-2')
    expect(formulaOf(removeLast([3]))).toBeNull()
  })
})

describe('which clutches are still going', () => {
  const state = (values: Record<string, CellValue>, opts?: { undated?: boolean }) =>
    clutchState(
      f => values[f] ?? null,
      (f: CountField) => readCount(values[f]),
      TODAY,
      opts,
    )
  it('eggs, larvae and pupae of a recent clutch: going, at its latest stage', () => {
    expect(state({ 'DATE LAID': 46290, 'NUMBER OF EGGS': '=7+6' })).toEqual({ stage: 'egg', start: 46290, ended: null })
    const pupae = state({ 'DATE LAID': 46271, 'NUMBER OF EGGS': '=17+23', 'NUMBER OF LARVAE': '=12+5+18-10', 'PUPA DATE': 46292, 'NUMBER OF PUPA': '=19' })
    expect(pupae).toEqual({ stage: 'pupa', start: 46271, ended: null })
    // Emerging: 3 of 7 pupae out, the first a week ago.
    expect(state({ 'DATE LAID': 46265, 'NUMBER OF PUPA': '=7', 'EMERGENCE DATE': 46289, 'NUMBER OF ADULTS': '=3' }).ended).toBeNull()
  })
  it('ended: a stage that never came, nothing left, all emerged, a note, or too old', () => {
    expect(state({ 'DATE LAID': 46275, 'NUMBER OF EGGS': '=10', 'NUMBER OF LARVAE': 'NA' }).ended).toBe('never')
    expect(state({ 'DATE LAID': 46256, 'NUMBER OF EGGS': '=5+5', 'NUMBER OF LARVAE': '=0' }).ended).toBe('none-left')
    expect(state({ 'DATE LAID': 46257, 'NUMBER OF LARVAE': '=16+5-5', 'NUMBER OF PUPA': '=0', 'NUMBER OF ADULTS': '=0' }).ended).toBe('none-left')
    expect(state({ 'DATE LAID': 46270, 'NUMBER OF PUPA': '=2', 'EMERGENCE DATE': 46290, 'NUMBER OF ADULTS': '=1+1' }).ended).toBe('emerged')
    expect(state({ 'DATE LAID': 46254, 'NUMBER OF PUPA': '=42', 'EMERGENCE DATE': 46274, 'NUMBER OF ADULTS': '=39' }).ended).toBe('emerged')
    expect(state({ 'DATE LAID': 46280, 'NUMBER OF LARVAE': '=17', NOTES: '29/9/26 FCH: clutch is dead; 17 larvae preserved' }).ended).toBe('note')
    expect(state({ 'DATE LAID': 46100, 'NUMBER OF EGGS': '=12' }).ended).toBe('old')
  })
  it('field larvae without a laying date start at their first date; undated rows only at the end of the sheet', () => {
    expect(state({ 'DATE LAID': 'NA', 'NUMBER OF LARVAE': '=24+38', 'PUPA DATE': 46282 }).start).toBe(46282)
    expect(state({ 'NUMBER OF EGGS': '=5' }).ended).toBe('old')
    expect(state({ 'NUMBER OF EGGS': '=5' }, { undated: true }).ended).toBeNull()
  })
  it('a row with only its clutch number is not a clutch yet', () => {
    expect(hasClutch(f => ({ 'CLUTCH NUMBER': 989 })[f] ?? null)).toBe(false)
    expect(hasClutch(f => ({ 'CLUTCH NUMBER': 996, SPECIES: 'NA' })[f] ?? null)).toBe(true)
  })
})

describe('clutch numbers and parents', () => {
  it('numbers and batches', () => {
    expect(clutchNumber('994(2)')).toEqual({ base: 994, batch: 2 })
    expect(clutchNumber('831 (3)')).toEqual({ base: 831, batch: 3 })
    expect(clutchNumber('1016')).toEqual({ base: 1016, batch: 1 })
    expect(clutchNumber('994(F1)')).toBeNull()
    expect(nextClutch(['1015', '1012(3)', '1016', '446W'])).toBe('1017')
    expect(nextBatch(994, ['994', '994(2)', '994(8)', '1004'])).toBe('994(9)')
    expect(nextBatch(1017, ['994'])).toBe('1017')
  })
  it('parents in NOTES, female first, in the 2026 form and the older one', () => {
    expect(parentsText('u8a', 'C8B ')).toBe('U8A♀ + C8B♂')
    expect(parentsOf('29/9/26 FCH: U8A♀ + C8B♂; preserved 14/9')).toEqual({ female: 'U8A', male: 'C8B', text: 'U8A♀ + C8B♂' })
    expect(parentsOf('28/7/26 MJS: F1 clutch parents J7A+ P5A | x')?.male).toBe('P5A')
    expect(parentsOf('Plant with ants')).toBeNull()
    const rows = [
      { number: '994', notes: '29/9/26 FCH: U7A♀ + C7B♂' },
      { number: '994(8)', notes: '29/9/26 FCH: U7A♀ + C7B♂' },
      { number: '1006', notes: '29/9/26 FCH: U9B♀ + C9B♂' },
    ]
    expect(sameMating(rows, 'u7a', 'C7B')).toBe('994')
    expect(sameMating(rows, 'C7B', 'U7A')).toBeNull()
  })
  it('notes are dated and initialled, added after the old ones', () => {
    expect(appendNote(null, '3 larvae dead', TODAY, 'FCH')).toBe('1/10/26 FCH: 3 larvae dead')
    expect(appendNote('29/9/26 FCH: 1 dry egg', 'Plant changed', TODAY, 'AA')).toBe('29/9/26 FCH: 1 dry egg | 1/10/26 AA: Plant changed')
    expect(notesOf('a | b |c')).toEqual(['a', 'b |c'])
  })
})

describe("the day's changes as text for the notebook", () => {
  it('counts as sums with totals, dates day first, notes as what was added', () => {
    expect(changeText({ field: 'NUMBER OF LARVAE', before: { formula: '=12+13' }, after: { formula: '=12+13-3' } }, formatSerial)).toBe(
      '−3 (25 → 22) · =12+13-3',
    )
    expect(changeText({ field: 'NUMBER OF LARVAE', before: { formula: '=3+5-2-3' }, after: { formula: '=3+5-2' } }, formatSerial)).toBe(
      'removed −3 (3 → 6) · =3+5-2',
    )
    expect(changeText({ field: 'NUMBER OF EGGS', before: 12, after: { formula: '=12+4+1' } }, formatSerial)).toBe('+4 +1 (12 → 17) · =12+4+1')
    expect(changeText({ field: 'NUMBER OF LARVAE', before: { formula: '=12+13' }, after: { formula: '=25' } }, formatSerial)).toBe(
      '=12+13 (25) → =25',
    )
    expect(changeText({ field: 'NUMBER OF PUPA', before: null, after: { formula: '=2' } }, formatSerial)).toBe('— → =2')
    expect(changeText({ field: 'PUPA DATE', before: null, after: TODAY }, formatSerial)).toBe('— → 1-Oct-26')
    expect(changeText({ field: 'NOTES', before: '29/9/26 FCH: x', after: '29/9/26 FCH: x | 1/10/26 FCH: 3 larvae dead' }, formatSerial)).toBe(
      '+ 1/10/26 FCH: 3 larvae dead',
    )
    const text = dayText(
      'Clutches 1/10/26',
      [{ clutch: '1012(2)', species: 'Mechanitis lysimnia', changes: [{ field: 'NUMBER OF EGGS', before: { formula: '=17' }, after: { formula: '=17+4' } }] }],
      formatSerial,
    )
    expect(text).toBe('Clutches 1/10/26\n\n1012(2) · Mechanitis lysimnia\n  NUMBER OF EGGS: +4 (17 → 21) · =17+4')
  })
})
