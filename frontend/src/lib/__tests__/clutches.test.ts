import { describe, expect, it } from 'vitest'
import {
  appendNote,
  appendTerm,
  changeText,
  clutchNumber,
  clutchOptions,
  clutchState,
  countedToday,
  dayText,
  effectLabel,
  eventText,
  formulaOf,
  gainOf,
  hasClutch,
  hasLosses,
  lossTakesOff,
  nextBatch,
  nextClutch,
  notebookText,
  noteParts,
  notesOf,
  parentsOf,
  parentsText,
  parseIds,
  readCount,
  removeLast,
  REVIEW_ORDER,
  reviewState,
  sameMating,
  stageOfCount,
  termLabels,
  totalOf,
  typedTotal,
  withParents,
  type CountField,
  preservedApart,
  todaySplit,
  eggGroups,
  eventNote,
  expectedNow,
  gainsOf,
  predict,
  stageDurations,
  USUAL_DURATIONS,
  withoutNote,
} from '../clutches'
import { formatSerial, isoToSerial } from '../dates'
import type { CellValue } from '../types'

const TODAY = 46296 // 1-Oct-26

describe('counts kept as sums', () => {
  it('reads the history of a count: formulas, plain numbers, NA and other text', () => {
    expect(readCount('=3+5-2')).toMatchObject({ terms: [3, 5, -2], na: false, text: null })
    expect(readCount('= 12 + 15')).toMatchObject({ terms: [12, 15], na: false, text: null })
    expect(readCount(6)).toMatchObject({ terms: [6], na: false, text: null })
    expect(readCount('=0')).toMatchObject({ terms: [0], na: false, text: null })
    expect(readCount('7')).toMatchObject({ terms: [7], na: false, text: null })
    expect(readCount(null)).toMatchObject({ terms: [], na: false, text: null })
    expect(readCount('NA')).toMatchObject({ terms: [], na: true, text: null })
    expect(readCount('3 pupas; 1 larva')).toMatchObject({ terms: [], na: false, text: '3 pupas; 1 larva' })
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
  it('the next new number is above every number used, batches included, and never one taken', () => {
    // The sheet's end on 1 Oct 2026: batches after the last plain number.
    const sheet = ['985', '986', '992(1)', '994', '994(8)', '1006(3)', '1015', '1012(2)', '1004(3)', '1012(3)', '1016']
    expect(nextClutch(sheet)).toBe('1017')
    expect(nextClutch([...sheet, '1017'])).toBe('1018')
    // A batch beyond the plain numbers still counts: 1017(2) means 1017 is taken.
    expect(nextClutch(['1016', '1017(2)'])).toBe('1018')
    // Out of order (pickup order, not laying order) changes nothing.
    expect(nextClutch(['1016', '985', '986'])).toBe('1017')
    expect(nextClutch([' 1016 ', 'NA', ''])).toBe('1017')
    expect(nextClutch([])).toBe('1')
  })
  it('batches in the current form, N then N(2), N(3)… without a space, also after the older forms', () => {
    expect(nextBatch(1016, ['1016'])).toBe('1016(2)')
    expect(nextBatch(992, ['992(1)', '992(2)', '992(3)'])).toBe('992(4)')
    expect(nextBatch(831, ['831', '831 (2)', '831 (3)'])).toBe('831(4)')
    expect(nextBatch(904, ['904', '904 (2)', '1904'])).toBe('904(3)')
  })
  it('lists each clutch once, newest first, with its next batch, species, generation and parents', () => {
    const rows = [
      { number: '985', species: 'Mechanitis polymnia proceriformis', generation: 'NA', laid: 46250, notes: '29/9/26 FCH: 4 pupas dead' },
      { number: '994', species: 'Mechanitis lysimnia', generation: 'F1', laid: 46270, notes: '29/9/26 FCH: U7A♀ + C7B♂; preserved 14/9' },
      { number: '1004', species: 'Mechanitis polymnia proceriformis', generation: 'F1', laid: 46280, notes: '29/9/26 FCH: Z5A♀ + E9B♂' },
      { number: '994(4)', species: 'Mechanitis lysimnia', generation: 'F1', laid: 46284, notes: '29/9/26 FCH: Preserved (21-sept 26 KG)' },
      { number: '1016', species: 'Mechanitis lysimnia', generation: 'NA', laid: null, notes: null },
    ]
    const options = clutchOptions(rows, rows.map(r => r.number))
    expect(options.map(o => o.next)).toEqual(['1016(2)', '994(5)', '1004(2)', '985(2)'])
    expect(options[1]).toEqual({
      base: 994,
      numbers: ['994', '994(4)'],
      next: '994(5)',
      species: 'Mechanitis lysimnia',
      generation: 'F1',
      // Batch 4's note does not name them: taken from the first batch.
      parents: { female: 'U7A', male: 'C7B' },
      laid: 46284,
    })
    expect(options[0].parents).toBeNull()
    expect(options[0].laid).toBeNull()
  })
  it('parents in NOTES, female first, in the 2026 form and the older one', () => {
    expect(parentsText('u8a', 'C8B ')).toBe('U8A♀ + C8B♂')
    expect(parentsOf('29/9/26 FCH: U8A♀ + C8B♂; preserved 14/9')).toEqual({ female: 'U8A', male: 'C8B', text: 'U8A♀ + C8B♂', index: 13 })
    expect(parentsOf('28/7/26 MJS: F1 clutch parents J7A+ P5A | x')).toEqual({ female: 'J7A', male: 'P5A', text: 'J7A+ P5A', index: 31 })
    expect(parentsOf('Plant with ants')).toBeNull()
    expect(parentsOf(null)).toBeNull()
    const rows = [
      { number: '994', notes: '29/9/26 FCH: U7A♀ + C7B♂' },
      { number: '994(8)', notes: '29/9/26 FCH: U7A♀ + C7B♂' },
      { number: '1006', notes: '29/9/26 FCH: U9B♀ + C9B♂' },
    ]
    expect(sameMating(rows, 'u7a', 'C7B')).toBe('994')
    expect(sameMating(rows, 'C7B', 'U7A')).toBeNull()
  })
  it('parents in the team’s older forms, and counts that are not IDs', () => {
    const pair = (notes: string) => {
      const p = parentsOf(notes)
      return p ? `${p.female}+${p.male}` : null
    }
    expect(pair('1/10/24 MJS: F2 clutch parents 0JF + 9HB')).toBe('0JF+9HB')
    expect(pair('25/4/24 MJS: F1F2 5AA + 3AD')).toBe('5AA+3AD')
    expect(pair('6 JUN 24 KG: F1F2 --> 0AW+6CI')).toBe('0AW+6CI')
    expect(pair('6 JUN 24 KG: F1F2-->8DE+2DA (7 larva were changed to another plant)')).toBe('8DE+2DA')
    expect(pair('2/11/23 MJS: F1/F2 mom 22L+20L | 6/12/23 MJS: Discard this clutch')).toBe('22L+20L')
    expect(pair('7/8/23 MJS: F1-F2 clutch 10B+11B ')).toBe('10B+11B')
    expect(pair('6 JUN 24 KG: F1F2: 91Z--->28Y | 20/06/24 AA: correction -> 91Z + 2BY')).toBe('91Z+2BY')
    expect(pair('29/9/26 FCH: Some eggs with fungus; U7A♀ + C7B♂; preserved 22/9')).toBe('U7A+C7B')
    // The 2026 form wins over an older pair in the same cell.
    expect(pair('F1 clutch parents J7A+ P5A | 1/10/26 FCH: J7B♀ + P5A♂')).toBe('J7B+P5A')
    // Sums of larvae and pupae, generations and dates are not parents.
    for (const notes of [
      '10/1/24 MJS: 1+2+1+10 pupae dead and 2+1 larvae dead',
      '29/9/26 FCH: 1 pupae dead → dark; 2 pupa dead +1',
      '12-3-26 MJS: 8 eggs get dry +2',
      '26-5-26 MJS: 2 larvae + 1 pupae dissected 17/5',
      'F1 + F2 larvae mixed',
      '6 JUN 24 KG: F1F2 --> 6EO +9 EN',
    ])
      expect(parentsOf(notes)).toBeNull()
  })
  it('changing the parents rewrites only the part that names them, in the standard form', () => {
    expect(withParents('29/9/26 FCH: U8A♀ + C8B♂; preserved 14/9', 'u8a', 'C9B', TODAY, 'AA')).toBe('29/9/26 FCH: U8A♀ + C9B♂; preserved 14/9')
    expect(withParents('28/7/26 MJS: F1 clutch parents J7A+ P5A | 29/9/26 FCH: 5 dry eggs', 'J7A', 'P5A', TODAY, 'FCH')).toBe(
      '28/7/26 MJS: F1 clutch parents J7A♀ + P5A♂ | 29/9/26 FCH: 5 dry eggs',
    )
    expect(withParents('6 JUN 24 KG: F1F2 --> 0AW+6CI', '0AW', '7CI', TODAY, 'FCH')).toBe('6 JUN 24 KG: F1F2 --> 0AW♀ + 7CI♂')
    // None written: a new dated note after the others.
    expect(withParents('29/9/26 FCH: 1 dry egg', 'W2B', 'F1B', TODAY, 'FCH')).toBe('29/9/26 FCH: 1 dry egg | 1/10/26 FCH: W2B♀ + F1B♂')
    expect(withParents(null, 'W2B', 'F1B', TODAY, 'FCH')).toBe('1/10/26 FCH: W2B♀ + F1B♂')
    // Written again, the parents are found again (and not added twice).
    const once = withParents('29/9/26 FCH: 1 dry egg', 'W2B', 'F1B', TODAY, 'FCH')
    expect(withParents(once, 'W2B', 'F2B', TODAY, 'FCH')).toBe('29/9/26 FCH: 1 dry egg | 1/10/26 FCH: W2B♀ + F2B♂')
  })
  it('a note shows who wrote it and when apart from its text', () => {
    expect(noteParts('29/9/26 FCH: U8A♀ + C8B♂; preserved 14/9')).toEqual({ head: '29/9/26 FCH', text: 'U8A♀ + C8B♂; preserved 14/9' })
    expect(noteParts('12-3-26 MJS: All larvae dead')).toEqual({ head: '12-3-26 MJS', text: 'All larvae dead' })
    expect(noteParts('6 JUN 24 KG: F1F2 --> 0AW+6CI')).toEqual({ head: '6 JUN 24 KG', text: 'F1F2 --> 0AW+6CI' })
    expect(noteParts('No hatch')).toEqual({ head: '', text: 'No hatch' })
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

describe('daily review marks', () => {
  it('the latest mark of the day counts; to verify comes first', () => {
    expect(reviewState([])).toBe('none')
    expect(reviewState([{ state: 'verify', createdAt: '2026-10-02T15:00:00Z' }])).toBe('verify')
    expect(
      reviewState([
        { state: 'checked', createdAt: '2026-10-02T16:00:00Z' },
        { state: 'verify', createdAt: '2026-10-02T15:00:00Z' },
      ]),
    ).toBe('checked')
    const order = (['checked', 'none', 'verify'] as const).slice().sort((a, b) => REVIEW_ORDER[a] - REVIEW_ORDER[b])
    expect(order).toEqual(['verify', 'none', 'checked'])
  })
})

describe('events and the preserved-larvae convention', () => {
  it('only preserved ones kept counted stay in the count', () => {
    expect(lossTakesOff('died', false)).toBe(true)
    expect(lossTakesOff('disappeared', false)).toBe(true)
    expect(lossTakesOff('preserved', false)).toBe(false)
    expect(lossTakesOff('preserved', true)).toBe(true)
  })
  it('stages, gains and losses', () => {
    expect(stageOfCount('NUMBER OF LARVAE')).toBe('larva')
    expect(stageOfCount('NUMBER OF PUPAE/LARVAE FOR DISECTIONS')).toBe(null)
    expect(gainOf('larva')).toBe('hatched')
    expect(gainOf('pupa')).toBe('pupated')
    expect(hasLosses('larva')).toBe(true)
    expect(hasLosses('adult')).toBe(false)
    expect(hasLosses(null)).toBe(false)
  })
  it('Insectary IDs typed in one box', () => {
    expect(parseIds('h0e, H1E  h2e;H1E w0b.1 x')).toEqual(['H0E', 'H1E', 'H2E', 'W0B.1'])
    expect(parseIds('')).toEqual([])
  })
  it('an event in short', () => {
    const word = (e: { kind: string }) => e.kind
    expect(eventText({ stage: 'larva', kind: 'hatched', count: 4, ids: [], note: null }, word)).toBe('+4 hatched')
    expect(eventText({ stage: 'larva', kind: 'preserved', count: 2, ids: ['M0E', 'N9E'], note: 'life history' }, word)).toBe(
      '−2 preserved (M0E, N9E) · life history',
    )
  })
})

describe("the notebook's list as text", () => {
  it('each clutch with its changes, who and when, then its events', () => {
    const text = notebookText(
      'Clutches',
      [
        {
          recordId: 'r1',
          clutch: '1012',
          species: 'Mechanitis lysimnia',
          isNew: false,
          lines: [
            {
              field: 'NUMBER OF LARVAE',
              before: { formula: '=12' },
              after: { formula: '=12+3' },
              actors: ['Ana Pérez'],
              sources: ['app'],
              firstAt: '2026-10-02T15:00:00Z',
              at: '2026-10-02T15:00:00Z',
            },
          ],
          events: [
            {
              id: 'e1',
              recordId: 'r1',
              clutch: '1012',
              day: '2026-10-02',
              stage: 'larva',
              kind: 'died',
              count: 1,
              ids: [],
              note: null,
              actor: 'u',
              username: 'bob',
              name: 'Bob Díaz',
              actionId: null,
              createdAt: '2026-10-02T16:00:00Z',
            },
          ],
        },
      ],
      { formatDate: formatSerial, when: () => '2/10 10:00', who: n => (n === 'Ana Pérez' ? 'AP' : 'BD'), event: e => `−${e.count} died` },
    )
    expect(text).toBe('Clutches\n\n1012 · Mechanitis lysimnia\n  NUMBER OF LARVAE: +3 (12 → 15) · =12+3  [AP 2/10 10:00]\n  −1 died  [BD 2/10 10:00]')
  })
  it("the day's text lists the events too", () => {
    expect(dayText('Day', [{ clutch: '999', species: '', changes: [], events: ['Larvae: −1 died'] }], formatSerial)).toBe('Day\n\n999\n  Larvae: −1 died')
  })
})

describe("before and after today's changes", () => {
  it("this morning's sum at the start; today's terms after it", () => {
    expect(todaySplit([11, -1, 4], [11, -1, 4, 5, -4])).toEqual({ kept: 3, added: [5, -4], removed: [] })
  })
  it('a term of this morning taken out today', () => {
    expect(todaySplit([11, -1, 4, 5], [11, -1, 5])).toEqual({ kept: 2, added: [5], removed: [4, 5] })
  })
  it('nothing counted this morning: everything is today', () => {
    expect(todaySplit([], [12])).toEqual({ kept: 0, added: [12], removed: [] })
  })
})

describe('a count read with its groups', () => {
  it('one parenthesized sub-sum per group; the terms in order; the total of all', () => {
    const c = readCount('=(6-2)+(5+3)')
    expect(c.groups).toEqual([
      [6, -2],
      [5, 3],
    ])
    expect(c.terms).toEqual([6, -2, 5, 3])
    expect(c.text).toBeNull()
  })
  it('a plain sum or a number is one group; NA and text are not sums', () => {
    expect(readCount('=27-2-11-3').groups).toEqual([[27, -2, -11, -3]])
    expect(readCount(12).groups).toEqual([[12]])
    expect(readCount('NA')).toMatchObject({ na: true, groups: [] })
    expect(readCount('=(6)-(2)')).toMatchObject({ text: '=(6)-(2)', groups: [] })
  })
})

describe('preserved ones kept counted: shown apart from the sum', () => {
  const tallies = {
    larva: { gained: 20, died: 2, disappeared: 0, preserved: 3 },
    egg: { gained: 0, died: 0, disappeared: 0, preserved: 1 },
  }

  it("the team keeps them counted: the stage's preserved ones, beside its sum", () => {
    expect(preservedApart(tallies, 'larva', false)).toBe(3)
    expect(preservedApart(tallies, 'egg', false)).toBe(1)
  })

  it('the team takes them off: they are a − in the sum, nothing apart', () => {
    expect(preservedApart(tallies, 'larva', true)).toBe(0)
  })

  it('none preserved, no events, or no stage (the dissections): nothing apart', () => {
    expect(preservedApart(tallies, 'pupa', false)).toBe(0)
    expect(preservedApart(undefined, 'larva', false)).toBe(0)
    expect(preservedApart(tallies, null, false)).toBe(0)
  })
})

// What a clutch's events write in its NOTES, what should be in the cage today and when the next stages come.

describe('the notes events write', () => {
  const today = isoToSerial('2026-10-05')
  it('each event as the team writes it in NOTES, with the day when it was not today', () => {
    expect(eventNote({ stage: 'larva', kind: 'died', count: 5 }, today)).toBe('5 larvae died')
    expect(eventNote({ stage: 'larva', kind: 'died', count: 1 }, today)).toBe('1 larva died')
    expect(eventNote({ stage: 'larva', kind: 'disappeared', count: 5 }, today)).toBe('5 larvae disappeared')
    expect(eventNote({ stage: 'larva', kind: 'preserved', count: 5, lifestage: '3rd instar larva' }, today)).toBe('5 larvae preserved as 3rd instar')
    expect(eventNote({ stage: 'larva', kind: 'preserved', count: 2, lifestage: '3rd instar larva', ids: ['R0C', 'R1C'] }, today)).toBe(
      '2 larvae preserved as 3rd instar (R0C, R1C)',
    )
    expect(eventNote({ stage: 'larva', kind: 'preserved', count: 1, lifestage: 'Pre-pupa' }, today)).toBe('1 larva preserved as prepupa')
    expect(eventNote({ stage: 'larva', kind: 'hatched', count: 3 }, today)).toBe('3 larvae hatched')
    expect(eventNote({ stage: 'pupa', kind: 'pupated', count: 3 }, today)).toBe('3 pupated')
    expect(eventNote({ stage: 'adult', kind: 'emerged', count: 4 }, today)).toBe('4 adults emerged')
    expect(eventNote({ stage: 'egg', kind: 'laid', count: 7 }, today)).toBe('7 eggs laid')
    expect(eventNote({ stage: 'egg', kind: 'died', count: 2 }, today)).toBe('2 eggs died')
    expect(eventNote({ stage: 'pupa', kind: 'died', count: 1 }, today)).toBe('1 pupa died')
    expect(eventNote({ stage: 'egg', kind: 'laid', count: 5, day: today - 1 }, today)).toBe('5 eggs laid on 4/10/26')
    // In NOTES: dated and signed by the person writing it.
    expect(appendNote('Some eggs dry', eventNote({ stage: 'larva', kind: 'died', count: 5 }, today), today, 'FCH')).toBe(
      'Some eggs dry | 5/10/26 FCH: 5 larvae died',
    )
  })
  it('a note taken back with its event: only that one, the others as written', () => {
    const notes = 'Old | 5/10/26 FCH: 5 larvae died | 5/10/26 FCH: 2 larvae disappeared'
    expect(withoutNote(notes, '5/10/26 FCH: 5 larvae died')).toBe('Old | 5/10/26 FCH: 2 larvae disappeared')
    expect(withoutNote(notes, '5/10/26 FCH: 2 larvae disappeared')).toBe('Old | 5/10/26 FCH: 5 larvae died')
    expect(withoutNote('5/10/26 FCH: 3 pupated', '5/10/26 FCH: 3 pupated')).toBe(null)
    expect(withoutNote('Old', 'not there')).toBe('Old')
  })
})

describe('what is expected in the cage, and when', () => {
  const c = (v: CellValue) => readCount(v)
  it('the counting convention: 20 larvae, 10 preserved, 5 pupated, 5 died → NUMBER OF LARVAE 15, none left to count', () => {
    // Preserved stay counted (the default): only the 5 dead are taken off.
    const kept = expectedNow({ eggs: c('=20'), larvae: c('=20-5'), pupae: c('=5'), adults: c(null) }, { larva: 10 }, false)
    expect(kept).toEqual({ eggs: 0, larvae: 0, pupae: 5 })
    // Preserved taken off (the other setting): the same cage.
    expect(expectedNow({ eggs: c('=20'), larvae: c('=20-5-10'), pupae: c('=5'), adults: c(null) }, { larva: 10 }, true)).toEqual(kept)
    // Groups laid on different days, some hatched: eggs left, larvae to count today.
    expect(expectedNow({ eggs: c('=3+5+7'), larvae: c('=3+5-1'), pupae: c(null), adults: c(null) }, { larva: 2 }, false)).toEqual({
      eggs: 7,
      larvae: 5,
      pupae: null,
    })
    expect(gainsOf([12, 5, -2, 3])).toBe(20)
  })
  it("each species' days per stage from the sheet's clutches; its genus' or all clutches' with too few", () => {
    const rows = [
      ...Array.from({ length: 5 }, (_, i) => ({ species: 'Melinaea mothone mothone', laid: 46000 + i, hatch: 46003 + i, pupa: 46020 + i, emerge: 46030 + i })),
      ...Array.from({ length: 6 }, () => ({ species: 'Mechanitis lysimnia', laid: 46000, hatch: 46005, pupa: 46020, emerge: 46028 })),
      // A typo (300 days) is left out; an NA species counts only for all clutches.
      { species: 'Mechanitis lysimnia', laid: 46000, hatch: 46300, pupa: null, emerge: null },
      { species: 'NA', laid: 46000, hatch: 46004, pupa: null, emerge: null },
    ]
    const d = stageDurations(rows)
    expect(d.of('Melinaea mothone mothone')).toEqual({ egg: 3, larva: 17, pupa: 10, from: 'species' })
    expect(d.of('Mechanitis lysimnia')).toEqual({ egg: 5, larva: 15, pupa: 8, from: 'species' })
    expect(d.of('Mechanitis polymnia proceriformis')).toEqual({ egg: 5, larva: 15, pupa: 8, from: 'genus' })
    expect(d.of('Ithomia salapia')).toMatchObject({ from: 'all' })
    expect(stageDurations([]).of('x')).toEqual(USUAL_DURATIONS)
  })
  it('predicted dates: hatching, pupation and emergence from the latest day of the stage before', () => {
    const days = { egg: 5, larva: 16, pupa: 8, from: 'species' as const }
    const laid = isoToSerial('2026-10-01')
    // Only eggs: all three ahead.
    expect(predict({ laid, hatch: null, pupa: null }, { eggs: 15, larvae: null, pupae: null }, days)).toEqual({
      hatch: laid + 5,
      pupa: laid + 21,
      emerge: laid + 29,
    })
    // A second group laid later: its hatching follows it.
    expect(predict({ laid, hatch: null, pupa: null }, { eggs: 15, larvae: null, pupae: null }, days, { laid: laid + 2 }).hatch).toBe(laid + 7)
    // All hatched, larvae alive: no hatching; pupation from the latest hatching.
    expect(predict({ laid, hatch: laid + 5, pupa: null }, { eggs: 0, larvae: 8, pupae: null }, days, { hatched: laid + 6 })).toEqual({
      hatch: null,
      pupa: laid + 22,
      emerge: laid + 30,
    })
    // Long overdue (eggs that dried): no hatching date; one a day late is still given.
    expect(predict({ laid, hatch: null, pupa: null }, { eggs: 5, larvae: null, pupae: null }, days, {}, laid + 30).hatch).toBe(null)
    expect(predict({ laid, hatch: null, pupa: null }, { eggs: 5, larvae: null, pupae: null }, days, {}, laid + 6).hatch).toBe(laid + 5)
    // Nothing left: no dates.
    expect(predict({ laid, hatch: laid + 5, pupa: laid + 21 }, { eggs: 0, larvae: 0, pupae: 0 }, days)).toEqual({ hatch: null, pupa: null, emerge: null })
  })
  it('eggs typed as groups', () => {
    expect(eggGroups('12')).toEqual([12])
    expect(eggGroups('3+5+7')).toEqual([3, 5, 7])
    expect(eggGroups(' =3 + 5 ')).toEqual([3, 5])
    expect(eggGroups('')).toEqual([])
    expect(eggGroups('3-1')).toBe(null)
    expect(eggGroups('abc')).toBe(null)
  })
})
