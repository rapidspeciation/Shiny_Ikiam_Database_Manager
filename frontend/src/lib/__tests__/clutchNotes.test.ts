import { describe, expect, it } from 'vitest'
import { appendNote, eggGroups, eventNote, expectedNow, gainsOf, predict, readCount, stageDurations, USUAL_DURATIONS, withoutNote } from '../clutches'
import { isoToSerial } from '../dates'
import type { CellValue } from '../types'

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
