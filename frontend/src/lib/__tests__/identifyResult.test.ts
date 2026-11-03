import { describe, expect, it } from 'vitest'
import { groupByGenus, headline, isIthomiini, percent, rank, stockGaps } from '../identifyResult'

const species = ['Mechanitis messenoides', 'Mechanitis polymnia', 'Melinaea menophilus', 'Oleria onega', 'Mechanitis lysimnia']

describe('result wording', () => {
  it('ranks species and sums every genus over all its species', () => {
    const { species: top, genus } = rank([0.5, 0.3, 0.1, 0.04, 0.06], species, 3)
    expect(top.map(c => c.name)).toEqual(['Mechanitis messenoides', 'Mechanitis polymnia', 'Melinaea menophilus'])
    expect(genus.get('Mechanitis')).toBeCloseTo(0.86)
    const { groups, rest } = groupByGenus(top, genus)
    expect(groups.map(g => [g.genus, g.species.length])).toEqual([
      ['Mechanitis', 2],
      ['Melinaea', 1],
    ])
    // Oleria (0.04) is not among the candidates shown: it is in «otros géneros».
    expect(rest).toBeCloseTo(0.04)
  })
  it('names the species only when its score is clear, else the genus when that is, else not sure', () => {
    const genus = new Map([
      ['Mechanitis', 0.9],
      ['Oleria', 0.4],
    ])
    expect(headline({ name: 'Mechanitis polymnia', p: 0.85 }, genus)).toEqual({ kind: 'species', name: 'Mechanitis polymnia' })
    expect(headline({ name: 'Mechanitis polymnia', p: 0.5 }, genus)).toEqual({ kind: 'genus', genus: 'Mechanitis' })
    expect(headline({ name: 'Oleria onega', p: 0.3 }, genus)).toEqual({ kind: 'unsure' })
    expect(headline(undefined, genus)).toEqual({ kind: 'unsure' })
  })
  it('percent and the Ithomiini genera', () => {
    expect(percent(0.784)).toBe('78 %')
    expect(percent(0.001)).toBe('<1 %')
    expect(isIthomiini('Oleria onega', ['Oleria'])).toBe(true)
    expect(isIthomiini('Heliconius erato', ['Oleria'])).toBe(false)
    expect(isIthomiini('Oleria onega', [])).toBe(null)
  })
})

describe('reference-genome gaps (3 ♂ + 3 ♀ per side)', () => {
  it('per species: the recorded sides, what is missing, specimens without a side apart', () => {
    const g = stockGaps({ '': { East: { M: 5, F: 3, unknown: 1 }, other: { M: 1, F: 0, unknown: 0 } } }, { East: true, West: true })
    expect(g.bySubspecies).toBe(false)
    expect(g.rows.map(r => [r.side, r.M, r.F, r.missingM, r.missingF])).toEqual([
      ['East', 5, 3, 0, 0],
      ['West', 0, 0, 3, 3],
    ])
    expect(g.missing).toEqual([{ side: 'West', M: 3, F: 3, short: [{ subspecies: '', M: 3, F: 3 }], unassigned: 0 }])
    expect(g.apart).toEqual([{ label: 'sin-lado', M: 1, F: 0, unknown: 0 }])
  })
  it('per subspecies when the inventory names any; none picked; a recorded side without stock still shows', () => {
    const g = stockGaps(
      {
        proceriformis: { East: { M: 3, F: 1, unknown: 0 } },
        eurydice: { East: { M: 0, F: 4, unknown: 0 } },
        '': { East: { M: 2, F: 0, unknown: 0 } },
      },
      { East: true, West: true },
    )
    expect(g.bySubspecies).toBe(true)
    expect(g.rows.map(r => [r.subspecies, r.side, r.missingM, r.missingF])).toEqual([
      ['eurydice', 'East', 3, 0],
      ['proceriformis', 'East', 0, 2],
      ['', 'West', 3, 3],
    ])
    expect(g.missing).toEqual([
      {
        side: 'East',
        M: 3,
        F: 2,
        short: [
          { subspecies: 'eurydice', M: 3, F: 0 },
          { subspecies: 'proceriformis', M: 0, F: 2 },
        ],
        unassigned: 2,
      },
      { side: 'West', M: 3, F: 3, short: [{ subspecies: '', M: 3, F: 3 }], unassigned: 0 },
    ])
    expect(g.apart).toEqual([{ label: 'sin-subespecie', side: 'East', M: 2, F: 0, unknown: 0 }])
  })
  it('a species with no stock and no record: nothing to set against', () => {
    expect(stockGaps(undefined, undefined)).toEqual({ rows: [], apart: [], missing: [], bySubspecies: false })
  })
})

describe('photo credits', async () => {
  const { shortCredit } = await import('../identifyRefs')
  it('keeps the author of an iNaturalist attribution', () => {
    expect(shortCredit('(c) alessandradalia, some rights reserved (CC BY-SA), uploaded by alessandradalia')).toBe('© alessandradalia')
    expect(shortCredit('Ana Pérez')).toBe('Ana Pérez')
  })
})
