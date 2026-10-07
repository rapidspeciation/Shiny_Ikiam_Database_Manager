import { describe, expect, it } from 'vitest'
import { overwriteEdit, overwriting, type Draft } from '../emerged'
import { choose, gapIds, gapOptions, gapSpan, nextFor, nextMany, sameGap, type IdGap } from '../idGaps'
import type { TableRow } from '../types'

// The free pre-made rows: an old gap (S3E–S7E, rows 100–104; S5E held by someone else's card, so not
// in the order), and the latest one after the last row used (C0F–C4F, rows 500–504).
const ROWS: [string, number][] = [
  ['S3E', 100],
  ['S4E', 101],
  ['S6E', 103],
  ['S7E', 104],
  ['C0F', 500],
  ['C1F', 501],
  ['C2F', 502],
  ['C3F', 503],
  ['C4F', 504],
]
const order = ROWS.map(([id]) => id)
const rowOf = new Map(ROWS)
const GAPS: IdGap[] = [
  { from: 'S3E', to: 'S7E', rowFrom: 100, rowTo: 104, free: 4, held: 1, latest: false },
  { from: 'C0F', to: 'C4F', rowFrom: 500, rowTo: 504, free: 5, held: 0, latest: true },
]
const OLD = { rowFrom: 100, rowTo: 104 }

describe('«Siguiente ID»: the gap the buttons take from', () => {
  it('lists the gaps newest first, each with the IDs this person can take from it', () => {
    const options = gapOptions(GAPS, order, rowOf, ['C0F'], null)
    expect(options.map(o => [o.latest, gapSpan(o.ids), o.ids.length])).toEqual([
      [true, 'C1F–C4F', 4],
      [false, 'S3E–S7E', 4],
    ])
  })

  it('by default the buttons go on after the last row used, as always', () => {
    expect(nextFor(order, 'C0F', [], null, rowOf)).toBe('C0F')
    expect(nextFor(order, 'C0F', ['C0F', 'C1F'], null, rowOf)).toBe('C2F')
  })

  it('an older gap: its IDs from its start, in order, skipping those held by others and by the cards', () => {
    expect(gapIds(order, rowOf, OLD)).toEqual(['S3E', 'S4E', 'S6E', 'S7E'])
    expect(nextFor(order, 'C0F', ['C0F'], OLD, rowOf)).toBe('S3E')
    // S5E (someone else's) is skipped.
    expect(nextMany(order, 'C0F', ['C0F'], OLD, rowOf, 3)).toEqual(['S3E', 'S4E', 'S6E'])
    // A card taken away leaves its ID to the next one; an ID typed further on is followed.
    expect(nextFor(order, 'C0F', ['S4E'], OLD, rowOf)).toBe('S6E')
    expect(nextFor(order, 'C0F', ['S4E', 'S6E', 'S7E'], OLD, rowOf)).toBe('S3E')
    // Nothing left in it: none (never one of the latest gap behind the person's back).
    expect(nextMany(order, 'C0F', [], OLD, rowOf, 6)).toEqual(['S3E', 'S4E', 'S6E', 'S7E'])
    expect(nextFor(order, 'C0F', ['S3E', 'S4E', 'S6E', 'S7E'], OLD, rowOf)).toBeNull()
  })

  it('a chosen gap stays chosen as its IDs go, and says when it is full', () => {
    // S3E saved by someone: the gap is now S4E–S7E, still the one chosen.
    const now: IdGap[] = [{ ...GAPS[0], from: 'S4E', rowFrom: 101 }, GAPS[1]]
    expect(sameGap(now[0], OLD)).toBe(true)
    expect(sameGap(now[1], OLD)).toBe(false)
    // All of it taken: listed, empty, while chosen; left out otherwise.
    const full = gapOptions([GAPS[1]], order.slice(4), rowOf, [], OLD)
    expect(full.map(o => [o.rowFrom, o.ids.length])).toEqual([
      [500, 5],
      [100, 0],
    ])
    expect(gapOptions(GAPS, order, rowOf, ['S3E', 'S4E', 'S6E', 'S7E'], null).map(o => o.rowFrom)).toEqual([500])
  })

  it('choosing the latest gap is no choice at all (the buttons as always)', () => {
    const [latest, old] = gapOptions(GAPS, order, rowOf, [], null)
    expect(choose(latest)).toBeNull()
    expect(choose(old)).toEqual(OLD)
    expect(choose(null)).toBeNull()
  })
})

describe('a card on a row with data', () => {
  const draft = (over: Partial<Draft> = {}): Draft => ({
    key: 'k1',
    id: 'S2E',
    clutch: '994',
    date: '2026-10-05',
    kind: 'adult',
    sex: 'male',
    fate: 'alive',
    species: '',
    stage: '',
    foundDead: false,
    note: '',
    cam: '',
    tube: '',
    ...over,
  })
  const row: TableRow = {
    id: 'rec-s2e',
    row: 13704,
    version: 3,
    observed: true,
    formulas: ['Insectary_ID', 'SPECIES', 'Pedigree'],
    values: {
      Insectary_ID: 'S2E',
      SPECIES: 'Mechanitis messenoides',
      Pedigree: 'x',
      'CLUTCH NUMBER': 990,
      Sex: 'female',
      Intro2Insectary_date: 46298,
      Wild_Reared: 'Reared',
      Stock_of_origin: 'NA',
      LIFESTAGE: 'Adult',
      Death_date: 46299,
      Notes_Insectary_data: 'old note',
    },
  }
  const ctx = { clutchValue: 994, clutchSpecies: 'Mechanitis messenoides', generation: '', today: 46300, initials: 'FC', medium: 'Flash frozen' }

  it('is written over only once confirmed for its ID', () => {
    expect(overwriting(draft())).toBe(false)
    expect(overwriting(draft({ overwrite: 'S2E' }))).toBe(true)
    // The ID changed afterwards: asked again.
    expect(overwriting(draft({ id: 'S3E', overwrite: 'S2E' }))).toBe(false)
  })

  it("edits that row: the card's values, the old butterfly's other cells emptied, what was seen as expected", () => {
    const edit = overwriteEdit(draft(), row, ctx)
    expect(edit.id).toBe('rec-s2e')
    expect(edit.values).toMatchObject({ 'CLUTCH NUMBER': 994, Sex: 'male', Intro2Insectary_date: 46300, Death_date: null, Notes_Insectary_data: null })
    // The ID and the row's formulas are left alone; cells already as the card says are not written.
    for (const field of ['Insectary_ID', 'SPECIES', 'Pedigree', 'Wild_Reared', 'Stock_of_origin', 'LIFESTAGE']) expect(edit.values).not.toHaveProperty(field)
    expect(edit.expected).toEqual(Object.fromEntries(Object.keys(edit.values).map(f => [f, row.values[f] ?? null])))
    expect(edit.replaceFormula).toEqual([])
    // Another subspecies typed over the row's SPECIES formula.
    const other = overwriteEdit(draft({ species: 'Mechanitis messenoides deceptus' }), row, ctx)
    expect(other.values.SPECIES).toBe('Mechanitis messenoides deceptus')
    expect(other.replaceFormula).toEqual(['SPECIES'])
  })
})
