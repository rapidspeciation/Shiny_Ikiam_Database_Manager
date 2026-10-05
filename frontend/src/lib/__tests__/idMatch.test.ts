import { describe, expect, it } from 'vitest'
import { idItems, lookAlikeTable, matchIds, parsePattern, sexOf, type Matchable } from '../idMatch'

interface B extends Matchable {
  id: string
  species: string
  sex: string
  alive: boolean
}
const POLY = 'Mechanitis polymnia proceriformis'
const LYS = 'Mechanitis lysimnia'
let order = 0
const b = (id: string, species = POLY, sex = 'female', alive = true): B => ({ id, key: id, order: ++order, species, sex, alive })
const run = (items: B[], typed: string, opts: Partial<Parameters<typeof matchIds<B>>[2]> = {}) =>
  matchIds(items, typed, { alive: x => x.alive, speciesOf: x => x.species, sexOf: x => x.sex, limit: 20, ...opts })
const ids = (items: B[], typed: string, opts = {}) => run(items, typed, opts).map(m => m.item.id)

describe('parsePattern', () => {
  it('reads wildcards, alternatives in brackets (with or without /), upper case, spaces gone', () => {
    expect(parsePattern('a1 b')).toEqual({
      slots: [
        { any: false, chars: ['A'] },
        { any: false, chars: ['1'] },
        { any: false, chars: ['B'] },
      ],
      literal: true,
      given: 3,
    })
    const p = parsePattern('A?[b/d]*')
    expect(p.literal).toBe(false)
    expect(p.given).toBe(2)
    expect(p.slots[1]).toEqual({ any: true })
    expect(p.slots[2]).toEqual({ any: false, chars: ['B', 'D'] })
    expect(p.slots[3]).toEqual({ any: true })
    // An unfinished bracket still counts as one position; an empty one as any.
    expect(parsePattern('A1[B').slots[2]).toEqual({ any: false, chars: ['B'] })
    expect(parsePattern('A1[]').slots[2]).toEqual({ any: true })
  })
})

describe('matchIds', () => {
  const items = [b('A1B'), b('A1D'), b('A7B'), b('A8B'), b('A6B'), b('A6D'), b('W2B'), b('W2B.1'), b('C3E')]

  it('exact first, then wildcard or alternatives, then IDs that start with it, then look-alikes (one, then two)', () => {
    const m = run(items, 'A1B')
    expect(m[0]).toMatchObject({ item: { id: 'A1B' }, kind: 'exact', tier: 0, at: [] })
    // 1↔7 and B↔D: one look-alike each, before two.
    expect(m.slice(1, 3).map(x => x.item.id).sort()).toEqual(['A1D', 'A7B'])
    expect(m.find(x => x.item.id === 'A7B')).toMatchObject({ kind: 'lookalike', at: [1], tier: 3 })
    // A7D needs two (absent here); A8B is not a look-alike of 1.
    expect(ids(items, 'A1B')).not.toContain('A8B')
    expect(ids(items, 'W2B')).toEqual(['W2B', 'W2B.1'])
    expect(run(items, 'W2B')[1].kind).toBe('prefix')
  })

  it('a position that cannot be read: * or ?, and alternatives [BD] or [B/D]', () => {
    // Every ID the wildcard fits (newest first), then look-alikes of the rest (B↔D).
    expect(ids(items, 'A?B').slice(0, 4)).toEqual(['A6B', 'A8B', 'A7B', 'A1B'])
    expect(ids(items, 'A?B').slice(4)).toEqual(['A6D', 'A1D'])
    expect(run(items, 'A*B')[0].kind).toBe('pattern')
    // Both alternatives, then a look-alike of them (A8B: 6↔8).
    expect(ids(items, 'A6[BD]')).toEqual(['A6D', 'A6B', 'A8B'])
    expect(ids(items, 'A6[B/D]')).toEqual(['A6D', 'A6B', 'A8B'])
    expect(run(items, 'A6[BD]')[2]).toMatchObject({ kind: 'lookalike', at: [1] })
    expect(ids(items, 'A6B')).toEqual(['A6B', 'A6D', 'A8B'])
    // Wildcards and look-alikes together.
    expect(ids(items, '?6B')).toEqual(['A6B', 'A6D', 'A8B'])
  })

  it('look-alike pairs of the table both ways, extra learned pairs, at most two', () => {
    const set = [b('O8E'), b('08E'), b('Q8E'), b('D8E'), b('O3E'), b('OBF')]
    // O typed: 0, Q and D look like it; 8 typed: 3 and B; E typed: F.
    expect(ids(set, 'O8E')[0]).toBe('O8E')
    expect(ids(set, 'O8E').slice(1).sort()).toEqual(['08E', 'D8E', 'O3E', 'OBF', 'Q8E'])
    expect(run(set, 'O8E').find(m => m.item.id === 'OBF')).toMatchObject({ at: [1, 2], tier: 5 })
    const learned = lookAlikeTable([['K', 'X']])
    expect(ids([b('A1K')], 'A1X', { table: learned })).toEqual(['A1K'])
    expect(ids([b('A1K')], 'A1X')).toEqual([])
    // Three characters read as look-alikes is too far.
    expect(ids([b('D7D')], 'B1B')).toEqual([])
  })

  it('no look-alikes for an ID still being typed (A1 does not offer A7…), only its continuations', () => {
    expect(ids(items, 'A1')).toEqual(['A1D', 'A1B'])
    expect(ids(items, 'A')).toHaveLength(6)
  })

  it('within a step: alive first, then the species given, then the sex given, then the newest; other species marked', () => {
    const set = [
      b('A1B', LYS, 'female'),
      b('A1D', POLY, 'male'),
      b('A7B', POLY, 'female'),
      b('A1F', POLY, 'female', false),
    ]
    // One look-alike each: A1D (B↔D), A7B (1↔7), A1F (dead, and E↔F is not B).
    const m = run(set, 'A1B', { species: POLY, sex: 'female' })
    expect(m.map(x => x.item.id)).toEqual(['A1B', 'A7B', 'A1D'])
    expect(m[0].sameSpecies).toBe(false)
    expect(m[1]).toMatchObject({ sameSpecies: true, sameSex: true })
    expect(m[2]).toMatchObject({ sameSpecies: true, sameSex: false })
    // Without a sex given, the newest first among equals.
    expect(ids(set, 'A1B', { species: POLY }).slice(1)).toEqual(['A7B', 'A1D'])
    expect(ids(set, 'A?B', { species: POLY })).toEqual(['A7B', 'A1B', 'A1D'])
  })

  it('a butterfly recorded dead ranks below a living one that starts with the text, above look-alikes', () => {
    const set = [b('B9', POLY, 'male', false), b('B9D'), b('B7D')]
    expect(ids(set, 'B9')).toEqual(['B9D', 'B9'])
    const dead = [b('A6B', POLY, 'male', false), b('A8B'), b('A6BX')]
    expect(ids(dead, 'A6B')).toEqual(['A6BX', 'A6B', 'A8B'])
    expect(run(dead, 'A6B')[1]).toMatchObject({ kind: 'exact', alive: false })
  })

  it('skips the chosen ones, keeps to the limit, and matches plain IDs (the table picker)', () => {
    expect(ids(items, 'A1B', { skip: (x: B) => x.id === 'A1B' })[0]).not.toBe('A1B')
    expect(run(items, 'A', { limit: 2 })).toHaveLength(2)
    const plain = idItems(['A1E', 'A1D', 'A7D'])
    expect(matchIds(plain, 'A1D').map(m => m.item.id)).toEqual(['A1D', 'A7D'])
    expect(matchIds(plain, '').length).toBe(0)
    expect(matchIds(plain, '***').length).toBe(0)
  })

  it('reads the sex filter as the sheet writes it', () => {
    expect(sexOf('Female ')).toBe('female')
    expect(sexOf('male')).toBe('male')
    expect(sexOf('NA')).toBe('unknown')
    expect(sexOf(null)).toBe('unknown')
  })
})
