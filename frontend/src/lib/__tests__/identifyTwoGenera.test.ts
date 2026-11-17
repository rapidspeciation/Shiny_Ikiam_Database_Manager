import { describe, expect, it } from 'vitest'
import { genusPair, rank, stripNames, THRESHOLDS, type Candidate, type Cuts, type Headline } from '../identifyResult'

// The genus not clear (17 Nov 2026, .work/two-genera-20261117/PREREG_two_genera.md): two genera named, each with its
// best species and its total, when the top two totals sum to at least cuts.pair and the second is at least cuts.second;
// else today's line: the top species, its genus «Incierto» and the clear tribe's row (the sum low, or the second small).
const tribe: Headline = { kind: 'tribe', name: 'Ithomiini', p: 0.96 }
const cuts: Cuts = { species: 0.7, genus: 0.8, higher: 0.8, pair: 0.7, second: 0.15 }
const c = (name: string, p: number): Candidate => ({ name, p })
const g = (...pairs: [string, number][]) => new Map(pairs)

describe('genusPair', () => {
  // The screenshot's photo (9981 lab, 16 Nov 2026): M. messenoides 40 %, Melinaea isocomma 37 %, M. menophilus 5 %.
  const species = [c('Mechanitis messenoides', 0.4), c('Melinaea isocomma', 0.37), c('Melinaea menophilus', 0.05), c('Mechanitis polymnia', 0.04)]
  const genus = g(['Mechanitis', 0.47], ['Melinaea', 0.43], ['Oleria', 0.05])

  it('names the two genera, each with its best species and its total, the higher first', () => {
    const p = genusPair(tribe, species[0], genus, species, cuts)!
    expect(p.map(x => [x.genus, x.p, x.species?.name])).toEqual([
      ['Mechanitis', 0.47, 'Mechanitis messenoides'],
      ['Melinaea', 0.43, 'Melinaea isocomma'],
    ])
    // the same under «not sure» and under a clear subfamily
    expect(genusPair({ kind: 'unsure' }, species[0], genus, species, cuts)).not.toBeNull()
    expect(genusPair({ kind: 'subfamily', name: 'Danainae', p: 0.9 }, species[0], genus, species, cuts)).not.toBeNull()
  })

  it('never when the species or the genus is clear (those answers are unchanged)', () => {
    expect(genusPair({ kind: 'species', name: 'Mechanitis messenoides' }, species[0], genus, species, cuts)).toBeNull()
    expect(genusPair({ kind: 'genus', genus: 'Mechanitis' }, species[0], genus, species, cuts)).toBeNull()
    expect(genusPair(null, species[0], genus, species, cuts)).toBeNull()
    expect(genusPair(tribe, undefined, genus, species, cuts)).toBeNull()
  })

  it('today\'s line when the two totals sum to less than cuts.pair (at the cut: the pair)', () => {
    expect(genusPair(tribe, species[0], g(['Mechanitis', 0.35], ['Melinaea', 0.34]), species, cuts)).toBeNull()
    expect(genusPair(tribe, species[0], g(['Mechanitis', 0.36], ['Melinaea', 0.34]), species, cuts)).not.toBeNull()
  })

  it('today\'s line when the second total is below cuts.second (at the cut: the pair)', () => {
    expect(genusPair(tribe, species[0], g(['Mechanitis', 0.7], ['Melinaea', 0.14]), species, cuts)).toBeNull()
    expect(genusPair(tribe, species[0], g(['Mechanitis', 0.7], ['Melinaea', 0.15]), species, cuts)).not.toBeNull()
    // second 0: any second genus makes a pair
    expect(genusPair(tribe, species[0], g(['Mechanitis', 0.7], ['Melinaea', 0.01]), species, { ...cuts, second: 0 })).not.toBeNull()
    // a single genus listed: nothing to pair
    expect(genusPair(tribe, species[0], g(['Mechanitis', 0.75]), species, { ...cuts, second: 0 })).toBeNull()
  })

  it('ties: the higher best species first, then by name', () => {
    const tied = g(['Melinaea', 0.4], ['Mechanitis', 0.4], ['Oleria', 0.1])
    expect(genusPair(tribe, species[0], tied, species, cuts)!.map(x => x.genus)).toEqual(['Mechanitis', 'Melinaea'])
    const even = [c('Oleria onega', 0.3), c('Hypothyris anastasia', 0.3)]
    expect(genusPair(tribe, even[0], g(['Oleria', 0.4], ['Hypothyris', 0.4]), even, cuts)!.map(x => x.genus)).toEqual(['Hypothyris', 'Oleria'])
  })

  it('keeps the top species named: none when its genus is not one of the two', () => {
    const list = [c('Heliconius erato', 0.12), c('Mechanitis polymnia', 0.1), c('Oleria onega', 0.08)]
    expect(genusPair(tribe, list[0], g(['Mechanitis', 0.4], ['Oleria', 0.35], ['Heliconius', 0.12]), list, cuts)).toBeNull()
    // its genus second: the pair, the top species' genus named second
    const p = genusPair(tribe, list[0], g(['Mechanitis', 0.5], ['Heliconius', 0.3]), list, cuts)!
    expect(p.map(x => x.species?.name)).toEqual(['Mechanitis polymnia', 'Heliconius erato'])
  })

  it('the best species from every species when known (the phone), else from the listed ones, else none', () => {
    const scores = [0.3, 0.02, 0.2, 0.15, 0.1, 0.08]
    const names = ['Mechanitis messenoides', 'Melinaea isocomma', 'Mechanitis polymnia', 'Mechanitis lysimnia', 'Mechanitis menapis', 'Melinaea menophilus']
    const r = rank(scores, names, 3)
    expect(r.best.get('Melinaea')).toEqual(c('Melinaea menophilus', 0.08))
    // Melinaea 0.10 is not among the 3 listed species: named from `best`, or by the genus alone without it.
    expect(genusPair(tribe, r.species[0], r.genus, r.species, { ...cuts, second: 0 }, r.best)![1].species?.name).toBe('Melinaea menophilus')
    expect(genusPair(tribe, r.species[0], r.genus, r.species, { ...cuts, second: 0 })![1]).toMatchObject({ genus: 'Melinaea', species: null })
  })

  it('has the measured cuts per engine', () => {
    // RESULT.md (17 Nov 2026): both engines name two genera only from a summed 0.70; the second genus at least 0.02
    // (small, the lead's display choice) or 0.15 (BioCLIP, chosen and checked); below either, today's line.
    expect([THRESHOLDS.small.pair, THRESHOLDS.small.second, THRESHOLDS.bioclip.pair, THRESHOLDS.bioclip.second]).toEqual([0.7, 0.02, 0.7, 0.15])
  })
})

describe('stripNames', () => {
  it('abbreviates the genus, unless two genera in the strip share the initial', () => {
    const n = stripNames(['Mechanitis messenoides', 'Melinaea isocomma', 'Mechanitis menophilus', 'Oleria onega'])
    expect([...n.values()]).toEqual(['Mechanitis messenoides', 'Melinaea isocomma', 'Mechanitis menophilus', 'O. onega'])
    expect([...stripNames(['Mechanitis polymnia', 'Mechanitis lysimnia', 'Hypothyris ninonia']).values()]).toEqual(['M. polymnia', 'M. lysimnia', 'H. ninonia'])
  })
})
