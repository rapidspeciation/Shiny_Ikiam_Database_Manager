import { readFileSync } from 'node:fs'
import { describe, expect, it } from 'vitest'
import { engineOf, headline, higherRanks, ranksAbove, rungs, shownTaxon, subspeciesHint, taxonOf, THRESHOLDS, type Cuts, type Taxonomy } from '../identifyResult'
import { serverRanking, type ServerAnswer } from '../identifyOnline'
import { isIthomiiniGenus } from '../ithomiiniGenera'

// The answer ladder (WingsClassificator X-A8): species >= cuts.species, genus >= cuts.genus, tribe >= cuts.higher,
// subfamily >= cuts.higher, else «not sure»; the cuts per engine (THRESHOLDS).
const species = ['Mechanitis polymnia', 'Mechanitis lysimnia', 'Oleria onega', 'Hypothyris anastasia', 'Danaus plexippus', 'Heliconius erato', 'Chetone angulosa', 'Athis inca']
const tax: Taxonomy = {
  head: 'h',
  tribes: ['Danaini', 'Heliconiini', 'Ithomiini', 'UNKNOWN_tribe_Chetone'],
  subfamilies: ['Ithomiinae', 'Nymphalinae', 'UNKNOWN_subfamily_Athis'],
  species: {
    'Mechanitis polymnia': [2, 0],
    'Mechanitis lysimnia': [2, 0],
    'Oleria onega': [2, 0],
    'Hypothyris anastasia': [-1, 0], // no tribe: skips the tribe rung, still counts for the subfamily
    'Danaus plexippus': [0, -1],
    'Heliconius erato': [1, 1],
    'Chetone angulosa': [3, 1], // a placeholder tribe: never a tribe answer
    'Athis inca': [-1, 2], // a placeholder subfamily: never a subfamily answer
  },
}
const answer = (scores: number[]) => {
  const genus = new Map<string, number>()
  species.forEach((s, i) => genus.set(s.split(' ')[0], (genus.get(s.split(' ')[0]) ?? 0) + scores[i]))
  const order = scores.map((p, i) => ({ name: species[i], p })).sort((a, b) => b.p - a.p)
  return { top: order[0], genus, higher: higherRanks(scores, species, tax) }
}
const say = (scores: number[], cuts: Cuts = THRESHOLDS.bioclip) => {
  const a = answer(scores)
  return headline(a.top, a.genus, a.higher, cuts)
}

describe('tribe and subfamily answer', () => {
  it('is always on: without totals (an older server) the ladder stops at the genus', () => {
    const a = answer([0.4, 0.1, 0.35, 0, 0.15, 0, 0, 0])
    expect(headline(a.top, a.genus, a.higher).kind).toBe('tribe')
    expect(headline(a.top, a.genus, null)).toEqual({ kind: 'unsure' })
  })

  it('has one set of cuts per engine: species 0.70 (measured), genus and higher ranks 0.80', () => {
    // The shipped values (15 Nov 2026, PREREG.md): when the cuts change, these lines change with them.
    expect(THRESHOLDS).toEqual({ small: { species: 0.7, genus: 0.8, higher: 0.8 }, bioclip: { species: 0.7, genus: 0.8, higher: 0.8 } })
    expect(engineOf('online')).toBe('bioclip')
    expect(engineOf('online', 'small')).toBe('bioclip')
    expect(engineOf('phone', 'full')).toBe('bioclip')
    expect(engineOf('phone', 'small')).toBe('small')
    expect(engineOf('phone')).toBe('small')
  })

  it('keeps the species and genus rungs first, each at its own cut', () => {
    expect(say([0.85, 0.05, 0.05, 0, 0.05, 0, 0, 0])).toEqual({ kind: 'species', name: 'Mechanitis polymnia' })
    expect(say([0.5, 0.35, 0.1, 0, 0.05, 0, 0, 0])).toEqual({ kind: 'genus', genus: 'Mechanitis' })
    // Separate cuts: a lower species cut names the species, a higher genus cut skips to the tribe.
    expect(say([0.6, 0.25, 0.1, 0, 0.05, 0, 0, 0], { species: 0.55, genus: 0.8, higher: 0.8 })).toEqual({ kind: 'species', name: 'Mechanitis polymnia' })
    expect(say([0.6, 0.25, 0.1, 0, 0.05, 0, 0, 0], { species: 0.8, genus: 0.9, higher: 0.8 })).toMatchObject({ kind: 'tribe', name: 'Ithomiini' })
    expect(say([0.6, 0.25, 0.1, 0, 0.05, 0, 0, 0], { species: 0.8, genus: 0.9, higher: 0.99 })).toEqual({ kind: 'unsure' })
    // The tribe cut is its own too.
    expect(say([0.4, 0.1, 0.35, 0, 0.15, 0, 0, 0], { species: 0.8, genus: 0.8, higher: 0.9 })).toEqual({ kind: 'unsure' })
  })

  it('names the tribe when its total is clear and the genus is not', () => {
    const h = say([0.4, 0.1, 0.35, 0, 0.15, 0, 0, 0])
    expect(h.kind).toBe('tribe')
    expect(h).toMatchObject({ name: 'Ithomiini' })
    expect((h as { p: number }).p).toBeCloseTo(0.85)
    // Just under the cut: no tribe; at it: the tribe.
    expect(say([0.4, 0.1, 0.29, 0, 0.21, 0, 0, 0]).kind).toBe('unsure')
    expect(say([0.4, 0.1, 0.3, 0, 0.2, 0, 0, 0]).kind).toBe('tribe')
  })

  it('names the subfamily when no tribe is clear; a species without a tribe still counts for its subfamily', () => {
    // Ithomiini 0.7 (Hypothyris has no tribe), Ithomiinae 0.7 + 0.15 = 0.85.
    const h = say([0.4, 0.1, 0.2, 0.15, 0.15, 0, 0, 0])
    expect(h).toMatchObject({ kind: 'subfamily', name: 'Ithomiinae' })
    expect((h as { p: number }).p).toBeCloseTo(0.85)
    const t = higherRanks([0.4, 0.1, 0.2, 0.15, 0.15, 0, 0, 0], species, tax)
    expect(t.tribe.get('Ithomiini')).toBeCloseTo(0.7)
    expect(t.subfamily.get('Ithomiinae')).toBeCloseTo(0.85)
  })

  it('never answers with a placeholder name', () => {
    // Chetone (placeholder tribe) and Athis (placeholder subfamily) hold nearly everything.
    expect(say([0, 0, 0, 0, 0, 0, 0.45, 0.55])).toEqual({ kind: 'unsure' })
    const t = higherRanks([0, 0, 0, 0, 0, 0, 0.45, 0.55], species, tax)
    expect([...t.tribe.keys()].filter(k => !shownTaxon(k))).toEqual([])
    expect([...t.tribe.values()].every(p => p === 0)).toBe(true)
    expect([...t.subfamily.keys()].sort()).toEqual(['Ithomiinae', 'Nymphalinae'])
    expect(t.subfamily.get('Nymphalinae')).toBeCloseTo(0.45)
    // The Chetone species still counts for its real subfamily.
    expect(say([0, 0, 0, 0, 0, 0.4, 0.45, 0.15])).toMatchObject({ kind: 'subfamily', name: 'Nymphalinae' })
    // A placeholder in the server's list is skipped too.
    const top = { name: 'Chetone angulosa', p: 0.4 }
    const genus = new Map([['Chetone', 0.5]])
    expect(headline(top, genus, { tribe: new Map([['UNKNOWN_tribe_Chetone', 0.9]]), subfamily: new Map([['incertae sedis', 0.9]]) })).toEqual({ kind: 'unsure' })
    expect(shownTaxon('UNKNOWN_subfamily_Athis')).toBe(false)
    expect(shownTaxon('Ithomiini')).toBe(true)
  })

  it('names a subspecies under an uncertain species only as «if it is this species»', () => {
    const top = { name: 'Mechanitis polymnia', p: 0.4 }
    const ssp = [{ name: 'Mechanitis polymnia casabranca', p: 0.3 }]
    for (const said of [{ kind: 'genus', genus: 'Mechanitis' }, { kind: 'tribe', name: 'Ithomiini', p: 0.85 }, { kind: 'subfamily', name: 'Ithomiinae', p: 0.85 }, { kind: 'unsure' }] as const)
      expect(subspeciesHint(said, top, ssp)?.kind).toBe('if')
    expect(subspeciesHint({ kind: 'species', name: top.name }, { ...top, p: 0.9 }, ssp)?.kind).toBe('species')
  })

  it('reads the server answer with the same rule', () => {
    const a: ServerAnswer = {
      species: [['Mechanitis polymnia', 0.4], ['Oleria onega', 0.3]],
      subspecies: [],
      genus: [['Mechanitis', 0.45], ['Oleria', 0.3]],
      tribe: [['Ithomiini', 0.86], ['Danaini', 0.1]],
      subfamily: [['Ithomiinae', 0.86]],
      crop: null, wingBox: null, prior: null, ms: null,
    }
    const r = serverRanking(a)
    expect(headline(r.species[0], r.genus, r.higher)).toMatchObject({ kind: 'tribe', name: 'Ithomiini' })
    // An older server sends no totals: no tribe answer.
    const old = serverRanking({ ...a, tribe: undefined, subfamily: undefined })
    expect(headline(old.species[0], old.genus, old.higher)).toEqual({ kind: 'unsure' })
  })

  it('ships a map of the live head without placeholder names', () => {
    const file = new URL('../../../../tools/identifier/taxonomy_6fa6ab78.json', import.meta.url)
    const t = JSON.parse(readFileSync(file, 'utf8')) as Taxonomy
    expect(t.head.startsWith('6fa6ab78')).toBe(true)
    expect([...t.tribes, ...t.subfamilies].filter(n => !shownTaxon(n))).toEqual([])
    const pairs = Object.values(t.species)
    expect(pairs.every(([a, b]) => a < t.tribes.length && b < t.subfamilies.length && (a >= 0 || b >= 0))).toBe(true)
    expect(t.species['Mechanitis polymnia']).toEqual([t.tribes.indexOf('Ithomiini'), t.subfamilies.indexOf('Danainae')])
  })

  it('files every Ithomiini genus under tribe Ithomiini and subfamily Danainae, and only them (the corrected genus table)', () => {
    const file = new URL('../../../../tools/identifier/taxonomy_6fa6ab78.json', import.meta.url)
    const t = JSON.parse(readFileSync(file, 'utf8')) as Taxonomy
    const name = (sp: string) => {
      const [a, b] = t.species[sp] ?? [-1, -1]
      return [a >= 0 ? t.tribes[a] : null, b >= 0 ? t.subfamilies[b] : null]
    }
    const species = Object.keys(t.species)
    const ithomiini = species.filter(sp => isIthomiiniGenus(sp.split(' ')[0]))
    expect(ithomiini.length).toBeGreaterThan(300)
    for (const sp of ithomiini) expect([sp, ...name(sp)]).toEqual([sp, 'Ithomiini', 'Danainae'])
    // No other genus counts toward Ithomiini: the moth Macrosoma (Hedylidae) above all.
    expect(species.filter(sp => name(sp)[0] === 'Ithomiini' && !isIthomiiniGenus(sp.split(' ')[0]))).toEqual([])
    const moth = species.find(sp => sp.startsWith('Macrosoma '))!
    expect(name(moth)).toEqual([null, 'Hedylinae'])
    // The old split of Ithomiini into tribes is gone.
    for (const old of ['Dircennini', 'Godyridini', 'Napeogenini', 'Mechanitini', 'Melinaeini', 'Oleriini', 'Tithoreini', 'Athesitini'])
      expect(t.tribes).not.toContain(old)
    expect(t.subfamilies).not.toContain('Ithomiinae')
    // Names wrong outside Ithomiini are left out: a Dismorphia (an Ithomiini mimic) is never «Tribu Pierini».
    const dismorphia = species.filter(sp => sp.startsWith('Dismorphia '))
    expect(dismorphia.length).toBeGreaterThan(0)
    for (const sp of dismorphia) expect(name(sp)).toEqual([null, 'Dismorphiinae'])
    expect(t.tribes.filter(n => !/^[A-Z][a-z]+ini$/.test(n))).toEqual([])
    expect(t.subfamilies.filter(n => !/^[A-Z][a-z]+inae$/.test(n))).toEqual([])
  })
})

describe('the top species\' ranks: the label and the rows under it', () => {
  it('names a species\' tribe and subfamily, never a placeholder; none for a species the file does not list', () => {
    expect(taxonOf('Mechanitis polymnia', tax)).toEqual({ tribe: 'Ithomiini', subfamily: 'Ithomiinae' })
    expect(ranksAbove('Mechanitis polymnia', tax)).toEqual({ tribe: 'Ithomiini', subfamily: 'Ithomiinae' })
    // No tribe: the subfamily alone.
    expect(ranksAbove('Hypothyris anastasia', tax)).toEqual({ tribe: null, subfamily: 'Ithomiinae' })
    // A placeholder tribe (UNKNOWN_tribe_Chetone) is never shown, even if a file holds one.
    expect(ranksAbove('Chetone angulosa', tax)).toEqual({ tribe: null, subfamily: 'Nymphalinae' })
    // A placeholder subfamily and no tribe: nothing to show.
    expect(ranksAbove('Athis inca', tax)).toBeNull()
    expect(ranksAbove('Morpho helenor', tax)).toBeNull()
    expect(ranksAbove('Mechanitis polymnia', null)).toBeNull()
  })

  it('the rows under an uncertain top species: its genus (sure or not), then the clear tribe or subfamily (a total, even when the top species lies outside it)', () => {
    const genus = new Map([['Mechanitis', 0.38], ['Oleria', 0.3]])
    const guess = { name: 'Mechanitis polymnia', p: 0.31 }
    expect(rungs({ kind: 'species', name: guess.name }, guess, genus)).toEqual([])
    expect(rungs({ kind: 'genus', genus: 'Mechanitis' }, guess, new Map([['Mechanitis', 0.86]]))).toEqual([{ kind: 'genus', name: 'Mechanitis', p: 0.86, sure: true }])
    expect(rungs({ kind: 'tribe', name: 'Ithomiini', p: 0.88 }, guess, genus)).toEqual([
      { kind: 'genus', name: 'Mechanitis', p: 0.38, sure: false },
      { kind: 'tribe', name: 'Ithomiini', p: 0.88, sure: true },
    ])
    expect(rungs({ kind: 'subfamily', name: 'Danainae', p: 0.9 }, guess, genus)[1]).toEqual({ kind: 'subfamily', name: 'Danainae', p: 0.9, sure: true })
    expect(rungs({ kind: 'unsure' }, guess, genus)).toEqual([{ kind: 'genus', name: 'Mechanitis', p: 0.38, sure: false }])
    expect(rungs({ kind: 'unsure' }, undefined, genus)).toEqual([])
    // The top species outside the clear tribe stays the guess: its own genus, and the tribe's total beside it.
    expect(rungs({ kind: 'tribe', name: 'Ithomiini', p: 0.86 }, { name: 'Heliconius erato', p: 0.12 }, new Map([['Heliconius', 0.12]]))).toEqual([
      { kind: 'genus', name: 'Heliconius', p: 0.12, sure: false },
      { kind: 'tribe', name: 'Ithomiini', p: 0.86, sure: true },
    ])
  })

  it('the live file names Ithomiini and Danainae for Mechanitis, Dismorphiinae alone for a Dismorphia', () => {
    const file = new URL('../../../../tools/identifier/taxonomy_6fa6ab78.json', import.meta.url)
    const t = JSON.parse(readFileSync(file, 'utf8')) as Taxonomy
    expect(ranksAbove('Mechanitis polymnia', t)).toEqual({ tribe: 'Ithomiini', subfamily: 'Danainae' })
    const dismorphia = Object.keys(t.species).find(sp => sp.startsWith('Dismorphia '))!
    expect(ranksAbove(dismorphia, t)).toEqual({ tribe: null, subfamily: 'Dismorphiinae' })
  })
})
