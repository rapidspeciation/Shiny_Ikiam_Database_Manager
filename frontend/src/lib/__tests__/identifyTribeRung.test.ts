import { readFileSync } from 'node:fs'
import { describe, expect, it } from 'vitest'
import { CLEAR, headline, higherRanks, shownTaxon, subspeciesHint, TRIBE_RUNG, type Taxonomy } from '../identifyResult'
import { serverRanking, type ServerAnswer } from '../identifyOnline'
import { isIthomiiniGenus } from '../ithomiiniGenera'

// The tribe / subfamily answer (WingsClassificator X-A8): species >= CLEAR, genus >= CLEAR, tribe >= CLEAR,
// subfamily >= CLEAR, else «not sure»; TRIBE_RUNG on unless built with VITE_TRIBE_RUNG=0.
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
const say = (scores: number[], on = true) => {
  const a = answer(scores)
  return headline(a.top, a.genus, a.higher, on)
}

describe('tribe and subfamily answer', () => {
  it('is on unless the page is built with VITE_TRIBE_RUNG=0', () => {
    expect(TRIBE_RUNG).toBe(true)
    const a = answer([0.3, 0.2, 0.3, 0, 0.1, 0, 0.1, 0])
    // Flag off: the old ladder, even with the totals given.
    expect(headline(a.top, a.genus, a.higher, false)).toEqual({ kind: 'unsure' })
    expect(headline(a.top, a.genus, null, true)).toEqual({ kind: 'unsure' })
  })

  it('keeps the species and genus rungs first, at the same cut', () => {
    expect(CLEAR).toBe(0.8)
    expect(say([0.85, 0.05, 0.05, 0, 0.05, 0, 0, 0])).toEqual({ kind: 'species', name: 'Mechanitis polymnia' })
    expect(say([0.5, 0.35, 0.1, 0, 0.05, 0, 0, 0])).toEqual({ kind: 'genus', genus: 'Mechanitis' })
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
    expect(headline(top, genus, { tribe: new Map([['UNKNOWN_tribe_Chetone', 0.9]]), subfamily: new Map([['incertae sedis', 0.9]]) }, true)).toEqual({ kind: 'unsure' })
    expect(shownTaxon('UNKNOWN_subfamily_Athis')).toBe(false)
    expect(shownTaxon('Ithomiini')).toBe(true)
  })

  it('names no subspecies under a tribe or subfamily answer', () => {
    const top = { name: 'Mechanitis polymnia', p: 0.4 }
    const ssp = [{ name: 'Mechanitis polymnia casabranca', p: 0.3 }]
    expect(subspeciesHint({ kind: 'tribe', name: 'Ithomiini', p: 0.85 }, top, ssp)).toBeNull()
    expect(subspeciesHint({ kind: 'subfamily', name: 'Ithomiinae', p: 0.85 }, top, ssp)).toBeNull()
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
    expect(headline(r.species[0], r.genus, r.higher, true)).toMatchObject({ kind: 'tribe', name: 'Ithomiini' })
    expect(headline(r.species[0], r.genus, r.higher, false)).toEqual({ kind: 'unsure' })
    // An older server sends no totals: no tribe answer.
    const old = serverRanking({ ...a, tribe: undefined, subfamily: undefined })
    expect(headline(old.species[0], old.genus, old.higher, true)).toEqual({ kind: 'unsure' })
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
  })
})
