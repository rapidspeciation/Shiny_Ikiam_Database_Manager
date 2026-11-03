import { describe, expect, it } from 'vitest'
import { effectiveMode, serverRanking, type ServerAnswer } from '../identifyOnline'
import { subspeciesHint, headline } from '../identifyResult'

describe('effectiveMode', () => {
  it('online by default when signed in with signal; the phone otherwise', () => {
    expect(effectiveMode(null, true, true)).toBe('online')
    expect(effectiveMode('phone', true, true)).toBe('phone')
    expect(effectiveMode('online', true, false)).toBe('phone')
    expect(effectiveMode('online', false, true)).toBe('phone')
    expect(effectiveMode(null, false, true)).toBe('phone')
  })
})

describe('serverRanking', () => {
  const answer: ServerAnswer = {
    species: [
      ['Mechanitis messenoides', 0.6],
      ['Mechanitis polymnia', 0.15],
      ['Melinaea menophilus', 0.05],
    ],
    subspecies: [
      ['Mechanitis messenoides deceptus', 0.31],
      ['Mechanitis messenoides messenoides', 0.2],
      ['Mechanitis polymnia proceriformis', 0.1],
      ['Hypothyris anastasia honesta', 0.01],
    ],
    genus: [
      ['Mechanitis', 0.78],
      ['Hypothyris', 0.02],
    ],
    crop: 'success',
    wingBox: null,
    prior: null,
    ms: 1000,
  }
  it('the species, the genus totals (a missing genus counts its listed species), the subspecies per listed species', () => {
    const r = serverRanking(answer)
    expect(r.species.map(c => c.name)).toEqual(['Mechanitis messenoides', 'Mechanitis polymnia', 'Melinaea menophilus'])
    expect(r.genus.get('Mechanitis')).toBe(0.78)
    expect(r.genus.get('Melinaea')).toBe(0.05)
    expect(r.ssp.get('Mechanitis messenoides')?.map(c => c.name)).toEqual(['Mechanitis messenoides deceptus', 'Mechanitis messenoides messenoides'])
    expect(r.ssp.has('Hypothyris anastasia')).toBe(false)
  })
  it('feeds the same subspecies line as the phone', () => {
    const r = serverRanking(answer)
    const top = r.species[0]
    const hint = subspeciesHint(headline(top, r.genus), top, r.ssp.get(top.name))
    expect(hint).toMatchObject({ kind: 'if', name: 'Mechanitis messenoides deceptus', alt: { name: 'Mechanitis messenoides messenoides' } })
  })
})
