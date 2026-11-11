import { describe, expect, it } from 'vitest'
import { headSource, listSource } from '../taxonPick'

describe('taxonPick', () => {
  const head = {
    species: ['Mechanitis polymnia', 'Mechanitis lysimnia', 'Melinaea menophilus', 'Oleria onega'],
    leaves: ['Mechanitis polymnia', 'Mechanitis polymnia proceriformis', 'Mechanitis lysimnia', 'Melinaea menophilus', 'Oleria onega'],
    is_ssp: '01000',
  }
  const s = headSource(head)

  it('genera first, then species, then subspecies; the start of a word before the middle', () => {
    expect(s.search('mech').map(h => [h.name, h.rank])).toEqual([
      ['Mechanitis', 'genus'],
      ['Mechanitis lysimnia', 'species'],
      ['Mechanitis polymnia', 'species'],
      ['Mechanitis polymnia proceriformis', 'subspecies'],
    ])
    expect(s.search('poly').map(h => h.name)).toEqual(['Mechanitis polymnia', 'Mechanitis polymnia proceriformis'])
    expect(s.search('ONEGA')[0].name).toBe('Oleria onega')
    expect(s.search('m')).toEqual([])
    expect(s.search('mel', 1)).toHaveLength(1)
  })

  it('knows its names (any case, extra spaces), not free text', () => {
    expect(s.knows('mechanitis  Polymnia')).toBe(true)
    expect(s.knows('Mechanitis')).toBe(true)
    expect(s.knows('Napeogenes sp. 2')).toBe(false)
  })

  it("the result's candidates alone, without a head", () => {
    expect(listSource(['Oleria onega']).search('ole').map(h => h.name)).toEqual(['Oleria', 'Oleria onega'])
  })
})
