import { describe, expect, it } from 'vitest'
import { catalog, norm, search } from '../taxonSearch'

// The Identificar search box: one box for a genus, a species or a subspecies, typed whole or in part.
const t = (name: string, group: 'ithomiini' | 'nymphalidae' | 'butterfly' | 'moth' = 'ithomiini', family = 'Nymphalidae') => {
  const p = name.split(' ')
  return { name, rank: (p.length > 2 ? 'subspecies' : 'species') as 'species' | 'subspecies', species: p.slice(0, 2).join(' '), genus: p[0], family, group }
}
const cat = catalog(
  [t('Mechanitis polymnia'), t('Mechanitis polymnia casabranca'), t('Mechanitis polymnia proceriformis'), t('Mechanitis mazaeus'), t('Melinaea menophilus')],
  [t('Mechanitis lysimnia'), t('Mechanitis mazaeus deceptus'), t('Urania leilus', 'moth', 'Uraniidae'), t('Mechanitis polymnia')],
)
const names = (q: string, n = 8) => search(cat, q, n).map(h => `${h.rank}:${h.name}`)

describe('taxon search', () => {
  it('a genus first, then its species and subspecies', () => {
    expect(names('mechanitis', 3)).toEqual(['genus:Mechanitis', 'species:Mechanitis lysimnia', 'species:Mechanitis mazaeus'])
    expect(search(cat, 'Mechanitis')[0]).toMatchObject({ rank: 'genus', species: 3, group: 'ithomiini' })
    expect(names('mech', 1)).toEqual(['genus:Mechanitis'])
  })
  it('a species by part of each word, or by its epithet', () => {
    expect(names('mech poly')).toEqual(['species:Mechanitis polymnia', 'subspecies:Mechanitis polymnia casabranca', 'subspecies:Mechanitis polymnia proceriformis'])
    expect(names('M. polymnia', 1)).toEqual(['species:Mechanitis polymnia'])
    expect(names('polymnia', 1)).toEqual(['species:Mechanitis polymnia'])
  })
  it('a subspecies by its own name; the whole name typed comes first', () => {
    expect(names('casabranca')).toEqual(['subspecies:Mechanitis polymnia casabranca'])
    expect(names('mechanitis mazaeus deceptus')).toEqual(['subspecies:Mechanitis mazaeus deceptus'])
    expect(names('Mechanitis polymnia', 1)).toEqual(['species:Mechanitis polymnia'])
  })
  it('a moth, accents and case ignored, short queries none', () => {
    expect(search(cat, 'urania')[0]).toMatchObject({ name: 'Urania', rank: 'genus', family: 'Uraniidae', group: 'moth' })
    expect(names('ÚRANIA LEI')).toEqual(['species:Urania leilus'])
    expect(norm('  Mechanitis.polymnia ')).toBe('mechanitis polymnia')
    expect(search(cat, 'm')).toEqual([])
    expect(search(cat, 'zzz')).toEqual([])
  })
  it('lists a genus’ species and a species’ subspecies, each once, from every list', () => {
    expect(cat.speciesOf.get('Mechanitis')).toEqual(['Mechanitis lysimnia', 'Mechanitis mazaeus', 'Mechanitis polymnia'])
    expect(cat.sspOf.get('Mechanitis polymnia')).toEqual(['Mechanitis polymnia casabranca', 'Mechanitis polymnia proceriformis'])
    expect(cat.entries.filter(e => e.name === 'Mechanitis polymnia')).toHaveLength(1)
  })
  it('a species known only through a subspecies is still found', () => {
    const c = catalog([t('Oleria onega janarilla')])
    expect(search(c, 'oleria onega').map(h => h.rank)).toEqual(['species', 'subspecies'])
  })
})
