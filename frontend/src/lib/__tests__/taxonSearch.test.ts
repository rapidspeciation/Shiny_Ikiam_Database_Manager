import { describe, expect, it } from 'vitest'
import { catalog, norm, search, taxonView, type ShownPhoto } from '../taxonSearch'

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
  it('r2-fable-F12: misspelt head labels (no family in any source) are not offered', () => {
    const c = catalog([t('Hwy rubrocaudata', 'moth', ''), t('Psilogramm menephron', 'moth', ''), t('Psilogramma menephron', 'moth', 'Sphingidae')])
    expect(search(c, 'hwy')).toEqual([])
    expect(search(c, 'psilogramm').map(h => h.name)).toEqual(['Psilogramma', 'Psilogramma menephron'])
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

describe('what a chosen taxon shows (review r1)', () => {
  const ph = (src: string, level: ShownPhoto['level'], taxon?: string): ShownPhoto => ({ src, credit: `© ${src}`, license: 'CC-BY', level, ...(taxon ? { taxon } : {}) })
  const hit = (name: string) => search(cat, name, 1)[0]

  it('sol-F3: a species’ extra photos and the team’s specimens are not called subspecies', () => {
    const v = taxonView(cat, hit('Mechanitis polymnia'), {
      'Mechanitis polymnia': { photos: [ph('a', 'species'), ph('b', 'species'), ph('cam', 'specimen'), ph('s1', 'subspecies', 'Mechanitis polymnia casabranca')] },
    })
    expect(v.lead?.photo.src).toBe('a')
    expect(v.sections.map(s => [s.id, s.cards.map(c => [c.photo.src, c.kicker])])).toEqual([
      ['more', [['b', 'species'], ['cam', 'specimen']]],
      ['ssp', [['s1', 'subspecies']]],
    ])
  })

  it('sol-F5: a genus card falls back to a saved subspecies photo and names that subspecies', () => {
    const v = taxonView(cat, hit('Mechanitis'), {
      'Mechanitis polymnia': { photos: [ph('s1', 'subspecies', 'Mechanitis polymnia casabranca')] },
      'Mechanitis lysimnia': { photos: [ph('l', 'species')] },
    })
    const cards = v.sections[0].cards.map(c => [c.label, c.viaSsp, c.name])
    expect(cards).toEqual([
      ['lysimnia', null, 'Mechanitis lysimnia'],
      ['polymnia', 'casabranca', 'Mechanitis polymnia casabranca'],
    ])
    expect(v.noPhoto).toEqual(['Mechanitis mazaeus'])
  })

  it('fable-F8: subspecies cards carry the epithet alone; the full name stays for the full-screen view', () => {
    const v = taxonView(cat, hit('Mechanitis polymnia casabranca'), {
      'Mechanitis polymnia': { photos: [ph('a', 'species'), ph('s1', 'subspecies', 'Mechanitis polymnia casabranca'), ph('s2', 'subspecies', 'Mechanitis polymnia proceriformis')] },
    })
    expect(v.lead).toMatchObject({ name: 'Mechanitis polymnia casabranca', photo: { src: 's1' } })
    const ssp = v.sections.find(s => s.id === 'ssp')!.cards
    expect(ssp.map(c => [c.label, c.name])).toEqual([['proceriformis', 'Mechanitis polymnia proceriformis']])
    expect(v.sections.find(s => s.id === 'more')!.cards.map(c => c.photo.src)).toEqual(['a'])
  })

  it('several photos of a subspecies: one card each among the others, all of them for the subspecies chosen', () => {
    const photos = { photos: [ph('a', 'species'), ph('p1', 'subspecies', 'Mechanitis polymnia proceriformis'), ph('c1', 'subspecies', 'Mechanitis polymnia casabranca'), ph('p2', 'subspecies', 'Mechanitis polymnia proceriformis')] }
    const sp = taxonView(cat, hit('Mechanitis polymnia'), { 'Mechanitis polymnia': photos })
    expect(sp.sections.find(s => s.id === 'ssp')!.cards.map(c => c.photo.src)).toEqual(['p1', 'c1'])
    const ssp = taxonView(cat, hit('Mechanitis polymnia proceriformis'), { 'Mechanitis polymnia': photos })
    expect(ssp.lead?.photo.src).toBe('p1')
    expect(ssp.sections.find(s => s.id === 'more')!.cards.map(c => c.photo.src)).toEqual(['p2', 'a'])
    expect(new Set([ssp.lead, ...ssp.sections.flatMap(s => s.cards)].map(c => c!.key)).size).toBe(4)
  })
})
