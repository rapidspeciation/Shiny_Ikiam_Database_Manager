import { describe, expect, it } from 'vitest'
import { commitText, filterChoices, labelOf, toChoices } from '../choices'
import { pickChoice } from '../paste'

const species = ['Oleria onega', 'Mechanitis messenoides', 'Mechanitis polymnia', 'Ithomia salapia', 'Hypothyris anastasia']
const fates = toChoices([
  { value: 'insectario', label: 'Collected_Sent2Insectary' },
  { value: 'preservada', label: 'Collected_Preserved' },
  { value: 'liberada', label: 'Released_Unmarked' },
])

describe('ChoiceField lists', () => {
  it('shows the same text first, then what starts with it, then what contains it, in list order', () => {
    const labels = (typed: string) => filterChoices(toChoices(species), typed).items.map(c => c.label)
    expect(labels('mech')).toEqual(['Mechanitis messenoides', 'Mechanitis polymnia'])
    expect(labels('mess')).toEqual(['Mechanitis messenoides'])
    expect(labels('ia')).toEqual(['Oleria onega', 'Mechanitis polymnia', 'Ithomia salapia', 'Hypothyris anastasia'])
    expect(labels('m')).toEqual(['Mechanitis messenoides', 'Mechanitis polymnia', 'Ithomia salapia'])
    expect(labels('i')).toEqual([
      'Ithomia salapia',
      'Oleria onega',
      'Mechanitis messenoides',
      'Mechanitis polymnia',
      'Hypothyris anastasia',
    ])
    expect(labels('')).toEqual(species)
  })
  it('marks first the suggestion the grid would take (pickChoice)', () => {
    for (const typed of ['mech', 'MESS', 'ia', 'oleria onega', 'x'])
      expect(filterChoices(toChoices(species), typed).items[0]?.label ?? null).toBe(pickChoice(typed, species))
  })
  it('renders at most the limit of a long list, counting the rest', () => {
    const many = toChoices(Array.from({ length: 10_000 }, (_, i) => `Species ${i}`))
    const { items, total } = filterChoices(many, 'species 1')
    expect(items).toHaveLength(100)
    expect(items[0].label).toBe('Species 1')
    expect(total).toBe(1111)
  })
  it('keeps grouped options together', () => {
    const racks = toChoices([
      { value: 'FS1', label: 'FS1 ethanol', group: 'Insectario' },
      { value: 'ET2', label: 'ET2', group: 'Colectas' },
    ])
    expect(filterChoices(racks, 'et').items.map(c => c.value)).toEqual(['FS1', 'ET2'])
    expect(filterChoices(racks, 'et2').items.map(c => c.value)).toEqual(['ET2'])
  })
})

describe('what a commit stores', () => {
  it('free text: completed to the first suggestion, else kept as typed', () => {
    const c = toChoices(species)
    expect(commitText('mess', c)).toBe('Mechanitis messenoides')
    expect(commitText(' oleria ONEGA ', c)).toBe('Oleria onega')
    expect(commitText('Greta andromica', c)).toBe('Greta andromica')
    expect(commitText('  ', c)).toBe('')
  })
  it('select-like: only an option, else back to the value (null)', () => {
    expect(commitText('pres', fates, { freetext: false })).toBe('preservada')
    expect(commitText('nothing', fates, { freetext: false })).toBeNull()
    expect(commitText('', fates, { freetext: false })).toBeNull()
    expect(commitText('', fates, { freetext: false, allowEmpty: true })).toBe('')
  })
  it('shows labels and stores values', () => {
    expect(commitText('released', fates, { freetext: false })).toBe('liberada')
    expect(commitText('liberada', fates, { freetext: false })).toBeNull()
    expect(labelOf(fates, 'insectario')).toBe('Collected_Sent2Insectary')
    expect(labelOf(fates, 'otra')).toBe('otra')
  })
})
