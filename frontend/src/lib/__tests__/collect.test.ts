import { describe, expect, it } from 'vitest'
import {
  addPhrase,
  isEmptyDraft,
  misfit,
  parseWeight,
  rankByRecency,
  searchSpecies,
  signNote,
  speciesEntries,
  speciesTotals,
  summarize,
  weatherLabel,
  type Draft,
} from '../collect'
import { weekdayOf } from '../dates'
import { complete, pickChoice } from '../paste'

describe('the summary shown before saving a Colecta', () => {
  const draft = (over: Partial<Draft>): Draft => ({
    key: Math.random().toString(),
    location: 'Cavernas Templo de Ceremonia',
    species: 'Ithomia salapia',
    subspecies: 'salapia',
    sex: 'female',
    fate: 'insectario',
    time: '',
    purpose: '',
    notes: '',
    insectaryId: '',
    cam: '',
    tube: '',
    medium: '',
    collector: 'PAS - Patricio Salazar',
    identifier: 'PAS - Patricio Salazar',
    rainfall: 'DY_(dry)',
    cloud: 'S&C_(sun_&_cloud_patches)',
    ...over,
  })
  it('counts sexes sent to the insectary, preserved ones with their CAM range, and species by sex', () => {
    const oleria = { species: 'Oleria onega', subspecies: 'astigara', sex: 'male', fate: 'preservada' } as const
    const s = summarize([
      draft({}),
      draft({ sex: 'male' }),
      draft({ sex: 'female ?' }),
      draft({ ...oleria, cam: 'CAM079896' }),
      draft({ ...oleria, cam: 'CAM079895' }),
      draft({ location: 'Ikiam', fate: 'liberada', sex: 'NOT_COLLECTED' }),
    ])
    expect(s.places).toEqual(['Cavernas Templo de Ceremonia', 'Ikiam'])
    expect(s.insectary).toEqual({ female: 2, male: 1, other: 0 })
    expect([s.preserved, s.released]).toEqual([2, 1])
    expect(s.cams).toEqual({ first: 'CAM079895', last: 'CAM079896', consecutive: true })
    expect(s.species[0]).toEqual({ name: 'Ithomia salapia salapia', female: 2, male: 1, other: 1 })
    expect(s.species[1]).toEqual({ name: 'Oleria onega astigara', female: 0, male: 2, other: 0 })
  })
  it('gives the weekday of the collection date', () => {
    expect(weekdayOf('2026-09-23')).toBe('miércoles')
    expect(weekdayOf('')).toBe('')
  })
})

describe('values that do not fit a Colecta column', () => {
  it('keeps IDs, CAMs, tubes and times that look right (and blanks or NA)', () => {
    expect(misfit('cam', 'CAM079895')).toBeNull()
    // CAM and tube together, as the worksheet writes them.
    expect(misfit('cam', 'CAM079895 · FS90415305 (Flash frozen)')).toBeNull()
    expect(misfit('tube', 'FS90415305')).toBeNull()
    expect(misfit('tube', 'fs9041530')).toBeNull()
    expect(misfit('insectaryId', 'N9D')).toBeNull()
    expect(misfit('time', '11:02')).toBeNull()
    expect(misfit('time', '1102')).toBeNull()
    for (const column of ['cam', 'tube', 'insectaryId', 'time'] as const) {
      expect(misfit(column, '')).toBeNull()
      expect(misfit(column, 'NA')).toBeNull()
    }
  })
  it('refuses a note in Tube_1_id, text in CAM_ID and a CAM in Insectary_ID (a block pasted a column off)', () => {
    expect(misfit('tube', 'Preserve dead aprox 2 hours ago')).toMatch(/no es un Tube_1_id/)
    expect(misfit('tube', 'CAM079895')).toMatch(/no es un Tube_1_id/)
    expect(misfit('cam', 'Pheromones')).toMatch(/no es un CAM_ID/)
    expect(misfit('insectaryId', 'CAM079895')).toMatch(/no es un Insectary_ID/)
    expect(misfit('time', 'Flash frozen')).toMatch(/no es una hora/)
    expect(misfit('sex', 'astigara')).toMatch(/no es un Sex/)
    expect(misfit('fate', 'male')).toMatch(/no es un Release_Collect/)
    expect(misfit('sex', '♀')).toBeNull()
    expect(misfit('fate', 'pres')).toBeNull()
  })
  it('free-text columns take anything', () => {
    expect(misfit('notes', 'AO: 23/9)2026: Preserve dead')).toBeNull()
    expect(misfit('subspecies', '(no subspecies described)')).toBeNull()
  })
})

describe('list choices with brackets', () => {
  const subspecies = ['ino', '(no subspecies described)', 'f. intermedia']
  it('"(no" picks "(no subspecies described)"; brackets are plain text, not a pattern', () => {
    expect(pickChoice('(no', subspecies)).toBe('(no subspecies described)')
    expect(complete('(no subspecies described)', subspecies)).toBe('(no subspecies described)')
    expect(complete('x(y', subspecies)).toBe('x(y')
  })
})

describe('Colecta cards', () => {
  const draft = (over: Partial<Draft>): Draft => ({
    key: Math.random().toString(),
    location: 'Cavernas Templo de Ceremonia',
    species: '',
    subspecies: '',
    sex: '',
    fate: 'insectario',
    time: '',
    purpose: '',
    notes: '',
    insectaryId: 'A8E',
    cam: '',
    tube: '',
    medium: '',
    collector: 'PAS - Patricio Salazar',
    identifier: 'AA - Someone',
    rainfall: 'DY_(dry)',
    cloud: '',
    ...over,
  })
  it('a card is empty until something is chosen in it (its ID is the app\'s)', () => {
    expect(isEmptyDraft(draft({}))).toBe(true)
    expect(isEmptyDraft(draft({ sex: 'male' }))).toBe(false)
  })
  it('places and species come latest first, NA and blanks left out', () => {
    expect(rankByRecency(['Ikiam', 'Apuya Y', 'NA', null, 'Ikiam', 'Cavernas', ''])).toEqual(['Cavernas', 'Ikiam', 'Apuya Y'])
    expect(rankByRecency(['a', 'b', 'c'], { window: 2 })).toEqual(['c', 'b'])
  })
  it('totals per species and form, by sex and fate, without empty cards', () => {
    const totals = speciesTotals([
      draft({ species: 'Mechanitis messenoides', subspecies: 'deceptus', sex: 'female' }),
      draft({ species: 'Mechanitis messenoides', subspecies: 'deceptus', sex: 'male', fate: 'preservada' }),
      draft({ species: 'Ithomia salapia', sex: 'male ?', fate: 'preservada' }),
      draft({}),
    ])
    expect(totals.map(s => [s.species, s.total, s.female, s.male, s.insectario, s.preservada])).toEqual([
      ['Mechanitis messenoides deceptus', 2, 1, 1, 1, 1],
      ['Ithomia salapia', 1, 0, 1, 0, 1],
    ])
  })
  it('quick phrases go after the note, once', () => {
    expect(addPhrase('', 'Sexed by genitalia')).toBe('Sexed by genitalia')
    expect(addPhrase('Worn', 'Recapture')).toBe('Worn; Recapture')
    expect(addPhrase('Worn; Recapture', 'Recapture')).toBe('Worn; Recapture')
  })
  it('weights in grams: comma or point, g, NA; milligrams or text refused', () => {
    expect(parseWeight('0,152')).toBe(0.152)
    expect(parseWeight('.15 g')).toBe(0.15)
    expect(parseWeight('0.12345')).toBe(0.123)
    expect(parseWeight('NA')).toBe('')
    expect(parseWeight('152')).toBeNull()
    expect(parseWeight('CAM079916')).toBeNull()
    expect(misfit('weight', '0.2')).toBeNull()
    expect(misfit('weight', 'heavy')).toMatch(/peso/)
  })
  it('notes are dated and signed as the team writes them, unless already signed', () => {
    expect(signNote('Sexed by genitalia', '3/10/26', 'FCH')).toBe('3/10/26 FCH: Sexed by genitalia')
    expect(signNote('29/09/2026 AA: Sexed by genitalia', '3/10/26', 'FCH')).toBe('29/09/2026 AA: Sexed by genitalia')
    expect(signNote('22Ago26 PAS Preserved dead ~4h', '3/10/26', 'FCH')).toBe('22Ago26 PAS Preserved dead ~4h')
    expect(signNote('  ', '3/10/26', 'FCH')).toBe('')
  })
  it('weather codes read as words', () => {
    expect(weatherLabel('S&C_(sun_&_cloud_patches)')).toEqual({ code: 'S&C', words: 'sun & cloud patches' })
    expect(weatherLabel('NA')).toEqual({ code: 'NA', words: '' })
  })
  it("species search: this list's first, forms with their species, the notebook's shorthand", () => {
    const forms: Record<string, string[]> = { 'Mechanitis messenoides': ['deceptus', 'intermedia'], 'Mechanitis polymnia': ['eurydice'] }
    const entries = speciesEntries(
      ['Ithomia salapia', 'Mechanitis messenoides', 'Mechanitis polymnia', 'Methona confusa'],
      s => forms[s] || [],
      [draft({ species: 'Methona confusa' })],
    )
    expect(entries.slice(0, 4).map(e => e.label)).toEqual([
      'Methona confusa',
      'Ithomia salapia',
      'Mechanitis messenoides',
      'Mechanitis messenoides deceptus',
    ])
    expect(searchSpecies(entries, 'deceptus').map(e => [e.species, e.form])).toEqual([['Mechanitis messenoides', 'deceptus']])
    expect(searchSpecies(entries, 'pol. eury').map(e => e.label)).toEqual(['Mechanitis polymnia eurydice'])
    expect(searchSpecies(entries, 'mech mess').map(e => e.label)).toEqual([
      'Mechanitis messenoides',
      'Mechanitis messenoides deceptus',
      'Mechanitis messenoides intermedia',
    ])
    expect(searchSpecies(entries, 'methona confusa')[0].label).toBe('Methona confusa')
    expect(searchSpecies(entries, '').length).toBe(entries.length)
  })
})
