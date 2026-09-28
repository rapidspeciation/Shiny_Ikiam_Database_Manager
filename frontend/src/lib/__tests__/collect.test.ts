import { describe, expect, it } from 'vitest'
import { misfit, summarize, type Draft } from '../collect'
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
