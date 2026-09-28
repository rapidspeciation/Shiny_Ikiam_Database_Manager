import { describe, expect, it } from 'vitest'
import { misfit } from '../collect'
import { complete, pickChoice } from '../paste'

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
