import { describe, expect, it } from 'vitest'
import { complete, parseBlock, parseCamTube, parseFate, parseSex, parseTime } from '../paste'

describe('pasting cells from a spreadsheet', () => {
  it('splits rows and cells, and leaves a single value to the input', () => {
    expect(parseBlock('Mechanitis polymnia\tproceriformis\r\nIthomia salapia\tsalapia\r\n')).toEqual([
      ['Mechanitis polymnia', 'proceriformis'],
      ['Ithomia salapia', 'salapia'],
    ])
    expect(parseBlock('Ithomia salapia')).toBeNull()
    // A single cell copied from a spreadsheet ends in a line break: still one value.
    expect(parseBlock('Ithomia salapia\n')).toBeNull()
  })
  it('reads sex, fate, CAM and tube, and time as the team writes them', () => {
    expect(['♀', '♂', '?', 'female', 'M', '', 'he', 'hem', 'fe', 'ma', 'mac', 'NA', 'x'].map(parseSex)).toEqual([
      'female', 'male', 'NOT_COLLECTED', 'female', 'male', '', 'female', 'female', 'female', 'male', 'male', 'NOT_COLLECTED', '',
    ])
    // Unsure sex, as the team writes it in Collection_data.
    expect(['female ?', 'female?', 'female_?', 'm ?', '♀?', 'not', 'NOT_COLLECTED'].map(parseSex)).toEqual([
      'female ?', 'female ?', 'female ?', 'male ?', 'female ?', 'NOT_COLLECTED', 'NOT_COLLECTED',
    ])
    expect(
      ['Insectario', 'Al insectario', 'Preservada', 'pres', 'i', 'Collected_Sent2Insectary', 'Released_Unmarked', 'x'].map(parseFate),
    ).toEqual(['insectario', 'insectario', 'preservada', 'preservada', 'insectario', 'insectario', 'liberada', null])
    expect(['col_p', 'collected_s', 'collected_', 'rel', 'Collected_Preserved'].map(parseFate)).toEqual([
      null, 'insectario', null, 'liberada', 'preservada',
    ])
    expect(parseCamTube('CAM079895 · FS90415305 (Flash frozen)')).toEqual({ cam: 'CAM079895', tube: 'FS90415305' })
    expect(parseCamTube('')).toEqual({ cam: '', tube: '' })
    expect(['11:02', '9:05', '1102', 'tarde'].map(parseTime)).toEqual(['11:02', '09:05', '11:02', 'tarde'])
  })
  it('completes a typed start to the only option it begins', () => {
    const species = ['Ithomia salapia', 'Ithomia agnosia', 'Mechanitis polymnia']
    expect(complete('Ithomia sal', species)).toBe('Ithomia salapia')
    expect(complete('ithomia AGNOSIA', species)).toBe('Ithomia agnosia')
    // Several begin that way, or none: kept as typed (a new name can be entered).
    expect(complete('Ithomia', species)).toBe('Ithomia')
    expect(complete('Oleria onega', species)).toBe('Oleria onega')
  })
})
