import { describe, expect, it } from 'vitest'
import { parseBlock, parseCamTube, parseFate, parseSex, parseTime } from '../paste'

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
    expect(['♀', '♂', '?', 'female', 'M', ''].map(parseSex)).toEqual(['female', 'male', 'NA', 'female', 'male', ''])
    expect(
      ['Insectario', 'Al insectario', 'Preservada', 'pres', 'i', 'Collected_Sent2Insectary', 'Released_Unmarked', 'x'].map(parseFate),
    ).toEqual(['insectario', 'insectario', 'preservada', 'preservada', 'insectario', 'insectario', 'liberada', null])
    expect(parseCamTube('CAM079895 · FS90415305 (Flash frozen)')).toEqual({ cam: 'CAM079895', tube: 'FS90415305' })
    expect(parseCamTube('')).toEqual({ cam: '', tube: '' })
    expect(['11:02', '9:05', '1102', 'tarde'].map(parseTime)).toEqual(['11:02', '09:05', '11:02', 'tarde'])
  })
})
