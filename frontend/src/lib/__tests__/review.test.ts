import { describe, expect, it } from 'vitest'
import { cropStyle, differingCells, fixText, otherField, photoList, photoUrl, type SideBySide } from '../review'

describe('envelope and wing crops', () => {
  it('scales and moves the photo so only the box shows, in a frame of its shape', () => {
    // The envelope at x 100–500, y 200–800 of a 1600×1200 photo.
    const { frame, image } = cropStyle([0.0625, 0.1667, 0.3125, 0.6667], 1600 / 1200)
    expect(image).toEqual({ width: '400%', height: '200%', left: '-25%', top: '-33.34%' })
    expect(frame.aspectRatio).toBe('0.667')
    expect('transform' in frame).toBe(false)
  })
  it('turns the frame of an envelope photographed upside down, and clamps boxes to the photo', () => {
    expect(cropStyle([0, 0, 1, 1], 1.5, 180).frame).toEqual({ aspectRatio: '1.5', transform: 'rotate(180deg)' })
    expect(cropStyle([-0.2, 0, 1.3, 1], 1).image).toEqual({ width: '100%', height: '100%', left: '0%', top: '0%' })
  })
})

describe('rows side by side', () => {
  const table: SideBySide = {
    fields: ['SPECIES', 'Sex', 'CAM_ID'],
    compare: ['SPECIES', 'Sex'],
    rows: [
      {
        sheet: 'Insectary_data',
        row: 2,
        recordId: 'a',
        label: 'N5D',
        values: { SPECIES: 'Mechanitis lysimnia', Sex: 'male', CAM_ID: 'CAM1' },
      },
      {
        sheet: 'Collection_data',
        row: 2,
        recordId: 'b',
        label: 'CAM1',
        values: { SPECIES: 'Ithomia salapia', Sex: 'Male ', CAM_ID: 'CAM1' },
      },
    ],
  }
  it('marks the compared cells that differ between the rows (not case or spaces)', () => {
    expect([...differingCells(table)]).toEqual(['0:SPECIES', '1:SPECIES'])
  })
  it('marks the issue column of a single row', () => {
    expect([...differingCells({ ...table, rows: table.rows.slice(0, 1) }, 'Sex')]).toEqual(['0:Sex'])
  })
})

describe('texts of a card', () => {
  it('says the fix or the task, and where another value goes', () => {
    expect(fixText({ fix: { recordId: 'a', values: { Sex: 'male' } }, fixNote: 'sexo del sobre' })).toBe(
      'Sex → male (sexo del sobre)',
    )
    const task = { type: 'rename', from: 'CAM1', to: 'CAM2', files: [], text: 'Renombrar' }
    expect(fixText({ task })).toBe('Renombrar')
    expect(otherField({ field: 'SPECIES', task })).toBe('CAM correcto')
    expect(otherField({ field: 'CAM_ID', fix: { recordId: 'a', values: { Sex: 'x' } } })).toBe('Sex')
    expect(otherField({ field: 'SPECIES' })).toBe('SPECIES')
  })
  it('lists photos dorsal first, with their names and wing boxes, and asks the server for them', () => {
    const list = photoList({
      dorsal: ['d1'],
      ventral: ['v1', 'v2'],
      files: { d1: { name: 'CAM1d', wings: [0, 0, 1, 1] }, v1: { name: 'CAM1v' } },
    })
    expect(list.map(p => p.name)).toEqual(['CAM1d', 'CAM1v', 'v2'])
    expect(list[0].wings).toEqual([0, 0, 1, 1])
    expect(photoUrl('ab-c_1', 1600)).toBe('api/photo/ab-c_1?w=1600')
  })
})
