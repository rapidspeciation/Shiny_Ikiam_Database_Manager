import { readFileSync } from 'node:fs'
import { join } from 'node:path'
import { describe, expect, it } from 'vitest'
import { formSections, phoneViewFor, sectionOf } from '../rowForm'
import { en } from '../../locales/en'
import type { Field } from '../types'

const schema = JSON.parse(readFileSync(join(__dirname, '..', '..', '..', '..', 'docs', 'workbook-schema.json'), 'utf8'))
const headers = (sheet: string): Field[] =>
  schema.sheets
    .find((s: { name: string }) => s.name === sheet)
    .headers.map((h: { name: string }) => ({ key: h.name, label: h.name, type: 'text' }) as Field)
const keysOf = (sections: ReturnType<typeof formSections>, key: string) =>
  sections.find(s => s.key === key)?.fields.map(f => f.key)

describe('phone view preference', () => {
  it('is the grid unless the form was chosen', () => {
    expect(phoneViewFor(null)).toBe('grid')
    expect(phoneViewFor('grid')).toBe('grid')
    expect(phoneViewFor('1')).toBe('grid')
    expect(phoneViewFor('form')).toBe('form')
  })
})

describe('form sections', () => {
  const insectary = formSections(headers('Insectary_data'), ['Insectary_ID', 'CAM_ID'])

  it('puts every Insectary_data column in one section, most used first', () => {
    expect(insectary.map(s => s.key)).toEqual(['id', 'dates', 'tubes', 'notes', 'storage', 'manifest'])
    expect(insectary.flatMap(s => s.fields).length).toBe(53)
    expect(keysOf(insectary, 'id')?.slice(0, 6)).toEqual(['Insectary_ID', 'Wild_Reared', 'CLUTCH NUMBER', 'Stock_of_origin', 'SPECIES', 'Sex'])
    expect(keysOf(insectary, 'id')).toContain('Collection_location')
    expect(keysOf(insectary, 'dates')).toEqual(['Intro2Insectary_date', 'Death_date', 'Death_cause', 'Preservation_date'])
    expect(keysOf(insectary, 'notes')).toEqual(['Photo_dorsal', 'Photo_ventral', 'Notes_Insectary_data'])
    expect(keysOf(insectary, 'storage')).toHaveLength(8)
    expect(keysOf(insectary, 'manifest')).toHaveLength(13)
  })

  it('folds only the formula and storage sections', () => {
    expect(insectary.filter(s => s.folded).map(s => s.key)).toEqual(['storage', 'manifest'])
  })

  it('tells the field columns from the manifest ones with the same words', () => {
    expect(sectionOf('Collection_location')).toBe('field')
    expect(sectionOf('COLLECTION_LOCATION')).toBe('manifest')
    expect(sectionOf('Collection_date')).toBe('field')
    expect(sectionOf('Preservation_date')).toBe('dates')
    expect(sectionOf('Tube_1_rack')).toBe('storage')
    expect(sectionOf('Location_body')).toBe('tubes')
    expect(sectionOf('Date', ['Date', 'Location'])).toBe('id')
  })

  it('sorts every Collection_data column', () => {
    const collection = formSections(headers('Collection_data'), ['CAM_ID', 'FieldMark_ID', 'Insectary_ID'])
    expect(collection.flatMap(s => s.fields).length).toBe(77)
    expect(keysOf(collection, 'other') ?? keysOf(collection, 'manifest')).toContain('Select_TEMP')
  })

  it('shows one plain list when all columns fall in one section', () => {
    const plain = formSections([{ key: 'A' }, { key: 'B' }] as Field[])
    expect(plain).toHaveLength(1)
    expect(plain[0].title).toBe('')
  })

  it('has English for every section title', () => {
    for (const s of [...insectary, ...formSections(headers('Insectary_stocks'))]) expect(en[s.title], s.title).toBeTruthy()
  })
})
