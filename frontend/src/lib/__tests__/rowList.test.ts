import { describe, expect, it } from 'vitest'
import { markParts, rowListItem, snippet } from '../rowList'
import { matchRanges } from '../search'
import type { Field } from '../types'

const field = (key: string, type: Field['type'] = 'text'): Field => ({ key, label: key, type })
const INSECTARY = [
  field('Insectary_ID'),
  field('Wild_Reared'),
  field('CLUTCH NUMBER'),
  field('SPECIES'),
  field('Sex'),
  field('Death_date', 'date'),
  field('CAM_ID'),
  field('Notes_Insectary_data'),
]
const row = (values: Record<string, string | number | null>, at = 13687) => ({ id: `r${at}`, row: at, values })

describe("a row's line in the phone list", () => {
  it('names the row by its ID and species, with its sex, clutch and CAM', () => {
    const item = rowListItem(
      row({ Insectary_ID: 'Q5E', Wild_Reared: 'Reared', 'CLUTCH NUMBER': '1012(2)', SPECIES: 'Mechanitis lysimnia', Sex: 'female', CAM_ID: 'CAM078415' }),
      INSECTARY,
      ['Insectary_ID'],
    )
    expect(item).toMatchObject({ id: 'r13687', row: 13687, title: 'Q5E', species: 'Mechanitis lysimnia', match: null, isNew: false })
    expect(item.facts).toEqual(['female', 'clutch 1012(2)', 'CAM078415'])
  })
  it('never shows the sheet\'s placeholders (NA, NOT_COLLECTED) as facts', () => {
    const item = rowListItem(row({ Insectary_ID: 'Q6E', SPECIES: 'NA', Sex: 'NA', 'CLUTCH NUMBER': 'NOT_COLLECTED', CAM_ID: 'NA', Wild_Reared: 'Wild' }), INSECTARY, ['Insectary_ID'])
    expect(item.species).toBe('')
    // Nothing of the usual facts: the first short cells after the ID say what the row is.
    expect(item.facts).toEqual(['Wild'])
  })
  it('a sheet without an ID column names the row by its first short cell', () => {
    const columns = [field('File'), field('Kind'), field('Capture Date', 'date'), field('URL')]
    const item = rowListItem(row({ File: 'CAM074065v2.HEIC', Kind: 'File', 'Capture Date': 45388, URL: 'https://drive.google.com/file/d/x' }), columns, ['CAM_ID'])
    expect(item.title).toBe('CAM074065v2.HEIC')
    // A date reads in the interface language (Spanish here), not as the grid writes it.
    expect(item.facts).toEqual(['File', '6-Abr-24'])
  })
  it('a match in a date is found and shown as the sheet writes it, and said once', () => {
    const columns = [field('File'), field('Kind'), field('Capture Date', 'date')]
    const item = rowListItem(row({ File: 'a.jpg', Kind: 'File', 'Capture Date': 45388 }), columns, [], 'apr', ['Capture Date'])
    expect(item.match).toEqual({ field: 'Capture Date', text: '6-Apr-24' })
    expect(item.facts).toEqual(['File'])
  })
  it('shows the cell where the searched text is, once, unless the ID or the species already shows it', () => {
    const values = { Insectary_ID: 'Q5E', SPECIES: 'Mechanitis lysimnia', Sex: 'female', Notes_Insectary_data: '25/11/26 L: Ala derecha rota; encontrada en el piso de la jaula 3' }
    const inNote = rowListItem(row(values), INSECTARY, ['Insectary_ID'], 'jaula', ['Notes_Insectary_data'])
    expect(inNote.match?.field).toBe('Notes_Insectary_data')
    expect(inNote.match?.text).toContain('jaula 3')
    expect(rowListItem(row(values), INSECTARY, ['Insectary_ID'], 'q5e').match).toBeNull()
    expect(rowListItem(row(values), INSECTARY, ['Insectary_ID'], 'lysimnia').match).toBeNull()
    // The fact that is the match is said once, with its column.
    const bySex = rowListItem(row(values), INSECTARY, ['Insectary_ID'], 'female', ['Sex'])
    expect(bySex.match).toEqual({ field: 'Sex', text: 'female' })
    expect(bySex.facts).not.toContain('female')
  })
})

describe('the searched text in a line', () => {
  it('is marked wherever it is, whatever its case', () => {
    expect(markParts('Mechanitis menapis', 'MENA')).toEqual([
      { text: 'Mechanitis ', hit: false },
      { text: 'mena', hit: true },
      { text: 'pis', hit: false },
    ])
    expect(markParts('Q5E', '')).toEqual([{ text: 'Q5E', hit: false }])
    expect(markParts('aXa', 'a').filter(p => p.hit)).toHaveLength(2)
  })
  it('a long cell is cut around it', () => {
    const long = `${'x'.repeat(120)} jaula ${'y'.repeat(120)}`
    const cut = snippet(long, 'jaula', 40)
    expect(cut.length).toBeLessThanOrEqual(42)
    expect(cut).toContain('jaula')
    expect(cut.startsWith('…') && cut.endsWith('…')).toBe(true)
    expect(snippet('short', 'sh')).toBe('short')
  })
  it('the whole searched text stays in, however long', () => {
    const query = 'Preserved alive 3rd instar larva, wing torn and found on the floor of cage three'
    const cell = `5/10/26 FCH: ${query}; then moved to the freezer and recorded in the notebook page 41`
    expect(snippet(cell, query)).toContain(query)
  })
  it('a link is cut short around the match: it is not read, only where the match is', () => {
    const url = 'https://drive.google.com/file/d/14x2tHm_M2PXQ5EQOqpGyvPmXuiEtBxgd/view?usp=drivesdk'
    const cut = snippet(url, 'q5e')
    expect(cut.length).toBeLessThanOrEqual(36)
    expect(cut).toContain('Q5E')
  })
})

describe('reading the next matches for the list', () => {
  it('takes the newest not read yet, neighbours in one range', () => {
    const matches = [10, 11, 12, 500, 505, 9000]
    expect(matchRanges(matches, new Set([9000]), 3)).toEqual([
      { from: 12, to: 12 },
      { from: 500, to: 505 },
    ])
    expect(matchRanges(matches, new Set(matches))).toEqual([])
  })
  it('no range is longer than the server gives at once', () => {
    const dense = Array.from({ length: 30 }, (_, i) => 1000 + i * 25)
    for (const r of matchRanges(dense, new Set(), 30, 30, 200)) expect(r.to - r.from).toBeLessThan(200)
    expect(matchRanges(dense, new Set(), 30, 30, 200).length).toBeGreaterThan(1)
  })
})
