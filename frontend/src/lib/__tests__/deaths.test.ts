import { describe, expect, it } from 'vitest'
import {
  NOT_PRESERVED,
  bestRack,
  buildIndex,
  daysAlive,
  deathCells,
  factsOf,
  lifeOf,
  lookAlikes,
  rankCauses,
  suggest,
  type Getter,
} from '../deaths'
import type { CellValue, TableRow } from '../types'

let next = 1
function row(values: Record<string, CellValue>, extra: Partial<TableRow> = {}): TableRow {
  const n = next++
  return { id: `r${n}`, row: 100 + n, version: 1, observed: true, values, formulas: [], ...extra }
}
const saved: Getter = (r, f) => r.values[f] ?? null
const asObject = (cells: { field: string; value: CellValue }[]) => Object.fromEntries(cells.map(c => [c.field, c.value]))
const DAY = 46295 // 30-Sep-26

describe('what a death writes', () => {
  it('not preserved: date, cause and the NA / NOT_COLLECTED block, only in empty or NA cells', () => {
    const r = row({ Insectary_ID: 'B9D', Death_date: null, Death_cause: 'NA', Notes_Insectary_data: 'keep' })
    const cells = deathCells(r, saved, { serial: DAY, cause: 'Eaten', notPreserved: true })
    expect(asObject(cells)).toEqual({ Death_date: DAY, Death_cause: 'Eaten', ...NOT_PRESERVED })
    expect(cells.every(c => !c.overwrite)).toBe(true)
  })
  it('keeps what a row already has, never writes formula cells, and skips the block for Killed_Preserved', () => {
    const r = row(
      { Insectary_ID: 'C1D', Death_date: 46290, Death_cause: null, CAM_ID: null, Preserved_Dead_Alive: 'Dead' },
      { formulas: ['Location_body'] },
    )
    const cells = asObject(deathCells(r, saved, { serial: DAY, cause: 'Spider', notPreserved: true }))
    expect(cells.Death_date).toBeUndefined()
    expect(cells.Death_cause).toBe('Spider')
    expect(cells.Preserved_Dead_Alive).toBeUndefined()
    expect('Location_body' in cells).toBe(false)
    expect(Object.keys(deathCells(r, saved, { serial: DAY, cause: 'Killed_Preserved', notPreserved: true }))).toHaveLength(1)
    // A butterfly with a CAM (a wing clip) is never marked "not preserved".
    const clipped = row({ Insectary_ID: 'C2D', CAM_ID: 'CAM1', Tube_1_id: 'FS1' })
    expect(asObject(deathCells(clipped, saved, { serial: DAY, cause: 'Eaten', notPreserved: true }))).toEqual({
      Death_date: DAY,
      Death_cause: 'Eaten',
    })
  })
  it('matches the desktop fills cell by cell (the cause just written counts for the block)', () => {
    const edits: Record<string, CellValue> = { Death_cause: 'Unknown' }
    const r = row({ Insectary_ID: 'D1D', Death_cause: null })
    const pendingGet: Getter = (x, f) => (f in edits ? edits[f] : (x.values[f] ?? null))
    // The pending cause is already there: the date and the block are written, the cause is not.
    const cells = asObject(deathCells(r, pendingGet, { serial: DAY, cause: 'Eaten', notPreserved: true }))
    expect(cells.Death_cause).toBeUndefined()
    expect(cells.CAM_ID).toBe('NA')
    expect(deathCells(r, pendingGet, { serial: DAY, cause: '', notPreserved: false })).toEqual([{ field: 'Death_date', value: DAY }])
  })
  it('preserved: CAM, the whole body in the first free tube, its medium, the rest NA as Tubos writes it', () => {
    const r = row({ Insectary_ID: 'E1D', T1_Preservation_medium: 'NOT_COLLECTED' })
    const cells = deathCells(r, saved, {
      serial: DAY,
      cause: 'Killed_Preserved',
      notPreserved: false,
      preserve: { cam: 'CAM078312', tube: 'FS90415421', medium: 'Flash frozen' },
    })
    expect(asObject(cells)).toEqual({
      Death_date: DAY,
      Death_cause: 'Killed_Preserved',
      CAM_ID: 'CAM078312',
      Tube_1_id: 'FS90415421',
      Tube_1_tissue: 'WHOLE_ORGANISM',
      T1_Preservation_medium: 'Flash frozen',
      Preservation_date: DAY,
      Preserved_Dead_Alive: 'Alive',
      Preservation_medium: 'NOT_COLLECTED',
      Location_body: 'Ikiam',
      Tube_2_id: 'NA',
      Tube_2_tissue: 'NOT_COLLECTED',
      T2_Preservation_medium: 'NOT_COLLECTED',
      Tube_3_id: 'NA',
      Tube_3_tissue: 'NOT_COLLECTED',
      Tube_4_id: 'NA',
      Tube_4_tissue: 'NOT_COLLECTED',
    })
    // NOT_COLLECTED left by an earlier "not preserved" is replaced.
    expect(cells.find(c => c.field === 'T1_Preservation_medium')?.overwrite).toBe(true)
  })
  it('a wing-clipped butterfly found dead keeps its CAM; the body goes to Tube_2, "Dead"', () => {
    const r = row({
      Insectary_ID: 'F1D',
      CAM_ID: 'CAM070000',
      Tube_1_id: 'FS1',
      Tube_1_tissue: '**OTHER_SOMATIC_ANIMAL_TISSUE** | WING CLIP',
      T1_Preservation_medium: 'Flash frozen',
    })
    const cells = asObject(
      deathCells(r, saved, { serial: DAY, cause: 'Unknown', notPreserved: false, preserve: { cam: 'CAM9', tube: 'FS2', medium: 'Ethanol' } }),
    )
    expect(cells.CAM_ID).toBeUndefined()
    expect(cells.Tube_1_id).toBeUndefined()
    expect(cells).toMatchObject({ Tube_2_id: 'FS2', Tube_2_tissue: 'WHOLE_ORGANISM', T2_Preservation_medium: 'Ethanol', Preserved_Dead_Alive: 'Dead' })
    expect(cells.Tube_3_id).toBe('NA')
  })
})

describe('alive or dead', () => {
  const get = (values: Record<string, CellValue>) => (f: string) => values[f] ?? null
  it('blank death cells mean alive; a date or a cause means dead; a date NA says neither', () => {
    expect(lifeOf(get({})).state).toBe('alive')
    expect(lifeOf(get({ Death_date: 46290, Death_cause: 'Eaten' }))).toEqual({ state: 'dead', death: 46290, cause: 'Eaten' })
    expect(lifeOf(get({ Death_cause: 'Disappearance' })).state).toBe('dead')
    expect(lifeOf(get({ Death_date: 'NA' })).state).toBe('unknown')
  })
  it('counts the days from entering the insectary to death, or to today', () => {
    expect(daysAlive(get({ Intro2Insectary_date: DAY - 23 }), DAY)).toBe(23)
    expect(daysAlive(get({ Intro2Insectary_date: DAY - 23, Death_date: DAY - 3 }), DAY)).toBe(20)
    expect(daysAlive(get({ Intro2Insectary_date: 'NA' }), DAY)).toBeNull()
    const facts = factsOf(get({ SPECIES: 'Mechanitis', Sex: 'female', 'CLUTCH NUMBER': '994(6)', Wild_Reared: 'Reared' }), DAY)
    expect(facts).toMatchObject({ species: 'Mechanitis', sex: 'female', clutch: '994(6)', wild: false })
  })
})

describe('search', () => {
  const rows = [
    row({ Insectary_ID: 'B9', Death_date: 44620, Death_cause: 'Unknown' }),
    row({ Insectary_ID: 'B9D', CAM_ID: 'CAM078001' }),
    row({ Insectary_ID: 'B1D', Death_date: 46290, Death_cause: 'Eaten' }),
    row({ Insectary_ID: 'B1E' }),
    row({ Insectary_ID: 'AB9' }),
    row({ Insectary_ID: 'O0D', Tube_1_id: 'FS90415421' }),
    row({ Insectary_ID: null }),
    row({ Insectary_ID: 'B9D', Death_date: 1 }),
  ]
  const index = buildIndex(rows)
  const alive = (e: (typeof index)[number]) => lifeOf(f => e.row.values[f] ?? null).state === 'alive'
  const ids = (q: string, skip: string[] = []) => suggest(index, q, { alive, skip: new Set(skip) }).map(s => s.entry.id)
  it('indexes each ID once (the first row of a repeated one), without empty IDs', () => {
    expect(index.map(e => e.id)).toEqual(['B9', 'B9D', 'B1D', 'B1E', 'AB9', 'O0D'])
  })
  it('puts living butterflies first: B9 (dead in 2022) after B9D', () => {
    expect(ids('b9')).toEqual(['B9D', 'B9', 'AB9'])
    expect(ids('B1')).toEqual(['B1E', 'B1D'])
    expect(ids('b 9 d')).toEqual(['B9D'])
    expect(ids('B9', ['B9D'])).toEqual(['B9', 'AB9'])
  })
  it('finds a butterfly by its CAM or tube from three characters', () => {
    const found = suggest(index, 'fs9041', { alive })
    expect(found.map(s => [s.entry.id, s.via])).toEqual([['O0D', 'FS90415421']])
    expect(suggest(index, 'CAM078001', { alive })[0]).toMatchObject({ via: 'CAM078001' })
    expect(ids('')).toEqual([])
  })
  it('offers look-alike IDs: 0/O, swapped neighbours, a character too many, a missing last letter', () => {
    const known = new Map(index.map(e => [e.key, e.id]))
    expect(lookAlikes('00D', known)).toEqual(['O0D'])
    expect(lookAlikes('BD9', known)).toEqual(['B9D', 'B9'])
    expect(lookAlikes('8ID', known)).toEqual([])
    expect(lookAlikes('81D', known)).toEqual(['B1D'])
    expect(lookAlikes('B1', known)).toEqual(['B1D', 'B1E'])
    expect(lookAlikes('B1EE', known)).toEqual(['B1E'])
  })
})

describe('causes and racks', () => {
  it('orders the list by use in the last year, then as the list has it; only listed values', () => {
    const rows = [
      row({ Death_date: DAY - 1, Death_cause: 'Eaten' }),
      row({ Death_date: DAY - 2, Death_cause: 'Eaten' }),
      row({ Death_date: DAY - 3, Death_cause: 'Spider' }),
      row({ Death_date: DAY - 900, Death_cause: 'Other' }),
      row({ Death_date: DAY - 4, Death_cause: 'Mantis' }),
    ]
    expect(rankCauses(['Unknown', 'Spider', 'Eaten', 'Other', ''], rows, DAY)).toEqual(['Eaten', 'Spider', 'Unknown', 'Other'])
  })
  it('takes the insectary rack in the medium chosen, or the crosses rack for cross butterflies', () => {
    const racks = [
      { value: 'FS1', medium: 'Flash frozen', context: 'Cruces' },
      { value: 'FS2', medium: 'Ethanol', context: 'Colecta' },
      { value: 'FS3', medium: 'Flash frozen', context: 'Insectario' },
    ]
    expect(bestRack(racks, [row({})], 'Flash frozen')?.value).toBe('FS3')
    expect(bestRack(racks, [row({ Research_purpose: 'F1/F2 mutation rate' })], 'Flash frozen')?.value).toBe('FS1')
    // No insectary rack in ethanol: still the insectary's (as Tubos does), never the collections' rack.
    expect(bestRack(racks, [row({})], 'Ethanol')?.value).toBe('FS3')
  })
})
