import { describe, expect, it } from 'vitest'
import {
  NOT_PRESERVED,
  bestRack,
  bodyReminder,
  buildIndex,
  cardCells,
  causeForKey,
  causesByToday,
  daysAlive,
  deathCells,
  diesBeforeEntry,
  factsOf,
  hasGap,
  hasSampleIds,
  lackOf,
  lifeOf,
  lookAlikes,
  noteCell,
  preservationGaps,
  priorDeath,
  rankCauses,
  replaceCells,
  replaceNote,
  suggest,
  usedSamples,
  type DeathChoice,
  type Getter,
} from '../deaths'
import { isBlank } from '../cells'
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
    // Unused tubes: ID NA, tissue and medium NOT_COLLECTED (Franz, 1 Oct 2026), CAM NA.
    const written = asObject(cells)
    for (const n of [1, 2, 3, 4]) {
      expect(written[`Tube_${n}_id`]).toBe('NA')
      expect(written[`Tube_${n}_tissue`]).toBe('NOT_COLLECTED')
    }
    expect(written).toMatchObject({ CAM_ID: 'NA', T1_Preservation_medium: 'NOT_COLLECTED', T2_Preservation_medium: 'NOT_COLLECTED' })
    // A tissue already typed NA (the habit until Sep 2026) takes NOT_COLLECTED; one with a value is kept.
    const typed = row({ Insectary_ID: 'B8D', Tube_1_tissue: 'NA', Tube_2_tissue: 'WHOLE_ORGANISM' })
    const again = asObject(deathCells(typed, saved, { serial: DAY, cause: 'Unknown', notPreserved: true }))
    expect(again.Tube_1_tissue).toBe('NOT_COLLECTED')
    expect('Tube_2_tissue' in again).toBe(false)
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
    // A butterfly whose body is in a tube is never marked "not preserved".
    const body = row({ Insectary_ID: 'C2D', CAM_ID: 'CAM1', Tube_1_id: 'FS1', Tube_1_tissue: 'WHOLE_ORGANISM' })
    expect(asObject(deathCells(body, saved, { serial: DAY, cause: 'Eaten', notPreserved: true }))).toEqual({
      Death_date: DAY,
      Death_cause: 'Eaten',
    })
  })
  it('a wing-clipped butterfly not preserved: the clip stays, the rest of the block is filled, with its purpose', () => {
    const values = {
      Insectary_ID: 'F1B',
      CAM_ID: 'CAM070000',
      Tube_1_id: 'FS1',
      Tube_1_tissue: '**OTHER_SOMATIC_ANIMAL_TISSUE** | WING CLIP',
      T1_Preservation_medium: 'Flash frozen',
    }
    const cells = asObject(deathCells(row(values), saved, { serial: DAY, cause: 'Ants', notPreserved: true }))
    expect(cells).toEqual({
      Death_date: DAY,
      Death_cause: 'Ants',
      Research_purpose: 'F1/F2 mutation rate',
      Preserved_Dead_Alive: 'NA',
      Tube_2_id: 'NA',
      Tube_2_tissue: 'NOT_COLLECTED',
      T2_Preservation_medium: 'NOT_COLLECTED',
      Tube_3_id: 'NA',
      Tube_3_tissue: 'NOT_COLLECTED',
      Tube_4_id: 'NA',
      Tube_4_tissue: 'NOT_COLLECTED',
      Preservation_medium: 'NOT_COLLECTED',
      Preservation_date: 'NA',
      Location_body: 'NA',
    })
    // The purpose chosen in Muertes, and one the row has already (set when it emerged) is kept.
    const chosen = deathCells(row(values), saved, { serial: DAY, cause: 'Ants', notPreserved: true, purpose: 'Pheromones' })
    expect(asObject(chosen).Research_purpose).toBe('Pheromones')
    const has = row({ ...values, Research_purpose: 'WEST x EAST polymnia crosses' })
    expect('Research_purpose' in asObject(deathCells(has, saved, { serial: DAY, cause: 'Ants', notPreserved: true }))).toBe(false)
  })
  it('the reminder to preserve a clipped butterfly: not when nothing is left of it', () => {
    const choice = (cause: string, note = '', preserved = false) => ({ cause, note, preserved })
    expect(['Unknown', 'Eaten', 'Heat stroke', 'Spider', ''].every(c => bodyReminder(choice(c)))).toBe(true)
    expect(['Disappearance', 'Ants', 'Unknown - Only wings'].some(c => bodyReminder(choice(c)))).toBe(false)
    expect(bodyReminder(choice('Unknown', 'Only wings found'))).toBe(false)
    expect(bodyReminder(choice('Unknown', '', true))).toBe(false)
  })
  it('the purpose: NA when not preserved, the one chosen for a body preserved now', () => {
    const plain = asObject(deathCells(row({ Insectary_ID: 'G1D' }), saved, { serial: DAY, cause: 'Unknown', notPreserved: true, purpose: 'Pheromones' }))
    expect(plain.Research_purpose).toBe('NA')
    const preserve = { cam: 'CAM9', tube: 'FS2', medium: 'Flash frozen' }
    const kept = asObject(deathCells(row({ Insectary_ID: 'G2D' }), saved, { serial: DAY, cause: 'Killed_Preserved', notPreserved: false, preserve }))
    expect('Research_purpose' in kept).toBe(false)
    const body = deathCells(row({ Insectary_ID: 'G3D' }), saved, { serial: DAY, cause: 'Killed_Preserved', notPreserved: false, preserve, purpose: 'Pheromones' })
    expect(asObject(body).Research_purpose).toBe('Pheromones')
  })
  it('the Tube 2 medium is written over its formula too; CAM_ID_CollData NA for a reared butterfly whose cell is typed', () => {
    const old = row({ Insectary_ID: 'D2B', Wild_Reared: 'Reared' }, { formulas: ['T2_Preservation_medium'] })
    const cells = asObject(deathCells(old, saved, { serial: DAY, cause: 'Heat stroke', notPreserved: true }))
    expect(cells).toMatchObject({ T2_Preservation_medium: 'NOT_COLLECTED', CAM_ID_CollData: 'NA' })
    // A newer row: the cell is a formula there. A wild-caught one has a Collection_data row.
    const newer = row({ Insectary_ID: 'J1E', Wild_Reared: 'Reared' }, { formulas: ['CAM_ID_CollData'] })
    expect('CAM_ID_CollData' in asObject(deathCells(newer, saved, { serial: DAY, cause: 'Unknown', notPreserved: true }))).toBe(false)
    const wild = row({ Insectary_ID: 'K1E', Wild_Reared: 'Wild-caught' })
    expect('CAM_ID_CollData' in asObject(deathCells(wild, saved, { serial: DAY, cause: 'Unknown', notPreserved: true }))).toBe(false)
    // Nothing of the death to write: nothing else either.
    const done = row({ Insectary_ID: 'L1E', Wild_Reared: 'Reared', Death_date: DAY, Death_cause: 'Killed_Preserved' })
    expect(deathCells(done, saved, { serial: DAY, cause: 'Killed_Preserved', notPreserved: false })).toEqual([])
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
  // Every cell of a preserved body, the same as Tubos writes it: tubes.test.
  it('preserved over the NOT_COLLECTED an earlier "not preserved" left: the medium is replaced', () => {
    const r = row({ Insectary_ID: 'E1D', T1_Preservation_medium: 'NOT_COLLECTED' })
    const cells = deathCells(r, saved, {
      serial: DAY,
      cause: 'Killed_Preserved',
      notPreserved: false,
      preserve: { cam: 'CAM078312', tube: 'FS90415421', medium: 'Flash frozen' },
    })
    expect(cells.find(c => c.field === 'T1_Preservation_medium')).toEqual({ field: 'T1_Preservation_medium', value: 'Flash frozen', overwrite: true })
    expect(asObject(cells)).toMatchObject({ Death_cause: 'Killed_Preserved', Tube_1_id: 'FS90415421', Preserved_Dead_Alive: 'Alive' })
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

describe('what a butterfly being preserved still lacks', () => {
  it('a CAM and a tube each, no repeats, a free slot; a CAM it has is kept', () => {
    const a = row({ Insectary_ID: 'G1D' })
    const b = row({ Insectary_ID: 'G2D', CAM_ID: 'CAM070001', Tube_1_id: 'FS1', Tube_1_tissue: 'WING' })
    const c = row({ Insectary_ID: 'G3D' })
    const full = row({ Insectary_ID: 'G4D', CAM_ID: 'CAM2', Tube_1_id: 'FS2', Tube_2_id: 'NA' })
    const gaps = preservationGaps([a, b, c, full], saved, {
      G1D: { cam: 'cam078001', tube: 'FS9' },
      G2D: { cam: '', tube: '' },
      G3D: { cam: 'CAM078001 ', tube: '' },
    })
    expect(gaps.map(g => [g.id, g.slot, g.keepsCam, g.cam, g.tube])).toEqual([
      ['G1D', 1, false, '', ''],
      ['G2D', 2, true, '', 'missing'],
      ['G3D', 1, false, 'repeated', 'missing'],
      ['G4D', null, true, '', 'missing'],
    ])
    expect(gaps[2]).toMatchObject({ with: 'G1D', value: 'CAM078001' })
    expect(gaps.map(hasGap)).toEqual([false, true, true, true])
  })
  it('a CAM or tube another butterfly has in the sheet is "repeated" (its own is fine)', () => {
    const owner = row({ Insectary_ID: 'H1D', CAM_ID: 'CAM078500', Tube_1_id: 'FS90415999' })
    const dying = row({ Insectary_ID: 'H2D' })
    const used = usedSamples(buildIndex([owner, dying]))
    expect(used.get('FS90415999')).toBe('H1D')
    const [gap] = preservationGaps([dying], saved, { H2D: { cam: 'CAM078501', tube: 'fs90415999' } }, used)
    expect(gap).toMatchObject({ cam: '', tube: 'repeated', with: 'H1D', value: 'FS90415999' })
    const [free] = preservationGaps([dying], saved, { H2D: { cam: 'CAM078501', tube: 'FS90416001' } }, used)
    expect(hasGap(free)).toBe(false)
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
    // The insectary's own rack weeks behind the crosses' one: the rack in use (one rack for both, Oct 2026).
    const dated = [
      { value: 'FS90415493', medium: 'Flash frozen', context: 'Cruces', date: 46297 },
      { value: 'FS63714724', medium: 'Flash frozen', context: 'Insectario', date: 46226 },
    ]
    expect(bestRack(dated, [row({})], 'Flash frozen')?.value).toBe('FS90415493')
    expect(bestRack([{ ...dated[0], date: 46230 }, dated[1]], [row({})], 'Flash frozen')?.value).toBe('FS63714724')
  })
})

describe('each card its own date, cause, preservation and note', () => {
  const all: DeathChoice = { date: '2026-09-30', cause: 'Unknown', preserved: false, note: '' }
  it('recording writes each card\'s own values', () => {
    const a = row({ Insectary_ID: 'B7A' })
    const b = row({ Insectary_ID: 'C8B' })
    const cellsA = asObject(cardCells(a, saved, all, { medium: 'Flash frozen', today: DAY }))
    expect(cellsA).toEqual({ Death_date: DAY, Death_cause: 'Unknown', ...NOT_PRESERVED })
    const own: DeathChoice = { ...all, cause: 'Killed_Preserved', preserved: true, date: '2026-09-29' }
    const cellsB = asObject(cardCells(b, saved, own, { sample: { cam: ' cam1 ', tube: 'fs9' }, medium: 'Flash frozen', today: DAY }))
    expect(cellsB).toMatchObject({
      Death_date: DAY - 1,
      Death_cause: 'Killed_Preserved',
      CAM_ID: 'CAM1',
      Tube_1_id: 'FS9',
      Tube_1_tissue: 'WHOLE_ORGANISM',
      T1_Preservation_medium: 'Flash frozen',
      Preservation_date: DAY - 1,
    })
    // Already recorded dead: no tube from here, only what is missing.
    const dead = row({ Insectary_ID: 'E2E', Death_date: DAY - 5, Death_cause: 'Eaten' })
    expect(cardCells(dead, saved, { ...all, preserved: true }, { sample: { cam: 'CAM2', tube: 'FS1' }, medium: 'Ethanol', today: DAY })).toEqual([])
  })
  it('the note: added after the old ones, dated and initialled; empty writes nothing', () => {
    const a = row({ Insectary_ID: 'B7A', Notes_Insectary_data: '29/9/26 MJS: marked with lines in the abdomen' })
    const b = row({ Insectary_ID: 'C8B', Notes_Insectary_data: null })
    const dead = row({ Insectary_ID: 'E2E', Death_date: DAY - 5, Death_cause: 'Eaten', CAM_ID: 'CAM9', Tube_1_id: 'FS2', Notes_Insectary_data: 'NA' })
    const note = (r: TableRow, text: string) =>
      asObject(cardCells(r, saved, { ...all, note: text }, { medium: 'Ethanol', today: DAY + 1, initials: 'FCH' })).Notes_Insectary_data
    expect(note(a, 'Only wings found')).toBe('29/9/26 MJS: marked with lines in the abdomen | 1/10/26 FCH: Only wings found')
    expect(note(b, '  Head eaten ')).toBe('1/10/26 FCH: Head eaten')
    expect(note(a, '')).toBeUndefined()
    // Already recorded dead: only the note is written ("NA" is no note to keep).
    expect(cardCells(dead, saved, { ...all, note: 'Only wings found' }, { medium: 'Ethanol', today: DAY + 1, initials: 'FCH' })).toEqual([
      { field: 'Notes_Insectary_data', value: '1/10/26 FCH: Only wings found', overwrite: true },
    ])
    expect(cardCells(dead, saved, all, { medium: 'Ethanol', today: DAY + 1, initials: 'FCH' })).toEqual([])
    // A blank note writes nothing; a formula cell is never written.
    expect(noteCell(b, saved, '   ', DAY, 'FCH')).toBeNull()
    expect(noteCell(row({ Notes_Insectary_data: 'x' }, { formulas: ['Notes_Insectary_data'] }), saved, 'Weak', DAY, 'FCH')).toBeNull()
  })
  it('what a butterfly still lacks: a date, a valid date, a cause while dying, its CAM or tube', () => {
    const c = (over: Partial<DeathChoice> = {}): DeathChoice => ({ date: '2026-10-05', cause: 'Natural', preserved: false, note: '', ...over })
    expect(lackOf(c(), true)).toBe('')
    expect(lackOf(c({ date: '' }), true)).toBe('date')
    expect(lackOf(c({ date: '1890-01-01' }), true)).toBe('bad-date')
    expect(lackOf(c({ cause: '' }), true)).toBe('cause')
    // Already dead: a note alone is enough.
    expect(lackOf(c({ cause: '', note: 'Head eaten' }), false)).toBe('')
    const gap = { id: 'A1B', slot: 1, keepsCam: false, cam: 'missing' as const, tube: '' as const }
    expect(lackOf(c({ preserved: true }), true, gap)).toBe('sample')
    expect(lackOf(c({ preserved: true }), true, { ...gap, cam: '' })).toBe('')
  })
})

describe('the cause buttons', () => {
  const list = ['Unknown', 'Heat stroke', 'Eaten', 'Disappearance', 'Spider']

  it("today's causes first, most first, each with its count; the rest keep their order", () => {
    const today = new Map([
      ['Eaten', 2],
      ['Spider', 5],
      ['Disappearance', 2],
    ])
    expect(causesByToday(list, today)).toEqual([
      { cause: 'Spider', today: 5 },
      { cause: 'Eaten', today: 2 },
      { cause: 'Disappearance', today: 2 },
      { cause: 'Unknown', today: 0 },
      { cause: 'Heat stroke', today: 0 },
    ])
    // Nothing recorded today: as they were. A cause not among the buttons is not added.
    expect(causesByToday(list, new Map()).map(c => c.cause)).toEqual(list)
    expect(causesByToday(list, new Map([['Old cause', 3]])).map(c => c.cause)).toEqual(list)
  })

  it('keys 1–9 pick the button in that place', () => {
    const shown = causesByToday(list, new Map([['Spider', 1]])).map(c => c.cause)
    expect(causeForKey('1', shown)).toBe('Spider')
    expect(causeForKey('3', shown)).toBe('Heat stroke')
    expect(causeForKey('5', shown)).toBe('Disappearance')
    expect(causeForKey('6', shown)).toBeNull()
    expect(causeForKey('0', shown)).toBeNull()
    expect(causeForKey('a', shown)).toBeNull()
    expect(causeForKey('12', shown)).toBeNull()
  })
})

describe('a butterfly already dead in the sheet: its death replaced', () => {
  const SEP3 = 46268 // 3-Sep-26
  const choice: DeathChoice = { date: '2026-09-30', cause: 'Heat stroke', preserved: false, note: '' }
  const notPreservedBlock = (extra: Record<string, CellValue> = {}) =>
    row({ Insectary_ID: 'B9', Death_date: SEP3, Death_cause: 'Unknown', ...NOT_PRESERVED, Notes_Insectary_data: 'old note', ...extra })

  it('knows the death it has, and whether the row holds a CAM or tube', () => {
    const r = notPreservedBlock()
    expect(priorDeath(f => r.values[f] ?? null)).toEqual({ date: SEP3, cause: 'Unknown' })
    expect(priorDeath(() => null)).toBeNull()
    expect(hasSampleIds(f => r.values[f] ?? null)).toBe(false)
    expect(hasSampleIds(f => (f === 'CAM_ID' ? 'CAM078001' : null))).toBe(true)
  })

  it('the note says what it replaces, in English', () => {
    expect(replaceNote({ date: SEP3, cause: 'Unknown' }, DAY, DAY)).toBe(
      'Found dead today; replaces the death recorded on 3/9/26 (Unknown), probably a misread ID',
    )
    expect(replaceNote({ date: null, cause: 'Eaten' }, DAY - 1, DAY)).toBe(
      'Found dead on 29/9/26; replaces the death recorded (Eaten), probably a misread ID',
    )
    expect(replaceNote({ date: SEP3, cause: '' }, DAY, DAY)).toBe('Found dead today; replaces the death recorded on 3/9/26, probably a misread ID')
  })

  it('writes the new date and cause over the old, and the note after the notes there, dated and signed', () => {
    const r = notPreservedBlock()
    const cells = replaceCells(r, saved, { ...choice, note: 'Head eaten' }, { medium: 'Flash frozen', today: DAY, initials: 'FCH' })
    expect(cells).toEqual([
      { field: 'Death_date', value: DAY, overwrite: true },
      { field: 'Death_cause', value: 'Heat stroke', overwrite: true },
      {
        field: 'Notes_Insectary_data',
        value: 'old note | 30/9/26 FCH: Found dead today; replaces the death recorded on 3/9/26 (Unknown), probably a misread ID; Head eaten',
        overwrite: true,
      },
    ])
  })

  it('preserved now, the row without a CAM or tube: the body goes in a tube over the old NA / NOT_COLLECTED block', () => {
    const r = notPreservedBlock()
    const how = { sample: { cam: 'cam078001', tube: 'fs12' }, medium: 'Flash frozen', today: DAY }
    const cells = replaceCells(r, saved, { ...choice, preserved: true }, how)
    expect(asObject(cells)).toMatchObject({
      Death_date: DAY,
      Death_cause: 'Heat stroke',
      CAM_ID: 'CAM078001',
      Tube_1_id: 'FS12',
      Tube_1_tissue: 'WHOLE_ORGANISM',
      T1_Preservation_medium: 'Flash frozen',
      Preservation_date: DAY,
      Preserved_Dead_Alive: 'Dead',
      Location_body: 'Ikiam',
    })
    // Each cell over a value the sheet has is marked to overwrite it.
    expect(cells.filter(c => !isBlank(r.values[c.field])).every(c => c.overwrite)).toBe(true)
  })

  it('a row preserved before keeps its CAM and tubes', () => {
    const r = row({ Insectary_ID: 'B9', Death_date: SEP3, Death_cause: 'Killed_Preserved', CAM_ID: 'CAM070001', Tube_1_id: 'FS3' })
    const how = { sample: { cam: 'CAM1', tube: 'FS1' }, medium: 'Ethanol', today: DAY }
    expect(Object.keys(asObject(replaceCells(r, saved, { ...choice, preserved: true }, how)))).toEqual([
      'Death_date',
      'Death_cause',
      'Notes_Insectary_data',
    ])
  })

  it('a death date before it entered the insectary is flagged', () => {
    expect(diesBeforeEntry('2026-09-30', DAY + 1)).toBe(true)
    expect(diesBeforeEntry('2026-09-30', DAY)).toBe(false)
    expect(diesBeforeEntry('2026-09-30', null)).toBe(false)
    expect(diesBeforeEntry('', DAY)).toBe(false)
  })
})
