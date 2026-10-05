import { describe, expect, it } from 'vitest'
import {
  NOT_PRESERVED,
  bestRack,
  buildIndex,
  cardCells,
  choiceFor,
  daysAlive,
  deathCells,
  factsOf,
  hasGap,
  lifeOf,
  lookAlikes,
  noteCell,
  preservationGaps,
  rankCauses,
  setChoice,
  sharedChoice,
  keepOwn,
  suggest,
  usedSamples,
  type DeathChoice,
  type Getter,
  type OwnChoices,
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
  it('nothing selected: the panel sets every card (and their own values of that field go)', () => {
    let state = { all, own: { B7A: { cause: 'Eaten' }, C8B: { cause: 'Spider', date: '2026-09-29' } } as OwnChoices }
    state = setChoice(state.all, state.own, [], 'cause', 'Disappearance')
    expect(state.all.cause).toBe('Disappearance')
    expect(state.own).toEqual({ C8B: { date: '2026-09-29' } })
    expect(choiceFor(state.all, state.own, 'B7A')).toEqual({ date: '2026-09-30', cause: 'Disappearance', preserved: false, note: '' })
    expect(choiceFor(state.all, state.own, 'C8B')).toEqual({ date: '2026-09-29', cause: 'Disappearance', preserved: false, note: '' })
  })
  it('cards selected: only theirs change; the panel\'s own value is not kept as theirs', () => {
    let state = { all, own: {} as OwnChoices }
    state = setChoice(state.all, state.own, ['B7A'], 'cause', 'Eaten')
    state = setChoice(state.all, state.own, ['B7A', 'D1C'], 'preserved', true)
    expect(state.all).toEqual(all)
    expect(state.own).toEqual({ B7A: { cause: 'Eaten', preserved: true }, D1C: { preserved: true } })
    expect(choiceFor(state.all, state.own, 'C8B')).toEqual(all)
    expect(sharedChoice(state.all, state.own, ['B7A', 'D1C'], 'preserved')).toBe(true)
    expect(sharedChoice(state.all, state.own, ['B7A', 'D1C'], 'cause')).toBeUndefined()
    expect(sharedChoice(state.all, state.own, [], 'cause')).toBeUndefined()
    // Back to the panel's cause: no longer its own.
    state = setChoice(state.all, state.own, ['B7A'], 'cause', 'Unknown')
    expect(state.own.B7A).toEqual({ preserved: true })
    expect(keepOwn(state.own, ['D1C'])).toEqual({ D1C: { preserved: true } })
  })
  it('Save writes each card\'s own values', () => {
    const a = row({ Insectary_ID: 'B7A' })
    const b = row({ Insectary_ID: 'C8B' })
    const own: OwnChoices = { C8B: { cause: 'Killed_Preserved', preserved: true, date: '2026-09-29' } }
    const cellsA = asObject(cardCells(a, saved, choiceFor(all, own, 'B7A'), { medium: 'Flash frozen', today: DAY }))
    expect(cellsA).toEqual({ Death_date: DAY, Death_cause: 'Unknown', ...NOT_PRESERVED })
    const cellsB = asObject(
      cardCells(b, saved, choiceFor(all, own, 'C8B'), { sample: { cam: ' cam1 ', tube: 'fs9' }, medium: 'Flash frozen', today: DAY }),
    )
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
  it('the note: added after the old ones, dated and initialled; own or for all; empty writes nothing', () => {
    const a = row({ Insectary_ID: 'B7A', Notes_Insectary_data: '29/9/26 MJS: marked with lines in the abdomen' })
    const b = row({ Insectary_ID: 'C8B', Notes_Insectary_data: null })
    const dead = row({ Insectary_ID: 'E2E', Death_date: DAY - 5, Death_cause: 'Eaten', CAM_ID: 'CAM9', Tube_1_id: 'FS2', Notes_Insectary_data: 'NA' })
    // For all cards (nothing selected).
    let state = setChoice(all, {}, [], 'note', 'Only wings found')
    const note = (r: TableRow, id: string) =>
      asObject(cardCells(r, saved, choiceFor(state.all, state.own, id), { medium: 'Ethanol', today: DAY + 1, initials: 'FCH' }))
        .Notes_Insectary_data
    expect(note(a, 'B7A')).toBe('29/9/26 MJS: marked with lines in the abdomen | 1/10/26 FCH: Only wings found')
    expect(note(b, 'C8B')).toBe('1/10/26 FCH: Only wings found')
    // Already recorded dead: only the note is written ("NA" is no note to keep).
    expect(cardCells(dead, saved, choiceFor(state.all, state.own, 'E2E'), { medium: 'Ethanol', today: DAY + 1, initials: 'FCH' })).toEqual([
      { field: 'Notes_Insectary_data', value: '1/10/26 FCH: Only wings found', overwrite: true },
    ])
    // B7A selected: its own note; C8B keeps the panel's.
    state = setChoice(state.all, state.own, ['B7A'], 'note', '  Head eaten ')
    expect(state.own).toEqual({ B7A: { note: '  Head eaten ' } })
    expect(note(a, 'B7A')).toBe('29/9/26 MJS: marked with lines in the abdomen | 1/10/26 FCH: Head eaten')
    expect(note(b, 'C8B')).toBe('1/10/26 FCH: Only wings found')
    // The panel's note emptied for all: B7A keeps nothing of its own either, and no note is written.
    state = setChoice(state.all, state.own, [], 'note', '')
    expect(state.own).toEqual({})
    expect(note(a, 'B7A')).toBeUndefined()
    expect(cardCells(dead, saved, choiceFor(state.all, state.own, 'E2E'), { medium: 'Ethanol', today: DAY + 1, initials: 'FCH' })).toEqual([])
    // A blank note writes nothing; a formula cell is never written.
    expect(noteCell(b, saved, '   ', DAY, 'FCH')).toBeNull()
    expect(noteCell(row({ Notes_Insectary_data: 'x' }, { formulas: ['Notes_Insectary_data'] }), saved, 'Weak', DAY, 'FCH')).toBeNull()
  })
})
