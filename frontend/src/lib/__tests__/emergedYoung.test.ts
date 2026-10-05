import { describe, expect, it } from 'vitest'
import { isoToSerial } from '../dates'
import { draftValues, setYoung, sharedYoung, youngSamples, youngStarts, youngValue, YOUNG_BATCH, type Draft, type RowContext, type YoungBatch } from '../emerged'
import { localRun } from '../tubes'

// Eggs and larvae preserved from a clutch (Emergidos): the batch panel and each card's CAM and tube.
const larva = (key: string, over: Partial<Draft> = {}): Draft => ({
  key,
  id: key.toUpperCase(),
  clutch: '994(3)',
  date: '2026-10-02',
  kind: 'young',
  sex: 'NA',
  fate: 'alive',
  species: '',
  stage: '4th instar larva',
  foundDead: false,
  note: '',
  cam: '',
  tube: '',
  ...over,
})
const batch = (over: Partial<YoungBatch> = {}): YoungBatch => ({ ...YOUNG_BATCH, ...over })
const used = new Set(['CAM078240', 'FS63886706'])
const run = (start: string, count: number) => localRun(start, count, id => used.has(id))
const RACKS: Record<string, string> = { 'Flash frozen': 'FS63886700', Ethanol: 'FA00001000' }
const starts = (d: Draft, b: YoungBatch) => youngStarts(d, b, { camFirst: 'CAM078238', rackFor: m => RACKS[m] ?? '' })
const samples = (cards: Draft[], b = batch(), taken?: Set<string>) =>
  youngSamples(
    cards.map(d => ({ key: d.key, ...starts(d, b), typedCam: d.typedCam, typedTube: d.typedTube })),
    { run, taken },
  )
const values = (out: ReturnType<typeof samples>, field: 'cam' | 'tube') => Object.values(out).map(s => s[field].value)

describe('the CAM and tube of each larva', () => {
  it('the next free ones, consecutive across the cards, skipping used ones and other cards', () => {
    const cards = ['a', 'b', 'c', 'd'].map(k => larva(k))
    const out = samples(cards, batch(), new Set(['FS63886701']))
    expect(values(out, 'cam')).toEqual(['CAM078238', 'CAM078239', 'CAM078241', 'CAM078242'])
    // FS63886701 is on another card (an adult preserved), FS63886706 used in the workbook.
    expect(values(out, 'tube')).toEqual(['FS63886700', 'FS63886702', 'FS63886703', 'FS63886704'])
    expect(out.a.cam.auto).toBe(true)
  })
  it('a CAM or tube typed on a card stays, and the cards after it go on from it', () => {
    // The rack ran out at card b: the new rack's first tube is typed there.
    const cards = [larva('a'), larva('b', { typedTube: 'FS50848946', typedCam: 'CAM078300' }), larva('c'), larva('d')]
    const out = samples(cards)
    expect(values(out, 'tube')).toEqual(['FS63886700', 'FS50848946', 'FS50848947', 'FS50848948'])
    expect(values(out, 'cam')).toEqual(['CAM078238', 'CAM078300', 'CAM078301', 'CAM078302'])
    expect(out.b.tube).toEqual({ value: 'FS50848946', auto: false })
    // A box emptied on purpose stays empty; a badly typed one does not move the run.
    const kept = samples([larva('a', { typedTube: '' }), larva('b', { typedTube: 'FS5084894' }), larva('c')])
    expect(values(kept, 'tube')).toEqual(['', 'FS5084894', 'FS63886700'])
  })
  it('cards with their own rack draw from it; the others go on with the batch rack', () => {
    const cards = [larva('a'), larva('b', { own: { tubeFrom: 'FS50848946' } }), larva('c', { own: { tubeFrom: 'FS50848946' } }), larva('d')]
    const out = samples(cards)
    expect(values(out, 'tube')).toEqual(['FS63886700', 'FS50848946', 'FS50848947', 'FS63886701'])
    // One CAM series for all of them.
    expect(values(out, 'cam')).toEqual(['CAM078238', 'CAM078239', 'CAM078241', 'CAM078242'])
  })
  it('in ethanol (the dry shipper failed): the ethanol rack, chosen by the app', () => {
    const cards = [larva('a'), larva('b', { own: { medium: 'Ethanol' } }), larva('c')]
    expect(values(samples(cards), 'tube')).toEqual(['FS63886700', 'FA00001000', 'FS63886701'])
    // The whole batch in ethanol.
    expect(values(samples(cards.map(d => ({ ...d, own: undefined })), batch({ medium: 'Ethanol' })), 'tube')).toEqual([
      'FA00001000',
      'FA00001001',
      'FA00001002',
    ])
  })
  it('a first CAM and a first tube for the batch', () => {
    const b = batch({ camStart: 'CAM090000', tubeStart: 'FS11111111' })
    const out = samples([larva('a'), larva('b'), larva('c', { own: { medium: 'Ethanol' } })], b)
    expect(values(out, 'cam')).toEqual(['CAM090000', 'CAM090001', 'CAM090002'])
    // The batch's rack is for its medium: a card in ethanol takes the ethanol rack.
    expect(values(out, 'tube')).toEqual(['FS11111111', 'FS11111112', 'FA00001000'])
  })
})

describe('the batch panel', () => {
  const cards = [larva('a'), larva('b'), larva('c')]
  const adult: Draft = { ...larva('x'), kind: 'adult', sex: 'female' }
  it('with none selected, for the whole batch: no card keeps its own value', () => {
    let s = setYoung([...cards, adult], batch(), ['b'], 'medium', 'Ethanol')
    expect(s.drafts.map(d => youngValue(d, s.batch, 'medium'))).toEqual(['Flash frozen', 'Ethanol', 'Flash frozen', 'Flash frozen'])
    expect(sharedYoung(s.drafts.slice(0, 3), s.batch, 'medium')).toBeUndefined()
    s = setYoung(s.drafts, s.batch, [], 'medium', 'DMSO')
    expect(s.batch.medium).toBe('DMSO')
    expect(s.drafts.every(d => d.own === undefined)).toBe(true)
  })
  it('for the selected cards only; the batch value given back drops their own', () => {
    let s = setYoung(cards, batch(), ['a', 'c'], 'purpose', 'Sperm dissections')
    expect(s.drafts.map(d => youngValue(d, s.batch, 'purpose'))).toEqual(['Sperm dissections', 'F1/F2 mutation rate', 'Sperm dissections'])
    s = setYoung(s.drafts, s.batch, ['a'], 'purpose', 'F1/F2 mutation rate')
    expect(s.drafts[0].own).toBeUndefined()
    // A rack for the selected; emptied, they follow the batch again.
    s = setYoung(s.drafts, s.batch, ['b'], 'tubeFrom', 'fs50848946 ')
    expect(s.drafts[1].own).toEqual({ tubeFrom: 'FS50848946' })
    s = setYoung(s.drafts, s.batch, ['b'], 'tubeFrom', '')
    expect(s.drafts[1].own).toBeUndefined()
  })
  it('a new medium for the batch drops the rack chosen for the old one', () => {
    let s = setYoung(cards, batch(), [], 'tubeFrom', 'FS22222222')
    expect(s.batch.tubeStart).toBe('FS22222222')
    s = setYoung(s.drafts, s.batch, [], 'medium', 'Flash frozen')
    expect(s.batch.tubeStart).toBe('FS22222222')
    s = setYoung(s.drafts, s.batch, [], 'medium', 'Ethanol')
    expect(s.batch.tubeStart).toBe('')
    expect(starts(cards[0], s.batch).tube).toBe('FA00001000')
  })
})

describe('the row of a larva', () => {
  const ctx: RowContext = {
    clutchValue: '994(3)',
    clutchSpecies: 'Mechanitis messenoides deceptus',
    generation: 'F1',
    formulas: ['Insectary_ID', 'SPECIES', 'T2_Preservation_medium'],
    today: isoToSerial('2026-10-03'),
    initials: 'FCH',
    medium: 'Ethanol',
  }
  it('the preservation block as the data rules give it, its medium in T1', () => {
    const v = draftValues(larva('a', { stage: '3rd instar larva', cam: 'CAM078238', tube: 'FA00001000' }), ctx)
    const day = isoToSerial('2026-10-02')
    expect(v).toMatchObject({
      Sex: 'NOT_COLLECTED',
      Intro2Insectary_date: 'NA',
      LIFESTAGE: '3rd instar larva',
      Death_date: day,
      Preservation_date: day,
      Death_cause: 'Killed_Preserved',
      Preserved_Dead_Alive: 'Alive',
      CAM_ID: 'CAM078238',
      Tube_1_id: 'FA00001000',
      Tube_1_tissue: 'WHOLE_ORGANISM',
      T1_Preservation_medium: 'Ethanol',
      Preservation_medium: 'NOT_COLLECTED',
      Tube_2_id: 'NA',
      Tube_2_tissue: 'NOT_COLLECTED',
      Tube_3_id: 'NA',
      Tube_4_id: 'NA',
      Tube_4_tissue: 'NOT_COLLECTED',
      Location_body: 'Ikiam',
      Research_purpose: 'F1/F2 mutation rate',
    })
    // T2's medium is a formula of the pre-made row.
    expect('T2_Preservation_medium' in v).toBe(false)
    expect(draftValues(larva('a', { foundDead: true, cam: 'CAM1', tube: 'FS1' }), { ...ctx, purpose: 'Sperm dissections' })).toMatchObject({
      Death_cause: 'Other',
      Preserved_Dead_Alive: 'Dead',
      Research_purpose: 'Sperm dissections',
    })
  })
})
