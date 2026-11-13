import { readFileSync } from 'node:fs'
import { describe, expect, it } from 'vitest'
import { FIELD_SEX_BAND, FIELD_SEX_ESTIMATE_MAX, FIELD_SEX_SPECIES, fieldSexState, type FieldSexAnswer, type FieldSexInput } from '../fieldSex'
import { ITHOMIINI_GENERA } from '../ithomiiniGenera'
import { SEX_GENERA, sexApplies } from '../sexModel'

// The general sex line on Identificar (lib/fieldSex.ts): when the page shows it and what. No model, no AI.

const block = (o: Partial<FieldSexAnswer> = {}): FieldSexAnswer => ({ species: 'Danaus plexippus', status: 'call', p: 0.03, sex: 'male', sure: true, pct: 97, version: 'field-sex/1', ...o })
const estimated = (o: Partial<FieldSexAnswer> = {}): FieldSexAnswer => ({
  species: 'Vanessa cardui', status: 'estimate', p: 0.62, sex: 'female', sure: false, pct: 62, version: 'field-sex/2', verified: false, level: 'species', trainPhotos: 900, ...o,
})
const input = (o: Partial<FieldSexInput> = {}): FieldSexInput => ({
  mode: 'online',
  source: 'online',
  topName: 'Danaus plexippus',
  answer: block(),
  ithomiiniGenera: ITHOMIINI_GENERA,
  ...o,
})

describe('fieldSexState', () => {
  it('a Supported answer outside the band: the sex and its %, no question asked', () => {
    expect(fieldSexState(input())).toEqual({ species: 'Danaus plexippus', status: 'call', p: 0.03, sex: 'male', sure: true, pct: 97, note: null, verified: true, level: null, trainPhotos: null })
  })
  it('inside the band: the side it leans to and the marker, no %', () => {
    const s = fieldSexState(input({ answer: block({ status: 'band', p: 0.62, sex: 'female', sure: false, pct: undefined }) }))
    expect(s).toMatchObject({ status: 'band', p: 0.62, sex: 'female', sure: false, pct: null, note: null })
    expect(FIELD_SEX_BAND).toEqual({ lo: 0.15, hi: 0.85 })
  })
  it('a species x sex cell under 90 %: bar and marker, no answer, no %', () => {
    expect(fieldSexState(input({ answer: block({ status: 'cell_under_90', pct: undefined }) }))).toMatchObject({ status: 'cell_under_90', pct: null, note: null })
    // a call that arrives without a valid % is shown the same way
    expect(fieldSexState(input({ answer: block({ pct: 89 }) }))).toMatchObject({ status: 'cell_under_90', pct: null })
    expect(fieldSexState(input({ answer: block({ pct: 100 }) }))).toMatchObject({ status: 'cell_under_90', pct: null })
  })
  it('species score under 0.8 or no wings: the bar with a note, no %', () => {
    expect(fieldSexState(input({ answer: block({ status: 'species_unsure', pct: undefined }) }))).toMatchObject({ status: 'species_unsure', pct: null, note: 'species' })
    expect(fieldSexState(input({ answer: block({ status: 'no_wings', pct: 97 }) }))).toMatchObject({ status: 'no_wings', pct: null, note: 'wings' })
  })
  it('shown whenever the page names one of the six, even when its headline is not sure of the species', () => {
    for (const s of FIELD_SEX_SPECIES) expect(fieldSexState(input({ topName: s, answer: block({ species: s }) }))?.species).toBe(s)
    expect(fieldSexState(input({ topName: 'Danaus gilippus', answer: block({ species: 'Danaus gilippus' }) }))).toBeNull()
    // the block is for another species than the one named: nothing
    expect(fieldSexState(input({ topName: 'Danaus plexippus', answer: block({ species: 'Papilio troilus' }) }))).toBeNull()
    expect(fieldSexState(input({ topName: undefined }))).toBeNull()
    expect(fieldSexState(input({ answer: null }))).toBeNull()
  })
  it('an odd block shows nothing', () => {
    expect(fieldSexState(input({ answer: block({ p: 1.5 }) }))).toBeNull()
    expect(fieldSexState(input({ answer: block({ sex: 'x' as never }) }))).toBeNull()
    expect(fieldSexState(input({ answer: block({ status: 'below_gate' as never }) }))).toBeNull()
  })
  it('online only: a phone result never shows it; picking «En este teléfono» hides it', () => {
    expect(fieldSexState(input({ source: 'phone' }))).toBeNull()
    expect(fieldSexState(input({ mode: 'phone' }))).toBeNull()
  })
})

describe('estimated species (every other species of the model)', () => {
  it('inside the band: the side it leans to and its %; outside: the sex and its %', () => {
    const named = { topName: 'Vanessa cardui' }
    expect(fieldSexState(input({ ...named, answer: estimated() }))).toEqual({ species: 'Vanessa cardui', status: 'estimate', p: 0.62, sex: 'female', sure: false, pct: 62, note: null, verified: false, level: 'species', trainPhotos: 900 })
    expect(fieldSexState(input({ ...named, answer: estimated({ p: 0.08, sex: 'male', sure: true, pct: 92 }) }))).toMatchObject({ sure: true, pct: 92, sex: 'male' })
  })
  it('never above 95 % nor under 50 %: a % out of range is dropped, the bar stays', () => {
    expect(FIELD_SEX_ESTIMATE_MAX).toBe(95)
    for (const pct of [96, 99, 49, 62.5]) expect(fieldSexState(input({ topName: 'Vanessa cardui', answer: estimated({ pct }) }))).toMatchObject({ status: 'estimate', pct: null })
  })
  it('species unsure or no wings: the bar and the note, no %', () => {
    expect(fieldSexState(input({ topName: 'Vanessa cardui', answer: estimated({ status: 'species_unsure' }) }))).toMatchObject({ pct: null, note: 'species', verified: false })
    expect(fieldSexState(input({ topName: 'Vanessa cardui', answer: estimated({ status: 'no_wings' }) }))).toMatchObject({ pct: null, note: 'wings' })
  })
  it('the level and the training photos come through; an odd level or status shows nothing', () => {
    for (const level of ['species', 'genus', 'family', 'global'] as const) expect(fieldSexState(input({ topName: 'Vanessa cardui', answer: estimated({ level, trainPhotos: 3 }) }))).toMatchObject({ level, trainPhotos: 3 })
    expect(fieldSexState(input({ topName: 'Vanessa cardui', answer: estimated({ level: 'order' as never }) }))).toBeNull()
    expect(fieldSexState(input({ topName: 'Vanessa cardui', answer: estimated({ status: 'call' }) }))).toBeNull()
    expect(fieldSexState(input({ topName: 'Vanessa cardui', answer: estimated({ species: 'Vanessa atalanta' }) }))).toBeNull()
  })
  it('the six are always verified: an estimated block for one of them shows nothing', () => {
    for (const s of FIELD_SEX_SPECIES) expect(fieldSexState(input({ topName: s, answer: estimated({ species: s }) }))).toBeNull()
  })
  it('online only, as the six', () => {
    expect(fieldSexState(input({ topName: 'Vanessa cardui', answer: estimated(), source: 'phone' }))).toBeNull()
    expect(fieldSexState(input({ topName: 'Vanessa cardui', answer: estimated(), mode: 'phone' }))).toBeNull()
  })
})

describe('the 13 Ithomiini genera keep the in-hand line exactly as it was', () => {
  it('no general line for the 13 genera, even with a block of either kind', () => {
    for (const g of SEX_GENERA) {
      const name = `${g} test`
      expect(sexApplies(name, ITHOMIINI_GENERA)).toBe('yes')
      expect(fieldSexState(input({ topName: name, answer: block({ species: name }) }))).toBeNull()
      expect(fieldSexState(input({ topName: name, answer: estimated({ species: name }) }))).toBeNull()
    }
  })
  it('another Ithomiini: never a verified line; an estimated block shows (it replaces the «No aplica» note)', () => {
    for (const g of ITHOMIINI_GENERA.filter(x => !(SEX_GENERA as readonly string[]).includes(x))) {
      const name = `${g} test`
      expect(sexApplies(name, ITHOMIINI_GENERA)).toBe('genus')
      expect(fieldSexState(input({ topName: name, answer: block({ species: name }) }))).toBeNull()
      expect(fieldSexState(input({ topName: name, answer: estimated({ species: name }) }))).toMatchObject({ verified: false, species: name })
    }
  })
  it('the in-hand line\'s rule is unchanged: yes for the 13 genera, genus for other Ithomiini, no otherwise', () => {
    expect(SEX_GENERA).toEqual(['Dircenna', 'Forbestra', 'Godyris', 'Hyposcada', 'Hypothyris', 'Ithomia', 'Mechanitis', 'Methona', 'Napeogenes', 'Oleria', 'Pteronymia', 'Scada', 'Tithorea'])
    expect(sexApplies('Mechanitis messenoides', ITHOMIINI_GENERA)).toBe('yes')
    expect(sexApplies('Danaus plexippus', ITHOMIINI_GENERA)).toBe('no')
  })
  it('the general line renders only where the in-hand one does not', () => {
    // The view hands both states to its sex panel, which shows the general line only without the in-hand one or in
    // place of its «No aplica» (a genus outside the 13).
    const view = readFileSync(new URL('../../views/IdentifyView.vue', import.meta.url), 'utf8')
    expect(view).toContain(':state="sexState"\n            :field="fieldSexState"')
    const panel = readFileSync(new URL('../../components/identify/SexPanel.vue', import.meta.url), 'utf8')
    expect(panel).toContain("const useField = computed(() => !!props.field && (!props.state || props.state.kind === 'genus'))")
    expect(panel).toContain('<FieldSexEstimate v-if="useField" :state="field!" bare />\n        <SexEstimate v-else-if="state"')
  })
})
