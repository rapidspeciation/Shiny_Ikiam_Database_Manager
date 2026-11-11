import { readFileSync } from 'node:fs'
import { describe, expect, it } from 'vitest'
import { FIELD_SEX_SPECIES, fieldSexState, type FieldSexAnswer, type FieldSexInput } from '../fieldSex'
import { ITHOMIINI_GENERA } from '../ithomiiniGenera'
import { SEX_GENERA, sexApplies } from '../sexModel'

// The general sex line on Identificar (lib/fieldSex.ts): when the page shows it. No model, no AI.

const call = (species = 'Danaus plexippus', sex: 'female' | 'male' = 'male', pct = 97): FieldSexAnswer => ({ species, status: 'call', sex, pct, version: 'field-sex/1' })
const input = (o: Partial<FieldSexInput> = {}): FieldSexInput => ({
  mode: 'online',
  source: 'online',
  headline: { kind: 'species', name: 'Danaus plexippus' },
  answer: call(),
  live: undefined,
  ithomiiniGenera: ITHOMIINI_GENERA,
  ...o,
})

describe('fieldSexState', () => {
  it('asks first (one tap); nothing shown until the person answers', () => {
    expect(fieldSexState(input())).toEqual({ kind: 'ask', species: 'Danaus plexippus' })
  })
  it('a live butterfly: the call and its %; not live: no call', () => {
    expect(fieldSexState(input({ live: 'yes' }))).toEqual({ kind: 'call', species: 'Danaus plexippus', sex: 'male', pct: 97 })
    expect(fieldSexState(input({ live: 'no' }))).toEqual({ kind: 'declined', species: 'Danaus plexippus' })
  })
  it('the six species only, and only the species the page names (the server gates without the prior)', () => {
    expect(FIELD_SEX_SPECIES).toHaveLength(6)
    for (const s of FIELD_SEX_SPECIES) expect(fieldSexState(input({ headline: { kind: 'species', name: s }, answer: call(s), live: 'yes' }))?.kind).toBe('call')
    expect(fieldSexState(input({ headline: { kind: 'species', name: 'Danaus gilippus' }, answer: call('Danaus gilippus') }))).toBeNull()
    // the page names another species (the side's prior moved it): the block is for the no-prior species, so nothing
    expect(fieldSexState(input({ headline: { kind: 'species', name: 'Danaus erippus' } }))).toBeNull()
    expect(fieldSexState(input({ headline: { kind: 'genus', genus: 'Danaus' } }))).toBeNull()
    expect(fieldSexState(input({ headline: { kind: 'unsure' } }))).toBeNull()
  })
  it('only a Supported call the server shows: below the gate, no wings, not supported or suppressed show nothing', () => {
    for (const status of ['below_gate', 'no_wings', 'not_supported', 'suppressed'] as const)
      expect(fieldSexState(input({ answer: { species: 'Danaus plexippus', status }, live: 'yes' }))).toBeNull()
    expect(fieldSexState(input({ answer: null, live: 'yes' }))).toBeNull()
    expect(fieldSexState(input({ answer: undefined, live: 'yes' }))).toBeNull()
    expect(fieldSexState(input({ answer: call('Danaus plexippus', 'male', 100), live: 'yes' }))).toBeNull()
  })
  it('online only: a phone result never shows it', () => {
    expect(fieldSexState(input({ source: 'phone', live: 'yes' }))).toBeNull()
  })
  it('switching to «En este teléfono» after an online result hides it, whatever was answered', () => {
    for (const live of [undefined, 'yes', 'no'] as const) {
      expect(fieldSexState(input({ live }))).not.toBeNull()
      expect(fieldSexState(input({ mode: 'phone', live }))).toBeNull()
    }
  })
})

describe('the Ithomiini keep the in-hand line exactly as it was', () => {
  it('no general line for any Ithomiini, even with a call block', () => {
    for (const g of [...SEX_GENERA, ...ITHOMIINI_GENERA]) {
      const name = `${g} test`
      expect(sexApplies(name, ITHOMIINI_GENERA)).not.toBe('no')
      expect(fieldSexState(input({ headline: { kind: 'species', name }, answer: call(name), live: 'yes' }))).toBeNull()
    }
  })
  it('the in-hand line\'s rule is unchanged: yes for the 13 genera, genus for other Ithomiini, no otherwise', () => {
    expect(SEX_GENERA).toEqual(['Dircenna', 'Forbestra', 'Godyris', 'Hyposcada', 'Hypothyris', 'Ithomia', 'Mechanitis', 'Methona', 'Napeogenes', 'Oleria', 'Pteronymia', 'Scada', 'Tithorea'])
    expect(sexApplies('Mechanitis messenoides', ITHOMIINI_GENERA)).toBe('yes')
    expect(sexApplies('Danaus plexippus', ITHOMIINI_GENERA)).toBe('no')
  })
  it('the general line renders only where the in-hand one does not', () => {
    const view = readFileSync(new URL('../../views/IdentifyView.vue', import.meta.url), 'utf8')
    expect(view).toContain('<SexEstimate v-if="sexState" :state="sexState" @download="downloadSexModel" />\n            <FieldSexEstimate v-else-if="fieldSexState"')
  })
})

describe('«Sí, viva» is asked again for each new photo and after a restore', () => {
  const view = readFileSync(new URL('../../views/IdentifyView.vue', import.meta.url), 'utf8')
  it('a new online photo builds a fresh result (no spread of the old one, so no answer carried over)', () => {
    const body = /async function identifyOnline\(photo: Photo\) \{.*?\n\}\n/s.exec(view)?.[0] ?? ''
    expect(body).toContain('result.value = {\n      at,\n      source: \'online\',')
    expect(body).not.toMatch(/\.\.\.(r|result\.value|old)\b/)
    expect(body).not.toContain('live')
  })
  it('a restored result drops the answer', () => {
    expect(view).toContain('result.value = { ...v, live: undefined }')
  })
})
