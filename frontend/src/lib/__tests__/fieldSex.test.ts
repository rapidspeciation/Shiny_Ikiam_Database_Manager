import { createHash } from 'node:crypto'
import { readFileSync } from 'node:fs'
import { describe, expect, it } from 'vitest'
import { FIELD_SEX_SPECIES, fieldSexState, type FieldSexAnswer, type FieldSexInput } from '../fieldSex'
import { ITHOMIINI_GENERA } from '../ithomiiniGenera'
import { SEX_GENERA, sexApplies } from '../sexModel'

// The general sex line on Identificar (lib/fieldSex.ts): when the page shows it. No model, no AI.

const call = (species = 'Danaus plexippus', sex: 'female' | 'male' = 'male', pct = 97): FieldSexAnswer => ({ species, status: 'call', sex, pct, version: 'field-sex/1' })
const input = (o: Partial<FieldSexInput> = {}): FieldSexInput => ({
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
  it('its files and the view\'s sexState are byte-identical to main (e3000a08); a deliberate change updates these pins', () => {
    const sha = (s: string) => createHash('sha256').update(s).digest('hex')
    const read = (p: string) => readFileSync(new URL(p, import.meta.url), 'utf8')
    expect(sha(read('../../components/identify/SexEstimate.vue'))).toBe('341c39096f92e5d5a7b147fb24f5d59affa4ac2c633c4d63f010f5e2d5962c8c')
    expect(sha(read('../sexModel.ts'))).toBe('fb1d6a4810bb1da7e27b913711b065d8948da8ae54ad8a62c6084d220bed8f7e')
    const view = read('../../views/IdentifyView.vue')
    const block = /const sexState = computed<SexState \| null>\(\(\) => \{.*?\n\}\)\n/s.exec(view)?.[0] ?? ''
    expect(sha(block)).toBe('a76af89528fed655e1bf03a7bc16faa941643fbf2591dc813d8de3980f77f84d')
    // the general line renders only where the in-hand one does not
    expect(view).toContain('<SexEstimate v-if="sexState" :state="sexState" @download="downloadSexModel" />\n            <FieldSexEstimate v-else-if="fieldSexState"')
  })
})
