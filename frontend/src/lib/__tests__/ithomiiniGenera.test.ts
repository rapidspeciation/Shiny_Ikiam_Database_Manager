import { describe, expect, it } from 'vitest'
import { ITHOMIINI_GENERA, isIthomiiniGenus } from '../ithomiiniGenera'
import { isIthomiini } from '../identifyResult'
import { sexApplies } from '../sexModel'

// The 46 genera Butterflies of America files under Danainae: Ithomiini, as frozen on 9 Nov 2026 in WingsClassificator
// reports/model_autoresearch_20261109/taxonomic/audit/tribe_audit.py (REFERENCE_GENERA), typed here again on purpose:
// a change to the app's list has to be made twice, with its source.
const REFERENCE_46 = `
Aeria Aremfoxia Athesis Athyrtis Brevioleria Callithomia Ceratinia Dircenna Elzunia Episcada Epityches Eutresis
Forbestra Godyris Greta Haenschia Heterosais Hyalenna Hyalyris Hypoleria Hypomenitis Hyposcada Hypothyris Ithomia
Mcclungia Mechanitis Megoleria Melinaea Methona Napeogenes Oleria Ollantaya Olyras Pachacutia Pagyris Paititia
Patricia Placidina Pseudoscada Pteronymia Sais Scada Thyridia Tithorea Veladyris Velamysta
`.split(/\s+/).filter(Boolean)
// Synonyms the identifier's head keeps as genera (tribe_audit.py REFERENCE_SYNONYMS).
const SYNONYMS = ['Leucothyris']

describe('the Ithomiini genera (lib/ithomiiniGenera.ts)', () => {
  it('are the 46-genus reference plus the head synonym Leucothyris, each once, sorted', () => {
    expect(REFERENCE_46).toHaveLength(46)
    expect([...ITHOMIINI_GENERA].sort()).toEqual([...REFERENCE_46, ...SYNONYMS].sort())
    expect(new Set(ITHOMIINI_GENERA).size).toBe(ITHOMIINI_GENERA.length)
    expect([...ITHOMIINI_GENERA]).toEqual([...ITHOMIINI_GENERA].sort())
    expect(Object.isFrozen(ITHOMIINI_GENERA)).toBe(true)
  })

  it('say Ithomiini for the old-tribe and missing genera, and not for Macrosoma (a moth) or other Nymphalidae', () => {
    for (const g of ['Pteronymia', 'Greta', 'Napeogenes', 'Tithorea', 'Athyrtis', 'Epityches', 'Leucothyris', 'Godyris', 'Elzunia'])
      expect(isIthomiiniGenus(g)).toBe(true)
    for (const g of ['Macrosoma', 'Danaus', 'Lycorea', 'Heliconius', 'Unknown', '', null, undefined, 'pteronymia'])
      expect(isIthomiiniGenus(g)).toBe(false)
  })

  it('give the Identificar badge and the sex line («no aplica») for Athyrtis, Leucothyris and Elzunia', () => {
    for (const name of ['Athyrtis mechanitis', 'Leucothyris victoria', 'Elzunia humboldt']) {
      expect(isIthomiini(name, ITHOMIINI_GENERA)).toBe(true)
      expect(sexApplies(name, ITHOMIINI_GENERA)).toBe('genus')
    }
    expect(isIthomiini('Macrosoma heliconiaria', ITHOMIINI_GENERA)).toBe(false)
    expect(sexApplies('Macrosoma heliconiaria', ITHOMIINI_GENERA)).toBe('no')
  })
})
