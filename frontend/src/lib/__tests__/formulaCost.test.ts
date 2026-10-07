import { afterEach, describe, expect, it } from 'vitest'
import { locale } from '../i18n'
import { costLine, costTip, type FormulaCost } from '../formulaCost'

afterEach(() => {
  locale.value = 'es'
})

// The formula written into Insectary_data CAM_ID_CollData, as the server estimates it on the real copy.
const CAM: FormulaCost = {
  sheet: 'Insectary_data',
  column: 'CAM_ID_CollData',
  formula: '=XLOOKUP(A12000,Collection_data!D:D,Collection_data!E:E,"NA")',
  cells: 1000,
  perCell: 9757,
  comparisons: 9_757_000,
  scans: [{ range: 'Collection_data!D:D', rows: 9755, lookup: true }],
  dependents: { cells: 6378, comparisons: 265_960_784, sheets: [['F1/F2_MutationRate', 6378]] },
  flags: ['wholeColumnLookup', 'crossSheetRepeated', 'heavyDependents'],
  heavy: true,
  tips: ['bounded', 'guard', 'batch'],
  bounded: 'Collection_data!$D$2:$D$9755',
}
const LIGHT: FormulaCost = {
  sheet: 'Insectary_data',
  column: 'T2_Preservation_medium',
  formula: '=IFS(U5="","",U5="NA","NA")',
  cells: 1,
  perCell: 2,
  comparisons: 2,
  scans: [],
  dependents: null,
  flags: [],
  heavy: false,
  tips: [],
}

describe('the cost line of a formula', () => {
  it('cells × rows ≈ comparisons, in the reader’s number format, then the lookups each write recalculates', () => {
    expect(costLine(CAM)).toMatch(/^1\.000 celdas × 9\.757 filas ≈ 9,8 M .* · .* 6\.378 .*F1\/F2_MutationRate$/)
    locale.value = 'en'
    expect(costLine(CAM)).toMatch(/^1,000 cells × 9,757 rows ≈ 9\.8 M .* · .* 6,378 .*F1\/F2_MutationRate$/)
  })

  it('one cell is singular; no dependents, no second part', () => {
    expect(costLine(LIGHT)).toMatch(/^1 celda × 2 filas ≈ 2 /)
    expect(costLine(LIGHT)).not.toContain(' · ')
  })
})

describe('the tip that would make it lighter', () => {
  it('the tips the server gave, in its order, the bounded range named', () => {
    const only = (tip: string) => costTip({ ...CAM, tips: [tip] })
    expect(only('bounded')).toContain('Collection_data!$D$2:$D$9755')
    expect(costTip(CAM)).toBe([only('bounded'), only('guard'), only('batch')].join('; '))
  })

  it('none for a light formula', () => {
    expect(costTip(LIGHT)).toBe('')
  })
})
