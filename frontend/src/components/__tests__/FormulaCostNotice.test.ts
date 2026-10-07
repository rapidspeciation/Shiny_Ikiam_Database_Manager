import { afterEach, describe, expect, it } from 'vitest'
import { createApp, nextTick } from 'vue'
import FormulaCostNotice from '../assistant/FormulaCostNotice.vue'
import { locale, t, tn } from '../../lib/i18n'
import { bigNumber, type FormulaCost } from '../../lib/formulaCost'

let unmount = () => {}
afterEach(() => {
  unmount()
  document.body.innerHTML = ''
  locale.value = 'es'
})

function mount(cost: FormulaCost[]) {
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp(FormulaCostNotice, { cost })
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  app.mount(host)
  unmount = () => app.unmount()
  return host
}

// The two formulas written into Insectary_data O and V, as the server estimates them on the real copy.
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

describe('FormulaCostNotice', () => {
  it('says cells × rows ≈ comparisons and the lookups each write recalculates, in amber when heavy', async () => {
    const host = mount([CAM, LIGHT])
    const notice = host.querySelector('[data-test="formula-cost"]')!
    expect(notice.classList.contains('is-heavy')).toBe(true)
    const text = notice.textContent!.replace(/\s+/g, ' ')
    expect(text).toContain(
      'ƒx CAM_ID_CollData: 1.000 celdas × 9.757 filas ≈ 9,8 M comparaciones por recálculo · cada escritura aquí recalcula 6.378 búsquedas de F1/F2_MutationRate',
    )
    expect(text).toContain('Más ligero: un rango que acabe en la última fila usada (Collection_data!$D$2:$D$9755)')
    expect(text).toContain('ƒx T2_Preservation_medium: 1 celda × 2 filas ≈ 2 comparaciones por recálculo')
    // A light formula: no tip.
    expect(notice.querySelectorAll('.block.pl-4').length).toBe(1)
    locale.value = 'en'
    await nextTick()
    expect(notice.textContent!.replace(/\s+/g, ' ')).toContain(
      '1,000 cells × 9,757 rows ≈ 9.8 M comparisons per recalculation · each write here recalculates 6,378 lookups in F1/F2_MutationRate',
    )
  })

  it('light formulas only: a plain grey line', () => {
    const host = mount([LIGHT])
    expect(host.querySelector('[data-test="formula-cost"]')!.classList.contains('is-heavy')).toBe(false)
    expect(bigNumber(57000)).toBe('57.000')
    expect(bigNumber(1_190_000_000)).toBe('1.190 M')
  })
})
