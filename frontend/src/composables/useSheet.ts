import { computed, type Ref, ref, watch } from 'vue'
import { buildOptions, listColumn } from '../lib/options'
import { errorText, notify } from '../lib/notice'
import { overlayTable } from '../lib/staged'
import { useLive } from '../stores/live'
import { usePending } from '../stores/pending'
import { useTables } from '../stores/tables'

/** Sheets whose choices depend on the insectary stocks (clutch numbers). */
const USES_STOCKS = new Set(['Insectary_data', 'Collection_data', 'Melinaea_eggs', 'Life_History'])

/**
 * Loads one sheet plus the reference sheets used for its dropdowns, and keeps
 * the options and pending new rows for it in sync. With `active`, the sheet is
 * read only while it is true (the Buscador shows search results meanwhile).
 * With `staged` (Emergidos, Clutches), the sheet and the clutches carry
 * everyone's entries kept in the app (lib/staged.ts): `marks` and `sums` say
 * which cells are not in Google Sheets yet, and the counts' sums they hold.
 */
export function useSheet(module: Ref<string>, active: Ref<boolean> = ref(true), { staged = false } = {}) {
  const tables = useTables()
  const pending = usePending()
  const live = useLive()

  // Saved rows are merged into the cached table in place, so each version gets a
  // fresh wrapper object; otherwise Vue would see "the same table" and not update.
  const snapshot = (name: string) => {
    void tables.versions[name]
    const t = tables.tables[name]
    return t ? { ...t } : undefined
  }
  const own = computed(() => (staged ? overlayTable(snapshot(module.value), live.items) : null))
  const ownStocks = computed(() =>
    staged && module.value !== 'Insectary_stocks' ? overlayTable(snapshot('Insectary_stocks'), live.items) : null,
  )
  const table = computed(() => (own.value ? own.value.table : snapshot(module.value)))
  const lists = computed(() => snapshot('Lists'))
  const stocks = computed(() =>
    module.value === 'Insectary_stocks' && own.value ? own.value.table : ownStocks.value ? ownStocks.value.table : snapshot('Insectary_stocks'),
  )
  /** Cells of the entries kept in the app, per row id (both sheets), and the sums they hold. */
  const marks = computed(() => ({ ...(ownStocks.value?.marks ?? {}), ...(own.value?.marks ?? {}) }))
  const sums = computed(() => ({ ...(ownStocks.value?.sums ?? {}), ...(own.value?.sums ?? {}) }))
  const loading = computed(() => !!tables.loading[module.value])
  /**
   * The sheet and the sheets its dropdowns come from have arrived (or failed).
   * Grids wait for this: built earlier, they would be built again when the
   * choices arrive, which freezes the page for seconds on a 13k-row sheet.
   */
  const ready = computed(
    () =>
      !!table.value &&
      !tables.loading.Lists &&
      !(USES_STOCKS.has(module.value) && tables.loading.Insectary_stocks),
  )

  async function load(force = false) {
    try {
      await Promise.all([
        tables.load(module.value, force),
        tables.load('Lists'),
        USES_STOCKS.has(module.value) ? tables.load('Insectary_stocks') : null,
      ])
    } catch (e) {
      notify(errorText(e), 'error')
    }
  }

  const clutches = computed(() => {
    if (!stocks.value) return []
    return [
      ...new Set(
        stocks.value.rows
          .filter(r => r.observed)
          .map(r => String(r.values['CLUTCH NUMBER'] ?? ''))
          .filter(Boolean),
      ),
    ].reverse()
  })

  const options = computed(() => {
    if (!table.value) return {}
    const extra: Record<string, string[]> = {}
    if (clutches.value.length) extra['CLUTCH NUMBER'] = clutches.value
    return buildOptions(table.value, lists.value, extra)
  })

  const creates = computed(() => pending.creates.filter(c => c.module === module.value))

  /** Formula columns of the unused row a new record will most likely be written into. */
  const createFormulas = computed(() => {
    const rows = table.value?.rows || []
    let last = -1
    rows.forEach((r, i) => {
      if (r.observed) last = i
    })
    return rows.slice(last + 1).find(r => !r.observed)?.formulas || []
  })

  watch([module, active], () => active.value && load(), { immediate: true })

  return {
    table,
    lists,
    stocks,
    loading,
    ready,
    options,
    creates,
    createFormulas,
    clutches,
    load,
    marks,
    sums,
    listColumn: (name: string) => listColumn(lists.value, name),
  }
}
