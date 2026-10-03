import { computed, type Ref, ref, watch } from 'vue'
import { buildOptions, listColumn } from '../lib/options'
import { errorText, notify } from '../lib/notice'
import { usePending } from '../stores/pending'
import { useTables } from '../stores/tables'

/** Sheets whose choices depend on the insectary stocks (clutch numbers). */
const USES_STOCKS = new Set(['Insectary_data', 'Collection_data', 'Melinaea_eggs', 'Life_History'])

/**
 * Loads one sheet plus the reference sheets used for its dropdowns, and keeps
 * the options and pending new rows for it in sync. With `active`, the sheet is
 * read only while it is true (the Buscador shows search results meanwhile).
 */
export function useSheet(module: Ref<string>, active: Ref<boolean> = ref(true)) {
  const tables = useTables()
  const pending = usePending()

  // Saved rows are merged into the cached table in place, so each version gets a
  // fresh wrapper object; otherwise Vue would see "the same table" and not update.
  const snapshot = (name: string) => {
    void tables.versions[name]
    const t = tables.tables[name]
    return t ? { ...t } : undefined
  }
  const table = computed(() => snapshot(module.value))
  const lists = computed(() => snapshot('Lists'))
  const stocks = computed(() => snapshot('Insectary_stocks'))
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
    listColumn: (name: string) => listColumn(lists.value, name),
  }
}
