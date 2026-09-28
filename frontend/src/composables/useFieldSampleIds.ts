import { computed, ref } from 'vue'
import { api } from '../lib/api'
import { isBlank } from '../lib/cells'
import { listColumn } from '../lib/options'
import type { Table } from '../lib/types'
import { usePending } from '../stores/pending'
import { useTables } from '../stores/tables'

/**
 * CAM and tube IDs for butterflies preserved in the field, from the sources
 * Colecta uses: CAMs from the Lists pool Wild_indv_CAMid (after the last one
 * used in Collection_data), tubes from the rack in use (/api/ids?kind=tube).
 * IDs already in the sheet or in unsaved rows are skipped. Monitoring tubes
 * come from the monitoring rack when there is one, otherwise from Colecta's.
 */
export function useFieldSampleIds(source: { table: () => Table | undefined; lists: () => Table | undefined }) {
  const tables = useTables()
  const pending = usePending()

  const unsaved = (fields: string[]) =>
    new Set(pending.creates.flatMap(c => fields.map(f => String(c.values[f] ?? '').trim())).filter(v => v && v !== 'NA'))

  const camPool = computed(() => {
    const rows = source.table()?.rows || []
    const used = new Set<string>()
    // Insectary_data is only counted when already loaded: Monitoreo does not need to load 13k rows for it.
    for (const [sheet, keys] of [
      ['Collection_data', ['CAM_ID', 'CAM_ID_insectary']],
      ['Insectary_data', ['CAM_ID', 'CAM_ID_CollData']],
    ] as const)
      for (const row of sheet === 'Collection_data' ? rows : tables.tables[sheet]?.rows || [])
        for (const key of keys) if (!isBlank(row.values[key])) used.add(String(row.values[key]).trim())
    const number = (id: string) => Number(/(\d+)$/.exec(id)?.[1] ?? -1)
    const pool = listColumn(source.lists(), 'Wild_indv_CAMid').sort((a, b) => number(a) - number(b))
    const latest = [...rows].reverse().find(r => r.observed && pool.includes(String(r.values.CAM_ID ?? '').trim()))
    const at = latest ? pool.indexOf(String(latest.values.CAM_ID).trim()) : -1
    return [...pool.slice(at + 1), ...pool.slice(0, at + 1)].filter(id => !used.has(id))
  })

  const tubeRun = ref<string[]>([])
  let loaded: Promise<void> | null = null
  /** The next tubes of the monitoring (or field collection) rack for this medium. */
  function loadTubes(medium = 'Flash frozen') {
    loaded = (async () => {
      const { suggestions } = await api<{ suggestions: { value: string; context: string; medium: string }[] }>('ids?kind=tube')
      const run =
        suggestions.find(s => s.context === 'Monitoreo' && s.medium === medium) ||
        suggestions.find(s => s.context === 'Colecta' && s.medium === medium) ||
        suggestions.find(s => s.context === 'Colecta')
      tubeRun.value = run ? (await api<{ sequence: string[] }>(`ids?kind=tube&start=${run.value}&count=60`)).sequence : []
    })()
    return loaded
  }

  /** `count` CAM and tube pairs not used anywhere yet (tubes wait for the rack to be read). */
  async function next(count: number): Promise<{ cam: string | null; tube: string | null }[]> {
    if (!loaded) loadTubes()
    await loaded?.catch(() => {})
    const cams = unsaved(['CAM_ID', 'CAM_ID_insectary'])
    const tubes = unsaved(['Tube_1_id', 'Tube_2_id', 'Tube_3_id', 'Tube_4_id_LEGS'])
    const freeCams = camPool.value.filter(id => !cams.has(id))
    const freeTubes = tubeRun.value.filter(id => !tubes.has(id))
    return Array.from({ length: count }, (_, i) => ({ cam: freeCams[i] ?? null, tube: freeTubes[i] ?? null }))
  }

  return { camPool, tubeRun, loadTubes, next }
}
