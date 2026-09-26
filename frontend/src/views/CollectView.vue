<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { UserPlus } from 'lucide-vue-next'
import SheetGrid from '../components/SheetGrid.vue'
import { useSheet } from '../composables/useSheet'
import { isBlank } from '../lib/cells'
import { isoToSerial, todayIso } from '../lib/dates'
import { notify } from '../lib/notice'
import { listColumn } from '../lib/options'
import { persistentRef } from '../lib/persist'
import { orderColumns } from '../lib/rows'
import type { CellValue } from '../lib/types'
import { usePending } from '../stores/pending'
import { useTables } from '../stores/tables'

/**
 * New field-collected individuals in Collection_data, as the original entry
 * prompt described: session defaults from the latest entries, suggested CAM
 * IDs and species-dependent subspecies, with the newest records below.
 */
const MODULE = 'Collection_data'
const SESSION_FIELDS = ['Country', 'Side_Andes', 'Collection_location', 'Transect_section', 'Collector', 'Identifier']
const module = ref(MODULE)
const pending = usePending()
const tables = useTables()
const { table, lists, options, creates, createFormulas } = useSheet(module)

const releaseCollect = persistentRef('collect:type', 'Collected_Sent2Insectary')
const date = persistentRef('collect:date', todayIso())
const defaults = persistentRef<Record<string, string>>('collect:defaults', {})
const recentCount = ref(10)

// Used CAM IDs live in both main sheets.
tables.load('Insectary_data').catch(() => {})

const observed = computed(() => table.value?.rows.filter(r => r.observed) || [])
// Suggestions need both main sheets and Lists; adding earlier would suggest the wrong CAM IDs.
const ready = computed(() => (tables.version, !!table.value && !!lists.value && !!tables.tables.Insectary_data))

/** Session defaults start from the most recent entry that has each value. */
watch(
  observed,
  rows => {
    if (!rows.length) return
    for (const field of SESSION_FIELDS) {
      if (defaults.value[field]) continue
      const latest = [...rows].reverse().find(r => !isBlank(r.values[field]))
      if (latest) defaults.value[field] = String(latest.values[field])
    }
  },
  { immediate: true },
)

const usedCams = computed(() => {
  tables.version
  const used = new Set<string>()
  for (const [sheet, keys] of [
    [MODULE, ['CAM_ID', 'CAM_ID_insectary']],
    ['Insectary_data', ['CAM_ID', 'CAM_ID_CollData']],
  ] as const)
    for (const row of tables.tables[sheet]?.rows || [])
      for (const key of keys) if (!isBlank(row.values[key])) used.add(String(row.values[key]))
  for (const c of pending.creates)
    for (const key of ['CAM_ID', 'CAM_ID_insectary']) if (c.values[key]) used.add(String(c.values[key]))
  return used
})
/**
 * Unused IDs from a Lists pool, continuing after the pool ID used most
 * recently in Collection_data (as the original entry prompt asked).
 */
function unusedPool(column: string, field: string) {
  const number = (id: string) => Number(/(\d+)$/.exec(id)?.[1] ?? -1)
  const pool = [...listColumn(lists.value, column)].sort((a, b) => number(a) - number(b))
  const inPool = new Set(pool)
  const latest = [...observed.value].reverse().find(r => inPool.has(String(r.values[field] ?? '')))
  const at = latest ? pool.indexOf(String(latest.values[field])) : -1
  return [...pool.slice(at + 1), ...pool.slice(0, at + 1)].filter(id => !usedCams.value.has(id))
}
const camPool = computed(() => unusedPool('Wild_indv_CAMid', 'CAM_ID'))
const insectaryCamPool = computed(() => unusedPool('InsectaryWild&Reared_CAMid', 'CAM_ID_insectary'))

const gridOptions = computed(() => ({
  ...options.value,
  CAM_ID: camPool.value.slice(0, 200),
  CAM_ID_insectary: insectaryCamPool.value.slice(0, 200),
}))

const subspeciesBySpecies = computed(() => {
  const map = new Map<string, Map<string, number>>()
  for (const row of observed.value) {
    const species = row.values.SPECIES
    const sub = row.values.Subspecies_Form
    if (isBlank(species) || isBlank(sub)) continue
    const counts = map.get(String(species)) || new Map<string, number>()
    counts.set(String(sub), (counts.get(String(sub)) || 0) + 1)
    map.set(String(species), counts)
  }
  return map
})
const rowOptions = {
  Subspecies_Form: (row: Record<string, CellValue>) => {
    const counts = subspeciesBySpecies.value.get(String(row.SPECIES ?? ''))
    return counts ? [...counts].sort((a, b) => b[1] - a[1]).map(([value]) => value) : options.value.Subspecies_Form || []
  },
}

function addIndividual() {
  const values: Record<string, CellValue> = {
    Release_Collect: releaseCollect.value,
    Collection_date: date.value ? isoToSerial(date.value) : null,
  }
  for (const field of SESSION_FIELDS) if (defaults.value[field]) values[field] = defaults.value[field]
  if (releaseCollect.value === 'Collected_Sent2Insectary') {
    const cam = insectaryCamPool.value[0]
    if (cam) values.CAM_ID_insectary = cam
  }
  for (const field of createFormulas.value) delete values[field]
  pending.addCreate(MODULE, String(values.CAM_ID_insectary || 'nuevo'), values)
  pending.touch()
}

const columns = computed(() =>
  table.value
    ? orderColumns(table.value.columns, [
        'Release_Collect',
        'Insectary_ID',
        'CAM_ID_insectary',
        'CAM_ID',
        'FieldMark_ID',
        'SPECIES',
        'Subspecies_Form',
        'Sex',
        'Collection_date',
        'Collection_time',
        'Collection_location',
        'Collector',
        'Identifier',
        'Tube_1_id',
        'Tube_1_tissue',
        'Death_date',
        'Preservation_date',
        'Notes_Collection_data',
      ])
    : [],
)
const recent = computed(() => observed.value.slice(-recentCount.value))
</script>

<template>
  <div class="flex h-full flex-col">
    <div class="toolbar">
      <label>
        <span class="field-label">Tipo</span>
        <select v-model="releaseCollect" class="field-input">
          <option value="Collected_Sent2Insectary">Colectado, enviado al insectario</option>
          <option value="Collected_Preserved">Colectado, preservado</option>
        </select>
      </label>
      <label>
        <span class="field-label">Fecha de colecta</span>
        <input v-model="date" type="date" class="field-input" />
      </label>
      <label v-for="field in ['Collection_location', 'Collector', 'Identifier']" :key="field" class="min-w-36">
        <span class="field-label">{{ field }}</span>
        <input v-model="defaults[field]" class="field-input" :list="`collect-${field}`" />
        <datalist :id="`collect-${field}`">
          <option v-for="o in (options[field] || []).slice(0, 200)" :key="o" :value="o" />
        </datalist>
      </label>
      <button class="btn-primary" :disabled="!ready" @click="addIndividual">
        <UserPlus :size="15" /> {{ ready ? 'Añadir individuo' : 'Cargando…' }}
      </button>
    </div>
    <p class="hint px-4 py-1">
      Las filas nuevas (verde) toman los valores de arriba y se escriben en las siguientes filas libres de Collection_data; las
      columnas con fórmula las calcula la hoja. Se muestran los últimos {{ recentCount }} registros.
      <button class="underline" @click="recentCount += 10">Cargar más</button>
    </p>
    <div class="min-h-0 flex-1">
      <p v-if="!table" class="p-6 text-stone-500">Cargando Collection_data…</p>
      <SheetGrid
        v-else
        :module="MODULE"
        :rows="recent"
        :creates="creates"
        :columns="columns"
        :options="gridOptions"
        :row-options="rowOptions"
        :create-formulas="createFormulas"
        :frozen="['Release_Collect']"
        label-field="CAM_ID"
        @notice="notify"
        @remove-create="
          id => {
            pending.removeCreate(id)
            pending.touch()
          }
        "
      />
    </div>
  </div>
</template>
