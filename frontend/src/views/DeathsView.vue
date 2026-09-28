<script setup lang="ts">
import DateField from '../components/DateField.vue'
import { computed, ref } from 'vue'
import { Download, Plus } from 'lucide-vue-next'
import IdPicker from '../components/IdPicker.vue'
import SheetGrid from '../components/SheetGrid.vue'
import { useSheet } from '../composables/useSheet'
import { isBlank } from '../lib/cells'
import { dayLabel, formatSerial, serialFromIso } from '../lib/dates'
import { notify } from '../lib/notice'
import { persistentRef } from '../lib/persist'
import { fillIfBlank, orderColumns, rowsById } from '../lib/rows'
import { usePending } from '../stores/pending'

/** "Registrar Muertes": choose butterflies, then set death date and cause for all of them. */
const MODULE = 'Insectary_data'
const module = ref(MODULE)
const pending = usePending()
const { table, ready, options } = useSheet(module)

/**
 * What the team writes for a butterfly that was not preserved (Unknown,
 * Disappearance, Eaten…), as in every such row of 2026: no CAM, no tubes,
 * media NOT_COLLECTED.
 */
const NOT_PRESERVED: Record<string, string> = {
  Preserved_Dead_Alive: 'NA',
  CAM_ID: 'NA',
  Tube_1_id: 'NA',
  Tube_1_tissue: 'NA',
  T1_Preservation_medium: 'NOT_COLLECTED',
  Tube_2_id: 'NA',
  Tube_2_tissue: 'NA',
  T2_Preservation_medium: 'NOT_COLLECTED',
  Tube_3_id: 'NA',
  Tube_3_tissue: 'NA',
  Tube_4_id: 'NA',
  Tube_4_tissue: 'NA',
  Preservation_medium: 'NOT_COLLECTED',
  Preservation_date: 'NA',
  Location_body: 'NA',
}

const picked = persistentRef<string[]>('deaths:picked', [])
const loaded = persistentRef<string[]>('deaths:loaded', [])
// The last date used stays on this device (a new tab does not reset it to today); its weekday is shown.
const date = persistentRef('deaths:date', '', { lasting: true })
const cause = persistentRef('deaths:cause', '')
const notPreserved = persistentRef('deaths:not-preserved', true)
const reviewOnly = persistentRef('deaths:review', false)
const recentCount = ref(30)

const ids = computed(() => {
  if (!table.value) return []
  const out: string[] = []
  for (const row of table.value.rows) if (row.observed && row.values.Insectary_ID) out.push(String(row.values.Insectary_ID))
  return [...new Set(out)].reverse()
})
const loadedRows = computed(() => (table.value ? rowsById(table.value.rows, 'Insectary_ID', loaded.value) : []))
/** The latest recorded deaths, newest death date first, so the tab never opens empty. */
const recentDeaths = computed(() => {
  if (!table.value) return []
  const chosen = new Set(loadedRows.value.map(r => r.id))
  return table.value.rows
    .filter(r => r.observed && typeof r.values.Death_date === 'number' && !chosen.has(r.id))
    .sort((a, b) => (b.values.Death_date as number) - (a.values.Death_date as number) || b.row - a.row)
    .slice(0, recentCount.value)
})
// Loaded IDs first (highlighted), then recent deaths.
const rows = computed(() => [...loadedRows.value, ...recentDeaths.value])
const highlight = computed(() => loadedRows.value.map(r => r.id))
const columns = computed(() =>
  table.value
    ? orderColumns(table.value.columns, [
        'Insectary_ID',
        'Death_date',
        'Death_cause',
        'Notes_Insectary_data',
        'CLUTCH NUMBER',
        'SPECIES',
        'Sex',
      ])
    : [],
)

const dateError = computed(() =>
  date.value && serialFromIso(date.value) === null ? 'Fecha no válida: el año debe estar entre 1990 y 2099' : '',
)

/** A butterfly already recorded dead is probably a mistyped ID (B9 of 2022 instead of B9D). */
function warn(id: string): string | null {
  const row = table.value ? rowsById(table.value.rows, 'Insectary_ID', [id])[0] : undefined
  const death = row?.values.Death_date
  return typeof death === 'number' ? `${id} ya murió el ${formatSerial(death)} (${row!.values.Death_cause ?? 'sin causa'})` : null
}

/** Loads the chosen rows; new rows get the date and cause where the cell is still empty. */
function load(append: boolean) {
  if (!table.value || !picked.value.length) return notify('Elige al menos un ID')
  if (dateError.value) return notify(dateError.value, 'error')
  const serial = date.value ? serialFromIso(date.value) : null
  const list = append ? [...new Set([...loaded.value, ...picked.value])] : [...picked.value]
  const fresh = append ? picked.value.filter(id => !loaded.value.includes(id)) : picked.value
  loaded.value = list
  let filled = 0
  if (!reviewOnly.value)
    for (const row of rowsById(table.value.rows, 'Insectary_ID', fresh)) {
      const label = String(row.values.Insectary_ID)
      const set = (field: string, value: string | number) => {
        if (fillIfBlank(MODULE, row, label, field, value)) filled++
      }
      if (serial !== null) set('Death_date', serial)
      if (cause.value) set('Death_cause', cause.value)
      // Not preserved: only rows without a CAM or tube yet (a preserved one keeps its IDs).
      const why = pending.value(row, 'Death_cause')
      if (
        notPreserved.value &&
        !isBlank(why) &&
        why !== 'Killed_Preserved' &&
        isBlank(pending.value(row, 'CAM_ID')) &&
        isBlank(pending.value(row, 'Tube_1_id'))
      )
        for (const [field, value] of Object.entries(NOT_PRESERVED)) set(field, value)
    }
  picked.value = []
  pending.touch()
  if (!date.value && !reviewOnly.value)
    notify('Sin fecha de muerte: elige la fecha y pulsa Cargar de nuevo para completarla', 'error')
  else notify(filled ? `${filled} celdas completadas; revisa y guarda` : 'Filas cargadas')
}
</script>

<template>
  <div class="flex h-full flex-col">
    <div class="toolbar">
      <IdPicker v-model="picked" :options="ids" :loading="!ready" :warn="warn" label="Insectary IDs" />
      <label>
        <span class="field-label">Fecha de muerte</span>
        <DateField v-model="date" class="field-input" />
        <span v-if="dateError" class="block text-xs text-red-700">{{ dateError }}</span>
        <span v-else-if="date" class="block text-xs text-stone-600">{{ dayLabel(date) }}</span>
        <span v-else class="block text-xs text-amber-800">Elige la fecha</span>
      </label>
      <label class="min-w-44">
        <span class="field-label">Causa por defecto</span>
        <input v-model="cause" class="field-input" list="death-causes" placeholder="p. ej. Natural" />
        <datalist id="death-causes">
          <option v-for="o in options.Death_cause || []" :key="o" :value="o" />
        </datalist>
      </label>
      <label
        class="flex max-w-64 items-center gap-2 pb-1.5 text-xs"
        title="Para causas distintas de Killed_Preserved y filas sin CAM ni tubo: CAM, tubos, tejidos, Preservation_date, Location_body y Preserved_Dead_Alive en NA; medios en NOT_COLLECTED"
      >
        <input v-model="notPreserved" type="checkbox" /> Sin preservar: CAM y tubos NA, medios NOT_COLLECTED
      </label>
      <label class="flex items-center gap-2 pb-1.5 text-sm"> <input v-model="reviewOnly" type="checkbox" /> Solo revisar </label>
      <div class="flex gap-2">
        <button class="btn-primary" @click="load(false)"><Download :size="15" /> Cargar</button>
        <button class="btn" @click="load(true)"><Plus :size="15" /> Añadir a la tabla</button>
      </div>
    </div>
    <p class="hint px-4 py-1">
      <template v-if="loaded.length">Arriba (resaltados) los {{ loadedRows.length }} IDs cargados; debajo, </template>
      <template v-else>Se muestran </template>
      las últimas {{ recentDeaths.length }} muertes registradas.
      <button class="underline" @click="recentCount += 30">ver más</button>
      <button v-if="loaded.length" class="ml-2 underline" @click="loaded = []">Quitar IDs cargados</button>
      · La fecha y la causa por defecto solo se escriben en las celdas vacías o NA de los IDs cargados.
    </p>
    <div class="min-h-0 flex-1">
      <p v-if="!ready" class="p-6 text-stone-500">Cargando Insectary_data…</p>
      <p v-else-if="!rows.length" class="p-6 text-stone-500">No hay muertes registradas. Elige IDs arriba y pulsa Cargar.</p>
      <SheetGrid
        v-else
        :module="MODULE"
        :rows="rows"
        :columns="columns"
        :options="options"
        :frozen="['Insectary_ID']"
        :highlight="highlight"
        :header-filters="false"
        :newest-first="false"
        label-field="Insectary_ID"
        @notice="notify"
      />
    </div>
  </div>
</template>
