<script setup lang="ts">
import { computed, ref } from 'vue'
import { Download, Plus } from 'lucide-vue-next'
import IdPicker from '../components/IdPicker.vue'
import SheetGrid from '../components/SheetGrid.vue'
import { useSheet } from '../composables/useSheet'
import { isoToSerial, todayIso } from '../lib/dates'
import { notify } from '../lib/notice'
import { persistentRef } from '../lib/persist'
import { fillIfBlank, orderColumns, rowsById } from '../lib/rows'
import { usePending } from '../stores/pending'

/** "Registrar Muertes": choose butterflies, then set death date and cause for all of them. */
const MODULE = 'Insectary_data'
const module = ref(MODULE)
const pending = usePending()
const { table, options } = useSheet(module)

const picked = persistentRef<string[]>('deaths:picked', [])
const loaded = persistentRef<string[]>('deaths:loaded', [])
const date = persistentRef('deaths:date', todayIso())
const cause = persistentRef('deaths:cause', '')
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

/** Loads the chosen rows; new rows get the date and cause where the cell is still empty. */
function load(append: boolean) {
  if (!table.value || !picked.value.length) return notify('Elige al menos un ID')
  const list = append ? [...new Set([...loaded.value, ...picked.value])] : [...picked.value]
  const fresh = append ? picked.value.filter(id => !loaded.value.includes(id)) : picked.value
  loaded.value = list
  let filled = 0
  if (!reviewOnly.value)
    for (const row of rowsById(table.value.rows, 'Insectary_ID', fresh)) {
      const label = String(row.values.Insectary_ID)
      if (date.value && fillIfBlank(MODULE, row, label, 'Death_date', isoToSerial(date.value))) filled++
      if (cause.value && fillIfBlank(MODULE, row, label, 'Death_cause', cause.value)) filled++
    }
  picked.value = []
  pending.touch()
  notify(filled ? `${filled} celdas completadas; revisa y guarda` : 'Filas cargadas')
}
</script>

<template>
  <div class="flex h-full flex-col">
    <div class="toolbar">
      <IdPicker v-model="picked" :options="ids" label="Insectary IDs" />
      <label>
        <span class="field-label">Fecha de muerte</span>
        <input v-model="date" type="date" class="field-input" />
      </label>
      <label class="min-w-44">
        <span class="field-label">Causa por defecto</span>
        <input v-model="cause" class="field-input" list="death-causes" placeholder="p. ej. Natural" />
        <datalist id="death-causes">
          <option v-for="o in options.Death_cause || []" :key="o" :value="o" />
        </datalist>
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
      <p v-if="!table" class="p-6 text-stone-500">Cargando Insectary_data…</p>
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
