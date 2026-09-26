<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { Rows3, Plus } from 'lucide-vue-next'
import SheetGrid from '../components/SheetGrid.vue'
import { useSheet } from '../composables/useSheet'
import { api } from '../lib/api'
import { isoToSerial, todayIso } from '../lib/dates'
import { errorText, notify } from '../lib/notice'
import { persistentRef } from '../lib/persist'
import { orderColumns } from '../lib/rows'
import type { CellValue } from '../lib/types'
import { usePending } from '../stores/pending'

/**
 * "Registrar Emergidos": new adults from a clutch go into the next unused
 * pre-filled rows of Insectary_data. SPECIES and Collection_location are
 * formulas in those rows, so the Sheet fills them from the clutch.
 */
const MODULE = 'Insectary_data'
const STOCK_ORIGINS = ['deceptus', 'messenoides', 'intermedia']
const module = ref(MODULE)
const pending = usePending()
const { table, stocks, options, creates, createFormulas, clutches } = useSheet(module)

const clutch = persistentRef('emerged:clutch', '')
const count = persistentRef('emerged:count', 1)
const startId = persistentRef('emerged:start', '')
const introDate = persistentRef('emerged:date', todayIso())
const freeIds = ref<string[]>([])
const recentCount = ref(15)

async function loadFreeIds() {
  try {
    const result = await api<{ sequence: string[] }>('ids?kind=insectary&count=200')
    const used = new Set(pending.creates.filter(c => c.module === MODULE).map(c => String(c.values.Insectary_ID)))
    freeIds.value = result.sequence.filter(id => !used.has(id))
    if (!startId.value || !freeIds.value.includes(startId.value)) startId.value = freeIds.value[0] || ''
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
watch(() => table.value?.revision, loadFreeIds, { immediate: true })

/** Species recorded for the clutch in Insectary_stocks (what the SPECIES formula will show). */
const species = computed(() => {
  const row = stocks.value?.rows.find(r => String(r.values['CLUTCH NUMBER']) === clutch.value)
  return row ? String(row.values.SPECIES || '') : ''
})

/** Same rules as the Shiny app's apply_row_core_defaults. */
function defaultsFor(speciesName: string): Record<string, CellValue> {
  const values: Record<string, CellValue> = { Wild_Reared: 'Reared', CAM_ID_CollData: 'NA' }
  const words = speciesName.trim().split(/\s+/)
  const origin = words.slice(0, 2).join(' ').toLowerCase() === 'mechanitis messenoides' ? words[2]?.toLowerCase() : ''
  values.Stock_of_origin = origin && STOCK_ORIGINS.includes(origin) ? origin : 'NA'
  if (/x/i.test(speciesName)) {
    values.Research_purpose = 'F1/F2 mutation rate'
    values.Pedigree = 'YES'
  }
  return values
}

function prepare(n: number) {
  if (!clutch.value) return notify('Elige el clutch')
  const start = freeIds.value.indexOf(startId.value)
  if (start < 0) return notify('Elige un Insectary ID inicial de la lista')
  const ids = freeIds.value.slice(start, start + n)
  if (ids.length < n) notify(`Solo hay ${ids.length} filas preasignadas libres`, 'error')
  for (const id of ids)
    pending.addCreate(MODULE, id, {
      Insectary_ID: id,
      'CLUTCH NUMBER': clutch.value,
      Intro2Insectary_date: introDate.value ? isoToSerial(introDate.value) : null,
      Sex: null,
      ...defaultsFor(species.value),
    })
  freeIds.value = freeIds.value.filter(id => !ids.includes(id))
  startId.value = freeIds.value[0] || ''
  pending.touch()
  notify(`${ids.length} filas nuevas; completa el sexo y guarda`)
}

const columns = computed(() =>
  table.value
    ? orderColumns(table.value.columns, [
        'Insectary_ID',
        'CLUTCH NUMBER',
        'Sex',
        'Intro2Insectary_date',
        'SPECIES',
        'Wild_Reared',
        'Stock_of_origin',
        'LIFESTAGE',
        'Research_purpose',
        'Pedigree',
        'Notes_Insectary_data',
      ])
    : [],
)
/** Recently recorded rows are shown below the new ones for context. */
const recent = computed(() => {
  if (!table.value) return []
  const observed = table.value.rows.filter(r => r.observed)
  return observed.slice(-recentCount.value)
})
</script>

<template>
  <div class="flex h-full flex-col">
    <div class="toolbar">
      <label class="min-w-40">
        <span class="field-label">CLUTCH NUMBER</span>
        <input v-model="clutch" class="field-input" list="clutches" placeholder="p. ej. 994(6)" />
        <datalist id="clutches">
          <option v-for="c in clutches" :key="c" :value="c" />
        </datalist>
      </label>
      <label>
        <span class="field-label">Número de individuos</span>
        <input v-model.number="count" type="number" min="1" max="100" class="field-input w-28" />
      </label>
      <label>
        <span class="field-label">Insectary ID inicial</span>
        <select v-model="startId" class="field-input w-32">
          <option v-for="id in freeIds.slice(0, 60)" :key="id" :value="id">{{ id }}</option>
        </select>
      </label>
      <label>
        <span class="field-label">Intro a insectario</span>
        <input v-model="introDate" type="date" class="field-input" />
      </label>
      <div class="flex gap-2">
        <button class="btn-primary" @click="prepare(count)"><Rows3 :size="15" /> Preparar filas</button>
        <button class="btn" @click="prepare(1)"><Plus :size="15" /> Añadir una</button>
      </div>
    </div>
    <p class="hint px-4 py-1">
      <template v-if="clutch"
        >Especie del clutch en Insectary_stocks: <strong>{{ species || 'no encontrada' }}</strong
        >.
      </template>
      Las filas nuevas (verde) usan las filas preasignadas; SPECIES y Collection_location los calcula la hoja al guardar. Debajo
      se muestran los últimos {{ recentCount }} registros.
      <button class="underline" @click="recentCount += 15">ver más</button>
    </p>
    <div class="min-h-0 flex-1">
      <p v-if="!table" class="p-6 text-stone-500">Cargando Insectary_data…</p>
      <SheetGrid
        v-else
        :module="MODULE"
        :rows="recent"
        :creates="creates"
        :columns="columns"
        :options="options"
        :frozen="['Insectary_ID']"
        :locked-fields="['SPECIES', 'Collection_location']"
        :create-formulas="createFormulas.filter(f => f !== 'Insectary_ID')"
        label-field="Insectary_ID"
        @notice="notify"
        @remove-create="
          id => {
            pending.removeCreate(id)
            pending.touch()
            loadFreeIds()
          }
        "
      />
    </div>
  </div>
</template>
