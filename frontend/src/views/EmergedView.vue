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
 * pre-filled rows of Insectary_data. SPECIES is a formula there that predicts
 * the species from the clutch; each butterfly shows that prediction and can be
 * changed to another subspecies of the same species when that is what emerged.
 */
const MODULE = 'Insectary_data'
const STOCK_ORIGINS = ['deceptus', 'messenoides', 'intermedia']
const module = ref(MODULE)
const pending = usePending()
const { table, ready, stocks, options, creates, createFormulas, clutches } = useSheet(module)

const clutch = persistentRef('emerged:clutch', '')
const females = persistentRef('emerged:females', 0)
const males = persistentRef('emerged:males', 0)
const unknown = persistentRef('emerged:unknown', 0)
const startId = persistentRef('emerged:start', '')
const introDate = persistentRef('emerged:date', todayIso())
/** Free pre-made IDs: those after the last row used first (the suggestion), then earlier empty rows. */
const freeIds = ref<string[]>([])
/** The same IDs in sheet order, which a batch follows from its first ID (H0B → H1B → H2B). */
const inOrder = ref<string[]>([])
const idsLoaded = ref(false)
const recentCount = ref(15)

async function loadFreeIds() {
  try {
    const result = await api<{ sequence: string[]; rows: { value: string; row: number }[] }>('ids?kind=insectary&count=5000')
    const used = new Set(pending.creates.filter(c => c.module === MODULE).map(c => String(c.values.Insectary_ID)))
    freeIds.value = result.sequence.filter(id => !used.has(id))
    inOrder.value = [...result.rows].sort((a, b) => a.row - b.row).map(r => r.value).filter(id => !used.has(id))
    if (!startId.value || !freeIds.value.includes(startId.value)) startId.value = freeIds.value[0] || ''
    idsLoaded.value = true
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

/** Other subspecies of the clutch's species: what may emerge instead (e.g. eurydice from a proceriformis clutch). */
const siblings = computed(() => {
  const words = species.value.trim().split(/\s+/)
  if (words.length < 2 || !table.value) return species.value ? [species.value] : []
  const stem = words.slice(0, 2).join(' ').toLowerCase()
  const hybrid = / x |\bVS\b/i.test(species.value)
  const seen = new Set([species.value])
  for (const row of table.value.rows) {
    const value = String(row.values.SPECIES ?? '')
    if (value.toLowerCase().startsWith(stem + ' ') && (hybrid || !/ x |\bVS\b/i.test(value))) seen.add(value)
  }
  return [...seen].sort()
})
const gridOptions = computed(() => ({ ...options.value, SPECIES: siblings.value }))

/** Same rules as the Shiny app's apply_row_core_defaults, without the formula columns (the sheet fills those). */
function defaultsFor(speciesName: string): Record<string, CellValue> {
  const values: Record<string, CellValue> = { Wild_Reared: 'Reared' }
  const words = speciesName.trim().split(/\s+/)
  const origin = words.slice(0, 2).join(' ').toLowerCase() === 'mechanitis messenoides' ? words[2]?.toLowerCase() : ''
  values.Stock_of_origin = origin && STOCK_ORIGINS.includes(origin) ? origin : 'NA'
  if (/ x /i.test(speciesName)) values.Research_purpose = 'F1/F2 mutation rate'
  for (const field of createFormulas.value) if (field !== 'SPECIES') delete values[field]
  return values
}

function prepare(sexes: (string | null)[]) {
  if (!clutch.value) return notify('Elige el clutch')
  if (!sexes.length) return notify('Indica cuántas hembras, machos o sin sexo emergieron')
  if (!inOrder.value.length) return notify('No quedan filas preasignadas libres: crea más filas preasignadas en Insectary_data', 'error')
  const start = inOrder.value.indexOf(startId.value.trim().toUpperCase())
  if (start < 0) return notify(`${startId.value || 'Ese ID'} no es una fila preasignada libre de Insectary_data: elige uno de la lista`)
  const ids = inOrder.value.slice(start, start + sexes.length)
  if (ids.length < sexes.length)
    notify(`Solo hay ${ids.length} filas preasignadas libres desde ${ids[0]}: crea más filas preasignadas en Insectary_data`, 'error')
  ids.forEach((id, i) =>
    pending.addCreate(MODULE, id, {
      Insectary_ID: id,
      'CLUTCH NUMBER': clutch.value,
      // The clutch's prediction; change it only if another subspecies emerged.
      SPECIES: species.value || null,
      Sex: sexes[i],
      Intro2Insectary_date: introDate.value ? isoToSerial(introDate.value) : null,
      ...defaultsFor(species.value),
    }),
  )
  freeIds.value = freeIds.value.filter(id => !ids.includes(id))
  inOrder.value = inOrder.value.filter(id => !ids.includes(id))
  startId.value = freeIds.value[0] || ''
  pending.touch()
  notify(`${ids.length} filas nuevas (${ids[0]}–${ids.at(-1)}); revisa la subespecie si alguna es distinta`)
}
const batch = () => [
  ...Array(Math.max(0, females.value)).fill('female'),
  ...Array(Math.max(0, males.value)).fill('male'),
  ...Array(Math.max(0, unknown.value)).fill(null),
]

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
        <span class="field-label">Hembras</span>
        <input v-model.number="females" type="number" min="0" max="100" class="field-input w-20" />
      </label>
      <label>
        <span class="field-label">Machos</span>
        <input v-model.number="males" type="number" min="0" max="100" class="field-input w-20" />
      </label>
      <label>
        <span class="field-label">Sin sexo</span>
        <input v-model.number="unknown" type="number" min="0" max="100" class="field-input w-20" />
      </label>
      <label>
        <span class="field-label">Insectary ID inicial</span>
        <!-- Any free pre-made row can start the batch (earlier empty rows too); type to search. -->
        <input
          v-model="startId"
          class="field-input w-32 uppercase"
          list="emerged-free-ids"
          :placeholder="freeIds.length ? '' : 'no quedan'"
          :title="freeIds.length ? `${freeIds.length} filas preasignadas libres` : 'Crea más filas preasignadas en Insectary_data'"
          @focus="($event.target as HTMLInputElement).select()"
        />
        <datalist id="emerged-free-ids">
          <option v-for="id in freeIds" :key="id" :value="id" />
        </datalist>
      </label>
      <label>
        <span class="field-label">Intro a insectario</span>
        <input v-model="introDate" type="date" class="field-input" />
      </label>
      <div class="flex gap-2">
        <button class="btn-primary" @click="prepare(batch())">
          <Rows3 :size="15" /> Preparar {{ batch().length || '' }} filas
        </button>
        <button class="btn" @click="prepare([null])"><Plus :size="15" /> Añadir una</button>
      </div>
    </div>
    <p class="hint px-4 py-1">
      <template v-if="clutch"
        >Especie del clutch: <strong>{{ species || 'no encontrada' }}</strong
        >. Si una mariposa emergió de otra subespecie, cámbiala en su fila<template v-if="siblings.length > 1">
          ({{
            siblings
              .map(s => s.split(' ').slice(2).join(' '))
              .filter(Boolean)
              .join(', ')
          }})</template
        >.
      </template>
      <strong v-if="idsLoaded && !freeIds.length" class="text-amber-800"
        >No quedan filas preasignadas libres: crea más filas preasignadas en Insectary_data.</strong
      >
      Las filas nuevas usan las filas preasignadas; escribe cada ID en las alas. Debajo se muestran los últimos
      {{ recentCount }} registros.
      <button class="underline" @click="recentCount += 15">ver más</button>
    </p>
    <div class="min-h-0 flex-1">
      <p v-if="!ready" class="p-6 text-stone-500">Cargando Insectary_data…</p>
      <SheetGrid
        v-else
        :module="MODULE"
        :rows="recent"
        :creates="creates"
        :columns="columns"
        :options="gridOptions"
        :frozen="['Insectary_ID']"
        :locked-fields="['Collection_location']"
        :create-formulas="createFormulas.filter(f => f !== 'Insectary_ID' && f !== 'SPECIES')"
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
