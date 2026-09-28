<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { Plus, Save, Trash2 } from 'lucide-vue-next'
import SheetGrid from '../components/SheetGrid.vue'
import { useSheet } from '../composables/useSheet'
import { api } from '../lib/api'
import { isBlank } from '../lib/cells'
import { isoToSerial, todayIso } from '../lib/dates'
import { errorText, notify } from '../lib/notice'
import { listColumn } from '../lib/options'
import { persistentRef } from '../lib/persist'
import { orderColumns } from '../lib/rows'
import type { CellValue } from '../lib/types'
import { usePending } from '../stores/pending'
import { useTables } from '../stores/tables'

/**
 * A day of field collection, entered in bulk. The header holds what the whole
 * outing shares (date, people, weather, place); each butterfly gets species,
 * sex and fate. Butterflies taken alive to the insectary get the next
 * Insectary ID (to write on the wings) and no CAM ID; their Insectary_data row
 * is filled in the same save. Butterflies preserved in the field get a CAM ID
 * and a tube. A second place can be added by changing the place and adding more.
 */
type Fate = 'insectario' | 'preservada' | 'liberada'
interface Draft {
  key: string
  location: string
  species: string
  subspecies: string
  sex: '' | 'female' | 'male' | 'NA'
  fate: Fate
  time: string
  purpose: string
  notes: string
  insectaryId: string
  cam: string
  tube: string
}
const FATES: Record<Fate, { label: string; value: string }> = {
  insectario: { label: 'Al insectario', value: 'Collected_Sent2Insectary' },
  preservada: { label: 'Preservada', value: 'Collected_Preserved' },
  liberada: { label: 'Liberada', value: 'Released_Unmarked' },
}
const MODULE = 'Collection_data'
const module = ref(MODULE)
const pending = usePending()
const tables = useTables()
const { table, lists, options, creates, createFormulas } = useSheet(module)
tables.load('Insectary_data').catch(() => {})

const header = persistentRef('collect:header', {
  date: todayIso(),
  collector: '',
  identifier: '',
  location: '',
  rainfall: '',
  cloud: '',
  medium: 'Flash frozen',
})
const drafts = persistentRef<Draft[]>('collect:drafts', [])
const addCount = ref(1)
const addFate = ref<Fate>('insectario')
const saving = ref(false)
const recentCount = ref(10)

const observed = computed(() => table.value?.rows.filter(r => r.observed) || [])
const latest = (field: string) => {
  const row = [...observed.value].reverse().find(r => !isBlank(r.values[field]))
  return row ? String(row.values[field]) : ''
}
watch(
  observed,
  rows => {
    if (!rows.length) return
    header.value.collector ||= latest('Collector')
    header.value.identifier ||= latest('Identifier')
  },
  { immediate: true },
)

/** Species in Collection_data are "Genus species"; the most used come first. */
const ranked = (field: string, filter?: (values: Record<string, CellValue>) => boolean) => {
  const counts = new Map<string, number>()
  for (const row of observed.value)
    if (!isBlank(row.values[field]) && (!filter || filter(row.values)))
      counts.set(String(row.values[field]), (counts.get(String(row.values[field])) || 0) + 1)
  return [...counts].sort((a, b) => b[1] - a[1]).map(([v]) => v)
}
const speciesList = computed(() => ranked('SPECIES'))
const subspeciesFor = (species: string) => ranked('Subspecies_Form', v => v.SPECIES === species)
const places = computed(() => [...new Set([...ranked('Collection_location'), ...(options.value.Collection_location || [])])])
const people = computed(() => options.value.Collector || ranked('Collector'))
const rainfalls = computed(() => options.value.Rainfall || ranked('Rainfall'))
const clouds = computed(() => options.value.Cloud_cover || ranked('Cloud_cover'))

// Insectary IDs: the pre-made unused rows of Insectary_data, in order.
const freeIds = ref<string[]>([])
async function loadFreeIds() {
  try {
    const { sequence } = await api<{ sequence: string[] }>('ids?kind=insectary&count=200')
    const pendingIds = new Set(pending.creates.filter(c => c.module === 'Insectary_data').map(c => String(c.values.Insectary_ID)))
    freeIds.value = sequence.filter(id => !pendingIds.has(id))
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
watch(() => tables.tables.Insectary_data?.revision, loadFreeIds, { immediate: true })
const nextInsectaryId = () => freeIds.value.find(id => !drafts.value.some(d => d.insectaryId === id)) || ''

// CAM IDs for field-preserved butterflies come from the Lists pool; tubes from the collection rack.
const usedCams = computed(() => {
  tables.version
  const used = new Set<string>()
  for (const [sheet, keys] of [
    [MODULE, ['CAM_ID', 'CAM_ID_insectary']],
    ['Insectary_data', ['CAM_ID', 'CAM_ID_CollData']],
  ] as const)
    for (const row of tables.tables[sheet]?.rows || [])
      for (const key of keys) if (!isBlank(row.values[key])) used.add(String(row.values[key]))
  return used
})
const camPool = computed(() => {
  const number = (id: string) => Number(/(\d+)$/.exec(id)?.[1] ?? -1)
  const pool = [...listColumn(lists.value, 'Wild_indv_CAMid')].sort((a, b) => number(a) - number(b))
  const at = pool.indexOf(latest('CAM_ID'))
  return [...pool.slice(at + 1), ...pool.slice(0, at + 1)].filter(id => !usedCams.value.has(id))
})
const nextCam = () => camPool.value.find(id => !drafts.value.some(d => d.cam === id)) || ''
const tubeRun = ref<string[]>([])
async function loadTubes() {
  try {
    const { suggestions } = await api<{ suggestions: { value: string; context: string; medium: string }[] }>('ids?kind=tube')
    const run =
      suggestions.find(s => s.context === 'Colecta' && s.medium === header.value.medium) ||
      suggestions.find(s => s.context === 'Colecta')
    tubeRun.value = run ? (await api<{ sequence: string[] }>(`ids?kind=tube&start=${run.value}&count=60`)).sequence : []
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
watch(() => header.value.medium, loadTubes, { immediate: true })
const nextTube = () => tubeRun.value.find(id => !drafts.value.some(d => d.tube === id)) || ''

function setFate(draft: Draft, fate: Fate) {
  draft.fate = fate
  draft.insectaryId = fate === 'insectario' ? draft.insectaryId || nextInsectaryId() : ''
  draft.cam = fate === 'preservada' ? draft.cam || nextCam() : ''
  draft.tube = fate === 'preservada' ? draft.tube || nextTube() : ''
}
function add() {
  if (!header.value.location) return notify('Elige el lugar de colecta')
  for (let i = 0; i < Math.max(1, addCount.value); i++) {
    const last = drafts.value.at(-1)
    const draft: Draft = {
      key: crypto.randomUUID(),
      location: header.value.location,
      species: last?.species ?? '',
      subspecies: last?.subspecies ?? '',
      sex: '',
      fate: addFate.value,
      time: '',
      purpose: '',
      notes: '',
      insectaryId: '',
      cam: '',
      tube: '',
    }
    drafts.value.push(draft)
    setFate(drafts.value.at(-1)!, addFate.value)
  }
}
function remove(key: string) {
  drafts.value = drafts.value.filter(d => d.key !== key)
}
const groups = computed(() => {
  const out = new Map<string, Record<Fate, number>>()
  for (const d of drafts.value) {
    const g = out.get(d.location) || { insectario: 0, preservada: 0, liberada: 0 }
    g[d.fate]++
    out.set(d.location, g)
  }
  return [...out]
})
const problems = computed(() =>
  drafts.value.flatMap((d, i) => {
    const n = i + 1
    const out: string[] = []
    if (!d.species) out.push(`fila ${n}: falta la especie`)
    if (!d.sex) out.push(`fila ${n}: falta el sexo`)
    if (d.fate === 'insectario' && !d.insectaryId) out.push(`fila ${n}: no quedan Insectary IDs libres`)
    if (d.fate === 'preservada' && (!d.cam || !d.tube)) out.push(`fila ${n}: falta CAM o tubo`)
    return out
  }),
)

const serial = (iso: string) => (iso ? isoToSerial(iso) : null)
const dayFraction = (time: string) => {
  const m = /^(\d{1,2}):(\d{2})$/.exec(time.trim())
  return m ? (Number(m[1]) * 60 + Number(m[2])) / (24 * 60) : null
}
/** The same columns the team fills by hand (checked against the 23-Sep-26 rows). */
function collectionRow(d: Draft): Record<string, CellValue> {
  const date = serial(header.value.date)
  const values: Record<string, CellValue> = {
    Release_Collect: FATES[d.fate].value,
    FieldMark_ID: 'NA',
    Insectary_ID: d.fate === 'insectario' ? d.insectaryId : 'NA',
    SPECIES: d.species,
    Subspecies_Form: d.subspecies || 'NA',
    Identifier: header.value.identifier || null,
    ID_status: 'COMPLETE',
    Sex: d.sex,
    Collection_location: d.location,
    Transect_section: 'NA',
    Bait: 'NA',
    Forest_stratum: 'NA',
    Collection_date: date,
    Collection_time: dayFraction(d.time),
    Collector: header.value.collector || null,
    Rainfall: header.value.rainfall || null,
    Cloud_cover: header.value.cloud || null,
    Flight_height: 'NA',
    Purpose: d.purpose || null,
    Notes_Collection_data: d.notes || null,
  }
  if (d.fate === 'preservada')
    Object.assign(values, {
      CAM_ID_insectary: 'NA',
      CAM_ID: d.cam,
      Tube_1_id: d.tube,
      Tube_1_tissue: 'WHOLE_ORGANISM',
      Tube_2_id: 'NA',
      Tube_2_tissue: 'NOT_COLLECTED',
      Tube_3_id: 'NA',
      Tube_3_tissue: 'NOT_PROVIDED',
      Tube_4_id_LEGS: 'NA',
      Butterfly_weight: 'NA',
      Death_date: date,
      Preservation_date: date,
      Preservation_medium: header.value.medium,
      Preserved_dead_alive: 'Alive',
      Splitted_body: 'No',
      Location_Head: 'Ikiam',
      Location_Torax: 'Ikiam',
      Location_abdomen: 'Ikiam',
      Location_Legs: 'Ikiam',
      Location_wings: 'Ikiam',
    })
  for (const field of createFormulas.value) delete values[field]
  for (const [k, v] of Object.entries(values)) if (v === null || v === '') delete values[k]
  return values
}
/** The butterfly's row in Insectary_data (a pre-made row with that ID). */
const insectaryRow = (d: Draft): Record<string, CellValue> => ({
  Insectary_ID: d.insectaryId,
  Wild_Reared: 'Wild-caught',
  'CLUTCH NUMBER': 'NA',
  Stock_of_origin: 'NA',
  SPECIES: [d.species, d.subspecies].filter(s => s && s !== 'NA').join(' '),
  Sex: d.sex,
  Collection_location: d.location,
  Intro2Insectary_date: serial(header.value.date),
})

async function save() {
  if (!drafts.value.length) return
  if (problems.value.length) return notify(problems.value.slice(0, 3).join('; '), 'error')
  saving.value = true
  const added: string[] = []
  for (const d of drafts.value) {
    added.push(pending.addCreate(MODULE, d.insectaryId || d.cam || 'nuevo', collectionRow(d)).clientId)
    if (d.fate === 'insectario') added.push(pending.addCreate('Insectary_data', d.insectaryId, insectaryRow(d)).clientId)
  }
  pending.touch()
  try {
    await pending.save(`Colecta ${header.value.date}`)
    if (added.some(id => pending.creates.some(c => c.clientId === id))) throw new Error('Revisa los errores marcados en la tabla')
    notify(`Colecta guardada: ${drafts.value.length} mariposas`, 'success')
    drafts.value = []
    loadFreeIds()
    loadTubes()
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    saving.value = false
  }
}

const columns = computed(() =>
  table.value
    ? orderColumns(table.value.columns, [
        'Collection_date',
        'Collection_location',
        'Release_Collect',
        'Insectary_ID',
        'CAM_ID',
        'Tube_1_id',
        'SPECIES',
        'Subspecies_Form',
        'Sex',
        'Collector',
        'Identifier',
        'Notes_Collection_data',
      ])
    : [],
)
const recent = computed(() => observed.value.slice(-recentCount.value))
</script>

<template>
  <div class="flex h-full flex-col overflow-y-auto">
    <div class="toolbar">
      <label>
        <span class="field-label">Fecha</span>
        <input v-model="header.date" type="date" class="field-input" />
      </label>
      <label class="min-w-52">
        <span class="field-label">Colector</span>
        <input v-model="header.collector" class="field-input" list="collect-people" />
      </label>
      <label class="min-w-52">
        <span class="field-label">Identificador</span>
        <input v-model="header.identifier" class="field-input" list="collect-people" />
      </label>
      <label>
        <span class="field-label">Lluvia</span>
        <select v-model="header.rainfall" class="field-input">
          <option value="">—</option>
          <option v-for="r in rainfalls" :key="r" :value="r">{{ r }}</option>
        </select>
      </label>
      <label>
        <span class="field-label">Nubosidad</span>
        <select v-model="header.cloud" class="field-input">
          <option value="">—</option>
          <option v-for="c in clouds" :key="c" :value="c">{{ c }}</option>
        </select>
      </label>
      <datalist id="collect-people">
        <option v-for="p in people" :key="p" :value="p" />
      </datalist>
    </div>
    <div class="toolbar border-t-0">
      <label class="min-w-64">
        <span class="field-label">Lugar (cámbialo para añadir mariposas de otro sitio)</span>
        <input v-model="header.location" class="field-input" list="collect-places" />
        <datalist id="collect-places">
          <option v-for="p in places" :key="p" :value="p" />
        </datalist>
      </label>
      <label>
        <span class="field-label">Añadir</span>
        <input v-model.number="addCount" type="number" min="1" max="60" class="field-input w-20" />
      </label>
      <label>
        <span class="field-label">Destino</span>
        <select v-model="addFate" class="field-input">
          <option v-for="(f, key) in FATES" :key="key" :value="key">{{ f.label }}</option>
        </select>
      </label>
      <button class="btn-primary" @click="add"><Plus :size="15" /> Añadir mariposas</button>
      <label v-if="drafts.some(d => d.fate === 'preservada')">
        <span class="field-label">Medio (preservadas)</span>
        <select v-model="header.medium" class="field-input">
          <option>Flash frozen</option>
          <option>Ethanol</option>
          <option>DMSO</option>
        </select>
      </label>
    </div>

    <div v-if="drafts.length" class="border-b border-stone-200 bg-white px-3 py-2">
      <div class="overflow-x-auto">
        <table class="w-full text-sm">
          <thead class="text-left text-xs text-stone-600">
            <tr>
              <th class="px-1 py-1">#</th>
              <th class="px-1">Lugar</th>
              <th class="px-1">Especie</th>
              <th class="px-1">Subespecie</th>
              <th class="px-1">Sexo</th>
              <th class="px-1">Destino</th>
              <th class="px-1">Insectary ID / CAM · tubo</th>
              <th class="px-1">Hora</th>
              <th class="px-1">Propósito</th>
              <th class="px-1">Notas</th>
              <th></th>
            </tr>
          </thead>
          <tbody>
            <tr v-for="(d, i) in drafts" :key="d.key" class="border-t border-stone-100 align-middle">
              <td class="px-1 text-stone-500 tabular-nums">{{ i + 1 }}</td>
              <td class="px-1"><input v-model="d.location" class="field-input w-44" list="collect-places" /></td>
              <td class="px-1">
                <input v-model="d.species" class="field-input w-48" list="collect-species" @change="d.subspecies = ''" />
              </td>
              <td class="px-1">
                <select v-model="d.subspecies" class="field-input w-36">
                  <option value="">—</option>
                  <option v-for="s in subspeciesFor(d.species)" :key="s" :value="s">{{ s }}</option>
                </select>
              </td>
              <td class="px-1 whitespace-nowrap">
                <button
                  v-for="[value, sign] in [
                    ['female', '♀'],
                    ['male', '♂'],
                    ['NA', '?'],
                  ] as const"
                  :key="value"
                  type="button"
                  class="mr-0.5 h-8 w-8 rounded border text-base"
                  :class="d.sex === value ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white'"
                  :aria-label="value"
                  @click="d.sex = value"
                >
                  {{ sign }}
                </button>
              </td>
              <td class="px-1">
                <select
                  :value="d.fate"
                  class="field-input w-36"
                  @change="setFate(d, ($event.target as HTMLSelectElement).value as Fate)"
                >
                  <option v-for="(f, key) in FATES" :key="key" :value="key">{{ f.label }}</option>
                </select>
              </td>
              <td class="px-1 whitespace-nowrap">
                <span
                  v-if="d.fate === 'insectario'"
                  class="rounded bg-brand-50 px-2 py-1 text-base font-semibold tracking-wide text-brand-800"
                >
                  {{ d.insectaryId || '—' }}
                </span>
                <template v-else-if="d.fate === 'preservada'">
                  <input v-model="d.cam" class="field-input w-28" />
                  <input v-model="d.tube" class="field-input ml-1 w-28" />
                </template>
              </td>
              <td class="px-1"><input v-model="d.time" class="field-input w-20" placeholder="hh:mm" /></td>
              <td class="px-1"><input v-model="d.purpose" class="field-input w-28" list="collect-purposes" /></td>
              <td class="px-1"><input v-model="d.notes" class="field-input w-48" /></td>
              <td class="px-1">
                <button class="btn-ghost" title="Quitar" @click="remove(d.key)"><Trash2 :size="14" /></button>
              </td>
            </tr>
          </tbody>
        </table>
        <datalist id="collect-species">
          <option v-for="s in speciesList" :key="s" :value="s" />
        </datalist>
        <datalist id="collect-purposes">
          <option v-for="p in options.Purpose || []" :key="p" :value="p" />
        </datalist>
      </div>
      <div class="mt-2 flex flex-wrap items-center gap-3 text-sm">
        <span v-for="[place, g] in groups" :key="place" class="rounded bg-stone-100 px-2 py-0.5">
          {{ place }}: {{ g.insectario }} al insectario · {{ g.preservada }} preservadas<template v-if="g.liberada">
            · {{ g.liberada }} liberadas</template
          >
        </span>
        <span v-if="problems.length" class="text-xs text-amber-800"
          >{{ problems[0] }}<template v-if="problems.length > 1"> (y {{ problems.length - 1 }} más)</template></span
        >
        <button class="btn-primary ml-auto" :disabled="saving || !!problems.length" @click="save">
          <Save :size="15" /> {{ saving ? 'Guardando…' : `Guardar colecta (${drafts.length})` }}
        </button>
      </div>
      <p class="hint mt-1">
        Las que van al insectario reciben el Insectary ID para escribir en las alas, sin CAM ID (se da al hacer el wing clip o al
        preservar). Se guardan en Collection_data e Insectary_data a la vez.
      </p>
    </div>

    <p class="hint px-4 py-1">
      Últimos {{ recentCount }} registros de Collection_data.
      <button class="underline" @click="recentCount += 20">ver más</button>
    </p>
    <div class="min-h-80 flex-1">
      <p v-if="!table" class="p-6 text-stone-500">Cargando Collection_data…</p>
      <SheetGrid
        v-else
        :module="MODULE"
        :rows="recent"
        :creates="creates"
        :columns="columns"
        :options="options"
        :frozen="['Collection_date']"
        :create-formulas="createFormulas"
        label-field="Insectary_ID"
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
