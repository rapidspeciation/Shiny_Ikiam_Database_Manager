<script setup lang="ts">
import { computed, onMounted, ref, watch } from 'vue'
import { Download, Plus, Printer, Wand2 } from 'lucide-vue-next'
import IdPicker from '../components/IdPicker.vue'
import SheetGrid from '../components/SheetGrid.vue'
import TubeLabels from '../components/TubeLabels.vue'
import { useSheet } from '../composables/useSheet'
import { api } from '../lib/api'
import { isBlank } from '../lib/cells'
import { isoToSerial, todayIso } from '../lib/dates'
import { errorText, notify } from '../lib/notice'
import { persistentRef } from '../lib/persist'
import { fillIfBlank, orderColumns, rowsById } from '../lib/rows'
import type { TableRow } from '../lib/types'
import { usePending } from '../stores/pending'
import { useSession } from '../stores/session'
import { initialsOf } from '../lib/rows'

/**
 * "Registrar Tubos": choose butterflies, then assign consecutive CAM IDs and
 * tube IDs with a default tissue, medium and preservation date.
 */
const MODULE = 'Insectary_data'
const WHOLE = 'WHOLE_ORGANISM'
const module = ref(MODULE)
const pending = usePending()
const session = useSession()
const { table, options } = useSheet(module)

interface Suggestion {
  value: string
  label: string
  medium?: string
  /** Kind of work the rack is used for: Cruces, Insectario, Monitoreo, Colecta. */
  context?: string
  date?: number | null
}
const camSuggestions = ref<Suggestion[]>([])
const tubeSuggestions = ref<Suggestion[]>([])

const picked = persistentRef<string[]>('tubes:picked', [])
const loaded = persistentRef<string[]>('tubes:loaded', [])
const camStart = persistentRef('tubes:cam', '')
const tubeStart = persistentRef('tubes:tube', '')
const tissue = persistentRef('tubes:tissue', '**OTHER_SOMATIC_ANIMAL_TISSUE** | WING CLIP')
const medium = persistentRef('tubes:medium', 'Flash frozen')
const presDate = persistentRef('tubes:date', todayIso())
const autofillNa = persistentRef('tubes:na', true)

onMounted(async () => {
  try {
    const [cam, tube] = await Promise.all([
      api<{ suggestions: Suggestion[] }>('ids?kind=cam'),
      api<{ suggestions: Suggestion[] }>('ids?kind=tube'),
    ])
    camSuggestions.value = cam.suggestions
    tubeSuggestions.value = tube.suggestions
    camStart.value ||= cam.suggestions[0]?.value || ''
    if (!tubeStart.value || tube.suggestions.some(t => t.value === tubeStart.value))
      tubeStart.value = bestRack()?.value || tubeStart.value
  } catch (e) {
    notify(errorText(e), 'error')
  }
})

const ids = computed(() => {
  if (!table.value) return []
  const out: string[] = []
  for (const row of table.value.rows) if (row.observed && row.values.Insectary_ID) out.push(String(row.values.Insectary_ID))
  return [...new Set(out)].reverse()
})
const rows = computed(() => (table.value ? rowsById(table.value.rows, 'Insectary_ID', loaded.value) : []))
const columns = computed(() =>
  table.value
    ? orderColumns(table.value.columns, [
        'Insectary_ID',
        'CAM_ID',
        'Death_date',
        'Death_cause',
        'Preservation_date',
        'Preserved_Dead_Alive',
        'Tube_1_id',
        'Tube_1_tissue',
        'T1_Preservation_medium',
        'Tube_2_id',
        'Tube_2_tissue',
        'T2_Preservation_medium',
        'Tube_3_id',
        'Tube_3_tissue',
        'Tube_4_id',
        'Tube_4_tissue',
      ])
    : [],
)
const tissues = computed(() => options.value.Tube_1_tissue || [WHOLE])
const mediums = computed(() => [
  ...new Set(['Flash frozen', 'Ethanol', 'DMSO', 'NA', ...(options.value.T1_Preservation_medium || [])]),
])

function load(append: boolean) {
  if (!picked.value.length) return notify('Elige al menos un ID')
  loaded.value = append ? [...new Set([...loaded.value, ...picked.value])] : [...picked.value]
  picked.value = []
  pending.touch()
}

/** Insectary racks first; the crosses rack when most loaded butterflies belong to crosses. */
const insectaryRacks = computed(() => tubeSuggestions.value.filter(s => s.context === 'Cruces' || s.context === 'Insectario'))
const otherRacks = computed(() => tubeSuggestions.value.filter(s => !insectaryRacks.value.includes(s)))
function bestRack() {
  const crosses = rows.value.filter(r =>
    /F1\/F2|WEST x EAST|cross|mutation/i.test(String(r.values.Research_purpose ?? '')),
  ).length
  const context = rows.value.length && crosses * 2 >= rows.value.length ? 'Cruces' : 'Insectario'
  return (
    tubeSuggestions.value.find(s => s.context === context && s.medium === medium.value) ||
    // Flash frozen and ethanol tubes live in different racks, so the medium matters more than the kind of work.
    insectaryRacks.value.find(s => s.medium === medium.value) ||
    tubeSuggestions.value.find(s => s.context === context) ||
    tubeSuggestions.value[0]
  )
}

// The app keeps choosing the rack (as butterflies are loaded or the medium changes) until the person picks one.
const rackChosen = ref(false)
watch([() => rows.value.length, medium], () => {
  if (!rackChosen.value && tubeSuggestions.value.some(t => t.value === tubeStart.value))
    tubeStart.value = bestRack()?.value || tubeStart.value
})

function pickTube(value: string) {
  rackChosen.value = true
  tubeStart.value = value
  const suggestion = tubeSuggestions.value.find(s => s.value === value)
  if (suggestion?.medium && mediums.value.includes(suggestion.medium)) medium.value = suggestion.medium
}

/** Fills CAM IDs and the next empty tube slot of every loaded row, in order. */
async function assign() {
  const target = rows.value
  if (!target.length) return notify('Carga primero las filas')
  const needCam = target.filter(r => isBlank(pending.value(r, 'CAM_ID'))).length
  const needTube = target.filter(r => firstEmptySlot(r) !== null).length
  try {
    const [cams, tubes] = await Promise.all([
      needCam && camStart.value ? sequence('cam', camStart.value, needCam) : [],
      needTube && tubeStart.value ? sequence('tube', tubeStart.value, needTube) : [],
    ])
    let camIndex = 0,
      tubeIndex = 0,
      filled = 0
    const date = presDate.value ? isoToSerial(presDate.value) : null
    for (const row of target) {
      const label = String(row.values.Insectary_ID)
      const set = (field: string, value: string | number | null, overwrite = false) => {
        if (fillIfBlank(MODULE, row, label, field, value, overwrite)) filled++
      }
      if (isBlank(pending.value(row, 'CAM_ID')) && cams[camIndex]) set('CAM_ID', cams[camIndex++])
      const slot = firstEmptySlot(row)
      if (slot === null || !tubes[tubeIndex]) continue
      set(`Tube_${slot}_id`, tubes[tubeIndex++])
      set(`Tube_${slot}_tissue`, tissue.value)
      // Wing clips have no date column yet: the date goes in the notes, as the team does.
      if (/WING CLIP/i.test(tissue.value) && presDate.value) {
        const note = `${noteDate(presDate.value)} ${initials.value}: Wing clip ${noteDate(presDate.value)}`
        const current = String(pending.value(row, 'Notes_Insectary_data') ?? '').trim()
        if (!current.includes('Wing clip ' + noteDate(presDate.value)))
          set('Notes_Insectary_data', current && !isBlank(current) ? `${current} | ${note}` : note, true)
      }
      if (slot <= 2) set(`T${slot}_Preservation_medium`, medium.value)
      if (tissue.value === WHOLE) {
        if (date !== null) {
          set('Preservation_date', date)
          set('Death_date', date)
        }
        set('Location_body', 'Ikiam')
        if (autofillNa.value)
          for (let next = slot + 1; next <= 4; next++) {
            set(`Tube_${next}_id`, 'NA')
            set(`Tube_${next}_tissue`, 'NA')
            if (next <= 2) set(`T${next}_Preservation_medium`, 'NOT_COLLECTED')
          }
      }
    }
    if (cams.length) camStart.value = nextAfter(cams.at(-1)!)
    if (tubes.length) tubeStart.value = nextAfter(tubes.at(-1)!)
    pending.touch()
    notify(filled ? `${filled} celdas completadas; revisa y guarda` : 'No había celdas vacías que completar')
  } catch (e) {
    notify(errorText(e), 'error')
  }
}

/** One label per tube ID in the loaded rows, including unsaved ones. */
const labels = computed(() =>
  rows.value.flatMap(row =>
    [1, 2, 3, 4]
      .map(slot => ({
        tube: String(pending.value(row, `Tube_${slot}_id`) ?? ''),
        id: String(row.values.Insectary_ID ?? ''),
        cam: String(pending.value(row, 'CAM_ID') ?? ''),
        tissue: String(pending.value(row, `Tube_${slot}_tissue`) ?? ''),
      }))
      .filter(label => !isBlank(label.tube)),
  ),
)
function printLabels() {
  if (!labels.value.length) return notify('No hay tubos con ID en la tabla')
  window.print()
}

/** "2026-09-27" → "27/9/26", the date form the team uses in notes. */
function noteDate(iso: string) {
  const [y, m, d] = iso.split('-').map(Number)
  return `${d}/${m}/${String(y).slice(2)}`
}
const initials = computed(() =>
  initialsOf(session.user?.displayName || session.user?.username || '', options.value.Collector || []),
)

function firstEmptySlot(row: TableRow): number | null {
  for (let slot = 1; slot <= 4; slot++)
    if (isBlank(pending.value(row, `Tube_${slot}_id`))) {
      // "NA" in an ID cell after a whole-organism tube means "no more tubes".
      if (pending.value(row, `Tube_${slot}_id`) === 'NA' && slot > 1) return null
      return slot
    }
  return null
}
async function sequence(kind: 'cam' | 'tube', start: string, count: number) {
  return (await api<{ sequence: string[] }>(`ids?kind=${kind}&start=${encodeURIComponent(start)}&count=${count}`)).sequence
}
function nextAfter(id: string) {
  const m = /^([A-Za-z]+)(\d+)$/.exec(id)
  return m ? `${m[1]}${String(Number(m[2]) + 1).padStart(m[2].length, '0')}` : id
}
</script>

<template>
  <div class="flex h-full flex-col">
    <div class="toolbar">
      <IdPicker v-model="picked" :options="ids" label="Insectary IDs" />
      <div class="flex gap-2">
        <button class="btn-primary" @click="load(false)"><Download :size="15" /> Cargar</button>
        <button class="btn" @click="load(true)"><Plus :size="15" /> Añadir a la tabla</button>
      </div>
    </div>
    <div class="toolbar">
      <label>
        <span class="field-label">CAM ID inicial</span>
        <input v-model="camStart" class="field-input w-36" list="cam-suggestions" autocapitalize="characters" />
        <datalist id="cam-suggestions">
          <option v-for="s in camSuggestions" :key="s.value" :value="s.value">{{ s.label }}</option>
        </datalist>
      </label>
      <label class="min-w-72">
        <span class="field-label">Rack en uso (siguiente tubo libre)</span>
        <select class="field-input" :value="tubeStart" @change="pickTube(($event.target as HTMLSelectElement).value)">
          <option v-if="tubeStart && !tubeSuggestions.some(s => s.value === tubeStart)" :value="tubeStart">
            {{ tubeStart }}
          </option>
          <optgroup label="Insectario y cruces">
            <option v-for="s in insectaryRacks" :key="s.value" :value="s.value">{{ s.label }}</option>
          </optgroup>
          <optgroup label="Colectas y monitoreo">
            <option v-for="s in otherRacks" :key="s.value" :value="s.value">{{ s.label }}</option>
          </optgroup>
        </select>
      </label>
      <label>
        <span class="field-label">o escribe el tubo</span>
        <input
          :value="tubeStart"
          class="field-input w-36"
          autocapitalize="characters"
          @change="pickTube(($event.target as HTMLInputElement).value)"
        />
      </label>
      <label class="min-w-56">
        <span class="field-label">Tejido por defecto</span>
        <select v-model="tissue" class="field-input">
          <option v-for="t in tissues" :key="t" :value="t">{{ t }}</option>
        </select>
      </label>
      <label>
        <span class="field-label">Medio por defecto</span>
        <select v-model="medium" class="field-input">
          <option v-for="m in mediums" :key="m" :value="m">{{ m }}</option>
        </select>
      </label>
      <label>
        <span class="field-label">Preservation_date</span>
        <input v-model="presDate" type="date" class="field-input" />
      </label>
      <label class="flex max-w-64 items-center gap-2 pb-1 text-xs">
        <input v-model="autofillNa" type="checkbox" /> Poner NA en los tubos siguientes si el tejido es WHOLE_ORGANISM
      </label>
      <button class="btn-primary" :disabled="!rows.length" @click="assign"><Wand2 :size="15" /> Asignar IDs</button>
      <button class="btn" :disabled="!rows.length" @click="printLabels"><Printer :size="15" /> Imprimir etiquetas</button>
    </div>
    <p class="hint px-4 py-1">
      "Asignar IDs" da CAM IDs y tubos consecutivos, saltando los ya usados, solo en celdas vacías y en el orden de la tabla.
      <button v-if="loaded.length" class="ml-2 underline" @click="loaded = []">Vaciar tabla</button>
    </p>
    <div class="min-h-0 flex-1">
      <p v-if="!table" class="p-6 text-stone-500">Cargando Insectary_data…</p>
      <p v-else-if="!rows.length" class="p-6 text-stone-500">Elige IDs arriba y pulsa Cargar.</p>
      <SheetGrid
        v-else
        :module="MODULE"
        :rows="rows"
        :columns="columns"
        :options="options"
        :frozen="['Insectary_ID']"
        :header-filters="false"
        :newest-first="false"
        label-field="Insectary_ID"
        @notice="notify"
      />
    </div>
    <TubeLabels :labels="labels" />
  </div>
</template>
