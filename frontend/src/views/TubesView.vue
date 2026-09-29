<script setup lang="ts">
import ChoiceField from '../components/ChoiceField.vue'
import DateField from '../components/DateField.vue'
import { computed, onMounted, ref, watch } from 'vue'
import { Download, Plus, Printer, Wand2 } from 'lucide-vue-next'
import IdPicker from '../components/IdPicker.vue'
import SheetGrid from '../components/SheetGrid.vue'
import TubeLabels from '../components/TubeLabels.vue'
import { useSheet } from '../composables/useSheet'
import { api } from '../lib/api'
import { isBlank } from '../lib/cells'
import { dayLabel, formatSerial, serialFromIso, todayIso } from '../lib/dates'
import { errorText, notify } from '../lib/notice'
import { persistentRef } from '../lib/persist'
import { fillIfBlank, initialsOf, orderColumns, rowsById } from '../lib/rows'
import type { CellValue, TableRow } from '../lib/types'
import { usePending } from '../stores/pending'
import { useSession } from '../stores/session'
import { t, tx, type Msg } from '../lib/i18n'

/**
 * "Registrar Tubos": choose butterflies, then assign consecutive CAM IDs and
 * tube IDs with a default tissue, medium and preservation date.
 */
const MODULE = 'Insectary_data'
const WHOLE = 'WHOLE_ORGANISM'
const WING_CLIP = '**OTHER_SOMATIC_ANIMAL_TISSUE** | WING CLIP'
const module = ref(MODULE)
const pending = usePending()
const session = useSession()
const { table, ready, options } = useSheet(module)

interface Suggestion {
  value: string
  label: string
  /** The label's descriptor (the rack's context and medium in the interface language). */
  labelMsg?: Msg
  medium?: string
  /** Kind of work the rack is used for: Cruces, Insectario, Monitoreo, Colecta. */
  context?: string
  date?: number | null
}
/** GET ids?kind=…&start=: the IDs from `start`, and where `start` is used if it is. */
interface Sequence {
  sequence: string[]
  startUsed?: { value: string; sheet: string; row: number; label: string | null }
  nextFree?: string
}
const camSuggestions = ref<Suggestion[]>([])
const tubeSuggestions = ref<Suggestion[]>([])

const picked = persistentRef<string[]>('tubes:picked', [])
const loaded = persistentRef<string[]>('tubes:loaded', [])
const camStart = persistentRef('tubes:cam', '')
const tubeStart = persistentRef('tubes:tube', '')
const tissue = persistentRef('tubes:tissue', WHOLE)
const medium = persistentRef('tubes:medium', 'Flash frozen')
// The last dates used are kept on this device (not reset to today in a new tab), with the weekday shown.
const presDate = persistentRef('tubes:date', '', { lasting: true })
const clipDate = persistentRef('tubes:clip-date', '', { lasting: true })
const initialsTyped = persistentRef('tubes:initials', '', { lasting: true })
const autofillNa = persistentRef('tubes:na', true)
/** A starting CAM or tube that is already used, and the next free one to offer. */
const startIssue = ref<{ kind: 'cam' | 'tube'; text: string; next?: string } | null>(null)
watch([camStart, tubeStart], () => (startIssue.value = null))

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
        'Preservation_medium',
        'Location_body',
      ])
    : [],
)
const tissues = computed(() => options.value.Tube_1_tissue || [WHOLE])
const mediums = computed(() => [
  ...new Set(['Flash frozen', 'Ethanol', 'DMSO', 'NA', ...(options.value.T1_Preservation_medium || [])]),
])
const isClip = computed(() => /WING CLIP/i.test(tissue.value))

/** A butterfly that died long ago or already has its tubes is probably a mistyped ID. */
function warn(id: string): string | null {
  const row = table.value ? rowsById(table.value.rows, 'Insectary_ID', [id])[0] : undefined
  if (!row) return null
  const cam = row.values.CAM_ID
  const tube = row.values.Tube_1_id
  if (!isBlank(cam) && !isBlank(tube))
    return t('{id} ya tiene {cam} y el tubo {tube}', { id, cam: String(cam), tube: String(tube) })
  const death = row.values.Death_date
  const today = serialFromIso(todayIso())
  if (typeof death === 'number' && today !== null && today - death > 7)
    return t('{id} ya murió el {date} (hace {n} días)', { id, date: formatSerial(death), n: today - death })
  return null
}

/**
 * Loads the chosen rows. The tissue follows them: butterflies killed to be
 * preserved go whole (WHOLE_ORGANISM); living ones get a wing clip.
 */
function load(append: boolean) {
  if (!picked.value.length) return notify(t('Elige al menos un ID'))
  loaded.value = append ? [...new Set([...loaded.value, ...picked.value])] : [...picked.value]
  picked.value = []
  const dead = rows.value.filter(r => !isBlank(pending.value(r, 'Death_cause')) || !isBlank(pending.value(r, 'Death_date')))
  const killed = dead.filter(r => pending.value(r, 'Death_cause') === 'Killed_Preserved')
  const next = dead.length * 2 > rows.value.length ? WHOLE : rows.value.length ? WING_CLIP : tissue.value
  if (next !== tissue.value && tissues.value.includes(next)) {
    tissue.value = next
    notify(
      next === WHOLE
        ? t('Tejido: {tissue} ({n} de {total} filas son muertes)', {
            tissue: WHOLE,
            n: killed.length || dead.length,
            total: rows.value.length,
          })
        : t('Tejido: WING CLIP (mariposas vivas)'),
    )
  }
  pending.touch()
}

/** Insectary racks first; the crosses rack when most loaded butterflies belong to crosses. */
const insectaryRacks = computed(() => tubeSuggestions.value.filter(s => s.context === 'Cruces' || s.context === 'Insectario'))
const otherRacks = computed(() => tubeSuggestions.value.filter(s => !insectaryRacks.value.includes(s)))
/** The racks by kind of work; a tube typed below that is not a rack's next one is listed first. */
const rackChoices = computed(() => [
  ...(tubeStart.value && !tubeSuggestions.value.some(s => s.value === tubeStart.value)
    ? [{ value: tubeStart.value, label: tubeStart.value }]
    : []),
  ...insectaryRacks.value.map(s => ({ value: s.value, label: tx(s.label, s.labelMsg), group: t('Insectario y cruces') })),
  ...otherRacks.value.map(s => ({ value: s.value, label: tx(s.label, s.labelMsg), group: t('Colectas y monitoreo') })),
])
/** Suggested CAM IDs, with the date of their run in grey ("CAM078278 27-Sep-26" → "27-Sep-26"). */
const camChoices = computed(() =>
  camSuggestions.value.map(s => ({
    value: s.value,
    label: s.value,
    hint: s.label.startsWith(s.value) ? s.label.slice(s.value.length).trim() : s.label,
  })),
)
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
  tubeStart.value = value.trim().toUpperCase()
  const suggestion = tubeSuggestions.value.find(s => s.value === value)
  if (suggestion?.medium && mediums.value.includes(suggestion.medium)) medium.value = suggestion.medium
}

const presError = computed(() =>
  presDate.value && serialFromIso(presDate.value) === null ? t('Fecha no válida: el año debe estar entre 1990 y 2099') : '',
)
const clipError = computed(() =>
  clipDate.value && serialFromIso(clipDate.value) === null ? t('Fecha no válida: el año debe estar entre 1990 y 2099') : '',
)
const initials = computed(
  () =>
    initialsTyped.value.trim().toUpperCase() ||
    initialsOf(session.user?.displayName || '', options.value.Collector || [], session.user?.username || ''),
)

const notePreview = computed(() => {
  const clip = clipDate.value ? serialFromIso(clipDate.value) : null
  return `${noteDate(serialFromIso(todayIso())!)} ${initials.value}: Wing clip ${clip === null ? t('(fecha del corte)') : noteDate(clip)}`
})

/** Where an unsaved change of another row already holds this CAM or tube. */
function pendingHolder(field: RegExp, value: string) {
  for (const edit of Object.values(pending.edits))
    for (const [f, v] of Object.entries(edit.values))
      if (field.test(f) && String(v ?? '').trim() === value && !loaded.value.includes(edit.label))
        return t('{value} ya está en {sheet} fila {row} ({label}), sin guardar todavía', {
          value,
          sheet: edit.module,
          row: edit.row,
          label: edit.label,
        })
  return null
}
/** Says why a starting ID cannot be used (and offers the next free one) instead of starting from another. */
function checkStart(kind: 'cam' | 'tube', start: string, result: Sequence | null) {
  const holder = result?.startUsed
  const text = holder
    ? t('{value} ya está usado en {sheet} fila {row}{label}', {
        value: holder.value,
        sheet: holder.sheet,
        row: holder.row,
        label: holder.label && holder.label !== holder.value ? ` (${holder.label})` : '',
      })
    : pendingHolder(kind === 'cam' ? /^CAM_ID$/ : /^Tube_\d_id/, start)
  if (!text) return true
  const what = kind === 'cam' ? t('CAM ID inicial') : t('tubo inicial')
  startIssue.value = { kind, text: `${what}: ${text}.`, next: result?.nextFree }
  notify(
    result?.nextFree
      ? t('{problem}. No se asignó nada; el siguiente libre es {next}.', { problem: text, next: result.nextFree })
      : t('{problem}. No se asignó nada.', { problem: text }),
    'error',
  )
  return false
}
function useNext() {
  const issue = startIssue.value
  if (!issue?.next) return
  if (issue.kind === 'cam') camStart.value = issue.next
  else pickTube(issue.next)
  startIssue.value = null
}

/** A value the team writes for "nothing here yet", which a fill may replace. */
const placeholder = (value: CellValue) => isBlank(value) || /^(NOT_COLLECTED|NOT_PROVIDED)$/.test(String(value).trim())

/** Fills CAM IDs and the next empty tube slot of every loaded row, in order. */
async function assign() {
  const target = rows.value
  if (!target.length) return notify(t('Carga primero las filas'))
  const date = presDate.value ? serialFromIso(presDate.value) : null
  const clip = clipDate.value ? serialFromIso(clipDate.value) : null
  if (isClip.value) {
    // The clip date goes in the notes: never a silent "today".
    if (clip === null) return notify(clipError.value || t('Escribe la fecha del corte de ala'), 'error')
  } else if (tissue.value === WHOLE && presDate.value && date === null) return notify(presError.value, 'error')
  else if (tissue.value === WHOLE && !presDate.value) return notify(t('Escribe la fecha de preservación'), 'error')
  const needCam = target.filter(r => isBlank(pending.value(r, 'CAM_ID'))).length
  const needTube = target.filter(r => firstEmptySlot(r) !== null).length
  try {
    const [camSeq, tubeSeq] = await Promise.all([
      needCam && camStart.value ? sequence('cam', camStart.value, needCam) : null,
      needTube && tubeStart.value ? sequence('tube', tubeStart.value, needTube) : null,
    ])
    if (camSeq && !checkStart('cam', camStart.value.trim().toUpperCase(), camSeq)) return
    if (tubeSeq && !checkStart('tube', tubeStart.value.trim().toUpperCase(), tubeSeq)) return
    const cams = camSeq?.sequence || []
    const tubes = tubeSeq?.sequence || []
    let camIndex = 0,
      tubeIndex = 0,
      filled = 0
    for (const row of target) {
      const label = String(row.values.Insectary_ID)
      const set = (field: string, value: CellValue, overwrite = false) => {
        if (fillIfBlank(MODULE, row, label, field, value, overwrite)) filled++
      }
      // Also replaces NOT_COLLECTED, e.g. left by Muertes on a butterfly then preserved after all.
      const put = (field: string, value: CellValue) => set(field, value, placeholder(pending.value(row, field)))
      if (isBlank(pending.value(row, 'CAM_ID')) && cams[camIndex]) set('CAM_ID', cams[camIndex++])
      const slot = firstEmptySlot(row)
      if (slot === null || !tubes[tubeIndex]) continue
      set(`Tube_${slot}_id`, tubes[tubeIndex++])
      set(`Tube_${slot}_tissue`, tissue.value)
      if (slot <= 2) put(`T${slot}_Preservation_medium`, medium.value)
      // Wing clips have no date column yet: the date goes in the notes, as the team does
      // ("27/9/26 FCH: Wing clip 27/9/26": the day of the note, then the day of the clip).
      if (isClip.value && clip !== null) {
        const clipped = `Wing clip ${noteDate(clip)}`
        const note = `${noteDate(serialFromIso(todayIso())!)} ${initials.value}: ${clipped}`
        const current = String(pending.value(row, 'Notes_Insectary_data') ?? '').trim()
        if (!current.includes(clipped))
          set('Notes_Insectary_data', current && !isBlank(current) ? `${current} | ${note}` : note, true)
      }
      if (tissue.value === WHOLE) {
        // As the team records a whole body (2,349 of 2,386 rows): the other tubes NA / NOT_COLLECTED,
        // T1 holds the medium and Preservation_medium says NOT_COLLECTED.
        const cause = pending.value(row, 'Death_cause')
        if (date !== null) {
          put('Preservation_date', date)
          put('Death_date', date)
        }
        if (isBlank(cause)) put('Death_cause', 'Killed_Preserved')
        put('Preserved_Dead_Alive', isBlank(cause) || cause === 'Killed_Preserved' ? 'Alive' : 'Dead')
        put('Preservation_medium', 'NOT_COLLECTED')
        put('Location_body', 'Ikiam')
        if (autofillNa.value)
          for (let next = slot + 1; next <= 4; next++) {
            set(`Tube_${next}_id`, 'NA')
            put(`Tube_${next}_tissue`, 'NOT_COLLECTED')
            if (next <= 2) put(`T${next}_Preservation_medium`, 'NOT_COLLECTED')
          }
      }
    }
    if (cams.length) camStart.value = nextAfter(cams.at(-1)!)
    if (tubes.length) tubeStart.value = nextAfter(tubes.at(-1)!)
    pending.touch()
    notify(filled ? t('{n} celdas completadas; revisa y guarda', { n: filled }) : t('No había celdas vacías que completar'))
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
  if (!labels.value.length) return notify(t('No hay tubos con ID en la tabla'))
  window.print()
}

/** A serial date as "27/9/26", the date form the team uses in notes. */
function noteDate(serial: number) {
  const d = new Date(Date.UTC(1899, 11, 30) + serial * 86_400_000)
  return `${d.getUTCDate()}/${d.getUTCMonth() + 1}/${String(d.getUTCFullYear()).slice(2)}`
}

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
  return api<Sequence>(`ids?kind=${kind}&start=${encodeURIComponent(start.trim().toUpperCase())}&count=${count}`)
}
function nextAfter(id: string) {
  const m = /^([A-Za-z]+)(\d+)$/.exec(id)
  return m ? `${m[1]}${String(Number(m[2]) + 1).padStart(m[2].length, '0')}` : id
}
</script>

<template>
  <div class="flex h-full flex-col">
    <div class="toolbar">
      <IdPicker v-model="picked" :options="ids" :loading="!ready" :warn="warn" label="Insectary IDs" />
      <div class="flex gap-2">
        <button class="btn-primary" @click="load(false)"><Download :size="15" /> {{ $t('Cargar') }}</button>
        <button class="btn" @click="load(true)"><Plus :size="15" /> {{ $t('Añadir a la tabla') }}</button>
      </div>
    </div>
    <div class="toolbar">
      <label>
        <span class="field-label">{{ $t('CAM ID inicial') }}</span>
        <ChoiceField v-model="camStart" class="field-input w-36" :options="camChoices" autocapitalize="characters" />
      </label>
      <label class="min-w-72">
        <span class="field-label">{{ $t('Rack en uso (siguiente tubo libre)') }}</span>
        <ChoiceField
          class="field-input"
          :model-value="tubeStart"
          :options="rackChoices"
          :freetext="false"
          @update:model-value="pickTube"
        />
      </label>
      <label>
        <span class="field-label">{{ $t('o escribe el tubo') }}</span>
        <input
          :value="tubeStart"
          class="field-input w-36"
          autocapitalize="characters"
          @change="pickTube(($event.target as HTMLInputElement).value)"
        />
      </label>
      <label class="min-w-56">
        <span class="field-label">{{ $t('Tejido por defecto') }}</span>
        <ChoiceField v-model="tissue" class="field-input" :options="tissues" :freetext="false" />
      </label>
      <label>
        <span class="field-label">{{ $t('Medio por defecto') }}</span>
        <ChoiceField v-model="medium" class="field-input" :options="mediums" :freetext="false" />
      </label>
      <template v-if="isClip">
        <label>
          <span class="field-label">{{ $t('Fecha del corte de ala') }}</span>
          <DateField v-model="clipDate" class="field-input" />
          <span v-if="clipError" class="block text-xs text-red-700">{{ clipError }}</span>
          <span v-else-if="clipDate" class="block text-xs text-stone-600">{{ dayLabel(clipDate) }}</span>
          <span v-else class="block text-xs text-amber-800">{{ $t('Obligatoria: va en la nota') }}</span>
        </label>
        <label>
          <span class="field-label">{{ $t('Iniciales (nota)') }}</span>
          <input
            v-model="initialsTyped"
            class="field-input w-20"
            :placeholder="initials"
            autocapitalize="characters"
            maxlength="5"
          />
        </label>
      </template>
      <label v-else>
        <span class="field-label">Preservation_date</span>
        <DateField v-model="presDate" class="field-input" />
        <span v-if="presError" class="block text-xs text-red-700">{{ presError }}</span>
        <span v-else-if="presDate" class="block text-xs text-stone-600">{{ dayLabel(presDate) }}</span>
        <span v-else class="block text-xs text-amber-800">{{ $t('Elige la fecha') }}</span>
      </label>
      <label class="flex max-w-64 items-center gap-2 pb-1 text-xs">
        <input v-model="autofillNa" type="checkbox" />
        {{ $t('Si el tejido es WHOLE_ORGANISM: tubos siguientes NA, tejido y medio NOT_COLLECTED') }}
      </label>
      <button class="btn-primary" :disabled="!rows.length" @click="assign"><Wand2 :size="15" /> {{ $t('Asignar IDs') }}</button>
      <button class="btn" :disabled="!rows.length" @click="printLabels">
        <Printer :size="15" /> {{ $t('Imprimir etiquetas') }}
      </button>
    </div>
    <p v-if="startIssue" class="px-4 py-1 text-sm text-red-800">
      {{ startIssue.text }} {{ $t('No se asignó nada.') }}
      <button v-if="startIssue.next" class="ml-1 underline" @click="useNext">
        {{ $t('Usar el siguiente libre: {next}', { next: startIssue.next }) }}
      </button>
    </p>
    <p class="hint px-4 py-1">
      {{
        $t(
          '"Asignar IDs" da CAM IDs y tubos consecutivos, saltando los ya usados, solo en celdas vacías y en el orden de la tabla.',
        )
      }}
      <template v-if="isClip">{{ $t('Nota que se añade: «{note}».', { note: notePreview }) }}</template>
      <button v-if="loaded.length" class="ml-2 underline" @click="loaded = []">{{ $t('Vaciar tabla') }}</button>
    </p>
    <div class="min-h-0 flex-1">
      <p v-if="!ready" class="p-6 text-stone-500">{{ $t('Cargando {sheet}…', { sheet: 'Insectary_data' }) }}</p>
      <p v-else-if="!rows.length" class="p-6 text-stone-500">{{ $t('Elige IDs arriba y pulsa Cargar.') }}</p>
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
