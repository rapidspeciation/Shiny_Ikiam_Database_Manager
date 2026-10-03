<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { AlertTriangle, Check, ChevronLeft, ChevronRight, Columns3, Loader2, X } from 'lucide-vue-next'
import ChoiceField from '../ChoiceField.vue'
import CountEditor from './CountEditor.vue'
import ClutchTimeline from './ClutchTimeline.vue'
import DateRow from './DateRow.vue'
import ClutchNotes from './ClutchNotes.vue'
import { useKeyboard } from '../../composables/usePhone'
import { useParents } from '../../composables/useParents'
import type { ClutchDay } from '../../composables/useClutchDay'
import { isBlank } from '../../lib/cells'
import {
  COUNTS,
  MODULE,
  STAGES,
  countCell,
  formulaOf,
  lockedFormula,
  readCount,
  totalOf,
  VERIFY_REASONS,
  type ClutchState,
  type EventKind,
  type Stage,
} from '../../lib/clutches'
import { isoToSerial, todayIso } from '../../lib/dates'
import { errorText, notify } from '../../lib/notice'
import type { CellValue, Field, TableRow } from '../../lib/types'
import { usePending } from '../../stores/pending'
import { t } from '../../lib/i18n'

/**
 * One clutch, to update during the round: each count's sum (tap the total and
 * type the new one; or +N / −N / "Counted today") and its stage date, notes dated and initialled, the parents
 * (F1/F2) in NOTES, species, generation and room. Changes are pending edits
 * saved as any other (automatically, or with «Guardar»); «Revisado» saves and
 * marks the clutch as checked today, with the fields it changed. Full screen on
 * a phone; beside the list (`docked`) on a tablet or computer.
 */
const props = defineProps<{
  rows: TableRow[]
  columns: Field[]
  options: Record<string, string[]>
  species: string[]
  day: ClutchDay
  state: (row: TableRow) => ClutchState
  canEdit: boolean
  initials: string
  /** A person's initials from their name (FCH), for the marks and events. */
  initialsFor?: (name: string) => string
  docked?: boolean
}>()
const index = defineModel<number>('index', { required: true })
const emit = defineEmits<{ close: []; more: [row: TableRow] }>()

const pending = usePending()
const keyboard = useKeyboard()
const parents = useParents()
const row = computed(() => props.rows[Math.min(index.value, props.rows.length - 1)])
const label = computed(() => String(row.value?.values['CLUTCH NUMBER'] ?? ''))
const formulas = computed(() => (row.value ? props.day.sums.value[row.value.id] : undefined))
const field = (key: string): Field | undefined => props.columns.find(c => c.key === key)
const has = (key: string) => !!field(key) && !field(key)!.unavailable
const get = (key: string): CellValue => (row.value ? pending.value(row.value, key) : null)
const dirty = (key: string) => !!row.value && pending.isDirty(row.value.id, key)
const countOf = (key: string) => (row.value ? countCell(row.value, key, pending.value, dirty(key), formulas.value) : null)
const savedOf = (key: string) => formulas.value?.[key] ?? row.value?.values[key] ?? null
/** A field as it was before today's changes (the day's first "before"), or undefined if not changed today. */
function startOfDay(key: string) {
  const change = row.value ? props.day.today(row.value.id).changes.find(c => c.field === key) : undefined
  if (!change) return undefined
  const before = change.before
  return before && typeof before === 'object' ? before.formula : before
}
const editable = (key: string) =>
  props.canEdit && !!row.value && has(key) && !field(key)!.readonly && (!row.value.formulas.includes(key) || !!formulas.value?.[key])
const today = computed(() => isoToSerial(todayIso()))
const status = computed(() => (row.value ? props.day.today(row.value.id) : null))
const clutchState = computed(() => (row.value ? props.state(row.value) : null))

/** What each count's + means (the button's second line). */
const MORE: Record<string, () => string> = {
  'NUMBER OF EGGS': () => t('puestos'),
  'NUMBER OF LARVAE': () => t('eclosionaron'),
  'NUMBER OF PUPA': () => t('pupas nuevas'),
  'NUMBER OF ADULTS': () => t('emergieron'),
  'NUMBER OF PUPAE/LARVAE FOR DISECTIONS': () => t('disecados'),
}
const FIRST: Record<string, () => string> = {
  'HATCHING DATE': () => t('primera eclosión'),
  'PUPA DATE': () => t('primera pupa'),
  'EMERGENCE DATE': () => t('primera emergencia'),
}

// --- What this opening changed (for the check), compared with the clutch as it was opened
const opened = ref<{ id: string; values: Record<string, string> } | null>(null)
const norm = (key: string, v: CellValue) =>
  (COUNTS as readonly string[]).includes(key) ? (formulaOf(readCount(v).terms) ?? String(v ?? '')) : JSON.stringify(v ?? null)
const current = (key: string) => ((COUNTS as readonly string[]).includes(key) ? countOf(key) : get(key))
/** Fields set in this opening, and the value they were last set to (a save may land before the sheet's formulas reload). */
const touched = ref<Record<string, string>>({})
const changedFields = computed(() =>
  Object.entries(touched.value)
    .filter(([key, value]) => opened.value?.values[key] !== value)
    .map(([key]) => key),
)
const rowPending = computed(() => (row.value ? Object.keys(pending.edits[row.value.id]?.values ?? {}).length : 0))

const message = ref('')
function setValue(key: string, value: CellValue) {
  const r = row.value
  if (!r || !editable(key)) return
  message.value = ''
  // Back to what the sheet has: no pending edit (a sum is compared as its formula).
  const back = (COUNTS as readonly string[]).includes(key) && norm(key, value) === norm(key, savedOf(key))
  pending.setCell(MODULE, r, label.value, key, back ? (r.values[key] ?? null) : value)
  // The save checks each cell against what the person saw: for a sum, its formula (=3+5-2),
  // which the server compares exactly (the total a formula shows can lag behind a save).
  const edit = pending.edits[r.id]
  if (edit && key in edit.before && formulas.value?.[key]) {
    edit.before[key] = formulas.value[key]
    pending.persist(false)
  }
  touched.value = { ...touched.value, [key]: norm(key, value) }
}

// --- Parents (F1/F2) and notes: components/clutches/ClutchNotes, the parents written in NOTES for now
/** Parents written on a clutch without a generation: an F1 (the generation buttons change it). */
function parentsWritten() {
  if (has('Generation') && isBlank(get('Generation'))) setValue('Generation', 'F1')
}
const generations = computed(() => [...new Set(['NA', 'F1', 'F2', 'Backcross', ...(props.options.Generation || [])])].filter(g => g.length < 20))

// --- Done: save, then mark as checked
const finishing = ref(false)
const waitIdle = async () => {
  for (let i = 0; i < 300 && pending.saving; i++) await new Promise(r => setTimeout(r, 100))
}
async function finish(state: 'checked' | 'verify' = 'checked', note = '') {
  const r = row.value
  if (!r || finishing.value) return
  finishing.value = true
  message.value = ''
  try {
    const fields = changedFields.value
    let actionId: string | undefined
    if (pending.edits[r.id]) {
      await waitIdle()
      const result = await pending.save('')
      actionId = result.actionId
      const refused = Object.entries(pending.issues).find(([k]) => k.startsWith(`${r.id}:`))
      if (refused) {
        message.value = t('No se guardó {ids}: {reason}', { ids: label.value, reason: refused[1] })
        return
      }
    }
    await props.day.markChecked(r.id, fields, actionId, { state, note })
    touched.value = {}
    verifying.value = false
    verifyNote.value = ''
    opened.value = { id: r.id, values: Object.fromEntries(props.columns.map(c => [c.key, norm(c.key, current(c.key))])) }
    notify(
      state === 'verify'
        ? t('Clutch {clutch} marcado por verificar', { clutch: label.value })
        : fields.length
          ? t('Clutch {clutch} guardado y revisado', { clutch: label.value })
          : t('Clutch {clutch} revisado, sin cambios', { clutch: label.value }),
      'success',
    )
    if (!props.docked) emit('close')
  } catch (e) {
    message.value = errorText(e)
  } finally {
    finishing.value = false
  }
}
/** "Checked, needs verification": with a short reason, so someone else looks again. */
const verifying = ref(false)
const verifyNote = ref('')
const who = (name: string) => (props.initialsFor ? props.initialsFor(name) : name)
const checkedLine = computed(() => {
  const s = status.value
  if (!s?.latest) return ''
  const by = who(s.latest.name || s.latest.username || '')
  if (s.review === 'verify')
    return s.latest.note ? t('Por verificar ({who}): {note}', { who: by, note: s.latest.note }) : t('Por verificar ({who})', { who: by })
  return t('Revisado hoy por {who} · solo en la app', { who: s.checkedBy.map(who).join(', ') })
})

// --- Events (only in the app): what a count's change was, recorded as the person says
/** The event behind each step of the counts (CountEditor's key → the server's id, once saved). */
const posted = new Map<string, Promise<string | null>>()
function recordEvent(stage: Stage, e: { key: string; kind: EventKind; count: number; ids: string[] }) {
  const r = row.value
  if (!r) return
  posted.set(
    e.key,
    props.day
      .addEvent({ recordId: r.id, stage, kind: e.kind, count: e.count, ids: e.ids })
      .then(ev => ev.id)
      .catch(err => {
        message.value = errorText(err)
        return null
      }),
  )
}
async function dropEvent(key: string) {
  const id = await posted.get(key)
  posted.delete(key)
  if (!id) return
  try {
    await props.day.removeEvent(id)
  } catch (err) {
    message.value = errorText(err)
  }
}
/** The sheet's totals as the person sees them, for the timeline's numbers. */
const stageTotals = computed(() => {
  const out: Partial<Record<Stage, number | null>> = {}
  for (const s of STAGES) {
    const c = readCount(countOf(s.count))
    out[s.stage] = c.terms.length ? totalOf(c.terms) : null
  }
  return out
})

function go(step: number) {
  const next = index.value + step
  if (next >= 0 && next < props.rows.length) index.value = next
}

// --- The keyboard: the box typed in stays in view, the buttons above the keyboard
const root = ref<HTMLElement>()
const scroller = ref<HTMLElement>()
const rootBottom = ref(0)
const measure = () => (rootBottom.value = root.value?.getBoundingClientRect().bottom ?? 0)
/** Docked beside the list, the keyboard covers the bottom of the pane: lift the footer that much. */
const lift = computed(() => (props.docked && keyboard.open.value ? Math.max(0, Math.round(rootBottom.value - keyboard.visibleBottom.value)) : 0))
function reveal(e: FocusEvent) {
  const el = e.target as HTMLElement
  if (!el.matches('input, textarea')) return
  measure()
  setTimeout(() => el.scrollIntoView({ block: 'center', behavior: 'smooth' }), 350)
}
/** The box being typed in, back in the middle of what is left once the keyboard has settled. */
let settle: ReturnType<typeof setTimeout> | undefined
watch([keyboard.visibleBottom, keyboard.visibleTop], () => {
  measure()
  clearTimeout(settle)
  settle = setTimeout(() => {
    const el = document.activeElement as HTMLElement | null
    if (el && scroller.value?.contains(el) && el.matches('input, textarea')) el.scrollIntoView({ block: 'center' })
  }, 150)
})
onMounted(measure)
const sizes = typeof ResizeObserver === 'undefined' ? null : new ResizeObserver(measure)
onMounted(() => root.value && sizes?.observe(root.value))
onBeforeUnmount(() => sizes?.disconnect())
/**
 * A phone on its side with the keyboard up leaves a strip of a couple of hundred pixels:
 * the header and the buttons at the bottom step aside for the box being typed in (and the
 * count's own +, − and Counted next to it); they come back when the keyboard closes.
 */
const tight = computed(() => keyboard.open.value && keyboard.visibleBottom.value - keyboard.visibleTop.value < 360)
const overlayStyle = computed(() =>
  props.docked ? undefined : { top: `${keyboard.visibleTop.value}px`, height: `${keyboard.visibleBottom.value - keyboard.visibleTop.value}px` },
)
const stageName: Record<string, () => string> = {
  egg: () => t('Huevos'),
  larva: () => t('Larvas'),
  pupa: () => t('Pupas'),
  adult: () => t('Emergiendo'),
}
// A clutch opened (or the next one): what it was like then, the boxes empty.
watch(
  () => row.value?.id,
  () => {
    const r = row.value
    touched.value = {}
    message.value = ''
    verifying.value = false
    verifyNote.value = ''
    opened.value = r ? { id: r.id, values: Object.fromEntries(props.columns.map(c => [c.key, norm(c.key, current(c.key))])) } : null
    nextTick(() => scroller.value?.scrollTo({ top: 0 }))
  },
  { immediate: true },
)
const endedText = (e: ClutchState['ended']) =>
  e === 'old'
    ? t('más de 90 días')
    : e === 'never'
      ? t('una etapa en NA')
      : e === 'none-left'
        ? t('ya no queda ninguno')
        : e === 'emerged'
          ? t('ya emergieron')
          : e === 'note'
            ? t('las notas lo dan por terminado')
            : ''
</script>

<template>
  <div
    v-if="row"
    ref="root"
    :class="docked ? 'flex h-full min-h-0 flex-col bg-white' : 'fixed inset-x-0 z-40 flex flex-col bg-white'"
    :style="overlayStyle"
    :role="docked ? 'region' : 'dialog'"
    :aria-label="$t('Clutch {clutch}', { clutch: label })"
  >
    <header v-show="!tight" class="flex items-center gap-1 border-b border-stone-200 px-1 py-1">
      <button class="grid h-11 w-11 place-items-center rounded-md text-stone-700 disabled:opacity-30" :disabled="index === 0" :aria-label="$t('Anterior')" @click="go(-1)">
        <ChevronLeft :size="24" />
      </button>
      <div class="min-w-0 flex-1 text-center">
        <p class="truncate text-xl leading-tight font-semibold">
          {{ $t('Clutch {clutch}', { clutch: label }) }}
          <span v-if="!isBlank(get('Generation')) && get('Generation') !== 'NA'" class="ml-1 rounded bg-violet-100 px-1.5 text-sm font-medium text-violet-800">{{ get('Generation') }}</span>
        </p>
        <p class="truncate text-xs text-stone-500">
          {{ isBlank(get('SPECIES')) ? $t('sin especie') : get('SPECIES') }} · {{ $t('fila {row}', { row: row.row }) }}
        </p>
      </div>
      <button
        class="grid h-11 w-11 place-items-center rounded-md text-stone-700 disabled:opacity-30"
        :disabled="index >= rows.length - 1"
        :aria-label="$t('Siguiente')"
        @click="go(1)"
      >
        <ChevronRight :size="24" />
      </button>
      <button v-if="!docked" class="grid h-11 w-11 place-items-center rounded-md text-stone-700" :aria-label="$t('Cerrar')" @click="emit('close')">
        <X :size="22" />
      </button>
    </header>
    <p v-if="message" class="bg-red-50 px-4 py-2 text-sm text-red-800" role="alert">{{ message }}</p>
    <div
      ref="scroller"
      class="min-h-0 flex-1 overflow-y-auto px-4 pb-6"
      :style="lift ? { paddingBottom: `${lift + 24}px` } : undefined"
      @focusin="reveal"
    >
      <div class="mx-auto max-w-2xl">
      <div class="flex flex-wrap items-center gap-1.5 pt-3 text-xs">
        <span v-if="clutchState?.stage" class="rounded-full bg-stone-100 px-2 py-0.5 font-medium text-stone-700">{{ stageName[clutchState.stage]() }}</span>
        <span v-if="clutchState?.ended" class="rounded-full bg-stone-200 px-2 py-0.5 text-stone-700">{{ $t('Terminado: {why}', { why: endedText(clutchState.ended) }) }}</span>
        <span
          v-if="checkedLine"
          class="flex items-center gap-1 rounded-full px-2 py-0.5 font-medium"
          :class="status?.review === 'verify' ? 'bg-orange-100 text-orange-900 ring-1 ring-orange-300' : 'bg-brand-50 text-brand-800'"
        >
          <AlertTriangle v-if="status?.review === 'verify'" :size="12" /><Check v-else :size="12" /> {{ checkedLine }}
        </span>
        <span v-if="status?.changed" class="rounded-full bg-amber-100 px-2 py-0.5 font-medium text-amber-900">{{ $t('Cambiado hoy') }}</span>
        <span class="text-stone-500">{{ get('INSECTARY OR LABORATORY') || '' }}</span>
      </div>
      <p v-if="!canEdit" class="mt-2 rounded bg-stone-100 px-3 py-2 text-sm text-stone-700">{{ $t('Solo lectura') }}</p>

      <!-- Parents and NOTES first: read before counting, and easy to find. -->
      <ClutchNotes
        v-if="has('NOTES')"
        class="border-b border-stone-100"
        :notes="get('NOTES')"
        :saved="row.values.NOTES ?? null"
        :dirty="dirty('NOTES')"
        :editable="editable('NOTES')"
        :initials="initials"
        :today="today"
        :parents="parents"
        :species="isBlank(get('SPECIES')) ? '' : String(get('SPECIES'))"
        :clutch-id="row.id"
        @set="setValue('NOTES', $event)"
        @parents-written="parentsWritten"
      />

      <!-- Each stage: its count kept as a sum, and the date it started. -->
      <section v-for="s in STAGES" :key="s.count" class="border-b border-stone-100 py-3">
        <CountEditor
          :key="`${row.id}:${s.count}`"
          :field="s.count"
          :value="countOf(s.count)"
          :saved="savedOf(s.count)"
          :dirty="dirty(s.count)"
          :editable="editable(s.count)"
          :locked="lockedFormula(row, s.count, formulas)"
          :more="MORE[s.count]()"
          :start-of-day="startOfDay(s.count)"
          :stage="s.stage"
          :subtract-preserved="day.settings.subtractPreserved"
          @set="setValue(s.count, $event)"
          @event="recordEvent(s.stage, $event)"
          @unevent="dropEvent"
        >
          <DateRow
            v-if="s.date && has(s.date)"
            :key="`${row.id}:${s.date}`"
            :field="s.date"
            :value="get(s.date)"
            :dirty="dirty(s.date)"
            :editable="editable(s.date)"
            :first="FIRST[s.date]?.()"
            :suggest="readCount(countOf(s.count)).terms.some(n => n > 0)"
            @set="setValue(s.date, $event)"
          />
        </CountEditor>
      </section>
      <!-- Hatched, died, disappeared, preserved: day by day, only in the app. -->
      <ClutchTimeline :record-id="row.id" :day="day" :totals="stageTotals" :can-edit="canEdit" :initials="who" />
      <section class="border-b border-stone-100 py-3">
        <CountEditor
          :key="`${row.id}:dissections`"
          field="NUMBER OF PUPAE/LARVAE FOR DISECTIONS"
          :value="countOf('NUMBER OF PUPAE/LARVAE FOR DISECTIONS')"
          :saved="savedOf('NUMBER OF PUPAE/LARVAE FOR DISECTIONS')"
          :dirty="dirty('NUMBER OF PUPAE/LARVAE FOR DISECTIONS')"
          :editable="editable('NUMBER OF PUPAE/LARVAE FOR DISECTIONS')"
          :locked="lockedFormula(row, 'NUMBER OF PUPAE/LARVAE FOR DISECTIONS', formulas)"
          :more="MORE['NUMBER OF PUPAE/LARVAE FOR DISECTIONS']()"
          @set="setValue('NUMBER OF PUPAE/LARVAE FOR DISECTIONS', $event)"
        />
      </section>

      <!-- Generation, room, species. -->
      <section v-if="has('Generation')" class="border-b border-stone-100 py-3">
        <span class="field-label">Generation</span>
        <div class="grid grid-cols-4 gap-2">
          <button
            v-for="g in generations"
            :key="g"
            type="button"
            class="min-h-11 rounded-lg border px-1 text-sm font-medium"
            :class="get('Generation') === g ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white text-stone-800'"
            :aria-pressed="get('Generation') === g"
            :disabled="!editable('Generation')"
            @click="setValue('Generation', g)"
          >
            {{ g }}
          </button>
        </div>
      </section>
      <section class="border-b border-stone-100 py-3">
        <span class="field-label">INSECTARY OR LABORATORY</span>
        <div class="grid grid-cols-2 gap-2">
          <button
            v-for="p in ['Insectary', 'Laboratory']"
            :key="p"
            type="button"
            class="min-h-11 rounded-lg border px-1 text-sm font-medium"
            :class="get('INSECTARY OR LABORATORY') === p ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white text-stone-800'"
            :aria-pressed="get('INSECTARY OR LABORATORY') === p"
            :disabled="!editable('INSECTARY OR LABORATORY')"
            @click="setValue('INSECTARY OR LABORATORY', p)"
          >
            {{ p }}
          </button>
        </div>
      </section>
      <section class="border-b border-stone-100 py-3">
        <label class="block">
          <span class="field-label">SPECIES</span>
          <ChoiceField
            v-if="editable('SPECIES')"
            :model-value="isBlank(get('SPECIES')) && get('SPECIES') !== 'NA' ? '' : String(get('SPECIES'))"
            class="field-input h-12 text-base"
            :class="{ 'is-dirty': dirty('SPECIES') }"
            :options="species"
            @update:model-value="setValue('SPECIES', $event || null)"
          />
          <p v-else class="text-sm">{{ get('SPECIES') || '—' }}</p>
        </label>
      </section>
      <p v-if="day.last.value[row.id]" class="pt-3 text-xs text-stone-500">
        {{ $t('Último cambio: {when} · {who}', { when: new Date(day.last.value[row.id].at).toLocaleString(), who: day.last.value[row.id].name || (day.last.value[row.id].actor === 'unknown' ? 'Google Sheets' : day.last.value[row.id].actor) }) }}
      </p>
      <button class="btn mt-3 h-11 w-full" @click="emit('more', row)">
        <Columns3 :size="16" /> {{ $t('Todas las columnas') }}
      </button>
      </div>
    </div>
    <!-- Checked, but someone should look again: why, in a few words. -->
    <div
      v-if="verifying && canEdit"
      v-show="!tight"
      class="relative z-10 border-t border-orange-200 bg-orange-50 px-3 py-2"
      :style="lift ? { transform: `translateY(-${lift}px)` } : undefined"
    >
      <label class="block text-sm font-medium text-orange-950" for="clutch-verify-note">{{ $t('¿Qué hay que verificar? (opcional)') }}</label>
      <div class="mt-1 flex flex-wrap gap-1.5">
        <button
          v-for="r in VERIFY_REASONS"
          :key="r"
          type="button"
          class="min-h-9 rounded-full border border-orange-300 bg-white px-3 text-sm text-orange-950"
          @click="verifyNote = r"
        >
          {{ r }}
        </button>
      </div>
      <div class="mt-1.5 flex gap-2">
        <input
          id="clutch-verify-note"
          v-model="verifyNote"
          class="field-input h-11 min-w-0 flex-1"
          maxlength="200"
          autocomplete="off"
          enterkeyhint="done"
          @keydown.enter.prevent="finish('verify', verifyNote)"
        />
        <button type="button" class="btn h-11 w-11 shrink-0 justify-center px-0" :aria-label="$t('Cancelar')" @click="verifying = false"><X :size="18" /></button>
      </div>
    </div>
    <footer
      v-show="!tight"
      class="relative z-10 flex items-center gap-2 border-t border-stone-200 bg-white px-3 py-2"
      :class="docked ? '' : 'pb-[calc(0.5rem+env(safe-area-inset-bottom))]'"
      :style="lift ? { transform: `translateY(-${lift}px)` } : undefined"
    >
      <span class="min-w-0 flex-1 text-xs text-stone-600">
        <template v-if="pending.saving"><Loader2 :size="12" class="inline animate-spin" /> {{ $t('Guardando…') }}</template>
        <template v-else-if="rowPending">{{ $tn(rowPending, '{n} cambio por guardar', '{n} cambios por guardar') }}</template>
        <template v-else-if="changedFields.length">{{ $t('Guardado en Google Sheets') }}</template>
        <template v-else-if="checkedLine">{{ checkedLine }}</template>
        <template v-else>{{ $t('Sin cambios') }}</template>
      </span>
      <template v-if="canEdit">
        <button
          v-if="verifying"
          class="h-12 shrink-0 rounded-lg border border-orange-400 bg-orange-100 px-3 text-sm font-semibold text-orange-950"
          :disabled="finishing"
          @click="finish('verify', verifyNote)"
        >
          <AlertTriangle :size="16" class="-mt-0.5 inline" /> {{ $t('Marcar por verificar') }}
        </button>
        <template v-else>
          <button
            class="grid h-12 w-12 shrink-0 place-items-center rounded-lg border border-orange-300 text-orange-800 active:bg-orange-50"
            :aria-label="$t('Revisado, pero hay que verificar')"
            :title="$t('Revisado, pero hay que verificar')"
            :disabled="finishing"
            @click="verifying = true"
          >
            <AlertTriangle :size="18" />
          </button>
          <button class="btn-primary h-12 px-4 text-base" :disabled="finishing" @click="finish()">
            <Loader2 v-if="finishing" :size="18" class="animate-spin" /><Check v-else :size="18" />
            {{ changedFields.length || rowPending ? $t('Guardar y revisado') : $t('Revisado, sin cambios') }}
          </button>
        </template>
      </template>
      <button v-else class="btn h-12 px-4" @click="emit('close')">{{ $t('Cerrar') }}</button>
    </footer>
  </div>
</template>
