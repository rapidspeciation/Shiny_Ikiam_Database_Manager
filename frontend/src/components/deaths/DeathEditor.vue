<script setup lang="ts">
import SexBadge from '../SexBadge.vue'
import { computed, ref, watch } from 'vue'
import { ChevronLeft, ChevronRight, Columns3, Loader2, StickyNote, X } from 'lucide-vue-next'
import ChoiceField from '../ChoiceField.vue'
import DateField from '../DateField.vue'
import LifeBadge from './LifeBadge.vue'
import { useKeyboard } from '../../composables/usePhone'
import { displayValue, isBlank, normalizeInput } from '../../lib/cells'
import { appendNote, noteDay, notesOf } from '../../lib/clutches'
import { formatSerial, isoToSerial, serialFromIso, serialToIso, todayIso } from '../../lib/dates'
import { DEATH_NOTE_PHRASES, KILLED, NOTES, factsOf } from '../../lib/deaths'
import { errorText } from '../../lib/notice'
import type { CellValue, Field, TableRow } from '../../lib/types'
import { usePending } from '../../stores/pending'
import { t } from '../../lib/i18n'

/**
 * One butterfly's death, full screen on a phone: big boxes for the death date,
 * cause and notes, and its CAM and first tube when it was preserved; every
 * other column through «Todas las columnas» (the row drawer). Swipe or the
 * arrows go through the list it was opened from. Each change is a pending edit,
 * saved like any other (automatically, or with the save bar's button). The
 * notes as written, one by one; a new one is dated and signed ("1/10/26 FCH:
 * …") and goes after them: opened from the cards it is the card's note, which
 * the cards' Save adds (`cardNote`, changed through `note`); opened from the
 * latest deaths, «Añadir nota» adds it at once as a pending edit.
 */
const MODULE = 'Insectary_data'
const props = defineProps<{
  rows: TableRow[]
  columns: Field[]
  causes: string[]
  options: Record<string, string[]>
  canEdit: boolean
  /** Who signs a new note ("FCH"). */
  initials: string
  /** Opened from the cards: a card's note, added by the cards' Save. */
  cardNote?: (row: TableRow) => string
}>()
const index = defineModel<number>('index', { required: true })
const emit = defineEmits<{ close: []; more: [row: TableRow]; note: [row: TableRow, text: string] }>()

const pending = usePending()
const keyboard = useKeyboard()
const message = ref('')
const row = computed(() => props.rows[Math.min(index.value, props.rows.length - 1)])
const today = computed(() => isoToSerial(todayIso()))
const get = (field: string) => (row.value ? pending.value(row.value, field) : null)
const facts = computed(() => (row.value ? factsOf(get, today.value) : null))
const label = computed(() => String(row.value?.values.Insectary_ID ?? ''))
const field = (key: string): Field => props.columns.find(c => c.key === key) ?? { key, label: key, type: 'text' }
const editable = (key: string) => props.canEdit && !!row.value && !row.value.formulas.includes(key) && !field(key).readonly
const dirty = (key: string) => !!row.value && pending.isDirty(row.value.id, key)
const shown = (key: string) => displayValue(get(key), field(key))

/** The preservation boxes show when the row has any, or the cause says it was preserved. */
const PRESERVATION = ['CAM_ID', 'Tube_1_id', 'Tube_1_tissue', 'T1_Preservation_medium']
const showPreservation = ref(false)
watch(
  row,
  r => {
    message.value = ''
    typed.value = ''
    editAll.value = false
    showPreservation.value = !!r && (PRESERVATION.some(k => !isBlank(pending.value(r, k))) || pending.value(r, 'Death_cause') === KILLED)
  },
  { immediate: true },
)

function setValue(key: string, value: CellValue) {
  if (!row.value || !editable(key)) return
  message.value = ''
  pending.setCell(MODULE, row.value, label.value, key, value)
}
function setText(key: string, text: string) {
  const result = normalizeInput(text, field(key), MODULE)
  if (!result.ok) {
    message.value = result.message
    return
  }
  setValue(key, result.value)
}
const deathIso = computed(() => {
  const v = get('Death_date')
  return typeof v === 'number' ? serialToIso(v) : ''
})
function setDeath(iso: string) {
  if (!iso) return setValue('Death_date', null)
  const serial = serialFromIso(iso)
  if (serial === null) message.value = t('Fecha no válida: el año debe estar entre 1990 y 2099')
  else setValue('Death_date', serial)
}
const yesterday = computed(() => serialToIso(today.value - 1))

// --- Notes: the ones written, then a new one, dated and signed
const notes = computed(() => notesOf(get(NOTES)))
const notePrefix = computed(() => `${noteDay(today.value)} ${props.initials}:`)
/** Opened from the latest deaths: the note being typed, added with «Añadir nota». */
const typed = ref('')
const newNote = computed(() => (props.cardNote && row.value ? props.cardNote(row.value) : typed.value))
function setNote(text: string) {
  if (props.cardNote && row.value) emit('note', row.value, text)
  else typed.value = text
}
function addPhrase(p: string) {
  const text = newNote.value.trim()
  setNote(text ? `${text}; ${p}` : p)
}
function addNote() {
  const text = typed.value.trim()
  if (!text) return
  setValue(NOTES, appendNote(get(NOTES), text, today.value, props.initials))
  typed.value = ''
}
/** The whole cell as text, to correct a note already written. */
const editAll = ref(false)
async function saveNow() {
  try {
    await pending.save('')
  } catch (e) {
    message.value = errorText(e)
  }
}

function go(step: number) {
  const next = index.value + step
  if (next >= 0 && next < props.rows.length) index.value = next
}
// A sideways swipe moves to the next or previous butterfly (not while selecting text in a box).
let start: { x: number; y: number } | null = null
function touchStart(e: TouchEvent) {
  const target = e.target as HTMLElement
  start = target.closest('input, textarea, .choice-list') ? null : { x: e.touches[0].clientX, y: e.touches[0].clientY }
}
function touchEnd(e: TouchEvent) {
  if (!start) return
  const dx = e.changedTouches[0].clientX - start.x
  const dy = e.changedTouches[0].clientY - start.y
  start = null
  if (Math.abs(dx) > 70 && Math.abs(dx) > 2 * Math.abs(dy)) go(dx < 0 ? 1 : -1)
}
/** The box being typed in stays above the keyboard. */
function reveal(e: FocusEvent) {
  const el = e.target as HTMLElement
  if (!el.matches('input, textarea')) return
  setTimeout(() => el.scrollIntoView({ block: 'center', behavior: 'smooth' }), 350)
}
</script>

<template>
  <!-- On a tablet or a PC: a centred sheet over the dimmed page (a tap outside closes it). -->
  <div v-if="row && facts" class="fixed inset-0 z-40 hidden bg-stone-900/40 sm:block" aria-hidden="true" @click="emit('close')" />
  <!-- Sized to what is visible, so the keyboard never hides the header or the buttons at the bottom. -->
  <div
    v-if="row && facts"
    class="fixed inset-x-0 z-40 flex flex-col bg-white sm:inset-x-auto sm:left-1/2 sm:w-[min(40rem,100%)] sm:-translate-x-1/2 sm:shadow-2xl"
    :style="{ top: `${keyboard.visibleTop.value}px`, height: `${keyboard.visibleBottom.value - keyboard.visibleTop.value}px` }"
    role="dialog"
    :aria-label="label"
  >
    <header class="flex items-center gap-1 border-b border-stone-200 px-1 py-1">
      <button
        class="grid h-11 w-11 place-items-center rounded-md text-stone-700 disabled:opacity-30"
        :disabled="index === 0"
        :aria-label="$t('Anterior')"
        @click="go(-1)"
      >
        <ChevronLeft :size="24" />
      </button>
      <div class="min-w-0 flex-1 text-center">
        <p class="text-xl leading-tight font-semibold">{{ label }}</p>
        <p class="text-xs text-stone-500">{{ $t('{i} de {n}', { i: index + 1, n: rows.length }) }} · {{ $t('fila {row}', { row: row.row }) }}</p>
      </div>
      <button
        class="grid h-11 w-11 place-items-center rounded-md text-stone-700 disabled:opacity-30"
        :disabled="index >= rows.length - 1"
        :aria-label="$t('Siguiente')"
        @click="go(1)"
      >
        <ChevronRight :size="24" />
      </button>
      <button class="grid h-11 w-11 place-items-center rounded-md text-stone-700" :aria-label="$t('Cerrar')" @click="emit('close')">
        <X :size="22" />
      </button>
    </header>
    <p v-if="message" class="bg-red-50 px-4 py-2 text-sm text-red-800">{{ message }}</p>
    <div class="min-h-0 flex-1 overflow-y-auto px-4 pb-6" @touchstart.passive="touchStart" @touchend="touchEnd" @focusin="reveal">
      <div class="flex items-start gap-2 py-3">
        <div class="min-w-0 flex-1 text-sm">
          <p class="font-medium">{{ facts.species || '—' }}</p>
          <p class="flex items-center gap-1.5 text-stone-600">
            <SexBadge :sex="facts.sex" />{{ facts.clutch ? $t('clutch {c}', { c: facts.clutch }) : '' }}
          </p>
          <p v-if="facts.entered !== null" class="text-stone-600">
            {{ facts.wild ? $t('Capturada {date}', { date: formatSerial(facts.entered) }) : $t('Emergió {date}', { date: formatSerial(facts.entered) }) }}
          </p>
        </div>
        <LifeBadge :facts="facts" />
      </div>
      <p v-if="!canEdit" class="mb-2 rounded bg-stone-100 px-3 py-2 text-sm text-stone-700">{{ $t('Solo lectura') }}</p>

      <section class="border-t border-stone-100 py-3">
        <span class="field-label">Death_date</span>
        <DateField
          :model-value="deathIso"
          class="field-input h-12 text-base"
          :class="{ 'is-dirty': dirty('Death_date') }"
          :disabled="!editable('Death_date')"
          @update:model-value="setDeath"
        />
        <div v-if="editable('Death_date')" class="mt-2 flex gap-2">
          <button class="btn h-11 flex-1" @click="setDeath(todayIso())">{{ $t('Hoy') }}</button>
          <button class="btn h-11 flex-1" @click="setDeath(yesterday)">{{ $t('Ayer') }}</button>
        </div>
      </section>

      <section class="border-t border-stone-100 py-3">
        <span class="field-label">Death_cause</span>
        <div class="grid grid-cols-2 gap-2">
          <button
            v-for="c in causes"
            :key="c"
            class="min-h-12 rounded-lg border px-2 py-2 text-sm font-medium break-words"
            :class="
              shown('Death_cause') === c
                ? 'border-brand-700 bg-brand-700 text-white'
                : 'border-stone-300 bg-white text-stone-800 active:bg-stone-100'
            "
            :disabled="!editable('Death_cause')"
            :aria-pressed="shown('Death_cause') === c"
            @click="setValue('Death_cause', c)"
          >
            {{ c }}
          </button>
        </div>
        <p v-if="shown('Death_cause') && !causes.includes(shown('Death_cause'))" class="mt-1 text-sm text-stone-600">
          Death_cause: {{ shown('Death_cause') }}
        </p>
      </section>

      <section class="border-t border-stone-100 py-3">
        <span class="field-label">Notes_Insectary_data</span>
        <ul v-if="notes.length" class="space-y-1">
          <li
            v-for="(n, i) in notes"
            :key="i"
            class="rounded-md px-2 py-1 text-sm break-words"
            :class="dirty(NOTES) && i === notes.length - 1 ? 'bg-amber-50' : 'bg-stone-50'"
          >
            {{ n }}
          </li>
        </ul>
        <p v-else class="text-sm text-stone-500">{{ $t('Sin notas') }}</p>
        <template v-if="editable(NOTES)">
          <div class="mt-2 flex flex-wrap gap-1.5">
            <button
              v-for="p in DEATH_NOTE_PHRASES"
              :key="p"
              type="button"
              class="min-h-10 rounded-full border border-stone-300 bg-white px-3 text-sm active:bg-stone-100"
              @click="addPhrase(p)"
            >
              {{ p }}
            </button>
          </div>
          <label class="mt-2 block">
            <span class="sr-only">{{ $t('Nota nueva') }}</span>
            <textarea
              :value="newNote"
              class="field-input min-h-20 text-base"
              rows="2"
              :placeholder="$t('Nota, en inglés (p. ej. Only wings found)')"
              enterkeyhint="done"
              data-note-input
              @input="setNote(($event.target as HTMLTextAreaElement).value)"
            />
          </label>
          <p v-if="cardNote" class="mt-1 flex items-start gap-1 text-xs text-stone-500">
            <StickyNote :size="13" class="mt-px shrink-0" />
            <span>{{ $t('Se añade al guardar la muerte: «{prefix} …»', { prefix: notePrefix }) }}</span>
          </p>
          <div v-else class="mt-1 flex items-center gap-2">
            <span class="min-w-0 flex-1 truncate text-xs text-stone-500">{{ $t('Se añade como «{prefix} …»', { prefix: notePrefix }) }}</span>
            <button type="button" class="btn h-11 px-4" :disabled="!typed.trim()" @click="addNote">{{ $t('Añadir nota') }}</button>
          </div>
          <button v-if="!editAll" type="button" class="mt-1 h-11 text-sm text-stone-600 underline" @click="editAll = true">
            {{ $t('Corregir las notas escritas') }}
          </button>
          <label v-else class="mt-2 block">
            <span class="field-label">{{ $t('Todo el texto de Notes_Insectary_data') }}</span>
            <textarea
              class="field-input min-h-24 text-base"
              :class="{ 'is-dirty': dirty(NOTES) }"
              :value="shown(NOTES)"
              rows="3"
              @change="setText(NOTES, ($event.target as HTMLTextAreaElement).value)"
            />
          </label>
        </template>
      </section>

      <section class="border-t border-stone-100 py-3">
        <button
          v-if="!showPreservation"
          class="btn h-11 w-full"
          @click="showPreservation = true"
        >
          {{ $t('Preservación: CAM y tubo') }}
        </button>
        <div v-else class="space-y-3">
          <label v-for="key in ['CAM_ID', 'Tube_1_id']" :key="key" class="block">
            <span class="field-label">{{ key }}</span>
            <input
              class="field-input h-12 text-base uppercase"
              :class="{ 'is-dirty': dirty(key) }"
              :value="shown(key)"
              :disabled="!editable(key)"
              autocapitalize="characters"
              autocomplete="off"
              spellcheck="false"
              enterkeyhint="done"
              @change="setText(key, ($event.target as HTMLInputElement).value.trim().toUpperCase())"
            />
          </label>
          <label v-for="key in ['Tube_1_tissue', 'T1_Preservation_medium']" :key="key" class="block">
            <span class="field-label">{{ key }}</span>
            <ChoiceField
              v-if="editable(key) && options[key]?.length"
              class="field-input h-12 text-base"
              :class="{ 'is-dirty': dirty(key) }"
              :model-value="shown(key)"
              :options="options[key]"
              @update:model-value="setText(key, $event)"
            />
            <p v-else class="min-h-6 text-sm text-stone-600">{{ shown(key) || '—' }}</p>
          </label>
        </div>
      </section>

      <button class="btn h-11 w-full" @click="emit('more', row)">
        <Columns3 :size="16" /> {{ $t('Todas las columnas') }}
      </button>
    </div>
    <footer class="flex items-center gap-2 border-t border-stone-200 px-3 py-2">
      <span class="min-w-0 flex-1 text-xs text-stone-600">
        <template v-if="pending.saving"><Loader2 :size="12" class="inline animate-spin" /> {{ $t('Guardando…') }}</template>
        <template v-else-if="pending.changeCount">{{
          $tn(pending.changeCount, '{n} cambio por guardar', '{n} cambios por guardar')
        }}</template>
        <template v-else>{{ $t('Todo guardado en Google Sheets') }}</template>
      </span>
      <button v-if="pending.changeCount && !pending.saving" class="btn h-11" @click="saveNow">
        {{ $t('Guardar ya') }}
      </button>
      <button class="btn-primary h-11 px-5" @click="emit('close')">{{ $t('Listo') }}</button>
    </footer>
  </div>
</template>
