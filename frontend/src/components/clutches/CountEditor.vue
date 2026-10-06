<script setup lang="ts">
import { computed, nextTick, ref, watch } from 'vue'
import { Camera, Check, Minus, PenLine, Plus, Tag, Undo2, X } from 'lucide-vue-next'
import DateField from '../DateField.vue'
import {
  appendTerm,
  chipEvents,
  countedToday,
  countValue,
  effectLabel,
  formulaOf,
  gainOf,
  hasLosses,
  lossTakesOff,
  LOSSES,
  parseIds,
  readCount,
  rebaseChips,
  termLabels,
  todaySplit,
  toggleStrike,
  totalOf,
  typedTotal,
  type ClutchEvent,
  type CountResult,
  type EventKind,
  type Loss,
  type Stage,
} from '../../lib/clutches'
import type { ClutchPhoto } from '../../lib/clutchPhotos'
import { dayFirst, isoToSerial, serialToIso } from '../../lib/dates'
import { LIFESTAGES, MAIN_STAGES } from '../../lib/emerged'
import type { CellValue } from '../../lib/types'
import { t, tn } from '../../lib/i18n'

/**
 * One count of a clutch kept as the notebook sums it (=3+5-2): its history as
 * chips and the total. Tapping the total and typing the new one is the main
 * way (32 → 30 adds −2 to the sum, as "Counted today"); also +N (more hatched,
 * pupated or emerged), − (died, disappeared or preserved: the cause first, then
 * how many) and "Counted today: N". A chip tapped is struck out of the sum
 * (tapped again, it is back) until the clutch is saved; the chips added today
 * are marked apart from earlier days', and "This morning → now" takes today's
 * changes back at once. A chip with its event (+3 hatched, −1 died) has a
 * camera for its photos. Each step writes the team's formula, never a plain
 * total. What happened is recorded apart, only in the app (`event`): a + as
 * hatched (pupated…), a − as the person says; preserved ones stay in the count
 * when the team keeps them counted (`subtractPreserved` false). Each event
 * happened today unless Yesterday or another day is chosen first, and the note
 * it adds to NOTES (`noteFor`) is shown before it is saved. Larvae (or eggs)
 * preserved can be registered one by one in Insectary_data (`register`: the
 * parent opens Emergidos' cards and answers with the IDs they took).
 */
const props = defineProps<{
  field: string
  /** The count as the person sees it (an unsaved edit, else the sheet's formula). */
  value: CellValue
  /** The sheet's value, to go back to. */
  saved: CellValue
  dirty: boolean
  editable: boolean
  /** A real formula of the sheet in this cell (not a sum): never written. */
  locked: boolean
  /** What a + means for this count ("hatched", "pupated"…). */
  more: string
  /** The count as it was before today's changes (undefined: not changed today). */
  startOfDay?: CellValue
  /** The stage this count follows (none for the dissections: no events). */
  stage?: Stage | null
  /** The team's setting: preserved ones taken off the count (true) or kept in it. */
  subtractPreserved?: boolean
  /** Preserved ones kept counted (lib/clutches.ts preservedApart): shown beside the sum, apart from it. */
  preserved?: number
  /** Today (a date serial): the day events happen unless another is chosen. */
  today?: number
  /** The note an event adds to NOTES ("5/10/26 FCH: 5 larvae died"), '' when NOTES cannot be written. */
  noteFor?: (e: { kind: EventKind; count: number; ids: string[]; day: number; lifestage?: string }) => string
  /** Preserved larvae and eggs can be registered in Insectary_data from here (a clutch in the sheet). */
  canRegister?: boolean
  /** The clutch's events (only in the app), to link each chip to its own. */
  events?: ClutchEvent[]
  /** The clutch's photos, to show which chips have some. */
  photos?: ClutchPhoto[]
  /** Photos can be added (a clutch already in the sheet). */
  canPhoto?: boolean
}>()
export interface CountEvent {
  key: string
  kind: EventKind
  count: number
  ids: string[]
  /** The day it happened (a date serial). */
  day: number
  /** LIFESTAGE of larvae preserved ("3rd instar larva"). */
  lifestage?: string
}
const emit = defineEmits<{
  set: [value: CellValue]
  event: [event: CountEvent]
  unevent: [key: string]
  register: [request: { count: number; lifestage: string; day: number; done: (ids: string[]) => void }]
  /** A chip's camera: its event's photos (or a new one). */
  photo: [event: ClutchEvent]
}>()

const count = computed(() => readCount(props.value))
const total = computed(() => totalOf(count.value.terms))
const typed = ref('')
const n = computed(() => (/^\d{1,4}$/.test(typed.value.trim()) ? Number(typed.value.trim()) : null))
const message = ref('')
const canWork = computed(() => props.editable && !props.locked && !count.value.text)

const reasonText = (reason: 'empty' | 'negative' | 'first' | 'unchanged') =>
  reason === 'empty'
    ? t('Escribe un número')
    : reason === 'negative'
      ? t('El total no puede quedar por debajo de 0')
      : reason === 'first'
        ? t('El primer número no puede ser una pérdida')
        : t('Igual que el total: nada que añadir')
/**
 * Every change made here, to take it back exactly (the earlier formula, not a
 * −N or +N added to it): Undo steps back one change at a time, and takes back
 * the event it recorded (`event`: its key); a step that only recorded an event
 * (preserved larvae kept counted) leaves the count alone (`only`).
 */
interface Step {
  value: CellValue
  event?: string
  only?: boolean
}
const steps = ref<Step[]>([])
function setCount(value: CellValue) {
  steps.value.push({ value: props.value })
  emit('set', value)
}
function undoStep() {
  const step = steps.value.pop()
  if (!step) return
  message.value = ''
  lastNote.value = ''
  asking.value = null
  losing.value = null
  if (step.event) emit('unevent', step.event)
  if (!step.only) emit('set', step.value)
}
/** Takes back every event recorded here (the count goes back as a whole). */
function forgetEvents() {
  lastNote.value = ''
  for (const s of steps.value)
    if (s.event) {
      emit('unevent', s.event)
      s.event = undefined
    }
  asking.value = null
  losing.value = null
}

// --- The chips: struck out of the sum by a tap (and back by another), today's told apart
const chips = ref<{ base: number[]; struck: number[] }>({ base: [...count.value.terms], struck: [] })
watch(
  () => count.value.terms.join(','),
  () => (chips.value = rebaseChips(chips.value.base, chips.value.struck, count.value.terms)),
)
function tapChip(i: number) {
  if (!canWork.value) return
  const r = toggleStrike(chips.value.base, chips.value.struck, i)
  if (!r.ok) {
    message.value = reasonText(r.reason)
    return
  }
  message.value = ''
  chips.value = { base: chips.value.base, struck: r.struck }
  setCount(r.terms.length ? countValue(r.terms) : null)
}
/** This morning's count: as it was before today's first change, else as the sheet has it. */
const morning = computed(() => (props.startOfDay !== undefined ? props.startOfDay : props.saved))
const morningCount = computed(() => readCount(morning.value))
/** A count compared as its sum (=3+5 and "=3 + 5" are one). */
const sumKey = (v: CellValue | undefined) => {
  const c = readCount(v)
  return c.text ?? (c.na ? 'NA' : c.terms.join(','))
}
/** Changed today (saved, or still to save). */
const changedToday = computed(() => sumKey(morning.value) !== sumKey(props.value))
/** The chips before this index are from earlier days (this morning's sum); those after it, today's. */
const firstToday = computed(() => (changedToday.value ? todaySplit(morningCount.value.terms, chips.value.base).kept : chips.value.base.length))
const shown = (c: { na: boolean; terms: number[] }) => (c.na ? 'NA' : c.terms.length ? String(totalOf(c.terms)) : '—')
const chipLabels = computed(() => termLabels(chips.value.base))
// Each chip's event (its photos), for a stage's count.
const chipEvent = computed(() =>
  props.stage && props.events?.length ? chipEvents(chips.value.base, props.events, props.stage, props.subtractPreserved !== false) : [],
)
const eventById = computed(() => new Map((props.events ?? []).map(e => [e.id, e])))
/** The preserved ones beside the sum: how many, and why they are not in it (a tap shows it, for phones). */
const preservedShown = computed(() => props.preserved ?? 0)
const preservedLabel = computed(() =>
  props.stage === 'egg'
    ? tn(preservedShown.value, '{n} preservado', '{n} preservados')
    : tn(preservedShown.value, '{n} preservada', '{n} preservadas'),
)
const preservedWhy = computed(() =>
  t('No se resta de {field}: no está en el cuaderno ni en la suma de la hoja; queda en la app y en NOTES.', { field: props.field }),
)
const explainPreserved = ref(false)
const photosOf = (id: string | null) => (id ? (props.photos ?? []).filter(p => p.eventId === id).length : 0)

// --- What happened, told apart (only in the app): died, disappeared or preserved; hatched…
const lossy = computed(() => hasLosses(props.stage ?? null))
const subtract = computed(() => props.subtractPreserved !== false)
let keys = 0
// --- The day of the next event: today unless Yesterday or another day is chosen (back to today after it).
const todaySerial = computed(() => props.today ?? isoToSerial(new Date().toISOString().slice(0, 10)))
const eventDay = ref(todaySerial.value)
const otherDay = ref(false)
watch(todaySerial, d => (eventDay.value = d))
const eventIso = computed({
  get: () => serialToIso(eventDay.value),
  set: (iso: string) => {
    const s = iso ? isoToSerial(iso) : todaySerial.value
    if (s <= todaySerial.value) eventDay.value = s
  },
})
function pickDay(which: 'today' | 'yesterday' | 'other') {
  otherDay.value = which === 'other'
  if (which !== 'other') eventDay.value = which === 'today' ? todaySerial.value : todaySerial.value - 1
}
/** LIFESTAGE of larvae preserved: the 3rd instar unless another is chosen. */
const lifestage = ref('3rd instar larva')
const moreStages = ref(false)
const lifestages = computed(() => (moreStages.value ? LIFESTAGES.filter(s => s !== 'Egg') : MAIN_STAGES))
/** The note the last event added to NOTES, shown until the next step (it is saved with the clutch). */
const lastNote = ref('')
function record(kind: EventKind, n: number, ids: string[] = []) {
  const key = `${props.field}:${Date.now()}:${++keys}`
  const day = eventDay.value
  const stage = kind === 'preserved' ? (props.stage === 'egg' ? 'Egg' : props.stage === 'larva' ? lifestage.value : undefined) : undefined
  emit('event', { key, kind, count: n, ids, day, ...(stage ? { lifestage: stage } : {}) })
  lastNote.value = props.noteFor?.({ kind, count: n, ids, day, lifestage: stage }) ?? ''
  eventDay.value = todaySerial.value
  otherDay.value = false
  return key
}

// --- − : the cause first (died, disappeared, preserved), then how many
const losing = ref<{ kind: Loss | null } | null>(null)
const lossText = ref('')
const lossN = computed(() => (/^\d{1,4}$/.test(lossText.value.trim()) ? Number(lossText.value.trim()) : null))
const lossBox = ref<HTMLInputElement>()
const idsText = ref('')
function startLoss() {
  if (!lossy.value) {
    // Adults (and the dissections): no cause to tell, the number typed comes off.
    if (n.value === null) return (message.value = reasonText('empty'))
    return apply(appendTerm(count.value.terms, -n.value))
  }
  message.value = ''
  lastNote.value = ''
  asking.value = null
  lossText.value = n.value !== null ? String(n.value) : ''
  typed.value = ''
  idsText.value = ''
  losing.value = { kind: null }
}
async function pickCause(kind: Loss) {
  if (!losing.value) return
  losing.value = { kind }
  if (!lossText.value) lossText.value = '1'
  await nextTick()
  if (kind !== 'preserved') lossBox.value?.select()
}
function stepLoss(by: number) {
  lossText.value = String(Math.max(1, Math.min(9999, (lossN.value ?? 0) + by)))
}
/** The −N a cause and number would add (or why not). */
const lossCheck = computed<CountResult | null>(() => {
  const kind = losing.value?.kind
  if (!kind || lossN.value === null) return null
  return lossTakesOff(kind, subtract.value) ? appendTerm(count.value.terms, -lossN.value) : { ok: true, terms: count.value.terms }
})
/** The note this answer would add, while it is being chosen. */
const lossNote = computed(() => {
  const kind = losing.value?.kind
  if (!kind || lossN.value === null || !props.noteFor) return ''
  return props.noteFor({
    kind,
    count: lossN.value,
    ids: kind === 'preserved' ? parseIds(idsText.value) : [],
    day: eventDay.value,
    lifestage: kind === 'preserved' && props.stage === 'larva' ? lifestage.value : undefined,
  })
})
function confirmLoss() {
  const kind = losing.value?.kind
  const amount = lossN.value
  if (!kind) return
  if (amount === null || amount === 0) return (message.value = reasonText('empty'))
  const ids = kind === 'preserved' ? parseIds(idsText.value) : []
  if (ids.length > amount) return (message.value = t('Más IDs que el número ({n})', { n: amount }))
  message.value = ''
  if (lossTakesOff(kind, subtract.value)) {
    const r = appendTerm(count.value.terms, -amount)
    if (!r.ok) return (message.value = reasonText(r.reason))
    setCount(countValue(r.terms))
    steps.value[steps.value.length - 1].event = record(kind, amount, ids)
  } else steps.value.push({ value: props.value, event: record(kind, amount, ids), only: true })
  losing.value = null
  idsText.value = ''
}
/** Registered in Insectary_data (Emergidos' cards): their IDs come back and the loss is recorded with them. */
function register() {
  const l = losing.value
  const amount = lossN.value
  if (!l || amount === null) return (message.value = reasonText('empty'))
  emit('register', {
    count: amount,
    lifestage: props.stage === 'egg' ? 'Egg' : lifestage.value,
    day: eventDay.value,
    done: ids => {
      if (losing.value !== l) return
      lossText.value = String(Math.max(amount, ids.length))
      idsText.value = ids.join(' ')
      confirmLoss()
    },
  })
}
const lossWord: Record<Loss, () => string> = {
  died: () => t('Murieron'),
  disappeared: () => t('Desaparecieron'),
  preserved: () => t('Se preservaron'),
}

/**
 * After a count changed by typing or Counted: what the difference was (eggs,
 * larvae, pupae); the total already changed, the answer only says why (a
 * recount is no event).
 */
const asking = ref<{ n: number; gain: boolean } | null>(null)
const choosingIds = ref(false)
function askAfter(before: number[], after: number[]) {
  if (!props.stage) return
  const diff = totalOf(after) - totalOf(before)
  lastNote.value = ''
  choosingIds.value = false
  idsText.value = ''
  if (diff < 0 && lossy.value) asking.value = { n: -diff, gain: false }
  else if (diff > 0 && before.length) asking.value = { n: diff, gain: true }
}
/** The note a preserved answer would add, while it is being chosen. */
const preservedNote = computed(() => {
  const a = asking.value
  if (!a || !props.noteFor) return ''
  return props.noteFor({ kind: 'preserved', count: a.n, ids: parseIds(idsText.value), day: eventDay.value, lifestage: props.stage === 'larva' ? lifestage.value : undefined })
})
function choose(kind: EventKind) {
  const a = asking.value
  if (!a) return
  if (kind === 'preserved' && !choosingIds.value) {
    choosingIds.value = true
    return
  }
  const ids = kind === 'preserved' ? parseIds(idsText.value) : []
  if (ids.length > a.n) {
    message.value = t('Más IDs que el número ({n})', { n: a.n })
    return
  }
  message.value = ''
  const takesOff = a.gain || lossTakesOff(kind as Loss, subtract.value)
  const last = steps.value[steps.value.length - 1]
  if (!takesOff && last && !last.event) {
    // Preserved, and the team keeps them counted: the count goes back to what it was.
    emit('set', last.value)
    last.only = true
  }
  if (last && !last.event) last.event = record(kind, a.n, ids)
  asking.value = null
  choosingIds.value = false
}

/** Today's changes to this count, taken back at once: the formula it had this morning. */
function backToMorning() {
  message.value = ''
  forgetEvents()
  setCount(morning.value ?? null)
}
function apply(result: CountResult) {
  if (!result.ok) {
    message.value = reasonText(result.reason)
    return
  }
  message.value = ''
  typed.value = ''
  setCount(countValue(result.terms))
}
// --- The whole formula, typed (=2+3+5-10)
const editingFormula = ref(false)
const formulaText = ref('')
function startFormula() {
  const c = count.value
  formulaText.value = c.na ? 'NA' : c.terms.length ? (formulaOf(c.terms) ?? '') : ''
  message.value = ''
  editingFormula.value = true
}
const formulaResult = computed(() => {
  const raw = formulaText.value.trim()
  if (!raw) return { ok: false as const, why: t('Escribe una suma, p. ej. =2+3') }
  const c = readCount(raw.startsWith('=') || /^(NA|N\/A)$/i.test(raw) || /^\d+$/.test(raw) ? raw : `=${raw}`)
  if (c.na) return { ok: true as const, value: 'NA' as CellValue, label: 'NA' }
  if (c.text !== null || !c.terms.length) return { ok: false as const, why: t('Solo números sumados o restados, p. ej. =2+3+5-10') }
  if (totalOf(c.terms) < 0) return { ok: false as const, why: t('El total no puede quedar por debajo de 0') }
  return { ok: true as const, value: countValue(c.terms), label: `= ${totalOf(c.terms)}` }
})
function applyFormula() {
  const r = formulaResult.value
  if (!r.ok) {
    message.value = r.why
    return
  }
  editingFormula.value = false
  message.value = ''
  setCount(r.value)
}
/** +N: more hatched, pupated or emerged; recorded as such. */
function plus() {
  if (n.value === null) return (message.value = reasonText('empty'))
  const added = n.value
  const before = steps.value.length
  losing.value = null
  apply(appendTerm(count.value.terms, added))
  if (props.stage && steps.value.length > before) steps.value[steps.value.length - 1].event = record(gainOf(props.stage), added)
}
function counted() {
  if (n.value === null) return (message.value = reasonText('empty'))
  const before = count.value.terms
  const r = countedToday(before, n.value)
  losing.value = null
  apply(r)
  if (r.ok) askAfter(before, r.terms)
}
/** What "Counted" would add, shown on its button. */
const countedEffect = computed(() => {
  if (n.value === null) return ''
  const r = countedToday(count.value.terms, n.value)
  if (!r.ok) return r.reason === 'unchanged' ? t('sin cambio') : ''
  const added = r.terms.length > count.value.terms.length ? r.terms[r.terms.length - 1] : null
  return added === null ? `= ${n.value}` : count.value.terms.length ? (added < 0 ? `−${-added}` : `+${added}`) : `= ${added}`
})
// --- The total, tapped and typed over
const typing = ref(false)
const newTotal = ref('')
const totalBox = ref<HTMLInputElement>()
const typedResult = computed(() => typedTotal(count.value.terms, newTotal.value))
/** What the typed total does, shown beside it: "32 → 30 · −2". */
const typedEffect = computed(() => {
  if (!newTotal.value.trim()) return ''
  const r = typedResult.value
  if (!r.ok) return r.reason === 'unchanged' ? t('Igual que el total: nada que añadir') : reasonText(r.reason)
  const label = effectLabel(count.value.terms, r)
  return count.value.terms.length ? `${total.value} → ${totalOf(r.terms)} · ${label}` : label
})
async function startTyping() {
  if (!canWork.value) return
  message.value = ''
  newTotal.value = ''
  losing.value = null
  typing.value = true
  await nextTick()
  totalBox.value?.focus()
}
function stopTyping() {
  cancelling = false
  typing.value = false
  newTotal.value = ''
}
/** Enter or ✓: the difference goes into the sum; the same total just closes the box. */
function applyTotal() {
  if (!typing.value) return
  const r = typedResult.value
  if (!r.ok && r.reason === 'unchanged') return stopTyping()
  if (!r.ok) {
    message.value = reasonText(r.reason)
    return
  }
  const before = count.value.terms
  apply(r)
  stopTyping()
  if (r.ok) askAfter(before, r.terms)
}
/** Leaving the box keeps a valid new total (as a spreadsheet cell does); anything else is dropped. */
/** ✕ pressed: its pointerdown comes before the box's blur, which then must not keep the total. */
let cancelling = false
const willCancel = () => (cancelling = true)
function onTotalBlur() {
  if (!typing.value) return
  if (cancelling) {
    cancelling = false
    return stopTyping()
  }
  if (typedResult.value.ok) applyTotal()
  else stopTyping()
}
</script>

<template>
  <div>
    <div class="flex items-center justify-between gap-2">
      <span class="field-label mb-0 break-all">{{ field }}</span>
      <!-- The total: tap it and type the new one (the difference goes into the sum). -->
      <input
        v-if="typing"
        ref="totalBox"
        v-model="newTotal"
        class="h-12 w-24 shrink-0 rounded-lg border-2 border-brand-600 bg-white px-2 text-right text-2xl font-semibold tabular-nums focus:ring-2 focus:ring-brand-100 focus:outline-none"
        type="text"
        inputmode="numeric"
        pattern="[0-9]*"
        maxlength="4"
        autocomplete="off"
        enterkeyhint="done"
        :placeholder="count.terms.length ? String(total) : ''"
        :aria-label="$t('Total nuevo de {field}', { field })"
        @input="message = ''"
        @keydown.enter.prevent="applyTotal"
        @keydown.esc.prevent.stop="stopTyping"
        @blur="onTotalBlur"
      />
      <button
        v-else-if="canWork"
        type="button"
        class="group -my-1 flex h-12 min-w-20 shrink-0 items-center justify-end gap-1.5 rounded-lg border border-dashed border-stone-300 bg-white px-2 tabular-nums hover:border-brand-600 hover:bg-brand-50 active:bg-brand-50"
        :class="dirty ? 'text-amber-800' : 'text-stone-900'"
        :aria-label="$t('Escribir el total de {field} (ahora {n})', { field, n: count.terms.length ? total : '—' })"
        :title="$t('Toca para escribir el total contado: la diferencia se suma')"
        @click="startTyping"
      >
        <PenLine :size="15" class="text-stone-400 group-hover:text-brand-700" />
        <span class="text-2xl font-semibold">
          <template v-if="count.na">NA</template>
          <template v-else-if="count.terms.length">{{ total }}</template>
          <template v-else>—</template>
        </span>
      </button>
      <span v-else class="shrink-0 text-2xl font-semibold tabular-nums" :class="dirty ? 'text-amber-800' : 'text-stone-900'">
        <template v-if="count.na">NA</template>
        <template v-else-if="count.text">{{ count.text }}</template>
        <template v-else-if="count.terms.length">{{ total }}</template>
        <template v-else>—</template>
      </span>
    </div>
    <!-- Typing a total: what it adds, and ✓ / ✕ (they keep the focus, so the box is not left first). -->
    <div v-if="typing" class="mt-1.5 flex items-center gap-2">
      <p class="min-w-0 flex-1 text-sm tabular-nums" :class="typedResult.ok ? 'font-medium text-brand-800' : 'text-stone-600'" role="status">
        {{ typedEffect || $t('Escribe el total contado hoy') }}
      </p>
      <button type="button" class="btn-primary h-11 shrink-0 px-4" :disabled="!typedResult.ok" @mousedown.prevent @click="applyTotal">
        <Check :size="18" /> {{ $t('Poner') }}
      </button>
      <button type="button" class="btn h-11 w-11 shrink-0 px-0" :aria-label="$t('Cancelar')" @pointerdown="willCancel" @mousedown.prevent @click="stopTyping">
        <X :size="18" />
      </button>
    </div>
    <!-- Today against this morning: what it was, what it is, and one tap to take today's changes back. -->
    <div v-if="changedToday && editable && !locked" class="mt-2 flex items-center gap-2 rounded-lg border border-amber-300 bg-amber-50 px-2.5 py-1.5" role="status">
      <p class="min-w-0 flex-1 text-sm leading-tight tabular-nums">
        <span class="text-stone-600">{{ $t('Esta mañana') }}</span> <strong class="text-base">{{ shown(morningCount) }}</strong>
        <span class="mx-1 text-stone-500">→</span>
        <span class="text-stone-600">{{ $t('ahora') }}</span> <strong class="text-base text-amber-900">{{ shown(count) }}</strong>
      </p>
      <button type="button" class="btn h-10 shrink-0 border-amber-400 bg-white px-2.5 text-sm text-amber-950" @click="backToMorning">
        <Undo2 :size="16" /> {{ $t('Deshacer lo de hoy') }}
      </button>
    </div>
    <!-- The history: each term a chip, earlier days' then today's; a tap strikes it out of the sum (another, back in). -->
    <div v-if="chips.base.length || preservedShown" class="mt-2 flex flex-wrap items-center gap-1.5" :aria-label="$t('Historia de la suma')">
      <template v-for="(label, i) in chipLabels" :key="i">
        <span v-if="i === firstToday && i < chips.base.length" class="text-[11px] font-semibold tracking-wide text-amber-800 uppercase">{{ $t('hoy') }}</span>
        <span
          class="flex min-h-10 items-stretch overflow-hidden rounded-lg text-base font-semibold tabular-nums"
          :class="[
            chips.struck.includes(i)
              ? 'border border-dashed border-stone-400 bg-white text-stone-400'
              : i >= firstToday
                ? 'bg-amber-100 ring-1 ring-amber-300 ' + (chips.base[i] < 0 ? 'text-red-800' : 'text-amber-950')
                : chips.base[i] < 0
                  ? 'bg-red-50 text-red-800'
                  : 'bg-stone-100 text-stone-800',
          ]"
        >
          <button
            type="button"
            class="px-2.5"
            :class="[chips.struck.includes(i) ? 'line-through decoration-2' : '', canWork ? 'active:bg-black/5' : 'cursor-default']"
            :disabled="!canWork"
            :aria-pressed="chips.struck.includes(i)"
            :title="canWork ? (chips.struck.includes(i) ? $t('Tachado: fuera de la suma. Toca para devolverlo') : $t('Toca para quitarlo de la suma')) : undefined"
            @click="tapChip(i)"
          >
            {{ label }}
          </button>
          <!-- Its event's photos: a camera, with how many. -->
          <button
            v-if="chipEvent[i] && (canPhoto || photosOf(chipEvent[i]))"
            type="button"
            class="flex items-center gap-0.5 border-l border-black/10 px-1.5 text-xs font-medium active:bg-black/5"
            :class="photosOf(chipEvent[i]) ? 'text-brand-800' : 'text-stone-400'"
            :aria-label="photosOf(chipEvent[i]) ? $t('Fotos de {term} ({n})', { term: label, n: photosOf(chipEvent[i]) }) : $t('Añadir foto a {term}', { term: label })"
            @click="emit('photo', eventById.get(chipEvent[i]!)!)"
          >
            <Camera :size="14" /><span v-if="photosOf(chipEvent[i])">{{ photosOf(chipEvent[i]) }}</span>
          </button>
        </span>
      </template>
      <span v-if="chips.base.length" class="text-sm text-stone-500 tabular-nums">= {{ total }}</span>
      <!-- Preserved ones the team keeps counted: beside the sum, not in it (only in the app and NOTES). -->
      <button
        v-if="preservedShown"
        type="button"
        class="ml-1 flex min-h-9 items-center gap-1.5 rounded-lg border border-dashed border-violet-400 bg-white px-2 text-sm font-medium text-violet-800 tabular-nums active:bg-violet-50"
        :title="preservedWhy"
        :aria-expanded="explainPreserved"
        @click="explainPreserved = !explainPreserved"
      >
        {{ preservedLabel }}
        <span class="rounded bg-violet-100 px-1 text-[10px] font-semibold tracking-wide text-violet-700 uppercase">{{ $t('solo en la app') }}</span>
      </button>
    </div>
    <p v-if="preservedShown && explainPreserved" class="mt-1 text-xs text-violet-800">{{ preservedWhy }}</p>
    <p v-if="chips.struck.length && canWork" class="mt-1 text-xs text-stone-600">{{ $t('Tachado = fuera de la suma. Tócalo otra vez para devolverlo.') }}</p>
    <p v-else-if="chips.base.length > 1 && canWork && !dirty" class="mt-1 text-xs text-stone-500">{{ $t('Toca un número para quitarlo de la suma.') }}</p>
    <p v-if="locked" class="mt-1 text-xs text-stone-500">{{ $t('Fórmula de la hoja (solo lectura)') }}</p>
    <p v-else-if="count.text && editable" class="mt-1 text-xs text-amber-900">
      {{ $t('No es una suma: corrígelo en la tabla') }}
    </p>
    <!-- The day of the next +N / −N (an egg group laid yesterday, larvae that hatched a day later). -->
    <div v-if="canWork && stage" class="mt-2 flex flex-wrap items-center gap-1.5 text-sm" role="group" :aria-label="$t('Día del evento')">
      <span class="text-xs text-stone-600">{{ $t('Pasó') }}:</span>
      <button
        v-for="d in (['today', 'yesterday', 'other'] as const)"
        :key="d"
        type="button"
        class="h-9 rounded-full border px-3"
        :class="
          (d === 'other' ? otherDay : !otherDay && eventDay === (d === 'today' ? todaySerial : todaySerial - 1))
            ? 'border-brand-700 bg-brand-50 font-medium text-brand-800'
            : 'border-stone-300 bg-white text-stone-700'
        "
        :aria-pressed="d === 'other' ? otherDay : !otherDay && eventDay === (d === 'today' ? todaySerial : todaySerial - 1)"
        @click="pickDay(d)"
      >
        {{ d === 'today' ? $t('Hoy') : d === 'yesterday' ? $t('Ayer') : otherDay ? dayFirst(eventIso) : $t('Otro día') }}
      </button>
      <DateField v-if="otherDay" v-model="eventIso" class="field-input h-9 w-36 text-sm" :aria-label="$t('Día del evento')" />
    </div>
    <div v-if="canWork" class="mt-2 flex gap-2">
      <input
        v-model="typed"
        class="h-12 w-16 shrink-0 rounded-lg border border-stone-300 bg-white text-center text-xl font-semibold tabular-nums focus:border-brand-600 focus:ring-2 focus:ring-brand-100 focus:outline-none"
        type="text"
        inputmode="numeric"
        pattern="[0-9]*"
        maxlength="4"
        autocomplete="off"
        enterkeyhint="done"
        :aria-label="$t('Número para {field}', { field })"
        placeholder="N"
        @input="message = ''"
        @keydown.enter.prevent="counted"
      />
      <button type="button" class="count-btn border-brand-600 text-brand-800" :aria-label="$t('Sumar {n}', { n: typed })" @click="plus">
        <span class="text-lg leading-none font-semibold">+{{ n ?? '' }}</span>
        <span class="text-[11px] leading-tight">{{ more }}</span>
      </button>
      <button
        type="button"
        class="count-btn border-red-300 text-red-800"
        :class="{ 'bg-red-50 ring-2 ring-red-200': losing }"
        :aria-label="lossy ? $t('Restar: murieron, desaparecieron o se preservaron') : $t('Restar {n}', { n: typed })"
        :aria-expanded="lossy ? !!losing : undefined"
        @click="losing ? (losing = null) : startLoss()"
      >
        <span class="text-lg leading-none font-semibold">−{{ n ?? '' }}</span>
        <span class="text-[11px] leading-tight">{{ lossy ? $t('¿qué pasó?') : $t('murieron / faltan') }}</span>
      </button>
      <button type="button" class="count-btn border-stone-400 text-stone-800" @click="counted">
        <span class="text-sm leading-none font-semibold">{{ n === null ? $t('Contados') : $t('Contados: {n}', { n }) }}</span>
        <span class="text-[11px] leading-tight">{{ countedEffect || $t('hoy') }}</span>
      </button>
    </div>
    <!-- −: the cause first, then how many (and, preserved, their stage and IDs). Recorded apart, only in the app. -->
    <div v-if="losing && canWork" class="mt-2 rounded-lg border border-red-200 bg-red-50/40 p-2.5" role="group" :aria-label="$t('Restar')">
      <template v-if="!losing.kind">
        <p class="text-sm font-medium">{{ $t('¿Qué pasó?') }} <span class="text-xs font-normal text-stone-500">{{ $t('primero la causa, luego cuántas') }}</span></p>
        <div class="mt-1.5 grid grid-cols-3 gap-1.5">
          <button
            v-for="k in LOSSES"
            :key="k"
            type="button"
            class="btn h-12 justify-center px-1 text-base"
            :class="k === 'preserved' ? 'border-sky-500 text-sky-900' : 'border-red-300 text-red-800'"
            @click="pickCause(k)"
          >
            {{ lossWord[k]() }}
          </button>
        </div>
        <button type="button" class="mt-1.5 h-10 w-full text-sm text-stone-600" @click="losing = null">{{ $t('Cancelar') }}</button>
      </template>
      <template v-else>
        <div class="flex flex-wrap items-center gap-2">
          <button
            type="button"
            class="flex h-10 items-center gap-1 rounded-full border px-3 text-sm font-semibold"
            :class="losing.kind === 'preserved' ? 'border-sky-500 bg-sky-50 text-sky-900' : 'border-red-300 bg-white text-red-800'"
            :title="$t('Cambiar la causa')"
            @click="losing = { kind: null }"
          >
            {{ lossWord[losing.kind]() }} <PenLine :size="13" class="opacity-60" />
          </button>
          <span class="text-sm text-stone-700">{{ $t('¿Cuántas?') }}</span>
          <span class="ml-auto flex items-stretch overflow-hidden rounded-lg border border-stone-300 bg-white">
            <button type="button" class="grid h-11 w-11 place-items-center active:bg-stone-100" :aria-label="$t('Una menos')" @click="stepLoss(-1)"><Minus :size="18" /></button>
            <input
              ref="lossBox"
              v-model="lossText"
              class="h-11 w-14 border-x border-stone-300 text-center text-xl font-semibold tabular-nums outline-none"
              type="text"
              inputmode="numeric"
              pattern="[0-9]*"
              maxlength="4"
              autocomplete="off"
              enterkeyhint="done"
              :aria-label="$t('Cuántas')"
              @input="message = ''"
              @keydown.enter.prevent="losing.kind === 'preserved' && canRegister && (stage === 'larva' || stage === 'egg') ? register() : confirmLoss()"
            />
            <button type="button" class="grid h-11 w-11 place-items-center active:bg-stone-100" :aria-label="$t('Una más')" @click="stepLoss(1)"><Plus :size="18" /></button>
          </span>
        </div>
        <template v-if="losing.kind === 'preserved'">
          <!-- LIFESTAGE of the larvae preserved: the 3rd instar unless another is chosen. -->
          <div v-if="stage === 'larva'" class="mt-2 flex flex-wrap gap-1" role="group" aria-label="LIFESTAGE">
            <button
              v-for="st in lifestages"
              :key="st"
              type="button"
              class="min-h-10 rounded-lg border px-2 text-sm font-medium"
              :class="lifestage === st ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white text-stone-800'"
              :aria-pressed="lifestage === st"
              @click="lifestage = st"
            >
              {{ st }}
            </button>
            <button type="button" class="min-h-10 rounded-lg border border-dashed border-stone-300 px-2 text-sm text-stone-700" @click="moreStages = !moreStages">
              {{ moreStages ? $t('Menos') : $t('Otro estadio') }}
            </button>
          </div>
          <button
            v-if="canRegister && (stage === 'larva' || stage === 'egg')"
            type="button"
            class="btn-primary mt-2 h-12 w-full flex-col justify-center leading-tight"
            :disabled="lossN === null"
            @click="register"
          >
            <span>{{ $tn(lossN ?? 0, 'Registrar {n} en Insectary_data', 'Registrar {n} en Insectary_data') }}</span>
            <span class="text-xs font-normal opacity-90">{{ $t('Insectary ID, CAM y tubo de cada una (Flash frozen)') }}</span>
          </button>
          <label class="mt-2 block text-xs text-stone-600" :for="`ids-${field}`">{{ canRegister ? $t('O solo contarlas, con sus IDs si los tienen (opcional)') : $t('IDs de Insectary (si los tienen, opcional)') }}</label>
          <div class="mt-1 flex gap-2">
            <input
              :id="`ids-${field}`"
              v-model="idsText"
              class="field-input h-11 min-w-0 flex-1 uppercase"
              type="text"
              autocomplete="off"
              autocapitalize="characters"
              spellcheck="false"
              enterkeyhint="done"
              placeholder="H0E H1E"
              @keydown.enter.prevent="confirmLoss"
            />
            <button type="button" :class="canRegister ? 'btn' : 'btn-primary'" class="h-11 shrink-0 px-4" @click="confirmLoss"><Check :size="18" /> {{ $t('Poner') }}</button>
          </div>
          <p class="mt-1 text-xs text-stone-600">
            {{
              subtract
                ? $t('Se restan de {field}, como dice el ajuste del equipo.', { field })
                : $t('Se quedan en {field}, como dice el ajuste del equipo.', { field })
            }}
          </p>
        </template>
        <p v-if="lossNote" class="mt-2 text-xs break-words text-stone-700">
          {{ $t('Se añade a NOTES:') }} <span class="rounded bg-amber-50 px-1 text-stone-900">{{ lossNote }}</span>
        </p>
        <p v-if="lossCheck && !lossCheck.ok" class="mt-1 text-sm text-red-700">{{ reasonText(lossCheck.reason) }}</p>
        <div v-if="losing.kind !== 'preserved'" class="mt-2 flex gap-2">
          <button type="button" class="btn h-12 flex-1" @click="losing = null">{{ $t('Cancelar') }}</button>
          <button type="button" class="btn-primary h-12 flex-[2] text-base" :disabled="!lossCheck?.ok" @click="confirmLoss">
            <Check :size="18" /> {{ $t('Restar {n}', { n: lossN ?? '' }) }}
          </button>
        </div>
        <button v-else type="button" class="mt-1 h-10 w-full text-sm text-stone-600" @click="losing = null">{{ $t('Cancelar') }}</button>
      </template>
    </div>
    <!-- A new total typed (or Counted): what the difference was. Recorded apart, only in the app (the sheet keeps its sum). -->
    <div v-if="asking && canWork" class="mt-2 rounded-lg border border-stone-300 bg-stone-50 p-2" role="group" :aria-label="$t('¿Qué pasó?')">
      <p class="text-sm font-medium">
        <span class="tabular-nums">{{ asking.gain ? '+' : '−' }}{{ asking.n }}</span> ·
        {{ asking.gain ? $t('¿Qué fue?') : $t('¿Qué pasó?') }}
        <span class="text-xs font-normal text-stone-500">{{ $t('solo en la app') }}</span>
      </p>
      <div v-if="!choosingIds" class="mt-1.5 grid gap-1.5" :class="asking.gain ? 'grid-cols-2' : 'grid-cols-2 min-[420px]:grid-cols-4'">
        <template v-if="asking.gain">
          <button type="button" class="btn h-11 justify-center border-brand-600 text-brand-800" @click="choose(gainOf(stage!))">{{ more }}</button>
        </template>
        <template v-else>
          <button v-for="k in LOSSES" :key="k" type="button" class="btn h-11 justify-center" :class="k === 'preserved' ? 'border-sky-500 text-sky-900' : 'border-red-300 text-red-800'" @click="choose(k)">
            {{ lossWord[k]() }}
          </button>
        </template>
        <button type="button" class="btn h-11 justify-center text-stone-600" @click="asking = null">{{ $t('Solo recuento') }}</button>
      </div>
      <div v-else class="mt-1.5">
        <div v-if="stage === 'larva'" class="mb-2 flex flex-wrap gap-1" role="group" aria-label="LIFESTAGE">
          <button
            v-for="st in lifestages"
            :key="st"
            type="button"
            class="min-h-10 rounded-lg border px-2 text-sm font-medium"
            :class="lifestage === st ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white text-stone-800'"
            :aria-pressed="lifestage === st"
            @click="lifestage = st"
          >
            {{ st }}
          </button>
        </div>
        <label class="block text-xs text-stone-600" :for="`ids-after-${field}`">{{ $t('IDs de Insectary (si los tienen, opcional)') }}</label>
        <div class="mt-1 flex gap-2">
          <input
            :id="`ids-after-${field}`"
            v-model="idsText"
            class="field-input h-11 min-w-0 flex-1 uppercase"
            type="text"
            autocomplete="off"
            autocapitalize="characters"
            spellcheck="false"
            enterkeyhint="done"
            placeholder="H0E H1E"
            @keydown.enter.prevent="choose('preserved')"
          />
          <button type="button" class="btn-primary h-11 shrink-0 px-4" @click="choose('preserved')"><Check :size="18" /> {{ $t('Poner') }}</button>
        </div>
        <p v-if="preservedNote" class="mt-1 text-xs break-words text-stone-700">
          {{ $t('Se añade a NOTES:') }} <span class="rounded bg-amber-50 px-1 text-stone-900">{{ preservedNote }}</span>
        </p>
      </div>
    </div>
    <!-- The whole formula, typed as in the sheet (=2+3+5-10). -->
    <div v-if="editingFormula" class="mt-2 flex flex-wrap items-center gap-2">
      <input
        v-model="formulaText"
        class="field-input h-11 min-w-40 flex-1 font-mono"
        type="text"
        inputmode="text"
        autocomplete="off"
        autocapitalize="off"
        spellcheck="false"
        enterkeyhint="done"
        :aria-label="$t('Fórmula de {field}', { field })"
        @input="message = ''"
        @keydown.enter.prevent="applyFormula"
        @keydown.esc.prevent.stop="editingFormula = false"
      />
      <span class="text-sm tabular-nums" :class="formulaResult.ok ? 'font-medium text-brand-800' : 'text-stone-600'">{{ formulaResult.ok ? formulaResult.label : '' }}</span>
      <button type="button" class="btn-primary h-11 px-4" :disabled="!formulaResult.ok" @click="applyFormula">
        <Check :size="18" /> {{ $t('Poner') }}
      </button>
      <button type="button" class="btn h-11 px-3" :aria-label="$t('Cancelar')" @click="editingFormula = false"><X :size="18" /></button>
    </div>
    <p v-if="message" class="mt-1 text-sm text-red-700" role="alert">{{ message }}</p>
    <p v-if="lastNote && !asking && !losing" class="mt-1.5 flex items-start gap-1.5 rounded-md bg-amber-50 px-2 py-1 text-xs text-stone-800" role="status">
      <Tag :size="13" class="mt-0.5 shrink-0 text-amber-800" />
      <span class="min-w-0 break-words">{{ $t('Añadido a NOTES (se guarda con el clutch):') }} <strong class="font-medium">{{ lastNote }}</strong></span>
    </p>
    <div v-if="canWork" class="mt-1 flex flex-wrap items-center gap-x-4">
      <button v-if="steps.length" type="button" class="flex h-9 items-center gap-1 text-sm font-medium text-brand-800 underline" @click="undoStep">
        <Undo2 :size="14" /> {{ $t('Deshacer el último paso') }}
      </button>
      <button v-if="!editingFormula" type="button" class="flex h-9 items-center gap-1 text-sm text-stone-600 underline" @click="startFormula">
        <PenLine :size="14" /> {{ $t('Editar la fórmula') }}
      </button>
    </div>
    <slot />
  </div>
</template>

<style scoped>
.count-btn {
  display: flex;
  min-height: 3rem;
  min-width: 0;
  flex: 1 1 0;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 0.125rem;
  border-radius: 0.5rem;
  border-width: 1px;
  background: white;
  padding: 0.25rem;
}
.count-btn:active {
  background: var(--color-stone-100);
}
</style>
