<script setup lang="ts">
import { computed, nextTick, ref, watch } from 'vue'
import { Camera, Check, CheckSquare, Minus, NotebookPen, PenLine, Plus, Shuffle, Undo2, X } from 'lucide-vue-next'
import DateField from '../DateField.vue'
import TermsPreview from './TermsPreview.vue'
import {
  LOSSES,
  hasLosses,
  lossTakesOff,
  parseIds,
  readCount,
  todaySplit,
  totalOf,
  type ClutchEvent,
  type EventKind,
  type Loss,
  type Stage,
} from '../../lib/clutches'
import {
  alignTerms,
  formulaOfGroups,
  groupName,
  groupTotal,
  groupsTotal,
  parseCounts,
  parseFormula,
  parseTerms,
  type GroupRow,
  type Groups,
} from '../../lib/clutchGroups'
import type { ClutchPhoto } from '../../lib/clutchPhotos'
import { dayFirst, isoToSerial, serialToIso, todayIso } from '../../lib/dates'
import { LIFESTAGES, MAIN_STAGES } from '../../lib/emerged'
import type { CellValue } from '../../lib/types'
import { locale, t, tn } from '../../lib/i18n'
import { kindWord } from './eventWords'

/**
 * One stage of a clutch (eggs, larvae, pupae, adults; also the dissections,
 * without events): its count as the sheet keeps it, a sum whose terms are
 * dated events (+27 hatched, −2 died), in groups when the stage sits in more
 * than one box (=(6-2)+(5+3): each group in its parentheses). The total after
 * «=» is the only total: tapped, the count typed there becomes a correction,
 * its own term. A term's chip opens its event (date, cause, group, note,
 * photos; delete). A group is selected by a tap (several too): what is done
 * next happens to it. The actions: + (laid, hatched with its day…), − (the
 * cause first, then how many), hatched or pupated from the groups selected,
 * a photo, the day's note; Reagrupar and «Editar la fórmula» (advanced).
 * Everything happens today, except the day of a hatch. The panel only says
 * what the person asked (`act`); the clutch editor works out the formula and
 * the events (lib/clutchGroups.ts) and records them.
 */
const props = defineProps<{
  field: string
  stage: Stage | null
  /** The count as the person sees it (an unsaved edit, else the sheet's formula). */
  value: CellValue
  saved: CellValue
  dirty: boolean
  editable: boolean
  locked: boolean
  /** What a + means here ("hatched"…). */
  more: string
  /** The count as it was before today's changes (undefined: not changed today). */
  startOfDay?: CellValue
  /** The app's groups of this count, in the order of its parentheses (GroupRow per position or null). */
  meta?: (GroupRow | null)[]
  /** The app's groups do not match the formula's parentheses (edited in Sheets, or not saved yet). */
  mismatch?: boolean
  /** What is left of each group that has not moved on (eggs not hatched, larvae not pupated). */
  left?: number[]
  /** The groups of the stage before (to say which eggs the larvae hatched from). */
  sources?: { index: number; name: string; left: number }[]
  /** The groups selected (indexes). */
  selected?: number[]
  events?: ClutchEvent[]
  photos?: ClutchPhoto[]
  subtractPreserved?: boolean
  /** Preserved ones kept counted, shown beside the sum. */
  preserved?: number
  /** The note an event adds to NOTES, '' when NOTES cannot be written. */
  noteFor?: (e: { kind: EventKind; count: number; ids: string[]; lifestage?: string; group?: string | null }) => string
  canRegister?: boolean
  canPhoto?: boolean
  /** Steps of this opening that Undo can take back. */
  undoable?: number
}>()
export type StageAct =
  | { type: 'gain'; count: number; groupIndex: number | null; fromIndex: number | null; day: string; dayKnown: boolean }
  | { type: 'loss'; kind: Loss | 'not_hatched'; count: number; groupIndex: number | null; ids: string[]; lifestage?: string }
  | { type: 'correction'; total: number; groupIndex: number | null; reason: string | null }
  | { type: 'formula'; groups: Groups }
  | { type: 'regroup'; targets: number[]; labels: (string | null)[] }
  | { type: 'split'; index: number; count: number; label: string | null }
  | { type: 'moveOn'; items: { index: number; count: number; notHatched: number }[] }
export interface TermRef {
  group: number
  index: number
  term: number
  event: ClutchEvent | null
}
const emit = defineEmits<{
  act: [act: StageAct]
  'update:selected': [selected: number[]]
  open: [ref: TermRef]
  groupPhotos: [index: number]
  photo: []
  note: []
  undo: []
  backToMorning: []
  register: [request: { count: number; lifestage: string; done: (ids: string[]) => void }]
}>()

const count = computed(() => readCount(props.value))
const groups = computed<Groups>(() => count.value.groups)
const total = computed(() => totalOf(count.value.terms))
const canWork = computed(() => props.editable && !props.locked && !count.value.text)
const meta = computed(() => props.meta ?? [])
/** Shown as groups: more than one, or one the app names (a box of its own). */
const grouped = computed(() => groups.value.length > 1 || meta.value.some(Boolean))
const selected = computed(() => props.selected ?? [])
const message = ref('')
const reasonText = (reason: string) =>
  reason === 'empty' || reason === 'invalid'
    ? t('Escribe un número')
    : reason === 'negative'
      ? t('El total no puede quedar por debajo de 0')
      : reason === 'first'
        ? t('El primer número no puede ser una pérdida')
        : reason === 'sum'
          ? t('Los grupos deben sumar el total')
          : t('Igual que el total: nada que añadir')
defineExpose({ fail: (reason: string) => (message.value = reasonText(reason)), clear: () => (message.value = '') })

// --- The chips: each term with its event
/** An event's term: its own, or (recorded before terms were kept) as the sum would have it. */
function termOf(e: ClutchEvent): number | null {
  if (e.term !== undefined && e.term !== null) return e.field && e.field !== props.field ? null : e.term
  if (e.field !== undefined && e.field !== null) return null
  if (e.stage !== props.stage) return null
  if (e.kind === 'hatched' || e.kind === 'laid' || e.kind === 'pupated' || e.kind === 'emerged') return e.count
  if (e.kind === 'died' || e.kind === 'disappeared' || (e.kind === 'preserved' && props.subtractPreserved)) return -e.count
  return null
}
const termed = computed(() =>
  props.stage
    ? (props.events ?? []).map(e => ({ ...e, term: termOf(e) })).filter(e => e.term !== null && (e.stage === props.stage || e.field === props.field))
    : [],
)
const byId = computed(() => new Map((props.events ?? []).map(e => [e.id, e])))
const chipIds = computed(() => alignTerms(groups.value, termed.value, meta.value.map(m => m?.id ?? null)))
const today = todayIso()
const morning = computed(() => readCount(props.startOfDay !== undefined ? props.startOfDay : props.saved))
const firstToday = computed(() => todaySplit(morning.value.terms, count.value.terms).kept)
const photosOf = (pred: (p: ClutchPhoto) => boolean) => (props.photos ?? []).filter(pred).length
const shortDay = (iso: string) => {
  const d = new Date(`${iso}T12:00:00Z`)
  return Number.isNaN(d.getTime()) ? iso : `${d.getUTCDate()} ${d.toLocaleString(locale.value, { month: 'short', timeZone: 'UTC' }).replace('.', '')}`
}
/** The other group of a transfer, by its name (when it is still a group of this count). */
const otherGroup = (id: string | null | undefined) => {
  const at = id ? meta.value.findIndex(m => m?.id === id) : -1
  return at >= 0 ? nameOf(at) : null
}
const chips = computed(() => {
  let flat = 0
  return groups.value.map((terms, g) =>
    terms.map((term, i) => {
      const id = chipIds.value[g]?.[i] ?? null
      const e = id ? (byId.value.get(id) ?? null) : null
      const sign = term < 0 ? `−${-term}` : `+${term}`
      const what =
        !e || e.kind === 'hatched' || e.kind === 'laid' || e.kind === 'pupated' || e.kind === 'emerged'
          ? ''
          : e.kind === 'transfer'
            ? `${term < 0 ? '→' : '←'} ${otherGroup(e.fromGroupId) ?? (term < 0 ? t('a otro grupo') : t('de otro grupo'))}`
            : kindWord(e.kind)
      const when = e && e.kind !== 'transfer' ? (e.dayKnown === false ? t('fecha NA') : shortDay(e.day)) : ''
      const isToday = e ? e.day === today && e.kind !== 'transfer' : flat >= firstToday.value
      flat++
      return { g, i, term, e, label: [sign, what].filter(Boolean).join(' '), when, photos: e ? photosOf(p => p.eventId === e.id) : 0, isToday }
    }),
  )
})
const groupPhotos = (g: number) => (meta.value[g] ? photosOf(p => p.groupId === meta.value[g]!.id) : 0)
const nameOf = (g: number) => groupName(meta.value[g], g)

// --- Selecting groups
function tapGroup(g: number) {
  if (!grouped.value) return
  emit('update:selected', selected.value.includes(g) ? selected.value.filter(i => i !== g) : [...selected.value, g].sort((a, b) => a - b))
}
const allSelected = computed(() => grouped.value && selected.value.length === groups.value.length)
const selectAll = () => emit('update:selected', allSelected.value ? [] : groups.value.map((_, i) => i))
/** The one group an action goes to: the one selected, else the last. */
const oneGroup = computed(() => (selected.value.length === 1 ? selected.value[0] : null))

// --- The panels: one open at a time
type Panel = 'gain' | 'loss' | 'total' | 'formula' | 'regroup' | 'moveOn' | null
const panel = ref<Panel>(null)
function open(p: Panel) {
  message.value = ''
  panel.value = panel.value === p ? null : p
}
watch(
  () => props.field + String(props.value),
  () => (message.value = ''),
)

// + : how many (a space is a plus), the day of a hatch, the eggs they came from
const gainText = ref('')
const gainTerms = computed(() => parseTerms(gainText.value))
const gainN = computed(() => (gainTerms.value.ok && gainTerms.value.terms.every(n => n > 0) ? totalOf(gainTerms.value.terms) : null))
const hatch = computed(() => props.stage === 'larva')
const gainDay = ref<'today' | 'yesterday' | 'other' | 'unknown'>('today')
const otherIso = ref(serialToIso(isoToSerial(today) - 1))
const gainFrom = ref<number | null>(null)
const gainBox = ref<HTMLInputElement>()
async function startGain() {
  open('gain')
  gainText.value = ''
  gainDay.value = 'today'
  gainFrom.value = props.sources?.length === 1 ? props.sources[0].index : null
  await nextTick()
  gainBox.value?.focus()
}
const gainIso = computed(() =>
  gainDay.value === 'yesterday' ? serialToIso(isoToSerial(today) - 1) : gainDay.value === 'other' && otherIso.value && otherIso.value <= today ? otherIso.value : today,
)
function confirmGain() {
  if (gainN.value === null || gainN.value < 1) return (message.value = reasonText('empty'))
  emit('act', { type: 'gain', count: gainN.value, groupIndex: oneGroup.value, fromIndex: gainFrom.value, day: gainIso.value, dayKnown: gainDay.value !== 'unknown' })
  panel.value = null
}

// − : the cause first, then how many (preserved: their stage and IDs; registered in Insectary_data)
const lossKind = ref<Loss | 'not_hatched' | null>(null)
const lossText = ref('1')
const lossN = computed(() => (/^\d{1,4}$/.test(lossText.value.trim()) ? Number(lossText.value.trim()) : null))
const lossGroup = ref<number | null>(null)
const idsText = ref('')
const lifestage = ref('3rd instar larva')
const moreStages = ref(false)
const lifestages = computed(() => (moreStages.value ? LIFESTAGES.filter(s => s !== 'Egg') : MAIN_STAGES))
const lossBox = ref<HTMLInputElement>()
const causes = computed<(Loss | 'not_hatched')[]>(() => (props.stage === 'egg' ? [...LOSSES, 'not_hatched'] : LOSSES))
function startLoss() {
  open('loss')
  lossKind.value = null
  lossText.value = '1'
  idsText.value = ''
  lossGroup.value = grouped.value ? (oneGroup.value ?? groups.value.length - 1) : null
}
async function pickCause(kind: Loss | 'not_hatched') {
  lossKind.value = kind
  await nextTick()
  if (kind !== 'preserved') lossBox.value?.select()
}
const stepLoss = (by: number) => (lossText.value = String(Math.max(1, Math.min(9999, (lossN.value ?? 0) + by))))
const lossWord = (k: Loss | 'not_hatched') =>
  k === 'died' ? t('Murieron') : k === 'disappeared' ? t('Desaparecieron') : k === 'preserved' ? t('Se preservaron') : t('No eclosionaron')
/** Taken off the count: deaths and disappearances; preserved ones as the team's setting says; eggs that did not hatch never. */
const lossOff = computed(() => (lossKind.value && lossKind.value !== 'not_hatched' ? lossTakesOff(lossKind.value, props.subtractPreserved !== false) : false))
const lossCheck = computed(() => {
  if (!lossKind.value || lossN.value === null) return ''
  if (!lossOff.value) return ''
  const g = lossGroup.value ?? groups.value.length - 1
  const have = groups.value[g] ? groupTotal(groups.value[g]) : total.value
  return have - lossN.value < 0 ? reasonText('negative') : ''
})
const lossNote = computed(() => {
  const k = lossKind.value
  if (!k || lossN.value === null || !props.noteFor) return ''
  return props.noteFor({
    kind: k,
    count: lossN.value,
    ids: k === 'preserved' ? parseIds(idsText.value) : [],
    lifestage: k === 'preserved' && props.stage === 'larva' ? lifestage.value : undefined,
    group: grouped.value && lossGroup.value !== null ? nameOf(lossGroup.value) : null,
  })
})
function confirmLoss() {
  const kind = lossKind.value
  const n = lossN.value
  if (!kind) return
  if (n === null || n < 1) return (message.value = reasonText('empty'))
  const ids = kind === 'preserved' ? parseIds(idsText.value) : []
  if (ids.length > n) return (message.value = t('Más IDs que el número ({n})', { n }))
  if (lossCheck.value) return (message.value = lossCheck.value)
  emit('act', {
    type: 'loss',
    kind,
    count: n,
    groupIndex: lossGroup.value,
    ids,
    ...(kind === 'preserved' && props.stage === 'larva' ? { lifestage: lifestage.value } : kind === 'preserved' && props.stage === 'egg' ? { lifestage: 'Egg' } : {}),
  })
  panel.value = null
}
function register() {
  const n = lossN.value
  if (n === null) return (message.value = reasonText('empty'))
  const asked = panel.value
  emit('register', {
    count: n,
    lifestage: props.stage === 'egg' ? 'Egg' : lifestage.value,
    done: ids => {
      if (panel.value !== asked) return
      lossText.value = String(Math.max(n, ids.length))
      idsText.value = ids.join(' ')
      confirmLoss()
    },
  })
}

// The total, tapped: the count typed (boxes added with spaces: «6 5») becomes a correction
const totalText = ref('')
const totalReason = ref('')
const totalBox = ref<HTMLInputElement>()
const counted = computed(() => {
  const c = parseCounts(totalText.value)
  return c && c.length ? totalOf(c) : null
})
async function startTotal() {
  if (!canWork.value) return
  open('total')
  totalText.value = ''
  totalReason.value = ''
  await nextTick()
  totalBox.value?.focus()
}
const totalEffect = computed(() => {
  if (counted.value === null) return ''
  const diff = counted.value - total.value
  if (!count.value.terms.length) return `= ${counted.value}`
  if (diff === 0) return t('Igual que el total: nada que añadir')
  const where = grouped.value ? ` · (${nameOf(oneGroup.value ?? groups.value.length - 1)})` : ''
  return `${total.value} → ${counted.value} · ${diff > 0 ? '+' : '−'}${Math.abs(diff)} ${t('corrección')}${where}`
})
function confirmTotal() {
  if (counted.value === null) return (message.value = reasonText('empty'))
  if (counted.value === total.value && count.value.terms.length) return (panel.value = null)
  emit('act', { type: 'correction', total: counted.value, groupIndex: oneGroup.value, reason: totalReason.value.trim() || null })
  panel.value = null
}

// The whole formula, by hand (advanced): validated, the total before it is put
const formulaText = ref('')
const formulaResult = computed(() => parseFormula(formulaText.value))
function startFormula() {
  open('formula')
  formulaText.value = count.value.terms.length ? (formulaOfGroups(groups.value) ?? '') : ''
}
function confirmFormula() {
  const r = formulaResult.value
  if (!r.ok) return (message.value = reasonText(r.reason))
  emit('act', { type: 'formula', groups: r.groups })
  panel.value = null
}

// Reagrupar: the groups' new counts («6 5», adding up to the total), or part of the one selected in a group of its own
const regroupText = ref('')
const regroupLabels = ref<string[]>([])
const splitText = ref('')
const regroupCounts = computed(() => parseCounts(regroupText.value) ?? [])
const regroupSum = computed(() => regroupCounts.value.reduce((a, b) => a + b, 0))
function startRegroup() {
  open('regroup')
  regroupText.value = groups.value.map(groupTotal).join(' ')
  regroupLabels.value = groups.value.map((_, i) => nameOf(i))
  splitText.value = ''
}
watch(regroupCounts, c => {
  const labels = [...regroupLabels.value]
  while (labels.length < c.length) {
    const used = new Set(labels.map(l => l.toUpperCase()))
    let letter = 'A'
    for (let i = 0; i < 26 && used.has(letter); i++) letter = String.fromCharCode(66 + i)
    labels.push(letter)
  }
  regroupLabels.value = labels
})
function confirmRegroup() {
  if (regroupSum.value !== total.value) return (message.value = reasonText('sum'))
  emit('act', { type: 'regroup', targets: regroupCounts.value, labels: regroupLabels.value.slice(0, regroupCounts.value.length).map(l => l.trim() || null) })
  panel.value = null
}
function confirmSplit() {
  const n = Number(splitText.value.trim())
  if (oneGroup.value === null || !Number.isInteger(n) || n < 1) return (message.value = reasonText('empty'))
  emit('act', { type: 'split', index: oneGroup.value, count: n, label: null })
  panel.value = null
}

// Hatched (pupated) today from the groups selected: each defaults to all it has left, editable; eggs left can be marked as not hatched
const moveRows = ref<{ index: number; name: string; left: number; text: string; rest: boolean }[]>([])
function startMoveOn() {
  open('moveOn')
  const chosen = grouped.value && selected.value.length ? selected.value : groups.value.map((_, i) => i)
  moveRows.value = chosen.map(i => {
    const left = props.left?.[i] ?? groupTotal(groups.value[i] ?? [])
    return { index: i, name: grouped.value ? nameOf(i) : '', left, text: String(left), rest: false }
  })
}
const moveN = (r: { text: string }) => (/^\d{1,4}$/.test(r.text.trim()) ? Number(r.text.trim()) : null)
const moveTotal = computed(() => moveRows.value.reduce((a, r) => a + (moveN(r) ?? 0), 0))
function confirmMoveOn() {
  if (moveRows.value.some(r => moveN(r) === null)) return (message.value = reasonText('empty'))
  const items = moveRows.value.map(r => ({ index: r.index, count: moveN(r)!, notHatched: r.rest && props.stage === 'egg' ? Math.max(0, r.left - moveN(r)!) : 0 }))
  if (!items.some(x => x.count > 0 || x.notHatched > 0)) return (message.value = reasonText('empty'))
  emit('act', { type: 'moveOn', items })
  panel.value = null
}

const gainButton = computed(() => `+ ${props.more}`)
const moveButton = computed(() => (props.stage === 'egg' ? t('Eclosionaron hoy') : props.stage === 'larva' ? t('Puparon hoy') : ''))
const preservedLabel = computed(() =>
  props.stage === 'egg' ? tn(props.preserved ?? 0, '{n} preservado', '{n} preservados') : tn(props.preserved ?? 0, '{n} preservada', '{n} preservadas'),
)
const preservedWhy = computed(() => t('No se resta de {field}: no está en el cuaderno ni en la suma de la hoja; queda en la app y en NOTES.', { field: props.field }))
const explainPreserved = ref(false)
const changedToday = computed(() => (morning.value.terms.join(',') || (morning.value.na ? 'NA' : '')) !== (count.value.terms.join(',') || (count.value.na ? 'NA' : '')))
const shown = (c: { na: boolean; terms: number[] }) => (c.na ? 'NA' : c.terms.length ? String(totalOf(c.terms)) : '—')
</script>

<template>
  <div>
    <div class="flex items-center gap-2">
      <span class="field-label mb-0 min-w-0 flex-1 break-all">{{ field }}</span>
      <button v-if="canWork && panel !== 'formula'" type="button" class="flex h-9 shrink-0 items-center gap-1 text-xs text-stone-600 underline" @click="startFormula">
        <PenLine :size="13" /> {{ $t('Editar la fórmula') }}
      </button>
    </div>

    <!-- The sum: its terms as chips (each its event), in their groups; «=» and the total, the only one, tapped to correct it. -->
    <div class="mt-1.5 flex flex-wrap items-center gap-1.5" :aria-label="$t('Historia de la suma')">
      <template v-for="(terms, g) in chips" :key="g">
        <div
          v-if="grouped"
          class="flex min-w-0 flex-wrap items-center gap-1 rounded-xl border-2 p-1"
          :class="selected.includes(g) ? 'border-brand-600 bg-brand-50' : 'border-stone-200 bg-white'"
          role="group"
          :aria-label="$t('Grupo {name}', { name: nameOf(g) })"
        >
          <button
            type="button"
            class="flex min-h-10 items-center gap-1 rounded-lg px-2 text-sm font-semibold"
            :class="selected.includes(g) ? 'bg-brand-700 text-white' : 'bg-stone-100 text-stone-800 active:bg-stone-200'"
            :aria-pressed="selected.includes(g)"
            :title="$t('Toca para seleccionar el grupo')"
            @click="tapGroup(g)"
          >
            <CheckSquare v-if="selected.includes(g)" :size="14" />
            <span class="tabular-nums">{{ groupTotal(groups[g]) }}</span> · {{ nameOf(g) }}
            <span v-if="left && left[g] !== undefined && left[g] !== groupTotal(groups[g])" class="text-xs font-normal opacity-80">({{ $t('quedan {n}', { n: left[g] }) }})</span>
          </button>
          <button
            v-if="meta[g] && (groupPhotos(g) || canPhoto)"
            type="button"
            class="flex h-10 items-center gap-0.5 rounded-lg px-1.5 text-xs"
            :class="groupPhotos(g) ? 'text-brand-800' : 'text-stone-400'"
            :aria-label="$t('Fotos del grupo {name}', { name: nameOf(g) })"
            @click="emit('groupPhotos', g)"
          >
            <Camera :size="14" /><span v-if="groupPhotos(g)">{{ groupPhotos(g) }}</span>
          </button>
          <button
            v-for="c in terms"
            :key="c.i"
            type="button"
            class="flex min-h-9 items-center gap-1 rounded-lg px-2 text-sm font-semibold tabular-nums"
            :class="[c.isToday ? 'bg-amber-100 ring-1 ring-amber-300' : c.term < 0 ? 'bg-red-50' : 'bg-stone-50', c.term < 0 ? 'text-red-800' : 'text-stone-800']"
            @click="emit('open', { group: c.g, index: c.i, term: c.term, event: c.e })"
          >
            {{ c.label }}<span v-if="c.when" class="text-xs font-normal opacity-75">· {{ c.when }}</span>
            <span v-if="c.photos" class="flex items-center text-xs text-brand-800"><Camera :size="12" />{{ c.photos }}</span>
          </button>
        </div>
        <template v-else>
          <button
            v-for="c in terms"
            :key="c.i"
            type="button"
            class="flex min-h-10 items-center gap-1 rounded-lg px-2.5 text-base font-semibold tabular-nums"
            :class="[c.isToday ? 'bg-amber-100 ring-1 ring-amber-300' : c.term < 0 ? 'bg-red-50' : 'bg-stone-100', c.term < 0 ? 'text-red-800' : 'text-stone-800']"
            @click="emit('open', { group: c.g, index: c.i, term: c.term, event: c.e })"
          >
            {{ c.label }}<span v-if="c.when" class="text-xs font-normal opacity-75">· {{ c.when }}</span>
            <span v-if="c.photos" class="flex items-center text-xs text-brand-800"><Camera :size="12" />{{ c.photos }}</span>
          </button>
        </template>
      </template>
      <span class="text-lg text-stone-500">=</span>
      <button
        v-if="canWork && panel !== 'total'"
        type="button"
        class="flex h-12 min-w-14 items-center justify-center rounded-lg border border-dashed border-stone-400 bg-white px-2 text-2xl font-semibold tabular-nums hover:border-brand-600 active:bg-brand-50"
        :class="dirty ? 'text-amber-800' : 'text-stone-900'"
        :aria-label="$t('Escribir el total de {field} (ahora {n})', { field, n: count.terms.length ? total : '—' })"
        :title="$t('Toca para escribir el total contado: la diferencia queda como corrección')"
        @click="startTotal"
      >
        {{ count.na ? 'NA' : count.terms.length ? total : '—' }}
      </button>
      <span v-else-if="panel !== 'total'" class="text-2xl font-semibold tabular-nums" :class="dirty ? 'text-amber-800' : 'text-stone-900'">
        {{ count.na ? 'NA' : count.text ? count.text : count.terms.length ? total : '—' }}
      </span>
      <input
        v-else
        ref="totalBox"
        v-model="totalText"
        class="h-12 w-28 rounded-lg border-2 border-brand-600 bg-white px-2 text-right text-2xl font-semibold tabular-nums focus:ring-2 focus:ring-brand-100 focus:outline-none"
        type="text"
        inputmode="numeric"
        autocomplete="off"
        enterkeyhint="done"
        :placeholder="count.terms.length ? String(total) : ''"
        :aria-label="$t('Total nuevo de {field}', { field })"
        @input="message = ''"
        @keydown.enter.prevent="confirmTotal"
        @keydown.esc.prevent.stop="panel = null"
      />
      <button
        v-if="preserved"
        type="button"
        class="ml-1 flex min-h-9 items-center gap-1.5 rounded-lg border border-dashed border-violet-400 bg-white px-2 text-sm font-medium text-violet-800 tabular-nums"
        :title="preservedWhy"
        :aria-expanded="explainPreserved"
        @click="explainPreserved = !explainPreserved"
      >
        {{ preservedLabel }} <span class="rounded bg-violet-100 px-1 text-[10px] font-semibold tracking-wide text-violet-700 uppercase">{{ $t('solo en la app') }}</span>
      </button>
    </div>
    <p v-if="preserved && explainPreserved" class="mt-1 text-xs text-violet-800">{{ preservedWhy }}</p>
    <p v-if="mismatch" class="mt-1 rounded bg-amber-50 px-2 py-1 text-xs text-amber-900">
      {{ $t('Los grupos de la fórmula no coinciden con los de la app (¿editada en Sheets?): los nombres siguen el orden de los paréntesis.') }}
    </p>
    <p v-if="locked" class="mt-1 text-xs text-stone-500">{{ $t('Fórmula de la hoja (solo lectura)') }}</p>
    <p v-else-if="count.text && editable" class="mt-1 text-xs text-amber-900">{{ $t('No es una suma: corrígelo en la tabla') }}</p>

    <!-- The total being typed: what it does, a reason if wanted, ✓ / ✕. -->
    <div v-if="panel === 'total'" class="mt-2 rounded-lg border border-brand-200 bg-brand-50/40 p-2">
      <TermsPreview :text="totalText" counts />
      <p class="text-sm font-medium tabular-nums" :class="counted !== null ? 'text-brand-800' : 'text-stone-600'" role="status">
        {{ totalEffect || $t('Escribe el total contado hoy (un espacio suma: 6 5 = 11)') }}
      </p>
      <input v-model="totalReason" class="field-input mt-1.5 h-11 text-base" maxlength="200" autocomplete="off" enterkeyhint="done" :placeholder="$t('Motivo (opcional, en inglés)')" @keydown.enter.prevent="confirmTotal" />
      <div class="mt-1.5 flex gap-2">
        <button type="button" class="btn h-11 flex-1" @click="panel = null"><X :size="16" /> {{ $t('Cancelar') }}</button>
        <button type="button" class="btn-primary h-11 flex-[2]" :disabled="counted === null" @click="confirmTotal"><Check :size="18" /> {{ $t('Poner') }}</button>
      </div>
    </div>

    <!-- Today against this morning. -->
    <div v-if="changedToday && editable && !locked" class="mt-2 flex items-center gap-2 rounded-lg border border-amber-300 bg-amber-50 px-2.5 py-1.5" role="status">
      <p class="min-w-0 flex-1 text-sm leading-tight tabular-nums">
        <span class="text-stone-600">{{ $t('Esta mañana') }}</span> <strong class="text-base">{{ shown(morning) }}</strong>
        <span class="mx-1 text-stone-500">→</span>
        <span class="text-stone-600">{{ $t('ahora') }}</span> <strong class="text-base text-amber-900">{{ shown(count) }}</strong>
      </p>
      <button type="button" class="btn h-10 shrink-0 border-amber-400 bg-white px-2.5 text-sm text-amber-950" @click="emit('backToMorning')">
        <Undo2 :size="16" /> {{ $t('Deshacer lo de hoy') }}
      </button>
    </div>

    <slot />

    <!-- Groups: all at once, or regrouped. -->
    <div v-if="canWork && stage && stage !== 'adult' && total > 0" class="mt-2 flex flex-wrap items-center gap-2 text-sm">
      <button v-if="grouped" type="button" class="flex h-9 items-center gap-1 rounded-full border border-stone-300 bg-white px-3 text-stone-700" :aria-pressed="allSelected" @click="selectAll">
        <CheckSquare :size="14" /> {{ allSelected ? $t('Ninguno') : $t('Seleccionar todos') }}
      </button>
      <button type="button" class="flex h-9 items-center gap-1 rounded-full border border-stone-300 bg-white px-3 text-stone-700" :aria-expanded="panel === 'regroup'" @click="startRegroup">
        <Shuffle :size="14" /> {{ $t('Reagrupar') }}
      </button>
      <span v-if="grouped && selected.length" class="text-xs text-stone-600">{{ tn(selected.length, '{n} grupo seleccionado', '{n} grupos seleccionados') }}</span>
    </div>

    <!-- What happened: + , −, from the groups selected, a photo, the day's note. -->
    <div v-if="canWork" class="mt-2 grid grid-cols-2 gap-2 min-[480px]:grid-cols-3">
      <button type="button" class="act-btn border-brand-600 text-brand-800" :class="{ 'ring-2 ring-brand-200': panel === 'gain' }" @click="panel === 'gain' ? (panel = null) : startGain()">
        <Plus :size="18" /> {{ more }}
      </button>
      <button v-if="hasLosses(stage)" type="button" class="act-btn border-red-300 text-red-800" :class="{ 'bg-red-50 ring-2 ring-red-200': panel === 'loss' }" @click="panel === 'loss' ? (panel = null) : startLoss()">
        <Minus :size="18" /> {{ $t('¿qué pasó?') }}
      </button>
      <button v-if="moveButton && total > 0" type="button" class="act-btn border-sky-500 text-sky-900" :class="{ 'ring-2 ring-sky-200': panel === 'moveOn' }" @click="panel === 'moveOn' ? (panel = null) : startMoveOn()">
        {{ moveButton }}<span v-if="grouped && selected.length" class="text-xs font-normal">({{ selected.map(nameOf).join(', ') }})</span>
      </button>
      <button v-if="stage && canPhoto" type="button" class="act-btn border-stone-300 text-stone-800" @click="emit('photo')">
        <Camera :size="18" /> {{ $t('Foto') }}<span v-if="grouped && oneGroup !== null" class="text-xs font-normal">({{ nameOf(oneGroup) }})</span>
      </button>
      <button v-if="stage" type="button" class="act-btn border-stone-300 text-stone-800" @click="emit('note')"><NotebookPen :size="18" /> {{ $t('Nota de hoy') }}</button>
    </div>

    <!-- +: how many, and (larvae) the day they hatched and the eggs they came from. -->
    <div v-if="panel === 'gain' && canWork" class="mt-2 rounded-lg border border-brand-200 bg-brand-50/40 p-2.5" role="group" :aria-label="gainButton">
      <div class="flex items-center gap-2">
        <span class="text-sm font-medium">{{ gainButton }}</span>
        <input
          ref="gainBox"
          v-model="gainText"
          class="h-12 w-28 rounded-lg border border-stone-300 bg-white text-center text-xl font-semibold tabular-nums focus:border-brand-600 focus:ring-2 focus:ring-brand-100 focus:outline-none"
          type="text"
          inputmode="numeric"
          autocomplete="off"
          enterkeyhint="done"
          placeholder="N"
          :aria-label="$t('Cuántos')"
          @input="message = ''"
          @keydown.enter.prevent="confirmGain"
        />
        <TermsPreview :text="gainText" counts class="min-w-0 flex-1" />
      </div>
      <div v-if="hatch" class="mt-2 flex flex-wrap items-center gap-1.5 text-sm" role="radiogroup" :aria-label="$t('Día de la eclosión')">
        <button
          v-for="d in (['today', 'yesterday', 'other', 'unknown'] as const)"
          :key="d"
          type="button"
          role="radio"
          class="h-9 rounded-full border px-3"
          :class="gainDay === d ? 'border-brand-700 bg-brand-50 font-medium text-brand-800' : 'border-stone-300 bg-white text-stone-700'"
          :aria-checked="gainDay === d"
          @click="gainDay = d"
        >
          {{ d === 'today' ? $t('Hoy') : d === 'yesterday' ? $t('Ayer') : d === 'other' ? (gainDay === 'other' ? dayFirst(otherIso) : $t('Otro día')) : $t('Desconocida (ya grandes)') }}
        </button>
        <DateField v-if="gainDay === 'other'" v-model="otherIso" class="field-input h-9 w-36 text-sm" :aria-label="$t('Día de la eclosión')" />
      </div>
      <div v-if="sources && sources.length > 1" class="mt-2 flex flex-wrap items-center gap-1.5 text-sm" role="radiogroup">
        <span class="text-xs text-stone-600">{{ stage === 'larva' ? $t('De los huevos:') : $t('De las larvas:') }}</span>
        <button
          v-for="s in sources"
          :key="s.index"
          type="button"
          role="radio"
          class="h-9 rounded-full border px-3"
          :class="gainFrom === s.index ? 'border-brand-700 bg-brand-50 font-medium text-brand-800' : 'border-stone-300 bg-white text-stone-700'"
          :aria-checked="gainFrom === s.index"
          @click="gainFrom = gainFrom === s.index ? null : s.index"
        >
          {{ s.name }} <span class="text-xs text-stone-500">({{ $t('quedan {n}', { n: s.left }) }})</span>
        </button>
      </div>
      <p v-if="grouped && oneGroup !== null" class="mt-1 text-xs text-stone-600">{{ $t('Se suma en el grupo {name}', { name: nameOf(oneGroup) }) }}</p>
      <div class="mt-2 flex gap-2">
        <button type="button" class="btn h-11 flex-1" @click="panel = null">{{ $t('Cancelar') }}</button>
        <button type="button" class="btn-primary h-11 flex-[2]" :disabled="gainN === null" @click="confirmGain"><Check :size="18" /> {{ gainN !== null ? `+${gainN}` : $t('Poner') }}</button>
      </div>
    </div>

    <!-- −: the cause first, then how many. -->
    <div v-if="panel === 'loss' && canWork" class="mt-2 rounded-lg border border-red-200 bg-red-50/40 p-2.5" role="group" :aria-label="$t('Restar')">
      <template v-if="!lossKind">
        <p class="text-sm font-medium">{{ $t('¿Qué pasó?') }} <span class="text-xs font-normal text-stone-500">{{ $t('primero la causa, luego cuántas') }}</span></p>
        <div class="mt-1.5 grid grid-cols-2 gap-1.5 min-[420px]:grid-cols-4">
          <button v-for="k in causes" :key="k" type="button" class="btn h-12 justify-center px-1 text-base" :class="k === 'preserved' ? 'border-sky-500 text-sky-900' : 'border-red-300 text-red-800'" @click="pickCause(k)">
            {{ lossWord(k) }}
          </button>
        </div>
        <button type="button" class="mt-1.5 h-10 w-full text-sm text-stone-600" @click="panel = null">{{ $t('Cancelar') }}</button>
      </template>
      <template v-else>
        <div class="flex flex-wrap items-center gap-2">
          <button type="button" class="flex h-10 items-center gap-1 rounded-full border border-red-300 bg-white px-3 text-sm font-semibold text-red-800" :title="$t('Cambiar la causa')" @click="lossKind = null">
            {{ lossWord(lossKind) }} <PenLine :size="13" class="opacity-60" />
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
              maxlength="4"
              autocomplete="off"
              enterkeyhint="done"
              :aria-label="$t('Cuántas')"
              @input="message = ''"
              @keydown.enter.prevent="confirmLoss"
            />
            <button type="button" class="grid h-11 w-11 place-items-center active:bg-stone-100" :aria-label="$t('Una más')" @click="stepLoss(1)"><Plus :size="18" /></button>
          </span>
        </div>
        <div v-if="grouped" class="mt-2 flex flex-wrap items-center gap-1.5 text-sm" role="radiogroup">
          <span class="text-xs text-stone-600">{{ $t('En el grupo:') }}</span>
          <button
            v-for="(_, g) in groups"
            :key="g"
            type="button"
            role="radio"
            class="h-9 rounded-full border px-3"
            :class="lossGroup === g ? 'border-brand-700 bg-brand-50 font-medium text-brand-800' : 'border-stone-300 bg-white text-stone-700'"
            :aria-checked="lossGroup === g"
            @click="lossGroup = g"
          >
            {{ nameOf(g) }} <span class="text-xs text-stone-500 tabular-nums">({{ groupTotal(groups[g]) }})</span>
          </button>
        </div>
        <template v-if="lossKind === 'preserved'">
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
          <button v-if="canRegister && (stage === 'larva' || stage === 'egg')" type="button" class="btn-primary mt-2 h-12 w-full flex-col justify-center leading-tight" :disabled="lossN === null" @click="register">
            <span>{{ $tn(lossN ?? 0, 'Registrar {n} en Insectary_data', 'Registrar {n} en Insectary_data') }}</span>
            <span class="text-xs font-normal opacity-90">{{ $t('Insectary ID, CAM y tubo de cada una (Flash frozen)') }}</span>
          </button>
          <label class="mt-2 block text-xs text-stone-600" :for="`ids-${field}`">{{ canRegister ? $t('O solo contarlas, con sus IDs si los tienen (opcional)') : $t('IDs de Insectary (si los tienen, opcional)') }}</label>
          <input
            :id="`ids-${field}`"
            v-model="idsText"
            class="field-input mt-1 h-11 uppercase"
            type="text"
            autocomplete="off"
            autocapitalize="characters"
            spellcheck="false"
            enterkeyhint="done"
            placeholder="H0E H1E"
            @keydown.enter.prevent="confirmLoss"
          />
          <p class="mt-1 text-xs text-stone-600">
            {{ subtractPreserved ? $t('Se restan de {field}, como dice el ajuste del equipo.', { field }) : $t('Se quedan en {field}, como dice el ajuste del equipo.', { field }) }}
          </p>
        </template>
        <p v-else-if="lossKind === 'not_hatched'" class="mt-1 text-xs text-stone-600">{{ $t('Los huevos puestos siguen contados; solo dejan de esperarse.') }}</p>
        <p v-if="lossNote" class="mt-2 text-xs break-words text-stone-700">
          {{ $t('Se añade a NOTES:') }} <span class="rounded bg-amber-50 px-1 text-stone-900">{{ lossNote }}</span>
        </p>
        <p v-if="lossCheck" class="mt-1 text-sm text-red-700">{{ lossCheck }}</p>
        <div class="mt-2 flex gap-2">
          <button type="button" class="btn h-12 flex-1" @click="panel = null">{{ $t('Cancelar') }}</button>
          <button type="button" class="btn-primary h-12 flex-[2] text-base" :disabled="lossN === null || !!lossCheck" @click="confirmLoss">
            <Check :size="18" /> {{ lossOff ? $t('Restar {n}', { n: lossN ?? '' }) : $t('Poner') }}
          </button>
        </div>
      </template>
    </div>

    <!-- Hatched (pupated) from the groups selected: each its count, editable; eggs left as not hatched. -->
    <div v-if="panel === 'moveOn' && canWork" class="mt-2 rounded-lg border border-sky-200 bg-sky-50/50 p-2.5" role="group" :aria-label="moveButton">
      <p class="text-sm font-medium">{{ moveButton }}</p>
      <ul class="mt-1.5 space-y-1.5">
        <li v-for="r in moveRows" :key="r.index" class="flex flex-wrap items-center gap-2">
          <span class="min-w-16 text-sm font-semibold">{{ r.name || $t('Todos') }}</span>
          <input
            v-model="r.text"
            class="h-11 w-20 rounded-lg border border-stone-300 bg-white text-center text-lg font-semibold tabular-nums"
            type="text"
            inputmode="numeric"
            maxlength="4"
            autocomplete="off"
            :aria-label="$t('Cuántos de {name}', { name: r.name || $t('Todos') })"
          />
          <span class="text-xs text-stone-600 tabular-nums">{{ $t('de {n}', { n: r.left }) }}</span>
          <label v-if="stage === 'egg' && moveN(r) !== null && moveN(r)! < r.left" class="flex items-center gap-1 text-xs text-stone-700">
            <input v-model="r.rest" type="checkbox" class="size-5" /> {{ $t('Los otros {n} no eclosionaron', { n: r.left - moveN(r)! }) }}
          </label>
        </li>
      </ul>
      <div class="mt-2 flex gap-2">
        <button type="button" class="btn h-11 flex-1" @click="panel = null">{{ $t('Cancelar') }}</button>
        <button type="button" class="btn-primary h-11 flex-[2]" @click="confirmMoveOn"><Check :size="18" /> {{ moveButton }} · {{ moveTotal }}</button>
      </div>
    </div>

    <!-- Regroup: the groups' counts, adding up to the total; or part of the one selected in a group of its own. -->
    <div v-if="panel === 'regroup' && canWork" class="mt-2 rounded-lg border border-stone-300 bg-stone-50 p-2.5" role="group" :aria-label="$t('Reagrupar')">
      <label class="block text-sm font-medium" :for="`regroup-${field}`">{{ $t('¿Cuántos en cada grupo? (un espacio separa: 6 5)') }}</label>
      <input
        :id="`regroup-${field}`"
        v-model="regroupText"
        class="field-input mt-1 h-11 text-lg tabular-nums"
        type="text"
        inputmode="numeric"
        autocomplete="off"
        enterkeyhint="done"
        @input="message = ''"
        @keydown.enter.prevent="confirmRegroup"
      />
      <div v-if="regroupCounts.length" class="mt-1.5 flex flex-wrap items-center gap-1.5">
        <span v-for="(n, i) in regroupCounts" :key="i" class="flex items-center gap-1 rounded-lg bg-white px-1.5 py-1 ring-1 ring-stone-200">
          <span class="font-semibold tabular-nums">{{ n }}</span> ·
          <input v-model="regroupLabels[i]" class="h-8 w-20 rounded border border-stone-300 px-1 text-sm" maxlength="40" :aria-label="$t('Nombre del grupo {n}', { n: i + 1 })" />
        </span>
        <span class="text-sm tabular-nums" :class="regroupSum === total ? 'text-brand-800' : 'text-red-700'">= {{ regroupSum }} / {{ total }}</span>
      </div>
      <p class="mt-1 text-xs text-stone-600">{{ $t('Se escribe como traspasos en la fórmula: el total y la historia quedan.') }}</p>
      <div class="mt-2 flex gap-2">
        <button type="button" class="btn h-11 flex-1" @click="panel = null">{{ $t('Cancelar') }}</button>
        <button type="button" class="btn-primary h-11 flex-[2]" :disabled="regroupSum !== total" @click="confirmRegroup"><Check :size="18" /> {{ $t('Reagrupar') }}</button>
      </div>
      <div v-if="grouped && oneGroup !== null" class="mt-3 border-t border-stone-200 pt-2">
        <label class="block text-sm font-medium" :for="`split-${field}`">{{ $t('Separar del grupo {name} en uno nuevo:', { name: nameOf(oneGroup) }) }}</label>
        <div class="mt-1 flex gap-2">
          <input :id="`split-${field}`" v-model="splitText" class="field-input h-11 w-24 text-lg tabular-nums" type="text" inputmode="numeric" maxlength="4" autocomplete="off" @keydown.enter.prevent="confirmSplit" />
          <button type="button" class="btn h-11 flex-1" :disabled="!splitText.trim()" @click="confirmSplit">{{ $t('Separar') }}</button>
        </div>
      </div>
    </div>

    <!-- The whole formula, by hand (advanced). -->
    <div v-if="panel === 'formula' && canWork" class="mt-2 rounded-lg border border-stone-300 bg-white p-2.5">
      <label class="block text-xs text-stone-600" :for="`formula-${field}`">{{ $t('La fórmula (un espacio suma; «-» resta; paréntesis por grupo)') }}</label>
      <input
        :id="`formula-${field}`"
        v-model="formulaText"
        class="field-input mt-1 h-11 font-mono"
        type="text"
        inputmode="text"
        autocomplete="off"
        autocapitalize="off"
        spellcheck="false"
        enterkeyhint="done"
        :aria-label="$t('Fórmula de {field}', { field })"
        @input="message = ''"
        @keydown.enter.prevent="confirmFormula"
        @keydown.esc.prevent.stop="panel = null"
      />
      <div v-if="formulaResult.ok" class="mt-1.5 flex flex-wrap items-center gap-1.5 text-sm tabular-nums">
        <span v-for="(g, i) in formulaResult.groups" :key="i" class="flex flex-wrap items-center gap-1" :class="formulaResult.groups.length > 1 ? 'rounded-lg px-1 ring-1 ring-stone-300' : ''">
          <span v-for="(n, j) in g" :key="j" class="rounded px-1.5 py-0.5" :class="n < 0 ? 'bg-red-50 text-red-800' : 'bg-stone-100'">{{ n < 0 ? `−${-n}` : j || i ? `+${n}` : n }}</span>
        </span>
        <span class="font-semibold text-brand-800">= {{ groupsTotal(formulaResult.groups) }}</span>
        <span class="font-mono text-xs text-stone-500">{{ formulaOfGroups(formulaResult.groups) }}</span>
      </div>
      <p v-else-if="formulaText.trim()" class="mt-1 text-sm text-stone-600">{{ reasonText(formulaResult.reason) }}</p>
      <p class="mt-1 text-xs text-stone-500">{{ $t('Solo para corregir: queda en la historia. Los eventos de los números que quites quedan fuera de la suma.') }}</p>
      <div class="mt-2 flex gap-2">
        <button type="button" class="btn h-11 flex-1" @click="panel = null">{{ $t('Cancelar') }}</button>
        <button type="button" class="btn-primary h-11 flex-[2]" :disabled="!formulaResult.ok" @click="confirmFormula"><Check :size="18" /> {{ $t('Poner') }}</button>
      </div>
    </div>

    <p v-if="message" class="mt-1 text-sm text-red-700" role="alert">{{ message }}</p>
    <slot name="after" />
    <div v-if="canWork && undoable" class="mt-1 flex items-center gap-1">
      <button type="button" class="flex h-9 items-center gap-1 text-sm font-medium text-brand-800 underline" @click="emit('undo')">
        <Undo2 :size="14" /> {{ $t('Deshacer el último paso') }}
      </button>
    </div>
  </div>
</template>

<style scoped>
.act-btn {
  display: flex;
  min-height: 3rem;
  min-width: 0;
  align-items: center;
  justify-content: center;
  gap: 0.375rem;
  border-radius: 0.5rem;
  border-width: 1px;
  background: white;
  padding: 0.25rem 0.5rem;
  font-size: 0.9375rem;
  font-weight: 500;
  line-height: 1.15;
  text-align: center;
}
.act-btn:active {
  background: var(--color-stone-100);
}
</style>
