<script setup lang="ts">
import { computed, nextTick, ref } from 'vue'
import { Check, Delete, PenLine, Undo2, X } from 'lucide-vue-next'
import {
  appendTerm,
  countedToday,
  countValue,
  effectLabel,
  formulaOf,
  readCount,
  removeLast,
  termLabels,
  totalOf,
  typedTotal,
  type CountResult,
} from '../../lib/clutches'
import type { CellValue } from '../../lib/types'
import { t } from '../../lib/i18n'

/**
 * One count of a clutch kept as the notebook sums it (=3+5-2): its history as
 * chips and the total. Tapping the total and typing the new one is the main
 * way (32 → 30 adds −2 to the sum, as "Counted today"); also +N (more hatched,
 * pupated or emerged), −N (died or missing), "Counted today: N" and "remove
 * the last term" (yesterday's −3, when the 3 turn up again). Each step writes
 * the team's formula, never a plain total.
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
}>()
const emit = defineEmits<{ set: [value: CellValue] }>()

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
 * −N or +N added to it): Undo steps back one change at a time.
 */
const steps = ref<CellValue[]>([])
function setCount(value: CellValue) {
  steps.value.push(props.value)
  emit('set', value)
}
function undoStep() {
  if (!steps.value.length) return
  message.value = ''
  emit('set', steps.value.pop() as CellValue)
}
const same = (a: CellValue | undefined, b: CellValue | undefined) => String(a ?? '').replace(/\s+/g, '') === String(b ?? '').replace(/\s+/g, '')
/** Today's changes to this count, taken back at once: the formula it had this morning. */
const changedToday = computed(() => props.startOfDay !== undefined && !same(props.startOfDay, props.value))
const startText = computed(() => {
  const c = readCount(props.startOfDay)
  return c.na ? 'NA' : c.terms.length ? `${formulaOf(c.terms)} (${totalOf(c.terms)})` : '—'
})
function backToMorning() {
  message.value = ''
  setCount(props.startOfDay ?? null)
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
const plus = () => (n.value === null ? (message.value = reasonText('empty')) : apply(appendTerm(count.value.terms, n.value)))
const minus = () => (n.value === null ? (message.value = reasonText('empty')) : apply(appendTerm(count.value.terms, -n.value)))
const counted = () => (n.value === null ? (message.value = reasonText('empty')) : apply(countedToday(count.value.terms, n.value)))
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
  apply(r)
  stopTyping()
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

function dropLast() {
  message.value = ''
  setCount(countValue(removeLast(count.value.terms)))
}
function revert() {
  message.value = ''
  setCount(props.saved)
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
      <button
        type="button"
        class="btn-primary h-11 shrink-0 px-4"
        :disabled="!typedResult.ok"
        @mousedown.prevent
        @click="applyTotal"
      >
        <Check :size="18" /> {{ $t('Poner') }}
      </button>
      <button
        type="button"
        class="btn h-11 w-11 shrink-0 px-0"
        :aria-label="$t('Cancelar')"
        @pointerdown="willCancel"
        @mousedown.prevent
        @click="stopTyping"
      >
        <X :size="18" />
      </button>
    </div>
    <!-- The history: each term a chip; the last one can be taken back. -->
    <div v-if="count.terms.length" class="mt-1 flex flex-wrap items-center gap-1" :aria-label="$t('Historia de la suma')">
      <span
        v-for="(label, i) in termLabels(count.terms)"
        :key="i"
        class="rounded-md px-2 py-0.5 text-sm font-medium tabular-nums"
        :class="[
          count.terms[i] < 0 ? 'bg-red-50 text-red-800' : 'bg-stone-100 text-stone-800',
          dirty && i === count.terms.length - 1 ? 'ring-1 ring-amber-400' : '',
        ]"
        >{{ label }}</span
      >
      <span class="text-sm text-stone-500 tabular-nums">= {{ total }}</span>
      <button
        v-if="canWork"
        type="button"
        class="ml-auto flex h-11 items-center gap-1 rounded-md px-2 text-sm text-stone-700 active:bg-stone-100"
        :aria-label="$t('Quitar el último término ({term})', { term: termLabels(count.terms).at(-1) ?? '' })"
        @click="dropLast"
      >
        <Delete :size="18" /> {{ $t('Quitar {term}', { term: termLabels(count.terms).at(-1) ?? '' }) }}
      </button>
    </div>
    <p v-if="locked" class="mt-1 text-xs text-stone-500">{{ $t('Fórmula de la hoja (solo lectura)') }}</p>
    <p v-else-if="count.text && editable" class="mt-1 text-xs text-amber-900">
      {{ $t('No es una suma: corrígelo en la tabla') }}
    </p>
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
      <button type="button" class="count-btn border-red-300 text-red-800" :aria-label="$t('Restar {n}', { n: typed })" @click="minus">
        <span class="text-lg leading-none font-semibold">−{{ n ?? '' }}</span>
        <span class="text-[11px] leading-tight">{{ $t('murieron / faltan') }}</span>
      </button>
      <button type="button" class="count-btn border-stone-400 text-stone-800" @click="counted">
        <span class="text-sm leading-none font-semibold">{{ n === null ? $t('Contados') : $t('Contados: {n}', { n }) }}</span>
        <span class="text-[11px] leading-tight">{{ countedEffect || $t('hoy') }}</span>
      </button>
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
      <span class="text-sm tabular-nums" :class="formulaResult.ok ? 'font-medium text-brand-800' : 'text-stone-600'">{{
        formulaResult.ok ? formulaResult.label : ''
      }}</span>
      <button type="button" class="btn-primary h-11 px-4" :disabled="!formulaResult.ok" @click="applyFormula">
        <Check :size="18" /> {{ $t('Poner') }}
      </button>
      <button type="button" class="btn h-11 px-3" :aria-label="$t('Cancelar')" @click="editingFormula = false"><X :size="18" /></button>
    </div>
    <p v-if="message" class="mt-1 text-sm text-red-700">{{ message }}</p>
    <div v-if="canWork" class="mt-1 flex flex-wrap items-center gap-x-4">
      <button v-if="steps.length" type="button" class="flex h-9 items-center gap-1 text-sm font-medium text-brand-800 underline" @click="undoStep">
        <Undo2 :size="14" /> {{ $t('Deshacer') }}
      </button>
      <button v-if="changedToday" type="button" class="flex h-9 items-center gap-1 text-sm text-stone-600 underline" @click="backToMorning">
        <Undo2 :size="14" /> {{ $t('Volver a como estaba esta mañana: {value}', { value: startText }) }}
      </button>
      <button v-if="!editingFormula" type="button" class="flex h-9 items-center gap-1 text-sm text-stone-600 underline" @click="startFormula">
        <PenLine :size="14" /> {{ $t('Editar la fórmula') }}
      </button>
    </div>
    <button v-if="dirty && editable" type="button" class="mt-1 flex h-9 items-center gap-1 text-sm text-stone-600 underline" @click="revert">
      <Undo2 :size="14" /> {{ $t('Deshacer los cambios de este número') }}
    </button>
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
