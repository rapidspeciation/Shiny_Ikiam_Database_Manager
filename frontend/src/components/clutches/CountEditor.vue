<script setup lang="ts">
import { computed, ref } from 'vue'
import { Delete, Undo2 } from 'lucide-vue-next'
import { appendTerm, countedToday, countValue, readCount, removeLast, termLabels, totalOf, type CountResult } from '../../lib/clutches'
import type { CellValue } from '../../lib/types'
import { t } from '../../lib/i18n'

/**
 * One count of a clutch kept as the notebook sums it (=3+5-2): its history as
 * chips and the total; +N (more hatched, pupated or emerged), −N (died or
 * missing), "Counted today: N" (the difference is added) and "remove the last
 * term" (yesterday's −3, when the 3 turn up again). Each step writes the
 * team's formula, never a plain total.
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
function apply(result: CountResult) {
  if (!result.ok) {
    message.value = reasonText(result.reason)
    return
  }
  message.value = ''
  typed.value = ''
  emit('set', countValue(result.terms))
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
function dropLast() {
  message.value = ''
  emit('set', countValue(removeLast(count.value.terms)))
}
function revert() {
  message.value = ''
  emit('set', props.saved)
}
</script>

<template>
  <div>
    <div class="flex items-baseline justify-between gap-2">
      <span class="field-label mb-0 break-all">{{ field }}</span>
      <span class="shrink-0 text-2xl font-semibold tabular-nums" :class="dirty ? 'text-amber-800' : 'text-stone-900'">
        <template v-if="count.na">NA</template>
        <template v-else-if="count.text">{{ count.text }}</template>
        <template v-else-if="count.terms.length">{{ total }}</template>
        <template v-else>—</template>
      </span>
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
    <p v-if="message" class="mt-1 text-sm text-red-700">{{ message }}</p>
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
