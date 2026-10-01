<script setup lang="ts">
import { computed, nextTick, ref } from 'vue'
import { PenLine, X } from 'lucide-vue-next'
import DateField from '../DateField.vue'
import { dayLabel, formatSerial, isoToSerial, serialFromIso, serialToIso, todayIso } from '../../lib/dates'
import type { CellValue } from '../../lib/types'
import { t } from '../../lib/i18n'

/**
 * A stage's date (DATE LAID, HATCHING DATE, PUPA DATE, EMERGENCE DATE), day
 * first, with Today and Yesterday. These dates are the first day of a stage
 * (the first pupation, the first emergence): once written, the date shows as
 * set and changes only through «Corregir» (pressed again, it closes), so a later
 * round does not move it.
 */
const props = defineProps<{
  field: string
  value: CellValue
  dirty: boolean
  editable: boolean
  /** What the date is the first of ("first pupation"), shown once it is set. */
  first?: string
  /** The count of this stage has something but the date is empty: suggest writing it. */
  suggest?: boolean
}>()
const emit = defineEmits<{ set: [value: CellValue] }>()

const serial = computed(() => (typeof props.value === 'number' ? props.value : null))
const iso = computed(() => (serial.value !== null ? serialToIso(serial.value) : ''))
const set = computed(() => props.value !== null && props.value !== undefined && String(props.value).trim() !== '')
// The parent keys each row by clutch, so a new clutch starts without «Corregir» open.
const correcting = ref(false)
/** The boxes show for a date still empty, or while correcting (a date just set folds back into its line). */
const open = computed(() => props.editable && (!set.value || correcting.value))
const message = ref('')
const dateField = ref<InstanceType<typeof DateField>>()
function pick(value: string) {
  message.value = ''
  if (!value) return emit('set', null)
  const s = serialFromIso(value)
  if (s === null) return (message.value = t('Fecha no válida: el año debe estar entre 1990 y 2099'))
  emit('set', s)
  // Corrected: back to the line (the calendar is gone with the boxes).
  correcting.value = false
}
/** «Corregir» opens the boxes with the calendar; pressed again (or «Cerrar»), it closes them. */
async function toggleCorrect() {
  message.value = ''
  correcting.value = !correcting.value
  if (correcting.value) {
    await nextTick()
    dateField.value?.openCalendar()
  }
}
const today = () => todayIso()
const yesterday = () => serialToIso(isoToSerial(todayIso()) - 1)
</script>

<template>
  <div class="mt-2">
    <!-- The date set: its line, with «Corregir» (a toggle). -->
    <div v-if="set" class="flex min-h-11 items-center gap-2">
      <span class="field-label mb-0 shrink-0">{{ field }}</span>
      <span class="min-w-0 flex-1 truncate text-sm" :class="{ 'rounded bg-amber-50 px-1 text-amber-900': dirty }">
        <template v-if="serial !== null">{{ formatSerial(serial) }}</template>
        <template v-else>{{ value }}</template>
        <span v-if="first" class="text-xs text-stone-500"> · {{ first }}</span>
      </span>
      <button
        v-if="editable"
        type="button"
        class="flex h-11 shrink-0 items-center gap-1 rounded-md border px-3 text-sm active:bg-stone-100"
        :class="correcting ? 'border-stone-400 bg-stone-100 text-stone-800' : 'border-transparent text-stone-600'"
        :aria-pressed="correcting"
        :aria-expanded="correcting"
        @click="toggleCorrect"
      >
        <template v-if="correcting"><X :size="15" /> {{ $t('Cerrar') }}</template>
        <template v-else><PenLine :size="15" /> {{ $t('Corregir') }}</template>
      </button>
    </div>
    <template v-if="open">
      <span v-if="!set" class="field-label">{{ field }}</span>
      <p v-if="correcting && first" class="mb-1 text-xs text-amber-900">
        {{ $t('{field} es la fecha de la {first}: cámbiala solo para corregirla.', { field, first }) }}
      </p>
      <div class="flex gap-2">
        <DateField
          ref="dateField"
          :model-value="iso"
          class="field-input h-12 text-base"
          :class="{ 'is-dirty': dirty, 'ring-2 ring-amber-300': suggest && !set }"
          @update:model-value="pick"
        />
        <button type="button" class="btn h-12 shrink-0 px-3" :class="{ 'border-brand-600 text-brand-800': suggest && !set }" @click="pick(today())">
          {{ $t('Hoy') }}
        </button>
        <button type="button" class="btn h-12 shrink-0 px-3" @click="pick(yesterday())">{{ $t('Ayer') }}</button>
      </div>
      <p v-if="message" class="mt-1 text-sm text-red-700">{{ message }}</p>
      <p v-else-if="iso" class="mt-1 text-xs text-stone-600">{{ dayLabel(iso) }}</p>
      <p v-else-if="suggest" class="mt-1 text-xs text-amber-900">{{ $t('Falta la fecha: ¿hoy?') }}</p>
    </template>
    <div v-else-if="!set" class="flex min-h-11 items-center gap-2">
      <span class="field-label mb-0 shrink-0">{{ field }}</span>
      <span class="text-sm">—</span>
    </div>
  </div>
</template>
