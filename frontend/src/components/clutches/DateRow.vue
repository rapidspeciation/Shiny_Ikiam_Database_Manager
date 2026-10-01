<script setup lang="ts">
import { computed, ref } from 'vue'
import { PenLine } from 'lucide-vue-next'
import DateField from '../DateField.vue'
import { dayLabel, formatSerial, isoToSerial, serialFromIso, serialToIso, todayIso } from '../../lib/dates'
import type { CellValue } from '../../lib/types'
import { t } from '../../lib/i18n'

/**
 * A stage's date (DATE LAID, HATCHING DATE, PUPA DATE, EMERGENCE DATE), day
 * first, with Today and Yesterday. These dates are the first day of a stage
 * (the first pupation, the first emergence): once written, the date shows as
 * set and changes only through «Corregir», so a later round does not move it.
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
const open = computed(() => props.editable && (!set.value || props.dirty || correcting.value))
const message = ref('')
function pick(value: string) {
  message.value = ''
  if (!value) return emit('set', null)
  const s = serialFromIso(value)
  if (s === null) message.value = t('Fecha no válida: el año debe estar entre 1990 y 2099')
  else emit('set', s)
}
const today = () => todayIso()
const yesterday = () => serialToIso(isoToSerial(todayIso()) - 1)
</script>

<template>
  <div class="mt-2">
    <div v-if="!open" class="flex min-h-11 items-center gap-2">
      <span class="field-label mb-0 shrink-0">{{ field }}</span>
      <span class="min-w-0 flex-1 truncate text-sm">
        <template v-if="serial !== null">{{ formatSerial(serial) }}</template>
        <template v-else-if="set">{{ value }}</template>
        <template v-else>—</template>
        <span v-if="first && set" class="text-xs text-stone-500"> · {{ first }}</span>
      </span>
      <button
        v-if="editable && set"
        type="button"
        class="flex h-11 shrink-0 items-center gap-1 rounded-md px-2 text-sm text-stone-600 active:bg-stone-100"
        @click="correcting = true"
      >
        <PenLine :size="15" /> {{ $t('Corregir') }}
      </button>
    </div>
    <template v-else>
      <span class="field-label">{{ field }}</span>
      <p v-if="correcting && first" class="mb-1 text-xs text-amber-900">
        {{ $t('{field} es la fecha de la {first}: cámbiala solo para corregirla.', { field, first }) }}
      </p>
      <div class="flex gap-2">
        <DateField
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
  </div>
</template>
