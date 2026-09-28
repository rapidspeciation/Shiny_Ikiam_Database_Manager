<script setup lang="ts">
import { ref, watch } from 'vue'
import { CalendarDays } from 'lucide-vue-next'
import { isoToSerial, parseDateInput, serialToIso, todayIso } from '../lib/dates'

/**
 * A date typed day first (28/09/2026), as the team writes it. The browser's
 * own date box follows the browser's language (month first in English), so
 * it is only used for the calendar button. v-model is an ISO date ('' when empty).
 * Accepts 28/09/2026, 28-9-26, 280926, 28-Sep-26, "hoy" and "ayer".
 */
defineOptions({ inheritAttrs: false })
const model = defineModel<string>({ default: '' })

const shown = (iso: string) => {
  if (!iso) return ''
  const [y, m, d] = iso.split('-')
  return `${d}/${m}/${y}`
}
const text = ref(shown(model.value))
const invalid = ref(false)
const picker = ref<HTMLInputElement>()
watch(model, iso => {
  text.value = shown(iso)
  invalid.value = false
})

function read(value: string): string | null {
  const s = value.trim().toLowerCase()
  if (!s) return ''
  if (s === 'hoy') return todayIso()
  if (s === 'ayer') return serialToIso(isoToSerial(todayIso()) - 1)
  const serial = parseDateInput(s)
  return serial === null ? null : serialToIso(serial)
}
function commit() {
  const iso = read(text.value)
  invalid.value = iso === null
  if (iso === null) return
  text.value = shown(iso)
  if (iso !== model.value) model.value = iso
}
function openCalendar() {
  const el = picker.value
  if (!el) return
  try {
    el.showPicker()
  } catch {
    el.click()
  }
}
</script>

<template>
  <span class="relative inline-flex w-full">
    <input
      v-bind="$attrs"
      v-model="text"
      type="text"
      autocomplete="off"
      placeholder="dd/mm/aaaa"
      :class="{ 'border-red-500 bg-red-50': invalid }"
      :title="invalid ? 'Fecha no válida: escribe día/mes/año, p. ej. 28/09/2026' : undefined"
      class="w-full pr-9"
      @change="commit"
      @keydown.enter="commit"
    />
    <button
      type="button"
      class="absolute inset-y-0 right-0 flex w-8 items-center justify-center text-stone-500 hover:text-brand-700"
      title="Elegir en el calendario"
      tabindex="-1"
      @click="openCalendar"
    >
      <CalendarDays :size="16" />
    </button>
    <!-- The calendar only; what shows is the text box above. -->
    <input
      ref="picker"
      type="date"
      class="pointer-events-none absolute right-0 bottom-0 h-0 w-0 opacity-0"
      tabindex="-1"
      aria-hidden="true"
      :value="model"
      @change="model = ($event.target as HTMLInputElement).value"
    />
  </span>
</template>
