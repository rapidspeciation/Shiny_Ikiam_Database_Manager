<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, ref, watch } from 'vue'
import { CalendarDays, ChevronLeft, ChevronRight, X } from 'lucide-vue-next'
import { calendarDays, dayFirst, isoToSerial, parseDateInput, serialToIso, shiftMonth, todayIso } from '../lib/dates'
import { intlLocale } from '../lib/i18n'

/**
 * A date typed day first (28/09/2026), as the team writes it, with the app's
 * own calendar (Monday first, day first, in the interface's language): the
 * browser's date box follows the browser's language and its picker could not
 * always be closed. v-model is an ISO date ('' when empty). Accepts
 * 28/09/2026, 28-9-26, 280926, 28-Sep-26, "hoy"/"today" and "ayer"/"yesterday".
 *
 * The calendar closes by choosing a day, with ✕ or «Cancelar», with Escape,
 * by tapping or clicking outside it, or with the calendar button again. On a
 * narrow or short screen it shows in the middle over a dimmed page; otherwise
 * under (or over) the box. `openCalendar()` / `closeCalendar()` for a parent.
 */
defineOptions({ inheritAttrs: false })
const model = defineModel<string>({ default: '' })
const emit = defineEmits<{ calendar: [open: boolean] }>()

const shown = (iso: string) => (iso ? dayFirst(iso) : '')
const text = ref(shown(model.value))
const invalid = ref(false)
watch(model, iso => {
  text.value = shown(iso)
  invalid.value = false
})

function read(value: string): string | null {
  const s = value.trim().toLowerCase()
  if (!s) return ''
  if (s === 'hoy' || s === 'today') return todayIso()
  if (s === 'ayer' || s === 'yesterday') return serialToIso(isoToSerial(todayIso()) - 1)
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

// --- The calendar
const box = ref<HTMLInputElement>()
const toggle = ref<HTMLButtonElement>()
const panel = ref<HTMLElement>()
const isOpen = ref(false)
const view = ref({ year: 2026, month: 1 })
/** In the middle of the screen (phones) or beside the box (wider screens). */
const centred = ref(false)
/** A short screen (a phone on its side): smaller days, «Hoy» in the top row, no bottom row (✕ closes). */
const short = ref(false)
const place = ref<{ top: number; left: number }>({ top: 0, left: 0 })
const WIDTH = 304
const HEIGHT = 372

/** Today, read again each time the calendar opens (a screen left open overnight). */
const today = ref(todayIso())
const days = computed(() => calendarDays(view.value.year, view.value.month))
const title = computed(() =>
  new Intl.DateTimeFormat(intlLocale(), { month: 'long', year: 'numeric', timeZone: 'UTC' }).format(
    Date.UTC(view.value.year, view.value.month - 1, 1),
  ),
)
/** Monday … Sunday, short, in the interface's language (5 Jan 2026 was a Monday). */
const weekdays = computed(() =>
  Array.from({ length: 7 }, (_, i) =>
    new Intl.DateTimeFormat(intlLocale(), { weekday: 'short', timeZone: 'UTC' }).format(Date.UTC(2026, 0, 5 + i)),
  ),
)
const dayName = (iso: string) =>
  new Intl.DateTimeFormat(intlLocale(), { weekday: 'long', day: 'numeric', month: 'long', year: 'numeric', timeZone: 'UTC' }).format(
    Date.parse(`${iso}T12:00:00Z`),
  )

function position() {
  const r = (box.value ?? toggle.value)?.getBoundingClientRect()
  const vw = window.innerWidth
  const vh = window.innerHeight
  centred.value = vw < 640 || vh < 520 || !r
  short.value = vh < 480
  if (centred.value || !r) return
  const below = r.bottom + 4
  const top = below + HEIGHT <= vh - 8 ? below : Math.max(8, r.top - 4 - HEIGHT)
  const left = Math.min(Math.max(8, r.left), vw - WIDTH - 8)
  place.value = { top, left }
}
function openCalendar() {
  today.value = todayIso()
  // What is in the box (typed, not yet confirmed) or the value; else this month.
  const start = read(text.value) || model.value || today.value
  const [y, m] = start.split('-').map(Number)
  view.value = { year: y, month: m }
  position()
  isOpen.value = true
  emit('calendar', true)
  nextTick(() => panel.value?.querySelector<HTMLButtonElement>('[data-chosen=true], [data-today=true]')?.focus({ preventScroll: true }))
}
function closeCalendar({ refocus = false } = {}) {
  if (!isOpen.value) return
  isOpen.value = false
  emit('calendar', false)
  // Back to the button (not the box: on a phone that would open the keyboard).
  if (refocus) toggle.value?.focus({ preventScroll: true })
}
const toggleCalendar = () => (isOpen.value ? closeCalendar() : openCalendar())
function choose(iso: string) {
  text.value = shown(iso)
  invalid.value = false
  if (iso !== model.value) model.value = iso
  closeCalendar({ refocus: true })
}
const go = (step: number) => (view.value = shiftMonth(view.value.year, view.value.month, step))

/** Escape closes; the arrows move the day in focus (a keyboard on a computer). */
function onKey(e: KeyboardEvent) {
  if (!isOpen.value) return
  if (e.key === 'Escape') {
    e.preventDefault()
    e.stopPropagation()
    closeCalendar({ refocus: true })
    return
  }
  const focused = (document.activeElement as HTMLElement | null)?.dataset?.iso
  const step = { ArrowLeft: -1, ArrowRight: 1, ArrowUp: -7, ArrowDown: 7 }[e.key]
  if (!focused || !step) return
  e.preventDefault()
  const next = serialToIso(isoToSerial(focused) + step)
  const [y, m] = next.split('-').map(Number)
  if (y !== view.value.year || m !== view.value.month) view.value = { year: y, month: m }
  nextTick(() => panel.value?.querySelector<HTMLButtonElement>(`[data-iso="${next}"]`)?.focus())
}
/** A tap or click outside the calendar (and outside its button, which toggles it) closes it. */
function onPointer(e: Event) {
  const target = e.target as Node | null
  if (!isOpen.value || !target) return
  if (panel.value?.contains(target) || toggle.value?.contains(target)) return
  closeCalendar()
}
const follow = () => isOpen.value && !centred.value && position()
function listen(on: boolean) {
  if (on) {
    document.addEventListener('keydown', onKey, true)
    document.addEventListener('pointerdown', onPointer, true)
    window.addEventListener('resize', follow)
    window.addEventListener('scroll', follow, true)
    return
  }
  document.removeEventListener('keydown', onKey, true)
  document.removeEventListener('pointerdown', onPointer, true)
  window.removeEventListener('resize', follow)
  window.removeEventListener('scroll', follow, true)
}
watch(isOpen, listen)
onBeforeUnmount(() => {
  isOpen.value = false
  document.removeEventListener('keydown', onKey, true)
  document.removeEventListener('pointerdown', onPointer, true)
  window.removeEventListener('resize', follow)
  window.removeEventListener('scroll', follow, true)
})
defineExpose({ openCalendar, closeCalendar, isOpen })
</script>

<template>
  <span class="relative inline-flex w-full">
    <input
      ref="box"
      v-bind="$attrs"
      v-model="text"
      type="text"
      autocomplete="off"
      :placeholder="$t('dd/mm/aaaa')"
      :class="{ 'border-red-500 bg-red-50': invalid }"
      :title="invalid ? $t('Fecha no válida: escribe día/mes/año, p. ej. 28/09/2026') : undefined"
      class="w-full pr-10"
      @change="commit"
      @keydown.enter="commit"
    />
    <button
      ref="toggle"
      type="button"
      class="absolute inset-y-0 right-0 flex w-10 items-center justify-center rounded-r-md text-stone-500 hover:text-brand-700"
      :class="{ 'text-brand-700': isOpen }"
      :title="$t('Elegir en el calendario')"
      :aria-label="$t('Elegir en el calendario')"
      :aria-expanded="isOpen"
      aria-haspopup="dialog"
      tabindex="-1"
      @click.prevent.stop="toggleCalendar"
    >
      <CalendarDays :size="18" />
    </button>
    <Teleport to="body">
      <div v-if="isOpen && centred" class="fixed inset-0 z-[9998] bg-black/30" aria-hidden="true" />
      <div
        v-if="isOpen"
        ref="panel"
        role="dialog"
        :aria-label="$t('Calendario')"
        class="fixed z-[9999] max-h-[calc(100dvh-1rem)] overflow-y-auto overscroll-contain rounded-xl border border-stone-200 bg-white p-2 text-stone-800 shadow-xl"
        :class="[short ? 'w-[21rem]' : 'w-[19rem]', centred ? 'top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2' : '']"
        :style="centred ? undefined : { top: `${place.top}px`, left: `${place.left}px` }"
      >
        <div class="flex items-center gap-1">
          <button type="button" class="grid h-11 w-11 place-items-center rounded-md hover:bg-stone-100" :aria-label="$t('Mes anterior')" @click="go(-1)">
            <ChevronLeft :size="20" />
          </button>
          <p class="min-w-0 flex-1 text-center text-base font-semibold first-letter:uppercase" aria-live="polite">{{ title }}</p>
          <button type="button" class="grid h-11 w-11 place-items-center rounded-md hover:bg-stone-100" :aria-label="$t('Mes siguiente')" @click="go(1)">
            <ChevronRight :size="20" />
          </button>
          <button v-if="short" type="button" class="btn h-11 px-2" @click="choose(today)">{{ $t('Hoy') }}</button>
          <button type="button" class="grid h-11 w-11 place-items-center rounded-md text-stone-600 hover:bg-stone-100" :aria-label="$t('Cerrar')" @click="closeCalendar({ refocus: true })">
            <X :size="20" />
          </button>
        </div>
        <div class="mt-1 grid grid-cols-7 text-center text-[11px] font-medium text-stone-500 uppercase">
          <span v-for="w in weekdays" :key="w" class="py-1">{{ w }}</span>
        </div>
        <div class="grid grid-cols-7 gap-0.5">
          <button
            v-for="d in days"
            :key="d.iso"
            type="button"
            :data-iso="d.iso"
            :data-chosen="d.iso === model"
            :data-today="d.iso === today"
            class="rounded-md text-sm tabular-nums focus:ring-2 focus:ring-brand-600 focus:outline-none"
            :class="[
              short ? 'h-8' : 'h-10',
              d.iso === model ? 'bg-brand-700 font-semibold text-white' : d.inMonth ? 'hover:bg-brand-50' : 'text-stone-400 hover:bg-stone-50',
              d.iso === today && d.iso !== model ? 'font-semibold text-brand-800 ring-1 ring-brand-600 ring-inset' : '',
            ]"
            :aria-label="dayName(d.iso)"
            :aria-pressed="d.iso === model"
            @click="choose(d.iso)"
          >
            {{ d.day }}
          </button>
        </div>
        <div v-if="!short" class="mt-2 flex gap-2 border-t border-stone-100 pt-2">
          <button type="button" class="btn h-11 flex-1" @click="choose(today)">{{ $t('Hoy') }}</button>
          <button type="button" class="btn h-11 flex-1" @click="closeCalendar({ refocus: true })">{{ $t('Cancelar') }}</button>
        </div>
      </div>
    </Teleport>
  </span>
</template>
