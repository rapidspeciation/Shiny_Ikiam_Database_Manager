<script setup lang="ts">
import { computed, ref } from 'vue'
import { ArrowDown, ArrowUp, GripVertical, RotateCcw, X } from 'lucide-vue-next'
import { moveColumn } from '../../lib/proposalColumns'

/**
 * The person's own columns of a sheet's table (the «Personal» view of
 * Cambios propuestos): the shown ones in their order, dragged by the grip (a
 * mouse or a finger) or moved with the arrows, and ticked to show or untick to
 * hide; the sheet's other columns below, to tick. Columns the proposal writes
 * or marks always show (`kept`). Saved per person and sheet in this browser.
 */
const props = defineProps<{ sheet: string; shown: string[]; others: string[]; kept: string[] }>()
const emit = defineEmits<{
  order: [list: string[]]
  toggle: [field: string, on: boolean]
  reset: []
  close: []
}>()

/** The list while dragging (moved live under the finger), else the order shown. */
const draft = ref<string[] | null>(null)
const list = computed(() => draft.value ?? props.shown)
const dragged = ref<string | null>(null)
const keep = computed(() => new Set(props.kept))

function start(field: string, down: PointerEvent) {
  if (down.button !== 0) return
  down.preventDefault()
  const grip = down.currentTarget as HTMLElement
  const listEl = grip.closest('ol')
  // Followed on the window: the dragged item moves in the list (and loses the pointer's capture).
  const id = down.pointerId
  draft.value = [...props.shown]
  dragged.value = field
  const move = (e: PointerEvent) => {
    if (!draft.value || e.pointerId !== id) return
    const from = draft.value.indexOf(field)
    // The item under the pointer (by the middle of each one) takes the dragged column's place.
    let to = draft.value.length - 1
    const items = [...(listEl?.querySelectorAll<HTMLElement>('li[data-shown]') ?? [])]
    for (const [i, el] of items.entries()) {
      const r = el.getBoundingClientRect()
      if (e.clientY < r.top + r.height / 2) {
        to = i > from ? i - 1 : i
        break
      }
    }
    if (to !== from) draft.value = moveColumn(draft.value, from, to)
  }
  const up = (e: PointerEvent) => {
    if (e.pointerId !== id) return
    window.removeEventListener('pointermove', move)
    window.removeEventListener('pointerup', up)
    window.removeEventListener('pointercancel', up)
    const done = draft.value
    draft.value = null
    dragged.value = null
    if (done && done.join('|') !== props.shown.join('|')) emit('order', done)
  }
  window.addEventListener('pointermove', move)
  window.addEventListener('pointerup', up)
  window.addEventListener('pointercancel', up)
}
const step = (i: number, by: number) => emit('order', moveColumn(props.shown, i, i + by))
</script>

<template>
  <div class="column-chooser max-w-lg" role="dialog" :aria-label="$t('Columnas de {sheet}', { sheet })">
    <div class="flex items-center gap-2 border-b border-stone-200 px-2 py-1">
      <span class="font-medium text-stone-700">{{ $t('Columnas de {sheet}', { sheet }) }}</span>
      <span class="hidden text-stone-500 sm:inline">{{
        $t('Arrastra ⋮⋮ para ordenarlas; desmarca para ocultarlas. Se guardan para ti en este navegador.')
      }}</span>
      <button
        type="button"
        class="ml-auto flex items-center gap-0.5 rounded px-1 py-0.5 hover:bg-stone-100 hover:text-stone-800"
        :title="$t('Volver al orden de la hoja, sin columnas ocultas')"
        @click="emit('reset')"
      >
        <RotateCcw :size="12" /> {{ $t('Restablecer') }}
      </button>
      <button type="button" class="rounded p-0.5 hover:bg-stone-100" :aria-label="$t('Cerrar')" @click="emit('close')">
        <X :size="13" />
      </button>
    </div>
    <ol class="max-h-64 overflow-y-auto py-1">
      <li
        v-for="(f, i) in list"
        :key="f"
        data-shown
        class="flex items-center gap-1 px-1 py-0.5"
        :class="{ 'bg-emerald-50': dragged === f }"
      >
        <button
          type="button"
          class="cursor-grab touch-none rounded p-0.5 text-stone-400 hover:bg-stone-100 hover:text-stone-700 active:cursor-grabbing"
          :aria-label="$t('Arrastrar para mover {field}', { field: f })"
          @pointerdown="start(f, $event)"
        >
          <GripVertical :size="13" />
        </button>
        <label class="flex min-w-0 flex-1 cursor-pointer items-center gap-1.5" :title="keep.has(f) ? $t('Esta propuesta escribe o marca algo en esta columna: siempre se muestra') : ''">
          <input
            type="checkbox"
            class="h-3 w-3"
            checked
            :disabled="keep.has(f)"
            @change="emit('toggle', f, false)"
          />
          <span class="truncate" :class="{ 'font-medium text-emerald-800': keep.has(f) }">{{ f }}</span>
        </label>
        <button
          type="button"
          class="rounded p-0.5 text-stone-500 hover:bg-stone-100 disabled:opacity-30"
          :disabled="i === 0"
          :aria-label="$t('Mover a la izquierda')"
          :title="$t('Mover a la izquierda')"
          @click="step(i, -1)"
        >
          <ArrowUp :size="12" />
        </button>
        <button
          type="button"
          class="rounded p-0.5 text-stone-500 hover:bg-stone-100 disabled:opacity-30"
          :disabled="i === list.length - 1"
          :aria-label="$t('Mover a la derecha')"
          :title="$t('Mover a la derecha')"
          @click="step(i, 1)"
        >
          <ArrowDown :size="12" />
        </button>
      </li>
      <li v-if="others.length" class="mt-1 border-t border-stone-100 px-2 pt-1 text-stone-500">{{ $t('Ocultas u otras de la hoja') }}</li>
      <li v-for="f in others" :key="`+${f}`" class="flex items-center gap-1 px-1 py-0.5 pl-6">
        <label class="flex min-w-0 flex-1 cursor-pointer items-center gap-1.5 text-stone-500">
          <input type="checkbox" class="h-3 w-3" :checked="false" @change="emit('toggle', f, true)" />
          <span class="truncate">{{ f }}</span>
        </label>
      </li>
    </ol>
  </div>
</template>

<style scoped>
.column-chooser {
  margin: 0.25rem 0.5rem;
  border: 1px solid var(--color-stone-300);
  border-radius: 0.375rem;
  background: white;
  font-size: 11px;
  color: var(--color-stone-700);
  box-shadow: 0 4px 12px rgb(0 0 0 / 0.08);
}
</style>
