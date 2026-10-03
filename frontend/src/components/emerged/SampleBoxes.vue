<script setup lang="ts">
import { nextTick, ref, watch } from 'vue'
import { RotateCcw } from 'lucide-vue-next'
import type { Sample } from '../../lib/emerged'
import { normalizeId } from '../../lib/tubes'

/**
 * The CAM and tube of an egg or larva card, as on the Tubos cards: the next
 * free ones in grey until one is typed (the number pad; the letters stay), a
 * tube's letters chosen apart, Enter on to the same box of the next card, and
 * a misread one offered back or kept as written on its label.
 */
type Field = 'cam' | 'tube'
const props = defineProps<{
  draftKey: string
  id: string
  cam: Sample
  tube: Sample
  /** Each box's state: '' fine, `missing` (amber), `bad` (red: misread, repeated, used). */
  state: Record<Field, '' | 'missing' | 'bad'>
  /** The reading one digit away that lands next to the run (formProblem). */
  fix: Record<Field, string>
  /** An odd length that may be right (an old series): it can be kept as written. */
  canAccept: Record<Field, boolean>
  canEdit: boolean
}>()
const emit = defineEmits<{ type: [field: Field, value: string | undefined]; accept: [value: string] }>()

const PREFIXES = ['FS', 'FA', 'FD', 'FF']
const sample = (field: Field) => (field === 'cam' ? props.cam : props.tube)
/** The letters of a box: the value's own, else the box's kind. */
const prefixOf = (field: Field) => /^([A-Z]+)\d*$/.exec(sample(field).value)?.[1] ?? (field === 'cam' ? 'CAM' : 'FS')
const digitsOf = (field: Field) => {
  const v = sample(field).value
  return /^[A-Z]+(\d*)$/.exec(v)?.[1] ?? v
}
/** Digits keep the box's letters; letters typed (or a reader) give the whole ID. */
function onBox(field: Field, event: Event) {
  const input = event.target as HTMLInputElement
  delete input.dataset.fresh
  const raw = input.value
  const digits = raw.replace(/\D/g, '')
  emit('type', field, /[A-Za-z]/.test(raw) ? normalizeId(raw) : digits ? prefixOf(field) + digits : '')
}
function selectAll(event: FocusEvent) {
  const input = event.target as HTMLInputElement
  input.dataset.fresh = '1'
  input.select()
}
/** Enter goes to the same box of the next card (a run of tubes typed card after card); the last one closes the keyboard. */
function nextBox(event: KeyboardEvent) {
  const input = event.target as HTMLInputElement
  const kind = input.dataset.box!.split(':').at(-1)
  const boxes = [...document.querySelectorAll<HTMLInputElement>(`input[data-box$=":${kind}"]`)]
  const next = boxes[boxes.indexOf(input) + 1]
  if (next) {
    next.focus()
    next.select()
  } else input.blur()
}
// A box entered and not typed in yet stays selected when its suggestion changes under it.
const root = ref<HTMLElement>()
watch(
  () => [props.cam.value, props.tube.value],
  () =>
    nextTick(() => {
      const el = document.activeElement as HTMLInputElement | null
      if (el?.dataset.fresh && root.value?.contains(el)) el.select()
    }),
)
const border = (field: Field) =>
  props.state[field] === 'bad' ? 'border-red-500' : props.state[field] === 'missing' ? 'border-amber-500' : 'border-stone-300 focus-within:border-brand-600'
</script>

<template>
  <div ref="root" class="mt-1.5 grid grid-cols-2 gap-1.5 px-2">
    <div v-for="field in ['cam', 'tube'] as const" :key="field" class="min-w-0">
      <div class="flex h-6 items-center justify-between gap-1">
        <span class="field-label mb-0">{{ field === 'cam' ? 'CAM_ID' : 'Tube_1_id' }}</span>
        <span v-if="sample(field).auto && sample(field).value" class="truncate text-[11px] text-stone-500">{{ $t('siguiente libre') }}</span>
        <button
          v-else-if="!sample(field).auto && canEdit"
          class="flex h-6 items-center gap-0.5 text-[11px] text-stone-600 underline"
          :title="$t('Volver al siguiente libre')"
          @click="emit('type', field, undefined)"
        >
          <RotateCcw :size="12" /> {{ $t('siguiente libre') }}
        </button>
      </div>
      <div class="flex h-11 items-stretch overflow-hidden rounded-lg border bg-white focus-within:ring-2 focus-within:ring-brand-100" :class="border(field)">
        <span v-if="field === 'cam'" class="grid place-items-center bg-stone-100 px-1.5 font-mono text-sm text-stone-600">CAM</span>
        <select
          v-else
          :value="prefixOf('tube')"
          class="bg-stone-100 px-0.5 font-mono text-sm text-stone-600"
          :aria-label="$t('Letras del tubo')"
          :disabled="!canEdit"
          @change="emit('type', 'tube', ($event.target as HTMLSelectElement).value + digitsOf('tube'))"
        >
          <option v-for="p in [...new Set([prefixOf('tube'), ...PREFIXES])]" :key="p" :value="p">{{ p }}</option>
        </select>
        <input
          :value="digitsOf(field)"
          :data-box="`${draftKey}:${field}`"
          class="w-full min-w-0 flex-1 px-1.5 font-mono text-base tracking-wide outline-none"
          :class="sample(field).auto ? 'text-stone-500' : 'font-semibold text-stone-900'"
          inputmode="numeric"
          autocomplete="off"
          spellcheck="false"
          enterkeyhint="next"
          :placeholder="$t('Falta')"
          :aria-label="field === 'cam' ? $t('CAM de {id}', { id }) : $t('Tubo de {id}', { id })"
          :disabled="!canEdit"
          @focus="selectAll"
          @input="onBox(field, $event)"
          @keydown.enter.prevent="nextBox"
        />
      </div>
      <div v-if="canEdit && (fix[field] || canAccept[field])" class="mt-1 flex flex-wrap gap-1">
        <button v-if="fix[field]" class="btn h-9 px-2 text-xs" @click="emit('type', field, fix[field])">{{ $t('Usar {value}', { value: fix[field] }) }}</button>
        <button v-if="canAccept[field]" class="h-9 px-1 text-xs text-stone-600 underline" @click="emit('accept', sample(field).value)">
          {{ $t('Así está en la etiqueta') }}
        </button>
      </div>
    </div>
  </div>
</template>
