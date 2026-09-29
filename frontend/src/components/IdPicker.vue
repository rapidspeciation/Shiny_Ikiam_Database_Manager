<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { X } from 'lucide-vue-next'
import { idTokens, resolveIds } from '../lib/ids'

/**
 * Multi-select for identifiers. Type an ID and press Enter to add it (only an
 * exact ID: "B9" does not pick B9D), pick from the list, or paste a list
 * ("N1D N2D, N3D") or a range ("B0D-B9D", in the sheet's pre-made order).
 */
const props = withDefaults(
  defineProps<{
    modelValue: string[]
    /** The IDs to choose from, newest first (the reverse of the sheet's row order). */
    options: string[]
    label?: string
    placeholder?: string
    /** The sheet is still loading: pasted IDs wait for it instead of being "not found". */
    loading?: boolean
    /** A warning for a chosen ID, e.g. "B9 ya murió el 3-Mar-22". */
    warn?: (id: string) => string | null
  }>(),
  { label: 'IDs', placeholder: 'Escribe, pega IDs o un rango (B0D-B9D)', loading: false, warn: undefined },
)
const emit = defineEmits<{ 'update:modelValue': [value: string[]] }>()

const text = ref('')
const open = ref(false)
const active = ref(-1)
const unknown = ref<string[]>([])
const hint = ref('')
/** IDs pasted before the sheet arrived. */
const waiting = ref<string[]>([])
const sheetOrder = computed(() => [...props.options].reverse())
const matches = computed(() => {
  const q = text.value.trim().toUpperCase()
  const chosen = new Set(props.modelValue)
  const free = props.options.filter(o => !chosen.has(o))
  if (!q) return free.slice(0, 60)
  // IDs that start with what was typed come first.
  const starts = free.filter(o => o.toUpperCase().startsWith(q))
  const within = free.filter(o => !o.toUpperCase().startsWith(q) && o.toUpperCase().includes(q))
  return [...starts, ...within].slice(0, 60)
})
const warnings = computed(() => (props.warn ? props.modelValue.map(id => props.warn!(id)).filter(Boolean) : []))

function add(tokens: string[]) {
  if (!tokens.length) return
  if (props.loading || !props.options.length) {
    waiting.value = [...waiting.value, ...tokens]
    return
  }
  const { found, missing } = resolveIds(tokens, sheetOrder.value)
  unknown.value = missing
  hint.value = ''
  emit('update:modelValue', [...new Set([...props.modelValue, ...found])])
}
// The sheet has arrived: the IDs pasted meanwhile are looked up now.
watch(
  () => [props.loading, props.options.length] as const,
  ([loading, count]) => {
    if (loading || !count || !waiting.value.length) return
    const tokens = waiting.value
    waiting.value = []
    add(tokens)
  },
)

function onKey(event: KeyboardEvent) {
  const typed = text.value.trim()
  if (event.key === 'ArrowDown' || event.key === 'ArrowUp') {
    event.preventDefault()
    open.value = true
    const step = event.key === 'ArrowDown' ? 1 : -1
    active.value = Math.max(-1, Math.min(matches.value.length - 1, active.value + step))
  } else if (['Enter', ',', 'Tab'].includes(event.key) && (typed || active.value >= 0)) {
    // Enter closes the list, so it cannot cover the buttons below (on a phone the next tap would pick from it).
    open.value = false
    if (active.value >= 0 && matches.value[active.value]) {
      event.preventDefault()
      choose(matches.value[active.value])
      return
    }
    const tokens = idTokens(typed)
    const exact = tokens.length === 1 && props.options.some(o => o.toUpperCase() === typed.toUpperCase())
    if (tokens.length > 1 || exact || tokens[0]?.includes('-') || props.loading || !props.options.length) {
      event.preventDefault()
      add(tokens)
      text.value = ''
    } else if (event.key !== 'Tab') {
      // Only an exact ID is added: a partial one ("B9") could be an old butterfly.
      event.preventDefault()
      hint.value = `«${typed}» no es un ID exacto: escríbelo completo o elígelo de la lista`
    }
  } else if (event.key === 'Backspace' && !text.value && props.modelValue.length) {
    emit('update:modelValue', props.modelValue.slice(0, -1))
  } else if (event.key === 'Escape') open.value = false
}
function onInput() {
  open.value = true
  active.value = -1
  hint.value = ''
}
function onPaste(event: ClipboardEvent) {
  const data = event.clipboardData?.getData('text') || ''
  const tokens = idTokens(data)
  if (tokens.length > 1 || tokens[0]?.includes('-')) {
    event.preventDefault()
    add(tokens)
  }
}
function choose(value: string) {
  add([value])
  text.value = ''
  active.value = -1
}
function closeSoon() {
  setTimeout(() => (open.value = false), 150)
}
function remove(value: string) {
  emit(
    'update:modelValue',
    props.modelValue.filter(v => v !== value),
  )
}
function clear() {
  waiting.value = []
  unknown.value = []
  emit('update:modelValue', [])
}
</script>

<template>
  <div class="relative min-w-64 flex-1">
    <span class="field-label">{{ label }} ({{ modelValue.length }})</span>
    <div
      class="flex min-h-9 flex-wrap items-center gap-1 rounded-md border border-stone-300 bg-white px-1.5 py-1 focus-within:border-brand-600 focus-within:ring-2 focus-within:ring-brand-100"
    >
      <span
        v-for="id in modelValue"
        :key="id"
        class="inline-flex items-center gap-0.5 rounded bg-brand-50 px-1.5 py-0.5 text-sm text-brand-800"
      >
        {{ id }}
        <button type="button" class="text-brand-700 hover:text-red-700" :aria-label="`Quitar ${id}`" @click="remove(id)">
          <X :size="13" />
        </button>
      </span>
      <input
        v-model="text"
        class="min-w-24 flex-1 border-0 p-0.5 text-sm outline-none"
        :placeholder="modelValue.length ? '' : placeholder"
        autocapitalize="characters"
        enterkeyhint="done"
        @focus="open = true"
        @input="onInput"
        @blur="closeSoon"
        @keydown="onKey"
        @paste="onPaste"
      />
      <button
        v-if="modelValue.length || waiting.length"
        type="button"
        class="px-1 text-xs text-stone-500 hover:underline"
        @click="clear"
      >
        limpiar
      </button>
    </div>
    <p v-if="waiting.length" class="mt-1 text-xs text-stone-600">
      Cargando IDs… {{ waiting.length }} {{ waiting.length === 1 ? 'ID espera' : 'IDs esperan' }} a que llegue la hoja
    </p>
    <p v-if="hint" class="mt-1 text-xs text-amber-800">{{ hint }}</p>
    <p v-if="unknown.length" class="mt-1 text-xs text-red-700">No encontrados: {{ unknown.join(', ') }}</p>
    <!-- Two at most, so a long batch does not push the page down; the rest on hover. -->
    <p v-for="w in warnings.slice(0, 2)" :key="w!" class="mt-1 text-xs text-amber-800">Atención: {{ w }}</p>
    <p v-if="warnings.length > 2" class="mt-1 text-xs text-amber-800" :title="warnings.slice(2).join('\n')">
      y {{ warnings.length - 2 }} avisos más (pasa el ratón para verlos)
    </p>
    <ul
      v-if="open && matches.length"
      class="absolute z-30 mt-1 max-h-64 w-full overflow-y-auto rounded-md border border-stone-200 bg-white py-1 text-sm shadow-lg"
    >
      <li v-for="(m, i) in matches" :key="m">
        <button
          type="button"
          class="w-full px-3 py-1 text-left hover:bg-brand-50"
          :class="{ 'bg-brand-50': i === active }"
          @mousedown.prevent="choose(m)"
        >
          {{ m }}
        </button>
      </li>
    </ul>
  </div>
</template>
