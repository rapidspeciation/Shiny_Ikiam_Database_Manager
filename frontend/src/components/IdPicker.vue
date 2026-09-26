<script setup lang="ts">
import { computed, ref } from 'vue'
import { X } from 'lucide-vue-next'

/**
 * Multi-select for identifiers. Type to search, press Enter to add, or paste
 * a list ("N1D N2D, N3D") to add many at once.
 */
const props = withDefaults(defineProps<{ modelValue: string[]; options: string[]; label?: string; placeholder?: string }>(), {
  label: 'IDs',
  placeholder: 'Escribe o pega IDs',
})
const emit = defineEmits<{ 'update:modelValue': [value: string[]] }>()

const text = ref('')
const open = ref(false)
const unknown = ref<string[]>([])
const known = computed(() => new Set(props.options))
const matches = computed(() => {
  const q = text.value.trim().toUpperCase()
  const chosen = new Set(props.modelValue)
  return props.options.filter(o => !chosen.has(o) && (!q || o.toUpperCase().includes(q))).slice(0, 60)
})

function add(values: string[]) {
  const next = [...props.modelValue]
  const missing: string[] = []
  for (const raw of values) {
    const value = raw.trim()
    if (!value) continue
    const match = known.value.has(value) ? value : props.options.find(o => o.toUpperCase() === value.toUpperCase())
    if (!match) missing.push(value)
    else if (!next.includes(match)) next.push(match)
  }
  unknown.value = missing
  emit('update:modelValue', next)
}
function onKey(event: KeyboardEvent) {
  if (['Enter', ',', 'Tab'].includes(event.key) && text.value.trim()) {
    event.preventDefault()
    add([matches.value.find(m => m.toUpperCase() === text.value.trim().toUpperCase()) || matches.value[0] || text.value])
    text.value = ''
  } else if (event.key === 'Backspace' && !text.value && props.modelValue.length) {
    emit('update:modelValue', props.modelValue.slice(0, -1))
  } else if (event.key === 'Escape') open.value = false
}
function onPaste(event: ClipboardEvent) {
  const data = event.clipboardData?.getData('text') || ''
  if (/[\s,;]/.test(data.trim())) {
    event.preventDefault()
    add(data.split(/[\s,;]+/))
  }
}
function choose(value: string) {
  add([value])
  text.value = ''
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
        @focus="open = true"
        @blur="closeSoon"
        @keydown="onKey"
        @paste="onPaste"
      />
      <button
        v-if="modelValue.length"
        type="button"
        class="px-1 text-xs text-stone-500 hover:underline"
        @click="emit('update:modelValue', [])"
      >
        limpiar
      </button>
    </div>
    <p v-if="unknown.length" class="mt-1 text-xs text-red-700">No encontrados: {{ unknown.join(', ') }}</p>
    <ul
      v-if="open && matches.length"
      class="absolute z-30 mt-1 max-h-64 w-full overflow-y-auto rounded-md border border-stone-200 bg-white py-1 text-sm shadow-lg"
    >
      <li v-for="m in matches" :key="m">
        <button type="button" class="w-full px-3 py-1 text-left hover:bg-brand-50" @mousedown.prevent="choose(m)">
          {{ m }}
        </button>
      </li>
    </ul>
  </div>
</template>
