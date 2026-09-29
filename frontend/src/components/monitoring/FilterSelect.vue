<script setup lang="ts">
import { computed, nextTick, ref } from 'vue'
import { ChevronDown, X } from 'lucide-vue-next'

/**
 * A searchable list of checkboxes with counts, like the species filter of the
 * atlas (ithomiini_maps). Nothing chosen means "all".
 */
export interface FilterOption {
  value: string
  label: string
  count: number
  color?: string
  /** Shown in italics (species names). */
  italic?: boolean
  /** Heading the option is listed under (e.g. the year of a date). */
  group?: string
}

// Without placeholder / allLabel: "Buscar…" and "Todos" in the interface language.
const props = defineProps<{
  modelValue: string[]
  options: FilterOption[]
  label: string
  placeholder?: string
  allLabel?: string
}>()
const emit = defineEmits<{ 'update:modelValue': [value: string[]] }>()

const open = ref(false)
const query = ref('')
const active = ref(0)
const input = ref<HTMLInputElement>()
const root = ref<HTMLDivElement>()

const fold = (s: string) =>
  s
    .normalize('NFD')
    .replace(/\p{Diacritic}/gu, '')
    .toLowerCase()
const matches = computed(() => {
  const q = fold(query.value.trim())
  return q ? props.options.filter(o => fold(`${o.label} ${o.group || ''}`).includes(q)) : props.options
})
const chosen = computed(() => new Set(props.modelValue))
const labelOf = computed(() => new Map(props.options.map(o => [o.value, o.label])))

function toggle(value: string) {
  emit('update:modelValue', chosen.value.has(value) ? props.modelValue.filter(v => v !== value) : [...props.modelValue, value])
}
function show() {
  open.value = true
  active.value = 0
  nextTick(() => input.value?.focus())
}
function onKey(event: KeyboardEvent) {
  if (event.key === 'ArrowDown') active.value = Math.min(active.value + 1, matches.value.length - 1)
  else if (event.key === 'ArrowUp') active.value = Math.max(active.value - 1, 0)
  else if (event.key === 'Enter' && matches.value[active.value]) toggle(matches.value[active.value].value)
  else if (event.key === 'Escape') open.value = false
  else return
  event.preventDefault()
  nextTick(() => root.value?.querySelector('[data-active="true"]')?.scrollIntoView({ block: 'nearest' }))
}
function onFocusOut(event: FocusEvent) {
  if (!root.value?.contains(event.relatedTarget as Node)) {
    open.value = false
    query.value = ''
  }
}
</script>

<template>
  <div ref="root" class="relative" @focusout="onFocusOut">
    <span class="field-label">{{ label }} ({{ options.length }})</span>
    <button
      type="button"
      class="field-input flex items-center gap-1 text-left"
      :aria-expanded="open"
      @click="open ? (open = false) : show()"
    >
      <span class="min-w-0 flex-1 truncate" :class="modelValue.length ? '' : 'text-stone-500'">
        {{
          modelValue.length === 0
            ? (allLabel ?? $t('Todos'))
            : modelValue.length === 1
              ? labelOf.get(modelValue[0]) || modelValue[0]
              : $t('{n} elegidos', { n: modelValue.length })
        }}
      </span>
      <ChevronDown :size="15" class="shrink-0 text-stone-500" />
    </button>
    <div v-if="modelValue.length > 1" class="mt-1 flex flex-wrap gap-1">
      <button
        v-for="v in modelValue"
        :key="v"
        type="button"
        class="inline-flex max-w-full items-center gap-1 rounded-full bg-brand-50 px-2 py-0.5 text-xs text-brand-900 hover:bg-brand-100"
        :title="$t('Quitar {name}', { name: labelOf.get(v) || v })"
        @click="toggle(v)"
      >
        <span class="truncate">{{ labelOf.get(v) || v }}</span>
        <X :size="11" class="shrink-0" />
      </button>
    </div>
    <div
      v-if="open"
      class="absolute inset-x-0 z-[1100] mt-1 flex max-h-80 flex-col rounded-md border border-stone-300 bg-white shadow-lg"
    >
      <div class="flex items-center gap-1 border-b border-stone-200 p-1.5">
        <input
          ref="input"
          v-model="query"
          class="min-w-0 flex-1 rounded border-0 px-1.5 py-1 text-sm focus:ring-0 focus:outline-none"
          :placeholder="placeholder ?? $t('Buscar…')"
          @keydown="onKey"
          @input="active = 0"
        />
        <button v-if="modelValue.length" type="button" class="btn-ghost text-xs" @click="emit('update:modelValue', [])">
          {{ $t('Quitar') }}
        </button>
      </div>
      <ul class="overflow-y-auto py-1" role="listbox" aria-multiselectable="true">
        <template v-for="(o, i) in matches" :key="o.value">
          <li
            v-if="o.group && o.group !== matches[i - 1]?.group"
            class="px-2.5 pt-1.5 pb-0.5 text-[11px] font-semibold tracking-wide text-stone-500 uppercase"
          >
            {{ o.group }}
          </li>
          <li
            role="option"
            :aria-selected="chosen.has(o.value)"
            :data-active="i === active"
            class="flex cursor-pointer items-center gap-2 px-2.5 py-1 text-sm"
            :class="[i === active ? 'bg-stone-100' : '', o.count ? '' : 'text-stone-400']"
            @mousedown.prevent="toggle(o.value)"
            @mousemove="active = i"
          >
            <input type="checkbox" tabindex="-1" class="pointer-events-none" :checked="chosen.has(o.value)" />
            <span
              v-if="o.color"
              class="h-2.5 w-2.5 shrink-0 rounded-full border border-stone-700"
              :style="{ background: o.color }"
            />
            <span class="min-w-0 flex-1 truncate" :class="o.italic ? 'italic' : ''">{{ o.label }}</span>
            <span class="text-xs text-stone-500 tabular-nums">{{ o.count }}</span>
          </li>
        </template>
        <li v-if="!matches.length" class="px-2.5 py-2 text-sm text-stone-500">{{ $t('Sin resultados') }}</li>
      </ul>
    </div>
  </div>
</template>
