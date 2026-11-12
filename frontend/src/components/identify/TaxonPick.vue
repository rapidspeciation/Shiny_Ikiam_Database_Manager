<script setup lang="ts">
/**
 * «Es otra especie»: a compact field that suggests names as the person types (a combobox), from a TaxonSource
 * (lib/taxonPick.ts). A name no list knows may be typed freely; it is marked «nombre no reconocido».
 */
import { computed, ref } from 'vue'
import type { TaxonHit, TaxonSource } from '../../lib/taxonPick'

const props = defineProps<{ source: TaxonSource; modelValue: string }>()
const emit = defineEmits<{ 'update:modelValue': [string]; pick: [TaxonHit] }>()

const open = ref(false)
const active = ref(-1)
const hits = computed(() => props.source.search(props.modelValue, 8))
const unknown = computed(() => props.modelValue.trim().length >= 3 && !props.source.knows(props.modelValue))
const listId = `taxon-pick-${Math.random().toString(36).slice(2, 8)}`
const RANK = { genus: 'género', species: 'especie', subspecies: 'subespecie' } as const

function choose(h: TaxonHit) {
  emit('update:modelValue', h.name)
  emit('pick', h)
  open.value = false
  active.value = -1
}
function key(e: KeyboardEvent) {
  if (!hits.value.length) return
  if (e.key === 'ArrowDown') active.value = (active.value + 1) % hits.value.length
  else if (e.key === 'ArrowUp') active.value = (active.value - 1 + hits.value.length) % hits.value.length
  else if (e.key === 'Enter' && active.value >= 0) choose(hits.value[active.value])
  else if (e.key === 'Escape') open.value = false
  else return
  e.preventDefault()
}
</script>

<template>
  <div class="relative">
    <input
      :value="modelValue"
      type="text"
      class="field-input py-2.5 text-base"
      role="combobox"
      autocomplete="off"
      autocapitalize="off"
      spellcheck="false"
      :aria-expanded="open && hits.length > 0"
      :aria-controls="listId"
      :aria-activedescendant="active >= 0 ? `${listId}-${active}` : undefined"
      :aria-label="$t('Nombre de la especie')"
      :placeholder="$t('Escriba el género o la especie')"
      data-testid="taxon-input"
      @input="(emit('update:modelValue', ($event.target as HTMLInputElement).value), (open = true), (active = -1))"
      @focus="open = true"
      @blur="open = false"
      @keydown="key"
    />
    <ul
      v-if="open && hits.length"
      :id="listId"
      role="listbox"
      class="absolute inset-x-0 z-20 mt-1 max-h-64 overflow-auto rounded-md border border-border bg-surface shadow-lg"
    >
      <li
        v-for="(h, i) in hits"
        :id="`${listId}-${i}`"
        :key="h.name"
        role="option"
        :aria-selected="i === active"
        class="flex min-h-11 cursor-pointer items-center justify-between gap-2 px-3 text-sm"
        :class="i === active ? 'bg-brand-soft' : 'hover:bg-surface-sunken'"
        @mousedown.prevent="choose(h)"
      >
        <i>{{ h.name }}</i><span class="text-xs text-fg-subtle">{{ $t(RANK[h.rank]) }}</span>
      </li>
    </ul>
    <p v-if="unknown" class="mt-1 text-xs text-pending-text" data-testid="taxon-unknown">{{ $t('Nombre no reconocido: la foto se guardará como «sin identificar».') }}</p>
  </div>
</template>
