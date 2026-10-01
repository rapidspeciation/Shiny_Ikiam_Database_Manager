<script setup lang="ts">
import { LayoutGrid, Table2 } from 'lucide-vue-next'
import type { EntryMode } from '../composables/useEntryMode'

/**
 * The cards / table switch of a data-entry tab (see useEntryMode). `compact`: icons
 * only (a narrow phone toolbar), as big touch targets; the names stay for screen readers.
 */
const mode = defineModel<EntryMode>({ required: true })
defineProps<{ compact?: boolean }>()
</script>

<template>
  <div class="inline-flex overflow-hidden rounded-md border border-stone-300 text-sm" role="group" :aria-label="$t('Vista')">
    <button
      type="button"
      class="flex items-center justify-center gap-1 px-2.5 py-1.5"
      :class="[mode === 'cards' ? 'bg-brand-700 text-white' : 'bg-white text-stone-700 hover:bg-stone-50', { 'min-h-11 min-w-11': compact }]"
      :aria-pressed="mode === 'cards'"
      :title="compact ? $t('Tarjetas') : undefined"
      @click="mode = 'cards'"
    >
      <LayoutGrid :size="compact ? 18 : 15" /> <span :class="{ 'sr-only': compact }">{{ $t('Tarjetas') }}</span>
    </button>
    <button
      type="button"
      class="flex items-center justify-center gap-1 border-l border-stone-300 px-2.5 py-1.5"
      :class="[mode === 'table' ? 'bg-brand-700 text-white' : 'bg-white text-stone-700 hover:bg-stone-50', { 'min-h-11 min-w-11': compact }]"
      :aria-pressed="mode === 'table'"
      :title="compact ? $t('Tabla') : undefined"
      @click="mode = 'table'"
    >
      <Table2 :size="compact ? 18 : 15" /> <span :class="{ 'sr-only': compact }">{{ $t('Tabla') }}</span>
    </button>
  </div>
</template>
