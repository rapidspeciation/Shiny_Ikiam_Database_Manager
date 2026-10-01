<script setup lang="ts">
import { LayoutGrid, Table2 } from 'lucide-vue-next'
import type { EntryMode } from '../composables/useEntryMode'

/** The cards / table switch of a data-entry tab (see useEntryMode). `compact`: icons only (a narrow phone). */
const mode = defineModel<EntryMode>({ required: true })
<<<<<<< HEAD
=======
/** Icons only (a narrow toolbar on a phone); the names stay for screen readers. */
>>>>>>> worktree-agent-abfd13d3cbaa1fbc5
defineProps<{ compact?: boolean }>()
</script>

<template>
  <div class="inline-flex overflow-hidden rounded-md border border-stone-300 text-sm" role="group" :aria-label="$t('Vista')">
    <button
      type="button"
<<<<<<< HEAD
      class="flex items-center justify-center gap-1 px-2.5 py-1.5"
      :class="mode === 'cards' ? 'bg-brand-700 text-white' : 'bg-white text-stone-700 hover:bg-stone-50'"
=======
      class="flex items-center gap-1 px-2.5 py-1.5"
      :class="[mode === 'cards' ? 'bg-brand-700 text-white' : 'bg-white text-stone-700 hover:bg-stone-50', { 'min-h-11 min-w-11 justify-center': compact }]"
>>>>>>> worktree-agent-abfd13d3cbaa1fbc5
      :aria-pressed="mode === 'cards'"
      :aria-label="compact ? $t('Tarjetas') : undefined"
      :title="compact ? $t('Tarjetas') : undefined"
      @click="mode = 'cards'"
    >
<<<<<<< HEAD
      <LayoutGrid :size="compact ? 18 : 15" /><template v-if="!compact"> {{ $t('Tarjetas') }}</template>
    </button>
    <button
      type="button"
      class="flex items-center justify-center gap-1 border-l border-stone-300 px-2.5 py-1.5"
      :class="mode === 'table' ? 'bg-brand-700 text-white' : 'bg-white text-stone-700 hover:bg-stone-50'"
=======
      <LayoutGrid :size="15" /> <span :class="{ 'sr-only': compact }">{{ $t('Tarjetas') }}</span>
    </button>
    <button
      type="button"
      class="flex items-center gap-1 border-l border-stone-300 px-2.5 py-1.5"
      :class="[mode === 'table' ? 'bg-brand-700 text-white' : 'bg-white text-stone-700 hover:bg-stone-50', { 'min-h-11 min-w-11 justify-center': compact }]"
>>>>>>> worktree-agent-abfd13d3cbaa1fbc5
      :aria-pressed="mode === 'table'"
      :aria-label="compact ? $t('Tabla') : undefined"
      :title="compact ? $t('Tabla') : undefined"
      @click="mode = 'table'"
    >
<<<<<<< HEAD
      <Table2 :size="compact ? 18 : 15" /><template v-if="!compact"> {{ $t('Tabla') }}</template>
=======
      <Table2 :size="15" /> <span :class="{ 'sr-only': compact }">{{ $t('Tabla') }}</span>
>>>>>>> worktree-agent-abfd13d3cbaa1fbc5
    </button>
  </div>
</template>
