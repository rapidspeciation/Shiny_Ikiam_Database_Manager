<script setup lang="ts">
import { ref } from 'vue'
import { ChevronDown, TriangleAlert } from 'lucide-vue-next'
import { WORKBOOK_WARNINGS } from '../lib/workbookWarnings'

/** Reminders of known faults in the Google Sheets workbook (formulas the app does not touch). */
defineProps<{ sheet?: string }>()
const panel = ref<HTMLDetailsElement>()
</script>

<template>
  <!-- Grey with an amber sign: apart from the unsaved-changes bar under it (the amber of unsaved cells). -->
  <details ref="panel" class="group border-t border-border bg-surface-sunken px-4 py-2 text-xs text-fg max-sm:px-3 max-sm:py-1.5">
    <!-- One short line on a phone, so the grid keeps the screen; the arrow shows it opens and closes. -->
    <summary class="flex cursor-pointer items-center gap-1.5 font-medium">
      <TriangleAlert :size="14" class="flex-none text-amber-700" />
      <span class="min-w-0 max-sm:hidden">
        {{ $t('Avisos del libro de Google Sheets ({n}) — fórmulas por corregir en la hoja', { n: WORKBOOK_WARNINGS.length }) }}
        <template v-if="sheet && WORKBOOK_WARNINGS.some(w => w.sheet === sheet)">{{ $t('· incluye {sheet}', { sheet }) }}</template>
      </span>
      <span class="min-w-0 sm:hidden">{{ $t('Avisos del libro ({n})', { n: WORKBOOK_WARNINGS.length }) }}</span>
      <ChevronDown :size="14" class="ml-auto flex-none text-fg-subtle transition-transform group-open:rotate-180 motion-reduce:transition-none" aria-hidden="true" />
    </summary>
    <ul class="mt-1.5 max-h-48 space-y-1 overflow-y-auto max-sm:max-h-32">
      <li v-for="(w, i) in WORKBOOK_WARNINGS" :key="i" :class="{ 'font-semibold': w.sheet === sheet }">
        <b>{{ w.sheet }}</b
        >: {{ $t(w.problem) }} <span class="text-fg-muted">→ {{ $t(w.fix) }}</span>
        <span class="text-fg-subtle">{{ $t('(visto {date})', { date: w.found }) }}</span>
      </li>
    </ul>
    <!-- On a phone the open list covers part of the grid: a close button under it, where the thumb is. -->
    <button type="button" class="btn mt-1.5 h-9 w-full sm:hidden" @click="panel && (panel.open = false)">{{ $t('Cerrar') }}</button>
  </details>
</template>
