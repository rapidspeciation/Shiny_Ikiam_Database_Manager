<script setup lang="ts">
import { TriangleAlert } from 'lucide-vue-next'
import { WORKBOOK_WARNINGS } from '../lib/workbookWarnings'

/** Reminders of known faults in the Google Sheets workbook (formulas the app does not touch). */
defineProps<{ sheet?: string }>()
</script>

<template>
  <details class="border-t border-amber-200 bg-amber-50 px-4 py-2 text-xs text-amber-950">
    <summary class="flex cursor-pointer items-center gap-1.5 font-medium">
      <TriangleAlert :size="14" /> Avisos del libro de Google Sheets ({{ WORKBOOK_WARNINGS.length }}) — fórmulas por corregir en
      la hoja
      <template v-if="sheet && WORKBOOK_WARNINGS.some(w => w.sheet === sheet)">· incluye {{ sheet }}</template>
    </summary>
    <ul class="mt-1.5 max-h-48 space-y-1 overflow-y-auto">
      <li v-for="(w, i) in WORKBOOK_WARNINGS" :key="i" :class="{ 'font-semibold': w.sheet === sheet }">
        <b>{{ w.sheet }}</b
        >: {{ w.problem }} <span class="text-amber-800">→ {{ w.fix }}</span>
        <span class="text-amber-700">(visto {{ w.found }})</span>
      </li>
    </ul>
  </details>
</template>
