<script setup lang="ts">
import { ref } from 'vue'

/** A titled chart with its legend and a table view of the same numbers. */
defineProps<{
  title: string
  subtitle?: string
  legend?: { label: string; color: string; line?: boolean }[]
}>()
const showTable = ref(false)
</script>

<template>
  <section class="min-w-0 rounded-md border border-stone-200 bg-white p-3">
    <header class="mb-2 flex flex-wrap items-start gap-x-4 gap-y-1">
      <div class="min-w-0 flex-1">
        <h3 class="text-sm font-semibold text-stone-900">{{ title }}</h3>
        <p v-if="subtitle" class="text-xs text-stone-500">{{ subtitle }}</p>
      </div>
      <ul v-if="legend && legend.length > 1" class="flex flex-wrap items-center gap-x-3 gap-y-1 text-xs text-stone-600">
        <li v-for="l in legend" :key="l.label" class="flex items-center gap-1.5">
          <span v-if="l.line" class="inline-block h-0.5 w-3 rounded" :style="{ background: l.color }" />
          <span v-else class="inline-block h-2.5 w-2.5 rounded-sm" :style="{ background: l.color }" />
          {{ l.label }}
        </li>
      </ul>
      <button class="text-xs text-stone-500 underline hover:text-stone-800" @click="showTable = !showTable">
        {{ showTable ? 'Gráfico' : 'Tabla' }}
      </button>
    </header>
    <div v-if="showTable" class="max-h-72 overflow-auto text-xs"><slot name="table" /></div>
    <slot v-else />
  </section>
</template>
