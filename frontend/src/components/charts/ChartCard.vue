<script setup lang="ts">
import { ref } from 'vue'

/**
 * A titled figure with a "Tabla" view of the same numbers (every value is
 * readable without hovering). The legend, when several series, is the chart's own.
 */
defineProps<{
  title: string
  subtitle?: string
  table?: { head: string[]; rows: (string | number)[][] }
}>()
const showTable = ref(false)
</script>

<template>
  <section class="min-w-0 rounded-md border border-stone-200 bg-white p-3">
    <header class="mb-1 flex items-start gap-3">
      <div class="min-w-0 flex-1">
        <h3 class="text-sm font-semibold text-stone-900">{{ title }}</h3>
        <p v-if="subtitle" class="text-xs text-stone-500">{{ subtitle }}</p>
      </div>
      <button v-if="table" class="shrink-0 text-xs text-stone-500 underline hover:text-stone-800" @click="showTable = !showTable">
        {{ showTable ? 'Gráfico' : 'Tabla' }}
      </button>
    </header>
    <div v-if="showTable && table" class="max-h-72 overflow-auto text-xs">
      <table class="w-full">
        <thead class="sticky top-0 bg-white text-stone-500">
          <tr>
            <th v-for="(h, i) in table.head" :key="h" class="px-1 py-1" :class="i ? 'text-right' : 'text-left'">{{ h }}</th>
          </tr>
        </thead>
        <tbody class="tabular-nums">
          <tr v-for="(r, j) in table.rows" :key="j" class="border-t border-stone-100">
            <td v-for="(c, i) in r" :key="i" class="px-1 py-0.5" :class="i ? 'text-right' : 'text-left'">{{ c }}</td>
          </tr>
        </tbody>
      </table>
    </div>
    <slot v-else />
  </section>
</template>
