<script setup lang="ts">
import { computed } from 'vue'
import { format } from './chart'

/**
 * Horizontal bars for a ranked list (species, categories): one colour, the
 * value at the bar tip, an optional marker line (e.g. the 30-preserved rule).
 */
const props = withDefaults(
  defineProps<{
    rows: { label: string; value: number; detail?: string; italic?: boolean }[]
    color?: string
    marker?: number | null
    markerLabel?: string
  }>(),
  { color: '#2a78d6', marker: null, markerLabel: '' },
)
const max = computed(() => Math.max(1, props.marker || 0, ...props.rows.map(r => r.value)))
const pct = (v: number) => `${(100 * v) / max.value}%`
</script>

<template>
  <ul class="space-y-1 text-xs">
    <li v-for="r in rows" :key="r.label" class="grid grid-cols-[minmax(7rem,12rem)_1fr] items-center gap-2" :title="r.detail">
      <span class="truncate text-stone-700" :class="{ italic: r.italic }">{{ r.label }}</span>
      <span class="relative flex h-4 items-center">
        <span class="h-3 rounded-r" :style="{ width: pct(r.value), background: color, minWidth: r.value ? '2px' : '0' }" />
        <span class="ml-1.5 text-stone-700 tabular-nums">{{ format(r.value) }}</span>
        <span
          v-if="marker"
          class="absolute inset-y-[-2px] w-px bg-stone-500"
          :style="{ left: pct(marker) }"
          :title="markerLabel"
          aria-hidden="true"
        />
      </span>
    </li>
  </ul>
</template>
