<script setup lang="ts">
import { defineAsyncComponent } from 'vue'
import type { EChartsCoreOption } from 'echarts/core'

/**
 * One Apache ECharts figure. The charting library (about 650 kB) is loaded on its
 * own, so a page shows its text and numbers without waiting for it; the figure's
 * space is kept meanwhile.
 */
const EChartCanvas = defineAsyncComponent(() => import('./EChartCanvas.vue'))
withDefaults(defineProps<{ option: EChartsCoreOption; height?: number }>(), { height: 240 })
</script>

<template>
  <div class="w-full min-w-0" :style="{ height: `${height}px` }">
    <EChartCanvas :option="option" :height="height" />
  </div>
</template>
