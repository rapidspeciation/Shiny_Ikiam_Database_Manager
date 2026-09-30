<script setup lang="ts">
import { onBeforeUnmount, onMounted, ref, watch } from 'vue'
import * as echarts from 'echarts/core'
import { BarChart, HeatmapChart, LineChart } from 'echarts/charts'
import {
  AriaComponent,
  DataZoomComponent,
  GridComponent,
  LegendComponent,
  MarkLineComponent,
  TooltipComponent,
  VisualMapComponent,
} from 'echarts/components'
import { SVGRenderer } from 'echarts/renderers'
import type { EChartsCoreOption } from 'echarts/core'

echarts.use([
  AriaComponent,
  BarChart,
  DataZoomComponent,
  GridComponent,
  HeatmapChart,
  LegendComponent,
  LineChart,
  MarkLineComponent,
  SVGRenderer,
  TooltipComponent,
  VisualMapComponent,
])

/** One Apache ECharts figure (SVG), resized with its container. */
const props = withDefaults(defineProps<{ option: EChartsCoreOption; height?: number }>(), { height: 240 })
const host = ref<HTMLDivElement>()
let chart: echarts.ECharts | null = null
let observer: ResizeObserver | null = null

function render() {
  if (!host.value) return
  chart ||= echarts.init(host.value, null, { renderer: 'svg' })
  chart.setOption(props.option, { notMerge: true })
}
onMounted(() => {
  render()
  observer = new ResizeObserver(() => chart?.resize())
  observer.observe(host.value!)
})
watch(() => props.option, render)
onBeforeUnmount(() => {
  observer?.disconnect()
  chart?.dispose()
  chart = null
})
</script>

<template>
  <div ref="host" class="w-full min-w-0" :style="{ height: `${height}px` }" />
</template>
