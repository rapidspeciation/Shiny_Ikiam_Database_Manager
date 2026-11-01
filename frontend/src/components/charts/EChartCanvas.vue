<script setup lang="ts">
import { onBeforeUnmount, onMounted, ref, watch } from 'vue'
import * as echarts from 'echarts/core'
import { BarChart, LineChart } from 'echarts/charts'
import { AriaComponent, GridComponent, LegendComponent, TooltipComponent } from 'echarts/components'
import { SVGRenderer } from 'echarts/renderers'
// @ts-expect-error: echarts ships its locale files (UMD) without types.
import langES from 'echarts/i18n/langES-obj.js'
import type { EChartsCoreOption } from 'echarts/core'
import { needsExtras } from './extras'
import { darkOption } from './chart'
import { locale } from '../../lib/i18n'
import { theme } from '../../lib/theme'

// What every chart uses (Inicio's bars and lines). Monitoreo's report also has a heat map, zoom
// bars and shaded areas: those parts load with the first chart that has them (echartsExtras.ts).
echarts.use([AriaComponent, BarChart, GridComponent, LegendComponent, LineChart, SVGRenderer, TooltipComponent])
let extras: Promise<unknown> | null = null
// What the chart says to a screen reader («This is a chart…») in the app's language.
echarts.registerLocale('ES', langES)

/** One Apache ECharts figure (SVG), resized with its container. */
const props = withDefaults(defineProps<{ option: EChartsCoreOption; height?: number }>(), { height: 240 })
const host = ref<HTMLDivElement>()
let chart: echarts.ECharts | null = null
let observer: ResizeObserver | null = null
let renders = 0

async function render() {
  const run = ++renders
  if (needsExtras(props.option)) await (extras ||= import('./echartsExtras'))
  // Another option arrived, or the chart was closed, while the extra parts loaded.
  if (run !== renders || !host.value) return
  chart ||= echarts.init(host.value, null, { renderer: 'svg', locale: locale.value === 'es' ? 'ES' : 'EN' })
  chart.setOption(theme.value === 'dark' ? darkOption(props.option) : props.option, { notMerge: true })
}
onMounted(() => {
  void render()
  observer = new ResizeObserver(() => chart?.resize())
  observer.observe(host.value!)
})
// A new theme: the same option in its colours (chart.ts darkOption).
watch([() => props.option, theme], render)
// Its locale is set when it is made: made again in the other language.
watch(locale, () => {
  chart?.dispose()
  chart = null
  void render()
})
onBeforeUnmount(() => {
  renders++
  observer?.disconnect()
  chart?.dispose()
  chart = null
})
</script>

<template>
  <div ref="host" class="w-full min-w-0" :style="{ height: `${height}px` }" />
</template>
