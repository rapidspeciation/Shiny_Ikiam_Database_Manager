<script setup lang="ts">
import { computed, onBeforeUnmount, ref } from 'vue'
import { format, niceTicks, type Series } from './chart'

/**
 * Columns over categories (months, hours, height classes), stacked when there
 * are several series. Hovering or focusing a column shows every series' value.
 */
const props = withDefaults(
  defineProps<{ categories: string[]; series: Series[]; height?: number; unit?: string; note?: (i: number) => string }>(),
  {
    height: 180,
    unit: '',
    note: undefined,
  },
)

const width = ref(640)
const host = ref<HTMLDivElement>()
const observer = new ResizeObserver(([e]) => (width.value = Math.max(240, e.contentRect.width)))
const bind = (el: unknown) => {
  if (el instanceof HTMLDivElement && el !== host.value) {
    host.value = el
    observer.observe(el)
  }
}
onBeforeUnmount(() => observer.disconnect())

const M = { top: 8, right: 8, bottom: 22, left: 34 }
const totals = computed(() => props.categories.map((_, i) => props.series.reduce((n, s) => n + (s.values[i] || 0), 0)))
const ticks = computed(() => niceTicks(Math.max(0, ...totals.value)))
const top = computed(() => ticks.value.at(-1) || 1)
const plotW = computed(() => width.value - M.left - M.right)
const plotH = computed(() => props.height - M.top - M.bottom)
const band = computed(() => plotW.value / Math.max(1, props.categories.length))
const barW = computed(() => Math.max(2, Math.min(24, band.value * 0.62)))
const y = (v: number) => M.top + plotH.value - (v / top.value) * plotH.value
/** Show a label every n categories so they never collide. */
const labelEvery = computed(() => Math.max(1, Math.ceil(props.categories.length / Math.max(1, Math.floor(plotW.value / 44)))))

/** Stacked segments with a 2px surface gap; only the top segment gets the rounded end. */
const columns = computed(() =>
  props.categories.map((label, i) => {
    let base = 0
    const parts = props.series
      .map(s => ({ s, v: s.values[i] || 0 }))
      .filter(p => p.v > 0)
      .map(p => {
        const y0 = y(base)
        base += p.v
        return { color: p.s.color, y: y(base), h: Math.max(0, y0 - y(base) - 2) }
      })
    return { label, i, x: M.left + band.value * i + (band.value - barW.value) / 2, parts }
  }),
)

const hover = ref<number | null>(null)
const tip = computed(() => {
  // The categories may have changed (another period) since the pointer hovered.
  if (hover.value === null || hover.value >= props.categories.length) return null
  const i = hover.value
  const x = M.left + band.value * (i + 0.5)
  return {
    left: Math.min(Math.max(x, 70), width.value - 70),
    title: props.categories[i],
    rows: props.series.map(s => ({ label: s.label, color: s.color, value: s.values[i] || 0 })),
    total: totals.value[i],
    note: props.note?.(i),
  }
})
function path(x: number, yTop: number, h: number, w: number, round: boolean) {
  const r = round ? Math.min(4, w / 2, h) : 0
  return `M${x},${yTop + h}V${yTop + r}Q${x},${yTop} ${x + r},${yTop}H${x + w - r}Q${x + w},${yTop} ${x + w},${yTop + r}V${yTop + h}Z`
}
</script>

<template>
  <div :ref="bind" class="relative w-full min-w-0 select-none overflow-hidden" @mouseleave="hover = null">
    <svg :width="width" :height="height" role="img" class="block">
      <g v-for="t in ticks" :key="t">
        <line
          :x1="M.left"
          :x2="width - M.right"
          :y1="y(t)"
          :y2="y(t)"
          :stroke="t === 0 ? '#c3c2b7' : '#e1e0d9'"
          stroke-width="1"
        />
        <text :x="M.left - 6" :y="y(t) + 3.5" text-anchor="end" font-size="10" fill="#898781" class="tabular-nums">
          {{ format(t) }}
        </text>
      </g>
      <g v-for="c in columns" :key="c.i">
        <path
          v-for="(p, j) in c.parts"
          :key="j"
          :d="path(c.x, p.y, p.h, barW, j === c.parts.length - 1)"
          :fill="p.color"
          :opacity="hover === null || hover === c.i ? 1 : 0.55"
        />
        <text
          v-if="c.i % labelEvery === 0"
          :x="c.x + barW / 2"
          :y="height - 6"
          text-anchor="middle"
          font-size="10"
          fill="#898781"
        >
          {{ c.label }}
        </text>
        <rect
          :x="M.left + band * c.i"
          :y="M.top"
          :width="band"
          :height="plotH"
          fill="transparent"
          tabindex="0"
          :aria-label="`${c.label}: ${format(totals[c.i])}`"
          @mouseenter="hover = c.i"
          @focus="hover = c.i"
          @blur="hover = null"
        />
      </g>
    </svg>
    <div
      v-if="tip"
      class="pointer-events-none absolute top-0 z-10 min-w-32 -translate-x-1/2 rounded-md border border-stone-200 bg-white px-2.5 py-1.5 text-xs shadow"
      :style="{ left: `${tip.left}px` }"
    >
      <p class="font-medium text-stone-600">{{ tip.title }}</p>
      <p v-if="series.length > 1" class="font-semibold text-stone-900">{{ format(tip.total) }} {{ unit }}</p>
      <p v-for="r in tip.rows" :key="r.label" class="flex items-center gap-1.5 text-stone-600">
        <span class="inline-block h-0.5 w-3 rounded" :style="{ background: r.color }" />
        <b class="text-stone-900">{{ format(r.value) }}</b> {{ series.length > 1 ? r.label : unit }}
      </p>
      <p v-if="tip.note" class="text-stone-500">{{ tip.note }}</p>
    </div>
  </div>
</template>
