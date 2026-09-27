<script setup lang="ts">
import { computed, onBeforeUnmount, ref } from 'vue'
import { format, niceTicks, type Series } from './chart'

/**
 * One line per series over the same categories (e.g. one line per year over
 * the months). A crosshair snaps to the nearest category and lists every series.
 */
const props = withDefaults(defineProps<{ categories: string[]; series: Series[]; height?: number; unit?: string }>(), {
  height: 200,
  unit: '',
})

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

const M = { top: 10, right: 12, bottom: 22, left: 34 }
const ticks = computed(() => niceTicks(Math.max(0, ...props.series.flatMap(s => s.values.filter(v => Number.isFinite(v))))))
const top = computed(() => ticks.value.at(-1) || 1)
const plotW = computed(() => width.value - M.left - M.right)
const plotH = computed(() => props.height - M.top - M.bottom)
const x = (i: number) =>
  M.left + (props.categories.length < 2 ? plotW.value / 2 : (i / (props.categories.length - 1)) * plotW.value)
const y = (v: number) => M.top + plotH.value - (v / top.value) * plotH.value

/** Missing values (NaN) break the line instead of dropping to zero. */
const paths = computed(() =>
  props.series.map(s => {
    let d = ''
    let pen = false
    s.values.forEach((v, i) => {
      if (!Number.isFinite(v)) {
        pen = false
        return
      }
      d += `${pen ? 'L' : 'M'}${x(i)},${y(v)}`
      pen = true
    })
    return { s, d }
  }),
)

const hover = ref<number | null>(null)
function move(event: PointerEvent) {
  const box = (event.currentTarget as SVGElement).getBoundingClientRect()
  const px = event.clientX - box.left
  const step = plotW.value / Math.max(1, props.categories.length - 1)
  hover.value = Math.max(0, Math.min(props.categories.length - 1, Math.round((px - M.left) / step)))
}
const tip = computed(() =>
  hover.value === null || hover.value >= props.categories.length
    ? null
    : {
        left: Math.min(Math.max(x(hover.value), 70), width.value - 70),
        title: props.categories[hover.value],
        rows: props.series
          .map(s => ({ label: s.label, color: s.color, value: s.values[hover.value!] }))
          .filter(r => Number.isFinite(r.value)),
      },
)
</script>

<template>
  <div :ref="bind" class="relative w-full min-w-0 select-none overflow-hidden">
    <svg
      :width="width"
      :height="height"
      role="img"
      class="block"
      tabindex="0"
      @pointermove="move"
      @pointerleave="hover = null"
      @keydown.right="hover = Math.min(categories.length - 1, (hover ?? -1) + 1)"
      @keydown.left="hover = Math.max(0, (hover ?? 1) - 1)"
      @blur="hover = null"
    >
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
      <text v-for="(c, i) in categories" :key="c" :x="x(i)" :y="height - 6" text-anchor="middle" font-size="10" fill="#898781">
        {{ c }}
      </text>
      <line
        v-if="hover !== null"
        :x1="x(hover)"
        :x2="x(hover)"
        :y1="M.top"
        :y2="M.top + plotH"
        stroke="#c3c2b7"
        stroke-width="1"
      />
      <path
        v-for="p in paths"
        :key="p.s.key"
        :d="p.d"
        fill="none"
        :stroke="p.s.color"
        stroke-width="2"
        stroke-linejoin="round"
        stroke-linecap="round"
      />
      <template v-if="hover !== null">
        <template v-for="p in paths" :key="`dot-${p.s.key}`">
          <circle
            v-if="Number.isFinite(p.s.values[hover])"
            :cx="x(hover)"
            :cy="y(p.s.values[hover])"
            r="4"
            :fill="p.s.color"
            stroke="#ffffff"
            stroke-width="2"
          />
        </template>
      </template>
    </svg>
    <div
      v-if="tip"
      class="pointer-events-none absolute top-0 z-10 min-w-28 -translate-x-1/2 rounded-md border border-stone-200 bg-white px-2.5 py-1.5 text-xs shadow"
      :style="{ left: `${tip.left}px` }"
    >
      <p class="font-medium text-stone-600">{{ tip.title }}</p>
      <p v-for="r in tip.rows" :key="r.label" class="flex items-center gap-1.5 text-stone-600">
        <span class="inline-block h-0.5 w-3 rounded" :style="{ background: r.color }" />
        <b class="text-stone-900">{{ format(r.value) }}</b> {{ r.label }}
      </p>
      <p v-if="!tip.rows.length" class="text-stone-500">Sin datos</p>
    </div>
  </div>
</template>
