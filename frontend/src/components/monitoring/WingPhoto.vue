<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref } from 'vue'

/**
 * A photo shown on the butterfly's wings: the wing detector's box (fractions of the photo, from the identifier's
 * reading) with some padding, scaled to fit the frame; the rest of the frame shows the photo around it. Without a
 * box, the whole photo covers the frame as before.
 */
const props = withDefaults(defineProps<{ src: string; box?: number[] | null; pad?: number }>(), { box: null, pad: 0.08 })

const root = ref<HTMLElement | null>(null)
const frame = ref({ w: 0, h: 0 })
const natural = ref({ w: 0, h: 0 })
let observer: ResizeObserver | null = null
onMounted(() => {
  if (!root.value || typeof ResizeObserver === 'undefined') return
  observer = new ResizeObserver(([e]) => (frame.value = { w: e.contentRect.width, h: e.contentRect.height }))
  observer.observe(root.value)
})
onBeforeUnmount(() => observer?.disconnect())
function loaded(event: Event) {
  const img = event.target as HTMLImageElement
  natural.value = { w: img.naturalWidth, h: img.naturalHeight }
}

const placed = computed(() => {
  const b = props.box
  const { w: W, h: H } = frame.value
  const { w: nw, h: nh } = natural.value
  if (!b || !W || !H || !nw || !nh) return null
  const bw = (b[2] - b[0]) * nw
  const bh = (b[3] - b[1]) * nh
  const x0 = Math.max(0, b[0] * nw - props.pad * bw)
  const x1 = Math.min(nw, b[2] * nw + props.pad * bw)
  const y0 = Math.max(0, b[1] * nh - props.pad * bh)
  const y1 = Math.min(nh, b[3] * nh + props.pad * bh)
  // The whole padded box in the frame, and never less than the photo covering it.
  const s = Math.max(Math.min(W / (x1 - x0), H / (y1 - y0)), W / nw, H / nh)
  const left = Math.min(0, Math.max(W - nw * s, W / 2 - ((x0 + x1) / 2) * s))
  const top = Math.min(0, Math.max(H - nh * s, H / 2 - ((y0 + y1) / 2) * s))
  return { width: `${nw * s}px`, height: `${nh * s}px`, left: `${left}px`, top: `${top}px`, maxWidth: 'none' }
})
</script>

<template>
  <div ref="root" class="relative w-full overflow-hidden">
    <img
      :src="src"
      alt=""
      loading="lazy"
      :class="placed ? 'absolute' : 'h-full w-full object-cover'"
      :style="placed ?? undefined"
      @load="loaded"
    />
  </div>
</template>
