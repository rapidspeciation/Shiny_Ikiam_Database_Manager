<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref } from 'vue'

/**
 * A photo cut to the butterfly's wings, as /gphotos-sexed/ shows them: the wing box (fractions of the photo: the
 * identifier's for a field photo, the photo readings' union of the four wings for a project photo) with some padding,
 * scaled to fit the frame and centred; nothing outside it is drawn (no label, no colour chart), the frame's own
 * background fills the rest. Without a box, the whole photo covers the frame as before.
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
  // The whole padded box, as large as the frame allows: a window of its size, centred, the photo moved under it.
  const s = Math.min(W / (x1 - x0), H / (y1 - y0))
  const cw = (x1 - x0) * s
  const ch = (y1 - y0) * s
  return {
    window: { width: `${cw}px`, height: `${ch}px`, left: `${(W - cw) / 2}px`, top: `${(H - ch) / 2}px` },
    image: { width: `${nw * s}px`, height: `${nh * s}px`, left: `${-x0 * s}px`, top: `${-y0 * s}px`, maxWidth: 'none' },
  }
})
</script>

<template>
  <div ref="root" class="relative w-full overflow-hidden">
    <div v-if="placed" class="absolute overflow-hidden" :style="placed.window">
      <img :src="src" alt="" class="absolute" :style="placed.image" @load="loaded" />
    </div>
    <img v-else :src="src" alt="" loading="lazy" class="h-full w-full object-cover" @load="loaded" />
  </div>
</template>
