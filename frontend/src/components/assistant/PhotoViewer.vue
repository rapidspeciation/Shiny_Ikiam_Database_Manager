<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import Panzoom, { type PanzoomObject } from '@panzoom/panzoom'
import { ExternalLink, Maximize, RotateCcw, RotateCw, ZoomIn, ZoomOut } from 'lucide-vue-next'
import { t } from '../../lib/i18n'

/**
 * A notebook photo to read beside its proposal (Revisar con la foto): zoomed
 * with the wheel, a pinch or the buttons, moved by dragging (a mouse or a
 * finger), turned a quarter at a time, and set back to the whole photo. The
 * proposal's other photos are its thumbnails above; the row selected in the
 * table brings its photo and says its line. Pan and zoom are @panzoom/panzoom,
 * as in the team's wings gallery. Also the clutches' photos (Clutches tab),
 * with their own names for the photos (`alt`).
 */
const props = defineProps<{
  /** The photos' addresses, by size. */
  url: (n: number, size: 'thumb' | 'view') => string
  count: number
  /** The photo shown (v-model:photo). */
  photo: number
  /** The line of the row selected in the table, on the photo shown. */
  line?: number | null
  /** What each photo is called (its alt text): «Foto n del cuaderno» by default. */
  alt?: (n: number) => string
}>()
const altOf = (n: number) => (props.alt ? props.alt(n) : t('Foto {n} del cuaderno', { n: n + 1 }))
const emit = defineEmits<{ 'update:photo': [n: number] }>()

const box = ref<HTMLDivElement>()
const stage = ref<HTMLDivElement>()
const img = ref<HTMLImageElement>()
/** Quarter turns, clockwise, per photo (kept while the review is open). */
const turns = ref<Record<number, number>>({})
const turn = computed(() => (turns.value[props.photo] ?? 0) % 4)
const natural = ref({ w: 0, h: 0 })
const size = ref({ w: 0, h: 0 })
const failed = ref(false)
const scale = ref(1)
let pz: PanzoomObject | null = null
/** The photo shown (when there are several) and the selected row's line on it. */
const where = computed(() =>
  [props.count > 1 ? t('Foto {n}', { n: props.photo + 1 }) : '', props.line ? t('Línea {n}', { n: props.line }) : '']
    .filter(Boolean)
    .join(' · '),
)

/**
 * The photo fitted whole in the pane, turned, centred in the stage. The stage
 * fills the pane: panzoom finds the point under the mouse as if its element
 * started at the pane's corner, so a smaller, centred stage zoomed off to one side.
 */
const fit = computed(() => {
  const { w, h } = natural.value
  if (!w || !h || !size.value.w || !size.value.h) return null
  const sideways = turn.value % 2 === 1
  const [bw, bh] = sideways ? [h, w] : [w, h]
  const s = Math.min(size.value.w / bw, size.value.h / bh)
  return { width: `${w * s}px`, height: `${h * s}px` }
})

function reset(animate = true) {
  pz?.reset({ animate })
}
const zoomIn = () => pz?.zoomIn()
const zoomOut = () => pz?.zoomOut()
function rotate(by: 1 | -1) {
  turns.value = { ...turns.value, [props.photo]: (turn.value + by + 4) % 4 }
  void nextTick(() => reset(false))
}
const MIN_SCALE = 0.5
const MAX_SCALE = 12
/**
 * Wheel or touchpad: zoom by how far it scrolled, towards the mouse. Panzoom's
 * own wheel zoom takes a whole step per event, and a smooth wheel or a touchpad
 * sends many small ones, so a little scroll zoomed a lot. A mouse notch (about
 * 100 px) is about 1.2 times; one event never more than 1.5 times.
 */
const onWheel = (e: WheelEvent) => {
  e.preventDefault()
  if (!pz) return
  const unit = e.deltaMode === 1 ? 16 : e.deltaMode === 2 ? (box.value?.clientHeight ?? 800) : 1
  const delta = (e.deltaY === 0 && e.deltaX ? e.deltaX : e.deltaY) * unit
  const factor = Math.exp(Math.max(-200, Math.min(200, -delta)) * 0.002)
  const to = Math.max(MIN_SCALE, Math.min(MAX_SCALE, pz.getScale() * factor))
  if (to !== pz.getScale()) pz.zoomToPoint(to, e, { animate: false })
}
/** Double-click: closer, towards the point clicked. */
const onDblclick = (e: MouseEvent) => pz?.zoomToPoint(Math.min(MAX_SCALE, pz.getScale() * 1.5), e, { animate: true })
function onLoad() {
  failed.value = false
  if (img.value) natural.value = { w: img.value.naturalWidth, h: img.value.naturalHeight }
  void nextTick(() => reset(false))
}
watch(
  () => props.photo,
  () => {
    natural.value = { w: 0, h: 0 }
    failed.value = false
    reset(false)
  },
)

let watcher: ResizeObserver | null = null
onMounted(() => {
  if (!stage.value || !box.value) return
  pz = Panzoom(stage.value, {
    maxScale: MAX_SCALE,
    minScale: MIN_SCALE,
    step: 0.4,
    canvas: true,
    cursor: 'grab',
    touchAction: 'none',
  })
  stage.value.addEventListener('panzoomchange', e => (scale.value = (e as CustomEvent<{ scale: number }>).detail.scale))
  box.value.addEventListener('wheel', onWheel, { passive: false })
  watcher = new ResizeObserver(([entry]) => (size.value = { w: entry.contentRect.width, h: entry.contentRect.height }))
  watcher.observe(box.value)
})
onBeforeUnmount(() => {
  box.value?.removeEventListener('wheel', onWheel)
  watcher?.disconnect()
  pz?.destroy()
})
</script>

<template>
  <div class="flex h-full min-h-0 flex-col bg-stone-800 text-stone-100">
    <div class="flex flex-wrap items-center gap-1 border-b border-stone-700 bg-stone-900 px-2 py-1 text-xs">
      <!-- The proposal's photos: the one shown outlined. -->
      <div v-if="count > 1" class="flex items-center gap-1 overflow-x-auto">
        <button
          v-for="n in count"
          :key="n"
          type="button"
          class="shrink-0 rounded border-2 bg-white"
          :class="n - 1 === photo ? 'border-emerald-400' : 'border-transparent opacity-70 hover:opacity-100'"
          :title="altOf(n - 1)"
          :aria-pressed="n - 1 === photo"
          @click="emit('update:photo', n - 1)"
        >
          <img :src="url(n - 1, 'thumb')" :alt="altOf(n - 1)" class="h-9 w-auto max-w-16 object-contain" />
        </button>
      </div>
      <span class="px-1 text-stone-300">{{ where }}</span>
      <span class="ml-auto flex items-center gap-0.5">
        <button type="button" class="viewer-btn" :title="$t('Alejar')" :aria-label="$t('Alejar')" @click="zoomOut"><ZoomOut :size="15" /></button>
        <span class="w-10 text-center tabular-nums text-stone-300">{{ Math.round(scale * 100) }}%</span>
        <button type="button" class="viewer-btn" :title="$t('Acercar')" :aria-label="$t('Acercar')" @click="zoomIn"><ZoomIn :size="15" /></button>
        <button type="button" class="viewer-btn" :title="$t('Girar a la izquierda')" :aria-label="$t('Girar a la izquierda')" @click="rotate(-1)">
          <RotateCcw :size="15" />
        </button>
        <button type="button" class="viewer-btn" :title="$t('Girar a la derecha')" :aria-label="$t('Girar a la derecha')" @click="rotate(1)">
          <RotateCw :size="15" />
        </button>
        <button type="button" class="viewer-btn" :title="$t('Ver la foto entera')" :aria-label="$t('Ver la foto entera')" @click="reset()">
          <Maximize :size="15" />
        </button>
        <a
          class="viewer-btn"
          :href="url(photo, 'view')"
          target="_blank"
          rel="noopener"
          :title="$t('Abrir la foto en una pestaña nueva')"
          :aria-label="$t('Abrir la foto en una pestaña nueva')"
        >
          <ExternalLink :size="15" />
        </a>
      </span>
    </div>
    <!-- Wheel or pinch: zoom; drag: move; double-click: closer. -->
    <div ref="box" class="relative min-h-0 flex-1 overflow-hidden" @dblclick="onDblclick">
      <div ref="stage" class="relative h-full w-full">
        <img
          ref="img"
          :key="photo"
          :src="url(photo, 'view')"
          :alt="altOf(photo)"
          class="absolute top-1/2 left-1/2 max-w-none select-none"
          :style="{
            ...(fit ?? { maxWidth: '100%', maxHeight: '100%' }),
            transform: `translate(-50%, -50%) rotate(${turn * 90}deg)`,
          }"
          draggable="false"
          @load="onLoad"
          @error="failed = true"
        />
      </div>
      <p v-if="failed" class="absolute inset-x-0 top-1/3 text-center text-sm text-stone-300">
        {{ $t('No se pudo cargar la foto.') }}
      </p>
    </div>
  </div>
</template>

<style scoped>
.viewer-btn {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  min-width: 2rem;
  min-height: 2rem;
  border-radius: 0.25rem;
  color: var(--color-stone-200);
}
.viewer-btn:hover {
  background: var(--color-stone-700);
  color: white;
}
</style>
