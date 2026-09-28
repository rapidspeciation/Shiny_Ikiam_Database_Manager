<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { Maximize, RotateCw, ZoomIn, ZoomOut } from 'lucide-vue-next'
import { bands } from '../../lib/notebook'

/**
 * The notebook photo beside the grid: zoom (wheel with Ctrl, pinch, buttons,
 * double click) and pan (drag). Each line read is a band at its height; the
 * selected line's band is marked and kept in view, and clicking a band
 * selects that line in the grid. The photo is shown upright (turned as the AI
 * said it must be read), which is the frame the bands refer to.
 */
const props = defineProps<{
  src: string
  rotate: number
  lines: { n: number; y: number | null; applied?: boolean; changes?: number }[]
  selected: number | null
}>()
const emit = defineEmits<{ select: [n: number] }>()

const box = ref<HTMLDivElement>()
const upright = ref('')
const size = ref({ w: 0, h: 0 })
const view = ref({ x: 0, y: 0, s: 1 })
const turned = ref(0)
const smooth = ref(false)
let objectUrl = ''

/** The photo turned upright on a canvas (only when it needs turning). */
async function prepare() {
  const degrees = (props.rotate + turned.value) % 360
  const image = new Image()
  image.decoding = 'async'
  image.src = props.src
  await image.decode().catch(() => {})
  if (!image.naturalWidth) return
  if (objectUrl) URL.revokeObjectURL(objectUrl)
  objectUrl = ''
  if (!degrees) {
    upright.value = props.src
    size.value = { w: image.naturalWidth, h: image.naturalHeight }
  } else {
    const side = degrees % 180 !== 0
    const canvas = document.createElement('canvas')
    canvas.width = side ? image.naturalHeight : image.naturalWidth
    canvas.height = side ? image.naturalWidth : image.naturalHeight
    const ctx = canvas.getContext('2d')!
    ctx.translate(canvas.width / 2, canvas.height / 2)
    ctx.rotate((degrees * Math.PI) / 180)
    ctx.drawImage(image, -image.naturalWidth / 2, -image.naturalHeight / 2)
    const blob = await new Promise<Blob | null>(resolve => canvas.toBlob(resolve, 'image/jpeg', 0.9))
    if (!blob) return
    objectUrl = URL.createObjectURL(blob)
    upright.value = objectUrl
    size.value = { w: canvas.width, h: canvas.height }
  }
  await nextTick()
  fit('width')
}

function fit(mode: 'width' | 'page' = 'width') {
  const el = box.value
  if (!el || !size.value.w) return
  const sw = el.clientWidth / size.value.w
  const sh = el.clientHeight / size.value.h
  const s = mode === 'page' ? Math.min(sw, sh) : sw
  view.value = { s, x: (el.clientWidth - size.value.w * s) / 2, y: 0 }
  if (props.selected !== null) show(props.selected, false)
}

function zoomAt(factor: number, cx: number, cy: number) {
  const { x, y, s } = view.value
  const next = Math.min(8, Math.max(0.1, s * factor))
  const k = next / s
  view.value = { s: next, x: cx - (cx - x) * k, y: cy - (cy - y) * k }
}
const centre = () => ({ x: (box.value?.clientWidth ?? 0) / 2, y: (box.value?.clientHeight ?? 0) / 2 })

function onWheel(e: WheelEvent) {
  e.preventDefault()
  const rect = box.value!.getBoundingClientRect()
  // Ctrl + wheel (and a trackpad pinch) zooms; the wheel alone scrolls the page up and down.
  if (e.ctrlKey || e.metaKey) zoomAt(Math.exp(-e.deltaY / 300), e.clientX - rect.left, e.clientY - rect.top)
  else view.value = { ...view.value, x: view.value.x - e.deltaX, y: view.value.y - e.deltaY }
}

// Dragging pans; two fingers pinch.
const pointers = new Map<number, { x: number; y: number }>()
let moved = 0
function onDown(e: PointerEvent) {
  box.value?.setPointerCapture(e.pointerId)
  pointers.set(e.pointerId, { x: e.clientX, y: e.clientY })
  moved = 0
  smooth.value = false
}
function onMove(e: PointerEvent) {
  const last = pointers.get(e.pointerId)
  if (!last) return
  if (pointers.size === 2) {
    const [a, b] = [...pointers.values()]
    const other = a === last ? b : a
    const before = Math.hypot(last.x - other.x, last.y - other.y)
    const after = Math.hypot(e.clientX - other.x, e.clientY - other.y)
    const rect = box.value!.getBoundingClientRect()
    if (before > 0) zoomAt(after / before, (e.clientX + other.x) / 2 - rect.left, (e.clientY + other.y) / 2 - rect.top)
  } else {
    view.value = { ...view.value, x: view.value.x + e.clientX - last.x, y: view.value.y + e.clientY - last.y }
  }
  moved += Math.abs(e.clientX - last.x) + Math.abs(e.clientY - last.y)
  pointers.set(e.pointerId, { x: e.clientX, y: e.clientY })
}
function onUp(e: PointerEvent) {
  const tap = pointers.size === 1 && moved < 6
  pointers.delete(e.pointerId)
  // A tap (not a drag) on a band selects its line. The box holds the pointer, so the band is found by height.
  if (!tap || !box.value) return
  const rect = box.value.getBoundingClientRect()
  const at = (e.clientY - rect.top - view.value.y) / (view.value.s * size.value.h)
  const hit = shownBands.value.find(b => at >= b.top && at <= b.bottom)
  if (hit) emit('select', hit.n)
}
function onDouble(e: MouseEvent) {
  const rect = box.value!.getBoundingClientRect()
  smooth.value = true
  zoomAt(2, e.clientX - rect.left, e.clientY - rect.top)
}

const shownBands = computed(() => (turned.value ? [] : bands(props.lines)))
const line = (n: number) => props.lines.find(l => l.n === n)

/** Brings a line's band into view (the middle third of the box) when it is outside it. */
function show(n: number, animate = true) {
  const band = shownBands.value.find(b => b.n === n)
  const el = box.value
  if (!band || !el) return
  const { s, y } = view.value
  const top = y + band.top * size.value.h * s
  const bottom = y + band.bottom * size.value.h * s
  if (top >= el.clientHeight * 0.2 && bottom <= el.clientHeight * 0.8) return
  smooth.value = animate
  // Centred, but no empty space above or below a photo taller than the box (a shorter one stays at the top).
  const tall = size.value.h * s - el.clientHeight
  const wanted = el.clientHeight * 0.4 - ((band.top + band.bottom) / 2) * size.value.h * s
  view.value = { ...view.value, y: tall > 0 ? Math.min(0, Math.max(-tall, wanted)) : 0 }
}

watch(() => props.selected, n => n !== null && show(n))
watch(() => [props.src, props.rotate, turned.value], prepare)
let resize: ResizeObserver | null = null
onMounted(() => {
  void prepare()
  resize = new ResizeObserver(() => fit('width'))
  if (box.value) resize.observe(box.value)
})
onBeforeUnmount(() => {
  resize?.disconnect()
  if (objectUrl) URL.revokeObjectURL(objectUrl)
})
</script>

<template>
  <div class="relative h-full min-h-0 overflow-hidden bg-stone-800 select-none">
    <div
      ref="box"
      class="absolute inset-0 cursor-grab touch-none active:cursor-grabbing"
      @wheel="onWheel"
      @pointerdown="onDown"
      @pointermove="onMove"
      @pointerup="onUp"
      @pointercancel="onUp"
      @dblclick="onDouble"
    >
      <div
        v-if="upright"
        class="absolute top-0 left-0 origin-top-left"
        :class="{ 'transition-transform duration-300': smooth }"
        :style="{
          width: `${size.w}px`,
          height: `${size.h}px`,
          transform: `translate(${view.x}px, ${view.y}px) scale(${view.s})`,
        }"
      >
        <img :src="upright" alt="Página del cuaderno" class="pointer-events-none block h-full w-full" draggable="false" />
        <div
          v-for="b in shownBands"
          :key="b.n"
          class="nb-band"
          :class="{ 'is-selected': b.n === selected, 'is-applied': line(b.n)?.applied, 'is-quiet': !line(b.n)?.changes }"
          :style="{ top: `${b.top * 100}%`, height: `${(b.bottom - b.top) * 100}%` }"
        >
          <span :style="{ fontSize: `${Math.max(11, 13 / view.s)}px` }">{{ b.n }}</span>
        </div>
      </div>
      <p v-else class="p-6 text-sm text-stone-300">Cargando la foto…</p>
    </div>
    <div class="absolute top-2 right-2 flex gap-1">
      <button class="nb-photo-btn" title="Acercar (Ctrl + rueda)" @click="zoomAt(1.4, centre().x, centre().y)"><ZoomIn :size="16" /></button>
      <button class="nb-photo-btn" title="Alejar" @click="zoomAt(1 / 1.4, centre().x, centre().y)"><ZoomOut :size="16" /></button>
      <button class="nb-photo-btn" title="Ver la página entera" @click="fit('page')"><Maximize :size="16" /></button>
      <button
        class="nb-photo-btn"
        title="Girar la foto (las bandas de las líneas solo se ven en la orientación leída)"
        @click="turned = (turned + 90) % 360"
      >
        <RotateCw :size="16" />
      </button>
    </div>
  </div>
</template>
