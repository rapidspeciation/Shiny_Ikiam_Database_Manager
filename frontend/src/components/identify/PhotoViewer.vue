<script setup lang="ts">
// A photo full screen (the one viewer for Identificar and Buscar): the photo as large as the screen allows, the name
// large under it (over the photo's lower edge when held sideways), the credit and licence small. Zoom to see the
// wings' detail: pinch, double tap (double click), the mouse wheel or the + and − buttons (keys + − 0); drag to move
// a zoomed photo (lib/zoomPan.ts). Swipe, the arrows or ← → for the next photo (at full size). Esc or the × close it;
// the phone's Back too: Buscar opens it as a history entry of its own (?foto=), Identificar passes `back` and the
// viewer adds one. The page behind does not scroll while it is open, and is inert: Tab stays in the viewer, and
// closing it gives the focus back to what opened it. A photo's `fallback` loads if its larger size does not.
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { ChevronLeft, ChevronRight, Minus, Plus, X } from 'lucide-vue-next'
import { licenseUrl } from '../../lib/identifyRefs'
import { countryName } from '../../lib/usePacks'
import { IDENTITY, panBy, pinch, toggleAt, wheel, zoomAt, type View } from '../../lib/zoomPan'

export interface ViewerPhoto {
  src: string
  /** The address to load if `src` fails (the page's own size of an iNaturalist photo). */
  fallback?: string
  name: string
  /** Shown instead of the italic name (e.g. «Su foto»): a plain caption. */
  caption?: string
  /** The rank's word, shown small above the name (e.g. «Subespecie»). */
  kicker?: string
  credit: string
  license: string
  source: string | null
  /** Where it was taken (ISO2), when known. */
  country?: string | null
  /** A preserved specimen: «Preserved specimen» and its ID before the credit. */
  preserved?: string | null
}
const props = defineProps<{ photos: ViewerPhoto[]; start: number; back?: boolean }>()
const emit = defineEmits<{ close: [] }>()
const at = ref(Math.min(Math.max(0, props.start), props.photos.length - 1))
const photo = computed(() => props.photos[at.value])
const view = ref<View>(IDENTITY)
const zoomed = computed(() => view.value.s > 1.01)
const step = (d: number) => {
  if (props.photos.length < 2) return
  at.value = (at.value + d + props.photos.length) % props.photos.length
}
watch(at, () => (view.value = IDENTITY))
const closeBtn = ref<HTMLButtonElement | null>(null)
const frame = ref<HTMLElement | null>(null)
const root = ref<HTMLElement | null>(null)
/** Photos whose larger size did not load: their fallback is shown. */
const failedSrc = ref(new Set<string>())
const srcOf = (p: ViewerPhoto) => (failedSrc.value.has(p.src) && p.fallback ? p.fallback : p.src)
const failed = (p: ViewerPhoto) => p.fallback && !failedSrc.value.has(p.src) && (failedSrc.value = new Set([...failedSrc.value, p.src]))
/** What had the focus when the viewer opened (the photo tapped): it gets it back on close. */
const opener = (typeof document !== 'undefined' ? document.activeElement : null) as HTMLElement | null
const size = () => {
  const r = frame.value?.getBoundingClientRect()
  return { w: r?.width || innerWidth, h: r?.height || innerHeight, left: r?.left ?? 0, top: r?.top ?? 0 }
}

// Back closes: with `back`, one history entry while open; closing by × or Esc takes it back.
let pushed = false
let closing = false
function close() {
  if (closing) return
  closing = true
  if (pushed) {
    pushed = false
    history.back() // popstate → emit
    return
  }
  emit('close')
}
function onPop() {
  if (!pushed && !closing) return
  pushed = false
  emit('close')
}

function zoomBy(k: number) {
  const { w, h } = size()
  // zooming out ends at the whole photo (no 1.1× left that would stop the swipe)
  view.value = k < 1 && view.value.s * k < 1.2 ? IDENTITY : zoomAt(view.value, view.value.s * k, w / 2, h / 2, w, h)
}
/** Tab and Shift+Tab go round the viewer's own controls. */
function trapTab(e: KeyboardEvent) {
  const els = [...(root.value?.querySelectorAll<HTMLElement>('button:not([disabled]), a[href]') ?? [])].filter(el => el.getClientRects().length)
  if (!els.length) return
  const i = els.indexOf(document.activeElement as HTMLElement)
  e.preventDefault()
  els[(i < 0 ? 0 : i + (e.shiftKey ? -1 : 1) + els.length) % els.length].focus()
}
function onKey(e: KeyboardEvent) {
  if (e.key === 'Tab') trapTab(e)
  else if (e.key === 'Escape') close()
  else if (e.key === 'ArrowRight' && !zoomed.value) step(1)
  else if (e.key === 'ArrowLeft' && !zoomed.value) step(-1)
  else if (e.key === '+' || e.key === '=') zoomBy(1.5)
  else if (e.key === '-') zoomBy(1 / 1.5)
  else if (e.key === '0') view.value = IDENTITY
}

// Pointers: one finger pans a zoomed photo or swipes; two pinch; two taps close together zoom in or out.
const pointers = new Map<number, { x: number; y: number }>()
let gesture: { kind: 'one'; x0: number; y0: number; v0: View; moved: boolean } | { kind: 'two'; d0: number; v0: View } | null = null
let lastTap: { t: number; x: number; y: number } | null = null
const dist = () => {
  const [a, b] = [...pointers.values()]
  return Math.hypot(a.x - b.x, a.y - b.y)
}
const centre = () => {
  const [a, b] = [...pointers.values()]
  return { x: (a.x + b.x) / 2, y: (a.y + b.y) / 2 }
}
function down(e: PointerEvent) {
  if ((e.target as HTMLElement).closest('button')) return
  frame.value?.setPointerCapture?.(e.pointerId)
  pointers.set(e.pointerId, { x: e.clientX, y: e.clientY })
  gesture = pointers.size === 2 ? { kind: 'two', d0: dist(), v0: view.value } : { kind: 'one', x0: e.clientX, y0: e.clientY, v0: view.value, moved: false }
}
function move(e: PointerEvent) {
  if (!pointers.has(e.pointerId) || !gesture) return
  pointers.set(e.pointerId, { x: e.clientX, y: e.clientY })
  const { w, h, left, top } = size()
  if (gesture.kind === 'two' && pointers.size === 2) {
    const c = centre()
    view.value = pinch(gesture.v0, gesture.d0, dist(), c.x - left, c.y - top, w, h)
  } else if (gesture.kind === 'one') {
    const dx = e.clientX - gesture.x0
    const dy = e.clientY - gesture.y0
    if (Math.hypot(dx, dy) > 8) gesture.moved = true
    if (gesture.v0.s > 1.01) view.value = panBy(gesture.v0, dx, dy, w, h)
  }
}
function up(e: PointerEvent) {
  if (!pointers.has(e.pointerId)) return
  pointers.delete(e.pointerId)
  const g = gesture
  if (g?.kind === 'two') {
    // one finger left after a pinch: it pans from here, never swipes
    const [rest] = [...pointers.values()]
    gesture = rest ? { kind: 'one', x0: rest.x, y0: rest.y, v0: view.value, moved: true } : null
    return
  }
  gesture = null
  if (!g) return
  const dx = e.clientX - g.x0
  if (!g.moved) {
    const { w, h, left, top } = size()
    const now = performance.now()
    if (lastTap && now - lastTap.t < 320 && Math.hypot(e.clientX - lastTap.x, e.clientY - lastTap.y) < 30) {
      view.value = toggleAt(view.value, e.clientX - left, e.clientY - top, w, h)
      lastTap = null
    } else lastTap = { t: now, x: e.clientX, y: e.clientY }
    return
  }
  if (g.v0.s <= 1.01 && Math.abs(dx) > 40 && Math.abs(dx) > Math.abs(e.clientY - g.y0)) step(dx < 0 ? 1 : -1)
}
function cancel(e: PointerEvent) {
  pointers.delete(e.pointerId)
  gesture = null
}
function onWheel(e: WheelEvent) {
  e.preventDefault()
  const { w, h, left, top } = size()
  view.value = wheel(view.value, e.deltaY, e.clientX - left, e.clientY - top, w, h)
}

let overflow = ''
let overscroll = ''
/** The page's parts made inert while the viewer is open (those that were not already). */
const madeInert: Element[] = []
onMounted(() => {
  addEventListener('keydown', onKey)
  for (const el of [...document.body.children]) {
    if (el === root.value || el.contains(root.value) || el.hasAttribute('inert') || el.tagName === 'SCRIPT') continue
    el.setAttribute('inert', '')
    madeInert.push(el)
  }
  overflow = document.body.style.overflow
  overscroll = document.documentElement.style.overscrollBehavior
  document.body.style.overflow = 'hidden'
  document.documentElement.style.overscrollBehavior = 'none'
  if (props.back) {
    history.pushState({ ...(history.state ?? {}), photoViewer: true }, '')
    pushed = true
    addEventListener('popstate', onPop)
  }
  closeBtn.value?.focus()
})
onBeforeUnmount(() => {
  removeEventListener('keydown', onKey)
  for (const el of madeInert.splice(0)) el.removeAttribute('inert')
  if (opener?.isConnected) opener.focus({ preventScroll: true })
  removeEventListener('popstate', onPop)
  document.body.style.overflow = overflow
  document.documentElement.style.overscrollBehavior = overscroll
  // closed from outside (the page changed): its history entry goes too
  if (pushed) {
    pushed = false
    history.back()
  }
})
const genusOf = (name: string) => name.split(' ')[0]
const restOf = (name: string) => name.split(' ').slice(1).join(' ')
</script>

<template>
  <Teleport to="body">
    <div ref="root" class="viewer fixed inset-0 z-[60] flex flex-col bg-black text-white" role="dialog" aria-modal="true" :aria-label="photo?.caption ?? photo?.name" data-testid="photo-viewer">
      <div class="viewer-top flex items-center justify-between gap-2 p-2">
        <span class="px-2 text-sm text-white/80 tabular-nums">{{ photos.length > 1 ? `${at + 1} / ${photos.length}` : '' }}</span>
        <button ref="closeBtn" class="grid size-12 place-items-center rounded-full bg-white/15 hover:bg-white/25 focus-visible:outline-2 focus-visible:outline-white" :aria-label="$t('Cerrar')" data-testid="viewer-close" @click="close">
          <X :size="26" />
        </button>
      </div>
      <div
        ref="frame"
        class="viewer-photo relative min-h-0 flex-1 overflow-hidden select-none"
        :class="zoomed ? 'cursor-grab' : 'cursor-zoom-in'"
        data-testid="viewer-frame"
        @pointerdown="down"
        @pointermove="move"
        @pointerup="up"
        @pointercancel="cancel"
        @wheel="onWheel"
      >
        <div class="absolute inset-0 origin-top-left will-change-transform" :style="{ transform: `translate(${view.x}px, ${view.y}px) scale(${view.s})` }" data-testid="viewer-zoom" :data-scale="view.s.toFixed(2)">
          <img v-if="photo" :key="photo.src" :src="srcOf(photo)" alt="" class="absolute inset-0 size-full object-contain" draggable="false" @error="failed(photo)" />
        </div>
        <template v-if="photos.length > 1 && !zoomed">
          <button class="absolute top-1/2 left-2 grid size-12 -translate-y-1/2 place-items-center rounded-full bg-black/55 hover:bg-black/75" :aria-label="$t('Foto anterior')" @click="step(-1)">
            <ChevronLeft :size="28" />
          </button>
          <button class="absolute top-1/2 right-2 grid size-12 -translate-y-1/2 place-items-center rounded-full bg-black/55 hover:bg-black/75" :aria-label="$t('Foto siguiente')" @click="step(1)">
            <ChevronRight :size="28" />
          </button>
        </template>
        <div class="absolute right-2 bottom-2 flex gap-2">
          <button class="grid size-11 place-items-center rounded-full bg-black/55 hover:bg-black/75" :aria-label="$t('Alejar')" :disabled="!zoomed" data-testid="viewer-zoom-out" @click="zoomBy(1 / 1.5)">
            <Minus :size="22" />
          </button>
          <button class="grid size-11 place-items-center rounded-full bg-black/55 hover:bg-black/75" :aria-label="$t('Acercar')" data-testid="viewer-zoom-in" @click="zoomBy(1.5)">
            <Plus :size="22" />
          </button>
        </div>
      </div>
      <div v-if="photo" class="viewer-name px-4 pt-3 pb-[max(1rem,env(safe-area-inset-bottom))]">
        <p v-if="photo.kicker" class="text-sm font-medium tracking-wide text-white/75 uppercase">{{ photo.kicker }}</p>
        <p v-if="photo.caption" class="text-[1.6rem] leading-tight font-semibold" data-testid="viewer-name">{{ photo.caption }}</p>
        <p v-else class="text-[2rem] leading-tight font-semibold break-words" data-testid="viewer-name">
          <i>{{ genusOf(photo.name) }}</i> <i>{{ restOf(photo.name) }}</i>
        </p>
        <p v-if="photo.credit || photo.license || photo.country || photo.preserved != null" class="mt-1 text-sm text-white/75">
          <template v-if="photo.preserved != null">{{ $t('Ejemplar preservado') }}{{ photo.preserved ? ` ${photo.preserved}` : '' }} · </template>
          <a v-if="photo.source" :href="photo.source" target="_blank" rel="noopener" class="underline-offset-2 hover:underline">{{ photo.credit }}</a
          ><template v-else>{{ photo.preserved != null ? $t('Foto: {who}', { who: photo.credit }) : photo.credit }}</template
          ><template v-if="photo.license">
            ·
            <a v-if="licenseUrl(photo.license)" :href="licenseUrl(photo.license)!" target="_blank" rel="noopener license" class="underline-offset-2 hover:underline">{{ photo.license }}</a
            ><template v-else>{{ photo.license }}</template></template
          ><template v-if="photo.country"> · {{ countryName(photo.country) }}</template>
        </p>
      </div>
    </div>
  </Teleport>
</template>

<style scoped>
/* Every gesture on the photo is the viewer's own (pinch, pan, swipe, double tap): the browser zooms nothing. */
.viewer-photo {
  touch-action: none;
  overscroll-behavior: contain;
}
/* Held sideways on a phone: the photo takes the whole height, the bar and the name lie over its edges. */
@media (orientation: landscape) and (max-height: 540px) {
  .viewer-photo {
    position: absolute;
    inset: 0;
  }
  .viewer-top {
    position: absolute;
    inset: 0 0 auto 0;
    z-index: 1;
  }
  .viewer-name {
    position: absolute;
    inset: auto 0 0 0;
    padding-top: 2.5rem;
    background: linear-gradient(to top, rgb(0 0 0 / 0.85), rgb(0 0 0 / 0.55) 60%, transparent);
    pointer-events: none;
  }
  .viewer-name a {
    pointer-events: auto;
  }
  .viewer-name [data-testid='viewer-name'] {
    font-size: 1.6rem;
  }
}
</style>
