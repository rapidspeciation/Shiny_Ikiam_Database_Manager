<script setup lang="ts">
// A taxon's photo full screen, to hold the phone up and show someone else: the photo as large as the screen allows,
// the name large under it, the credit and licence small. Swipe, the arrows or ← → for the next photo; Esc, the ×
// or the back gesture's Escape to close. Used by the taxon search (TaxonSearch.vue).
import { computed, onBeforeUnmount, onMounted, ref } from 'vue'
import { ChevronLeft, ChevronRight, X } from 'lucide-vue-next'
import { licenseUrl } from '../../lib/identifyRefs'

export interface ViewerPhoto {
  src: string
  name: string
  /** The rank's word, shown small above the name (e.g. «Subespecie»). */
  kicker?: string
  credit: string
  license: string
  source: string | null
}
const props = defineProps<{ photos: ViewerPhoto[]; start: number }>()
const emit = defineEmits<{ close: [] }>()
const at = ref(Math.min(Math.max(0, props.start), props.photos.length - 1))
const photo = computed(() => props.photos[at.value])
const step = (d: number) => props.photos.length > 1 && (at.value = (at.value + d + props.photos.length) % props.photos.length)
const closeBtn = ref<HTMLButtonElement | null>(null)

function onKey(e: KeyboardEvent) {
  if (e.key === 'Escape') emit('close')
  else if (e.key === 'ArrowRight') step(1)
  else if (e.key === 'ArrowLeft') step(-1)
}
let x0: number | null = null
const down = (e: PointerEvent) => (x0 = e.clientX)
function up(e: PointerEvent) {
  if (x0 == null) return
  const dx = e.clientX - x0
  x0 = null
  if (Math.abs(dx) > 40) step(dx < 0 ? 1 : -1)
}
let overflow = ''
onMounted(() => {
  addEventListener('keydown', onKey)
  overflow = document.body.style.overflow
  document.body.style.overflow = 'hidden'
  closeBtn.value?.focus()
})
onBeforeUnmount(() => {
  removeEventListener('keydown', onKey)
  document.body.style.overflow = overflow
})
const genusOf = (name: string) => name.split(' ')[0]
const restOf = (name: string) => name.split(' ').slice(1).join(' ')
</script>

<template>
  <Teleport to="body">
    <div class="viewer fixed inset-0 z-[60] flex flex-col bg-black text-white" role="dialog" aria-modal="true" :aria-label="photo?.name" data-testid="photo-viewer">
      <div class="flex items-center justify-between gap-2 p-2">
        <span class="px-2 text-sm text-white/80 tabular-nums">{{ photos.length > 1 ? `${at + 1} / ${photos.length}` : '' }}</span>
        <button ref="closeBtn" class="grid size-12 place-items-center rounded-full bg-white/15 hover:bg-white/25 focus-visible:outline-2 focus-visible:outline-white" :aria-label="$t('Cerrar')" @click="emit('close')">
          <X :size="26" />
        </button>
      </div>
      <div class="relative min-h-0 flex-1 touch-pan-y select-none" @pointerdown="down" @pointerup="up">
        <img v-if="photo" :key="photo.src" :src="photo.src" alt="" class="absolute inset-0 size-full object-contain" draggable="false" />
        <template v-if="photos.length > 1">
          <button class="absolute top-1/2 left-2 grid size-12 -translate-y-1/2 place-items-center rounded-full bg-black/55 hover:bg-black/75" :aria-label="$t('Foto anterior')" @click="step(-1)">
            <ChevronLeft :size="28" />
          </button>
          <button class="absolute top-1/2 right-2 grid size-12 -translate-y-1/2 place-items-center rounded-full bg-black/55 hover:bg-black/75" :aria-label="$t('Foto siguiente')" @click="step(1)">
            <ChevronRight :size="28" />
          </button>
        </template>
      </div>
      <div v-if="photo" class="px-4 pt-3 pb-[max(1rem,env(safe-area-inset-bottom))]">
        <p v-if="photo.kicker" class="text-sm font-medium tracking-wide text-white/75 uppercase">{{ photo.kicker }}</p>
        <p class="text-[2rem] leading-tight font-semibold break-words" data-testid="viewer-name">
          <i>{{ genusOf(photo.name) }}</i> <i>{{ restOf(photo.name) }}</i>
        </p>
        <p class="mt-1 text-sm text-white/75">
          <a v-if="photo.source" :href="photo.source" target="_blank" rel="noopener" class="underline-offset-2 hover:underline">{{ photo.credit }}</a
          ><template v-else>{{ photo.credit }}</template
          ><template v-if="photo.license">
            ·
            <a v-if="licenseUrl(photo.license)" :href="licenseUrl(photo.license)!" target="_blank" rel="noopener license" class="underline-offset-2 hover:underline">{{ photo.license }}</a
            ><template v-else>{{ photo.license }}</template></template
          >
        </p>
      </div>
    </div>
  </Teleport>
</template>
