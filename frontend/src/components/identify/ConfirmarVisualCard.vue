<script setup lang="ts">
/**
 * Phone-first visual field-ID guide card for /identificar/ (VITE_CONFIRM_VISUAL_CARDS=1).
 *
 * Shows either:
 *  - an active, full-width readable visual comparison card with a tap-to-zoom full-screen
 *    pinch/pan lightbox (PhotoViewer) and a region switcher pill when multiple regional forms
 *    exist (e.g. Heliconius erato/melpomene Occidente vs Oriente · Napo y Amazonía), or
 *  - a crisp, honest plain-language note when the top look-alike pair is known to be cryptic,
 *    inseparable in field photos, or lacks verified references for the selected region.
 */
import { computed, ref, watch } from 'vue'
import { Info, ZoomIn } from 'lucide-vue-next'
import PhotoViewer, { type ViewerPhoto } from './PhotoViewer.vue'
import { t } from '../../lib/i18n'
import type { ConfirmarCardMatch } from '../../lib/confirmarVisualRouter'

const props = defineProps<{
  match: ConfirmarCardMatch
  /** Optional user photo URL so the full-screen lightbox lets them swipe between the guide and their photo. */
  userPhotoSrc?: string | null
}>()

const emit = defineEmits<{
  'select-card': [cardId: string]
}>()

const lightboxOpen = ref(false)
const imgError = ref(false)

watch(
  () => props.match.imageUrl,
  () => {
    imgError.value = false
  },
)

const hasRegionSwitcher = computed(() => props.match.displayKind === 'card' && props.match.alternateCards.length > 1)

const viewerPhotos = computed<ViewerPhoto[]>(() => {
  if (!props.match.imageUrl) return []
  const guide: ViewerPhoto = {
    src: props.match.imageUrl,
    name: `${props.match.taxonA} vs. ${props.match.taxonB}`,
    caption: `${props.match.taxonA} vs. ${props.match.taxonB}`,
    kicker: [props.match.regionBadge, props.match.viewBadge].filter(Boolean).join(' · ') || undefined,
    credit: '',
    license: '',
    source: null,
  }
  if (!props.userPhotoSrc) return [guide]
  return [
    guide,
    {
      src: props.userPhotoSrc,
      name: '',
      caption: t('Su foto'),
      credit: '',
      license: '',
      source: null,
    },
  ]
})

function openLightbox() {
  if (!props.match.imageUrl || imgError.value) return
  lightboxOpen.value = true
}
</script>

<template>
  <!-- Active verified visual comparison card -->
  <section
    v-if="match.displayKind === 'card' && match.imageUrl"
    class="confirmar-card overflow-hidden rounded-lg border border-brand/50 bg-surface shadow-xs transition-shadow hover:shadow-sm"
    :aria-label="$t('Guía visual para distinguir especies parecidas')"
    data-testid="confirmar-visual-card"
    :data-card-id="match.cardId"
    :data-pair-id="match.pairId"
  >
    <div class="border-b border-border px-3.5 py-3">
      <div class="flex flex-wrap items-center gap-1.5 text-xs">
        <span class="rounded bg-brand-soft px-2 py-0.5 font-semibold text-brand-text">{{ $t('Guía visual') }}</span>
        <span
          v-if="match.regionBadge && !hasRegionSwitcher"
          class="rounded border border-border-strong bg-surface-sunken px-2 py-0.5 font-medium text-fg"
          data-testid="confirmar-region-badge"
        >
          {{ match.regionBadge }}
        </span>
        <span
          v-if="match.viewBadge"
          class="rounded border border-brand/40 bg-surface px-2 py-0.5 font-medium text-brand-text"
          data-testid="confirmar-view-badge"
        >
          {{ match.viewBadge }}
        </span>
      </div>

      <h2 class="mt-1.5 text-base leading-snug font-semibold text-fg" data-testid="confirmar-pair-title">
        <i>{{ match.taxonA }}</i>
        <span class="mx-1 font-normal text-fg-muted">vs.</span>
        <i>{{ match.taxonB }}</i>
      </h2>

      <div
        v-if="hasRegionSwitcher"
        class="mt-2.5 flex flex-wrap gap-1.5"
        role="radiogroup"
        :aria-label="$t('Elegir región para la guía visual')"
        data-testid="confirmar-region-switcher"
      >
        <button
          v-for="alt in match.alternateCards"
          :key="alt.cardId"
          type="button"
          role="radio"
          :aria-checked="match.cardId === alt.cardId"
          class="min-h-11 rounded-md border px-3 text-sm font-medium transition-colors"
          :class="
            match.cardId === alt.cardId
              ? 'border-brand bg-brand text-on-brand shadow-xs'
              : 'border-control bg-surface text-fg hover:border-brand hover:bg-surface-sunken'
          "
          :data-testid="`confirmar-region-opt-${alt.cardId}`"
          @click="emit('select-card', alt.cardId)"
        >
          {{ alt.regionBadge }}
        </button>
      </div>
    </div>

    <div class="bg-surface-sunken p-2 sm:p-2.5">
      <button
        v-if="!imgError"
        type="button"
        class="confirmar-zoomable group relative block min-h-11 w-full overflow-hidden rounded-md border border-border-strong bg-photo-mat text-left focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-[var(--focus)]"
        :aria-label="$t('Ampliar guía visual')"
        data-testid="confirmar-zoom-trigger"
        @click="openLightbox"
      >
        <img
          :key="match.imageUrl"
          :src="match.imageUrl"
          :alt="match.altText ?? ''"
          :width="match.imageWidth ?? undefined"
          :height="match.imageHeight ?? undefined"
          loading="lazy"
          decoding="async"
          class="block h-auto w-full transition-transform duration-150"
          data-testid="confirmar-card-image"
          @error="imgError = true"
        />
        <span
          class="pointer-events-none absolute right-2 bottom-2 inline-flex items-center gap-1.5 rounded-full bg-black/70 px-2.5 py-1 text-xs font-medium text-white shadow-xs"
          aria-hidden="true"
        >
          <ZoomIn :size="14" />
          {{ $t('Ampliar') }}
        </span>
      </button>
      <p v-else class="rounded-md border border-border bg-surface p-3 text-sm text-fg-muted" role="status" data-testid="confirmar-img-error">
        {{ $t('No se pudo cargar la guía visual.') }}
      </p>
    </div>

    <PhotoViewer
      v-if="lightboxOpen && viewerPhotos.length"
      :photos="viewerPhotos"
      :start="0"
      back
      @close="lightboxOpen = false"
    />
  </section>

  <!-- Honest plain-language note for known cryptic / inseparable / deferred pairs -->
  <section
    v-else-if="match.displayKind === 'honest_note' && match.honestNote"
    class="rounded-lg border border-pending-edge/60 bg-cell-pending px-3.5 py-3 text-sm text-fg"
    data-testid="confirmar-honest-note"
    :data-pair-id="match.pairId"
    :data-status="match.status"
  >
    <div class="flex items-start gap-2.5">
      <Info :size="18" class="mt-0.5 shrink-0 text-pending-text" aria-hidden="true" />
      <div class="min-w-0 flex-1">
        <p class="font-semibold text-pending-text">
          <i>{{ match.taxonA }}</i>
          <span class="mx-1 font-normal">vs.</span>
          <i>{{ match.taxonB }}</i>
          <span v-if="match.regionBadge" class="ml-1.5 text-xs font-medium opacity-85">· {{ match.regionBadge }}</span>
        </p>
        <p class="mt-1 leading-snug text-fg" data-testid="confirmar-honest-note-text">{{ match.honestNote }}</p>
      </div>
    </div>
  </section>
</template>

<style scoped>
.confirmar-zoomable {
  cursor: zoom-in;
  transition: transform 120ms ease;
}
@media (hover: hover) {
  .confirmar-zoomable:hover img {
    transform: scale(1.03);
  }
}
.confirmar-zoomable:active {
  transform: scale(0.995);
}
@media (prefers-reduced-motion: reduce) {
  .confirmar-zoomable img,
  .confirmar-zoomable {
    transition: none;
    transform: none;
  }
}
</style>
