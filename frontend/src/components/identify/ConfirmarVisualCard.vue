<script setup lang="ts">
/**
 * Phone-first visual field-ID guide card for /identificar/ (VITE_CONFIRM_VISUAL_CARDS=1).
 *
 * Shows either:
 *  - an active, full-width readable visual comparison card with a tap-to-zoom full-screen
 *    pinch/pan lightbox (PhotoViewer) and a region switcher pill when multiple regional forms
 *    exist (e.g. Heliconius erato/melpomene Occidente vs Oriente · Napo y Amazonía); when the side of the
 *    Andes is not known and the guide is for one side only, a line at its top says so, with the side to choose, or
 *  - a crisp, honest plain-language note when the top look-alike pair is known to be cryptic,
 *    inseparable in field photos, or lacks verified references for the selected region.
 */
import { computed, ref, watch } from 'vue'
import { Info, ZoomIn } from 'lucide-vue-next'
import PhotoViewer, { type ViewerPhoto } from './PhotoViewer.vue'
import { locale, t } from '../../lib/i18n'
import type { ConfirmarCardMatch, SideAndes } from '../../lib/confirmarVisualRouter'
import { usePhoneWidth } from '../../composables/usePhone'

// Component-scoped ES/EN UI copy so OFF builds ship zero Confirmar strings in the shared i18n bundle.
const COPY = {
  es: {
    ariaCard: 'Guía visual para distinguir especies parecidas',
    badge: 'Guía visual',
    ariaRegion: 'Elegir región para la guía visual',
    ariaZoom: 'Ampliar guía visual',
    zoom: 'Ampliar la guía',
    loading: 'Cargando guía…',
    imgError: 'No se pudo cargar la guía visual.',
    regionUnknown: 'Región no determinada: esta guía es de',
    region: { East: 'Oriente', West: 'Occidente' },
    ariaSide: 'Lado de los Andes de la foto',
    side: { West: 'Occidente', East: 'Oriente' },
  },
  en: {
    ariaCard: 'Visual guide to tell look-alike species apart',
    badge: 'Visual guide',
    ariaRegion: 'Choose region for the visual guide',
    ariaZoom: 'Enlarge visual guide',
    zoom: 'Enlarge the guide',
    loading: 'Loading guide…',
    imgError: "Couldn't load the visual guide.",
    regionUnknown: 'Region not determined: this guide is for',
    region: { East: 'East Ecuador', West: 'West Ecuador' },
    ariaSide: "The photo's side of the Andes",
    side: { West: 'West', East: 'East' },
  },
} as const
const copy = computed(() => (locale.value === 'en' ? COPY.en : COPY.es))

const props = defineProps<{
  match: ConfirmarCardMatch
  /** Optional user photo URL so the full-screen lightbox lets them swipe between the guide and their photo. */
  userPhotoSrc?: string | null
}>()

const emit = defineEmits<{
  'select-card': [cardId: string]
  /** The person says which side of the Andes the photo is from (the page's own setting, kept for the next photos). */
  'select-side': [side: SideAndes]
}>()

/** West first, as on a map. */
const SIDES: SideAndes[] = ['West', 'East']

// A computer has the zoom action as a button beside the badges: that one is the keyboard's stop, the plate answers the mouse.
const phone = usePhoneWidth()
const lightboxOpen = ref(false)
const imgError = ref(false)
/** The plate's file arrived: until then its box (already at the plate's shape) says it is loading. */
const imgLoaded = ref(false)

// Another region or language is another file.
watch(
  () => props.match.imageUrl,
  () => {
    imgError.value = false
    imgLoaded.value = false
  },
)

/**
 * A taxon as its species (genus and epithet) and what follows (the subspecies). The species reads strongest; in a pair
 * of two forms of one species the subspecies does, since it is all that differs.
 */
const split = (taxon: string) => {
  const words = taxon.trim().split(/\s+/)
  return { species: words.slice(0, 2).join(' '), form: words.slice(2).join(' ') }
}
const pair = computed(() => {
  const a = split(props.match.taxonA)
  const b = split(props.match.taxonB)
  return { a, b, byForm: !!a.form && !!b.form && a.species === b.species }
})

const hasRegionSwitcher = computed(
  () => props.match.displayKind === 'card' && props.match.canSwitchRegion && props.match.alternateCards.length > 1,
)

/** The side is not known and the guide shown is for one side only: said at the top of the card, never left to the badge. */
const unknownSide = computed(() => (props.match.displayKind === 'card' && props.match.regionFallback ? props.match.guideSide : null))
/** The line already names the region in the badge's own words: the badge would say it twice. */
const regionSaid = computed(() => !!unknownSide.value && props.match.regionBadge === copy.value.region[unknownSide.value])

const viewerPhotos = computed<ViewerPhoto[]>(() => {
  if (!props.match.imageUrl) return []
  // The plate names both taxa itself: under it only what it is, so it gets the screen.
  const guide: ViewerPhoto = {
    src: props.match.imageUrl,
    name: `${props.match.taxonA} vs. ${props.match.taxonB}`,
    caption: copy.value.badge,
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
    :aria-label="copy.ariaCard"
    data-testid="confirmar-visual-card"
    :data-card-id="match.cardId"
    :data-pair-id="match.pairId"
  >
    <!-- The side of the Andes is not known: which side this guide is for, and the two sides to choose from (a pair
         with a guide per region has its own switch below, so only the line). -->
    <div
      v-if="unknownSide"
      class="flex flex-wrap items-center gap-x-3 gap-y-2 border-b border-pending-edge/60 bg-cell-pending px-3.5 py-2"
      data-testid="confirmar-region-unknown"
    >
      <p class="flex min-w-0 flex-1 basis-56 items-start gap-2 text-sm leading-snug text-pending-text" role="status">
        <Info :size="16" class="mt-0.5 shrink-0" aria-hidden="true" />
        <span data-testid="confirmar-region-unknown-text">{{ copy.regionUnknown }} {{ copy.region[unknownSide] }}</span>
      </p>
      <div
        v-if="!hasRegionSwitcher"
        class="inline-flex shrink-0 rounded-md border border-control bg-surface"
        role="group"
        :aria-label="copy.ariaSide"
        data-testid="confirmar-side-switch"
      >
        <button
          v-for="s in SIDES"
          :key="s"
          type="button"
          aria-pressed="false"
          class="min-h-11 px-3.5 text-sm font-medium text-fg transition-colors first:rounded-l-md last:rounded-r-md hover:bg-surface-sunken active:bg-brand-soft [&+&]:border-l [&+&]:border-control"
          :data-testid="`confirmar-side-${s}`"
          @click="emit('select-side', s)"
        >
          {{ copy.side[s] }}
        </button>
      </div>
    </div>

    <div class="border-b border-border px-3.5 py-3">
      <div class="flex flex-wrap items-center gap-1.5 text-xs">
        <span class="rounded bg-brand-soft px-2 py-0.5 font-semibold text-brand-text">{{ copy.badge }}</span>
        <span
          v-if="match.regionBadge && !hasRegionSwitcher && !regionSaid"
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
        <!-- A computer: the zoom action beside the badges, so the card is no taller than its plate needs. -->
        <button
          v-if="!imgError"
          type="button"
          class="btn ml-auto px-2 py-0.5 text-xs text-brand-text max-sm:hidden"
          data-testid="confirmar-zoom-button"
          @click="openLightbox"
        >
          <ZoomIn :size="14" aria-hidden="true" />
          {{ copy.zoom }}
        </button>
      </div>

      <!-- A phone: one taxon per line, so a long subspecies name never breaks mid-name. A computer: one line. -->
      <h2 class="confirmar-pair mt-2 text-base leading-snug text-fg" :class="{ 'by-form': pair.byForm }" data-testid="confirmar-pair-title">
        <span class="max-sm:block"><i class="species">{{ pair.a.species }}</i>{{ ' ' }}<i v-if="pair.a.form" class="form">{{ pair.a.form }}</i></span>
        {{ ' ' }}
        <span class="max-sm:block"
          ><span class="text-sm text-fg-muted">vs.</span>{{ ' ' }}<i class="species">{{ pair.b.species }}</i>{{ ' ' }}<i v-if="pair.b.form" class="form">{{ pair.b.form }}</i></span
        >
      </h2>

      <div
        v-if="hasRegionSwitcher"
        class="mt-2.5 flex flex-wrap gap-1.5"
        role="group"
        :aria-label="copy.ariaRegion"
        data-testid="confirmar-region-switcher"
      >
        <button
          v-for="alt in match.alternateCards"
          :key="alt.cardId"
          type="button"
          :aria-pressed="match.cardId === alt.cardId"
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

    <!-- The plate and, under it, what a tap does: nothing lies over the plate's own text. Edge to edge on a phone. -->
    <button
      v-if="!imgError"
      type="button"
      class="confirmar-zoomable group block min-h-11 w-full bg-surface-sunken text-left sm:p-2.5"
      :tabindex="phone ? undefined : -1"
      :aria-label="copy.ariaZoom"
      data-testid="confirmar-zoom-trigger"
      @click="openLightbox"
    >
      <span
        class="confirmar-plate relative block overflow-hidden bg-surface-sunken sm:rounded-md sm:border sm:border-border-strong"
        :style="match.imageWidth && match.imageHeight ? { aspectRatio: `${match.imageWidth} / ${match.imageHeight}` } : undefined"
      >
        <span
          v-if="!imgLoaded"
          class="absolute inset-0 grid place-items-center text-sm text-fg-muted"
          role="status"
          data-testid="confirmar-img-loading"
        >
          {{ copy.loading }}
        </span>
        <img
          :key="match.imageUrl"
          :src="match.imageUrl"
          :alt="match.altText ?? ''"
          :width="match.imageWidth ?? undefined"
          :height="match.imageHeight ?? undefined"
          loading="lazy"
          decoding="async"
          class="relative block h-auto w-full"
          :class="{ 'opacity-0': !imgLoaded }"
          data-testid="confirmar-card-image"
          @load="imgLoaded = true"
          @error="imgError = true"
        />
      </span>
      <span
        class="confirmar-zoom-row flex min-h-11 items-center justify-center gap-1.5 px-3.5 text-sm font-medium text-brand-text sm:hidden"
        data-testid="confirmar-zoom-hint"
      >
        <ZoomIn :size="16" aria-hidden="true" />
        {{ copy.zoom }}
      </span>
    </button>
    <p v-else class="m-2 rounded-md border border-border bg-surface p-3 text-sm text-fg-muted" role="status" data-testid="confirmar-img-error">
      {{ copy.imgError }}
    </p>

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
        <!-- Its region as the card's badges are: a fill, no outline (it is not a control). -->
        <p
          v-if="match.regionBadge"
          class="mb-1 inline-block rounded bg-pending-edge/15 px-2 py-0.5 text-xs font-medium text-pending-text"
          data-testid="confirmar-note-region"
        >
          {{ match.regionBadge }}
        </p>
        <p class="confirmar-pair text-[0.9375rem] leading-snug text-pending-text" :class="{ 'by-form': pair.byForm }">
          <span class="block"><i class="species">{{ pair.a.species }}</i>{{ ' ' }}<i v-if="pair.a.form" class="form">{{ pair.a.form }}</i></span>
          <span class="block"
            ><span class="text-sm">vs.</span>{{ ' ' }}<i class="species">{{ pair.b.species }}</i>{{ ' ' }}<i v-if="pair.b.form" class="form">{{ pair.b.form }}</i></span
          >
        </p>
        <p class="mt-1.5 leading-snug text-fg" data-testid="confirmar-honest-note-text">{{ match.honestNote }}</p>
      </div>
    </div>
  </section>
</template>

<style scoped>
/* The pair's names: the species strongest, its subspecies lighter; two forms of one species, the other way round. */
.confirmar-pair .species {
  font-weight: 600;
}
.confirmar-pair .form {
  font-weight: 400;
}
.confirmar-pair.by-form .species {
  font-weight: 400;
}
.confirmar-pair.by-form .form {
  font-weight: 600;
}
.confirmar-zoomable {
  cursor: zoom-in;
}
.confirmar-zoomable img {
  transition:
    transform 150ms ease,
    opacity 150ms ease;
}
@media (hover: hover) {
  .confirmar-zoomable:hover img {
    transform: scale(1.03);
  }
  .confirmar-zoomable:hover .confirmar-zoom-row {
    text-decoration: underline;
    text-underline-offset: 3px;
  }
}
/* A press answers at once, on a phone too. */
.confirmar-zoomable:active .confirmar-zoom-row {
  background: var(--brand-soft);
}
.confirmar-zoomable:focus-visible {
  outline: 2px solid var(--focus);
  outline-offset: -2px;
}
@media (prefers-reduced-motion: reduce) {
  .confirmar-zoomable img {
    transition: none;
    transform: none;
  }
}
</style>
