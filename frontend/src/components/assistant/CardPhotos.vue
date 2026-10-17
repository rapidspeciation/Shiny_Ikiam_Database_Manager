<script setup lang="ts">
import { defineAsyncComponent, ref, watch } from 'vue'

/** Pan and zoom load apart, as in «Revisar con la foto». */
const PhotoViewer = defineAsyncComponent(() => import('./PhotoViewer.vue'))

/**
 * The photos of the rows picked in a photo card's table (a Photo_links reading, a plan of changes to
 * the files), beside it: to look at an envelope, a wing or which way up a photo is without leaving
 * the card. One row picked: its photo; several: their thumbnails above, the one added last shown.
 * The server serves a card's own files even before Photo_links lists them (?card=).
 */
const props = defineProps<{
  /** The card's proposal. */
  card: string
  photos: { file: string; name: string; note?: string }[]
}>()

const photo = ref(0)
watch(
  () => props.photos.length,
  (n, before) => (photo.value = n > (before ?? 0) ? n - 1 : Math.min(photo.value, Math.max(0, n - 1))),
)
const url = (n: number, size: 'thumb' | 'view') =>
  `api/photo/${encodeURIComponent(props.photos[n]?.file ?? '')}?w=${size === 'thumb' ? 400 : 1600}&card=${props.card}`
</script>

<template>
  <div class="flex min-h-0 flex-col overflow-hidden rounded border border-stone-200 bg-stone-900" data-card-photos>
    <PhotoViewer
      v-if="photos.length"
      v-model:photo="photo"
      :url="url"
      :count="photos.length"
      whole
      :alt="n => photos[n]?.name ?? ''"
      :notes="photos.map(p => [p.name, p.note].filter(Boolean).join(' · '))"
      class="min-h-0 flex-1"
    />
    <p v-else class="m-auto px-4 text-center text-xs text-stone-300">
      {{ $t('Haz clic en una fila para ver su foto aquí; Ctrl o Mayús + clic para ver varias.') }}
    </p>
  </div>
</template>
