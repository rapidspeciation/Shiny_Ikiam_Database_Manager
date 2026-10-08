<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { ImageOff } from 'lucide-vue-next'
import SpeciesGallery from './SpeciesGallery.vue'
import { photoUrl } from '../../lib/review'
import { photoName, speciesPhotos } from '../../lib/speciesPhotos'

/**
 * A species' photo filling its frame (a report card): the first iNaturalist
 * photo at medium size, else a specimen's dorsal photo; a click opens them all.
 */
const props = defineProps<{ species: string; subspecies?: string }>()
const name = computed(() => photoName(props.species, props.subspecies))
const photos = computed(() => speciesPhotos(name.value))
const photo = computed(() => {
  const p = photos.value
  const live = p?.inat?.photos[0]
  if (live) return { src: live.thumb.replace(/\/small\.(\w+)$/, '/medium.$1'), specimen: false }
  return p?.specimens[0] ? { src: photoUrl(p.specimens[0].dorsal), specimen: true } : null
})
/** Still waiting for the server (or iNaturalist) to answer. */
const searching = computed(() => !!name.value && (!photos.value || photos.value.pending))
const open = ref(false)
const broken = ref(false)
watch(photo, () => (broken.value = false))
</script>

<template>
  <button
    v-if="photo && !broken"
    type="button"
    class="group block size-full overflow-hidden focus-visible:ring-2 focus-visible:ring-brand-600 focus-visible:outline-none focus-visible:ring-inset"
    :title="$t('Ver fotos de {name}', { name })"
    @click="open = true"
  >
    <img
      :src="photo.src"
      alt=""
      loading="lazy"
      decoding="async"
      class="size-full transition-transform duration-300 group-hover:scale-[1.03]"
      :class="photo.specimen ? 'object-contain' : 'object-cover'"
      @error="broken = true"
    />
  </button>
  <span
    v-else
    class="flex size-full items-center justify-center text-stone-300"
    :class="{ 'animate-pulse': searching }"
    :title="searching ? $t('Buscando fotos…') : $t('Sin fotos')"
  >
    <ImageOff v-if="!searching" :size="22" />
  </span>
  <SpeciesGallery v-if="open && photos" :name="name" :photos="photos" @close="open = false" />
</template>
