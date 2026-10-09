<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref } from 'vue'
import { ChevronLeft, ChevronRight, ExternalLink, X } from 'lucide-vue-next'
import { photoUrl } from '../../lib/review'
import { t } from '../../lib/i18n'
import type { SpeciesPhotos } from '../../lib/speciesPhotos'

/**
 * A species' photos over the page: iNaturalist's (with the attribution their
 * licence asks for) and the team's specimens (dorsal and ventral). Esc closes,
 * arrows step.
 */
const props = defineProps<{ name: string; photos: SpeciesPhotos; start?: number }>()
const emit = defineEmits<{ close: [] }>()

interface Item {
  thumb: string
  src: string
  caption: string
  link?: string
  specimen: boolean
}
const items = computed<Item[]>(() => [
  ...(props.photos.inat?.photos ?? []).map(p => ({
    thumb: p.thumb,
    src: p.url,
    caption: p.attribution,
    link: p.observation ?? props.photos.inat!.taxon.url,
    specimen: false,
  })),
  ...props.photos.specimens.flatMap(s =>
    [s.dorsal, s.ventral].filter((id): id is string => !!id).map((id, i) => ({
      thumb: photoUrl(id),
      src: photoUrl(id, 1600),
      caption: [s.cam, i ? t('ventral') : t('dorsal'), s.sex === 'female' ? '♀' : s.sex === 'male' ? '♂' : '']
        .filter(Boolean)
        .join(' · '),
      specimen: true,
    })),
  ),
])
const index = ref(props.start ?? 0)
const current = computed(() => items.value[index.value])
const step = (by: number) => (index.value = (index.value + by + items.value.length) % items.value.length)
const inat = computed(() => props.photos.inat)
/** When iNaturalist had no photos of the name itself. */
const broader = computed(() => {
  const level = inat.value?.level
  const asked = props.name.split(' ').length
  if (level === 'genus' && asked > 1) return t('Fotos del género (iNaturalist no tiene de la especie)')
  if (level === 'species' && asked > 2) return t('Fotos de la especie (iNaturalist no tiene de la subespecie)')
  return ''
})

function onKey(event: KeyboardEvent) {
  if (event.key === 'Escape') emit('close')
  else if (event.key === 'ArrowRight') step(1)
  else if (event.key === 'ArrowLeft') step(-1)
}
onMounted(() => window.addEventListener('keydown', onKey))
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
</script>

<template>
  <Teleport to="body">
    <div
      class="fixed inset-0 z-[2000] flex flex-col bg-black/95 text-white"
      role="dialog"
      aria-modal="true"
      @click.self="emit('close')"
    >
      <div class="flex items-start gap-3 px-4 py-2 text-sm">
        <div class="min-w-0">
          <p>
            <i class="font-medium">{{ name }}</i>
            <span v-if="inat?.taxon.common" class="text-white/70"> · {{ inat.taxon.common }}</span>
          </p>
          <p v-if="inat?.synonymOf" class="text-xs text-white/70">
            {{ $t('En iNaturalist: {name}', { name: inat.synonymOf }) }}
          </p>
          <p v-if="broader" class="text-xs text-amber-200">{{ broader }}</p>
        </div>
        <a
          v-if="inat"
          :href="inat.taxon.url"
          target="_blank"
          rel="noopener noreferrer"
          class="ml-auto flex shrink-0 items-center gap-1 rounded px-2 py-1 text-xs underline hover:bg-white/10"
        >
          iNaturalist <ExternalLink :size="12" />
        </a>
        <button class="shrink-0 rounded p-1 hover:bg-white/10" :class="{ 'ml-auto': !inat }" :title="$t('Cerrar (Esc)')" @click="emit('close')">
          <X :size="20" />
        </button>
      </div>

      <p v-if="!items.length" class="m-auto text-white/70">{{ $t('Sin fotos') }}</p>
      <template v-else>
        <div class="relative flex min-h-0 flex-1 items-center justify-center px-2" @click.self="emit('close')">
          <button
            v-if="items.length > 1"
            class="absolute left-2 rounded-full bg-white/10 p-2 hover:bg-white/20"
            :title="$t('Anterior')"
            @click="step(-1)"
          >
            <ChevronLeft :size="24" />
          </button>
          <img :key="current.src" :src="current.src" alt="" class="max-h-full max-w-full object-contain" />
          <button
            v-if="items.length > 1"
            class="absolute right-2 rounded-full bg-white/10 p-2 hover:bg-white/20"
            :title="$t('Siguiente')"
            @click="step(1)"
          >
            <ChevronRight :size="24" />
          </button>
        </div>
        <p class="px-4 pt-2 text-center text-xs text-white/80">
          <template v-if="current.specimen">{{ $t('Espécimen del equipo') }}: </template>
          <a v-if="current.link" :href="current.link" target="_blank" rel="noopener noreferrer" class="underline">{{
            current.caption
          }}</a>
          <template v-else>{{ current.caption }}</template>
        </p>
        <div class="flex gap-1.5 overflow-x-auto px-4 py-2">
          <button
            v-for="(item, i) in items"
            :key="item.src"
            class="size-14 shrink-0 overflow-hidden rounded ring-white"
            :class="[i === index ? 'ring-2' : 'opacity-70 hover:opacity-100', { 'ml-3': item.specimen && !items[i - 1]?.specimen && i > 0 }]"
            :title="item.specimen ? $t('Espécimen del equipo') : 'iNaturalist'"
            @click="index = i"
          >
            <img :src="item.thumb" alt="" loading="lazy" class="size-full object-cover" />
          </button>
        </div>
      </template>
    </div>
  </Teleport>
</template>
