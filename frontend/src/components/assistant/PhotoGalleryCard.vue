<script setup lang="ts">
import { computed } from 'vue'
import { AlertTriangle, ExternalLink, GalleryHorizontalEnd, X } from 'lucide-vue-next'
import type { Proposal } from '../../lib/proposals'
import { photoUrl, type Box } from '../../lib/review'
import CropImage from '../review/CropImage.vue'

/**
 * Butterflies' photos the assistant shows beside the chat (show_photos), by CAM ID: the dorsal and
 * ventral photos side by side, its note, and the views Photo_links does not list. Only to look at
 * (a photo opens larger in another tab); «Cerrar» takes the card away. With a `view`, each photo cut
 * to its wings, and/or the butterfly's envelope (boxes read from the photo archive).
 */
interface GalleryItem {
  id: string
  note?: string
  inWorkbook: boolean
  missing: string[]
  photos: { view: string; name: string; file: string; url: string; folder: string; wings?: Box }[]
  envelope?: { fileId: string; name: string; bbox: Box; turned: number; aspect: number }
}
const props = defineProps<{ proposal: Proposal }>()
const emit = defineEmits<{ discard: [] }>()
const card = computed(() => (props.proposal.sync ?? {}) as unknown as { items?: GalleryItem[]; view?: 'wings' | 'envelope' | 'both' })
const items = computed(() => card.value.items ?? [])
/** The envelope alone stands for the photos (`view: envelope`), when it is known. */
const photosOf = (item: GalleryItem) => (card.value.view === 'envelope' && item.envelope ? [] : item.photos)
const open = computed(() => props.proposal.status === 'pending')
const viewName = (view: string) => (view.startsWith('d') ? 'dorsal' : view.startsWith('v') ? 'ventral' : view)
</script>

<template>
  <div class="mt-2 rounded-md border border-stone-300 bg-white text-sm" data-photo-gallery>
    <p class="flex items-center gap-2 border-b border-stone-200 px-3 py-2">
      <GalleryHorizontalEnd :size="15" class="shrink-0 text-emerald-700" />
      <strong class="font-medium">{{ proposal.reason }}</strong>
      <span class="text-xs text-stone-500">{{ $tn(items.length, '{n} mariposa', '{n} mariposas') }}</span>
      <button v-if="open" class="btn-ghost ml-auto p-1" :title="$t('Cerrar')" @click="emit('discard')"><X :size="14" /></button>
    </p>
    <ul class="max-h-[36rem] space-y-2 overflow-y-auto p-2">
      <li v-for="item in items" :key="item.id" class="rounded border border-stone-200 p-2" :data-cam="item.id">
        <p class="mb-1 flex flex-wrap items-baseline gap-x-2 text-xs">
          <span class="font-mono font-medium text-stone-800">{{ item.id }}</span>
          <span v-if="item.note" class="text-stone-700">{{ item.note }}</span>
          <span v-if="!item.inWorkbook" class="text-amber-800">{{ $t('no está en la base') }}</span>
        </p>
        <div class="flex flex-wrap gap-2">
          <a
            v-if="item.envelope"
            :href="photoUrl(item.envelope.fileId, 1600)"
            target="_blank"
            rel="noopener"
            class="relative block shrink-0 overflow-hidden rounded"
            :title="$t('Sobre en {name}', { name: item.envelope.name })"
          >
            <CropImage
              :src="photoUrl(item.envelope.fileId, 1600)"
              :alt="$t('Sobre en {name}', { name: item.envelope.name })"
              :box="item.envelope.bbox"
              :aspect="item.envelope.aspect"
              :turned="item.envelope.turned"
              class="h-32 w-auto"
            />
            <span class="absolute top-1 left-1 rounded bg-black/60 px-1 text-[11px] text-white">{{ $t('Sobre') }}</span>
          </a>
          <a
            v-for="p in photosOf(item)"
            :key="p.file"
            :href="photoUrl(p.file, 1600)"
            target="_blank"
            rel="noopener"
            class="group relative block w-[calc(50%-0.25rem)] min-w-[9rem] overflow-hidden rounded bg-stone-100"
            :title="`${p.folder}/${p.name}`"
          >
            <CropImage v-if="p.wings" :src="photoUrl(p.file)" :alt="p.name" :box="p.wings" class="mx-auto h-32 w-auto!" />
            <img v-else :src="photoUrl(p.file)" :alt="p.name" loading="lazy" class="h-32 w-full object-contain" />
            <span class="absolute top-1 left-1 rounded bg-black/60 px-1 text-[11px] text-white">{{ viewName(p.view) }} · {{ p.name }}</span>
          </a>
          <p v-for="v in photosOf(item).length || !item.envelope ? item.missing : []" :key="v" class="flex h-32 w-[calc(50%-0.25rem)] min-w-[9rem] items-center justify-center gap-1 rounded border border-dashed border-amber-300 bg-amber-50 text-xs text-amber-900">
            <AlertTriangle :size="13" /> {{ $t('sin foto {view} en Photo_links', { view: viewName(v) }) }}
          </p>
        </div>
        <a v-if="item.photos[0]" :href="item.photos[0].url" target="_blank" rel="noopener" class="mt-1 inline-flex items-center gap-1 text-[11px] text-stone-500 hover:text-emerald-800">
          {{ item.photos[0].folder }} <ExternalLink :size="10" />
        </a>
      </li>
    </ul>
  </div>
</template>
