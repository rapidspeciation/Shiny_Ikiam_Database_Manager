<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted } from 'vue'
import { ChevronLeft, ChevronRight, ExternalLink, X } from 'lucide-vue-next'
import { photoUrl } from '../../lib/review'

/** A card's photos full size, one at a time: ←/→ (or the buttons, or swipe with the arrows) and Esc. */
const props = defineProps<{ photos: { id: string; name: string }[] }>()
const index = defineModel<number>({ required: true })
const emit = defineEmits<{ close: [] }>()
const photo = computed(() => props.photos[index.value])
const move = (step: number) => {
  const n = props.photos.length
  if (n) index.value = (index.value + step + n) % n
}
function onKey(event: KeyboardEvent) {
  if (event.key === 'Escape') emit('close')
  else if (event.key === 'ArrowRight') move(1)
  else if (event.key === 'ArrowLeft') move(-1)
  else return
  event.preventDefault()
}
onMounted(() => window.addEventListener('keydown', onKey))
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
</script>

<template>
  <div
    class="fixed inset-0 z-50 flex flex-col bg-black/90 text-white"
    role="dialog"
    aria-modal="true"
    @click.self="emit('close')"
  >
    <div class="flex items-center gap-2 px-3 py-2 text-sm">
      <span class="font-medium">{{ photo?.name }}</span>
      <span class="text-stone-400">{{ index + 1 }} / {{ photos.length }}</span>
      <a
        v-if="photo"
        :href="`https://drive.google.com/file/d/${photo.id}/view`"
        target="_blank"
        rel="noopener"
        class="ml-2 inline-flex items-center gap-1 text-stone-300 hover:text-white"
        :title="$t('Abrir en Google Drive')"
      >
        Drive <ExternalLink :size="13" />
      </a>
      <button class="ml-auto rounded p-1.5 hover:bg-white/10" :title="$t('Cerrar (Esc)')" @click="emit('close')">
        <X :size="20" />
      </button>
    </div>
    <div class="relative flex min-h-0 flex-1 items-center justify-center px-2 pb-3" @click.self="emit('close')">
      <img
        v-if="photo"
        :key="photo.id"
        :src="photoUrl(photo.id, 1600)"
        :alt="photo.name"
        class="max-h-full max-w-full object-contain"
      />
      <template v-if="photos.length > 1">
        <button
          class="absolute top-1/2 left-2 -translate-y-1/2 rounded-full bg-black/50 p-2 hover:bg-black/70"
          :title="$t('Anterior (←)')"
          @click="move(-1)"
        >
          <ChevronLeft :size="26" />
        </button>
        <button
          class="absolute top-1/2 right-2 -translate-y-1/2 rounded-full bg-black/50 p-2 hover:bg-black/70"
          :title="$t('Siguiente (→)')"
          @click="move(1)"
        >
          <ChevronRight :size="26" />
        </button>
      </template>
    </div>
  </div>
</template>
