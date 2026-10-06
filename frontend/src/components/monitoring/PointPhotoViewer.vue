<script setup lang="ts">
import { onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { ChevronLeft, ChevronRight, X } from 'lucide-vue-next'

/** A Wikiloc point's photos enlarged over the page, with its note (Esc closes, arrows step). */
const props = defineProps<{ photos: string[]; start?: number; text: string }>()
const emit = defineEmits<{ close: [] }>()

const index = ref(props.start ?? 0)
watch(
  () => [props.photos, props.start],
  () => (index.value = props.start ?? 0),
)
const step = (by: number) => (index.value = (index.value + by + props.photos.length) % props.photos.length)
const photoUrl = (id: string) => `api/monitoring/photos/${id}`
function onKey(event: KeyboardEvent) {
  if (event.key === 'Escape') emit('close')
  else if (event.key === 'ArrowRight') step(1)
  else if (event.key === 'ArrowLeft') step(-1)
}
onMounted(() => window.addEventListener('keydown', onKey))
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
</script>

<template>
  <div
    class="fixed inset-0 z-[2000] flex flex-col bg-black/90 text-white"
    role="dialog"
    aria-modal="true"
    @click.self="emit('close')"
  >
    <div class="flex items-center gap-3 px-4 py-2 text-sm">
      <span>«{{ text }}» · {{ $t('foto {n} de {total}', { n: index + 1, total: photos.length }) }}</span>
      <button class="ml-auto rounded p-1 hover:bg-white/10" :title="$t('Cerrar (Esc)')" @click="emit('close')">
        <X :size="20" />
      </button>
    </div>
    <div class="relative flex min-h-0 flex-1 items-center justify-center" @click.self="emit('close')">
      <button v-if="photos.length > 1" class="absolute left-2 rounded-full bg-white/10 p-2 hover:bg-white/20" @click="step(-1)">
        <ChevronLeft :size="24" />
      </button>
      <img :src="photoUrl(photos[index])" alt="" class="max-h-full max-w-full object-contain" />
      <button v-if="photos.length > 1" class="absolute right-2 rounded-full bg-white/10 p-2 hover:bg-white/20" @click="step(1)">
        <ChevronRight :size="24" />
      </button>
    </div>
  </div>
</template>
