<script setup lang="ts">
import { computed, ref } from 'vue'
import { ImageOff } from 'lucide-vue-next'
import { cropStyle, type Box } from '../../lib/review'

/**
 * A photo, or only a box of it (the envelope, the wings), cut out by CSS on the
 * cached Drive image: the server needs no image library. Loaded when it comes
 * into view. Without a known shape the photo's own is used once it loads.
 */
const props = withDefaults(defineProps<{ src: string; alt: string; box?: Box; aspect?: number; turned?: number }>(), {
  box: undefined,
  aspect: undefined,
  turned: 0,
})
const loaded = ref<number | null>(null)
const failed = ref(false)
const crop = computed(() => (props.box ? cropStyle(props.box, props.aspect ?? loaded.value ?? 4 / 3, props.turned) : null))
function onLoad(event: Event) {
  const img = event.target as HTMLImageElement
  if (img.naturalHeight) loaded.value = img.naturalWidth / img.naturalHeight
}
</script>

<template>
  <div
    v-if="failed"
    class="grid aspect-[4/3] place-items-center rounded bg-stone-100 p-2 text-center text-xs text-stone-500"
    :title="`Drive no dio ${alt}`"
  >
    <span><ImageOff :size="18" class="mx-auto mb-1" />Sin foto</span>
  </div>
  <div v-else-if="crop" class="relative overflow-hidden rounded bg-stone-200" :style="crop.frame">
    <img
      :src="src"
      :alt="alt"
      loading="lazy"
      decoding="async"
      class="absolute max-w-none"
      :style="crop.image"
      @load="onLoad"
      @error="failed = true"
    />
  </div>
  <img
    v-else
    :src="src"
    :alt="alt"
    loading="lazy"
    decoding="async"
    class="aspect-[4/3] w-full rounded bg-stone-200 object-contain"
    @error="failed = true"
  />
</template>
