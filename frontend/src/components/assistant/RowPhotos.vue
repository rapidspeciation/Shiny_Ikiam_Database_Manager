<script setup lang="ts">
import { computed, defineAsyncComponent, ref, watch } from 'vue'
import { photoReadings, readingText } from '../../lib/walkReadings'

/** Pan and zoom load apart, as in «Revisar con la foto». */
const PhotoViewer = defineAsyncComponent(() => import('./PhotoViewer.vue'))

/**
 * The photos of the row selected in a proposal's table, beside it, when its values come from
 * photos of their own (a Wikiloc walk's point): to check the species, sex and mark without leaving
 * the table. Under the photo, what the AI Identifier reads in it (Monitoreo → Importar follows it).
 */
const props = defineProps<{
  photos: { id: string; url: string; note?: string }[]
  /** The row's label and note (the Wikiloc note). */
  label: string
  note?: string
}>()
const photo = ref(0)
watch(
  () => props.photos.map(p => p.id).join('|'),
  () => (photo.value = 0),
)
const url = (n: number) => props.photos[n]?.url ?? ''
const reading = computed(() => photoReadings.get(props.photos[photo.value]?.id ?? ''))
</script>

<template>
  <aside class="flex w-72 shrink-0 flex-col gap-1 border-l border-stone-200 bg-stone-50 p-2 text-xs" data-row-photos>
    <p class="truncate font-medium text-stone-800" :title="label">{{ label }}</p>
    <div class="h-64 overflow-hidden rounded border border-stone-200 bg-stone-900">
      <PhotoViewer
        v-if="photos.length"
        v-model:photo="photo"
        :url="url"
        :count="photos.length"
        whole
        :links="photos.map(p => p.url)"
        :alt="n => $t('Foto {n} del punto', { n: n + 1 })"
        class="h-full"
      />
      <p v-else class="m-auto px-4 py-20 text-center text-stone-300">{{ $t('Sin fotos en Wikiloc') }}</p>
    </div>
    <p v-if="reading" class="rounded bg-white px-1.5 py-1 text-stone-700" data-reading>
      <span class="font-medium">{{ $t('Identificador') }}:</span>
      <template v-if="reading.status === 'done'">
        {{ readingText(reading.species) || $t('sin especie') }}
        <span v-if="reading.crop" class="text-amber-800"> · {{ $t('recorte: {crop}', { crop: reading.crop }) }}</span>
      </template>
      <template v-else-if="reading.status === 'failed'">{{ $t('no pudo leerla') }} ({{ reading.error }})</template>
      <template v-else-if="reading.status === 'reading'">{{ $t('leyendo la foto…') }}</template>
      <template v-else>{{ $t('en cola') }}</template>
    </p>
    <p v-if="note" class="line-clamp-4 text-stone-600" :title="note">{{ note }}</p>
  </aside>
</template>
