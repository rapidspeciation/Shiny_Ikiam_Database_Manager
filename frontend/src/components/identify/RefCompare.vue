<script setup lang="ts">
// The person's photo beside reference photos of one candidate (views/IdentifyView.vue), with each photo's credit.
// A genus fallback is said to be another species, a subspecies' photo names it; the team's specimens are named as such.
import { computed, ref, watch } from 'vue'
import { ChevronLeft, ChevronRight } from 'lucide-vue-next'
import { licenseUrl, type Refs } from '../../lib/identifyRefs'

const props = defineProps<{ mine: string; name: string; refs: Refs | undefined; loading: boolean; online: boolean; hint?: string | null }>()
defineEmits<{ hint: [] }>()
const at = ref(0)
const broken = ref(new Set<string>())
watch(
  () => props.name,
  () => (at.value = 0),
)
const photos = computed(() => (props.refs?.photos ?? []).filter(p => !broken.value.has(p.src)))
const photo = computed(() => photos.value[Math.min(at.value, photos.value.length - 1)])
const genus = computed(() => props.name.split(' ')[0])
const step = (d: number) => (at.value = (at.value + d + photos.value.length) % photos.value.length)
// A photo the phone does not have and cannot fetch (no signal): left out.
const failed = (src: string) => (broken.value = new Set([...broken.value, src]))
</script>

<template>
  <div class="grid grid-cols-2 gap-2">
    <figure class="min-w-0">
      <img :src="mine" alt="" class="aspect-square w-full rounded-md bg-photo-mat object-cover" />
      <figcaption class="mt-1 text-xs text-fg-muted">{{ $t('Su foto (lo que miró la IA)') }}</figcaption>
    </figure>
    <figure class="min-w-0">
      <div class="relative">
        <img
          v-if="photo"
          :key="photo.src"
          :src="photo.src"
          alt=""
          class="aspect-square w-full rounded-md bg-photo-mat object-cover"
          @error="failed(photo.src)"
        />
        <div v-else class="grid aspect-square w-full place-items-center rounded-md bg-surface-sunken p-3 text-center text-xs text-fg-muted">
          <span>
            {{ loading ? $t('Buscando fotos…') : online ? $t('No encontramos fotos de esta especie.') : $t('No hay fotos guardadas de esta especie en el teléfono.') }}
            <button v-if="hint && !loading" class="mt-2 block min-h-11 w-full rounded-md border border-brand px-2 font-medium text-brand-text hover:bg-brand-soft" data-testid="gap-hint" @click="$emit('hint')">{{ hint }}</button>
          </span>
        </div>
        <template v-if="photos.length > 1">
          <button class="btn-ghost absolute top-1/2 left-1 -translate-y-1/2 bg-surface/80" :aria-label="$t('Foto anterior')" @click="step(-1)">
            <ChevronLeft :size="18" />
          </button>
          <button class="btn-ghost absolute top-1/2 right-1 -translate-y-1/2 bg-surface/80" :aria-label="$t('Foto siguiente')" @click="step(1)">
            <ChevronRight :size="18" />
          </button>
        </template>
      </div>
      <figcaption v-if="photo" class="mt-1 text-xs text-fg-muted">
        <span v-if="photo.level === 'genus'" class="font-medium text-amber-800">{{ $t('Foto de otra especie del género {genus}', { genus }) }}. </span>
        <span v-if="photo.level === 'subspecies' && photo.taxon" class="font-medium">{{ $t('Subespecie') }} <i>{{ photo.taxon.split(' ').slice(2).join(' ') }}</i>. </span>
        <span v-if="photo.level === 'specimen'">{{ $t('Ejemplar del proyecto') }} {{ photo.credit }}<template v-if="photo.sex"> · {{ photo.sex }}</template></span>
        <span v-else class="break-words"
          ><a v-if="photo.source" :href="photo.source" target="_blank" rel="noopener" class="underline-offset-2 hover:underline">{{ photo.credit }}</a
          ><template v-else>{{ photo.credit }}</template
          ><template v-if="photo.license">
            ·
            <a v-if="licenseUrl(photo.license)" :href="licenseUrl(photo.license)!" target="_blank" rel="noopener license" class="underline-offset-2 hover:underline">{{ photo.license }}</a
            ><template v-else>{{ photo.license }}</template></template
          ></span
        >
        <span v-if="photos.length > 1" class="text-fg-subtle"> · {{ Math.min(at, photos.length - 1) + 1 }}/{{ photos.length }}</span>
      </figcaption>
    </figure>
  </div>
</template>
