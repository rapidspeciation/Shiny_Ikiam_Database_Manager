<script setup lang="ts">
// The person's photo beside reference photos of one candidate (views/IdentifyView.vue), with each photo's credit.
// A genus fallback is said to be another species, a subspecies' photo names it; the team's specimens are named as such,
// a preserved specimen with its ID. Each photo's country follows its credit. As the Wings Gallery's Field / Museum
// tabs, two chips on the reference photo switch between the live photos («Campo», the default) and the museum ones
// («Museo»: the species' Sanger wing crop first, lib/curatedPhotos.ts); the arrows browse the chosen set.
// Tapping either photo opens it full screen with zoom (`zoom`: the person's photo, or a reference photo by its src).
import { computed, ref, watch } from 'vue'
import { ChevronLeft, ChevronRight, ZoomIn } from 'lucide-vue-next'
import { licenseUrl, type RefPhoto, type Refs } from '../../lib/identifyRefs'
import { fieldAndMuseum, sexLabel } from '../../lib/bestPhotos'
import { countryName } from '../../lib/usePacks'
import { probePhoto } from '../../lib/visitorLimit'

const props = defineProps<{ mine: string; name: string; refs: Refs | undefined; loading: boolean; online: boolean; hint?: { text: string; go: boolean } | null; wing?: RefPhoto | null; preferMuseum?: boolean }>()
defineEmits<{ hint: []; zoom: [which: 'mine' | 'ref', src?: string] }>()
const at = ref(0)
const broken = ref(new Set<string>())
const sets = computed(() => {
  const s = fieldAndMuseum(props.refs?.photos ?? [], props.wing)
  const ok = (p: RefPhoto) => !broken.value.has(p.src)
  return { field: s.field.filter(ok), museum: s.museum.filter(ok) }
})
/** The person's choice of set for this species (null: the default). */
const set = ref<'field' | 'museum' | null>(null)
watch(
  () => props.name,
  () => ((at.value = 0), (set.value = null)),
)
function choose(s: 'field' | 'museum') {
  set.value = s
  at.value = 0
}
/**
 * The set shown: the person's choice; by default «Campo», unless it is empty or has no photo the candidates'
 * thumbnail would take (`preferMuseum`: e.g. only an old plate of many specimens) and there is a museum photo.
 */
const shown = computed(() => {
  if (!sets.value.museum.length) return 'field'
  if (!sets.value.field.length) return 'museum'
  return set.value ?? (props.preferMuseum ? 'museum' : 'field')
})
const photos = computed(() => sets.value[shown.value])
const photo = computed(() => photos.value[Math.min(at.value, photos.value.length - 1)])
const genus = computed(() => props.name.split(' ')[0])
const step = (d: number) => (at.value = (at.value + d + photos.value.length) % photos.value.length)
// A photo the phone does not have and cannot fetch (no signal): left out.
const failed = (src: string) => {
  broken.value = new Set([...broken.value, src])
  probePhoto(src)
}
// Until a reference photo is in: a light surface saying it loads, not a black box beside the person's photo.
const loaded = ref(new Set<string>())
const done = (src: string) => (loaded.value = new Set([...loaded.value, src]))
</script>

<template>
  <div class="grid grid-cols-2 gap-2">
    <figure class="min-w-0">
      <button type="button" class="zoomable relative block w-full overflow-hidden rounded-md" :aria-label="$t('Ver su foto en grande')" data-testid="zoom-mine" @click="$emit('zoom', 'mine')">
        <img :src="mine" alt="" class="aspect-square w-full rounded-md bg-photo-mat object-cover" />
        <span class="zoom-badge" aria-hidden="true"><ZoomIn :size="16" /></span>
      </button>
      <figcaption class="mt-1 text-xs text-fg-muted">{{ $t('Su foto') }}</figcaption>
    </figure>
    <figure class="min-w-0">
      <div class="relative">
        <button v-if="photo" type="button" class="zoomable relative block w-full overflow-hidden rounded-md" :aria-label="$t('Ver la foto de referencia en grande')" data-testid="zoom-ref" @click="$emit('zoom', 'ref', photo.src)">
          <img
            :key="photo.src"
            :src="photo.src"
            alt=""
            class="aspect-square w-full rounded-md"
            :class="[loaded.has(photo.src) ? 'bg-photo-mat' : 'bg-surface-sunken', photo.kind === 'preserved' ? 'object-contain' : 'object-cover']"
            @load="done(photo.src)"
            @error="failed(photo.src)"
          />
          <span v-if="loaded.has(photo.src)" class="zoom-badge" aria-hidden="true"><ZoomIn :size="16" /></span>
        </button>
        <div
          v-if="photo && !loaded.has(photo.src)"
          class="pointer-events-none absolute inset-0 grid place-items-center text-xs text-fg-muted"
          role="status"
          data-ref-loading
        >
          {{ $t('Cargando foto…') }}
        </div>
        <div v-if="!photo" class="grid aspect-square w-full place-items-center rounded-md bg-surface-sunken p-3 text-center text-xs text-fg-muted">
          <span>
            {{ loading ? $t('Buscando fotos…') : online ? $t('No encontramos fotos de esta especie.') : $t('No hay fotos guardadas de esta especie en el teléfono.') }}
            <!-- Offline: the choice in Descargas that has its photo (a tap goes there), or that none can be downloaded. -->
            <button v-if="hint?.go && !loading" class="mt-2 block min-h-11 w-full rounded-md border border-brand px-2 font-medium text-brand-text hover:bg-brand-soft" data-testid="gap-hint" @click="$emit('hint')">{{ hint.text }}</button>
            <span v-else-if="hint && !loading" class="mt-2 block font-medium" data-testid="gap-hint">{{ hint.text }}</span>
          </span>
        </div>
        <template v-if="photos.length > 1">
          <button class="absolute top-1/2 left-1 grid size-9 -translate-y-1/2 place-items-center rounded-full bg-black/55 text-white hover:bg-black/75" :aria-label="$t('Foto anterior')" @click="step(-1)">
            <ChevronLeft :size="20" />
          </button>
          <button class="absolute top-1/2 right-1 grid size-9 -translate-y-1/2 place-items-center rounded-full bg-black/55 text-white hover:bg-black/75" :aria-label="$t('Foto siguiente')" @click="step(1)">
            <ChevronRight :size="20" />
          </button>
        </template>
        <div
          v-if="sets.field.length && sets.museum.length"
          class="absolute top-1.5 left-1.5 flex gap-0.5 rounded-full bg-black/60 p-0.5 text-xs font-medium"
          role="radiogroup"
          :aria-label="$t('Fotos de referencia')"
          data-testid="ref-sets"
        >
          <button
            v-for="k in (['field', 'museum'] as const)"
            :key="k"
            role="radio"
            :aria-checked="shown === k"
            class="hit-target rounded-full px-2 py-1 whitespace-nowrap"
            :class="shown === k ? 'bg-white text-black' : 'text-white hover:bg-white/20'"
            :data-testid="`ref-set-${k}`"
            @click="choose(k)"
          >
            {{ k === 'field' ? $t('Campo') : $t('Museo') }} {{ sets[k].length }}
          </button>
        </div>
      </div>
      <figcaption v-if="photo" class="mt-1 line-clamp-2 text-xs text-fg-muted">
        <span v-if="photo.level === 'genus'" class="font-medium text-amber-800">{{ $t('Foto de otra especie del género {genus}', { genus }) }}. </span>
        <span v-if="photo.level === 'subspecies' && photo.taxon" class="font-medium">{{ $t('Subespecie') }} <i>{{ photo.taxon.split(' ').slice(2).join(' ') }}</i>. </span>
        <span v-if="photo.level === 'specimen'">{{ $t('Ejemplar del proyecto') }} {{ photo.credit }}<template v-if="photo.sex"> · {{ sexLabel(photo.sex) }}</template></span>
        <span v-else class="break-words"
          ><span v-if="photo.kind === 'preserved'" class="font-medium" data-testid="preserved">{{ $t('Ejemplar preservado') }}{{ photo.specimen ? ` ${photo.specimen}` : '' }} · </span
          ><a v-if="photo.source" :href="photo.source" target="_blank" rel="noopener" class="underline-offset-2 hover:underline">{{ photo.credit }}</a
          ><template v-else>{{ photo.kind === 'preserved' ? $t('Foto: {who}', { who: photo.credit }) : photo.credit }}</template
          ><template v-if="photo.license">
            ·
            <a v-if="licenseUrl(photo.license)" :href="licenseUrl(photo.license)!" target="_blank" rel="noopener license" class="underline-offset-2 hover:underline">{{ photo.license }}</a
            ><template v-else>{{ photo.license }}</template></template
          ><template v-if="photo.country">
            · <span data-testid="photo-country">{{ countryName(photo.country) }}</span></template
          ></span
        >
        <span v-if="photos.length > 1" class="text-fg-subtle"> · {{ Math.min(at, photos.length - 1) + 1 }}/{{ photos.length }}</span>
      </figcaption>
    </figure>
  </div>
</template>

<style scoped>
.zoomable {
  cursor: zoom-in;
}
.zoomable img {
  transition: transform 160ms ease;
}
@media (hover: hover) {
  .zoomable:hover img {
    transform: scale(1.03);
  }
}
.zoom-badge {
  position: absolute;
  right: 0.375rem;
  bottom: 0.375rem;
  display: grid;
  place-items: center;
  width: 1.75rem;
  height: 1.75rem;
  border-radius: 9999px;
  background: rgb(0 0 0 / 0.55);
  color: #fff;
}
@media (prefers-reduced-motion: reduce) {
  .zoomable img {
    transition: none;
  }
}
</style>
