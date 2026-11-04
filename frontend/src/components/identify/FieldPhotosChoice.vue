<script setup lang="ts">
// «Fotos para el campo»: which photos to keep on the phone (species: all or Ecuador's; subspecies: none, all or
// Ecuador's), each option with how many photos and how much space, from the field set's list (lib/identifyRefs.ts).
// Used in the «Modelo y almacenamiento» box and in the model's download panel (views/IdentifyView.vue).
import { computed } from 'vue'
import { t } from '../../lib/i18n'
import { inScope, sizeOf, type FieldIndex, type FieldScope } from '../../lib/identifyRefs'

const props = defineProps<{ index: FieldIndex | null; name: string }>()
const scope = defineModel<FieldScope>({ required: true })

const size = (s: Pick<FieldScope, 'species' | 'ssp'>, part: 'species' | 'subspecies') =>
  props.index ? sizeOf(inScope(props.index, s)[part]) : null
const mbText = (b: number) => (b / 1048576).toFixed(1)
const opts = computed(() => ({
  species: (['all', 'ec'] as const).map(v => ({ value: v, size: size({ species: v, ssp: 'none' }, 'species') })),
  ssp: (['none', 'ec', 'all'] as const).map(v => ({ value: v, size: v === 'none' ? null : size({ species: 'all', ssp: v }, 'subspecies') })),
}))
const counts = computed(() => props.index?.counts)
const set = (k: 'species' | 'ssp', v: string) => (scope.value = { ...scope.value, [k]: v })
const label = (v: string) => (v === 'all' ? t('Todas') : v === 'ec' ? t('Solo Ecuador') : t('Ninguna'))
</script>

<template>
  <div class="space-y-2 text-sm">
    <fieldset v-for="k in ['species', 'ssp'] as const" :key="k">
      <legend class="mb-1 font-medium">{{ k === 'species' ? $t('Especies') : $t('Subespecies') }}</legend>
      <div class="flex flex-wrap gap-1.5">
        <label
          v-for="o in opts[k]"
          :key="o.value"
          class="flex cursor-pointer items-center gap-1.5 rounded-md border px-2.5 py-1.5"
          :class="scope[k] === o.value ? 'border-brand bg-brand-soft' : 'border-border bg-surface'"
        >
          <input type="radio" :name="`${name}-${k}`" :value="o.value" :checked="scope[k] === o.value" class="size-4 accent-[var(--focus)]" @change="set(k, o.value)" />
          <span>
            {{ label(o.value) }}
            <span v-if="o.size" class="whitespace-nowrap text-fg-muted">· {{ $t('{n} fotos, {mb} MB', { n: o.size.photos, mb: mbText(o.size.bytes) }) }}</span>
          </span>
        </label>
      </div>
    </fieldset>
    <p v-if="counts" class="text-xs text-fg-subtle">
      {{
        $t('No todas tienen una foto libre en iNaturalist: {sp} de {spAll} especies y {ssp} de {sspAll} subespecies (en Ecuador, {spEc} de {spEcAll} y {sspEc} de {sspEcAll}). «Solo Ecuador»: las registradas en Ecuador.', {
          sp: counts.species.photos,
          spAll: counts.species.head,
          ssp: counts.subspecies.photos,
          sspAll: counts.subspecies.head,
          spEc: counts.species.ecuador_photos,
          spEcAll: counts.species.ecuador,
          sspEc: counts.subspecies.ecuador_photos,
          sspEcAll: counts.subspecies.ecuador,
        })
      }}
    </p>
  </div>
</template>
