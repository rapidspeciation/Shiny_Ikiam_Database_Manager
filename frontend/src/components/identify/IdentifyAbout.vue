<script setup lang="ts">
// «Acerca de y licencias» of the Identificar tab (views/IdentifyView.vue): where the models come from, their
// licences (lib/publicModels.ts MODEL_PAGES, the Hugging Face model cards) and the reference photos' credit.
import { MODEL_PAGES } from '../../lib/publicModels'
/** Signed in: the tab also offers «En línea (servidor)», which sends the photo. */
defineProps<{ team?: boolean }>()
</script>

<template>
  <details class="rounded-lg border border-border bg-surface p-3 text-sm">
    <summary class="cursor-pointer font-medium">{{ $t('Acerca de y licencias') }}</summary>
    <p v-if="team" class="mt-2 text-fg-muted">
      {{ $t('Gratis y sin fines comerciales. «En este teléfono»: la foto se analiza en este dispositivo y no sale de él. «En línea (servidor)», solo para el equipo: la foto va al servidor del proyecto, que la analiza y no la guarda.') }}
    </p>
    <p v-else class="mt-2 text-fg-muted">
      {{ $t('Gratis y sin fines comerciales. Las fotos se analizan en este dispositivo: no se envían a ningún servidor.') }}
    </p>
    <ul class="mt-2 list-disc space-y-1.5 pl-5">
      <li>
        {{ $t('Detector de alas: YOLO26n-seg de Ultralytics, ajustado por el proyecto.') }}
        <a :href="MODEL_PAGES.detector" target="_blank" rel="noopener" class="text-brand-text underline">AGPL-3.0</a> ·
        <a :href="MODEL_PAGES.detectorSource" target="_blank" rel="noopener" class="text-brand-text underline">{{ $t('código fuente') }}</a>
      </li>
      <li>
        {{ $t('Modelo «Más preciso»: BioCLIP 2.5 (Imageomics Institute), convertido a int8 por el proyecto.') }}
        <a :href="MODEL_PAGES.encoder" target="_blank" rel="noopener" class="text-brand-text underline">MIT</a> ·
        <a :href="MODEL_PAGES.upstream" target="_blank" rel="noopener" class="text-brand-text underline">{{ $t('original') }}</a>
      </li>
      <li>
        {{ $t('Modelo «Rápido y liviano»: entrenado por el proyecto para imitar a BioCLIP 2.5.') }}
        <a :href="MODEL_PAGES.small" target="_blank" rel="noopener" class="text-brand-text underline">CC BY-NC 4.0</a>
      </li>
      <li>
        {{ $t('Clasificador de especies: entrenado por el proyecto con fotos y nombres de Butterflies of America, Sangay, Noreste, Cotacachi, GBIF, iNaturalist y otras bases.') }}
        <a :href="MODEL_PAGES.head" target="_blank" rel="noopener" class="text-brand-text underline">CC BY-NC 4.0</a>
      </li>
      <li v-if="team">{{ $t('También estima el sexo, en línea, en fotos de seis especies fuera de las Ithomiini (experimental, probado en mariposas vivas).') }}</li>
      <li>{{ $t('Fotos de referencia: iNaturalist, con licencias Creative Commons; bajo cada foto, su autor (enlace a la foto) y su licencia.') }}</li>
    </ul>
  </details>
</template>
