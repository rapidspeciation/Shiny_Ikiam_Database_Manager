<script setup lang="ts">
import { computed, defineAsyncComponent } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import ImportPanel from '../components/monitoring/ImportPanel.vue'

// The report (ECharts) and the map (Leaflet) load only when opened.
const SummaryPanel = defineAsyncComponent(() => import('../components/monitoring/SummaryPanel.vue'))
const MapPanel = defineAsyncComponent(() => import('../components/monitoring/MapPanel.vue'))
const RecapturePanel = defineAsyncComponent(() => import('../components/monitoring/RecapturePanel.vue'))

/** Ikiam monthly monitoring: import a Wikiloc walk, the report tables, the map, and the recaptured individuals. */
const PANELS = [
  { id: 'importar', label: 'Importar recorrido' },
  { id: 'resumen', label: 'Reporte' },
  { id: 'mapa', label: 'Mapa' },
  { id: 'recapturas', label: 'Recapturas' },
] as const
const route = useRoute()
const router = useRouter()
const panel = computed(() => PANELS.find(p => p.id === route.query.vista)?.id || 'resumen')
const show = (id: string) => router.replace({ query: { ...route.query, vista: id } })
</script>

<template>
  <div class="flex h-full flex-col">
    <nav class="flex gap-1 border-b border-stone-200 bg-white px-3 pt-2 sm:px-4" aria-label="Monitoreo">
      <button
        v-for="p in PANELS"
        :key="p.id"
        class="-mb-px rounded-t-md border border-b-0 px-3 py-1.5 text-sm font-medium"
        :class="
          panel === p.id
            ? 'border-stone-200 bg-stone-50 text-brand-700'
            : 'border-transparent text-stone-600 hover:text-stone-900'
        "
        @click="show(p.id)"
      >
        {{ p.label }}
      </button>
    </nav>
    <div class="min-h-0 flex-1">
      <ImportPanel v-if="panel === 'importar'" />
      <SummaryPanel v-else-if="panel === 'resumen'" />
      <RecapturePanel v-else-if="panel === 'recapturas'" />
      <MapPanel v-else />
    </div>
  </div>
</template>
