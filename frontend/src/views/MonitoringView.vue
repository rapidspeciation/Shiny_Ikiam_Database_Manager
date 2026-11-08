<script setup lang="ts">
import { computed, defineAsyncComponent, ref } from 'vue'
import { ChevronRight } from 'lucide-vue-next'
import { useRoute, useRouter } from 'vue-router'
import ImportPanel from '../components/monitoring/ImportPanel.vue'
import ScrollRow from '../components/ScrollRow.vue'
import type { MoreSide } from '../lib/scrollRow'
import { useSession } from '../stores/session'

// The report (ECharts) and the map (Leaflet) load only when opened.
const SummaryPanel = defineAsyncComponent(() => import('../components/monitoring/SummaryPanel.vue'))
const MapPanel = defineAsyncComponent(() => import('../components/monitoring/MapPanel.vue'))
const RecapturePanel = defineAsyncComponent(() => import('../components/monitoring/RecapturePanel.vue'))
const DoubtsPanel = defineAsyncComponent(() => import('../components/monitoring/DoubtsPanel.vue'))
const WikilocDataPanel = defineAsyncComponent(() => import('../components/monitoring/WikilocDataPanel.vue'))

/**
 * Ikiam monthly monitoring: import a Wikiloc walk, the report tables, the map,
 * the recaptured individuals, (for editors) the doubtful pairings of walk points with rows, and
 * the Wikiloc data: what the app holds, downloads, and the corrections it suggests.
 */
const ALL_PANELS = [
  { id: 'importar', label: 'Importar recorrido', short: 'Importar' },
  { id: 'resumen', label: 'Reporte', short: 'Reporte' },
  { id: 'mapa', label: 'Mapa', short: 'Mapa' },
  { id: 'recapturas', label: 'Recapturas', short: 'Recapturas' },
  { id: 'dudas', label: 'Dudas de emparejamiento', short: 'Dudas' },
  { id: 'wikiloc', label: 'Datos de Wikiloc', short: 'Wikiloc' },
] as const
const session = useSession()
const PANELS = computed(() => ALL_PANELS.filter(p => p.id !== 'dudas' || session.canEdit))
const route = useRoute()
const router = useRouter()
const panel = computed(() => PANELS.value.find(p => p.id === route.query.vista)?.id || 'resumen')
// Changing tabs shows the whole list: a butterfly chosen with «En el mapa» or «ver fotos» stays only while you look at it.
const show = (id: string) => router.replace({ query: { ...route.query, vista: id, individuo: undefined } })
/** The tab row hides more tabs at its right (a phone): a › there says so and scrolls on. */
const tabRow = ref<{ scrollOn: () => void }>()
const tabsMore = ref<MoreSide>('none')
</script>

<template>
  <div class="flex h-full flex-col">
    <!-- One row that scrolls sideways on a phone (the last tab faded at the edge, and a › while more tabs wait to the
         right; the open one brought into sight), never wider than the screen. -->
    <div class="relative">
    <ScrollRow
      ref="tabRow"
      as="nav"
      :active="panel"
      class="flex gap-0.5 border-b border-stone-200 bg-surface px-3 pt-2 sm:gap-1 sm:px-4"
      :aria-label="$t('Monitoreo')"
      @more="side => (tabsMore = side)"
    >
      <button
        v-for="p in PANELS"
        :key="p.id"
        class="-mb-px shrink-0 rounded-t-md border border-b-0 px-2 py-1.5 text-sm font-medium whitespace-nowrap focus-visible:-outline-offset-2 sm:px-3"
        :data-active="panel === p.id ? '' : undefined"
        :class="
          panel === p.id
            ? 'border-stone-200 bg-stone-50 text-brand-700'
            : 'border-transparent text-stone-600 hover:text-stone-900'
        "
        @click="show(p.id)"
      >
        <!-- One line on phones: the tabs' second line cost the grid a row. -->
        <span class="sm:hidden">{{ $t(p.short) }}</span
        ><span class="hidden sm:inline">{{ $t(p.label) }}</span>
      </button>
    </ScrollRow>
    <button
      v-if="tabsMore === 'end' || tabsMore === 'both'"
      type="button"
      tabindex="-1"
      aria-hidden="true"
      class="absolute top-0 right-0 bottom-px flex w-11 items-center justify-end bg-linear-to-l from-surface from-50% to-transparent pr-1.5 text-fg-muted"
      data-more-tabs
      @click="tabRow?.scrollOn()"
    >
      <ChevronRight :size="18" />
    </button>
    </div>
    <div class="min-h-0 flex-1">
      <ImportPanel v-if="panel === 'importar'" />
      <SummaryPanel v-else-if="panel === 'resumen'" />
      <RecapturePanel v-else-if="panel === 'recapturas'" />
      <DoubtsPanel v-else-if="panel === 'dudas'" />
      <WikilocDataPanel v-else-if="panel === 'wikiloc'" />
      <MapPanel v-else />
    </div>
  </div>
</template>
