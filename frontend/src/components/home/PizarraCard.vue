<script setup lang="ts">
import { dateLabel, type BoardEntry, type Team } from '../../lib/summary'

/**
 * The lab whiteboard ("Último CAMID o ID de monitoreo usado"), kept up to date
 * from the workbook: the last CAM ID used in the insectary and in field
 * collections, the last monitoring mark, and the next free ones.
 */
defineProps<{ pizarra: Team['pizarra'] }>()
const rows = (p: Team['pizarra']): { label: string; entry: BoardEntry | null; hint: string }[] => [
  { label: 'CAMID insectario', entry: p.insectaryCam, hint: 'Insectary_data (preservación o muerte)' },
  { label: 'CAMID colecta', entry: p.collectionCam, hint: 'Collection_data' },
  { label: 'Marca de monitoreo', entry: p.mark, hint: 'Marcadas y liberadas' },
]
</script>

<template>
  <section class="rounded-lg border border-stone-300 bg-white p-4 shadow-sm">
    <h2 class="mb-3 text-lg font-semibold">Pizarra · último usado y siguiente</h2>
    <div class="grid gap-3 sm:grid-cols-2 lg:grid-cols-5">
      <div v-for="r in rows(pizarra)" :key="r.label" class="rounded-md bg-stone-50 px-3 py-2" :title="r.hint">
        <p class="text-xs font-medium text-stone-500">{{ r.label }}</p>
        <template v-if="r.entry">
          <p class="font-mono text-2xl font-semibold tracking-tight">{{ r.entry.last }}</p>
          <p class="text-xs text-stone-600">
            {{ dateLabel(r.entry.date) }} · siguiente
            <span class="font-mono font-semibold text-brand-700">{{ r.entry.next ?? '—' }}</span>
          </p>
        </template>
        <p v-else class="text-2xl text-stone-400">—</p>
      </div>
      <div class="rounded-md bg-stone-50 px-3 py-2" title="Primera fila libre de Insectary_data con ID ya escrito">
        <p class="text-xs font-medium text-stone-500">Siguiente Insectary ID</p>
        <p class="font-mono text-2xl font-semibold tracking-tight text-brand-700">{{ pizarra.insectaryId ?? '—' }}</p>
      </div>
      <div class="rounded-md bg-stone-50 px-3 py-2" title="El número de clutch más alto + 1">
        <p class="text-xs font-medium text-stone-500">Siguiente clutch</p>
        <p class="font-mono text-2xl font-semibold tracking-tight text-brand-700">{{ pizarra.clutch ?? '—' }}</p>
      </div>
    </div>
  </section>
</template>
