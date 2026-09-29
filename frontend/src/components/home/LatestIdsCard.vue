<script setup lang="ts">
import { dateLabel, type BoardEntry, type Team } from '../../lib/summary'
import InsectaryIdsWarning from '../InsectaryIdsWarning.vue'

/**
 * The IDs the team needs when labelling, from the workbook: the last CAM ID
 * used in the insectary and in field collections, the last monitoring mark,
 * each with its date and the next free one, and the next Insectary ID and clutch.
 */
defineProps<{ ids: Team['latestIds'] }>()
defineEmits<{ extended: [] }>()
const rows = (p: Team['latestIds']): { label: string; entry: BoardEntry | null; hint: string }[] => [
  { label: 'CAMID insectario', entry: p.insectaryCam, hint: 'Insectary_data (preservación o muerte)' },
  { label: 'CAMID colecta', entry: p.collectionCam, hint: 'Collection_data' },
  { label: 'Marca de monitoreo', entry: p.mark, hint: 'Marcadas y liberadas' },
]
</script>

<template>
  <section class="rounded-lg border border-stone-300 bg-white p-4 shadow-sm">
    <h2 class="mb-3 text-lg font-semibold">Últimos IDs usados</h2>
    <div class="grid gap-3 sm:grid-cols-2 lg:grid-cols-5">
      <div v-for="r in rows(ids)" :key="r.label" class="rounded-md bg-stone-50 px-3 py-2" :title="r.hint">
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
        <p v-if="ids.insectaryId" class="font-mono text-2xl font-semibold tracking-tight text-brand-700">{{ ids.insectaryId }}</p>
        <p v-else class="mt-1 text-xs text-amber-800">No quedan filas preasignadas al final de Insectary_data: crea más.</p>
      </div>
      <div class="rounded-md bg-stone-50 px-3 py-2" title="El número de clutch más alto + 1">
        <p class="text-xs font-medium text-stone-500">Siguiente clutch</p>
        <p class="font-mono text-2xl font-semibold tracking-tight text-brand-700">{{ ids.clutch ?? '—' }}</p>
      </div>
    </div>
    <InsectaryIdsWarning class="mt-3" @extended="$emit('extended')" />
  </section>
</template>
