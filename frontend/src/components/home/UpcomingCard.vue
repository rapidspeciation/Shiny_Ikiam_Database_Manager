<script setup lang="ts">
import { computed } from 'vue'
import { dateLabel, type Coming, type Team } from '../../lib/summary'

/**
 * What should happen soon in the insectary: eggs about to hatch, larvae about
 * to pupate, pupae about to emerge. Expected dates are the start of the
 * clutch's current stage plus its species' median time in that stage.
 */
const props = defineProps<{ upcoming: Team['upcoming'] }>()

const COLUMNS = [
  { event: 'hatch', title: 'Huevos por eclosionar', unit: 'huevos' },
  { event: 'pupate', title: 'Larvas por pupar', unit: 'larvas' },
  { event: 'emerge', title: 'Pupas por emerger', unit: 'pupas' },
] as const
const byEvent = computed(() =>
  Object.fromEntries(COLUMNS.map(c => [c.event, props.upcoming.items.filter(i => i.event === c.event)])),
)
const when = (d: number) =>
  d === 0 ? 'hoy' : d === 1 ? 'mañana' : d === -1 ? 'ayer' : d > 0 ? `en ${d} días` : `hace ${-d} días`
const EVENT = { hatch: 'eclosión', pupate: 'pupa', emerge: 'emergencia' }
const START = { hatch: 'puesta', pupate: 'eclosión', emerge: 'pupa' }
const detail = (c: Coming) => `${START[c.event]} ${dateLabel(c.since)} → ${EVENT[c.event]} esperada ${dateLabel(c.expected)}`
</script>

<template>
  <section class="rounded-lg border border-stone-300 bg-white p-4 shadow-sm">
    <h2 class="text-lg font-semibold">Próximos días en el insectario</h2>
    <p class="hint mb-3">
      Fecha esperada: el inicio de la etapa actual de la postura más la mediana de su especie en esa etapa. Se listan de
      {{ upcoming.lateDays }} días de atraso a {{ upcoming.aheadDays }} días adelante.
    </p>
    <div class="grid gap-3 md:grid-cols-3">
      <div v-for="c in COLUMNS" :key="c.event" class="rounded-md bg-stone-50 px-3 py-2">
        <p class="mb-1 text-sm font-semibold">{{ c.title }}</p>
        <p v-if="!byEvent[c.event].length" class="py-2 text-sm text-stone-400">Nada en estos días</p>
        <ul class="divide-y divide-stone-200">
          <li v-for="i in byEvent[c.event]" :key="i.clutch" class="flex items-baseline gap-2 py-1.5 text-sm" :title="detail(i)">
            <span class="w-16 shrink-0 font-mono font-semibold">{{ i.clutch }}</span>
            <span class="min-w-0 flex-1 truncate italic">{{ i.species ?? 'Sin especie' }}</span>
            <span v-if="i.n" class="shrink-0 text-xs text-stone-500 tabular-nums">{{ i.n }} {{ c.unit }}</span>
            <span
              class="w-20 shrink-0 text-right text-xs font-medium"
              :class="i.inDays < 0 ? 'text-amber-700' : i.inDays <= 1 ? 'text-brand-700' : 'text-stone-600'"
              >{{ when(i.inDays) }}</span
            >
          </li>
        </ul>
      </div>
    </div>
    <details v-if="upcoming.late.length" class="mt-3 text-sm">
      <summary class="cursor-pointer text-amber-800">
        {{ upcoming.late.length }} clutches pasaron su fecha esperada hace más de {{ upcoming.lateDays }} días: revisar si falta
        registrar la eclosión, la pupa o la emergencia
      </summary>
      <ul class="mt-2 grid gap-x-6 sm:grid-cols-2 lg:grid-cols-3">
        <li v-for="i in upcoming.late" :key="i.clutch" class="flex gap-2 border-t border-stone-100 py-1" :title="detail(i)">
          <span class="w-16 shrink-0 font-mono">{{ i.clutch }}</span>
          <span class="min-w-0 flex-1 truncate text-stone-600">{{ EVENT[i.event] }} · <i>{{ i.species ?? 'Sin especie' }}</i></span>
          <span class="shrink-0 text-xs text-amber-700">{{ when(i.inDays) }}</span>
        </li>
      </ul>
    </details>
  </section>
</template>
