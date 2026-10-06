<script setup lang="ts">
import { computed } from 'vue'
import type { CensusSummary } from '../../lib/census'
import { t, tn } from '../../lib/i18n'

/** A census in one line: seen, disappeared, left out, findings, and where its disappearances are. */
const props = defineProps<{ census: CensusSummary }>()
const findings = computed(() => {
  const c = props.census.counts
  return c.doubts + c.otherSpecies + c.offList + c.unknown
})
const deaths = computed(() => {
  switch (props.census.deaths) {
    case 'staged':
      return { text: t('en la app, aún no en Google Sheets'), tone: 'bg-amber-100 text-amber-900' }
    case 'sending':
      return { text: t('escribiéndose en Google Sheets'), tone: 'bg-amber-100 text-amber-900' }
    case 'queued':
      return { text: t('esperando a Google Sheets'), tone: 'bg-amber-100 text-amber-900' }
    case 'failed':
      return { text: t('no escritas en Google Sheets'), tone: 'bg-red-100 text-red-900' }
    case 'written':
      return { text: t('en Google Sheets'), tone: 'bg-brand-50 text-brand-800' }
    default:
      return null
  }
})
</script>

<template>
  <span class="flex flex-wrap items-center gap-x-2 gap-y-0.5">
    <span v-if="census.status === 'cancelled'" class="rounded bg-stone-200 px-1.5 font-medium text-stone-700">{{
      $t('Cancelado')
    }}</span>
    <span class="font-medium text-brand-800">☺ {{ census.counts.seen }}/{{ census.counts.roster }}</span>
    <span v-if="census.counts.disappeared" class="text-stone-700">{{
      $tn(census.counts.disappeared, '{n} desaparecida', '{n} desaparecidas')
    }}</span>
    <span v-if="census.counts.excluded" class="text-stone-600">{{
      $tn(census.counts.excluded, '{n} no contada', '{n} no contadas')
    }}</span>
    <span v-if="findings" class="text-violet-800">{{ tn(findings, '{n} hallazgo', '{n} hallazgos') }}</span>
    <span v-if="deaths" class="rounded px-1.5 font-medium" :class="deaths.tone">{{ deaths.text }}</span>
  </span>
</template>
