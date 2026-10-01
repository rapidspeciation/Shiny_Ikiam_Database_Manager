<script setup lang="ts">
import { computed } from 'vue'
import { formatSerial } from '../../lib/dates'
import type { Facts } from '../../lib/deaths'
import { t, tn } from '../../lib/i18n'

/** Alive or dead at a glance: green while alive (with its days), grey with the death date once dead. */
const props = defineProps<{ facts: Facts }>()
const text = computed(() => {
  const { life, days } = props.facts
  if (life.state === 'alive') return days === null ? t('Viva') : `${t('Viva')} · ${tn(days, '{n} día', '{n} días')}`
  if (life.state === 'dead') return life.death === null ? t('Muerta') : `${t('Muerta')} ${formatSerial(life.death)}`
  return 'Death_date NA'
})
</script>

<template>
  <span
    v-if="facts.life.state === 'alive'"
    class="inline-flex shrink-0 items-center gap-1 rounded-full bg-brand-50 px-2 py-0.5 text-xs font-semibold text-brand-800 ring-1 ring-brand-100"
  >
    <span class="h-2 w-2 rounded-full bg-brand-600" aria-hidden="true" />
    {{ text }}
  </span>
  <span
    v-else-if="facts.life.state === 'dead'"
    class="inline-flex shrink-0 items-center gap-1 rounded-full bg-stone-200 px-2 py-0.5 text-xs font-semibold text-stone-800"
  >
    {{ text }}
  </span>
  <span v-else class="inline-flex shrink-0 rounded-full bg-amber-100 px-2 py-0.5 text-xs font-semibold text-amber-900">
    {{ text }}
  </span>
</template>
