<script setup lang="ts">
import { computed } from 'vue'
import SexBadge from './SexBadge.vue'
import LifeBadge from './deaths/LifeBadge.vue'
import { formatSerial } from '../lib/dates'
import type { Facts } from '../lib/deaths'
import { t } from '../lib/i18n'

/**
 * One butterfly offered for an Insectary ID typed (Muertes, Censo): the ID,
 * with the characters read as a look-alike lit in amber; species, sex, clutch
 * and the day it entered the insectary (emerged, or caught in the wild); alive
 * (with its days) or dead (with the date). `greyed`: another species than the
 * one looked for, or not in the insectary any more.
 */
const props = withDefaults(
  defineProps<{
    id: string
    facts: Facts
    /** Positions of `id` read as a look-alike. */
    at?: number[]
    /** Found by its CAM or tube: which one. */
    via?: string
    greyed?: boolean
    /** A line about it here (e.g. "seen by Ana 10:31"). */
    tag?: string
  }>(),
  { at: () => [], via: undefined, greyed: false, tag: undefined },
)
const chars = computed(() => [...props.id].map((c, i) => ({ c, lit: props.at.includes(i) })))
const line = computed(() => {
  const f = props.facts
  return [
    f.clutch && t('clutch {c}', { c: f.clutch }),
    f.entered !== null &&
      (f.wild
        ? t('Capturada {date}', { date: formatSerial(f.entered) })
        : t('Emergió {date}', { date: formatSerial(f.entered) })),
  ]
    .filter(Boolean)
    .join(' · ')
})
</script>

<template>
  <span class="flex min-w-0 flex-1 items-center gap-3">
    <span
      class="w-20 shrink-0 text-lg font-semibold tracking-wide"
      :class="greyed ? 'text-stone-400' : 'text-stone-900'"
      :title="at.length ? $t('Los caracteres en ámbar se leyeron como parecidos') : undefined"
    >
      <template v-for="(x, i) in chars" :key="i"
        ><span v-if="x.lit" class="rounded-sm bg-amber-200 px-px text-amber-950">{{ x.c }}</span
        ><template v-else>{{ x.c }}</template></template
      >
    </span>
    <span class="min-w-0 flex-1" :class="greyed ? 'opacity-55' : ''">
      <span class="block truncate text-sm">
        {{ facts.species || '—' }}
        <span v-if="facts.wild" class="ml-1 rounded bg-stone-100 px-1 text-xs text-stone-600">Wild-caught</span>
      </span>
      <span class="flex items-center gap-1.5 truncate text-xs text-stone-500"><SexBadge :sex="facts.sex" />{{ line }}</span>
      <span v-if="via" class="block truncate text-xs text-brand-700">{{ via }}</span>
      <span v-if="tag" class="block truncate text-xs font-semibold text-brand-800">{{ tag }}</span>
    </span>
    <LifeBadge :facts="facts" />
  </span>
</template>
