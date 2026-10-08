<script setup lang="ts">
import { computed } from 'vue'
import SpeciesPhoto from './SpeciesPhoto.vue'
import { OTHER, SERIES } from '../charts/chart'
import { MARK_THRESHOLD, type SpeciesStat } from '../../lib/monitoring'

/**
 * One species of the report: its photo, the 30 rule as a bar, and its
 * individuals split by fate and by sex as thin stacked bars (the colours of
 * the report's charts: fates as in Individuos por mes, sexes as in Proporción
 * de sexos).
 */
const props = defineProps<{
  s: SpeciesStat
  rule: { applies: boolean; preserved: number; done: boolean; missing: number }
}>()

type Part = { key: string; label: string; n: number; color: string }
const share = (parts: Part[], total: number) => parts.filter(p => p.n).map(p => ({ ...p, width: `${(100 * p.n) / total}%` }))
const fates = computed(() =>
  share(
    [
      { key: 'preserved', label: 'preserv.', n: props.s.preserved, color: SERIES[0] },
      { key: 'marked', label: 'marcados', n: props.s.marked, color: SERIES[1] },
      { key: 'recaptured', label: 'recapt.', n: props.s.recaptured, color: SERIES[2] },
      { key: 'other', label: 'otros', n: props.s.other, color: OTHER },
    ],
    props.s.total,
  ),
)
const sexes = computed(() =>
  share(
    [
      { key: 'female', label: '♀', n: props.s.female, color: '#e34948' },
      { key: 'male', label: '♂', n: props.s.male, color: SERIES[0] },
      { key: 'unknown', label: '?', n: props.s.total - props.s.female - props.s.male, color: '#e7e5e4' },
    ],
    props.s.total,
  ),
)
const ruleWidth = computed(() => `${Math.min(100, (100 * props.rule.preserved) / MARK_THRESHOLD)}%`)
</script>

<template>
  <article class="flex flex-col overflow-hidden rounded-lg border border-stone-200 bg-white">
    <div class="relative aspect-[4/3] bg-stone-100">
      <SpeciesPhoto :species="s.species" :subspecies="s.subspecies" />
      <span
        class="pointer-events-none absolute top-2 right-2 rounded-full bg-white/90 px-2 py-0.5 text-xs font-semibold tabular-nums shadow-sm"
        :title="$t('{n} individuos', { n: s.total })"
        >{{ s.total }}</span
      >
    </div>
    <div class="flex flex-1 flex-col gap-2.5 p-3">
      <h3 class="text-sm leading-snug">
        <i class="font-medium">{{ s.species === 'Sin especie' ? $t('Sin especie') : s.species }}</i>
        <span v-if="s.subspecies" class="text-stone-500"> {{ s.subspecies }}</span>
      </h3>

      <div>
        <div class="flex items-baseline justify-between gap-2 text-[11px]">
          <span class="shrink-0 whitespace-nowrap text-stone-500">{{ $t('Regla de {n}', { n: MARK_THRESHOLD }) }}</span>
          <span
            v-if="rule.applies"
            class="text-right font-medium tabular-nums"
            :class="rule.done ? 'text-brand-700' : 'text-amber-800'"
          >
            {{ rule.preserved }}/{{ MARK_THRESHOLD }} ·
            {{ rule.done ? $t('marcar y liberar') : $t('preservar (faltan {n})', { n: rule.missing }) }}
          </span>
          <span v-else class="text-stone-400">{{ $t('no aplica (no es Ithomiini)') }}</span>
        </div>
        <div class="mt-1 h-1.5 overflow-hidden rounded-full bg-stone-100">
          <div
            v-if="rule.applies"
            class="h-full rounded-full"
            :class="rule.done ? 'bg-brand-600' : 'bg-amber-500'"
            :style="{ width: ruleWidth }"
          />
        </div>
      </div>

      <div v-for="bar in [fates, sexes]" :key="bar[0]?.key">
        <div class="flex h-1.5 gap-px overflow-hidden rounded-full bg-stone-100">
          <div v-for="p in bar" :key="p.key" class="h-full" :style="{ width: p.width, background: p.color }" />
        </div>
        <div class="mt-1 flex flex-wrap gap-x-2.5 gap-y-0.5 text-[11px] text-stone-600 tabular-nums">
          <span v-for="p in bar" :key="p.key" class="inline-flex items-center gap-1">
            <span class="size-1.5 rounded-full" :style="{ background: p.color }" />
            <b class="font-medium text-stone-800">{{ p.n }}</b> {{ $t(p.label) }}
          </span>
        </div>
      </div>
    </div>
  </article>
</template>
