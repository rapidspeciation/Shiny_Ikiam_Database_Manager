<script setup lang="ts">
/**
 * One sex estimate beside the sheet's sex in Dudas (the field photo's model, or the collection model on the project
 * photos): the call with the % of the sex called, «no seguro (♀ 62 %)» in a neutral colour, and ✓ or ✗ against the
 * row's sex; or why there is none. `note`: said on hover only (the collection model's % is not calibrated).
 */
export type SexEvidence =
  | { kind: 'call'; call: 'hembra' | 'macho' | 'no seguro'; sign: '♀' | '♂'; p: number; agree: boolean | null; weak?: string; note?: string }
  | { kind: 'reason'; text: string }
  | { kind: 'running' }

defineProps<{ evidence: SexEvidence }>()
const pct = (p: number) => (p >= 0.995 ? '> 99 %' : `${Math.round(p * 100)} %`)
</script>

<template>
  <span v-if="evidence.kind === 'call'" class="inline-flex items-baseline gap-1 whitespace-nowrap" :title="[evidence.weak, evidence.note].filter(Boolean).join('. ') || undefined">
    <span
      class="tabular-nums"
      :class="evidence.call === 'no seguro' || evidence.weak ? 'text-stone-600' : evidence.call === 'hembra' ? 'sex-f' : 'sex-m'"
      ><template v-if="evidence.call === 'no seguro'">{{ $t('no seguro') }} ({{ evidence.sign }} {{ pct(evidence.p) }})</template
      ><template v-else>{{ evidence.sign }} {{ pct(evidence.p) }}</template></span
    >
    <span
      v-if="evidence.agree !== null"
      class="text-xs"
      :class="evidence.weak ? 'text-stone-500' : evidence.agree ? 'text-green-700' : 'font-semibold text-amber-800'"
      :title="evidence.agree ? $t('Coincide con la fila') : $t('No coincide con la fila')"
      >{{ evidence.agree ? '✓' : '✗' }}</span
    >
  </span>
  <span v-else-if="evidence.kind === 'reason'" class="text-xs text-stone-500">{{ evidence.text }}</span>
  <span v-else class="text-xs text-stone-500">{{ $t('calculando…') }}</span>
</template>

<style scoped>
.sex-f {
  color: var(--color-fill-pink);
}
.sex-m {
  color: var(--color-fill-sky);
}
[data-theme='dark'] .sex-f {
  color: oklch(80% 0.11 4);
}
[data-theme='dark'] .sex-m {
  color: oklch(80% 0.09 242);
}
</style>
