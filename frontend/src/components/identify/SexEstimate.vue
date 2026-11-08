<script setup lang="ts">
/**
 * Identificar's sex estimate for the team (signed in only; lib/sexModel.ts): the call (hembra / macho / no seguro),
 * the model's P(female), and the scale with the in-hand cutoff and the «no seguro» band. Experimental: in-hand
 * photos only. The view decides the state; this only shows it.
 */
import { computed } from 'vue'
import { locale } from '../../lib/i18n'
import { SEX_CUT, sexCall, sigmoid } from '../../lib/sexModel'

export type SexState =
  | { kind: 'download'; mb: number }
  | { kind: 'downloading'; done: number; total: number }
  | { kind: 'running' }
  | { kind: 'error'; message: string }
  | { kind: 'genus'; genus: string }
  | { kind: 'nowings' }
  | { kind: 'ready'; z: number }

const props = defineProps<{ state: SexState }>()
defineEmits<{ download: [] }>()

/** The scale: logit -1 to 7 (P 27 % to 99.9 %), where the cutoff and the band sit apart. */
const LO = -1, HI = 7
const at = (z: number) => `${(100 * (Math.min(HI, Math.max(LO, z)) - LO)) / (HI - LO)}%`
const pct = (p: number) => {
  const f = new Intl.NumberFormat(locale.value === 'es' ? 'es' : 'en', { maximumFractionDigits: p >= 0.9 && p < 0.9995 ? 1 : 0 })
  return p >= 0.9995 ? '> 99.9 %' : `${f.format(p * 100)} %`
}
const z = computed(() => (props.state.kind === 'ready' ? props.state.z : null))
const call = computed(() => (z.value == null ? null : sexCall(z.value)))
const ticks = [
  { z: SEX_CUT.lo, label: pct(sigmoid(SEX_CUT.lo)) },
  { z: SEX_CUT.t, label: pct(sigmoid(SEX_CUT.t)) },
  { z: SEX_CUT.hi, label: pct(sigmoid(SEX_CUT.hi)) },
]
</script>

<template>
  <section
    class="sex-box mt-4 rounded-md border border-border border-l-4 bg-surface p-3"
    :class="call === 'hembra' ? 'border-l-fill-pink' : call === 'macho' ? 'border-l-fill-sky' : 'border-l-border-strong'"
    data-testid="sex-estimate"
    :aria-label="$t('Sexo estimado')"
  >
    <div class="flex flex-wrap items-baseline gap-x-2 gap-y-1">
      <h2 class="font-semibold">{{ $t('Sexo estimado') }}</h2>
      <span class="rounded border border-border-strong px-1.5 py-0.5 text-xs text-fg-muted">{{ $t('experimental: fotos en mano') }}</span>
    </div>

    <template v-if="state.kind === 'ready' && z != null">
      <div class="mt-2 flex items-baseline justify-between gap-3">
        <p class="text-xl font-semibold" :class="call === 'hembra' ? 'text-fill-pink' : call === 'macho' ? 'text-fill-sky' : 'text-fg-muted'" data-testid="sex-call">
          <template v-if="call === 'hembra'">♀ {{ $t('Hembra') }}</template>
          <template v-else-if="call === 'macho'">♂ {{ $t('Macho') }}</template>
          <template v-else>{{ $t('No seguro') }}</template>
        </p>
        <p class="text-sm whitespace-nowrap tabular-nums" data-testid="sex-p">P({{ $t('hembra') }}) {{ pct(sigmoid(z)) }}</p>
      </div>
      <div class="mt-2" aria-hidden="true">
        <div class="relative h-3 overflow-hidden rounded-sm">
          <div class="absolute inset-y-0 left-0 bg-fill-sky/30" :style="{ width: at(SEX_CUT.lo) }" />
          <div class="sex-band absolute inset-y-0" :style="{ left: at(SEX_CUT.lo), right: `calc(100% - ${at(SEX_CUT.hi)})` }" />
          <div class="absolute inset-y-0 right-0 bg-fill-pink/30" :style="{ left: at(SEX_CUT.hi) }" />
          <div class="absolute inset-y-0 w-px bg-fg-muted" :style="{ left: at(SEX_CUT.t) }" />
        </div>
        <div class="relative h-3">
          <div class="sex-marker absolute top-0 -translate-x-1/2" :style="{ left: at(z) }" />
        </div>
        <div class="relative h-4 text-[0.7rem] leading-4 text-fg-muted tabular-nums">
          <span v-for="(tk, i) in ticks" :key="i" class="absolute -translate-x-1/2 whitespace-nowrap" :style="{ left: at(tk.z) }">{{ tk.label }}</span>
        </div>
        <div class="mt-0.5 flex justify-between text-xs text-fg-muted">
          <span>♂ {{ $t('macho') }}</span><span>{{ $t('no seguro') }}</span><span>♀ {{ $t('hembra') }}</span>
        </div>
      </div>
      <p class="sr-only">
        {{ $t('Corte en fotos en mano: {t}; no seguro entre {lo} y {hi}.', { t: ticks[1].label, lo: ticks[0].label, hi: ticks[2].label }) }}
      </p>
    </template>

    <p v-else-if="state.kind === 'genus'" class="mt-1 text-sm text-fg-muted" data-testid="sex-na">
      {{ $t('No aplica a {genus}: el modelo no se entrenó con este género.', { genus: state.genus }) }}
    </p>
    <p v-else-if="state.kind === 'nowings'" class="mt-1 text-sm text-fg-muted" data-testid="sex-na">
      {{ $t('No aplica: no se encontraron las alas en la foto.') }}
    </p>
    <p v-else-if="state.kind === 'running'" class="mt-1 text-sm text-fg-muted">{{ $t('Calculando…') }}</p>
    <p v-else-if="state.kind === 'error'" class="mt-1 text-sm text-error-text" role="alert">{{ state.message }}</p>
    <div v-else-if="state.kind === 'download'" class="mt-2">
      <button class="btn px-3 py-1.5" data-testid="sex-download" @click="$emit('download')">{{ $t('Descargar el modelo ({mb} MB)', { mb: state.mb }) }}</button>
      <p class="mt-1 text-xs text-fg-subtle">{{ $t('Una sola vez: queda en este teléfono.') }}</p>
    </div>
    <div v-else-if="state.kind === 'downloading'" class="mt-2">
      <div class="h-2 overflow-hidden rounded-sm bg-surface-sunken" role="progressbar" :aria-valuenow="Math.round((100 * state.done) / Math.max(1, state.total))" aria-valuemin="0" aria-valuemax="100">
        <div class="h-full bg-brand" :style="{ width: `${(100 * state.done) / Math.max(1, state.total)}%` }" />
      </div>
      <p class="mt-1 text-xs text-fg-muted tabular-nums">{{ $t('Descargando: {done} de {total} MB', { done: Math.round(state.done / 1e6), total: Math.round(state.total / 1e6) }) }}</p>
    </div>

    <p v-if="state.kind !== 'genus'" class="mt-2 text-xs text-fg-muted">
      {{ $t('Entrenado con compuestos de alas de museo y comprobado solo en fotos en mano: no vale para fotos de mariposas libres.') }}
    </p>
  </section>
</template>

<style scoped>
.sex-band {
  background: repeating-linear-gradient(135deg, var(--color-border-strong) 0 3px, transparent 3px 7px);
  opacity: 0.6;
}
.sex-marker {
  width: 0;
  height: 0;
  border-left: 6px solid transparent;
  border-right: 6px solid transparent;
  border-bottom: 9px solid var(--color-fg);
}
</style>
