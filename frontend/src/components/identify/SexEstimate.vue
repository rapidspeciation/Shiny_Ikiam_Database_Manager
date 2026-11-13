<script setup lang="ts">
/**
 * Identificar's sex estimate for the team (signed in only; lib/sexModel.ts): the call (hembra / macho / no seguro),
 * the model's P(female), and the scale with the in-hand cutoff and the «no seguro» band. Experimental: in-hand
 * photos only. The view decides the state; this only shows it.
 */
import { computed } from 'vue'
import { locale, t } from '../../lib/i18n'
import { SEX_DEFAULT, sexShown, sexShownFemale, type SexCalibration } from '../../lib/sexModel'

export type SexState =
  | { kind: 'download'; mb: number }
  | { kind: 'downloading'; done: number; total: number }
  | { kind: 'running' }
  | { kind: 'error'; message: string }
  | { kind: 'genus'; genus: string }
  | { kind: 'nowings' }
  /** `calibration`: the model's that gave the score (lib/sexModel.ts; model B's when not given). */
  | { kind: 'ready'; z: number; calibration?: SexCalibration }

/** `bare`: inside Identificar's sex panel (its own box and title): no box, no title here. */
const props = defineProps<{ state: SexState; bare?: boolean }>()
defineEmits<{ download: [] }>()

/** The model's calibration: its cutoff and band (logits) and display. */
const cal = computed(() => (props.state.kind === 'ready' && props.state.calibration) || SEX_DEFAULT)
const CUT = computed(() => cal.value.cut)
/**
 * The scale, from the model's own band: 2.6 logit below lo to 2.1 above hi (model B: -0.98 to 6.97, as its -1 to 7
 * before; model C: -2.79 to 3.30, where its team scores lie). No fixed floor or ceiling: a model whose scores sit
 * lower would otherwise leave part of the bar empty.
 */
const LO = computed(() => CUT.value.lo - 2.6)
const HI = computed(() => CUT.value.hi + 2.1)
const at = (z: number) => `${(100 * (Math.min(HI.value, Math.max(LO.value, z)) - LO.value)) / (HI.value - LO.value)}%`
const fmt = (v: number, digits: number) => new Intl.NumberFormat(locale.value === 'es' ? 'es' : 'en', { minimumFractionDigits: digits, maximumFractionDigits: digits }).format(v)
/** The calibrated % (lib/sexModel.ts SEX_DISPLAY): whole numbers, never a sure-looking 100 %. */
const pct = (p: number) => (p >= 0.995 ? `> ${fmt(99, 0)} %` : `${fmt(p * 100, 0)} %`)
/** The scale's marks in calibrated P(hembra), one decimal (the band edges are 39.7 and 71.8 %), marked ♀. */
const tick = (z: number) => `♀ ${fmt(100 * sexShownFemale(z, cal.value.display), 1)} %`
const z = computed(() => (props.state.kind === 'ready' ? props.state.z : null))
const shown = computed(() => (z.value == null ? null : sexShown(z.value, cal.value)))
const call = computed(() => shown.value?.call ?? null)
const ticks = computed(() => ({ lo: tick(CUT.value.lo), t: `${t('corte')} ${tick(CUT.value.t)}`, hi: tick(CUT.value.hi) }))
</script>

<template>
  <section
    class="sex-box"
    :class="bare ? '' : ['mt-4 rounded-md border border-border border-l-4 bg-surface p-3', call === 'hembra' ? 'border-l-fill-pink' : call === 'macho' ? 'border-l-fill-sky' : 'border-l-border-strong']"
    data-testid="sex-estimate"
    :aria-label="$t('Sexo estimado')"
  >
    <div class="flex flex-wrap items-baseline gap-x-2 gap-y-1">
      <h2 v-if="!bare" class="font-semibold">{{ $t('Sexo estimado') }}</h2>
      <span class="rounded border border-border-strong px-1.5 py-0.5 text-xs text-fg-muted">{{ $t('experimental: fotos en mano') }}</span>
    </div>

    <template v-if="state.kind === 'ready' && z != null">
      <div class="mt-2">
        <p class="text-xl font-semibold" :class="call === 'hembra' ? 'text-sex-female' : call === 'macho' ? 'text-sex-male' : 'text-fg-muted'" data-testid="sex-call">
          <template v-if="call === 'hembra'">♀ {{ $t('Hembra') }} <span class="tabular-nums" data-testid="sex-p">{{ pct(shown!.p) }}</span></template>
          <template v-else-if="call === 'macho'">♂ {{ $t('Macho') }} <span class="tabular-nums" data-testid="sex-p">{{ pct(shown!.p) }}</span></template>
          <template v-else>{{ $t('No seguro') }} <span class="tabular-nums" data-testid="sex-p">({{ shown!.sign }} {{ pct(shown!.p) }})</span></template>
        </p>
      </div>
      <div class="mt-2" aria-hidden="true">
        <!--
          The labels never overlap, however close the marks (model C: the cutoff and hi are 0.2 logit apart): the
          cutoff's label has its own row above the bar; below it, lo's label ends at lo and hi's starts at hi.
        -->
        <div class="relative h-4 text-[0.7rem] leading-4 text-fg-muted tabular-nums">
          <span class="absolute -translate-x-1/2 whitespace-nowrap" data-testid="sex-tick-t" :style="{ left: at(CUT.t) }">{{ ticks.t }}</span>
        </div>
        <div class="relative h-3 overflow-hidden rounded-sm">
          <div class="absolute inset-y-0 left-0 bg-fill-sky/30" :style="{ width: at(CUT.lo) }" />
          <div class="sex-band absolute inset-y-0" :style="{ left: at(CUT.lo), right: `calc(100% - ${at(CUT.hi)})` }" />
          <div class="absolute inset-y-0 right-0 bg-fill-pink/30" :style="{ left: at(CUT.hi) }" />
          <div class="absolute inset-y-0 w-px bg-fg-muted" :style="{ left: at(CUT.t) }" />
        </div>
        <div class="relative h-3">
          <div class="sex-marker absolute top-0 -translate-x-1/2" :style="{ left: at(z) }" />
        </div>
        <div class="relative h-4 text-[0.7rem] leading-4 text-fg-muted tabular-nums">
          <span class="absolute -translate-x-full whitespace-nowrap pr-0.5" data-testid="sex-tick-lo" :style="{ left: at(CUT.lo) }">{{ ticks.lo }}</span>
          <span class="absolute whitespace-nowrap pl-0.5" data-testid="sex-tick-hi" :style="{ left: at(CUT.hi) }">{{ ticks.hi }}</span>
        </div>
        <div class="relative mt-0.5 h-4 text-xs leading-4 text-fg-muted">
          <span class="absolute left-0">♂ {{ $t('macho') }}</span>
          <span class="absolute -translate-x-1/2 whitespace-nowrap" :style="{ left: `calc((${at(CUT.lo)} + ${at(CUT.hi)}) / 2)` }">{{ $t('no seguro') }}</span>
          <span class="absolute right-0">♀ {{ $t('hembra') }}</span>
        </div>
      </div>
      <p class="sr-only">
        {{ $t('Corte en fotos en mano: {t}; no seguro entre {lo} y {hi}.', { t: tick(CUT.t), lo: ticks.lo, hi: ticks.hi }) }}
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
