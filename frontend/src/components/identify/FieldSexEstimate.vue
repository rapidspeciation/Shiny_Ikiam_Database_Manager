<script setup lang="ts">
/**
 * Identificar's general sex line (team only, «En línea», experimental; lib/fieldSex.ts): live butterflies of six
 * species outside the Ithomiini. First one question (is the photo of a live butterfly?), then the call and the % of
 * that call that was right on the fresh test. Same look as the in-hand line (SexEstimate.vue). The view decides the
 * state; this only shows it and reports the answer.
 */
import { locale } from '../../lib/i18n'
import type { FieldSexState, LiveAnswer } from '../../lib/fieldSex'

defineProps<{ state: FieldSexState }>()
defineEmits<{ live: [answer: LiveAnswer | undefined] }>()

const pct = (n: number) => `${new Intl.NumberFormat(locale.value === 'es' ? 'es' : 'en', { maximumFractionDigits: 0 }).format(n)} %`
</script>

<template>
  <section
    class="sex-box mt-4 rounded-md border border-border border-l-4 bg-surface p-3"
    :class="state.kind === 'call' ? (state.sex === 'female' ? 'border-l-fill-pink' : 'border-l-fill-sky') : 'border-l-border-strong'"
    data-testid="field-sex"
    :aria-label="$t('Sexo estimado')"
  >
    <div class="flex flex-wrap items-baseline gap-x-2 gap-y-1">
      <h2 class="font-semibold">{{ $t('Sexo estimado') }}</h2>
      <span class="rounded border border-border-strong px-1.5 py-0.5 text-xs text-fg-muted">{{ $t('experimental: mariposas vivas') }}</span>
    </div>

    <div v-if="state.kind === 'ask'" class="mt-2">
      <p class="text-sm" data-testid="field-sex-ask">{{ $t('¿Es una foto de una mariposa viva (no de colección ni montada)?') }}</p>
      <div class="mt-2 flex flex-wrap gap-2">
        <button class="btn px-3 py-1.5" data-testid="field-sex-yes" @click="$emit('live', 'yes')">{{ $t('Sí, viva') }}</button>
        <button class="btn px-3 py-1.5" data-testid="field-sex-no" @click="$emit('live', 'no')">{{ $t('No') }}</button>
      </div>
    </div>

    <div v-else-if="state.kind === 'call'" class="mt-2">
      <p class="text-xl font-semibold" :class="state.sex === 'female' ? 'text-fill-pink' : 'text-fill-sky'" data-testid="field-sex-call">
        <template v-if="state.sex === 'female'">♀ {{ $t('Hembra') }}</template>
        <template v-else>♂ {{ $t('Macho') }}</template>
        {{ ' ' }}<span class="tabular-nums" data-testid="field-sex-p">{{ pct(state.pct) }}</span>
      </p>
      <p class="mt-1 text-xs text-fg-muted">{{ $t('El % es cuántas veces acertó esta llamada en fotos de prueba de esta especie.') }}</p>
    </div>

    <p v-else class="mt-2 text-sm text-fg-muted" data-testid="field-sex-declined">
      {{ $t('No se muestra: el modelo solo se comprobó en fotos de mariposas vivas.') }}
    </p>

    <button v-if="state.kind !== 'ask'" class="mt-1 text-xs text-brand-text hover:underline" data-testid="field-sex-change" @click="$emit('live', undefined)">
      {{ $t('cambiar') }}
    </button>

    <ul class="mt-2 list-disc space-y-0.5 pl-4 text-xs text-fg-muted">
      <li>{{ $t('Comprobado en fotos de iNaturalist de mariposas vivas de seis especies: Danaus plexippus, Hypolimnas bolina, Hypolimnas misippus, Papilio polyxenes, Papilio troilus y Poanes zabulon.') }}</li>
      <li>{{ $t('Las hembras se reconocen peor: en la prueba acertó con el 93 % de las hembras y el 99 % de los machos.') }}</li>
      <li>{{ $t('No vale para ejemplares de colección ni montados.') }}</li>
      <li>{{ $t('Los % se fijaron con esa misma prueba: sin validar hasta la próxima.') }}</li>
    </ul>
  </section>
</template>
