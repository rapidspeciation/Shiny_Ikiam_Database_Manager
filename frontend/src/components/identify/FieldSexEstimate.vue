<script setup lang="ts">
/**
 * Identificar's general sex line (team only, «En línea», experimental; lib/fieldSex.ts): butterflies outside the 13
 * Ithomiini genera of the in-hand line. A bar like the in-hand line's (SexEstimate.vue): macho on the left, hembra on
 * the right, the «no seguro» band in the middle, a marker at the photo's P(female).
 * - The six verified species: the sex and its % for a Supported answer outside the band (the % estimates how often an
 *   answer like this is right, never above what the fresh test measured); «No seguro» and the side it leans to inside
 *   the band; no % otherwise.
 * - Every other species: «estimado, sin verificar para esta especie»; the sex and its % outside the band, «No seguro
 *   (se inclina a hembra, 62 %)» inside it; the % comes from the level table (never above 95 %).
 * The view decides the state; this only shows it.
 */
import { computed } from 'vue'
import { locale } from '../../lib/i18n'
import { FIELD_SEX_BAND, type FieldSexState } from '../../lib/fieldSex'

/** `bare`: inside Identificar's sex panel (its own box and title): no box, no title here. */
const props = defineProps<{ state: FieldSexState; bare?: boolean }>()

const at = (p: number) => `${100 * Math.min(1, Math.max(0, p))}%`
const pct = (n: number) => `${new Intl.NumberFormat(locale.value === 'es' ? 'es' : 'en', { maximumFractionDigits: 0 }).format(n)} %`
/** The sex is named when it is outside the band; a % only for a call. */
const named = computed(() => props.state.status !== 'cell_under_90' && props.state.sure)
/** Under this many training photos the note says the % comes from similar species (calibration.json bins B1 and B2:
 * such a species never has its own level). */
const FEW_PHOTOS = 20
const colour = computed(() => (props.state.status === 'cell_under_90' || !props.state.sure ? null : props.state.sex))
</script>

<template>
  <section
    class="sex-box"
    :class="bare ? '' : ['mt-4 rounded-md border border-border border-l-4 bg-surface p-3', colour === 'female' ? 'border-l-fill-pink' : colour === 'male' ? 'border-l-fill-sky' : 'border-l-border-strong']"
    data-testid="field-sex"
    :data-status="state.status"
    :aria-label="$t('Sexo estimado')"
  >
    <div class="flex flex-wrap items-baseline gap-x-2 gap-y-1">
      <h2 v-if="!bare" class="font-semibold">{{ $t('Sexo estimado') }}</h2>
      <span class="rounded border border-border-strong px-1.5 py-0.5 text-xs text-fg-muted">{{ $t('experimental: mariposas vivas') }}</span>
      <span v-if="!state.verified" class="rounded border border-pending-edge px-1.5 py-0.5 text-xs text-pending-text" data-testid="field-sex-unverified">{{ $t('estimado, sin verificar para esta especie') }}</span>
    </div>

    <p
      class="mt-2 text-xl font-semibold"
      :class="colour === 'female' ? 'text-fill-pink' : colour === 'male' ? 'text-fill-sky' : 'text-fg-muted'"
      data-testid="field-sex-call"
    >
      <template v-if="state.status === 'cell_under_90'">{{ $t('Sin respuesta') }}</template>
      <template v-else-if="named">
        <template v-if="state.sex === 'female'">♀ {{ $t('Hembra') }}</template>
        <template v-else>♂ {{ $t('Macho') }}</template>
        <template v-if="state.pct != null">{{ ' ' }}<span class="tabular-nums" data-testid="field-sex-p">{{ pct(state.pct) }}</span></template>
      </template>
      <template v-else>
        {{ $t('No seguro') }}
        <span v-if="state.pct != null" class="text-base font-normal" data-testid="field-sex-lean">({{ state.sex === 'female' ? $t('se inclina a hembra, {pct}', { pct: pct(state.pct) }) : $t('se inclina a macho, {pct}', { pct: pct(state.pct) }) }})</span>
        <span v-else class="text-base font-normal" data-testid="field-sex-lean">({{ state.sex === 'female' ? $t('se inclina a hembra') : $t('se inclina a macho') }})</span>
      </template>
    </p>

    <div class="mt-2" aria-hidden="true">
      <div class="relative h-3 overflow-hidden rounded-sm">
        <div class="absolute inset-y-0 left-0 bg-fill-sky/30" :style="{ width: at(FIELD_SEX_BAND.lo) }" />
        <div class="sex-band absolute inset-y-0" :style="{ left: at(FIELD_SEX_BAND.lo), right: `calc(100% - ${at(FIELD_SEX_BAND.hi)})` }" />
        <div class="absolute inset-y-0 right-0 bg-fill-pink/30" :style="{ left: at(FIELD_SEX_BAND.hi) }" />
      </div>
      <div class="relative h-3">
        <div class="sex-marker absolute top-0 -translate-x-1/2" data-testid="field-sex-marker" :style="{ left: at(state.p) }" />
      </div>
      <div class="relative mt-0.5 h-4 text-xs leading-4 text-fg-muted">
        <span class="absolute left-0">♂ {{ $t('macho') }}</span>
        <span class="absolute left-1/2 -translate-x-1/2 whitespace-nowrap">{{ $t('no seguro') }}</span>
        <span class="absolute right-0">♀ {{ $t('hembra') }}</span>
      </div>
    </div>

    <p v-if="state.pct != null && state.verified" class="mt-2 text-xs text-fg-muted">{{ $t('El % estima cuántas veces acierta una respuesta así; nunca supera lo medido en la prueba.') }}</p>
    <p v-if="state.pct != null && !state.verified" class="mt-2 text-xs text-fg-muted" data-testid="field-sex-level">
      {{ $t('El % estima cuántas veces acierta una respuesta así; nunca supera el 95 %.') }}
      <template v-if="state.level === 'species'">{{ ' ' }}{{ $t('Viene de fotos de entrenamiento de esta especie.') }}</template>
      <template v-else-if="state.level === 'genus'">{{ ' ' }}{{ $t('Viene de especies de su género con un número parecido de fotos de entrenamiento.') }}</template>
      <template v-else-if="state.level === 'family'">{{ ' ' }}{{ $t('Viene de especies de su familia con un número parecido de fotos de entrenamiento.') }}</template>
      <template v-else>{{ ' ' }}{{ $t('Viene de todas las especies con un número parecido de fotos de entrenamiento.') }}</template>
    </p>
    <p v-if="!state.verified && state.pct != null && state.trainPhotos != null && state.trainPhotos < FEW_PHOTOS" class="mt-2 text-sm text-fg-muted" data-testid="field-sex-few">
      {{ $t('Hay pocas fotos de entrenamiento de esta especie ({n}): el % viene de especies parecidas, no de ella.', { n: state.trainPhotos }) }}
    </p>
    <p v-if="state.status === 'cell_under_90'" class="mt-2 text-sm text-fg-muted" data-testid="field-sex-cell">
      {{ $t('En la prueba, las respuestas de este sexo para esta especie acertaron menos del 90 %.') }}
    </p>
    <p v-if="state.note === 'species'" class="mt-2 text-sm text-pending-text" data-testid="field-sex-note">{{ $t('La especie no es segura; el sexo puede estar mal.') }}</p>
    <p v-else-if="state.note === 'wings'" class="mt-2 text-sm text-pending-text" data-testid="field-sex-note">{{ $t('No se encontraron las alas; el sexo puede estar mal.') }}</p>

    <ul v-if="!state.verified" class="mt-2 list-disc space-y-0.5 pl-4 text-xs text-fg-muted" data-testid="field-sex-notes-estimated">
      <li>{{ $t('Solo se comprobó con fotos nuevas en seis especies; esta no es una de ellas.') }}</li>
      <li>{{ $t('Las hembras suelen reconocerse peor que los machos.') }}</li>
      <li>{{ $t('No vale para ejemplares de colección ni montados.') }}</li>
    </ul>
    <ul v-else class="mt-2 list-disc space-y-0.5 pl-4 text-xs text-fg-muted">
      <li data-testid="field-sex-tested">{{ $t('Probado en fotos de mariposas vivas.') }}</li>
      <li>{{ $t('Comprobado en fotos de iNaturalist de seis especies: Danaus plexippus, Hypolimnas bolina, Hypolimnas misippus, Papilio polyxenes, Papilio troilus y Poanes zabulon.') }}</li>
      <li>{{ $t('Las hembras se reconocen peor: en la prueba, cuando respondió, acertó con el 93 % de las hembras y el 99 % de los machos.') }}</li>
      <li>{{ $t('No vale para ejemplares de colección ni montados.') }}</li>
      <li>{{ $t('Los % se fijaron con esa misma prueba: sin validar hasta la próxima.') }}</li>
    </ul>
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
