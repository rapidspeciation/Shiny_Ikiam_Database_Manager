<script setup lang="ts">
import { computed, ref } from 'vue'
import { VenusAndMars } from 'lucide-vue-next'
import { renameHost } from '../lib/renameHost'
import {
  FLAG_ORDER,
  databaseText,
  envelopeText,
  flagText,
  modelText,
  pairKey,
  runSexCheck,
  sexCheck,
  sexCounts,
  sexPairs,
  type SexFlag,
  type SexResult,
} from '../lib/sexCheck'

/**
 * The «Fotos» tab's sex check, above the frame (lib/sexCheck.ts, docs/sex-check.md): a line with the button and
 * the counts, and the butterflies to look at before uploading. Only a Supported model call against the database
 * is shown as a likely error; the rest are weaker or only informing.
 */
const pairs = computed(() => (renameHost.state?.reading.on ? [] : sexPairs(renameHost.state)))
const counts = computed(() => sexCounts(pairs.value))
const open = ref(true)
const all = ref(false)
const shown = computed(() => {
  const rows = pairs.value.map(p => sexCheck.results[pairKey(p)]).filter((r): r is SexResult => !!r)
  const rank = (r: SexResult) => (r.answer ? FLAG_ORDER.indexOf(r.answer.flag) : 1.5)
  return rows.filter(r => all.value || !r.answer || r.answer.flag === 'disagree' || r.answer.flag === 'check').sort((a, b) => rank(a) - rank(b))
})
const left = computed(() => pairs.value.length - counts.value.checked)
const STYLE: Record<SexFlag, string> = {
  disagree: 'bg-red-100 text-red-900',
  check: 'bg-amber-100 text-amber-900',
  agree: 'bg-emerald-50 text-emerald-900',
  uncertain: 'bg-stone-100 text-stone-700',
  none: 'bg-stone-50 text-stone-500',
}
</script>

<template>
  <div v-if="pairs.length" class="border-b border-stone-200 bg-white px-3 py-1 text-xs" data-sex-check>
    <div class="flex flex-wrap items-center gap-x-2 gap-y-1">
      <span class="flex items-center gap-1 font-medium text-stone-800"><VenusAndMars :size="14" /> {{ $t('Sexo') }}</span>
      <button
        v-if="!sexCheck.running"
        class="btn px-2 py-0.5"
        :title="$t('Compara el sexo del modelo (fotos d y v), el del sobre y el de la base, antes de subir las fotos')"
        data-sex-run
        @click="runSexCheck(pairs, { again: !left })"
      >
        {{ left ? $t('Comprobar {n} mariposas', { n: left }) : $t('Comprobar de nuevo') }}
      </button>
      <template v-else>
        <span class="tabular-nums text-stone-600" data-sex-progress>{{ $t('comprobando {done} de {total}…', { done: sexCheck.done, total: sexCheck.total }) }}</span>
        <button class="btn-ghost px-1.5 py-0.5" @click="sexCheck.stop = true">{{ $t('Parar') }}</button>
      </template>
      <span v-if="counts.disagree" class="rounded px-1.5 py-0.5" :class="STYLE.disagree" data-sex-count="disagree">{{ $t('{n} no coinciden', { n: counts.disagree }) }}</span>
      <span v-if="counts.check" class="rounded px-1.5 py-0.5" :class="STYLE.check" data-sex-count="check">{{ $t('{n} a revisar', { n: counts.check }) }}</span>
      <span v-if="counts.agree" class="rounded px-1.5 py-0.5" :class="STYLE.agree">{{ $t('{n} coinciden', { n: counts.agree }) }}</span>
      <span v-if="counts.uncertain" class="rounded px-1.5 py-0.5" :class="STYLE.uncertain" :title="$t('El modelo no está seguro: no se marcan como error')">{{
        $t('{n} inseguras', { n: counts.uncertain })
      }}</span>
      <span v-if="counts.errors" class="rounded bg-stone-100 px-1.5 py-0.5 text-stone-600">{{ $t('{n} sin leer', { n: counts.errors }) }}</span>
      <span class="ml-auto flex items-center gap-2">
        <label v-if="counts.checked" class="flex items-center gap-1 text-stone-600"><input v-model="all" type="checkbox" /> {{ $t('ver todas') }}</label>
        <button v-if="counts.checked" class="btn-ghost px-1.5 py-0.5" @click="open = !open">{{ open ? $t('Ocultar') : $t('Mostrar') }}</button>
      </span>
    </div>
    <table v-if="open && shown.length" class="mt-1 mb-0.5 block max-h-[32vh] w-full overflow-auto" data-sex-list>
      <thead class="text-left text-stone-500">
        <tr>
          <th class="pr-3 font-normal">CAM</th>
          <th class="pr-3 font-normal">{{ $t('Modelo') }}</th>
          <th class="pr-3 font-normal">{{ $t('Sobre') }}</th>
          <th class="pr-3 font-normal">{{ $t('Base') }}</th>
          <th class="font-normal" />
        </tr>
      </thead>
      <tbody>
        <tr v-for="r in shown" :key="pairKey(r)" class="border-t border-stone-100 align-top" :data-sex-flag="r.answer?.flag ?? 'error'" :data-cam="r.cam">
          <td class="pr-3 py-0.5 whitespace-nowrap">
            <span class="font-medium tabular-nums">{{ r.cam }}</span>
            <span v-if="r.answer?.species" class="block text-[11px] text-stone-500 italic">{{ r.answer.species }}</span>
          </td>
          <template v-if="r.answer">
            <td class="pr-3 py-0.5 whitespace-nowrap" :title="typeof r.answer.model.unreliable === 'string' ? r.answer.model.unreliable : ''">{{ modelText(r.answer) }}</td>
            <td class="pr-3 py-0.5 whitespace-nowrap" :title="r.answer.envelope?.line ?? ''">{{ envelopeText(r.answer) }}</td>
            <td class="pr-3 py-0.5 whitespace-nowrap">{{ databaseText(r.answer) }}</td>
            <td class="py-0.5">
              <span class="rounded px-1.5 py-0.5" :class="STYLE[r.answer.flag]">{{ flagText(r.answer) }}</span>
              <span class="ml-1 text-[11px] text-stone-400">{{ r.d }} · {{ r.v }}</span>
            </td>
          </template>
          <td v-else colspan="4" class="py-0.5 text-stone-500">{{ $t('No se pudo comprobar: {why}', { why: r.error ?? '' }) }}</td>
        </tr>
      </tbody>
    </table>
  </div>
</template>
