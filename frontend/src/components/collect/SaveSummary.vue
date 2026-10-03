<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted } from 'vue'
import { Save, X } from 'lucide-vue-next'
import { isEmptyDraft, summarize, type Draft } from '../../lib/collect'
import { formatSerial, isoToSerial, todayIso, weekdayOf } from '../../lib/dates'
import { t } from '../../lib/i18n'

/**
 * Read over before saving a Colecta (cards and table alike): the day (a list
 * typed days later was saved as today), places, sexes sent to the insectary,
 * the CAM range, species by sex, people and weather.
 */
const props = defineProps<{ drafts: Draft[]; date: string; saving: boolean }>()
const emit = defineEmits<{ close: []; confirm: [] }>()

const summary = computed(() => summarize(props.drafts))
const isToday = computed(() => props.date === todayIso())
const longDate = computed(() => (props.date ? `${weekdayOf(props.date)} ${formatSerial(isoToSerial(props.date))}` : t('sin fecha')))
/** The people and weather of the list's rows. */
const distinct = (field: 'collector' | 'identifier' | 'rainfall' | 'cloud') =>
  [
    ...new Set(
      props.drafts
        .filter(d => !isEmptyDraft(d))
        .map(d => d[field])
        .filter(Boolean),
    ),
  ].join(', ') || '—'
// Escape closes it wherever the focus is (the dialog itself is not focused when it opens).
const onKey = (e: KeyboardEvent) => e.key === 'Escape' && emit('close')
onMounted(() => window.addEventListener('keydown', onKey))
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
</script>

<template>
  <div class="fixed inset-0 z-40 grid place-items-center bg-black/40 p-2" @click.self="emit('close')">
    <section class="flex max-h-[90vh] w-full max-w-lg flex-col rounded-lg bg-white shadow-xl" role="dialog" :aria-label="$t('Guardar colecta')">
      <header class="flex items-center border-b border-stone-200 px-4 py-3">
        <h2 class="flex-1 text-lg font-semibold">{{ $t('Guardar {n} mariposas', { n: drafts.length }) }}</h2>
        <button class="btn-ghost" :aria-label="$t('Cerrar')" @click="emit('close')"><X :size="20" /></button>
      </header>
      <div class="flex-1 space-y-2 overflow-y-auto px-4 py-3 text-sm">
        <p class="text-base">
          <strong class="capitalize">{{ longDate }}</strong> · {{ summary.places.join(', ') }}
        </p>
        <p v-if="isToday" class="rounded bg-amber-50 px-2 py-1 text-amber-800">
          {{ $t('La fecha es hoy. Si pasas a limpio una colecta de otro día, cambia Collection_date antes de guardar.') }}
        </p>
        <p>
          {{ $t('Al insectario:') }} <strong>{{ summary.insectary.female }} ♀ · {{ summary.insectary.male }} ♂</strong
          ><template v-if="summary.insectary.other"> · {{ $t('{n} sin sexo', { n: summary.insectary.other }) }}</template>
          <br />
          {{ $t('Preservadas:') }} <strong>{{ summary.preserved }}</strong
          ><template v-if="summary.cams">
            ({{ summary.cams.first }}<template v-if="summary.cams.first !== summary.cams.last"> – {{ summary.cams.last }}</template
            ><template v-if="!summary.cams.consecutive">, {{ $t('con saltos') }}</template>)</template
          >
          <template v-if="summary.released"
            ><br />{{ $t('Liberadas:') }} <strong>{{ summary.released }}</strong></template
          >
        </p>
        <table class="w-full">
          <thead class="text-left text-xs text-stone-500">
            <tr>
              <th class="py-1">{{ $t('Especie') }}</th>
              <th class="w-10 text-right">♀</th>
              <th class="w-10 text-right">♂</th>
              <th class="w-10 text-right">?</th>
            </tr>
          </thead>
          <tbody>
            <tr v-for="s in summary.species" :key="s.name" class="border-t border-stone-100">
              <td class="py-1 pr-2">{{ s.name }}</td>
              <td class="text-right tabular-nums">{{ s.female || '' }}</td>
              <td class="text-right tabular-nums">{{ s.male || '' }}</td>
              <td class="text-right tabular-nums">{{ s.other || '' }}</td>
            </tr>
          </tbody>
        </table>
        <p class="text-stone-600">
          Collector: {{ distinct('collector') }} · Identifier: {{ distinct('identifier') }} · Rainfall: {{ distinct('rainfall') }} ·
          Cloud_cover: {{ distinct('cloud') }}
        </p>
      </div>
      <footer class="flex justify-end gap-2 border-t border-stone-200 px-4 py-3">
        <button class="btn h-11" @click="emit('close')">{{ $t('Volver') }}</button>
        <button class="btn-primary h-11" :disabled="saving" @click="emit('confirm')"><Save :size="15" /> {{ $t('Guardar en la hoja') }}</button>
      </footer>
    </section>
  </div>
</template>
