<script setup lang="ts">
import { computed, ref } from 'vue'
import { ArrowLeft, BookCheck, Copy, RotateCcw, Smile } from 'lucide-vue-next'
import CensusCounts from './CensusCounts.vue'
import { useCensus } from '../../composables/useCensus'
import { findingsOf, notebookLines, type Finding } from '../../lib/census'
import { dayLabel, isoToSerial } from '../../lib/dates'
import { notify } from '../../lib/notice'
import type { Table } from '../../lib/types'
import { useSession } from '../../stores/session'
import { t } from '../../lib/i18n'

/**
 * A census done (or cancelled): its counts, where its disappearances are
 * (in the app until «Guardar en Google Sheets», or in the sheet), the lines to
 * bring the paper notebook up to date (in Insectary ID order: ☺ for seen, the
 * day for disappeared), the findings, and «Reabrir» while its disappearances
 * are still only in the app.
 */
defineProps<{ table: Table | undefined }>()
const emit = defineEmits<{ leave: [] }>()
const census = useCensus()
const session = useSession()
const detail = computed(() => census.detail.value!)
const c = computed(() => detail.value.census)
const serial = computed(() => isoToSerial(c.value.day))
const lines = computed(() =>
  notebookLines(detail.value.roster, serial.value, { seen: '☺', disappeared: t('desaparecida'), excluded: t('no contada') }),
)
/** The day as the notebook writes it (5/10/26), said once above the list instead of on every line. */
const notebookDay = computed(() => lines.value.find(l => l.status === 'disappeared')?.text.split(' ').pop() ?? '')
const counts = computed(() => ({
  seen: lines.value.filter(l => l.status === 'seen').length,
  disappeared: lines.value.filter(l => l.status === 'disappeared').length,
  excluded: lines.value.filter(l => l.status === 'excluded').length,
}))
/** Which lines the list shows: all (the notebook's order), or one kind to find them quickly. */
const showing = ref<'all' | 'seen' | 'disappeared' | 'excluded'>('all')
const shown = computed(() => (showing.value === 'all' ? lines.value : lines.value.filter(l => l.status === showing.value)))
const findings = computed(() => findingsOf(c.value.species, detail.value.roster, detail.value.marks))
const canReopen = computed(
  () => session.canEdit && c.value.status === 'finished' && (c.value.deaths === 'staged' || c.value.deaths === 'none'),
)

async function copy() {
  const text = [
    `${c.value.species} · ${dayLabel(c.value.day).split(' · ')[0]}`,
    ...lines.value.map(l => `${l.id}\t${l.text}${l.note ? ` (${l.note})` : ''}`),
  ].join('\n')
  try {
    await navigator.clipboard.writeText(text)
    notify(t('Copiado'), 'success')
  } catch {
    notify(t('No se pudo copiar'), 'error')
  }
}
async function reopen() {
  if (
    c.value.deaths === 'staged' &&
    !confirm(t('Se deshacen sus desapariciones aún no guardadas en Google Sheets y el censo vuelve a estar abierto. ¿Seguir?'))
  )
    return
  await census.reopen()
}
function findingText(f: Finding) {
  const m = f.mark
  if (f.kind === 'unknown') return t('{id}: no está en Insectary_data', { id: m.insectaryId })
  if (f.kind === 'otherSpecies')
    return t('{id}: es {species}, encontrada en esta jaula', { id: m.insectaryId, species: m.species ?? '—' })
  if (f.kind === 'offList')
    return t('{id}: vista viva, pero no está en la lista (figura muerta o ID repetido)', { id: m.insectaryId })
  const what =
    m.doubt === 'sex'
      ? t('el sexo se ve distinto')
      : m.doubt === 'species'
        ? t('la especie se ve distinta')
        : t('algo se ve distinto')
  return `${m.insectaryId}: ${what}`
}
/** A line's look: the seen stand out (they get the smiley); the disappeared are quiet; left out, dashed. */
const look = (status: string) =>
  status === 'seen'
    ? 'border-emerald-500 bg-emerald-100 text-emerald-900'
    : status === 'disappeared'
      ? 'border-stone-200 bg-white text-stone-500'
      : 'border-dashed border-stone-300 bg-white text-stone-400'
</script>

<template>
  <div class="h-full overflow-y-auto">
    <header class="sticky top-0 z-10 flex items-center gap-2 border-b border-stone-200 bg-white px-3 py-2 sm:px-4">
      <button
        class="grid h-11 w-11 shrink-0 place-items-center rounded-lg text-stone-600 active:bg-stone-100"
        :aria-label="$t('Volver a los censos')"
        @click="emit('leave')"
      >
        <ArrowLeft :size="22" />
      </button>
      <div class="min-w-0 flex-1">
        <p class="truncate text-base leading-tight font-semibold italic">{{ c.species }}</p>
        <p class="truncate text-xs text-stone-600">
          {{ $t('Censo') }} · {{ dayLabel(c.day) }}
          <template v-if="c.finishedByName">
            ·
            {{
              c.status === 'cancelled'
                ? $t('cancelado por {who}', { who: c.finishedByName })
                : $t('terminado por {who}', { who: c.finishedByName })
            }}</template
          >
        </p>
      </div>
    </header>

    <div class="mx-auto max-w-3xl space-y-5 px-3 pt-3 pb-10 sm:px-4">
      <CensusCounts :census="c" class="text-sm" />
      <p v-if="c.people.length" class="text-sm text-stone-600">{{ $t('Marcaron: {people}', { people: c.people.join(', ') }) }}</p>

      <p v-if="c.deaths === 'staged'" class="rounded-xl border border-amber-300 bg-amber-50 p-3 text-sm text-amber-950">
        {{
          $tn(
            c.counts.disappeared,
            '{n} desaparición espera en la app: «Guardar en Google Sheets» (arriba) la escribe.',
            '{n} desapariciones esperan en la app: «Guardar en Google Sheets» (arriba) las escribe.',
          )
        }}
      </p>
      <p v-else-if="c.deaths === 'sending'" class="rounded-xl border border-amber-300 bg-amber-50 p-3 text-sm text-amber-950">
        {{ $t('Escribiéndose en Google Sheets (o esperando a que responda).') }}
      </p>
      <p v-else-if="c.deaths === 'written'" class="rounded-xl border border-brand-200 bg-brand-50 p-3 text-sm text-brand-900">
        {{ $t('Las desapariciones están en Google Sheets.') }}
      </p>

      <!-- The notebook: what to write next to each ID, in its order. -->
      <section v-if="c.status === 'finished'">
        <div class="mb-1.5 flex flex-wrap items-center gap-2">
          <h2 class="flex-1 text-base font-semibold text-stone-800">{{ $t('Para el cuaderno') }}</h2>
          <button class="btn h-10" @click="copy"><Copy :size="15" /> {{ $t('Copiar') }}</button>
        </div>
        <!-- What to write: ☺ next to the seen, «desaparecida <day>» next to the others (the day said once). -->
        <div class="mb-2 flex flex-wrap items-center gap-1.5 text-sm" role="group" :aria-label="$t('Mostrar')">
          <button
            v-for="f in [
              { key: 'all', label: $t('Todas ({n})', { n: lines.length }), cls: '' },
              { key: 'seen', label: $t('☺ vistas ({n})', { n: counts.seen }), cls: 'text-brand-800' },
              {
                key: 'disappeared',
                label: $t('desaparecidas {day} ({n})', { day: notebookDay, n: counts.disappeared }),
                cls: 'text-stone-700',
              },
              ...(counts.excluded ? [{ key: 'excluded', label: $t('no contadas ({n})', { n: counts.excluded }), cls: 'text-stone-500' }] : []),
            ]"
            :key="f.key"
            type="button"
            class="min-h-9 rounded-full border px-3"
            :class="[showing === f.key ? 'border-stone-800 bg-stone-800 text-white' : 'border-stone-300 bg-white ' + f.cls]"
            :aria-pressed="showing === f.key"
            @click="showing = f.key as typeof showing"
          >
            {{ f.label }}
          </button>
        </div>
        <p class="mb-2 text-xs text-stone-500">
          {{ $t('En el orden del cuaderno: ☺ junto a las vistas; «desaparecida {day}» junto a las demás.', { day: notebookDay }) }}
        </p>
        <ol class="grid grid-cols-[repeat(auto-fill,minmax(7.5rem,1fr))] gap-1.5">
          <li
            v-for="l in shown"
            :key="l.id"
            class="flex min-h-11 items-center justify-between gap-1 rounded-lg border px-2.5 py-1"
            :class="look(l.status)"
            :title="l.note || undefined"
          >
            <span class="font-semibold tabular-nums" :class="l.status === 'disappeared' ? 'text-stone-700' : ''">{{ l.id }}</span>
            <Smile v-if="l.status === 'seen'" :size="22" class="shrink-0 text-emerald-700" aria-hidden="true" />
            <span v-else-if="l.status === 'disappeared'" class="text-xs">{{ $t('desap.') }}</span>
            <span v-else class="truncate text-xs">{{ l.note || $t('no contada') }}</span>
            <span class="sr-only">{{ l.text }}</span>
          </li>
        </ol>
        <button
          v-if="session.canEdit"
          class="mt-2 flex min-h-12 w-full items-center justify-center gap-2 rounded-lg border px-3 text-base font-medium"
          :class="
            c.notebookAt
              ? 'border-brand-700 bg-brand-700 text-white'
              : 'border-stone-300 bg-white text-stone-800 active:bg-stone-100'
          "
          :aria-pressed="!!c.notebookAt"
          @click="census.notebook(!c.notebookAt)"
        >
          <BookCheck :size="18" />
          {{ c.notebookAt ? $t('Pasado al cuaderno por {who}', { who: c.notebookByName ?? '' }) : $t('Ya lo pasé al cuaderno') }}
        </button>
      </section>

      <section v-if="findings.length">
        <h2 class="mb-1 text-sm font-semibold text-violet-900">
          {{ $tn(findings.length, '{n} hallazgo para revisar', '{n} hallazgos para revisar') }}
        </h2>
        <ul class="divide-y divide-violet-100 rounded-xl border border-violet-200 bg-violet-50">
          <li v-for="f in findings" :key="f.mark.id" class="px-3 py-1.5 text-sm text-violet-950">
            {{ findingText(f) }}<template v-if="f.mark.note"> — {{ f.mark.note }}</template>
            <span class="text-xs text-violet-700"> · {{ f.mark.actorName }}</span>
          </li>
        </ul>
      </section>

      <button v-if="canReopen" class="btn h-11" @click="reopen"><RotateCcw :size="15" /> {{ $t('Reabrir el censo') }}</button>
    </div>
  </div>
</template>
