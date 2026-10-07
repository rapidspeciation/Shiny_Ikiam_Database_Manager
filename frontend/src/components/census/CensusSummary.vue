<script setup lang="ts">
import { computed, ref } from 'vue'
import { ArrowLeft, BookCheck, RotateCcw } from 'lucide-vue-next'
import CensusCounts from './CensusCounts.vue'
import CensusNotebook from './CensusNotebook.vue'
import { useCensus } from '../../composables/useCensus'
import { findingsOf, type Finding } from '../../lib/census'
import { dayLabel } from '../../lib/dates'
import type { Table } from '../../lib/types'
import { useSession } from '../../stores/session'
import { t } from '../../lib/i18n'

/**
 * A census done (or cancelled): its counts, where its disappearances are
 * (in the app until «Guardar en Google Sheets», waiting for Google with staged
 * saving off, or in the sheet), «Actualizar el cuaderno» (CensusNotebook: what to
 * mark on paper for the species censused that day), the findings, and
 * «Reabrir» while its disappearances are not in the sheet.
 */
defineProps<{ table: Table | undefined }>()
const emit = defineEmits<{ leave: [] }>()
const census = useCensus()
const session = useSession()
const detail = computed(() => census.detail.value!)
const c = computed(() => detail.value.census)
const findings = computed(() => findingsOf(c.value.species, detail.value.roster, detail.value.marks))
const canReopen = computed(
  () => session.canEdit && c.value.status === 'finished' && ['staged', 'none', 'failed'].includes(c.value.deaths ?? ''),
)

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
      <p v-else-if="c.deaths === 'queued'" class="rounded-xl border border-amber-300 bg-amber-50 p-3 text-sm text-amber-950">
        {{
          $tn(
            c.counts.disappeared,
            '{n} desaparición espera a que Google Sheets responda; se escribe sola.',
            '{n} desapariciones esperan a que Google Sheets responda; se escriben solas.',
          )
        }}
      </p>
      <p v-else-if="c.deaths === 'written'" class="rounded-xl border border-brand-200 bg-brand-50 p-3 text-sm text-brand-900">
        {{
          $tn(
            c.counts.disappeared,
            '{n} desaparición escrita en Google Sheets.',
            '{n} desapariciones escritas en Google Sheets.',
          )
        }}
      </p>
      <p v-else-if="c.deaths === 'failed'" class="rounded-xl border border-red-300 bg-red-50 p-3 text-sm text-red-950">
        {{
          $t(
            'Google Sheets no aceptó las desapariciones: no se escribió nada (el motivo está en Historial). Reabre el censo para terminarlo otra vez.',
          )
        }}
      </p>

      <!-- The paper notebook: what to mark next to each ID, for every species censused that day. -->
      <section v-if="c.status === 'finished'">
        <h2 class="mb-1.5 text-base font-semibold text-stone-800">{{ $t('Actualizar el cuaderno') }}</h2>
        <CensusNotebook :key="c.day" :day="c.day" />
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
