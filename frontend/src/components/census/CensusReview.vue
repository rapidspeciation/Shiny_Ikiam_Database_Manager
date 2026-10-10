<script setup lang="ts">
import { computed, ref } from 'vue'
import { AlertTriangle, ArrowLeft, Flag, Loader2, Undo2 } from 'lucide-vue-next'
import SexBadge from '../SexBadge.vue'
import { useCensus } from '../../composables/useCensus'
import {
  DISAPPEARED,
  OLD_DAYS,
  clippedWithoutPurpose,
  disappearanceEdits,
  findingsOf,
  markFinder,
  notSeen,
  type CensusSummary,
  type Finding,
  type RosterEntry,
} from '../../lib/census'
import { dayLabel, formatSerial, isoToSerial, todayIso } from '../../lib/dates'
import { CROSS_PURPOSE } from '../../lib/deaths'
import { ApiError } from '../../lib/api'
import { errorText, notify } from '../../lib/notice'
import type { Table } from '../../lib/types'
import { initialsOf } from '../../lib/rows'
import { useSession } from '../../stores/session'
import { t, tn } from '../../lib/i18n'
import { stagedSaving } from '../../lib/stagedSwitch'

/**
 * Before the census ends: (a) the butterflies seen alive; (b) those not seen,
 * which will die as disappeared on the census day (each can be left out, e.g.
 * it is in another cage); (c) the findings, kept for review. «Marcar N como
 * desaparecidas» keeps those deaths in the app, written as Muertes writes a
 * death not preserved (lib/census.ts disappearanceEdits; one with a wing clip
 * takes the Research_purpose chosen here), until «Guardar en
 * Google Sheets»; with staged saving off it writes them to the sheet at once
 * (or they wait for Google when it is busy).
 */
const props = defineProps<{ table: Table | undefined; ready: boolean; collectors?: string[]; purposes?: string[] }>()
const session = useSession()
/** Whose initials sign the note «Disappeared in census» (FCH). */
const initials = computed(() => initialsOf(session.user?.displayName || '', props.collectors ?? [], session.user?.username || ''))
const emit = defineEmits<{ back: [] }>()
const census = useCensus()
/** Whether the disappearances wait in the app for «Guardar en Google Sheets» (or are written when finishing). */
const staged = stagedSaving()

const detail = computed(() => census.detail.value!)
const species = computed(() => detail.value.census.species)
const serial = computed(() => isoToSerial(detail.value.census.day))
const today = computed(() => isoToSerial(todayIso()))
const markOf = computed(() => markFinder(detail.value.marks))
const roster = computed(() => detail.value.roster)
const missing = computed(() => notSeen(roster.value, detail.value.marks))
const seen = computed(() => roster.value.filter(b => markOf.value(b)?.kind === 'seen'))
const excluded = computed(() => roster.value.filter(b => markOf.value(b)?.kind === 'excluded'))
const findings = computed(() => findingsOf(species.value, roster.value, detail.value.marks))
const rowById = computed(() => new Map((props.table?.rows ?? []).map(r => [r.id, r])))
/** Not seen, with a wing clip and no Research_purpose yet: the purpose their disappearance writes. */
const clipped = computed(() => clippedWithoutPurpose(missing.value, rowById.value))
const purpose = ref(CROSS_PURPOSE)
const purposeList = computed(() => [...new Set([CROSS_PURPOSE, ...(props.purposes ?? [])])].filter(Boolean))
const ageOf = (b: RosterEntry) => (b.entered === null ? null : Math.max(0, today.value - b.entered))
const old = (b: RosterEntry) => (ageOf(b) ?? 0) > OLD_DAYS

// --- Leaving one out (in another cage…): a mark everyone sees, undone with «Contar otra vez».
/** Reasons as the team writes notes (English). */
const REASONS = ['In another cage', 'Seen before the census', 'Escaped, not dead']
const leaving = ref<string | null>(null)
const reason = ref('')
async function leaveOut(b: RosterEntry, note: string) {
  leaving.value = null
  reason.value = ''
  await census.mark({ recordId: b.recordId, insectaryId: b.id, kind: 'excluded', note: note.trim() || undefined })
}
async function countAgain(b: RosterEntry) {
  const m = markOf.value(b)
  if (m) await census.unmark(m.id)
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

// --- Finish
const saving = ref(false)
/** What finishing did with the disappearances: kept in the app, written, or waiting for Google. */
function finishedText(deaths: CensusSummary['deaths'], n: number) {
  if (!n || deaths === 'none') return t('Censo terminado')
  if (deaths === 'written') return tn(n, '{n} desaparición escrita en Google Sheets', '{n} desapariciones escritas en Google Sheets')
  if (deaths === 'queued')
    return tn(
      n,
      '{n} desaparición espera a que Google Sheets responda; se escribe sola',
      '{n} desapariciones esperan a que Google Sheets responda; se escriben solas',
    )
  if (deaths === 'sending') return t('Escribiéndose en Google Sheets (o esperando a que responda).')
  return tn(n, '{n} desaparición en la app: falta «Guardar en Google Sheets»', '{n} desapariciones en la app: falta «Guardar en Google Sheets»')
}
async function finish() {
  if (saving.value) return
  const { edits, absent } = disappearanceEdits(
    missing.value,
    rowById.value,
    serial.value,
    { today: isoToSerial(todayIso()), initials: initials.value },
    purpose.value,
  )
  if (absent.length)
    return notify(t('Falta cargar {ids} de Insectary_data; espera un momento', { ids: absent.join(', ') }), 'error')
  saving.value = true
  try {
    const out = await census.finish(edits)
    notify(finishedText(out?.census.deaths ?? null, edits.length), 'success')
  } catch (e) {
    if (e instanceof ApiError && e.code === 'CENSUS_CHANGED') await census.loadDetail()
    notify(errorText(e), 'error')
  } finally {
    saving.value = false
  }
}
</script>

<template>
  <div class="flex h-full flex-col">
    <header class="flex items-center gap-2 border-b border-stone-200 bg-white px-3 py-2 sm:px-4">
      <button
        class="grid h-11 w-11 shrink-0 place-items-center rounded-lg text-stone-600 active:bg-stone-100"
        :aria-label="$t('Volver a marcar')"
        @click="emit('back')"
      >
        <ArrowLeft :size="22" />
      </button>
      <div class="min-w-0 flex-1">
        <p class="truncate text-base leading-tight font-semibold">{{ $t('Revisar antes de terminar') }}</p>
        <p class="truncate text-xs text-stone-600">
          <span class="italic">{{ species }}</span> · {{ dayLabel(detail.census.day) }}
        </p>
      </div>
    </header>

    <div class="min-h-0 flex-1 overflow-y-auto">
      <div class="mx-auto max-w-3xl space-y-5 px-3 pt-3 pb-8 sm:px-4">
        <!-- (b) first: it is what will be written. -->
        <section>
          <h2 class="text-base font-semibold text-stone-800">
            {{ $tn(missing.length, '{n} sin ver: desaparecida', '{n} sin ver: desaparecidas') }}
          </h2>
          <p class="mb-2 text-sm text-stone-600">
            {{
              $t(
                'Death_date {date}, Death_cause {cause}; sin CAM ni tubo: CAM y tubos NA, tejidos y medios NOT_COLLECTED (como en Muertes).',
                {
                  date: formatSerial(serial),
                  cause: DISAPPEARED,
                },
              )
            }}
            {{ $t('Si una está en otra jaula, «No contar».') }}
          </p>
          <p v-if="!missing.length" class="rounded-xl border border-brand-200 bg-brand-50 p-3 text-sm font-medium text-brand-900">
            {{ $t('Ninguna: todas vistas o no contadas.') }}
          </p>
          <ul class="divide-y divide-stone-100 overflow-hidden rounded-xl border border-stone-200 bg-white">
            <li v-for="b in missing" :key="b.recordId" class="px-3 py-2">
              <div class="flex items-center gap-2">
                <span class="w-16 text-lg font-semibold">{{ b.id }}</span>
                <span class="flex min-w-0 flex-1 flex-wrap items-center gap-1.5 text-xs text-stone-600">
                  <SexBadge :sex="b.sex" />
                  <span v-if="b.clutch && b.clutch !== 'NA'">{{ $t('clutch {c}', { c: b.clutch }) }}</span>
                  <span v-if="ageOf(b) !== null">· {{ tn(ageOf(b)!, '{n} día', '{n} días') }}</span>
                  <span v-if="b.wild">· {{ $t('silvestre') }}</span>
                </span>
                <button
                  class="h-11 shrink-0 rounded-lg border border-stone-300 px-3 text-sm font-medium active:bg-stone-100"
                  @click="leaving = leaving === b.recordId ? null : b.recordId"
                >
                  {{ $t('No contar') }}
                </button>
              </div>
              <p v-if="old(b)" class="mt-1 flex items-start gap-1 text-xs font-medium text-amber-900">
                <AlertTriangle :size="14" class="mt-px shrink-0" />
                {{
                  $t('{n} días en el insectario: quizá murió antes sin registrarse; mira su fila antes de marcarla hoy.', {
                    n: ageOf(b)!,
                  })
                }}
              </p>
              <div v-if="leaving === b.recordId" class="mt-2 flex flex-wrap gap-2">
                <button
                  v-for="r in REASONS"
                  :key="r"
                  class="min-h-11 rounded-lg border border-stone-300 bg-white px-3 text-sm"
                  @click="leaveOut(b, r)"
                >
                  {{ r }}
                </button>
                <input
                  v-model="reason"
                  class="field-input h-11 min-w-40 flex-1"
                  :placeholder="$t('Otra razón (en inglés)')"
                  @keydown.enter.prevent="leaveOut(b, reason)"
                />
                <button class="btn h-11" :disabled="!reason.trim()" @click="leaveOut(b, reason)">{{ $t('No contar') }}</button>
              </div>
            </li>
          </ul>
          <!-- Cross or pheromone parents among them: the purpose written with their disappearance. -->
          <label v-if="clipped.length" class="mt-2 block rounded-xl border border-amber-200 bg-amber-50 p-3" data-purpose>
            <span class="flex items-start gap-1.5 text-sm text-amber-900">
              <AlertTriangle :size="16" class="mt-0.5 shrink-0" />
              <span class="min-w-0">{{
                $tn(
                  clipped.length,
                  '{ids} tiene un clip de ala (cruces o feromonas). Su Research_purpose:',
                  '{ids} tienen un clip de ala (cruces o feromonas). Su Research_purpose:',
                  { ids: clipped.map(b => b.id).join(', ') },
                )
              }}</span>
            </span>
            <select v-model="purpose" class="field-input mt-1.5 h-11 text-base">
              <option v-for="p in purposeList" :key="p" :value="p">{{ p }}</option>
            </select>
          </label>
        </section>

        <section v-if="excluded.length">
          <h2 class="mb-1 text-sm font-semibold text-stone-700">
            {{ $tn(excluded.length, '{n} no contada', '{n} no contadas') }}
          </h2>
          <ul class="divide-y divide-stone-100 rounded-xl border border-stone-200 bg-white">
            <li v-for="b in excluded" :key="b.recordId" class="flex items-center gap-2 px-3 py-1.5 text-sm">
              <span class="w-16 font-semibold">{{ b.id }}</span>
              <span class="min-w-0 flex-1 truncate text-stone-600"
                >{{ markOf(b)?.note || '—' }} · {{ markOf(b)?.actorName }}</span
              >
              <button
                class="flex h-10 items-center gap-1 rounded-lg px-2 text-stone-600 active:bg-stone-100"
                @click="countAgain(b)"
              >
                <Undo2 :size="15" /> {{ $t('Contar otra vez') }}
              </button>
            </li>
          </ul>
        </section>

        <section>
          <h2 class="mb-1 text-sm font-semibold text-stone-700">{{ $tn(seen.length, '{n} vista viva', '{n} vistas vivas') }}</h2>
          <p class="flex flex-wrap gap-1.5">
            <span
              v-for="b in seen"
              :key="b.recordId"
              class="rounded-md bg-brand-50 px-2 py-0.5 text-sm font-medium text-brand-900"
              >☺ {{ b.id }}</span
            >
          </p>
        </section>

        <section v-if="findings.length">
          <h2 class="mb-1 text-sm font-semibold text-violet-900">
            {{ $tn(findings.length, '{n} hallazgo para revisar', '{n} hallazgos para revisar') }}
          </h2>
          <p class="mb-1 text-xs text-stone-600">{{ $t('Se guardan con el censo; no se cambia nada en la hoja por ellos.') }}</p>
          <ul class="divide-y divide-violet-100 rounded-xl border border-violet-200 bg-violet-50">
            <li v-for="f in findings" :key="f.mark.id" class="px-3 py-1.5 text-sm text-violet-950">
              {{ findingText(f) }}<template v-if="f.mark.note"> — {{ f.mark.note }}</template>
              <span class="text-xs text-violet-700"> · {{ f.mark.actorName }}</span>
            </li>
          </ul>
        </section>
      </div>
    </div>

    <footer class="border-t border-stone-200 bg-white px-3 py-2 sm:px-4">
      <div class="mx-auto flex max-w-3xl items-center gap-3">
        <p class="min-w-0 flex-1 text-xs text-stone-600">
          {{
            staged
              ? $t('Quedan en la app (como Emergidos y Clutches) hasta «Guardar en Google Sheets».')
              : $t('Se escriben en Google Sheets al terminar (si Google está ocupado, esperan y se escriben solas).')
          }}
        </p>
        <button class="btn-primary h-12 px-4 text-base" :disabled="saving || !ready" @click="finish">
          <Loader2 v-if="saving" :size="18" class="animate-spin" /><Flag v-else :size="18" />
          {{
            missing.length
              ? $tn(missing.length, 'Marcar {n} como desaparecida', 'Marcar {n} como desaparecidas')
              : $t('Terminar sin desapariciones')
          }}
        </button>
      </div>
    </footer>
  </div>
</template>
