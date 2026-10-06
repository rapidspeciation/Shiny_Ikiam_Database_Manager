<script setup lang="ts">
import { computed, nextTick, ref } from 'vue'
import { AlertTriangle, ArrowLeft, CheckCircle2, Flag, Loader2, Search, Smile, Undo2, X } from 'lucide-vue-next'
import EntryModeToggle from '../EntryModeToggle.vue'
import IdFilters from '../IdFilters.vue'
import IdSuggestion from '../IdSuggestion.vue'
import SexBadge from '../SexBadge.vue'
import { useCensus } from '../../composables/useCensus'
import { useEntryMode } from '../../composables/useEntryMode'
import { useLookAlikes } from '../../composables/useLookAlikes'
import { useMedia } from '../../composables/usePhone'
import {
  censusIndex,
  findingsOf,
  markFinder,
  notSeen,
  progressOf,
  sameSpecies,
  type CensusMark,
  type Doubt,
  type RosterEntry,
} from '../../lib/census'
import { dayLabel, formatSerial, isoToSerial, todayIso } from '../../lib/dates'
import { factsOf, lifeOf, searchKey, type Facts } from '../../lib/deaths'
import { isPattern, matchIds, sexOf, type SexFilter } from '../../lib/idMatch'
import { notify } from '../../lib/notice'
import type { Table, TableRow } from '../../lib/types'
import { useSession } from '../../stores/session'
import { t, tn } from '../../lib/i18n'

/**
 * The census being done, for one hand: the ID read on the wing goes in the
 * box (a character that cannot be read: `?`; one of two: `[BD]`; look-alikes
 * are offered too, lib/idMatch.ts), the species is the census's and the sex
 * seen can be tapped; tapping the right butterfly (or Enter for the first)
 * marks it seen, with a big smiley and Undo. If its sex or species looks
 * different it is still alive: «Se ve distinta» keeps a note for review. A
 * butterfly of another species found in this cage, or an ID that is in no row,
 * is kept as a finding. Below: everyone's latest marks and the butterflies not
 * seen yet (cards or a table), each can be marked from there too; «Terminar»
 * goes to the review.
 */
const props = defineProps<{ table: Table | undefined; ready: boolean }>()
const emit = defineEmits<{ review: []; leave: [] }>()
const census = useCensus()
const session = useSession()
const lookAlikes = useLookAlikes()
const { mode } = useEntryMode('census')
const wide = useMedia('(min-width: 1024px)')

const detail = computed(() => census.detail.value!)
const species = computed(() => detail.value.census.species)
const roster = computed(() => detail.value.roster)
const marks = computed(() => detail.value.marks)
const markOf = computed(() => markFinder(marks.value))
const progress = computed(() => progressOf(roster.value, marks.value))
const findings = computed(() => findingsOf(species.value, roster.value, marks.value))
const today = computed(() => isoToSerial(todayIso()))
const canEdit = computed(() => session.canEdit)

// --- The butterflies, indexed once per version of the sheet (repeated IDs kept: an old dead B9D, a living one).
const index = computed(() => (props.table ? censusIndex(props.table.rows) : []))
const rowById = computed(() => new Map((props.table?.rows ?? []).map(r => [r.id, r])))
const listed = computed(() => new Set(roster.value.map(b => b.recordId)))
const valuesOf = (row: TableRow) => (field: string) => row.values[field] ?? null
const alive = (row: TableRow) => listed.value.has(row.id) || lifeOf(valuesOf(row)).state === 'alive'
const factsFor = (row: TableRow): Facts => factsOf(valuesOf(row), today.value)
/** A butterfly of the list as facts (its row, or what the list says while the sheet loads). */
function rosterFacts(b: RosterEntry): Facts {
  const row = rowById.value.get(b.recordId)
  if (row) return factsFor(row)
  return {
    species: b.species,
    sex: b.sex,
    clutch: b.clutch,
    wild: b.wild,
    entered: b.entered,
    days: b.entered === null ? null : Math.max(0, today.value - b.entered),
    life: { state: 'alive', death: null, cause: '' },
    notes: '',
  }
}

// --- Typing the ID
const input = ref<HTMLInputElement>()
const query = ref('')
const sex = ref<SexFilter>('')
const suggestions = computed(() => {
  if (!query.value.trim()) return []
  return matchIds(index.value, query.value, {
    alive: e => alive(e.row),
    speciesOf: e => e.row.values.SPECIES,
    sexOf: e => e.row.values.Sex,
    species: species.value,
    sex: sex.value,
    table: lookAlikes.value,
    limit: 8,
    // A butterfly recorded dead is not in the cage: after every living one (still offered, greyed).
    deadStep: 10,
  })
})
/** Nothing fits what was typed: it can be kept as a finding (an ID in no row). */
const nothing = computed(() => !!query.value.trim() && props.ready && !suggestions.value.length && !isPattern(query.value))
/** Only look-alikes or longer IDs fit what was typed (whole, no pattern): it may still be an ID in no row. */
const noExact = computed(
  () =>
    !!suggestions.value.length &&
    searchKey(query.value).length >= 2 &&
    !isPattern(query.value) &&
    !suggestions.value.some(s => s.kind === 'exact'),
)
const clock = new Intl.DateTimeFormat('en-GB', { timeZone: 'America/Guayaquil', hour: '2-digit', minute: '2-digit' })
const timeOf = (iso: string) => clock.format(new Date(iso))
function tagOf(rowId: string, id: string): string {
  const m = markOf.value({ recordId: rowId, id })
  if (!m) return ''
  if (m.kind === 'excluded') return t('No contada: {note}', { note: m.note || '—' })
  return m.sending ? t('☺ marcando…') : t('☺ vista por {who} {time}', { who: m.actorName, time: timeOf(m.createdAt) })
}
const focusInput = () => nextTick(() => input.value?.focus({ preventScroll: true }))

// --- Marking, and what the person sees after it
interface Feedback {
  mark: CensusMark
  already: boolean
  facts: Facts
  /** Recorded dead, or of another species: kept as a finding. */
  warn: string
}
const feedback = ref<Feedback | null>(null)
const doubt = ref<Doubt | null>(null)
const doubtNote = ref('')
const doubtOpen = ref(false)

async function markRow(row: TableRow) {
  if (!canEdit.value) return
  const id = String(row.values.Insectary_ID ?? '').trim()
  const facts = factsFor(row)
  query.value = ''
  doubtOpen.value = false
  doubt.value = null
  doubtNote.value = ''
  const existing = markOf.value({ recordId: row.id, id })
  if (existing?.kind === 'seen') {
    feedback.value = { mark: existing, already: true, facts, warn: '' }
    return focusInput()
  }
  const warn = !alive(row)
    ? t('{id} figura muerta ({date}): queda como hallazgo para revisar', {
        id,
        date: facts.life.death !== null ? formatSerial(facts.life.death) : String(facts.life.cause || 'NA'),
      })
    : !sameSpecies(facts.species, species.value)
      ? t('{id} es {species}: queda como hallazgo (encontrada en esta jaula)', { id, species: facts.species || '—' })
      : ''
  focusInput()
  // Felt as well as seen: a short buzz on phones that have it.
  navigator.vibrate?.(warn ? [40, 60, 40] : 35)
  const pending = census.mark({ recordId: row.id, insectaryId: id, species: facts.species })
  const temp = census.detail.value?.marks.at(-1)
  if (temp) feedback.value = { mark: temp, already: false, facts, warn }
  const out = await pending
  if (!out) {
    feedback.value = null
    return
  }
  feedback.value = { mark: out.mark, already: out.already, facts, warn: out.already ? '' : warn }
}
function enter() {
  const top = suggestions.value[0]
  if (top) void markRow(top.item.row)
}
async function keepUnknown() {
  const text = searchKey(query.value)
  if (!text) return
  query.value = ''
  const out = await census.mark({ kind: 'unknown', text, insectaryId: text })
  if (out) {
    feedback.value = {
      mark: out.mark,
      already: false,
      facts: emptyFacts,
      warn: t('{id} no está en Insectary_data: queda como hallazgo', { id: text }),
    }
    notify(t('{id} anotado como hallazgo', { id: text }))
  }
  focusInput()
}
const emptyFacts: Facts = {
  species: '',
  sex: '',
  clutch: '',
  wild: false,
  entered: null,
  days: null,
  life: { state: 'unknown', death: null, cause: '' },
  notes: '',
}
async function undo(m: CensusMark) {
  if (m.sending) return
  await census.unmark(m.id)
  if (feedback.value?.mark.id === m.id) feedback.value = null
  focusInput()
}
async function saveDoubt() {
  const f = feedback.value
  if (!f) return
  await census.setDoubt(f.mark.id, doubt.value ?? 'other', doubtNote.value.trim() || null)
  doubtOpen.value = false
  notify(t('Guardado para revisar'), 'success')
  focusInput()
}
const feedbackMark = computed(() => {
  const f = feedback.value
  if (!f) return null
  // The latest of that mark (a doubt saved, the server's answer), or gone (undone by someone).
  return marks.value.find(m => m.id === f.mark.id || (f.mark.recordId && m.recordId === f.mark.recordId)) ?? null
})

// --- Everyone's latest marks
const latest = computed(() => [...marks.value].reverse().slice(0, 8))
const DOUBTS = computed<Record<string, string>>(() => ({
  sex: t('el sexo se ve distinto'),
  species: t('la especie se ve distinta'),
  other: t('algo se ve distinto'),
}))

// --- Not seen yet: filterable; marked from here too
const listText = ref('')
const listSex = ref<SexFilter>('')
const left = computed(() => notSeen(roster.value, marks.value))
const shownLeft = computed(() => {
  const q = searchKey(listText.value)
  return left.value.filter(b => (!q || searchKey(b.id).includes(q)) && (!listSex.value || sexOf(b.sex) === listSex.value))
})
function markListed(b: RosterEntry) {
  const row = rowById.value.get(b.recordId)
  if (row) void markRow(row)
  else notify(t('Cargando {sheet}…', { sheet: 'Insectary_data' }))
}
/** The table: the whole list in the notebook's order, with each one's mark. */
const allRows = computed(() => roster.value.map(b => ({ b, m: markOf.value(b) })))
const ageOf = (b: RosterEntry) => (b.entered === null ? null : Math.max(0, today.value - b.entered))

function cancel() {
  if (!confirm(t('¿Cancelar este censo? Las marcas se conservan en el historial, pero no se registra ninguna desaparición.')))
    return
  void census.cancel()
}
</script>

<template>
  <div class="flex h-full flex-col">
    <!-- The census, and how far it is. -->
    <header class="border-b border-stone-200 bg-white px-3 py-2 sm:px-4">
      <div class="flex items-center gap-2">
        <button
          class="grid h-11 w-11 shrink-0 place-items-center rounded-lg text-stone-600 active:bg-stone-100"
          :aria-label="$t('Volver a los censos')"
          @click="emit('leave')"
        >
          <ArrowLeft :size="22" />
        </button>
        <div class="min-w-0 flex-1">
          <p class="truncate text-base leading-tight font-semibold italic">{{ species }}</p>
          <p class="truncate text-xs text-stone-600">
            {{ $t('Censo') }} · {{ dayLabel(detail.census.day) }}
            <template v-if="detail.census.people.length"> · {{ detail.census.people.join(', ') }}</template>
          </p>
        </div>
        <div class="shrink-0 text-right" role="status">
          <p class="text-2xl leading-none font-bold text-brand-800 tabular-nums">
            {{ progress.seen }}<span class="text-base font-medium text-stone-500">/{{ progress.total }}</span>
          </p>
          <p class="text-xs text-stone-600">{{ $t('vistas') }}</p>
        </div>
      </div>
      <div class="mt-2 h-2 overflow-hidden rounded-full bg-stone-200" aria-hidden="true">
        <div
          class="h-full bg-brand-600 transition-all"
          :style="{ width: `${progress.total ? ((progress.seen + progress.excluded) / progress.total) * 100 : 0}%` }"
        />
      </div>
    </header>

    <div class="min-h-0 flex-1 overflow-y-auto" data-scroll>
      <div class="mx-auto grid max-w-6xl gap-x-6 lg:grid-cols-2">
        <div class="min-w-0">
          <!-- The ID read on the wing: stays at the top while the rest scrolls. -->
          <div
            class="sticky top-0 z-20 border-b border-stone-200 bg-stone-50/95 px-3 pt-3 pb-2 backdrop-blur sm:px-4 lg:border-0"
          >
            <div class="relative">
              <Search :size="20" class="pointer-events-none absolute top-1/2 left-3 -translate-y-1/2 text-stone-400" />
              <input
                ref="input"
                v-model="query"
                class="h-14 w-full rounded-xl border-2 border-stone-300 bg-white pr-12 pl-10 text-2xl font-semibold tracking-wider uppercase placeholder:text-base placeholder:font-normal placeholder:tracking-normal placeholder:normal-case focus:border-brand-600 focus:ring-2 focus:ring-brand-100 focus:outline-none"
                :placeholder="$t('ID del ala (A?B, A[16]B)')"
                :aria-label="$t('Insectary ID leído en el ala')"
                :disabled="!canEdit"
                type="text"
                inputmode="text"
                autocapitalize="characters"
                autocomplete="off"
                autocorrect="off"
                spellcheck="false"
                enterkeyhint="done"
                @keydown.enter.prevent="enter"
              />
              <button
                v-if="query"
                class="absolute top-1/2 right-1 grid h-11 w-11 -translate-y-1/2 place-items-center text-stone-500"
                :aria-label="$t('Borrar búsqueda')"
                @mousedown.prevent
                @click="query = ''"
              >
                <X :size="20" />
              </button>
            </div>
            <IdFilters v-model:sex="sex" class="mt-2" />
            <p v-if="!ready" class="mt-1.5 text-sm text-stone-500">{{ $t('Cargando {sheet}…', { sheet: 'Insectary_data' }) }}</p>
            <!-- The butterflies that fit: tap one (or Enter for the first). -->
            <ul
              v-if="suggestions.length"
              class="mt-2 max-h-[55vh] divide-y divide-stone-100 overflow-y-auto rounded-xl border border-stone-200 bg-white shadow-lg"
              role="listbox"
            >
              <li v-for="(s, i) in suggestions" :key="s.item.row.id">
                <button
                  class="flex min-h-15 w-full items-center gap-2 px-3 py-2 text-left active:bg-brand-50"
                  :class="i === 0 ? 'bg-brand-50/60' : ''"
                  role="option"
                  :aria-selected="i === 0"
                  @mousedown.prevent
                  @click="markRow(s.item.row)"
                >
                  <IdSuggestion
                    :id="s.item.id"
                    :facts="factsFor(s.item.row)"
                    :at="s.at"
                    :greyed="!s.alive || !s.sameSpecies"
                    :tag="tagOf(s.item.row.id, s.item.id)"
                  />
                  <Smile :size="22" class="shrink-0" :class="i === 0 ? 'text-brand-700' : 'text-stone-300'" />
                </button>
              </li>
              <li v-if="noExact" class="px-3 py-1.5">
                <button class="h-11 text-sm text-amber-900 underline" @mousedown.prevent @click="keepUnknown">
                  {{ $t('Ninguna de estas: anotar {id} como hallazgo', { id: searchKey(query) }) }}
                </button>
              </li>
            </ul>
            <div v-else-if="nothing" class="mt-2 rounded-xl border border-amber-300 bg-amber-50 p-3 text-sm text-amber-950">
              <p>{{ $t('Ningún ID se parece a {id}.', { id: searchKey(query) }) }}</p>
              <p class="mt-0.5 text-xs">
                {{ $t('Prueba con ? en el carácter que no se lee (A?B) o con dos opciones (A[16]B).') }}
              </p>
              <button
                class="mt-2 h-11 rounded-lg border border-amber-500 bg-white px-3 font-medium"
                @mousedown.prevent
                @click="keepUnknown"
              >
                {{ $t('Anotar {id} como hallazgo', { id: searchKey(query) }) }}
              </button>
            </div>
          </div>

          <!-- What the last tap did, big: the smiley, the butterfly, Undo, and «se ve distinta». -->
          <section v-if="feedback && feedbackMark" class="px-3 pt-3 sm:px-4" aria-live="polite">
            <div
              class="rounded-2xl border-2 p-3"
              :class="
                feedback.already
                  ? 'border-amber-400 bg-amber-50'
                  : feedback.warn
                    ? 'border-violet-400 bg-violet-50'
                    : 'border-brand-600 bg-brand-50'
              "
            >
              <div class="flex items-center gap-3">
                <component
                  :is="feedback.already ? CheckCircle2 : feedbackMark.kind === 'unknown' ? Flag : Smile"
                  :size="44"
                  class="shrink-0"
                  :class="feedback.already ? 'text-amber-700' : feedback.warn ? 'text-violet-700' : 'text-brand-700'"
                />
                <div class="min-w-0 flex-1">
                  <p class="text-2xl leading-tight font-bold">
                    {{ feedbackMark.insectaryId }}
                    <Loader2 v-if="feedbackMark.sending" :size="18" class="inline animate-spin text-stone-500" />
                  </p>
                  <p v-if="feedback.already" class="text-sm font-medium text-amber-900">
                    {{
                      $t('Ya estaba marcada: {who} {time}', { who: feedbackMark.actorName, time: timeOf(feedbackMark.createdAt) })
                    }}
                  </p>
                  <p v-else-if="feedbackMark.kind === 'seen'" class="text-sm font-medium text-brand-900">
                    {{ $t('Vista viva') }}
                  </p>
                  <p
                    v-if="feedback.facts.species || feedback.facts.sex"
                    class="flex flex-wrap items-center gap-1.5 text-sm text-stone-700"
                  >
                    <SexBadge :sex="feedback.facts.sex" />
                    <span class="italic">{{ feedback.facts.species }}</span>
                    <span v-if="feedback.facts.clutch && feedback.facts.clutch !== 'NA'"
                      >· {{ $t('clutch {c}', { c: feedback.facts.clutch }) }}</span
                    >
                    <span v-if="feedback.facts.days !== null && feedback.facts.life.state === 'alive'"
                      >· {{ tn(feedback.facts.days, '{n} día', '{n} días') }}</span
                    >
                  </p>
                </div>
                <button
                  v-if="!feedback.already && !feedbackMark.sending"
                  class="flex h-12 shrink-0 items-center gap-1 rounded-lg border border-stone-300 bg-white px-3 text-sm font-medium active:bg-stone-100"
                  @click="undo(feedbackMark)"
                >
                  <Undo2 :size="16" /> {{ $t('Deshacer') }}
                </button>
              </div>
              <p v-if="feedback.warn" class="mt-2 flex items-start gap-1.5 text-sm font-medium text-violet-900">
                <AlertTriangle :size="16" class="mt-0.5 shrink-0" /> {{ feedback.warn }}
              </p>
              <p v-if="feedbackMark.doubt" class="mt-2 rounded-md bg-violet-100 px-2 py-1 text-sm text-violet-900">
                {{ DOUBTS[feedbackMark.doubt] }}<template v-if="feedbackMark.note">: {{ feedbackMark.note }}</template>
              </p>
              <!-- Alive, but what is seen does not match the record: kept for review, nothing corrected here. -->
              <template v-if="feedbackMark.kind === 'seen' && !feedback.already && !feedbackMark.sending">
                <button
                  v-if="!doubtOpen"
                  class="mt-2 h-11 text-sm font-medium text-violet-800 underline"
                  @click="doubtOpen = true"
                >
                  {{ $t('Viva, pero se ve distinta…') }}
                </button>
                <div v-else class="mt-2 space-y-2">
                  <div class="grid grid-cols-2 gap-2">
                    <button
                      v-for="d in ['sex', 'species'] as Doubt[]"
                      :key="d"
                      class="min-h-11 rounded-lg border px-2 text-sm font-medium"
                      :class="doubt === d ? 'border-violet-700 bg-violet-700 text-white' : 'border-stone-300 bg-white'"
                      :aria-pressed="doubt === d"
                      @click="doubt = doubt === d ? null : d"
                    >
                      {{ d === 'sex' ? $t('Otro sexo') : $t('Otra especie') }}
                    </button>
                  </div>
                  <input
                    v-model="doubtNote"
                    class="field-input h-11 w-full"
                    :placeholder="$t('Nota corta (en inglés), p. ej. looks male')"
                    @keydown.enter.prevent="saveDoubt"
                  />
                  <div class="flex gap-2">
                    <button class="btn-primary h-11 flex-1" :disabled="!doubt && !doubtNote.trim()" @click="saveDoubt">
                      {{ $t('Guardar para revisar') }}
                    </button>
                    <button class="btn h-11" @click="doubtOpen = false">{{ $t('Cancelar') }}</button>
                  </div>
                </div>
              </template>
            </div>
          </section>

          <!-- Everyone's latest marks (the other phones' too). -->
          <section v-if="latest.length" class="px-3 pt-4 sm:px-4">
            <h2 class="mb-1 text-sm font-semibold text-stone-700">{{ $t('Últimas marcas') }}</h2>
            <ul class="divide-y divide-stone-100 rounded-xl border border-stone-200 bg-white">
              <li v-for="m in latest" :key="m.id" class="flex min-h-12 items-center gap-2 px-3 py-1.5 text-sm">
                <span class="w-6 text-center text-lg" aria-hidden="true">{{
                  m.kind === 'seen' ? '☺' : m.kind === 'unknown' ? '?' : '–'
                }}</span>
                <span class="w-16 font-semibold">{{ m.insectaryId }}</span>
                <span class="min-w-0 flex-1 truncate text-xs text-stone-600">
                  {{ m.actorName }} {{ timeOf(m.createdAt) }}
                  <template v-if="m.kind === 'excluded'"> · {{ $t('no contada') }}{{ m.note ? `: ${m.note}` : '' }}</template>
                  <template v-else-if="m.kind === 'unknown'"> · {{ $t('no está en la hoja') }}</template>
                  <template v-else-if="m.species && !sameSpecies(m.species, species)">
                    · <span class="italic">{{ m.species }}</span></template
                  >
                  <template v-if="m.doubt">
                    · <span class="text-violet-800">{{ DOUBTS[m.doubt] }}</span></template
                  >
                </span>
                <Loader2 v-if="m.sending" :size="16" class="animate-spin text-stone-400" />
                <button
                  v-else-if="canEdit"
                  class="grid h-10 w-10 place-items-center rounded-lg text-stone-500 active:bg-stone-100"
                  :aria-label="$t('Quitar la marca de {id}', { id: m.insectaryId })"
                  :title="$t('Quitar la marca de {id}', { id: m.insectaryId })"
                  @click="undo(m)"
                >
                  <X :size="18" />
                </button>
              </li>
            </ul>
          </section>
        </div>

        <!-- Not seen yet (in the notebook's order): filter, and mark from here. -->
        <section class="min-w-0 px-3 pt-4 pb-28 sm:px-4" :class="wide ? 'lg:pt-3' : ''">
          <div class="mb-1.5 flex flex-wrap items-center gap-2">
            <h2 class="flex-1 text-sm font-semibold text-stone-700">
              {{
                mode === 'table' ? $t('Lista del censo ({n})', { n: roster.length }) : $t('Aún sin ver ({n})', { n: left.length })
              }}
            </h2>
            <EntryModeToggle v-model="mode" compact />
          </div>
          <div class="mb-2 flex flex-wrap items-center gap-2">
            <input
              v-model="listText"
              class="field-input h-10 w-28 uppercase"
              :placeholder="$t('Filtrar ID')"
              :aria-label="$t('Filtrar por ID')"
              autocapitalize="characters"
              autocomplete="off"
            />
            <IdFilters v-model:sex="listSex" />
          </div>
          <template v-if="mode === 'cards'">
            <p v-if="!left.length" class="rounded-xl border border-brand-200 bg-brand-50 p-3 text-sm font-medium text-brand-900">
              {{ $t('Todas vistas o no contadas.') }}
            </p>
            <ul class="grid grid-cols-[repeat(auto-fill,minmax(10.5rem,1fr))] gap-2">
              <li
                v-for="b in shownLeft"
                :key="b.recordId"
                class="flex items-center gap-2 rounded-xl border border-stone-200 bg-white py-1.5 pr-1.5 pl-3 shadow-sm"
              >
                <span class="min-w-0 flex-1">
                  <span class="block text-lg font-semibold">{{ b.id }}</span>
                  <span class="flex flex-wrap items-center gap-x-1 text-xs text-stone-600">
                    <SexBadge :sex="b.sex" />
                    <span v-if="ageOf(b) !== null" class="whitespace-nowrap">{{ tn(ageOf(b)!, '{n} día', '{n} días') }}</span>
                    <span v-if="b.wild" class="whitespace-nowrap text-stone-500">{{ $t('silvestre') }}</span>
                  </span>
                </span>
                <button
                  v-if="canEdit"
                  class="grid h-12 w-12 shrink-0 place-items-center rounded-lg border border-brand-600 text-brand-700 active:bg-brand-50"
                  :aria-label="$t('Marcar {id} como vista', { id: b.id })"
                  :title="$t('Marcar {id} como vista', { id: b.id })"
                  @click="markListed(b)"
                >
                  <Smile :size="24" />
                </button>
              </li>
            </ul>
          </template>
          <div v-else class="overflow-x-auto rounded-xl border border-stone-200 bg-white">
            <table class="w-full text-sm">
              <thead class="bg-stone-100 text-left text-xs text-stone-600">
                <tr>
                  <th class="px-2 py-1.5">Insectary_ID</th>
                  <th class="px-2 py-1.5">Sex</th>
                  <th class="px-2 py-1.5">CLUTCH NUMBER</th>
                  <th class="px-2 py-1.5">{{ $t('Días') }}</th>
                  <th class="px-2 py-1.5">{{ $t('Censo') }}</th>
                </tr>
              </thead>
              <tbody class="divide-y divide-stone-100">
                <tr
                  v-for="{ b, m } in allRows.filter(
                    x =>
                      (!searchKey(listText) || searchKey(x.b.id).includes(searchKey(listText))) &&
                      (!listSex || sexOf(x.b.sex) === listSex),
                  )"
                  :key="b.recordId"
                  :class="m?.kind === 'seen' ? 'bg-brand-50/50' : ''"
                >
                  <td class="px-2 py-1 font-semibold">{{ b.id }}</td>
                  <td class="px-2 py-1"><SexBadge :sex="b.sex" /></td>
                  <td class="px-2 py-1">{{ b.clutch }}</td>
                  <td class="px-2 py-1 tabular-nums">{{ ageOf(b) ?? '' }}{{ b.wild ? ' W' : '' }}</td>
                  <td class="px-2 py-1">
                    <template v-if="m?.kind === 'seen'">☺ {{ m.actorName }} {{ timeOf(m.createdAt) }}</template>
                    <template v-else-if="m?.kind === 'excluded'"
                      >{{ $t('no contada') }}{{ m.note ? `: ${m.note}` : '' }}</template
                    >
                    <button v-else-if="canEdit" class="text-brand-700 underline" @click="markListed(b)">
                      {{ $t('Marcar vista') }}
                    </button>
                  </td>
                </tr>
              </tbody>
            </table>
          </div>
          <p class="mt-3 text-xs text-stone-500">
            <button v-if="canEdit" class="underline" @click="cancel">{{ $t('Cancelar el censo') }}</button>
          </p>
        </section>
      </div>
    </div>

    <!-- Done releasing them: review what is left. -->
    <footer v-if="canEdit" class="border-t border-stone-200 bg-white px-3 py-2 sm:px-4">
      <div class="mx-auto flex max-w-6xl items-center gap-3">
        <p class="min-w-0 flex-1 text-sm text-stone-700">
          {{ $tn(progress.left, '{n} sin ver', '{n} sin ver') }}
          <template v-if="findings.length">
            · <span class="text-violet-800">{{ $tn(findings.length, '{n} hallazgo', '{n} hallazgos') }}</span></template
          >
        </p>
        <button class="btn-primary h-12 px-5 text-base" @click="emit('review')">
          <Flag :size="18" /> {{ $t('Terminar el censo') }}
        </button>
      </div>
    </footer>
  </div>
</template>
