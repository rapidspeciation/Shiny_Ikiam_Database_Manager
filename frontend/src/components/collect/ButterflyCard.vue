<script setup lang="ts">
import { computed, ref } from 'vue'
import { AlertTriangle, ChevronDown, ChevronUp, CopyPlus, Search, StickyNote, X } from 'lucide-vue-next'
import ChoiceField from '../ChoiceField.vue'
import SexBadge from '../SexBadge.vue'
import type { CollectState } from '../../composables/useCollect'
import { COLLECT_NOTE_PHRASES, addPhrase, isEmptyDraft, personCode, weatherLabel, type Column, type Draft, type Fate } from '../../lib/collect'
import { parseTime } from '../../lib/paste'
import { t } from '../../lib/i18n'

/**
 * One butterfly of the day, as a card: species (a tap opens the search; the
 * day's species as chips), its form, sex as big ♀ / ♂ buttons, what happened to
 * it (to the insectary, preserved, released) with only what that needs (the
 * Insectary ID to write on the wings; CAM, tube, weight and medium), the time
 * and who caught it, a note, and the rest under «Más». Folded, it is one line;
 * the problems that keep it from being saved show in place.
 */
const props = defineProps<{
  draft: Draft
  number: number
  state: CollectState
  open: boolean
  /** The day's species as chips for a card without one (this list's first, then the latest collected). */
  quickSpecies: { species: string; form: string; label: string }[]
}>()
const emit = defineEmits<{ toggle: []; pickSpecies: []; duplicate: []; remove: [] }>()
const { header, setColumn, setFate, draftIssues, subspeciesFor, places, people, rainfalls, clouds, purposes, idProblem } = props.state

const d = computed(() => props.draft)
const issues = computed(() => draftIssues(d.value))
const issueOf = (...columns: Column[]) => issues.value.filter(i => columns.includes(i.column)).map(i => i.text)
/** Problems without a place of their own in the card (shown at its foot). */
const otherIssues = computed(() =>
  issues.value
    .filter(i => !['species', 'sex', 'insectaryId', 'cam', 'tube', 'weight', 'medium', 'time', 'collector'].includes(i.column))
    .map(i => i.text),
)
const forms = computed(() => subspeciesFor(d.value.species).slice(0, 8))
/** The media field samples go in (the sheet's list is long): the most used, and this one's. */
const mediumChips = computed(() => [...new Set([...props.state.fieldMediums.value, d.value.medium].filter(Boolean))])
/** The three fates as buttons (Mark_Released belongs to Monitoreo). */
const fateButtons = computed<{ fate: Fate; label: string; hint: string }[]>(() => [
  { fate: 'insectario', label: t('Al insectario'), hint: 'Collected_Sent2Insectary' },
  { fate: 'preservada', label: t('Preservada'), hint: 'Collected_Preserved' },
  { fate: 'liberada', label: t('Liberada'), hint: 'Released_Unmarked' },
])
/** «?» opens the unsure and unknown sexes (not for a live butterfly: Insectary_data needs it sure). */
const unsureOpen = ref(['female ?', 'male ?', 'NOT_COLLECTED'].includes(d.value.sex))
const unsureSexes = computed(() => (d.value.fate === 'insectario' ? [] : (['female ?', 'male ?', 'NOT_COLLECTED'] as const)))
function chooseFate(fate: Fate) {
  setFate(d.value, fate)
  props.state.addFate.value = fate
}
const team = computed(() => header.value.team)
/** The collector chips: the day's team, and this one's own if it is someone else. */
const collectors = computed(() => [...new Set([...team.value, d.value.collector].filter(Boolean))])
const moreOpen = ref(false)
/** What «Más» holds that differs from the day (shown on its button so nothing is hidden by surprise). */
const ownValues = computed(() =>
  [
    d.value.location !== header.value.location && d.value.location,
    d.value.identifier !== header.value.identifier && d.value.identifier && personCode(d.value.identifier),
    d.value.rainfall !== header.value.rainfall && d.value.rainfall && weatherLabel(d.value.rainfall).code,
    d.value.cloud !== header.value.cloud && d.value.cloud && weatherLabel(d.value.cloud).code,
    d.value.purpose && d.value.purpose,
  ].filter(Boolean) as string[],
)
const noteOpen = ref(!!d.value.notes)
const set = (column: Column, text: string) => setColumn(d.value, column, text)
const onTime = (e: Event) => set('time', parseTime((e.target as HTMLInputElement).value))
const value = (e: Event) => (e.target as HTMLInputElement).value
/** The folded card's line: what was chosen, at a glance. */
const idText = computed(() => (d.value.fate === 'insectario' ? d.value.insectaryId : d.value.fate === 'preservada' ? d.value.cam : t('Liberada')))
const choice = (on: boolean) =>
  on ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white text-stone-800 active:bg-stone-100'
const sexChoice = (sex: string, on: boolean) =>
  on
    ? sex.startsWith('female')
      ? 'border-pink-600 bg-pink-600 text-white'
      : sex.startsWith('male')
        ? 'border-sky-600 bg-sky-600 text-white'
        : 'border-stone-700 bg-stone-700 text-white'
    : 'border-stone-300 bg-white text-stone-800 active:bg-stone-100'
</script>

<template>
  <li
    class="rounded-xl border-2 bg-white shadow-sm"
    :class="open ? 'border-brand-600' : issues.length && !isEmptyDraft(draft) ? 'border-amber-300' : 'border-stone-200'"
    :data-card="draft.key"
  >
    <!-- Folded: one line; a tap opens it. -->
    <button v-if="!open" class="flex min-h-14 w-full items-center gap-2 px-3 py-2 text-left" :aria-expanded="false" @click="emit('toggle')">
      <span class="w-7 shrink-0 text-sm font-semibold text-stone-500 tabular-nums">{{ number }}</span>
      <span class="min-w-0 flex-1">
        <span class="block truncate text-base" :class="draft.species ? 'italic' : 'text-stone-400'">
          {{ [draft.species, draft.subspecies].filter(Boolean).join(' ') || $t('Sin especie') }}
        </span>
        <span class="flex items-center gap-1.5 truncate text-xs text-stone-600">
          <SexBadge :sex="draft.sex" />
          <span class="font-mono font-semibold" :class="draft.fate === 'insectario' ? 'text-brand-800' : ''">{{ idText }}</span>
          <span v-if="draft.time">· {{ draft.time }}</span>
          <span v-if="draft.collector">· {{ personCode(draft.collector) }}</span>
          <StickyNote v-if="draft.notes" :size="12" class="shrink-0" />
        </span>
        <span v-if="issues.length && !isEmptyDraft(draft)" class="flex items-center gap-1 truncate text-xs font-medium text-amber-800">
          <AlertTriangle :size="12" class="shrink-0" />{{ issues[0].text }}<template v-if="issues.length > 1"> (+{{ issues.length - 1 }})</template>
        </span>
      </span>
      <ChevronDown :size="20" class="shrink-0 text-stone-400" />
    </button>

    <div v-else class="space-y-4 px-3 pt-2 pb-3">
      <!-- Species: the search, or the day's species as chips. -->
      <div class="flex items-start gap-2">
        <span class="w-7 shrink-0 pt-3 text-sm font-semibold text-stone-500 tabular-nums">{{ number }}</span>
        <button
          class="flex min-h-12 min-w-0 flex-1 items-center gap-2 rounded-lg border px-3 py-1.5 text-left"
          :class="issueOf('species').length && !isEmptyDraft(draft) ? 'border-amber-500 bg-amber-50' : 'border-stone-300 bg-white active:bg-stone-50'"
          data-species
          @click="emit('pickSpecies')"
        >
          <Search :size="18" class="shrink-0 text-stone-400" />
          <span v-if="draft.species" class="min-w-0 flex-1">
            <span class="block truncate text-lg font-medium italic">{{ draft.species }}</span>
          </span>
          <span v-else class="flex-1 text-base text-stone-500">{{ $t('Elegir especie') }}</span>
        </button>
        <button class="grid h-12 w-11 shrink-0 place-items-center rounded-lg text-stone-500 active:bg-stone-100" :aria-label="$t('Plegar la tarjeta {n}', { n: number })" @click="emit('toggle')">
          <ChevronUp :size="20" />
        </button>
        <button class="grid h-12 w-11 shrink-0 place-items-center rounded-lg text-stone-500 active:bg-stone-100" :aria-label="$t('Quitar la mariposa {n}', { n: number })" @click="emit('remove')">
          <X :size="20" />
        </button>
      </div>
      <div v-if="!draft.species && quickSpecies.length" class="-mt-2 flex flex-wrap gap-1.5 pl-9">
        <button
          v-for="s in quickSpecies"
          :key="s.label"
          class="min-h-10 rounded-full border border-stone-300 bg-white px-3 text-sm italic active:bg-stone-100"
          @click="
            () => {
              set('species', s.species)
              set('subspecies', s.form)
            }
          "
        >
          {{ s.label }}
        </button>
      </div>
      <!-- The form: those used with this species. -->
      <div v-if="draft.species && (forms.length || draft.subspecies)" class="-mt-2 pl-9">
        <span class="field-label">{{ $t('Forma') }} <span class="font-normal text-stone-500">Subspecies_Form</span></span>
        <div class="flex flex-wrap gap-1.5">
          <button
            v-for="f in forms"
            :key="f"
            class="min-h-10 rounded-full border px-3 text-sm"
            :class="choice(draft.subspecies === f)"
            :aria-pressed="draft.subspecies === f"
            @click="set('subspecies', draft.subspecies === f ? '' : f)"
          >
            {{ f }}
          </button>
          <button
            v-if="draft.subspecies && !forms.includes(draft.subspecies)"
            class="min-h-10 rounded-full border px-3 text-sm"
            :class="choice(true)"
            aria-pressed="true"
            @click="set('subspecies', '')"
          >
            {{ draft.subspecies }}
          </button>
        </div>
      </div>
      <p v-for="text in issueOf('species')" v-show="!isEmptyDraft(draft)" :key="text" class="-mt-3 pl-9 text-sm text-amber-800">{{ text }}</p>

      <!-- Sex: big buttons; «?» for unsure or unknown. -->
      <div class="pl-9">
        <div class="grid grid-cols-[1fr_1fr_auto] gap-2">
          <button data-sex="female" class="h-12 rounded-lg border text-lg font-semibold" :class="sexChoice('female', draft.sex === 'female')" :aria-pressed="draft.sex === 'female'" @click="set('sex', 'female')">
            ♀ <span class="text-sm font-medium">female</span>
          </button>
          <button data-sex="male" class="h-12 rounded-lg border text-lg font-semibold" :class="sexChoice('male', draft.sex === 'male')" :aria-pressed="draft.sex === 'male'" @click="set('sex', 'male')">
            ♂ <span class="text-sm font-medium">male</span>
          </button>
          <button
            v-if="unsureSexes.length"
            class="h-12 w-12 rounded-lg border text-lg font-semibold"
            :class="choice(unsureOpen || unsureSexes.includes(draft.sex as never))"
            :aria-expanded="unsureOpen"
            :aria-label="$t('Sexo dudoso o no visto')"
            @click="unsureOpen = !unsureOpen"
          >
            ?
          </button>
        </div>
        <div v-if="unsureOpen && unsureSexes.length" class="mt-2 flex flex-wrap gap-1.5">
          <button
            v-for="s in unsureSexes"
            :key="s"
            class="min-h-10 rounded-full border px-3 text-sm"
            :class="sexChoice(s, draft.sex === s)"
            :aria-pressed="draft.sex === s"
            @click="set('sex', s)"
          >
            {{ s }}
          </button>
        </div>
        <p v-for="text in issueOf('sex')" v-show="!isEmptyDraft(draft)" :key="text" class="mt-1 text-sm text-amber-800">{{ text }}</p>
      </div>

      <!-- What happened to it, and only what that needs. -->
      <div class="pl-9">
        <div class="grid grid-cols-3 gap-2">
          <button
            v-for="f in fateButtons"
            :key="f.fate"
            class="min-h-12 rounded-lg border px-1 py-1 text-sm leading-tight font-semibold"
            :class="choice(draft.fate === f.fate)"
            :aria-pressed="draft.fate === f.fate"
            :title="f.hint"
            @click="chooseFate(f.fate)"
          >
            {{ f.label }}
          </button>
        </div>
        <p class="mt-1 text-xs text-stone-500">Release_Collect: {{ fateButtons.find(f => f.fate === draft.fate)?.hint }}</p>

        <label v-if="draft.fate === 'insectario'" class="mt-2 block">
          <span class="field-label">Insectary_ID <span class="font-normal text-stone-500">{{ $t('(escríbelo en las alas)') }}</span></span>
          <input
            :value="draft.insectaryId"
            class="field-input h-12 w-40 font-mono text-xl font-semibold tracking-wide text-brand-800 uppercase"
            :class="{ 'border-red-500 bg-red-50': issueOf('insectaryId').length }"
            autocapitalize="characters"
            autocomplete="off"
            spellcheck="false"
            enterkeyhint="done"
            data-field="insectaryId"
            @change="set('insectaryId', value($event))"
          />
          <span v-if="!issueOf('insectaryId').length" class="ml-2 text-xs text-stone-500">{{ $t('siguiente libre') }}</span>
          <span v-for="text in issueOf('insectaryId')" :key="text" class="mt-1 block text-sm text-red-700">{{ text }}</span>
        </label>

        <div v-else-if="draft.fate === 'preservada'" class="mt-2 space-y-2">
          <div class="grid grid-cols-2 gap-2">
            <label class="min-w-0">
              <span class="field-label">CAM_ID</span>
              <input
                :value="draft.cam"
                class="field-input h-11 font-mono text-base uppercase"
                :class="{ 'border-red-500 bg-red-50': issueOf('cam').length }"
                autocapitalize="characters"
                autocomplete="off"
                spellcheck="false"
                enterkeyhint="next"
                data-field="cam"
                @change="set('cam', value($event))"
              />
            </label>
            <label class="min-w-0">
              <span class="field-label">Tube_1_id</span>
              <input
                :value="draft.tube"
                class="field-input h-11 font-mono text-base uppercase"
                :class="{ 'border-red-500 bg-red-50': issueOf('tube').length }"
                autocapitalize="characters"
                autocomplete="off"
                spellcheck="false"
                enterkeyhint="next"
                data-field="tube"
                @change="set('tube', value($event))"
              />
            </label>
          </div>
          <p v-for="text in issueOf('cam', 'tube', 'medium')" :key="text" class="text-sm text-red-700">{{ text }}</p>
          <p v-if="!issueOf('cam', 'tube').length && (idProblem(draft, 'cam') === null)" class="text-xs text-stone-500">{{ $t('Los siguientes libres; cámbialos si la etiqueta dice otro.') }}</p>
          <div class="grid grid-cols-2 gap-2">
            <label class="min-w-0">
              <span class="field-label">{{ $t('Peso (g)') }} <span class="font-normal text-stone-500">Butterfly_weight</span></span>
              <input
                :value="draft.weight"
                class="field-input h-11 text-base"
                :class="{ 'border-red-500 bg-red-50': issueOf('weight').length }"
                inputmode="decimal"
                autocomplete="off"
                placeholder="0.152"
                enterkeyhint="done"
                data-field="weight"
                @change="set('weight', value($event))"
              />
            </label>
            <div class="min-w-0">
              <span class="field-label">Preserved_dead_alive</span>
              <div class="grid grid-cols-2 gap-1">
                <button class="h-11 rounded-lg border text-sm font-medium" :class="choice(draft.deadAlive !== 'Dead')" @click="set('deadAlive', 'Alive')">Alive</button>
                <button class="h-11 rounded-lg border text-sm font-medium" :class="choice(draft.deadAlive === 'Dead')" @click="set('deadAlive', 'Dead')">Dead</button>
              </div>
            </div>
          </div>
          <p v-for="text in issueOf('weight')" :key="text" class="text-sm text-red-700">{{ text }}</p>
          <p v-if="draft.deadAlive === 'Dead' && !draft.notes" class="text-xs text-amber-800">{{ $t('Añade en la nota cuánto tiempo llevaba muerta (p. ej. Preserved dead ~2h).') }}</p>
          <div v-if="mediumChips.length > 1">
            <span class="field-label">Preservation_medium</span>
            <div class="flex flex-wrap gap-1.5">
              <button
                v-for="m in mediumChips"
                :key="m"
                class="min-h-10 rounded-full border px-3 text-sm"
                :class="choice(draft.medium === m)"
                :aria-pressed="draft.medium === m"
                @click="set('medium', m)"
              >
                {{ m }}
              </button>
            </div>
          </div>
        </div>
      </div>

      <!-- When and who. -->
      <div class="flex flex-wrap items-end gap-3 pl-9">
        <label>
          <span class="field-label">{{ $t('Hora de captura') }} <span class="font-normal text-stone-500">Collection_time</span></span>
          <input
            :value="draft.time"
            class="field-input h-11 w-24 text-base tabular-nums"
            :class="{ 'border-amber-500 bg-amber-50': issueOf('time').length }"
            inputmode="numeric"
            placeholder="hh:mm"
            autocomplete="off"
            enterkeyhint="done"
            data-field="time"
            @change="onTime"
          />
        </label>
        <div v-if="collectors.length > 1" class="min-w-0">
          <span class="field-label">Collector</span>
          <div class="flex flex-wrap gap-1.5">
            <button
              v-for="p in collectors"
              :key="p"
              class="h-11 min-w-12 rounded-lg border px-2 text-sm font-semibold"
              :class="choice(draft.collector === p)"
              :aria-pressed="draft.collector === p"
              :title="p"
              @click="set('collector', p)"
            >
              {{ personCode(p) }}
            </button>
          </div>
        </div>
        <p v-else-if="draft.collector" class="pb-3 text-sm text-stone-600">Collector: {{ personCode(draft.collector) }}</p>
      </div>
      <p v-for="text in issueOf('time', 'collector')" :key="text" class="-mt-3 pl-9 text-sm text-amber-800">{{ text }}</p>

      <!-- A note, in English: the team's phrases or typed; saved dated and signed. -->
      <div class="pl-9">
        <button v-if="!noteOpen" class="flex min-h-10 items-center gap-1.5 text-sm text-brand-700" @click="noteOpen = true">
          <StickyNote :size="16" /> {{ $t('Añadir nota') }}
        </button>
        <template v-else>
          <span class="field-label">{{ $t('Nota') }} <span class="font-normal text-stone-500">Notes_Collection_data</span></span>
          <div class="mb-1.5 flex flex-wrap gap-1.5">
            <button
              v-for="p in COLLECT_NOTE_PHRASES"
              :key="p"
              class="min-h-9 rounded-full border border-stone-300 bg-white px-3 text-left text-xs active:bg-stone-100"
              @click="
                () => {
                  set('notes', addPhrase(draft.notes, p))
                  if (p === 'Preserved dead ~2h' && draft.fate === 'preservada') set('deadAlive', 'Dead')
                }
              "
            >
              {{ p }}
            </button>
          </div>
          <textarea
            :value="draft.notes"
            class="field-input min-h-16 text-base"
            rows="2"
            :placeholder="$t('Nota, en inglés (p. ej. Sexed by genitalia)')"
            enterkeyhint="done"
            data-field="notes"
            @input="set('notes', ($event.target as HTMLTextAreaElement).value)"
          />
        </template>
      </div>

      <!-- The rest: own place, identifier, weather, purpose (the day's unless changed here). -->
      <div class="pl-9">
        <button class="flex min-h-10 items-center gap-1 text-sm text-stone-600" :aria-expanded="moreOpen" @click="moreOpen = !moreOpen">
          <component :is="moreOpen ? ChevronUp : ChevronDown" :size="16" /> {{ $t('Más') }}
          <span v-if="ownValues.length" class="ml-1 rounded-md bg-violet-100 px-1.5 py-0.5 text-xs text-violet-900">{{ ownValues.join(' · ') }}</span>
        </button>
        <div v-if="moreOpen" class="mt-2 grid gap-2 sm:grid-cols-2">
          <label>
            <span class="field-label">Collection_location</span>
            <ChoiceField :model-value="draft.location" class="field-input h-11 text-base" :options="places" @update:model-value="set('location', $event)" />
          </label>
          <label>
            <span class="field-label">Identifier</span>
            <ChoiceField :model-value="draft.identifier" class="field-input h-11 text-base" :options="people" :freetext="false" @update:model-value="set('identifier', $event)" />
          </label>
          <label v-if="collectors.length <= 1">
            <span class="field-label">Collector</span>
            <ChoiceField :model-value="draft.collector" class="field-input h-11 text-base" :options="people" :freetext="false" @update:model-value="set('collector', $event)" />
          </label>
          <label>
            <span class="field-label">Rainfall</span>
            <ChoiceField :model-value="draft.rainfall" class="field-input h-11 text-base" :options="rainfalls" :freetext="false" allow-empty @update:model-value="set('rainfall', $event)" />
          </label>
          <label>
            <span class="field-label">Cloud_cover</span>
            <ChoiceField :model-value="draft.cloud" class="field-input h-11 text-base" :options="clouds" :freetext="false" allow-empty @update:model-value="set('cloud', $event)" />
          </label>
          <label>
            <span class="field-label">Purpose <span class="font-normal text-stone-500">{{ $t('(NA si no hay)') }}</span></span>
            <ChoiceField :model-value="draft.purpose" class="field-input h-11 text-base" :options="purposes" allow-empty @update:model-value="set('purpose', $event)" />
          </label>
        </div>
      </div>
      <p v-for="text in otherIssues" v-show="!isEmptyDraft(draft)" :key="text" class="pl-9 text-sm text-amber-800">{{ text }}</p>

      <button
        class="ml-9 flex min-h-11 items-center gap-2 rounded-lg border border-brand-600 bg-white px-4 text-base font-medium text-brand-800 active:bg-brand-50"
        :disabled="!draft.species"
        :class="{ 'opacity-50': !draft.species }"
        @click="emit('duplicate')"
      >
        <CopyPlus :size="18" /> {{ $t('Otra igual') }}
      </button>
    </div>
  </li>
</template>
