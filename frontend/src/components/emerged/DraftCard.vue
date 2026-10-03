<script setup lang="ts">
import { computed, nextTick, ref } from 'vue'
import { AlertTriangle, Pencil, StickyNote, X } from 'lucide-vue-next'
import ChoiceField from '../ChoiceField.vue'
import { EMERGED_NOTE_PHRASES, LIFESTAGES, preserving, speciesOf, type Draft, type Fate, type Sex } from '../../lib/emerged'
import { dayFirst } from '../../lib/dates'
import { t } from '../../lib/i18n'

/**
 * One butterfly being registered (or an egg or larva preserved): its
 * Insectary ID (the next free one, or what the wing says: tap to change it),
 * its sex, what became of it on its emergence day, what emerged when it is not
 * the clutch's species, a note, and the CAM and tube of a preserved body.
 * Every change is emitted as a patch; the cards keep them (useEmergedState).
 */
const props = defineProps<{
  draft: Draft
  clutchSpecies: string
  /** The clutch's species and its other subspecies (siblingSpecies). */
  siblings: string[]
  /** Every species, to type one that is not among the siblings. */
  allSpecies: string[]
  /** What is wrong with this card (its ID, its species, its CAM or tube), in words. */
  problems: string[]
  /** Worth a look, not blocking (an ID from an empty row higher up in the sheet). */
  hint?: string
  /** The day shown above (a card of another day says its own). */
  day: string
  canEdit: boolean
  /** Just added: lit for a moment. */
  fresh?: boolean
}>()
const emit = defineEmits<{ update: [patch: Partial<Draft>]; remove: [] }>()

const editingId = ref(false)
const idText = ref('')
const idInput = ref<HTMLInputElement>()
async function editId() {
  if (!props.canEdit) return
  idText.value = props.draft.id
  editingId.value = true
  await nextTick()
  idInput.value?.select()
}
function commitId() {
  const id = idText.value.trim().toUpperCase()
  editingId.value = false
  if (id && id !== props.draft.id) emit('update', { id })
}

const SEXES: { value: Sex; label: string; name: () => string; on: string }[] = [
  { value: 'female', label: '♀', name: () => t('Hembra'), on: 'border-pink-600 bg-pink-600 text-white' },
  { value: 'male', label: '♂', name: () => t('Macho'), on: 'border-sky-600 bg-sky-600 text-white' },
  { value: 'NA', label: '?', name: () => t('Sexo no visible (NA)'), on: 'border-stone-600 bg-stone-600 text-white' },
]
const FATES: { value: Fate; name: () => string; hint: () => string }[] = [
  { value: 'alive', name: () => t('Viva'), hint: () => t('Entra viva al insectario') },
  { value: 'deformed', name: () => t('Deforme'), hint: () => t('Murió deforme el día que emergió (Deformed)') },
  { value: 'dead', name: () => t('Murió'), hint: () => t('Murió el día que emergió (Unknown)') },
  { value: 'preserved', name: () => t('Preservada'), hint: () => t('Sacrificada y preservada el día que emergió: CAM y tubo') },
]
const STAGE_SHORT: Record<string, string> = {
  Egg: 'Egg',
  '1st instar larva': 'L1',
  '2nd instar larva': 'L2',
  '3rd instar larva': 'L3',
  '4th instar larva': 'L4',
  '5th instar larva': 'L5',
  'Pre-pupa': 'Pre-pupa',
}

const species = computed(() => speciesOf(props.draft, props.clutchSpecies))
/** What emerged is not the clutch's species (typed over the formula). */
const own = computed(() => !!props.draft.species && props.draft.species !== props.clutchSpecies)
const showSpecies = ref(false)
const showNote = ref(!!props.draft.note)
function pickSpecies(s: string) {
  emit('update', { species: s === props.clutchSpecies ? '' : s })
  if (s) showSpecies.value = false
}
function addPhrase(p: string) {
  const text = props.draft.note.trim()
  emit('update', { note: text ? `${text}; ${p}` : p })
}
const choice = (on: boolean, color = 'border-brand-700 bg-brand-700 text-white') =>
  on ? color : 'border-stone-300 bg-white text-stone-800 active:bg-stone-100'
const otherDay = computed(() => props.draft.date !== props.day)
</script>

<template>
  <li
    class="relative flex flex-col rounded-xl border-2 bg-white shadow-sm transition-shadow duration-700"
    :class="[problems.length ? 'border-amber-400' : 'border-stone-200', fresh ? 'shadow-[0_0_0_4px_var(--color-brand-100)]' : '']"
    :data-draft="draft.id"
  >
    <div class="flex items-center gap-1.5 px-2 pt-2">
      <!-- The ID on the wing: tap to change it. -->
      <input
        v-if="editingId"
        ref="idInput"
        v-model="idText"
        class="field-input h-11 w-28 text-xl font-semibold uppercase"
        autocapitalize="characters"
        autocomplete="off"
        autocorrect="off"
        spellcheck="false"
        enterkeyhint="done"
        :aria-label="$t('Insectary ID escrito en el ala')"
        @keydown.enter.prevent="commitId"
        @keydown.escape="editingId = false"
        @blur="commitId"
      />
      <button
        v-else
        class="flex h-11 items-center gap-1 rounded-lg px-1 text-2xl font-bold tracking-wide tabular-nums active:bg-stone-100"
        :disabled="!canEdit"
        :title="$t('Cambiar el ID: el que está escrito en el ala')"
        :aria-label="$t('Insectary ID {id}: tocar para cambiarlo', { id: draft.id })"
        @click="editId"
      >
        {{ draft.id || '—' }} <Pencil v-if="canEdit" :size="14" class="text-stone-400" />
      </button>
      <!-- An adult's sex, beside its ID (one tap). -->
      <span v-if="draft.kind === 'adult'" class="flex gap-1" role="group" :aria-label="$t('Sexo')">
        <button
          v-for="s in SEXES"
          :key="s.value"
          class="h-11 w-11 rounded-lg border text-xl leading-none font-bold"
          :class="choice(draft.sex === s.value, s.on)"
          :aria-pressed="draft.sex === s.value"
          :aria-label="s.name()"
          :title="s.name()"
          :disabled="!canEdit"
          @click="emit('update', { sex: s.value })"
        >
          {{ s.label }}
        </button>
      </span>
      <span v-else class="rounded-full bg-violet-100 px-2 py-0.5 text-xs font-medium text-violet-900">{{ $t('Huevo o larva') }}</span>
      <span class="ml-auto flex shrink-0">
        <button
          class="grid h-11 w-11 place-items-center rounded-lg active:bg-stone-100"
          :class="draft.note || showNote ? 'text-brand-700' : 'text-stone-500'"
          :aria-label="$t('Nota')"
          :aria-pressed="showNote"
          :title="$t('Nota')"
          @click="showNote = !showNote"
        >
          <StickyNote :size="19" />
        </button>
        <button v-if="canEdit" class="grid h-11 w-11 place-items-center rounded-lg text-stone-500 active:bg-stone-100" :aria-label="$t('Quitar {id}', { id: draft.id })" @click="emit('remove')">
          <X :size="20" />
        </button>
      </span>
    </div>

    <!-- What emerged: the clutch's species (the sheet's formula) unless another one is chosen. -->
    <div class="flex min-w-0 items-center gap-2 px-3">
      <span class="min-w-0 flex-1 truncate text-sm" :class="own ? 'font-medium text-violet-900' : species ? 'text-stone-700' : 'font-medium text-amber-900'">
        {{ species || $t('Falta la especie') }}
      </span>
      <span v-if="otherDay" class="shrink-0 rounded-full bg-amber-100 px-2 py-0.5 text-xs font-medium text-amber-900">{{ dayFirst(draft.date) }}</span>
      <button
        v-if="canEdit"
        class="h-9 shrink-0 rounded-full border px-3 text-xs font-medium"
        :class="own || showSpecies ? 'border-violet-400 bg-violet-50 text-violet-900' : 'border-stone-300 bg-white text-stone-700'"
        :aria-expanded="showSpecies"
        @click="showSpecies = !showSpecies"
      >
        {{ !clutchSpecies ? $t('Especie') : siblings.length > 1 ? $t('Otra subespecie') : $t('Otra especie') }}
      </button>
    </div>
    <div v-if="showSpecies && canEdit" class="mx-2 mt-1.5 rounded-lg border border-violet-200 bg-violet-50/60 p-2">
      <div class="flex flex-wrap gap-1.5">
        <button
          v-for="s in siblings"
          :key="s"
          class="min-h-10 rounded-lg border px-2.5 text-left text-sm"
          :class="choice(species === s, 'border-violet-700 bg-violet-700 text-white')"
          :aria-pressed="species === s"
          @click="pickSpecies(s)"
        >
          {{ s === clutchSpecies ? `${s} (${$t('del clutch')})` : s }}
        </button>
      </div>
      <label class="mt-2 block">
        <span class="field-label">{{ $t('Otra especie') }}</span>
        <ChoiceField :model-value="own ? draft.species : ''" class="field-input h-11 text-base" :options="allSpecies" @update:model-value="pickSpecies($event)" />
      </label>
    </div>

    <!-- An adult: its sex and what became of it on its emergence day. -->
    <template v-if="draft.kind === 'adult'">
      <div class="mt-1.5 grid grid-cols-4 gap-1 px-2" role="group" :aria-label="$t('Al emerger')">
        <button
          v-for="f in FATES"
          :key="f.value"
          class="min-h-10 rounded-lg border px-0.5 text-[13px] font-medium"
          :class="choice(draft.fate === f.value, f.value === 'alive' ? 'border-brand-700 bg-brand-700 text-white' : 'border-amber-700 bg-amber-700 text-white')"
          :aria-pressed="draft.fate === f.value"
          :title="f.hint()"
          :disabled="!canEdit"
          @click="emit('update', { fate: f.value })"
        >
          {{ f.name() }}
        </button>
      </div>
    </template>
    <!-- An egg or larva preserved: its stage, alive or found dead. -->
    <template v-else>
      <div class="mt-2 flex flex-wrap gap-1 px-2" role="group" aria-label="LIFESTAGE">
        <button
          v-for="s in LIFESTAGES"
          :key="s"
          class="min-h-10 min-w-11 rounded-lg border px-2 text-sm font-medium"
          :class="choice(draft.stage === s)"
          :aria-pressed="draft.stage === s"
          :title="s"
          :disabled="!canEdit"
          @click="emit('update', { stage: s })"
        >
          {{ STAGE_SHORT[s] }}
        </button>
      </div>
      <div class="mt-1.5 grid grid-cols-2 gap-1.5 px-2">
        <button class="min-h-10 rounded-lg border text-sm font-medium" :class="choice(!draft.foundDead)" :aria-pressed="!draft.foundDead" :disabled="!canEdit" @click="emit('update', { foundDead: false })">
          {{ $t('Viva, preservada') }}
        </button>
        <button class="min-h-10 rounded-lg border text-sm font-medium" :class="choice(draft.foundDead, 'border-amber-700 bg-amber-700 text-white')" :aria-pressed="draft.foundDead" :disabled="!canEdit" @click="emit('update', { foundDead: true })">
          {{ $t('Encontrada muerta') }}
        </button>
      </div>
    </template>

    <!-- A preserved body: its CAM and tube (the next free ones, or what the envelope says). -->
    <div v-if="preserving(draft)" class="mt-1.5 grid grid-cols-2 gap-1.5 px-2">
      <label class="min-w-0">
        <span class="field-label">CAM_ID</span>
        <input
          :value="draft.cam"
          class="field-input h-11 text-base uppercase"
          :class="{ 'border-amber-500 ring-2 ring-amber-200': !draft.cam }"
          :placeholder="$t('Falta')"
          autocapitalize="characters"
          autocomplete="off"
          spellcheck="false"
          enterkeyhint="next"
          :data-sample="`${draft.key}:cam`"
          :disabled="!canEdit"
          @input="emit('update', { cam: ($event.target as HTMLInputElement).value.toUpperCase() })"
        />
      </label>
      <label class="min-w-0">
        <span class="field-label">Tube_1_id</span>
        <input
          :value="draft.tube"
          class="field-input h-11 text-base uppercase"
          :class="{ 'border-amber-500 ring-2 ring-amber-200': !draft.tube }"
          :placeholder="$t('Falta')"
          autocapitalize="characters"
          autocomplete="off"
          spellcheck="false"
          enterkeyhint="done"
          :data-sample="`${draft.key}:tube`"
          :disabled="!canEdit"
          @input="emit('update', { tube: ($event.target as HTMLInputElement).value.toUpperCase() })"
        />
      </label>
    </div>

    <!-- A note, in English: quick phrases or typed; Save adds it dated and signed. -->
    <div v-if="showNote" class="mt-1.5 px-2">
      <div v-if="canEdit && draft.kind === 'adult'" class="mb-1.5 flex flex-wrap gap-1">
        <button v-for="p in EMERGED_NOTE_PHRASES" :key="p" type="button" class="min-h-9 rounded-full border border-stone-300 bg-white px-2.5 text-xs active:bg-stone-100" @click="addPhrase(p)">
          {{ p }}
        </button>
      </div>
      <input
        :value="draft.note"
        class="field-input h-11 text-base"
        :placeholder="draft.kind === 'young' ? $t('Nota (por defecto: {note})', { note: 'Preserved alive 4th instar' }) : $t('Nota, en inglés (p. ej. Deformed wings, can fly)')"
        enterkeyhint="done"
        :disabled="!canEdit"
        @input="emit('update', { note: ($event.target as HTMLInputElement).value })"
      />
    </div>
    <p v-else-if="draft.note" class="mx-2 mt-1.5 truncate rounded-md bg-stone-100 px-1.5 py-0.5 text-xs text-stone-700">{{ draft.note }}</p>

    <ul v-if="problems.length" class="mt-1.5 space-y-0.5 rounded-b-[10px] border-t border-amber-200 bg-amber-50 px-3 py-1.5 text-sm text-amber-950">
      <li v-for="p in problems" :key="p" class="flex items-start gap-1.5"><AlertTriangle :size="15" class="mt-0.5 shrink-0" />{{ p }}</li>
    </ul>
    <p v-else-if="hint" class="mt-1.5 flex items-start gap-1.5 rounded-b-[10px] border-t border-amber-100 bg-amber-50/60 px-3 py-1.5 text-xs text-amber-900">
      <AlertTriangle :size="14" class="mt-px shrink-0" />{{ hint }}
    </p>
    <div v-else class="h-2" />
  </li>
</template>
