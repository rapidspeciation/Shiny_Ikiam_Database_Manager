<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { Loader2, X } from 'lucide-vue-next'
import ChoiceField from '../ChoiceField.vue'
import DateField from '../DateField.vue'
import ParentsPicker from './ParentsPicker.vue'
import { useKeyboard } from '../../composables/usePhone'
import { useParents } from '../../composables/useParents'
import { isBlank } from '../../lib/cells'
import { MODULE, appendNote, hasClutch, nextBatch, nextClutch, parentsText, sameMating } from '../../lib/clutches'
import { dayLabel, isoToSerial, serialFromIso, serialToIso, todayIso } from '../../lib/dates'
import { errorText, notify } from '../../lib/notice'
import type { CellValue, TableRow } from '../../lib/types'
import { usePending } from '../../stores/pending'
import { t } from '../../lib/i18n'

/**
 * A new clutch, as the table's «Nuevo clutch» writes it (a new row in the next
 * free row of Insectary_stocks, saved at once): its number (the next one, or the
 * next batch N(k) of the same parents), species (the mother's when the parents
 * are given), date laid, eggs as the team's formula (=12), room, generation and
 * the parents in NOTES ("d/m/yy INI: U8A♀ + C8B♂").
 */
const props = defineProps<{
  rows: TableRow[]
  numbers: string[]
  species: string[]
  createFormulas: string[]
  initials: string
  hasGeneration: boolean
  docked?: boolean
}>()
const emit = defineEmits<{ close: []; created: [clutch: string] }>()

const pending = usePending()
const keyboard = useKeyboard()
const parents = useParents()
const withParents = ref(false)
const female = ref('')
const male = ref('')
const number = ref('')
const numberTyped = ref(false)
const chosen = ref('')
const speciesTyped = ref(false)
const date = ref(todayIso())
const eggs = ref('')
const place = ref('Insectary')
const generation = ref('NA')
const note = ref('')
const message = ref('')
const saving = ref(false)

const mating = computed(() =>
  withParents.value
    ? sameMating(
        props.rows.map(r => ({ number: String(r.values['CLUTCH NUMBER'] ?? ''), notes: r.values.NOTES })),
        female.value,
        male.value,
      )
    : null,
)
/** The number suggested: the next batch of the same parents, else the next free number. */
const suggested = computed(() => (mating.value ? nextBatch(Number(mating.value), props.numbers) : nextClutch(props.numbers)))
watch(suggested, s => {
  if (!numberTyped.value) number.value = s
}, { immediate: true })
watch([female, male, () => parents.loaded.value], () => {
  if (!withParents.value || speciesTyped.value) return
  const s = parents.speciesFrom(female.value, male.value, props.species)
  if (s) chosen.value = s
})
watch(withParents, on => {
  if (on && generation.value === 'NA') generation.value = 'F1'
  if (!on && generation.value === 'F1') generation.value = 'NA'
})
const quickDates = computed(() => [
  { iso: todayIso(), name: t('Hoy') },
  { iso: serialToIso(isoToSerial(todayIso()) - 1), name: t('Ayer') },
])

/** A row with this number already: an empty one (only the number written) is filled; one with a clutch is refused. */
const existing = computed(() => {
  const n = number.value.trim()
  return props.rows.find(r => String(r.values['CLUTCH NUMBER'] ?? '').trim() === n) ?? null
})
const blocker = computed(() => {
  const n = number.value.trim()
  if (!n) return t('Escribe el número del clutch')
  if (existing.value && hasClutch(f => pending.value(existing.value!, f))) return t('El clutch {clutch} ya existe', { clutch: n })
  if (pending.creates.some(c => c.module === MODULE && String(c.values['CLUTCH NUMBER']) === n))
    return t('El clutch {clutch} ya existe', { clutch: n })
  if (!chosen.value) return t('Elige la especie del clutch')
  if (date.value && serialFromIso(date.value) === null) return t('Fecha no válida: el año debe estar entre 1990 y 2099')
  if (eggs.value.trim() && !/^\d{1,4}$/.test(eggs.value.trim())) return t('Huevos: escribe un número')
  if (withParents.value && (!female.value.trim() || !male.value.trim())) return t('Escribe el ID de la hembra y del macho')
  return ''
})

async function create() {
  if (blocker.value || saving.value) return
  saving.value = true
  message.value = ''
  const clutch = number.value.trim()
  const today = isoToSerial(todayIso())
  let notes: CellValue = null
  if (withParents.value) notes = appendNote(null, parentsText(female.value, male.value), today, props.initials)
  if (note.value.trim()) notes = appendNote(notes, note.value.trim(), today, props.initials)
  const values: Record<string, CellValue> = {
    'CLUTCH NUMBER': /^\d+$/.test(clutch) ? Number(clutch) : clutch,
    SPECIES: chosen.value,
    'DATE LAID': date.value ? serialFromIso(date.value) : null,
    // Counts are the team's formulas, even a single one (=12).
    'NUMBER OF EGGS': eggs.value.trim() ? `=${Number(eggs.value.trim())}` : null,
    'INSECTARY OR LABORATORY': place.value,
    NOTES: notes,
  }
  if (props.hasGeneration) values.Generation = generation.value
  try {
    if (existing.value) {
      // A row with only the number written beforehand: its cells are filled.
      for (const [field, value] of Object.entries(values))
        if (field !== 'CLUTCH NUMBER' && value !== null && !existing.value.formulas.includes(field) && isBlank(pending.value(existing.value, field)))
          pending.setCell(MODULE, existing.value, clutch, field, value)
    } else {
      for (const field of props.createFormulas) delete values[field]
      pending.addCreate(MODULE, clutch, values)
    }
    pending.touch()
    for (let i = 0; i < 300 && pending.saving; i++) await new Promise(r => setTimeout(r, 100))
    await pending.save('')
    const refused = Object.values(pending.issues)[0]
    if (refused) {
      message.value = t('No se guardó {ids}: {reason}', { ids: clutch, reason: refused })
      return
    }
    notify(t('Clutch {clutch} añadido', { clutch }), 'success')
    emit('created', clutch)
  } catch (e) {
    message.value = errorText(e)
  } finally {
    saving.value = false
  }
}
/** A phone on its side with the keyboard up: the header and the button step aside for the box typed in. */
const tight = computed(() => keyboard.open.value && keyboard.visibleBottom.value - keyboard.visibleTop.value < 360)
const overlayStyle = computed(() =>
  props.docked ? undefined : { top: `${keyboard.visibleTop.value}px`, height: `${keyboard.visibleBottom.value - keyboard.visibleTop.value}px` },
)
/** The box being typed in, back in the middle of what is left once the keyboard has settled. */
let settle: ReturnType<typeof setTimeout> | undefined
watch([keyboard.visibleBottom, keyboard.visibleTop], () => {
  clearTimeout(settle)
  settle = setTimeout(() => {
    const el = document.activeElement as HTMLElement | null
    if (el?.closest('[aria-label]') && el.matches('input, textarea')) el.scrollIntoView({ block: 'center' })
  }, 150)
})
function reveal(e: FocusEvent) {
  const el = e.target as HTMLElement
  if (el.matches('input, textarea')) setTimeout(() => el.scrollIntoView({ block: 'center', behavior: 'smooth' }), 350)
}
const generations = ['NA', 'F1', 'F2', 'Backcross']
</script>

<template>
  <div
    :class="docked ? 'flex h-full min-h-0 flex-col bg-white' : 'fixed inset-x-0 z-40 flex flex-col bg-white'"
    :style="overlayStyle"
    :role="docked ? 'region' : 'dialog'"
    :aria-label="$t('Nuevo clutch')"
  >
    <header v-show="!tight" class="flex items-center gap-2 border-b border-stone-200 py-1 pr-1 pl-4">
      <h2 class="min-w-0 flex-1 text-lg font-semibold">{{ $t('Nuevo clutch') }}</h2>
      <button class="grid h-11 w-11 place-items-center rounded-md text-stone-700" :aria-label="$t('Cerrar')" @click="emit('close')">
        <X :size="22" />
      </button>
    </header>
    <div class="min-h-0 flex-1 space-y-4 overflow-y-auto px-4 py-3" @focusin="reveal">
      <section>
        <div class="grid grid-cols-2 gap-2">
          <button
            type="button"
            class="min-h-11 rounded-lg border px-2 text-sm font-medium"
            :class="!withParents ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white'"
            :aria-pressed="!withParents"
            @click="withParents = false"
          >
            {{ $t('De stock') }}
          </button>
          <button
            type="button"
            class="min-h-11 rounded-lg border px-2 text-sm font-medium"
            :class="withParents ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white'"
            :aria-pressed="withParents"
            @click="withParents = true"
          >
            {{ $t('Cruce: con padres') }}
          </button>
        </div>
        <ParentsPicker v-if="withParents" v-model:female="female" v-model:male="male" class="mt-2" :parents="parents" />
      </section>
      <section class="grid grid-cols-2 gap-2">
        <label class="block min-w-0">
          <span class="field-label">CLUTCH NUMBER</span>
          <input
            v-model="number"
            class="field-input h-12 text-lg font-semibold"
            autocomplete="off"
            enterkeyhint="next"
            @input="numberTyped = true"
          />
          <span class="text-xs text-stone-500">
            <template v-if="mating">{{ $t('siguiente lote de {base} (mismos padres)', { base: mating }) }}</template>
            <template v-else>{{ $t('siguiente número: {n}', { n: suggested }) }}</template>
          </span>
        </label>
        <label class="block min-w-0">
          <span class="field-label">NUMBER OF EGGS</span>
          <input
            v-model="eggs"
            class="field-input h-12 text-lg"
            type="text"
            inputmode="numeric"
            pattern="[0-9]*"
            autocomplete="off"
            enterkeyhint="done"
          />
          <span v-if="eggs.trim()" class="text-xs text-stone-500">={{ eggs.trim() }}</span>
        </label>
      </section>
      <label class="block">
        <span class="field-label">SPECIES</span>
        <ChoiceField v-model="chosen" class="field-input h-12 text-base" :options="species" @update:model-value="speciesTyped = true" />
      </label>
      <section>
        <span class="field-label">DATE LAID</span>
        <div class="flex gap-2">
          <DateField v-model="date" class="field-input h-12 text-base" />
          <button v-for="d in quickDates" :key="d.iso" type="button" class="btn h-12 shrink-0 px-3" :class="{ 'border-brand-600 text-brand-800': date === d.iso }" @click="date = d.iso">
            {{ d.name }}
          </button>
        </div>
        <p v-if="date && serialFromIso(date) !== null" class="mt-1 text-xs text-stone-600">{{ dayLabel(date) }}</p>
      </section>
      <section>
        <span class="field-label">INSECTARY OR LABORATORY</span>
        <div class="grid grid-cols-2 gap-2">
          <button
            v-for="p in ['Insectary', 'Laboratory']"
            :key="p"
            type="button"
            class="min-h-11 rounded-lg border text-sm font-medium"
            :class="place === p ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white'"
            :aria-pressed="place === p"
            @click="place = p"
          >
            {{ p }}
          </button>
        </div>
      </section>
      <section v-if="hasGeneration">
        <span class="field-label">Generation</span>
        <div class="grid grid-cols-4 gap-2">
          <button
            v-for="g in generations"
            :key="g"
            type="button"
            class="min-h-11 rounded-lg border text-sm font-medium"
            :class="generation === g ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white'"
            :aria-pressed="generation === g"
            @click="generation = g"
          >
            {{ g }}
          </button>
        </div>
      </section>
      <label class="block">
        <span class="field-label">NOTES</span>
        <textarea v-model="note" class="field-input min-h-16 text-base" rows="2" :placeholder="$t('Nota, en inglés (opcional)')" />
      </label>
    </div>
    <footer v-show="!tight" class="border-t border-stone-200 bg-white px-3 pt-2 pb-[calc(0.5rem+env(safe-area-inset-bottom))]">
      <p class="mb-1.5 truncate text-xs" :class="message || blocker ? 'text-amber-900' : 'text-stone-600'">
        {{ message || blocker || [number, chosen, date && dayLabel(date).split(' · ')[0]].filter(Boolean).join(' · ') }}
      </p>
      <button class="btn-primary h-12 w-full text-base" :disabled="!!blocker || saving" @click="create">
        <Loader2 v-if="saving" :size="18" class="animate-spin" />
        {{ saving ? $t('Guardando…') : $t('Añadir clutch {clutch}', { clutch: number.trim() }) }}
      </button>
    </footer>
  </div>
</template>
