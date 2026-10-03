<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, ref, watch } from 'vue'
import { AlertTriangle, Check, ChevronDown, ChevronUp, History, Loader2, Plus, Trash2, Undo2, X } from 'lucide-vue-next'
import ButterflyCard from './ButterflyCard.vue'
import DayPanel from './DayPanel.vue'
import SaveSummary from './SaveSummary.vue'
import SpeciesPicker from './SpeciesPicker.vue'
import EntryModeToggle from '../EntryModeToggle.vue'
import InsectaryIdsWarning from '../InsectaryIdsWarning.vue'
import SexBadge from '../SexBadge.vue'
import TabHistory from '../history/TabHistory.vue'
import type { CollectState } from '../../composables/useCollect'
import type { EntryMode } from '../../composables/useEntryMode'
import { useKeyboard, useMedia } from '../../composables/usePhone'
import { isEmptyDraft, personCode, speciesEntries, speciesTotals, type Draft, type SpeciesEntry } from '../../lib/collect'
import { formatSerial, isoToSerial, todayIso, weekdayOf } from '../../lib/dates'
import { notify } from '../../lib/notice'
import { useSession } from '../../stores/session'
import { useTables } from '../../stores/tables'
import { t, tn } from '../../lib/i18n'

/**
 * Colecta as cards, for a thumb (and the default on computers too): the day
 * on top (date, place, who went out, who identified, weather), folded into
 * one line once chosen; then one card per butterfly, in the order they are
 * typed (the notebook's order: CAMs and IDs run on); «+ Mariposa» and «Otra
 * igual» add the next one; the totals per species; one Save for the whole
 * day, with an Undo. Shares the list and the day with the table (useCollect).
 */
const props = defineProps<{ state: CollectState }>()
const mode = defineModel<EntryMode>('mode', { required: true })
const state = props.state
const {
  ready,
  header,
  drafts,
  addFate,
  recentSpecies,
  allSpecies,
  subspeciesFor,
  options,
  addDrafts,
  duplicate,
  remove,
  draftIssues,
  earlierIds,
  loadFreeIds,
  saving,
  save,
  lastSave,
  undoing,
  undo,
  clearAll,
} = state

const session = useSession()
const tables = useTables()
const keyboard = useKeyboard()
const roomy = useMedia('(min-width: 1024px)')
const canEdit = computed(() => session.canEdit)

// --- The day: open until the place, the people and the identifier are chosen
const dayReady = computed(() => !!header.value.date && !!header.value.location && header.value.team.length > 0 && !!header.value.identifier)
const dayOpen = ref(!dayReady.value || !drafts.value.length)
const dayLine = computed(() => {
  const h = header.value
  const day = h.date ? `${weekdayOf(h.date).slice(0, 3)} ${formatSerial(isoToSerial(h.date))}` : t('sin fecha')
  return [day, h.location || t('sin lugar'), h.team.map(personCode).join(', ') || t('sin Collector')].join(' · ')
})
function closeDay() {
  dayOpen.value = false
  if (!drafts.value.length && dayReady.value) addButterfly()
}

// --- The cards: one open at a time (the one being typed)
const openKey = ref<string | null>(drafts.value.at(-1)?.key ?? null)
const scroller = ref<HTMLElement>()
async function reveal(key: string) {
  await nextTick()
  const el = scroller.value?.querySelector<HTMLElement>(`[data-card="${CSS.escape(key)}"]`)
  el?.scrollIntoView({ block: 'start', behavior: 'smooth' })
}
function toggle(d: Draft) {
  openKey.value = openKey.value === d.key ? null : d.key
  if (openKey.value) reveal(d.key)
}
/** The next butterfly: the fate of the last one chosen, and the Collector of the card before it. */
function addButterfly() {
  if (!header.value.location) {
    dayOpen.value = true
    return notify(t('Elige el lugar de colecta'))
  }
  const before = drafts.value.at(-1)
  const at = addDrafts(1, { fate: addFate.value })
  const d = drafts.value[at]
  if (before?.collector && header.value.team.includes(before.collector)) d.collector = before.collector
  openKey.value = d.key
  reveal(d.key)
}
function another(d: Draft) {
  const copy = duplicate(d)
  openKey.value = copy.key
  reveal(copy.key)
}
function removeCard(d: Draft, n: number) {
  remove(d.key)
  if (!isEmptyDraft(d)) notify(t('Quitada la mariposa {n}', { n }))
}

// --- Species: this list's and the latest collected first; the picker over the page
const entries = computed(() =>
  speciesEntries([...new Set([...recentSpecies.value, ...allSpecies.value])], subspeciesFor, drafts.value),
)
/** Chips on a card without a species: this list's species (with their forms), then the latest collected. */
const quickSpecies = computed<SpeciesEntry[]>(() => {
  const out: SpeciesEntry[] = []
  const seen = new Set<string>()
  for (const d of [...drafts.value].reverse())
    if (d.species && !seen.has(`${d.species} ${d.subspecies}`)) {
      seen.add(`${d.species} ${d.subspecies}`)
      out.push({ species: d.species, form: d.subspecies, label: [d.species, d.subspecies].filter(Boolean).join(' ') })
    }
  for (const species of recentSpecies.value)
    if (out.length < 6 && !out.some(e => e.species === species)) out.push({ species, form: '', label: species })
  return out.slice(0, 6)
})
const picking = ref<Draft | null>(null)
function pickSpecies(entry: SpeciesEntry) {
  const d = picking.value
  picking.value = null
  if (!d) return
  state.setColumn(d, 'species', entry.species)
  state.setColumn(d, 'subspecies', entry.form)
}

// --- Totals per species, and what keeps the day from being saved
const totals = computed(() => speciesTotals(drafts.value))
const counts = computed(() => {
  const filled = drafts.value.filter(d => !isEmptyDraft(d))
  return {
    insectario: filled.filter(d => d.fate === 'insectario').length,
    preservada: filled.filter(d => d.fate === 'preservada').length,
    liberada: filled.filter(d => d.fate === 'liberada').length,
  }
})
const blocker = computed<{ text: string; key?: string }>(() => {
  if (!drafts.value.length) return { text: t('Añade al menos una mariposa') }
  if (!header.value.date) return { text: t('Elige la fecha') }
  for (const [i, d] of drafts.value.entries()) {
    if (isEmptyDraft(d)) return { text: t('La mariposa {n} está vacía: llénala o quítala', { n: i + 1 }), key: d.key }
    const issue = draftIssues(d)[0]
    if (issue) return { text: `${i + 1}: ${issue.text}`, key: d.key }
  }
  return { text: '' }
})
function goTo(key?: string) {
  if (!key) return
  openKey.value = key
  reveal(key)
}
const summaryLine = computed(() =>
  [
    counts.value.insectario && t('{n} al insectario', { n: counts.value.insectario }),
    counts.value.preservada && t('{n} preservadas', { n: counts.value.preservada }),
    counts.value.liberada && t('{n} liberadas', { n: counts.value.liberada }),
  ]
    .filter(Boolean)
    .join(' · '),
)

// --- Save: read over, then one save for the day; Undo brings the cards back
const confirming = ref(false)
function askSave() {
  if (blocker.value.text) return goTo(blocker.value.key)
  confirming.value = true
}
async function confirmSave() {
  confirming.value = false
  if (await save()) {
    openKey.value = null
    scroller.value?.scrollTo({ top: 0 })
  }
}
async function undoLast() {
  await undo()
  openKey.value = drafts.value.at(-1)?.key ?? null
}
const showHistory = ref(false)

// --- The keyboard: the box being typed in stays above it; Save waits under it
const footer = ref<HTMLElement>()
function keepInView() {
  const el = document.activeElement as HTMLElement | null
  if (!el || !el.matches('input, textarea') || !scroller.value?.contains(el)) return
  const r = el.getBoundingClientRect()
  const bottom = keyboard.visibleBottom.value - 16
  const top = (scroller.value.getBoundingClientRect().top ?? 0) + 56
  if (r.bottom > bottom) scroller.value.scrollTop += r.bottom - bottom
  else if (r.top < top) scroller.value.scrollTop -= top - r.top
}
let timer: ReturnType<typeof setTimeout> | undefined
function onFocusIn() {
  clearTimeout(timer)
  timer = setTimeout(keepInView, 350)
}
watch(keyboard.visibleBottom, () => requestAnimationFrame(keepInView))
onBeforeUnmount(() => clearTimeout(timer))
const today = todayIso()
</script>

<template>
  <div class="flex h-full flex-col bg-stone-50" @focusin="onFocusIn">
    <div ref="scroller" class="min-h-0 flex-1 overflow-y-auto" :style="keyboard.open.value ? { paddingBottom: `${keyboard.cover.value}px` } : undefined">
      <!-- The day in one line (a tap opens it), the view and the history: always at the top. -->
      <div class="sticky top-0 z-20 flex items-center gap-2 border-b border-stone-200 bg-white px-3 py-2">
        <button
          class="flex min-h-11 min-w-0 flex-1 items-center gap-2 rounded-lg border px-3 py-1.5 text-left text-sm"
          :class="dayReady ? 'border-stone-300 bg-stone-50' : 'border-amber-400 bg-amber-50 text-amber-900'"
          :aria-expanded="dayOpen"
          data-day
          @click="dayOpen = !dayOpen"
        >
          <span class="min-w-0 flex-1 truncate font-medium first-letter:uppercase">{{ dayLine }}</span>
          <component :is="dayOpen ? ChevronUp : ChevronDown" :size="18" class="shrink-0" />
        </button>
        <EntryModeToggle v-model="mode" :compact="!roomy" class="h-11 shrink-0 *:min-w-11" />
        <button
          class="flex h-11 min-w-11 shrink-0 items-center justify-center gap-1 rounded-md border border-stone-300 bg-white px-2 text-sm text-stone-700 active:bg-stone-100"
          :aria-label="$t('Historial de Colecta')"
          :title="$t('Historial de Colecta')"
          @click="showHistory = true"
        >
          <History :size="18" /> <span v-if="roomy">{{ $t('Historial') }}</span>
        </button>
      </div>

      <div class="mx-auto max-w-3xl">
        <DayPanel v-if="dayOpen" :state="state" @done="closeDay" />
        <InsectaryIdsWarning class="mx-3 mt-2" :revision="tables.tables.Insectary_data?.revision" @extended="loadFreeIds" />
        <p v-if="!ready" class="px-4 py-3 text-sm text-stone-500">{{ $t('Cargando {sheet}…', { sheet: 'Collection_data' }) }}</p>

        <!-- The butterflies of the day, in the order typed. -->
        <section v-if="drafts.length" class="px-3 pt-3">
          <div class="mb-1.5 flex items-center justify-between gap-2">
            <h2 class="text-sm font-semibold text-stone-700">{{ $tn(drafts.length, '{n} mariposa', '{n} mariposas') }}</h2>
            <button class="flex h-10 items-center gap-1 px-2 text-sm text-stone-600" @click="clearAll"><Trash2 :size="15" /> {{ $t('Vaciar lista') }}</button>
          </div>
          <ul class="space-y-2">
            <ButterflyCard
              v-for="(d, i) in drafts"
              :key="d.key"
              :draft="d"
              :number="i + 1"
              :state="state"
              :open="openKey === d.key"
              :quick-species="quickSpecies"
              @toggle="toggle(d)"
              @pick-species="picking = d"
              @duplicate="another(d)"
              @remove="removeCard(d, i + 1)"
            />
          </ul>
        </section>
        <div v-if="canEdit && !dayOpen" class="px-3 pt-3">
          <button class="flex h-13 w-full items-center justify-center gap-2 rounded-xl border-2 border-dashed border-brand-600 bg-white text-base font-semibold text-brand-800 active:bg-brand-50" data-add @click="addButterfly">
            <Plus :size="20" /> {{ $t('Mariposa') }}
          </button>
        </div>
        <p v-if="earlierIds.length" class="mx-3 mt-2 text-xs text-amber-800">
          {{
            $t(
              'Ya no quedan filas preasignadas al final de Insectary_data: {ids} son filas vacías anteriores. Comprueba que ningún ID esté ya escrito en otra mariposa, o crea más filas preasignadas en Insectary_data.',
              { ids: earlierIds.slice(0, 4).join(', ') + (earlierIds.length > 4 ? '…' : '') },
            )
          }}
        </p>

        <!-- Totals per species, as the notebook's tally. -->
        <section v-if="totals.length" class="px-3 pt-5 pb-6">
          <h2 class="mb-1.5 text-sm font-semibold text-stone-700">{{ $t('Por especie') }}</h2>
          <ul class="divide-y divide-stone-100 overflow-hidden rounded-xl border border-stone-200 bg-white text-sm">
            <li v-for="s in totals" :key="s.species" class="flex items-center gap-2 px-3 py-2">
              <span class="min-w-0 flex-1 truncate italic">{{ s.species }}</span>
              <span v-if="s.female" class="flex items-center gap-0.5 tabular-nums"><SexBadge sex="female" />{{ s.female }}</span>
              <span v-if="s.male" class="flex items-center gap-0.5 tabular-nums"><SexBadge sex="male" />{{ s.male }}</span>
              <span v-if="s.other" class="text-stone-500 tabular-nums">?{{ s.other }}</span>
              <span class="w-8 text-right font-semibold tabular-nums">{{ s.total }}</span>
            </li>
          </ul>
        </section>
        <p v-if="!drafts.length && !dayOpen && !lastSave" class="px-4 py-6 text-sm text-stone-500">
          {{ $t('Añade cada mariposa del día con «+ Mariposa»: especie, sexo y destino; se guardan todas juntas.') }}
        </p>
      </div>
    </div>

    <!-- Save (or the last save, with Undo): at the foot; under the keyboard while typing. -->
    <footer
      v-if="(canEdit && drafts.length) || lastSave"
      v-show="!keyboard.open.value"
      ref="footer"
      class="shrink-0 border-t border-stone-200 bg-white px-3 pt-2 pb-[calc(0.5rem+env(safe-area-inset-bottom))]"
    >
      <div class="mx-auto max-w-3xl">
        <div v-if="lastSave && !drafts.length" class="flex items-center gap-2" role="status">
          <Check :size="22" class="shrink-0 text-brand-700" />
          <p class="min-w-0 flex-1 text-sm font-medium">
            {{ $tn(lastSave.count, 'Colecta guardada: {n} mariposa', 'Colecta guardada: {n} mariposas') }}
          </p>
          <button class="btn h-12 px-4 text-base" :disabled="undoing" @click="undoLast">
            <Loader2 v-if="undoing" :size="18" class="animate-spin" /><Undo2 v-else :size="18" /> {{ $t('Deshacer') }}
          </button>
          <button class="btn h-12 px-3" :aria-label="$t('Cerrar')" @click="lastSave = null"><X :size="18" /></button>
        </div>
        <template v-else>
          <button
            v-if="blocker.text && blocker.key"
            class="mb-1 flex min-h-9 w-full items-center gap-1 text-left text-sm font-medium text-amber-900 underline decoration-amber-400 underline-offset-2"
            @click="goTo(blocker.key)"
          >
            <AlertTriangle :size="16" class="shrink-0" /><span class="min-w-0 truncate">{{ blocker.text }}</span>
          </button>
          <p v-else class="mb-1 truncate text-xs" :class="blocker.text ? 'text-amber-900' : 'text-stone-600'">
            {{ blocker.text || summaryLine }}<template v-if="!blocker.text && header.date === today"> · {{ $t('fecha: hoy') }}</template>
          </p>
          <div class="flex gap-2">
            <button class="btn h-13 shrink-0 px-4 text-base" :aria-label="$t('Añadir mariposa')" @click="addButterfly"><Plus :size="20" /></button>
            <button class="btn-primary h-13 flex-1 text-base" :disabled="saving || !!blocker.text" @click="askSave">
              <Loader2 v-if="saving" :size="18" class="animate-spin" />
              {{ saving ? $t('Guardando…') : $tn(drafts.length, 'Guardar {n} mariposa', 'Guardar {n} mariposas') }}
            </button>
          </div>
        </template>
      </div>
    </footer>

    <SpeciesPicker
      v-if="picking"
      :entries="entries"
      :current="[picking.species, picking.subspecies].filter(Boolean).join(' ')"
      :known="!!options.SPECIES?.length"
      @pick="pickSpecies"
      @close="picking = null"
    />
    <SaveSummary v-if="confirming" :drafts="drafts" :date="header.date" :saving="saving" @close="confirming = false" @confirm="confirmSave" />
    <TabHistory v-if="showHistory" :title="$t('Historial de Colecta')" purpose="colecta" @close="showHistory = false" />
  </div>
</template>
