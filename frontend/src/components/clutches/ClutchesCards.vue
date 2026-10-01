<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { Check, Plus, Search, X } from 'lucide-vue-next'
import ClutchEditor from './ClutchEditor.vue'
import NewClutch from './NewClutch.vue'
import TodayChanges from './TodayChanges.vue'
import EntryModeToggle from '../EntryModeToggle.vue'
import RowDrawer from '../RowDrawer.vue'
import type { EntryMode } from '../../composables/useEntryMode'
import { useClutchDay } from '../../composables/useClutchDay'
import { usePhoneWidth } from '../../composables/usePhone'
import { isBlank } from '../../lib/cells'
import {
  MODULE,
  STAGES,
  clutchState,
  countCell,
  hasClutch,
  parentsOf,
  readCount,
  termsText,
  totalOf,
  undatedTail,
  type ClutchState,
  type CountField,
} from '../../lib/clutches'
import { formatSerial, isoToSerial, todayIso } from '../../lib/dates'
import { persistentRef } from '../../lib/persist'
import { initialsOf } from '../../lib/rows'
import type { Table, TableRow } from '../../lib/types'
import { usePending } from '../../stores/pending'
import { useSession } from '../../stores/session'
import { t } from '../../lib/i18n'

/**
 * Clutches on a phone or tablet, for the daily round: the clutches still going
 * as cards (counts with their sums, stage, checked or changed today), any clutch
 * by number, species or parent; a clutch opens in its editor (full screen on a
 * phone, beside the list on a tablet); today's changes to copy into the paper
 * notebook; and a new clutch. Every change is a pending edit saved like the
 * table's, so history, undo and checks are the same.
 */
const props = defineProps<{
  table: Table | undefined
  ready: boolean
  options: Record<string, string[]>
  species: string[]
  collectors: string[]
  createFormulas: string[]
}>()
const mode = defineModel<EntryMode>('mode', { required: true })

const pending = usePending()
const session = useSession()
const day = useClutchDay()
const canEdit = computed(() => session.canEdit)
const initials = computed(() =>
  initialsOf(session.user?.displayName || '', props.collectors, session.user?.username || ''),
)
const today = computed(() => isoToSerial(todayIso()))
const phone = usePhoneWidth()

/** List and editor side by side: a tablet or a computer (not a phone on its side). */
const WIDE = '(min-width: 768px) and (min-height: 560px)'
const wideQuery = typeof window !== 'undefined' && window.matchMedia ? window.matchMedia(WIDE) : null
const wide = ref(!!wideQuery?.matches)
const followWide = (e: MediaQueryListEvent) => (wide.value = e.matches)
onMounted(() => wideQuery?.addEventListener('change', followWide))
onBeforeUnmount(() => wideQuery?.removeEventListener('change', followWide))
/** Tall enough to keep the search and filters on screen while the cards scroll (not a phone on its side). */
const TALL = '(min-height: 600px)'
const tallQuery = typeof window !== 'undefined' && window.matchMedia ? window.matchMedia(TALL) : null
const tall = ref(!!tallQuery?.matches)
const followTall = (e: MediaQueryListEvent) => (tall.value = e.matches)
onMounted(() => tallQuery?.addEventListener('change', followTall))
onBeforeUnmount(() => tallQuery?.removeEventListener('change', followTall))

const view = persistentRef<'ongoing' | 'today'>('clutches:view', 'ongoing')
const filter = persistentRef<'all' | 'todo' | 'changed'>('clutches:filter', 'all')
const query = ref('')

// --- Every clutch with what a card shows, read as the person sees it (pending edits included)
interface Item {
  row: TableRow
  number: string
  species: string
  counts: Record<CountField, { terms: number[]; na: boolean; text: string | null }>
  state: ClutchState
  search: string
}
const rows = computed(() => (props.table?.rows || []).filter(r => r.observed))
const value = (row: TableRow, field: string) => pending.value(row, field)
function countsOf(row: TableRow) {
  const formulas = day.sums.value[row.id]
  const out = {} as Item['counts']
  for (const f of [...STAGES.map(s => s.count), 'NUMBER OF PUPAE/LARVAE FOR DISECTIONS'] as CountField[])
    out[f] = readCount(countCell(row, f, pending.value, pending.isDirty(row.id, f), formulas))
  return out
}
const items = computed<Item[]>(() => {
  void pending.edited
  const all = rows.value
  const out: Item[] = []
  all.forEach((row, i) => {
    const get = (f: string) => value(row, f)
    if (!hasClutch(get)) return
    const counts = countsOf(row)
    const state = clutchState(get, f => counts[f], today.value, { undated: undatedTail(i, all.length) })
    const number = String(row.values['CLUTCH NUMBER'] ?? '')
    const species = isBlank(get('SPECIES')) ? '' : String(get('SPECIES'))
    const parents = parentsOf(get('NOTES'))
    out.push({ row, number, species, counts, state, search: `${number} ${species} ${parents ? `${parents.female} ${parents.male}` : ''}`.toLowerCase() })
  })
  return out
})
const stateOf = (row: TableRow) => {
  const counts = countsOf(row)
  const all = rows.value
  return clutchState(f => value(row, f), f => counts[f], today.value, { undated: undatedTail(all.indexOf(row), all.length) })
}
const ongoing = computed(() => items.value.filter(i => !i.state.ended))
/** What is listed: the ongoing clutches (filtered), or every clutch matching the search, newest first. */
const listed = computed(() => {
  const q = query.value.trim().toLowerCase()
  if (q) {
    const starts = items.value.filter(i => i.number.toLowerCase().startsWith(q))
    const contains = items.value.filter(i => !i.number.toLowerCase().startsWith(q) && i.search.includes(q))
    return [...starts.reverse(), ...contains.reverse()].slice(0, 60)
  }
  if (filter.value === 'todo') return ongoing.value.filter(i => !day.today(i.row.id).checked)
  if (filter.value === 'changed') return ongoing.value.filter(i => day.today(i.row.id).changed)
  return ongoing.value
})
const counts = computed(() => ({
  all: ongoing.value.length,
  todo: ongoing.value.filter(i => !day.today(i.row.id).checked).length,
  changed: ongoing.value.filter(i => day.today(i.row.id).changed).length,
}))
const changedToday = computed(() => new Set(day.day.value.changes.map(c => c.recordId)).size)

// --- The editor: over the list on a phone, beside it on a tablet
const editing = ref<{ ids: string[]; index: number } | null>(null)
const rowById = computed(() => new Map((props.table?.rows || []).map(r => [r.id, r])))
const editorRows = computed(() => (editing.value ? editing.value.ids.map(id => rowById.value.get(id)).filter((r): r is TableRow => !!r) : []))
const selectedId = computed(() => (editing.value ? editorRows.value[editing.value.index]?.id : null))
function open(at: number) {
  creating.value = false
  editing.value = { ids: listed.value.map(i => i.row.id), index: at }
}
function openRecord(id: string) {
  const at = listed.value.findIndex(i => i.row.id === id)
  if (at >= 0) return open(at)
  creating.value = false
  editing.value = { ids: [id], index: 0 }
}
const drawerRow = ref<TableRow | null>(null)

// --- Mark as checked from a card
const marking = ref<string | null>(null)
const lastMark = ref<{ id: string; clutch: string } | null>(null)
async function markChecked(row: TableRow) {
  marking.value = row.id
  try {
    const check = await day.markChecked(row.id, [])
    lastMark.value = { id: check.id, clutch: String(row.values['CLUTCH NUMBER'] ?? '') }
    setTimeout(() => {
      if (lastMark.value?.id === check.id) lastMark.value = null
    }, 6000)
  } finally {
    marking.value = null
  }
}
async function undoMark() {
  const m = lastMark.value
  if (!m) return
  lastMark.value = null
  await day.unmark(m.id)
}

// --- A new clutch
const creating = ref(false)
const numbers = computed(() => [
  ...rows.value.map(r => String(r.values['CLUTCH NUMBER'] ?? '')),
  ...pending.creates.filter(c => c.module === MODULE).map(c => String(c.values['CLUTCH NUMBER'] ?? '')),
])
function startNew() {
  editing.value = null
  creating.value = true
}
async function created(clutch: string) {
  creating.value = false
  view.value = 'ongoing'
  query.value = ''
  await nextTick()
  const row = rows.value.find(r => String(r.values['CLUTCH NUMBER'] ?? '') === clutch)
  if (row) {
    try {
      await day.markChecked(row.id, ['CLUTCH NUMBER'])
    } catch {
      /* the clutch is saved; the check can be marked from its card */
    }
    openRecord(row.id)
  }
}

// --- What a card shows
const SHORT: Record<string, () => string> = {
  'NUMBER OF EGGS': () => t('Huevos'),
  'NUMBER OF LARVAE': () => t('Larvas'),
  'NUMBER OF PUPA': () => t('Pupas'),
  'NUMBER OF ADULTS': () => t('Adultos'),
}
const stageName: Record<string, () => string> = {
  egg: () => t('Huevos'),
  larva: () => t('Larvas'),
  pupa: () => t('Pupas'),
  adult: () => t('Emergiendo'),
}
const laidText = (row: TableRow) => {
  const laid = value(row, 'DATE LAID')
  if (typeof laid !== 'number') return ''
  const ago = today.value - laid
  return `${formatSerial(laid)} · ${ago === 0 ? t('hoy') : ago === 1 ? t('ayer') : t('hace {n} días', { n: ago })}`
}
const initialsFor = (name: string) => (name === 'Google Sheets' ? 'Sheets' : initialsOf(name, props.collectors))
const lastText = (row: TableRow) => {
  const last = day.last.value[row.id]
  if (!last) return ''
  const days = today.value - isoToSerial(new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' }).format(new Date(last.at)))
  const when = days <= 0 ? t('hoy') : days === 1 ? t('ayer') : t('hace {n} días', { n: days })
  return t('Último cambio {when} · {who}', { when, who: last.actor === 'unknown' ? 'Google Sheets' : initialsFor(last.name || '') })
}
const unsavedRow = (row: TableRow) => !!pending.edits[row.id]
const listEl = ref<HTMLElement>()
const rootEl = ref<HTMLElement>()
watch(view, () => (wide.value ? listEl.value : rootEl.value)?.scrollTo({ top: 0 }))
</script>

<template>
  <!-- On a phone the controls scroll away with the cards (held at the top only when the screen is tall). -->
  <div ref="rootEl" class="h-full bg-stone-50" :class="wide ? 'flex flex-col' : 'overflow-y-auto'">
    <!-- What is shown, the search and the filters. -->
    <div class="shrink-0 border-b border-stone-200 bg-white px-3 pt-2 pb-2" :class="{ 'sticky top-0 z-20': !wide && tall }">
      <div class="flex items-center gap-2">
        <div class="inline-flex overflow-hidden rounded-lg border border-stone-300 text-sm" role="tablist">
          <button
            role="tab"
            class="h-11 px-3 font-medium whitespace-nowrap"
            :class="view === 'ongoing' ? 'bg-brand-700 text-white' : 'bg-white text-stone-700'"
            :aria-selected="view === 'ongoing'"
            @click="view = 'ongoing'"
          >
            {{ $t('En curso') }} <span class="tabular-nums opacity-80">{{ counts.all }}</span>
          </button>
          <button
            role="tab"
            class="h-11 border-l border-stone-300 px-3 font-medium whitespace-nowrap"
            :class="view === 'today' ? 'bg-brand-700 text-white' : 'bg-white text-stone-700'"
            :aria-selected="view === 'today'"
            @click="view = 'today'"
          >
            {{ $t('Hoy') }} <span class="tabular-nums opacity-80">{{ changedToday }}</span>
          </button>
        </div>
        <button v-if="canEdit" class="btn h-11 shrink-0 px-3" :aria-label="$t('Nuevo clutch')" @click="startNew">
          <Plus :size="18" /> <span class="hidden min-[420px]:inline">{{ $t('Nuevo') }}</span>
        </button>
        <EntryModeToggle v-model="mode" class="ml-auto shrink-0" :compact="phone" />
      </div>
      <template v-if="view === 'ongoing'">
        <div class="relative mt-2">
          <Search :size="18" class="pointer-events-none absolute top-1/2 left-3 -translate-y-1/2 text-stone-400" />
          <input
            v-model="query"
            class="h-12 w-full rounded-xl border border-stone-300 bg-white pr-11 pl-9 text-base placeholder:text-stone-400 focus:border-brand-600 focus:ring-2 focus:ring-brand-100 focus:outline-none"
            type="search"
            inputmode="search"
            autocomplete="off"
            enterkeyhint="search"
            :placeholder="$t('Buscar clutch: número, especie o padre')"
            :aria-label="$t('Buscar clutch: número, especie o padre')"
          />
          <button v-if="query" class="absolute top-1/2 right-0.5 grid h-11 w-11 -translate-y-1/2 place-items-center text-stone-500" :aria-label="$t('Borrar búsqueda')" @click="query = ''">
            <X :size="18" />
          </button>
        </div>
        <div v-if="!query" class="mt-2 flex gap-1.5 overflow-x-auto text-sm">
          <button
            v-for="f in (['all', 'todo', 'changed'] as const)"
            :key="f"
            class="h-9 shrink-0 rounded-full border px-3"
            :class="filter === f ? 'border-brand-700 bg-brand-50 font-medium text-brand-800' : 'border-stone-300 bg-white text-stone-700'"
            :aria-pressed="filter === f"
            @click="filter = f"
          >
            {{ f === 'all' ? $t('Todos') : f === 'todo' ? $t('Sin revisar hoy') : $t('Cambiados hoy') }}
            <span class="tabular-nums opacity-70">{{ counts[f] }}</span>
          </button>
        </div>
      </template>
    </div>

    <div :class="wide ? 'flex min-h-0 flex-1' : ''">
      <div ref="listEl" :class="wide ? 'min-h-0 w-[360px] shrink-0 overflow-y-auto border-r border-stone-200 lg:w-[400px]' : ''">
        <p v-if="!ready" class="p-6 text-stone-500">{{ $t('Cargando {sheet}…', { sheet: MODULE }) }}</p>
        <TodayChanges v-else-if="view === 'today'" :day="day" :initials="initials" @open="openRecord" />
        <template v-else>
          <p v-if="query && !listed.length" class="p-6 text-center text-sm text-stone-500">{{ $t('Ningún clutch con «{q}»', { q: query }) }}</p>
          <p v-else-if="!listed.length" class="p-6 text-center text-sm text-stone-500">
            {{ filter === 'todo' ? $t('Todos los clutches en curso están revisados hoy.') : $t('Ningún clutch aquí.') }}
          </p>
          <ul class="space-y-2 p-3">
            <li
              v-for="(item, i) in listed"
              :key="item.row.id"
              class="overflow-hidden rounded-xl border bg-white shadow-sm"
              :class="selectedId === item.row.id && wide ? 'border-brand-600 ring-2 ring-brand-100' : 'border-stone-200'"
            >
              <button class="block w-full px-3 pt-2 pb-1.5 text-left active:bg-stone-50" @click="open(i)">
                <span class="flex flex-wrap items-center gap-1.5">
                  <span class="text-xl font-semibold tabular-nums">{{ item.number }}</span>
                  <span v-if="!isBlank(value(item.row, 'Generation')) && value(item.row, 'Generation') !== 'NA'" class="rounded bg-violet-100 px-1.5 text-xs font-medium text-violet-800">
                    {{ value(item.row, 'Generation') }}
                  </span>
                  <span v-if="item.state.stage" class="rounded-full bg-stone-100 px-2 py-0.5 text-xs text-stone-700">{{ stageName[item.state.stage]() }}</span>
                  <span v-if="item.state.ended" class="rounded-full bg-stone-200 px-2 py-0.5 text-xs text-stone-600">{{ $t('Terminado') }}</span>
                  <span class="ml-auto flex flex-wrap justify-end gap-1">
                    <span v-if="unsavedRow(item.row)" class="rounded-full bg-amber-50 px-2 py-0.5 text-xs font-medium text-amber-900 ring-1 ring-amber-300">{{ $t('Sin guardar') }}</span>
                    <span v-if="day.today(item.row.id).changed" class="rounded-full bg-amber-100 px-2 py-0.5 text-xs font-medium text-amber-900">{{ $t('Cambiado hoy') }}</span>
                    <span v-if="day.today(item.row.id).checked" class="flex items-center gap-0.5 rounded-full bg-brand-50 px-2 py-0.5 text-xs font-medium text-brand-800">
                      <Check :size="12" /> {{ day.today(item.row.id).who.map(initialsFor).join(', ') || $t('Revisado') }}
                    </span>
                  </span>
                </span>
                <span class="mt-0.5 block truncate text-sm">{{ item.species || $t('sin especie') }}</span>
                <span class="block truncate text-xs text-stone-500">
                  {{ [laidText(item.row) && $t('Puesta {date}', { date: laidText(item.row) }), value(item.row, 'INSECTARY OR LABORATORY') === 'Laboratory' ? 'Laboratory' : ''].filter(Boolean).join(' · ') }}
                </span>
                <!-- The four counts: total big, the sum's history small. -->
                <span class="mt-1.5 grid grid-cols-4 gap-1">
                  <span v-for="s in STAGES" :key="s.count" class="min-w-0 rounded-md bg-stone-50 px-1.5 py-1" :class="{ 'bg-amber-50': pending.isDirty(item.row.id, s.count) }">
                    <span class="block text-[11px] leading-tight text-stone-500">{{ SHORT[s.count]() }}</span>
                    <span class="block text-lg leading-tight font-semibold tabular-nums">
                      {{ item.counts[s.count].na ? 'NA' : item.counts[s.count].text ? '?' : item.counts[s.count].terms.length ? totalOf(item.counts[s.count].terms) : '—' }}
                    </span>
                    <span v-if="item.counts[s.count].terms.length > 1" class="block truncate text-[11px] leading-tight text-stone-500 tabular-nums">{{ termsText(item.counts[s.count].terms) }}</span>
                  </span>
                </span>
                <span v-if="lastText(item.row)" class="mt-1 block truncate text-[11px] text-stone-500">{{ lastText(item.row) }}</span>
              </button>
              <div v-if="canEdit && !day.today(item.row.id).checked" class="border-t border-stone-100">
                <button class="flex h-11 w-full items-center justify-center gap-1.5 text-sm font-medium text-brand-800 active:bg-brand-50" :disabled="marking === item.row.id" @click="markChecked(item.row)">
                  <Check :size="16" /> {{ $t('Revisado, sin cambios') }}
                </button>
              </div>
            </li>
          </ul>
        </template>
      </div>
      <template v-if="wide">
        <div class="min-w-0 flex-1">
          <NewClutch
            v-if="creating"
            docked
            :rows="rows"
            :numbers="numbers"
            :species="species"
            :create-formulas="createFormulas"
            :initials="initials"
            :has-generation="!!table?.columns.some(c => c.key === 'Generation')"
            @close="creating = false"
            @created="created"
          />
          <ClutchEditor
            v-else-if="editing && editorRows.length"
            v-model:index="editing.index"
            docked
            :rows="editorRows"
            :columns="table?.columns || []"
            :options="options"
            :species="species"
            :day="day"
            :state="stateOf"
            :can-edit="canEdit"
            :initials="initials"
            @close="editing = null"
            @more="drawerRow = $event"
          />
          <div v-else class="grid h-full place-items-center p-8 text-center text-sm text-stone-500">
            {{ $t('Elige un clutch de la lista para actualizarlo.') }}
          </div>
        </div>
      </template>
    </div>

    <!-- A check marked from a card can be taken back for a moment. -->
    <div v-if="lastMark" class="fixed inset-x-3 bottom-20 z-30 mx-auto flex max-w-md items-center gap-2 rounded-xl bg-stone-800 px-4 py-2 text-sm text-white shadow-lg" role="status">
      <Check :size="16" /> <span class="flex-1">{{ $t('Clutch {clutch} revisado, sin cambios', { clutch: lastMark.clutch }) }}</span>
      <button class="h-11 px-2 font-semibold underline" @click="undoMark">{{ $t('Deshacer') }}</button>
    </div>

    <template v-if="!wide">
      <NewClutch
        v-if="creating"
        :rows="rows"
        :numbers="numbers"
        :species="species"
        :create-formulas="createFormulas"
        :initials="initials"
        :has-generation="!!table?.columns.some(c => c.key === 'Generation')"
        @close="creating = false"
        @created="created"
      />
      <ClutchEditor
        v-else-if="editing && editorRows.length"
        v-model:index="editing.index"
        :rows="editorRows"
        :columns="table?.columns || []"
        :options="options"
        :species="species"
        :day="day"
        :state="stateOf"
        :can-edit="canEdit"
        :initials="initials"
        @close="editing = null"
        @more="drawerRow = $event"
      />
    </template>
    <RowDrawer
      v-if="drawerRow && table"
      :module="MODULE"
      :row-id="drawerRow.id"
      :rows="table.rows"
      :creates="[]"
      :columns="table.columns"
      :options="options"
      :locked-fields="[]"
      :create-formulas="[]"
      label-field="CLUTCH NUMBER"
      @close="drawerRow = null"
      @changed="pending.touch()"
    />
  </div>
</template>
