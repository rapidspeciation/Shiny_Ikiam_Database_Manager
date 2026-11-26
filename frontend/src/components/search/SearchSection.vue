<script setup lang="ts">
import { computed, markRaw, onBeforeUnmount, onMounted, ref, shallowRef, watch } from 'vue'
import { ChevronDown, ChevronRight, ChevronUp, Maximize2 } from 'lucide-vue-next'
import SheetGrid from '../SheetGrid.vue'
import RowDrawer from '../RowDrawer.vue'
import RowList from './RowList.vue'
import { buildOptions } from '../../lib/options'
import { notify, errorText } from '../../lib/notice'
import { type Loaded, STEP, matchRanges, mergeRows, rangeFor, sheetRows, stepMatch, type SearchSheet } from '../../lib/search'
import { rowListItem } from '../../lib/rowList'
import type { Field, Table, TableRow } from '../../lib/types'
import type { HistoryTarget } from '../../lib/history'
import { rowFromWire, toRow, useTables } from '../../stores/tables'
import { useSession } from '../../stores/session'

/**
 * One sheet in the Buscador's results: its rows around a match, as an editable
 * grid. Scrolling near the top or bottom reads more rows of the sheet, so the
 * neighbours of a match (and their mistakes) can be followed; ↑ ↓ go to the
 * other matches, as Ctrl+F does.
 */
const props = defineProps<{ result: SearchSheet; query: string; pinned?: boolean; /** A phone: the matches as a list of lines (a tap opens the row). */ list?: boolean }>()
const emit = defineEmits<{ open: [module: string, row: number]; history: [target: HistoryTarget]; form: [open: boolean] }>()

const session = useSession()
const tables = useTables()
const module = props.result.module
const mod = computed(() => session.module(module))
/** Every column in sheet order (as the server sends values), and each once for the grid. */
const keys = computed(() => mod.value?.fields.map(f => f.key) || [])
const columns = computed<Field[]>(() => {
  const missing = new Set(props.result.unavailable)
  return (mod.value?.fields || [])
    .filter((f, i, all) => all.findIndex(g => g.key === f.key) === i)
    .map(f => (missing.has(f.key) ? { ...f, readonly: true, unavailable: true } : f))
})
const frozen = computed(() => mod.value?.identityFields.slice(0, 1) || [])
/**
 * Dropdown choices from the Lists sheet (and from the whole sheet when the page
 * has it already): not from the rows shown, which change while scrolling.
 */
const options = computed(() => {
  void tables.versions[module]
  void tables.versions.Lists
  const full = tables.tables[module]
  const empty = { module, revision: '', columns: columns.value, rows: [], headerProblems: [] } as Table
  return buildOptions(full ?? empty, tables.tables.Lists)
})

const rows = shallowRef<TableRow[]>([])
let loaded: Loaded | null = null
const current = ref(props.result.focus)
const open = ref(false)
const busy = ref(false)
const grid = ref<InstanceType<typeof SheetGrid>>()
const matchRows = computed(() => new Set(props.result.matches))

function take(wire: { rows: Parameters<typeof rowFromWire>[1][] }) {
  return wire.rows.map(r => markRaw(rowFromWire(keys.value, r)))
}

if (props.result.window) {
  rows.value = take(props.result.window)
  loaded = { from: props.result.window.from, to: props.result.window.to }
  open.value = true
}

/** Reads rows `from`..`to` and adds them to those shown (or shows them instead). */
async function read(from: number, to: number, { replace = false, above = false } = {}) {
  busy.value = true
  try {
    const reply = await sheetRows(module, from, to)
    const more = take(reply)
    grid.value?.keepView(above)
    rows.value = replace ? more : mergeRows(rows.value, more)
    loaded = replace || !loaded ? { from, to: reply.to } : { from: Math.min(loaded.from, from), to: Math.max(loaded.to, reply.to) }
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    busy.value = false
  }
}

/** Shows row `row` (a match), reading the rows around it when they are not here yet. */
async function show(row: number) {
  current.value = row
  open.value = true
  const need = rangeFor(loaded, row)
  if (need) await read(need.from, need.to, { replace: need.replace })
}

function onEdge(side: 'top' | 'bottom') {
  if (busy.value || !loaded) return
  const { first, last } = props.result
  if (side === 'top' && (first === null || loaded.from > first))
    read(Math.max(0, loaded.from - STEP), loaded.from - 1, { above: true })
  if (side === 'bottom' && (last === null || loaded.to < last)) read(loaded.to + 1, loaded.to + STEP)
}

// ---------------------------------------------------------------- a phone's list of the matches
/** The matches read so far (those among the rows shown, and those read for the list). */
const listRows = shallowRef<TableRow[]>([])
const listBusy = ref(false)
const keepMatches = (more: TableRow[]) => {
  const found = more.filter(r => matchRows.value.has(r.row))
  if (found.length) listRows.value = mergeRows(listRows.value, found)
}
watch(rows, keepMatches, { immediate: true })
/** The match the sheet opens at first, then the newest. */
const listItems = computed(() => {
  const identity = mod.value?.identityFields ?? []
  return [...listRows.value]
    .sort((a, b) => Number(b.row === props.result.focus) - Number(a.row === props.result.focus) || b.row - a.row)
    .map(r => rowListItem(r, columns.value, identity, props.query, props.result.columns))
})
const listLeft = computed(() => Math.max(0, props.result.matches.length - listRows.value.length))
/** Reads the next matches (the newest not read yet), a few ranges of rows at a time. */
async function moreMatches() {
  if (listBusy.value || !listLeft.value) return
  listBusy.value = true
  try {
    const have = new Set(listRows.value.map(r => r.row))
    for (const range of matchRanges(props.result.matches, have)) keepMatches(take(await sheetRows(module, range.from, range.to)))
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    listBusy.value = false
  }
}
/** The row whose form is open over the list. */
const formId = ref<string | null>(null)
watch(
  () => !!formId.value,
  shown => emit('form', shown),
)
// Back to the grid: its own row form reports itself.
watch(
  () => props.list,
  list => !list && (formId.value = null),
)

/** Where the text is: the first columns by number of matches. */
const shownColumns = computed(() => {
  const list = props.result.columns
  return list.slice(0, 4).join(', ') + (list.length > 4 ? ` +${list.length - 4}` : '')
})
const position = computed(() => props.result.matches.indexOf(current.value) + 1)
const focusId = computed(() => rows.value.find(r => r.row === current.value)?.id ?? null)
const highlight = computed(() => rows.value.filter(r => matchRows.value.has(r.row)).map(r => r.id))

function toggle() {
  if (open.value) open.value = false
  else if (rows.value.length) open.value = true
  else show(current.value)
}
// The sheet picked above the results comes first, open.
watch(
  () => props.pinned,
  pinned => pinned && !open.value && toggle(),
  { immediate: true },
)

/**
 * A grid takes a moment to build: those of sections further down the page are
 * built when the page is scrolled near them.
 */
const GRID_HEIGHT = 'min(60vh, 23rem)'
const root = ref<HTMLElement>()
const near = ref(typeof IntersectionObserver === 'undefined')
let observer: IntersectionObserver | null = null
onMounted(() => {
  if (near.value || !root.value) return
  observer = new IntersectionObserver(
    entries => {
      if (!entries.some(e => e.isIntersecting)) return
      near.value = true
      observer?.disconnect()
    },
    { rootMargin: '300px 0px' },
  )
  observer.observe(root.value)
})
onBeforeUnmount(() => observer?.disconnect())
// The list starts with a screenful of matches, not only those beside the first one.
watch(
  () => props.list && open.value && near.value && rows.value.length > 0,
  ready => ready && listRows.value.length < Math.min(props.result.matches.length, 12) && void moreMatches(),
  { immediate: true },
)

// Saved rows (by anyone on this page) replace the ones shown.
const stop = tables.$onAction(({ name, args, after }) => {
  if (name !== 'merge') return
  after(() => {
    const saved = new Map(
      (args[0] as Parameters<typeof tables.merge>[0]).filter(r => r.sheet === module).map(r => [r.id, r]),
    )
    if (saved.size && rows.value.some(r => saved.has(r.id)))
      rows.value = rows.value.map(r => (saved.has(r.id) ? markRaw(toRow(saved.get(r.id)!)) : r))
    if (saved.size && listRows.value.some(r => saved.has(r.id)))
      listRows.value = listRows.value.map(r => (saved.has(r.id) ? markRaw(toRow(saved.get(r.id)!)) : r))
  })
})
onBeforeUnmount(stop)
</script>

<template>
  <section ref="root" class="rounded-lg border border-stone-200 bg-surface">
    <!-- On a phone: the name and the ↑ ↓ buttons on one line, what matched under them. -->
    <header class="flex flex-wrap items-center gap-x-3 gap-y-1 px-3 py-2 max-sm:gap-x-2 max-sm:py-1.5">
      <button
        type="button"
        class="hit-target flex min-w-0 items-center gap-1.5 text-left font-semibold text-stone-800 max-sm:min-h-11 max-sm:flex-1"
        :aria-expanded="open"
        @click="toggle"
      >
        <component :is="open ? ChevronDown : ChevronRight" :size="16" class="flex-none text-stone-500" />
        <span class="truncate">{{ module }}</span>
      </button>
      <span class="text-sm text-stone-600 max-sm:order-last max-sm:w-full max-sm:text-xs">
        {{ $tn(result.total, '{n} coincidencia', '{n} coincidencias') }}
        <template v-if="result.idExact"> · {{ $tn(result.idExact, '{n} ID exacto', '{n} IDs exactos') }}</template>
        <template v-else-if="result.exact"> · {{ $tn(result.exact, '{n} celda exacta', '{n} celdas exactas') }}</template>
        <span class="text-stone-500">
          · {{ $t('en {columns}', { columns: shownColumns }) }}
        </span>
      </span>
      <span v-if="!list" class="ml-auto flex items-center gap-1">
        <button
          type="button"
          class="btn px-2"
          :disabled="busy || result.matches.length < 2"
          :title="$t('Coincidencia anterior')"
          @click="show(stepMatch(result.matches, current, -1))"
        >
          <ChevronUp :size="15" />
        </button>
        <span class="min-w-24 text-center text-sm text-stone-600 tabular-nums max-sm:min-w-12">
          {{
            result.truncated
              ? $t('{at} de {shown} ({total} en total)', { at: position || '–', shown: result.matches.length, total: result.total })
              : $t('{at} de {shown}', { at: position || '–', shown: result.matches.length })
          }}
          <span class="max-sm:hidden">· {{ $t('fila {row}', { row: current }) }}</span>
        </span>
        <button
          type="button"
          class="btn px-2"
          :disabled="busy || result.matches.length < 2"
          :title="$t('Coincidencia siguiente')"
          @click="show(stepMatch(result.matches, current, 1))"
        >
          <ChevronDown :size="15" />
        </button>
        <button type="button" class="btn" :title="$t('Ver esta fila en la hoja completa')" @click="emit('open', module, current)">
          <Maximize2 :size="15" /> <span class="max-sm:hidden">{{ $t('Hoja completa') }}</span>
        </button>
      </span>
    </header>
    <div v-if="open" class="border-t border-stone-200">
      <p v-if="!rows.length" class="p-4 text-sm text-stone-500">{{ $t('Cargando {sheet}…', { sheet: module }) }}</p>
      <div v-else-if="!near" :style="{ height: GRID_HEIGHT }" />
      <!-- A phone: the matches as lines; a tap opens the row's form over them. -->
      <div v-else-if="list" class="bg-canvas" data-match-list>
        <RowList
          :items="listItems"
          :mark="query"
          :current="formId"
          :left="listLeft"
          :busy="listBusy"
          :more-label="listBusy ? $t('Cargando…') : $tn(listLeft, 'Ver {n} coincidencia más', 'Ver {n} coincidencias más')"
          @open="id => (formId = id)"
          @more="moreMatches"
        />
        <RowDrawer
          v-if="formId"
          :module="module"
          :row-id="formId"
          :rows="listRows"
          :creates="[]"
          :columns="columns"
          :options="options"
          :locked-fields="[]"
          :create-formulas="[]"
          @close="formId = null"
        />
      </div>
      <SheetGrid
        v-else
        ref="grid"
        :module="module"
        :rows="rows"
        :columns="columns"
        :options="options"
        :frozen="frozen"
        :header-filters="false"
        :newest-first="false"
        :highlight="highlight"
        :mark="query"
        :focus-row="focusId"
        :height="GRID_HEIGHT"
        cell-history
        @notice="notify"
        @edge="onEdge"
        @history="target => emit('history', target)"
        @form="open => emit('form', open)"
      />
    </div>
  </section>
</template>
