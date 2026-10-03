<script setup lang="ts">
import ChoiceField from '../components/ChoiceField.vue'
import { computed, onBeforeUnmount, ref, watch } from 'vue'
import { RouterLink, useRoute, useRouter } from 'vue-router'
import {
  Plus,
  RefreshCw,
  ArrowDownToLine,
  ArrowLeft,
  EllipsisVertical,
  ExternalLink,
  Search,
  FileDown,
  Info,
  ShieldAlert,
  X,
} from 'lucide-vue-next'
import SheetGrid from '../components/SheetGrid.vue'
import SearchResults from '../components/search/SearchResults.vue'
import CellHistory from '../components/search/CellHistory.vue'
import SheetAsOf from '../components/search/SheetAsOf.vue'
import WorkbookWarnings from '../components/WorkbookWarnings.vue'
import ExtendRowsButton from '../components/ExtendRowsButton.vue'
import type { HeaderProblem } from '../lib/types'
import type { AsOfSide, HistoryTarget } from '../lib/history'
import { useSheet } from '../composables/useSheet'
import { usePhoneWidth } from '../composables/usePhone'
import { notify } from '../lib/notice'
import { usePending } from '../stores/pending'
import { useSession } from '../stores/session'
import { t } from '../lib/i18n'

/**
 * The "Buscador": a text searched in every sheet, as Google Sheets' Ctrl+F,
 * each sheet's match shown among its neighbouring rows (SearchResults); with
 * no text, any sheet whole as an editable spreadsheet, as in the original app.
 * A cell's history opens beside the grid (CellHistory), and from it the sheet
 * as it was before or after one of its edits (SheetAsOf).
 * Links: ?buscar=<text> searches (?hoja=<sheet> puts that sheet first);
 * ?hoja=<sheet>&fila=<row> opens the sheet at that row; with &antes=<save>
 * (or &despues=) and &campo=<column>, as it was before (after) that save.
 * On a phone the tools fold into a ⋯ menu, so the grid gets the screen.
 */
const session = useSession()
const pending = usePending()
const route = useRoute()
const router = useRouter()

/** Group names in Spanish (the key of their English), translated where shown. */
const GROUPS: Record<string, string> = {
  insectary: 'Insectario',
  field: 'Campo',
  breeding: 'Cruces',
  research: 'Experimentos',
  samples: 'Muestras',
  media: 'Fotos',
  reference: 'Referencia',
}
const touch = window.matchMedia('(pointer: coarse)').matches
const module = ref(String(route.query.hoja || 'Insectary_data'))
const search = ref(String(route.query.buscar || ''))
const debounced = ref(search.value)
/** The row opened from a search result or a link (?fila=), brought into view and marked. */
const rowParam = (sheet: unknown, value: unknown) =>
  Number(value) > 0 ? { sheet: String(sheet || module.value), at: Math.trunc(Number(value)) } : null
const row = ref(rowParam(route.query.hoja, route.query.fila))
/** The search a result was opened from (and the sheet then first), to go back to it. */
const lastSearch = ref<{ text: string; sheet: string } | null>(null)
const showUnused = ref(false)
const grid = ref<InstanceType<typeof SheetGrid>>()
// "Revisión de datos" is its own tab now (Revisión); old links to it land there (also when Tablas is already open).
const toRevision = (query: typeof route.query) =>
  query.revision === '1' && router.replace({ path: '/revision', query: query.hoja ? { hoja: query.hoja } : {} })
toRevision(route.query)

/** The sheet as it was at a save (SheetAsOf), shown over the sheet or the results it was opened from. */
interface Past {
  module: string
  row: number
  action: string
  side: AsOfSide
  field: string | null
}
const pastParam = (query: typeof route.query): Past | null => {
  const action = query.antes ?? query.despues
  if (!action || !(Number(query.fila) > 0)) return null
  return {
    module: String(query.hoja || module.value),
    row: Math.trunc(Number(query.fila)),
    action: String(action),
    side: query.antes ? 'before' : 'after',
    field: query.campo ? String(query.campo) : null,
  }
}
const past = ref<Past | null>(pastParam(route.query))
const samePast = (a: Past | null, b: Past | null) => JSON.stringify(a) === JSON.stringify(b)

const searching = computed(() => !!debounced.value.trim())
/** The results stay (hidden) while a result is open in its sheet, so going back is instant. */
const resultsQuery = ref(searching.value ? debounced.value : '')
watch(debounced, text => text.trim() && (resultsQuery.value = text))
/** The whole sheet is read (and its grid built) only once it is shown: not under a past view opened from a link. */
const sheetShown = computed(() => !searching.value && !past.value)
const sheetSeen = ref(sheetShown.value)
watch(sheetShown, shown => shown && (sheetSeen.value = true))
const { table, ready, loading, options, creates, createFormulas, load } = useSheet(module, sheetShown)

let timer: ReturnType<typeof setTimeout>
watch(search, value => {
  clearTimeout(timer)
  // Emptied: back to the sheet at once.
  timer = setTimeout(() => (debounced.value = value), value.trim() ? 300 : 0)
})
// The link keeps what is shown, so a copied link opens the same view.
const linkQuery = () =>
  past.value
    ? {
        hoja: past.value.module,
        fila: String(past.value.row),
        [past.value.side === 'before' ? 'antes' : 'despues']: past.value.action,
        ...(past.value.field ? { campo: past.value.field } : {}),
      }
    : searching.value
      ? { buscar: debounced.value, hoja: module.value }
      : { hoja: module.value, ...(row.value?.sheet === module.value ? { fila: String(row.value.at) } : {}) }
// Tablas stays alive in the background (KeepAlive): only its own route is touched.
const here = () => route.path === '/tablas'
watch([module, debounced, row, past], () => here() && router.replace({ query: linkQuery() }))
watch(
  () => route.query,
  query => {
    if (!here() || toRevision(query)) return
    // The tab itself (no query) shows what was there; the address follows.
    if (!query.hoja && query.buscar === undefined) return void router.replace({ query: linkQuery() })
    // A past view (a link from the Historial, or this page's own address): the search and sheet stay as they were.
    const linkedPast = pastParam(query)
    if (linkedPast) {
      if (!samePast(linkedPast, past.value)) past.value = linkedPast
      return
    }
    past.value = null
    if (query.hoja && query.hoja !== module.value) module.value = String(query.hoja)
    const text = String(query.buscar ?? '')
    if (text !== debounced.value) search.value = debounced.value = text
    const linked = rowParam(query.hoja, query.fila)
    if (!text && linked && (linked.sheet !== row.value?.sheet || linked.at !== row.value?.at)) row.value = linked
  },
)

/** A search result opened in its whole sheet, at its row. */
function openRow(sheet: string, at: number) {
  lastSearch.value = { text: debounced.value, sheet: module.value }
  module.value = sheet
  row.value = { sheet, at }
  search.value = debounced.value = ''
}
function backToResults() {
  if (!lastSearch.value) return
  module.value = lastSearch.value.sheet
  search.value = debounced.value = lastSearch.value.text
  lastSearch.value = null
}
function clearSearch() {
  search.value = debounced.value = ''
}

/** The cell whose history shows beside the grid (from the sheet, a search result or a past view). */
const historyTarget = ref<HistoryTarget | null>(null)
function openPast(moment: { action: string; side: AsOfSide }, field: string | null) {
  const target = historyTarget.value
  if (!target?.row) return
  past.value = { module: target.module, row: target.row, field, ...moment }
  // On a phone the panel would cover the view it opened.
  if (phone.value) historyTarget.value = null
}
function movePast(moment: { action: string; side: AsOfSide }) {
  if (past.value) past.value = { ...past.value, ...moment }
}
/** Back to the sheet as it is now, at the same row (or to the results the past view was opened from). */
function closePast() {
  const shown = past.value
  past.value = null
  if (!shown || searching.value) return
  module.value = shown.module
  row.value = { sheet: shown.module, at: shown.row }
}

// Phones: one row of controls (search, sheet, ⋯); the other tools in the ⋯ menu.
const phone = usePhoneWidth()
const menuOpen = ref(false)
const toolbar = ref<HTMLElement>()
function closeMenuOutside(event: PointerEvent) {
  if (menuOpen.value && !toolbar.value?.contains(event.target as Node)) menuOpen.value = false
}
document.addEventListener('pointerdown', closeMenuOutside)
onBeforeUnmount(() => document.removeEventListener('pointerdown', closeMenuOutside))

/** How to use the grid: shown until hidden, then behind the ⓘ button. */
const HINT_KEY = 'buscador.hint-hidden'
const hintHidden = ref(localStorage.getItem(HINT_KEY) === '1')
function setHint(hidden: boolean) {
  hintHidden.value = hidden
  if (hidden) localStorage.setItem(HINT_KEY, '1')
  else localStorage.removeItem(HINT_KEY)
}
/** A changed header's warning: one line on a phone until tapped. */
const noticeOpen = ref(false)

const grouped = computed(() => {
  const out: Record<string, { id: string; count: number }[]> = {}
  for (const m of session.modules)
    (out[GROUPS[m.group] ? t(GROUPS[m.group]) : m.group] ||= []).push({ id: m.id, count: m.recordCount })
  return out
})
/** The sheets by group, each with its number of rows in grey. */
const sheetChoices = computed(() =>
  Object.entries(grouped.value).flatMap(([group, list]) =>
    list.map(m => ({ value: m.id, label: m.id, hint: String(m.count), group })),
  ),
)
/** The row opened, in this sheet's copy (an unused pre-made row too). */
const opened = computed(() =>
  row.value?.sheet === module.value && table.value ? table.value.rows.find(r => r.row === row.value?.at) : undefined,
)
watch(opened, r => r && !r.observed && (showUnused.value = true))
const rows = computed(() => (table.value ? (showUnused.value ? table.value.rows : table.value.rows.filter(r => r.observed)) : []))
const mod = computed(() => session.module(module.value))
const frozen = computed(() => mod.value?.identityFields.slice(0, 1) || [])
const sheetLink = computed(() =>
  session.settings?.sheetUrl && mod.value ? `${session.settings.sheetUrl}#gid=${mod.value.sheetId}` : undefined,
)

function addRow() {
  pending.addCreate(module.value, t('nueva'), {})
  pending.touch()
}

/** How the sheet's header row differs from the columns the app knows (moved columns are not a problem). */
const headerNotice = computed(() => {
  const problems = table.value?.headerProblems || []
  if (!problems.length) return null
  const text = (p: HeaderProblem) =>
    p.kind === 'missing'
      ? t('falta la columna {field}', { field: p.field })
      : p.kind === 'new'
        ? t('columna nueva {field} en {column} (ignorada)', { field: p.field, column: p.column })
        : p.kind === 'duplicate'
          ? t('{field} aparece dos veces ({columns})', { field: p.field, columns: p.columns?.join(', ') })
          : t('no se reconoce la fila de encabezados')
  const shown = problems.filter(p => p.blocking || !problems.some(q => q.blocking)).slice(0, 8)
  return {
    blocking: problems.some(p => p.blocking),
    missing: problems.some(p => p.kind === 'missing'),
    text: shown.map(text).join(' · ') + (problems.length > shown.length ? ' · …' : ''),
  }
})
</script>

<template>
  <div class="flex h-full flex-col">
    <div ref="toolbar" class="toolbar relative max-sm:flex-nowrap max-sm:items-center max-sm:gap-2 max-sm:py-2">
      <label class="min-w-0 flex-1 sm:min-w-48">
        <span class="field-label max-sm:sr-only">{{ $t('Buscar en todas las hojas') }}</span>
        <span class="relative block">
          <Search :size="15" class="absolute top-2.5 left-2.5 text-stone-400" />
          <input
            v-model="search"
            class="field-input pr-8 pl-8"
            :placeholder="phone ? $t('Buscar en todas las hojas') : $t('Insectary ID, CAM, tubo, clutch, especie, nota…')"
            type="text"
            enterkeyhint="search"
            @keydown.esc="clearSearch"
          />
          <button
            v-if="search"
            type="button"
            class="absolute top-1.5 right-1.5 rounded p-1 text-stone-500 hover:bg-stone-100"
            :title="$t('Borrar la búsqueda y volver a la hoja')"
            @click="clearSearch"
          >
            <X :size="15" />
          </button>
        </span>
      </label>
      <label class="max-sm:w-[9.5rem] max-sm:flex-none">
        <span class="field-label max-sm:sr-only">{{ searching ? $t('Hoja primero') : $t('Hoja') }}</span>
        <ChoiceField v-model="module" class="field-input sm:min-w-52" :options="sheetChoices" :freetext="false" />
      </label>
      <template v-if="!searching && !past">
        <button
          type="button"
          class="btn flex-none px-2 sm:hidden"
          :class="{ 'bg-stone-100': menuOpen }"
          :title="$t('Más herramientas')"
          :aria-label="$t('Más herramientas')"
          :aria-expanded="menuOpen"
          @click="menuOpen = !menuOpen"
        >
          <EllipsisVertical :size="17" />
        </button>
        <!-- The tools: in the toolbar on a computer, in the ⋯ menu on a phone (a tap on one closes it). -->
        <div class="buscador-tools" :class="{ 'is-open': menuOpen }" @click="menuOpen = false">
          <label class="flex items-center gap-2 pb-1.5 text-sm max-sm:py-1" @click.stop>
            <input v-model="showUnused" type="checkbox" /> {{ $t('Filas vacías preasignadas') }}
          </label>
          <div class="flex flex-wrap gap-2 max-sm:flex-col">
            <button v-if="lastSearch" class="btn" @click="backToResults">
              <ArrowLeft :size="15" /> {{ $t('Resultados de «{text}»', { text: lastSearch.text }) }}
            </button>
            <RouterLink
              v-if="session.canEdit"
              :to="{ path: '/revision', query: { hoja: module } }"
              class="btn"
              :title="
                $t(
                  'Datos que no cuadran en todo el libro (IDs repetidos, fechas, colecta ↔ insectario, sobres y fotos), en la pestaña Revisión',
                )
              "
            >
              <ShieldAlert :size="15" /> {{ $t('Revisión de datos') }}
            </RouterLink>
            <button v-if="session.canEdit" class="btn" @click="addRow"><Plus :size="15" /> {{ $t('Añadir fila') }}</button>
            <ExtendRowsButton v-if="session.isReviewer" :sheet="module" :count="50" @done="load(true)" />
            <button class="btn" :title="$t('Copiar la primera fila seleccionada hacia abajo (Ctrl+D)')" @click="grid?.fillDown()">
              <ArrowDownToLine :size="15" /> {{ $t('Rellenar') }}
            </button>
            <button class="btn" :disabled="loading" :title="$t('Volver a cargar desde el servidor')" @click="load(true)">
              <RefreshCw :size="15" :class="{ 'animate-spin': loading }" />
              <span class="sm:hidden">{{ $t('Volver a cargar') }}</span>
            </button>
            <a
              :href="`api/export?module=${encodeURIComponent(module)}&format=csv`"
              class="btn"
              :title="$t('Descargar la hoja como CSV')"
            >
              <FileDown :size="15" /> <span class="sm:hidden">{{ $t('Descargar CSV') }}</span>
            </a>
            <a v-if="sheetLink" :href="sheetLink" target="_blank" rel="noopener" class="btn" :title="$t('Abrir en Google Sheets')">
              <ExternalLink :size="15" /> <span class="sm:hidden">{{ $t('Abrir en Google Sheets') }}</span>
            </a>
            <button v-if="hintHidden" class="btn" :title="$t('Cómo usar la tabla')" @click="setHint(false)">
              <Info :size="15" /> <span class="sm:hidden">{{ $t('Cómo usar la tabla') }}</span>
            </button>
          </div>
        </div>
      </template>
    </div>
    <div class="flex min-h-0 flex-1">
      <div class="flex min-h-0 min-w-0 flex-1 flex-col">
        <SheetAsOf
          v-if="past"
          :module="past.module"
          :row="past.row"
          :action="past.action"
          :side="past.side"
          :field="past.field"
          @now="closePast"
          @move="movePast"
          @history="target => (historyTarget = target)"
        />
        <div v-if="resultsQuery" v-show="searching && !past" class="min-h-0 flex-1 overflow-y-auto overscroll-contain bg-stone-50">
          <SearchResults :query="resultsQuery" :pin="module" @open="openRow" @history="target => (historyTarget = target)" />
        </div>
        <div v-if="sheetSeen" v-show="!searching && !past" class="flex min-h-0 flex-1 flex-col">
          <!-- A changed header: one line on a phone until tapped. -->
          <p
            v-if="headerNotice?.blocking"
            class="bg-red-50 px-4 py-2 text-sm text-red-800 max-sm:px-3 max-sm:py-1.5"
            :class="{ 'max-sm:line-clamp-1': !noticeOpen }"
            @click="noticeOpen = !noticeOpen"
          >
            {{
              $t(
                'La hoja {sheet} cambió en Google Sheets: {problems}. No se lee ni se guarda en esta hoja hasta corregir los encabezados.',
                { sheet: module, problems: headerNotice.text },
              )
            }}
          </p>
          <p
            v-else-if="headerNotice && session.isReviewer"
            class="bg-amber-50 px-4 py-2 text-sm text-amber-900 max-sm:px-3 max-sm:py-1.5"
            :class="{ 'max-sm:line-clamp-1': !noticeOpen }"
            @click="noticeOpen = !noticeOpen"
          >
            {{ $t('La hoja cambió: {problems}.', { problems: headerNotice.text }) }}
            <template v-if="headerNotice.missing">{{
              $t('Esas columnas se muestran con su último valor y no se pueden editar.')
            }}</template>
          </p>
          <p v-if="!hintHidden" class="hint flex items-start gap-2 px-4 py-1 max-sm:px-3">
            <span class="min-w-0 flex-1">
              <template v-if="touch">{{
                $t(
                  'Toca una celda para seleccionarla y dos veces para editarla · arrastra el círculo para ampliar la selección · abajo: Copiar, Pegar, Rellenar ↓, Borrar · toca el número de fila para ver la fila completa · gris = fórmula.',
                )
              }}</template>
              <template v-else>{{
                $t(
                  'Escribe sobre una celda, o pulsa Enter o doble clic para editarla · pega rangos desde Excel o Sheets · Ctrl+D rellena hacia abajo · las celdas grises son fórmulas · clic en el número de fila para ver la fila completa.',
                )
              }}</template>
              {{ $t('Historial: los cambios de la celda elegida.') }}
            </span>
            <button
              type="button"
              class="-my-0.5 flex-none rounded p-1 text-stone-500 hover:bg-stone-100"
              :title="$t('Ocultar la ayuda (vuelve con ⓘ)')"
              :aria-label="$t('Ocultar la ayuda (vuelve con ⓘ)')"
              @click="setHint(true)"
            >
              <X :size="14" />
            </button>
          </p>
          <div class="min-h-0 flex-1">
            <p v-if="!ready || !table" class="p-6 text-stone-500">{{ $t('Cargando {sheet}…', { sheet: module }) }}</p>
            <SheetGrid
              v-else
              ref="grid"
              :module="module"
              :rows="rows"
              :columns="table.columns"
              :creates="creates"
              :options="options"
              :frozen="frozen"
              :create-formulas="createFormulas"
              :focus-row="opened?.id ?? null"
              :mark="opened && lastSearch ? lastSearch.text : ''"
              cell-history
              @notice="notify"
              @history="target => (historyTarget = target)"
              @remove-create="
                id => {
                  pending.removeCreate(id)
                  pending.touch()
                }
              "
            />
          </div>
        </div>
        <WorkbookWarnings v-if="!past" :sheet="module" />
      </div>
      <!-- A cell's history: beside the grid on a computer (the grid narrows), over its bottom on a phone. -->
      <CellHistory v-if="historyTarget" :target="historyTarget" @close="historyTarget = null" @view="openPast" />
    </div>
  </div>
</template>
