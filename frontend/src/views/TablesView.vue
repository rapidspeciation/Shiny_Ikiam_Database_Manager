<script setup lang="ts">
import ChoiceField from '../components/ChoiceField.vue'
import { computed, ref, watch } from 'vue'
import { RouterLink, useRoute, useRouter } from 'vue-router'
import { Plus, RefreshCw, ArrowDownToLine, ArrowLeft, ExternalLink, Search, FileDown, ShieldAlert, X } from 'lucide-vue-next'
import SheetGrid from '../components/SheetGrid.vue'
import SearchResults from '../components/search/SearchResults.vue'
import WorkbookWarnings from '../components/WorkbookWarnings.vue'
import ExtendRowsButton from '../components/ExtendRowsButton.vue'
import type { HeaderProblem } from '../lib/types'
import { useSheet } from '../composables/useSheet'
import { notify } from '../lib/notice'
import { usePending } from '../stores/pending'
import { useSession } from '../stores/session'
import { t } from '../lib/i18n'

/**
 * The "Buscador": a text searched in every sheet, as Google Sheets' Ctrl+F,
 * each sheet's match shown among its neighbouring rows (SearchResults); with
 * no text, any sheet whole as an editable spreadsheet, as in the original app.
 * Links: ?buscar=<text> searches (?hoja=<sheet> puts that sheet first);
 * ?hoja=<sheet>&fila=<row> opens the sheet at that row.
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

const searching = computed(() => !!debounced.value.trim())
/** The results stay (hidden) while a result is open in its sheet, so going back is instant. */
const resultsQuery = ref(searching.value ? debounced.value : '')
watch(debounced, text => text.trim() && (resultsQuery.value = text))
/** The whole sheet is read (and its grid built) only once it is shown. */
const sheetShown = computed(() => !searching.value)
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
  searching.value
    ? { buscar: debounced.value, hoja: module.value }
    : { hoja: module.value, ...(row.value?.sheet === module.value ? { fila: String(row.value.at) } : {}) }
// Tablas stays alive in the background (KeepAlive): only its own route is touched.
const here = () => route.path === '/tablas'
watch([module, debounced, row], () => here() && router.replace({ query: linkQuery() }))
watch(
  () => route.query,
  query => {
    if (!here() || toRevision(query)) return
    // The tab itself (no query) shows what was there; the address follows.
    if (!query.hoja && query.buscar === undefined) return void router.replace({ query: linkQuery() })
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
    <div class="toolbar">
      <label class="min-w-48 flex-1">
        <span class="field-label">{{ $t('Buscar en todas las hojas') }}</span>
        <span class="relative block">
          <Search :size="15" class="absolute top-2.5 left-2.5 text-stone-400" />
          <input
            v-model="search"
            class="field-input pr-8 pl-8"
            :placeholder="$t('Insectary ID, CAM, tubo, clutch, especie, nota…')"
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
      <label>
        <span class="field-label">{{ searching ? $t('Hoja primero') : $t('Hoja') }}</span>
        <ChoiceField v-model="module" class="field-input min-w-52" :options="sheetChoices" :freetext="false" />
      </label>
      <template v-if="!searching">
        <label class="flex items-center gap-2 pb-1.5 text-sm">
          <input v-model="showUnused" type="checkbox" /> {{ $t('Filas vacías preasignadas') }}
        </label>
        <div class="flex flex-wrap gap-2">
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
          </button>
          <a
            :href="`api/export?module=${encodeURIComponent(module)}&format=csv`"
            class="btn"
            :title="$t('Descargar la hoja como CSV')"
          >
            <FileDown :size="15" />
          </a>
          <a v-if="sheetLink" :href="sheetLink" target="_blank" rel="noopener" class="btn" :title="$t('Abrir en Google Sheets')">
            <ExternalLink :size="15" />
          </a>
        </div>
      </template>
    </div>
    <div v-if="resultsQuery" v-show="searching" class="min-h-0 flex-1 overflow-y-auto bg-stone-50">
      <SearchResults :query="resultsQuery" :pin="module" @open="openRow" />
    </div>
    <div v-if="sheetSeen" v-show="!searching" class="flex min-h-0 flex-1 flex-col">
      <p v-if="headerNotice?.blocking" class="bg-red-50 px-4 py-2 text-sm text-red-800">
        {{
          $t(
            'La hoja {sheet} cambió en Google Sheets: {problems}. No se lee ni se guarda en esta hoja hasta corregir los encabezados.',
            { sheet: module, problems: headerNotice.text },
          )
        }}
      </p>
      <p v-else-if="headerNotice && session.isReviewer" class="bg-amber-50 px-4 py-2 text-sm text-amber-900">
        {{ $t('La hoja cambió: {problems}.', { problems: headerNotice.text }) }}
        <template v-if="headerNotice.missing">{{
          $t('Esas columnas se muestran con su último valor y no se pueden editar.')
        }}</template>
      </p>
      <p class="hint px-4 py-1">
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
          @notice="notify"
          @remove-create="
            id => {
              pending.removeCreate(id)
              pending.touch()
            }
          "
        />
      </div>
    </div>
    <WorkbookWarnings :sheet="module" />
  </div>
</template>
