<script setup lang="ts">
import ChoiceField from '../components/ChoiceField.vue'
import DateField from '../components/DateField.vue'
import { computed, defineAsyncComponent, nextTick, onBeforeUnmount, onDeactivated, ref, watch } from 'vue'
import { Eraser, History, Loader2, Plus, Save, Trash2, Undo2, X } from 'lucide-vue-next'
import type CollectGridType from '../components/CollectGrid.vue'
import CollectCards from '../components/collect/CollectCards.vue'
import SaveSummary from '../components/collect/SaveSummary.vue'
import EntryModeToggle from '../components/EntryModeToggle.vue'
import TabHistory from '../components/history/TabHistory.vue'
import InsectaryIdsWarning from '../components/InsectaryIdsWarning.vue'
import { MODULE, useCollect } from '../composables/useCollect'
import { useEntryMode } from '../composables/useEntryMode'
import { formatSerial, isoToSerial, todayIso, weekdayOf } from '../lib/dates'
import { notify } from '../lib/notice'
import { FATES, type Fate } from '../lib/collect'
import { orderColumns } from '../lib/rows'
import { usePending } from '../stores/pending'
import { useTables } from '../stores/tables'
import { t, tn } from '../lib/i18n'

/**
 * Colecta: a day of field collection, as cards (the default on every device:
 * the day on top, one card per butterfly, big buttons) or as the table (a
 * spreadsheet: ranges, paste from Excel, the fill handle). Both share the list
 * being typed and the day (useCollect), so switching keeps them, and save it
 * the same way: Collection_data, and Insectary_data for the live ones, in one
 * save with an Undo. The latest rows of Collection_data are below the table.
 */
const pending = usePending()
const tables = useTables()
const state = useCollect()
const {
  table,
  ready,
  options,
  creates,
  createFormulas,
  header,
  drafts,
  addFate,
  observed,
  speciesList,
  subspeciesFor,
  places,
  people,
  rainfalls,
  clouds,
  mediums,
  purposes,
  upcoming,
  earlierIds,
  nextId,
  loadFreeIds,
  pasteText,
  editCell,
  addDrafts,
  remove,
  emptyCount,
  removeEmpty,
  clearAll,
  idProblem,
  cellProblem,
  problems,
  saving,
  save,
  lastSave,
  undoing,
  undo,
} = state
const { mode } = useEntryMode('collect')
// The grids (Tabulator) load only for the table: the cards open without them.
const CollectGrid = defineAsyncComponent(() => import('../components/CollectGrid.vue'))
const SheetGrid = defineAsyncComponent(() => import('../components/SheetGrid.vue'))

const addCount = ref(1)
/** Optional: the species of all the rows being added (e.g. five Mechanitis at once). */
const addSpecies = ref('')
const touchScreen = window.matchMedia('(pointer: coarse)').matches
/** Phones show the outing as one line ("23-Sep · Cavernas · PAS ✎") until tapped; open while no place is chosen. */
const headerOpen = ref(!touchScreen || !header.value.location)
const headerChip = computed(() => {
  const h = header.value
  const day = h.date ? formatSerial(isoToSerial(h.date)).replace(/-\d{2}$/, '') : t('sin fecha')
  return [day, h.location || t('sin lugar'), h.collector.split(' - ')[0]].filter(Boolean).join(' · ')
})
const isToday = computed(() => header.value.date === todayIso())
const grid = ref<InstanceType<typeof CollectGridType>>()
/** The list's bar stays at the top while scrolling, and the grid's column names right under it (style.css). */
const stickyBar = ref<HTMLElement>()
const stickyBarHeight = ref(0)
const barSize =
  typeof ResizeObserver === 'undefined'
    ? null
    : new ResizeObserver(([entry]) => {
        stickyBarHeight.value = Math.round((entry.target as HTMLElement).offsetHeight)
      })
watch(stickyBar, (el, old) => {
  if (old) barSize?.unobserve(old)
  if (el) barSize?.observe(el)
})
onBeforeUnmount(() => barSize?.disconnect())
const recentCount = ref(10)
const showHistory = ref(false)
const fateChoices = Object.entries(FATES).map(([value, f]) => ({ value, label: f.label }))

async function add() {
  if (!header.value.location) {
    headerOpen.value = true
    return notify(t('Elige el lugar de colecta'))
  }
  const count = Math.min(60, Math.max(1, Math.round(addCount.value || 1)))
  const first = addDrafts(count, { fate: addFate.value, species: addSpecies.value })
  notify(
    tn(count, 'Se añadieron {n} fila: la lista tiene {total}', 'Se añadieron {n} filas: la lista tiene {total}', {
      total: drafts.value.length,
    }),
  )
  // Straight to the first new row, ready to type its species.
  await nextTick()
  grid.value?.focusCell(first, 'species')
}
const groups = computed(() => {
  const out = new Map<string, Record<Fate, number>>()
  for (const d of drafts.value) {
    const g = out.get(d.location) || { insectario: 0, preservada: 0, liberada: 0 }
    g[d.fate]++
    out.set(d.location, g)
  }
  return [...out]
})

/** Read over before saving. */
const confirming = ref(false)
onDeactivated(() => (confirming.value = false))
function askSave() {
  if (!drafts.value.length) return
  if (problems.value.length) return notify(problems.value.slice(0, 3).join('; '), 'error')
  confirming.value = true
}
function confirmSave() {
  confirming.value = false
  save()
}

const columns = computed(() =>
  table.value
    ? // Who the butterfly is first (Insectary_ID pinned), then what it is, then what happened, when, where and by whom.
      orderColumns(table.value.columns, [
        'Insectary_ID',
        'CAM_ID',
        'Tube_1_id',
        'SPECIES',
        'Subspecies_Form',
        'Sex',
        'Release_Collect',
        'Collection_date',
        'Collection_location',
        'Collector',
        'Identifier',
        'Rainfall',
        'Cloud_cover',
        'Preservation_medium',
        'Notes_Collection_data',
      ])
    : [],
)
const recent = computed(() => observed.value.slice(-recentCount.value))
</script>

<template>
  <CollectCards v-if="mode === 'cards'" v-model:mode="mode" :state="state" />
  <div v-else class="flex h-full flex-col overflow-y-auto">
    <!-- Phones: the outing folds into one line, so the list is in sight; a tap opens it. -->
    <button
      v-if="touchScreen"
      type="button"
      class="flex w-full items-center gap-2 border-b border-stone-200 bg-white px-3 py-2 text-left text-sm"
      :aria-expanded="headerOpen"
      @click="headerOpen = !headerOpen"
    >
      <span class="min-w-0 flex-1 truncate rounded-full bg-stone-100 px-3 py-1" :class="{ 'bg-amber-50 text-amber-900': isToday }">
        {{ headerChip }}
      </span>
      <span class="shrink-0 text-brand-700">{{ headerOpen ? $t('Cerrar') : '✎' }}</span>
    </button>
    <div v-if="headerOpen" class="toolbar">
      <label>
        <span class="field-label"
          >Collection_date <span class="font-normal text-stone-500">{{ weekdayOf(header.date) }}</span></span
        >
        <DateField v-model="header.date" class="field-input" :class="{ 'border-amber-500 bg-amber-50': isToday }" />
        <span v-if="isToday" class="block text-xs text-amber-800">{{ $t('¿Es hoy la fecha de la colecta?') }}</span>
      </label>
      <label class="min-w-52">
        <span class="field-label"
          >Collector <span class="font-normal text-stone-500">{{ $t('(filas nuevas)') }}</span></span
        >
        <ChoiceField v-model="header.collector" class="field-input" :options="people" />
      </label>
      <label class="min-w-52">
        <span class="field-label"
          >Identifier <span class="font-normal text-stone-500">{{ $t('(filas nuevas)') }}</span></span
        >
        <ChoiceField v-model="header.identifier" class="field-input" :options="people" />
      </label>
      <label>
        <span class="field-label"
          >Rainfall <span class="font-normal text-stone-500">{{ $t('(filas nuevas)') }}</span></span
        >
        <ChoiceField v-model="header.rainfall" class="field-input" :options="rainfalls" :freetext="false" allow-empty />
      </label>
      <label>
        <span class="field-label"
          >Cloud_cover <span class="font-normal text-stone-500">{{ $t('(filas nuevas)') }}</span></span
        >
        <ChoiceField v-model="header.cloud" class="field-input" :options="clouds" :freetext="false" allow-empty />
      </label>
      <div class="ml-auto flex items-end gap-2 self-end">
        <button class="btn" :title="$t('Historial de Colecta')" @click="showHistory = true">
          <History :size="15" /> {{ $t('Historial') }}
        </button>
        <EntryModeToggle v-model="mode" />
      </div>
    </div>
    <div class="toolbar border-t-0" :class="{ 'gap-2 py-2': !headerOpen }">
      <label v-if="headerOpen" class="min-w-64">
        <span class="field-label">{{ $t('Collection_location (cámbialo para añadir mariposas de otro sitio)') }}</span>
        <ChoiceField v-model="header.location" class="field-input" :options="places" />
      </label>
      <label>
        <span class="field-label">{{ $t('Filas a añadir') }}</span>
        <input v-model.number="addCount" type="number" min="1" max="60" class="field-input w-20" />
      </label>
      <label v-if="headerOpen" class="min-w-48">
        <span class="field-label">{{ $t('SPECIES (opcional)') }}</span>
        <ChoiceField v-model="addSpecies" class="field-input" :options="speciesList" :placeholder="$t('la misma para todas')" />
      </label>
      <label>
        <span class="field-label">Release_Collect</span>
        <ChoiceField v-model="addFate" class="field-input" :options="fateChoices" :freetext="false" />
      </label>
      <button class="btn-primary" @click="add">
        <Plus :size="15" /> {{ $tn(Math.max(1, addCount || 1), 'Añadir {n} fila', 'Añadir {n} filas') }}
      </button>
      <EntryModeToggle v-if="!headerOpen" v-model="mode" compact class="ml-auto" />
      <div v-if="headerOpen" class="ml-auto flex gap-2">
        <div
          class="rounded-md border border-brand-600 bg-brand-50 px-3 py-1"
          :title="$t('Wild_indv_CAMid de Lists: quedan {n} sin usar', { n: upcoming.camsLeft })"
        >
          <p class="text-xs text-brand-700">{{ $t('Próximo CAM_ID') }}</p>
          <p class="font-mono text-lg font-semibold text-brand-700">{{ upcoming.cam || '—' }}</p>
          <p v-if="upcoming.camsLeft < 50" class="text-xs text-amber-800">{{ $t('quedan {n}', { n: upcoming.camsLeft }) }}</p>
        </div>
        <div class="rounded-md border border-stone-300 bg-stone-50 px-3 py-1" :title="$t('Tubo de la colecta ({medium})', { medium: header.medium })">
          <p class="text-xs text-stone-600">{{ $t('Próximo tubo') }}</p>
          <p class="font-mono text-lg font-semibold">{{ upcoming.tube || '—' }}</p>
        </div>
        <div class="rounded-md border border-stone-300 bg-stone-50 px-3 py-1" :title="$t('Para las mariposas que van al insectario')">
          <p class="text-xs text-stone-600">{{ $t('Próximo Insectary ID') }}</p>
          <p class="font-mono text-lg font-semibold">{{ upcoming.insectaryId || '—' }}</p>
        </div>
      </div>
    </div>
    <!-- Live butterflies take the next pre-made Insectary IDs. -->
    <InsectaryIdsWarning class="mx-3 my-2" :revision="tables.tables.Insectary_data?.revision" @extended="loadFreeIds" />

    <!-- The last save, to undo it (its butterflies come back to the list). -->
    <div v-if="lastSave && !drafts.length" class="mx-3 my-2 flex items-center gap-2 rounded-md border border-brand-200 bg-brand-50 px-3 py-2 text-sm" role="status">
      <span class="flex-1">{{ $tn(lastSave.count, 'Colecta guardada: {n} mariposa', 'Colecta guardada: {n} mariposas') }}</span>
      <button class="btn" :disabled="undoing" @click="undo">
        <Loader2 v-if="undoing" :size="15" class="animate-spin" /><Undo2 v-else :size="15" /> {{ $t('Deshacer') }}
      </button>
      <button class="btn-ghost" :aria-label="$t('Cerrar')" @click="lastSave = null"><X :size="15" /></button>
    </div>

    <div v-if="drafts.length" class="border-b border-stone-200 bg-white px-3 pb-2" :style="{ '--sticky-bar-height': `${stickyBarHeight}px` }">
      <!-- Always in sight while scrolling the list, and kept to one line: how long it is, how to trim it. -->
      <div
        ref="stickyBar"
        data-sticky-bar
        class="sticky top-0 z-10 -mx-3 flex items-center gap-2 border-b border-stone-200 bg-white px-3 py-1.5 text-sm"
      >
        <span class="font-semibold whitespace-nowrap">{{ $tn(drafts.length, '{n} fila', '{n} filas') }}</span>
        <span v-if="emptyCount" class="whitespace-nowrap text-amber-800">{{ $t('{n} vacías', { n: emptyCount }) }}</span>
        <button v-if="emptyCount" class="btn px-2 py-1" :title="$t('Quitar filas vacías')" @click="removeEmpty">
          <Eraser :size="15" /><span class="max-sm:hidden">{{ $t('Quitar vacías') }}</span>
        </button>
        <button class="btn px-2 py-1" :title="$t('Vaciar lista')" @click="clearAll">
          <Trash2 :size="15" /><span class="max-sm:hidden">{{ $t('Vaciar lista') }}</span>
        </button>
      </div>
      <!-- How to use it: scrolls away with the page; folded on phones, where space is short. -->
      <details class="hint mt-1" :open="!touchScreen">
        <summary class="cursor-pointer select-none">{{ $t('Cómo se usa · la lista se guarda en este navegador') }}</summary>
        <p>{{ $t('Se guarda en este navegador, aunque recargues o cierres la página, hasta que la guardes o la vacíes.') }}</p>
        <p v-if="touchScreen">
          {{
            $t(
              'Toca una celda para seleccionarla y dos veces para editarla · arrastra el círculo de la esquina para ampliar la selección · la barra de abajo copia, pega, rellena hacia abajo o borra lo seleccionado.',
            )
          }}
        </p>
        <p v-else>
          {{
            $t(
              'Como en una hoja de cálculo: selecciona celdas y arrastra el cuadrito de la esquina hacia abajo para copiarlas (Insectary_ID, CAM_ID y Tube_1_id siguen la serie: O6D → O7D, CAM079895 → CAM079896) · pega celdas de Excel o Sheets (llena hacia abajo y a la derecha, y añade filas si faltan) · Ctrl+D copia la primera fila de la selección · escribe sobre una celda para reemplazarla, doble clic para editarla.',
            )
          }}
        </p>
      </details>
      <CollectGrid
        ref="grid"
        class="mt-2"
        :drafts="drafts"
        :places="places"
        :species="speciesList"
        :subspecies-for="subspeciesFor"
        :purposes="purposes"
        :mediums="mediums"
        :people="people"
        :rainfalls="rainfalls"
        :clouds="clouds"
        :paste="pasteText"
        :id-problem="idProblem"
        :next-id="nextId"
        :cell-problem="cellProblem"
        @edit="editCell"
        @remove="remove"
        @notice="notify"
      />
      <div class="mt-2 flex flex-wrap items-center gap-3 text-sm">
        <span v-for="[place, g] in groups" :key="place" class="rounded bg-stone-100 px-2 py-0.5">
          {{ place }}: {{ g.insectario }} Collected_Sent2Insectary · {{ g.preservada }} Collected_Preserved<template v-if="g.liberada">
            · {{ g.liberada }} Released_Unmarked</template
          >
        </span>
        <span v-if="problems.length" class="text-xs text-amber-800"
          >{{ problems[0] }}<template v-if="problems.length > 1"> {{ $t('(y {n} más)', { n: problems.length - 1 }) }}</template></span
        >
        <span v-if="earlierIds.length" class="w-full text-xs text-amber-800">
          {{
            $t(
              'Ya no quedan filas preasignadas al final de Insectary_data: {ids} son filas vacías anteriores. Comprueba que ningún ID esté ya escrito en otra mariposa, o crea más filas preasignadas en Insectary_data.',
              { ids: earlierIds.slice(0, 4).join(', ') + (earlierIds.length > 4 ? '…' : '') },
            )
          }}
        </span>
        <button v-if="emptyCount" class="btn" @click="removeEmpty"><Eraser :size="15" /> {{ $t('Quitar filas vacías') }}</button>
        <button class="btn-primary ml-auto" :disabled="saving || !!problems.length" @click="askSave">
          <Save :size="15" /> {{ saving ? $t('Guardando…') : $t('Guardar colecta ({n})', { n: drafts.length }) }}
        </button>
      </div>
      <p class="hint mt-1">
        {{
          $t(
            'Las que van al insectario reciben el Insectary ID para escribir en las alas, sin CAM ID (se da al hacer el wing clip o al preservar). Se guardan en Collection_data e Insectary_data a la vez.',
          )
        }}
      </p>
    </div>

    <p class="hint px-4 py-1">
      {{ $t('Últimos {n} registros de Collection_data.', { n: recentCount }) }}
      <button class="underline" @click="recentCount += 20">{{ $t('ver más') }}</button>
    </p>
    <div class="min-h-80 flex-1">
      <p v-if="!ready" class="p-6 text-stone-500">{{ $t('Cargando {sheet}…', { sheet: 'Collection_data' }) }}</p>
      <SheetGrid
        v-else
        :module="MODULE"
        :rows="recent"
        :creates="creates"
        :columns="columns"
        :options="options"
        :frozen="['Insectary_ID']"
        :create-formulas="createFormulas"
        label-field="Insectary_ID"
        @notice="notify"
        @remove-create="
          id => {
            pending.removeCreate(id)
            pending.touch()
          }
        "
      />
    </div>

    <SaveSummary v-if="confirming" :drafts="drafts" :date="header.date" :saving="saving" @close="confirming = false" @confirm="confirmSave" />
    <TabHistory v-if="showHistory" :title="$t('Historial de Colecta')" purpose="colecta" @close="showHistory = false" />
  </div>
</template>
