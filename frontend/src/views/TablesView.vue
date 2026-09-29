<script setup lang="ts">
import ChoiceField from '../components/ChoiceField.vue'
import { computed, ref, watch } from 'vue'
import { RouterLink, useRoute, useRouter } from 'vue-router'
import { Plus, RefreshCw, ArrowDownToLine, ExternalLink, Search, FileDown, ShieldAlert } from 'lucide-vue-next'
import SheetGrid from '../components/SheetGrid.vue'
import WorkbookWarnings from '../components/WorkbookWarnings.vue'
import { useSheet } from '../composables/useSheet'
import { notify } from '../lib/notice'
import { usePending } from '../stores/pending'
import { useSession } from '../stores/session'

/** Any sheet as an editable spreadsheet: the "Buscador" of the original app. */
const session = useSession()
const pending = usePending()
const route = useRoute()
const router = useRouter()

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
const showUnused = ref(false)
const grid = ref<InstanceType<typeof SheetGrid>>()
// "Revisión de datos" is its own tab now (Revisión); old links to it land there.
if (route.query.revision === '1') router.replace({ path: '/revision', query: route.query.hoja ? { hoja: route.query.hoja } : {} })
const { table, ready, loading, options, creates, createFormulas, load } = useSheet(module)

let timer: ReturnType<typeof setTimeout>
watch(search, value => {
  clearTimeout(timer)
  timer = setTimeout(() => (debounced.value = value), 250)
})
watch(module, value => router.replace({ query: { hoja: value } }))
watch(
  () => route.query,
  query => {
    if (query.hoja && query.hoja !== module.value) module.value = String(query.hoja)
    if (query.buscar !== undefined) search.value = debounced.value = String(query.buscar)
  },
)

const grouped = computed(() => {
  const out: Record<string, { id: string; count: number }[]> = {}
  for (const m of session.modules) (out[GROUPS[m.group] || m.group] ||= []).push({ id: m.id, count: m.recordCount })
  return out
})
/** The sheets by group, each with its number of rows in grey. */
const sheetChoices = computed(() =>
  Object.entries(grouped.value).flatMap(([group, list]) =>
    list.map(m => ({ value: m.id, label: m.id, hint: String(m.count), group })),
  ),
)
const rows = computed(() => (table.value ? (showUnused.value ? table.value.rows : table.value.rows.filter(r => r.observed)) : []))
const mod = computed(() => session.module(module.value))
const frozen = computed(() => mod.value?.identityFields.slice(0, 1) || [])
const sheetLink = computed(() =>
  session.settings && mod.value ? `${session.settings.sheetUrl}#gid=${mod.value.sheetId}` : undefined,
)

function addRow() {
  pending.addCreate(module.value, 'nueva', {})
  pending.touch()
}
</script>

<template>
  <div class="flex h-full flex-col">
    <div class="toolbar">
      <label>
        <span class="field-label">Hoja</span>
        <ChoiceField v-model="module" class="field-input min-w-52" :options="sheetChoices" :freetext="false" />
      </label>
      <label class="min-w-48 flex-1">
        <span class="field-label">Buscar en todas las columnas</span>
        <span class="relative block">
          <Search :size="15" class="absolute top-2.5 left-2.5 text-stone-400" />
          <input v-model="search" class="field-input pl-8" placeholder="ID, CAM, tubo, especie…" type="search" />
        </span>
      </label>
      <label class="flex items-center gap-2 pb-1.5 text-sm">
        <input v-model="showUnused" type="checkbox" /> Filas vacías preasignadas
      </label>
      <div class="flex gap-2">
        <RouterLink
          v-if="session.canEdit"
          :to="{ path: '/revision', query: { hoja: module } }"
          class="btn"
          title="Datos que no cuadran en todo el libro (IDs repetidos, fechas, colecta ↔ insectario, sobres y fotos), en la pestaña Revisión"
        >
          <ShieldAlert :size="15" /> Revisión de datos
        </RouterLink>
        <button v-if="session.canEdit" class="btn" @click="addRow"><Plus :size="15" /> Añadir fila</button>
        <button class="btn" title="Copiar la primera fila seleccionada hacia abajo (Ctrl+D)" @click="grid?.fillDown()">
          <ArrowDownToLine :size="15" /> Rellenar
        </button>
        <button class="btn" :disabled="loading" title="Volver a cargar desde el servidor" @click="load(true)">
          <RefreshCw :size="15" :class="{ 'animate-spin': loading }" />
        </button>
        <a :href="`api/export?module=${encodeURIComponent(module)}&format=csv`" class="btn" title="Descargar la hoja como CSV">
          <FileDown :size="15" />
        </a>
        <a v-if="sheetLink" :href="sheetLink" target="_blank" rel="noopener" class="btn" title="Abrir en Google Sheets">
          <ExternalLink :size="15" />
        </a>
      </div>
    </div>
    <p v-if="table?.headerProblems.length" class="bg-red-50 px-4 py-2 text-sm text-red-800">
      Las columnas de {{ module }} cambiaron en Google Sheets ({{ table.headerProblems.map(p => p.field).join(', ') }}). No se
      puede guardar en esta hoja hasta actualizar la aplicación.
    </p>
    <p class="hint px-4 py-1">
      <template v-if="touch"
        >Toca una celda para seleccionarla y dos veces para editarla · arrastra el círculo para ampliar la selección · abajo:
        Copiar, Pegar, Rellenar ↓, Borrar · toca el número de fila para ver la fila completa · gris = fórmula.</template
      >
      <template v-else>
        Escribe sobre una celda o haz doble clic para editar · pega rangos desde Excel o Sheets · Ctrl+D rellena hacia abajo · las
        celdas grises son fórmulas · clic en el número de fila para ver la fila completa.
      </template>
    </p>
    <div class="min-h-0 flex-1">
      <p v-if="!ready || !table" class="p-6 text-stone-500">Cargando {{ module }}…</p>
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
        :search="debounced"
        @notice="notify"
        @remove-create="
          id => {
            pending.removeCreate(id)
            pending.touch()
          }
        "
      />
    </div>
    <WorkbookWarnings :sheet="module" />
  </div>
</template>
