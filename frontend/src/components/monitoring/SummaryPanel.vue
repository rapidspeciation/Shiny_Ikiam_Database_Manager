<script setup lang="ts">
import { computed, ref } from 'vue'
import { Download, SlidersHorizontal } from 'lucide-vue-next'
import { useRoute, useRouter } from 'vue-router'
import ReportCharts from './ReportCharts.vue'
import { format, heat } from '../charts/chart'
import { useMonitoring } from '../../composables/useMonitoring'
import { formatSerial, isoToSerial, serialToIso, todayIso } from '../../lib/dates'
import {
  MARK_THRESHOLD,
  effortDays,
  formatMinutes,
  markConflicts,
  markHistories,
  median,
  monthOf,
  monthRange,
  nextMarkId,
  noteRecaptures,
  preservedForRule,
  recaptureIds,
  sectionsByMonth,
  speciesStats,
} from '../../lib/monitoring'
import type { TableRow } from '../../lib/types'
import { usePending } from '../../stores/pending'
import { useTables } from '../../stores/tables'

/**
 * "Reporte": a live monitoring report. The filters live in the link (so a
 * filtered view can be shared) and scope every number, chart and table below.
 */
const { table, rows: allRows, isIthomiini, withoutPurpose, tracks } = useMonitoring()
const pending = usePending()
const tables = useTables()
const route = useRoute()
const router = useRouter()
tables.load('SamplingDay_data').catch(() => {})

/** A filter stored in the page link (…?vista=resumen&desde=2025-01&rec=AA). */
function queryRef(key: string) {
  return computed<string>({
    get: () => String(route.query[key] ?? ''),
    set: value => router.replace({ query: { ...route.query, [key]: value || undefined } }),
  })
}
const from = queryRef('desde')
const to = queryRef('hasta')
const collector = queryRef('rec')
const section = queryRef('t')
const speciesFilter = queryRef('sp')
const onlyIthomiini = computed({
  get: () => route.query.ith === '1',
  set: v => router.replace({ query: { ...route.query, ith: v ? '1' : undefined } }),
})
const bySubspecies = computed({
  get: () => route.query.sub === '1',
  set: v => router.replace({ query: { ...route.query, sub: v ? '1' : undefined } }),
})

const thisMonth = todayIso().slice(0, 7)
const years = computed(
  () => [...new Set(allRows.value.map(r => monthOf(r)?.slice(0, 4)).filter(Boolean))].sort().reverse() as string[],
)
const period = computed({
  get: () => {
    if (!from.value && !to.value) return 'todo'
    const y = from.value.slice(0, 4)
    if (from.value === `${y}-01` && to.value === `${y}-12`) return y
    if (from.value === monthRange('2000-01', thisMonth).at(-12) && !to.value) return '12m'
    return 'otro'
  },
  set: value => {
    const range =
      value === 'todo'
        ? { desde: undefined, hasta: undefined }
        : value === '12m'
          ? { desde: monthRange('2000-01', thisMonth).at(-12), hasta: undefined }
          : /^\d{4}$/.test(value)
            ? { desde: `${value}-01`, hasta: `${value}-12` }
            : {}
    router.replace({ query: { ...route.query, ...range } })
  },
})

/** On phones the filters fold away behind a button (they would fill the screen). */
const showFilters = ref(false)
const activeFilters = computed(
  () => [from.value || to.value, collector.value, section.value, speciesFilter.value, onlyIthomiini.value].filter(Boolean).length,
)
const inIthomiini = (r: TableRow) => !onlyIthomiini.value || isIthomiini(String(r.values.SPECIES ?? ''))
const inPeriod = (r: TableRow) => {
  const month = monthOf(r)
  return !(from.value && (!month || month < from.value)) && !(to.value && (!month || month > to.value))
}
const byCollector = (name: string) => !collector.value || name.split(' - ')[0].trim() === collector.value
const filtered = computed(() =>
  allRows.value.filter(
    r =>
      inIthomiini(r) &&
      inPeriod(r) &&
      byCollector(String(r.values.Collector ?? '')) &&
      (!section.value || String(r.values.Transect_section ?? '') === section.value) &&
      (!speciesFilter.value || r.values.SPECIES === speciesFilter.value),
  ),
)
const collectors = computed(() =>
  [...new Set(allRows.value.map(r => String(r.values.Collector ?? '')).filter(c => / - /.test(c)))].sort(),
)
const speciesList = computed(() =>
  [
    ...new Set(
      allRows.value
        .filter(inIthomiini)
        .map(r => String(r.values.SPECIES ?? ''))
        .filter(Boolean),
    ),
  ].sort(),
)

// Recaptures and the 30 rule always look at the whole history, whatever the filters.
const recaptures = computed(() => recaptureIds(allRows.value))
// The 30 rule counts every preserved butterfly from Ikiam and Casa de Lin, whatever its purpose.
const rulePreserved = computed(() => preservedForRule(table.value?.rows || []))
const species = computed(() => speciesStats(filtered.value, bySubspecies.value, recaptures.value))
const allSpecies = ref(false)
const shownSpecies = computed(() => (allSpecies.value ? species.value : species.value.slice(0, 10)))
const totals = computed(() => {
  const t = { total: 0, preserved: 0, marked: 0, recaptured: 0 }
  for (const s of species.value) {
    t.total += s.total
    t.preserved += s.preserved
    t.marked += s.marked
    t.recaptured += s.recaptured
  }
  return t
})
const nextMark = computed(() => nextMarkId(allRows.value))
const ruleOf = (name: string) => {
  const preserved = rulePreserved.value.get(name) || 0
  return {
    applies: isIthomiini(name),
    preserved,
    done: preserved >= MARK_THRESHOLD,
    missing: Math.max(0, MARK_THRESHOLD - preserved),
  }
}

/**
 * Effort: days of monitoring per collector (SamplingDay_data plus days with
 * captures), in the period and for the chosen collector. Species and transect
 * filters do not change the effort.
 */
const effort = computed(() => {
  void tables.version
  const days = effortDays(tables.tables.SamplingDay_data?.rows || [], allRows.value)
  return [...days].filter(key => {
    const [date, ini] = key.split('|')
    const month = date.slice(0, 7)
    return (
      (!from.value || month >= from.value) && (!to.value || month <= to.value) && (!collector.value || ini === collector.value)
    )
  })
})
const perDay = computed(() => (effort.value.length ? totals.value.total / effort.value.length : 0))

const MONTHS = ['Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic']
const monthLabel = (m: string) => `${MONTHS[Number(m.slice(5)) - 1]} ${m.slice(2, 4)}`

const perSection = computed(() => sectionsByMonth(filtered.value))
const sectionMax = computed(() => Math.max(1, ...[...perSection.value.values()].flatMap(c => c.slice(1))))
const sectionTotals = computed(() => [1, 2, 3, 4, 0].map(t => [...perSection.value.values()].reduce((n, c) => n + c[t], 0)))

const histories = computed(() => {
  const inRange = new Set(filtered.value.map(r => r.id))
  return markHistories(allRows.value).filter(h => h.events.some(e => inRange.has(e.row.id)))
})
const intervals = computed(() =>
  histories.value
    .flatMap(h =>
      h.events.slice(1).map((e, i) => (e.date !== null && h.events[i].date !== null ? e.date - h.events[i].date! : null)),
    )
    .filter((d): d is number => d !== null),
)
const markedIndividuals = computed(
  () =>
    new Set(filtered.value.filter(r => r.values.Release_Collect === 'Mark_Released').map(r => String(r.values.FieldMark_ID)))
      .size,
)

/** Data to review: marks given to two species, and recaptures written only in notes. */
const conflicts = computed(() => markConflicts(allRows.value))
const noteOnly = computed(() =>
  noteRecaptures(allRows.value).filter(
    n =>
      !pending.creates.some(
        c =>
          c.values.FieldMark_ID === n.row.values.FieldMark_ID &&
          c.values.Collection_date === (n.date ? isoToSerial(n.date) : null),
      ),
  ),
)
const days = (a: number | null, b: number | null) => (a === null || b === null ? '' : `${b - a} días`)
const date = (d: number | null) => (d === null ? '—' : formatSerial(d))

/** The filtered monitoring rows, as they are in the sheet. */
function exportCsv() {
  const columns = table.value?.columns.map(c => c.key) || []
  const cell = (v: unknown) => `"${String(v ?? '').replaceAll('"', '""')}"`
  const lines = filtered.value.map(r =>
    columns
      .map(c => cell(c.includes('date') && typeof r.values[c] === 'number' ? serialToIso(r.values[c] as number) : r.values[c]))
      .join(','),
  )
  const a = document.createElement('a')
  a.href = URL.createObjectURL(
    new Blob(['﻿' + [columns.map(cell).join(','), ...lines].join('\r\n')], { type: 'text/csv;charset=utf-8' }),
  )
  a.download = `monitoreo_${from.value || 'inicio'}_${to.value || 'hoy'}.csv`
  a.click()
  URL.revokeObjectURL(a.href)
}
const sexOf = (row: TableRow) => String(row.values.Sex ?? '')
const pct = (a: number, b: number) => (b ? `${Math.round((100 * a) / b)} %` : '—')
</script>

<template>
  <div class="flex h-full flex-col">
    <button class="btn m-2 self-start sm:hidden" @click="showFilters = !showFilters">
      <SlidersHorizontal :size="15" /> Filtros<template v-if="activeFilters"> ({{ activeFilters }})</template>
    </button>
    <div class="toolbar" :class="showFilters ? 'max-sm:flex' : 'max-sm:hidden'">
      <label>
        <span class="field-label">Periodo</span>
        <select v-model="period" class="field-input">
          <option value="todo">Todo</option>
          <option value="12m">Últimos 12 meses</option>
          <option v-for="y in years" :key="y" :value="y">{{ y }}</option>
          <option value="otro" disabled>Personalizado</option>
        </select>
      </label>
      <label>
        <span class="field-label">Desde</span>
        <input v-model="from" type="month" class="field-input" />
      </label>
      <label>
        <span class="field-label">Hasta</span>
        <input v-model="to" type="month" class="field-input" />
      </label>
      <label class="min-w-40">
        <span class="field-label">Recolector</span>
        <select v-model="collector" class="field-input">
          <option value="">Todos</option>
          <option v-for="c in collectors" :key="c" :value="c.split(' - ')[0].trim()">{{ c }}</option>
        </select>
      </label>
      <label>
        <span class="field-label">Transecto</span>
        <select v-model="section" class="field-input">
          <option value="">Todos</option>
          <option v-for="t in ['1', '2', '3', '4']" :key="t" :value="t">T{{ t }}</option>
        </select>
      </label>
      <label class="min-w-48">
        <span class="field-label">Especie</span>
        <select v-model="speciesFilter" class="field-input">
          <option value="">Todas</option>
          <option v-for="s in speciesList" :key="s" :value="s">{{ s }}</option>
        </select>
      </label>
      <label class="flex items-center gap-2 pb-1.5 text-sm"
        ><input v-model="onlyIthomiini" type="checkbox" /> Solo Ithomiini</label
      >
      <label class="flex items-center gap-2 pb-1.5 text-sm"
        ><input v-model="bySubspecies" type="checkbox" /> Por subespecie</label
      >
      <button class="btn" title="Las filas filtradas, como están en la hoja" @click="exportCsv">
        <Download :size="15" /> CSV
      </button>
    </div>

    <p v-if="!table" class="p-6 text-stone-500">Cargando Collection_data…</p>
    <div v-else class="min-h-0 flex-1 space-y-4 overflow-auto bg-stone-50 p-4">
      <div class="grid grid-cols-2 gap-2 sm:grid-cols-4 xl:grid-cols-7">
        <div class="rounded-md border border-stone-200 bg-white px-3 py-2">
          <p class="text-xs text-stone-500">Individuos</p>
          <p class="text-2xl font-semibold">{{ format(totals.total) }}</p>
        </div>
        <div
          class="rounded-md border border-stone-200 bg-white px-3 py-2"
          title="Días por recolector (SamplingDay_data y días con capturas)"
        >
          <p class="text-xs text-stone-500">Días de monitoreo</p>
          <p class="text-2xl font-semibold">{{ format(effort.length) }}</p>
        </div>
        <div class="rounded-md border border-stone-200 bg-white px-3 py-2">
          <p class="text-xs text-stone-500">Individuos por día</p>
          <p class="text-2xl font-semibold">{{ format(Math.round(perDay * 10) / 10) }}</p>
        </div>
        <div class="rounded-md border border-stone-200 bg-white px-3 py-2">
          <p class="text-xs text-stone-500">Especies</p>
          <p class="text-2xl font-semibold">{{ speciesStats(filtered, false, recaptures).length }}</p>
        </div>
        <div class="rounded-md border border-stone-200 bg-white px-3 py-2">
          <p class="text-xs text-stone-500">Preservados · marcados</p>
          <p class="text-2xl font-semibold">{{ totals.preserved }} · {{ totals.marked }}</p>
        </div>
        <div class="rounded-md border border-stone-200 bg-white px-3 py-2" title="Recapturas / individuos marcados en el periodo">
          <p class="text-xs text-stone-500">Recapturas</p>
          <p class="text-2xl font-semibold">
            {{ totals.recaptured }}
            <span class="text-sm font-normal text-stone-500">{{ pct(totals.recaptured, markedIndividuals) }}</span>
          </p>
        </div>
        <div class="rounded-md border border-brand-600 bg-brand-50 px-3 py-2">
          <p class="text-xs text-brand-700">Próxima marca</p>
          <p class="text-2xl font-semibold text-brand-700">{{ nextMark || '—' }}</p>
        </div>
      </div>

      <ReportCharts :rows="filtered" :effort="effort" :recaptures="recaptures" :from="from" :to="to" :tracks="tracks" />

      <section>
        <h2 class="mb-1 font-semibold">Especies</h2>
        <p class="hint mb-2">
          Regla del protocolo: con {{ MARK_THRESHOLD }} individuos preservados de una especie de Ithomiini se pasa a marcar y
          liberar. Cuentan todos los preservados de Ikiam y Casa de Lin (monitoreo u otro propósito, todo el histórico); las demás
          columnas siguen los filtros.
        </p>
        <div class="overflow-x-auto rounded-md border border-stone-200 bg-white">
          <table class="w-full text-sm">
            <thead class="bg-stone-100 text-left text-xs text-stone-600">
              <tr>
                <th class="px-3 py-2">Especie</th>
                <th class="px-3 py-2">Regla de {{ MARK_THRESHOLD }}</th>
                <th class="px-3 py-2 text-right">Preserv.</th>
                <th class="px-3 py-2 text-right">Marcados</th>
                <th class="px-3 py-2 text-right">Recapt.</th>
                <th class="px-3 py-2 text-right">♀</th>
                <th class="px-3 py-2 text-right">♂</th>
                <th class="px-3 py-2 text-right">Total</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="s in shownSpecies" :key="s.key" class="border-t border-stone-100">
                <td class="px-3 py-1.5">
                  <i>{{ s.species }}</i> <span class="text-stone-500">{{ s.subspecies }}</span>
                </td>
                <td class="min-w-48 px-3 py-1.5">
                  <span v-if="!ruleOf(s.species).applies" class="text-xs text-stone-400">no aplica (no es Ithomiini)</span>
                  <div v-else class="flex items-center gap-2">
                    <div class="h-2 w-24 overflow-hidden rounded bg-stone-200">
                      <div
                        class="h-full"
                        :class="ruleOf(s.species).done ? 'bg-brand-600' : 'bg-amber-500'"
                        :style="{ width: `${Math.min(100, (100 * ruleOf(s.species).preserved) / MARK_THRESHOLD)}%` }"
                      />
                    </div>
                    <span class="text-xs whitespace-nowrap" :class="ruleOf(s.species).done ? 'text-brand-700' : 'text-amber-800'">
                      {{ ruleOf(s.species).preserved }}/{{ MARK_THRESHOLD }} ·
                      {{ ruleOf(s.species).done ? 'marcar y liberar' : `preservar (faltan ${ruleOf(s.species).missing})` }}
                    </span>
                  </div>
                </td>
                <td class="px-3 py-1.5 text-right tabular-nums">{{ s.preserved || '' }}</td>
                <td class="px-3 py-1.5 text-right tabular-nums">{{ s.marked || '' }}</td>
                <td class="px-3 py-1.5 text-right tabular-nums">{{ s.recaptured || '' }}</td>
                <td class="px-3 py-1.5 text-right tabular-nums">{{ s.female || '' }}</td>
                <td class="px-3 py-1.5 text-right tabular-nums">{{ s.male || '' }}</td>
                <td class="px-3 py-1.5 text-right font-medium tabular-nums">{{ s.total }}</td>
              </tr>
              <tr v-if="species.length > 10" class="border-t border-stone-100">
                <td colspan="8" class="px-3 py-1.5">
                  <button class="text-xs text-brand-700 underline" @click="allSpecies = !allSpecies">
                    {{ allSpecies ? 'Mostrar solo las 10 más abundantes' : `Ver todas (${species.length} especies)` }}
                  </button>
                </td>
              </tr>
              <tr class="border-t border-stone-300 bg-stone-50 font-medium">
                <td class="px-3 py-1.5" colspan="2">Total</td>
                <td class="px-3 py-1.5 text-right tabular-nums">{{ totals.preserved }}</td>
                <td class="px-3 py-1.5 text-right tabular-nums">{{ totals.marked }}</td>
                <td class="px-3 py-1.5 text-right tabular-nums">{{ totals.recaptured }}</td>
                <td class="px-3 py-1.5 text-right tabular-nums">{{ species.reduce((n, s) => n + s.female, 0) }}</td>
                <td class="px-3 py-1.5 text-right tabular-nums">{{ species.reduce((n, s) => n + s.male, 0) }}</td>
                <td class="px-3 py-1.5 text-right tabular-nums">{{ totals.total }}</td>
              </tr>
            </tbody>
          </table>
        </div>
      </section>

      <div class="grid gap-4 xl:grid-cols-2">
        <section>
          <h2 class="mb-2 font-semibold">Individuos por transecto y mes</h2>
          <div class="max-h-96 overflow-auto rounded-md border border-stone-200 bg-white">
            <table class="w-full text-sm">
              <thead class="sticky top-0 bg-stone-100 text-xs text-stone-600">
                <tr>
                  <th class="px-3 py-2 text-left">Mes</th>
                  <th v-for="t in 4" :key="t" class="px-3 py-2 text-right">T{{ t }}</th>
                  <th class="px-3 py-2 text-right" title="Filas sin Transect_section">Sin T</th>
                  <th class="px-3 py-2 text-right">Total</th>
                </tr>
              </thead>
              <tbody>
                <tr class="border-b border-stone-300 bg-stone-50 font-medium">
                  <td class="px-3 py-1">Total</td>
                  <td v-for="(n, i) in sectionTotals" :key="i" class="px-3 py-1 text-right tabular-nums">{{ n }}</td>
                  <td class="px-3 py-1 text-right tabular-nums">{{ sectionTotals.reduce((a, b) => a + b, 0) }}</td>
                </tr>
                <tr v-for="[month, counts] in perSection" :key="month" class="border-t border-stone-100">
                  <td class="px-3 py-1">{{ monthLabel(month) }}</td>
                  <td
                    v-for="t in 4"
                    :key="t"
                    class="px-3 py-1 text-right tabular-nums"
                    :style="{ background: heat(counts[t], sectionMax).fill, color: heat(counts[t], sectionMax).ink }"
                  >
                    {{ counts[t] || '' }}
                  </td>
                  <td class="px-3 py-1 text-right text-stone-500 tabular-nums">{{ counts[0] || '' }}</td>
                  <td class="px-3 py-1 text-right font-medium tabular-nums">{{ counts.reduce((a, b) => a + b, 0) }}</td>
                </tr>
              </tbody>
            </table>
          </div>
        </section>

        <section>
          <h2 class="mb-1 font-semibold">Recapturas ({{ histories.length }} individuos)</h2>
          <p class="hint mb-2">
            Mediana entre capturas: {{ median(intervals) ?? '—' }} días · máximo
            {{ intervals.length ? Math.max(...intervals) : '—' }}
            días.
          </p>
          <div class="max-h-96 overflow-auto rounded-md border border-stone-200 bg-white">
            <table class="w-full text-sm">
              <thead class="sticky top-0 bg-stone-100 text-left text-xs text-stone-600">
                <tr>
                  <th class="px-3 py-2">Marca</th>
                  <th class="px-3 py-2">Especie</th>
                  <th class="px-3 py-2">Capturas (fecha · recolector · transecto · hora)</th>
                </tr>
              </thead>
              <tbody>
                <tr v-for="h in histories" :key="`${h.id}-${h.species}`" class="border-t border-stone-100 align-top">
                  <td class="px-3 py-1.5 font-medium">{{ h.id }}</td>
                  <td class="px-3 py-1.5">
                    <i>{{ h.species }}</i> <span class="text-stone-500">{{ sexOf(h.events[0].row) }}</span>
                  </td>
                  <td class="px-3 py-1.5">
                    <span v-for="(e, i) in h.events" :key="e.row.id" class="whitespace-nowrap">
                      <template v-if="i">
                        → <span class="text-xs text-stone-500">({{ days(h.events[i - 1].date, e.date) }})</span>
                      </template>
                      {{ date(e.date) }} · {{ e.collector }}<template v-if="e.section"> · T{{ e.section }}</template
                      ><template v-if="e.minutes !== null"> · {{ formatMinutes(e.minutes) }}</template>
                    </span>
                  </td>
                </tr>
              </tbody>
            </table>
          </div>
        </section>
      </div>

      <section
        v-if="conflicts.length || noteOnly.length || withoutPurpose.length"
        class="rounded-md border border-amber-300 bg-amber-50 p-3 text-sm"
      >
        <h2 class="font-semibold text-amber-950">Revisión de datos</h2>
        <details v-if="withoutPurpose.length" class="mt-2">
          <summary class="cursor-pointer">
            {{ withoutPurpose.length }} filas de monitoreo con Purpose vacío o “NA” (de un día registrado como monitoreo en
            SamplingDay_data); se cuentan como monitoreo
          </summary>
          <p class="mt-1 text-xs">
            Filas
            <span v-for="(r, i) in withoutPurpose" :key="r.id"
              >{{ i ? ', ' : '' }}{{ r.row }} ({{ date(r.values.Collection_date as number) }}
              {{ String(r.values.Collector ?? '').split(' - ')[0] }})</span
            >
          </p>
        </details>
        <details v-if="conflicts.length" class="mt-2">
          <summary class="cursor-pointer">
            {{ conflicts.length }} marcas registradas en más de una especie (ID repetida o especie equivocada); no cuentan como
            recaptura
          </summary>
          <ul class="mt-1 space-y-0.5 text-xs">
            <li v-for="c in conflicts" :key="c.id">
              <b>{{ c.id }}</b
              >:
              <span v-for="(r, i) in c.rows" :key="r.id"
                >{{ i ? '; ' : '' }}<i>{{ r.values.SPECIES }}</i> {{ date(r.values.Collection_date as number) }}
                {{ String(r.values.Collector ?? '').split(' - ')[0] }} (fila {{ r.row }})</span
              >
            </li>
          </ul>
        </details>
        <div v-if="noteOnly.length" class="mt-2">
          <p>{{ noteOnly.length }} recapturas escritas solo en las notas de la fila de marcaje (no son filas propias):</p>
          <ul class="mt-1 space-y-0.5 text-xs">
            <li v-for="(n, i) in noteOnly" :key="i">
              <b>{{ n.row.values.FieldMark_ID }}</b> <i>{{ n.row.values.SPECIES }}</i> · {{ n.date }} ·
              {{ formatMinutes(n.minutes) || 'sin hora' }} (fila {{ n.row.row }}): “{{ n.note }}”
            </li>
          </ul>
        </div>
      </section>
    </div>
  </div>
</template>
