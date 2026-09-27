<script setup lang="ts">
import { computed, ref } from 'vue'
import { Download, ListPlus, SlidersHorizontal } from 'lucide-vue-next'
import { useRoute, useRouter } from 'vue-router'
import BarList from '../charts/BarList.vue'
import ChartCard from '../charts/ChartCard.vue'
import ColumnChart from '../charts/ColumnChart.vue'
import LineChart from '../charts/LineChart.vue'
import { OTHER, SERIES, format, heat } from '../charts/chart'
import { useMonitoring } from '../../composables/useMonitoring'
import { formatSerial, isoToSerial, serialToIso, todayIso } from '../../lib/dates'
import {
  CLOUD_CLASSES,
  HEIGHT_CLASSES,
  MARK_THRESHOLD,
  byCloud,
  byHeight,
  byHour,
  effortDays,
  formatMinutes,
  kindsByMonth,
  markConflicts,
  markHistories,
  median,
  monthOf,
  monthRange,
  nextMarkId,
  noteRecaptureValues,
  noteRecaptures,
  preservedForRule,
  recaptureIds,
  sectionsByMonth,
  speciesStats,
} from '../../lib/monitoring'
import { notify } from '../../lib/notice'
import type { TableRow } from '../../lib/types'
import { usePending } from '../../stores/pending'
import { useTables } from '../../stores/tables'

/**
 * "Reporte": a live monitoring report. The filters live in the link (so a
 * filtered view can be shared) and scope every number, chart and table below.
 */
const { table, rows: allRows, isIthomiini, options, createFormulas } = useMonitoring()
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

// ---- charts
const MONTHS = ['Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic']
const monthLabel = (m: string) => `${MONTHS[Number(m.slice(5)) - 1]} ${m.slice(2, 4)}`
const months = computed(() => {
  const seen = filtered.value.map(monthOf).filter(Boolean).sort() as string[]
  const start = from.value || seen[0]
  const end = to.value || seen.at(-1)
  return start && end && start <= end ? monthRange(start, end) : []
})
const daysByMonth = computed(() => {
  const out = new Map<string, number>()
  for (const key of effort.value) out.set(key.slice(0, 7), (out.get(key.slice(0, 7)) || 0) + 1)
  return out
})
const KINDS = [
  { key: 'preserved', label: 'Preservados', color: SERIES[0] },
  { key: 'marked', label: 'Marcados (nuevos)', color: SERIES[1] },
  { key: 'recaptured', label: 'Recapturas', color: SERIES[2] },
  { key: 'other', label: 'Otros', color: OTHER },
] as const
const monthly = computed(() => {
  const counts = kindsByMonth(filtered.value, months.value, recaptures.value)
  return KINDS.map(k => ({ key: k.key, label: k.label, color: k.color, values: counts[k.key] }))
})
const monthlyTotal = computed(() => months.value.map((_, i) => monthly.value.reduce((n, s) => n + s.values[i], 0)))
const perDaySeries = computed(() => [
  {
    key: 'perday',
    label: 'individuos por día',
    color: SERIES[0],
    values: months.value.map((m, i) => {
      const d = daysByMonth.value.get(m) || 0
      return d ? Math.round((monthlyTotal.value[i] / d) * 10) / 10 : 0
    }),
  },
])

/** One line per year; a year keeps its colour whatever the filters (colour follows the year). */
const yearColor = (year: string) => SERIES[(Number(year) - 2023 + SERIES.length * 10) % SERIES.length]
const yearly = computed(() => {
  const byYear = new Map<string, number[]>()
  for (const r of filtered.value) {
    const m = monthOf(r)
    if (!m) continue
    const values = byYear.get(m.slice(0, 4)) || MONTHS.map(() => 0)
    values[Number(m.slice(5)) - 1]++
    byYear.set(m.slice(0, 4), values)
  }
  return [...byYear.entries()]
    .sort((a, b) => a[0].localeCompare(b[0]))
    .map(([year, values]) => ({
      key: year,
      label: year,
      color: yearColor(year),
      // Months without monitoring are gaps, not zeros.
      values: values.map((v, i) =>
        [...daysByMonth.value.keys()].includes(`${year}-${String(i + 1).padStart(2, '0')}`) || v ? v : NaN,
      ),
    }))
})

const topSpecies = computed(() =>
  speciesStats(filtered.value, false, recaptures.value)
    .slice(0, 15)
    .map(s => ({
      label: s.species,
      value: s.total,
      italic: true,
      detail: `${s.preserved} preservados, ${s.marked} marcados, ${s.recaptured} recapturas`,
    })),
)
const HOURS = Array.from({ length: 9 }, (_, i) => `${i + 7}h`)
const hours = computed(() => [{ key: 'h', label: 'individuos', color: SERIES[0], values: byHour(filtered.value) }])
const heights = computed(() => [{ key: 'a', label: 'individuos', color: SERIES[0], values: byHeight(filtered.value) }])
const clouds = computed(() => [{ key: 'c', label: 'individuos', color: SERIES[0], values: byCloud(filtered.value) }])

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
function createNoteRows() {
  const names = options.value.Collector || []
  for (const n of noteOnly.value) {
    const values = noteRecaptureValues(n, names)
    for (const field of createFormulas.value) delete values[field]
    pending.addCreate('Collection_data', `${n.row.values.FieldMark_ID} ${n.date}`, values)
  }
  pending.touch()
  notify('Filas de recaptura añadidas: revísalas en "Importar recorrido" y pulsa Guardar.', 'success')
  router.replace({ query: { vista: 'importar' } })
}
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

      <section v-if="conflicts.length || noteOnly.length" class="rounded-md border border-amber-300 bg-amber-50 p-3 text-sm">
        <h2 class="font-semibold text-amber-950">Revisión de datos</h2>
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
          <p>{{ noteOnly.length }} recapturas escritas solo en las notas de la fila de marcaje:</p>
          <ul class="mt-1 space-y-0.5 text-xs">
            <li v-for="(n, i) in noteOnly" :key="i">
              <b>{{ n.row.values.FieldMark_ID }}</b> <i>{{ n.row.values.SPECIES }}</i> · {{ n.date }} ·
              {{ formatMinutes(n.minutes) || 'sin hora' }} (fila {{ n.row.row }}): “{{ n.note }}”
            </li>
          </ul>
          <button class="btn mt-2" @click="createNoteRows">
            <ListPlus :size="15" /> Crear {{ noteOnly.length }} filas de recaptura para revisar
          </button>
        </div>
      </section>

      <div class="grid gap-4 xl:grid-cols-2">
        <ChartCard
          title="Individuos por mes"
          :subtitle="`${months.length} meses · pasa el cursor para ver los días de monitoreo`"
          :legend="KINDS.map(k => ({ label: k.label, color: k.color }))"
        >
          <ColumnChart
            :categories="months.map(monthLabel)"
            :series="monthly"
            unit="individuos"
            :note="i => `${daysByMonth.get(months[i]) || 0} días de monitoreo`"
          />
          <template #table>
            <table class="w-full">
              <thead class="sticky top-0 bg-white text-stone-500">
                <tr>
                  <th class="py-1 text-left">Mes</th>
                  <th v-for="k in KINDS" :key="k.key" class="py-1 text-right">{{ k.label }}</th>
                  <th class="py-1 text-right">Total</th>
                  <th class="py-1 text-right">Días</th>
                </tr>
              </thead>
              <tbody class="tabular-nums">
                <tr v-for="(m, i) in months" :key="m" class="border-t border-stone-100">
                  <td class="py-0.5">{{ monthLabel(m) }}</td>
                  <td v-for="s in monthly" :key="s.key" class="text-right">{{ s.values[i] || '' }}</td>
                  <td class="text-right font-medium">{{ monthlyTotal[i] || '' }}</td>
                  <td class="text-right">{{ daysByMonth.get(m) || '' }}</td>
                </tr>
              </tbody>
            </table>
          </template>
        </ChartCard>

        <ChartCard
          title="Individuos por día de monitoreo"
          subtitle="Captura por esfuerzo: individuos del mes ÷ días de monitoreo"
        >
          <ColumnChart
            :categories="months.map(monthLabel)"
            :series="perDaySeries"
            unit="individuos por día"
            :note="i => `${monthlyTotal[i]} individuos en ${daysByMonth.get(months[i]) || 0} días`"
          />
          <template #table>
            <table class="w-full">
              <thead class="sticky top-0 bg-white text-stone-500">
                <tr>
                  <th class="py-1 text-left">Mes</th>
                  <th class="py-1 text-right">Individuos</th>
                  <th class="py-1 text-right">Días</th>
                  <th class="py-1 text-right">Por día</th>
                </tr>
              </thead>
              <tbody class="tabular-nums">
                <tr v-for="(m, i) in months" :key="m" class="border-t border-stone-100">
                  <td class="py-0.5">{{ monthLabel(m) }}</td>
                  <td class="text-right">{{ monthlyTotal[i] }}</td>
                  <td class="text-right">{{ daysByMonth.get(m) || 0 }}</td>
                  <td class="text-right">{{ perDaySeries[0].values[i] }}</td>
                </tr>
              </tbody>
            </table>
          </template>
        </ChartCard>

        <ChartCard
          title="Comparación entre años"
          subtitle="Individuos por mes; los meses sin monitoreo quedan vacíos"
          :legend="yearly.map(s => ({ label: s.label, color: s.color, line: true }))"
        >
          <LineChart :categories="MONTHS" :series="yearly" />
          <template #table>
            <table class="w-full">
              <thead class="sticky top-0 bg-white text-stone-500">
                <tr>
                  <th class="py-1 text-left">Mes</th>
                  <th v-for="s in yearly" :key="s.key" class="py-1 text-right">{{ s.label }}</th>
                </tr>
              </thead>
              <tbody class="tabular-nums">
                <tr v-for="(m, i) in MONTHS" :key="m" class="border-t border-stone-100">
                  <td class="py-0.5">{{ m }}</td>
                  <td v-for="s in yearly" :key="s.key" class="text-right">
                    {{ Number.isFinite(s.values[i]) ? s.values[i] : '—' }}
                  </td>
                </tr>
              </tbody>
            </table>
          </template>
        </ChartCard>

        <ChartCard
          title="Especies más abundantes"
          :subtitle="`Individuos en el periodo (${topSpecies.length} de ${species.length})`"
        >
          <BarList :rows="topSpecies" />
          <template #table>
            <p class="text-stone-500">La tabla completa está más abajo.</p>
          </template>
        </ChartCard>
      </div>

      <div class="grid gap-4 lg:grid-cols-3">
        <ChartCard title="Hora de captura">
          <ColumnChart :categories="HOURS" :series="hours" :height="150" unit="individuos" />
          <template #table>
            <p v-for="(h, i) in HOURS" :key="h" class="tabular-nums">{{ h }}: {{ hours[0].values[i] }}</p>
          </template>
        </ChartCard>
        <ChartCard title="Altura de vuelo (m)">
          <ColumnChart :categories="HEIGHT_CLASSES" :series="heights" :height="150" unit="individuos" />
          <template #table>
            <p v-for="(h, i) in HEIGHT_CLASSES" :key="h" class="tabular-nums">{{ h }} m: {{ heights[0].values[i] }}</p>
          </template>
        </ChartCard>
        <ChartCard title="Nubosidad">
          <ColumnChart :categories="CLOUD_CLASSES.map(c => c[1])" :series="clouds" :height="150" unit="individuos" />
          <template #table>
            <p v-for="(c, i) in CLOUD_CLASSES" :key="c[0]" class="tabular-nums">{{ c[1] }}: {{ clouds[0].values[i] }}</p>
          </template>
        </ChartCard>
      </div>

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
              <tr v-for="s in species" :key="s.key" class="border-t border-stone-100">
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
    </div>
  </div>
</template>
