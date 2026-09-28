<script setup lang="ts">
import { computed } from 'vue'
import ChartCard from '../charts/ChartCard.vue'
import EChart from '../charts/EChart.vue'
import { BLUES, OTHER, SERIES, axis, barStyle, base, format, tipRow, tipTitle } from '../charts/chart'
import type { StoredTrack } from '../../composables/useMonitoring'
import { formatSerial } from '../../lib/dates'
import {
  CLOUD_CLASSES,
  HEIGHT_CLASSES,
  byCloud,
  byHeight,
  byHour,
  kindsByMonth,
  markHistories,
  monthOf,
  monthRange,
  rareSpecies,
  recaptureDistances,
  seasonality,
  speciesAccumulation,
  speciesBySection,
  speciesStats,
} from '../../lib/monitoring'
import { distance } from '../../lib/transects'
import type { TableRow } from '../../lib/types'

/**
 * The figures of the live report (Apache ECharts). Everything they show comes
 * from the already-filtered rows and monitoring effort passed in.
 */
const props = defineProps<{
  rows: TableRow[]
  /** Monitoring days in the filters, "YYYY-MM-DD|INI". */
  effort: string[]
  recaptures: Set<string>
  from: string
  to: string
  tracks: StoredTrack[]
}>()

const MONTHS = ['Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic']
const monthLabel = (m: string) => `${MONTHS[Number(m.slice(5)) - 1]} ${m.slice(2, 4)}`
const sp = (name: string) => `<i>${name}</i>`

// ------------------------------------------------------------ abundance
const months = computed(() => {
  const seen = props.rows.map(monthOf).filter(Boolean).sort() as string[]
  const start = props.from || seen[0]
  const end = props.to || seen.at(-1)
  return start && end && start <= end ? monthRange(start, end) : []
})
const daysByMonth = computed(() => {
  const out = new Map<string, number>()
  for (const key of props.effort) out.set(key.slice(0, 7), (out.get(key.slice(0, 7)) || 0) + 1)
  return out
})
const KINDS = [
  { key: 'preserved', label: 'Preservados', color: SERIES[0] },
  { key: 'marked', label: 'Marcados (nuevos)', color: SERIES[1] },
  { key: 'recaptured', label: 'Recapturas', color: SERIES[2] },
  { key: 'other', label: 'Otros', color: OTHER },
] as const
const kinds = computed(() => kindsByMonth(props.rows, months.value, props.recaptures))
const totals = computed(() => months.value.map((_, i) => KINDS.reduce((n, k) => n + kinds.value[k.key][i], 0)))
const zoom = computed(() => months.value.length > 24)

const monthlyOption = computed(() => ({
  ...base({ legend: true, zoom: zoom.value }),
  tooltip: {
    ...base().tooltip,
    trigger: 'axis',
    axisPointer: { type: 'shadow', shadowStyle: { color: 'rgba(0,0,0,.04)' } },
    formatter: (p: { dataIndex: number }[]) => {
      const i = p[0].dataIndex
      return (
        tipTitle(monthLabel(months.value[i])) +
        `<b>${totals.value[i]}</b> individuos · ${daysByMonth.value.get(months.value[i]) || 0} días de monitoreo` +
        KINDS.filter(k => kinds.value[k.key][i])
          .map(k => tipRow(k.color, format(kinds.value[k.key][i]), k.label))
          .join('')
      )
    },
  },
  xAxis: axis('category', months.value.map(monthLabel)),
  yAxis: axis('value'),
  series: KINDS.map((k, n) => ({
    name: k.label,
    type: 'bar',
    stack: 'fate',
    barMaxWidth: 24,
    itemStyle: barStyle(k.color, n === KINDS.length - 1),
    emphasis: { focus: 'series' },
    data: kinds.value[k.key],
  })),
}))

const perDay = computed(() =>
  months.value.map((m, i) => {
    const d = daysByMonth.value.get(m) || 0
    return d ? Math.round((totals.value[i] / d) * 10) / 10 : null
  }),
)
const perDayOption = computed(() => ({
  ...base({ zoom: zoom.value }),
  tooltip: {
    ...base().tooltip,
    trigger: 'axis',
    axisPointer: { type: 'shadow', shadowStyle: { color: 'rgba(0,0,0,.04)' } },
    formatter: (p: { dataIndex: number }[]) => {
      const i = p[0].dataIndex
      const m = months.value[i]
      return (
        tipTitle(monthLabel(m)) +
        `<b>${format(perDay.value[i])}</b> por día · ${totals.value[i]} en ${daysByMonth.value.get(m) || 0} días`
      )
    },
  },
  xAxis: axis('category', months.value.map(monthLabel)),
  yAxis: axis('value'),
  series: [{ type: 'bar', barMaxWidth: 24, itemStyle: barStyle(SERIES[0]), data: perDay.value }],
}))

/** Monitoring days per collector and month: who walked, and when. */
const collectorsInEffort = computed(() => [...new Set(props.effort.map(k => k.split('|')[1]))].sort())
const COLLECTOR_COLOR: Record<string, string> = { FCH: SERIES[0], AA: SERIES[1], MJS: SERIES[2] }
const colorOfCollector = (ini: string, i: number) => COLLECTOR_COLOR[ini] || SERIES[(3 + i) % SERIES.length]
const effortByCollector = computed(() =>
  collectorsInEffort.value.map(ini =>
    months.value.map(m => props.effort.filter(k => k.startsWith(m) && k.endsWith(`|${ini}`)).length),
  ),
)
const effortOption = computed(() => ({
  ...base({ legend: true, zoom: zoom.value }),
  tooltip: {
    ...base().tooltip,
    trigger: 'axis',
    axisPointer: { type: 'shadow', shadowStyle: { color: 'rgba(0,0,0,.04)' } },
    formatter: (p: { dataIndex: number }[]) => {
      const i = p[0].dataIndex
      return (
        tipTitle(monthLabel(months.value[i])) +
        collectorsInEffort.value
          .map((ini, c) =>
            effortByCollector.value[c][i]
              ? tipRow(colorOfCollector(ini, c), String(effortByCollector.value[c][i]), `días ${ini}`)
              : '',
          )
          .join('')
      )
    },
  },
  xAxis: axis('category', months.value.map(monthLabel)),
  yAxis: { ...axis('value'), minInterval: 1 },
  series: collectorsInEffort.value.map((ini, c) => ({
    name: ini,
    type: 'bar',
    stack: 'days',
    barMaxWidth: 24,
    itemStyle: barStyle(colorOfCollector(ini, c), c === collectorsInEffort.value.length - 1),
    data: effortByCollector.value[c],
  })),
}))

/** One line per year; a year keeps its colour whatever the filters (colour follows the year). */
const yearColor = (year: string) => SERIES[(((Number(year) - 2023) % SERIES.length) + SERIES.length) % SERIES.length]
const yearly = computed(() => {
  const byYear = new Map<string, (number | null)[]>()
  for (const r of props.rows) {
    const m = monthOf(r)
    if (!m) continue
    const values = byYear.get(m.slice(0, 4)) || MONTHS.map(() => 0)
    values[Number(m.slice(5)) - 1]!++
    byYear.set(m.slice(0, 4), values)
  }
  return [...byYear.entries()]
    .sort((a, b) => a[0].localeCompare(b[0]))
    .map(([year, values]) => ({
      year,
      // Months without monitoring are gaps, not zeros.
      values: values.map((v, i) => (daysByMonth.value.has(`${year}-${String(i + 1).padStart(2, '0')}`) || v ? v : null)),
    }))
})
const yearlyOption = computed(() => ({
  ...base({ legend: true }),
  tooltip: {
    ...base().tooltip,
    trigger: 'axis',
    formatter: (p: { dataIndex: number }[]) =>
      tipTitle(MONTHS[p[0].dataIndex]) +
      yearly.value
        .filter(y => y.values[p[0].dataIndex] !== null)
        .map(y => tipRow(yearColor(y.year), format(y.values[p[0].dataIndex]), y.year))
        .join(''),
  },
  xAxis: { ...axis('category', MONTHS), boundaryGap: false },
  yAxis: axis('value'),
  series: yearly.value.map(y => ({
    name: y.year,
    type: 'line',
    data: y.values,
    connectNulls: false,
    symbol: 'circle',
    symbolSize: 7,
    showSymbol: false,
    lineStyle: { width: 2, color: yearColor(y.year) },
    itemStyle: { color: yearColor(y.year), borderColor: '#fff', borderWidth: 2 },
    emphasis: { focus: 'series' },
  })),
}))

// ------------------------------------------------------------ species
const stats = computed(() => speciesStats(props.rows, false, props.recaptures))
const top = computed(() => stats.value.slice(0, 15).reverse())
const topOption = computed(() => ({
  ...base({ legend: true }),
  tooltip: {
    ...base().tooltip,
    trigger: 'axis',
    axisPointer: { type: 'shadow', shadowStyle: { color: 'rgba(0,0,0,.04)' } },
    formatter: (p: { dataIndex: number }[]) => {
      const s = top.value[p[0].dataIndex]
      return (
        tipTitle(s.species) +
        `<b>${s.total}</b> individuos` +
        tipRow(SERIES[0], String(s.preserved), 'preservados') +
        tipRow(SERIES[1], String(s.marked), 'marcados') +
        tipRow(SERIES[2], String(s.recaptured), 'recapturas')
      )
    },
  },
  xAxis: axis('value'),
  yAxis: {
    ...axis(
      'category',
      top.value.map(s => s.species),
    ),
    axisLabel: { color: '#52514e', fontSize: 11, fontStyle: 'italic' },
  },
  series: (['preserved', 'marked', 'recaptured'] as const).map((key, n) => ({
    name: ['Preservados', 'Marcados', 'Recapturas'][n],
    type: 'bar',
    stack: 'fate',
    barMaxWidth: 14,
    itemStyle: barStyle(SERIES[n], n === 2, true),
    data: top.value.map(s => s[key]),
  })),
}))

const accumulation = computed(() => speciesAccumulation(props.rows))
const rare = computed(() => rareSpecies(props.rows))
const accumulationOption = computed(() => ({
  ...base(),
  tooltip: {
    ...base().tooltip,
    trigger: 'axis',
    formatter: (p: { dataIndex: number }[]) => {
      const a = accumulation.value[p[0].dataIndex]
      return tipTitle(`Día ${a.day} · ${formatSerial(a.date)}`) + `<b>${a.species}</b> especies acumuladas`
    },
  },
  xAxis: {
    ...axis(
      'category',
      accumulation.value.map(a => String(a.day)),
    ),
    boundaryGap: false,
    name: 'días de monitoreo',
  },
  yAxis: { ...axis('value'), minInterval: 1 },
  series: [
    {
      type: 'line',
      step: 'end',
      showSymbol: false,
      lineStyle: { width: 2, color: SERIES[0] },
      areaStyle: { color: 'rgba(42,120,214,.10)' },
      data: accumulation.value.map(a => a.species),
    },
  ],
}))

/** Top species × calendar month, individuals per monitoring day (all years pooled). */
const seasonSpecies = computed(() => stats.value.slice(0, 12).map(s => s.species))
const season = computed(() => seasonality(props.rows, props.effort, seasonSpecies.value))
const seasonMax = computed(() => Math.max(0.1, ...season.value.values.flat()))
const seasonOption = computed(() => ({
  ...base(),
  grid: { left: 8, right: 12, top: 8, bottom: 40, containLabel: true },
  tooltip: {
    ...base().tooltip,
    trigger: 'item',
    formatter: (p: { data: [number, number, number] }) => {
      const [m, s] = p.data
      const days = season.value.daysPerMonth[m]
      return (
        tipTitle(`${seasonSpecies.value[s]} · ${MONTHS[m]}`) +
        `<b>${format(season.value.values[s][m])}</b> por día · ${season.value.counts[s][m]} en ${days} días`
      )
    },
  },
  xAxis: { ...axis('category', MONTHS), splitArea: { show: false } },
  yAxis: { ...axis('category', seasonSpecies.value), axisLabel: { color: '#52514e', fontSize: 11, fontStyle: 'italic' } },
  visualMap: {
    min: 0,
    max: seasonMax.value,
    calculable: false,
    orient: 'horizontal',
    left: 'center',
    bottom: 0,
    itemHeight: 120,
    itemWidth: 10,
    text: ['más', 'menos'],
    textStyle: { color: '#898781', fontSize: 11 },
    inRange: { color: BLUES },
  },
  series: [
    {
      type: 'heatmap',
      data: season.value.values.flatMap((row, s) => row.map((v, m) => [m, s, v])),
      itemStyle: { borderColor: '#fff', borderWidth: 2 },
      emphasis: { itemStyle: { borderColor: '#0b0b0b', borderWidth: 1 } },
    },
  ],
}))

const sectionSpecies = computed(() => stats.value.slice(0, 6).map(s => s.species))
const bySection = computed(() => speciesBySection(props.rows, sectionSpecies.value))
const sectionTotals = computed(() =>
  [0, 1, 2, 3].map(t => bySection.value.bySpecies.reduce((n, s) => n + s[t], 0) + bySection.value.other[t]),
)
const sectionOption = computed(() => {
  const share = (n: number, t: number) => (sectionTotals.value[t] ? Math.round((1000 * n) / sectionTotals.value[t]) / 10 : 0)
  const names = [...sectionSpecies.value, 'Otras']
  const counts = [...bySection.value.bySpecies, bySection.value.other]
  return {
    ...base({ legend: true }),
    legend: { ...base({ legend: true }).legend, textStyle: { color: '#52514e', fontSize: 11, fontStyle: 'italic' } },
    tooltip: {
      ...base().tooltip,
      trigger: 'axis',
      axisPointer: { type: 'shadow', shadowStyle: { color: 'rgba(0,0,0,.04)' } },
      formatter: (p: { dataIndex: number }[]) => {
        const t = 3 - p[0].dataIndex
        return (
          tipTitle(`T${t + 1} · ${sectionTotals.value[t]} individuos`) +
          names.map((n, i) => (counts[i][t] ? tipRow(i < 6 ? SERIES[i] : OTHER, `${share(counts[i][t], t)} %`, n) : '')).join('')
        )
      },
    },
    xAxis: { ...axis('value'), max: 100, axisLabel: { color: '#898781', fontSize: 11, formatter: '{value} %' } },
    yAxis: axis('category', ['T4', 'T3', 'T2', 'T1']),
    series: names.map((n, i) => ({
      name: n,
      type: 'bar',
      stack: 'share',
      barMaxWidth: 22,
      itemStyle: barStyle(i < 6 ? SERIES[i] : OTHER, false),
      data: [3, 2, 1, 0].map(t => share(counts[i][t], t)),
    })),
  }
})

/** Sex ratio: females to the left, males to the right. */
const sexSpecies = computed(() =>
  stats.value
    .filter(s => s.female + s.male >= 5)
    .slice(0, 12)
    .reverse(),
)
const sexOption = computed(() => ({
  ...base({ legend: true }),
  tooltip: {
    ...base().tooltip,
    trigger: 'axis',
    axisPointer: { type: 'shadow', shadowStyle: { color: 'rgba(0,0,0,.04)' } },
    formatter: (p: { dataIndex: number }[]) => {
      const s = sexSpecies.value[p[0].dataIndex]
      const pct = Math.round((100 * s.female) / (s.female + s.male))
      return (
        tipTitle(s.species) +
        tipRow('#e34948', String(s.female), `hembras (${pct} %)`) +
        tipRow(SERIES[0], String(s.male), 'machos')
      )
    },
  },
  xAxis: { ...axis('value'), axisLabel: { color: '#898781', fontSize: 11, formatter: (v: number) => String(Math.abs(v)) } },
  yAxis: {
    ...axis(
      'category',
      sexSpecies.value.map(s => s.species),
    ),
    axisLabel: { color: '#52514e', fontSize: 11, fontStyle: 'italic' },
  },
  series: [
    {
      name: 'Hembras',
      type: 'bar',
      stack: 'sex',
      barMaxWidth: 14,
      itemStyle: { color: '#e34948', borderRadius: [4, 0, 0, 4] },
      data: sexSpecies.value.map(s => -s.female),
    },
    {
      name: 'Machos',
      type: 'bar',
      stack: 'sex',
      barMaxWidth: 14,
      itemStyle: { color: SERIES[0], borderRadius: [0, 4, 4, 0] },
      data: sexSpecies.value.map(s => s.male),
    },
  ],
}))

// ------------------------------------------------------ behaviour & weather
const simpleBars = (categories: string[], values: number[], unit = 'individuos') => ({
  ...base(),
  tooltip: {
    ...base().tooltip,
    trigger: 'axis',
    axisPointer: { type: 'shadow', shadowStyle: { color: 'rgba(0,0,0,.04)' } },
    formatter: (p: { dataIndex: number }[]) => tipTitle(categories[p[0].dataIndex]) + `<b>${values[p[0].dataIndex]}</b> ${unit}`,
  },
  xAxis: axis('category', categories),
  yAxis: axis('value'),
  series: [{ type: 'bar', barMaxWidth: 24, itemStyle: barStyle(SERIES[0]), data: values }],
})
const HOURS = Array.from({ length: 9 }, (_, i) => `${i + 7}h`)
const hours = computed(() => byHour(props.rows))
const heights = computed(() => byHeight(props.rows))
const clouds = computed(() => byCloud(props.rows))

// ------------------------------------------------------ marks & recaptures
const histories = computed(() => {
  const inRange = new Set(props.rows.map(r => r.id))
  return markHistories(props.rows).filter(h => h.events.some(e => inRange.has(e.row.id)))
})
const intervals = computed(() =>
  histories.value.flatMap(h =>
    h.events.slice(1).flatMap((e, i) => (e.date !== null && h.events[i].date !== null ? [e.date - h.events[i].date!] : [])),
  ),
)
const INTERVALS = ['≤ 7', '8–30', '31–90', '91–180', '> 180']
const intervalBins = computed(() => {
  const out = [0, 0, 0, 0, 0]
  for (const d of intervals.value) out[d <= 7 ? 0 : d <= 30 ? 1 : d <= 90 ? 2 : d <= 180 ? 3 : 4]++
  return out
})
/** Movement between captures, from the GPS points of the walks on the map. */
const moves = computed(() => {
  const inRange = new Set(props.rows.map(r => `${String(r.values.FieldMark_ID ?? '').toUpperCase()}|${r.values.SPECIES}`))
  const points = props.tracks.flatMap(t =>
    t.captures.map(c => ({ markId: c.markId, species: c.species, date: t.date, lat: c.lat, lon: c.lon })),
  )
  return recaptureDistances(points, distance).filter(m => inRange.has(`${m.id}|${m.species}`))
})
const MOVES = ['< 25 m', '25–100', '100–300', '300–600', '> 600 m']
const moveBins = computed(() => {
  const out = [0, 0, 0, 0, 0]
  for (const m of moves.value) out[m.metres < 25 ? 0 : m.metres < 100 ? 1 : m.metres < 300 ? 2 : m.metres < 600 ? 3 : 4]++
  return out
})

const table = (head: string[], rows: (string | number)[][]) => ({ head, rows })
</script>

<template>
  <div class="space-y-6">
    <section class="space-y-3">
      <h2 class="font-semibold">Abundancia y esfuerzo</h2>
      <div class="grid gap-4 xl:grid-cols-2">
        <ChartCard
          title="Individuos por mes"
          :subtitle="zoom ? 'Arrastra la barra inferior (o Shift + rueda) para acercar un periodo' : undefined"
          :table="
            table(
              ['Mes', ...KINDS.map(k => k.label), 'Total', 'Días'],
              months.map((m, i) => [monthLabel(m), ...KINDS.map(k => kinds[k.key][i]), totals[i], daysByMonth.get(m) || 0]),
            )
          "
        >
          <EChart :option="monthlyOption" :height="260" />
        </ChartCard>
        <ChartCard
          title="Individuos por día de monitoreo"
          subtitle="Captura por esfuerzo: individuos del mes ÷ días de monitoreo"
          :table="
            table(
              ['Mes', 'Individuos', 'Días', 'Por día'],
              months.map((m, i) => [monthLabel(m), totals[i], daysByMonth.get(m) || 0, format(perDay[i])]),
            )
          "
        >
          <EChart :option="perDayOption" :height="260" />
        </ChartCard>
        <ChartCard
          title="Días de monitoreo por recolector"
          subtitle="Quién monitoreó y cuándo (SamplingDay_data y días con capturas)"
          :table="
            table(
              ['Mes', ...collectorsInEffort],
              months.map((m, i) => [monthLabel(m), ...effortByCollector.map(c => c[i])]),
            )
          "
        >
          <EChart :option="effortOption" :height="240" />
        </ChartCard>
        <ChartCard
          title="Comparación entre años"
          subtitle="Individuos por mes; los meses sin monitoreo quedan vacíos"
          :table="
            table(
              ['Mes', ...yearly.map(y => y.year)],
              MONTHS.map((m, i) => [m, ...yearly.map(y => (y.values[i] === null ? '—' : y.values[i]!))]),
            )
          "
        >
          <EChart :option="yearlyOption" :height="240" />
        </ChartCard>
      </div>
    </section>

    <section class="space-y-3">
      <h2 class="font-semibold">Especies</h2>
      <div class="grid gap-4 xl:grid-cols-2">
        <ChartCard
          title="Especies más abundantes"
          :subtitle="`${Math.min(15, stats.length)} de ${stats.length} especies`"
          :table="
            table(
              ['Especie', 'Preservados', 'Marcados', 'Recapturas', 'Total'],
              stats.map(s => [s.species, s.preserved, s.marked, s.recaptured, s.total]),
            )
          "
        >
          <EChart :option="topOption" :height="Math.max(200, 34 + top.length * 22)" />
        </ChartCard>
        <ChartCard
          title="Curva de acumulación de especies"
          :subtitle="`Especies nuevas por día de monitoreo; si se aplana, el muestreo está completo. ${rare.once} especies vistas una sola vez, ${rare.twice} dos veces.`"
          :table="
            table(
              ['Día', 'Fecha', 'Especies'],
              accumulation.map(a => [a.day, formatSerial(a.date), a.species]),
            )
          "
        >
          <EChart :option="accumulationOption" :height="Math.max(200, 34 + top.length * 22)" />
        </ChartCard>
        <ChartCard
          title="Estacionalidad"
          subtitle="Individuos por día de monitoreo en cada mes del año (todos los años juntos), especies más abundantes"
          :table="
            table(
              ['Especie', ...MONTHS],
              seasonSpecies.map((s, i) => [s, ...season.values[i].map(v => format(v))]),
            )
          "
        >
          <EChart :option="seasonOption" :height="Math.max(240, 60 + seasonSpecies.length * 22)" />
        </ChartCard>
        <ChartCard
          title="Composición por transecto"
          subtitle="Proporción de individuos de cada especie en cada sección del sendero"
          :table="
            table(
              ['Especie', 'T1', 'T2', 'T3', 'T4'],
              [...sectionSpecies.map((s, i) => [s, ...bySection.bySpecies[i]]), ['Otras', ...bySection.other]],
            )
          "
        >
          <EChart :option="sectionOption" :height="240" />
        </ChartCard>
        <ChartCard
          title="Proporción de sexos"
          subtitle="Especies con al menos 5 individuos sexados: hembras a la izquierda, machos a la derecha"
          :table="
            table(
              ['Especie', 'Hembras', 'Machos'],
              sexSpecies
                .slice()
                .reverse()
                .map(s => [s.species, s.female, s.male]),
            )
          "
        >
          <EChart :option="sexOption" :height="Math.max(200, 34 + sexSpecies.length * 22)" />
        </ChartCard>
      </div>
    </section>

    <section class="space-y-3">
      <h2 class="font-semibold">Comportamiento y clima</h2>
      <div class="grid gap-4 lg:grid-cols-3">
        <ChartCard
          title="Hora de captura"
          :table="
            table(
              ['Hora', 'Individuos'],
              HOURS.map((h, i) => [h, hours[i]]),
            )
          "
        >
          <EChart :option="simpleBars(HOURS, hours)" :height="180" />
        </ChartCard>
        <ChartCard
          title="Altura de vuelo (m)"
          :table="
            table(
              ['Altura', 'Individuos'],
              HEIGHT_CLASSES.map((h, i) => [h, heights[i]]),
            )
          "
        >
          <EChart :option="simpleBars(HEIGHT_CLASSES, heights)" :height="180" />
        </ChartCard>
        <ChartCard
          title="Nubosidad"
          :table="
            table(
              ['Nubosidad', 'Individuos'],
              CLOUD_CLASSES.map((c, i) => [c[1], clouds[i]]),
            )
          "
        >
          <EChart
            :option="
              simpleBars(
                CLOUD_CLASSES.map(c => c[1]),
                clouds,
              )
            "
            :height="180"
          />
        </ChartCard>
      </div>
    </section>

    <section class="space-y-3">
      <h2 class="font-semibold">Marcaje y recaptura</h2>
      <div class="grid gap-4 lg:grid-cols-2">
        <ChartCard
          title="Tiempo entre capturas (días)"
          :subtitle="`${intervals.length} recapturas del mismo individuo`"
          :table="
            table(
              ['Días', 'Recapturas'],
              INTERVALS.map((d, i) => [d, intervalBins[i]]),
            )
          "
        >
          <EChart :option="simpleBars(INTERVALS, intervalBins, 'recapturas')" :height="180" />
        </ChartCard>
        <!-- Built from the GPS tracks, which visitors without an account do not get. -->
        <ChartCard
          v-if="tracks.length"
          title="Distancia entre capturas"
          :subtitle="
            moves.length
              ? `${moves.length} recapturas con GPS en ambas capturas (recorridos del mapa)`
              : 'Aún no hay recapturas con GPS en ambas capturas: aparecen al pasar recorridos al mapa'
          "
          :table="
            table(
              ['Marca', 'Especie', 'Desde', 'Hasta', 'Metros'],
              moves.map(m => [m.id, m.species, m.from, m.to, m.metres]),
            )
          "
        >
          <EChart v-if="moves.length" :option="simpleBars(MOVES, moveBins, 'recapturas')" :height="180" />
          <p v-else class="py-10 text-center text-xs text-stone-500">Sin datos todavía.</p>
        </ChartCard>
      </div>
    </section>
  </div>
</template>
