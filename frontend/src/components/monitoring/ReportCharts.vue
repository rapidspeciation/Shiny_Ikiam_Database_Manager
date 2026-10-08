<script setup lang="ts">
import { computed } from 'vue'
import ChartCard from '../charts/ChartCard.vue'
import EChart from '../charts/EChart.vue'
import { BLUES, OTHER, SERIES, axis, barStyle, base, format, tipRow, tipTitle } from '../charts/chart'
import type { StoredTrack } from '../../composables/useMonitoring'
import { formatSerial } from '../../lib/dates'
import { t } from '../../lib/i18n'
import { monthLabel, monthNames } from '../../lib/summary'
import {
  CLOUD_CLASSES,
  HEIGHT_CLASSES,
  byCloud,
  byHeight,
  byTime,
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
// Spanish labels; t() where shown. The option computeds call t(), so they are rebuilt when the language changes.
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
        t('{n} individuos · {days} días de monitoreo', {
          n: `<b>${totals.value[i]}</b>`,
          days: daysByMonth.value.get(months.value[i]) || 0,
        }) +
        KINDS.filter(k => kinds.value[k.key][i])
          .map(k => tipRow(k.color, format(kinds.value[k.key][i]), t(k.label)))
          .join('')
      )
    },
  },
  xAxis: axis('category', months.value.map(monthLabel)),
  yAxis: axis('value'),
  series: KINDS.map((k, n) => ({
    name: t(k.label),
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
        t('{rate} por día · {n} en {days} días', {
          rate: `<b>${format(perDay.value[i])}</b>`,
          n: totals.value[i],
          days: daysByMonth.value.get(m) || 0,
        })
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
              ? tipRow(colorOfCollector(ini, c), String(effortByCollector.value[c][i]), t('días {ini}', { ini }))
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
    const values = byYear.get(m.slice(0, 4)) || Array.from({ length: 12 }, () => 0)
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
      tipTitle(monthNames()[p[0].dataIndex]) +
      yearly.value
        .filter(y => y.values[p[0].dataIndex] !== null)
        .map(y => tipRow(yearColor(y.year), format(y.values[p[0].dataIndex]), y.year))
        .join(''),
  },
  xAxis: { ...axis('category', monthNames()), boundaryGap: false },
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
/** Rows without a species count as "Sin especie" (lib/monitoring.ts); shown in the interface language. */
const spName = (s: string) => (s === 'Sin especie' ? t('Sin especie') : s)
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
        tipTitle(spName(s.species)) +
        t('{n} individuos', { n: `<b>${s.total}</b>` }) +
        tipRow(SERIES[0], String(s.preserved), t('preservados')) +
        tipRow(SERIES[1], String(s.marked), t('marcados')) +
        tipRow(SERIES[2], String(s.recaptured), t('recapturas'))
      )
    },
  },
  xAxis: axis('value'),
  yAxis: {
    ...axis(
      'category',
      top.value.map(s => spName(s.species)),
    ),
    axisLabel: { color: '#52514e', fontSize: 11, fontStyle: 'italic' },
  },
  series: (['preserved', 'marked', 'recaptured'] as const).map((key, n) => ({
    name: [t('Preservados'), t('Marcados'), t('Recapturas')][n],
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
      return (
        tipTitle(t('Día {n} · {date}', { n: a.day, date: formatSerial(a.date) })) +
        t('{n} especies acumuladas', { n: `<b>${a.species}</b>` })
      )
    },
  },
  xAxis: {
    ...axis(
      'category',
      accumulation.value.map(a => String(a.day)),
    ),
    boundaryGap: false,
    name: t('días de monitoreo'),
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
        tipTitle(`${spName(seasonSpecies.value[s])} · ${monthNames()[m]}`) +
        t('{rate} por día · {n} en {days} días', {
          rate: `<b>${format(season.value.values[s][m])}</b>`,
          n: season.value.counts[s][m],
          days,
        })
      )
    },
  },
  xAxis: { ...axis('category', monthNames()), splitArea: { show: false } },
  yAxis: {
    ...axis('category', seasonSpecies.value.map(spName)),
    axisLabel: { color: '#52514e', fontSize: 11, fontStyle: 'italic' },
  },
  visualMap: {
    min: 0,
    max: seasonMax.value,
    calculable: false,
    orient: 'horizontal',
    left: 'center',
    bottom: 0,
    itemHeight: 120,
    itemWidth: 10,
    text: [t('más'), t('menos')],
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
  const names = [...sectionSpecies.value.map(spName), t('Otras')]
  const counts = [...bySection.value.bySpecies, bySection.value.other]
  return {
    ...base({ legend: true }),
    legend: { ...base({ legend: true }).legend, textStyle: { color: '#52514e', fontSize: 11, fontStyle: 'italic' } },
    tooltip: {
      ...base().tooltip,
      trigger: 'axis',
      axisPointer: { type: 'shadow', shadowStyle: { color: 'rgba(0,0,0,.04)' } },
      formatter: (p: { dataIndex: number }[]) => {
        const k = 3 - p[0].dataIndex
        return (
          tipTitle(`T${k + 1} · ${t('{n} individuos', { n: sectionTotals.value[k] })}`) +
          names.map((n, i) => (counts[i][k] ? tipRow(i < 6 ? SERIES[i] : OTHER, `${share(counts[i][k], k)} %`, n) : '')).join('')
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
        tipTitle(spName(s.species)) +
        tipRow('#e34948', String(s.female), t('hembras ({pct} %)', { pct })) +
        tipRow(SERIES[0], String(s.male), t('machos'))
      )
    },
  },
  xAxis: { ...axis('value'), axisLabel: { color: '#898781', fontSize: 11, formatter: (v: number) => String(Math.abs(v)) } },
  yAxis: {
    ...axis(
      'category',
      sexSpecies.value.map(s => spName(s.species)),
    ),
    axisLabel: { color: '#52514e', fontSize: 11, fontStyle: 'italic' },
  },
  series: [
    {
      name: t('Hembras'),
      type: 'bar',
      stack: 'sex',
      barMaxWidth: 14,
      itemStyle: { color: '#e34948', borderRadius: [4, 0, 0, 4] },
      data: sexSpecies.value.map(s => -s.female),
    },
    {
      name: t('Machos'),
      type: 'bar',
      stack: 'sex',
      barMaxWidth: 14,
      itemStyle: { color: SERIES[0], borderRadius: [0, 4, 4, 0] },
      data: sexSpecies.value.map(s => s.male),
    },
  ],
}))

// ------------------------------------------------------ behaviour & weather
/** Categories and unit are shown as given (translate them before). */
const simpleBars = (categories: string[], values: number[], unit = t('individuos')) => ({
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
/** Capture time in 10-minute bins around the usual walk (9:00–11:00), with the captures earlier and later at the ends. */
const times = computed(() => byTime(props.rows))
const timeLabels = computed(() => ['< 8:30', ...times.value.labels, '≥ 11:30'])
const timeValues = computed(() => [times.value.before, ...times.value.bins, times.value.after])
const timeOption = computed(() => {
  const last = timeLabels.value.length - 1
  return {
    ...base(),
    grid: { ...base().grid, top: 24 },
    tooltip: {
      ...base().tooltip,
      trigger: 'axis',
      axisPointer: { type: 'shadow', shadowStyle: { color: 'rgba(0,0,0,.04)' } },
      formatter: (p: { dataIndex: number }[]) => {
        const i = p[0].dataIndex
        const label = i === 0 || i === last ? timeLabels.value[i] : `${timeLabels.value[i]}–${timeLabels.value[i + 1].replace('≥ ', '')}`
        return tipTitle(label) + `<b>${timeValues.value[i]}</b> ${t('individuos')}`
      },
    },
    xAxis: axis('category', timeLabels.value),
    yAxis: { ...axis('value'), minInterval: 1 },
    series: [
      {
        type: 'bar',
        barMaxWidth: 24,
        data: timeValues.value.map((v, i) => ({ value: v, itemStyle: barStyle(i === 0 || i === last ? OTHER : SERIES[0]) })),
        // The usual walk, shaded.
        markArea: {
          silent: true,
          itemStyle: { color: 'rgba(42,120,214,.06)' },
          label: { show: true, position: 'top', color: '#898781', fontSize: 11, formatter: t('horario habitual') },
          data: [[{ xAxis: '9:00' }, { xAxis: '10:50' }]],
        },
      },
    ],
  }
})
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
    t.captures.filter(c => !c.doubt).map(c => ({ markId: c.markId, species: c.species, date: t.date, lat: c.lat, lon: c.lon })),
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
      <h2 class="font-semibold">{{ $t('Abundancia y esfuerzo') }}</h2>
      <div class="grid gap-4 xl:grid-cols-2">
        <ChartCard
          :title="$t('Individuos por mes')"
          :subtitle="zoom ? $t('Arrastra la barra inferior (o Shift + rueda) para acercar un periodo') : undefined"
          :table="
            table(
              [$t('Mes'), ...KINDS.map(k => $t(k.label)), $t('Total'), $t('Días')],
              months.map((m, i) => [monthLabel(m), ...KINDS.map(k => kinds[k.key][i]), totals[i], daysByMonth.get(m) || 0]),
            )
          "
        >
          <EChart :option="monthlyOption" :height="260" />
        </ChartCard>
        <ChartCard
          :title="$t('Individuos por día de monitoreo')"
          :subtitle="$t('Captura por esfuerzo: individuos del mes ÷ días de monitoreo')"
          :table="
            table(
              [$t('Mes'), $t('Individuos'), $t('Días'), $t('Por día')],
              months.map((m, i) => [monthLabel(m), totals[i], daysByMonth.get(m) || 0, format(perDay[i])]),
            )
          "
        >
          <EChart :option="perDayOption" :height="260" />
        </ChartCard>
        <ChartCard
          :title="$t('Días de monitoreo por recolector')"
          :subtitle="$t('Quién monitoreó y cuándo (SamplingDay_data y días con capturas)')"
          :table="
            table(
              [$t('Mes'), ...collectorsInEffort],
              months.map((m, i) => [monthLabel(m), ...effortByCollector.map(c => c[i])]),
            )
          "
        >
          <EChart :option="effortOption" :height="240" />
        </ChartCard>
        <ChartCard
          :title="$t('Comparación entre años')"
          :subtitle="$t('Individuos por mes; los meses sin monitoreo quedan vacíos')"
          :table="
            table(
              [$t('Mes'), ...yearly.map(y => y.year)],
              monthNames().map((m, i) => [m, ...yearly.map(y => (y.values[i] === null ? '—' : y.values[i]!))]),
            )
          "
        >
          <EChart :option="yearlyOption" :height="240" />
        </ChartCard>
      </div>
    </section>

    <section class="space-y-3">
      <h2 class="font-semibold">{{ $t('Especies') }}</h2>
      <div class="grid gap-4 xl:grid-cols-2">
        <ChartCard
          :title="$t('Especies más abundantes')"
          :subtitle="$t('{n} de {total} especies', { n: Math.min(15, stats.length), total: stats.length })"
          :table="
            table(
              [$t('Especie'), $t('Preservados'), $t('Marcados'), $t('Recapturas'), $t('Total')],
              stats.map(s => [spName(s.species), s.preserved, s.marked, s.recaptured, s.total]),
            )
          "
        >
          <EChart :option="topOption" :height="Math.max(200, 34 + top.length * 22)" />
        </ChartCard>
        <ChartCard
          :title="$t('Curva de acumulación de especies')"
          :subtitle="
            $t(
              'Especies nuevas por día de monitoreo; si se aplana, el muestreo está completo. {once} especies vistas una sola vez, {twice} dos veces.',
              { once: rare.once, twice: rare.twice },
            )
          "
          :table="
            table(
              [$t('Día'), $t('Fecha'), $t('Especies')],
              accumulation.map(a => [a.day, formatSerial(a.date), a.species]),
            )
          "
        >
          <EChart :option="accumulationOption" :height="Math.max(200, 34 + top.length * 22)" />
        </ChartCard>
        <ChartCard
          :title="$t('Estacionalidad')"
          :subtitle="$t('Individuos por día de monitoreo en cada mes del año (todos los años juntos), especies más abundantes')"
          :table="
            table(
              [$t('Especie'), ...monthNames()],
              seasonSpecies.map((s, i) => [spName(s), ...season.values[i].map(v => format(v))]),
            )
          "
        >
          <EChart :option="seasonOption" :height="Math.max(240, 60 + seasonSpecies.length * 22)" />
        </ChartCard>
        <ChartCard
          :title="$t('Composición por transecto')"
          :subtitle="$t('Proporción de individuos de cada especie en cada sección del sendero')"
          :table="
            table(
              [$t('Especie'), 'T1', 'T2', 'T3', 'T4'],
              [...sectionSpecies.map((s, i) => [spName(s), ...bySection.bySpecies[i]]), [$t('Otras'), ...bySection.other]],
            )
          "
        >
          <EChart :option="sectionOption" :height="240" />
        </ChartCard>
        <ChartCard
          :title="$t('Proporción de sexos')"
          :subtitle="$t('Especies con al menos 5 individuos sexados: hembras a la izquierda, machos a la derecha')"
          :table="
            table(
              [$t('Especie'), $t('Hembras'), $t('Machos')],
              sexSpecies
                .slice()
                .reverse()
                .map(s => [spName(s.species), s.female, s.male]),
            )
          "
        >
          <EChart :option="sexOption" :height="Math.max(200, 34 + sexSpecies.length * 22)" />
        </ChartCard>
      </div>
    </section>

    <section class="space-y-3">
      <h2 class="font-semibold">{{ $t('Comportamiento y clima') }}</h2>
      <div class="grid gap-4 lg:grid-cols-2">
        <ChartCard
          class="lg:col-span-2"
          :title="$t('Hora de captura')"
          :subtitle="
            $t(
              'Cada 10 minutos alrededor del monitoreo habitual (9:00–11:00, sombreado); en gris, las capturas antes de 8:30 ({before}) y desde 11:30 ({after}). {none} sin hora.',
              { before: times.before, after: times.after, none: times.none },
            )
          "
          :table="
            table(
              [$t('Hora'), $t('Individuos')],
              [...timeLabels.map((h, i) => [h, timeValues[i]]), [$t('Sin hora'), times.none]],
            )
          "
        >
          <EChart :option="timeOption" :height="200" />
        </ChartCard>
        <ChartCard
          :title="$t('Altura de vuelo (m)')"
          :table="
            table(
              [$t('Altura'), $t('Individuos')],
              HEIGHT_CLASSES.map((h, i) => [$t(h), heights[i]]),
            )
          "
        >
          <EChart
            :option="
              simpleBars(
                HEIGHT_CLASSES.map(h => $t(h)),
                heights,
              )
            "
            :height="180"
          />
        </ChartCard>
        <ChartCard
          :title="$t('Nubosidad')"
          :table="
            table(
              [$t('Nubosidad'), $t('Individuos')],
              CLOUD_CLASSES.map((c, i) => [$t(c[1]), clouds[i]]),
            )
          "
        >
          <EChart
            :option="
              simpleBars(
                CLOUD_CLASSES.map(c => $t(c[1])),
                clouds,
              )
            "
            :height="180"
          />
        </ChartCard>
      </div>
    </section>

    <section class="space-y-3">
      <h2 class="font-semibold">{{ $t('Marcaje y recaptura') }}</h2>
      <div class="grid gap-4 lg:grid-cols-2">
        <ChartCard
          :title="$t('Tiempo entre capturas (días)')"
          :subtitle="$t('{n} recapturas del mismo individuo', { n: intervals.length })"
          :table="
            table(
              [$t('Días'), $t('Recapturas')],
              INTERVALS.map((d, i) => [d, intervalBins[i]]),
            )
          "
        >
          <EChart :option="simpleBars(INTERVALS, intervalBins, $t('recapturas'))" :height="180" />
        </ChartCard>
        <ChartCard
          :title="$t('Distancia entre capturas')"
          :subtitle="
            moves.length
              ? $t('{n} recapturas con GPS en ambas capturas (recorridos del mapa)', { n: moves.length })
              : $t('Aún no hay recapturas con GPS en ambas capturas: aparecen al pasar recorridos al mapa')
          "
          :table="
            table(
              [$t('Marca'), $t('Especie'), $t('Desde'), $t('Hasta'), $t('Metros')],
              moves.map(m => [m.id, m.species, m.from, m.to, m.metres]),
            )
          "
        >
          <EChart v-if="moves.length" :option="simpleBars(MOVES, moveBins, $t('recapturas'))" :height="180" />
          <p v-else class="py-10 text-center text-xs text-stone-500">{{ $t('Sin datos todavía.') }}</p>
        </ChartCard>
      </div>
    </section>
  </div>
</template>
