<script setup lang="ts">
import { computed } from 'vue'
import ChartCard from '../charts/ChartCard.vue'
import EChart from '../charts/EChart.vue'
import { SERIES, axis, barStyle, base, format, tipRow, tipTitle } from '../charts/chart'
import { MONTHS, table, type Nature } from '../../lib/summary'

/**
 * What the data says about the butterflies themselves, for anyone: how long
 * each life stage lasts, when and in what weather they fly, where each species
 * is found, and what kills them in the insectary. Only rates and proportions.
 */
const props = defineProps<{ nature: Nature }>()

const shadow = { type: 'shadow', shadowStyle: { color: 'rgba(0,0,0,.04)' } }
const hour = (h: number) => `${h}:00`

const STAGES = [
  { key: 'egg', label: 'Huevo', color: SERIES[3] },
  { key: 'larva', label: 'Larva', color: SERIES[2] },
  { key: 'pupa', label: 'Pupa', color: SERIES[0] },
] as const
const lifeRows = computed(() => [...props.nature.lifeCycle].reverse())
const lifeOption = computed(() => ({
  ...base({ legend: true }),
  tooltip: {
    ...base().tooltip,
    trigger: 'axis',
    axisPointer: shadow,
    formatter: (p: { dataIndex: number }[]) => {
      const r = lifeRows.value[p[0].dataIndex]
      return (
        tipTitle(r.name) +
        STAGES.map(s => tipRow(s.color, `${format(r[s.key])} días`, s.label)).join('') +
        `<div style="margin-top:2px">De huevo a adulto: <b>${format(r.total)} días</b></div>`
      )
    },
  },
  xAxis: axis('value'),
  yAxis: { ...axis('category', lifeRows.value.map(r => r.name)), axisLabel: { color: '#52514e', fontSize: 11, fontStyle: 'italic' } },
  series: STAGES.map((s, n) => ({
    name: s.label,
    type: 'bar',
    stack: 'days',
    barMaxWidth: 18,
    itemStyle: barStyle(s.color, n === STAGES.length - 1, true),
    label: { show: true, color: '#ffffff', fontSize: 10, formatter: (v: { value: number }) => format(v.value) },
    data: lifeRows.value.map(r => r[s.key]),
  })),
}))

/** Legend on top, then the two axis names: the plot starts lower. */
const twoAxes = () => ({ ...base({ legend: true }), grid: { ...base({ legend: true }).grid, top: 58 } })

const activityOption = computed(() => {
  const rows = props.nature.activity
  return {
    ...twoAxes(),
    tooltip: {
      ...base().tooltip,
      trigger: 'axis',
      axisPointer: shadow,
      formatter: (p: { dataIndex: number }[]) => {
        const r = rows[p[0].dataIndex]
        return (
          tipTitle(`${hour(r.hour)} – ${hour(r.hour + 1)}`) +
          tipRow(SERIES[0], format(r.perHour), 'mariposas por hora de búsqueda') +
          tipRow(SERIES[1], `${format(r.share)} %`, 'de las capturas (sin corregir)')
        )
      },
    },
    xAxis: axis('category', rows.map(r => hour(r.hour))),
    yAxis: [
      { ...axis('value'), name: 'por hora', nameTextStyle: { color: '#898781', fontSize: 11 } },
      { ...axis('value'), name: '% capturas', splitLine: { show: false }, nameTextStyle: { color: '#898781', fontSize: 11 } },
    ],
    series: [
      {
        name: 'Por hora de búsqueda',
        type: 'bar',
        barMaxWidth: 28,
        itemStyle: barStyle(SERIES[0]),
        data: rows.map(r => r.perHour),
      },
      {
        name: '% de las capturas',
        type: 'line',
        yAxisIndex: 1,
        symbolSize: 6,
        lineStyle: { color: SERIES[1], width: 2 },
        itemStyle: { color: SERIES[1] },
        data: rows.map(r => r.share),
      },
    ],
  }
})

/** Horizontal bars, one per row, with the value at the end. */
function bars(rows: { label: string; value: number }[], unit: string, color: string) {
  const list = [...rows].reverse()
  return {
    ...base(),
    grid: { ...base().grid, right: 40 },
    tooltip: {
      ...base().tooltip,
      trigger: 'axis',
      axisPointer: shadow,
      formatter: (p: { dataIndex: number }[]) =>
        tipTitle(list[p[0].dataIndex].label) + tipRow(color, format(list[p[0].dataIndex].value), unit),
    },
    xAxis: { ...axis('value'), axisLabel: { show: false }, splitLine: { show: false } },
    yAxis: { ...axis('category', list.map(r => r.label)), axisLabel: { color: '#52514e', fontSize: 11 } },
    series: [
      {
        type: 'bar',
        barMaxWidth: 16,
        itemStyle: barStyle(color, true, true),
        label: { show: true, position: 'right', color: '#52514e', fontSize: 11, formatter: (v: { value: number }) => format(v.value) },
        data: list.map(r => r.value),
      },
    ],
  }
}
const cloudsOption = computed(() =>
  bars(
    props.nature.weather.clouds.map(w => ({ label: w.label, value: w.perHour })),
    'mariposas por hora de búsqueda',
    SERIES[0],
  ),
)
const rainOption = computed(() =>
  bars(
    props.nature.weather.rain.map(w => ({ label: w.label, value: w.perHour })),
    'mariposas por hora de búsqueda',
    SERIES[6],
  ),
)
const deathsOption = computed(() =>
  bars(
    props.nature.deaths.map(d => ({ label: d.name, value: d.percent })),
    '% de las muertes registradas',
    SERIES[7],
  ),
)
const rainSessions = computed(() =>
  props.nature.weather.rain.filter(w => w.code !== 'DY_(dry)').reduce((n, w) => n + w.sessions, 0),
)

const seasonsOption = computed(() => {
  const rows = props.nature.seasons
  return {
    ...twoAxes(),
    tooltip: {
      ...base().tooltip,
      trigger: 'axis',
      axisPointer: shadow,
      formatter: (p: { dataIndex: number }[]) => {
        const r = rows[p[0].dataIndex]
        return (
          tipTitle(MONTHS[r.month - 1]) +
          tipRow(SERIES[0], format(r.perDay), 'mariposas por día de monitoreo') +
          tipRow(SERIES[2], format(r.species), 'especies vistas')
        )
      },
    },
    xAxis: axis('category', MONTHS),
    yAxis: [
      { ...axis('value'), name: 'por día', nameTextStyle: { color: '#898781', fontSize: 11 } },
      { ...axis('value'), name: 'especies', splitLine: { show: false }, nameTextStyle: { color: '#898781', fontSize: 11 } },
    ],
    series: [
      { name: 'Mariposas por día', type: 'bar', barMaxWidth: 24, itemStyle: barStyle(SERIES[0]), data: rows.map(r => r.perDay) },
      {
        name: 'Especies vistas',
        type: 'line',
        yAxisIndex: 1,
        symbolSize: 6,
        lineStyle: { color: SERIES[2], width: 2 },
        itemStyle: { color: SERIES[2] },
        data: rows.map(r => r.species),
      },
    ],
  }
})
const best = computed(() => {
  const months = props.nature.seasons.filter(r => r.perDay !== null).sort((a, b) => b.perDay! - a.perDay!)
  return months.slice(0, 3).map(r => MONTHS[r.month - 1].toLowerCase())
})
const sky = computed(() => {
  const rows = [...props.nature.weather.clouds].sort((a, b) => b.perHour - a.perHour)
  return rows.length >= 2 ? { best: rows[0], worst: rows.at(-1)! } : null
})
/** The most common causes that were actually identified (not "unknown", "disappeared" or "other"). */
const identified = computed(() =>
  props.nature.deaths
    .filter(d => !/desconocida|desaparecieron|otra causa/i.test(d.name))
    .slice(0, 3)
    .map(d => `${d.name.toLowerCase()} (${format(d.percent)} %)`),
)
const unexplained = computed(() =>
  props.nature.deaths.filter(d => /desconocida|desaparecieron/i.test(d.name)).reduce((n, d) => n + d.percent, 0),
)
const peak = computed(() => {
  const rows = props.nature.activity
  if (!rows.length) return null
  const busiest = [...rows].sort((a, b) => b.share - a.share)[0]
  const richest = [...rows].sort((a, b) => b.perHour - a.perHour)[0]
  return { busiest: hour(busiest.hour), richest: hour(richest.hour) }
})
</script>

<template>
  <section class="space-y-4">
    <h2 class="text-lg font-semibold">Historia natural</h2>

    <div class="grid gap-4 lg:grid-cols-2">
      <ChartCard
        title="Ciclo de vida"
        subtitle="Días que pasa cada especie como huevo, larva y pupa (mediana de las posturas criadas en el insectario de Ikiam)"
        :table="
          table(
            ['Especie', 'Huevo', 'Larva', 'Pupa', 'Total'],
            nature.lifeCycle.map(r => [r.name, r.egg, r.larva, r.pupa, r.total]),
          )
        "
      >
        <EChart :option="lifeOption" :height="60 + 30 * nature.lifeCycle.length" />
      </ChartCard>

      <ChartCard
        title="¿A qué hora se encuentran más mariposas?"
        :subtitle="`Mariposas por hora de búsqueda en ${format(nature.sessions)} salidas de campo`"
        :table="
          table(
            ['Hora', 'Por hora de búsqueda', '% de capturas', 'Horas de búsqueda'],
            nature.activity.map(r => [`${r.hour}:00`, r.perHour, r.share, r.effortHours]),
          )
        "
      >
        <EChart :option="activityOption" :height="230" />
        <p class="hint mt-2">
          Contar solo las capturas favorece las horas en que más se sale al campo
          <template v-if="peak">(la mayoría se registra a las {{ peak.busiest }})</template>. Por eso las barras dividen lo
          capturado en cada hora entre las horas que alguien estaba buscando: desde el inicio hasta el final del muestreo
          anotado, o de la primera a la última captura del día.
          <template v-if="peak"> Corregido así, la mejor hora es las {{ peak.richest }}.</template>
        </p>
      </ChartCard>

      <ChartCard
        title="¿En qué meses?"
        subtitle="Monitoreo mensual en Ikiam (transectos fijos), todos los años juntos"
        :table="
          table(
            ['Mes', 'Mariposas por día', 'Especies'],
            nature.seasons.map(r => [MONTHS[r.month - 1], r.perDay, r.species]),
          )
        "
      >
        <EChart :option="seasonsOption" :height="230" />
        <p v-if="best.length" class="hint mt-2">Los meses con más mariposas por día: {{ best.join(', ') }}.</p>
      </ChartCard>

      <ChartCard title="¿Con qué clima?" subtitle="Mariposas por hora de búsqueda según el cielo y la lluvia registrados ese día">
        <div class="grid gap-3 sm:grid-cols-2">
          <div>
            <p class="text-xs text-stone-500">Cielo</p>
            <EChart :option="cloudsOption" :height="40 + 34 * nature.weather.clouds.length" />
          </div>
          <div>
            <p class="text-xs text-stone-500">Lluvia</p>
            <EChart :option="rainOption" :height="40 + 34 * nature.weather.rain.length" />
          </div>
        </div>
        <p class="hint mt-2">
          <template v-if="sky"
            >Con «{{ sky.best.label.toLowerCase() }}» se encuentran {{ format(sky.best.perHour) }} por hora; con «{{
              sky.worst.label.toLowerCase()
            }}», {{ format(sky.worst.perHour) }}.
          </template>
          Pocas salidas se hacen con lluvia ({{ rainSessions }} con llovizna o lluvia), así que esa comparación es aproximada.
        </p>
      </ChartCard>
    </div>

    <ChartCard
      title="¿Dónde encontrar cada especie?"
      subtitle="Las Ithomiini registradas con más frecuencia: lugares con más individuos por día de colecta (visitados al menos 3 días), altitud donde aparece el 80 % de los registros y proporción de hembras"
    >
      <div class="overflow-x-auto">
        <table class="w-full text-sm">
          <thead class="text-xs text-stone-500">
            <tr>
              <th class="py-1 text-left">Especie</th>
              <th class="px-2 text-left">Dónde se ven más (por día de colecta)</th>
              <th class="px-2 text-right">Altitud</th>
              <th class="pl-2 text-right" title="Entre los individuos con sexo registrado">Hembras</th>
            </tr>
          </thead>
          <tbody>
            <tr v-for="s in nature.species" :key="s.name" class="border-t border-stone-100 align-top">
              <td class="py-1.5 italic">{{ s.name }}</td>
              <td class="px-2 py-1.5">
                <span v-for="(site, i) in s.sites" :key="site.name" class="whitespace-nowrap"
                  >{{ i ? ' · ' : '' }}{{ site.name }} <span class="text-xs text-stone-500">{{ format(site.perDay) }}</span></span
                >
                <span v-if="!s.sites.length" class="text-stone-400">—</span>
              </td>
              <td class="px-2 py-1.5 text-right whitespace-nowrap tabular-nums">
                {{ s.elevation ? `${format(s.elevation[0])}–${format(s.elevation[1])} m` : '—' }}
              </td>
              <td class="py-1.5 pl-2 text-right tabular-nums">{{ s.female === null ? '—' : `${s.female} %` }}</td>
            </tr>
          </tbody>
        </table>
      </div>
    </ChartCard>

    <div class="grid gap-4 lg:grid-cols-2">
      <ChartCard
        title="Lugares con más especies de Ithomiini"
        subtitle="Cuantos más días se muestrea un lugar, más especies aparecen: compárese con los días de muestreo"
      >
        <table class="w-full text-sm">
          <thead class="text-xs text-stone-500">
            <tr>
              <th class="py-1 text-left">Lugar</th>
              <th class="px-2 text-right">Ithomiini</th>
              <th class="px-2 text-right">Todas</th>
              <th class="px-2 text-right">Días</th>
              <th class="pl-2 text-right">Altitud</th>
            </tr>
          </thead>
          <tbody class="tabular-nums">
            <tr v-for="s in nature.sites" :key="s.name" class="border-t border-stone-100">
              <td class="py-1">{{ s.name }}</td>
              <td class="px-2 text-right font-medium">{{ s.ithomiini }}</td>
              <td class="px-2 text-right">{{ s.species }}</td>
              <td class="px-2 text-right">{{ s.days }}</td>
              <td class="pl-2 text-right text-stone-600">{{ s.elevation === null ? '—' : `${format(s.elevation)} m` }}</td>
            </tr>
          </tbody>
        </table>
      </ChartCard>

      <ChartCard
        title="¿De qué mueren en el insectario?"
        subtitle="Causas anotadas al registrar cada muerte, en % (no incluye las sacrificadas para muestras)"
        :table="table(['Causa', '%'], nature.deaths.map(d => [d.name, d.percent]))"
      >
        <EChart :option="deathsOption" :height="40 + 30 * nature.deaths.length" />
        <p class="hint mt-2">
          En {{ format(Math.round(unexplained)) }} % de los casos no se sabe la causa o la mariposa desapareció.
          <template v-if="identified.length">Entre las causas identificadas, las más comunes: {{ identified.join(', ') }}.</template>
        </p>
      </ChartCard>
    </div>
  </section>
</template>
