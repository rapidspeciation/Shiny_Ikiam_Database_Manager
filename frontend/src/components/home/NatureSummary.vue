<script setup lang="ts">
import { computed } from 'vue'
import ChartCard from '../charts/ChartCard.vue'
import EChart from '../charts/EChart.vue'
import { SERIES, axis, barStyle, base, format } from '../charts/chart'
import { locale, t } from '../../lib/i18n'
import { monthNames, table, type Nature } from '../../lib/summary'

/**
 * What the data says about the butterflies themselves, for anyone: how long
 * each life stage lasts, when and in what weather they fly, where each species
 * is found, and what kills them in the insectary. Only rates and proportions.
 */
const props = defineProps<{ nature: Nature }>()

const hour = (h: number) => `${h}:00`
const MUTED = '#898781'
/** No hover popups on Inicio (they covered the chart): values are written on it, and "Tabla" lists them. */
const still = () => ({ ...base(), tooltip: { show: false } })
/** An axis title under or beside the axis. */
const title = (name: string, gap = 26) => ({
  name,
  nameLocation: 'middle',
  nameGap: gap,
  nameTextStyle: { color: MUTED, fontSize: 11 },
})
/** Labels of a line drawn over bars: on a white chip so they stay readable. */
const chip = { backgroundColor: 'rgba(255,255,255,.9)', padding: [1, 3], borderRadius: 3 }
const valueLabel = (position: string, color = '#52514e') => ({
  show: true,
  position,
  color,
  fontSize: 10,
  formatter: (v: { value: number | null }) => (v.value === null ? '' : format(v.value)),
})

// Spanish labels; t() where shown.
const STAGES = [
  { key: 'egg', label: 'Huevo', color: SERIES[3] },
  { key: 'larva', label: 'Larva', color: SERIES[2] },
  { key: 'pupa', label: 'Pupa', color: SERIES[0] },
] as const
const lifeRows = computed(() => [...props.nature.lifeCycle].reverse())
const lifeOption = computed(() => ({
  ...still(),
  legend: base({ legend: true }).legend,
  grid: { ...base({ legend: true }).grid, bottom: 28, right: 44 },
  xAxis: { ...axis('value'), ...title(t('Número de días')) },
  yAxis: {
    ...axis(
      'category',
      lifeRows.value.map(r => r.name),
    ),
    axisLabel: { color: '#52514e', fontSize: 11, fontStyle: 'italic' },
  },
  series: [
    ...STAGES.map((s, n) => ({
      name: t(s.label),
      type: 'bar',
      stack: 'days',
      barMaxWidth: 18,
      itemStyle: barStyle(s.color, n === STAGES.length - 1, true),
      label: valueLabel('inside', '#ffffff'),
      data: lifeRows.value.map(r => r[s.key]),
    })),
    // The total, written after each bar.
    {
      type: 'bar',
      stack: 'days',
      silent: true,
      itemStyle: { color: 'transparent' },
      label: {
        ...valueLabel('right', '#1c1917'),
        fontWeight: 600,
        formatter: (v: { dataIndex: number }) => `${format(lifeRows.value[v.dataIndex].total)} d`,
      },
      data: lifeRows.value.map(() => 0.01),
    },
  ],
}))

/** Legend on top, then the two axis names: the plot starts lower. */
const twoAxes = () => ({
  ...still(),
  legend: base({ legend: true }).legend,
  grid: { ...base({ legend: true }).grid, top: 58, bottom: 28 },
})

const activityOption = computed(() => {
  const rows = props.nature.activity
  return {
    ...twoAxes(),
    labelLayout: { hideOverlap: true },
    xAxis: {
      ...axis(
        'category',
        rows.map(r => hour(r.hour)),
      ),
      ...title(t('Hora del día')),
    },
    yAxis: [
      { ...axis('value'), name: t('por hora'), nameTextStyle: { color: MUTED, fontSize: 11 } },
      { ...axis('value'), name: t('% capturas'), splitLine: { show: false }, nameTextStyle: { color: MUTED, fontSize: 11 } },
    ],
    series: [
      {
        name: t('Por hora de búsqueda'),
        type: 'bar',
        barMaxWidth: 28,
        itemStyle: barStyle(SERIES[0]),
        label: valueLabel('insideTop', '#ffffff'),
        data: rows.map(r => r.perHour),
      },
      {
        name: t('% de las capturas'),
        type: 'line',
        yAxisIndex: 1,
        symbolSize: 6,
        lineStyle: { color: SERIES[1], width: 2 },
        itemStyle: { color: SERIES[1] },
        label: { ...valueLabel('top', SERIES[1]), ...chip, formatter: (v: { value: number }) => `${format(v.value)} %` },
        data: rows.map(r => r.share),
      },
    ],
  }
})

/** Horizontal bars, one per row, with the value at the end. */
function bars(rows: { label: string; value: number }[], color: string, suffix = '') {
  const list = [...rows].reverse()
  return {
    ...still(),
    grid: { ...base().grid, right: 44 },
    xAxis: { ...axis('value'), axisLabel: { show: false }, splitLine: { show: false } },
    yAxis: {
      ...axis(
        'category',
        list.map(r => r.label),
      ),
      axisLabel: { color: '#52514e', fontSize: 11 },
    },
    series: [
      {
        type: 'bar',
        barMaxWidth: 16,
        itemStyle: barStyle(color, true, true),
        label: { ...valueLabel('right'), fontSize: 11, formatter: (v: { value: number }) => `${format(v.value)}${suffix}` },
        data: list.map(r => r.value),
      },
    ],
  }
}
const cloudsOption = computed(() =>
  bars(
    props.nature.weather.clouds.map(w => ({ label: t(w.label), value: w.perHour })),
    SERIES[0],
  ),
)
const rainOption = computed(() =>
  bars(
    props.nature.weather.rain.map(w => ({ label: t(w.label), value: w.perHour })),
    SERIES[6],
  ),
)
const deathsOption = computed(() =>
  bars(
    props.nature.deaths.map(d => ({ label: t(d.name), value: d.percent })),
    SERIES[7],
    ' %',
  ),
)
const rainSessions = computed(() =>
  props.nature.weather.rain.filter(w => w.code !== 'DY_(dry)').reduce((n, w) => n + w.sessions, 0),
)

const seasonsOption = computed(() => {
  const rows = props.nature.seasons
  return {
    ...twoAxes(),
    labelLayout: { hideOverlap: true },
    xAxis: { ...axis('category', monthNames()), ...title(t('Mes')) },
    yAxis: [
      { ...axis('value'), name: t('por día'), nameTextStyle: { color: MUTED, fontSize: 11 } },
      { ...axis('value'), name: t('especies'), splitLine: { show: false }, nameTextStyle: { color: MUTED, fontSize: 11 } },
    ],
    series: [
      {
        name: t('Mariposas por día'),
        type: 'bar',
        barMaxWidth: 24,
        itemStyle: barStyle(SERIES[0]),
        label: valueLabel('insideTop', '#ffffff'),
        data: rows.map(r => r.perDay),
      },
      {
        name: t('Especies vistas'),
        type: 'line',
        yAxisIndex: 1,
        symbolSize: 6,
        lineStyle: { color: SERIES[2], width: 2 },
        itemStyle: { color: SERIES[2] },
        label: { ...valueLabel('top', SERIES[2]), ...chip },
        data: rows.map(r => r.species),
      },
    ],
  }
})
const best = computed(() => {
  const months = props.nature.seasons.filter(r => r.perDay !== null).sort((a, b) => b.perDay! - a.perDay!)
  // Spanish month names are lowercase in a sentence, English ones keep their capital.
  return months
    .slice(0, 3)
    .map(r => (locale.value === 'es' ? monthNames()[r.month - 1].toLowerCase() : monthNames()[r.month - 1]))
})
const sky = computed(() => {
  const rows = [...props.nature.weather.clouds].sort((a, b) => b.perHour - a.perHour)
  return rows.length >= 2 ? { best: rows[0], worst: rows.at(-1)! } : null
})
/** The most common causes that were actually identified (not "unknown", "disappeared" or "other"; the server's Spanish labels). */
const identified = computed(() =>
  props.nature.deaths
    .filter(d => !/desconocida|desaparecieron|otra causa/i.test(d.name))
    .slice(0, 3)
    .map(d => `${t(d.name).toLowerCase()} (${format(d.percent)} %)`),
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
    <h2 class="text-lg font-semibold">{{ $t('Historia natural') }}</h2>

    <div class="grid gap-4 lg:grid-cols-2">
      <ChartCard
        :title="$t('Ciclo de vida')"
        :subtitle="
          $t(
            'Días que pasa cada especie como huevo, larva y pupa (mediana de los clutches criados en el insectario de Ikiam; las subespecies juntas)',
          )
        "
        :table="
          table(
            [$t('Especie'), $t('Huevo'), $t('Larva'), $t('Pupa'), $t('Total')],
            nature.lifeCycle.map(r => [r.name, r.egg, r.larva, r.pupa, r.total]),
          )
        "
      >
        <EChart :option="lifeOption" :height="80 + 30 * nature.lifeCycle.length" />
      </ChartCard>

      <ChartCard
        :title="$t('¿A qué hora se encuentran más mariposas?')"
        :subtitle="$t('Mariposas por hora de búsqueda en {n} salidas de campo', { n: format(nature.sessions) })"
        :table="
          table(
            [$t('Hora'), $t('Por hora de búsqueda'), $t('% de capturas'), $t('Horas de búsqueda')],
            nature.activity.map(r => [`${r.hour}:00`, r.perHour, r.share, r.effortHours]),
          )
        "
      >
        <EChart :option="activityOption" :height="250" />
        <p class="hint mt-2">
          {{ $t('Contar solo las capturas favorece las horas en que más se sale al campo') }}
          <template v-if="peak">{{ $t('(la mayoría se registra a las {hour})', { hour: peak.busiest }) }}</template
          >.
          {{
            $t(
              'Por eso las barras dividen lo capturado en cada hora entre las horas que alguien estaba buscando: desde el inicio hasta el final del muestreo anotado, o de la primera a la última captura del día.',
            )
          }}
          <template v-if="peak"> {{ $t('Corregido así, la mejor hora es las {hour}.', { hour: peak.richest }) }}</template>
        </p>
      </ChartCard>

      <ChartCard
        :title="$t('¿En qué meses?')"
        :subtitle="$t('Monitoreo mensual en Ikiam (transectos fijos), todos los años juntos')"
        :table="
          table(
            [$t('Mes'), $t('Mariposas por día'), $t('Especies')],
            nature.seasons.map(r => [monthNames()[r.month - 1], r.perDay, r.species]),
          )
        "
      >
        <EChart :option="seasonsOption" :height="250" />
        <p v-if="best.length" class="hint mt-2">
          {{ $t('Los meses con más mariposas por día: {months}.', { months: best.join(', ') }) }}
        </p>
      </ChartCard>

      <ChartCard
        :title="$t('¿Con qué clima?')"
        :subtitle="$t('Mariposas por hora de búsqueda según el cielo y la lluvia registrados ese día')"
      >
        <div class="grid gap-3 sm:grid-cols-2">
          <div>
            <p class="text-xs text-stone-500">{{ $t('Cielo') }}</p>
            <EChart :option="cloudsOption" :height="40 + 34 * nature.weather.clouds.length" />
          </div>
          <div>
            <p class="text-xs text-stone-500">{{ $t('Lluvia') }}</p>
            <EChart :option="rainOption" :height="40 + 34 * nature.weather.rain.length" />
          </div>
        </div>
        <p class="hint mt-2">
          <template v-if="sky">
            {{
              $t('Con «{best}» se encuentran {bestRate} por hora; con «{worst}», {worstRate}.', {
                best: $t(sky.best.label).toLowerCase(),
                bestRate: format(sky.best.perHour),
                worst: $t(sky.worst.label).toLowerCase(),
                worstRate: format(sky.worst.perHour),
              })
            }}
          </template>
          {{
            $t('Pocas salidas se hacen con lluvia ({n} con llovizna o lluvia), así que esa comparación es aproximada.', {
              n: rainSessions,
            })
          }}
        </p>
      </ChartCard>
    </div>

    <ChartCard
      :title="$t('¿Dónde encontrar cada especie?')"
      :subtitle="
        $t(
          'Las Ithomiini registradas con más frecuencia: lugares con más individuos por día de colecta (visitados al menos 3 días), altitud donde aparece el 80 % de los registros y proporción de hembras',
        )
      "
    >
      <div class="overflow-x-auto">
        <table class="w-full text-sm">
          <thead class="text-xs text-stone-500">
            <tr>
              <th class="py-1 text-left">{{ $t('Especie') }}</th>
              <th class="px-2 text-left">{{ $t('Dónde se ven más (por día de colecta)') }}</th>
              <th class="px-2 text-right">{{ $t('Altitud') }}</th>
              <th class="pl-2 text-right" :title="$t('Entre los individuos con sexo registrado')">{{ $t('Hembras') }}</th>
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
        :title="$t('Lugares con más especies de Ithomiini')"
        :subtitle="$t('Cuantos más días se muestrea un lugar, más especies aparecen: compárese con los días de muestreo')"
      >
        <table class="w-full text-sm">
          <thead class="text-xs text-stone-500">
            <tr>
              <th class="py-1 text-left">{{ $t('Lugar') }}</th>
              <th class="px-2 text-right">Ithomiini</th>
              <th class="px-2 text-right">{{ $t('Todas') }}</th>
              <th class="px-2 text-right">{{ $t('Días') }}</th>
              <th class="pl-2 text-right">{{ $t('Altitud') }}</th>
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
        :title="$t('¿De qué mueren en el insectario?')"
        :subtitle="$t('Causas anotadas al registrar cada muerte, en % (no incluye las sacrificadas para muestras)')"
        :table="
          table(
            [$t('Causa'), '%'],
            nature.deaths.map(d => [$t(d.name), d.percent]),
          )
        "
      >
        <EChart :option="deathsOption" :height="40 + 30 * nature.deaths.length" />
        <p class="hint mt-2">
          {{ $t('En {n} % de los casos no se sabe la causa o la mariposa desapareció.', { n: format(Math.round(unexplained)) }) }}
          <template v-if="identified.length">{{
            $t('Entre las causas identificadas, las más comunes: {causes}.', { causes: identified.join(', ') })
          }}</template>
        </p>
      </ChartCard>
    </div>
  </section>
</template>
