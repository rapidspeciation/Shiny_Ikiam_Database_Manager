<script setup lang="ts">
import { computed, onMounted, ref, watch } from 'vue'
import { RouterLink } from 'vue-router'
import { ArrowRight, Lock } from 'lucide-vue-next'
import ChartCard from '../components/charts/ChartCard.vue'
import EChart from '../components/charts/EChart.vue'
import { OTHER, SERIES, axis, barStyle, base, format, tipRow, tipTitle } from '../components/charts/chart'
import { api } from '../lib/api'
import { errorText } from '../lib/notice'
import { useSession } from '../stores/session'

/**
 * "Inicio": the project at a glance. Collections, monitoring, CRISPR and
 * crosses are open to anyone; the insectary's state (alive, clutches in
 * progress, deaths) shows only with an account. Everything is computed on the
 * server from the local copy of the workbook (GET /api/summary).
 */
interface Named {
  name: string
  n: number
}
interface Summary {
  today: string
  collections: {
    total: number
    preserved: number
    toInsectary: number
    marked: number
    species: number
    genera: number
    places: number
    lastDate: string | null
    last12: number
    months: string[]
    byMonth: Record<'preserved' | 'insectary' | 'marked' | 'released', number[]>
    byYear: { year: string; n: number }[]
    topSpecies: Named[]
    topPlaces: { name: string; n: number; species: number; last: string | null }[]
  }
  monitoring: {
    individuals: number
    days: number
    species: number
    marked: number
    preserved: number
    first: string | null
    last: string | null
    months: string[]
    perMonth: number[]
    daysPerMonth: number[]
    topSpecies: Named[]
  }
  crispr: {
    experiments: number
    eggs: number
    hatched: number
    pupae: number
    adults: number
    mutants: number
    checked: number
    first: string | null
    last: string | null
    stocks: {
      name: string
      experiments: number
      eggs: number
      hatched: number
      pupae: number
      adults: number
      mutants: number
      checked: number
    }[]
  }
  crosses: {
    lines: { name: string; P: number; F1: number; F2: number; Backcross: number; total: number }[]
    individuals: number
    melinaea: {
      couples: number
      mated: number
      clutches: number
      directions: { name: string; couples: number; mated: number; clutches: number }[]
    }
    matings: { total: number; bySpecies: Named[] }
    lysimniaPolymnia: { couples: number; attempts: number; mated: number }
    clutchesByGeneration: Named[]
  }
  insectary: null | {
    aliveDays: number
    clutchDays: number
    alive: number
    aliveFemale: number
    aliveMale: number
    aliveWild: number
    stale: number
    aliveBySpecies: { name: string; female: number; male: number; other: number; wild: number; reared: number; total: number }[]
    clutches: number
    stages: Record<'egg' | 'larva' | 'pupa', { clutches: number; n: number }>
    clutchesBySpecies: { name: string; clutches: number; egg: number; larva: number; pupa: number }[]
    arrivals30: number
    deaths30: number
    deathCauses30: Named[]
    deathSpecies30: Named[]
    months: string[]
    deathsPerMonth: number[]
    preservedPerMonth: number[]
  }
}

const session = useSession()
const data = ref<Summary | null>(null)
const problem = ref('')
async function load() {
  try {
    data.value = await api<Summary>('summary')
    problem.value = ''
  } catch (e) {
    problem.value = errorText(e)
  }
}
onMounted(load)
watch(() => session.user?.username, load)

const MONTHS = ['Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic']
const monthLabel = (m: string) => `${MONTHS[Number(m.slice(5)) - 1]} ${m.slice(2, 4)}`
const dateLabel = (d: string | null) => (d ? `${Number(d.slice(8))} ${MONTHS[Number(d.slice(5, 7)) - 1]} ${d.slice(0, 4)}` : '—')
const pct = (a: number, b: number) => (b ? `${Math.round((100 * a) / b)} %` : '—')

/** Stacked monthly bars with a hover summary. */
function monthly(months: string[], series: { label: string; color: string; data: number[] }[], unit: string) {
  return {
    ...base({ legend: series.length > 1 }),
    tooltip: {
      ...base().tooltip,
      trigger: 'axis',
      axisPointer: { type: 'shadow', shadowStyle: { color: 'rgba(0,0,0,.04)' } },
      formatter: (p: { dataIndex: number }[]) => {
        const i = p[0].dataIndex
        return (
          tipTitle(monthLabel(months[i])) +
          series
            .filter(s => s.data[i])
            .map(s => tipRow(s.color, format(s.data[i]), series.length > 1 ? s.label : unit))
            .join('')
        )
      },
    },
    xAxis: axis('category', months.map(monthLabel)),
    yAxis: { ...axis('value'), minInterval: 1 },
    series: series.map((s, n) => ({
      name: s.label,
      type: 'bar',
      stack: 'total',
      barMaxWidth: 22,
      itemStyle: barStyle(s.color, n === series.length - 1),
      data: s.data,
    })),
  }
}
const table = (head: string[], rows: (string | number)[][]) => ({ head, rows })

const collectionChart = computed(() => {
  const c = data.value!.collections
  return monthly(
    c.months,
    [
      { label: 'Preservadas', color: SERIES[0], data: c.byMonth.preserved },
      { label: 'Al insectario', color: SERIES[1], data: c.byMonth.insectary },
      { label: 'Marcadas y liberadas', color: SERIES[2], data: c.byMonth.marked },
      { label: 'Liberadas sin marca', color: OTHER, data: c.byMonth.released },
    ].filter(s => s.data.some(Boolean)),
    'individuos',
  )
})
const monitoringChart = computed(() => {
  const m = data.value!.monitoring
  return monthly(m.months, [{ label: 'Individuos', color: SERIES[0], data: m.perMonth }], 'individuos')
})
const deathsChart = computed(() => {
  const i = data.value!.insectary!
  return monthly(
    i.months,
    [
      { label: 'Preservadas (sacrificadas)', color: SERIES[0], data: i.preservedPerMonth },
      { label: 'Otras muertes', color: SERIES[1], data: i.deathsPerMonth.map((n, k) => n - i.preservedPerMonth[k]) },
    ],
    'muertes',
  )
})
</script>

<template>
  <div class="h-full overflow-auto bg-stone-50">
    <div class="mx-auto max-w-7xl space-y-8 p-4 sm:p-6">
      <header class="flex flex-wrap items-end gap-x-4 gap-y-1">
        <div class="min-w-0 flex-1">
          <h1 class="text-xl font-semibold">Mariposas Ithomiini · Ikiam</h1>
          <p class="text-sm text-stone-600">
            Resumen del proyecto a partir del libro de datos (colectas, monitoreo, insectario, CRISPR y cruces).
          </p>
        </div>
        <p v-if="data" class="text-xs text-stone-500">Datos al {{ dateLabel(data.today) }}</p>
      </header>

      <p v-if="problem" class="rounded bg-red-50 px-3 py-2 text-sm text-red-800">{{ problem }}</p>
      <p v-else-if="!data" class="text-stone-500">Cargando resúmenes…</p>

      <template v-if="data">
        <!-- ============================================================ insectary (team only) -->
        <section v-if="data.insectary" class="space-y-3">
          <h2 class="flex items-center gap-2 text-lg font-semibold">
            Insectario
            <span class="flex items-center gap-1 rounded bg-stone-200 px-1.5 py-0.5 text-xs font-normal text-stone-600">
              <Lock :size="11" /> solo con cuenta
            </span>
          </h2>
          <div class="grid grid-cols-2 gap-2 sm:grid-cols-3 lg:grid-cols-6">
            <div class="stat" :title="`Sin fecha de muerte y en el insectario desde hace menos de ${data.insectary.aliveDays} días`">
              <p class="stat-label">Mariposas vivas</p>
              <p class="stat-value">{{ format(data.insectary.alive) }}</p>
              <p class="stat-note">
                {{ data.insectary.aliveFemale }} ♀ · {{ data.insectary.aliveMale }} ♂ · {{ data.insectary.aliveWild }} silvestres
              </p>
            </div>
            <div class="stat" :title="`Puestas en los últimos ${data.insectary.clutchDays} días que aún no emergen`">
              <p class="stat-label">Posturas en curso</p>
              <p class="stat-value">{{ format(data.insectary.clutches) }}</p>
            </div>
            <div class="stat">
              <p class="stat-label">Huevos</p>
              <p class="stat-value">{{ format(data.insectary.stages.egg.n) }}</p>
              <p class="stat-note">en {{ data.insectary.stages.egg.clutches }} posturas</p>
            </div>
            <div class="stat">
              <p class="stat-label">Larvas</p>
              <p class="stat-value">{{ format(data.insectary.stages.larva.n) }}</p>
              <p class="stat-note">en {{ data.insectary.stages.larva.clutches }} posturas</p>
            </div>
            <div class="stat">
              <p class="stat-label">Pupas</p>
              <p class="stat-value">{{ format(data.insectary.stages.pupa.n) }}</p>
              <p class="stat-note">en {{ data.insectary.stages.pupa.clutches }} posturas</p>
            </div>
            <div class="stat">
              <p class="stat-label">Últimos 30 días</p>
              <p class="stat-value">{{ data.insectary.arrivals30 }} <span class="text-sm font-normal">ingresos</span></p>
              <p class="stat-note">{{ data.insectary.deaths30 }} muertes</p>
            </div>
          </div>
          <p v-if="data.insectary.stale" class="hint">
            Además hay {{ format(data.insectary.stale) }} mariposas sin fecha de muerte que entraron hace más de
            {{ data.insectary.aliveDays }} días: probablemente murieron sin registrarse (no se cuentan como vivas).
          </p>
          <div class="grid gap-4 lg:grid-cols-2">
            <ChartCard title="Vivas por especie" subtitle="Sexo y origen">
              <div class="max-h-80 overflow-auto text-sm">
                <table class="w-full">
                  <thead class="sticky top-0 bg-white text-xs text-stone-500">
                    <tr>
                      <th class="py-1 text-left">Especie</th>
                      <th class="px-2 text-right">♀</th>
                      <th class="px-2 text-right">♂</th>
                      <th class="px-2 text-right" title="Sin sexo registrado">?</th>
                      <th class="px-2 text-right">Silv.</th>
                      <th class="px-2 text-right">Criadas</th>
                      <th class="pl-2 text-right">Total</th>
                    </tr>
                  </thead>
                  <tbody class="tabular-nums">
                    <tr v-for="s in data.insectary.aliveBySpecies" :key="s.name" class="border-t border-stone-100">
                      <td class="py-1 italic">{{ s.name }}</td>
                      <td class="px-2 text-right">{{ s.female || '' }}</td>
                      <td class="px-2 text-right">{{ s.male || '' }}</td>
                      <td class="px-2 text-right text-stone-500">{{ s.other || '' }}</td>
                      <td class="px-2 text-right">{{ s.wild || '' }}</td>
                      <td class="px-2 text-right">{{ s.reared || '' }}</td>
                      <td class="pl-2 text-right font-medium">{{ s.total }}</td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </ChartCard>
            <ChartCard title="Posturas en curso por especie" subtitle="Individuos según la etapa más reciente registrada">
              <div class="max-h-80 overflow-auto text-sm">
                <table class="w-full">
                  <thead class="sticky top-0 bg-white text-xs text-stone-500">
                    <tr>
                      <th class="py-1 text-left">Especie</th>
                      <th class="px-2 text-right">Posturas</th>
                      <th class="px-2 text-right">Huevos</th>
                      <th class="px-2 text-right">Larvas</th>
                      <th class="pl-2 text-right">Pupas</th>
                    </tr>
                  </thead>
                  <tbody class="tabular-nums">
                    <tr v-for="s in data.insectary.clutchesBySpecies" :key="s.name" class="border-t border-stone-100">
                      <td class="py-1 italic">{{ s.name }}</td>
                      <td class="px-2 text-right font-medium">{{ s.clutches }}</td>
                      <td class="px-2 text-right">{{ s.egg || '' }}</td>
                      <td class="px-2 text-right">{{ s.larva || '' }}</td>
                      <td class="pl-2 text-right">{{ s.pupa || '' }}</td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </ChartCard>
            <ChartCard
              title="Muertes por mes"
              subtitle="Últimos 12 meses, en Insectary_data"
              :table="
                table(
                  ['Mes', 'Preservadas', 'Otras'],
                  data.insectary.months.map((m, i) => [
                    monthLabel(m),
                    data!.insectary!.preservedPerMonth[i],
                    data!.insectary!.deathsPerMonth[i] - data!.insectary!.preservedPerMonth[i],
                  ]),
                )
              "
            >
              <EChart :option="deathsChart" :height="220" />
            </ChartCard>
            <ChartCard title="Muertes de los últimos 30 días">
              <div class="grid gap-4 text-sm sm:grid-cols-2">
                <div>
                  <p class="mb-1 text-xs text-stone-500">Por causa</p>
                  <p v-for="c in data.insectary.deathCauses30" :key="c.name" class="flex justify-between border-t border-stone-100 py-1">
                    <span>{{ c.name }}</span> <span class="tabular-nums">{{ c.n }}</span>
                  </p>
                </div>
                <div>
                  <p class="mb-1 text-xs text-stone-500">Por especie</p>
                  <p v-for="c in data.insectary.deathSpecies30" :key="c.name" class="flex justify-between border-t border-stone-100 py-1">
                    <span class="italic">{{ c.name }}</span> <span class="tabular-nums">{{ c.n }}</span>
                  </p>
                </div>
              </div>
            </ChartCard>
          </div>
        </section>
        <p v-else class="flex items-center gap-2 rounded-md border border-stone-200 bg-white px-3 py-2 text-sm text-stone-600">
          <Lock :size="14" />
          <span
            >El estado del insectario (mariposas vivas, posturas en curso, muertes) se ve al
            <RouterLink :to="{ path: '/entrar', query: { volver: '/inicio' } }" class="text-brand-700 underline"
              >iniciar sesión</RouterLink
            >.</span
          >
        </p>

        <!-- ============================================================ monitoring -->
        <section class="space-y-3">
          <div class="flex flex-wrap items-baseline gap-x-3">
            <h2 class="text-lg font-semibold">Monitoreo en Ikiam</h2>
            <p class="text-sm text-stone-500">Transectos T1–T4, desde {{ dateLabel(data.monitoring.first) }}</p>
            <RouterLink to="/monitoreo?vista=resumen" class="ml-auto flex items-center gap-1 text-sm text-brand-700 hover:underline">
              Reporte completo <ArrowRight :size="14" />
            </RouterLink>
          </div>
          <div class="grid grid-cols-2 gap-2 sm:grid-cols-5">
            <div class="stat">
              <p class="stat-label">Individuos</p>
              <p class="stat-value">{{ format(data.monitoring.individuals) }}</p>
            </div>
            <div class="stat">
              <p class="stat-label">Días de monitoreo</p>
              <p class="stat-value">{{ format(data.monitoring.days) }}</p>
            </div>
            <div class="stat">
              <p class="stat-label">Especies</p>
              <p class="stat-value">{{ data.monitoring.species }}</p>
            </div>
            <div class="stat">
              <p class="stat-label">Preservados · marcados</p>
              <p class="stat-value">{{ format(data.monitoring.preserved) }} · {{ format(data.monitoring.marked) }}</p>
            </div>
            <div class="stat">
              <p class="stat-label">Último monitoreo</p>
              <p class="stat-value text-lg">{{ dateLabel(data.monitoring.last) }}</p>
            </div>
          </div>
          <div class="grid gap-4 lg:grid-cols-3">
            <ChartCard
              class="lg:col-span-2"
              title="Individuos por mes"
              subtitle="Últimos 12 meses"
              :table="
                table(
                  ['Mes', 'Individuos', 'Días'],
                  data.monitoring.months.map((m, i) => [monthLabel(m), data!.monitoring.perMonth[i], data!.monitoring.daysPerMonth[i]]),
                )
              "
            >
              <EChart :option="monitoringChart" :height="220" />
            </ChartCard>
            <ChartCard title="Especies más registradas">
              <p v-for="s in data.monitoring.topSpecies" :key="s.name" class="flex justify-between border-t border-stone-100 py-1 text-sm">
                <span class="italic">{{ s.name }}</span> <span class="tabular-nums">{{ s.n }}</span>
              </p>
            </ChartCard>
          </div>
        </section>

        <!-- ============================================================ collections -->
        <section class="space-y-3">
          <h2 class="text-lg font-semibold">Colectas</h2>
          <div class="grid grid-cols-2 gap-2 sm:grid-cols-3 lg:grid-cols-6">
            <div class="stat">
              <p class="stat-label">Individuos registrados</p>
              <p class="stat-value">{{ format(data.collections.total) }}</p>
              <p class="stat-note">{{ format(data.collections.last12) }} en los últimos 12 meses</p>
            </div>
            <div class="stat">
              <p class="stat-label">Preservados</p>
              <p class="stat-value">{{ format(data.collections.preserved) }}</p>
            </div>
            <div class="stat">
              <p class="stat-label">Llevados al insectario</p>
              <p class="stat-value">{{ format(data.collections.toInsectary) }}</p>
            </div>
            <div class="stat">
              <p class="stat-label">Especies · géneros</p>
              <p class="stat-value">{{ data.collections.species }} · {{ data.collections.genera }}</p>
            </div>
            <div class="stat">
              <p class="stat-label">Localidades</p>
              <p class="stat-value">{{ data.collections.places }}</p>
            </div>
            <div class="stat">
              <p class="stat-label">Última colecta</p>
              <p class="stat-value text-lg">{{ dateLabel(data.collections.lastDate) }}</p>
            </div>
          </div>
          <div class="grid gap-4 lg:grid-cols-3">
            <ChartCard
              class="lg:col-span-2"
              title="Individuos por mes y destino"
              subtitle="Últimos 12 meses (incluye el monitoreo)"
              :table="
                table(
                  ['Mes', 'Preservadas', 'Insectario', 'Marcadas', 'Sin marca'],
                  data.collections.months.map((m, i) => [
                    monthLabel(m),
                    data!.collections.byMonth.preserved[i],
                    data!.collections.byMonth.insectary[i],
                    data!.collections.byMonth.marked[i],
                    data!.collections.byMonth.released[i],
                  ]),
                )
              "
            >
              <EChart :option="collectionChart" :height="220" />
            </ChartCard>
            <ChartCard title="Por año">
              <p v-for="y in data.collections.byYear" :key="y.year" class="flex justify-between border-t border-stone-100 py-1 text-sm">
                <span>{{ y.year }}</span> <span class="tabular-nums">{{ format(y.n) }}</span>
              </p>
            </ChartCard>
            <ChartCard title="Especies más colectadas">
              <p v-for="s in data.collections.topSpecies" :key="s.name" class="flex justify-between border-t border-stone-100 py-1 text-sm">
                <span class="italic">{{ s.name }}</span> <span class="tabular-nums">{{ format(s.n) }}</span>
              </p>
            </ChartCard>
            <ChartCard class="lg:col-span-2" title="Localidades con más registros">
              <table class="w-full text-sm">
                <thead class="text-xs text-stone-500">
                  <tr>
                    <th class="py-1 text-left">Localidad</th>
                    <th class="px-2 text-right">Individuos</th>
                    <th class="px-2 text-right">Especies</th>
                    <th class="pl-2 text-right">Última visita</th>
                  </tr>
                </thead>
                <tbody class="tabular-nums">
                  <tr v-for="p in data.collections.topPlaces" :key="p.name" class="border-t border-stone-100">
                    <td class="py-1">{{ p.name }}</td>
                    <td class="px-2 text-right">{{ format(p.n) }}</td>
                    <td class="px-2 text-right">{{ p.species }}</td>
                    <td class="pl-2 text-right text-stone-600">{{ dateLabel(p.last) }}</td>
                  </tr>
                </tbody>
              </table>
            </ChartCard>
          </div>
        </section>

        <!-- ============================================================ CRISPR -->
        <section class="space-y-3">
          <div class="flex flex-wrap items-baseline gap-x-3">
            <h2 class="text-lg font-semibold">CRISPR</h2>
            <p class="text-sm text-stone-500">
              {{ dateLabel(data.crispr.first) }} – {{ dateLabel(data.crispr.last) }}
            </p>
          </div>
          <div class="grid grid-cols-2 gap-2 sm:grid-cols-3 lg:grid-cols-6">
            <div class="stat">
              <p class="stat-label">Experimentos</p>
              <p class="stat-value">{{ data.crispr.experiments }}</p>
            </div>
            <div class="stat">
              <p class="stat-label">Huevos inyectados</p>
              <p class="stat-value">{{ format(data.crispr.eggs) }}</p>
            </div>
            <div class="stat">
              <p class="stat-label">Eclosionaron</p>
              <p class="stat-value">{{ format(data.crispr.hatched) }}</p>
              <p class="stat-note">{{ pct(data.crispr.hatched, data.crispr.eggs) }} de los huevos</p>
            </div>
            <div class="stat">
              <p class="stat-label">Pupas</p>
              <p class="stat-value">{{ format(data.crispr.pupae) }}</p>
            </div>
            <div class="stat">
              <p class="stat-label">Adultos</p>
              <p class="stat-value">{{ format(data.crispr.adults) }}</p>
            </div>
            <div class="stat">
              <p class="stat-label">Mutantes</p>
              <p class="stat-value">{{ data.crispr.mutants }}</p>
              <p class="stat-note">de {{ data.crispr.checked }} revisados</p>
            </div>
          </div>
          <ChartCard title="Por stock de origen">
            <div class="overflow-x-auto">
              <table class="w-full text-sm">
                <thead class="text-xs text-stone-500">
                  <tr>
                    <th class="py-1 text-left">Stock</th>
                    <th class="px-2 text-right">Experimentos</th>
                    <th class="px-2 text-right">Huevos</th>
                    <th class="px-2 text-right">Eclosión</th>
                    <th class="px-2 text-right">Pupas</th>
                    <th class="px-2 text-right">Adultos</th>
                    <th class="pl-2 text-right">Mutantes / revisados</th>
                  </tr>
                </thead>
                <tbody class="tabular-nums">
                  <tr v-for="s in data.crispr.stocks" :key="s.name" class="border-t border-stone-100">
                    <td class="py-1 italic">{{ s.name }}</td>
                    <td class="px-2 text-right">{{ s.experiments }}</td>
                    <td class="px-2 text-right">{{ format(s.eggs) }}</td>
                    <td class="px-2 text-right">{{ s.hatched }} <span class="text-xs text-stone-500">({{ pct(s.hatched, s.eggs) }})</span></td>
                    <td class="px-2 text-right">{{ s.pupae }}</td>
                    <td class="px-2 text-right">{{ s.adults }}</td>
                    <td class="pl-2 text-right">{{ s.mutants }} / {{ s.checked }}</td>
                  </tr>
                </tbody>
              </table>
            </div>
          </ChartCard>
        </section>

        <!-- ============================================================ crosses -->
        <section class="space-y-3">
          <h2 class="text-lg font-semibold">Cruces</h2>
          <div class="grid grid-cols-2 gap-2 sm:grid-cols-4">
            <div class="stat">
              <p class="stat-label">Individuos en líneas de cruce</p>
              <p class="stat-value">{{ format(data.crosses.individuals) }}</p>
              <p class="stat-note">F1/F2_MutationRate</p>
            </div>
            <div class="stat">
              <p class="stat-label">Apareamientos registrados</p>
              <p class="stat-value">{{ data.crosses.matings.total }}</p>
              <p class="stat-note">Stocks_Matings</p>
            </div>
            <div class="stat">
              <p class="stat-label">Parejas de Melinaea</p>
              <p class="stat-value">{{ data.crosses.melinaea.couples }}</p>
              <p class="stat-note">{{ data.crosses.melinaea.mated }} aparearon · {{ data.crosses.melinaea.clutches }} con postura</p>
            </div>
            <div class="stat">
              <p class="stat-label">Posturas de cruces</p>
              <p class="stat-value">{{ data.crosses.clutchesByGeneration.reduce((n, g) => n + g.n, 0) }}</p>
              <p class="stat-note">
                {{ data.crosses.clutchesByGeneration.map(g => `${g.name} ${g.n}`).join(' · ') }}
              </p>
            </div>
          </div>
          <div class="grid gap-4 lg:grid-cols-2">
            <ChartCard title="Individuos por cruce y generación">
              <table class="w-full text-sm">
                <thead class="text-xs text-stone-500">
                  <tr>
                    <th class="py-1 text-left">Cruce</th>
                    <th class="px-2 text-right">P</th>
                    <th class="px-2 text-right">F1</th>
                    <th class="px-2 text-right">F2</th>
                    <th class="px-2 text-right">Retrocruce</th>
                    <th class="pl-2 text-right">Total</th>
                  </tr>
                </thead>
                <tbody class="tabular-nums">
                  <tr v-for="l in data.crosses.lines" :key="l.name" class="border-t border-stone-100">
                    <td class="py-1">{{ l.name }}</td>
                    <td class="px-2 text-right">{{ l.P || '' }}</td>
                    <td class="px-2 text-right">{{ l.F1 || '' }}</td>
                    <td class="px-2 text-right">{{ l.F2 || '' }}</td>
                    <td class="px-2 text-right">{{ l.Backcross || '' }}</td>
                    <td class="pl-2 text-right font-medium">{{ format(l.total) }}</td>
                  </tr>
                </tbody>
              </table>
            </ChartCard>
            <div class="space-y-4">
              <ChartCard title="Parejas de Melinaea por dirección de cruce">
                <table class="w-full text-sm">
                  <thead class="text-xs text-stone-500">
                    <tr>
                      <th class="py-1 text-left">Dirección</th>
                      <th class="px-2 text-right">Parejas</th>
                      <th class="px-2 text-right">Aparearon</th>
                      <th class="pl-2 text-right">Con postura</th>
                    </tr>
                  </thead>
                  <tbody class="tabular-nums">
                    <tr v-for="d in data.crosses.melinaea.directions" :key="d.name" class="border-t border-stone-100">
                      <td class="py-1">{{ d.name }}</td>
                      <td class="px-2 text-right">{{ d.couples }}</td>
                      <td class="px-2 text-right">{{ d.mated || '' }}</td>
                      <td class="pl-2 text-right">{{ d.clutches || '' }}</td>
                    </tr>
                  </tbody>
                </table>
              </ChartCard>
              <ChartCard title="Apareamientos por especie" subtitle="Stocks_Matings">
                <p v-for="s in data.crosses.matings.bySpecies" :key="s.name" class="flex justify-between border-t border-stone-100 py-1 text-sm">
                  <span class="italic">{{ s.name }}</span> <span class="tabular-nums">{{ s.n }}</span>
                </p>
                <p class="mt-2 text-xs text-stone-500">
                  Cruces <i>lysimnia</i> × <i>polymnia</i>: {{ data.crosses.lysimniaPolymnia.couples }} parejas en
                  {{ data.crosses.lysimniaPolymnia.attempts }} intentos, {{ data.crosses.lysimniaPolymnia.mated }} apareamientos.
                </p>
              </ChartCard>
            </div>
          </div>
        </section>
      </template>
    </div>
  </div>
</template>

