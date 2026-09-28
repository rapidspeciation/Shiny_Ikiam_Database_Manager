<script setup lang="ts">
import { computed } from 'vue'
import { RouterLink } from 'vue-router'
import { ArrowRight } from 'lucide-vue-next'
import ChartCard from '../charts/ChartCard.vue'
import EChart from '../charts/EChart.vue'
import { OTHER, SERIES, format } from '../charts/chart'
import { monthly } from '../charts/monthly'
import { dateLabel, monthLabel, pct, table, type Team } from '../../lib/summary'

/** The team's counts (signed-in people only): monitoring, collections, CRISPR and crosses. */
const props = defineProps<{ team: Team }>()
const collectionChart = computed(() => {
  const c = props.team.collections
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
const monitoringChart = computed(() =>
  monthly(props.team.monitoring.months, [{ label: 'Individuos', color: SERIES[0], data: props.team.monitoring.perMonth }], 'individuos'),
)
</script>

<template>
  <div class="space-y-8">
    <!-- ============================================================ monitoring -->
    <section class="space-y-3">
      <div class="flex flex-wrap items-baseline gap-x-3">
        <h2 class="text-lg font-semibold">Monitoreo en Ikiam</h2>
        <p class="text-sm text-stone-500">Transectos T1–T4, desde {{ dateLabel(team.monitoring.first) }}</p>
        <RouterLink to="/monitoreo?vista=resumen" class="ml-auto flex items-center gap-1 text-sm text-brand-700 hover:underline">
          Reporte completo <ArrowRight :size="14" />
        </RouterLink>
      </div>
      <div class="grid grid-cols-2 gap-2 sm:grid-cols-5">
        <div class="stat">
          <p class="stat-label">Individuos</p>
          <p class="stat-value">{{ format(team.monitoring.individuals) }}</p>
        </div>
        <div class="stat">
          <p class="stat-label">Días de monitoreo</p>
          <p class="stat-value">{{ format(team.monitoring.days) }}</p>
        </div>
        <div class="stat">
          <p class="stat-label">Especies</p>
          <p class="stat-value">{{ team.monitoring.species }}</p>
        </div>
        <div class="stat">
          <p class="stat-label">Preservados · marcados</p>
          <p class="stat-value">{{ format(team.monitoring.preserved) }} · {{ format(team.monitoring.marked) }}</p>
        </div>
        <div class="stat">
          <p class="stat-label">Último monitoreo</p>
          <p class="stat-value text-lg">{{ dateLabel(team.monitoring.last) }}</p>
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
              team.monitoring.months.map((m, i) => [monthLabel(m), team.monitoring.perMonth[i], team.monitoring.daysPerMonth[i]]),
            )
          "
        >
          <EChart :option="monitoringChart" :height="220" />
        </ChartCard>
        <ChartCard title="Especies más registradas">
          <p v-for="s in team.monitoring.topSpecies" :key="s.name" class="flex justify-between border-t border-stone-100 py-1 text-sm">
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
          <p class="stat-value">{{ format(team.collections.total) }}</p>
          <p class="stat-note">{{ format(team.collections.last12) }} en los últimos 12 meses</p>
        </div>
        <div class="stat">
          <p class="stat-label">Preservados</p>
          <p class="stat-value">{{ format(team.collections.preserved) }}</p>
        </div>
        <div class="stat">
          <p class="stat-label">Llevados al insectario</p>
          <p class="stat-value">{{ format(team.collections.toInsectary) }}</p>
        </div>
        <div class="stat">
          <p class="stat-label">Especies · géneros</p>
          <p class="stat-value">{{ team.collections.species }} · {{ team.collections.genera }}</p>
        </div>
        <div class="stat">
          <p class="stat-label">Localidades</p>
          <p class="stat-value">{{ team.collections.places }}</p>
        </div>
        <div class="stat">
          <p class="stat-label">Última colecta</p>
          <p class="stat-value text-lg">{{ dateLabel(team.collections.lastDate) }}</p>
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
              team.collections.months.map((m, i) => [
                monthLabel(m),
                team.collections.byMonth.preserved[i],
                team.collections.byMonth.insectary[i],
                team.collections.byMonth.marked[i],
                team.collections.byMonth.released[i],
              ]),
            )
          "
        >
          <EChart :option="collectionChart" :height="220" />
        </ChartCard>
        <ChartCard title="Por año">
          <p v-for="y in team.collections.byYear" :key="y.year" class="flex justify-between border-t border-stone-100 py-1 text-sm">
            <span>{{ y.year }}</span> <span class="tabular-nums">{{ format(y.n) }}</span>
          </p>
        </ChartCard>
        <ChartCard title="Especies más colectadas">
          <p v-for="s in team.collections.topSpecies" :key="s.name" class="flex justify-between border-t border-stone-100 py-1 text-sm">
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
              <tr v-for="p in team.collections.topPlaces" :key="p.name" class="border-t border-stone-100">
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
          {{ dateLabel(team.crispr.first) }} – {{ dateLabel(team.crispr.last) }}
        </p>
      </div>
      <div class="grid grid-cols-2 gap-2 sm:grid-cols-3 lg:grid-cols-6">
        <div class="stat">
          <p class="stat-label">Experimentos</p>
          <p class="stat-value">{{ team.crispr.experiments }}</p>
        </div>
        <div class="stat">
          <p class="stat-label">Huevos inyectados</p>
          <p class="stat-value">{{ format(team.crispr.eggs) }}</p>
        </div>
        <div class="stat">
          <p class="stat-label">Eclosionaron</p>
          <p class="stat-value">{{ format(team.crispr.hatched) }}</p>
          <p class="stat-note">{{ pct(team.crispr.hatched, team.crispr.eggs) }} de los huevos</p>
        </div>
        <div class="stat">
          <p class="stat-label">Pupas</p>
          <p class="stat-value">{{ format(team.crispr.pupae) }}</p>
        </div>
        <div class="stat">
          <p class="stat-label">Adultos</p>
          <p class="stat-value">{{ format(team.crispr.adults) }}</p>
        </div>
        <div class="stat">
          <p class="stat-label">Mutantes</p>
          <p class="stat-value">{{ team.crispr.mutants }}</p>
          <p class="stat-note">de {{ team.crispr.checked }} revisados</p>
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
              <tr v-for="s in team.crispr.stocks" :key="s.name" class="border-t border-stone-100">
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
          <p class="stat-value">{{ format(team.crosses.individuals) }}</p>
          <p class="stat-note">F1/F2_MutationRate</p>
        </div>
        <div class="stat">
          <p class="stat-label">Apareamientos registrados</p>
          <p class="stat-value">{{ team.crosses.matings.total }}</p>
          <p class="stat-note">Stocks_Matings</p>
        </div>
        <div class="stat">
          <p class="stat-label">Parejas de Melinaea</p>
          <p class="stat-value">{{ team.crosses.melinaea.couples }}</p>
          <p class="stat-note">{{ team.crosses.melinaea.mated }} aparearon · {{ team.crosses.melinaea.clutches }} con postura</p>
        </div>
        <div class="stat">
          <p class="stat-label">Posturas de cruces</p>
          <p class="stat-value">{{ team.crosses.clutchesByGeneration.reduce((n, g) => n + g.n, 0) }}</p>
          <p class="stat-note">
            {{ team.crosses.clutchesByGeneration.map(g => `${g.name} ${g.n}`).join(' · ') }}
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
              <tr v-for="l in team.crosses.lines" :key="l.name" class="border-t border-stone-100">
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
                <tr v-for="d in team.crosses.melinaea.directions" :key="d.name" class="border-t border-stone-100">
                  <td class="py-1">{{ d.name }}</td>
                  <td class="px-2 text-right">{{ d.couples }}</td>
                  <td class="px-2 text-right">{{ d.mated || '' }}</td>
                  <td class="pl-2 text-right">{{ d.clutches || '' }}</td>
                </tr>
              </tbody>
            </table>
          </ChartCard>
          <ChartCard title="Apareamientos por especie" subtitle="Stocks_Matings">
            <p v-for="s in team.crosses.matings.bySpecies" :key="s.name" class="flex justify-between border-t border-stone-100 py-1 text-sm">
              <span class="italic">{{ s.name }}</span> <span class="tabular-nums">{{ s.n }}</span>
            </p>
            <p class="mt-2 text-xs text-stone-500">
              Cruces <i>lysimnia</i> × <i>polymnia</i>: {{ team.crosses.lysimniaPolymnia.couples }} parejas en
              {{ team.crosses.lysimniaPolymnia.attempts }} intentos, {{ team.crosses.lysimniaPolymnia.mated }} apareamientos.
            </p>
          </ChartCard>
        </div>
      </div>
    </section>
  </div>
</template>
