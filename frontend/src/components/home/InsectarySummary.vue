<script setup lang="ts">
import { computed } from 'vue'
import { Lock } from 'lucide-vue-next'
import ChartCard from '../charts/ChartCard.vue'
import EChart from '../charts/EChart.vue'
import { SERIES, format } from '../charts/chart'
import { monthly } from '../charts/monthly'
import { monthLabel, table, type Team } from '../../lib/summary'

/** The insectary right now (signed-in people only): alive, clutches in progress, deaths. */
const props = defineProps<{ team: Team }>()
const deathsChart = computed(() => {
  const i = props.team.insectary
  return monthly(
    i.months,
    [
      { label: 'Preservadas (sacrificadas)', color: SERIES[0], data: i.preservedPerMonth },
      { label: 'Otras muertes', color: SERIES[1], data: i.deathsPerMonth.map((n, k) => n - i.preservedPerMonth[k]) },
    ],
  )
})
</script>

<template>
  <section class="space-y-3">
    <h2 class="flex items-center gap-2 text-lg font-semibold">
      Insectario
      <span class="flex items-center gap-1 rounded bg-stone-200 px-1.5 py-0.5 text-xs font-normal text-stone-600">
        <Lock :size="11" /> solo con cuenta
      </span>
    </h2>
    <div class="grid grid-cols-2 gap-2 sm:grid-cols-3 lg:grid-cols-6">
      <div class="stat" :title="`Sin fecha de muerte y en el insectario desde hace menos de ${team.insectary.aliveDays} días`">
        <p class="stat-label">Mariposas vivas</p>
        <p class="stat-value">{{ format(team.insectary.alive) }}</p>
        <p class="stat-note">
          {{ team.insectary.aliveFemale }} ♀ · {{ team.insectary.aliveMale }} ♂ · {{ team.insectary.aliveWild }} silvestres
        </p>
      </div>
      <div class="stat" :title="`Puestas en los últimos ${team.insectary.clutchDays} días que aún no emergen`">
        <p class="stat-label">Clutches en curso</p>
        <p class="stat-value">{{ format(team.insectary.clutches) }}</p>
      </div>
      <div class="stat">
        <p class="stat-label">Huevos</p>
        <p class="stat-value">{{ format(team.insectary.stages.egg.n) }}</p>
        <p class="stat-note">en {{ team.insectary.stages.egg.clutches }} clutches</p>
      </div>
      <div class="stat">
        <p class="stat-label">Larvas</p>
        <p class="stat-value">{{ format(team.insectary.stages.larva.n) }}</p>
        <p class="stat-note">en {{ team.insectary.stages.larva.clutches }} clutches</p>
      </div>
      <div class="stat">
        <p class="stat-label">Pupas</p>
        <p class="stat-value">{{ format(team.insectary.stages.pupa.n) }}</p>
        <p class="stat-note">en {{ team.insectary.stages.pupa.clutches }} clutches</p>
      </div>
      <div class="stat">
        <p class="stat-label">Últimos 30 días</p>
        <p class="stat-value">{{ team.insectary.arrivals30 }} <span class="text-sm font-normal">ingresos</span></p>
        <p class="stat-note">{{ team.insectary.deaths30 }} muertes</p>
      </div>
    </div>
    <p v-if="team.insectary.stale" class="hint">
      Además hay {{ format(team.insectary.stale) }} mariposas sin fecha de muerte que entraron hace más de
      {{ team.insectary.aliveDays }} días: probablemente murieron sin registrarse (no se cuentan como vivas).
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
              <tr v-for="s in team.insectary.aliveBySpecies" :key="s.name" class="border-t border-stone-100">
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
      <ChartCard title="Clutches en curso por especie" subtitle="Individuos según la etapa más reciente registrada">
        <div class="max-h-80 overflow-auto text-sm">
          <table class="w-full">
            <thead class="sticky top-0 bg-white text-xs text-stone-500">
              <tr>
                <th class="py-1 text-left">Especie</th>
                <th class="px-2 text-right">Clutches</th>
                <th class="px-2 text-right">Huevos</th>
                <th class="px-2 text-right">Larvas</th>
                <th class="pl-2 text-right">Pupas</th>
              </tr>
            </thead>
            <tbody class="tabular-nums">
              <tr v-for="s in team.insectary.clutchesBySpecies" :key="s.name" class="border-t border-stone-100">
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
            team.insectary.months.map((m, i) => [
              monthLabel(m),
              team.insectary.preservedPerMonth[i],
              team.insectary.deathsPerMonth[i] - team.insectary.preservedPerMonth[i],
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
            <p v-for="c in team.insectary.deathCauses30" :key="c.name" class="flex justify-between border-t border-stone-100 py-1">
              <span>{{ c.name }}</span> <span class="tabular-nums">{{ c.n }}</span>
            </p>
          </div>
          <div>
            <p class="mb-1 text-xs text-stone-500">Por especie</p>
            <p v-for="c in team.insectary.deathSpecies30" :key="c.name" class="flex justify-between border-t border-stone-100 py-1">
              <span class="italic">{{ c.name }}</span> <span class="tabular-nums">{{ c.n }}</span>
            </p>
          </div>
        </div>
      </ChartCard>
    </div>
  </section>
</template>
