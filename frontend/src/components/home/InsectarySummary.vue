<script setup lang="ts">
import { computed } from 'vue'
import { Lock } from 'lucide-vue-next'
import ChartCard from '../charts/ChartCard.vue'
import EChart from '../charts/EChart.vue'
import { SERIES, format } from '../charts/chart'
import { monthly } from '../charts/monthly'
import { t } from '../../lib/i18n'
import { monthLabel, table, type Team } from '../../lib/summary'

/** The insectary right now (signed-in people only): alive, clutches in progress, deaths. */
const props = defineProps<{ team: Team }>()
const deathsChart = computed(() => {
  const i = props.team.insectary
  return monthly(i.months, [
    { label: t('Preservadas (sacrificadas)'), color: SERIES[0], data: i.preservedPerMonth },
    { label: t('Otras muertes'), color: SERIES[1], data: i.deathsPerMonth.map((n, k) => n - i.preservedPerMonth[k]) },
  ])
})
</script>

<template>
  <section class="space-y-3">
    <h2 class="flex items-center gap-2 text-lg font-semibold">
      {{ $t('Insectario') }}
      <span class="flex items-center gap-1 rounded bg-stone-200 px-1.5 py-0.5 text-xs font-normal text-stone-600">
        <Lock :size="11" /> {{ $t('solo con cuenta') }}
      </span>
    </h2>
    <div class="grid grid-cols-2 gap-2 sm:grid-cols-3 lg:grid-cols-6">
      <div
        class="stat"
        :title="$t('Sin fecha de muerte y en el insectario desde hace menos de {n} días', { n: team.insectary.aliveDays })"
      >
        <p class="stat-label">{{ $t('Mariposas vivas') }}</p>
        <p class="stat-value">{{ format(team.insectary.alive) }}</p>
        <p class="stat-note">
          {{ team.insectary.aliveFemale }} ♀ · {{ team.insectary.aliveMale }} ♂ ·
          {{ $t('{n} silvestres', { n: team.insectary.aliveWild }) }}
        </p>
      </div>
      <div class="stat" :title="$t('Puestas en los últimos {n} días que aún no emergen', { n: team.insectary.clutchDays })">
        <p class="stat-label">{{ $t('Clutches en curso') }}</p>
        <p class="stat-value">{{ format(team.insectary.clutches) }}</p>
      </div>
      <div class="stat">
        <p class="stat-label">{{ $t('Huevos') }}</p>
        <p class="stat-value">{{ format(team.insectary.stages.egg.n) }}</p>
        <p class="stat-note">{{ $t('en {n} clutches', { n: team.insectary.stages.egg.clutches }) }}</p>
      </div>
      <div class="stat">
        <p class="stat-label">{{ $t('Larvas') }}</p>
        <p class="stat-value">{{ format(team.insectary.stages.larva.n) }}</p>
        <p class="stat-note">{{ $t('en {n} clutches', { n: team.insectary.stages.larva.clutches }) }}</p>
      </div>
      <div class="stat">
        <p class="stat-label">{{ $t('Pupas') }}</p>
        <p class="stat-value">{{ format(team.insectary.stages.pupa.n) }}</p>
        <p class="stat-note">{{ $t('en {n} clutches', { n: team.insectary.stages.pupa.clutches }) }}</p>
      </div>
      <div class="stat">
        <p class="stat-label">{{ $t('Últimos 30 días') }}</p>
        <p class="stat-value">
          {{ team.insectary.arrivals30 }} <span class="text-sm font-normal">{{ $t('ingresos') }}</span>
        </p>
        <p class="stat-note">{{ $t('{n} muertes', { n: team.insectary.deaths30 }) }}</p>
      </div>
    </div>
    <p v-if="team.insectary.stale" class="hint">
      {{
        $t(
          'Además hay {n} mariposas sin fecha de muerte que entraron hace más de {days} días: probablemente murieron sin registrarse (no se cuentan como vivas).',
          { n: format(team.insectary.stale), days: team.insectary.aliveDays },
        )
      }}
    </p>
    <div class="grid gap-4 lg:grid-cols-2">
      <ChartCard :title="$t('Vivas por especie')" :subtitle="$t('Sexo y origen')">
        <div class="max-h-80 overflow-auto text-sm">
          <table class="w-full">
            <thead class="sticky top-0 bg-white text-xs text-stone-500">
              <tr>
                <th class="py-1 text-left">{{ $t('Especie') }}</th>
                <th class="px-2 text-right">♀</th>
                <th class="px-2 text-right">♂</th>
                <th class="px-2 text-right" :title="$t('Sin sexo registrado')">?</th>
                <th class="px-2 text-right">{{ $t('Silv.') }}</th>
                <th class="px-2 text-right">{{ $t('Criadas') }}</th>
                <th class="pl-2 text-right">{{ $t('Total') }}</th>
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
      <ChartCard :title="$t('Clutches en curso por especie')" :subtitle="$t('Individuos según la etapa más reciente registrada')">
        <div class="max-h-80 overflow-auto text-sm">
          <table class="w-full">
            <thead class="sticky top-0 bg-white text-xs text-stone-500">
              <tr>
                <th class="py-1 text-left">{{ $t('Especie') }}</th>
                <th class="px-2 text-right">{{ $t('Clutches') }}</th>
                <th class="px-2 text-right">{{ $t('Huevos') }}</th>
                <th class="px-2 text-right">{{ $t('Larvas') }}</th>
                <th class="pl-2 text-right">{{ $t('Pupas') }}</th>
              </tr>
            </thead>
            <tbody class="tabular-nums">
              <tr v-for="s in team.insectary.clutchesBySpecies" :key="s.name" class="border-t border-stone-100">
                <td class="py-1 italic">{{ s.name === 'Sin especie' ? $t('Sin especie') : s.name }}</td>
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
        :title="$t('Muertes por mes')"
        :subtitle="$t('Últimos 12 meses, en Insectary_data')"
        :table="
          table(
            [$t('Mes'), $t('Preservadas'), $t('Otras')],
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
      <ChartCard :title="$t('Muertes de los últimos 30 días')">
        <div class="grid gap-4 text-sm sm:grid-cols-2">
          <div>
            <p class="mb-1 text-xs text-stone-500">{{ $t('Por causa') }}</p>
            <p
              v-for="c in team.insectary.deathCauses30"
              :key="c.name"
              class="flex justify-between border-t border-stone-100 py-1"
            >
              <span>{{ c.name === 'Sin causa' ? $t('Sin causa') : c.name }}</span> <span class="tabular-nums">{{ c.n }}</span>
            </p>
          </div>
          <div>
            <p class="mb-1 text-xs text-stone-500">{{ $t('Por especie') }}</p>
            <p
              v-for="c in team.insectary.deathSpecies30"
              :key="c.name"
              class="flex justify-between border-t border-stone-100 py-1"
            >
              <span class="italic">{{ c.name }}</span> <span class="tabular-nums">{{ c.n }}</span>
            </p>
          </div>
        </div>
      </ChartCard>
    </div>
  </section>
</template>
