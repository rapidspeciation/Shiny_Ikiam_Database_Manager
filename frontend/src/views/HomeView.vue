<script setup lang="ts">
import { onMounted, ref, watch } from 'vue'
import { RouterLink } from 'vue-router'
import { Lock } from 'lucide-vue-next'
import InsectarySummary from '../components/home/InsectarySummary.vue'
import NatureSummary from '../components/home/NatureSummary.vue'
import LatestIdsCard from '../components/home/LatestIdsCard.vue'
import TeamCounts from '../components/home/TeamCounts.vue'
import UpcomingCard from '../components/home/UpcomingCard.vue'
import AlertsCard from '../components/home/AlertsCard.vue'
import { format } from '../components/charts/chart'
import { api } from '../lib/api'
import { errorText } from '../lib/notice'
import { dateLabel, type Summary } from '../lib/summary'
import { useSession } from '../stores/session'

/**
 * "Inicio". Anyone sees the natural history (rates and proportions, never how
 * many butterflies were collected or reared). Signed-in people also see the
 * insectary right now, and the team's counts. Computed on the server from the
 * local copy of the workbook (GET /api/summary).
 */
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
</script>

<template>
  <div class="h-full overflow-auto bg-stone-50">
    <div class="mx-auto max-w-7xl space-y-8 p-4 sm:p-6">
      <!-- For the team, what they need when writing labels comes first. -->
      <LatestIdsCard v-if="data?.team" :ids="data.team.latestIds" @extended="load" />
      <AlertsCard v-if="data?.team" />
      <UpcomingCard v-if="data?.team" :upcoming="data.team.upcoming" />
      <header class="space-y-3">
        <div class="flex flex-wrap items-end gap-x-4 gap-y-1">
          <h1 class="min-w-0 flex-1 text-xl font-semibold">{{ $t('Mariposas Ithomiini · Ikiam') }}</h1>
          <p v-if="data" class="text-xs text-stone-500">{{ $t('Datos al {date}', { date: dateLabel(data.today) }) }}</p>
        </div>
        <p class="max-w-3xl text-sm text-stone-700">
          {{
            $t(
              'Las Ithomiini son mariposas neotropicales, muchas de alas transparentes, que forman anillos de mimetismo: especies distintas comparten los mismos colores de advertencia. Desde Ikiam (Tena, Ecuador) el proyecto las estudia en el campo, en el insectario y en el laboratorio.',
            )
          }}
        </p>
        <div v-if="data" class="grid grid-cols-2 gap-2 sm:grid-cols-4">
          <div class="stat">
            <p class="stat-label">{{ $t('Especies de Ithomiini registradas') }}</p>
            <p class="stat-value">{{ data.nature.facts.ithomiini }}</p>
            <p class="stat-note">{{ $t('{n} especies de mariposas en total', { n: data.nature.facts.species }) }}</p>
          </div>
          <div class="stat">
            <p class="stat-label">{{ $t('Lugares muestreados') }}</p>
            <p class="stat-value">{{ data.nature.facts.places }}</p>
          </div>
          <div class="stat">
            <p class="stat-label">{{ $t('Altitudes') }}</p>
            <p class="stat-value text-lg">
              {{
                data.nature.facts.elevation
                  ? `${format(data.nature.facts.elevation[0])}–${format(data.nature.facts.elevation[1])} m`
                  : '—'
              }}
            </p>
          </div>
          <div class="stat">
            <p class="stat-label">{{ $t('Registros desde') }}</p>
            <p class="stat-value">{{ data.nature.facts.since ?? '—' }}</p>
          </div>
        </div>
      </header>

      <p v-if="problem" class="rounded bg-red-50 px-3 py-2 text-sm text-red-800">{{ problem }}</p>
      <p v-else-if="!data" class="text-stone-500">{{ $t('Cargando resúmenes…') }}</p>

      <template v-if="data">
        <InsectarySummary v-if="data.team" :team="data.team" />
        <NatureSummary :nature="data.nature" />
        <TeamCounts v-if="data.team" :team="data.team" />
        <p v-else class="flex items-center gap-2 rounded-md border border-stone-200 bg-white px-3 py-2 text-sm text-stone-600">
          <Lock :size="14" />
          <span
            >{{ $t('El equipo ve además el estado del insectario, el monitoreo y los registros completos al') }}
            <RouterLink :to="{ path: '/entrar', query: { volver: '/inicio' } }" class="text-brand-700 underline">{{
              $t('iniciar sesión')
            }}</RouterLink
            >.</span
          >
        </p>
      </template>
    </div>
  </div>
</template>
