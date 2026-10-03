<script setup lang="ts">
import { computed, onMounted, ref } from 'vue'
import { RouterLink } from 'vue-router'
import { AlertTriangle, Info } from 'lucide-vue-next'
import { api } from '../../lib/api'
import { tx } from '../../lib/i18n'
import type { AlertsData } from '../../lib/review'
import { useSession } from '../../stores/session'

/**
 * Inicio: the team's alerts (server/alerts.mjs): a CAM range running out, a
 * species that reached 30 preserved, species close to it, a butterfly preserved
 * without CAM or tube (its text opens the row). Hidden when there is
 * nothing to say; the details are in Revisión → Alertas.
 */
const session = useSession()
const data = ref<AlertsData | null>(null)
onMounted(async () => {
  try {
    data.value = await api<AlertsData>('alerts')
  } catch {
    /* Inicio works without them. */
  }
})
const SHOWN = 6
const alerts = computed(() => data.value?.alerts ?? [])
</script>

<template>
  <section v-if="alerts.length" class="rounded-lg border border-amber-300 bg-white p-4 shadow-sm">
    <div class="mb-2 flex flex-wrap items-baseline gap-x-3">
      <h2 class="text-lg font-semibold">{{ $t('Alertas') }}</h2>
      <RouterLink
        v-if="session.canEdit"
        :to="{ path: '/revision', query: { vista: 'alertas' } }"
        class="text-sm text-brand-700 hover:underline"
        >{{ $t('Ver rangos de CAM y la regla de los 30') }}</RouterLink
      >
    </div>
    <ul class="space-y-1">
      <li
        v-for="a in alerts.slice(0, SHOWN)"
        :key="a.id"
        class="flex items-start gap-2 rounded px-2 py-1 text-sm"
        :class="a.level === 'warn' ? 'bg-amber-50 text-amber-950' : 'text-stone-700'"
      >
        <AlertTriangle v-if="a.level === 'warn'" :size="15" class="mt-0.5 shrink-0 text-amber-700" />
        <Info v-else :size="15" class="mt-0.5 shrink-0 text-stone-500" />
        <a v-if="a.link?.startsWith('#/tablas')" :href="a.link" class="hover:underline">{{ tx(a.text, a.textMsg) }}</a>
        <span v-else>{{ tx(a.text, a.textMsg) }}</span>
      </li>
    </ul>
    <p v-if="alerts.length > SHOWN" class="mt-1 text-xs text-stone-500">
      {{ $t('y {n} más', { n: alerts.length - SHOWN }) }}
    </p>
  </section>
</template>
