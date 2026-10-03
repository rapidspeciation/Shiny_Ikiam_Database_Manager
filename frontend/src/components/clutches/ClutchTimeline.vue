<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { Info, Trash2 } from 'lucide-vue-next'
import type { ClutchDay } from '../../composables/useClutchDay'
import { api } from '../../lib/api'
import { aliveAndSurvived, type ClutchEvent, type ClutchTallies, type Stage, type StageTally, type YoungRow } from '../../lib/clutches'
import { dayLabel, formatSerial, isoToSerial } from '../../lib/dates'
import { errorText, notify } from '../../lib/notice'
import { useSession } from '../../stores/session'
import { eventLine, stageWord } from './eventWords'

/**
 * What the paper cannot hold, for one clutch (only in the app): its events day
 * by day (hatched, died, disappeared, preserved: who and when), the eggs and
 * larvae of it registered one by one in Emergidos, and the numbers the team
 * debates, each explained: alive in the cage, survived, and what the sheet's
 * NUMBER OF LARVAE holds under the team's setting.
 */
const props = defineProps<{
  recordId: string
  day: ClutchDay
  /** The sheet's totals as the person sees them (pending edits included), by stage. */
  totals: Partial<Record<Stage, number | null>>
  canEdit: boolean
  initials: (name: string) => string
}>()
const session = useSession()

const data = ref<{ events: ClutchEvent[]; young: YoungRow[]; tally: ClutchTallies } | null>(null)
async function load() {
  const id = props.recordId
  try {
    const out = await api<{ events: ClutchEvent[]; young: YoungRow[]; tally: ClutchTallies }>(`clutches/events?recordId=${encodeURIComponent(id)}`)
    if (id === props.recordId) data.value = out
  } catch {
    /* offline: keep what was shown */
  }
}
watch(() => [props.recordId, props.day.eventsVersion.value, props.day.day.value.events?.length], load, { immediate: true })

const subtract = computed(() => props.day.settings.subtractPreserved)
const tally = (stage: Stage): StageTally => data.value?.tally[stage] ?? { gained: 0, died: 0, disappeared: 0, preserved: 0 }
const known = (stage: Stage) => Object.values(tally(stage)).some(n => n > 0)
/** The stages with something recorded: their numbers, explained. */
const numbers = computed(() =>
  (['larva', 'pupa'] as Stage[])
    .filter(known)
    .map(stage => {
      const t = tally(stage)
      const sheet = props.totals[stage] ?? 0
      return { stage, tally: t, sheet, ...aliveAndSurvived(sheet, t.preserved, subtract.value) }
    }),
)
/** Days newest first; Emergidos' rows whose ID an event already names are left out. */
const days = computed(() => {
  const named = new Set((data.value?.events ?? []).flatMap(e => e.ids))
  const out = new Map<string, { events: ClutchEvent[]; young: YoungRow[] }>()
  const at = (day: string) => out.get(day) ?? (out.set(day, { events: [], young: [] }), out.get(day)!)
  for (const e of data.value?.events ?? []) at(e.day).events.push(e)
  for (const y of data.value?.young ?? []) if (!named.has(y.id)) at(y.day ?? '').young.push(y)
  return [...out.entries()].sort((a, b) => b[0].localeCompare(a[0]))
})
const time = (iso: string) => new Date(iso).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })
const mine = (e: ClutchEvent) => e.actor === session.user?.id || ['reviewer', 'admin'].includes(session.user?.role ?? '')
const removing = ref<string | null>(null)
async function remove(e: ClutchEvent) {
  removing.value = e.id
  try {
    await props.day.removeEvent(e.id)
  } catch (err) {
    notify(errorText(err), 'error')
  } finally {
    removing.value = null
  }
}
const explain = ref(false)
</script>

<template>
  <section class="border-b border-stone-100 py-3" :aria-label="$t('Eventos (solo en la app)')">
    <div class="flex items-center gap-2">
      <h3 class="field-label mb-0 flex-1">{{ $t('Eventos (solo en la app)') }}</h3>
      <button type="button" class="flex h-9 items-center gap-1 text-xs text-stone-600 underline" :aria-expanded="explain" @click="explain = !explain">
        <Info :size="14" /> {{ $t('¿Qué significa cada número?') }}
      </button>
    </div>
    <div v-if="explain" class="mt-1 space-y-1 rounded-md bg-stone-100 px-3 py-2 text-xs text-stone-700">
      <p>{{ $t('Vivas en la jaula: las que hay ahora (eclosionaron − murieron − desaparecieron − se preservaron).') }}</p>
      <p>{{ $t('Sobrevivieron: vivas + preservadas (estaban vivas cuando se preservaron).') }}</p>
      <p>
        {{
          subtract
            ? $t('NUMBER OF LARVAE en la hoja: el equipo resta las preservadas, así que guarda las vivas en la jaula.')
            : $t('NUMBER OF LARVAE en la hoja: el equipo deja contadas las preservadas, así que guarda las que sobrevivieron.')
        }}
        {{ $t('Murieron y desaparecieron se restan siempre; aquí se ven por separado.') }}
      </p>
      <p>{{ $t('Los eventos se guardan solo en la app (no en Google Sheets), con quién y cuándo. Las larvas preservadas en Emergidos cuentan también.') }}</p>
    </div>
    <ul v-if="numbers.length" class="mt-2 space-y-2">
      <li v-for="s in numbers" :key="s.stage" class="rounded-lg border border-stone-200 bg-white px-3 py-2">
        <p class="text-xs font-semibold tracking-wide text-stone-500 uppercase">{{ stageWord(s.stage) }}</p>
        <div class="mt-1 grid grid-cols-3 gap-2 text-center">
          <div>
            <p class="text-xl font-semibold tabular-nums">{{ s.alive }}</p>
            <p class="text-[11px] leading-tight text-stone-600">{{ $t('vivas en la jaula') }}</p>
          </div>
          <div>
            <p class="text-xl font-semibold tabular-nums">{{ s.survived }}</p>
            <p class="text-[11px] leading-tight text-stone-600">{{ $t('sobrevivieron (vivas + preservadas)') }}</p>
          </div>
          <div>
            <p class="text-xl font-semibold tabular-nums">{{ s.sheet }}</p>
            <p class="text-[11px] leading-tight text-stone-600">{{ $t('en la hoja') }}</p>
          </div>
        </div>
        <p class="mt-1 text-xs text-stone-600 tabular-nums">
          <span v-if="s.tally.gained">+{{ s.tally.gained }} {{ s.stage === 'larva' ? $t('eclosionaron') : s.stage === 'pupa' ? $t('pupas nuevas') : $t('puestos') }} · </span>
          −{{ s.tally.died }} {{ $t('murieron') }} · −{{ s.tally.disappeared }} {{ $t('desaparecieron') }} · {{ s.tally.preserved }} {{ $t('se preservaron') }}
        </p>
      </li>
    </ul>
    <p v-if="!days.length" class="mt-1 text-sm text-stone-500">
      {{ canEdit ? $t('Nada todavía. Al restar con −N la app pregunta si murieron, desaparecieron o se preservaron.') : $t('Nada todavía.') }}
    </p>
    <ol v-else class="mt-2 space-y-2">
      <li v-for="[d, g] in days" :key="d">
        <p class="text-xs font-medium text-stone-500">{{ d ? dayLabel(d) : $t('sin fecha') }}</p>
        <ul class="mt-0.5 divide-y divide-stone-100 rounded-md border border-stone-200 bg-white">
          <li v-for="e in g.events" :key="e.id" class="flex items-center gap-2 py-1 pr-1 pl-3 text-sm">
            <span class="min-w-0 flex-1 break-words tabular-nums">{{ eventLine(e) }}</span>
            <span class="shrink-0 text-xs text-stone-500">{{ initials(e.name || e.username || '') }} {{ time(e.createdAt) }}</span>
            <button
              v-if="canEdit && mine(e)"
              type="button"
              class="grid h-11 w-11 shrink-0 place-items-center rounded-md text-stone-500 active:bg-stone-100"
              :aria-label="$t('Borrar este evento')"
              :title="$t('Borrar este evento')"
              :disabled="removing === e.id"
              @click="remove(e)"
            >
              <Trash2 :size="16" />
            </button>
          </li>
          <li v-for="y in g.young" :key="y.id" class="flex items-center gap-2 px-3 py-1.5 text-sm">
            <span class="min-w-0 flex-1">
              {{ stageWord(y.stage) }}: {{ y.kind === 'preserved' ? $t('se preservó') : $t('se encontró muerta') }}
              <span class="font-medium">{{ y.id }}</span> <span class="text-xs text-stone-500">{{ y.lifestage }}</span>
            </span>
            <span class="shrink-0 text-xs text-stone-500">{{ $t('Emergidos') }}{{ y.day ? ` · ${formatSerial(isoToSerial(y.day))}` : '' }}</span>
          </li>
        </ul>
      </li>
    </ol>
  </section>
</template>
