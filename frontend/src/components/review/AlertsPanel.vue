<script setup lang="ts">
import { computed } from 'vue'
import { AlertTriangle, Info } from 'lucide-vue-next'
import { dayFirst, tablesLink, type AlertsData } from '../../lib/review'
import { tx } from '../../lib/i18n'

/**
 * Revisión → Alertas: what the team must act on in time (server/alerts.mjs).
 * The CAM pools of the Lists sheet with what is left in each range, so a new
 * range is asked for before one runs out; and the 30-preserved rule, per
 * species: which reached 30 (mark and release from then on), which are close,
 * and which were preserved after 30, as information.
 */
const props = defineProps<{ data: AlertsData | null }>()
const rule = computed(() => props.data?.preserveRule)
const percentLeft = (left: number, size: number) => `${Math.round((100 * left) / size)} %`
const levelTone: Record<string, string> = {
  low: 'bg-amber-50 text-amber-950',
  done: 'text-stone-400',
  ok: '',
}
</script>

<template>
  <div class="min-h-0 flex-1 overflow-auto bg-stone-50">
    <div class="mx-auto max-w-6xl space-y-5 p-3 sm:p-4">
      <p v-if="!data" class="text-sm text-stone-500">{{ $t('Revisando…') }}</p>
      <template v-else>
        <section class="rounded-lg border border-stone-300 bg-white p-3 shadow-sm">
          <h2 class="mb-2 font-semibold">{{ $t('Alertas') }}</h2>
          <p v-if="!data.alerts.length" class="text-sm text-stone-500">{{ $t('Nada que avisar.') }}</p>
          <ul class="space-y-1">
            <li
              v-for="a in data.alerts"
              :key="a.id"
              class="flex items-start gap-2 rounded px-2 py-1.5 text-sm"
              :class="a.level === 'warn' ? 'bg-amber-50 text-amber-950' : 'bg-stone-50 text-stone-700'"
            >
              <AlertTriangle v-if="a.level === 'warn'" :size="15" class="mt-0.5 shrink-0 text-amber-700" />
              <Info v-else :size="15" class="mt-0.5 shrink-0 text-stone-500" />
              <span>{{ tx(a.text, a.textMsg) }}</span>
            </li>
          </ul>
        </section>

        <section class="rounded-lg border border-stone-300 bg-white p-3 shadow-sm">
          <h2 class="font-semibold">{{ $t('Rangos de CAM') }}</h2>
          <p class="hint mb-2">
            {{
              $t(
                'Los CAM_ID salen de los rangos de la hoja Lists. «Quedan» cuenta los CAM por encima del más alto usado en cualquier hoja; los huecos más abajo se cuentan aparte porque casi siempre se saltaron. Se avisa cuando un rango usado en el último año tiene menos de {left} o menos del {share} % libre: hay que pedir a PAS o AA un rango nuevo.',
                { left: data.thresholds.camLeft, share: Math.round(data.thresholds.camShare * 100) },
              )
            }}
          </p>
          <div v-for="p in data.camPools" :key="p.pool" class="mb-3">
            <p class="text-sm">
              <strong class="font-mono">{{ p.pool }}</strong>
              <span class="text-xs text-stone-500"> · {{ p.fields.join(', ') }}</span>
              <span
                class="ml-2 rounded px-1.5 py-0.5 text-xs font-medium"
                :class="p.level === 'ok' ? 'bg-emerald-100 text-emerald-900' : 'bg-amber-100 text-amber-900'"
                >{{ $t('quedan {n}', { n: p.left }) }}</span
              >
            </p>
            <div class="overflow-x-auto">
              <table class="mt-1 w-full border-collapse text-xs">
                <thead class="bg-stone-50 text-left text-stone-600">
                  <tr>
                    <th class="px-1.5 py-1 font-medium">{{ $t('Rango') }}</th>
                    <th class="px-1.5 py-1 text-right font-medium">{{ $t('Tamaño') }}</th>
                    <th class="px-1.5 py-1 text-right font-medium">{{ $t('Usados') }}</th>
                    <th class="px-1.5 py-1 font-medium">{{ $t('Más alto') }}</th>
                    <th class="px-1.5 py-1 font-medium">{{ $t('Siguiente') }}</th>
                    <th class="px-1.5 py-1 text-right font-medium">{{ $t('Quedan') }}</th>
                    <th class="px-1.5 py-1 text-right font-medium">{{ $t('Huecos') }}</th>
                    <th class="px-1.5 py-1 font-medium">{{ $t('Último uso') }}</th>
                  </tr>
                </thead>
                <tbody>
                  <tr v-for="r in p.ranges" :key="r.first" class="border-t border-stone-100" :class="levelTone[r.level]">
                    <td class="px-1.5 py-1 font-mono whitespace-nowrap">{{ r.first }}–{{ r.last }}</td>
                    <td class="px-1.5 py-1 text-right tabular-nums">{{ r.size }}</td>
                    <td class="px-1.5 py-1 text-right tabular-nums">{{ r.used }}</td>
                    <td class="px-1.5 py-1 font-mono">{{ r.highest ?? '—' }}</td>
                    <td class="px-1.5 py-1 font-mono">{{ r.next ?? '—' }}</td>
                    <td class="px-1.5 py-1 text-right font-semibold tabular-nums">
                      {{ r.left }} <span class="font-normal text-stone-500">({{ percentLeft(r.left, r.size) }})</span>
                    </td>
                    <td class="px-1.5 py-1 text-right tabular-nums">{{ r.gaps }}</td>
                    <td class="px-1.5 py-1 whitespace-nowrap">
                      {{ r.lastUsed ? `${r.lastUsed.cam} · ${dayFirst(r.lastUsed.date)}` : '—' }}
                    </td>
                  </tr>
                </tbody>
              </table>
            </div>
            <p v-if="p.fullRanges" class="mt-0.5 text-[11px] text-stone-500">
              {{ $t('{n} rangos anteriores, ya completos, no se muestran.', { n: p.fullRanges }) }}
            </p>
          </div>
        </section>

        <section v-if="rule" class="rounded-lg border border-stone-300 bg-white p-3 shadow-sm">
          <h2 class="font-semibold">{{ $t('Regla de los {n} preservados', { n: rule.limit }) }}</h2>
          <p class="hint mb-2">
            {{
              $t(
                'Cuando una especie de Ithomiini tiene {n} individuos preservados (Collected_Preserved) de {places}, se deja de preservar: desde entonces se marca y libera. Se cuenta por especie (género y epíteto, sin subespecie), con cualquier Purpose y todo el histórico. Las preservadas después de llegar a {n} se listan como información, no como error.',
                { n: rule.limit, places: rule.locations.join(', ') },
              )
            }}
          </p>
          <div class="grid gap-4 lg:grid-cols-[2fr_1fr]">
            <div class="overflow-x-auto">
              <h3 class="mb-1 text-sm font-semibold">{{ $t('Llegaron a {n}: marcar y liberar', { n: rule.limit }) }}</h3>
              <table class="w-full border-collapse text-xs">
                <thead class="bg-stone-50 text-left text-stone-600">
                  <tr>
                    <th class="px-1.5 py-1 font-medium">{{ $t('Especie') }}</th>
                    <th class="px-1.5 py-1 text-right font-medium">{{ $t('Preservadas') }}</th>
                    <th class="px-1.5 py-1 font-medium">{{ $t('Llegó a {n}', { n: rule.limit }) }}</th>
                    <th class="px-1.5 py-1 font-medium">{{ $t('Preservadas después') }}</th>
                  </tr>
                </thead>
                <tbody>
                  <tr
                    v-for="s in rule.reached"
                    :key="s.species"
                    class="border-t border-stone-100 align-top"
                    :class="s.recent ? 'bg-amber-50' : ''"
                  >
                    <td class="px-1.5 py-1 italic">{{ s.species }}</td>
                    <td class="px-1.5 py-1 text-right tabular-nums">{{ s.preserved }}</td>
                    <td class="px-1.5 py-1 whitespace-nowrap">
                      <a :href="tablesLink(s.reachedRow.sheet, s.reachedRow.label)" class="text-brand-700 hover:underline">{{
                        dayFirst(s.reachedOn) ?? '—'
                      }}</a>
                    </td>
                    <td class="px-1.5 py-1">
                      <span class="tabular-nums">{{ s.after }}</span>
                      <span v-if="s.afterRows.length" class="text-stone-500">
                        ·
                        <a
                          v-for="(r, i) in s.afterRows"
                          :key="r.recordId"
                          :href="tablesLink(r.sheet, r.label)"
                          class="text-brand-700 hover:underline"
                          :title="r.label"
                          >{{ $t('fila {row}', { row: r.row }) }}{{ r.date ? ` (${dayFirst(r.date)})` : ''
                          }}{{ i < s.afterRows.length - 1 ? ', ' : '' }}</a
                        >
                      </span>
                    </td>
                  </tr>
                </tbody>
              </table>
            </div>
            <div>
              <h3 class="mb-1 text-sm font-semibold">{{ $t('Cerca de {n} ({near} o más)', { n: rule.limit, near: rule.near }) }}</h3>
              <p v-if="!rule.close.length" class="text-xs text-stone-500">{{ $t('Ninguna especie.') }}</p>
              <ul class="space-y-0.5 text-xs">
                <li v-for="s in rule.close" :key="s.species" class="flex justify-between gap-2 border-t border-stone-100 py-1">
                  <span class="italic">{{ s.species }}</span>
                  <span class="tabular-nums"
                    >{{ s.preserved }} · <span class="text-amber-800">{{ $t('faltan {n}', { n: s.left }) }}</span></span
                  >
                </li>
              </ul>
            </div>
          </div>
        </section>
      </template>
    </div>
  </div>
</template>
