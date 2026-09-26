<script setup lang="ts">
import { computed } from 'vue'
import { Download } from 'lucide-vue-next'
import { useMonitoring } from '../../composables/useMonitoring'
import { formatSerial, serialToIso } from '../../lib/dates'
import {
  MARK_THRESHOLD,
  formatMinutes,
  markHistories,
  monthOf,
  monthsByYear,
  nextMarkId,
  recaptureIds,
  sectionsByMonth,
  speciesStats,
} from '../../lib/monitoring'
import { persistentRef } from '../../lib/persist'
import type { TableRow } from '../../lib/types'

/**
 * "Resumen": the tables of the monthly monitoring reports, computed from the
 * Ikiam monitoring rows of Collection_data, plus the 30-preserved rule.
 */
const { table, rows: allRows, isIthomiini } = useMonitoring()

const from = persistentRef('monitoring:from', '')
const to = persistentRef('monitoring:to', '')
const collector = persistentRef('monitoring:who', '')
const section = persistentRef('monitoring:section', '')
const bySubspecies = persistentRef('monitoring:subspecies', false)
// The monthly reports count only Ithomiini.
const onlyIthomiini = persistentRef('monitoring:ithomiini', true)
const rows = computed(() =>
  onlyIthomiini.value ? allRows.value.filter(r => isIthomiini(String(r.values.SPECIES ?? ''))) : allRows.value,
)

const collectors = computed(() => [...new Set(rows.value.map(r => String(r.values.Collector ?? '')).filter(Boolean))].sort())
const filtered = computed(() =>
  rows.value.filter(r => {
    const month = monthOf(r)
    if (from.value && (!month || month < from.value)) return false
    if (to.value && (!month || month > to.value)) return false
    if (collector.value && r.values.Collector !== collector.value) return false
    if (section.value && String(r.values.Transect_section ?? '') !== section.value) return false
    return true
  }),
)

// Recaptures and the 30 rule always look at the whole history, whatever the filters.
const recaptures = computed(() => recaptureIds(allRows.value))
const allTime = computed(() => new Map(speciesStats(allRows.value, false, recaptures.value).map(s => [s.species, s])))
const species = computed(() => speciesStats(filtered.value, bySubspecies.value, recaptures.value))
const totals = computed(() => {
  const t = { total: 0, preserved: 0, marked: 0, recaptured: 0, days: new Set<number>() }
  for (const s of species.value) {
    t.total += s.total
    t.preserved += s.preserved
    t.marked += s.marked
    t.recaptured += s.recaptured
  }
  for (const r of filtered.value) if (typeof r.values.Collection_date === 'number') t.days.add(r.values.Collection_date)
  return t
})
const nextMark = computed(() => nextMarkId(allRows.value))

/** Species that already reached the threshold and those closest to it. */
const ruleOf = (name: string) => {
  const preserved = allTime.value.get(name)?.preserved || 0
  return {
    applies: isIthomiini(name),
    preserved,
    done: preserved >= MARK_THRESHOLD,
    missing: Math.max(0, MARK_THRESHOLD - preserved),
  }
}

const perSection = computed(() => sectionsByMonth(filtered.value))
const sectionMax = computed(() => Math.max(1, ...[...perSection.value.values()].flatMap(c => c.slice(1))))
const heat = (n: number) => (n ? `rgba(220, 38, 38, ${0.12 + (0.7 * n) / sectionMax.value})` : 'transparent')

const byMonth = computed(() => monthsByYear(filtered.value, recaptures.value))
const years = computed(() => [...new Set([...byMonth.value.keys()].map(k => k.slice(0, 4)))].sort().reverse())
const MONTHS = ['Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic']
const monthCell = (year: string, m: number) => byMonth.value.get(`${year}-${String(m + 1).padStart(2, '0')}`)

const histories = computed(() => {
  const inRange = new Set(filtered.value.map(r => r.id))
  return markHistories(rows.value).filter(h => h.events.some(e => inRange.has(e.row.id)))
})
const days = (a: number | null, b: number | null) => (a === null || b === null ? '' : `${b - a} días`)
const date = (d: number | null) => (d === null ? '—' : formatSerial(d))

function exportCsv() {
  const head = [
    'Especie',
    'Subespecie',
    'Preservados',
    'Marcados',
    'Recapturas',
    'Otros',
    'Hembras',
    'Machos',
    'Total',
    'Preservados (histórico)',
  ]
  const lines = species.value.map(s => [
    s.species,
    s.subspecies,
    s.preserved,
    s.marked,
    s.recaptured,
    s.other,
    s.female,
    s.male,
    s.total,
    ruleOf(s.species).preserved,
  ])
  const csv = [head, ...lines].map(l => l.map(v => `"${String(v).replaceAll('"', '""')}"`).join(',')).join('\r\n')
  const a = document.createElement('a')
  a.href = URL.createObjectURL(new Blob(['﻿' + csv], { type: 'text/csv;charset=utf-8' }))
  a.download = `monitoreo_${from.value || 'inicio'}_${to.value || 'hoy'}.csv`
  a.click()
  URL.revokeObjectURL(a.href)
}
const firstMonth = computed(() => {
  const dates = rows.value.map(r => r.values.Collection_date).filter((d): d is number => typeof d === 'number')
  return dates.length ? serialToIso(Math.min(...dates)).slice(0, 7) : ''
})
const sexOf = (row: TableRow) => String(row.values.Sex ?? '')
</script>

<template>
  <div class="flex h-full flex-col">
    <div class="toolbar">
      <label>
        <span class="field-label">Desde (mes)</span>
        <input v-model="from" type="month" class="field-input" :min="firstMonth" />
      </label>
      <label>
        <span class="field-label">Hasta (mes)</span>
        <input v-model="to" type="month" class="field-input" />
      </label>
      <label class="min-w-44">
        <span class="field-label">Recolector</span>
        <select v-model="collector" class="field-input">
          <option value="">Todos</option>
          <option v-for="c in collectors" :key="c" :value="c">{{ c }}</option>
        </select>
      </label>
      <label>
        <span class="field-label">Transecto</span>
        <select v-model="section" class="field-input">
          <option value="">Todos</option>
          <option v-for="t in ['1', '2', '3', '4']" :key="t" :value="t">T{{ t }}</option>
        </select>
      </label>
      <label class="flex items-center gap-2 pb-1.5 text-sm"
        ><input v-model="onlyIthomiini" type="checkbox" /> Solo Ithomiini</label
      >
      <label class="flex items-center gap-2 pb-1.5 text-sm"
        ><input v-model="bySubspecies" type="checkbox" /> Por subespecie</label
      >
      <button class="btn" @click="exportCsv"><Download :size="15" /> CSV</button>
    </div>

    <p v-if="!table" class="p-6 text-stone-500">Cargando Collection_data…</p>
    <div v-else class="min-h-0 flex-1 space-y-6 overflow-auto p-4">
      <div class="grid grid-cols-2 gap-2 sm:grid-cols-3 lg:grid-cols-6">
        <div class="rounded-md border border-stone-200 bg-white px-3 py-2">
          <p class="text-xs text-stone-500">Individuos</p>
          <p class="text-xl font-semibold">{{ totals.total }}</p>
        </div>
        <div class="rounded-md border border-stone-200 bg-white px-3 py-2">
          <p class="text-xs text-stone-500">Preservados</p>
          <p class="text-xl font-semibold">{{ totals.preserved }}</p>
        </div>
        <div class="rounded-md border border-stone-200 bg-white px-3 py-2">
          <p class="text-xs text-stone-500">Marcados y liberados</p>
          <p class="text-xl font-semibold">{{ totals.marked }}</p>
        </div>
        <div class="rounded-md border border-stone-200 bg-white px-3 py-2">
          <p class="text-xs text-stone-500">Recapturas</p>
          <p class="text-xl font-semibold">{{ totals.recaptured }}</p>
        </div>
        <div class="rounded-md border border-stone-200 bg-white px-3 py-2">
          <p class="text-xs text-stone-500">Días de monitoreo</p>
          <p class="text-xl font-semibold">{{ totals.days.size }}</p>
        </div>
        <div class="rounded-md border border-brand-600 bg-brand-50 px-3 py-2">
          <p class="text-xs text-brand-700">Próxima marca</p>
          <p class="text-xl font-semibold text-brand-700">{{ nextMark || '—' }}</p>
        </div>
      </div>

      <section>
        <h2 class="mb-1 font-semibold">Especies</h2>
        <p class="hint mb-2">
          Regla del protocolo: con {{ MARK_THRESHOLD }} individuos preservados de una especie de Ithomiini (monitoreo de Ikiam,
          todo el histórico) se pasa a marcar y liberar. Las demás columnas siguen los filtros.
        </p>
        <div class="overflow-x-auto rounded-md border border-stone-200 bg-white">
          <table class="w-full text-sm">
            <thead class="bg-stone-100 text-left text-xs text-stone-600">
              <tr>
                <th class="px-3 py-2">Especie</th>
                <th class="px-3 py-2">Regla de {{ MARK_THRESHOLD }}</th>
                <th class="px-3 py-2 text-right">Preserv.</th>
                <th class="px-3 py-2 text-right">Marcados</th>
                <th class="px-3 py-2 text-right">Recapt.</th>
                <th class="px-3 py-2 text-right">♀</th>
                <th class="px-3 py-2 text-right">♂</th>
                <th class="px-3 py-2 text-right">Total</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="s in species" :key="s.key" class="border-t border-stone-100">
                <td class="px-3 py-1.5">
                  <i>{{ s.species }}</i> <span class="text-stone-500">{{ s.subspecies }}</span>
                </td>
                <td class="min-w-48 px-3 py-1.5">
                  <span v-if="!ruleOf(s.species).applies" class="text-xs text-stone-400">no aplica (no es Ithomiini)</span>
                  <div v-else class="flex items-center gap-2">
                    <div class="h-2 w-24 overflow-hidden rounded bg-stone-200">
                      <div
                        class="h-full"
                        :class="ruleOf(s.species).done ? 'bg-brand-600' : 'bg-amber-500'"
                        :style="{ width: `${Math.min(100, (100 * ruleOf(s.species).preserved) / MARK_THRESHOLD)}%` }"
                      />
                    </div>
                    <span class="text-xs whitespace-nowrap" :class="ruleOf(s.species).done ? 'text-brand-700' : 'text-amber-800'">
                      {{ ruleOf(s.species).preserved }}/{{ MARK_THRESHOLD }} ·
                      {{ ruleOf(s.species).done ? 'marcar y liberar' : `preservar (faltan ${ruleOf(s.species).missing})` }}
                    </span>
                  </div>
                </td>
                <td class="px-3 py-1.5 text-right tabular-nums">{{ s.preserved || '' }}</td>
                <td class="px-3 py-1.5 text-right tabular-nums">{{ s.marked || '' }}</td>
                <td class="px-3 py-1.5 text-right tabular-nums">{{ s.recaptured || '' }}</td>
                <td class="px-3 py-1.5 text-right tabular-nums">{{ s.female || '' }}</td>
                <td class="px-3 py-1.5 text-right tabular-nums">{{ s.male || '' }}</td>
                <td class="px-3 py-1.5 text-right font-medium tabular-nums">{{ s.total }}</td>
              </tr>
              <tr class="border-t border-stone-300 bg-stone-50 font-medium">
                <td class="px-3 py-1.5" colspan="2">Total</td>
                <td class="px-3 py-1.5 text-right tabular-nums">{{ totals.preserved }}</td>
                <td class="px-3 py-1.5 text-right tabular-nums">{{ totals.marked }}</td>
                <td class="px-3 py-1.5 text-right tabular-nums">{{ totals.recaptured }}</td>
                <td class="px-3 py-1.5 text-right tabular-nums">{{ species.reduce((n, s) => n + s.female, 0) }}</td>
                <td class="px-3 py-1.5 text-right tabular-nums">{{ species.reduce((n, s) => n + s.male, 0) }}</td>
                <td class="px-3 py-1.5 text-right tabular-nums">{{ totals.total }}</td>
              </tr>
            </tbody>
          </table>
        </div>
      </section>

      <div class="grid gap-6 xl:grid-cols-2">
        <section>
          <h2 class="mb-2 font-semibold">Individuos por transecto y mes</h2>
          <div class="max-h-96 overflow-auto rounded-md border border-stone-200 bg-white">
            <table class="w-full text-sm">
              <thead class="sticky top-0 bg-stone-100 text-xs text-stone-600">
                <tr>
                  <th class="px-3 py-2 text-left">Mes</th>
                  <th v-for="t in 4" :key="t" class="px-3 py-2 text-right">T{{ t }}</th>
                  <th class="px-3 py-2 text-right" title="Filas sin Transect_section">Sin T</th>
                  <th class="px-3 py-2 text-right">Total</th>
                </tr>
              </thead>
              <tbody>
                <tr v-for="[month, counts] in perSection" :key="month" class="border-t border-stone-100">
                  <td class="px-3 py-1">{{ month }}</td>
                  <td v-for="t in 4" :key="t" class="px-3 py-1 text-right tabular-nums" :style="{ background: heat(counts[t]) }">
                    {{ counts[t] || '' }}
                  </td>
                  <td class="px-3 py-1 text-right text-stone-500 tabular-nums">{{ counts[0] || '' }}</td>
                  <td class="px-3 py-1 text-right font-medium tabular-nums">{{ counts.reduce((a, b) => a + b, 0) }}</td>
                </tr>
              </tbody>
            </table>
          </div>
        </section>

        <section>
          <h2 class="mb-2 font-semibold">Comparación entre años</h2>
          <div class="max-h-96 overflow-auto rounded-md border border-stone-200 bg-white">
            <table class="w-full text-sm">
              <thead class="sticky top-0 bg-stone-100 text-xs text-stone-600">
                <tr>
                  <th class="px-3 py-2 text-left">Mes</th>
                  <th v-for="y in years" :key="y" class="px-3 py-2 text-right">{{ y }}</th>
                </tr>
              </thead>
              <tbody>
                <tr v-for="(name, m) in MONTHS" :key="name" class="border-t border-stone-100">
                  <td class="px-3 py-1">{{ name }}</td>
                  <td v-for="y in years" :key="y" class="px-3 py-1 text-right whitespace-nowrap tabular-nums">
                    <template v-if="monthCell(y, m)">
                      <span class="font-medium">{{ monthCell(y, m)!.total }}</span>
                      <span class="text-xs text-stone-500">
                        · {{ monthCell(y, m)!.marked }} marc.<template v-if="monthCell(y, m)!.recaptured">
                          · {{ monthCell(y, m)!.recaptured }} recapt.</template
                        >
                      </span>
                    </template>
                  </td>
                </tr>
              </tbody>
            </table>
          </div>
          <p class="hint mt-1">Total de individuos · nuevas marcas · recapturas.</p>
        </section>
      </div>

      <section>
        <h2 class="mb-2 font-semibold">Recapturas ({{ histories.length }} marcas)</h2>
        <div class="overflow-x-auto rounded-md border border-stone-200 bg-white">
          <table class="w-full text-sm">
            <thead class="bg-stone-100 text-left text-xs text-stone-600">
              <tr>
                <th class="px-3 py-2">Marca</th>
                <th class="px-3 py-2">Especie</th>
                <th class="px-3 py-2">Capturas (fecha · recolector · transecto · hora)</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="h in histories" :key="h.id" class="border-t border-stone-100 align-top">
                <td class="px-3 py-1.5 font-medium">{{ h.id }}</td>
                <td class="px-3 py-1.5">
                  <i>{{ h.species }}</i> <span class="text-stone-500">{{ sexOf(h.events[0].row) }}</span>
                </td>
                <td class="px-3 py-1.5">
                  <span v-for="(e, i) in h.events" :key="e.row.id" class="whitespace-nowrap">
                    <template v-if="i">
                      → <span class="text-xs text-stone-500">({{ days(h.events[i - 1].date, e.date) }})</span>
                    </template>
                    {{ date(e.date) }} · {{ e.collector }}<template v-if="e.section"> · T{{ e.section }}</template
                    ><template v-if="e.minutes !== null"> · {{ formatMinutes(e.minutes) }}</template>
                  </span>
                </td>
              </tr>
            </tbody>
          </table>
        </div>
      </section>
    </div>
  </div>
</template>
