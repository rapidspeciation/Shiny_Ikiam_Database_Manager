<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { ArrowRight, ChevronLeft, ChevronRight, ClipboardCopy, Download, RefreshCw, Search, SlidersHorizontal, X } from 'lucide-vue-next'
import ChoiceField from '../ChoiceField.vue'
import { api } from '../../lib/api'
import { errorText, notify } from '../../lib/notice'
import {
  CERTAINTIES,
  cellText,
  certaintyOf,
  stamp,
  tablesLink,
  type Certainty,
  type SuggestionPage,
} from '../../lib/review'
import { t, tn, tx } from '../../lib/i18n'

/**
 * Revisión → Sugerencias: corrections the app computes from the workbook,
 * grouped by where they come from, each with how sure it is and why. Read
 * only: there is no apply button, so nothing is written by accident. The list
 * can be downloaded (CSV) or copied to paste into a sheet; to make the changes,
 * a person asks the assistant for an ordinary proposal from the ones they chose.
 * The filters live in the address, like the rest of Revisión.
 */
const route = useRoute()
const router = useRouter()
const PAGE = 100

const q = (key: string) => String(route.query[key] ?? '')
const source = ref(q('fuente'))
const certainty = ref(q('certeza'))
const sheet = ref(q('hoja'))
const search = ref(q('q'))
const offset = ref(0)
const page = ref<SuggestionPage | null>(null)
const loading = ref(false)
const showFilters = ref(false)
const list = ref<HTMLElement>()

function params(extra: Record<string, string> = {}) {
  const out = new URLSearchParams({ limit: String(PAGE), offset: String(offset.value), ...extra })
  const map: Record<string, string> = { source: source.value, certainty: certainty.value, sheet: sheet.value, q: search.value }
  for (const [k, v] of Object.entries(map)) if (v) out.set(k, v)
  return out
}
async function load() {
  loading.value = true
  try {
    page.value = await api<SuggestionPage>(`suggested-edits?${params()}`)
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    loading.value = false
  }
}
let typing: ReturnType<typeof setTimeout>
watch([source, certainty, sheet], () => (offset.value ? (offset.value = 0) : load()))
watch(search, () => {
  clearTimeout(typing)
  typing = setTimeout(() => (offset.value ? (offset.value = 0) : load()), 300)
})
watch(offset, () => {
  load()
  list.value?.scrollTo({ top: 0 })
})
watch([source, certainty], () => (showFilters.value = false))
// The address keeps the view and its filters (only while this view is open).
watch([source, certainty, sheet, search], () => {
  if (route.path !== '/revision' || route.query.vista !== 'sugerencias') return
  const query: Record<string, string> = { vista: 'sugerencias', fuente: source.value, certeza: certainty.value, hoja: sheet.value, q: search.value }
  router.replace({ query: Object.fromEntries(Object.entries(query).filter(([, v]) => v)) })
})
// A link from elsewhere (Inicio, a chat) sets its filters while the view is open.
watch(
  () => route.query,
  () => {
    if (route.path !== '/revision' || route.query.vista !== 'sugerencias') return
    for (const [target, key] of [
      [source, 'fuente'],
      [certainty, 'certeza'],
      [sheet, 'hoja'],
      [search, 'q'],
    ] as const)
      if (target.value !== q(key)) target.value = q(key)
  },
)
load()

const sources = computed(() => page.value?.sources ?? [])
const titleOf = (id: string) => {
  const found = sources.value.find(s => s.id === id)
  return found ? t(found.title) : id
}
const chosen = computed(() => sources.value.find(s => s.id === source.value))
/** Counts per certainty for the chosen source, or all of them. */
const certaintyCounts = computed(() => {
  const out: Record<string, number> = { certain: 0, likely: 0, check: 0, total: 0 }
  for (const s of chosen.value ? [chosen.value] : sources.value)
    for (const k of Object.keys(out)) out[k] += s.counts[k as Certainty | 'total'] ?? 0
  return out
})
const sheetOptions = computed(() => page.value?.sheets ?? [])
/** The list with a heading where the source changes, and one per group in a source listed by group. */
const grouped = computed(() => new Set(sources.value.filter(s => s.byGroup).map(s => s.id)))
const rows = computed(() =>
  (page.value?.items ?? []).map((item, i, all) => {
    const heading = i === 0 || all[i - 1].source !== item.source
    const group =
      grouped.value.has(item.source) && item.group && (heading || all[i - 1].group !== item.group)
        ? { name: item.group, counts: page.value?.groups?.[item.group] }
        : null
    return { item, heading, group }
  }),
)

async function copy() {
  try {
    const response = await fetch(`api/suggested-edits/csv?${params({ format: 'tsv' })}`, { credentials: 'same-origin' })
    if (!response.ok) throw new Error(response.statusText)
    const text = await response.text()
    await navigator.clipboard.writeText(text)
    const n = Math.max(text.trim().split('\n').length - 1, 0)
    notify(tn(n, '{n} sugerencia copiada: pégala en una hoja', '{n} sugerencias copiadas: pégalas en una hoja'), 'success')
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
const csvLink = computed(() => `api/suggested-edits/csv?${params()}`)
const filtered = computed(() => !!(source.value || certainty.value || sheet.value || search.value))
function clearFilters() {
  source.value = certainty.value = sheet.value = search.value = ''
}
</script>

<template>
  <div class="flex min-h-0 flex-1">
    <aside
      class="w-60 shrink-0 flex-col overflow-y-auto border-r border-stone-200 bg-white text-sm"
      :class="showFilters ? 'fixed inset-0 z-40 flex w-full md:static md:w-60' : 'hidden md:flex'"
    >
      <div class="flex items-center justify-between px-3 pt-3 md:hidden">
        <strong>{{ $t('Filtros') }}</strong>
        <button class="btn-ghost" @click="showFilters = false"><X :size="16" /></button>
      </div>
      <div class="grid grid-cols-2 gap-1 p-2" role="group" :aria-label="$t('Certeza')">
        <button
          class="rounded px-1.5 py-1 text-xs leading-tight"
          :class="!certainty ? 'bg-brand-700 text-white' : 'bg-stone-100 text-stone-700 hover:bg-stone-200'"
          @click="certainty = ''"
        >
          {{ $t('Todas') }}<span class="block tabular-nums opacity-80">{{ certaintyCounts.total }}</span>
        </button>
        <button
          v-for="c in CERTAINTIES"
          :key="c.key"
          class="rounded px-1.5 py-1 text-xs leading-tight"
          :class="certainty === c.key ? 'bg-brand-700 text-white' : 'bg-stone-100 text-stone-700 hover:bg-stone-200'"
          :title="$t(c.hint)"
          @click="certainty = certainty === c.key ? '' : c.key"
        >
          {{ $t(c.label) }}<span class="block tabular-nums opacity-80">{{ certaintyCounts[c.key] }}</span>
        </button>
      </div>
      <nav class="border-t border-stone-100 py-1">
        <button
          class="flex w-full items-center justify-between px-3 py-1 text-left hover:bg-stone-50"
          :class="!source ? 'bg-stone-100 font-medium' : ''"
          @click="source = ''"
        >
          {{ $t('Todo') }}
          <span class="text-xs tabular-nums text-stone-500">{{ page ? sources.reduce((n, s) => n + s.counts.total, 0) : '…' }}</span>
        </button>
        <button
          v-for="s in sources"
          :key="s.id"
          class="flex w-full items-center justify-between gap-2 px-3 py-1 text-left hover:bg-stone-50"
          :class="source === s.id ? 'bg-brand-50 font-medium text-brand-800' : 'text-stone-700'"
          :title="$t(s.describe)"
          @click="source = source === s.id ? '' : s.id"
        >
          <span class="truncate">{{ $t(s.title) }}</span>
          <span class="text-xs tabular-nums text-stone-500">{{ s.counts.total }}</span>
        </button>
      </nav>
      <div class="space-y-2 border-t border-stone-100 p-3">
        <label class="block">
          <span class="field-label">{{ $t('Hoja') }}</span>
          <ChoiceField
            v-model="sheet"
            class="field-input"
            :options="sheetOptions"
            :freetext="false"
            allow-empty
            :placeholder="$t('Todas')"
          />
        </label>
        <div class="flex flex-wrap gap-1">
          <button v-if="filtered" class="btn flex-1" :title="$t('Quitar los filtros')" @click="clearFilters">
            <X :size="15" /> {{ $t('Quitar') }}
          </button>
          <a :href="csvLink" class="btn" :title="$t('Descargar la lista filtrada (CSV)')"><Download :size="15" /> CSV</a>
          <button class="btn" :title="$t('Copiar la lista filtrada para pegarla en una hoja')" @click="copy">
            <ClipboardCopy :size="15" /> {{ $t('Copiar') }}
          </button>
        </div>
      </div>
    </aside>

    <section class="flex min-w-0 flex-1 flex-col">
      <div class="flex items-center gap-2 border-b border-stone-200 bg-white px-2 py-1.5 text-xs sm:px-3">
        <button class="btn px-2 py-1 md:hidden" :class="{ 'bg-stone-800 text-white': filtered }" @click="showFilters = true">
          <SlidersHorizontal :size="14" /> {{ $t('Filtros') }}
        </button>
        <span class="relative min-w-0 flex-1 md:max-w-md">
          <Search :size="14" class="absolute top-2 left-2 text-stone-400" />
          <input v-model="search" type="search" class="field-input py-1 pl-7 text-sm" :placeholder="$t('Buscar ID, valor o motivo…')" />
        </span>
        <span v-if="page" class="ml-auto tabular-nums whitespace-nowrap text-stone-600">{{
          page.total
            ? $t('{from}–{to} de {total}', {
                from: page.offset + 1,
                to: Math.min(page.offset + page.limit, page.total),
                total: page.total,
              })
            : '0'
        }}</span>
        <button class="btn-ghost" :disabled="!page?.offset" :title="$t('Anteriores')" @click="offset = Math.max(0, offset - PAGE)">
          <ChevronLeft :size="16" />
        </button>
        <button
          class="btn-ghost"
          :disabled="!page || page.offset + page.limit >= page.total"
          :title="$t('Siguientes')"
          @click="offset += PAGE"
        >
          <ChevronRight :size="16" />
        </button>
        <button class="btn-ghost" :disabled="loading" :title="$t('Volver a revisar')" @click="load">
          <RefreshCw :size="15" :class="{ 'animate-spin': loading }" />
        </button>
      </div>
      <p class="border-b border-stone-200 bg-stone-100 px-3 py-1.5 text-xs text-stone-700">
        {{
          $t(
            'Solo para mirar: nada se aplica desde aquí. Para corregir, pídele al asistente una propuesta con las que elijas (por ejemplo «propón las sugerencias seguras de tubos»); la revisas y la confirmas como siempre.',
          )
        }}
      </p>
      <p v-if="chosen" class="border-b border-stone-200 bg-white px-3 py-1.5 text-xs text-stone-600">{{ $t(chosen.describe) }}</p>

      <div ref="list" class="min-h-0 flex-1 overflow-auto bg-stone-50">
        <p v-if="!page" class="p-6 text-sm text-stone-500">{{ $t('Revisando…') }}</p>
        <p v-else-if="!page.items.length" class="p-6 text-sm text-stone-500">{{ $t('No hay sugerencias con estos filtros.') }}</p>
        <ul v-else class="divide-y divide-stone-200 bg-white">
          <template v-for="{ item, heading, group } in rows" :key="item.key">
            <li v-if="heading" class="sticky top-0 z-10 bg-stone-100 px-3 py-1 text-xs font-semibold tracking-wide text-stone-600 uppercase">
              {{ titleOf(item.source) }}
            </li>
            <li v-if="group" class="flex flex-wrap items-baseline gap-x-3 bg-stone-50 px-3 py-1 text-xs text-stone-700">
              <strong class="font-mono">{{ group.name }}</strong>
              <span v-if="group.counts" class="tabular-nums text-stone-500">{{
                [
                  tn(group.counts.total, '{n} fila', '{n} filas'),
                  ...CERTAINTIES.filter(c => group.counts?.[c.key]).map(c => `${$t(c.label)} ${group.counts?.[c.key]}`),
                ].join(' · ')
              }}</span>
            </li>
            <li class="flex flex-wrap items-baseline gap-x-3 gap-y-1 px-3 py-2 text-sm">
              <span
                class="w-20 shrink-0 rounded px-1.5 py-0.5 text-center text-xs font-medium"
                :class="certaintyOf(item.certainty).tone"
                :title="$t(certaintyOf(item.certainty).hint)"
                >{{ $t(certaintyOf(item.certainty).label) }}</span
              >
              <a
                :href="tablesLink(item.sheet, item.label)"
                class="shrink-0 text-brand-700 hover:underline"
                :title="$t('Abrir la fila en Tablas')"
                >{{ $t('{sheet} fila {row}', { sheet: item.sheet, row: item.row }) }}</a
              >
              <strong class="shrink-0 font-mono text-xs">{{ item.label }}</strong>
              <span class="shrink-0 text-xs text-stone-500">{{ item.field }}</span>
              <span class="flex min-w-0 items-baseline gap-1.5">
                <span class="break-all text-red-900 line-through decoration-red-300">{{ cellText(item.current) }}</span>
                <ArrowRight :size="13" class="shrink-0 self-center text-stone-400" />
                <span v-if="item.suggested !== null" class="font-medium break-all text-emerald-900"
                  ><span
                    v-if="item.formula"
                    class="mr-1 rounded border border-emerald-700 px-0.5 text-[11px] font-normal"
                    :title="$t('Una fórmula: el asistente puede proponerla, con las demás de su grupo')"
                    >ƒx</span
                  >{{ cellText(item.suggested) }}</span
                >
                <span v-else class="text-amber-800 italic">{{ $t('decidir') }}</span>
              </span>
              <span
                v-if="item.manual"
                class="shrink-0 rounded bg-stone-200 px-1.5 py-0.5 text-[11px] text-stone-700"
                :title="$t('Es una fórmula: se hace a mano en Google Sheets; las propuestas del asistente no la escriben')"
                >{{ $t('en Google Sheets') }}</span
              >
              <span class="basis-full text-xs text-stone-600 md:basis-auto md:flex-1">{{ tx(item.reason, item.reasonMsg) }}</span>
              <span v-if="item.firstSeen" class="shrink-0 text-[11px] text-stone-400" :title="$t('Visto por primera vez')">{{
                $t('desde {date}', { date: stamp(item.firstSeen, false) })
              }}</span>
            </li>
          </template>
        </ul>
      </div>
    </section>
  </div>
</template>
