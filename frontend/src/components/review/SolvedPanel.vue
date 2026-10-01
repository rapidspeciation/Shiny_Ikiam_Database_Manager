<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { ArrowRight, ChevronLeft, ChevronRight, History, RefreshCw, Search } from 'lucide-vue-next'
import { api } from '../../lib/api'
import { errorText, notify } from '../../lib/notice'
import { cellText, stamp, tablesLink, type SolvedItem, type SolvedPage } from '../../lib/review'
import { t, tx } from '../../lib/i18n'

/**
 * Revisión → Resueltos: problems of the checks and suggested edits that the
 * sheet no longer has, newest first: what it was, since when it was seen, when
 * it went away and, when the history knows, who changed what (with a link to
 * that save in Historial). Kept by the server (server/findings.mjs) every time
 * it looks again after the copy of the workbook changed.
 */
const route = useRoute()
const router = useRouter()
const PAGE = 50
const q = (key: string) => String(route.query[key] ?? '')
const type = ref(q('tipo'))
const search = ref(q('q'))
const offset = ref(0)
const page = ref<SolvedPage | null>(null)
const loading = ref(false)

async function load() {
  loading.value = true
  try {
    const params = new URLSearchParams({ limit: String(PAGE), offset: String(offset.value) })
    if (type.value) params.set('type', type.value)
    if (search.value) params.set('q', search.value)
    page.value = await api<SolvedPage>(`solved?${params}`)
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    loading.value = false
  }
}
let typing: ReturnType<typeof setTimeout>
watch(type, () => (offset.value ? (offset.value = 0) : load()))
watch(search, () => {
  clearTimeout(typing)
  typing = setTimeout(() => (offset.value ? (offset.value = 0) : load()), 300)
})
watch(offset, load)
watch([type, search], () => {
  if (route.path !== '/revision' || route.query.vista !== 'resueltos') return
  const query: Record<string, string> = { vista: 'resueltos', tipo: type.value, q: search.value }
  router.replace({ query: Object.fromEntries(Object.entries(query).filter(([, v]) => v)) })
})
watch(
  () => route.query,
  () => {
    if (route.path !== '/revision' || route.query.vista !== 'resueltos') return
    if (type.value !== q('tipo')) type.value = q('tipo')
    if (search.value !== q('q')) search.value = q('q')
  },
)
load()

const TYPES = [
  { key: '', label: 'Todo' },
  { key: 'check', label: 'Problemas' },
  { key: 'suggestion', label: 'Sugerencias' },
]
const kindTitle = (item: SolvedItem) => {
  const title = page.value?.titles[item.type]?.[item.kind]
  return title ? t(title) : item.kind
}
/** A suggestion's values when it was listed. */
const suggestion = (item: SolvedItem) =>
  item.type === 'suggestion' && item.value && typeof item.value === 'object'
    ? (item.value as { current: unknown; suggested: unknown })
    : null
/** Who solved it: the person, Google Sheets (no name), or nobody the history knows. */
const who = (item: SolvedItem) =>
  item.solved.user ?? (item.solved.actionId ? 'Google Sheets' : t('sin registro en el historial'))
const counts = computed(() => page.value?.open ?? {})
</script>

<template>
  <section class="flex min-h-0 min-w-0 flex-1 flex-col">
    <div class="flex flex-wrap items-center gap-2 border-b border-stone-200 bg-white px-2 py-1.5 text-xs sm:px-3">
      <div class="flex gap-1" role="group" :aria-label="$t('Tipo')">
        <button
          v-for="opt in TYPES"
          :key="opt.key"
          class="rounded px-2 py-1"
          :class="type === opt.key ? 'bg-brand-700 text-white' : 'bg-stone-100 text-stone-700 hover:bg-stone-200'"
          @click="type = opt.key"
        >
          {{ $t(opt.label) }}
        </button>
      </div>
      <span class="relative min-w-0 flex-1 md:max-w-md">
        <Search :size="14" class="absolute top-2 left-2 text-stone-400" />
        <input v-model="search" type="search" class="field-input py-1 pl-7 text-sm" :placeholder="$t('Buscar ID, valor o problema…')" />
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
          'Lo que la hoja ya no tiene: un problema o una sugerencia sale de su lista cuando los chequeos dejan de encontrarlo. Abiertos ahora: {checks} problemas, {suggestions} sugerencias.',
          { checks: counts.check ?? 0, suggestions: counts.suggestion ?? 0 },
        )
      }}
    </p>
    <div class="min-h-0 flex-1 overflow-auto bg-stone-50">
      <p v-if="!page" class="p-6 text-sm text-stone-500">{{ $t('Revisando…') }}</p>
      <p v-else-if="!page.items.length" class="p-6 text-sm text-stone-500">
        {{ $t('Nada resuelto todavía con estos filtros. Aparece aquí cuando la hoja cambia y el chequeo ya no lo encuentra.') }}
      </p>
      <ul v-else class="divide-y divide-stone-200 bg-white">
        <li v-for="item in page.items" :key="`${item.type}:${item.key}`" class="space-y-1 px-3 py-2 text-sm">
          <div class="flex flex-wrap items-center gap-x-2 gap-y-0.5 text-xs">
            <span class="rounded bg-brand-100 px-1.5 py-0.5 font-medium text-brand-800">{{ $t('Resuelto') }}</span>
            <span class="rounded bg-stone-800 px-1.5 py-0.5 text-white">{{ kindTitle(item) }}</span>
            <a
              v-if="item.sheet && item.row && !item.solved.rowGone"
              :href="tablesLink(item.sheet, item.label ?? '')"
              class="text-brand-700 hover:underline"
              :title="$t('Abrir la fila en Tablas')"
              >{{ $t('{sheet} fila {row}', { sheet: item.sheet, row: item.row }) }}</a
            >
            <span v-else-if="item.sheet" class="text-stone-500">{{ item.sheet }}</span>
            <strong class="font-mono">{{ item.label }}</strong>
            <span v-if="item.field" class="text-stone-500">{{ item.field }}</span>
          </div>
          <p class="text-stone-900">{{ item.text ? tx(item.text, item.textMsg) : '' }}</p>
          <p v-if="suggestion(item)" class="text-xs text-stone-600">
            {{ $t('Sugerido:') }} {{ cellText(suggestion(item)!.current) }} → {{ cellText(suggestion(item)!.suggested) }}
          </p>
          <p class="flex flex-wrap items-center gap-x-3 gap-y-0.5 text-xs text-stone-600">
            <span>{{ $t('visto desde {date}', { date: stamp(item.firstSeen) }) }}</span>
            <span
              >{{ $t('resuelto el {date}', { date: stamp(item.solvedAt) }) }} · <strong>{{ who(item) }}</strong></span
            >
            <span v-if="item.solved.field" class="inline-flex items-center gap-1">
              {{ item.solved.field }}: {{ cellText(item.solved.before) }} <ArrowRight :size="12" class="text-stone-400" />
              {{ cellText(item.solved.after) }}
            </span>
            <span v-else-if="item.solved.rowGone">{{ $t('la fila ya no está en la hoja') }}</span>
            <span v-else-if="item.field">{{ $t('ahora: {value}', { value: String(cellText(item.solved.now)) }) }}</span>
            <a
              v-if="item.solved.actionId"
              :href="`#/historial?accion=${encodeURIComponent(item.solved.actionId)}`"
              class="inline-flex items-center gap-0.5 text-brand-700 hover:underline"
              ><History :size="12" /> {{ $t('ver en Historial') }}</a
            >
          </p>
        </li>
      </ul>
    </div>
  </section>
</template>
