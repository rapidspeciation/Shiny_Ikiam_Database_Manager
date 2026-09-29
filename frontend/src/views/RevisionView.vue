<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { ChevronLeft, ChevronRight, Download, RefreshCw, Search, SlidersHorizontal, Wand2, X } from 'lucide-vue-next'
import ChoiceField from '../components/ChoiceField.vue'
import DateField from '../components/DateField.vue'
import IssueCard from '../components/review/IssueCard.vue'
import PhotoViewer from '../components/review/PhotoViewer.vue'
import { api } from '../lib/api'
import { errorText, notify } from '../lib/notice'
import { STATUSES, type Issue, type ReviewPage, type Verdict } from '../lib/review'
import { useSession } from '../stores/session'

/**
 * Revisión: every inconsistency of the workbook and of the specimen photos as a
 * card, judged by people (aceptar / rechazar / otro valor). Accepted fixes wait
 * until someone asks T3 ("aplica las correcciones acordadas") or prepares the
 * proposal here; either way a person confirms it before anything is written.
 * The filters live in the address, so a view can be shared or linked (Tablas).
 */
const session = useSession()
const route = useRoute()
const router = useRouter()
const PAGE = 25

const q = (key: string) => String(route.query[key] ?? '')
const kind = ref(q('tipo'))
const sheet = ref(q('hoja'))
const person = ref(q('persona'))
const from = ref(q('desde'))
const to = ref(q('hasta'))
const status = ref(q('estado') || 'pending')
const group = ref(q('lote'))
const search = ref(q('q'))
const offset = ref(0)
const page = ref<ReviewPage | null>(null)
const loading = ref(false)
const busy = ref(false)
const list = ref<HTMLElement>()
const showFilters = ref(false)
const viewer = ref<{ photos: { id: string; name: string }[]; index: number } | null>(null)

const filters = computed(() => ({
  tipo: kind.value,
  hoja: sheet.value,
  persona: person.value,
  desde: from.value,
  hasta: to.value,
  estado: status.value === 'pending' ? '' : status.value,
  lote: group.value,
  q: search.value,
}))
function params(extra: Record<string, string> = {}) {
  const out = new URLSearchParams({ limit: String(PAGE), offset: String(offset.value), status: status.value, ...extra })
  const map: Record<string, string> = {
    kind: kind.value,
    sheet: sheet.value,
    person: person.value,
    from: from.value,
    to: to.value,
    group: group.value,
    q: search.value,
  }
  for (const [k, v] of Object.entries(map)) if (v) out.set(k, v)
  return out
}
async function load() {
  loading.value = true
  try {
    page.value = await api<ReviewPage>(`review?${params()}`)
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    loading.value = false
  }
}
/** Counts and the "ready to apply" number after a verdict, keeping the cards on screen (who decided stays visible). */
async function refreshCounts() {
  try {
    const fresh = await api<ReviewPage>(`review?${params({ limit: '1', offset: '0' })}`)
    if (page.value) Object.assign(page.value, { counts: fresh.counts, statuses: fresh.statuses, agreed: fresh.agreed })
  } catch {
    /* The next load shows them. */
  }
}

let typing: ReturnType<typeof setTimeout>
watch([kind, sheet, person, from, to, status, group], () => {
  if (offset.value) offset.value = 0
  else load()
})
watch(search, () => {
  clearTimeout(typing)
  typing = setTimeout(() => (offset.value ? (offset.value = 0) : load()), 300)
})
watch(offset, () => {
  load()
  list.value?.scrollTo({ top: 0 })
})
watch(filters, value => {
  const query = Object.fromEntries(Object.entries(value).filter(([, v]) => v))
  if (route.path === '/revision') router.replace({ query })
})
// A link from elsewhere (Tablas → Revisión de datos) sets exactly its filters; the tab's own
// address updates (above) give back the same values, so they change nothing.
watch(
  () => route.query,
  () => {
    if (route.path !== '/revision') return
    const set = (target: { value: string }, value: string) => {
      if (target.value !== value) target.value = value
    }
    set(kind, q('tipo'))
    set(sheet, q('hoja'))
    set(person, q('persona'))
    set(from, q('desde'))
    set(to, q('hasta'))
    set(status, q('estado') || 'pending')
    set(group, q('lote'))
    set(search, q('q'))
  },
)
load()
// On a phone, picking a kind or status goes back to the cards.
watch([kind, status], () => (showFilters.value = false))

const allKinds = computed(() =>
  page.value ? Object.entries(page.value.kinds).filter(([k]) => page.value!.counts[k] || k === kind.value) : [],
)
const allCount = computed(() => (page.value ? Object.values(page.value.counts).reduce((a, b) => a + b, 0) : 0))
// The sidebar lists the sheet's own checks, then the ones read from photos and envelopes.
const PHOTO_KIND = /^(photo_|envelope_|ai_)/
const kindGroups = computed(() => [
  { title: 'Datos de la hoja', kinds: allKinds.value.filter(([k]) => !PHOTO_KIND.test(k)) },
  { title: 'Fotos y sobres', kinds: allKinds.value.filter(([k]) => PHOTO_KIND.test(k)) },
])
const sheetOptions = computed(() => page.value?.sheets ?? [])
const personOptions = computed(() => (page.value?.people ?? []).map(p => ({ value: p.name, label: p.name, hint: String(p.n) })))
const groupLabel = computed(
  () => page.value?.issues.find(i => i.group?.key === group.value)?.group?.label ?? group.value.split(':').slice(1).join(':'),
)

function applyVerdict(ids: Set<string>, verdict: Verdict) {
  for (const issue of page.value?.issues ?? []) if (ids.has(issue.id)) issue.verdict = verdict
}
async function judge(issue: Issue, verdict: string, value?: string, comment?: string) {
  busy.value = true
  try {
    const out = await api<{ ids: string[]; verdict: Verdict }>('review/verdicts', {
      method: 'POST',
      body: { ids: [issue.id], verdict, value, comment },
    })
    applyVerdict(new Set(out.ids), out.verdict)
    refreshCounts()
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    busy.value = false
  }
}
async function judgeBatch(issue: Issue, verdict: string) {
  if (!issue.group) return
  if (!confirm(`${verdict === 'accepted' ? 'Aceptar' : 'Rechazar'} los ${issue.group.size} del lote «${issue.group.label}»?`))
    return
  busy.value = true
  try {
    const out = await api<{ ids: string[]; saved: number; verdict: Verdict }>('review/verdicts', {
      method: 'POST',
      body: { group: issue.group.key, verdict },
    })
    applyVerdict(new Set(out.ids), out.verdict)
    notify(`${out.saved} ${verdict === 'accepted' ? 'aceptados' : 'rechazados'}`, 'success')
    refreshCounts()
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    busy.value = false
  }
}
async function prepare() {
  busy.value = true
  try {
    const out = await api<{ rows: number; tasks: number }>('chat/proposals/from-review', { method: 'POST', body: {} })
    notify(
      `Propuesta de ${out.rows} ${out.rows === 1 ? 'fila' : 'filas'}: confírmala en Asistente → Cambios propuestos`,
      'success',
    )
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    busy.value = false
  }
}
function openRow(sheetName: string, label: string) {
  router.push({ path: '/tablas', query: { hoja: sheetName, buscar: label } })
}
function clearFilters() {
  kind.value = sheet.value = person.value = from.value = to.value = group.value = search.value = ''
  status.value = 'pending'
}
const filtered = computed(
  () => !!(kind.value || sheet.value || person.value || from.value || to.value || group.value || search.value),
)
</script>

<template>
  <div class="flex h-full flex-col">
    <div v-if="!session.canEdit" class="p-6 text-sm text-stone-600">La revisión es para quienes editan la hoja.</div>
    <div v-else class="flex min-h-0 flex-1">
      <!-- Sidebar: status, kinds and filters in the height the screen has to spare (on phones, behind «Filtros»). -->
      <aside
        class="w-60 shrink-0 flex-col overflow-y-auto border-r border-stone-200 bg-white text-sm"
        :class="showFilters ? 'fixed inset-0 z-40 flex w-full md:static md:w-60' : 'hidden md:flex'"
      >
        <div class="flex items-center justify-between px-3 pt-3 md:hidden">
          <strong>Filtros</strong>
          <button class="btn-ghost" @click="showFilters = false"><X :size="16" /></button>
        </div>
        <div class="grid grid-cols-3 gap-1 p-2" role="group" aria-label="Estado">
          <button
            v-for="st in STATUSES"
            :key="st.key"
            class="rounded px-1.5 py-1 text-xs leading-tight"
            :class="status === st.key ? 'bg-brand-700 text-white' : 'bg-stone-100 text-stone-700 hover:bg-stone-200'"
            @click="status = st.key"
          >
            {{ st.label }}<span v-if="page && st.key !== 'all'" class="block tabular-nums opacity-80">{{
              page.statuses[st.key] ?? 0
            }}</span>
          </button>
        </div>
        <nav class="border-t border-stone-100 py-1">
          <button
            class="flex w-full items-center justify-between px-3 py-1 text-left hover:bg-stone-50"
            :class="!kind ? 'bg-stone-100 font-medium' : ''"
            @click="kind = ''"
          >
            Todo <span class="text-xs tabular-nums text-stone-500">{{ page ? allCount : '…' }}</span>
          </button>
          <template v-for="g in kindGroups" :key="g.title">
            <p v-if="g.kinds.length" class="px-3 pt-2 pb-0.5 text-[11px] font-semibold tracking-wide text-stone-500 uppercase">
              {{ g.title }}
            </p>
            <button
              v-for="[key, label] in g.kinds"
              :key="key"
              class="flex w-full items-center justify-between gap-2 px-3 py-1 text-left hover:bg-stone-50"
              :class="kind === key ? 'bg-brand-50 font-medium text-brand-800' : 'text-stone-700'"
              @click="kind = kind === key ? '' : key"
            >
              <span class="truncate">{{ label }}</span>
              <span class="text-xs tabular-nums text-stone-500">{{ page?.counts[key] ?? 0 }}</span>
            </button>
          </template>
        </nav>
        <div class="space-y-2 border-t border-stone-100 p-3">
          <label class="block">
            <span class="field-label">Hoja</span>
            <ChoiceField v-model="sheet" class="field-input" :options="sheetOptions" :freetext="false" allow-empty placeholder="Todas" />
          </label>
          <label class="block">
            <span class="field-label">Colector o identificador</span>
            <ChoiceField v-model="person" class="field-input" :options="personOptions" :freetext="false" allow-empty placeholder="Todos" />
          </label>
          <div class="grid grid-cols-2 gap-2">
            <label class="block">
              <span class="field-label">Desde</span>
              <DateField v-model="from" class="field-input" />
            </label>
            <label class="block">
              <span class="field-label">Hasta</span>
              <DateField v-model="to" class="field-input" />
            </label>
          </div>
          <div class="flex gap-1">
            <button v-if="filtered" class="btn flex-1" title="Quitar los filtros" @click="clearFilters"><X :size="15" /> Quitar</button>
            <a href="api/review/labels" class="btn" title="Descargar los veredictos sobre lecturas de fotos (etiquetas de entrenamiento)">
              <Download :size="15" />
            </a>
          </div>
        </div>
      </aside>

      <section class="flex min-w-0 flex-1 flex-col">
        <!-- One slim bar: search, where you are in the list, the pages. -->
        <div class="flex items-center gap-2 border-b border-stone-200 bg-white px-2 py-1.5 text-xs sm:px-3">
          <button
            class="btn px-2 py-1 md:hidden"
            :class="{ 'bg-stone-800 text-white': filtered || kind }"
            @click="showFilters = true"
          >
            <SlidersHorizontal :size="14" /> Filtros
          </button>
          <span class="relative min-w-0 flex-1 md:max-w-md">
            <Search :size="14" class="absolute top-2 left-2 text-stone-400" />
            <input v-model="search" type="search" class="field-input py-1 pl-7 text-sm" placeholder="Buscar CAM, ID, especie…" />
          </span>
          <span v-if="page" class="ml-auto tabular-nums whitespace-nowrap text-stone-600">{{
            page.total ? `${page.offset + 1}–${Math.min(page.offset + page.limit, page.total)} de ${page.total}` : '0'
          }}</span>
          <button class="btn-ghost" :disabled="!page?.offset" title="Anteriores" @click="offset = Math.max(0, offset - PAGE)">
            <ChevronLeft :size="16" />
          </button>
          <button
            class="btn-ghost"
            :disabled="!page || page.offset + page.limit >= page.total"
            title="Siguientes"
            @click="offset += PAGE"
          >
            <ChevronRight :size="16" />
          </button>
          <button class="btn-ghost" :disabled="loading" title="Volver a revisar" @click="load">
            <RefreshCw :size="15" :class="{ 'animate-spin': loading }" />
          </button>
        </div>

        <div
          v-if="page && (page.agreed.fixes || page.agreed.tasks)"
          class="flex flex-wrap items-center gap-x-3 gap-y-1 border-b border-emerald-200 bg-emerald-50 px-3 py-1.5 text-sm text-emerald-950"
        >
          <strong
            >{{ page.agreed.fixes }} {{ page.agreed.fixes === 1 ? 'arreglo aceptado listo' : 'arreglos aceptados listos' }} para
            aplicar</strong
          >
          <span v-if="page.agreed.tasks">· {{ page.agreed.tasks }} {{ page.agreed.tasks === 1 ? 'tarea' : 'tareas' }} en Drive</span>
          <span class="text-emerald-800">Pídele en T3: «aplica las correcciones acordadas»</span>
          <button
            v-if="page.agreed.fixes"
            class="btn ml-auto py-1"
            :disabled="busy"
            title="Una propuesta con todos, para confirmar en Asistente"
            @click="prepare"
          >
            <Wand2 :size="15" /> Preparar propuesta aquí
          </button>
        </div>

        <p v-if="group" class="flex items-center gap-2 bg-stone-100 px-3 py-1 text-xs">
          Solo el lote «{{ groupLabel }}»
          <button class="text-brand-700 hover:underline" @click="group = ''">ver todos</button>
        </p>

        <div ref="list" class="min-h-0 flex-1 overflow-auto bg-stone-50">
          <div class="space-y-2 p-2">
            <p v-if="!page" class="p-6 text-sm text-stone-500">Revisando…</p>
            <p v-else-if="!page.issues.length" class="p-6 text-sm text-stone-500">
              No hay nada {{ STATUSES.find(st => st.key === status)?.label.toLowerCase() }} con estos filtros.
            </p>
            <IssueCard
              v-for="issue in page?.issues"
              :key="issue.id"
              :issue="issue"
              :kind-label="page?.kinds[issue.kind] ?? issue.kind"
              :can-edit="session.canEdit"
              :busy="busy"
              @verdict="judge"
              @batch="judgeBatch"
              @group="key => (group = key)"
              @photos="(photos, index) => (viewer = { photos, index })"
              @open="openRow"
            />
          </div>
        </div>
      </section>
    </div>
    <PhotoViewer v-if="viewer" v-model="viewer.index" :photos="viewer.photos" @close="viewer = null" />
  </div>
</template>
