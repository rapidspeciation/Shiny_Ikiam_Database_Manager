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

const allKinds = computed(() =>
  page.value ? Object.entries(page.value.kinds).filter(([k]) => page.value!.counts[k] || k === kind.value) : [],
)
const allCount = computed(() => (page.value ? Object.values(page.value.counts).reduce((a, b) => a + b, 0) : 0))
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
    <template v-else>
      <div class="toolbar">
        <div class="flex flex-wrap gap-1" role="group" aria-label="Estado">
          <button
            v-for="s in STATUSES"
            :key="s.key"
            class="rounded-full px-2.5 py-1 text-xs whitespace-nowrap"
            :class="status === s.key ? 'bg-brand-700 text-white' : 'bg-stone-100 text-stone-700 hover:bg-stone-200'"
            @click="status = s.key"
          >
            {{ s.label }}<template v-if="page && s.key !== 'all'"> ({{ page.statuses[s.key] ?? 0 }})</template>
          </button>
          <!-- Phones: the other filters behind one button, so the cards keep the screen. -->
          <button
            class="rounded-full px-2.5 py-1 text-xs whitespace-nowrap md:hidden"
            :class="showFilters || filtered ? 'bg-stone-800 text-white' : 'bg-stone-100 text-stone-700'"
            @click="showFilters = !showFilters"
          >
            <SlidersHorizontal :size="12" class="inline" /> Filtros
          </button>
        </div>
        <div class="w-full flex-wrap items-end gap-3 md:contents" :class="showFilters ? 'flex' : 'hidden'">
          <label class="w-44">
            <span class="field-label">Hoja</span>
            <ChoiceField
              v-model="sheet"
              class="field-input"
              :options="sheetOptions"
              :freetext="false"
              allow-empty
              placeholder="Todas"
            />
          </label>
          <label class="w-52">
            <span class="field-label">Colector o identificador</span>
            <ChoiceField
              v-model="person"
              class="field-input"
              :options="personOptions"
              :freetext="false"
              allow-empty
              placeholder="Todos"
            />
          </label>
          <label class="w-36">
            <span class="field-label">Desde</span>
            <DateField v-model="from" class="field-input" />
          </label>
          <label class="w-36">
            <span class="field-label">Hasta</span>
            <DateField v-model="to" class="field-input" />
          </label>
          <label class="min-w-40 flex-1">
            <span class="field-label">Buscar</span>
            <span class="relative block">
              <Search :size="15" class="absolute top-2.5 left-2.5 text-stone-400" />
              <input v-model="search" type="search" class="field-input pl-8" placeholder="CAM, ID, especie…" />
            </span>
          </label>
          <div class="flex gap-1 pb-0.5">
            <button v-if="filtered" class="btn" title="Quitar los filtros" @click="clearFilters"><X :size="15" /></button>
            <button class="btn" :disabled="loading" title="Volver a revisar" @click="load">
              <RefreshCw :size="15" :class="{ 'animate-spin': loading }" />
            </button>
            <a
              href="api/review/labels"
              class="btn"
              title="Descargar los veredictos sobre lecturas de fotos (etiquetas de entrenamiento)"
            >
              <Download :size="15" />
            </a>
          </div>
        </div>
      </div>

      <!-- One scrolling row of kinds on phones, so the cards keep the screen. -->
      <div
        class="flex items-center gap-1.5 overflow-x-auto border-b border-stone-200 bg-white px-3 py-2 text-xs sm:px-4 md:flex-wrap"
      >
        <button
          class="shrink-0 rounded-full px-2.5 py-1 whitespace-nowrap"
          :class="!kind ? 'bg-stone-800 text-white' : 'bg-stone-100 text-stone-700 hover:bg-stone-200'"
          @click="kind = ''"
        >
          Todo ({{ page ? allCount : '…' }})
        </button>
        <button
          v-for="[key, label] in allKinds"
          :key="key"
          class="shrink-0 rounded-full px-2.5 py-1 whitespace-nowrap"
          :class="kind === key ? 'bg-stone-800 text-white' : 'bg-stone-100 text-stone-700 hover:bg-stone-200'"
          @click="kind = kind === key ? '' : key"
        >
          {{ label }} ({{ page?.counts[key] ?? 0 }})
        </button>
      </div>

      <div
        v-if="page && (page.agreed.fixes || page.agreed.tasks)"
        class="flex flex-wrap items-center gap-x-3 gap-y-1 border-b border-emerald-200 bg-emerald-50 px-3 py-2 text-sm text-emerald-950 sm:px-4"
      >
        <strong
          >{{ page.agreed.fixes }} {{ page.agreed.fixes === 1 ? 'arreglo aceptado listo' : 'arreglos aceptados listos' }} para
          aplicar</strong
        >
        <span v-if="page.agreed.tasks"
          >· {{ page.agreed.tasks }} {{ page.agreed.tasks === 1 ? 'tarea' : 'tareas' }} en Drive</span
        >
        <span class="text-emerald-800">Pídele en T3: «aplica las correcciones acordadas»</span>
        <button
          v-if="page.agreed.fixes"
          class="btn ml-auto"
          :disabled="busy"
          title="Una propuesta con todos, para confirmar en Asistente"
          @click="prepare"
        >
          <Wand2 :size="15" /> Preparar propuesta aquí
        </button>
      </div>

      <p v-if="group" class="flex items-center gap-2 bg-stone-100 px-3 py-1.5 text-xs sm:px-4">
        Solo el lote «{{ groupLabel }}»
        <button class="text-brand-700 hover:underline" @click="group = ''">ver todos</button>
      </p>

      <div ref="list" class="min-h-0 flex-1 overflow-auto bg-stone-50">
        <div class="mx-auto max-w-6xl space-y-3 p-2 sm:p-4">
          <p v-if="!page" class="p-6 text-sm text-stone-500">Revisando…</p>
          <p v-else-if="!page.issues.length" class="p-6 text-sm text-stone-500">
            No hay nada {{ STATUSES.find(s => s.key === status)?.label.toLowerCase() }} con estos filtros.
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

      <div v-if="page" class="flex flex-wrap items-center gap-2 border-t border-stone-200 bg-white px-3 py-2 text-xs sm:px-4">
        <span>{{
          page.total ? `${page.offset + 1}–${Math.min(page.offset + page.limit, page.total)} de ${page.total}` : '0'
        }}</span>
        <button class="btn-ghost" :disabled="!page.offset" title="Anteriores" @click="offset = Math.max(0, offset - PAGE)">
          <ChevronLeft :size="15" />
        </button>
        <button class="btn-ghost" :disabled="page.offset + page.limit >= page.total" title="Siguientes" @click="offset += PAGE">
          <ChevronRight :size="15" />
        </button>
        <span class="hint ml-auto hidden md:inline">
          Revisado {{ new Date(page.checkedAt).toLocaleTimeString('es-EC', { hour: '2-digit', minute: '2-digit' }) }} · el
          asistente ve la misma lista (check_data, list_agreed_fixes)
        </span>
      </div>
    </template>
    <PhotoViewer v-if="viewer" v-model="viewer.index" :photos="viewer.photos" @close="viewer = null" />
  </div>
</template>
