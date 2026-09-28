<script setup lang="ts">
import { computed, onActivated, onBeforeUnmount, onDeactivated, onMounted, ref, watch } from 'vue'
import { RouterLink, useRoute, useRouter } from 'vue-router'
import {
  AlertTriangle,
  Camera,
  Check,
  ChevronDown,
  ChevronRight,
  History,
  ImagePlus,
  Loader2,
  MessageSquare,
  RotateCcw,
  X,
} from 'lucide-vue-next'
import ChoiceField from '../components/ChoiceField.vue'
import PagePhoto from '../components/notebook/PagePhoto.vue'
import NotebookGrid from '../components/notebook/NotebookGrid.vue'
import { displayValue } from '../lib/cells'
import { notify } from '../lib/notice'
import { persistentRef } from '../lib/persist'
import { KINDS, cellTitle, jobState, keyRange, nextJob, warningText, type Kind } from '../lib/notebook'
import { useNotebook } from '../stores/notebook'
import { useSession } from '../stores/session'

/**
 * "Digitalizar cuaderno": photograph notebook pages (several at once; each is
 * read in the background while you photograph the next), then review each
 * page beside its photo and apply the chosen rows to the sheet.
 */
const store = useNotebook()
const session = useSession()
const route = useRoute()
const router = useRouter()
const kind = persistentRef<Kind | 'auto'>('notebook:kind', 'auto', { lasting: true })
const year = ref<number | null>(null)
const current = persistentRef<string | null>('notebook:job', null)
const selected = ref<number | null>(null)
const cell = ref<{ n: number; field: string | null } | null>(null)
const showHistory = ref(false)
const photoOpen = ref(true)
const applying = ref(false)
const camera = ref<HTMLInputElement>()
const gallery = ref<HTMLInputElement>()
const grid = ref<InstanceType<typeof NotebookGrid>>()
const phone = window.matchMedia('(max-width: 767px)').matches
const now = ref(Date.now())
const tick = setInterval(() => (now.value = Date.now()), 1000)

let stop: (() => void) | null = null
onMounted(() => (stop = store.watch()))
onActivated(() => {
  if (!stop) stop = store.watch()
  readQuery()
})
onDeactivated(() => {
  stop?.()
  stop = null
})
onBeforeUnmount(() => {
  stop?.()
  clearInterval(tick)
})

/** ?tipo=stocks (from Posturas or Tablas) chooses the notebook; ?pagina=<id> opens a page. */
function readQuery() {
  const tipo = String(route.query.tipo ?? '')
  if (KINDS.some(k => k.id === tipo)) kind.value = tipo as Kind | 'auto'
  if (route.query.pagina) current.value = String(route.query.pagina)
}
readQuery()
watch(() => route.query, readQuery)

const job = computed(() => store.jobs.find(j => j.id === current.value) ?? null)
const detail = computed(() => (current.value ? store.details[current.value] : undefined))
// The first page of the strip is shown when none is chosen (or the chosen one is gone).
watch(
  () => [store.jobs.map(j => j.id).join(), store.loaded],
  () => {
    if (store.loaded && (!current.value || !store.jobs.some(j => j.id === current.value)))
      current.value = store.open[0]?.id ?? null
  },
  { immediate: true },
)
watch(
  () => [current.value, job.value?.status],
  () => {
    selected.value = null
    cell.value = null
    if (current.value && job.value && ['ready', 'done'].includes(job.value.status) && !store.details[current.value])
      void store.load(current.value)
  },
  { immediate: true },
)

async function add(event: Event) {
  const input = event.target as HTMLInputElement
  const files = [...(input.files ?? [])]
  input.value = ''
  await addFiles(files)
}
async function addFiles(files: File[]) {
  const images = files.filter(f => f.type.startsWith('image/'))
  if (!images.length) return
  const created = await store.addPhotos(images, kind.value, year.value)
  if (created[0] && (!current.value || !job.value || job.value.status !== 'ready')) current.value = created[0]
}
const dragging = ref(false)
function onDrop(event: DragEvent) {
  dragging.value = false
  void addFiles([...(event.dataTransfer?.files ?? [])])
}

const lines = computed(() => detail.value?.reviewLines ?? [])
const picked = computed(() => lines.value.filter(l => l.picked && l.changes))
const counts = computed(() => detail.value?.counts)
const editable = computed(() => session.canEdit && job.value?.status === 'ready')
const photoLines = computed(() => lines.value.map(l => ({ n: l.n, y: l.y, applied: l.applied, changes: l.changes })))

async function apply() {
  if (!current.value || !picked.value.length) return
  applying.value = true
  try {
    const ok = await store.apply(
      current.value,
      picked.value.map(l => l.n),
    )
    if (ok && !store.details[current.value]?.reviewLines.some(l => l.picked && l.changes)) goNext()
  } finally {
    applying.value = false
  }
}
async function discard() {
  if (!current.value) return
  const applied = job.value?.appliedLines.length
  if (!confirm(applied ? '¿Cerrar la página? Las filas ya aplicadas se quedan en la hoja.' : '¿Descartar esta página? No se escribe nada.'))
    return
  const id = current.value
  goNext()
  await store.discard(id)
}
function goNext() {
  const next = nextJob(store.jobs, current.value)
  if (next) current.value = next
  else notify('No quedan más páginas por revisar')
}
async function retry(k: Kind | 'auto') {
  if (!current.value) return
  const corrected = lines.value.some(l => Object.values(l.cells).some(c => c.edited))
  if (corrected && !confirm('Se perderán las correcciones hechas en esta página. ¿Volver a leerla?')) return
  await store.retry(current.value, k)
}
const kindChoices = KINDS.map(k => ({ value: k.id, label: k.sheet ? `${k.label} (${k.sheet})` : k.label }))
/** "Leer como": another notebook reads the page again. */
const readAs = computed({
  get: () => job.value?.kind ?? job.value?.requestedKind ?? 'auto',
  set: value => {
    if (value !== (job.value?.kind ?? job.value?.requestedKind)) void retry(value as Kind | 'auto')
  },
})
function setYear(event: Event) {
  const value = Number((event.target as HTMLInputElement).value)
  if (current.value) void store.setYear(current.value, Number.isInteger(value) && value > 1989 ? value : null)
}

// ---- The bar under the grid: the selected cell, its other readings, confirm ----
const inspected = computed(() => {
  const at = cell.value
  if (!at?.field || !detail.value) return null
  const line = lines.value.find(l => l.n === at.n)
  const c = line?.cells[at.field]
  if (!line || !c) return null
  const type = detail.value.types[at.field] ?? 'text'
  const show = (v: unknown) => displayValue((v ?? null) as never, { key: at.field!, type })
  return {
    line,
    field: at.field,
    cell: c,
    title: cellTitle(at.field, c, type),
    value: show(c.value),
    before: show(c.before),
    alternatives: [...new Set(c.alternatives.map(show))].filter(a => a && a !== show(c.value)),
  }
})
function choose(value: string | null) {
  const at = inspected.value
  if (!current.value || !at) return
  store.edit(current.value, at.line.n, at.field, value)
  grid.value?.focus()
}
/** The other doubtful readings of the selected line, to confirm all at once after looking at the photo. */
const lineDoubts = computed(() => {
  const at = inspected.value
  if (!at || !detail.value) return []
  return Object.entries(at.line.cells)
    .filter(([, c]) => c.doubt && c.value !== null && ['fill', 'conflict', 'new'].includes(c.status))
    .map(([field, c]) => [field, displayValue(c.value, { key: field, type: detail.value!.types[field] ?? 'text' })] as const)
})
function confirmLine() {
  const at = inspected.value
  if (!current.value || !at) return
  for (const [field, text] of lineDoubts.value) store.edit(current.value, at.line.n, field, text)
  grid.value?.focus()
}

const state = (j: (typeof store.jobs)[number]) => jobState(j, now.value)
const thumb = (id: string) => `api/attachments/${id}/content`
const when = (iso: string) => new Date(iso).toLocaleString('es-EC', { day: 'numeric', month: 'short', hour: '2-digit', minute: '2-digit' })
const chat = computed(() => (job.value?.threadId ? { path: '/asistente', query: { hilo: job.value.threadId } } : null))
const history = computed(() => store.jobs)
function openJob(id: string) {
  current.value = id
  showHistory.value = false
  void router.replace({ query: {} })
}
</script>

<template>
  <!-- On phones the screen scrolls as a page (photo, grid, then the sticky actions); on computers it fills the window. -->
  <div
    class="flex h-full flex-col max-md:overflow-y-auto"
    @dragover.prevent="dragging = true"
    @dragleave.self="dragging = false"
    @drop.prevent="onDrop"
  >
    <!-- Photographing: the notebook, the year if known, the camera. -->
    <div class="toolbar items-end max-md:gap-2 max-md:py-2">
      <label class="max-md:flex-1">
        <span class="field-label max-md:hidden">Cuaderno</span>
        <ChoiceField
          v-model="kind"
          class="field-input md:min-w-60"
          :options="kindChoices"
          :freetext="false"
          title="Qué cuaderno se fotografía (se detecta por los encabezados si no se elige)"
        />
      </label>
      <label class="max-md:hidden">
        <span class="field-label">Año de las fechas</span>
        <input
          v-model.number="year"
          type="number"
          min="1990"
          max="2099"
          class="field-input w-28"
          placeholder="automático"
          title="Solo si el cuaderno no lo dice: se deduce de la hoja"
        />
      </label>
      <input ref="camera" type="file" accept="image/*" capture="environment" multiple class="hidden" @change="add" />
      <input ref="gallery" type="file" accept="image/*" multiple class="hidden" @change="add" />
      <button class="btn-primary py-2" :disabled="!session.canEdit" @click="camera?.click()">
        <Camera :size="17" /> Tomar foto
      </button>
      <button class="btn py-2" :disabled="!session.canEdit" title="Elegir fotos" @click="gallery?.click()">
        <ImagePlus :size="17" /><span class="max-md:hidden">Elegir fotos</span>
      </button>
      <button
        class="btn py-2 md:ml-auto"
        :class="{ 'bg-brand-50 text-brand-700': showHistory }"
        title="Páginas digitalizadas"
        @click="showHistory = !showHistory"
      >
        <History :size="16" /><span class="max-md:hidden">Historial ({{ history.length }})</span>
      </button>
    </div>

    <!-- The pages: uploading, being read, ready to review. -->
    <div v-if="store.uploads.length || store.open.length" class="flex gap-2 overflow-x-auto border-b border-stone-200 bg-stone-50 px-3 py-2">
      <div v-for="u in store.uploads" :key="u.localId" class="nb-chip" :class="u.status === 'error' ? 'is-bad' : 'is-busy'">
        <img :src="u.url" alt="" class="h-10 w-8 rounded object-cover" />
        <span class="min-w-0">
          <span class="block truncate text-xs font-medium">{{ u.name }}</span>
          <span class="block text-[11px]">{{ u.status === 'error' ? u.error : 'Subiendo…' }}</span>
        </span>
        <Loader2 v-if="u.status === 'subiendo'" :size="14" class="animate-spin" />
        <button v-else class="btn-ghost p-0.5" title="Quitar" @click="store.dropUpload(u.localId)"><X :size="13" /></button>
      </div>
      <button
        v-for="j in store.open"
        :key="j.id"
        class="nb-chip"
        :class="[`is-${state(j).tone}`, { 'is-current': j.id === current }]"
        @click="openJob(j.id)"
      >
        <img :src="thumb(j.attachmentId)" alt="" class="h-10 w-8 rounded object-cover" loading="lazy" />
        <span class="min-w-0 text-left">
          <span class="block truncate text-xs font-medium">{{ j.label }}{{ j.keys.length ? ` · ${keyRange(j.keys)}` : '' }}</span>
          <span class="block text-[11px]">{{ state(j).text }}</span>
        </span>
        <Loader2 v-if="state(j).tone === 'busy'" :size="14" class="animate-spin" />
        <AlertTriangle v-else-if="j.warnings.some(w => w.kind !== 'reused')" :size="14" class="text-amber-600" />
      </button>
    </div>

    <!-- History of digitized pages. -->
    <div v-if="showHistory" class="min-h-0 flex-1 overflow-auto p-4">
      <table class="w-full max-w-5xl text-sm">
        <thead class="text-left text-xs text-stone-500">
          <tr>
            <th class="py-1 pr-3">Fecha</th>
            <th class="pr-3">Cuaderno</th>
            <th class="pr-3">Líneas</th>
            <th class="pr-3">Claves</th>
            <th class="pr-3">Aplicadas</th>
            <th class="pr-3">Estado</th>
            <th>Quién</th>
          </tr>
        </thead>
        <tbody>
          <tr
            v-for="j in history"
            :key="j.id"
            class="cursor-pointer border-t border-stone-200 hover:bg-brand-50"
            @click="openJob(j.id)"
          >
            <td class="py-1.5 pr-3 whitespace-nowrap">{{ when(j.createdAt) }}</td>
            <td class="pr-3">{{ j.label }}<span v-if="j.sheet" class="text-stone-500"> · {{ j.sheet }}</span></td>
            <td class="pr-3 tabular-nums">{{ j.lines }}</td>
            <td class="pr-3">{{ keyRange(j.keys) }}</td>
            <td class="pr-3 tabular-nums">{{ j.appliedLines.length }}</td>
            <td class="pr-3">{{ state(j).text }}</td>
            <td>{{ j.owner }}</td>
          </tr>
          <tr v-if="!history.length">
            <td colspan="7" class="py-4 text-stone-500">Todavía no se ha digitalizado ninguna página.</td>
          </tr>
        </tbody>
      </table>
    </div>

    <!-- Nothing yet: what to do. -->
    <div
      v-else-if="!job"
      class="grid min-h-0 flex-1 place-items-center p-6"
      :class="{ 'bg-brand-50': dragging }"
    >
      <div class="max-w-xl text-center">
        <Camera :size="40" class="mx-auto text-brand-700" />
        <h2 class="mt-3 text-lg font-medium">Fotografía las páginas del cuaderno</h2>
        <p class="mt-2 text-sm text-stone-600">
          Una foto por página, de frente y con buena luz. Cada página se lee en segundo plano (unos 40–60 s) mientras
          fotografías la siguiente; luego revisas cada línea junto a su foto y aplicas las filas que estén bien. Nada se escribe
          en la hoja hasta que pulses Aplicar.
        </p>
        <div class="mt-4 flex justify-center gap-2">
          <button class="btn-primary px-5 py-3 text-base" :disabled="!session.canEdit" @click="camera?.click()">
            <Camera :size="19" /> Tomar foto
          </button>
          <button class="btn px-5 py-3 text-base" :disabled="!session.canEdit" @click="gallery?.click()">
            <ImagePlus :size="19" /> Elegir fotos
          </button>
        </div>
        <p class="hint mt-3">También puedes arrastrar las fotos aquí.</p>
      </div>
    </div>

    <!-- One page: photo and grid. -->
    <template v-else>
      <div v-if="job.warnings.length" class="bg-amber-50 px-4 py-1.5 text-sm text-amber-900">
        <p v-for="(w, i) in job.warnings" :key="i" class="flex items-center gap-2">
          <AlertTriangle :size="14" class="shrink-0" /> {{ warningText(w) }}
          <button
            v-if="w.jobId && w.kind !== 'reused' && store.jobs.some(j => j.id === w.jobId)"
            class="underline"
            @click="openJob(w.jobId)"
          >
            ver esa página
          </button>
        </p>
      </div>
      <div class="flex flex-col md:min-h-0 md:flex-1 md:flex-row">
        <section class="flex shrink-0 flex-col border-stone-300 md:w-[40%] md:border-r" :class="photoOpen ? 'h-[38vh] md:h-auto' : ''">
          <button class="flex items-center gap-1 bg-stone-100 px-3 py-1 text-left text-xs text-stone-600 md:hidden" @click="photoOpen = !photoOpen">
            <component :is="photoOpen ? ChevronDown : ChevronRight" :size="14" /> Foto de la página
          </button>
          <PagePhoto
            v-if="photoOpen || !phone"
            class="min-h-0 flex-1"
            :src="thumb(job.attachmentId)"
            :rotate="detail?.rotate ?? 0"
            :lines="photoLines"
            :selected="selected"
            @select="n => (selected = n)"
          />
        </section>
        <section class="flex min-w-0 flex-col md:min-h-0 md:flex-1">
          <header class="flex flex-wrap items-center gap-x-3 gap-y-1 border-b border-stone-200 bg-white px-3 py-1.5 text-sm">
            <span class="font-medium">{{ job.label }}<template v-if="job.sheet"> · {{ job.sheet }}</template></span>
            <template v-if="detail">
              <label class="flex items-center gap-1 text-xs text-stone-600">
                Año
                <input
                  type="number"
                  class="field-input w-20 py-0.5"
                  :value="detail.year"
                  :disabled="!editable"
                  :title="detail.yearSource === 'inferred' ? 'Deducido de las filas de la hoja; cámbialo si no es' : 'Año de las fechas sin año'"
                  @change="setYear"
                />
                <span v-if="detail.yearSource === 'inferred'" class="hint">(deducido)</span>
              </label>
              <span v-if="counts" class="flex flex-wrap gap-x-3 text-xs text-stone-600">
                <span><i class="nb-dot bg-emerald-400" /> {{ counts.fills }} por llenar</span>
                <span><i class="nb-dot bg-red-400" /> {{ counts.conflicts }} diferencias</span>
                <span><i class="nb-dot bg-amber-400" /> {{ counts.doubts }} dudosas</span>
                <span v-if="counts.created"><i class="nb-dot bg-emerald-600" /> {{ counts.created }} filas nuevas</span>
                <span v-if="counts.errors"><i class="nb-dot bg-red-700" /> {{ counts.errors }} con problemas</span>
                <span>{{ counts.same }} iguales</span>
              </span>
            </template>
            <label class="ml-auto flex items-center gap-1 text-xs text-stone-600">
              Leer como
              <ChoiceField
                v-model="readAs"
                class="field-input w-44 py-0.5"
                :options="kindChoices"
                :freetext="false"
                :disabled="['queued', 'reading'].includes(job.status) || !session.canEdit"
                title="Si el cuaderno no es el que se detectó, se vuelve a leer la página"
              />
            </label>
          </header>

          <div v-if="['queued', 'reading'].includes(job.status)" class="grid flex-1 place-items-center p-6 text-center text-stone-600">
            <div>
              <Loader2 :size="28" class="mx-auto animate-spin text-brand-700" />
              <p class="mt-2">{{ state(job).text }}</p>
              <p class="hint mt-1">La IA lee cada línea (40–60 s por página). Puedes seguir fotografiando otras páginas.</p>
            </div>
          </div>
          <div v-else-if="job.status === 'error'" class="flex-1 p-6 text-sm">
            <p class="text-red-800">No se pudo leer la página: {{ job.error }}</p>
            <button class="btn mt-3" @click="retry(job.requestedKind)"><RotateCcw :size="15" /> Volver a leer</button>
          </div>
          <div v-else-if="!detail" class="flex-1 p-6 text-stone-500">Cargando la página…</div>
          <template v-else>
            <div class="relative h-[62vh] md:h-auto md:min-h-0 md:flex-1">
              <NotebookGrid
                ref="grid"
                :lines="lines"
                :fields="detail.fields"
                :types="detail.types"
                :key-fields="detail.keyFields"
                :options="detail.options"
                :selected="selected"
                :editable="editable"
                @select="n => (selected = n)"
                @cell="(n, field) => (cell = { n, field })"
                @edit="(n, field, value) => current && store.edit(current, n, field, value)"
                @pick="(n, on) => current && store.pick(current, n, on)"
                @notice="notify"
              />
            </div>
            <!-- The selected cell: what the notebook and the sheet say, the other readings. -->
            <div v-if="inspected" class="flex flex-wrap items-center gap-2 border-t border-stone-200 bg-stone-50 px-3 py-1.5 text-xs">
              <span class="font-medium">Línea {{ inspected.line.n }} · {{ inspected.field }}</span>
              <span class="text-stone-600">{{ inspected.title }}</span>
              <template v-if="editable && !['formula', 'same', 'empty'].includes(inspected.cell.status)">
                <button
                  v-if="inspected.value && (inspected.cell.doubt || !inspected.cell.include)"
                  class="btn py-0.5 text-xs"
                  @click="choose(inspected.value)"
                >
                  <Check :size="13" /> Confirmar «{{ inspected.value }}»<span class="text-stone-400 max-md:hidden">(Ctrl+Enter)</span>
                </button>
                <button v-if="lineDoubts.length > 1" class="btn py-0.5 text-xs" @click="confirmLine">
                  <Check :size="13" /> Confirmar las {{ lineDoubts.length }} dudosas de la línea
                </button>
                <button v-for="a in inspected.alternatives" :key="a" class="btn py-0.5 text-xs" @click="choose(a)">{{ a }}</button>
                <button
                  v-if="inspected.cell.status === 'conflict' && inspected.before"
                  class="btn py-0.5 text-xs"
                  title="Deja la hoja como está"
                  @click="choose(inspected.before)"
                >
                  Dejar el de la hoja: {{ inspected.before }}
                </button>
              </template>
            </div>
            <p v-else class="hint border-t border-stone-200 px-3 py-1">
              {{
                phone
                  ? 'Toca una línea de la foto o una celda · dos toques para corregir · ☐ elige las filas.'
                  : 'Clic en una banda de la foto o en una celda · escribe para corregir, ▾ otras lecturas · ↑/↓ mueven la banda · Espacio marca la fila · Ctrl+C/V, Ctrl+D y el cuadrito para rellenar.'
              }}
            </p>
          </template>

          <!-- The page's actions: large on phones. -->
          <footer class="flex flex-wrap items-center gap-2 border-t border-stone-300 bg-white px-3 py-2 max-md:sticky max-md:bottom-0 max-md:z-10">

            <button
              class="btn-primary bg-emerald-700 py-2 hover:bg-emerald-800 max-md:flex-1 max-md:py-3"
              :disabled="!editable || !picked.length || applying"
              @click="apply"
            >
              <Loader2 v-if="applying" :size="16" class="animate-spin" /><Check v-else :size="16" />
              Aplicar seleccionadas ({{ picked.length }})
            </button>
            <button class="btn py-2 max-md:py-3" :disabled="!session.canEdit || !['ready', 'error'].includes(job.status)" @click="discard">
              <X :size="16" /> Descartar
            </button>
            <button class="btn py-2 max-md:py-3" @click="goNext">Siguiente página <ChevronRight :size="16" /></button>
            <RouterLink v-if="chat" :to="chat" class="btn py-2 max-md:py-3" title="La conversación de esta página: pregunta por una línea">
              <MessageSquare :size="15" /> Preguntar en el chat
            </RouterLink>
            <span v-if="job.durationMs" class="hint ml-auto" :title="job.model ? `Leída con ${job.model}` : undefined">
              leída en {{ Math.round(job.durationMs / 1000) }} s<template v-if="job.costUsd !== null"> · {{ job.costUsd.toFixed(2) }} US$</template>
            </span>
          </footer>
        </section>
      </div>
    </template>
  </div>
</template>
