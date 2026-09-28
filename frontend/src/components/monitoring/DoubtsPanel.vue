<script setup lang="ts">
import ChoiceField from '../ChoiceField.vue'
import { computed, onBeforeUnmount, onMounted, ref } from 'vue'
import { Check, ChevronLeft, ChevronRight, Copy, ExternalLink, MapPin, RefreshCw, X } from 'lucide-vue-next'
import { useMonitoring } from '../../composables/useMonitoring'
import { api, requestId } from '../../lib/api'
import { formatSerial, isoToSerial } from '../../lib/dates'
import { formatMinutes, type MatchConfidence, type MatchConflict } from '../../lib/monitoring'
import { errorText, notify } from '../../lib/notice'
import { useSession } from '../../stores/session'

/**
 * "Dudas de emparejamiento": walk points whose sheet row is not certain (a tie
 * with another row, placed only by the walk's order, a note that disagrees with
 * its row, or a row that matching again would change), each beside its Wikiloc
 * photos and note, with the rows it could be. One click keeps a row (never
 * changed by matching again); "No es ninguna" leaves the point without a row.
 * Old walks still waiting in "por revisar" are paired here point by point and
 * then go on the map. Reviewers apply the changes of matching all walks again.
 */
interface RowInfo {
  recordId: string
  row: number
  species: string | null
  subspecies: string | null
  sex: 'female' | 'male' | null
  minutes: number | null
  markId: string | null
  kind: string | null
  section: string | null
}
interface Doubt {
  source: 'track' | 'walk'
  trackId?: string
  walkId?: string
  indexes: number[]
  date: string
  collector: string | null
  name: string
  wikiloc: string | null
  text: string
  minutes: number | null
  photos: string[]
  photoLinks: string[]
  confidence: MatchConfidence
  conflicts: MatchConflict[]
  changed: boolean
  current: (RowInfo | null)[]
  proposed: (RowInfo | null)[]
  candidates: RowInfo[]
}
interface Change {
  trackId: string
  index: number
  from: string | null
  to: string | null
  date: string
  collector: string | null
  name: string
  text: string
  confidence: MatchConfidence
  before: RowInfo | null
  after: RowInfo | null
}
interface WalkCount {
  source: 'track' | 'walk'
  id: string
  date: string
  collector: string | null
  name: string
  doubts: number
}

const session = useSession()
const { loadTracks, loadWalks } = useMonitoring()
const canReview = computed(() => ['reviewer', 'admin'].includes(session.user?.role || ''))

const data = ref<{ changes: Change[]; doubts: Doubt[]; walks: WalkCount[] } | null>(null)
const loading = ref(false)
const busy = ref(false)
async function load() {
  loading.value = true
  try {
    data.value = await api('monitoring/rematch')
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    loading.value = false
  }
}
onMounted(load)

const who = ref('')
const initials = (c: string | null) => (c || '').split(' - ')[0].trim()
const collectors = computed(() => [...new Set((data.value?.walks || []).map(w => initials(w.collector)))].filter(Boolean).sort())
const walkKey = (d: { source: string; trackId?: string; walkId?: string; id?: string }) =>
  `${d.source}|${d.trackId ?? d.walkId ?? d.id}`
/** Doubts per walk, newest walk first. */
const groups = computed(() => {
  const all = (data.value?.walks || []).filter(w => !who.value || initials(w.collector) === who.value)
  return all
    .map(w => ({ walk: w, doubts: (data.value?.doubts || []).filter(d => walkKey(d) === walkKey(w)) }))
    .sort((a, b) => b.walk.date.localeCompare(a.walk.date))
})
const total = computed(() => groups.value.reduce((n, g) => n + (g.walk.source === 'track' ? g.doubts.length : 0), 0))
const waitingWalks = computed(() => groups.value.filter(g => g.walk.source === 'walk').length)

// ------------------------------------------------------------ labels
const day = (iso: string) => formatSerial(isoToSerial(iso))
const sexLabel = (s: string | null) => (s === 'female' ? 'hembra' : s === 'male' ? 'macho' : '')
const REASON: Record<MatchConfidence, string> = {
  mark: 'Por marca',
  sure: 'Segura',
  tie: 'Empate: otra fila encaja igual',
  order: 'Por orden: la nota no tiene hora que encaje',
  none: 'Sin fila',
}
const CONFLICT: Record<MatchConflict, string> = {
  sexo: 'el sexo no coincide',
  especie: 'la especie no coincide',
  marca: 'la marca no coincide',
  hora: 'otra hora',
}
const rowLabel = (r: RowInfo) =>
  [
    `fila ${r.row}`,
    r.species || 'sin especie',
    sexLabel(r.sex),
    formatMinutes(r.minutes),
    r.markId || '',
    r.section ? `T${r.section}` : '',
  ]
    .filter(Boolean)
    .join(' · ')

// ------------------------------------------------------------ choosing rows
/** Rows chosen for the points of a waiting walk (by point index), before it goes on the map. */
const picks = ref<Record<string, string[]>>({})
const pickKey = (d: Doubt) => `${d.walkId}|${d.indexes[0]}`
function chosenOf(d: Doubt): string[] {
  if (d.source === 'walk') return picks.value[pickKey(d)] ?? d.proposed.flatMap(r => (r ? [r.recordId] : []))
  return d.current.flatMap(r => (r ? [r.recordId] : []))
}
const isProposed = (d: Doubt, r: RowInfo) => d.proposed.some(p => p?.recordId === r.recordId)

async function choose(d: Doubt, r: RowInfo | null) {
  if (d.source === 'walk') {
    picks.value = { ...picks.value, [pickKey(d)]: r ? [r.recordId] : [] }
    return
  }
  // Several butterflies at one point: the next stored copy without this row takes it.
  const k = r
    ? Math.max(
        0,
        d.current.findIndex(c => c?.recordId !== r.recordId),
      )
    : 0
  busy.value = true
  try {
    for (const index of r ? [d.indexes[k]] : d.indexes)
      await api(`monitoring/tracks/${encodeURIComponent(d.trackId!)}/link`, {
        method: 'POST',
        body: { index, recordId: r?.recordId ?? null },
      })
    notify(r ? `Punto emparejado con la fila ${r.row}` : 'Punto sin fila', 'success', 2000)
    await Promise.all([load(), loadTracks()])
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    busy.value = false
  }
}

/** A waiting walk goes on the map with the rows chosen for each point. */
async function storeWalk(walk: WalkCount, doubts: Doubt[]) {
  const links = doubts
    .slice()
    .sort((a, b) => a.indexes[0] - b.indexes[0])
    .map(d => chosenOf(d))
  const used = links.flat()
  if (new Set(used).size !== used.length) return notify('Una fila está elegida para dos puntos', 'error')
  if (!confirm(`¿Pasar al mapa "${walk.name}" con estas filas? Las filas de la hoja no cambian.`)) return
  busy.value = true
  try {
    await api(`monitoring/wikiloc/${encodeURIComponent(walk.id)}/store`, {
      method: 'POST',
      body: { requestId: requestId(), date: walk.date, collector: walk.collector, links },
    })
    notify('Recorrido pasado al mapa', 'success')
    await Promise.all([load(), loadTracks(), loadWalks()])
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    busy.value = false
  }
}

// ------------------------------------------------------------ matching again
const showChanges = ref(false)
async function applyChanges() {
  const changes = data.value?.changes || []
  if (!changes.length) return
  if (!confirm(`¿Aplicar ${changes.length} cambios de emparejamiento? Solo cambian los enlaces del mapa, no la hoja.`)) return
  busy.value = true
  try {
    const out = await api<{ applied: number; skipped: number }>('monitoring/rematch', {
      method: 'POST',
      body: { changes: changes.map(c => ({ trackId: c.trackId, index: c.index, from: c.from, to: c.to })) },
    })
    notify(`${out.applied} enlaces cambiados${out.skipped ? `; ${out.skipped} ya no eran iguales` : ''}`, 'success')
    await Promise.all([load(), loadTracks()])
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    busy.value = false
  }
}
const changedWalks = computed(() => new Set((data.value?.changes || []).map(c => c.trackId)).size)

// ------------------------------------------------------------ asking the collector
const FIRST_NAMES: Record<string, string> = { AA: 'Alex', MJS: 'María José', FCH: 'Franz' }
const firstName = (collector: string | null) => {
  const i = initials(collector)
  return FIRST_NAMES[i] || (collector || '').split(' - ')[1]?.split(' ')[0] || i
}
function message(d: Doubt) {
  const options = [...d.proposed.filter((r): r is RowInfo => !!r), ...d.candidates.filter(r => !isProposed(d, r))].slice(0, 3)
  const rows = options.map(
    r =>
      `la fila ${r.row} (${[r.species || 'sin especie', sexLabel(r.sex), formatMinutes(r.minutes)].filter(Boolean).join(', ')})`,
  )
  return [
    `Hola ${firstName(d.collector)}, estoy revisando el monitoreo del ${d.date.split('-').reverse().map(Number).join('/')}.`,
    `En Wikiloc hay un punto con la nota «${d.text}»${d.minutes !== null ? ` (${formatMinutes(d.minutes)})` : ''}: ¿qué mariposa es?`,
    rows.length ? `Puede ser ${rows.join(' o ')} de Collection_data.` : 'No encuentro su fila en Collection_data.',
    d.photoLinks[0] ? `Foto: ${d.photoLinks[0]}` : d.wikiloc ? `Recorrido: ${d.wikiloc}` : '',
  ]
    .filter(Boolean)
    .join(' ')
}
async function ask(d: Doubt) {
  try {
    await navigator.clipboard.writeText(message(d))
    notify(`Mensaje para ${firstName(d.collector)} copiado`, 'success', 2000)
  } catch {
    notify('No se pudo copiar el mensaje', 'error')
  }
}

// ------------------------------------------------------------ enlarged photo
const photoUrl = (id: string) => `api/monitoring/photos/${id}`
const viewer = ref<{ photos: string[]; index: number; text: string } | null>(null)
function onKey(event: KeyboardEvent) {
  if (!viewer.value) return
  const n = viewer.value.photos.length
  if (event.key === 'Escape') viewer.value = null
  else if (event.key === 'ArrowRight') viewer.value.index = (viewer.value.index + 1) % n
  else if (event.key === 'ArrowLeft') viewer.value.index = (viewer.value.index - 1 + n) % n
}
onMounted(() => window.addEventListener('keydown', onKey))
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
</script>

<template>
  <div class="h-full overflow-y-auto">
    <div class="toolbar">
      <label class="block">
        <span class="field-label">Colector</span>
        <ChoiceField
          v-model="who"
          class="field-input"
          :freetext="false"
          :options="[{ value: '', label: 'Todos' }, ...collectors.map(c => ({ value: c, label: c }))]"
        />
      </label>
      <p class="hint pb-2">
        {{ total }} puntos dudosos en {{ groups.length - waitingWalks }} recorridos del mapa<template v-if="waitingWalks"
          >; {{ waitingWalks }} recorridos por revisar para emparejar a mano</template
        >
      </p>
      <button class="btn ml-auto" :disabled="loading" title="Volver a emparejar y calcular las dudas" @click="load">
        <RefreshCw :size="14" :class="{ 'animate-spin': loading }" /> Actualizar
      </button>
    </div>

    <div class="space-y-3 p-3 sm:p-4">
      <p v-if="loading && !data" class="hint">Emparejando los recorridos con la hoja…</p>

      <!-- Matching every stored walk again: what would change (reviewers apply it). -->
      <section v-if="data?.changes.length" class="rounded-md border border-amber-300 bg-amber-50 p-3 text-sm">
        <div class="flex flex-wrap items-center gap-2">
          <p>
            Al emparejar de nuevo todos los recorridos con el método actual cambian
            <b>{{ data.changes.length }}</b> enlaces en {{ changedWalks }} recorridos (están también en la lista de abajo).
          </p>
          <button class="btn-ghost text-xs underline" @click="showChanges = !showChanges">
            {{ showChanges ? 'Ocultar' : 'Ver' }} cambios
          </button>
          <button v-if="canReview" class="btn-primary ml-auto" :disabled="busy" @click="applyChanges">
            <Check :size="14" /> Aplicar {{ data.changes.length }} cambios
          </button>
          <span v-else class="hint ml-auto">Los aplica un revisor o administrador.</span>
        </div>
        <ul v-if="showChanges" class="mt-2 space-y-0.5 text-xs">
          <li v-for="c in data.changes" :key="`${c.trackId}|${c.index}`">
            {{ day(c.date) }} {{ initials(c.collector) }} · «{{ c.text }}»:
            <span class="text-stone-500">{{ c.before ? rowLabel(c.before) : 'sin fila' }}</span> →
            <b>{{ c.after ? rowLabel(c.after) : 'sin fila' }}</b>
            <span class="text-stone-500"> ({{ REASON[c.confidence] }})</span>
          </li>
        </ul>
      </section>

      <p v-if="data && !groups.length" class="hint">No hay dudas de emparejamiento.</p>

      <section v-for="g in groups" :key="walkKey(g.walk)" class="rounded-md border border-stone-200 bg-white">
        <header class="flex flex-wrap items-baseline gap-x-3 gap-y-1 border-b border-stone-100 px-3 py-2">
          <h2 class="text-base font-semibold">{{ day(g.walk.date) }} · {{ initials(g.walk.collector) }}</h2>
          <span class="text-sm text-stone-500">{{ g.walk.name }}</span>
          <span class="text-sm text-stone-500">
            <template v-if="g.walk.source === 'walk'">por revisar: {{ g.doubts.length }} puntos para emparejar</template>
            <template v-else>{{ g.doubts.length }} {{ g.doubts.length === 1 ? 'duda' : 'dudas' }}</template>
          </span>
          <a
            v-if="g.doubts[0]?.wikiloc"
            :href="g.doubts[0].wikiloc!"
            target="_blank"
            rel="noopener"
            class="btn-ghost text-sm"
            title="Abrir en Wikiloc"
            ><ExternalLink :size="14"
          /></a>
          <button
            v-if="g.walk.source === 'walk' && session.canEdit"
            class="btn-primary ml-auto"
            :disabled="busy"
            title="Guarda el recorrido en el mapa con las filas elegidas"
            @click="storeWalk(g.walk, g.doubts)"
          >
            <MapPin :size="14" /> Pasar al mapa
          </button>
        </header>
        <ol class="divide-y divide-stone-100">
          <li v-for="d in g.doubts" :key="`${walkKey(d)}|${d.indexes[0]}`" class="flex flex-col gap-3 p-3 md:flex-row">
            <figure class="w-full shrink-0 md:w-56">
              <div class="flex gap-1">
                <button
                  v-for="(id, k) in d.photos.slice(0, 2)"
                  :key="id"
                  type="button"
                  class="block flex-1 overflow-hidden rounded bg-stone-100"
                  @click="viewer = { photos: d.photos, index: k, text: d.text }"
                >
                  <img :src="photoUrl(id)" alt="" loading="lazy" class="h-36 w-full object-cover" />
                </button>
                <div
                  v-if="!d.photos.length"
                  class="flex h-36 w-full items-center justify-center rounded border border-dashed border-stone-300 text-xs text-stone-400"
                >
                  Sin foto
                </div>
              </div>
              <figcaption class="mt-1 text-sm">
                «{{ d.text }}»
                <span v-if="d.indexes.length > 1" class="text-stone-500"> ({{ d.indexes.length }} mariposas)</span>
              </figcaption>
            </figure>
            <div class="min-w-0 flex-1 text-sm">
              <p class="mb-1">
                <span class="rounded bg-amber-100 px-1 text-xs font-medium text-amber-800">{{ REASON[d.confidence] }}</span>
                <span
                  v-for="c in d.conflicts.filter(c => c !== 'hora' || d.confidence !== 'mark')"
                  :key="c"
                  class="ml-1 text-xs text-amber-800"
                  >{{ CONFLICT[c] }}</span
                >
                <span v-if="d.changed" class="ml-1 rounded bg-sky-100 px-1 text-xs text-sky-800"
                  >cambia al emparejar de nuevo</span
                >
              </p>
              <p v-if="d.source === 'track'" class="mb-1 text-xs text-stone-500">
                Ahora:
                {{
                  d.current
                    .filter(Boolean)
                    .map(r => rowLabel(r!))
                    .join('; ') || 'sin fila'
                }}
              </p>
              <ul class="space-y-1">
                <li v-for="r in d.candidates" :key="r.recordId">
                  <button
                    type="button"
                    class="w-full rounded border px-2 py-1 text-left hover:border-brand-600 disabled:opacity-60"
                    :class="chosenOf(d).includes(r.recordId) ? 'border-brand-600 bg-brand-50' : 'border-stone-200'"
                    :disabled="busy || !session.canEdit"
                    :title="chosenOf(d).includes(r.recordId) ? 'Elegida' : 'Es esta fila'"
                    @click="choose(d, r)"
                  >
                    <Check v-if="chosenOf(d).includes(r.recordId)" :size="13" class="mr-1 inline text-brand-700" />{{ rowLabel(r)
                    }}<span v-if="r.kind" class="text-stone-500"> · {{ r.kind }}</span
                    ><span v-if="isProposed(d, r)" class="ml-1 text-xs text-sky-700">(propuesta)</span>
                  </button>
                </li>
              </ul>
              <div class="mt-2 flex flex-wrap gap-2">
                <button class="btn py-0.5" :disabled="busy || !session.canEdit" @click="choose(d, null)">
                  <X :size="13" /> No es ninguna
                </button>
                <button class="btn py-0.5" :title="message(d)" @click="ask(d)">
                  <Copy :size="13" /> Preguntar a {{ firstName(d.collector) }}
                </button>
              </div>
            </div>
          </li>
        </ol>
      </section>
    </div>

    <div
      v-if="viewer"
      class="fixed inset-0 z-[2000] flex flex-col bg-black/90 text-white"
      role="dialog"
      aria-modal="true"
      @click.self="viewer = null"
    >
      <div class="flex items-center gap-3 px-4 py-2 text-sm">
        <span>«{{ viewer.text }}» · foto {{ viewer.index + 1 }} de {{ viewer.photos.length }}</span>
        <button class="ml-auto rounded p-1 hover:bg-white/10" title="Cerrar (Esc)" @click="viewer = null">
          <X :size="20" />
        </button>
      </div>
      <div class="relative flex min-h-0 flex-1 items-center justify-center" @click.self="viewer = null">
        <button
          v-if="viewer.photos.length > 1"
          class="absolute left-2 rounded-full bg-white/10 p-2 hover:bg-white/20"
          @click="viewer.index = (viewer.index - 1 + viewer.photos.length) % viewer.photos.length"
        >
          <ChevronLeft :size="24" />
        </button>
        <img :src="photoUrl(viewer.photos[viewer.index])" alt="" class="max-h-full max-w-full object-contain" />
        <button
          v-if="viewer.photos.length > 1"
          class="absolute right-2 rounded-full bg-white/10 p-2 hover:bg-white/20"
          @click="viewer.index = (viewer.index + 1) % viewer.photos.length"
        >
          <ChevronRight :size="24" />
        </button>
      </div>
    </div>
  </div>
</template>
