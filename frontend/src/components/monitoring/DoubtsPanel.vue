<script setup lang="ts">
import ChoiceField from '../ChoiceField.vue'
import { computed, defineAsyncComponent, onMounted, ref } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { Check, Columns2, ExternalLink, MapPin, RefreshCw, X } from 'lucide-vue-next'
import PointPhotoViewer from './PointPhotoViewer.vue'
import PointSection from './PointSection.vue'
import { useMonitoring, type WikilocWalk } from '../../composables/useMonitoring'
import { api, requestId } from '../../lib/api'
import { formatSerial, isoToSerial } from '../../lib/dates'
import { t } from '../../lib/i18n'
import { formatMinutes, type MatchConfidence, type MatchConflict } from '../../lib/monitoring'
import { errorText, notify } from '../../lib/notice'
import { CONFLICT } from '../../lib/walkBoard'
import { useSession } from '../../stores/session'

const WalkBoard = defineAsyncComponent(() => import('./WalkBoard.vue'))

/**
 * "Dudas de emparejamiento": walk points whose sheet row is not certain (a tie
 * with another row, placed only by the walk's order, a note that disagrees with
 * its row, or a row that matching again would change), each beside its Wikiloc
 * photos and note, with the rows it could be. Points that Pasar al mapa stored
 * without a row because of such a doubt wait here (and in Revisión de datos)
 * with every free row of the day. One click keeps a row (never
 * changed by matching again); "No es ninguna" leaves the point without a row.
 * Old walks still waiting in "por revisar" are paired here point by point and
 * then go on the map. Reviewers apply the changes of matching all walks again.
 * Any Wikiloc walk (waiting, or imported) also opens whole on its pairing
 * board (WalkBoard): every point beside every row of the day.
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
interface NoteInfo {
  species: string | null
  subspecies: string | null
  sex: 'female' | 'male' | null
  minutes: number | null
  markId: string | null
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
  /** The point's GPS position (its section beside the row's Transect_section). */
  lat?: number
  lon?: number
  minutes: number | null
  /** What the note itself says (species, sex, mark, time), to set beside its row. */
  note?: NoteInfo
  photos: string[]
  photoLinks: string[]
  confidence: MatchConfidence
  conflicts: MatchConflict[]
  changed: boolean
  /** Stored without a row because of the doubt: off the map until a row is chosen. */
  pending?: boolean
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
const { loadTracks, loadWalks, walks } = useMonitoring()
const route = useRoute()
const router = useRouter()
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
const sexLabel = (s: string | null) => (s === 'female' ? t('hembra') : s === 'male' ? t('macho') : '')
// Spanish; t() where shown.
const REASON: Record<MatchConfidence, string> = {
  mark: 'Por marca',
  sure: 'Segura',
  tie: '¿Qué fila es? Otra fila encaja igual',
  order: '¿Qué fila es? Puesta por orden: la nota no tiene hora que encaje',
  none: 'Sin fila',
}
const rowLabel = (r: RowInfo) =>
  [
    t('fila {n}', { n: r.row }),
    r.species || t('sin especie'),
    sexLabel(r.sex),
    formatMinutes(r.minutes),
    r.markId || '',
    r.section ? `T${r.section}` : '',
  ]
    .filter(Boolean)
    .join(' · ')

// ------------------------------------------------------------ the note beside its row
/**
 * Paired surely (by its mark, or the only row at its minute) but the note says
 * something else than its row: the question is which of the two is right, not
 * which row it is. Shown as the note beside the row; the other rows of the
 * day only on asking.
 */
function settledRow(d: Doubt): RowInfo | null {
  if (d.source !== 'track' || d.pending || d.changed || d.indexes.length !== 1) return null
  if (d.confidence !== 'mark' && d.confidence !== 'sure') return null
  return d.current[0] ?? null
}
const lower = (v: string | null | undefined) => (v || '').trim().toLowerCase()
/** Species, sex, mark and time of the note and of its row; `differ` where both say something and it is not the same. */
function compare(d: Doubt, r: RowInfo) {
  const n = d.note
  const species = (s: string | null, sub: string | null) => [s, sub].filter(Boolean).join(' ') || null
  // Spanish labels (the key of each line); t() where shown.
  const lines: { label: string; note: string | null; row: string | null; differ: boolean }[] = [
    {
      label: 'Especie',
      note: species(n?.species ?? null, n?.subspecies ?? null),
      row: species(r.species, r.subspecies),
      // A note without the subspecies agrees with a row that has it.
      differ: !!n?.species && !!r.species && lower(n.species) !== lower(r.species),
    },
    {
      label: 'Sexo',
      note: sexLabel(n?.sex ?? null) || null,
      row: sexLabel(r.sex) || null,
      differ: !!n?.sex && !!r.sex && n.sex !== r.sex,
    },
    {
      label: 'Marca',
      note: n?.markId ?? null,
      row: r.markId,
      differ: !!n?.markId && !!r.markId && lower(n.markId) !== lower(r.markId),
    },
    {
      label: 'Hora',
      note: n?.minutes != null ? formatMinutes(n.minutes) : null,
      row: r.minutes != null ? formatMinutes(r.minutes) : null,
      differ: false,
    },
  ]
  return lines.filter(l => l.note || l.row)
}
/** Why the point is surely this row: the same mark, the same minute. */
function pairedBy(d: Doubt, r: RowInfo) {
  const n = d.note
  const why = [
    n?.markId && lower(n.markId) === lower(r.markId) ? t('la misma marca {mark}', { mark: r.markId }) : '',
    n?.minutes != null && r.minutes != null && Math.abs(n.minutes - r.minutes) <= 2
      ? t('la misma hora {time}', { time: formatMinutes(r.minutes) })
      : '',
  ].filter(Boolean)
  return why.length === 2
    ? t('tienen {a} y {b}', { a: why[0], b: why[1] })
    : why.length
      ? t('tienen {a}', { a: why[0] })
      : d.confidence === 'mark'
        ? t('por la marca')
        : t('es la única fila que encaja a esa hora')
}
/** Doubts whose other rows are shown ("Es otra fila"). */
const opened = ref(new Set<string>())
const doubtKey = (d: Doubt) => `${walkKey(d)}|${d.indexes[0]}`
function toggleOthers(d: Doubt) {
  const next = new Set(opened.value)
  if (!next.delete(doubtKey(d))) next.add(doubtKey(d))
  opened.value = next
}

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
  // Several butterflies at one point: a copy without a row first, else the next one without this row.
  const empty = d.current.findIndex(c => !c)
  const k = r
    ? empty >= 0
      ? empty
      : Math.max(
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
    notify(r ? t('Punto emparejado con la fila {n}', { n: r.row }) : t('Punto sin fila'), 'success', 2000)
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
  if (new Set(used).size !== used.length) return notify(t('Una fila está elegida para dos puntos'), 'error')
  if (!confirm(t('¿Pasar al mapa "{name}" con estas filas? Las filas de la hoja no cambian.', { name: walk.name }))) return
  busy.value = true
  try {
    await api(`monitoring/wikiloc/${encodeURIComponent(walk.id)}/store`, {
      method: 'POST',
      body: { requestId: requestId(), date: walk.date, collector: walk.collector, links },
    })
    notify(t('Recorrido pasado al mapa'), 'success')
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
  if (
    !confirm(t('¿Aplicar {n} cambios de emparejamiento? Solo cambian los enlaces del mapa, no la hoja.', { n: changes.length }))
  )
    return
  busy.value = true
  try {
    const out = await api<{ applied: number; skipped: number }>('monitoring/rematch', {
      method: 'POST',
      body: { changes: changes.map(c => ({ trackId: c.trackId, index: c.index, from: c.from, to: c.to })) },
    })
    notify(
      out.skipped
        ? t('{n} enlaces cambiados; {skipped} ya no eran iguales', { n: out.applied, skipped: out.skipped })
        : t('{n} enlaces cambiados', { n: out.applied }),
      'success',
    )
    await Promise.all([load(), loadTracks()])
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    busy.value = false
  }
}
const changedWalks = computed(() => new Set((data.value?.changes || []).map(c => c.trackId)).size)

/** The row a point is paired with now (or chosen for it), for its section. */
function rowOf(d: Doubt): RowInfo | null {
  const settled = settledRow(d)
  if (settled) return settled
  const id = chosenOf(d)[0]
  return (id && d.candidates.find(r => r.recordId === id)) || null
}

// ------------------------------------------------------------ the pairing board of one walk
/** The walk open on its board (in the page link, so it can be shared). */
const boardWalk = computed({
  get: () => String(route.query.recorrido ?? ''),
  set: v => router.replace({ query: { ...route.query, recorrido: v || undefined } }),
})
/** Wikiloc walks that open on a board: those waiting first, then the imported ones, newest first. */
const boardChoices = computed(() =>
  [...walks.value]
    .filter(w => w.date && (w.collector || w.trackId))
    .sort((a, b) => (a.status === b.status ? (b.date || '').localeCompare(a.date || '') : a.status === 'waiting' ? -1 : 1))
    .map(w => ({
      value: w.id,
      label: `${day(w.date!)} · ${initials(w.collector ?? null)} · ${w.name}`,
      group: w.status === 'waiting' ? t('Por revisar') : t('Importados'),
    })),
)
/** The Wikiloc walk of a group of doubts: the waiting walk, or the one its stored track came from. */
const walkOfGroup = (g: WalkCount): WikilocWalk | null =>
  walks.value.find(w => (g.source === 'walk' ? w.id === g.id : w.trackId === g.id)) || null
async function boardStored() {
  boardWalk.value = ''
  await load()
}

// ------------------------------------------------------------ enlarged photo
const photoUrl = (id: string) => `api/monitoring/photos/${id}`
const viewer = ref<{ photos: string[]; index: number; text: string } | null>(null)
</script>

<template>
  <div v-if="boardWalk" class="h-full overflow-y-auto">
    <WalkBoard :walk-id="boardWalk" @close="boardWalk = ''" @stored="boardStored" />
  </div>
  <div v-else class="h-full overflow-y-auto">
    <div class="toolbar">
      <label class="block">
        <span class="field-label">{{ $t('Colector') }}</span>
        <ChoiceField
          v-model="who"
          class="field-input"
          :freetext="false"
          :options="[{ value: '', label: $t('Todos') }, ...collectors.map(c => ({ value: c, label: c }))]"
        />
      </label>
      <p class="hint pb-2">
        {{ $t('{n} puntos dudosos en {walks} recorridos del mapa', { n: total, walks: groups.length - waitingWalks })
        }}<template v-if="waitingWalks"
          >; {{ $t('{n} recorridos por revisar para emparejar a mano', { n: waitingWalks }) }}</template
        >
      </p>
      <p class="hint order-last basis-full pb-1 text-xs">
        {{
          $t(
            'Cada tarjeta es un punto de Wikiloc (una mariposa) y la fila de Collection_data que le corresponde. No son recapturas: «La nota no coincide» es un punto ya emparejado donde la nota y la hoja dicen algo distinto; «¿Qué fila es?» es un punto que puede ser más de una fila.',
          )
        }}
      </p>
      <label class="block min-w-0 sm:w-80">
        <span class="field-label">{{ $t('Tablero de un recorrido') }}</span>
        <ChoiceField
          v-model="boardWalk"
          class="field-input"
          :freetext="false"
          :placeholder="$t('Elige un recorrido de Wikiloc…')"
          :options="boardChoices"
        />
      </label>
      <button class="btn ml-auto" :disabled="loading" :title="$t('Volver a emparejar y calcular las dudas')" @click="load">
        <RefreshCw :size="14" :class="{ 'animate-spin': loading }" /> {{ $t('Actualizar') }}
      </button>
    </div>

    <div class="space-y-3 p-3 sm:p-4">
      <p v-if="loading && !data" class="hint">{{ $t('Emparejando los recorridos con la hoja…') }}</p>

      <!-- Matching every stored walk again: what would change (reviewers apply it). -->
      <section v-if="data?.changes.length" class="rounded-md border border-amber-300 bg-amber-50 p-3 text-sm">
        <div class="flex flex-wrap items-center gap-2">
          <p>
            {{
              $t(
                'Al emparejar de nuevo todos los recorridos con el método actual cambian {n} enlaces en {walks} recorridos (están también en la lista de abajo).',
                { n: data.changes.length, walks: changedWalks },
              )
            }}
          </p>
          <button class="btn-ghost text-xs underline" @click="showChanges = !showChanges">
            {{ showChanges ? $t('Ocultar cambios') : $t('Ver cambios') }}
          </button>
          <button v-if="canReview" class="btn-primary ml-auto" :disabled="busy" @click="applyChanges">
            <Check :size="14" /> {{ $t('Aplicar {n} cambios', { n: data.changes.length }) }}
          </button>
          <span v-else class="hint ml-auto">{{ $t('Los aplica un revisor o administrador.') }}</span>
        </div>
        <ul v-if="showChanges" class="mt-2 space-y-0.5 text-xs">
          <li v-for="c in data.changes" :key="`${c.trackId}|${c.index}`">
            {{ day(c.date) }} {{ initials(c.collector) }} · «{{ c.text }}»:
            <span class="text-stone-500">{{ c.before ? rowLabel(c.before) : $t('sin fila') }}</span> →
            <b>{{ c.after ? rowLabel(c.after) : $t('sin fila') }}</b>
            <span class="text-stone-500"> ({{ $t(REASON[c.confidence]) }})</span>
          </li>
        </ul>
      </section>

      <p v-if="data && !groups.length" class="hint">{{ $t('No hay dudas de emparejamiento.') }}</p>

      <section v-for="g in groups" :key="walkKey(g.walk)" class="rounded-md border border-stone-200 bg-white">
        <header class="flex flex-wrap items-baseline gap-x-3 gap-y-1 border-b border-stone-100 px-3 py-2">
          <h2 class="text-base font-semibold">{{ day(g.walk.date) }} · {{ initials(g.walk.collector) }}</h2>
          <span class="text-sm text-stone-500">{{ g.walk.name }}</span>
          <span class="text-sm text-stone-500">
            <template v-if="g.walk.source === 'walk'">{{
              $t('por revisar: {n} puntos para emparejar', { n: g.doubts.length })
            }}</template>
            <template v-else>{{ $tn(g.doubts.length, '{n} duda', '{n} dudas') }}</template>
          </span>
          <a
            v-if="g.doubts[0]?.wikiloc"
            :href="g.doubts[0].wikiloc!"
            target="_blank"
            rel="noopener"
            class="btn-ghost text-sm"
            :title="$t('Abrir en Wikiloc')"
            ><ExternalLink :size="14"
          /></a>
          <button
            v-if="walkOfGroup(g.walk)"
            class="btn ml-auto"
            :title="$t('Todos los puntos del recorrido junto a todas las filas de ese día')"
            @click="boardWalk = walkOfGroup(g.walk)!.id"
          >
            <Columns2 :size="14" /> {{ $t('Tablero') }}
          </button>
          <button
            v-if="g.walk.source === 'walk' && session.canEdit"
            class="btn-primary"
            :disabled="busy"
            :title="$t('Guarda el recorrido en el mapa con las filas elegidas')"
            @click="storeWalk(g.walk, g.doubts)"
          >
            <MapPin :size="14" /> {{ $t('Pasar al mapa') }}
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
                  {{ $t('Sin foto') }}
                </div>
              </div>
              <figcaption class="mt-1 text-sm">
                «{{ d.text }}»
                <span v-if="d.indexes.length > 1" class="text-stone-500">
                  ({{ $t('{n} mariposas', { n: d.indexes.length }) }})</span
                >
                <span class="mt-0.5 block"><PointSection :lat="d.lat" :lon="d.lon" :row="rowOf(d)?.section ?? null" /></span>
              </figcaption>
            </figure>
            <div v-if="settledRow(d)" class="min-w-0 flex-1 text-sm">
              <p class="mb-1">
                <span class="rounded bg-amber-100 px-1 text-xs font-medium text-amber-800">{{
                  $t('La nota no coincide con su fila')
                }}</span>
              </p>
              <p class="mb-2 text-xs text-stone-500">
                {{
                  $t('Es la fila {n} ({why}); falta saber cuál de las dos tiene razón.', {
                    n: settledRow(d)!.row,
                    why: pairedBy(d, settledRow(d)!),
                  })
                }}
              </p>
              <table class="mb-2 w-full max-w-xl text-left">
                <thead class="text-xs text-stone-500">
                  <tr>
                    <th class="w-20 py-0.5 pr-2 font-normal"></th>
                    <th class="py-0.5 pr-2 font-normal">{{ $t('Nota de Wikiloc') }}</th>
                    <th class="py-0.5 font-normal">
                      {{ $t('Fila {n}', { n: settledRow(d)!.row })
                      }}<span v-if="settledRow(d)!.kind"> · {{ settledRow(d)!.kind }}</span>
                    </th>
                  </tr>
                </thead>
                <tbody>
                  <tr v-for="l in compare(d, settledRow(d)!)" :key="l.label" :class="l.differ ? 'bg-amber-50 font-medium' : ''">
                    <td class="py-0.5 pr-2 text-xs text-stone-500">{{ $t(l.label) }}</td>
                    <td class="py-0.5 pr-2" :class="l.differ ? 'text-amber-900' : ''">{{ l.note || '—' }}</td>
                    <td class="py-0.5" :class="l.differ ? 'text-amber-900' : ''">{{ l.row || '—' }}</td>
                  </tr>
                </tbody>
              </table>
              <ul v-if="opened.has(doubtKey(d))" class="mb-2 space-y-1">
                <li class="text-xs text-stone-500">{{ $t('Otras filas de ese día:') }}</li>
                <li v-for="r in d.candidates.filter(c => c.recordId !== settledRow(d)!.recordId)" :key="r.recordId">
                  <button
                    type="button"
                    class="w-full rounded border border-stone-200 px-2 py-1 text-left hover:border-brand-600 disabled:opacity-60"
                    :disabled="busy || !session.canEdit"
                    :title="$t('Es esta fila')"
                    @click="choose(d, r)"
                  >
                    {{ rowLabel(r) }}<span v-if="r.kind" class="text-stone-500"> · {{ r.kind }}</span>
                  </button>
                </li>
              </ul>
              <div class="flex flex-wrap gap-2">
                <button
                  class="btn py-0.5"
                  :disabled="busy || !session.canEdit"
                  :title="$t('Queda emparejado con esta fila y sale de las dudas; si la hoja está mal, corrígela en Tablas')"
                  @click="choose(d, settledRow(d))"
                >
                  <Check :size="13" /> {{ $t('Sí es la fila {n}', { n: settledRow(d)!.row }) }}
                </button>
                <button class="btn py-0.5" @click="toggleOthers(d)">
                  {{ opened.has(doubtKey(d)) ? $t('Ocultar otras filas') : $t('Es otra fila…') }}
                </button>
              </div>
            </div>
            <div v-else class="min-w-0 flex-1 text-sm">
              <p class="mb-1">
                <span class="rounded bg-amber-100 px-1 text-xs font-medium text-amber-800">{{ $t(REASON[d.confidence]) }}</span>
                <span
                  v-for="c in d.conflicts.filter(c => c !== 'hora' || d.confidence !== 'mark')"
                  :key="c"
                  class="ml-1 text-xs text-amber-800"
                  >{{ $t(CONFLICT[c]) }}</span
                >
                <span v-if="d.changed" class="ml-1 rounded bg-sky-100 px-1 text-xs text-sky-800">{{
                  $t('cambia al emparejar de nuevo')
                }}</span>
                <span
                  v-if="d.pending"
                  class="ml-1 rounded bg-stone-100 px-1 text-xs text-stone-700"
                  :title="$t('Pasar al mapa lo guardó sin fila por la duda; no aparece en el mapa hasta elegir su fila')"
                  >{{ $t('guardado sin fila') }}</span
                >
              </p>
              <p v-if="d.source === 'track'" class="mb-1 text-xs text-stone-500">
                {{ $t('Ahora:') }}
                {{
                  d.current
                    .filter(Boolean)
                    .map(r => rowLabel(r!))
                    .join('; ') || $t('sin fila')
                }}
              </p>
              <p class="mb-1 text-xs text-stone-500">{{ $t('Filas de ese día que puede ser (elige una):') }}</p>
              <ul class="space-y-1">
                <li v-for="r in d.candidates" :key="r.recordId">
                  <button
                    type="button"
                    class="w-full rounded border px-2 py-1 text-left hover:border-brand-600 disabled:opacity-60"
                    :class="chosenOf(d).includes(r.recordId) ? 'border-brand-600 bg-brand-50' : 'border-stone-200'"
                    :disabled="busy || !session.canEdit"
                    :title="chosenOf(d).includes(r.recordId) ? $t('Elegida') : $t('Es esta fila')"
                    @click="choose(d, r)"
                  >
                    <Check v-if="chosenOf(d).includes(r.recordId)" :size="13" class="mr-1 inline text-brand-700" />{{ rowLabel(r)
                    }}<span v-if="r.kind" class="text-stone-500"> · {{ r.kind }}</span
                    ><span v-if="isProposed(d, r)" class="ml-1 text-xs text-sky-700">{{ $t('(propuesta)') }}</span>
                  </button>
                </li>
              </ul>
              <div class="mt-2 flex flex-wrap gap-2">
                <button class="btn py-0.5" :disabled="busy || !session.canEdit" @click="choose(d, null)">
                  <X :size="13" /> {{ $t('No es ninguna') }}
                </button>
              </div>
            </div>
          </li>
        </ol>
      </section>
    </div>

    <PointPhotoViewer v-if="viewer" :photos="viewer.photos" :start="viewer.index" :text="viewer.text" @close="viewer = null" />
  </div>
</template>
