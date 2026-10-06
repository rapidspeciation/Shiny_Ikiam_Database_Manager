<script setup lang="ts">
import { computed, defineAsyncComponent, ref, shallowRef, watch } from 'vue'
import { Ban, Check, ChevronLeft, ExternalLink, MapPin, RotateCcw, Rows3, Undo2, X } from 'lucide-vue-next'
import PointPhotoViewer from './PointPhotoViewer.vue'
import PointSection from './PointSection.vue'
import type { MiniPoint } from './TrailMiniMap.vue'
import { useMonitoring } from '../../composables/useMonitoring'
import { api, requestId } from '../../lib/api'
import { formatSerial, isoToSerial } from '../../lib/dates'
import { t } from '../../lib/i18n'
import { formatMinutes } from '../../lib/monitoring'
import { errorText, notify } from '../../lib/notice'
import {
  CONFIDENCE,
  CONFLICT,
  boardSummary,
  buildBoard,
  click,
  confirm as confirmPair,
  decide,
  droppedRows,
  isDecided,
  linksToStore,
  pointOfRow,
  recaptureRowValues,
  toggleRowWithoutPoint,
  type Board,
  type BoardPoint,
  type BoardRow,
  type BoardState,
  type Selection,
} from '../../lib/walkBoard'
import { usePending } from '../../stores/pending'
import { useSession } from '../../stores/session'

// Leaflet loads with the board's map only.
const TrailMiniMap = defineAsyncComponent(() => import('./TrailMiniMap.vue'))

/**
 * The pairing board of one Wikiloc walk: its points in walk order beside the
 * collector's rows of that day, the pairs the app suggests, and what a person
 * decides (lib/walkBoard). Clicking a point and then a row (or a row and then a
 * point) pairs or unpairs them. "Importar recorrido" stores the walk on the map
 * with those rows, as Pasar al mapa does, once every point is decided.
 */
const props = defineProps<{ walkId: string }>()
const emit = defineEmits<{ close: []; stored: [] }>()

const MODULE = 'Collection_data'
const session = useSession()
const pending = usePending()
const { table, taxa, localTaxa, options, createFormulas, walks, tracks, tracksLoaded, loadTracks, loadWalks } = useMonitoring()

const walk = computed(() => walks.value.find(w => w.id === props.walkId) || null)
const board = shallowRef<Board | null>(null)
const state = ref<BoardState | null>(null)
const busy = ref(false)

/** Built once the sheet, the walk and (for an imported walk) its track are read; again only on asking. */
function build() {
  const w = walk.value
  if (!table.value || !w || (w.trackId && !tracksLoaded.value)) return
  const track = w.trackId ? tracks.value.find(x => x.id === w.trackId) || null : null
  board.value = buildBoard(table.value.rows, w, taxa.value, localTaxa.value, {
    track,
    pending: pending.creates.filter(c => c.module === MODULE),
  })
  state.value = board.value ? structuredClone(board.value.initial) : null
}
watch(
  () => [props.walkId, !!table.value, !!walk.value, tracksLoaded.value],
  () => {
    if (!board.value || board.value.walk.id !== props.walkId) build()
  },
  { immediate: true },
)
function startAgain() {
  if (state.value && board.value) for (const id of droppedRows(state.value, board.value.initial)) pending.removeCreate(id)
  build()
}

/** Every change of state goes through here: a new recapture row the state no longer holds leaves the unsaved changes. */
function apply(next: BoardState) {
  if (state.value) for (const id of droppedRows(state.value, next)) pending.removeCreate(id)
  state.value = next
}

const rowById = computed(() => new Map((board.value?.rows || []).map(r => [r.id, r])))
const summary = computed(() => (board.value && state.value ? boardSummary(board.value, state.value) : null))
const decided = (i: number) => !!board.value && !!state.value && isDecided(board.value, state.value, i)
const decision = (i: number) => state.value?.decisions[i] ?? null
const rowsOf = (i: number) => {
  const d = decision(i)
  return d?.kind === 'rows' ? d.ids.map(id => rowById.value.get(id)).filter((r): r is BoardRow => !!r) : []
}
const pointOf = (id: string) => (state.value ? pointOfRow(state.value, id) : -1)
const withoutPoint = (id: string) => !!state.value?.rowsWithoutPoint.includes(id)

const selectedPoint = computed(() => {
  const s = state.value?.selected
  return s && 'point' in s ? s.point : null
})
const selectedRow = computed(() => {
  const s = state.value?.selected
  return s && 'row' in s ? s.row : null
})
/** Hovering a point or a row lights up its partner. */
const hover = ref<Selection | null>(null)
const lit = (target: Selection) => {
  const h = hover.value
  if (!h) return false
  if ('point' in target && 'row' in h) return pointOf(h.row) === target.point
  if ('row' in target && 'point' in h) return rowsOf(h.point).some(r => r.id === target.row)
  return false
}

function select(target: Selection) {
  if (!board.value || !state.value || !session.canEdit) return
  apply(click(board.value, state.value, target))
}

// ------------------------------------------------------------ labels
const day = (iso: string) => formatSerial(isoToSerial(iso))
const initials = (c: string | null | undefined) => (c || '').split(' - ')[0].trim()
const sexLabel = (s: string | null) => (s === 'female' ? t('hembra') : s === 'male' ? t('macho') : '')
const pointName = (i: number) => t('Punto {n}', { n: i + 1 })
const species = (r: { species: string | null; subspecies: string | null }) =>
  [r.species, r.subspecies].filter(Boolean).join(' ') || t('sin especie')
/** How the point's rows were decided, in a word. */
function pairLabel(p: BoardPoint) {
  const d = decision(p.index)
  if (d?.kind !== 'rows') return ''
  if (d.by === 'stored') return t('guardada')
  if (d.by === 'person') return t('elegida')
  return t('sugerida: {how}', { how: t(CONFIDENCE[p.suggestion.confidence]) })
}
/** Where the note disagrees with its suggested row (once a person chose, it is answered). */
function conflicts(p: BoardPoint) {
  const d = decision(p.index)
  if (d?.kind !== 'rows' || d.by !== 'app') return []
  return p.suggestion.conflicts.filter(c => c !== 'hora' || p.suggestion.confidence !== 'mark').map(c => t(CONFLICT[c]))
}

// ------------------------------------------------------------ actions on a point
function neverEntered(p: BoardPoint) {
  if (state.value) apply(decide(state.value, p.index, { kind: 'none' }))
}
function undecide(p: BoardPoint) {
  if (state.value) apply(decide(state.value, p.index, null))
}
function keep(p: BoardPoint) {
  if (state.value) apply(confirmPair(state.value, p.index))
}
/** A recapture that is not a row (in notes, or only in Wikiloc) becomes its own Mark_Released row (unsaved until Guardar). */
function makeRecapture(p: BoardPoint) {
  if (!board.value || !state.value) return
  const values = recaptureRowValues(board.value, p, options.value.Collector || [])
  if (!values) return
  for (const field of createFormulas.value) delete values[field]
  const mark = String(values.FieldMark_ID ?? '')
  // Saved with Guardar, like the rows of an import: nothing reaches the sheet before.
  const item = pending.addCreate(MODULE, `${mark} ${t('recaptura')}`.trim(), values, { manual: true })
  pending.touch()
  apply(decide(state.value, p.index, { kind: 'new', clientId: item.clientId }))
  notify(t('Fila de recaptura {mark} añadida: revisa y pulsa Guardar', { mark }), 'success', 2500)
}
function rowWithoutPoint(id: string) {
  if (state.value) apply(toggleRowWithoutPoint(state.value, id))
}

// ------------------------------------------------------------ storing the walk
async function importWalk() {
  if (!board.value || !state.value || !summary.value?.ready) return
  const b = board.value
  const imported = b.walk.status === 'imported'
  const question = imported
    ? t('¿Guardar el emparejamiento de "{name}"? Las filas de la hoja no cambian.', { name: b.walk.name })
    : t('¿Importar "{name}" al mapa con estas filas? Las filas de la hoja no cambian.', { name: b.walk.name })
  if (!confirm(question)) return
  busy.value = true
  try {
    await api(`monitoring/wikiloc/${encodeURIComponent(b.walk.id)}/store`, {
      method: 'POST',
      body: {
        requestId: requestId(),
        date: b.date,
        collector: b.collector,
        links: linksToStore(b, state.value),
        rowsWithoutPoint: state.value.rowsWithoutPoint,
      },
    })
    notify(imported ? t('Emparejamiento guardado') : t('Recorrido importado al mapa'), 'success')
    await Promise.all([loadTracks(), loadWalks()])
    emit('stored')
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    busy.value = false
  }
}

// ------------------------------------------------------------ map and photos
const mapPoints = computed<MiniPoint[]>(() =>
  (board.value?.points || []).map(p => ({
    lat: p.capture.lat,
    lon: p.capture.lon,
    label: `${pointName(p.index)} · «${p.capture.text}»`,
    state: decision(p.index)?.kind === 'none' ? 'none' : decided(p.index) ? 'paired' : 'open',
  })),
)
const mapSelected = computed(() => {
  if (selectedPoint.value !== null) return selectedPoint.value
  const s = selectedRow.value ?? (hover.value && 'row' in hover.value ? hover.value.row : null)
  const i = s ? pointOf(s) : -1
  return i >= 0 ? i : hover.value && 'point' in hover.value ? hover.value.point : null
})
const trackLine = computed<[number, number][]>(() => (walk.value?.track || []).map(p => [p[0], p[1]]))
const photoUrl = (id: string) => `api/monitoring/photos/${id}`
const viewer = ref<{ photos: string[]; index: number; text: string } | null>(null)
</script>

<template>
  <div class="relative">
    <div class="toolbar">
      <button class="btn" @click="emit('close')"><ChevronLeft :size="14" /> {{ $t('Dudas') }}</button>
      <div v-if="board" class="min-w-0">
        <h2 class="text-base font-semibold">
          {{ day(board.date) }} · {{ initials(board.collector) }}
          <span class="text-sm font-normal text-stone-500">{{ board.walk.name }}</span>
          <a
            :href="board.walk.url"
            target="_blank"
            rel="noopener"
            class="btn-ghost ml-1 inline-flex text-sm"
            :title="$t('Abrir en Wikiloc')"
            ><ExternalLink :size="14"
          /></a>
        </h2>
        <p v-if="summary" class="hint text-xs">
          <span
            class="rounded px-1"
            :class="board.walk.status === 'waiting' ? 'bg-amber-100 text-amber-800' : 'bg-stone-100 text-stone-700'"
            >{{ board.walk.status === 'waiting' ? $t('por revisar') : $t('importado') }}</span
          >
          · {{ $tn(board.points.length, '{n} punto', '{n} puntos') }} · {{ $tn(board.rows.length, '{n} fila', '{n} filas') }} ·
          <span :class="summary.undecided.length ? 'font-medium text-amber-800' : ''">{{
            $tn(summary.undecided.length, '{n} punto sin decidir', '{n} puntos sin decidir')
          }}</span>
          ·
          <span :class="summary.unpairedRows.length ? 'font-medium text-amber-800' : ''">{{
            $tn(summary.unpairedRows.length, '{n} fila sin punto', '{n} filas sin punto')
          }}</span>
        </p>
      </div>
      <div class="ml-auto flex flex-wrap gap-2">
        <button
          class="btn"
          :disabled="busy || !board"
          :title="$t('Deshace lo decidido aquí y vuelve a lo guardado y sugerido')"
          @click="startAgain"
        >
          <RotateCcw :size="14" /> {{ $t('Empezar de nuevo') }}
        </button>
        <button
          v-if="board && session.canEdit"
          class="btn-primary"
          :disabled="busy || !summary?.ready"
          :title="
            summary?.ready
              ? $t('Guarda el recorrido en el mapa con las filas elegidas')
              : $t('Falta decidir {n} puntos', { n: summary?.undecided.length ?? 0 })
          "
          @click="importWalk"
        >
          <MapPin :size="14" /> {{ board.walk.status === 'imported' ? $t('Guardar emparejamiento') : $t('Importar recorrido') }}
        </button>
      </div>
    </div>

    <p v-if="!table || !walk" class="hint p-4">{{ $t('Cargando…') }}</p>
    <p v-else-if="!board" class="hint p-4">
      {{ $t('Este recorrido no tiene fecha o colector: complétalos en Importar recorrido.') }}
    </p>
    <template v-else-if="state">
      <p class="hint px-3 pt-2 text-xs sm:px-4">
        {{
          $t(
            'Toca un punto y luego su fila (o la fila y luego el punto) para emparejarlos; tocar una pareja ya unida la separa. Los puntos en ámbar están sin decidir; las filas en ámbar no tienen punto.',
          )
        }}
      </p>
      <div class="grid gap-3 p-3 sm:p-4 lg:grid-cols-[minmax(0,1fr)_minmax(0,1fr)_22rem]">
        <!-- The map first on phones (small), beside the lists on wide screens. -->
        <div class="h-48 lg:sticky lg:top-2 lg:order-last lg:h-[28rem]">
          <TrailMiniMap :points="mapPoints" :selected="mapSelected" :track="trackLine" @select="i => select({ point: i })" />
        </div>

        <section class="min-w-0">
          <h3 class="mb-1 text-sm font-semibold">{{ $t('Puntos del recorrido ({n})', { n: board.points.length }) }}</h3>
          <ol class="space-y-1.5">
            <li
              v-for="p in board.points"
              :key="p.index"
              class="flex cursor-pointer gap-2 rounded border p-1.5 text-sm"
              :class="[
                selectedPoint === p.index
                  ? 'border-brand-600 ring-2 ring-brand-600'
                  : lit({ point: p.index })
                    ? 'border-sky-400 bg-sky-50'
                    : decided(p.index)
                      ? 'border-stone-200 bg-white'
                      : 'border-amber-300 bg-amber-50',
              ]"
              @click="select({ point: p.index })"
              @mouseenter="hover = { point: p.index }"
              @mouseleave="hover = null"
            >
              <button
                v-if="p.capture.photos.length"
                type="button"
                class="h-16 w-16 shrink-0 overflow-hidden rounded bg-stone-100"
                :title="$t('Ver fotos')"
                @click.stop="viewer = { photos: p.capture.photos, index: 0, text: p.capture.text }"
              >
                <img :src="photoUrl(p.capture.photos[0])" alt="" loading="lazy" class="h-full w-full object-cover" />
              </button>
              <div
                v-else
                class="flex h-16 w-16 shrink-0 items-center justify-center rounded border border-dashed border-stone-300 text-center text-[10px] text-stone-400"
              >
                {{ $t('Sin foto') }}
              </div>
              <div class="min-w-0 flex-1">
                <p class="flex flex-wrap items-baseline gap-x-1.5">
                  <b>{{ pointName(p.index) }}</b>
                  <span v-if="p.capture.minutes !== null">{{ formatMinutes(p.capture.minutes) }}</span>
                  <span v-else class="text-xs text-stone-400">{{ $t('sin hora') }}</span>
                  <span v-if="p.capture.markId" class="font-medium">{{ p.capture.markId }}</span>
                  <PointSection
                    :lat="p.capture.lat"
                    :lon="p.capture.lon"
                    :row="rowsOf(p.index).length === 1 ? rowsOf(p.index)[0].section : undefined"
                  />
                </p>
                <p class="break-words text-stone-700">«{{ p.capture.text }}»</p>
                <p class="mt-0.5 flex flex-wrap items-center gap-1 text-xs">
                  <template v-if="decision(p.index)?.kind === 'rows'">
                    <span v-for="r in rowsOf(p.index)" :key="r.id" class="rounded bg-brand-50 px-1 text-brand-800">
                      → {{ $t('fila {n}', { n: r.row }) }}
                    </span>
                    <span :class="decided(p.index) ? 'text-stone-500' : 'font-medium text-amber-800'">{{ pairLabel(p) }}</span>
                    <span v-for="c in conflicts(p)" :key="c" class="text-amber-800">{{ c }}</span>
                  </template>
                  <span v-else-if="decision(p.index)?.kind === 'none'" class="rounded bg-stone-100 px-1 text-stone-700">{{
                    $t('Nunca se pasó a la hoja')
                  }}</span>
                  <span v-else-if="decision(p.index)?.kind === 'new'" class="rounded bg-sky-100 px-1 text-sky-800">{{
                    $t('Fila de recaptura nueva, sin guardar')
                  }}</span>
                  <span v-else class="font-medium text-amber-800">{{ $t('Sin decidir') }}</span>
                  <span v-if="p.recapture && decision(p.index)?.kind !== 'new'" class="text-sky-800" :title="p.recapture.note">{{
                    p.recapture.note
                      ? $t('recaptura escrita en la nota de la fila {n}', { n: p.recapture.row.row })
                      : $t('recaptura de la marca de la fila {n}, sin fila propia', { n: p.recapture.row.row })
                  }}</span>
                </p>
                <div v-if="selectedPoint === p.index && session.canEdit" class="mt-1 flex flex-wrap gap-1.5" @click.stop>
                  <button
                    v-if="decision(p.index)?.kind === 'rows' && !decided(p.index)"
                    class="btn py-0.5 text-xs"
                    :title="$t('La fila sugerida es la correcta')"
                    @click="keep(p)"
                  >
                    <Check :size="13" /> {{ $t('Sí es esa fila') }}
                  </button>
                  <button
                    v-if="decision(p.index)?.kind !== 'none'"
                    class="btn py-0.5 text-xs"
                    :title="$t('El punto queda en el mapa sin fila: la mariposa nunca se anotó en la hoja')"
                    @click="neverEntered(p)"
                  >
                    <Ban :size="13" /> {{ $t('Nunca se pasó a la hoja (error de campo)') }}
                  </button>
                  <button
                    v-if="p.recapture && decision(p.index)?.kind !== 'new'"
                    class="btn py-0.5 text-xs"
                    :title="
                      $t(
                        'Añade una fila Mark_Released con la marca y la especie de la fila {n} y la fecha, hora y transecto del punto (sin guardar)',
                        {
                          n: p.recapture.row.row,
                        },
                      )
                    "
                    @click="makeRecapture(p)"
                  >
                    <Rows3 :size="13" /> {{ $t('Hacer fila de recaptura') }}
                  </button>
                  <button
                    v-if="decision(p.index)"
                    class="btn py-0.5 text-xs"
                    :title="$t('El punto vuelve a quedar sin decidir')"
                    @click="undecide(p)"
                  >
                    <Undo2 :size="13" /> {{ $t('Quitar lo decidido') }}
                  </button>
                </div>
              </div>
            </li>
          </ol>
        </section>

        <section class="min-w-0">
          <h3 class="mb-1 text-sm font-semibold">
            {{ $t('Filas de ese día de {who} ({n})', { who: initials(board.collector), n: board.rows.length }) }}
          </h3>
          <p v-if="!board.rows.length" class="hint text-sm">{{ $t('No hay filas de este colector ese día.') }}</p>
          <ol class="space-y-1.5">
            <li
              v-for="r in board.rows"
              :key="r.id"
              class="cursor-pointer rounded border px-2 py-1.5 text-sm"
              :class="[
                selectedRow === r.id
                  ? 'border-brand-600 ring-2 ring-brand-600'
                  : lit({ row: r.id })
                    ? 'border-sky-400 bg-sky-50'
                    : pointOf(r.id) >= 0
                      ? 'border-stone-200 bg-white'
                      : withoutPoint(r.id)
                        ? 'border-stone-200 bg-stone-50 text-stone-500'
                        : 'border-amber-300 bg-amber-50',
              ]"
              @click="select({ row: r.id })"
              @mouseenter="hover = { row: r.id }"
              @mouseleave="hover = null"
            >
              <p class="flex flex-wrap items-baseline gap-x-1.5">
                <b>{{ $t('fila {n}', { n: r.row }) }}</b>
                <i>{{ species(r) }}</i>
                <span v-if="r.sex" class="text-stone-500">{{ sexLabel(r.sex) }}</span>
                <span>{{ r.minutes !== null ? formatMinutes(r.minutes) : $t('sin hora') }}</span>
                <span>{{ r.section ? `T${r.section}` : $t('sin transecto') }}</span>
                <span v-if="r.markId" class="font-medium">{{ r.markId }}</span>
                <span v-if="r.kind" class="text-xs text-stone-500">{{ r.kind }}</span>
              </p>
              <p class="mt-0.5 flex flex-wrap items-center gap-1 text-xs">
                <template v-if="pointOf(r.id) >= 0">
                  <span class="rounded bg-brand-50 px-1 text-brand-800">← {{ pointName(pointOf(r.id)) }}</span>
                  <span :class="decided(pointOf(r.id)) ? 'text-stone-500' : 'font-medium text-amber-800'">{{
                    pairLabel(board.points[pointOf(r.id)])
                  }}</span>
                </template>
                <span v-else-if="withoutPoint(r.id)" class="rounded bg-stone-100 px-1 text-stone-700">{{
                  $t('Fila sin punto')
                }}</span>
                <span v-else class="font-medium text-amber-800">{{ $t('Sin punto') }}</span>
              </p>
              <div
                v-if="selectedRow === r.id && session.canEdit && pointOf(r.id) < 0"
                class="mt-1 flex flex-wrap gap-1.5"
                @click.stop
              >
                <button
                  class="btn py-0.5 text-xs"
                  :title="$t('La fila no tiene punto en Wikiloc (el punto no se marcó en el campo)')"
                  @click="rowWithoutPoint(r.id)"
                >
                  <template v-if="withoutPoint(r.id)"><Undo2 :size="13" /> {{ $t('Sí tiene punto') }}</template>
                  <template v-else><Ban :size="13" /> {{ $t('Fila sin punto') }}</template>
                </button>
              </div>
            </li>
          </ol>
        </section>
      </div>

      <!-- What the next tap does (phones: the other column is below). -->
      <div
        v-if="state.selected"
        class="sticky bottom-0 z-10 flex items-center gap-2 border-t border-brand-200 bg-brand-50 px-3 py-2 text-sm"
      >
        <span v-if="selectedPoint !== null">{{ $t('{point} elegido: toca su fila', { point: pointName(selectedPoint) }) }}</span>
        <span v-else-if="selectedRow">{{
          $t('Fila {n} elegida: toca su punto', { n: rowById.get(selectedRow)?.row ?? '' })
        }}</span>
        <button class="btn-ghost ml-auto" :title="$t('Cancelar')" @click="state = { ...state, selected: null }">
          <X :size="16" />
        </button>
      </div>
    </template>

    <PointPhotoViewer v-if="viewer" :photos="viewer.photos" :start="viewer.index" :text="viewer.text" @close="viewer = null" />
  </div>
</template>
