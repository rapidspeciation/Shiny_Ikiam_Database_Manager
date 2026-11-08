<script setup lang="ts">
import ChoiceField from '../ChoiceField.vue'
import { computed, defineAsyncComponent, nextTick, onBeforeUnmount, onMounted, ref } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { Check, Columns2, ExternalLink, MapPin, RefreshCw, X } from 'lucide-vue-next'
import PointPhotoViewer from './PointPhotoViewer.vue'
import PointSection from './PointSection.vue'
import IdentBox from './IdentBox.vue'
import WingPhoto from './WingPhoto.vue'
import { useMonitoring, type WikilocWalk } from '../../composables/useMonitoring'
import { api, requestId } from '../../lib/api'
import { appRoot } from '../../lib/appRoot'
import { formatSerial, isoToSerial } from '../../lib/dates'
import { t } from '../../lib/i18n'
import { formatMinutes, type MatchConfidence, type MatchConflict } from '../../lib/monitoring'
import { errorText, notify } from '../../lib/notice'
import { createEngine, type ModelFile } from '../../lib/offlineEngine'
import { SEX_FILE, SEX_GENERA, sexCall, sigmoid } from '../../lib/sexModel'
import { estimatedSection, sectionDiffers } from '../../lib/transects'
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
/** The identifier's reading of one photo (server/walk-ids.mjs): its first species and the wing box (fractions). */
interface Reading {
  photo: string
  species: [string, number][]
  box: number[] | null
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
  /** When the point was saved in Wikiloc (its GPS time, minutes of the day), if Wikiloc gave it. */
  gps?: number | null
  photos: string[]
  photoLinks: string[]
  /** The identifier's readings of its photos, for those already read. */
  readings?: Reading[]
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
    estimateAll()
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
  ]
  return lines.filter(l => l.note || l.row)
}
/** The table already shows why the note and its row disagree (a field differs): no sentence needed. */
const tableShowsIt = (d: Doubt, r: RowInfo) => compare(d, r).some(l => l.differ)

// ------------------------------------------------------------ evidence beside the table
/** The note's time, its row's and the GPS time of the point, as one line; `off` when two are more than 10 min apart. */
function times(d: Doubt, r: RowInfo | null) {
  const parts = (
    [
      ['nota', d.note?.minutes ?? null],
      ['fila', r?.minutes ?? null],
      ['GPS', d.gps ?? null],
    ] as [string, number | null][]
  ).filter((p): p is [string, number] => p[1] != null)
  if (!parts.length) return null
  const all = parts.map(p => p[1])
  return {
    text: parts.map(([label, m]) => `${label === 'GPS' ? 'GPS' : t(label)} ${formatMinutes(m)}`).join(' · '),
    off: parts.length > 1 && Math.max(...all) - Math.min(...all) > 10,
  }
}
/** The reading of the photo shown (the first one), else of any photo of the point. */
const readingOf = (d: Doubt) => d.readings?.find(r => r.photo === d.photos[0]) ?? d.readings?.[0] ?? null
const boxOf = (d: Doubt, photo: string) => d.readings?.find(r => r.photo === photo)?.box ?? null
const sameSpecies = (a: string | null | undefined, b: string | null | undefined) => !!a && !!b && lower(a) === lower(b)
/** The identifier's first three species, and the note's or the row's when they come lower in its list. */
function idLines(d: Doubt, r: RowInfo | null) {
  const list = readingOf(d)?.species ?? []
  return list
    .map(([name, p], i) => ({ name, p, note: sameSpecies(name, d.note?.species), row: sameSpecies(name, r?.species), i }))
    .filter(l => l.i < 3 || l.note || l.row)
}
/** A percentage; above 99.5 % it reads «> 99 %» (never a sure-looking 100 %). */
const pct = (p: number) => (p >= 0.995 ? '> 99 %' : `${Math.round(p * 100)} %`)
/** A Collected_Preserved row with a field mark: marked butterflies are released, so the rows may be shifted by one. */
const shifted = (r: RowInfo | null | undefined) => !!r?.markId && /preserv/i.test(r.kind || '')
/** How a candidate row agrees with the note: species, sex and its time against the note's (or the GPS time). */
function agreement(d: Doubt, r: RowInfo) {
  const n = d.note
  const out: { ok: boolean; label: string }[] = []
  if (n?.species && r.species) out.push({ ok: sameSpecies(n.species, r.species), label: t('especie') })
  if (n?.sex && r.sex) out.push({ ok: n.sex === r.sex, label: t('sexo') })
  const at = n?.minutes ?? d.gps ?? null
  if (at != null && r.minutes != null) {
    const diff = r.minutes - at
    out.push({ ok: Math.abs(diff) <= 10, label: diff === 0 ? t('misma hora') : `${diff > 0 ? '+' : '−'}${Math.abs(diff)} min` })
  }
  return out
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
/** The point's GPS section only when it differs from its row's Transect_section (evidence, not noise). */
const sectionOff = (d: Doubt) => {
  const row = rowOf(d)?.section ?? null
  const est = typeof d.lat === 'number' && typeof d.lon === 'number' ? estimatedSection(d.lat, d.lon) : null
  return !!row && !!est && sectionDiffers(est, row)
}

// ------------------------------------------------------------ sex estimate (team only, experimental; lib/sexModel.ts)
/**
 * The in-hand sex model of Identificar, run in this browser on the photo's wings, for the genera it was trained on.
 * Only once the model is on this device (downloaded here or on Identificar); shown in the Sexo line as «Estimado».
 */
const engine = createEngine(new URL('prueba-offline/', appRoot()).href, () => [], { signedIn: () => !!session.user })
const sexModel = ref<{ state: 'download'; mb: number; files: ModelFile[] } | { state: 'downloading'; done: number; total: number } | { state: 'ready' } | null>(null)
/** Logit per photo; null: no wings found. */
const sexZ = ref<Record<string, number | null>>({})
/** The genus the estimate is for: the identifier's first species, else the note's or the row's. */
function sexGenus(d: Doubt, r: RowInfo | null) {
  const name = readingOf(d)?.species[0]?.[0] || d.note?.species || r?.species || ''
  const genus = name.split(' ')[0]
  return SEX_GENERA.includes(genus) ? genus : null
}
function sexOf(d: Doubt, r: RowInfo | null) {
  const z = d.photos[0] ? sexZ.value[d.photos[0]] : undefined
  if (z == null || !sexGenus(d, r)) return null
  const call = sexCall(z)
  const female = sigmoid(z)
  // The probability of the sex called (of the likelier one when not sure), never P(hembra) beside a male call.
  const leans = call === 'hembra' || (call === 'no seguro' && female >= 0.5)
  return { call, sign: leans ? '♀' : '♂', p: leans ? female : 1 - female }
}
async function checkSexModel() {
  if (!session.canEdit) return
  const listing = await engine.list().catch(() => null)
  if (!listing || 'error' in listing || listing.source !== 'server') return
  if (!listing.files.some(f => f.role === 'sex' && f.name === SEX_FILE.name)) return
  if (await engine.sexReady(listing.files)) {
    sexModel.value = { state: 'ready' }
    estimateAll()
  } else
    sexModel.value = {
      state: 'download',
      mb: Math.max(1, Math.round(engine.sexToDownload(listing.files, await engine.stored()).plain / 1e6)),
      files: listing.files,
    }
}
async function downloadSexModel() {
  const m = sexModel.value
  if (m?.state !== 'download') return
  sexModel.value = { state: 'downloading', done: 0, total: 1 }
  try {
    await engine.downloadSex(m.files, (done, total) => (sexModel.value = { state: 'downloading', done, total }))
    sexModel.value = { state: 'ready' }
    estimateAll()
  } catch (e) {
    sexModel.value = m
    notify(errorText(e), 'error')
  }
}
let estimating = false
/** One photo after another, in the order of the list; each photo once. */
async function estimateAll() {
  if (estimating || sexModel.value?.state !== 'ready') return
  estimating = true
  try {
    for (const d of flat.value) {
      const id = d.photos[0]
      if (!id || id in sexZ.value || !sexGenus(d, rowOf(d))) continue
      try {
        const bytes = await (await fetch(photoUrl(id), { credentials: 'same-origin' })).arrayBuffer()
        const blob = new Blob([bytes], { type: 'image/jpeg' })
        const cut = await engine.square({ bytes, blob, name: `${id}.jpg`, type: 'image/jpeg', size: bytes.byteLength })
        sexZ.value = { ...sexZ.value, [id]: cut.status === 'success' ? await engine.sexLogit(cut.square) : null }
      } catch {
        sexZ.value = { ...sexZ.value, [id]: null }
      }
    }
  } finally {
    estimating = false
  }
}

// ------------------------------------------------------------ keys
/** Every card in the order shown; the focused one takes the keys (S, O, N, 1 to 9, j and k). */
const flat = computed(() => groups.value.flatMap(g => g.doubts))
const focusAt = ref(0)
const focused = computed(() => flat.value[Math.min(focusAt.value, flat.value.length - 1)] ?? null)
const isFocused = (d: Doubt) => !!focused.value && doubtKey(focused.value) === doubtKey(d)
function focusCard(d: Doubt) {
  const i = flat.value.findIndex(x => doubtKey(x) === doubtKey(d))
  if (i >= 0) focusAt.value = i
}
async function move(by: number) {
  focusAt.value = Math.max(0, Math.min(flat.value.length - 1, focusAt.value + by))
  await nextTick()
  document.querySelector('[data-doubt-focused]')?.scrollIntoView({ block: 'nearest', behavior: 'smooth' })
}
/** The rows a key number picks on the focused card: the other rows when open, or the card's rows. */
function keyRows(d: Doubt): RowInfo[] {
  const settled = settledRow(d)
  if (settled) return opened.value.has(doubtKey(d)) ? d.candidates.filter(c => c.recordId !== settled.recordId) : []
  return d.candidates
}
function onKey(event: KeyboardEvent) {
  if (event.ctrlKey || event.metaKey || event.altKey || viewer.value || boardWalk.value) return
  const el = event.target as HTMLElement | null
  if (el && (el.isContentEditable || /^(INPUT|TEXTAREA|SELECT)$/.test(el.tagName))) return
  const d = focused.value
  if (!d) return
  const key = event.key.toLowerCase()
  const can = session.canEdit && !busy.value
  if (key === 'j' || key === 'k') move(key === 'j' ? 1 : -1)
  else if (key === 's' && can && settledRow(d)) choose(d, settledRow(d))
  else if (key === 'o' && settledRow(d)) toggleOthers(d)
  else if (key === 'n' && can && !settledRow(d)) choose(d, null)
  else if (/^[1-9]$/.test(key) && can && keyRows(d)[Number(key) - 1]) choose(d, keyRows(d)[Number(key) - 1])
  else return
  event.preventDefault()
}
onMounted(() => {
  window.addEventListener('keydown', onKey)
  checkSexModel()
})
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
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
      <div class="order-last flex basis-full flex-wrap items-baseline gap-x-4 gap-y-1 pb-1 text-xs">
        <details class="hint">
          <summary class="cursor-pointer select-none">{{ $t('¿Cómo funciona?') }}</summary>
          <p class="mt-1 max-w-3xl">
            {{
              $t(
                'Cada tarjeta es un punto de Wikiloc (una mariposa) y la fila de Collection_data que le corresponde. No son recapturas: «La nota no coincide» es un punto ya emparejado donde la nota y la hoja dicen algo distinto; «¿Qué fila es?» es un punto que puede ser más de una fila.',
              )
            }}
          </p>
        </details>
        <p v-if="session.canEdit" class="key-hint hint">
          {{ $t('Teclas: j/k mover · S sí es la fila · O otra fila · N ninguna · 1–9 elegir fila') }}
        </p>
        <button v-if="sexModel?.state === 'download'" class="btn-ghost text-xs underline" @click="downloadSexModel">
          {{ $t('Sexo estimado (experimental): descargar el modelo, {mb} MB', { mb: sexModel.mb }) }}
        </button>
        <span v-else-if="sexModel?.state === 'downloading'" class="hint tabular-nums">{{
          $t('Descargando el modelo de sexo: {done} de {total} MB', {
            done: Math.round(sexModel.done / 1e6),
            total: Math.round(sexModel.total / 1e6),
          })
        }}</span>
      </div>
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

      <section v-for="g in groups" :key="walkKey(g.walk)" class="rounded-md border border-stone-200 bg-surface">
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
          <li
            v-for="d in g.doubts"
            :key="`${walkKey(d)}|${d.indexes[0]}`"
            class="doubt flex flex-col gap-3 p-3 md:flex-row"
            :class="{ 'doubt-focused': isFocused(d) }"
            :data-doubt-focused="isFocused(d) ? '' : undefined"
            @pointerdown="focusCard(d)"
          >
            <figure class="w-full shrink-0 md:w-56">
              <div class="flex gap-1">
                <button
                  v-for="(id, k) in d.photos.slice(0, 2)"
                  :key="id"
                  type="button"
                  class="block flex-1 overflow-hidden rounded bg-stone-100"
                  @click="viewer = { photos: d.photos, index: k, text: d.text }"
                >
                  <WingPhoto :src="photoUrl(id)" :box="boxOf(d, id)" class="h-36" />
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
                <span v-if="sectionOff(d)" class="mt-0.5 block"
                  ><PointSection :lat="d.lat" :lon="d.lon" :row="rowOf(d)?.section ?? null"
                /></span>
              </figcaption>
            </figure>
            <div v-if="settledRow(d)" class="min-w-0 flex-1 text-sm">
              <p class="mb-1 flex flex-wrap gap-1">
                <span class="rounded bg-amber-100 px-1 text-xs font-medium text-amber-800">{{
                  $t('La nota no coincide con su fila')
                }}</span>
                <span v-if="shifted(settledRow(d))" class="rounded bg-amber-100 px-1 text-xs font-medium text-amber-800">{{
                  $t('Fila conservada con marca de campo: posible fila corrida')
                }}</span>
              </p>
              <p v-if="!tableShowsIt(d, settledRow(d)!)" class="mb-2 text-xs text-stone-500">
                {{
                  $t('Es la fila {n} ({why}); falta saber cuál de las dos tiene razón.', {
                    n: settledRow(d)!.row,
                    why: pairedBy(d, settledRow(d)!),
                  })
                }}
              </p>
              <div class="mb-2 flex flex-wrap items-start gap-x-4 gap-y-2">
              <table class="min-w-0 max-w-xl flex-1 basis-72 text-left">
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
                    <td class="py-0.5" :class="l.differ ? 'text-amber-900' : ''">
                      {{ l.row || '—' }}
                      <span
                        v-if="l.label === 'Sexo' && sexOf(d, settledRow(d))"
                        class="estimate ml-1 inline-block rounded border border-stone-300 px-1 text-xs font-normal whitespace-nowrap"
                        :class="
                          sexOf(d, settledRow(d))!.call === 'hembra'
                            ? 'text-fill-pink'
                            : sexOf(d, settledRow(d))!.call === 'macho'
                              ? 'text-fill-sky'
                              : 'text-stone-600'
                        "
                        :title="$t('Modelo experimental de sexo en fotos en mano, calculado en este navegador sobre las alas')"
                        ><template v-if="sexOf(d, settledRow(d))!.call === 'no seguro'"
                          >{{ $t('Estimado') }} {{ $t('no seguro') }} ({{ sexOf(d, settledRow(d))!.sign }}
                          {{ pct(sexOf(d, settledRow(d))!.p) }})</template
                        ><template v-else
                          >{{ $t('Estimado') }} {{ sexOf(d, settledRow(d))!.sign }} {{ pct(sexOf(d, settledRow(d))!.p) }}</template
                        >
                        · <i>{{ $t('experimental') }}</i></span
                      >
                    </td>
                  </tr>
                  <tr v-if="times(d, settledRow(d))">
                    <td class="py-0.5 pr-2 text-xs text-stone-500">{{ $t('Hora') }}</td>
                    <td colspan="2" class="py-0.5">
                      <span
                        class="rounded px-1 text-xs tabular-nums"
                        :class="times(d, settledRow(d))!.off ? 'bg-amber-100 font-medium text-amber-900' : 'bg-stone-100 text-stone-700'"
                        :title="times(d, settledRow(d))!.off ? $t('Más de 10 min entre dos de estas horas') : undefined"
                        >{{ times(d, settledRow(d))!.text }}</span
                      >
                    </td>
                  </tr>
                </tbody>
              </table>
              <IdentBox v-if="idLines(d, settledRow(d)).length" :lines="idLines(d, settledRow(d))" />
              </div>
              <ul v-if="opened.has(doubtKey(d))" class="mb-2 space-y-1">
                <li class="text-xs text-stone-500">{{ $t('Otras filas de ese día:') }}</li>
                <li v-for="(r, k) in d.candidates.filter(c => c.recordId !== settledRow(d)!.recordId)" :key="r.recordId">
                  <button
                    type="button"
                    class="w-full rounded border border-stone-200 px-2 py-1 text-left hover:border-brand-600 disabled:opacity-60"
                    :disabled="busy || !session.canEdit"
                    :title="$t('Es esta fila')"
                    @click="choose(d, r)"
                  >
                    <kbd v-if="isFocused(d) && k < 9" class="key-hint">{{ k + 1 }}</kbd>
                    {{ rowLabel(r) }}<span v-if="r.kind" class="text-stone-500"> · {{ r.kind }}</span>
                    <span class="ml-1 inline-flex flex-wrap gap-x-1.5 text-xs">
                      <span v-for="a in agreement(d, r)" :key="a.label" :class="a.ok ? 'text-green-700' : 'text-amber-800'"
                        >{{ a.ok ? '✓' : '✗' }} {{ a.label }}</span
                      >
                    </span>
                    <span v-if="shifted(r)" class="ml-1 rounded bg-amber-100 px-1 text-xs text-amber-800">{{
                      $t('Fila conservada con marca de campo: posible fila corrida')
                    }}</span>
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
                  v-if="times(d, null)"
                  class="ml-1 rounded px-1 text-xs tabular-nums"
                  :class="times(d, null)!.off ? 'bg-amber-100 font-medium text-amber-900' : 'bg-stone-100 text-stone-700'"
                  >{{ times(d, null)!.text }}</span
                >
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
              <div class="flex flex-wrap items-start gap-x-4 gap-y-2">
              <ul class="min-w-0 flex-1 basis-72 space-y-1">
                <li v-for="(r, k) in d.candidates" :key="r.recordId">
                  <button
                    type="button"
                    class="w-full rounded border px-2 py-1 text-left hover:border-brand-600 disabled:opacity-60"
                    :class="chosenOf(d).includes(r.recordId) ? 'border-brand-600 bg-brand-50' : 'border-stone-200'"
                    :disabled="busy || !session.canEdit"
                    :title="chosenOf(d).includes(r.recordId) ? $t('Elegida') : $t('Es esta fila')"
                    @click="choose(d, r)"
                  >
                    <kbd v-if="isFocused(d) && k < 9" class="key-hint">{{ k + 1 }}</kbd>
                    <Check v-if="chosenOf(d).includes(r.recordId)" :size="13" class="mr-1 inline text-brand-700" />{{ rowLabel(r)
                    }}<span v-if="r.kind" class="text-stone-500"> · {{ r.kind }}</span
                    ><span v-if="isProposed(d, r)" class="ml-1 text-xs text-sky-700">{{ $t('(propuesta)') }}</span>
                    <span class="ml-1 inline-flex flex-wrap gap-x-1.5 text-xs">
                      <span v-for="a in agreement(d, r)" :key="a.label" :class="a.ok ? 'text-green-700' : 'text-amber-800'"
                        >{{ a.ok ? '✓' : '✗' }} {{ a.label }}</span
                      >
                    </span>
                    <span v-if="shifted(r)" class="ml-1 rounded bg-amber-100 px-1 text-xs text-amber-800">{{
                      $t('Fila conservada con marca de campo: posible fila corrida')
                    }}</span>
                  </button>
                </li>
              </ul>
              <IdentBox v-if="idLines(d, rowOf(d)).length" :lines="idLines(d, rowOf(d))" />
              </div>
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

<style scoped>
/* The card the keys act on (S, O, N, 1 to 9; j and k move it): marked on screens with a mouse only. */
@media (hover: hover) {
  .doubt-focused {
    box-shadow: inset 3px 0 0 var(--color-brand, #2f6b4f);
  }
}
.key-hint {
  display: none;
}
@media (hover: hover) and (pointer: fine) {
  .key-hint {
    display: initial;
  }
  kbd.key-hint {
    display: inline-block;
    min-width: 1.1rem;
    margin-right: 0.25rem;
    border: 1px solid var(--color-border-strong, #a8a29e);
    border-radius: 0.25rem;
    text-align: center;
    font-size: 0.7rem;
    line-height: 1rem;
  }
}
</style>
