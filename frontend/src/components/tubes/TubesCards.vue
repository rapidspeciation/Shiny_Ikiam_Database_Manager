<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, reactive, ref, toRaw, watch } from 'vue'
import {
  AlertTriangle,
  ChevronDown,
  ChevronUp,
  Check,
  CheckCircle2,
  ChevronRight,
  Circle,
  History,
  Loader2,
  Plus,
  Printer,
  RotateCcw,
  ScanLine,
  Search,
  Undo2,
  X,
} from 'lucide-vue-next'
import DateField from '../DateField.vue'
import EntryModeToggle from '../EntryModeToggle.vue'
import RowDrawer from '../RowDrawer.vue'
import SexBadge from '../SexBadge.vue'
import TubeLabels from '../TubeLabels.vue'
import LifeBadge from '../deaths/LifeBadge.vue'
import TabHistory from '../history/TabHistory.vue'
import TubeScanner from './TubeScanner.vue'
import { useTubesState } from '../../composables/useTubesState'
import type { EntryMode } from '../../composables/useEntryMode'
import { useKeyboard, useMedia } from '../../composables/usePhone'
import { api, requestId } from '../../lib/api'
import { isBlank } from '../../lib/cells'
import { noteDay } from '../../lib/clutches'
import { dayLabel, formatSerial, isoToSerial, serialFromIso, serialToIso, todayIso } from '../../lib/dates'
import {
  bestRack,
  buildIndex,
  factsOf,
  lifeOf,
  lookAlikes,
  searchKey,
  suggest,
  usedSamples,
  type DeathCell,
  type Entry,
  type Facts,
  type RackSuggestion,
} from '../../lib/deaths'
import { idTokens, resolveIds } from '../../lib/ids'
import { errorText, notify } from '../../lib/notice'
import { persistentRef } from '../../lib/persist'
import { initialsOf, fillIfBlank } from '../../lib/rows'
import {
  WHOLE,
  WING_CLIP,
  assign,
  canScan,
  choiceFor,
  formProblem,
  freeSlots,
  isBody,
  kindOfTissue,
  localRun,
  nextAfter,
  normalizeId,
  problemsOf,
  setChoice,
  sharedChoice,
  tissuesOf,
  tubeCells,
  tubesIn,
  tubesOf,
  untouched,
  type ChoiceKey,
  type Field,
  type Problem,
  type SampleKind,
  type TubeChoice,
  type Typed,
} from '../../lib/tubes'
import type { Table, TableRow } from '../../lib/types'
import { verificationsFor } from '../../lib/verifications'
import { usePending } from '../../stores/pending'
import { useSession } from '../../stores/session'
import { type ServerRecord, useTables } from '../../stores/tables'
import { t, tn, tx, type Msg } from '../../lib/i18n'

/**
 * Tubos as cards: the butterflies found by ID, CAM or tube (or a pasted list
 * or range, as in Muertes), one card each in the order added; what goes in
 * the tubes (whole body, wing clip, a body in parts, or not preserved), the
 * day and the medium, for all cards or the selected ones; and on each card its
 * CAM and tube boxes, filled with the next free ones (tubes from the rack in
 * use, in the cards' order) until a tube is typed or scanned, which the next
 * cards then follow. A tube's form (two letters, eight digits), repeats on two
 * cards and IDs used anywhere in the workbook are flagged before Save. One
 * Save writes what Tubos' table writes (lib/tubes.ts) and saves it, with an
 * Undo. What is chosen is shared with the table (useTubesState).
 */
const MODULE = 'Insectary_data'
const props = defineProps<{
  table: Table | undefined
  ready: boolean
  options: Record<string, string[]>
  /** The Abbr_name list ("FCH - Franz Chandi"), for the initials that sign a clip note. */
  collectors: string[]
}>()
const mode = defineModel<EntryMode>('mode', { required: true })

const pending = usePending()
const session = useSession()
const tables = useTables()
const keyboard = useKeyboard()
/** Two columns from 700 px (tablets, phones sideways); the toggle names its modes from 1024 px. */
const wide = useMedia('(min-width: 700px)')
const roomy = useMedia('(min-width: 1024px)')

const state = useTubesState()
const { picked, tissue, none, parts, medium, presDate, clipDate, camStart, tubeStart, rackChosen, closeRest, own, typed, accepted, selected } =
  state
const query = ref('')
const today = computed(() => isoToSerial(todayIso()))
const initials = computed(
  () =>
    state.initials.value.trim().toUpperCase() ||
    initialsOf(session.user?.displayName || '', props.collectors, session.user?.username || ''),
)
const canEdit = computed(() => session.canEdit)

// The days start at today (kept from the last time otherwise; the weekday and "N days ago" show under them).
if (!presDate.value) presDate.value = todayIso()
if (!clipDate.value) clipDate.value = todayIso()

// --- The butterflies, indexed once per version of the sheet
const index = computed(() => (props.table ? buildIndex(props.table.rows) : []))
const byKey = computed(() => new Map(index.value.map(e => [e.key, e])))
const known = computed(() => new Map(index.value.map(e => [e.key, e.id])))
const aliveSaved = computed(() => {
  const out = new Set<string>()
  for (const e of index.value) if (lifeOf(f => e.row.values[f] ?? null).state === 'alive') out.add(e.row.id)
  return out
})
const get = (row: TableRow) => (field: string) => pending.value(row, field)
const isAlive = (entry: Entry) =>
  toRaw(pending.edits)[entry.row.id] ? lifeOf(get(entry.row)).state === 'alive' : aliveSaved.value.has(entry.row.id)
const factsFor = (row: TableRow): Facts => factsOf(get(row), today.value)
const idOf = (row: TableRow) => String(row.values.Insectary_ID)

/** The cards, in the order added: tubes are handed out in this order. */
const cards = computed(() => {
  const seen = new Set<string>()
  const out: TableRow[] = []
  for (const id of picked.value) {
    const row = byKey.value.get(searchKey(id))?.row
    if (row && !seen.has(row.id)) {
      seen.add(row.id)
      out.push(row)
    }
  }
  return out
})

// --- Search (as in Muertes)
const searchInput = ref<HTMLInputElement>()
const focused = ref(false)
const suggestions = computed(() =>
  query.value.trim() ? suggest(index.value, query.value, { alive: isAlive, skip: new Set(picked.value) }) : [],
)
const missing = ref<string[]>([])
const typedMissing = computed(() => {
  const q = searchKey(query.value)
  if (q.length < 2 || suggestions.value.length || !props.ready) return null
  return { typed: query.value.trim(), alike: lookAlikes(q, known.value) }
})
const missingAlike = computed(() => (missing.value.length ? lookAlikes(missing.value[0], known.value) : []))
const alreadyChosen = computed(() => {
  const q = searchKey(query.value)
  return q && picked.value.some(id => searchKey(id) === q) ? query.value.trim().toUpperCase() : ''
})
function add(ids: string[]) {
  const have = new Set(picked.value.map(searchKey))
  const fresh = ids.filter(id => !have.has(searchKey(id)))
  if (fresh.length) picked.value = [...picked.value, ...fresh]
  lastSave.value = null
}
function choose(id: string) {
  add([id])
  query.value = ''
  missing.value = []
}
/** Enter: the exact ID (or CAM/tube), several IDs, or a range in the sheet's pre-made order (B0D-B9D). */
function enter() {
  const text = query.value.trim()
  if (!text) return
  const tokens = idTokens(text)
  if (tokens.length > 1 || tokens[0]?.includes('-')) return addTokens(tokens)
  const exact = byKey.value.get(searchKey(text))
  const top = suggestions.value[0]
  if (exact) choose(exact.id)
  else if (top && top.via === searchKey(text)) choose(top.entry.id)
  else missing.value = [text]
}
function addTokens(tokens: string[]) {
  const { found, missing: none } = resolveIds(
    tokens,
    index.value.map(e => e.id),
  )
  add(found)
  missing.value = none
  query.value = ''
}
function onPaste(event: ClipboardEvent) {
  const tokens = idTokens(event.clipboardData?.getData('text') || '')
  if (tokens.length > 1 || tokens[0]?.includes('-')) {
    event.preventDefault()
    addTokens(tokens)
  }
}
function forget(ids: string[]) {
  const gone = new Set(ids.map(searchKey))
  const keep = (map: Record<string, unknown>) => Object.fromEntries(Object.entries(map).filter(([id]) => !gone.has(searchKey(id))))
  picked.value = picked.value.filter(p => !gone.has(searchKey(p)))
  selected.value = selected.value.filter(p => !gone.has(searchKey(p)))
  own.value = keep(own.value) as typeof own.value
  typed.value = keep(typed.value) as typeof typed.value
}
const remove = (id: string) => forget([id])
function removeAll() {
  forget(picked.value)
  missing.value = []
}

// --- What goes in the tubes: for all cards, or for the selected ones
const all = computed<TubeChoice>(() => {
  const kind: SampleKind = none.value ? 'none' : kindOfTissue(tissue.value)
  return {
    kind,
    parts: kind === 'parts' ? (parts.value.length ? parts.value : [tissue.value]) : parts.value,
    medium: medium.value,
    date: kind === 'clip' ? clipDate.value : presDate.value,
    closeRest: closeRest.value,
  }
})
const choiceOf = (row: TableRow) => choiceFor(all.value, own.value, idOf(row))
const hasOwn = (row: TableRow, field: ChoiceKey) => own.value[idOf(row)]?.[field] !== undefined
const selectedIds = computed(() => {
  const keys = new Set(selected.value.map(searchKey))
  return cards.value.map(idOf).filter(id => keys.has(searchKey(id)))
})
const isSelected = (row: TableRow) => selectedIds.value.includes(idOf(row))
function toggleSelect(row: TableRow) {
  const id = idOf(row)
  selected.value = isSelected(row) ? selected.value.filter(s => searchKey(s) !== searchKey(id)) : [...selected.value, id]
}
const doneSelecting = () => (selected.value = [])
function shown<F extends ChoiceKey>(field: F): TubeChoice[F] | undefined {
  return selectedIds.value.length ? sharedChoice(all.value, own.value, selectedIds.value, field) : all.value[field]
}
/** The panel's value of a field into the shared state (what the table reads too). */
function applyAll<F extends ChoiceKey>(field: F, value: TubeChoice[F]) {
  if (field === 'kind') {
    const kind = value as SampleKind
    none.value = kind === 'none'
    if (kind === 'whole') tissue.value = WHOLE
    else if (kind === 'clip') tissue.value = WING_CLIP
    else if (kind === 'parts') {
      if (!parts.value.length) parts.value = ['']
      if (parts.value[0]) tissue.value = parts.value[0]
    }
  } else if (field === 'parts') {
    parts.value = value as string[]
    if (all.value.kind === 'parts' && parts.value[0]) tissue.value = parts.value[0]
  } else if (field === 'medium') medium.value = value as string
  else if (field === 'closeRest') closeRest.value = value as boolean
  else if (field === 'date') {
    if (all.value.kind === 'clip') clipDate.value = value as string
    else presDate.value = value as string
  }
}
/** Sets a field for the selected cards (or `ids`), or with none selected for all of them. */
function setField<F extends ChoiceKey>(field: F, value: TubeChoice[F], ids = selectedIds.value) {
  const next = setChoice(all.value, own.value, ids, field, value)
  if (JSON.stringify(next.all[field]) !== JSON.stringify(all.value[field])) applyAll(field, next.all[field])
  own.value = next.own
}
function ownOf(field: ChoiceKey) {
  if (selectedIds.value.length) return []
  return cards.value.filter(r => hasOwn(r, field)).map(idOf)
}
const KINDS: { kind: SampleKind; name: () => string }[] = [
  { kind: 'whole', name: () => t('Cuerpo entero') },
  { kind: 'clip', name: () => t('Corte de ala') },
  { kind: 'parts', name: () => t('Por partes') },
  { kind: 'none', name: () => t('No preservada') },
]
function pickKind(kind: SampleKind) {
  setField('kind', kind)
  if (kind === 'parts' && !(shown('parts') ?? []).filter(Boolean).length) setField('parts', [''])
}
const MEDIUMS = ['Flash frozen', 'Ethanol', 'DMSO']
const quickDates = computed(() => [
  { iso: todayIso(), name: t('Hoy') },
  { iso: serialToIso(today.value - 1), name: t('Ayer') },
])
const shownKind = computed(() => shown('kind'))
const shownDate = computed(() => shown('date') ?? '')
const dateError = computed(() =>
  shownDate.value && serialFromIso(shownDate.value) === null ? t('Fecha no válida: el año debe estar entre 1990 y 2099') : '',
)
/** A day that is not today or yesterday is said in amber: dates are kept on this device from one day to the next. */
const oldDate = computed(() => {
  const s = serialFromIso(shownDate.value)
  return s !== null && today.value - s > 1
})
/** The tissues a body in parts can take (the sheet's ORGANISM_PART list), the usual ones first. */
const COMMON_PARTS = ['HEAD | ABDOMEN', 'THORAX', 'THORAX | LEG', 'LEG', 'ABDOMEN']
const partOptions = computed(() => {
  const list = verificationsFor(MODULE)?.lists.Tube_1_tissue?.values
  const values = list?.size ? [...list] : props.options.Tube_1_tissue || []
  return [...new Set([...COMMON_PARTS.filter(p => !values.length || values.includes(p)), ...values])].filter(
    v => v && v !== WHOLE && v !== 'NOT_COLLECTED' && v !== 'NA' && !/WING CLIP/i.test(v),
  )
})
function setPart(i: number, value: string) {
  const list = [...(shown('parts') ?? [''])]
  list[i] = value
  setField('parts', list)
}
function addPart() {
  setField('parts', [...(shown('parts') ?? []), ''])
}
function dropPart(i: number) {
  const list = [...(shown('parts') ?? [])]
  list.splice(i, 1)
  setField('parts', list.length ? list : [''])
}

// --- Where the CAMs and tubes come from: the next free CAM; the rack in use (or a tube typed)
interface Rack extends RackSuggestion {
  label: string
  labelMsg?: Msg
}
const racks = ref<Rack[]>([])
const camFirst = ref('')
const camChoices = ref<{ value: string; label: string }[]>([])
async function loadSuggestions() {
  try {
    const [cam, tube] = await Promise.all([
      api<{ suggestions: { value: string; label: string }[] }>('ids?kind=cam'),
      api<{ suggestions: Rack[] }>('ids?kind=tube'),
    ])
    camFirst.value = cam.suggestions[0]?.value || ''
    camChoices.value = cam.suggestions
    racks.value = tube.suggestions
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
onMounted(loadSuggestions)
/** The app's rack: the crosses' or the insectary's, in the medium chosen (until the person picks one). */
const autoRack = computed(() => bestRack(racks.value, cards.value, medium.value))
watch(autoRack, rack => {
  if (!rackChosen.value && rack) tubeStart.value = rack.value
})
const tubeFrom = computed(() => normalizeId(rackChosen.value ? tubeStart.value : autoRack.value?.value || tubeStart.value))
const rackNow = computed(() => racks.value.find(r => r.value === tubeFrom.value))
const camFrom = computed(() => normalizeId(camStart.value) || camFirst.value)
const editingStarts = ref(false)
const typedStart = ref('')
function pickRack(value: string) {
  tubeStart.value = normalizeId(value)
  rackChosen.value = true
  const rack = racks.value.find(r => r.value === value)
  if (rack?.medium && MEDIUMS.includes(rack.medium) && rack.medium !== medium.value) setField('medium', rack.medium, [])
}
function autoRackAgain() {
  rackChosen.value = false
  if (autoRack.value) tubeStart.value = autoRack.value.value
}
const insectaryRacks = computed(() => racks.value.filter(s => s.context === 'Cruces' || s.context === 'Insectario'))
const otherRacks = computed(() => racks.value.filter(s => !insectaryRacks.value.includes(s)))

// --- The runs of free IDs: the server's (skipping IDs used anywhere), counted up here until they arrive
/** CAMs and tubes the sheet's butterflies have (search key → Insectary ID). */
const usedHere = computed(() => usedSamples(index.value))
/** CAMs and tubes in unsaved changes of rows that are not cards. */
const pendingUsed = computed(() => {
  const mine = new Set(cards.value.map(r => r.id))
  const out = new Map<string, string>()
  for (const e of Object.values(pending.edits)) {
    if (mine.has(e.id)) continue
    for (const [f, v] of Object.entries(e.values))
      if ((f === 'CAM_ID' || /^Tube_\d_id/.test(f)) && !isBlank(v)) out.set(normalizeId(String(v)), t('{label} (sin guardar)', { label: e.label }))
  }
  return out
})
const runs = reactive(new Map<string, string[]>())
const asked = new Set<string>()
function run(start: string, count: number): string[] {
  const have = runs.get(start)
  if (have && have.length >= count) return have
  const want = Math.ceil(count / 20) * 20
  const key = `${start}:${want}`
  if (!asked.has(key)) {
    asked.add(key)
    const kind = start.startsWith('CAM') ? 'cam' : 'tube'
    api<{ sequence: string[] }>(`ids?kind=${kind}&start=${encodeURIComponent(start)}&count=${want}`)
      .then(r => runs.set(start, r.sequence))
      .catch(() => {})
  }
  const used = (id: string) => usedHere.value.has(id) || pendingUsed.value.has(id)
  return localRun(start, count, used)
}

/** Each card's sample: its choice, the tube columns it can use, and whether it still needs a CAM. */
const needs = computed(() =>
  cards.value.map(row => {
    const choice = choiceOf(row)
    const free = freeSlots(get(row)).length
    const want = choice.kind === 'none' ? 0 : Math.min(tubesOf(choice), free)
    return {
      row,
      id: idOf(row),
      choice,
      free,
      cam: choice.kind !== 'none' && free > 0 && isBlank(pending.value(row, 'CAM_ID')),
      tubes: want,
    }
  }),
)
const assigned = computed(() =>
  assign(needs.value, typed.value, {
    camStart: camFrom.value,
    tubeStart: tubeFrom.value,
    run,
    taken: new Set(pendingUsed.value.keys()),
  }),
)

// --- Checks: the form, repeats, and IDs used anywhere (the server knows the other sheets)
const serverUsed = reactive(new Map<string, string | null>())
let checkTimer: ReturnType<typeof setTimeout> | undefined
const toCheck = computed(() => {
  const cams = new Set<string>()
  const tubes = new Set<string>()
  for (const a of Object.values(assigned.value)) {
    if (a.cam?.value && !usedHere.value.has(a.cam.value) && !serverUsed.has(a.cam.value)) cams.add(a.cam.value)
    for (const tube of a.tubes) if (tube.value && !usedHere.value.has(tube.value) && !serverUsed.has(tube.value)) tubes.add(tube.value)
  }
  return { cams: [...cams], tubes: [...tubes] }
})
watch(toCheck, ({ cams, tubes }) => {
  clearTimeout(checkTimer)
  if (!cams.length && !tubes.length) return
  checkTimer = setTimeout(async () => {
    const ask = async (kind: string, values: string[]) => {
      if (!values.length) return
      const r = await api<{ used: Record<string, { sheet: string; row: number; label: string | null }> }>(
        `ids?kind=${kind}&check=${encodeURIComponent(values.join(','))}`,
      )
      for (const v of values) {
        const h = r.used[v]
        serverUsed.set(v, h ? t('{sheet} fila {row}{label}', { sheet: h.sheet, row: h.row, label: h.label ? ` (${h.label})` : '' }) : null)
      }
    }
    try {
      await Promise.all([ask('cam', cams), ask('tube', tubes)])
    } catch {
      /* Save checks again on the server. */
    }
  }, 400)
})
onBeforeUnmount(() => clearTimeout(checkTimer))
/** Where a CAM or tube is used already: a butterfly of the sheet, another unsaved change, or another sheet. */
const used = {
  has: (v: string) => usedHere.value.has(v) || pendingUsed.value.has(v) || !!serverUsed.get(v),
  get: (v: string) =>
    usedHere.value.has(v) ? `Insectary_data · ${usedHere.value.get(v)}` : pendingUsed.value.get(v) ?? serverUsed.get(v) ?? undefined,
}
const near = computed(() => {
  // The racks in use, the runs' starts and every CAM and tube on the cards: a misread one lands next to one of them.
  const out = [tubeFrom.value, camFrom.value, camFirst.value, ...racks.value.map(r => r.value)]
  for (const a of Object.values(assigned.value)) {
    if (a.cam) out.push(a.cam.value)
    for (const tube of a.tubes) out.push(tube.value)
  }
  return out.filter(Boolean)
})
const problems = computed(() =>
  problemsOf(
    needs.value.map(n => ({ id: n.id, choice: n.choice, free: n.free, needsCam: n.cam, assigned: assigned.value[n.id] })),
    { used, accepted: new Set(accepted.value), near: near.value },
  ),
)
const problemsAt = (id: string, field: Field) => (problems.value.get(id) ?? []).filter(p => 'field' in p && p.field === field)
const cardProblems = (id: string) => (problems.value.get(id) ?? []).filter(p => !('field' in p))
function problemText(p: Problem, id: string): string {
  switch (p.kind) {
    case 'slots':
      return p.free
        ? t('{id}: {need} tubos y solo {free} columnas libres', { id, need: p.need, free: p.free })
        : t('{id} ya tiene sus tubos: no queda columna libre', { id })
    case 'date':
      return t('Falta la fecha de {id}', { id })
    case 'badDate':
      return `${id}: ${t('Fecha no válida: el año debe estar entre 1990 y 2099')}`
    case 'parts':
      return t('Elige el tejido de cada tubo de {id}', { id })
    case 'missing':
      return p.field === 'cam' ? t('Falta el CAM de {id}', { id }) : t('Falta el tubo de {id}', { id })
    case 'repeated':
      return t('{value} está en {a} y en {b}', { value: p.value, a: p.with, b: id })
    case 'used':
      return t('{value} ya está usado: {where}', { value: p.value, where: p.where })
    case 'form': {
      const f = p.form
      if (f.problem === 'cam') return t('{value} es un CAM, no un tubo', { value: p.value })
      if (f.problem === 'tube') return t('{value} es un tubo, no un CAM', { value: p.value })
      if (f.problem === 'digits')
        return p.field === 'cam'
          ? t('{value}: un CAM lleva 6 dígitos', { value: p.value })
          : t('{value}: un tubo lleva 2 letras y 8 dígitos', { value: p.value })
      return p.field === 'cam'
        ? t('{value} no parece un CAM (CAM y 6 dígitos)', { value: p.value })
        : t('{value} no parece un tubo (2 letras y 8 dígitos)', { value: p.value })
    }
  }
}

// --- Typing and scanning on a card
function setTyped(id: string, field: Field, value: string | undefined) {
  const mine: Typed = { ...(typed.value[id] ?? {}) }
  if (field === 'cam') mine.cam = value
  else {
    const list = [...(mine.tubes ?? [])]
    while (list.length <= field) list.push(null)
    list[field] = value
    mine.tubes = list
  }
  typed.value = { ...typed.value, [id]: mine }
}
/** A box typed in: digits keep the box's prefix (FS, CAM); letters typed or scanned give the whole ID. */
function onBox(id: string, field: Field, event: Event) {
  typedIn(event)
  const raw = (event.target as HTMLInputElement).value
  const prefix = prefixOf(id, field)
  const value = /[A-Za-z]/.test(raw) ? normalizeId(raw) : raw.replace(/\D/g, '') ? prefix + raw.replace(/\D/g, '') : ''
  setTyped(id, field, value)
}
const valueAt = (id: string, field: Field) =>
  (field === 'cam' ? assigned.value[id]?.cam : assigned.value[id]?.tubes[field]) ?? { value: '', auto: true }
/** The letters of a box: the value's own, else its run's (FS…, CAM). */
function prefixOf(id: string, field: Field) {
  const v = valueAt(id, field).value
  const own = /^([A-Z]+)\d*$/.exec(v)?.[1]
  if (own) return own
  if (field === 'cam') return 'CAM'
  return /^([A-Z]+)\d/.exec(tubeFrom.value)?.[1] || 'FS'
}
const digitsOf = (id: string, field: Field) => {
  const v = valueAt(id, field).value
  return /^[A-Z]+(\d*)$/.exec(v)?.[1] ?? v
}
const PREFIXES = ['FS', 'FA', 'FD', 'FF']
function setPrefix(id: string, field: Field, prefix: string) {
  setTyped(id, field, prefix + digitsOf(id, field))
}
const fixOf = (id: string, field: Field) => {
  for (const p of problemsAt(id, field)) if (p.kind === 'form' && p.form.fix) return p.form.fix
  return ''
}
const formAt = (id: string, field: Field) => problemsAt(id, field).find(p => p.kind === 'form')
/** A tube or CAM of an unusual length may be right (old series): it can be kept as written on its label. */
const canAccept = (id: string, field: Field) => {
  const p = formAt(id, field)
  return p?.kind === 'form' && (p.form.problem === 'digits' || p.form.problem === 'format')
}
function accept(value: string) {
  if (!accepted.value.includes(value)) accepted.value = [...accepted.value, value]
}
/**
 * Enter in a box goes to the next box of its kind (tube to tube, CAM to CAM: a reader types each
 * tube and Enter, card after card); the last one closes the keyboard.
 */
function nextBox(event: KeyboardEvent) {
  const input = event.target as HTMLInputElement
  const cam = input.dataset.box?.endsWith(':cam')
  const boxes = [...(root.value?.querySelectorAll<HTMLInputElement>('input[data-box]') ?? [])].filter(b => b.dataset.box!.endsWith(':cam') === cam)
  const at = boxes.indexOf(input)
  const next = boxes[at + 1]
  if (next) {
    next.focus()
    // Selected, so the next tube typed or read replaces the suggestion.
    next.select()
  } else input.blur()
}
function selectAll(event: FocusEvent) {
  const input = event.target as HTMLInputElement
  input.dataset.fresh = '1'
  input.select()
}
/**
 * A box entered and not typed in yet stays selected when its suggestion changes under it
 * (the run moves on after the tube typed just before, or the server's run arrives).
 */
watch(
  assigned,
  () =>
    nextTick(() => {
      const el = document.activeElement as HTMLInputElement | null
      if (el?.dataset.box && el.dataset.fresh && root.value?.contains(el)) el.select()
    }),
  { flush: 'post' },
)
const typedIn = (event: Event) => delete (event.target as HTMLInputElement).dataset.fresh

// Camera: each tube read goes to the next tube box, from the one the scan started at.
const scanning = ref(false)
const scanAt = ref<{ id: string; k: number } | null>(null)
const scanStatus = ref<{ text: string; kind: 'ok' | 'error' | 'idle' }>({ text: '', kind: 'idle' })
const scanCan = canScan()
const tubeBoxes = computed(() => needs.value.flatMap(n => Array.from({ length: n.tubes }, (_, k) => ({ id: n.id, k }))))
function startScan(at?: { id: string; k: number }) {
  const firstOpen = tubeBoxes.value.find(b => valueAt(b.id, b.k).auto || !valueAt(b.id, b.k).value)
  scanAt.value = at ?? firstOpen ?? tubeBoxes.value[0] ?? null
  scanStatus.value = { text: '', kind: 'idle' }
  scanning.value = true
}
const scanNext = computed(() => (scanAt.value ? t('Siguiente: {id}', { id: scanAt.value.id }) : t('Todas las tarjetas tienen tubo')))
function onScan(raw: string) {
  const value = normalizeId(raw)
  const form = formProblem('tube', value)
  if (form && form.problem !== 'digits') return (scanStatus.value = { text: t('No es un tubo: {value}', { value }), kind: 'error' })
  const target = scanAt.value
  if (!target) return (scanStatus.value = { text: t('{value}: ya no quedan tubos por leer', { value }), kind: 'error' })
  const other = tubeBoxes.value.find(b => !valueAt(b.id, b.k).auto && valueAt(b.id, b.k).value === value)
  if (other && (other.id !== target.id || other.k !== target.k))
    return (scanStatus.value = { text: t('{value} ya está en {id}', { value, id: other.id }), kind: 'error' })
  if (used.has(value)) return (scanStatus.value = { text: t('{value} ya está usado: {where}', { value, where: used.get(value) ?? '' }), kind: 'error' })
  setTyped(target.id, target.k, value)
  scanStatus.value = { text: `${value} → ${target.id}`, kind: 'ok' }
  const at = tubeBoxes.value.findIndex(b => b.id === target.id && b.k === target.k)
  scanAt.value = tubeBoxes.value[at + 1] ?? null
}

// --- What Save writes
const plans = computed(() => {
  const out = new Map<string, DeathCell[]>()
  for (const n of needs.value) {
    const a = assigned.value[n.id]
    out.set(
      n.row.id,
      tubeCells(
        n.row,
        pending.value,
        n.choice,
        { cam: a?.cam?.value ?? '', tubes: (a?.tubes ?? []).map(x => x.value) },
        { today: today.value, initials: initials.value },
      ),
    )
  }
  return out
})
const toSave = computed(() => cards.value.filter(r => plans.value.get(r.id)?.length))
const readyCount = computed(() => needs.value.filter(n => !(problems.value.get(n.id) ?? []).length).length)
/** Why Save cannot run yet; `id` and `field` take you to it. */
const blocker = computed<{ text: string; id?: string; field?: Field }>(() => {
  if (!cards.value.length) return { text: t('Añade al menos una mariposa') }
  for (const n of needs.value) {
    const list = problems.value.get(n.id) ?? []
    if (!list.length) continue
    const total = [...problems.value.values()].filter(l => l.length).length - 1
    const text = problemText(list[0], n.id)
    return { text: total ? `${text} ${tn(total, '(y {n} más)', '(y {n} más)')}` : text, id: n.id, field: 'field' in list[0] ? list[0].field : undefined }
  }
  if (!toSave.value.length) return { text: t('Nada que escribir: esas filas ya tienen sus tubos') }
  return { text: '' }
})
const otherPending = computed(() => {
  const mine = new Set(cards.value.map(r => r.id))
  let n = pending.creates.length
  for (const e of Object.values(pending.edits)) if (!mine.has(e.id)) n += Object.keys(e.values).length
  return n
})
const summary = computed(() => {
  const ids = cards.value.map(idOf)
  const kind = sharedChoice(all.value, own.value, ids, 'kind')
  const day = sharedChoice(all.value, own.value, ids, 'date')
  const tubes = needs.value.reduce((s, n) => s + n.tubes, 0)
  return [
    kind === undefined ? t('varios tipos') : KINDS.find(k => k.kind === kind)?.name(),
    day === undefined ? t('varias fechas') : day ? dayLabel(day).split(' · ')[0] : '',
    tubes ? tn(tubes, '{n} tubo', '{n} tubos') : '',
  ]
    .filter(Boolean)
    .join(' · ')
})

// --- Save, then Undo
interface Label {
  tube: string
  id: string
  cam: string
  tissue: string
}
const saving = ref(false)
const lastSave = ref<null | { actionId: string; ids: string[]; count: number; labels: Label[]; typed: Record<string, Typed>; own: typeof own.value }>(null)
const undoing = ref(false)
const waitIdle = async () => {
  for (let i = 0; i < 300 && pending.saving; i++) await new Promise(r => setTimeout(r, 100))
}
function labelsOf(rows: TableRow[]): Label[] {
  return rows.flatMap(row => {
    const n = needs.value.find(x => x.row.id === row.id)
    const a = assigned.value[idOf(row)]
    if (!n || !a) return []
    const cam = a.cam?.value || String(pending.value(row, 'CAM_ID') ?? '')
    const tissues = tissuesOf(n.choice)
    return a.tubes.filter(x => x.value).map((x, k) => ({ tube: x.value, id: idOf(row), cam, tissue: tissues[k] ?? '' }))
  })
}
async function save() {
  if (blocker.value.text || saving.value) return
  saving.value = true
  const rows = toSave.value
  const ids = rows.map(idOf)
  const labels = labelsOf(rows)
  const snapshot = { typed: JSON.parse(JSON.stringify(typed.value)), own: JSON.parse(JSON.stringify(own.value)) }
  const lastTube = labels.at(-1)?.tube
  // Taken before writing: each cell written changes what the cards still need (a CAM given is no longer
  // missing), so the plans read during the loop would hand the next card the same CAM again.
  const cells = rows.map(row => [row, plans.value.get(row.id) || []] as const)
  try {
    for (const [row, list] of cells) for (const c of list) fillIfBlank(MODULE, row, idOf(row), c.field, c.value, c.overwrite)
    pending.touch()
    await waitIdle()
    const result = await pending.save('')
    const refused = rows.filter(r => Object.keys(pending.issues).some(k => k.startsWith(`${r.id}:`)))
    if (refused.length) {
      notify(t('No se guardó {ids}: {reason}', { ids: refused.map(idOf).join(', '), reason: Object.values(pending.issues)[0] ?? '' }), 'error')
      forget(ids.filter(id => !refused.some(r => idOf(r) === id)))
      return
    }
    lastSave.value = result.actionId ? { actionId: result.actionId, ids, count: rows.length, labels, ...snapshot } : null
    forget(ids)
    // The next batch goes on after the last tube used (a tube typed as the start), or from the app's rack again.
    if (rackChosen.value && lastTube) tubeStart.value = nextAfter(lastTube)
    if (camStart.value) camStart.value = ''
    runs.clear()
    asked.clear()
    loadSuggestions()
    if (!result.actionId) notify(tn(rows.length, '{n} mariposa con tubo guardada', '{n} mariposas con tubo guardadas'), 'success')
    scroller.value?.scrollTo({ top: 0 })
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    saving.value = false
  }
}
async function undo() {
  const last = lastSave.value
  if (!last || undoing.value) return
  undoing.value = true
  try {
    const result = await api<{ records?: ServerRecord[] }>('history/undo', {
      method: 'POST',
      body: { actionIds: [last.actionId], requestId: requestId(), reason: null },
    })
    if (result.records?.length) tables.merge(result.records)
    else await tables.load(MODULE, true)
    // The cards come back with what was typed on them, to correct and save again.
    picked.value = [...new Set([...picked.value, ...last.ids])]
    typed.value = { ...typed.value, ...Object.fromEntries(Object.entries(last.typed).filter(([id]) => last.ids.includes(id))) }
    own.value = { ...own.value, ...Object.fromEntries(Object.entries(last.own).filter(([id]) => last.ids.includes(id))) }
    runs.clear()
    asked.clear()
    loadSuggestions()
    lastSave.value = null
    notify(tn(last.count, '{n} mariposa deshecha en Google Sheets', '{n} mariposas deshechas en Google Sheets'), 'success')
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    undoing.value = false
  }
}
const printLabels = () => window.print()

// --- Card details
/** What a card shows of the row: the tubes it has, its CAM. */
const existing = (row: TableRow) => tubesIn(get(row))
const camNow = (row: TableRow) => {
  const v = pending.value(row, 'CAM_ID')
  return isBlank(v) ? '' : String(v)
}
const slotsOf = (row: TableRow) => freeSlots(get(row))
const shortTissue = (tissue: string) => tissue.replace('**OTHER_SOMATIC_ANIMAL_TISSUE** | ', '')
/** The card's choice as chips; `own` ones (set for this card only) in violet. */
function chipsOf(row: TableRow) {
  const c = choiceOf(row)
  const date = c.date && serialFromIso(c.date) !== null ? formatSerial(isoToSerial(c.date)) : c.date || t('sin fecha')
  const chips: { field: ChoiceKey; text: string }[] = [{ field: 'kind', text: KINDS.find(k => k.kind === c.kind)?.name() ?? '' }]
  if (c.kind !== 'none') chips.push({ field: 'date', text: c.kind === 'clip' ? t('corte {date}', { date }) : date }, { field: 'medium', text: c.medium })
  return chips.map(chip => ({ ...chip, own: hasOwn(row, chip.field) }))
}
/** The note a clip adds, as it will be written. */
const clipNote = (row: TableRow) => {
  const c = choiceOf(row)
  const s = serialFromIso(c.date)
  return c.kind === 'clip' && s !== null ? `${noteDay(today.value)} ${initials.value}: Wing clip ${noteDay(s)}` : ''
}
/** A body: the tube columns after its tubes, closed as NA / NOT_COLLECTED. */
function restText(row: TableRow) {
  const c = choiceOf(row)
  if (!isBody(c.kind) || !c.closeRest) return ''
  const slots = slotsOf(row)
  const after = slots[Math.min(tubesOf(c), slots.length) - 1]
  if (after === undefined || after >= 4) return ''
  return after + 1 === 4 ? t('Tube_4: NA · NOT_COLLECTED') : t('Tube_{from}–4: NA · NOT_COLLECTED', { from: after + 1 })
}
const line = (f: Facts) =>
  [
    f.clutch && t('clutch {c}', { c: f.clutch }),
    f.entered !== null &&
      (f.wild ? t('Capturada {date}', { date: formatSerial(f.entered) }) : t('Emergió {date}', { date: formatSerial(f.entered) })),
  ]
    .filter(Boolean)
    .join(' · ')
const choice = (on: boolean) =>
  on ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white text-stone-800 active:bg-stone-100'
const showHistory = ref(false)
const drawerRow = ref<TableRow | null>(null)

/** Goes to what blocks Save: the box, or the card. */
function goTo(id?: string, field?: Field) {
  if (!id) return
  const box = root.value?.querySelector<HTMLInputElement>(`[data-box="${CSS.escape(`${id}:${field ?? ''}`)}"]`)
  if (box) {
    box.scrollIntoView({ block: 'center' })
    box.focus()
    return
  }
  root.value?.querySelector(`[data-card="${CSS.escape(id)}"]`)?.scrollIntoView({ block: 'center', behavior: 'smooth' })
}
/**
 * On a phone held upright the options fold into one line above the cards (they are
 * usually the same as last time); a tap opens them. With cards selected they open.
 */
const panelOpen = persistentRef('tubes:panel-open', false)
const panel = ref<HTMLElement>()
function toPanel() {
  panelOpen.value = true
  nextTick(() => panel.value?.scrollIntoView({ block: 'start', behavior: 'smooth' }))
}
watch(
  () => selectedIds.value.length,
  (n, old) => {
    if (n && !old && !wide.value) panelOpen.value = true
  },
)
const panelSummary = computed(() => {
  const kind = shownKind.value
  const parts = [kind === undefined ? t('varios tipos') : KINDS.find(k => k.kind === kind)?.name()]
  if (kind !== 'none') {
    parts.push(shownDate.value ? dayLabel(shownDate.value).split(' · ')[0] : t('sin fecha'), shown('medium') ?? '')
    if (tubeFrom.value) parts.push(t('tubos desde {tube}', { tube: tubeFrom.value }))
  }
  return parts.filter(Boolean).join(' · ')
})

// --- The keyboard: the screen fits the part one can see; the box being typed in stays in view
const root = ref<HTMLElement>()
const scroller = ref<HTMLElement>()
const fitted = computed(() => keyboard.open.value)
function onFocusIn(event: FocusEvent) {
  const el = event.target as HTMLElement
  if (!el.matches('input[data-box], textarea')) return
  setTimeout(() => el.scrollIntoView({ block: 'center' }), 350)
}
watch(keyboard.visibleBottom, () => {
  const el = document.activeElement as HTMLElement | null
  if (el?.matches('input[data-box]') && root.value?.contains(el)) requestAnimationFrame(() => el.scrollIntoView({ block: 'center' }))
})
watch(
  () => cards.value.length,
  (n, old) => {
    // A card added from the search: in view (the keyboard stays for the next ID).
    if (n > old && !focused.value) nextTick(() => root.value?.querySelector('[data-card]:last-of-type')?.scrollIntoView({ block: 'nearest' }))
  },
)
</script>

<template>
  <div
    ref="root"
    class="flex h-full bg-stone-50"
    :class="[wide ? 'flex-row' : 'flex-col', fitted ? 'fixed inset-x-0 z-30' : '']"
    :style="fitted ? { top: `${keyboard.visibleTop.value}px`, height: `${keyboard.visibleBottom.value - keyboard.visibleTop.value}px` } : undefined"
    @focusin="onFocusIn"
  >
    <div ref="scroller" data-scroll class="min-h-0 min-w-0 flex-1 overflow-y-auto">
      <!-- The search stays at the top while the cards scroll. -->
      <div class="sticky top-0 z-20 border-b border-stone-200 bg-white px-3 pt-3 pb-2 short:pt-1.5 short:pb-1.5">
        <div class="flex items-start gap-2">
          <div class="relative min-w-0 flex-1">
            <Search :size="20" class="pointer-events-none absolute top-1/2 left-3 -translate-y-1/2 text-stone-400" />
            <input
              ref="searchInput"
              v-model="query"
              class="h-13 w-full rounded-xl border border-stone-300 bg-white pr-12 pl-10 text-lg font-medium uppercase placeholder:text-base placeholder:font-normal placeholder:normal-case focus:border-brand-600 focus:ring-2 focus:ring-brand-100 focus:outline-none short:h-11"
              :placeholder="$t('Insectary ID, CAM o tubo')"
              :aria-label="$t('Buscar una mariposa por Insectary ID, CAM o tubo')"
              type="text"
              autocapitalize="characters"
              autocomplete="off"
              autocorrect="off"
              spellcheck="false"
              enterkeyhint="go"
              @focus="focused = true"
              @blur="focused = false"
              @input="missing = []"
              @keydown.enter.prevent="enter"
              @paste="onPaste"
            />
            <button
              v-if="query"
              class="absolute top-1/2 right-1 grid h-11 w-11 -translate-y-1/2 place-items-center text-stone-500"
              :aria-label="$t('Borrar búsqueda')"
              @mousedown.prevent
              @click="query = ''"
            >
              <X :size="20" />
            </button>
            <ul
              v-if="focused && suggestions.length"
              class="absolute inset-x-0 top-full z-30 mt-1 max-h-[60vh] divide-y divide-stone-100 overflow-y-auto rounded-xl border border-stone-200 bg-white shadow-lg"
              role="listbox"
            >
              <li v-for="s in suggestions" :key="s.entry.id">
                <button
                  class="flex min-h-14 w-full items-center gap-3 px-3 py-2 text-left active:bg-brand-50 short:min-h-12"
                  role="option"
                  @mousedown.prevent
                  @click="choose(s.entry.id)"
                >
                  <span class="w-16 shrink-0 text-lg font-semibold">{{ s.entry.id }}</span>
                  <span class="min-w-0 flex-1">
                    <span class="block truncate text-sm">{{ factsFor(s.entry.row).species || '—' }}</span>
                    <span class="flex items-center gap-1.5 truncate text-xs text-stone-500"
                      ><SexBadge :sex="factsFor(s.entry.row).sex" />{{ line(factsFor(s.entry.row)) }}</span
                    >
                    <span v-if="s.via" class="block truncate text-xs text-brand-700">{{ s.via }}</span>
                  </span>
                  <LifeBadge :facts="factsFor(s.entry.row)" />
                </button>
              </li>
            </ul>
          </div>
          <EntryModeToggle v-model="mode" :compact="!roomy" class="h-13 shrink-0 short:h-11 *:min-w-11" />
          <button
            class="flex h-13 min-w-11 shrink-0 items-center justify-center gap-1 rounded-md border border-stone-300 bg-white px-2 text-sm text-stone-700 active:bg-stone-100 short:h-11"
            :aria-label="$t('Historial de Tubos')"
            :title="$t('Historial de Tubos')"
            @click="showHistory = true"
          >
            <History :size="18" /> <span v-if="roomy">{{ $t('Historial') }}</span>
          </button>
        </div>
        <p v-if="!ready" class="mt-1.5 text-sm text-stone-500">{{ $t('Cargando {sheet}…', { sheet: MODULE }) }}</p>
        <p v-else-if="alreadyChosen" class="mt-1.5 text-sm text-stone-600">{{ $t('{id} ya está en las tarjetas', { id: alreadyChosen }) }}</p>
        <div v-else-if="typedMissing || missing.length" class="mt-1.5 text-sm">
          <p class="text-red-700">
            {{ $t('No encontrado: {ids}', { ids: typedMissing ? typedMissing.typed.toUpperCase() : missing.join(', ') }) }}
          </p>
          <div v-if="(typedMissing ? typedMissing.alike : missingAlike).length" class="mt-1 flex flex-wrap items-center gap-2">
            <span class="text-stone-600">{{ $t('¿Quisiste decir?') }}</span>
            <button
              v-for="id in typedMissing ? typedMissing.alike : missingAlike"
              :key="id"
              class="h-11 rounded-lg border border-brand-600 bg-white px-4 font-semibold text-brand-800"
              @mousedown.prevent
              @click="choose(id)"
            >
              {{ id }}
            </button>
          </div>
        </div>
        <p v-else-if="!picked.length && !query" class="mt-1.5 text-xs text-stone-500 short:hidden">
          {{ $t('Escribe o pega los IDs de las mariposas que reciben tubo (uno, varios o un rango como B0D-B9D).') }}
        </p>
      </div>

      <!-- What goes in the tubes: above the cards on a phone held upright, in the right column on a wide screen. -->
      <Teleport to="#tubes-panel" defer :disabled="!wide">
        <section v-if="canEdit && cards.length && !wide && !panelOpen" class="px-3 pt-3">
          <button
            class="flex min-h-12 w-full items-center gap-2 rounded-xl border border-stone-300 bg-white px-3 py-2 text-left text-sm active:bg-stone-100"
            :aria-expanded="false"
            @click="panelOpen = true"
          >
            <span class="min-w-0 flex-1">
              <span class="block font-semibold text-stone-800">{{ panelSummary }}</span>
              <span v-if="oldDate || dateError" class="block text-xs font-medium text-amber-800">{{ dateError || dayLabel(shownDate) }}</span>
            </span>
            <span class="shrink-0 font-medium text-brand-800">{{ $t('Cambiar') }}</span>
            <ChevronDown :size="18" class="shrink-0 text-stone-500" />
          </button>
        </section>
        <section v-else-if="canEdit && cards.length" ref="panel" class="space-y-4 px-3 pb-2 pt-3">
          <div
            class="sticky top-0 z-10 -mx-3 flex min-h-12 items-center gap-2 border-y px-3 py-1.5"
            :class="selectedIds.length ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-200 bg-stone-100 text-stone-800'"
            role="status"
          >
            <p class="min-w-0 flex-1 text-sm">
              <span class="font-semibold">{{
                selectedIds.length
                  ? $t('Se aplica a {ids}', { ids: selectedIds.join(', ') })
                  : $tn(cards.length, 'Se aplica a la única tarjeta', 'Se aplica a las {n} tarjetas')
              }}</span>
              <span v-if="selectedIds.length" class="block text-xs opacity-90">{{ $t('Solo a las seleccionadas; las demás siguen igual.') }}</span>
            </p>
            <button v-if="selectedIds.length" class="h-10 shrink-0 rounded-lg bg-white px-4 text-sm font-semibold text-brand-800 active:bg-brand-50" @click="doneSelecting">
              {{ $t('Listo') }}
            </button>
            <button
              v-else-if="!wide"
              class="grid h-11 w-11 shrink-0 place-items-center rounded-lg text-stone-600 active:bg-stone-200"
              :aria-label="$t('Plegar las opciones')"
              :aria-expanded="true"
              @click="panelOpen = false"
            >
              <ChevronUp :size="20" />
            </button>
          </div>
          <div>
            <h2 class="mb-1.5 text-sm font-semibold text-stone-700">{{ $t('Qué va en el tubo') }}</h2>
            <div class="grid grid-cols-2 gap-2">
              <button
                v-for="k in KINDS"
                :key="k.kind"
                class="min-h-12 rounded-lg border px-2 py-1.5 text-base font-medium"
                :class="choice(shownKind === k.kind)"
                :aria-pressed="shownKind === k.kind"
                @click="pickKind(k.kind)"
              >
                {{ k.name() }}
              </button>
            </div>
            <p v-if="shownKind === undefined" class="mt-1 text-sm text-stone-600">{{ $t('Distintos: elige uno para todas las seleccionadas') }}</p>
            <p v-else-if="shownKind === 'whole'" class="mt-1 text-xs text-stone-600">
              {{ $t('{tissue}: el cuerpo con las patas en un tubo; se escriben también Preservation_date, Death_date y Killed_Preserved si están vacíos.', { tissue: WHOLE }) }}
            </p>
            <p v-else-if="shownKind === 'clip'" class="mt-1 text-xs text-stone-600">{{ $t('La mariposa sigue viva; el día del corte va en la nota.') }}</p>
            <p v-else-if="shownKind === 'none'" class="mt-1 text-xs text-stone-600">
              {{ $t('Sin preservar: CAM y tubos NA, tejidos y medios NOT_COLLECTED') }}
            </p>
            <p v-if="ownOf('kind').length" class="mt-1 text-xs text-violet-800">{{ $t('Con tipo propio: {ids}', { ids: ownOf('kind').join(', ') }) }}</p>
            <!-- A body in parts: one tube per part, in order. -->
            <div v-if="shownKind === 'parts'" class="mt-2 space-y-2">
              <div v-for="(part, i) in shown('parts') ?? ['']" :key="i" class="flex items-center gap-2">
                <span class="w-14 shrink-0 text-sm text-stone-600">{{ $t('Tubo {n}', { n: i + 1 }) }}</span>
                <select
                  :value="part"
                  class="field-input h-12 min-w-0 flex-1 text-base"
                  :class="{ 'border-amber-500 ring-2 ring-amber-200': !part }"
                  @change="setPart(i, ($event.target as HTMLSelectElement).value)"
                >
                  <option value="" disabled>{{ $t('Elige el tejido') }}</option>
                  <option v-for="o in partOptions" :key="o" :value="o">{{ o }}</option>
                </select>
                <button class="grid h-12 w-12 shrink-0 place-items-center rounded-lg text-stone-500 active:bg-stone-100" :aria-label="$t('Quitar')" @click="dropPart(i)">
                  <X :size="18" />
                </button>
              </div>
              <button class="btn h-11" @click="addPart"><Plus :size="16" /> {{ $t('Otro tubo') }}</button>
            </div>
          </div>
          <template v-if="shownKind !== 'none'">
            <div>
              <h2 class="mb-1.5 text-sm font-semibold text-stone-700">
                {{ shownKind === 'clip' ? $t('Fecha del corte de ala') : 'Preservation_date' }}
              </h2>
              <div class="grid grid-cols-[1fr_1fr_minmax(9rem,1.4fr)] gap-2">
                <button
                  v-for="d in quickDates"
                  :key="d.iso"
                  class="h-12 rounded-lg border text-base font-medium"
                  :class="choice(shownDate === d.iso)"
                  :aria-pressed="shownDate === d.iso"
                  @click="setField('date', d.iso)"
                >
                  {{ d.name }}
                </button>
                <DateField :model-value="shownDate" class="field-input h-12 text-base" @update:model-value="setField('date', $event)" />
              </div>
              <p v-if="dateError" class="mt-1 text-sm text-red-700">{{ dateError }}</p>
              <p v-else-if="shownDate" class="mt-1 text-sm" :class="oldDate ? 'font-medium text-amber-800' : 'text-stone-600'">
                {{ dayLabel(shownDate) }}
              </p>
              <p v-else-if="shown('date') === undefined" class="mt-1 text-sm text-stone-600">{{ $t('Fechas distintas: elige una para todas las seleccionadas') }}</p>
              <p v-if="ownOf('date').length" class="mt-1 text-xs text-violet-800">{{ $t('Con fecha propia: {ids}', { ids: ownOf('date').join(', ') }) }}</p>
            </div>
            <div>
              <h2 class="mb-1.5 text-sm font-semibold text-stone-700">{{ $t('Medio') }}</h2>
              <div class="grid grid-cols-3 gap-2">
                <button
                  v-for="m in MEDIUMS"
                  :key="m"
                  class="min-h-12 rounded-lg border px-1 text-sm font-medium"
                  :class="choice(shown('medium') === m)"
                  :aria-pressed="shown('medium') === m"
                  @click="setField('medium', m)"
                >
                  {{ m }}
                </button>
              </div>
              <p v-if="ownOf('medium').length" class="mt-1 text-xs text-violet-800">{{ $t('Con medio propio: {ids}', { ids: ownOf('medium').join(', ') }) }}</p>
            </div>
            <label v-if="shownKind === 'whole' || shownKind === 'parts'" class="flex min-h-12 items-center gap-3 rounded-lg border border-stone-200 bg-white px-3 py-2 text-sm">
              <input
                type="checkbox"
                class="h-5 w-5 shrink-0"
                :checked="shown('closeRest') !== false"
                @change="setField('closeRest', ($event.target as HTMLInputElement).checked)"
              />
              <span>{{ $t('Tubos sin usar: ID NA, tejido y medio NOT_COLLECTED') }}</span>
            </label>
            <label v-if="shownKind === 'clip'" class="block">
              <span class="field-label">{{ $t('Iniciales (nota)') }}</span>
              <input
                v-model="state.initials.value"
                class="field-input h-11 w-24 text-base"
                :placeholder="initials"
                autocapitalize="characters"
                maxlength="5"
              />
            </label>
            <!-- Where the CAMs and tubes come from. -->
            <div class="rounded-xl border border-stone-200 bg-white p-3 text-sm">
              <div class="flex items-start gap-2">
                <p class="min-w-0 flex-1">
                  <span class="block">
                    {{ $t('Tubos desde') }} <strong class="font-mono">{{ tubeFrom || '—' }}</strong>
                    <span v-if="rackNow" class="text-stone-500"> · {{ tx(rackNow.label, rackNow.labelMsg) }}</span>
                    <span v-else-if="rackChosen" class="text-stone-500"> · {{ $t('escrito') }}</span>
                  </span>
                  <span class="block">
                    {{ $t('CAM desde') }} <strong class="font-mono">{{ camFrom || '—' }}</strong>
                    <span class="text-stone-500"> · {{ $t('el siguiente libre') }}</span>
                  </span>
                  <span class="block text-xs text-stone-500">{{ $t('Un tubo escaneado o escrito en una tarjeta hace que las siguientes sigan desde él.') }}</span>
                </p>
                <button class="btn h-11 shrink-0" @click="editingStarts = !editingStarts">{{ editingStarts ? $t('Cerrar') : $t('Cambiar') }}</button>
              </div>
              <div v-if="editingStarts" class="mt-3 space-y-3">
                <div>
                  <span class="field-label">{{ $t('Gradilla en uso (siguiente tubo libre)') }}</span>
                  <div class="space-y-1.5">
                    <p class="text-xs font-semibold text-stone-500">{{ $t('Insectario y cruces') }}</p>
                    <button
                      v-for="r in insectaryRacks"
                      :key="r.value"
                      class="block min-h-11 w-full rounded-lg border px-3 py-1.5 text-left text-sm"
                      :class="choice(tubeFrom === r.value)"
                      @click="pickRack(r.value)"
                    >
                      {{ tx(r.label, r.labelMsg) }}
                    </button>
                    <details>
                      <summary class="min-h-11 py-2 text-xs font-semibold text-stone-500">{{ $t('Colectas y monitoreo') }}</summary>
                      <button
                        v-for="r in otherRacks"
                        :key="r.value"
                        class="mt-1.5 block min-h-11 w-full rounded-lg border px-3 py-1.5 text-left text-sm"
                        :class="choice(tubeFrom === r.value)"
                        @click="pickRack(r.value)"
                      >
                        {{ tx(r.label, r.labelMsg) }}
                      </button>
                    </details>
                  </div>
                  <div class="mt-2 flex items-end gap-2">
                    <label class="min-w-0 flex-1">
                      <span class="field-label">{{ $t('o escribe el primer tubo') }}</span>
                      <input
                        v-model="typedStart"
                        class="field-input h-11 font-mono text-base uppercase"
                        autocapitalize="characters"
                        autocomplete="off"
                        spellcheck="false"
                        placeholder="FS90415474"
                        @keydown.enter.prevent="typedStart && pickRack(typedStart)"
                      />
                    </label>
                    <button class="btn h-11" :disabled="!typedStart" @click="pickRack(typedStart)">{{ $t('Usar') }}</button>
                  </div>
                  <button v-if="rackChosen" class="mt-2 text-sm underline" @click="autoRackAgain">{{ $t('Que la app elija la gradilla') }}</button>
                </div>
                <label class="block">
                  <span class="field-label">{{ $t('Primer CAM') }}</span>
                  <input
                    :value="camStart"
                    class="field-input h-11 font-mono text-base uppercase"
                    autocapitalize="characters"
                    autocomplete="off"
                    spellcheck="false"
                    :placeholder="camFirst"
                    @change="camStart = normalizeId(($event.target as HTMLInputElement).value)"
                  />
                  <span class="text-xs text-stone-500">{{ $t('Vacío: el siguiente libre ({cam}).', { cam: camFirst }) }}</span>
                </label>
              </div>
            </div>
          </template>
          <p v-if="otherPending" class="text-xs text-amber-900">
            {{ $tn(otherPending, 'Se guardará también {n} cambio pendiente de otras filas.', 'Se guardarán también {n} cambios pendientes de otras filas.') }}
          </p>
        </section>
        <p v-else-if="wide && canEdit" class="px-4 py-6 text-sm text-stone-500">
          {{ $t('Busca y añade mariposas: aquí eliges qué va en los tubos, la fecha y el medio, y las guardas.') }}
        </p>
      </Teleport>

      <!-- The butterflies, in the order the tubes go. -->
      <section v-if="cards.length" class="px-3 pt-3 pb-6">
        <div class="flex items-center justify-between gap-2">
          <h2 class="text-sm font-semibold text-stone-700">
            {{ $t('Tarjetas ({n})', { n: cards.length }) }}
            <span class="font-normal" :class="readyCount === cards.length ? 'text-brand-700' : 'text-amber-800'">
              · {{ $t('{ok} de {n} listas', { ok: readyCount, n: cards.length }) }}</span
            >
          </h2>
          <div class="flex items-center">
            <button v-if="scanCan && canEdit && tubeBoxes.length" class="btn h-11" @click="startScan()">
              <ScanLine :size="18" /> {{ $t('Escanear') }}
            </button>
            <button class="h-11 px-2 text-sm text-stone-600 underline" @click="removeAll">{{ $t('Quitar todas') }}</button>
          </div>
        </div>
        <p v-if="canEdit && cards.length > 1 && !selectedIds.length" class="mb-1.5 text-xs text-stone-500 short:hidden">
          {{ $t('Toca el nombre de una tarjeta para darle su propio tipo, fecha o medio.') }}
        </p>
        <ul class="grid grid-cols-[repeat(auto-fill,minmax(18rem,1fr))] gap-2">
          <li
            v-for="(n, i) in needs"
            :key="n.row.id"
            :data-card="n.id"
            class="relative flex flex-col rounded-xl border-2 shadow-sm"
            :class="isSelected(n.row) ? 'border-brand-600 bg-brand-50 ring-4 ring-brand-600/30' : 'border-stone-200 bg-white'"
          >
            <button
              class="block w-full rounded-t-xl px-3 pt-2.5 pr-24 pb-1.5 text-left"
              :aria-pressed="canEdit ? isSelected(n.row) : undefined"
              :aria-label="canEdit ? (isSelected(n.row) ? $t('{id} seleccionada: toca para quitarla de la selección', { id: n.id }) : $t('Seleccionar {id}', { id: n.id })) : undefined"
              @click="canEdit ? toggleSelect(n.row) : (drawerRow = n.row)"
            >
              <span class="flex flex-wrap items-center gap-2">
                <component
                  :is="isSelected(n.row) ? CheckCircle2 : Circle"
                  v-if="canEdit"
                  :size="22"
                  class="shrink-0"
                  :class="isSelected(n.row) ? 'text-brand-700' : 'text-stone-300'"
                />
                <span class="text-xs font-semibold text-stone-400">{{ i + 1 }}</span>
                <span class="text-xl font-semibold">{{ n.id }}</span>
                <SexBadge :sex="factsFor(n.row).sex" />
                <LifeBadge :facts="factsFor(n.row)" />
              </span>
              <span class="mt-0.5 block truncate text-sm">{{ factsFor(n.row).species || '—' }}</span>
              <span class="block truncate text-xs text-stone-600">{{ line(factsFor(n.row)) }}</span>
            </button>
            <div class="absolute top-1 right-1 flex">
              <button
                class="grid h-11 w-11 place-items-center rounded-lg text-stone-600 active:bg-stone-100"
                :aria-label="$t('Ver la fila de {id}', { id: n.id })"
                :title="$t('Ver la fila de {id}', { id: n.id })"
                @click="drawerRow = n.row"
              >
                <ChevronRight :size="22" />
              </button>
              <button class="grid h-11 w-11 place-items-center rounded-lg text-stone-500 active:bg-stone-100" :aria-label="$t('Quitar {id}', { id: n.id })" @click="remove(n.id)">
                <X :size="20" />
              </button>
            </div>
            <!-- What the row has already. -->
            <p v-if="camNow(n.row) || existing(n.row).length" class="flex flex-wrap gap-1 px-3 pb-1.5 text-xs">
              <span v-if="camNow(n.row)" class="rounded-md bg-stone-100 px-1.5 py-0.5 font-mono text-stone-700">{{ camNow(n.row) }}</span>
              <span v-for="e in existing(n.row)" :key="e.slot" class="rounded-md bg-stone-100 px-1.5 py-0.5 text-stone-700">
                Tube_{{ e.slot }} <span class="font-mono">{{ e.tube }}</span><template v-if="e.tissue"> · {{ shortTissue(e.tissue) }}</template>
              </span>
            </p>
            <div class="space-y-2 border-t px-3 py-2" :class="isSelected(n.row) ? 'border-brand-100' : 'border-stone-100'">
              <p v-for="p in cardProblems(n.id)" :key="p.kind" class="flex items-start gap-1.5 text-sm font-medium text-amber-900">
                <AlertTriangle :size="16" class="mt-0.5 shrink-0" />{{ problemText(p, n.id) }}
              </p>
              <p v-if="n.choice.kind === 'none'" class="text-sm text-stone-600">
                {{
                  untouched(get(n.row))
                    ? $t('Sin preservar: CAM y tubos NA, tejidos y medios NOT_COLLECTED')
                    : $t('Ya tiene CAM o tubo: no se cambia')
                }}
              </p>
              <template v-else-if="n.free">
                <!-- The CAM: kept when the row has one (one CAM per butterfly). -->
                <div v-if="n.cam">
                  <div class="flex items-center justify-between gap-2">
                    <span class="field-label">CAM_ID</span>
                    <span v-if="valueAt(n.id, 'cam').auto && valueAt(n.id, 'cam').value" class="text-xs text-stone-500">{{ $t('siguiente libre') }}</span>
                    <button v-else-if="!valueAt(n.id, 'cam').auto" class="flex h-8 items-center gap-1 text-xs text-stone-600 underline" @click="setTyped(n.id, 'cam', undefined)">
                      <RotateCcw :size="13" /> {{ $t('siguiente libre') }}
                    </button>
                  </div>
                  <div
                    class="flex h-12 items-stretch overflow-hidden rounded-lg border bg-white focus-within:ring-2 focus-within:ring-brand-100"
                    :class="problemsAt(n.id, 'cam').length ? (problemsAt(n.id, 'cam').some(p => p.kind === 'missing') ? 'border-amber-500' : 'border-red-500') : 'border-stone-300 focus-within:border-brand-600'"
                  >
                    <span class="grid place-items-center bg-stone-100 px-2.5 font-mono text-base text-stone-600">{{ prefixOf(n.id, 'cam') }}</span>
                    <input
                      :value="digitsOf(n.id, 'cam')"
                      :data-box="`${n.id}:cam`"
                      class="min-w-0 flex-1 px-2 font-mono text-lg tracking-wide outline-none"
                      :class="valueAt(n.id, 'cam').auto ? 'text-stone-500' : 'font-semibold text-stone-900'"
                      inputmode="numeric"
                      autocomplete="off"
                      spellcheck="false"
                      enterkeyhint="next"
                      :placeholder="$t('Falta')"
                      :aria-label="$t('CAM de {id}', { id: n.id })"
                      @focus="selectAll"
                      @input="onBox(n.id, 'cam', $event)"
                      @keydown.enter.prevent="nextBox"
                    />
                  </div>
                  <p v-for="p in problemsAt(n.id, 'cam').filter(p => p.kind !== 'missing')" :key="p.kind" class="mt-1 text-xs text-red-700">
                    {{ problemText(p, n.id) }}
                  </p>
                  <div v-if="formAt(n.id, 'cam')" class="mt-1 flex flex-wrap gap-2">
                    <button v-if="fixOf(n.id, 'cam')" class="btn h-10" @click="setTyped(n.id, 'cam', fixOf(n.id, 'cam'))">
                      {{ $t('Usar {value}', { value: fixOf(n.id, 'cam') }) }}
                    </button>
                    <button v-if="canAccept(n.id, 'cam')" class="h-10 px-2 text-xs text-stone-600 underline" @click="accept(valueAt(n.id, 'cam').value)">{{ $t('Así está escrito') }}</button>
                  </div>
                </div>
                <p v-else class="text-sm text-stone-600">
                  CAM_ID <span class="font-mono font-medium text-stone-800">{{ camNow(n.row) }}</span> · {{ $t('se conserva') }}
                </p>
                <!-- The tubes: typed, scanned (camera or a reader), or the next free ones. -->
                <div v-for="k in n.tubes" :key="k">
                  <div class="flex items-center justify-between gap-2">
                    <span class="field-label">
                      Tube_{{ slotsOf(n.row)[k - 1] }}_id
                      <span class="font-normal text-stone-500">· {{ shortTissue(tissuesOf(n.choice)[k - 1] || $t('tejido sin elegir')) }}</span>
                    </span>
                    <span v-if="valueAt(n.id, k - 1).auto && valueAt(n.id, k - 1).value" class="text-xs text-stone-500">{{ $t('siguiente libre') }}</span>
                    <button
                      v-else-if="!valueAt(n.id, k - 1).auto"
                      class="flex h-8 items-center gap-1 text-xs text-stone-600 underline"
                      @click="setTyped(n.id, k - 1, undefined)"
                    >
                      <RotateCcw :size="13" /> {{ $t('siguiente libre') }}
                    </button>
                  </div>
                  <div class="flex gap-2">
                    <div
                      class="flex h-12 min-w-0 flex-1 items-stretch overflow-hidden rounded-lg border bg-white focus-within:ring-2 focus-within:ring-brand-100"
                      :class="
                        problemsAt(n.id, k - 1).length
                          ? problemsAt(n.id, k - 1).some(p => p.kind === 'missing')
                            ? 'border-amber-500'
                            : 'border-red-500'
                          : 'border-stone-300 focus-within:border-brand-600'
                      "
                    >
                      <select
                        :value="prefixOf(n.id, k - 1)"
                        class="bg-stone-100 px-1.5 font-mono text-base text-stone-600"
                        :aria-label="$t('Letras del tubo')"
                        @change="setPrefix(n.id, k - 1, ($event.target as HTMLSelectElement).value)"
                      >
                        <option v-for="p in [...new Set([prefixOf(n.id, k - 1), ...PREFIXES])]" :key="p" :value="p">{{ p }}</option>
                      </select>
                      <input
                        :value="digitsOf(n.id, k - 1)"
                        :data-box="`${n.id}:${k - 1}`"
                        class="min-w-0 flex-1 px-2 font-mono text-lg tracking-wide outline-none"
                        :class="valueAt(n.id, k - 1).auto ? 'text-stone-500' : 'font-semibold text-stone-900'"
                        inputmode="numeric"
                        autocomplete="off"
                        spellcheck="false"
                        enterkeyhint="next"
                        :placeholder="$t('Falta')"
                        :aria-label="$t('Tubo de {id}', { id: n.id })"
                        @focus="selectAll"
                        @input="onBox(n.id, k - 1, $event)"
                        @keydown.enter.prevent="nextBox"
                      />
                    </div>
                    <button
                      v-if="scanCan && canEdit"
                      class="grid h-12 w-12 shrink-0 place-items-center rounded-lg border border-stone-300 bg-white text-stone-700 active:bg-stone-100"
                      :aria-label="$t('Escanear el tubo de {id}', { id: n.id })"
                      @click="startScan({ id: n.id, k: k - 1 })"
                    >
                      <ScanLine :size="20" />
                    </button>
                  </div>
                  <p v-for="p in problemsAt(n.id, k - 1).filter(p => p.kind !== 'missing')" :key="p.kind" class="mt-1 text-xs text-red-700">
                    {{ problemText(p, n.id) }}
                  </p>
                  <div v-if="formAt(n.id, k - 1)" class="mt-1 flex flex-wrap gap-2">
                    <button v-if="fixOf(n.id, k - 1)" class="btn h-10" @click="setTyped(n.id, k - 1, fixOf(n.id, k - 1))">
                      {{ $t('Usar {value}', { value: fixOf(n.id, k - 1) }) }}
                    </button>
                    <button v-if="canAccept(n.id, k - 1)" class="h-10 px-2 text-xs text-stone-600 underline" @click="accept(valueAt(n.id, k - 1).value)">
                      {{ $t('Así está en la etiqueta') }}
                    </button>
                  </div>
                </div>
                <p v-if="restText(n.row)" class="text-xs text-stone-500">{{ restText(n.row) }}</p>
              </template>
              <p class="flex flex-wrap gap-1 text-xs">
                <span
                  v-for="chip in chipsOf(n.row)"
                  :key="chip.field"
                  class="rounded-md px-1.5 py-0.5 font-medium"
                  :class="chip.own ? 'bg-violet-100 text-violet-900 ring-1 ring-violet-300' : 'bg-stone-100 text-stone-700'"
                  :title="chip.own ? $t('Solo de esta tarjeta') : undefined"
                  >{{ chip.text }}</span
                >
              </p>
              <p v-if="clipNote(n.row)" class="text-xs text-stone-500">{{ $t('Nota: «{note}»', { note: clipNote(n.row) }) }}</p>
            </div>
          </li>
        </ul>
      </section>
      <section v-else-if="ready" class="px-4 py-8 text-sm text-stone-600">
        <p>{{ $t('Para dar CAM y tubos: añade las mariposas arriba, elige qué va en el tubo y comprueba el tubo de cada tarjeta con su etiqueta (escribe o escanea).') }}</p>
      </section>
    </div>

    <!-- The right column on a wide screen: the options, with Save at its foot. -->
    <aside v-show="wide" class="flex min-h-0 w-[min(30rem,46%)] shrink-0 flex-col border-l border-stone-200 bg-stone-50" :aria-label="$t('Registrar tubos')">
      <div id="tubes-panel" data-scroll class="min-h-0 flex-1 overflow-y-auto" />
      <div id="tubes-save" />
    </aside>

    <!-- Save (or the last save, with Undo): hidden while typing on a phone held upright. -->
    <Teleport to="#tubes-save" defer :disabled="!wide">
      <footer
        v-if="(lastSave && !cards.length) || (canEdit && cards.length)"
        class="relative z-20 shrink-0 border-t border-stone-200 bg-white px-3 pt-2 pb-[calc(0.5rem+env(safe-area-inset-bottom))] short:pt-1.5 short:pb-1.5"
        :class="{ hidden: fitted && !wide }"
      >
        <div v-if="lastSave && !cards.length" class="flex items-center gap-2" role="status">
          <Check :size="22" class="shrink-0 text-brand-700" />
          <p class="min-w-0 flex-1 text-sm">
            <span class="font-medium">{{ $tn(lastSave.count, '{n} mariposa con tubo guardada', '{n} mariposas con tubo guardadas') }}</span>
            <span class="block truncate text-xs text-stone-500">{{ lastSave.labels.map(l => `${l.id} ${l.tube}`).join(', ') }}</span>
          </p>
          <button class="btn h-12 px-3" :title="$t('Imprimir etiquetas')" :aria-label="$t('Imprimir etiquetas')" @click="printLabels"><Printer :size="18" /></button>
          <button class="btn h-12 px-4 text-base" :disabled="undoing" @click="undo">
            <Loader2 v-if="undoing" :size="18" class="animate-spin" /><Undo2 v-else :size="18" /> {{ $t('Deshacer') }}
          </button>
          <button class="btn h-12 px-3" :aria-label="$t('Cerrar')" @click="lastSave = null"><X :size="18" /></button>
        </div>
        <template v-else>
          <div v-if="selectedIds.length && !wide" class="mb-1.5 flex items-center gap-2 text-sm">
            <span class="min-w-0 flex-1 truncate font-medium text-brand-800">{{ $t('Seleccionadas: {ids}', { ids: selectedIds.join(', ') }) }}</span>
            <button class="btn h-10" @click="toPanel">{{ $t('Opciones ↑') }}</button>
            <button class="btn h-10" @click="doneSelecting">{{ $t('Listo') }}</button>
          </div>
          <div :class="wide ? 'flex items-center gap-3' : ''">
            <button
              v-if="blocker.id"
              class="flex min-h-11 w-full min-w-0 items-center gap-1 text-left text-sm font-medium text-amber-900 underline decoration-amber-400 underline-offset-2"
              :class="wide ? 'flex-1' : 'mb-1 short:min-h-8'"
              @click="goTo(blocker.id, blocker.field)"
            >
              <AlertTriangle :size="16" class="shrink-0" /><span class="min-w-0 truncate">{{ blocker.text }}</span>
            </button>
            <p v-else :class="[blocker.text ? 'text-amber-900' : 'text-stone-600', wide ? 'line-clamp-2 min-w-0 flex-1 text-sm' : 'mb-1.5 truncate text-xs']">
              {{ blocker.text || summary }}
            </p>
            <button class="btn-primary h-13 text-base short:h-11" :class="wide ? 'shrink-0 px-6' : 'w-full'" :disabled="!!blocker.text || saving" @click="save">
              <Loader2 v-if="saving" :size="18" class="animate-spin" />
              {{ saving ? $t('Guardando…') : $tn(toSave.length, 'Guardar {n} mariposa', 'Guardar {n} mariposas') }}
            </button>
          </div>
        </template>
      </footer>
    </Teleport>

    <TubeScanner v-if="scanning" :status="scanStatus.text" :status-kind="scanStatus.kind" :next="scanNext" @read="onScan" @close="scanning = false" />
    <TabHistory v-if="showHistory" :title="$t('Historial de Tubos')" purpose="tubos" @close="showHistory = false" />
    <RowDrawer
      v-if="drawerRow && table"
      :module="MODULE"
      :row-id="drawerRow.id"
      :rows="table.rows"
      :creates="[]"
      :columns="table.columns"
      :options="options"
      :locked-fields="[]"
      :create-formulas="[]"
      label-field="Insectary_ID"
      @close="drawerRow = null"
      @changed="pending.touch()"
    />
    <TubeLabels :labels="lastSave?.labels ?? []" />
  </div>
</template>
