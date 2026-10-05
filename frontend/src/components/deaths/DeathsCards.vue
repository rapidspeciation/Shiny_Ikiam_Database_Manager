<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref, toRaw, watch } from 'vue'
import {
  AlertTriangle,
  Check,
  CheckCircle2,
  ChevronRight,
  Circle,
  History,
  Loader2,
  Search,
  StickyNote,
  Undo2,
  X,
} from 'lucide-vue-next'
import DateField from '../DateField.vue'
import EntryModeToggle from '../EntryModeToggle.vue'
import RowDrawer from '../RowDrawer.vue'
import IdFilters from '../IdFilters.vue'
import IdSuggestion from '../IdSuggestion.vue'
import DeathEditor from './DeathEditor.vue'
import SexBadge from '../SexBadge.vue'
import LifeBadge from './LifeBadge.vue'
import TabHistory from '../history/TabHistory.vue'
import { useDeathsState } from '../../composables/useDeathsState'
import type { EntryMode } from '../../composables/useEntryMode'
import { useKeyboard, useMedia } from '../../composables/usePhone'
import { api, requestId } from '../../lib/api'
import { isBlank } from '../../lib/cells'
import { dayLabel, formatSerial, isoToSerial, serialFromIso, serialToIso, todayIso } from '../../lib/dates'
import { noteDay } from '../../lib/clutches'
import {
  DEATH_NOTE_PHRASES,
  KILLED,
  WHOLE,
  bestRack,
  buildIndex,
  cardCells,
  choiceFor,
  factsOf,
  hasGap,
  lifeOf,
  lookAlikes,
  preservationGaps,
  usedSamples,
  rankCauses,
  searchKey,
  setChoice,
  sharedChoice,
  keepOwn,
  suggest,
  type ChoiceField,
  type DeathCell,
  type DeathChoice,
  type Entry,
  type Facts,
  type PreservationGap,
  type RackSuggestion,
  type Suggestion,
} from '../../lib/deaths'
import { isPattern, matchIds, type SexFilter } from '../../lib/idMatch'
import { useLookAlikes } from '../../composables/useLookAlikes'
import { idTokens, resolveIds } from '../../lib/ids'
import { errorText, notify } from '../../lib/notice'
import { verificationsFor } from '../../lib/verifications'
import type { AlertsData, MissingSample } from '../../lib/review'
import { fillIfBlank, initialsOf } from '../../lib/rows'
import type { CellValue, Table, TableRow } from '../../lib/types'
import { usePending } from '../../stores/pending'
import { useSession } from '../../stores/session'
import { type ServerRecord, useTables } from '../../stores/tables'
import { t, tn, tx, type Msg } from '../../lib/i18n'

/**
 * Muertes as cards, for a thumb in the insectary (phones, tablets, or anyone
 * who picks «Tarjetas»): a search box that finds a butterfly by Insectary ID
 * (or CAM or tube) and says at once whether it is alive; the butterflies
 * chosen as cards; the death date, the cause as big buttons and preserved or
 * not (each one's CAM and tube right under «Preservada»), and a note (quick
 * phrases or typed, in English) added to Notes_Insectary_data dated and
 * signed as the team writes notes; then one "Save" that writes exactly what
 * the table's «Escribir fecha y causa» writes, plus the note (lib/deaths.ts),
 * and saves it, with an Undo. With no card selected the date, cause,
 * preservation and note apply to all cards; tapping cards selects them, and
 * then they apply to those only (each card shows its own, and Save writes each
 * card's). The latest deaths below, by day; a card's › opens the full-screen
 * editor; «Historial» lists today's saves of Muertes, to undo one. On a wide
 * screen (a tablet, a phone held sideways) the search and cards take the left
 * and the registering, with Save, a column on the right. What is chosen is
 * shared with the table (useDeathsState).
 */
const MODULE = 'Insectary_data'
const props = defineProps<{
  table: Table | undefined
  ready: boolean
  options: Record<string, string[]>
  /** The Abbr_name list ("FCH - Franz Chandi"), for the initials that sign a note. */
  collectors: string[]
}>()
const mode = defineModel<EntryMode>('mode', { required: true })

const pending = usePending()
const session = useSession()
const tables = useTables()
const keyboard = useKeyboard()
/** Two columns from 700 px (tablets, phones sideways); the toggle names its modes from 1024 px (icons below). */
const wide = useMedia('(min-width: 700px)')
const roomy = useMedia('(min-width: 1024px)')
/** A short screen (a phone sideways, ~300 px): what is typed comes before explanations. */
const short = useMedia('(max-height: 520px)')

const { picked, date, cause, preserved, note, medium, samples, suggested, own, selected } = useDeathsState()
const query = ref('')
const recentCount = ref(30)
const today = computed(() => isoToSerial(todayIso()))
/** Who signs the notes added here ("1/10/26 FCH: …"), as in Clutches. */
const initials = computed(() => initialsOf(session.user?.displayName || '', props.collectors, session.user?.username || ''))
const notePrefix = computed(() => `${noteDay(today.value)} ${initials.value}:`)

// --- The butterflies, indexed once per version of the sheet (13,500 rows: typing must stay instant).
const index = computed(() => (props.table ? buildIndex(props.table.rows) : []))
const byKey = computed(() => new Map(index.value.map(e => [e.key, e])))
const known = computed(() => new Map(index.value.map(e => [e.key, e.id])))
/** Rows alive in the sheet; a row with unsaved edits is asked again (its death may be pending). */
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

const chosen = computed(() => picked.value.map(id => byKey.value.get(searchKey(id))?.row).filter((r): r is TableRow => !!r))
/** Newest first: the card just added shows right under the search box. */
const cards = computed(() => [...chosen.value].reverse())

// --- Search
const searchInput = ref<HTMLInputElement>()
const focused = ref(false)
/** What is seen on the butterfly in hand (IdFilters): ranks the IDs that fit what was typed. */
const sexFilter = ref<SexFilter>('')
const speciesFilter = ref('')
const lookAlikeTable = useLookAlikes()
/** Species of the butterflies alive, most first: the species filter's list. */
const aliveSpecies = computed(() => {
  const counts = new Map<string, number>()
  for (const e of index.value) {
    if (!aliveSaved.value.has(e.row.id)) continue
    const s = String(e.row.values.SPECIES ?? '').trim()
    if (s && !/^(NA|null)$/i.test(s)) counts.set(s, (counts.get(s) ?? 0) + 1)
  }
  return [...counts].sort((a, b) => b[1] - a[1]).map(([species, alive]) => ({ species, alive }))
})
/**
 * IDs that fit what was typed (lib/idMatch.ts: `*` or `?` for a character that
 * cannot be read, `[BD]` for one of two, look-alikes such as 6/8), then the
 * butterflies whose CAM or tube starts with it, or whose ID contains it.
 */
const suggestions = computed<Suggestion[]>(() => {
  const q = query.value.trim()
  if (!q) return []
  const skip = new Set(picked.value)
  const out: Suggestion[] = matchIds(index.value, q, {
    alive: isAlive,
    speciesOf: e => pending.value(e.row, 'SPECIES'),
    sexOf: e => pending.value(e.row, 'Sex'),
    species: speciesFilter.value,
    sex: sexFilter.value,
    skip: e => skip.has(e.id),
    table: lookAlikeTable.value,
  }).map(m => ({ entry: m.item, match: { at: m.at, greyed: !m.alive || !m.sameSpecies } }))
  if (!isPattern(q))
    for (const s of suggest(index.value, q, { alive: isAlive, skip })) {
      if (out.some(o => o.entry.id === s.entry.id)) continue
      // A CAM or tube typed whole comes first.
      if (s.via === searchKey(q)) out.unshift(s)
      else out.push(s)
    }
  return out.slice(0, 8)
})
/** What was typed that is no ID, with IDs that look like it ("did you mean"). */
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
  // A pattern (A?B, A[16]B): the best of the IDs it fits.
  else if (top && isPattern(text)) choose(top.entry.id)
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
function remove(id: string) {
  picked.value = picked.value.filter(p => searchKey(p) !== searchKey(id))
  selected.value = selected.value.filter(p => searchKey(p) !== searchKey(id))
  own.value = keepOwn(own.value, picked.value)
}
function removeAll() {
  picked.value = []
  missing.value = []
  selected.value = []
  own.value = {}
}

// --- Register: date, cause, preserved or not
const canEdit = computed(() => session.canEdit)
/**
 * The cause buttons: the sheet's own dropdown list for Death_cause (its data
 * validation), most used first; until it arrives, the values the column uses.
 */
const causes = computed(() => {
  const list = verificationsFor(MODULE)?.lists.Death_cause?.values
  const values = list?.size ? [...list] : (props.options.Death_cause || []).filter(v => !/^\d+$/.test(v))
  return rankCauses(values, props.table?.rows || [], today.value)
})
const quickDates = computed(() => [
  { iso: todayIso(), name: t('Hoy') },
  { iso: serialToIso(today.value - 1), name: t('Ayer') },
])
const dateError = computed(() =>
  shownDate.value && serialFromIso(shownDate.value) === null ? t('Fecha no válida: el año debe estar entre 1990 y 2099') : '',
)
// --- Each card its own: tap cards to select them; the panel then sets theirs only
/** The panel's values: what every card gets unless it has its own. */
const all = computed<DeathChoice>(() => ({ date: date.value, cause: cause.value, preserved: preserved.value, note: note.value }))
const choiceOf = (row: TableRow) => choiceFor(all.value, own.value, idOf(row))
const hasOwn = (row: TableRow, field: ChoiceField) => own.value[idOf(row)]?.[field] !== undefined
/** The selected cards still chosen, in the cards' order. */
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
/** What the panel shows for a field: the selected cards' shared value (undefined when they differ), else the panel's. */
function shown<F extends ChoiceField>(field: F): DeathChoice[F] | undefined {
  return selectedIds.value.length ? sharedChoice(all.value, own.value, selectedIds.value, field) : all.value[field]
}
/** Sets a field for the selected cards (or `ids`), or (none selected) for all of them. */
function setField<F extends ChoiceField>(field: F, value: DeathChoice[F], ids = selectedIds.value) {
  const next = setChoice(all.value, own.value, ids, field, value)
  if (next.all.date !== date.value) date.value = next.all.date
  if (next.all.cause !== cause.value) cause.value = next.all.cause
  if (next.all.preserved !== preserved.value) preserved.value = next.all.preserved
  if (next.all.note !== note.value) note.value = next.all.note
  own.value = next.own
}
/** With nothing selected: the cards that keep their own value of a field ("B7A: Eaten"). */
function ownOf(field: ChoiceField) {
  if (selectedIds.value.length) return []
  return cards.value.filter(r => hasOwn(r, field)).map(idOf)
}
const shownDate = computed(() => shown('date') ?? '')
function pickCause(c: string) {
  setField('cause', c)
  // Killed to be preserved: the body goes in a tube.
  if (c === KILLED) setField('preserved', true)
}
const mediums = ['Flash frozen', 'Ethanol', 'DMSO']

// --- The note: typed or quick phrases (English), added on Save after the notes there
const shownNote = computed(() => shown('note') ?? '')
/** A quick phrase goes after what is typed ("Head eaten; With fungi"). */
function addPhrase(p: string) {
  const text = shownNote.value.trim()
  setField('note', text ? `${text}; ${p}` : p)
}
/** The note a card adds on Save, as it will be written ("1/10/26 FCH: Only wings found"); `own`: this card's only. */
function noteOf(row: TableRow) {
  const text = choiceOf(row).note.trim()
  return text ? { text: `${notePrefix.value} ${text}`, own: hasOwn(row, 'note') } : null
}

/** Rows dying now (no death date yet): in "preserved" each gets its CAM and tube. */
const dying = (row: TableRow) => lifeOf(get(row)).state !== 'dead'
const toPreserve = computed(() => cards.value.filter(r => choiceOf(r).preserved && dying(r)))
/** Already recorded dead: "preserved" does not give them a tube here (Tubos does). */
const notToPreserve = computed(() => cards.value.filter(r => choiceOf(r).preserved && !dying(r)))
/** The medium and the CAM and tube boxes show while any card is preserved, or the panel says «Preservada». */
const showPreservation = computed(
  () => shown('preserved') === true || toPreserve.value.length > 0 || notToPreserve.value.length > 0,
)
const sampleOf = (id: string) => (samples[id] ??= { cam: '', tube: '' })

// The next free CAM IDs and tubes (server/grid.mjs idSuggestions), asked for only when preserving.
const camRun = ref<string[]>([])
const tubeRun = ref<string[]>([])
const racks = ref<(RackSuggestion & { label: string; labelMsg?: Msg })[]>([])
const rack = computed(() => bestRack(racks.value, chosen.value, medium.value))
let camStart = ''
async function loadSampleIds() {
  if (!toPreserve.value.length) return
  try {
    if (!camStart || !racks.value.length) {
      const [cam, tube] = await Promise.all([
        api<{ suggestions: { value: string }[] }>('ids?kind=cam'),
        api<{ suggestions: (RackSuggestion & { label: string; labelMsg?: Msg })[] }>('ids?kind=tube'),
      ])
      camStart = cam.suggestions[0]?.value || ''
      racks.value = tube.suggestions
    }
    const count = toPreserve.value.length + 4
    const run = (kind: string, start: string) =>
      api<{ sequence: string[] }>(`ids?kind=${kind}&start=${encodeURIComponent(start)}&count=${count}`).then(r => r.sequence)
    const [cams, tubes] = await Promise.all([
      camStart ? run('cam', camStart) : [],
      rack.value ? run('tube', rack.value.value) : [],
    ])
    // The same runs again (a second load while racks arrived) must not refill a box the person just emptied.
    if (cams.join() !== camRun.value.join()) camRun.value = cams
    if (tubes.join() !== tubeRun.value.join()) tubeRun.value = tubes
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
watch([() => toPreserve.value.length, () => rack.value?.value], loadSampleIds, { immediate: true })
/**
 * Gives each card being preserved the next free CAM and tube no other card has,
 * keeping what the person typed and what was suggested before (a suggestion
 * from another rack, after the medium changed, gives way to the new rack's).
 */
watch([toPreserve, camRun, tubeRun], () => {
  const ids = toPreserve.value.map(idOf)
  const taken = (kind: 'cam' | 'tube', value: string, owner: string) =>
    ids.some(other => other !== owner && samples[other]?.[kind] === value)
  for (const row of toPreserve.value) {
    const id = idOf(row)
    const s = sampleOf(id)
    const auto = (suggested[id] ??= { cam: '', tube: '' })
    if (s.cam && s.cam === auto.cam && camRun.value.length && !camRun.value.includes(s.cam)) s.cam = auto.cam = ''
    if (s.tube && s.tube === auto.tube && tubeRun.value.length && !tubeRun.value.includes(s.tube)) s.tube = auto.tube = ''
    // A box the person emptied stays empty (`auto` still holds what was suggested there).
    if (!s.cam && !auto.cam && isBlank(pending.value(row, 'CAM_ID'))) {
      const next = camRun.value.find(v => !taken('cam', v, id))
      if (next) s.cam = auto.cam = next
    }
    if (!s.tube && !auto.tube) {
      const next = tubeRun.value.find(v => !taken('tube', v, id))
      if (next) s.tube = auto.tube = next
    }
  }
})
/** What each butterfly being preserved still lacks (CAM, tube, a free slot), shown in its row and above Save. */
/** The CAMs and tubes already in the sheet: one typed again is flagged before Save (the server would refuse it). */
const used = computed(() => usedSamples(index.value))
const gaps = computed(() => preservationGaps(toPreserve.value, pending.value, samples, used.value))
const gapById = computed(() => new Map(gaps.value.map(g => [g.id, g])))
const readyCount = computed(() => gaps.value.filter(g => !hasGap(g)).length)
const isSuggested = (id: string, kind: 'cam' | 'tube') => !!samples[id]?.[kind] && samples[id]?.[kind] === suggested[id]?.[kind]
function gapText(g: PreservationGap) {
  if (g.slot === null) return t('{id} no tiene sitio para otro tubo', { id: g.id })
  if (g.cam === 'missing') return t('Falta el CAM de {id}', { id: g.id })
  if (g.tube === 'missing') return t('Falta el tubo de {id}', { id: g.id })
  return t('{value} está en {a} y en {b}', { value: g.value ?? '', a: g.with ?? '', b: g.id })
}

/** The cells "Save" would write in each chosen row (the same as the table's «Escribir fecha y causa»). */
const plans = computed(() => {
  const out = new Map<string, DeathCell[]>()
  for (const row of cards.value)
    out.set(
      row.id,
      cardCells(row, pending.value, choiceOf(row), {
        sample: samples[idOf(row)],
        medium: medium.value,
        today: today.value,
        initials: initials.value,
      }),
    )
  return out
})
const toSave = computed(() => cards.value.filter(r => plans.value.get(r.id)?.length))
/** Already recorded dead and nothing of its death left to write (its note may still be added). */
const registered = (row: TableRow) =>
  factsFor(row).life.state === 'dead' && !(plans.value.get(row.id) || []).some(c => c.field !== 'Notes_Insectary_data')
/** A card's plan in words: the date and cause, the CAM and tube, and how many NA / NOT_COLLECTED cells. */
function planText(row: TableRow) {
  const cells = plans.value.get(row.id) || []
  if (!cells.length) return ''
  const main: string[] = []
  let rest = 0
  for (const c of cells) {
    if (c.field === 'Death_date' && typeof c.value === 'number') main.push(formatSerial(c.value))
    else if ((['Death_cause', 'CAM_ID'].includes(c.field) || /^Tube_\d_id$/.test(c.field)) && !isBlank(c.value))
      main.push(String(c.value))
    else rest++
  }
  return rest ? `${main.join(' · ')} ${tn(rest, '+{n} celda', '+{n} celdas')}` : main.join(' · ')
}

/** Why "Save" cannot run yet (shown above the button: there is no hover on a phone); `gap` takes you to it. */
const blocker = computed<{ text: string; gap?: string }>(() => {
  if (!chosen.value.length) return { text: t('Añade al menos una mariposa') }
  // Each card's own date and cause; "of B7A" only when some cards have theirs.
  const name = (rows: TableRow[]) => (rows.length < cards.value.length ? rows.map(idOf).join(', ') : '')
  const badDate = cards.value.filter(r => choiceOf(r).date && serialFromIso(choiceOf(r).date) === null)
  if (badDate.length)
    return {
      text: name(badDate)
        ? `${idOf(badDate[0])}: ${t('Fecha no válida: el año debe estar entre 1990 y 2099')}`
        : t('Fecha no válida: el año debe estar entre 1990 y 2099'),
    }
  const noDate = cards.value.filter(r => !choiceOf(r).date)
  if (noDate.length)
    return { text: name(noDate) ? t('Falta la fecha de {ids}', { ids: name(noDate) }) : t('Elige la fecha de muerte') }
  const noCause = cards.value.filter(r => !choiceOf(r).cause && dying(r))
  if (noCause.length) return { text: name(noCause) ? t('Falta la causa de {ids}', { ids: name(noCause) }) : t('Elige la causa') }
  const first = gaps.value.find(hasGap)
  if (first) {
    const more = gaps.value.filter(hasGap).length - 1
    const text = gapText(first)
    return { text: more ? `${text} ${tn(more, '(y {n} más)', '(y {n} más)')}` : text, gap: first.id }
  }
  if (!toSave.value.length) return { text: t('Nada que escribir: esas filas ya tienen fecha y causa') }
  return { text: '' }
})
/** Pending changes of other rows, which go to the sheet in the same save. */
const otherPending = computed(() => {
  const mine = new Set(chosen.value.map(r => r.id))
  let n = pending.creates.length
  for (const e of Object.values(pending.edits)) if (!mine.has(e.id)) n += Object.keys(e.values).length
  return n
})

// --- Save, then Undo
const saving = ref(false)
const lastSave = ref<null | { actionId: string; ids: string[]; count: number; all: DeathChoice; own: typeof own.value }>(null)
const undoing = ref(false)
const waitIdle = async () => {
  for (let i = 0; i < 300 && pending.saving; i++) await new Promise(r => setTimeout(r, 100))
}
async function save() {
  if (blocker.value.text || saving.value) return
  saving.value = true
  const rows = toSave.value
  const ids = rows.map(idOf)
  const choices = { all: { ...all.value }, own: JSON.parse(JSON.stringify(own.value)) as typeof own.value }
  try {
    for (const row of rows)
      for (const c of plans.value.get(row.id) || []) fillIfBlank(MODULE, row, idOf(row), c.field, c.value, c.overwrite)
    pending.touch()
    // An automatic save already running would leave these cells for later (and give no Undo).
    await waitIdle()
    const result = await pending.save('')
    const refused = rows.filter(r => Object.keys(pending.issues).some(k => k.startsWith(`${r.id}:`)))
    if (refused.length) {
      notify(
        t('No se guardó {ids}: {reason}', {
          ids: refused.map(idOf).join(', '),
          reason: Object.values(pending.issues)[0] ?? '',
        }),
        'error',
      )
      picked.value = picked.value.filter(id => refused.some(r => idOf(r) === id))
      own.value = keepOwn(own.value, picked.value)
      selected.value = []
      return
    }
    lastSave.value = result.actionId ? { actionId: result.actionId, ids, count: rows.length, ...choices } : null
    picked.value = []
    cause.value = ''
    preserved.value = false
    note.value = ''
    own.value = {}
    selected.value = []
    for (const id of ids) {
      delete samples[id]
      delete suggested[id]
    }
    if (!result.actionId)
      notify(tn(rows.length, '{n} muerte guardada en Google Sheets', '{n} muertes guardadas en Google Sheets'), 'success')
    // Back to the top: the search for the next ones, and today's deaths in the list.
    scroller.value?.scrollTo({ top: 0 })
    aside.value?.scrollTo({ top: 0 })
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
    // The cards come back, to correct and save again.
    // (`ids` are in card order, newest first: added back oldest first, the cards look as before.)
    picked.value = [...new Set([...picked.value, ...[...last.ids].reverse()])]
    // With the date, cause and preservation they were saved with (each card's own too).
    date.value = last.all.date
    cause.value = last.all.cause
    preserved.value = last.all.preserved
    note.value = last.all.note ?? ''
    own.value = { ...own.value, ...keepOwn(last.own, last.ids) }
    lastSave.value = null
    notify(tn(last.count, '{n} muerte deshecha en Google Sheets', '{n} muertes deshechas en Google Sheets'), 'success')
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    undoing.value = false
  }
}

// --- Preserved without CAM or tube (server/alerts.mjs, kept by the server): an amber chip on its card and line.
const noSample = ref(new Map<string, MissingSample>())
onMounted(async () => {
  try {
    const data = await api<AlertsData>('alerts')
    noSample.value = new Map((data.missingSamples ?? []).map(s => [s.recordId, s]))
  } catch {
    /* The cards work without it. */
  }
})
/** Listed, and a cell it lacked still holds no ID (a CAM or tube typed here takes the chip away at once). */
const sampleGap = (row: TableRow) => {
  const s = noSample.value.get(row.id)
  return s && s.missing.some(f => !/\d/.test(String(pending.value(row, f) ?? ''))) ? s : null
}
const sampleTitle = (s: MissingSample) => `${t('Preservada sin CAM o tubo')} · ${t('pregunta al equipo')}`

// --- Latest deaths, by day
const recent = computed(() => {
  if (!props.table) return []
  return props.table.rows
    .filter(r => r.observed && typeof r.values.Death_date === 'number' && !isBlank(r.values.Insectary_ID))
    .sort((a, b) => (b.values.Death_date as number) - (a.values.Death_date as number) || b.row - a.row)
    .slice(0, recentCount.value)
})
const recentGroups = computed(() => {
  const groups: { label: string; rows: { row: TableRow; at: number }[] }[] = []
  recent.value.forEach((row, at) => {
    const day = row.values.Death_date as number
    const ago = today.value - day
    const label = ago === 0 ? t('Hoy') : ago === 1 ? t('Ayer') : dayLabel(serialToIso(day)).split(' · ')[0]
    if (groups.at(-1)?.label !== label) groups.push({ label, rows: [] })
    groups.at(-1)!.rows.push({ row, at })
  })
  return groups
})

// --- The full-screen editor, over the list it was opened from
const editing = ref<null | { list: 'cards' | 'recent'; ids: string[]; index: number }>(null)
const rowById = computed(() => new Map((props.table?.rows || []).map(r => [r.id, r])))
const editorRows = computed(() =>
  editing.value ? editing.value.ids.map(id => rowById.value.get(id)).filter((r): r is TableRow => !!r) : [],
)
function openEditor(list: 'cards' | 'recent', at: number) {
  const rows = list === 'cards' ? cards.value : recent.value
  editing.value = { list, ids: rows.map(r => r.id), index: at }
}
/** A card's note (its own or the panel's), shown and edited in the editor opened from the cards. */
const cardNote = (row: TableRow) => choiceOf(row).note
const drawerRow = ref<TableRow | null>(null)

// --- The keyboard: the screen fits above it, and the box being typed in stays in view
const root = ref<HTMLElement>()
/** The page's scroller (one column), or the left column's (search, cards, latest deaths). */
const scroller = ref<HTMLElement>()
/** The right column's scroller on a wide screen (date, cause, preservation). */
const aside = ref<HTMLElement>()
const footer = ref<HTMLElement>()
const searchBar = ref<HTMLElement>()
const details = ref<HTMLElement>()
const rootBottom = ref(0)
const footerHeight = ref(0)
const measure = () => {
  rootBottom.value = root.value?.getBoundingClientRect().bottom ?? 0
  footerHeight.value = footer.value?.offsetHeight ?? 0
}
/**
 * With the keyboard open the screen is pinned to what is visible (Chrome only
 * shrinks the visible area and may pan the page, hiding the search bar).
 */
const fitted = computed(() => keyboard.open.value)
/**
 * Too little left to show the box being typed and Save together (Gboard
 * sideways can leave 50 px until its suggestion strip appears): the box wins,
 * Save comes back with more room or once the keyboard closes.
 */
const cramped = computed(() => fitted.value && keyboard.visibleBottom.value - keyboard.visibleTop.value < 140)
/** The column the save bar is in: on a wide screen the right one. */
const saveColumn = computed(() => (wide.value ? aside.value : scroller.value))
/** The part of a column one can see: under its sticky bar and the top of the screen, above the keyboard and the save bar. */
function visibleBand(box: HTMLElement) {
  const sticky = box === scroller.value ? (searchBar.value?.getBoundingClientRect().bottom ?? 0) : box.getBoundingClientRect().top
  const top = Math.max(sticky, keyboard.visibleTop.value) + 8
  const save = box === saveColumn.value ? footerHeight.value : 0
  const bottom = Math.min(rootBottom.value, keyboard.visibleBottom.value) - save - 8
  return { top, bottom }
}
/** The suggestions fill the space between the search box and the save bar (or the keyboard). */
const listHeight = computed(() => {
  const top = searchBar.value?.getBoundingClientRect().bottom ?? 0
  const save = wide.value ? 0 : footerHeight.value
  const bottom = Math.min(rootBottom.value, keyboard.visibleBottom.value) - save
  return Math.max(120, Math.round(bottom - top - 8))
})
/** Scrolls its column so `el` is in view; `above` keeps that much of what is just above it visible too. */
function reveal(el: Element | null | undefined, above = 0) {
  const box = el?.closest<HTMLElement>('[data-scroll]')
  if (!el || !box) return
  const r = el.getBoundingClientRect()
  const { top, bottom } = visibleBand(box)
  if (r.bottom > bottom) box.scrollTop += Math.min(r.bottom - bottom, r.top - above - top)
  else if (r.top - above < top) box.scrollTop -= top - (r.top - above)
}
function keepInView() {
  const el = document.activeElement as HTMLElement | null
  // Only boxes one types in (a tapped button stays where it is).
  if (!el || el === searchInput.value || !el.matches('input, textarea') || !root.value?.contains(el)) return
  reveal(el)
}
watch([keyboard.visibleBottom, keyboard.visibleTop], () => {
  measure()
  requestAnimationFrame(keepInView)
})
const sizes = typeof ResizeObserver === 'undefined' ? null : new ResizeObserver(measure)
onMounted(() => {
  measure()
  if (root.value) sizes?.observe(root.value)
  if (footer.value) sizes?.observe(footer.value)
})
watch(footer, (el, old) => {
  if (old) sizes?.unobserve(old)
  if (el) sizes?.observe(el)
  nextTick(measure)
})
watch(wide, () => nextTick(measure))
onBeforeUnmount(() => sizes?.disconnect())
function onFocusIn() {
  measure()
  setTimeout(keepInView, 350)
}

// --- Preserved: its details show right under the choice, in view, and lit for a moment
const flash = ref(false)
let flashTimer: ReturnType<typeof setTimeout> | undefined
watch(showPreservation, now => {
  if (!now) return
  nextTick(() => {
    // The boxes to fill, with «Preservada» still in view above them where there is room.
    if (short.value) reveal(details.value?.querySelector('ul') ?? details.value, 8)
    else reveal(details.value, 120)
    flash.value = true
    clearTimeout(flashTimer)
    flashTimer = setTimeout(() => (flash.value = false), 1400)
  })
})
onBeforeUnmount(() => clearTimeout(flashTimer))
/** Goes to the box a butterfly still lacks (from its card or the message above Save) and opens it for typing. */
function goToGap(id: string) {
  const g = gapById.value.get(id)
  const kind = g?.cam ? 'cam' : 'tube'
  const input = details.value?.querySelector<HTMLInputElement>(`[data-sample="${CSS.escape(id)}:${kind}"]`)
  if (!input) return reveal(details.value?.querySelector(`[data-row="${CSS.escape(id)}"]`))
  reveal(input)
  input.focus()
}
/** Enter in a CAM or tube box goes to the next box (the next butterfly's), the last one closes the keyboard. */
function nextBox(event: KeyboardEvent) {
  const boxes = [...(details.value?.querySelectorAll<HTMLInputElement>('input[data-sample]') ?? [])]
  const at = boxes.indexOf(event.target as HTMLInputElement)
  const next = boxes[at + 1]
  if (next) next.focus()
  else (event.target as HTMLInputElement).blur()
}

const summary = computed(() => {
  const ids = cards.value.map(idOf)
  const same = <F extends ChoiceField>(field: F) => sharedChoice(all.value, own.value, ids, field)
  const day = same('date')
  const why = same('cause')
  const kept = same('preserved')
  const noted = cards.value.filter(noteOf).length
  const parts = [
    day === undefined ? t('varias fechas') : day ? dayLabel(day).split(' · ')[0] : '',
    why === undefined ? t('varias causas') : why,
    kept === undefined ? t('algunas preservadas') : kept ? t('preservadas') : t('sin preservar'),
    noted ? (noted === cards.value.length ? t('con nota') : t('algunas con nota')) : '',
  ].filter(Boolean)
  return parts.join(' · ')
})
/** A card's date, cause and preservation, as chips; `own`: set for this card only. */
function chipsOf(row: TableRow) {
  const c = choiceOf(row)
  const s = samples[idOf(row)]
  const preserving = c.preserved && dying(row)
  return [
    {
      field: 'date' as const,
      text: c.date ? (serialFromIso(c.date) !== null ? formatSerial(isoToSerial(c.date)) : c.date) : t('sin fecha'),
      missing: !c.date,
    },
    { field: 'cause' as const, text: c.cause || t('sin causa'), missing: !c.cause && dying(row) },
    {
      field: 'preserved' as const,
      text: preserving
        ? [t('Preservada'), s?.cam.trim().toUpperCase(), s?.tube.trim().toUpperCase()].filter(Boolean).join(' · ')
        : c.preserved
          ? t('Preservada')
          : t('Sin preservar'),
      missing: false,
    },
  ].map(chip => ({ ...chip, own: hasOwn(row, chip.field) }))
}
const showHistory = ref(false)
/** Clutch and the day it entered the insectary, as one line under the species (the sex is a badge beside it). */
const line = (f: Facts) =>
  [
    f.clutch && t('clutch {c}', { c: f.clutch }),
    f.entered !== null &&
      (f.wild
        ? t('Capturada {date}', { date: formatSerial(f.entered) })
        : t('Emergió {date}', { date: formatSerial(f.entered) })),
  ]
    .filter(Boolean)
    .join(' · ')
const cellText = (v: CellValue) => (v === null || v === undefined ? '' : String(v))
const choice = (on: boolean) =>
  on ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white text-stone-800 active:bg-stone-100'
</script>

<template>
  <!-- While the keyboard is open the screen fits the part one can see (a phone sideways keeps ~115 px):
       the search bar at its top, Save at its bottom, the box being typed in between. -->
  <div
    ref="root"
    class="flex h-full bg-stone-50"
    :class="[wide ? 'flex-row' : 'flex-col', fitted ? 'fixed inset-x-0 z-30' : '']"
    :style="
      fitted
        ? { top: `${keyboard.visibleTop.value}px`, height: `${keyboard.visibleBottom.value - keyboard.visibleTop.value}px` }
        : undefined
    "
    @focusin="onFocusIn"
  >
    <div ref="scroller" data-scroll class="min-h-0 min-w-0 flex-1 overflow-y-auto">
      <!-- The search stays at the top while the cards scroll. -->
      <div ref="searchBar" class="sticky top-0 z-20 border-b border-stone-200 bg-white px-3 pt-3 pb-2 short:pt-1.5 short:pb-1.5">
        <div class="flex items-start gap-2">
          <div class="relative min-w-0 flex-1">
            <div class="relative">
              <Search :size="20" class="pointer-events-none absolute top-1/2 left-3 -translate-y-1/2 text-stone-400" />
              <input
                ref="searchInput"
                v-model="query"
                class="h-13 w-full rounded-xl border border-stone-300 bg-white pr-12 pl-10 text-lg font-medium uppercase placeholder:text-base placeholder:font-normal placeholder:normal-case focus:border-brand-600 focus:ring-2 focus:ring-brand-100 focus:outline-none short:h-11"
                :placeholder="$t('Insectary ID, CAM o tubo (A?B, A[16]B)')"
                :aria-label="$t('Buscar una mariposa por Insectary ID, CAM o tubo')"
                type="text"
                inputmode="text"
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
            </div>
            <!-- What is seen on the butterfly ranks the IDs offered (only while typing: the bar stays small otherwise). -->
            <IdFilters
              v-if="focused || query"
              v-model:sex="sexFilter"
              v-model:species="speciesFilter"
              :species-list="aliveSpecies"
              class="mt-2 short:hidden"
            />
            <!-- Suggestions: tapping one adds its card and keeps the keyboard for the next ID. -->
            <ul
              v-if="focused && suggestions.length"
              class="absolute inset-x-0 top-full z-30 mt-1 divide-y divide-stone-100 overflow-y-auto rounded-xl border border-stone-200 bg-white shadow-lg"
              :style="{ maxHeight: `${listHeight}px` }"
              role="listbox"
            >
              <li v-for="s in suggestions" :key="s.entry.id">
                <button
                  class="flex min-h-14 w-full items-center gap-3 px-3 py-2 text-left active:bg-brand-50 short:min-h-12"
                  role="option"
                  @mousedown.prevent
                  @click="choose(s.entry.id)"
                >
                  <IdSuggestion
                    :id="s.entry.id"
                    :facts="factsFor(s.entry.row)"
                    :at="s.match?.at"
                    :via="s.via"
                    :greyed="s.match?.greyed"
                  />
                </button>
              </li>
            </ul>
          </div>
          <!-- Cards or the table: kept in this browser. -->
          <EntryModeToggle v-model="mode" :compact="!roomy" class="h-13 shrink-0 short:h-11 *:min-w-11" />
          <!-- Today's saves of Muertes, to see and undo an accident. -->
          <button
            class="flex h-13 min-w-11 shrink-0 items-center justify-center gap-1 rounded-md border border-stone-300 bg-white px-2 text-sm text-stone-700 active:bg-stone-100 short:h-11"
            :aria-label="$t('Historial de Muertes')"
            :title="$t('Historial de Muertes')"
            @click="showHistory = true"
          >
            <History :size="18" /> <span v-if="roomy">{{ $t('Historial') }}</span>
          </button>
        </div>
        <p v-if="!ready" class="mt-1.5 text-sm text-stone-500">{{ $t('Cargando {sheet}…', { sheet: MODULE }) }}</p>
        <p v-else-if="alreadyChosen" class="mt-1.5 text-sm text-stone-600">
          {{ $t('{id} ya está en las tarjetas', { id: alreadyChosen }) }}
        </p>
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
          {{ $t('Escribe un ID para ver si está viva; pega varios o un rango (B0D-B9D) para registrar muertes.') }}
        </p>
      </div>

      <!-- The chosen butterflies: tap one (or several) to give it its own date, cause, preservation or note. -->
      <section v-if="cards.length" class="px-3 pt-3">
        <div class="flex items-center justify-between gap-2">
          <h2 class="text-sm font-semibold text-stone-700">{{ $t('Elegidas ({n})', { n: cards.length }) }}</h2>
          <button class="h-11 px-2 text-sm text-stone-600 underline" @click="removeAll">{{ $t('Quitar todas') }}</button>
        </div>
        <p v-if="canEdit && cards.length > 1 && !selectedIds.length" class="mb-1.5 text-xs text-stone-500 short:hidden">
          {{ $t('Toca una tarjeta para darle su propia fecha, causa, preservación o nota.') }}
        </p>
        <ul class="grid grid-cols-[repeat(auto-fill,minmax(17rem,1fr))] gap-2">
          <li
            v-for="(row, i) in cards"
            :key="row.id"
            class="relative flex flex-col rounded-xl border-2 shadow-sm"
            :class="isSelected(row) ? 'border-brand-600 bg-brand-50 ring-4 ring-brand-600/30' : 'border-stone-200 bg-white'"
          >
            <button
              class="block w-full flex-1 rounded-t-xl px-3 pt-2.5 pr-24 pb-2 text-left"
              :aria-pressed="canEdit ? isSelected(row) : undefined"
              :aria-label="
                canEdit
                  ? isSelected(row)
                    ? $t('{id} seleccionada: toca para quitarla de la selección', { id: idOf(row) })
                    : $t('Seleccionar {id}', { id: idOf(row) })
                  : undefined
              "
              @click="canEdit ? toggleSelect(row) : openEditor('cards', i)"
            >
              <span class="flex flex-wrap items-center gap-2">
                <component
                  :is="isSelected(row) ? CheckCircle2 : Circle"
                  v-if="canEdit"
                  :size="22"
                  class="shrink-0"
                  :class="isSelected(row) ? 'text-brand-700' : 'text-stone-300'"
                />
                <span class="text-xl font-semibold">{{ row.values.Insectary_ID }}</span>
                <LifeBadge :facts="factsFor(row)" />
                <span
                  v-if="sampleGap(row)"
                  class="rounded-md bg-amber-100 px-1.5 py-0.5 text-xs font-medium text-amber-900"
                  :title="sampleTitle(sampleGap(row)!)"
                  >{{ $t('Sin CAM/tubo') }}</span
                >
              </span>
              <span class="mt-0.5 block text-sm">{{ factsFor(row).species || '—' }}</span>
              <span class="flex items-center gap-1.5 text-xs text-stone-600"
                ><SexBadge :sex="factsFor(row).sex" />{{ line(factsFor(row)) }}</span
              >
              <span v-if="factsFor(row).life.cause" class="block text-xs text-stone-700"
                >Death_cause: {{ factsFor(row).life.cause }}</span
              >
              <span v-if="factsFor(row).notes" class="block truncate text-xs text-stone-500">{{ factsFor(row).notes }}</span>
            </button>
            <div class="absolute top-1 right-1 flex">
              <button
                class="grid h-11 w-11 place-items-center rounded-lg text-stone-600 active:bg-stone-100"
                :aria-label="$t('Ver la ficha de {id}', { id: idOf(row) })"
                :title="$t('Ver la ficha de {id}', { id: idOf(row) })"
                @click="openEditor('cards', i)"
              >
                <ChevronRight :size="22" />
              </button>
              <button
                class="grid h-11 w-11 place-items-center rounded-lg text-stone-500 active:bg-stone-100"
                :aria-label="$t('Quitar {id}', { id: idOf(row) })"
                @click="remove(idOf(row))"
              >
                <X :size="20" />
              </button>
            </div>
            <!-- Preserved and still lacking its CAM or tube: a tap goes to the box, under «Preservada». -->
            <button
              v-if="canEdit && gapById.get(idOf(row)) && hasGap(gapById.get(idOf(row))!)"
              class="flex min-h-11 items-center gap-1.5 border-t border-amber-200 bg-amber-50 px-3 py-1.5 text-left text-sm font-medium text-amber-900"
              @click="goToGap(idOf(row))"
            >
              <AlertTriangle :size="16" class="shrink-0" />
              <span class="min-w-0 flex-1">{{ gapText(gapById.get(idOf(row))!) }}</span>
              <ChevronRight :size="16" class="shrink-0" />
            </button>
            <!-- This card's date, cause and preservation (its own ones in violet), or why nothing is written. -->
            <div
              v-if="canEdit"
              class="rounded-b-xl border-t px-3 py-1.5 text-xs"
              :class="isSelected(row) ? 'border-brand-100' : 'border-stone-100'"
            >
              <p v-if="registered(row)" class="text-stone-500">
                {{ noteOf(row) ? $t('Ya registrada: solo se añade la nota') : $t('Ya registrada: no se cambiará') }}
              </p>
              <p v-else class="flex flex-wrap gap-1" :data-choice="idOf(row)">
                <span
                  v-for="chip in chipsOf(row)"
                  :key="chip.field"
                  class="rounded-md px-1.5 py-0.5 font-medium"
                  :class="
                    chip.missing
                      ? 'bg-amber-100 text-amber-900'
                      : chip.own
                        ? 'bg-violet-100 text-violet-900 ring-1 ring-violet-300'
                        : 'bg-stone-100 text-stone-700'
                  "
                  :title="chip.own ? $t('Solo de esta tarjeta') : undefined"
                  >{{ chip.text }}</span
                >
              </p>
              <!-- The note Save adds, as it will be written (its own one in violet). -->
              <p
                v-if="noteOf(row)"
                class="mt-1 flex items-start gap-1 rounded-md px-1.5 py-0.5 break-words"
                :class="noteOf(row)!.own ? 'bg-violet-100 text-violet-900 ring-1 ring-violet-300' : 'bg-stone-100 text-stone-700'"
                :title="noteOf(row)!.own ? $t('Solo de esta tarjeta') : undefined"
                :data-note="idOf(row)"
              >
                <StickyNote :size="13" class="mt-px shrink-0" /><span class="min-w-0">{{ noteOf(row)!.text }}</span>
              </p>
            </div>
          </li>
        </ul>
      </section>

      <!-- How they died: here on a phone held upright, in the right column on a wide screen. -->
      <Teleport to="#deaths-register" defer :disabled="!wide">
        <section v-if="canEdit && cards.length" class="space-y-5 px-3 pb-2" :class="wide ? 'pt-3' : 'pt-5'">
          <!-- What the choices below change: the selected cards, or all of them. -->
          <div
            class="sticky top-0 z-10 -mx-3 flex min-h-12 items-center gap-2 border-y px-3 py-1.5"
            :class="
              selectedIds.length ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-200 bg-stone-100 text-stone-800'
            "
            role="status"
            data-applies
          >
            <p class="min-w-0 flex-1 text-sm">
              <span class="font-semibold">{{
                selectedIds.length
                  ? $t('Se aplica a {ids}', { ids: selectedIds.join(', ') })
                  : $tn(cards.length, 'Se aplica a la única tarjeta', 'Se aplica a las {n} tarjetas')
              }}</span>
              <span v-if="selectedIds.length" class="block text-xs opacity-90">{{
                $t('Solo a las seleccionadas; las demás siguen igual.')
              }}</span>
            </p>
            <button
              v-if="selectedIds.length"
              class="h-10 shrink-0 rounded-lg bg-white px-4 text-sm font-semibold text-brand-800 active:bg-brand-50"
              @click="doneSelecting"
            >
              {{ $t('Listo') }}
            </button>
          </div>
          <div>
            <h2 class="mb-1.5 text-sm font-semibold text-stone-700">{{ $t('Fecha de muerte') }}</h2>
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
              <DateField
                :model-value="shownDate"
                class="field-input h-12 text-base"
                @update:model-value="setField('date', $event)"
              />
            </div>
            <p v-if="dateError" class="mt-1 text-sm text-red-700">{{ dateError }}</p>
            <p v-else-if="shownDate" class="mt-1 text-sm text-stone-600">{{ dayLabel(shownDate) }}</p>
            <p v-else-if="shown('date') === undefined" class="mt-1 text-sm text-stone-600">
              {{ $t('Fechas distintas: elige una para todas las seleccionadas') }}
            </p>
            <p v-if="ownOf('date').length" class="mt-1 text-xs text-violet-800">
              {{ $t('Con fecha propia: {ids}', { ids: ownOf('date').join(', ') }) }}
            </p>
          </div>
          <div>
            <h2 class="mb-1.5 text-sm font-semibold text-stone-700">Death_cause</h2>
            <div class="grid grid-cols-[repeat(auto-fill,minmax(8.5rem,1fr))] gap-2">
              <button
                v-for="c in causes"
                :key="c"
                class="min-h-12 rounded-lg border px-2 py-2 text-base font-medium break-words"
                :class="choice(shown('cause') === c)"
                :aria-pressed="shown('cause') === c"
                @click="pickCause(c)"
              >
                {{ c }}
              </button>
            </div>
            <p v-if="shown('cause') === undefined" class="mt-1 text-sm text-stone-600">
              {{ $t('Causas distintas: elige una para todas las seleccionadas') }}
            </p>
            <p v-if="ownOf('cause').length" class="mt-1 text-xs text-violet-800">
              {{ $t('Con causa propia: {ids}', { ids: ownOf('cause').join(', ') }) }}
            </p>
          </div>
          <div>
            <h2 class="mb-1.5 text-sm font-semibold text-stone-700">{{ $t('Preservación') }}</h2>
            <div class="grid grid-cols-2 gap-2">
              <button
                class="min-h-12 rounded-lg border px-2 text-base font-medium"
                :class="choice(shown('preserved') === false)"
                :aria-pressed="shown('preserved') === false"
                @click="setField('preserved', false)"
              >
                {{ $t('Sin preservar') }}
              </button>
              <button
                class="min-h-12 rounded-lg border px-2 text-base font-medium"
                :class="choice(shown('preserved') === true)"
                :aria-pressed="shown('preserved') === true"
                @click="setField('preserved', true)"
              >
                {{ $t('Preservada') }}
              </button>
            </div>
            <p v-if="ownOf('preserved').length" class="mt-1 text-xs text-violet-800">
              {{ $t('Con preservación propia: {ids}', { ids: ownOf('preserved').join(', ') }) }}
            </p>
            <p v-if="shown('preserved') === false" class="mt-1.5 text-sm text-stone-600">
              {{ $t('Sin preservar: CAM y tubos NA, tejidos y medios NOT_COLLECTED') }}
            </p>
            <p v-else-if="shown('preserved') === undefined" class="mt-1.5 text-sm text-stone-600">
              {{ $t('Unas preservadas y otras no: elige una opción para todas las seleccionadas') }}
            </p>
            <!-- Preserved: the medium, then each butterfly's CAM and tube, right here where the eye is. -->
            <div
              v-if="showPreservation"
              ref="details"
              class="mt-2 rounded-xl border bg-white p-3 transition-shadow duration-700"
              :class="flash ? 'border-brand-600 shadow-[0_0_0_4px_var(--color-brand-100)]' : 'border-stone-200'"
            >
              <p class="text-sm text-stone-600 short:hidden">
                {{
                  $t('Cuerpo entero ({tissue}) en un tubo: confirma o escribe el CAM y el tubo de cada una.', { tissue: WHOLE })
                }}
              </p>
              <span class="field-label mt-3 short:mt-0">{{ $t('Medio') }}</span>
              <div class="grid grid-cols-3 gap-2">
                <button
                  v-for="m in mediums"
                  :key="m"
                  class="min-h-11 rounded-lg border px-1 text-sm font-medium"
                  :class="choice(medium === m)"
                  :aria-pressed="medium === m"
                  @click="medium = m"
                >
                  {{ m }}
                </button>
              </div>
              <p v-if="rack" class="mt-1 text-xs text-stone-500">
                {{ $t('Tubos de la gradilla {rack}', { rack: tx(rack.label, rack.labelMsg) }) }}
              </p>

              <div v-if="toPreserve.length" class="mt-3 flex items-baseline justify-between gap-2">
                <span class="text-sm font-semibold text-stone-700">{{ $t('CAM y tubo de cada una') }}</span>
                <span
                  class="text-xs font-semibold"
                  :class="readyCount === toPreserve.length ? 'text-brand-700' : 'text-amber-800'"
                  role="status"
                >
                  {{ $t('{ok} de {n} listas', { ok: readyCount, n: toPreserve.length }) }}
                </span>
              </div>
              <ul v-if="toPreserve.length" class="mt-1.5 divide-y divide-stone-100 rounded-lg border border-stone-200">
                <li
                  v-for="row in toPreserve"
                  :key="row.id"
                  :data-row="idOf(row)"
                  class="grid grid-cols-[minmax(3.25rem,auto)_1fr_1fr] items-start gap-2 px-2 py-2"
                  :class="hasGap(gapById.get(idOf(row))!) ? 'bg-amber-50/70' : ''"
                >
                  <span class="pt-6 text-lg leading-tight font-semibold">{{ idOf(row) }}</span>
                  <label v-if="!gapById.get(idOf(row))?.keepsCam" class="min-w-0">
                    <span class="field-label">CAM_ID</span>
                    <input
                      v-model="sampleOf(idOf(row)).cam"
                      :data-sample="`${idOf(row)}:cam`"
                      class="field-input h-11 text-base uppercase"
                      :class="{
                        'border-amber-500 bg-white ring-2 ring-amber-200': gapById.get(idOf(row))?.cam === 'missing',
                        'border-red-500 ring-2 ring-red-100': gapById.get(idOf(row))?.cam === 'repeated',
                      }"
                      :placeholder="$t('Falta')"
                      autocapitalize="characters"
                      autocomplete="off"
                      spellcheck="false"
                      enterkeyhint="next"
                      @keydown.enter.prevent="nextBox"
                    />
                    <span v-if="isSuggested(idOf(row), 'cam')" class="text-xs text-stone-500">{{ $t('siguiente libre') }}</span>
                  </label>
                  <p v-else class="min-w-0 text-sm">
                    <span class="field-label">CAM_ID</span>
                    <span class="block truncate pt-2 font-medium">{{ cellText(pending.value(row, 'CAM_ID')) }}</span>
                    <span class="text-xs text-stone-500">{{ $t('se conserva') }}</span>
                  </p>
                  <label v-if="gapById.get(idOf(row))?.slot !== null" class="min-w-0">
                    <span class="field-label">Tube_{{ gapById.get(idOf(row))?.slot }}_id</span>
                    <input
                      v-model="sampleOf(idOf(row)).tube"
                      :data-sample="`${idOf(row)}:tube`"
                      class="field-input h-11 text-base uppercase"
                      :class="{
                        'border-amber-500 bg-white ring-2 ring-amber-200': gapById.get(idOf(row))?.tube === 'missing',
                        'border-red-500 ring-2 ring-red-100': gapById.get(idOf(row))?.tube === 'repeated',
                      }"
                      :placeholder="$t('Falta')"
                      autocapitalize="characters"
                      autocomplete="off"
                      spellcheck="false"
                      enterkeyhint="next"
                      @keydown.enter.prevent="nextBox"
                    />
                    <span v-if="isSuggested(idOf(row), 'tube')" class="text-xs text-stone-500">{{ $t('siguiente libre') }}</span>
                  </label>
                  <p v-else class="min-w-0 pt-6 text-sm text-red-700">{{ $t('Sin sitio para otro tubo') }}</p>
                  <p
                    v-if="gapById.get(idOf(row))?.cam === 'repeated' || gapById.get(idOf(row))?.tube === 'repeated'"
                    class="col-span-3 text-xs text-red-700"
                  >
                    {{ gapText(gapById.get(idOf(row))!) }}
                  </p>
                </li>
              </ul>
              <p v-if="notToPreserve.length" class="mt-2 text-xs text-stone-500">
                {{ $t('Ya registradas como muertas, sin tubo aquí: {ids}', { ids: notToPreserve.map(idOf).join(', ') }) }}
              </p>
            </div>
          </div>
          <!-- A note, in English: typed or quick phrases; Save adds it, dated and signed, after the notes there. -->
          <div>
            <h2 class="mb-1.5 text-sm font-semibold text-stone-700">{{ $t('Nota') }}</h2>
            <div class="mb-2 flex flex-wrap gap-1.5">
              <button
                v-for="p in DEATH_NOTE_PHRASES"
                :key="p"
                type="button"
                class="min-h-10 rounded-full border border-stone-300 bg-white px-3 text-sm active:bg-stone-100"
                @click="addPhrase(p)"
              >
                {{ p }}
              </button>
            </div>
            <label class="block">
              <span class="sr-only">{{ $t('Nota') }}</span>
              <textarea
                :value="shownNote"
                class="field-input min-h-20 text-base short:min-h-0"
                :rows="short ? 1 : 2"
                :placeholder="
                  shown('note') === undefined
                    ? $t('Notas distintas: lo que escribas será la de todas las seleccionadas')
                    : $t('Nota, en inglés (p. ej. Only wings found)')
                "
                enterkeyhint="done"
                data-note-input
                @input="setField('note', ($event.target as HTMLTextAreaElement).value)"
              />
            </label>
            <p class="mt-1 text-xs text-stone-500">
              {{
                $t('Al guardar se añade a Notes_Insectary_data, tras las notas que ya tiene: «{prefix} …»', {
                  prefix: notePrefix,
                })
              }}
            </p>
            <p v-if="ownOf('note').length" class="mt-1 text-xs text-violet-800">
              {{ $t('Con nota propia: {ids}', { ids: ownOf('note').join(', ') }) }}
            </p>
          </div>
          <p v-if="otherPending" class="text-xs text-amber-900">
            {{
              $tn(
                otherPending,
                'Se guardará también {n} cambio pendiente de otras filas.',
                'Se guardarán también {n} cambios pendientes de otras filas.',
              )
            }}
          </p>
        </section>
        <p v-else-if="wide && canEdit" class="px-4 py-6 text-sm text-stone-500">
          {{ $t('Busca y añade mariposas: aquí eliges la fecha, la causa, si se preservan y una nota, y las guardas.') }}
        </p>
      </Teleport>

      <!-- The latest deaths, by day. -->
      <section class="px-3 pt-6 pb-8">
        <h2 class="text-sm font-semibold text-stone-700">{{ $t('Últimas muertes registradas') }}</h2>
        <p v-if="ready && !recent.length" class="py-3 text-sm text-stone-500">{{ $t('No hay muertes registradas.') }}</p>
        <template v-for="group in recentGroups" :key="group.label">
          <h3 class="mt-3 mb-1 text-xs font-semibold tracking-wide text-stone-500 uppercase">{{ group.label }}</h3>
          <ul class="divide-y divide-stone-100 overflow-hidden rounded-xl border border-stone-200 bg-white">
            <li v-for="{ row, at } in group.rows" :key="row.id">
              <button
                class="flex min-h-12 w-full items-center gap-3 px-3 py-1.5 text-left active:bg-stone-50"
                @click="openEditor('recent', at)"
              >
                <span class="w-14 shrink-0 font-semibold">{{ row.values.Insectary_ID }}</span>
                <span class="min-w-0 flex-1">
                  <span class="block truncate text-sm">{{ cellText(row.values.Death_cause) || '—' }}</span>
                  <span class="block truncate text-xs text-stone-500">
                    {{ cellText(row.values.SPECIES) }} <SexBadge :sex="cellText(row.values.Sex)" />
                  </span>
                </span>
                <span
                  v-if="sampleGap(row)"
                  class="shrink-0 rounded-md bg-amber-100 px-1.5 py-0.5 text-xs font-medium text-amber-900"
                  :title="sampleTitle(sampleGap(row)!)"
                  >{{ $t('Sin CAM/tubo') }}</span
                >
                <span v-else-if="!isBlank(row.values.CAM_ID)" class="shrink-0 text-xs text-brand-700">{{
                  row.values.CAM_ID
                }}</span>
              </button>
            </li>
          </ul>
        </template>
        <button v-if="recent.length >= recentCount" class="btn mt-3 h-11 w-full" @click="recentCount += 30">
          {{ $t('ver más') }}
        </button>
      </section>
    </div>

    <!-- The right column on a wide screen: registering, with Save at its foot. -->
    <aside
      v-show="wide"
      class="flex min-h-0 w-[min(30rem,46%)] shrink-0 flex-col border-l border-stone-200 bg-stone-50"
      :aria-label="$t('Registrar muertes')"
    >
      <div id="deaths-register" ref="aside" data-scroll class="min-h-0 flex-1 overflow-y-auto" />
      <div id="deaths-save" />
    </aside>

    <!-- Save (or the last save, with Undo): at the foot of what is visible, above the keyboard. -->
    <Teleport to="#deaths-save" defer :disabled="!wide">
      <footer
        v-if="lastSave || (canEdit && cards.length)"
        ref="footer"
        class="relative z-20 shrink-0 border-t border-stone-200 bg-white px-3 pt-2 pb-[calc(0.5rem+env(safe-area-inset-bottom))] short:pt-1.5 short:pb-1.5"
        :class="{ hidden: cramped }"
      >
        <div v-if="lastSave && !cards.length" class="flex items-center gap-2" role="status">
          <Check :size="22" class="shrink-0 text-brand-700" />
          <p class="min-w-0 flex-1 text-sm">
            <span class="font-medium">{{
              $tn(lastSave.count, '{n} muerte guardada en Google Sheets', '{n} muertes guardadas en Google Sheets')
            }}</span>
            <span class="block truncate text-xs text-stone-500">{{ lastSave.ids.join(', ') }}</span>
          </p>
          <button class="btn h-12 px-4 text-base" :disabled="undoing" @click="undo">
            <Loader2 v-if="undoing" :size="18" class="animate-spin" /><Undo2 v-else :size="18" /> {{ $t('Deshacer') }}
          </button>
          <button class="btn h-12 px-3" :aria-label="$t('Cerrar')" @click="lastSave = null"><X :size="18" /></button>
        </div>
        <!-- Upright phone: the message above a full-width button; sideways or wide: side by side. -->
        <div v-else :class="wide ? 'flex items-center gap-3' : ''">
          <button
            v-if="blocker.gap"
            class="flex min-h-11 w-full min-w-0 items-center gap-1 text-left text-sm font-medium text-amber-900 underline decoration-amber-400 underline-offset-2"
            :class="wide ? 'flex-1' : 'mb-1 short:min-h-8'"
            @click="goToGap(blocker.gap)"
          >
            <AlertTriangle :size="16" class="shrink-0" /><span class="min-w-0 truncate">{{ blocker.text }}</span>
          </button>
          <p
            v-else
            :class="[
              blocker.text ? 'text-amber-900' : 'text-stone-600',
              wide ? 'line-clamp-2 min-w-0 flex-1 text-sm' : 'mb-1.5 truncate text-xs',
            ]"
          >
            {{ blocker.text || summary }}
          </p>
          <button
            class="btn-primary h-13 text-base short:h-11"
            :class="wide ? 'shrink-0 px-6' : 'w-full'"
            :disabled="!!blocker.text || saving"
            @click="save"
          >
            <Loader2 v-if="saving" :size="18" class="animate-spin" />
            {{ saving ? $t('Guardando…') : $tn(toSave.length, 'Guardar {n} muerte', 'Guardar {n} muertes') }}
          </button>
        </div>
      </footer>
    </Teleport>

    <TabHistory v-if="showHistory" :title="$t('Historial de Muertes')" purpose="muertes" @close="showHistory = false" />
    <DeathEditor
      v-if="editing && editorRows.length"
      v-model:index="editing.index"
      :rows="editorRows"
      :columns="table?.columns || []"
      :causes="causes"
      :options="options"
      :can-edit="canEdit"
      :initials="initials"
      :card-note="editing.list === 'cards' ? cardNote : undefined"
      @note="(row: TableRow, text: string) => setField('note', text, [idOf(row)])"
      @close="editing = null"
      @more="drawerRow = $event"
    />
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
  </div>
</template>
