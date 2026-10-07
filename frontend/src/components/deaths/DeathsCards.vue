<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref, toRaw, watch } from 'vue'
import {
  AlertTriangle,
  CheckCircle2,
  ChevronDown,
  ChevronUp,
  Circle,
  Columns3,
  History,
  ListChecks,
  Loader2,
  Plus,
  Search,
  StickyNote,
  X,
} from 'lucide-vue-next'
import DateField from '../DateField.vue'
import EntryModeToggle from '../EntryModeToggle.vue'
import RowDrawer from '../RowDrawer.vue'
import IdFilters from '../IdFilters.vue'
import IdSuggestion from '../IdSuggestion.vue'
import SexBadge from '../SexBadge.vue'
import LifeBadge from './LifeBadge.vue'
import DeathsRecorded, { type RecordedItem } from './DeathsRecorded.vue'
import TabHistory from '../history/TabHistory.vue'
import UndoDialog from '../history/UndoDialog.vue'
import { useDeathsState } from '../../composables/useDeathsState'
import type { EntryMode } from '../../composables/useEntryMode'
import { useKeyboard, useMedia } from '../../composables/usePhone'
import { useUndo } from '../../composables/useUndo'
import { api } from '../../lib/api'
import { isBlank } from '../../lib/cells'
import { noteDay } from '../../lib/clutches'
import { dayLabel, formatSerial, isoToSerial, serialFromIso, serialToIso, todayIso } from '../../lib/dates'
import {
  DEATH_COLUMNS,
  DEATH_NOTE_PHRASES,
  KILLED,
  NOTES,
  WHOLE,
  bestRack,
  buildIndex,
  cardCells,
  causeForKey,
  causesByToday,
  diesBeforeEntry,
  factsOf,
  hasGap,
  hasSampleIds,
  lackOf,
  lifeOf,
  lookAlikes,
  noteCell,
  preservationGaps,
  priorDeath,
  rankCauses,
  replaceCells,
  searchKey,
  suggest,
  usedSamples,
  type ChoiceField,
  type DeathCell,
  type DeathChoice,
  type Entry,
  type Facts,
  type Lack,
  type PreservationGap,
  type RackSuggestion,
  type Suggestion,
} from '../../lib/deaths'
import {
  addCards,
  addPhraseTo,
  applyToAll,
  clickCard,
  commonChoice,
  differing,
  focusAfterPick,
  hasCard,
  namedIds,
  nextDefaults,
  readOrder,
  withPhrase,
  recordedOn,
  removeCards,
  setCardField,
  stillRecorded,
  DEFAULT_ORDER,
  type DeathCard,
  type HistoryAction,
  type RecordedOrder,
} from '../../lib/deathsCart'
import { isPattern, matchIds, type SexFilter } from '../../lib/idMatch'
import { useLookAlikes } from '../../composables/useLookAlikes'
import { idTokens, resolveIds } from '../../lib/ids'
import { errorText, notify } from '../../lib/notice'
import { persistentRef } from '../../lib/persist'
import { verificationsFor } from '../../lib/verifications'
import type { AlertsData, MissingSample } from '../../lib/review'
import { fillIfBlank, initialsOf } from '../../lib/rows'
import type { CellValue, Table, TableRow } from '../../lib/types'
import { usePending, type SaveResult } from '../../stores/pending'
import { useSession } from '../../stores/session'
import { useTables } from '../../stores/tables'
import { t, tn, tx, type Msg } from '../../lib/i18n'

/**
 * Muertes as cards, in two levels like a shopping cart. A search box finds a
 * butterfly by Insectary ID (or CAM or tube; worn wings with A?B, A[16]B and
 * look-alikes) and says at once whether it is alive; Enter or a tap puts it
 * in «Seleccionadas», with the panel's values for the next butterflies (date,
 * cause, preservation, note: choose Heat stroke once, then pick ten IDs; after
 * recording, they become that death's, so a run of the same cause needs one
 * tap). A card clicked opens in the panel, which then changes that card only
 * (clicked again it stays, and pulses); Ctrl/⌘+click and Shift+click (a long
 * press on a phone) open several, and a change goes to each; Esc, «Listo» or
 * a click on empty space go back to the values for the next ones. Each
 * card's «Añadir a muertes» (or «Añadir todas», Ctrl+Enter) records its death
 * as before (lib/deaths.ts cardCells: the NA / NOT_COLLECTED block or the
 * preserved body's CAM and tube, the note dated and signed) and saves it;
 * × takes it out, nothing saved. «Registradas hoy» lists the deaths saved
 * from Muertes today (the history), sorted to copy into the notebook: one
 * tapped opens in the panel to correct it, «Deshacer» undoes its death
 * through the history's undo. One butterfly picked alone is shown in the panel
 * at once (one at a time); «Seleccionar varias» adds each ID tapped without that.
 * On a wide screen the panel is the right column, always there; on a phone it
 * folds into one line above the cards, with the button at the foot.
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
/** A finger rather than a mouse: the keyboard closes after a pick, nothing is focused by itself. */
const touch = useMedia('(pointer: coarse)')

const { defaults, cards: scratch, selected, several, medium, samples, suggested } = useDeathsState()
const query = ref('')
const today = computed(() => isoToSerial(todayIso()))
/** Who signs the notes added here ("1/10/26 FCH: …"), as in Clutches. */
const initials = computed(() => initialsOf(session.user?.displayName || '', props.collectors, session.user?.username || ''))
const notePrefix = computed(() => `${noteDay(today.value)} ${initials.value}:`)
const canEdit = computed(() => session.canEdit)

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
const sameId = (a: string, b: string) => searchKey(a) === searchKey(b)
const rowById = computed(() => new Map((props.table?.rows || []).map(r => [r.id, r])))
const cellText = (v: CellValue) => (isBlank(v) ? '' : String(v))

// --- «Seleccionadas»: each butterfly with its own death values
/** The cards with their rows, newest first: the one just picked shows right under the search box. */
const cardRows = computed(() => {
  const out: TableRow[] = []
  for (const card of scratch.value) {
    const row = byKey.value.get(searchKey(card.id))?.row
    if (row) out.push(row)
  }
  return out.reverse()
})
const choices = computed(() => new Map(scratch.value.map(c => [searchKey(c.id), c.choice])))
const choiceOf = (row: TableRow): DeathChoice => choices.value.get(searchKey(idOf(row))) ?? defaults.value
// IDs no longer in the sheet (renamed, a row deleted) leave the list once it has loaded.
watch(
  () => props.ready && index.value.length > 0,
  loaded => {
    if (!loaded) return
    const gone = scratch.value.filter(c => !byKey.value.has(searchKey(c.id))).map(c => c.id)
    if (!gone.length) return
    scratch.value = removeCards(scratch.value, gone)
    notify(t('No encontrado: {ids}', { ids: gone.join(', ') }), 'info')
  },
  { immediate: true },
)

/** The cards the panel shows (one, or several: a change goes to each), in the cards' order; none: the values for the next butterflies. */
const selectedRows = computed(() => cardRows.value.filter(r => selected.value.some(id => sameId(id, idOf(r)))))
const selectedIds = computed(() => selectedRows.value.map(idOf))
/** The one card open, when only one is. */
const focusRow = computed(() => (selectedRows.value.length === 1 ? selectedRows.value[0] : null))
const multi = computed(() => selectedRows.value.length > 1)
const isSelected = (row: TableRow) => selected.value.some(id => sameId(id, idOf(row)))
watch(scratch, list => {
  const kept = selected.value.filter(id => hasCard(list, id))
  if (kept.length !== selected.value.length) selected.value = kept
})
/** A death of «Registradas hoy» open in the panel to correct it (its row id). */
const editingId = ref<string | null>(null)
const editingRow = computed(() => (editingId.value ? (rowById.value.get(editingId.value) ?? null) : null))
const panelMode = computed<'defaults' | 'card' | 'recorded'>(() =>
  editingRow.value ? 'recorded' : selectedRows.value.length ? 'card' : 'defaults',
)

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
  const out: Suggestion[] = matchIds(index.value, q, {
    alive: isAlive,
    speciesOf: e => pending.value(e.row, 'SPECIES'),
    sexOf: e => pending.value(e.row, 'Sex'),
    species: speciesFilter.value,
    sex: sexFilter.value,
    table: lookAlikeTable.value,
  }).map(m => ({ entry: m.item, match: { at: m.at, greyed: !m.alive || !m.sameSpecies } }))
  if (!isPattern(q))
    for (const s of suggest(index.value, q, { alive: isAlive })) {
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

const isPicked = (id: string) => hasCard(scratch.value, id)
/**
 * IDs into «Seleccionadas», each with the panel's values; the panel then shows
 * the one picked alone (one at a time), else the values for the next ones.
 */
function pick(ids: string[], add = false) {
  if (!ids.length) return
  const before = scratch.value
  const after = addCards(before, ids, defaults.value).cards
  scratch.value = after
  const open = focusAfterPick(before, after, ids, several.value || add)
  selected.value = open ? [open] : []
  anchor.value = open
  if (open) editingId.value = null
  cleared.value = null
}
/** One ID chosen (a suggestion, Enter, «¿Quisiste decir?»); in several a tap on one there takes it out. */
function choose(id: string, add = false, tapped = false) {
  if (tapped && several.value && isPicked(id)) remove(id)
  else pick([id], add)
  query.value = ''
  missing.value = []
  // On a phone the keyboard closes on the butterfly and its death; in several it stays for the next ID.
  if (touch.value && !several.value && !add) searchInput.value?.blur()
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
/** Several IDs or a range: all into «Seleccionadas». */
function addTokens(tokens: string[]) {
  const { found, missing: none } = resolveIds(
    tokens,
    index.value.map(e => e.id),
  )
  pick(found)
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
/** A suggestion: Ctrl, Cmd or Shift adds it beside the others; so does a long press on a touch screen. */
let pressTimer: ReturnType<typeof setTimeout> | undefined
let longPressed = false
function pressStart(event: PointerEvent, id: string) {
  longPressed = false
  clearTimeout(pressTimer)
  if (event.pointerType !== 'touch') return
  pressTimer = setTimeout(() => {
    longPressed = true
    navigator.vibrate?.(30)
    choose(id, true)
  }, 500)
}
const pressEnd = () => clearTimeout(pressTimer)
function tapSuggestion(event: MouseEvent, id: string) {
  if (longPressed) {
    longPressed = false
    return
  }
  choose(id, event.ctrlKey || event.metaKey || event.shiftKey, true)
}
onBeforeUnmount(pressEnd)
function searchKeys(event: KeyboardEvent) {
  if (event.key === 'Escape' && query.value) {
    event.stopPropagation()
    query.value = ''
    missing.value = []
  }
}

/** A card's ×: it leaves «Seleccionadas», with what was chosen for it (nothing was saved). */
function remove(id: string) {
  scratch.value = removeCards(scratch.value, [id])
  delete samples[id]
  delete suggested[id]
  delete refusals.value[id]
}
/** «Vaciar»: every card out, to bring back with «Deshacer». */
const cleared = ref<DeathCard[] | null>(null)
function clearAll() {
  cleared.value = scratch.value
  scratch.value = []
  selected.value = []
  selecting.value = false
}
function undoClear() {
  if (!cleared.value) return
  scratch.value = addCards(cleared.value, scratch.value.map(c => c.id), defaults.value).cards
  cleared.value = null
}
/** «Seleccionar varias»: each ID tapped joins (or leaves) without opening it; the search is ready for the next. */
function enterSeveral() {
  several.value = true
  selected.value = []
  selecting.value = false
  cleared.value = null
  searchInput.value?.focus()
}
const leaveSeveral = () => (several.value = false)

// --- Cards open in the panel: a click opens one, Ctrl/⌘ adds or takes one out, Shift a run; on a phone a long press
/** The card a Shift+click counts from (the last one clicked). */
const anchor = ref<string | null>(null)
/** Choosing cards on a touch screen (after a long press on one): each tap adds or takes one out. */
const selecting = ref(false)
/** The card clicked while already open: it pulses for a moment, to show it is the one in the panel. */
const pulsing = ref<string | null>(null)
let pulseTimer: ReturnType<typeof setTimeout> | undefined
function pulse(id: string) {
  clearTimeout(pulseTimer)
  pulsing.value = null
  // A frame without the class, so a second click plays it again.
  requestAnimationFrame(() => {
    pulsing.value = id
    pulseTimer = setTimeout(() => (pulsing.value = null), 700)
  })
}
onBeforeUnmount(() => clearTimeout(pulseTimer))
function clickRow(event: MouseEvent, row: TableRow) {
  if (cardLongPressed) {
    cardLongPressed = false
    return
  }
  editingId.value = null
  const how = selecting.value ? { toggle: true } : { toggle: event.ctrlKey || event.metaKey, range: event.shiftKey }
  const next = clickCard({ ids: selectedIds.value, anchor: anchor.value }, cardRows.value.map(idOf), idOf(row), how)
  selected.value = next.ids
  anchor.value = next.anchor
  if (next.again) pulse(idOf(row))
  if (selecting.value) {
    if (!next.ids.length) selecting.value = false
  } else if (next.ids.length) revealPanel()
}
/** Shift+click chooses cards, not text. */
const noTextSelect = (event: MouseEvent) => event.shiftKey && event.preventDefault()
let cardTimer: ReturnType<typeof setTimeout> | undefined
let cardLongPressed = false
function cardPressStart(event: PointerEvent, row: TableRow) {
  cardLongPressed = false
  clearTimeout(cardTimer)
  if (event.pointerType !== 'touch' || selecting.value) return
  cardTimer = setTimeout(() => {
    cardLongPressed = true
    navigator.vibrate?.(30)
    selecting.value = true
    several.value = false
    editingId.value = null
    if (!isSelected(row)) {
      const next = clickCard({ ids: selectedIds.value, anchor: anchor.value }, cardRows.value.map(idOf), idOf(row), { toggle: true })
      selected.value = next.ids
      anchor.value = next.anchor
    }
  }, 500)
}
const cardPressEnd = () => clearTimeout(cardTimer)
onBeforeUnmount(cardPressEnd)
/** «Listo» on the bar: done choosing; the panel (above the cards) shows them. */
function doneSelecting() {
  selecting.value = false
  if (selectedRows.value.length) revealPanel()
}
/** Back to the values for the next butterflies. */
function closePanel() {
  selected.value = []
  selecting.value = false
  editingId.value = null
}
/** A click on empty space among the cards (not on a card, a button or a box): back to the values for the next ones. */
function onEmptyClick(event: MouseEvent) {
  if (panelMode.value === 'defaults' && !selecting.value) return
  // The path as dispatched: a tap on a card may re-render what was tapped (its check icon) before the click gets here.
  const controls = 'button, a, input, textarea, select, label, [role="listbox"], [data-card], [data-panel], [data-panel-folded], [data-search-bar]'
  if (event.composedPath().some(el => el instanceof Element && el.matches(controls))) return
  // Text being selected (to copy an ID) is not a click on nothing.
  if (window.getSelection()?.toString()) return
  closePanel()
}

// --- The panel: the values for the next butterflies, one card's, or a recorded death being corrected
/**
 * The cause buttons: the sheet's own dropdown list for Death_cause (its data
 * validation), most used first; until it arrives, the values the column uses.
 */
const causes = computed(() => {
  const list = verificationsFor(MODULE)?.lists.Death_cause?.values
  const values = list?.size ? [...list] : (props.options.Death_cause || []).filter(v => !/^\d+$/.test(v))
  return rankCauses(values, props.table?.rows || [], today.value)
})
/** The causes of today's deaths («Registradas hoy», everyone's), how many each. */
const todayCauses = computed(() => {
  const out = new Map<string, number>()
  for (const item of allRecorded.value) {
    const cause = cellText(pending.value(item.row, 'Death_cause'))
    if (cause) out.set(cause, (out.get(cause) ?? 0) + 1)
  }
  return out
})
/** The buttons: today's causes first, most first, with their count («Heat stroke ×5»); then the rest as before. Keys 1–9 pick them. */
const causeButtons = computed(() => causesByToday(causes.value, todayCauses.value))
const quickDates = computed(() => [
  { iso: todayIso(), name: t('Hoy') },
  { iso: serialToIso(today.value - 1), name: t('Ayer') },
])
/** A recorded death being corrected: its date and cause as the sheet has them, and a note to add. */
const draft = ref<DeathChoice>({ date: '', cause: '', preserved: false, note: '' })
/** The cards open: the values they share, and the fields where they differ («varios», shown empty). */
const common = computed(() => commonChoice(scratch.value, selectedIds.value))
const shown = computed<DeathChoice>(() =>
  panelMode.value === 'recorded' ? draft.value : panelMode.value === 'card' ? common.value.choice : defaults.value,
)
const isMixed = (field: ChoiceField) => panelMode.value === 'card' && common.value.mixed.includes(field)
const mixedChip = 'ml-1 rounded-md bg-amber-100 px-1.5 py-0.5 text-xs font-medium text-amber-900'
/** «Muerte de G7D, G8D (2)», «Muerte de G7D, G8D y 3 más (5)». */
const selectionTitle = computed(() => {
  const { shown: ids, more } = namedIds(selectedIds.value)
  const vars = { ids: ids.join(', '), more, n: selectedIds.value.length }
  return more ? t('Muerte de {ids} y {more} más ({n})', vars) : t('Muerte de {ids} ({n})', vars)
})
const shownDate = computed(() => shown.value.date)
const dateError = computed(() =>
  shownDate.value && serialFromIso(shownDate.value) === null ? t('Fecha no válida: el año debe estar entre 1990 y 2099') : '',
)
/** A value chosen in the panel: the card's, the death's being corrected, or the next butterflies'. */
function setField<F extends ChoiceField>(field: F, value: DeathChoice[F]) {
  if (panelMode.value === 'recorded') draft.value = { ...draft.value, [field]: value }
  else if (panelMode.value === 'card') scratch.value = setCardField(scratch.value, selectedIds.value, field, value)
  else defaults.value = { ...defaults.value, [field]: value }
}
function pickCause(c: string) {
  setField('cause', c)
  // Killed to be preserved: the body goes in a tube.
  if (c === KILLED && panelMode.value !== 'recorded') setField('preserved', true)
}
/** The values for the next ones, while cards have another: «Aplicar también a las N seleccionadas». */
const spread = (field: ChoiceField) => (panelMode.value === 'defaults' ? differing(scratch.value, defaults.value, field) : [])
function spreadField(field: ChoiceField) {
  scratch.value = applyToAll(scratch.value, field, defaults.value[field])
}
const mediums = ['Flash frozen', 'Ethanol', 'DMSO']
/** A quick phrase goes after what is typed ("Head eaten; With fungi"): each card open, after its own note. */
function addPhrase(p: string) {
  if (panelMode.value === 'card') scratch.value = addPhraseTo(scratch.value, selectedIds.value, p)
  else setField('note', withPhrase(shown.value.note, p))
}
/** The note a card adds, as it will be written ("1/10/26 FCH: Only wings found"). */
function noteOf(row: TableRow) {
  const text = choiceOf(row).note.trim()
  return text ? `${notePrefix.value} ${text}` : ''
}
const panelSummary = computed(() => {
  const c = defaults.value
  return [
    c.date ? dayLabel(c.date).split(' · ')[0] : t('sin fecha'),
    c.cause || t('sin causa'),
    c.preserved ? t('Preservada') : t('Sin preservar'),
    c.note.trim() ? t('con nota') : '',
  ]
    .filter(Boolean)
    .join(' · ')
})
/** On a phone the values for the next ones fold into one line («Cambiar»); a card or a death opened unfolds them. */
const panelOpen = persistentRef('deaths:panel-open', false)
const unfolded = computed(() => wide.value || panelOpen.value || panelMode.value !== 'defaults')
const panel = ref<HTMLElement>()
function revealPanel() {
  if (wide.value) return nextTick(() => aside.value?.scrollTo({ top: 0, behavior: 'smooth' }))
  nextTick(() => panel.value?.scrollIntoView({ block: 'start', behavior: 'smooth' }))
}

// --- Preserved: each card's CAM and tube
/** Rows dying now (no death date yet). */
const dying = (row: TableRow) => lifeOf(get(row)).state !== 'dead'
/** The death the sheet already has for a butterfly (a misread ID, found dead again): its card replaces it, explicitly. */
const priorOf = (row: TableRow) => priorDeath(f => row.values[f] ?? null)
const replacing = (row: TableRow) => priorOf(row) !== null
/** Preserved, each gets its CAM and tube: a death now, or one replacing a death whose row holds no CAM or tube ID yet. */
const canPreserve = (row: TableRow) => (replacing(row) ? !hasSampleIds(f => pending.value(row, f)) : dying(row))
const toPreserve = computed(() => cardRows.value.filter(r => choiceOf(r).preserved && canPreserve(r)))
/** Already preserved before: "preserved" does not give them a tube here (Tubos does). */
const notToPreserve = computed(() => cardRows.value.filter(r => choiceOf(r).preserved && !canPreserve(r)))
/** The CAM and tube boxes in the panel: the card's, or every card's being preserved (for «Añadir todas»). */
const panelPreserve = computed(() =>
  panelMode.value === 'card' ? toPreserve.value.filter(isSelected) : panelMode.value === 'defaults' ? toPreserve.value : [],
)
const panelNotPreserve = computed(() =>
  panelMode.value === 'card' ? notToPreserve.value.filter(isSelected) : panelMode.value === 'defaults' ? notToPreserve.value : [],
)
const showPreservation = computed(() => panelMode.value !== 'recorded' && (shown.value.preserved || panelPreserve.value.length > 0))
const sampleOf = (id: string) => (samples[id] ??= { cam: '', tube: '' })

// The next free CAM IDs and tubes (server/grid.mjs idSuggestions), asked for only when preserving.
const camRun = ref<string[]>([])
const tubeRun = ref<string[]>([])
const racks = ref<(RackSuggestion & { label: string; labelMsg?: Msg })[]>([])
const rack = computed(() => bestRack(racks.value, cardRows.value, medium.value))
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
/** The CAMs and tubes already in the sheet: one typed again is flagged before recording (the server would refuse it). */
const used = computed(() => usedSamples(index.value))
/** What each butterfly being preserved still lacks (CAM, tube, a free slot), shown on its card and in the panel. */
const gaps = computed(() => preservationGaps(toPreserve.value, pending.value, samples, used.value))
const gapById = computed(() => new Map(gaps.value.map(g => [g.id, g])))
const isSuggested = (id: string, kind: 'cam' | 'tube') => !!samples[id]?.[kind] && samples[id]?.[kind] === suggested[id]?.[kind]
const panelReady = computed(() => panelPreserve.value.filter(r => !hasGap(gapById.value.get(idOf(r))!)).length)
function gapText(g: PreservationGap) {
  if (g.slot === null) return t('{id} no tiene sitio para otro tubo', { id: g.id })
  if (g.cam === 'missing') return t('Falta el CAM de {id}', { id: g.id })
  if (g.tube === 'missing') return t('Falta el tubo de {id}', { id: g.id })
  return t('{value} está en {a} y en {b}', { value: g.value ?? '', a: g.with ?? '', b: g.id })
}

// --- What recording each card writes, and what it still lacks
/** The cells recording each card writes (the same as the table's «Escribir fecha y causa», plus CAM, tube and note). */
const plans = computed(() => {
  const out = new Map<string, DeathCell[]>()
  for (const row of cardRows.value) {
    const how = { sample: samples[idOf(row)], medium: medium.value, today: today.value, initials: initials.value }
    out.set(row.id, (replacing(row) ? replaceCells : cardCells)(row, pending.value, choiceOf(row), how))
  }
  return out
})
type CardLack = Lack | 'nothing'
/** Why a card cannot be recorded yet ('' when it can); 'nothing': already dead and nothing new to write. */
const lackFor = (row: TableRow): CardLack =>
  lackOf(choiceOf(row), dying(row) || replacing(row), gapById.value.get(idOf(row))) ||
  ((plans.value.get(row.id) || []).length ? '' : 'nothing')
/** «Ya muerta: 3-Sep-26, Unknown»: the death in the sheet its card would replace. */
function priorText(row: TableRow) {
  const prior = priorOf(row)
  if (!prior) return ''
  const what = [prior.date !== null ? formatSerial(prior.date) : '', prior.cause].filter(Boolean).join(', ')
  return t('Ya muerta: {what}', { what })
}
/** The death date chosen is before it entered the insectary: a warning, not a block. */
const beforeEntry = (row: TableRow) => diesBeforeEntry(choiceOf(row).date, factsFor(row).entered)
/** The cards open in the panel whose death date is before their entry. */
const panelBeforeEntry = computed(() => (panelMode.value === 'card' ? selectedRows.value.filter(beforeEntry) : []))
const beforeEntryText = (row: TableRow) =>
  t('La fecha de muerte es anterior a su entrada al insectario ({date})', { date: formatSerial(factsFor(row).entered ?? NaN) })
const lackText = (lack: CardLack, row?: TableRow) => {
  if (lack === 'sample' && row) {
    const g = gapById.value.get(idOf(row))
    if (g) return gapText(g)
  }
  return {
    '': '',
    date: t('Elige la fecha de muerte'),
    'bad-date': t('Fecha no válida: el año debe estar entre 1990 y 2099'),
    cause: t('Elige la causa'),
    sample: t('falta el CAM o el tubo'),
    nothing: t('Ya registrada: no se cambiará'),
  }[lack]
}
/** What «Añadir todas» records: the cards ready, but not those replacing a death (each one only from its own button). */
const readyRows = computed(() => cardRows.value.filter(r => !lackFor(r) && !replacing(r)))
const skippedRows = computed(() => cardRows.value.filter(replacing))
const skippedText = (n: number) =>
  n ? tn(n, '{n} ya muerta se omite: reemplázala desde su tarjeta', '{n} ya muertas se omiten: reemplázalas desde su tarjeta') : ''
/** A card's date, cause and preservation, as chips. */
function chipsOf(row: TableRow) {
  const c = choiceOf(row)
  const s = samples[idOf(row)]
  const preserving = c.preserved && canPreserve(row)
  return [
    {
      field: 'date',
      text: c.date ? (serialFromIso(c.date) !== null ? formatSerial(isoToSerial(c.date)) : c.date) : t('sin fecha'),
      missing: !c.date,
    },
    { field: 'cause', text: c.cause || t('sin causa'), missing: !c.cause && (dying(row) || replacing(row)) },
    {
      field: 'preserved',
      text: preserving
        ? [t('Preservada'), s?.cam.trim().toUpperCase(), s?.tube.trim().toUpperCase()].filter(Boolean).join(' · ')
        : c.preserved
          ? t('Preservada')
          : t('Sin preservar'),
      missing: false,
    },
  ]
}
/** Pending changes of other rows, which go to the sheet in the same save. */
const otherPending = computed(() => {
  const mine = new Set(cardRows.value.map(r => r.id))
  let n = pending.creates.length
  for (const e of Object.values(pending.edits)) if (!mine.has(e.id)) n += Object.keys(e.values).length
  return n
})

// --- Recording: «Añadir a muertes», «Añadir todas»
const saving = ref(false)
/** The cards being recorded now (their buttons turn). */
const recording = ref<string[]>([])
/** Why the sheet did not take a card's death, by Insectary ID: shown on the card, which stays to be corrected. */
const refusals = ref<Record<string, string>>({})
const waitIdle = async () => {
  for (let i = 0; i < 300 && pending.saving; i++) await new Promise(r => setTimeout(r, 100))
}
const issueOf = (row: TableRow) => Object.entries(pending.issues).find(([k]) => k.startsWith(`${row.id}:`))?.[1]
/**
 * Writes each card's cells and saves them, as Muertes always saved a death
 * (the pending changes' save: the same history, the outbox when Google is
 * busy). Those saved leave «Seleccionadas» for «Registradas hoy»; one the
 * sheet refuses gets its cells back and stays, with the reason. A save whose
 * outcome is unclear leaves its cells among the changes to save («Por guardar»).
 */
async function record(rows: TableRow[], skipped = 0) {
  const go = rows.filter(r => !lackFor(r))
  if (!go.length || saving.value) return
  saving.value = true
  recording.value = go.map(idOf)
  // Each one's death as recorded: the last one's becomes the values for the next butterflies.
  const deaths = new Map(go.map(r => [r.id, { ...choiceOf(r) }]))
  const written = new Map<string, DeathCell[]>()
  try {
    for (const row of go) {
      const cells = plans.value.get(row.id) || []
      for (const c of cells) fillIfBlank(MODULE, row, idOf(row), c.field, c.value, c.overwrite)
      written.set(row.id, cells)
      delete refusals.value[idOf(row)]
    }
    pending.touch()
    // An automatic save already running would leave these cells for later.
    await waitIdle()
    let result: SaveResult | null = null
    let failure: unknown = null
    try {
      result = await pending.save('')
    } catch (e) {
      failure = e
    }
    const refused = go.filter(r => issueOf(r) !== undefined)
    for (const row of refused) {
      refusals.value = { ...refusals.value, [idOf(row)]: issueOf(row) ?? '' }
      for (const c of written.get(row.id) || [])
        if (pending.isDirty(row.id, c.field) && pending.value(row, c.field) === c.value)
          pending.setCell(MODULE, row, idOf(row), c.field, row.values[c.field] ?? null)
    }
    const doneRows = go.filter(r => !refused.includes(r))
    const done = doneRows.map(idOf)
    // A run of the same death needs one tap: the next ones start with it (the last card's, if they differed).
    defaults.value = nextDefaults(
      doneRows.map(r => deaths.get(r.id)!),
      defaults.value,
    )
    scratch.value = removeCards(scratch.value, done)
    for (const id of done) {
      delete samples[id]
      delete suggested[id]
    }
    if (refused.length)
      notify(t('No se guardó {ids}: {reason}', { ids: refused.map(idOf).join(', '), reason: issueOf(refused[0]) ?? refusals.value[idOf(refused[0])] }), 'error')
    else if (failure) notify(errorText(failure), 'error')
    else if (done.length && result?.queued)
      notify(tn(done.length, '{n} muerte esperando a Google Sheets: se escribe cuando responda', '{n} muertes esperando a Google Sheets: se escriben cuando responda'))
    else if (done.length)
      notify(tn(done.length, '{n} muerte guardada en Google Sheets', '{n} muertes guardadas en Google Sheets') + ` · ${[done.join(', '), skippedText(skipped)].filter(Boolean).join(' · ')}`, 'success')
    loadRecorded()
    if (!touch.value) searchInput.value?.focus()
  } finally {
    saving.value = false
    recording.value = []
  }
}

// --- «Registradas hoy»: today's saves of Muertes, from the history
const actions = ref<HistoryAction[]>([])
const loadingRecorded = ref(false)
let asked = 0
async function loadRecorded() {
  const ask = ++asked
  loadingRecorded.value = true
  try {
    // A day around today in UTC; recordedOn keeps today's in Ecuador.
    const day = today.value
    const q = new URLSearchParams({ purpose: 'muertes', from: serialToIso(day - 1), to: serialToIso(day + 1), limit: '500' })
    const data = await api<{ actions: HistoryAction[] }>(`history?${q}`)
    if (ask === asked) actions.value = data.actions
  } catch {
    /* The list stays as it was until the next look. */
  } finally {
    if (ask === asked) loadingRecorded.value = false
  }
}
onMounted(loadRecorded)
// Someone else's save (the sheet changes): the list follows a moment later.
let reloadTimer: ReturnType<typeof setTimeout> | undefined
watch(
  () => tables.versions[MODULE],
  () => {
    clearTimeout(reloadTimer)
    reloadTimer = setTimeout(loadRecorded, 1500)
  },
)
onBeforeUnmount(() => clearTimeout(reloadTimer))
const mineOnly = persistentRef('deaths:recorded-mine', false, { lasting: true })
const storedOrder = persistentRef<RecordedOrder>('deaths:recorded-order', DEFAULT_ORDER, { lasting: true })
const order = computed<RecordedOrder>({ get: () => readOrder(storedOrder.value), set: v => (storedOrder.value = v) })
/** Today's deaths, everyone's («Mías» filters them below; the cause buttons count them all). */
const allRecorded = computed<(RecordedItem & { mine: boolean })[]>(() => {
  const me = session.user?.id
  const out: (RecordedItem & { mine: boolean })[] = []
  const seen = new Set<string>()
  // Not in Google Sheets yet (refused, waiting for Google, or typed in the table): first.
  for (const e of Object.values(pending.edits)) {
    if (e.module !== MODULE || !('Death_date' in e.values || 'Death_cause' in e.values)) continue
    const row = rowById.value.get(e.id)
    if (!row) continue
    seen.add(row.id)
    const queued = pending.isQueued(row.id, 'Death_date') || pending.isQueued(row.id, 'Death_cause')
    out.push({ row, status: queued ? 'queued' : 'pending', others: [], changeIds: [], mine: true })
  }
  for (const r of recordedOn(actions.value, todayIso())) {
    if (seen.has(r.recordId)) continue
    const row = rowById.value.get(r.recordId)
    // Undone since (alive again, or the death it replaced back): no longer listed.
    if (!row || lifeOf(get(row)).state !== 'dead' || !stillRecorded(r, get(row))) continue
    const mine = r.actors.some(a => a.id === me)
    out.push({ row, status: 'saved', others: r.actors.filter(a => a.id !== me).map(a => a.name), changeIds: r.changeIds, mine })
  }
  return out
})
const recorded = computed<RecordedItem[]>(() => allRecorded.value.filter(item => !mineOnly.value || item.mine))

/** A recorded death opened in the panel: its date and cause as they are, and a note to add. */
function startEdit(row: TableRow) {
  selected.value = []
  selecting.value = false
  if (editingId.value === row.id) return closePanel()
  editingId.value = row.id
  const death = pending.value(row, 'Death_date')
  draft.value = {
    date: typeof death === 'number' ? serialToIso(death) : '',
    cause: cellText(pending.value(row, 'Death_cause')),
    preserved: preservedNow(row),
    note: '',
  }
  revealPanel()
}
/** Its CAM or first tube holds an ID: preserved. */
const preservedNow = (row: TableRow) =>
  ['CAM_ID', 'Tube_1_id'].some(f => {
    const v = cellText(pending.value(row, f))
    return v && v !== 'NA'
  })
const preservationLine = (row: TableRow) =>
  preservedNow(row)
    ? [t('Preservada'), cellText(pending.value(row, 'CAM_ID')), cellText(pending.value(row, 'Tube_1_id'))].filter(v => v && v !== 'NA').join(' · ')
    : t('Sin preservar')
/** What correcting it writes: a new date or cause, and the note after the notes there. */
const draftCells = computed<DeathCell[]>(() => {
  const row = editingRow.value
  if (!row) return []
  const out: DeathCell[] = []
  const serial = draft.value.date ? serialFromIso(draft.value.date) : null
  if (serial !== null && serial !== pending.value(row, 'Death_date')) out.push({ field: 'Death_date', value: serial })
  if (draft.value.cause && draft.value.cause !== cellText(pending.value(row, 'Death_cause')))
    out.push({ field: 'Death_cause', value: draft.value.cause })
  const note = noteCell(row, pending.value, draft.value.note, today.value, initials.value)
  if (note) out.push(note)
  return out.filter(c => !row.formulas.includes(c.field))
})
async function saveEdit() {
  const row = editingRow.value
  if (!row || !draftCells.value.length || saving.value || dateError.value) return
  saving.value = true
  const id = idOf(row)
  try {
    for (const c of draftCells.value) pending.setCell(MODULE, row, id, c.field, c.value)
    pending.touch()
    await waitIdle()
    const result = await pending.save('')
    const issue = issueOf(row)
    if (issue !== undefined) notify(t('No se guardó {ids}: {reason}', { ids: id, reason: issue }), 'error')
    else {
      notify(result.queued ? t('Esperando a Google Sheets: se escribe cuando responda') : t('Muerte de {id} corregida', { id }), 'success')
      editingId.value = null
    }
    loadRecorded()
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    saving.value = false
  }
}

// «Deshacer muerte»: a saved one through the history's undo (preview, then confirm); one not saved yet, its cells dropped.
const { undoing, reason: undoReason, busy: undoBusy, review, cancel: cancelUndo, confirm: confirmUndo } = useUndo(() => loadRecorded())
/** The short question the undo asks here: which death, and that the butterfly is alive again. */
const undoSummary = ref('')
function undoDeath(item: RecordedItem) {
  const id = idOf(item.row)
  const when = item.row.values.Death_date
  undoSummary.value = t('¿Deshacer la muerte de {id}{what}? Vuelve a estar viva en la hoja.', {
    id,
    what: [typeof when === 'number' ? formatSerial(when) : '', item.row.values.Death_cause ?? ''].filter(Boolean).length
      ? ` (${[typeof when === 'number' ? formatSerial(when) : '', item.row.values.Death_cause ?? ''].filter(Boolean).join(', ')})`
      : '',
  })
  if (editingId.value === item.row.id) editingId.value = null
  if (item.status === 'queued') return
  if (item.status === 'saved') return review({ changeIds: item.changeIds }, t('Deshacer la muerte de {id}', { id }))
  if (!window.confirm(t('¿Descartar la muerte de {id}, aún sin guardar?', { id }))) return
  for (const field of Object.keys(pending.edits[item.row.id]?.values ?? {}))
    if (DEATH_COLUMNS.includes(field) || field === NOTES) pending.setCell(MODULE, item.row, id, field, item.row.values[field] ?? null)
  pending.touch()
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
const sampleTitle = () => `${t('Preservada sin CAM o tubo')} · ${t('pregunta al equipo')}`
const gapChip = (row: TableRow) => (sampleGap(row) ? t('Sin CAM/tubo') : '')

// --- The button at the foot of the panel, and Ctrl+Enter
const footer = computed(() => {
  if (panelMode.value === 'recorded') {
    const row = editingRow.value!
    return {
      text: dateError.value || (draftCells.value.length ? '' : t('Cambia la fecha, la causa o escribe una nota')),
      gap: '',
      label: t('Guardar el cambio de {id}', { id: idOf(row) }),
      disabled: !!dateError.value || !draftCells.value.length,
      run: saveEdit,
    }
  }
  if (panelMode.value === 'card' && focusRow.value) {
    const row = focusRow.value
    const lack = lackFor(row)
    return {
      text: lackText(lack, row),
      gap: lack === 'sample' ? idOf(row) : '',
      label: replacing(row) ? t('Reemplazar la muerte anterior de {id}', { id: idOf(row) }) : t('Añadir {id} a muertes', { id: idOf(row) }),
      disabled: !!lack,
      run: () => record([row]),
    }
  }
  // The cards open in the panel, or all of them; those replacing a death only from their own button.
  const rows = panelMode.value === 'card' ? selectedRows.value : cardRows.value
  if (!rows.length) return null
  const skipped = rows.filter(replacing)
  const ready = rows.filter(r => !lackFor(r) && !replacing(r))
  const waiting = rows.filter(r => lackFor(r) && !replacing(r))
  const first = waiting[0]
  const lack = first ? lackFor(first) : ''
  return {
    text: [
      first
        ? tn(waiting.length, '{n} sin terminar: {why}', '{n} sin terminar: {why}', {
            why: `${idOf(first)}, ${lackText(lack, first).replace(/^./, c => c.toLowerCase())}`,
          })
        : '',
      skippedText(skipped.length),
    ]
      .filter(Boolean)
      .join(' · '),
    gap: lack === 'sample' ? idOf(first!) : '',
    label: ready.length ? tn(ready.length, 'Añadir {n} a muertes', 'Añadir las {n} a muertes') : t('Añadir a muertes'),
    disabled: !ready.length,
    run: () => record(ready, skipped.length),
  }
})
/** A box one types in has the keys (the search, the note, a date, a CAM). */
const typing = (target: EventTarget | null) =>
  target instanceof HTMLElement && (target.isContentEditable || !!target.closest('input, textarea, select, [contenteditable]'))
function onKey(e: KeyboardEvent) {
  if (undoing.value || showHistory.value || drawerRow.value) return
  if (e.key === 'Enter' && (e.ctrlKey || e.metaKey)) {
    e.preventDefault()
    if (footer.value && !footer.value.disabled && !saving.value) footer.value.run()
  } else if (e.key === 'Escape' && (panelMode.value !== 'defaults' || selecting.value)) closePanel()
  else if (!e.ctrlKey && !e.metaKey && !e.altKey && canEdit.value && !typing(e.target)) {
    // 1–9: the cause in that place, for the card(s) open or the next butterflies.
    const cause = causeForKey(e.key, causeButtons.value.map(b => b.cause))
    if (!cause || (!wide.value && !unfolded.value)) return
    e.preventDefault()
    pickCause(cause)
  }
}
onMounted(() => window.addEventListener('keydown', onKey))
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
const macKeys = !!navigator.platform?.startsWith('Mac')
const shortcut = computed(() => (touch.value ? '' : macKeys ? '⌘+Enter' : 'Ctrl+Enter'))

const drawerRow = ref<TableRow | null>(null)
const showHistory = ref(false)

// --- The keyboard: the screen fits above it, and the box being typed in stays in view
const root = ref<HTMLElement>()
/** The page's scroller (one column), or the left column's (search, cards, recorded deaths). */
const scroller = ref<HTMLElement>()
/** The right column's scroller on a wide screen (the panel). */
const aside = ref<HTMLElement>()
const footerEl = ref<HTMLElement>()
const searchBar = ref<HTMLElement>()
const details = ref<HTMLElement>()
const rootBottom = ref(0)
const footerHeight = ref(0)
const measure = () => {
  rootBottom.value = root.value?.getBoundingClientRect().bottom ?? 0
  footerHeight.value = footerEl.value?.offsetHeight ?? 0
}
/**
 * With the keyboard open the screen is pinned to what is visible (Chrome only
 * shrinks the visible area and may pan the page, hiding the search bar).
 */
const fitted = computed(() => keyboard.open.value)
/**
 * Too little left to show the box being typed and the button together (Gboard
 * sideways can leave 50 px until its suggestion strip appears): the box wins,
 * the button comes back with more room or once the keyboard closes.
 */
const cramped = computed(() => fitted.value && keyboard.visibleBottom.value - keyboard.visibleTop.value < 140)
/** The column the button is in: on a wide screen the right one. */
const saveColumn = computed(() => (wide.value ? aside.value : scroller.value))
/** The part of a column one can see: under its sticky bar and the top of the screen, above the keyboard and the button. */
function visibleBand(box: HTMLElement) {
  const sticky = box === scroller.value ? (searchBar.value?.getBoundingClientRect().bottom ?? 0) : box.getBoundingClientRect().top
  const top = Math.max(sticky, keyboard.visibleTop.value) + 8
  const save = box === saveColumn.value ? footerHeight.value : 0
  const bottom = Math.min(rootBottom.value, keyboard.visibleBottom.value) - save - 8
  return { top, bottom }
}
/** The suggestions fill the space between the search box and the button (or the keyboard). */
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
  if (footerEl.value) sizes?.observe(footerEl.value)
})
watch(footerEl, (el, old) => {
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
/** Goes to the box a butterfly still lacks (from its card or the message above the button) and opens it for typing. */
function goToGap(id: string) {
  const g = gapById.value.get(id)
  const kind = g?.cam ? 'cam' : 'tube'
  // Its box is in the panel already when it is open there (alone or with others) or, on a wide screen, the next ones' values are.
  if ((panelMode.value !== 'defaults' || !wide.value) && !(panelMode.value === 'card' && selectedIds.value.some(s => sameId(s, id)))) {
    editingId.value = null
    selected.value = [id]
    anchor.value = id
  }
  nextTick(() => {
    const input = details.value?.querySelector<HTMLInputElement>(`[data-sample="${CSS.escape(id)}:${kind}"]`)
    if (!input) return reveal(details.value?.querySelector(`[data-row="${CSS.escape(id)}"]`))
    reveal(input)
    input.focus()
  })
}
/** Enter in a CAM or tube box goes to the next box (the next butterfly's), the last one closes the keyboard. */
function nextBox(event: KeyboardEvent) {
  if (event.ctrlKey || event.metaKey) return
  const boxes = [...(details.value?.querySelectorAll<HTMLInputElement>('input[data-sample]') ?? [])]
  const at = boxes.indexOf(event.target as HTMLInputElement)
  const next = boxes[at + 1]
  if (next) next.focus()
  else (event.target as HTMLInputElement).blur()
}

/** Clutch and the day it entered the insectary, as one line under the species (the sex is a badge beside it). */
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
</script>

<template>
  <!-- While the keyboard is open the screen fits the part one can see (a phone sideways keeps ~115 px):
       the search bar at its top, the button at its bottom, the box being typed in between. -->
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
    <div ref="scroller" data-scroll class="min-h-0 min-w-0 flex-1 overflow-y-auto" @click="onEmptyClick">
      <!-- The search stays at the top while the cards scroll. -->
      <div ref="searchBar" data-search-bar class="sticky top-0 z-20 border-b border-stone-200 bg-white px-3 pt-3 pb-2 short:pt-1.5 short:pb-1.5">
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
                @keydown.enter.exact.prevent="enter"
                @keydown="searchKeys"
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
        <!-- What is seen on the butterfly in hand ranks the IDs offered (on a phone only while typing: the bar stays
             small; on a wide screen always, so nothing below moves when the search box loses the focus). -->
        <IdFilters
          v-if="wide || focused || query"
          v-model:sex="sexFilter"
          v-model:species="speciesFilter"
          :species-list="aliveSpecies"
          class="mt-2 short:hidden"
        />
        <!-- Suggestions: a tap puts the butterfly in «Seleccionadas»; in several it joins or leaves and the keyboard
             stays for the next ID. Ctrl/Shift-click or a long press adds it beside the others. -->
        <ul
          v-if="focused && suggestions.length"
          class="absolute inset-x-3 top-full z-30 mt-1 divide-y divide-stone-100 overflow-y-auto rounded-xl border border-stone-200 bg-white shadow-lg"
          :style="{ maxHeight: `${listHeight}px` }"
          role="listbox"
        >
          <li v-for="s in suggestions" :key="s.entry.id">
            <button
              class="flex min-h-14 w-full items-center gap-3 px-3 py-2 text-left select-none active:bg-brand-50 short:min-h-12"
              :class="isPicked(s.entry.id) ? 'bg-brand-50' : ''"
              role="option"
              :aria-selected="isPicked(s.entry.id)"
              @mousedown.prevent
              @pointerdown="pressStart($event, s.entry.id)"
              @pointerup="pressEnd"
              @pointercancel="pressEnd"
              @pointerleave="pressEnd"
              @contextmenu.prevent
              @click="tapSuggestion($event, s.entry.id)"
            >
              <component
                :is="isPicked(s.entry.id) ? CheckCircle2 : Circle"
                v-if="several"
                :size="22"
                class="shrink-0"
                :class="isPicked(s.entry.id) ? 'text-brand-700' : 'text-stone-300'"
              />
              <IdSuggestion :id="s.entry.id" :facts="factsFor(s.entry.row)" :at="s.match?.at" :via="s.via" :greyed="s.match?.greyed" />
            </button>
          </li>
        </ul>
        <p v-if="!ready" class="mt-1.5 text-sm text-stone-500">{{ $t('Cargando {sheet}…', { sheet: MODULE }) }}</p>
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
        <!-- Choosing cards on a touch screen (a long press on one): each tap adds or takes one out; «Listo» shows them in the panel. -->
        <div
          v-if="ready && canEdit && selecting"
          class="-mx-3 mt-2 -mb-2 flex min-h-12 items-center gap-2 bg-brand-700 px-3 py-1 text-white short:mt-1.5 short:-mb-1.5"
          role="status"
          data-selecting
        >
          <CheckCircle2 :size="20" class="shrink-0" />
          <p class="min-w-0 flex-1 text-sm leading-tight">
            <span class="font-semibold">{{ $tn(selectedRows.length, '{n} seleccionada', '{n} seleccionadas') }}</span>
            <span class="block text-xs opacity-90 short:hidden">{{ $t('Toca las tarjetas para añadirlas o quitarlas') }}</span>
          </p>
          <button
            class="h-10 shrink-0 rounded-lg bg-white px-4 text-sm font-semibold text-brand-800 active:bg-brand-50"
            @click="doneSelecting"
          >
            {{ $t('Listo') }}
          </button>
        </div>
        <!-- «Seleccionar varias»: always in sight, with what a tap on an ID will do. -->
        <div
          v-else-if="ready && canEdit && several"
          class="-mx-3 mt-2 -mb-2 flex min-h-12 items-center gap-2 bg-brand-700 px-3 py-1 text-white short:mt-1.5 short:-mb-1.5"
          role="status"
          data-several
        >
          <ListChecks :size="20" class="shrink-0" />
          <p class="min-w-0 flex-1 text-sm leading-tight">
            <span class="font-semibold">{{ $tn(scratch.length, '{n} seleccionada', '{n} seleccionadas') }}</span>
            <span class="block text-xs opacity-90 short:hidden">{{ $t('Cada ID que toques se añade o se quita') }}</span>
          </p>
          <button
            class="flex h-10 shrink-0 items-center gap-1 rounded-lg bg-white px-3 text-sm font-semibold text-brand-800 active:bg-brand-50"
            :aria-label="$t('Salir de la selección: una a una')"
            @click="leaveSeveral"
          >
            <X :size="16" /> {{ $t('Una a una') }}
          </button>
        </div>
        <div v-else-if="ready" class="mt-1.5 flex min-h-11 items-center gap-2">
          <p class="min-w-0 flex-1 text-xs text-stone-500">
            <template v-if="cleared">
              {{ $t('Selección vaciada ({n})', { n: cleared.length }) }}
              <button class="ml-1 h-11 font-semibold text-brand-800 underline" @click="undoClear">{{ $t('Deshacer') }}</button>
            </template>
            <span v-else-if="!query" class="short:hidden">{{
              shortcut
                ? $t('Enter añade la mariposa a Seleccionadas; {keys} la añade a muertes. Pega varias o un rango (B0D-B9D).', {
                    keys: shortcut,
                  })
                : $t('Escribe un ID para ver si está viva; pega varios o un rango (B0D-B9D) para registrar muertes.')
            }}</span>
          </p>
          <button
            v-if="canEdit"
            class="flex h-11 shrink-0 items-center gap-1.5 rounded-lg border border-stone-300 bg-white px-3 text-sm font-medium text-stone-800 active:bg-stone-100"
            :aria-pressed="false"
            @mousedown.prevent
            @click="enterSeveral"
          >
            <ListChecks :size="18" /> {{ $t('Seleccionar varias') }}
          </button>
        </div>
      </div>

      <!-- The panel: in the right column on a wide screen; on a phone held upright above the cards, folded into one line
           for the next butterflies' values and unfolded with a card or a recorded death open. -->
      <Teleport to="#deaths-register" defer :disabled="!wide">
        <section v-if="canEdit && !unfolded" class="px-3 pt-3">
          <button
            class="flex min-h-12 w-full items-center gap-2 rounded-xl border border-stone-300 bg-white px-3 py-2 text-left text-sm active:bg-stone-100"
            :aria-expanded="false"
            data-panel-folded
            @click="panelOpen = true"
          >
            <span class="min-w-0 flex-1">
              <span class="block text-xs text-stone-500">{{ $t('Para las próximas mariposas') }}</span>
              <span class="block font-semibold text-stone-800">{{ panelSummary }}</span>
            </span>
            <span class="shrink-0 font-medium text-brand-800">{{ $t('Cambiar') }}</span>
            <ChevronDown :size="18" class="shrink-0 text-stone-500" />
          </button>
        </section>
        <section v-else-if="canEdit" ref="panel" class="space-y-5 px-3 pb-2" :class="wide ? 'pt-3' : 'pt-3'" data-panel>
          <!-- Whose death the choices below are: the next butterflies', one card's, or a recorded one being corrected. -->
          <div
            class="sticky top-0 z-10 -mx-3 flex min-h-12 items-center gap-2 border-y px-3 py-1.5"
            :class="panelMode === 'defaults' ? 'border-stone-200 bg-stone-100 text-stone-800' : 'border-brand-700 bg-brand-700 text-white'"
            role="status"
            data-applies
          >
            <p class="min-w-0 flex-1 text-sm leading-tight">
              <template v-if="panelMode === 'defaults'">
                <span class="block font-semibold">{{ $t('Para las próximas mariposas') }}</span>
                <span class="block text-xs text-stone-600">{{ $t('Cada mariposa que añadas empieza con estos valores.') }}</span>
              </template>
              <template v-else-if="panelMode === 'card' && multi">
                <span class="block font-semibold" data-selection-title>{{ selectionTitle }}</span>
                <span class="block text-xs opacity-90">{{
                  $t('Cada cambio va a las {n} seleccionadas; las demás siguen igual.', { n: selectedRows.length })
                }}</span>
              </template>
              <template v-else-if="panelMode === 'card'">
                <span class="block font-semibold">{{ $t('Muerte de {id}', { id: idOf(focusRow!) }) }}</span>
                <span class="block text-xs opacity-90">{{ $t('Solo esta tarjeta; las demás siguen igual.') }}</span>
              </template>
              <template v-else>
                <span class="block font-semibold">{{ $t('Muerte de {id}', { id: idOf(editingRow!) }) }}</span>
                <span class="block text-xs opacity-90">{{ $t('Registrada: corrígela y guarda el cambio.') }}</span>
              </template>
            </p>
            <button
              v-if="panelMode !== 'defaults'"
              class="h-10 shrink-0 rounded-lg bg-white px-4 text-sm font-semibold text-brand-800 active:bg-brand-50"
              :title="shortcut ? 'Esc' : undefined"
              @click="closePanel"
            >
              {{ panelMode === 'card' ? $t('Listo') : $t('Cancelar') }}
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
          <!-- The card or the death opened: what it is, at a glance. -->
          <div v-if="panelMode === 'recorded' || focusRow" class="flex items-start gap-2 text-sm">
            <div class="min-w-0 flex-1">
              <p class="font-medium">{{ factsFor((focusRow ?? editingRow)!).species || '—' }}</p>
              <p class="flex items-center gap-1.5 text-stone-600">
                <SexBadge :sex="factsFor((focusRow ?? editingRow)!).sex" />{{ line(factsFor((focusRow ?? editingRow)!)) }}
              </p>
            </div>
            <LifeBadge :facts="factsFor((focusRow ?? editingRow)!)" />
          </div>
          <!-- Dead in the sheet already: recording replaces that death, with a note saying so. -->
          <p
            v-for="row in panelMode === 'card' ? selectedRows.filter(replacing) : []"
            :key="row.id"
            class="flex items-start gap-1.5 rounded-lg bg-red-50 px-2 py-1.5 text-sm text-red-800"
          >
            <AlertTriangle :size="16" class="mt-0.5 shrink-0" />
            <span class="min-w-0"
              ><span class="font-semibold">{{ multi ? `${idOf(row)}: ` : '' }}{{ priorText(row) }}</span>
              {{ multi ? $t('se omite al añadir las seleccionadas; reemplázala desde su tarjeta') : $t('Registrarla reemplaza esa muerte y lo anota en Notes_Insectary_data.') }}</span
            >
          </p>
          <div>
            <h2 class="mb-1.5 text-sm font-semibold text-stone-700">
              {{ $t('Fecha de muerte') }} <span v-if="isMixed('date')" :class="mixedChip">{{ $t('varios') }}</span>
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
            <p v-else-if="shownDate" class="mt-1 text-sm text-stone-600">{{ dayLabel(shownDate) }}</p>
            <p v-for="row in panelBeforeEntry" :key="row.id" class="mt-1 text-sm text-amber-900">
              {{ multi ? `${idOf(row)}: ` : '' }}{{ beforeEntryText(row) }}
            </p>
            <button v-if="spread('date').length" class="mt-1 text-xs font-semibold text-brand-800 underline" @click="spreadField('date')">
              {{ $tn(spread('date').length, 'Aplicar también a {n} seleccionada', 'Aplicar también a las {n} seleccionadas') }}
            </button>
          </div>
          <div>
            <h2 class="mb-1.5 text-sm font-semibold text-stone-700">
              Death_cause <span v-if="isMixed('cause')" :class="mixedChip">{{ $t('varios') }}</span>
            </h2>
            <div class="grid grid-cols-[repeat(auto-fill,minmax(8.5rem,1fr))] gap-2">
              <button
                v-for="({ cause: c, today: n }, i) in causeButtons"
                :key="c"
                class="relative min-h-12 rounded-lg border px-2 py-2 text-base font-medium break-words"
                :class="choice(shown.cause === c)"
                :aria-pressed="shown.cause === c"
                :aria-keyshortcuts="!touch && i < 9 ? String(i + 1) : undefined"
                :data-cause="c"
                @click="pickCause(c)"
              >
                <!-- The number key that picks it (computers only). -->
                <span v-if="!touch && i < 9" class="absolute top-0.5 left-1.5 text-[0.65rem] leading-none font-semibold opacity-50" aria-hidden="true">{{
                  i + 1
                }}</span>
                {{ c }}
                <span
                  v-if="n"
                  class="ml-0.5 rounded px-1 text-xs font-semibold"
                  :class="shown.cause === c ? 'bg-white/20' : 'bg-brand-50 text-brand-800'"
                  :title="$tn(n, '{n} hoy', '{n} hoy')"
                  >×{{ n }}</span
                >
              </button>
            </div>
            <button v-if="spread('cause').length" class="mt-1 text-xs font-semibold text-brand-800 underline" @click="spreadField('cause')">
              {{ $tn(spread('cause').length, 'Aplicar también a {n} seleccionada', 'Aplicar también a las {n} seleccionadas') }}
            </button>
          </div>
          <div>
            <h2 class="mb-1.5 text-sm font-semibold text-stone-700">
              {{ $t('Preservación') }} <span v-if="isMixed('preserved')" :class="mixedChip">{{ $t('varios') }}</span>
            </h2>
            <!-- A recorded death keeps its preservation here (Tubos changes it, or undo and add it again). -->
            <template v-if="panelMode === 'recorded'">
              <p class="rounded-lg border border-stone-200 bg-white px-3 py-2 text-sm font-medium">{{ preservationLine(editingRow!) }}</p>
              <p class="mt-1 text-xs text-stone-500">
                {{ $t('Para cambiarla: Tubos, o «Deshacer muerte» y añadirla otra vez.') }}
              </p>
            </template>
            <template v-else>
              <div class="grid grid-cols-2 gap-2">
                <button
                  class="min-h-12 rounded-lg border px-2 text-base font-medium"
                  :class="choice(shown.preserved === false && !isMixed('preserved'))"
                  :aria-pressed="shown.preserved === false && !isMixed('preserved')"
                  @click="setField('preserved', false)"
                >
                  {{ $t('Sin preservar') }}
                </button>
                <button
                  class="min-h-12 rounded-lg border px-2 text-base font-medium"
                  :class="choice(shown.preserved === true)"
                  :aria-pressed="shown.preserved === true"
                  @click="setField('preserved', true)"
                >
                  {{ $t('Preservada') }}
                </button>
              </div>
              <button
                v-if="spread('preserved').length"
                class="mt-1 text-xs font-semibold text-brand-800 underline"
                @click="spreadField('preserved')"
              >
                {{ $tn(spread('preserved').length, 'Aplicar también a {n} seleccionada', 'Aplicar también a las {n} seleccionadas') }}
              </button>
              <p v-if="shown.preserved === false && !isMixed('preserved')" class="mt-1.5 text-sm text-stone-600">
                {{ $t('Sin preservar: CAM y tubos NA, tejidos y medios NOT_COLLECTED') }}
              </p>
              <!-- Preserved: the medium, then each butterfly's CAM and tube, right here where the eye is. -->
              <div
                v-if="showPreservation"
                ref="details"
                class="mt-2 rounded-xl border bg-white p-3 transition-shadow duration-700"
                :class="flash ? 'border-brand-600 shadow-[0_0_0_4px_var(--color-brand-100)]' : 'border-stone-200'"
              >
                <p class="text-sm text-stone-600 short:hidden">
                  {{ $t('Cuerpo entero ({tissue}) en un tubo: confirma o escribe el CAM y el tubo de cada una.', { tissue: WHOLE }) }}
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
                <div v-if="panelPreserve.length" class="mt-3 flex items-baseline justify-between gap-2">
                  <span class="text-sm font-semibold text-stone-700">{{ $t('CAM y tubo de cada una') }}</span>
                  <span
                    class="text-xs font-semibold"
                    :class="panelReady === panelPreserve.length ? 'text-brand-700' : 'text-amber-800'"
                    role="status"
                  >
                    {{ $t('{ok} de {n} listas', { ok: panelReady, n: panelPreserve.length }) }}
                  </span>
                </div>
                <ul v-if="panelPreserve.length" class="mt-1.5 divide-y divide-stone-100 rounded-lg border border-stone-200">
                  <li
                    v-for="row in panelPreserve"
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
                        @keydown.enter.exact.prevent="nextBox"
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
                        @keydown.enter.exact.prevent="nextBox"
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
                <p v-if="panelNotPreserve.length" class="mt-2 text-xs text-stone-500">
                  {{ $t('Ya registradas como muertas, sin tubo aquí: {ids}', { ids: panelNotPreserve.map(idOf).join(', ') }) }}
                </p>
              </div>
            </template>
          </div>
          <!-- A note, in English: typed or quick phrases; recording adds it, dated and signed, after the notes there. -->
          <div>
            <h2 class="mb-1.5 text-sm font-semibold text-stone-700">
              {{ $t('Nota') }} <span v-if="isMixed('note')" :class="mixedChip">{{ $t('varios') }}</span>
            </h2>
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
                :value="shown.note"
                class="field-input min-h-20 text-base short:min-h-0"
                :rows="short ? 1 : 2"
                :placeholder="
                  isMixed('note') ? $t('Varias notas: lo que escribas reemplaza la de cada una') : $t('Nota, en inglés (p. ej. Only wings found)')
                "
                enterkeyhint="done"
                data-note-input
                @input="setField('note', ($event.target as HTMLTextAreaElement).value)"
              />
            </label>
            <p class="mt-1 text-xs text-stone-500">
              {{ $t('Al guardar se añade a Notes_Insectary_data, tras las notas que ya tiene: «{prefix} …»', { prefix: notePrefix }) }}
            </p>
            <button v-if="spread('note').length" class="mt-1 text-xs font-semibold text-brand-800 underline" @click="spreadField('note')">
              {{ $tn(spread('note').length, 'Aplicar también a {n} seleccionada', 'Aplicar también a las {n} seleccionadas') }}
            </button>
          </div>
          <button v-if="panelMode === 'recorded' || focusRow" class="btn h-11 w-full" @click="drawerRow = (focusRow ?? editingRow)!">
            <Columns3 :size="16" /> {{ $t('Todas las columnas') }}
          </button>
          <p v-if="otherPending && panelMode !== 'recorded' && cardRows.length" class="text-xs text-amber-900">
            {{
              $tn(
                otherPending,
                'Se guardará también {n} cambio pendiente de otras filas.',
                'Se guardarán también {n} cambios pendientes de otras filas.',
              )
            }}
          </p>
        </section>
      </Teleport>

      <!-- «Seleccionadas»: the butterflies picked, each with its own death; a tap opens it in the panel, × takes it out. -->
      <section v-if="cardRows.length" class="px-3 pt-4" data-selected>
        <div class="flex flex-wrap items-center gap-2">
          <h2 class="text-sm font-semibold text-stone-700">
            {{ $t('Seleccionadas') }} <span class="font-normal text-stone-500">({{ cardRows.length }})</span>
          </h2>
          <span class="min-w-0 flex-1" />
          <button v-if="cardRows.length > 1" class="h-10 rounded-lg px-2 text-sm text-stone-600 underline active:bg-stone-100" @click="clearAll">
            {{ $t('Vaciar') }}
          </button>
          <button
            v-if="canEdit && cardRows.length > 1"
            class="btn h-10"
            :disabled="!readyRows.length || saving"
            :title="[shortcut && panelMode === 'defaults' ? shortcut : '', skippedText(skippedRows.length)].filter(Boolean).join(' · ') || undefined"
            @click="record(readyRows, skippedRows.length)"
          >
            <Plus :size="16" /> {{ $t('Añadir todas ({n})', { n: readyRows.length }) }}
          </button>
        </div>
        <p v-if="canEdit && cardRows.length > 1 && skippedRows.length" class="mt-1 text-right text-xs text-red-800">
          {{ skippedText(skippedRows.length) }}
        </p>
        <ul class="mt-2 grid grid-cols-[repeat(auto-fill,minmax(17rem,1fr))] gap-2">
          <li
            v-for="row in cardRows"
            :key="row.id"
            class="relative flex flex-col rounded-xl border-2 bg-white shadow-sm"
            :class="[
              isSelected(row) ? 'border-brand-600 ring-2 ring-brand-100' : 'border-stone-200',
              pulsing && sameId(pulsing, idOf(row)) ? 'card-pulse' : '',
            ]"
            :data-card="idOf(row)"
            :data-selected="isSelected(row) || undefined"
          >
            <button
              class="block w-full flex-1 rounded-t-xl px-3 pt-2.5 pr-12 pb-2 text-left"
              :class="touch ? 'select-none [-webkit-touch-callout:none]' : ''"
              :aria-label="$t('Abrir {id} en el panel', { id: idOf(row) })"
              :aria-pressed="isSelected(row)"
              :title="shortcut ? $t('{key}+clic o Mayús+clic: varias a la vez', { key: macKeys ? '⌘' : 'Ctrl' }) : undefined"
              @mousedown="noTextSelect"
              @pointerdown="cardPressStart($event, row)"
              @pointerup="cardPressEnd"
              @pointercancel="cardPressEnd"
              @pointerleave="cardPressEnd"
              @contextmenu="touch && $event.preventDefault()"
              @click="clickRow($event, row)"
            >
              <span class="flex flex-wrap items-center gap-2">
                <CheckCircle2 v-if="isSelected(row)" :size="20" class="shrink-0 text-brand-700" />
                <Circle v-else-if="selecting" :size="20" class="shrink-0 text-stone-300" />
                <span class="text-xl font-semibold">{{ row.values.Insectary_ID }}</span>
                <LifeBadge :facts="factsFor(row)" />
                <span v-if="sampleGap(row)" class="rounded-md bg-amber-100 px-1.5 py-0.5 text-xs font-medium text-amber-900" :title="sampleTitle()">{{
                  $t('Sin CAM/tubo')
                }}</span>
              </span>
              <span class="mt-0.5 block text-sm">{{ factsFor(row).species || '—' }}</span>
              <span class="flex items-center gap-1.5 text-xs text-stone-600"><SexBadge :sex="factsFor(row).sex" />{{ line(factsFor(row)) }}</span>
              <span v-if="factsFor(row).life.cause" class="block text-xs text-stone-700">Death_cause: {{ factsFor(row).life.cause }}</span>
            </button>
            <button
              class="absolute top-1 right-1 grid h-11 w-11 place-items-center rounded-lg text-stone-500 active:bg-stone-100"
              :aria-label="$t('Quitar {id}', { id: idOf(row) })"
              :title="$t('Quitar {id}', { id: idOf(row) })"
              @click="remove(idOf(row))"
            >
              <X :size="20" />
            </button>
            <!-- Preserved and still lacking its CAM or tube: a tap goes to the box. -->
            <button
              v-if="canEdit && gapById.get(idOf(row)) && hasGap(gapById.get(idOf(row))!)"
              class="flex min-h-11 items-center gap-1.5 border-t border-amber-200 bg-amber-50 px-3 py-1.5 text-left text-sm font-medium text-amber-900"
              @click="goToGap(idOf(row))"
            >
              <AlertTriangle :size="16" class="shrink-0" />
              <span class="min-w-0 flex-1">{{ gapText(gapById.get(idOf(row))!) }}</span>
            </button>
            <!-- This card's date, cause and preservation, and the note it adds; then its button. -->
            <div v-if="canEdit" class="rounded-b-xl border-t border-stone-100 px-3 py-1.5 text-xs">
              <!-- Dead in the sheet already: what its button would replace. -->
              <p
                v-if="replacing(row)"
                class="mb-1 flex items-start gap-1 rounded-md bg-red-50 px-1.5 py-1 text-sm font-semibold text-red-800"
                :data-prior="idOf(row)"
              >
                <AlertTriangle :size="15" class="mt-0.5 shrink-0" /><span class="min-w-0">{{ priorText(row) }}</span>
              </p>
              <p class="flex flex-wrap gap-1" :data-choice="idOf(row)">
                <span
                  v-for="chip in chipsOf(row)"
                  :key="chip.field"
                  class="rounded-md px-1.5 py-0.5 font-medium"
                  :class="chip.missing ? 'bg-amber-100 text-amber-900' : 'bg-stone-100 text-stone-700'"
                  >{{ chip.text }}</span
                >
              </p>
              <p v-if="noteOf(row)" class="mt-1 flex items-start gap-1 rounded-md bg-stone-100 px-1.5 py-0.5 break-words text-stone-700" :data-note="idOf(row)">
                <StickyNote :size="13" class="mt-px shrink-0" /><span class="min-w-0">{{ noteOf(row) }}</span>
              </p>
              <p v-if="beforeEntry(row)" class="mt-1 text-amber-900" :data-before-entry="idOf(row)">{{ beforeEntryText(row) }}</p>
              <p v-if="refusals[idOf(row)]" class="mt-1 text-sm text-red-700">{{ $t('No se guardó: {reason}', { reason: refusals[idOf(row)] }) }}</p>
              <div class="mt-1.5 flex flex-wrap items-center justify-end gap-2">
                <span class="min-w-24 flex-1 text-xs" :class="lackFor(row) === 'nothing' ? 'text-stone-500' : 'text-amber-900'">{{
                  lackFor(row) && lackFor(row) !== 'sample' ? lackText(lackFor(row), row) : ''
                }}</span>
                <button
                  class="h-10 shrink-0 px-3 text-sm"
                  :class="replacing(row) ? 'btn border-red-300 text-red-800' : 'btn-primary'"
                  :disabled="!!lackFor(row) || saving"
                  :data-add="idOf(row)"
                  @click="record([row])"
                >
                  <Loader2 v-if="recording.includes(idOf(row))" :size="15" class="animate-spin" /><Plus v-else-if="!replacing(row)" :size="16" />
                  {{ replacing(row) ? $t('Reemplazar la muerte anterior') : $t('Añadir a muertes') }}
                </button>
              </div>
            </div>
          </li>
        </ul>
      </section>

      <DeathsRecorded
        v-model:order="order"
        v-model:mine-only="mineOnly"
        :items="recorded"
        :editing="editingId"
        :can-edit="canEdit"
        :loading="loadingRecorded"
        :gap-of="gapChip"
        @edit="startEdit"
        @undo="undoDeath"
        @refresh="loadRecorded"
      />
    </div>

    <!-- The right column on a wide screen: the panel, always there, with its button at the foot. -->
    <aside
      v-show="wide"
      class="flex min-h-0 w-[min(30rem,46%)] shrink-0 flex-col border-l border-stone-200 bg-stone-50"
      :aria-label="$t('Registrar muertes')"
    >
      <div id="deaths-register" ref="aside" data-scroll class="min-h-0 flex-1 overflow-y-auto" />
      <div id="deaths-save" />
    </aside>

    <!-- The panel's button: at the foot of what is visible, above the keyboard. -->
    <Teleport to="#deaths-save" defer :disabled="!wide">
      <footer
        v-if="canEdit && (footer || wide)"
        ref="footerEl"
        class="relative z-20 shrink-0 border-t border-stone-200 bg-white px-3 pt-2 pb-[calc(0.5rem+env(safe-area-inset-bottom))] short:pt-1.5 short:pb-1.5"
        :class="{ hidden: cramped }"
      >
        <p v-if="!footer" class="py-2 text-sm text-stone-500">
          {{ $t('Busca y añade mariposas: cada una lleva los valores de arriba, y la añades a muertes desde su tarjeta.') }}
        </p>
        <!-- Upright phone: the message above a full-width button; sideways or wide: side by side. -->
        <div v-else :class="wide ? 'flex items-center gap-3' : ''">
          <button
            v-if="footer.gap"
            class="flex min-h-11 w-full min-w-0 items-center gap-1 text-left text-sm font-medium text-amber-900 underline decoration-amber-400 underline-offset-2"
            :class="wide ? 'flex-1' : 'mb-1 short:min-h-8'"
            @click="goToGap(footer.gap)"
          >
            <AlertTriangle :size="16" class="shrink-0" /><span class="min-w-0 truncate">{{ footer.text }}</span>
          </button>
          <p
            v-else-if="footer.text || wide"
            :class="[
              footer.disabled ? 'text-amber-900' : 'text-stone-600',
              wide ? 'line-clamp-2 min-w-0 flex-1 text-sm' : 'mb-1.5 truncate text-xs',
            ]"
          >
            {{ footer.text }}
          </p>
          <button
            class="btn-primary h-13 text-base short:h-11"
            :class="wide ? 'shrink-0 px-5' : 'w-full'"
            :disabled="footer.disabled || saving"
            :title="shortcut || undefined"
            data-primary
            @click="footer.run()"
          >
            <Loader2 v-if="saving" :size="18" class="animate-spin" /><Plus v-else-if="panelMode !== 'recorded'" :size="18" />
            {{ saving ? $t('Guardando…') : footer.label }}
          </button>
        </div>
      </footer>
    </Teleport>

    <TabHistory v-if="showHistory" :title="$t('Historial de Muertes')" purpose="muertes" @close="showHistory = false" />
    <UndoDialog
      v-if="undoing"
      v-model:reason="undoReason"
      :review="undoing"
      :busy="undoBusy"
      :summary="undoSummary"
      :action="$t('Deshacer la muerte')"
      @cancel="cancelUndo"
      @confirm="confirmUndo"
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

<style scoped>
/* A card clicked while already open in the panel: a short ring and lift, to say "this one". */
.card-pulse {
  animation: card-pulse 600ms ease-out;
}
@keyframes card-pulse {
  0% {
    box-shadow: 0 0 0 0 color-mix(in srgb, var(--color-brand-600) 55%, transparent);
    transform: scale(1);
  }
  30% {
    transform: scale(1.015);
  }
  100% {
    box-shadow: 0 0 0 12px transparent;
    transform: scale(1);
  }
}
/* Without motion: the card lights up for that moment instead. */
@media (prefers-reduced-motion: reduce) {
  .card-pulse {
    animation: none;
    background-color: var(--color-brand-50);
  }
}
</style>
