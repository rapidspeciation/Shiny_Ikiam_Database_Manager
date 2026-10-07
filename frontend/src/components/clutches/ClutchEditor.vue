<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { AlertTriangle, CalendarClock, Check, ChevronDown, ChevronLeft, ChevronRight, Columns3, Loader2, X } from 'lucide-vue-next'
import ChoiceField from '../ChoiceField.vue'
import StagePanel, { type StageAct, type TermRef } from './StagePanel.vue'
import EventSheet from './EventSheet.vue'
import DayNoteSheet from './DayNoteSheet.vue'
import PreserveYoung from '../emerged/PreserveYoung.vue'
import ClutchTimeline from './ClutchTimeline.vue'
import DateRow from './DateRow.vue'
import ClutchNotes from './ClutchNotes.vue'
import ClutchPhotoAdd from './ClutchPhotoAdd.vue'
import ClutchPhotoViewer from './ClutchPhotoViewer.vue'
import { useClutchRecord } from '../../composables/useClutchRecord'
import { useKeyboard } from '../../composables/usePhone'
import { useParents } from '../../composables/useParents'
import type { ClutchDay } from '../../composables/useClutchDay'
import { isBlank } from '../../lib/cells'
import {
  COUNTS,
  MODULE,
  STAGES,
  STAGE_FIELD,
  appendNote,
  countCell,
  eventNote,
  formulaOf,
  gainOf,
  groupsValue,
  latestGains,
  lockedFormula,
  lossTakesOff,
  noteDay,
  notesOf,
  noteParts,
  outlook,
  preservedApart,
  parentsOf,
  readCount,
  totalOf,
  VERIFY_REASONS,
  withoutNote,
  type ClutchEvent,
  type ClutchState,
  type CountField,
  type EventKind,
  type Stage,
  type StageDurations,
} from '../../lib/clutches'
import {
  appendInGroup,
  countState,
  formulaOfGroups,
  groupName,
  leftOf,
  nextLabel,
  planCorrection,
  planMoveOn,
  planRegroup,
  planRemove,
  planSplit,
  planTerm,
  type CountState,
  type GroupRow,
  type Groups,
  type Plan,
  type StepGroupItem,
} from '../../lib/clutchGroups'
import type { ClutchPhoto } from '../../lib/clutchPhotos'
import { formatSerial, isoToSerial, serialToIso, todayIso } from '../../lib/dates'
import { errorText, notify } from '../../lib/notice'
import type { CellValue, Field, TableRow } from '../../lib/types'
import { usePending } from '../../stores/pending'
import { useSession } from '../../stores/session'
import { t, tn } from '../../lib/i18n'

/**
 * One clutch, to update during the round, one stage at a time (tabs Eggs,
 * Larvae, Pupae, Adults with each count on its tab, the clutch's stage first):
 * its count's sum (tap the total and type the new one; or +N / − / "Counted
 * today") and its stage date; below, folded, the notes dated and initialled
 * with the parents (F1/F2) in NOTES, the history and photos, and the rest
 * (dissections, species, generation and room). Changes are pending edits
 * saved as any other (kept in the app until «Guardar en Google Sheets»);
 * «Marcar como revisado» saves and marks the clutch as checked today, with the
 * fields it changed. Every event (+N hatched, −N died…) also writes its dated,
 * signed note in NOTES (shown before saving) and, for a stage's first one,
 * its date; larvae preserved can be registered one by one in Insectary_data
 * (Emergidos' cards, components/emerged/PreserveYoung). On top: what should be
 * in the cage today and when the next stages are expected. Full screen on a
 * phone; beside the list (`docked`) on a tablet or computer.
 */
const props = defineProps<{
  rows: TableRow[]
  columns: Field[]
  options: Record<string, string[]>
  species: string[]
  day: ClutchDay
  state: (row: TableRow) => ClutchState
  canEdit: boolean
  initials: string
  /** A person's initials from their name (FCH), for the marks and events. */
  initialsFor?: (name: string) => string
  /** Each species' days per stage (lib/clutches stageDurations), for the dates expected. */
  durations: StageDurations
  docked?: boolean
}>()
const index = defineModel<number>('index', { required: true })
const emit = defineEmits<{ close: []; more: [row: TableRow] }>()

const pending = usePending()
const keyboard = useKeyboard()
const parents = useParents()
const row = computed(() => props.rows[Math.min(index.value, props.rows.length - 1)])
const label = computed(() => String(row.value?.values['CLUTCH NUMBER'] ?? ''))
const formulas = computed(() => (row.value ? props.day.sums.value[row.value.id] : undefined))
const field = (key: string): Field | undefined => props.columns.find(c => c.key === key)
const has = (key: string) => !!field(key) && !field(key)!.unavailable
const get = (key: string): CellValue => (row.value ? pending.value(row.value, key) : null)
const dirty = (key: string) => !!row.value && pending.isDirty(row.value.id, key)
const countOf = (key: string) => (row.value ? countCell(row.value, key, pending.value, dirty(key), formulas.value) : null)
const savedOf = (key: string) => formulas.value?.[key] ?? row.value?.values[key] ?? null
/** A field as it was before today's changes (the day's first "before"), or undefined if not changed today. */
function startOfDay(key: string) {
  const change = row.value ? props.day.today(row.value.id).changes.find(c => c.field === key) : undefined
  if (!change) return undefined
  const before = change.before
  return before && typeof before === 'object' ? before.formula : before
}
const editable = (key: string) =>
  props.canEdit && !!row.value && has(key) && !field(key)!.readonly && (!row.value.formulas.includes(key) || !!formulas.value?.[key])
const today = computed(() => isoToSerial(todayIso()))
const status = computed(() => (row.value ? props.day.today(row.value.id) : null))
const clutchState = computed(() => (row.value ? props.state(row.value) : null))

/** What each count's + means (the button's second line). */
const MORE: Record<string, () => string> = {
  'NUMBER OF EGGS': () => t('puestos'),
  'NUMBER OF LARVAE': () => t('eclosionaron'),
  'NUMBER OF PUPA': () => t('pupas nuevas'),
  'NUMBER OF ADULTS': () => t('emergieron'),
  'NUMBER OF PUPAE/LARVAE FOR DISECTIONS': () => t('disecados'),
}
const FIRST: Record<string, () => string> = {
  'HATCHING DATE': () => t('primera eclosión'),
  'PUPA DATE': () => t('primera pupa'),
  'EMERGENCE DATE': () => t('primera emergencia'),
}

// --- What this opening changed (for the check), compared with the clutch as it was opened
const opened = ref<{ id: string; values: Record<string, string> } | null>(null)
const norm = (key: string, v: CellValue) =>
  (COUNTS as readonly string[]).includes(key) ? (formulaOf(readCount(v).terms) ?? String(v ?? '')) : JSON.stringify(v ?? null)
const current = (key: string) => ((COUNTS as readonly string[]).includes(key) ? countOf(key) : get(key))
/** Fields set in this opening, and the value they were last set to (a save may land before the sheet's formulas reload). */
const touched = ref<Record<string, string>>({})
const changedFields = computed(() =>
  Object.entries(touched.value)
    .filter(([key, value]) => opened.value?.values[key] !== value)
    .map(([key]) => key),
)
const rowPending = computed(() => (row.value ? Object.keys(pending.edits[row.value.id]?.values ?? {}).length : 0))

const message = ref('')
function setValue(key: string, value: CellValue) {
  const r = row.value
  if (!r || !editable(key)) return
  message.value = ''
  // Back to what the sheet has: no pending edit (a sum is compared as its formula).
  const back = (COUNTS as readonly string[]).includes(key) && norm(key, value) === norm(key, savedOf(key))
  pending.setCell(MODULE, r, label.value, key, back ? (r.values[key] ?? null) : value)
  // The save checks each cell against what the person saw: for a sum, its formula (=3+5-2),
  // which the server compares exactly (the total a formula shows can lag behind a save).
  const edit = pending.edits[r.id]
  if (edit && key in edit.before && formulas.value?.[key]) {
    edit.before[key] = formulas.value[key]
    pending.persist(false)
  }
  touched.value = { ...touched.value, [key]: norm(key, value) }
}

// --- Parents (F1/F2) and notes: components/clutches/ClutchNotes, the parents written in NOTES for now
/** Parents written on a clutch without a generation: an F1 (the generation buttons change it). */
function parentsWritten() {
  if (has('Generation') && isBlank(get('Generation'))) setValue('Generation', 'F1')
}
const generations = computed(() => [...new Set(['NA', 'F1', 'F2', 'Backcross', ...(props.options.Generation || [])])].filter(g => g.length < 20))

// --- Done: save, then mark as checked
const finishing = ref(false)
const waitIdle = async () => {
  for (let i = 0; i < 300 && pending.saving; i++) await new Promise(r => setTimeout(r, 100))
}
async function finish(state: 'checked' | 'verify' = 'checked', note = '') {
  const r = row.value
  if (!r || finishing.value) return
  finishing.value = true
  message.value = ''
  try {
    const fields = changedFields.value
    let actionId: string | undefined
    let stagedEntry: string | undefined
    if (pending.edits[r.id]) {
      await waitIdle()
      // Kept in the app for everyone until «Guardar en Google Sheets» (server/staged.mjs).
      const result = await pending.save('')
      actionId = result.actionId
      stagedEntry = result.stagedEntry
      const refused = Object.entries(pending.issues).find(([k]) => k.startsWith(`${r.id}:`))
      if (refused) {
        message.value = t('No se guardó {ids}: {reason}', { ids: label.value, reason: refused[1] })
        return
      }
    }
    // A clutch entered here and not in the sheet yet has no row to mark: it is marked once written.
    if (!r.id.startsWith('staged:')) await props.day.markChecked(r.id, fields, actionId, { state, note, stagedEntry })
    touched.value = {}
    verifying.value = false
    verifyNote.value = ''
    opened.value = { id: r.id, values: Object.fromEntries(props.columns.map(c => [c.key, norm(c.key, current(c.key))])) }
    notify(
      state === 'verify'
        ? t('Clutch {clutch} marcado por verificar', { clutch: label.value })
        : fields.length
          ? t('Clutch {clutch} guardado en la app y revisado', { clutch: label.value })
          : t('Clutch {clutch} revisado, sin cambios', { clutch: label.value }),
      'success',
    )
    if (!props.docked) emit('close')
  } catch (e) {
    message.value = errorText(e)
  } finally {
    finishing.value = false
  }
}
/** "Checked, needs verification": with a short reason, so someone else looks again. */
const verifying = ref(false)
const verifyNote = ref('')
const who = (name: string) => (props.initialsFor ? props.initialsFor(name) : name)
const checkedLine = computed(() => {
  const s = status.value
  if (!s?.latest) return ''
  const by = who(s.latest.name || s.latest.username || '')
  if (s.review === 'verify')
    return s.latest.note ? t('Por verificar ({who}): {note}', { who: by, note: s.latest.note }) : t('Por verificar ({who})', { who: by })
  return t('Revisado hoy por {who} · solo en la app', { who: s.checkedBy.map(who).join(', ') })
})

// --- What happened, recorded as one step each: the count's formula (its terms in their groups),
// the events behind the terms and the groups (only in the app), the short line in NOTES, the stage's first date.
/** The first date of each stage, set by its first gain when empty (or later than it). */
const GAIN_DATE: Record<Stage, string> = { egg: 'DATE LAID', larva: 'HATCHING DATE', pupa: 'PUPA DATE', adult: 'EMERGENCE DATE' }
const NEXT: Partial<Record<Stage, Stage>> = { egg: 'larva', larva: 'pupa' }
const BEFORE: Partial<Record<Stage, Stage>> = { larva: 'egg', pupa: 'larva' }
const groupRows = computed<GroupRow[]>(() => record.data.value?.groups ?? [])
/** A count's groups as written and what the app keeps of each, by position. */
function stateOf(stage: Stage) {
  const field = STAGE_FIELD[stage]
  return countState(field, stage, readCount(countOf(field)).groups, groupRows.value)
}
/** The groups selected in each stage (indexes into its parentheses). */
const selected = ref<Partial<Record<Stage, number[]>>>({})
/** This opening's steps, newest last: what each changed in the row (to put back) and its record in the app (to take back). */
const steps = ref<{ id: string | null; before: Record<string, CellValue>; fields: string[] }[]>([])
const stepsOf = (field: string) => steps.value.filter(s => s.fields.includes(field)).length
/** The note each event wrote in NOTES (by event id), to take it out with the event. */
const madeNotes = new Map<string, string>()
const noteLine = (text: string) => `${noteDay(today.value)} ${props.initials}: ${text}`
/** The note an event adds to NOTES, dated and signed: "5/10/26 FCH: 5 larvae died" ('' when NOTES cannot be written). */
function noteFor(stage: Stage, e: { kind: EventKind; count: number; ids: string[]; lifestage?: string; group?: string | null }) {
  if (!editable('NOTES')) return ''
  return noteLine(eventNote({ stage, ...e }, today.value))
}
const panels = ref<Partial<Record<string, InstanceType<typeof StagePanel>>>>({})
const failIn = (field: string, reason: string) => panels.value[field]?.fail(reason)

/**
 * Applies a plan: the counts' new formulas, a line in NOTES per event, a
 * stage's first date; then records its events, groups and history at once (a
 * step). If the app cannot record it, the row goes back as it was.
 */
async function apply(plan: Plan, { notes = [], dates = [] }: { notes?: (string | null)[]; dates?: { field: string; value: CellValue }[] } = {}) {
  const r = row.value
  if (!r) return
  const fields = [...Object.keys(plan.counts), ...(notes.some(Boolean) && editable('NOTES') ? ['NOTES'] : []), ...dates.map(d => d.field).filter(editable)]
  const before = Object.fromEntries(fields.map(f => [f, (COUNTS as readonly string[]).includes(f) ? countOf(f) : get(f)]))
  for (const [field, groups] of Object.entries(plan.counts)) setValue(field, groupsValue(groups as Groups))
  const lines = notes.map(n => (n && editable('NOTES') ? noteLine(n) : ''))
  if (lines.some(Boolean)) {
    let all = get('NOTES')
    for (const n of notes) if (n) all = appendNote(all, n, today.value, props.initials)
    setValue('NOTES', all)
  }
  for (const d of dates) if (editable(d.field)) setValue(d.field, d.value)
  const step: (typeof steps.value)[number] = { id: null, before, fields }
  steps.value = [...steps.value, step]
  if (!inSheet.value || (!plan.events.length && !plan.groups.length && !plan.log.length)) return
  try {
    const saved = await props.day.addStep({ recordId: r.id, events: plan.events, groups: plan.groups, log: plan.log })
    step.id = saved.id
    if (lines.length === saved.events.length) saved.events.forEach((e, i) => lines[i] && madeNotes.set(e.id, lines[i]))
    record.data.value = record.data.value ? { ...record.data.value, groups: [...(record.data.value.groups ?? []).filter(g => g.endedAt), ...saved.groups] } : record.data.value
  } catch (err) {
    message.value = errorText(err)
    steps.value = steps.value.filter(s => s !== step)
    for (const [field, value] of Object.entries(before)) setValue(field, value)
  }
}
/** Undoes this opening's last step: the row as it was before it, its events and groups taken back. */
async function undoStep() {
  const step = steps.value.at(-1)
  if (!step) return
  steps.value = steps.value.slice(0, -1)
  for (const [field, value] of Object.entries(step.before)) setValue(field, value)
  if (step.id)
    try {
      await props.day.undoStep(step.id)
    } catch (err) {
      message.value = errorText(err)
    }
}
/** Today's changes to a count taken back: this opening's steps on it undone, then the formula it had this morning. */
async function backToMorning(field: string, morning: CellValue | undefined) {
  while (steps.value.length && steps.value.at(-1)!.fields.includes(field)) await undoStep()
  setValue(field, morning ?? savedOf(field))
}
/** A stage's first date, from its first gain (or NA when the day is not known). */
function firstDate(stage: Stage, day: string | undefined, dayKnown = true): { field: string; value: CellValue }[] {
  const field = GAIN_DATE[stage]
  if (!has(field)) return []
  const now = get(field)
  if (!dayKnown) return now === null || now === '' ? [{ field, value: 'NA' }] : []
  const serial = isoToSerial(day ?? todayIso())
  return now === null || now === '' || now === 'NA' || (typeof now === 'number' && serial < now) ? [{ field, value: serial }] : []
}
const nameIn = (s: CountState, index: number | null | undefined) => (s.groups.length > 1 && index !== null && index !== undefined ? groupName(s.meta[index], index) : null)

/** What a stage panel asked for, worked out (lib/clutchGroups.ts) and applied. */
async function act(stage: Stage | null, field: string, a: StageAct) {
  message.value = ''
  if (!stage) return actPlain(field, a)
  const s = stateOf(stage)
  const fail = (r: { ok: false; reason: string }) => failIn(field, r.reason)
  if (a.type === 'gain') {
    const kind = gainOf(stage)
    const from = BEFORE[stage]
    if (a.fromIndex !== null && from && a.groupIndex === null) {
      const r = planMoveOn(stateOf(from), s, [{ index: a.fromIndex, count: a.count }], { day: a.day, dayKnown: a.dayKnown })
      if (!r.ok) return fail(r)
      const after = { ...s, groups: r.plan.counts[s.field]! }
      return apply(r.plan, {
        notes: r.plan.events.map(e => eventNote({ stage, kind, count: e.count, day: isoToSerial(a.day), dayKnown: a.dayKnown, group: after.groups.length > 1 ? groupName(stateOf(from).meta[a.fromIndex!], a.fromIndex!) : null }, today.value)),
        dates: firstDate(stage, a.day, a.dayKnown),
      })
    }
    const r = planTerm(s, a.groupIndex, { stage, kind, count: a.count, term: a.count, day: a.day, dayKnown: a.dayKnown })
    if (!r.ok) return fail(r)
    return apply(r.plan, {
      notes: [eventNote({ stage, kind, count: a.count, day: isoToSerial(a.day), dayKnown: a.dayKnown, group: nameIn(s, a.groupIndex ?? s.groups.length - 1) }, today.value)],
      dates: firstDate(stage, a.day, a.dayKnown),
    })
  }
  if (a.type === 'loss') {
    const term = a.kind !== 'not_hatched' && lossTakesOff(a.kind, props.day.settings.subtractPreserved) ? -a.count : null
    const r = planTerm(s, a.groupIndex, { stage, kind: a.kind, count: a.count, term, ids: a.ids })
    if (!r.ok) return fail(r)
    return apply(r.plan, { notes: [eventNote({ stage, kind: a.kind, count: a.count, ids: a.ids, lifestage: a.lifestage, group: nameIn(s, a.groupIndex) }, today.value)] })
  }
  if (a.type === 'correction') {
    const r = planCorrection(s, a.groupIndex, a.total, a.reason)
    if (!r.ok) return fail(r)
    const e = r.plan.events[0]
    return apply(r.plan, { notes: [eventNote({ stage, kind: 'correction', count: e.count, term: e.term, total: a.total, reason: a.reason, group: nameIn(s, a.groupIndex ?? s.groups.length - 1) }, today.value)] })
  }
  if (a.type === 'formula') return apply(formulaPlan(s, a.groups))
  if (a.type === 'regroup' || a.type === 'split') {
    const r = a.type === 'regroup' ? planRegroup(s, a.targets, a.labels) : planSplit(s, a.index, a.count, a.label)
    if (!r.ok) return fail(r)
    const after = r.plan.counts[s.field]!
    const names = r.plan.groups[0]?.list.map((g, i) => `${g.label ?? groupName(null, i)} ${after[i] ? after[i].reduce((x, y) => x + y, 0) : 0}`) ?? []
    return apply(r.plan, { notes: [`${STAGE_NOUN[stage]} regrouped: ${names.join(', ')}`] })
  }
  if (a.type === 'moveOn') {
    const next = NEXT[stage]
    if (!next) return
    const r = planMoveOn(s, stateOf(next), a.items)
    if (!r.ok) return fail(r)
    const grouped = r.plan.counts[STAGE_FIELD[next]]!.length > 1
    const fromName = (id: string | null | undefined) => {
      const at = s.meta.findIndex(m => m && m.id === id)
      return at >= 0 ? groupName(s.meta[at], at) : null
    }
    return apply(r.plan, {
      notes: r.plan.events.map(e =>
        eventNote({ stage: e.stage, kind: e.kind, count: e.count, group: grouped || e.kind === 'not_hatched' ? fromName(e.kind === 'not_hatched' ? e.groupId : e.fromGroupId) : null }, today.value),
      ),
      dates: firstDate(next, undefined),
    })
  }
}
const STAGE_NOUN: Record<Stage, string> = { egg: 'eggs', larva: 'larvae', pupa: 'pupae', adult: 'adults' }
/** The formula typed by hand: its groups by position (new ones named by letter, those beyond it gone) and a line of history. */
function formulaPlan(s: CountState | { field: CountField; stage: null; groups: Groups; meta: (GroupRow | null)[] }, groups: Groups): Plan {
  const before = s.groups
  const named = s.meta.some(Boolean) || groups.length > 1
  const list: StepGroupItem[] = []
  if (s.stage && s.stage !== 'adult' && named) {
    const used = s.meta.map(m => m?.label ?? null)
    groups.forEach((_, i) => {
      const m = s.meta[i]
      if (m) list.push({ id: m.id, label: m.label })
      else {
        const label = nextLabel(used)
        used.push(label)
        list.push({ key: `f${Date.now().toString(36)}${i}`, label, originId: null })
      }
    })
  }
  return {
    counts: { [s.field]: groups },
    groups: list.length ? [{ field: s.field, list }] : [],
    events: [],
    log: [{ kind: 'formula', field: s.field, before: formulaOfGroups(before), after: formulaOfGroups(groups) }],
  }
}
/** The dissections (no events): + and the total add a term; the formula as typed. */
function actPlain(field: string, a: StageAct) {
  const groups = readCount(countOf(field)).groups
  if (a.type === 'gain' || a.type === 'correction') {
    const term = a.type === 'gain' ? a.count : a.total - groups.flat().reduce((x, y) => x + y, 0)
    const r = appendInGroup(groups, null, term)
    if (!r.ok) return failIn(field, r.reason)
    return apply({ counts: { [field]: r.groups }, groups: [], events: [], log: [] })
  }
  if (a.type === 'formula') return apply(formulaPlan({ field: field as CountField, stage: null, groups, meta: [] }, a.groups))
}

// --- A term opened from its chip: its event, to correct, give photos, or delete with its term
const opened2 = ref<(TermRef & { stage: Stage | null; field: string }) | null>(null)
const me = useSession()
const mayChange = (e: ClutchEvent | null) => !e || e.actor === me.user?.id || ['reviewer', 'admin'].includes(me.user?.role ?? '')
const openedPhotos = computed(() => (opened2.value?.event ? recordPhotos.value.filter(p => p.eventId === opened2.value!.event!.id) : []))
const openedGroup = computed(() => {
  const o = opened2.value
  if (!o?.stage) return null
  const s = stateOf(o.stage)
  return s.groups.length > 1 ? groupName(s.meta[o.group], o.group) : null
})
async function saveOpened(patch: { day?: string; dayKnown?: boolean; kind?: string; note?: string | null }) {
  const e = opened2.value?.event
  if (!e) return
  try {
    await props.day.updateEvent(e.id, patch)
    opened2.value = null
  } catch (err) {
    message.value = errorText(err)
  }
}
async function removeOpened() {
  const o = opened2.value
  if (!o) return
  opened2.value = null
  const groups = readCount(countOf(o.field)).groups
  const s = o.stage ? stateOf(o.stage) : { field: o.field as CountField, stage: 'egg' as Stage, groups, meta: [] }
  const r = planRemove(s, o.group, o.index)
  if (!r.ok) return failIn(o.field, r.reason)
  setValue(o.field, groupsValue(r.groups))
  steps.value = []
  const line = o.event ? madeNotes.get(o.event.id) : undefined
  if (line) setValue('NOTES', withoutNote(get('NOTES'), line))
  try {
    if (o.event) await props.day.removeEvent(o.event.id)
    if (r.list && row.value && inSheet.value) await props.day.addStep({ recordId: row.value.id, groups: [{ field: o.field, list: r.list }] })
  } catch (err) {
    message.value = errorText(err)
  }
}

// --- The day's note (one per clutch and day, only in the app)
const notingDay = ref(false)
const savingNote = ref(false)
async function saveDayNote(text: string) {
  const r = row.value
  if (!r) return
  savingNote.value = true
  try {
    await props.day.setNote(r.id, text)
    notingDay.value = false
  } catch (err) {
    message.value = errorText(err)
  } finally {
    savingNote.value = false
  }
}
/** The groups of every stage, named, for a photo to show one. */
const photoGroups = computed(() =>
  (['egg', 'larva', 'pupa'] as Stage[]).flatMap(stage => {
    const s = stateOf(stage)
    return s.meta.flatMap((m, i) => (m ? [{ id: m.id, name: `${STAGE_TAB[stage]()} · ${groupName(m, i)}` }] : []))
  }),
)
/** 📷 in a stage: of the one group selected, else of the clutch today. */
function stagePhoto(stage: Stage) {
  const s = stateOf(stage)
  const sel = selected.value[stage] ?? []
  const m = sel.length === 1 ? s.meta[sel[0]] : null
  addingPhoto.value = { day: todayIso(), eventId: null, groupId: m?.id ?? null }
}
/** A group's photos (and its earlier stage's, the group it came from), or the camera for its first. */
function groupPhotos(stage: Stage, index: number) {
  const m = stateOf(stage).meta[index]
  if (!m) return
  const photos = recordPhotos.value.filter(p => p.groupId === m.id || (m.originId && p.groupId === m.originId))
  if (photos.length) viewingPhotos.value = { photos, index: 0, event: null, groupId: m.id }
  else addingPhoto.value = { day: todayIso(), eventId: null, groupId: m.id }
}
/** What is left of each group (eggs not hatched yet, larvae not pupated) and the groups of the stage before, for a panel. */
function leftIn(stage: Stage) {
  const next = NEXT[stage]
  return next ? leftOf(stateOf(stage), recordEvents.value, next) : undefined
}
function sourcesOf(stage: Stage) {
  const from = BEFORE[stage]
  if (!from) return undefined
  const s = stateOf(from)
  if (!s.meta.some(Boolean)) return undefined
  const left = leftOf(s, recordEvents.value, stage)
  return s.groups.map((_, i) => ({ index: i, name: groupName(s.meta[i], i), left: left[i] ?? 0 }))
}
/** The sheet's totals as the person sees them, for the timeline's numbers. */
const stageTotals = computed(() => {
  const out: Partial<Record<Stage, number | null>> = {}
  for (const s of STAGES) {
    const c = readCount(countOf(s.count))
    out[s.stage] = c.terms.length ? totalOf(c.terms) : null
  }
  return out
})

// --- What should be in the cage today, and when the next stages come
/** The latest laid, hatched and pupated days of this clutch's events (the timeline loads them). */
const record = useClutchRecord(computed(() => row.value?.id ?? ''), props.day)
const recordEvents = computed<ClutchEvent[]>(() => record.data.value?.events ?? [])
const recordPhotos = computed<ClutchPhoto[]>(() => record.data.value?.photos ?? [])
const gains = computed(() => latestGains(recordEvents.value))
/** A stage's preserved ones kept counted, shown beside its sum (none when the team takes them off). */
const preservedOf = (stage: Stage) =>
  preservedApart(row.value ? props.day.tallies.value[row.value.id] : undefined, stage, props.day.settings.subtractPreserved)
const speciesName = computed(() => (isBlank(get('SPECIES')) ? '' : String(get('SPECIES'))))
const days = computed(() => props.durations.of(speciesName.value))
const view = computed(() =>
  outlook(
    get,
    f => readCount(countOf(f as CountField)),
    row.value ? props.day.tallies.value[row.value.id] : undefined,
    props.day.settings.subtractPreserved,
    days.value,
    gains.value,
    today.value,
  ),
)
const ahead = computed(() => {
  if (clutchState.value?.ended) return []
  const p = view.value.predicted
  return [
    { key: 'hatch', name: t('eclosión'), day: p.hatch },
    { key: 'pupa', name: t('pupa'), day: p.pupa },
    { key: 'emerge', name: t('emergencia'), day: p.emerge },
  ].filter((x): x is { key: string; name: string; day: number } => x.day !== null)
})
const inCage = computed(() => {
  const e = view.value.expected
  return [
    e.eggs ? t('{n} huevos sin eclosionar', { n: e.eggs }) : '',
    e.larvae !== null ? t('{n} larvas', { n: e.larvae }) : '',
    e.pupae ? t('{n} pupas', { n: e.pupae }) : '',
  ].filter(Boolean)
})
const daysText = computed(() =>
  t('{species}: huevo {egg} d · larva {larva} d · pupa {pupa} d ({from})', {
    species: speciesName.value || t('sin especie'),
    egg: days.value.egg,
    larva: days.value.larva,
    pupa: days.value.pupa,
    from: days.value.from === 'species' ? t('sus clutches') : days.value.from === 'genus' ? t('su género') : t('todos los clutches'),
  }),
)

// --- One stage at a time: the tab of the stage the clutch is in, unless another is chosen
const STAGE_TAB: Record<Stage, () => string> = { egg: () => t('Huevos'), larva: () => t('Larvas'), pupa: () => t('Pupas'), adult: () => t('Adultos') }
const tab = ref<Stage>('egg')
const tabDirty = (s: (typeof STAGES)[number]) => dirty(s.count) || (!!s.date && dirty(s.date))
/** The rest, folded under the stage: notes and parents, history and photos, the other columns. */
const folds = ref({ notes: false, history: false, more: false })
const notesPreview = computed(() => {
  const p = parentsOf(get('NOTES'))
  const last = notesOf(get('NOTES')).at(-1)
  return [p ? `${p.female}♀ + ${p.male}♂` : '', last ? noteParts(last).text : ''].filter(Boolean).join(' · ')
})
const notesCount = computed(() => notesOf(get('NOTES')).length)
const historyPreview = computed(() =>
  [
    recordEvents.value.length ? tn(recordEvents.value.length, '{n} evento', '{n} eventos') : '',
    recordPhotos.value.length ? tn(recordPhotos.value.length, '{n} foto', '{n} fotos') : '',
  ]
    .filter(Boolean)
    .join(' · ') || t('Nada todavía.'),
)
const morePreview = computed(() =>
  [isBlank(get('Generation')) ? '' : String(get('Generation')), String(get('INSECTARY OR LABORATORY') ?? ''), speciesName.value].filter(Boolean).join(' · '),
)

// --- Photos of a chip's event: its photos to see (and add to), or the camera for its first
const addingPhoto = ref<{ day: string; eventId: string | null; groupId?: string | null } | null>(null)
const viewingPhotos = ref<{ photos: ClutchPhoto[]; index: number; event: ClutchEvent | null; groupId?: string | null } | null>(null)
function photoRemoved(id: string) {
  record.photoRemoved(id)
  const v = viewingPhotos.value
  if (v) viewingPhotos.value = { ...v, photos: v.photos.filter(p => p.id !== id), index: Math.max(0, Math.min(v.index, v.photos.length - 2)) }
}

// --- Larvae (or eggs) preserved, registered in Insectary_data with Emergidos' cards
const preserving = ref<{ count: number; lifestage: string; done: (ids: string[]) => void } | null>(null)
const inSheet = computed(() => !!row.value && !row.value.id.startsWith('staged:'))
function preserved(result: { ids: string[] }) {
  const p = preserving.value
  preserving.value = null
  p?.done(result.ids)
  notify(t('{n} en Insectary_data (en la app): {ids}', { n: result.ids.length, ids: result.ids.join(', ') }), 'success')
}

function go(step: number) {
  const next = index.value + step
  if (next >= 0 && next < props.rows.length) index.value = next
}

// --- The keyboard: the box typed in stays in view, the buttons above the keyboard
const root = ref<HTMLElement>()
const scroller = ref<HTMLElement>()
const rootBottom = ref(0)
const measure = () => (rootBottom.value = root.value?.getBoundingClientRect().bottom ?? 0)
/** Docked beside the list, the keyboard covers the bottom of the pane: lift the footer that much. */
const lift = computed(() => (props.docked && keyboard.open.value ? Math.max(0, Math.round(rootBottom.value - keyboard.visibleBottom.value)) : 0))
function reveal(e: FocusEvent) {
  const el = e.target as HTMLElement
  if (!el.matches('input, textarea')) return
  measure()
  setTimeout(() => el.scrollIntoView({ block: 'center', behavior: 'smooth' }), 350)
}
/** The box being typed in, back in the middle of what is left once the keyboard has settled. */
let settle: ReturnType<typeof setTimeout> | undefined
watch([keyboard.visibleBottom, keyboard.visibleTop], () => {
  measure()
  clearTimeout(settle)
  settle = setTimeout(() => {
    const el = document.activeElement as HTMLElement | null
    if (el && scroller.value?.contains(el) && el.matches('input, textarea')) el.scrollIntoView({ block: 'center' })
  }, 150)
})
onMounted(measure)
const sizes = typeof ResizeObserver === 'undefined' ? null : new ResizeObserver(measure)
onMounted(() => root.value && sizes?.observe(root.value))
onBeforeUnmount(() => sizes?.disconnect())
/**
 * A phone on its side with the keyboard up leaves a strip of a couple of hundred pixels:
 * the header and the buttons at the bottom step aside for the box being typed in (and the
 * count's own +, − and Counted next to it); they come back when the keyboard closes.
 */
const tight = computed(() => keyboard.open.value && keyboard.visibleBottom.value - keyboard.visibleTop.value < 360)
const overlayStyle = computed(() =>
  props.docked ? undefined : { top: `${keyboard.visibleTop.value}px`, height: `${keyboard.visibleBottom.value - keyboard.visibleTop.value}px` },
)
const stageName: Record<string, () => string> = {
  egg: () => t('Huevos'),
  larva: () => t('Larvas'),
  pupa: () => t('Pupas'),
  adult: () => t('Emergiendo'),
}
// A clutch opened (or the next one): what it was like then, the boxes empty.
watch(
  () => row.value?.id,
  () => {
    const r = row.value
    touched.value = {}
    message.value = ''
    verifying.value = false
    verifyNote.value = ''
    opened.value = r ? { id: r.id, values: Object.fromEntries(props.columns.map(c => [c.key, norm(c.key, current(c.key))])) } : null
    tab.value = clutchState.value?.stage ?? 'egg'
    folds.value = { notes: false, history: false, more: false }
    addingPhoto.value = null
    viewingPhotos.value = null
    selected.value = {}
    steps.value = []
    madeNotes.clear()
    opened2.value = null
    notingDay.value = false
    nextTick(() => scroller.value?.scrollTo({ top: 0 }))
  },
  { immediate: true },
)
const endedText = (e: ClutchState['ended']) =>
  e === 'old'
    ? t('más de 90 días')
    : e === 'never'
      ? t('una etapa en NA')
      : e === 'none-left'
        ? t('ya no queda ninguno')
        : e === 'emerged'
          ? t('ya emergieron')
          : e === 'note'
            ? t('las notas lo dan por terminado')
            : ''
</script>

<template>
  <div
    v-if="row"
    ref="root"
    :class="docked ? 'flex h-full min-h-0 flex-col bg-white' : 'fixed inset-x-0 z-40 flex flex-col bg-white'"
    :style="overlayStyle"
    :role="docked ? 'region' : 'dialog'"
    :aria-label="$t('Clutch {clutch}', { clutch: label })"
  >
    <header v-show="!tight" class="flex items-center gap-1 border-b border-stone-200 px-1 py-1">
      <button class="grid h-11 w-11 place-items-center rounded-md text-stone-700 disabled:opacity-30" :disabled="index === 0" :aria-label="$t('Anterior')" @click="go(-1)">
        <ChevronLeft :size="24" />
      </button>
      <div class="min-w-0 flex-1 text-center">
        <p class="truncate text-xl leading-tight font-semibold">
          {{ $t('Clutch {clutch}', { clutch: label }) }}
          <span v-if="!isBlank(get('Generation')) && get('Generation') !== 'NA'" class="ml-1 rounded bg-violet-100 px-1.5 text-sm font-medium text-violet-800">{{ get('Generation') }}</span>
        </p>
        <p class="truncate text-xs text-stone-500">
          {{ isBlank(get('SPECIES')) ? $t('sin especie') : get('SPECIES') }} · {{ $t('fila {row}', { row: row.row }) }}
        </p>
      </div>
      <button
        class="grid h-11 w-11 place-items-center rounded-md text-stone-700 disabled:opacity-30"
        :disabled="index >= rows.length - 1"
        :aria-label="$t('Siguiente')"
        @click="go(1)"
      >
        <ChevronRight :size="24" />
      </button>
      <button v-if="!docked" class="grid h-11 w-11 place-items-center rounded-md text-stone-700" :aria-label="$t('Cerrar')" @click="emit('close')">
        <X :size="22" />
      </button>
    </header>
    <p v-if="message" class="bg-red-50 px-4 py-2 text-sm text-red-800" role="alert">{{ message }}</p>
    <div
      ref="scroller"
      class="min-h-0 flex-1 overflow-y-auto px-4 pb-6"
      :style="lift ? { paddingBottom: `${lift + 24}px` } : undefined"
      @focusin="reveal"
    >
      <div class="mx-auto max-w-2xl">
      <div class="flex flex-wrap items-center gap-1.5 pt-3 text-xs">
        <span v-if="clutchState?.stage" class="rounded-full bg-stone-100 px-2 py-0.5 font-medium text-stone-700">{{ stageName[clutchState.stage]() }}</span>
        <span v-if="clutchState?.ended" class="rounded-full bg-stone-200 px-2 py-0.5 text-stone-700">{{ $t('Terminado: {why}', { why: endedText(clutchState.ended) }) }}</span>
        <span
          v-if="checkedLine"
          class="flex items-center gap-1 rounded-full px-2 py-0.5 font-medium"
          :class="status?.review === 'verify' ? 'bg-orange-100 text-orange-900 ring-1 ring-orange-300' : 'bg-brand-50 text-brand-800'"
        >
          <AlertTriangle v-if="status?.review === 'verify'" :size="12" /><Check v-else :size="12" /> {{ checkedLine }}
        </span>
        <span v-if="status?.changed" class="rounded-full bg-amber-100 px-2 py-0.5 font-medium text-amber-900">{{ $t('Cambiado hoy') }}</span>
        <span class="text-stone-500">{{ get('INSECTARY OR LABORATORY') || '' }}</span>
      </div>
      <p v-if="!canEdit" class="mt-2 rounded bg-stone-100 px-3 py-2 text-sm text-stone-700">{{ $t('Solo lectura') }}</p>
      <!-- Today in the cage, and the dates expected (from the species' usual days). -->
      <div v-if="!clutchState?.ended && (inCage.length || ahead.length)" class="mt-2 rounded-lg border border-sky-200 bg-sky-50/60 px-3 py-2 text-sm" role="status">
        <p v-if="inCage.length">
          <span class="font-medium">{{ $t('Para contar hoy') }}:</span> <span class="tabular-nums">{{ inCage.join(' · ') }}</span>
        </p>
        <p v-if="ahead.length" class="mt-0.5 flex flex-wrap items-center gap-x-3">
          <CalendarClock :size="14" class="-mr-1.5 text-sky-800" />
          <span v-for="a in ahead" :key="a.key" class="tabular-nums" :class="a.day <= today ? 'font-semibold text-brand-800' : ''">
            {{ a.name }} ≈ {{ formatSerial(a.day) }}<template v-if="a.day <= today"> ({{ a.day === today ? $t('hoy') : $t('ya') }})</template>
          </span>
        </p>
        <p class="mt-0.5 text-[11px] text-stone-600">{{ daysText }}</p>
      </div>

      <!-- One stage at a time: each count on its tab, the clutch's stage chosen when it opens. -->
      <div class="sticky top-0 z-10 -mx-4 mt-2 border-b border-stone-200 bg-white px-4 pt-1" role="tablist" :aria-label="$t('Etapa')">
        <div class="grid grid-cols-4 gap-1">
          <button
            v-for="s in STAGES"
            :key="s.stage"
            type="button"
            role="tab"
            class="relative flex h-14 flex-col items-center justify-center rounded-t-lg border-b-[3px] leading-tight"
            :class="tab === s.stage ? 'border-brand-700 bg-brand-50 text-brand-900' : 'border-transparent text-stone-600 active:bg-stone-50'"
            :aria-selected="tab === s.stage"
            :aria-controls="`clutch-stage-${s.stage}`"
            @click="tab = s.stage"
          >
            <span class="text-xs font-medium">{{ STAGE_TAB[s.stage]() }}</span>
            <span class="text-xl font-semibold tabular-nums">{{ readCount(countOf(s.count)).na ? 'NA' : (stageTotals[s.stage] ?? '—') }}</span>
            <span v-if="tabDirty(s)" class="absolute top-1.5 right-2 size-2 rounded-full bg-amber-500" :title="$t('Sin guardar')" />
          </button>
        </div>
      </div>

      <!-- The stage chosen: its count kept as a sum, and the date it started (the others stay, hidden, with their undo). -->
      <section
        v-for="s in STAGES"
        v-show="tab === s.stage"
        :id="`clutch-stage-${s.stage}`"
        :key="s.count"
        class="py-3"
        role="tabpanel"
      >
        <StagePanel
          :key="`${row.id}:${s.count}`"
          :ref="(el: unknown) => (panels[s.count] = (el ?? undefined) as InstanceType<typeof StagePanel> | undefined)"
          v-model:selected="selected[s.stage]"
          :field="s.count"
          :stage="s.stage"
          :value="countOf(s.count)"
          :saved="savedOf(s.count)"
          :dirty="dirty(s.count)"
          :editable="editable(s.count)"
          :locked="lockedFormula(row, s.count, formulas)"
          :more="MORE[s.count]()"
          :start-of-day="startOfDay(s.count)"
          :meta="s.stage === 'adult' ? [] : stateOf(s.stage).meta"
          :mismatch="s.stage !== 'adult' && stateOf(s.stage).mismatch"
          :left="leftIn(s.stage)"
          :sources="sourcesOf(s.stage)"
          :events="recordEvents"
          :photos="recordPhotos"
          :subtract-preserved="day.settings.subtractPreserved"
          :preserved="preservedOf(s.stage)"
          :note-for="e => noteFor(s.stage, e)"
          :can-register="canEdit && inSheet"
          :can-photo="canEdit && inSheet"
          :undoable="stepsOf(s.count)"
          @act="act(s.stage, s.count, $event)"
          @open="opened2 = { ...$event, stage: s.stage, field: s.count }"
          @group-photos="groupPhotos(s.stage, $event)"
          @photo="stagePhoto(s.stage)"
          @note="notingDay = true"
          @undo="undoStep"
          @back-to-morning="backToMorning(s.count, startOfDay(s.count))"
          @register="preserving = $event"
        >
          <DateRow
            v-if="s.date && has(s.date)"
            :key="`${row.id}:${s.date}`"
            :field="s.date"
            :value="get(s.date)"
            :dirty="dirty(s.date)"
            :editable="editable(s.date)"
            :first="FIRST[s.date]?.()"
            :suggest="readCount(countOf(s.count)).terms.some(n => n > 0)"
            @set="setValue(s.date, $event)"
          />
        </StagePanel>
      </section>

      <!-- Folded under the stage: notes and parents, history and photos, the other columns. -->
      <div class="divide-y divide-stone-100 border-y border-stone-200">
        <section v-if="has('NOTES')">
          <button type="button" class="flex min-h-14 w-full items-center gap-2 py-2 text-left" :aria-expanded="folds.notes" @click="folds.notes = !folds.notes">
            <span class="min-w-0 flex-1">
              <span class="flex items-center gap-1.5 text-sm font-semibold">
                {{ $t('Notas y padres') }}
                <span v-if="notesCount" class="rounded-full bg-stone-100 px-1.5 text-xs font-medium text-stone-600 tabular-nums">{{ notesCount }}</span>
                <span v-if="dirty('NOTES')" class="size-2 rounded-full bg-amber-500" :title="$t('Sin guardar')" />
              </span>
              <span v-if="!folds.notes" class="block truncate text-xs text-stone-600">{{ notesPreview || $t('Sin notas') }}</span>
            </span>
            <ChevronDown :size="20" class="shrink-0 text-stone-500 transition-transform" :class="{ 'rotate-180': folds.notes }" />
          </button>
          <ClutchNotes
            v-if="folds.notes"
            class="-mt-2"
            :notes="get('NOTES')"
            :saved="row.values.NOTES ?? null"
            :dirty="dirty('NOTES')"
            :editable="editable('NOTES')"
            :initials="initials"
            :today="today"
            :parents="parents"
            :species="isBlank(get('SPECIES')) ? '' : String(get('SPECIES'))"
            :clutch-id="row.id"
            @set="setValue('NOTES', $event)"
            @parents-written="parentsWritten"
          />
        </section>
        <section>
          <button type="button" class="flex min-h-14 w-full items-center gap-2 py-2 text-left" :aria-expanded="folds.history" @click="folds.history = !folds.history">
            <span class="min-w-0 flex-1">
              <span class="block text-sm font-semibold">{{ $t('Historia y fotos') }}</span>
              <span v-if="!folds.history" class="block truncate text-xs text-stone-600">{{ historyPreview }}</span>
            </span>
            <ChevronDown :size="20" class="shrink-0 text-stone-500 transition-transform" :class="{ 'rotate-180': folds.history }" />
          </button>
          <!-- Hatched, died, disappeared, preserved: day by day, only in the app. -->
          <ClutchTimeline
            v-if="folds.history"
            class="-mt-2 !border-b-0"
            :record-id="row.id"
            :clutch="label"
            :day="day"
            :totals="stageTotals"
            :expected="view.expected"
            :can-edit="canEdit"
            :can-photo="inSheet"
            :initials="who"
            :record="record"
          />
        </section>
        <section>
          <button type="button" class="flex min-h-14 w-full items-center gap-2 py-2 text-left" :aria-expanded="folds.more" @click="folds.more = !folds.more">
            <span class="min-w-0 flex-1">
              <span class="flex items-center gap-1.5 text-sm font-semibold">
                {{ $t('Más: disecciones, generación, sala, especie') }}
                <span
                  v-if="['NUMBER OF PUPAE/LARVAE FOR DISECTIONS', 'Generation', 'INSECTARY OR LABORATORY', 'SPECIES'].some(dirty)"
                  class="size-2 rounded-full bg-amber-500"
                  :title="$t('Sin guardar')"
                />
              </span>
              <span v-if="!folds.more" class="block truncate text-xs text-stone-600">{{ morePreview }}</span>
            </span>
            <ChevronDown :size="20" class="shrink-0 text-stone-500 transition-transform" :class="{ 'rotate-180': folds.more }" />
          </button>
          <template v-if="folds.more">
            <section class="border-t border-stone-100 py-3">
              <StagePanel
                :key="`${row.id}:dissections`"
                :ref="(el: unknown) => (panels['NUMBER OF PUPAE/LARVAE FOR DISECTIONS'] = (el ?? undefined) as InstanceType<typeof StagePanel> | undefined)"
                field="NUMBER OF PUPAE/LARVAE FOR DISECTIONS"
                :stage="null"
                :value="countOf('NUMBER OF PUPAE/LARVAE FOR DISECTIONS')"
                :saved="savedOf('NUMBER OF PUPAE/LARVAE FOR DISECTIONS')"
                :dirty="dirty('NUMBER OF PUPAE/LARVAE FOR DISECTIONS')"
                :editable="editable('NUMBER OF PUPAE/LARVAE FOR DISECTIONS')"
                :locked="lockedFormula(row, 'NUMBER OF PUPAE/LARVAE FOR DISECTIONS', formulas)"
                :more="MORE['NUMBER OF PUPAE/LARVAE FOR DISECTIONS']()"
                :undoable="stepsOf('NUMBER OF PUPAE/LARVAE FOR DISECTIONS')"
                @act="act(null, 'NUMBER OF PUPAE/LARVAE FOR DISECTIONS', $event)"
                @open="opened2 = { ...$event, stage: null, field: 'NUMBER OF PUPAE/LARVAE FOR DISECTIONS' }"
                @undo="undoStep"
              />
            </section>
            <section v-if="has('Generation')" class="border-t border-stone-100 py-3">
              <span class="field-label">Generation</span>
              <div class="grid grid-cols-4 gap-2">
                <button
                  v-for="g in generations"
                  :key="g"
                  type="button"
                  class="min-h-11 rounded-lg border px-1 text-sm font-medium"
                  :class="get('Generation') === g ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white text-stone-800'"
                  :aria-pressed="get('Generation') === g"
                  :disabled="!editable('Generation')"
                  @click="setValue('Generation', g)"
                >
                  {{ g }}
                </button>
              </div>
            </section>
            <section class="border-t border-stone-100 py-3">
              <span class="field-label">INSECTARY OR LABORATORY</span>
              <div class="grid grid-cols-2 gap-2">
                <button
                  v-for="p in ['Insectary', 'Laboratory']"
                  :key="p"
                  type="button"
                  class="min-h-11 rounded-lg border px-1 text-sm font-medium"
                  :class="get('INSECTARY OR LABORATORY') === p ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white text-stone-800'"
                  :aria-pressed="get('INSECTARY OR LABORATORY') === p"
                  :disabled="!editable('INSECTARY OR LABORATORY')"
                  @click="setValue('INSECTARY OR LABORATORY', p)"
                >
                  {{ p }}
                </button>
              </div>
            </section>
            <section class="border-t border-stone-100 py-3">
              <label class="block">
                <span class="field-label">SPECIES</span>
                <ChoiceField
                  v-if="editable('SPECIES')"
                  :model-value="isBlank(get('SPECIES')) && get('SPECIES') !== 'NA' ? '' : String(get('SPECIES'))"
                  class="field-input h-12 text-base"
                  :class="{ 'is-dirty': dirty('SPECIES') }"
                  :options="species"
                  @update:model-value="setValue('SPECIES', $event || null)"
                />
                <p v-else class="text-sm">{{ get('SPECIES') || '—' }}</p>
              </label>
            </section>
          </template>
        </section>
      </div>
      <p v-if="day.last.value[row.id]" class="pt-3 text-xs text-stone-500">
        {{ $t('Último cambio: {when} · {who}', { when: new Date(day.last.value[row.id].at).toLocaleString(), who: day.last.value[row.id].name || (day.last.value[row.id].actor === 'unknown' ? 'Google Sheets' : day.last.value[row.id].actor) }) }}
      </p>
      <button class="btn mt-3 h-11 w-full" @click="emit('more', row)">
        <Columns3 :size="16" /> {{ $t('Todas las columnas') }}
      </button>
      </div>
    </div>
    <!-- Checked, but someone should look again: why, in a few words. -->
    <div
      v-if="verifying && canEdit"
      v-show="!tight"
      class="relative z-10 border-t border-orange-200 bg-orange-50 px-3 py-2"
      :style="lift ? { transform: `translateY(-${lift}px)` } : undefined"
    >
      <label class="block text-sm font-medium text-orange-950" for="clutch-verify-note">{{ $t('¿Qué hay que verificar? (opcional)') }}</label>
      <div class="mt-1 flex flex-wrap gap-1.5">
        <button
          v-for="r in VERIFY_REASONS"
          :key="r"
          type="button"
          class="min-h-9 rounded-full border border-orange-300 bg-white px-3 text-sm text-orange-950"
          @click="verifyNote = r"
        >
          {{ r }}
        </button>
      </div>
      <div class="mt-1.5 flex gap-2">
        <input
          id="clutch-verify-note"
          v-model="verifyNote"
          class="field-input h-11 min-w-0 flex-1"
          maxlength="200"
          autocomplete="off"
          enterkeyhint="done"
          @keydown.enter.prevent="finish('verify', verifyNote)"
        />
        <button type="button" class="btn h-11 w-11 shrink-0 justify-center px-0" :aria-label="$t('Cancelar')" @click="verifying = false"><X :size="18" /></button>
      </div>
    </div>
    <footer
      v-show="!tight"
      class="relative z-10 flex items-center gap-2 border-t border-stone-200 bg-white px-3 py-2"
      :class="docked ? '' : 'pb-[calc(0.5rem+env(safe-area-inset-bottom))]'"
      :style="lift ? { transform: `translateY(-${lift}px)` } : undefined"
    >
      <span class="min-w-0 flex-1 text-xs text-stone-600">
        <template v-if="pending.saving"><Loader2 :size="12" class="inline animate-spin" /> {{ $t('Guardando…') }}</template>
        <template v-else-if="rowPending">{{ $tn(rowPending, '{n} cambio por guardar', '{n} cambios por guardar') }}</template>
        <template v-else-if="changedFields.length">{{ $t('Guardado en Google Sheets') }}</template>
        <template v-else-if="checkedLine">{{ checkedLine }}</template>
        <template v-else>{{ $t('Sin cambios') }}</template>
      </span>
      <template v-if="canEdit">
        <button
          v-if="verifying"
          class="h-12 shrink-0 rounded-lg border border-orange-400 bg-orange-100 px-3 text-sm font-semibold text-orange-950"
          :disabled="finishing"
          @click="finish('verify', verifyNote)"
        >
          <AlertTriangle :size="16" class="-mt-0.5 inline" /> {{ $t('Pedir verificación') }}
        </button>
        <template v-else>
          <button
            class="grid h-12 w-12 shrink-0 place-items-center rounded-lg border border-orange-300 text-orange-800 active:bg-orange-50"
            :aria-label="$t('Pedir que alguien lo verifique')"
            :title="$t('Pedir que alguien lo verifique')"
            :disabled="finishing"
            @click="verifying = true"
          >
            <AlertTriangle :size="18" />
          </button>
          <button class="btn-primary h-12 px-4 text-base" :disabled="finishing" @click="finish()">
            <Loader2 v-if="finishing" :size="18" class="animate-spin" /><Check v-else :size="18" />
            {{ changedFields.length || rowPending ? $t('Guardar y marcar como revisado') : status?.review === 'verify' ? $t('Marcar como verificado') : $t('Marcar como revisado') }}
          </button>
        </template>
      </template>
      <button v-else class="btn h-12 px-4" @click="emit('close')">{{ $t('Cerrar') }}</button>
    </footer>
    <!-- Photos: of an event, a group or the clutch today; to zoom, or the camera for the first. -->
    <ClutchPhotoAdd
      v-if="addingPhoto"
      :record-id="row.id"
      :clutch="label"
      :day="addingPhoto.day"
      :events="recordEvents.filter(e => e.day === addingPhoto!.day && e.kind !== 'transfer')"
      :event-id="addingPhoto.eventId"
      :groups="photoGroups"
      :group-id="addingPhoto.groupId ?? null"
      @close="addingPhoto = null"
    />
    <ClutchPhotoViewer
      v-if="viewingPhotos && viewingPhotos.photos.length"
      v-model="viewingPhotos.index"
      :photos="viewingPhotos.photos"
      :events="recordEvents"
      :initials="who"
      :can-add="canEdit && inSheet"
      @add="(addingPhoto = { day: viewingPhotos.event?.day ?? todayIso(), eventId: viewingPhotos.event?.id ?? null, groupId: viewingPhotos.groupId ?? null }), (viewingPhotos = null)"
      @removed="photoRemoved"
      @close="viewingPhotos = null"
    />
    <EventSheet
      v-if="opened2"
      :field="opened2.field"
      :term="opened2.term"
      :event="opened2.event"
      :group="openedGroup"
      :photos="openedPhotos"
      :can-edit="canEdit"
      :mine="mayChange(opened2.event)"
      :can-photo="inSheet"
      :initials="who"
      @close="opened2 = null"
      @save="saveOpened"
      @remove="removeOpened"
      @add-photo="(addingPhoto = { day: opened2.event!.day, eventId: opened2.event!.id }), (opened2 = null)"
      @view-photo="(viewingPhotos = { photos: openedPhotos, index: $event, event: opened2.event }), (opened2 = null)"
    />
    <DayNoteSheet
      v-if="notingDay"
      :clutch="label"
      :day="todayIso()"
      :text="status?.note?.text ?? ''"
      :by="status?.note ? who(status.note.updatedName || '') : null"
      :saving="savingNote"
      @close="notingDay = false"
      @save="saveDayNote"
    />
    <PreserveYoung
      v-if="preserving"
      :clutch="label"
      :count="preserving.count"
      :stage="preserving.lifestage"
      :date="todayIso()"
      @preserved="preserved"
      @close="preserving = null"
    />
  </div>
</template>
