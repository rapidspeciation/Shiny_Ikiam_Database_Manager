<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, ref, watch } from 'vue'
import { AlertTriangle, Check, ChevronRight, History, Loader2, Plus, Undo2, X } from 'lucide-vue-next'
import DateField from '../DateField.vue'
import EntryModeToggle from '../EntryModeToggle.vue'
import InsectaryIdsWarning from '../InsectaryIdsWarning.vue'
import RowDrawer from '../RowDrawer.vue'
import SexBadge from '../SexBadge.vue'
import TabHistory from '../history/TabHistory.vue'
import ClutchPicker from './ClutchPicker.vue'
import DraftCard from './DraftCard.vue'
import { useClutchDay } from '../../composables/useClutchDay'
import { heldIds, useEmergedState } from '../../composables/useEmergedState'
import type { EntryMode } from '../../composables/useEntryMode'
import { useKeyboard, useMedia } from '../../composables/usePhone'
import { api, ApiError, requestId } from '../../lib/api'
import { isBlank } from '../../lib/cells'
import { countCell, readCount, totalOf, type Count } from '../../lib/clutches'
import { dayFirst, dayLabel, formatSerial, isoToSerial, serialFromIso, serialToIso, todayIso } from '../../lib/dates'
import { bestRack, buildIndex, searchKey, usedSamples, type RackSuggestion } from '../../lib/deaths'
import {
  CROSS_PURPOSE,
  DEFAULT_STAGE,
  MODULE,
  STOCKS,
  dayDoubt,
  draftValues,
  idProblem,
  isAdult,
  knownSpecies,
  nextId,
  preserving,
  siblingSpecies,
  skippedIds,
  stockPlan,
  tallies,
  type Draft,
  type Kind,
  type Sex,
  type StockPlan,
} from '../../lib/emerged'
import { errorText, notify } from '../../lib/notice'
import { initialsOf } from '../../lib/rows'
import type { CellValue, Table, TableRow } from '../../lib/types'
import { useSession } from '../../stores/session'
import { type ServerRecord, useTables } from '../../stores/tables'
import { t, tn } from '../../lib/i18n'

/**
 * Emergidos as cards, for the round in the insectary (phones first): the day,
 * the clutch (those emerging and with pupae first), then «+♀» / «+♂» add a
 * card per butterfly with the next free Insectary ID to write on its wing (the
 * ID can be changed to what the wing says). Each card can say it was deformed,
 * died or was preserved that day, what emerged when it is not the clutch's
 * species, and a note; eggs and larvae preserved from the clutch get their
 * cards too. One Save writes the rows (into the pre-made rows of their IDs)
 * and, for each clutch, its adults as one more term of NUMBER OF ADULTS (as
 * Clutches counts them), all or nothing, with an Undo; «Historial» lists the
 * tab's saves. The cards are shared with the table (useEmergedState).
 */
const props = defineProps<{
  table: Table | undefined
  stocks: Table | undefined
  ready: boolean
  options: Record<string, string[]>
  collectors: string[]
  createFormulas: string[]
}>()
const mode = defineModel<EntryMode>('mode', { required: true })

const session = useSession()
const tables = useTables()
const state = useEmergedState()
const { date, clutch, drafts, skipStock, medium, freeIds, inOrder, rowOf, idsLoaded } = state
const day = useClutchDay()
const keyboard = useKeyboard()
const roomy = useMedia('(min-width: 1024px)')
const canEdit = computed(() => session.canEdit)
const today = computed(() => isoToSerial(todayIso()))
const initials = computed(() => initialsOf(session.user?.displayName || '', props.collectors, session.user?.username || ''))

// --- The clutches (Insectary_stocks) and what Insectary_data already holds of each
const stockRows = computed(() => (props.stocks?.rows || []).filter(r => r.observed))
const stockByClutch = computed(() => {
  const out = new Map<string, TableRow>()
  for (const r of stockRows.value) {
    const n = String(r.values['CLUTCH NUMBER'] ?? '').trim()
    if (n) out.set(n, r)
  }
  return out
})
const stockOf = (c: string) => stockByClutch.value.get(c.trim())
const speciesOfClutch = (c: string) => knownSpecies(stockOf(c)?.values.SPECIES)
const registered = computed(() => {
  const out = new Map<string, { n: number; last: number | null }>()
  for (const r of props.table?.rows || []) {
    if (!r.observed) continue
    const c = String(r.values['CLUTCH NUMBER'] ?? '').trim()
    if (!c || c === 'NA') continue
    const e = out.get(c) ?? { n: 0, last: null }
    e.n++
    const d = r.values.Intro2Insectary_date
    if (typeof d === 'number' && (e.last === null || d > e.last)) e.last = d
    out.set(c, e)
  }
  return out
})
const draftsByClutch = computed(() => {
  const out = new Map<string, number>()
  for (const d of drafts.value) out.set(d.clutch, (out.get(d.clutch) ?? 0) + 1)
  return out
})
/** Every species written in Insectary_data, for the other subspecies and the species list. */
const knownList = computed(() => {
  const seen = new Set<string>()
  for (const r of props.table?.rows || []) if (r.observed && !isBlank(r.values.SPECIES)) seen.add(String(r.values.SPECIES).trim())
  for (const s of props.options.SPECIES || []) seen.add(s)
  seen.delete('NA')
  return [...seen].sort()
})
const siblingsOf = (c: string) => siblingSpecies(speciesOfClutch(c), knownList.value)

const picking = ref(false)
const showPicker = computed(() => !clutch.value || picking.value || !stockOf(clutch.value))
function pick(c: string) {
  clutch.value = c
  picking.value = false
  nextTick(() => scroller.value?.scrollTo({ top: 0 }))
}
const current = computed(() => stockOf(clutch.value))
const currentSpecies = computed(() => speciesOfClutch(clutch.value))
const countOf = (row: TableRow, field: string): Count => readCount(countCell(row, field, (r, f) => r.values[f] ?? null, false, day.sums.value[row.id]))
const totalIn = (row: TableRow | undefined, field: string) => {
  if (!row) return null
  const c = countOf(row, field)
  return c.na ? 'NA' : c.terms.length ? totalOf(c.terms) : null
}

// --- The day
const quickDates = computed(() => [
  { iso: todayIso(), name: t('Hoy') },
  { iso: serialToIso(today.value - 1), name: t('Ayer') },
])
const daySerial = computed(() => serialFromIso(date.value))
const dateError = computed(() => (date.value && daySerial.value === null ? t('Fecha no válida: el año debe estar entre 1990 y 2099') : ''))
const doubt = computed(() => {
  const row = current.value
  if (daySerial.value === null) return ''
  const why = dayDoubt(daySerial.value, today.value, row?.values['DATE LAID'] ?? null, row?.values['PUPA DATE'] ?? null)
  if (why === 'future') return t('El día es posterior a hoy')
  if (why === 'before-laid') return t('El día es anterior a la puesta del clutch ({date})', { date: formatSerial(row!.values['DATE LAID'] as number) })
  if (why === 'before-pupa') return t('El día es anterior a la primera pupa del clutch ({date}): ¿mes equivocado?', { date: formatSerial(row!.values['PUPA DATE'] as number) })
  return ''
})
/** Cards of another day than the one shown: offered to move to it (the day was chosen after adding them). */
const otherDay = computed(() => drafts.value.filter(d => d.date !== date.value))
function moveToDay() {
  drafts.value = drafts.value.map(d => (d.date === date.value ? d : { ...d, date: date.value }))
}

// --- Adding cards
const held = computed(() => heldIds(drafts.value))
const next = computed(() => (idsLoaded.value ? nextId(inOrder.value, freeIds.value[0] ?? '', held.value) : null))
const skipped = computed(() => skippedIds(inOrder.value, held.value))
const fresh = ref<string[]>([])
let freshTimer: ReturnType<typeof setTimeout> | undefined
onBeforeUnmount(() => clearTimeout(freshTimer))

function add(kind: Kind, sex: Sex, count = 1) {
  if (!clutch.value) return notify(t('Elige el clutch'))
  if (dateError.value || !date.value) return notify(dateError.value || t('Elige el día'), 'error')
  if (!idsLoaded.value) return notify(t('Cargando los Insectary IDs libres…'))
  const added: Draft[] = []
  for (let i = 0; i < count; i++) {
    const id = nextId(inOrder.value, freeIds.value[0] ?? '', [...held.value, ...added.map(d => d.id)])
    if (!id) {
      notify(t('No quedan filas preasignadas libres: crea más filas preasignadas en Insectary_data'), 'error')
      break
    }
    added.push({
      key: requestId(),
      id,
      clutch: clutch.value,
      date: date.value,
      kind,
      sex: kind === 'young' ? 'NA' : sex,
      fate: 'alive',
      species: '',
      stage: kind === 'young' ? DEFAULT_STAGE : '',
      foundDead: false,
      note: '',
      cam: '',
      tube: '',
    })
  }
  if (!added.length) return
  drafts.value = [...drafts.value, ...added]
  lastSave.value = null
  fresh.value = added.map(d => d.key)
  clearTimeout(freshTimer)
  freshTimer = setTimeout(() => (fresh.value = []), 1200)
}
const many = ref<{ open: boolean; female: number | null; male: number | null; none: number | null }>({ open: false, female: null, male: null, none: null })
function addMany() {
  const { female, male, none } = many.value
  const n = (v: number | null) => Math.max(0, Math.min(60, Math.floor(v ?? 0)))
  if (!n(female) && !n(male) && !n(none)) return notify(t('Indica cuántas hembras, machos o sin sexo emergieron'))
  add('adult', 'female', n(female))
  add('adult', 'male', n(male))
  add('adult', 'NA', n(none))
  many.value = { open: false, female: null, male: null, none: null }
}
function update(key: string, patch: Partial<Draft>) {
  drafts.value = drafts.value.map(d => (d.key === key ? { ...d, ...patch } : d))
  delete refused.value[key]
}
function remove(key: string) {
  drafts.value = drafts.value.filter(d => d.key !== key)
}
function removeClutch(c: string) {
  drafts.value = drafts.value.filter(d => d.clutch !== c)
}

/** The cards by clutch: the chosen clutch first (its newest card on top, under the buttons), then the others. */
const sections = computed(() => {
  const by = new Map<string, Draft[]>()
  for (const d of drafts.value) by.set(d.clutch, [...(by.get(d.clutch) ?? []), d])
  const order = [...by.keys()].sort((a, b) => (a === clutch.value ? -1 : b === clutch.value ? 1 : 0))
  return order.map(c => ({ clutch: c, cards: [...by.get(c)!].reverse() }))
})

// --- CAM and tube of preserved bodies: the next free ones, never one in the sheet or on another card
const index = computed(() => (props.table ? buildIndex(props.table.rows) : []))
const usedSampleIds = computed(() => usedSamples(index.value))
const usedIds = computed(() => new Set(index.value.map(e => e.key)))
const freeSet = computed(() => new Set(freeIds.value.map(id => id.toUpperCase())))
/** Cards whose CAM or tube was suggested once: a box the person empties stays empty. */
const suggested = new Set<string>()
let camStart = ''
let racks: (RackSuggestion & { label: string })[] = []
async function suggestSamples() {
  const want = drafts.value.filter(d => preserving(d) && !suggested.has(d.key) && (!d.cam || !d.tube))
  if (!want.length) return
  try {
    if (!camStart || !racks.length) {
      const [cam, tube] = await Promise.all([
        api<{ suggestions: { value: string }[] }>('ids?kind=cam'),
        api<{ suggestions: (RackSuggestion & { label: string })[] }>('ids?kind=tube'),
      ])
      camStart = cam.suggestions[0]?.value || ''
      racks = tube.suggestions
    }
    // Eggs, larvae and cross offspring go to the crosses' rack (lib/deaths bestRack).
    const rows = want.map(d => ({ values: { Research_purpose: d.kind === 'young' ? CROSS_PURPOSE : '' } }) as unknown as TableRow)
    const rack = bestRack(racks, rows, medium.value)
    const count = drafts.value.filter(preserving).length + 4
    const run = (kind: string, start: string) =>
      api<{ sequence: string[] }>(`ids?kind=${kind}&start=${encodeURIComponent(start)}&count=${count}`).then(r => r.sequence)
    const [cams, tubes] = await Promise.all([camStart ? run('cam', camStart) : [], rack ? run('tube', rack.value) : []])
    const taken = (field: 'cam' | 'tube', v: string) => drafts.value.some(d => searchKey(d[field]) === searchKey(v))
    let list = drafts.value
    for (const w of want) {
      const d = list.find(x => x.key === w.key)
      if (!d) continue
      const patch: Partial<Draft> = {}
      if (!d.cam) patch.cam = cams.find(v => !taken('cam', v) && !list.some(o => o.cam === v)) ?? ''
      if (!d.tube) patch.tube = tubes.find(v => !taken('tube', v) && !list.some(o => o.tube === v)) ?? ''
      list = list.map(x => (x.key === d.key ? { ...x, ...patch } : x))
      suggested.add(d.key)
    }
    drafts.value = list
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
watch(() => drafts.value.filter(d => preserving(d) && (!d.cam || !d.tube)).map(d => d.key).join(), () => void suggestSamples(), { immediate: true })

// --- What is wrong with each card
const refused = ref<Record<string, string>>({})
const ID_PROBLEM: Record<string, (id: string) => string> = {
  empty: () => t('Falta el Insectary ID'),
  repeated: id => t('{id} está en otra tarjeta', { id }),
  used: id => t('{id} ya es una mariposa de Insectary_data', { id }),
  'not-free': id => t('{id} no es una fila preasignada libre de Insectary_data', { id }),
}
function problemsOf(d: Draft): string[] {
  const out: string[] = []
  if (idsLoaded.value) {
    const others = drafts.value.filter(o => o.key !== d.key).map(o => o.id)
    const p = idProblem(d.id, others, freeSet.value, usedIds.value)
    if (p) out.push(ID_PROBLEM[p](d.id.trim().toUpperCase()))
  }
  if (!speciesOfClutch(d.clutch) && !d.species) out.push(t('El clutch no tiene especie: elige qué emergió'))
  if (serialFromIso(d.date) === null) out.push(t('Fecha no válida: el año debe estar entre 1990 y 2099'))
  if (preserving(d)) {
    for (const field of ['cam', 'tube'] as const) {
      const v = searchKey(d[field])
      const name = field === 'cam' ? 'CAM' : t('tubo')
      if (!v) out.push(field === 'cam' ? t('Falta el CAM') : t('Falta el tubo'))
      else if (usedSampleIds.value.has(v)) out.push(t('{value} ya está en {id}', { value: v, id: usedSampleIds.value.get(v)! }))
      else if (drafts.value.some(o => o.key !== d.key && preserving(o) && searchKey(o[field]) === v)) out.push(t('{name} {value} repetido en otra tarjeta', { name, value: v }))
    }
  }
  if (refused.value[d.key]) out.push(refused.value[d.key])
  return out
}
const problems = computed(() => new Map(drafts.value.map(d => [d.key, problemsOf(d)])))
/** An ID from an empty row higher up than the next one: right when the wing says so (it once sent a batch to A0D by mistake). */
function hintOf(d: Draft): string {
  const id = d.id.trim().toUpperCase()
  const first = freeIds.value[0]?.toUpperCase()
  const row = rowOf.value.get(id)
  const firstRow = first ? rowOf.value.get(first) : undefined
  if (!first || row === undefined || firstRow === undefined || row >= firstRow) return ''
  // Said once, on the first card of a run (A0D, not A1D after it).
  const before = inOrder.value[inOrder.value.findIndex(x => x.toUpperCase() === id) - 1]?.toUpperCase()
  if (before && held.value.includes(before)) return ''
  return t('{id} es una fila vacía más arriba en la hoja (fila {row}), no la siguiente ({next}).', { id, row, next: first })
}

// --- The clutches' rows: adults as one more term, eggs and larvae preserved taken off
interface StockLine {
  clutch: string
  row: TableRow | undefined
  plan: StockPlan | null
  adults: number
  /** NUMBER OF ADULTS changed today already (in Clutches): adding again may count them twice. */
  changedToday: boolean
  on: boolean
}
const stockLines = computed<StockLine[]>(() =>
  tallies(drafts.value).map(tally => {
    const row = stockOf(tally.clutch)
    const plan = row
      ? stockPlan(tally, f => countOf(row, f), f => day.sums.value[row.id]?.[f] ?? row.values[f] ?? null, { today: today.value, initials: initials.value })
      : null
    const changedToday = !!row && day.today(row.id).changes.some(c => c.field === 'NUMBER OF ADULTS')
    return {
      clutch: tally.clutch,
      row,
      plan,
      adults: tally.adults.reduce((n, [, k]) => n + k, 0),
      changedToday,
      on: !!plan?.cells.length && !skipStock.value.includes(tally.clutch),
    }
  }),
)
function toggleStock(c: string) {
  skipStock.value = skipStock.value.includes(c) ? skipStock.value.filter(x => x !== c) : [...skipStock.value, c]
}
const shown = (v: CellValue, field: string) =>
  field === 'EMERGENCE DATE' && typeof v === 'number' ? formatSerial(v) : v === null || v === '' ? '—' : String(v)
function cellText(field: string, before: CellValue, value: CellValue) {
  if (field === 'NOTES') return `NOTES + «${String(value).slice(String(before ?? '').length).replace(/^\s*\|\s*/, '')}»`
  if (field.startsWith('NUMBER OF')) {
    const after = readCount(value)
    return `${field} ${shown(before, field)} → ${String(value)} (${totalOf(after.terms)})`
  }
  return `${field} ${shown(before, field)} → ${shown(value, field)}`
}

// --- Save, then Undo
const blocker = computed(() => {
  if (!drafts.value.length) return t('Añade al menos una mariposa')
  if (!idsLoaded.value) return t('Cargando los Insectary IDs libres…')
  const bad = drafts.value.find(d => problems.value.get(d.key)?.length)
  if (bad) {
    const n = drafts.value.filter(d => problems.value.get(d.key)?.length).length
    return `${bad.id || '—'}: ${problems.value.get(bad.key)![0]}${n > 1 ? ` ${tn(n - 1, '(y {n} más)', '(y {n} más)')}` : ''}`
  }
  if (stockLines.value.some(l => l.on) && !day.loaded.value) return t('Cargando los clutches…')
  return ''
})
const summary = computed(() => {
  const adults = drafts.value.filter(isAdult)
  const ids = [...drafts.value].sort((a, b) => (rowOf.value.get(a.id) ?? 0) - (rowOf.value.get(b.id) ?? 0)).map(d => d.id)
  const parts = [
    tn(adults.length, '{n} adulto', '{n} adultos'),
    drafts.value.length > adults.length ? tn(drafts.value.length - adults.length, '{n} huevo o larva', '{n} huevos o larvas') : '',
    ids.length > 1 ? `${ids[0]}–${ids.at(-1)}` : ids[0],
  ]
  return parts.filter(Boolean).join(' · ')
})
const saving = ref(false)
const undoing = ref(false)
const lastSave = ref<null | { actionId: string; drafts: Draft[]; ids: string[]; count: number }>(null)
/** Kept until the server answers for sure, so a retry after an unclear outcome never writes twice. */
let pendingRequest: { id: string; body: string } | null = null

async function save() {
  if (blocker.value || saving.value) return
  saving.value = true
  refused.value = {}
  const list = [...drafts.value].sort((a, b) => (rowOf.value.get(a.id) ?? 0) - (rowOf.value.get(b.id) ?? 0))
  const creates = list.map(d => {
    const row = stockOf(d.clutch)
    const values = draftValues(d, {
      clutchValue: row?.values['CLUTCH NUMBER'] ?? d.clutch,
      clutchSpecies: speciesOfClutch(d.clutch),
      generation: String(row?.values.Generation ?? ''),
      formulas: props.createFormulas,
      today: today.value,
      initials: initials.value,
      medium: medium.value,
    })
    return { clientId: d.key, module: MODULE, values, replaceFormula: values.SPECIES ? ['SPECIES'] : [] }
  })
  const edits = stockLines.value
    .filter(l => l.on && l.row && l.plan)
    .map(l => ({
      id: l.row!.id,
      values: Object.fromEntries(l.plan!.cells.map(c => [c.field, c.value])),
      expected: Object.fromEntries(l.plan!.cells.map(c => [c.field, c.before])),
    }))
  const body = { reason: null, purpose: 'emergidos', creates, edits }
  const fingerprint = JSON.stringify(body)
  if (!pendingRequest || pendingRequest.body !== fingerprint) pendingRequest = { id: requestId(), body: fingerprint }
  try {
    const result = await api<{ records: ServerRecord[]; action?: { id: string } | null }>('records/batch', {
      method: 'POST',
      body: { requestId: pendingRequest.id, ...body },
    })
    pendingRequest = null
    tables.merge(result.records)
    lastSave.value = result.action?.id ? { actionId: result.action.id, drafts: list, ids: list.map(d => d.id), count: list.length } : null
    drafts.value = []
    skipStock.value = []
    for (const d of list) suggested.delete(d.key)
    if (!result.action?.id) notify(tn(list.length, '{n} emergido guardado en Google Sheets', '{n} emergidos guardados en Google Sheets'), 'success')
    scroller.value?.scrollTo({ top: 0 })
  } catch (e) {
    if (!(e instanceof ApiError) || !['WRITE_UNCERTAIN', 'OFFLINE', 'SERVER_ERROR'].includes(e.code)) pendingRequest = null
    // Refused cells: on their card (a new row) or said for the clutch (its stocks row).
    const items = e instanceof ApiError ? ((e.details as { items?: { id?: string; clientId?: string; field?: string; message: string }[] })?.items ?? []) : []
    for (const item of items) {
      if (item.clientId && drafts.value.some(d => d.key === item.clientId)) refused.value[item.clientId] = t(item.message)
      else if (item.id) {
        const line = stockLines.value.find(l => l.row?.id === item.id)
        if (line) notify(`${line.clutch}: ${t(item.message)}`, 'error')
      }
    }
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
    else await Promise.all([tables.load(MODULE, true), tables.load(STOCKS, true)])
    // The cards come back, to correct and save again.
    const have = new Set(drafts.value.map(d => d.key))
    drafts.value = [...last.drafts.filter(d => !have.has(d.key)), ...drafts.value]
    lastSave.value = null
    notify(tn(last.count, '{n} emergido deshecho en Google Sheets', '{n} emergidos deshechos en Google Sheets'), 'success')
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    undoing.value = false
  }
}

// --- The latest emergences, by day (what is in the sheet already: nothing is entered twice)
const recentCount = ref(30)
const recent = computed(() => {
  if (!props.table) return []
  return props.table.rows
    .filter(r => r.observed && typeof r.values.Intro2Insectary_date === 'number' && /reared/i.test(String(r.values.Wild_Reared ?? '')))
    .sort((a, b) => (b.values.Intro2Insectary_date as number) - (a.values.Intro2Insectary_date as number) || b.row - a.row)
    .slice(0, recentCount.value)
})
const recentGroups = computed(() => {
  const groups: { label: string; rows: TableRow[] }[] = []
  for (const row of recent.value) {
    const d = row.values.Intro2Insectary_date as number
    const ago = today.value - d
    const label = ago === 0 ? t('Hoy') : ago === 1 ? t('Ayer') : dayLabel(serialToIso(d)).split(' · ')[0]
    if (groups.at(-1)?.label !== label) groups.push({ label, rows: [] })
    groups.at(-1)!.rows.push(row)
  }
  return groups
})
const text = (v: CellValue | undefined) => (v === null || v === undefined ? '' : String(v))
const drawerRow = ref<TableRow | null>(null)
const showHistory = ref(false)

// --- The keyboard: Save hides while typing, the box stays in view
const scroller = ref<HTMLElement>()
function onFocusIn(e: FocusEvent) {
  const el = e.target as HTMLElement
  if (!el.matches('input, textarea')) return
  setTimeout(() => el.scrollIntoView({ block: 'center', behavior: 'smooth' }), 350)
}
const nextRow = computed(() => (next.value ? rowOf.value.get(next.value.toUpperCase()) : undefined))
</script>

<template>
  <div class="flex h-full flex-col bg-stone-50" @focusin="onFocusIn">
    <div ref="scroller" class="min-h-0 flex-1 overflow-y-auto">
      <!-- The day the butterflies emerged, the mode and the tab's history. -->
      <div class="border-b border-stone-200 bg-white px-3 pt-2.5 pb-2">
        <!-- Upright phone: the day on its own row, the mode and history beside its label; wider: one row. -->
        <div class="flex flex-wrap items-center gap-x-2 gap-y-1.5">
          <div class="grid min-w-0 flex-[1_1_20rem] grid-cols-[auto_auto_minmax(8.5rem,1fr)] gap-1.5">
            <button
              v-for="d in quickDates"
              :key="d.iso"
              class="h-11 rounded-lg border px-3 text-base font-medium"
              :class="date === d.iso ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white text-stone-800 active:bg-stone-100'"
              :aria-pressed="date === d.iso"
              @click="date = d.iso"
            >
              {{ d.name }}
            </button>
            <DateField v-model="date" class="field-input h-11 text-base" :aria-label="$t('Día de emergencia')" />
          </div>
          <p class="min-w-0 flex-[1_1_12rem] text-sm" :class="dateError ? 'text-red-700' : 'text-stone-600'">
            <span class="font-medium">{{ $t('Emergieron') }}:</span> {{ dateError || (date ? dayLabel(date) : $t('Elige el día')) }}
          </p>
          <span class="ml-auto flex shrink-0 gap-2">
            <EntryModeToggle v-model="mode" :compact="!roomy" class="h-11 *:min-w-11" />
            <button
              class="flex h-11 min-w-11 items-center justify-center gap-1 rounded-md border border-stone-300 bg-white px-2 text-sm text-stone-700 active:bg-stone-100"
              :aria-label="$t('Historial de Emergidos')"
              :title="$t('Historial de Emergidos')"
              @click="showHistory = true"
            >
              <History :size="18" /> <span v-if="roomy">{{ $t('Historial') }}</span>
            </button>
          </span>
        </div>
        <p v-if="otherDay.length" class="mt-1 flex flex-wrap items-center gap-2 text-sm text-amber-900">
          {{ $tn(otherDay.length, '{n} tarjeta tiene otro día', '{n} tarjetas tienen otro día') }}
          <button class="h-9 rounded-lg border border-amber-400 bg-amber-50 px-3 font-medium" @click="moveToDay">
            {{ $t('Pasarlas al {date}', { date: dayFirst(date) }) }}
          </button>
        </p>
      </div>
      <InsectaryIdsWarning class="mx-3 mt-2" :revision="table?.revision" @extended="state.loadFreeIds()" />
      <p v-if="!ready" class="p-6 text-stone-500">{{ $t('Cargando {sheet}…', { sheet: MODULE }) }}</p>

      <!-- One readable column on a wide screen. -->
      <div v-else class="mx-auto max-w-5xl">
        <ClutchPicker
          v-if="showPicker"
          :rows="stocks?.rows || []"
          :sums="day.sums.value"
          :registered="registered"
          :drafts="draftsByClutch"
          :today="today"
          :current="current ? clutch : ''"
          @pick="pick"
          @close="picking = false"
        />

        <!-- The clutch: what it has, and the buttons that add what emerged today. -->
        <section v-else class="px-3 pt-3">
          <div class="rounded-xl border border-stone-200 bg-white p-3 shadow-sm">
            <div class="flex items-start gap-2">
              <div class="min-w-0 flex-1">
                <p class="text-xs text-stone-500">Clutch</p>
                <p class="text-2xl leading-tight font-semibold tabular-nums">{{ clutch }}</p>
                <p class="text-sm" :class="currentSpecies ? 'text-stone-700' : 'font-medium text-amber-900'">
                  {{ currentSpecies || $t('Sin especie (NA): elige en cada tarjeta qué emergió') }}
                </p>
              </div>
              <button class="btn h-11 shrink-0 px-3" @click="picking = true">{{ $t('Cambiar') }} <ChevronRight :size="16" /></button>
            </div>
            <div class="mt-2 grid grid-cols-3 gap-1">
              <span class="rounded-md bg-stone-50 px-2 py-1">
                <span class="block text-[11px] leading-tight text-stone-500">{{ $t('Pupas') }}</span>
                <span class="block text-lg leading-tight font-semibold tabular-nums">{{ totalIn(current, 'NUMBER OF PUPA') ?? '—' }}</span>
              </span>
              <span class="rounded-md bg-stone-50 px-2 py-1">
                <span class="block text-[11px] leading-tight text-stone-500">{{ $t('Adultos') }}</span>
                <span class="block text-lg leading-tight font-semibold tabular-nums">{{ totalIn(current, 'NUMBER OF ADULTS') ?? '—' }}</span>
              </span>
              <span class="rounded-md bg-stone-50 px-2 py-1" :title="$t('Mariposas de este clutch en Insectary_data')">
                <span class="block text-[11px] leading-tight text-stone-500">{{ $t('Registrados') }}</span>
                <span class="block text-lg leading-tight font-semibold tabular-nums">{{ registered.get(clutch)?.n ?? 0 }}</span>
              </span>
            </div>
            <p v-if="doubt" class="mt-2 flex items-start gap-1.5 text-sm font-medium text-amber-900"><AlertTriangle :size="16" class="mt-0.5 shrink-0" />{{ doubt }}</p>

            <template v-if="canEdit">
              <div class="mt-3 grid grid-cols-2 gap-2">
                <button
                  class="flex h-16 flex-col items-center justify-center rounded-xl bg-pink-600 text-white shadow-sm active:bg-pink-700 disabled:opacity-50"
                  :disabled="!next"
                  :aria-label="$t('Añadir una hembra')"
                  @click="add('adult', 'female')"
                >
                  <span class="text-2xl leading-none font-bold">+ ♀</span>
                  <span v-if="next" class="text-xs font-medium opacity-90">{{ next }}</span>
                </button>
                <button
                  class="flex h-16 flex-col items-center justify-center rounded-xl bg-sky-600 text-white shadow-sm active:bg-sky-700 disabled:opacity-50"
                  :disabled="!next"
                  :aria-label="$t('Añadir un macho')"
                  @click="add('adult', 'male')"
                >
                  <span class="text-2xl leading-none font-bold">+ ♂</span>
                  <span v-if="next" class="text-xs font-medium opacity-90">{{ next }}</span>
                </button>
              </div>
              <div class="mt-2 grid grid-cols-3 gap-2">
                <button class="btn h-11 justify-center px-1 text-sm" :disabled="!next" :title="$t('Sexo no visible (NA)')" @click="add('adult', 'NA')"><Plus :size="15" /> {{ $t('Sin sexo') }}</button>
                <button class="btn h-11 justify-center px-1 text-sm" :aria-expanded="many.open" @click="many.open = !many.open">{{ $t('Varios…') }}</button>
                <button class="btn h-11 justify-center px-1 text-sm" :disabled="!next" :title="$t('Huevo o larva preservado (Sex NOT_COLLECTED, LIFESTAGE)')" @click="add('young', 'NA')">
                  <Plus :size="15" /> {{ $t('Larva') }}
                </button>
              </div>
              <form v-if="many.open" class="mt-2 grid grid-cols-[1fr_1fr_1fr_auto] items-end gap-2 rounded-lg border border-stone-200 bg-stone-50 p-2" @submit.prevent="addMany">
                <label><span class="field-label">♀ {{ $t('Hembras') }}</span><input v-model.number="many.female" type="number" inputmode="numeric" min="0" max="60" class="field-input h-11 text-base" /></label>
                <label><span class="field-label">♂ {{ $t('Machos') }}</span><input v-model.number="many.male" type="number" inputmode="numeric" min="0" max="60" class="field-input h-11 text-base" /></label>
                <label><span class="field-label">{{ $t('Sin sexo') }}</span><input v-model.number="many.none" type="number" inputmode="numeric" min="0" max="60" class="field-input h-11 text-base" /></label>
                <button class="btn-primary h-11 px-4">{{ $t('Añadir') }}</button>
              </form>
              <p class="mt-2 text-xs text-stone-600">
                <template v-if="next">{{ $t('Siguiente ID: {id} (fila {row}). Escríbelo en el ala; si el ala dice otro, toca el ID de la tarjeta.', { id: next, row: nextRow ?? '?' }) }}</template>
                <template v-else-if="idsLoaded">{{ $t('No quedan filas preasignadas libres: crea más filas preasignadas en Insectary_data.') }}</template>
                <template v-else>{{ $t('Cargando los Insectary IDs libres…') }}</template>
              </p>
              <p v-if="skipped.length" class="mt-1 flex items-start gap-1.5 text-sm text-amber-900">
                <AlertTriangle :size="15" class="mt-0.5 shrink-0" />
                {{ $t('Quedan filas vacías entre las tarjetas: {ids}', { ids: skipped.slice(0, 6).join(', ') + (skipped.length > 6 ? '…' : '') }) }}
              </p>
            </template>
          </div>
        </section>

        <!-- The cards, by clutch: the chosen clutch's newest on top. -->
        <section v-for="s in sections" :key="s.clutch" class="px-3 pt-4">
          <div class="mb-1.5 flex items-center gap-2">
            <h2 class="text-sm font-semibold text-stone-700">
              <template v-if="s.clutch === clutch">{{ $tn(s.cards.length, '{n} tarjeta de este clutch', '{n} tarjetas de este clutch') }}</template>
              <button v-else class="underline decoration-stone-300 underline-offset-2" @click="pick(s.clutch)">{{ $t('Clutch {clutch}', { clutch: s.clutch }) }} · {{ s.cards.length }}</button>
            </h2>
            <button v-if="canEdit" class="ml-auto h-10 px-2 text-sm text-stone-600 underline" @click="removeClutch(s.clutch)">{{ $t('Quitar todas') }}</button>
          </div>
          <ul class="grid grid-cols-[repeat(auto-fill,minmax(min(100%,20rem),1fr))] items-start gap-2">
            <DraftCard
              v-for="d in s.cards"
              :key="d.key"
              :draft="d"
              :clutch-species="speciesOfClutch(d.clutch)"
              :siblings="siblingsOf(d.clutch)"
              :all-species="knownList"
              :problems="problems.get(d.key) || []"
              :hint="hintOf(d)"
              :day="date"
              :can-edit="canEdit"
              :fresh="fresh.includes(d.key)"
              @update="update(d.key, $event)"
              @remove="remove(d.key)"
            />
          </ul>
        </section>

        <!-- What the save does to each clutch's row (Insectary_stocks), each one can be left out. -->
        <section v-if="canEdit && stockLines.length" class="px-3 pt-4">
          <h2 class="mb-1.5 text-sm font-semibold text-stone-700">{{ $t('Al guardar, en Insectary_stocks') }}</h2>
          <ul class="space-y-2">
            <li v-for="line in stockLines" :key="line.clutch" class="rounded-xl border border-stone-200 bg-white p-2.5 text-sm">
              <label v-if="line.plan?.cells.length" class="flex items-start gap-2.5">
                <input type="checkbox" class="mt-1 size-5 shrink-0" :checked="line.on" @change="toggleStock(line.clutch)" />
                <span class="min-w-0">
                  <span class="font-semibold">{{ line.clutch }}</span>
                  <span v-for="c in line.plan.cells" :key="c.field" class="block break-words text-stone-700" :class="{ 'line-through opacity-60': !line.on }">{{ cellText(c.field, c.before, c.value) }}</span>
                  <span v-if="line.changedToday" class="mt-0.5 block text-amber-900">{{ $t('NUMBER OF ADULTS ya cambió hoy (Clutches): desmarca si ya se contaron estos adultos.') }}</span>
                  <span v-if="line.plan.skipped.length" class="mt-0.5 block text-amber-900">{{ $t('Sin cambiar (no se puede restar): {fields}', { fields: line.plan.skipped.join(', ') }) }}</span>
                </span>
              </label>
              <p v-else-if="!line.row" class="text-amber-900">{{ $t('{clutch}: no está en Insectary_stocks', { clutch: line.clutch }) }}</p>
              <p v-else class="text-stone-600">{{ $t('{clutch}: nada que cambiar', { clutch: line.clutch }) }}</p>
            </li>
          </ul>
          <p class="mt-1 text-xs text-stone-500">{{ $t('Los adultos del día se suman a NUMBER OF ADULTS como en Clutches; Number of Adults in Insectary_data se cuenta solo.') }}</p>
        </section>

        <!-- The latest emergences in the sheet, by day. -->
        <section class="px-3 pt-6 pb-8">
          <h2 class="text-sm font-semibold text-stone-700">{{ $t('Últimos emergidos registrados') }}</h2>
          <p v-if="!recent.length" class="py-3 text-sm text-stone-500">{{ $t('No hay emergidos registrados.') }}</p>
          <template v-for="group in recentGroups" :key="group.label">
            <h3 class="mt-3 mb-1 text-xs font-semibold tracking-wide text-stone-500 uppercase">{{ group.label }}</h3>
            <ul class="divide-y divide-stone-100 overflow-hidden rounded-xl border border-stone-200 bg-white">
              <li v-for="row in group.rows" :key="row.id">
                <button class="flex min-h-12 w-full items-center gap-3 px-3 py-1.5 text-left active:bg-stone-50" @click="drawerRow = row">
                  <span class="w-14 shrink-0 font-semibold">{{ text(row.values.Insectary_ID) }}</span>
                  <SexBadge :sex="text(row.values.Sex)" />
                  <span class="min-w-0 flex-1">
                    <span class="block truncate text-sm">{{ text(row.values.SPECIES) || speciesOfClutch(text(row.values['CLUTCH NUMBER'])) || '—' }}</span>
                    <span class="block truncate text-xs text-stone-500">{{ $t('clutch {c}', { c: text(row.values['CLUTCH NUMBER']) }) }}<template v-if="!isBlank(row.values.Death_cause)"> · {{ text(row.values.Death_cause) }}</template></span>
                  </span>
                </button>
              </li>
            </ul>
          </template>
          <button v-if="recent.length >= recentCount" class="btn mt-3 h-11 w-full" @click="recentCount += 30">{{ $t('ver más') }}</button>
        </section>
      </div>
    </div>

    <!-- Save (or the last save, with Undo), under the cards; hidden while typing. -->
    <footer
      v-if="(lastSave && !drafts.length) || (canEdit && drafts.length)"
      v-show="!keyboard.open.value"
      class="shrink-0 border-t border-stone-200 bg-white px-3 pt-2 pb-[calc(0.5rem+env(safe-area-inset-bottom))]"
    >
      <div v-if="lastSave && !drafts.length" class="flex items-center gap-2" role="status">
        <Check :size="22" class="shrink-0 text-brand-700" />
        <p class="min-w-0 flex-1 text-sm">
          <span class="font-medium">{{ $tn(lastSave.count, '{n} emergido guardado en Google Sheets', '{n} emergidos guardados en Google Sheets') }}</span>
          <span class="block truncate text-xs text-stone-500">{{ lastSave.ids.join(', ') }}</span>
        </p>
        <button class="btn h-12 px-4 text-base" :disabled="undoing" @click="undo">
          <Loader2 v-if="undoing" :size="18" class="animate-spin" /><Undo2 v-else :size="18" /> {{ $t('Deshacer') }}
        </button>
        <button class="btn h-12 px-3" :aria-label="$t('Cerrar')" @click="lastSave = null"><X :size="18" /></button>
      </div>
      <div v-else class="mx-auto flex max-w-3xl items-center gap-3">
        <p class="line-clamp-2 min-w-0 flex-1 text-sm" :class="blocker ? 'text-amber-900' : 'text-stone-600'">{{ blocker || summary }}</p>
        <button class="btn-primary h-13 shrink-0 px-5 text-base" :disabled="!!blocker || saving" @click="save">
          <Loader2 v-if="saving" :size="18" class="animate-spin" />
          {{ saving ? $t('Guardando…') : $t('Guardar {n}', { n: drafts.length }) }}
        </button>
      </div>
    </footer>

    <TabHistory v-if="showHistory" :title="$t('Historial de Emergidos')" purpose="emergidos" @close="showHistory = false" />
    <RowDrawer
      v-if="drawerRow && table"
      :module="MODULE"
      :row-id="drawerRow.id"
      :rows="table.rows"
      :creates="[]"
      :columns="table.columns"
      :options="options"
      :locked-fields="['Collection_location']"
      :create-formulas="[]"
      label-field="Insectary_ID"
      @close="drawerRow = null"
    />
  </div>
</template>
