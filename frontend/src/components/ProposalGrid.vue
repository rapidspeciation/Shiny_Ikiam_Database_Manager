<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, ref, watch } from 'vue'
import {
  AlertTriangle,
  ArrowRight,
  Check,
  CircleHelp,
  Clock,
  Columns3,
  ListFilter,
  Plus,
  Send,
  Settings2,
  Sparkles,
  SquarePen,
  Table2,
  X,
} from 'lucide-vue-next'
import ProposalSheet, { type CellEdit } from './assistant/ProposalSheet.vue'
import ColumnChooser from './assistant/ColumnChooser.vue'
import { api } from '../lib/api'
import { displayValue } from '../lib/cells'
import { errorText, notify } from '../lib/notice'
import { persistentRef } from '../lib/persist'
import {
  cellId,
  cellOrder,
  changedCells,
  changedText,
  expandProposal,
  nextCell,
  notApplied,
  notebookColumns,
  orderText,
  photoSummaries,
  rowKey,
  rowsToWrite,
  sameAsFormula,
  sheetEdits,
  sheetGroups,
  sampleWarnings,
  tellText,
  uncheckedDoubts,
  unfilledUnreadable,
  whenText,
  withLocal,
  type LocalCell,
  type Proposal,
  type SheetEdit,
  type ProposalChange,
} from '../lib/proposals'
import type { CellValue } from '../lib/types'
import { columnChoices, orderColumns, viewFor, type ColumnView } from '../lib/proposalColumns'
import { layRows, repeatSummary, shownRows, unexplained, type RowOrder, type SheetRows } from '../lib/proposalRows'
import { useSession } from '../stores/session'
import { useLive } from '../stores/live'
import { t, tx } from '../lib/i18n'

export type { Proposal, ProposalChange } from '../lib/proposals'

/**
 * A proposal of the assistant as an editable sheet (one table per sheet it
 * touches): the assistant's values in green, the person's in blue. The person
 * corrects cells as in Colecta and they are saved to the proposal at once
 * (the assistant sees them and does not overwrite them); what the assistant
 * changes meanwhile flashes. Selected cells go back to the sheet's value (the
 * assistant's kept aside, marked) or take the assistant's again; "Aplicar"
 * writes what the table shows (as does "aplica" in the chat). Doubtful cells
 * (amber, "?") are counted at the top (a click, or Ctrl+Enter in the table,
 * goes to the next one); "Aplicar" with some still unreviewed
 * asks first: apply them anyway, only the sure cells, or go and review them.
 * Cells the assistant could not read (hatched red, "unreadable") are counted
 * apart; they are never written until the person types them, and "Aplicar"
 * says so before applying.
 * A notebook page's proposal follows the page: a header per photo (its
 * thumbnail, which opens it upright in a new tab, the assistant's few words on
 * why it is there, and how many of its lines change), every line in the
 * sheet's order with its photo and line, lines a
 * photo has the other way round in the sheet told and marked ↕ ("solo cambios" hides the lines
 * that write nothing), the notebook's columns first and the template's NA /
 * NOT_COLLECTED columns folded.
 * Each table shows its rows whole enough to spot a wrong one, whatever the
 * proposal changes: in Insectary_data the notebook's columns, then the sheet's
 * others up to Notes_Insectary_data (unchanged cells grey, as the sheet has
 * them); in a sheet no notebook fills, its IDs, species, CAM and first tube
 * (sheetGroups, and reviewColumns in server/notebook.mjs).
 * Cells someone edited in the sheet after the proposal (violet, "hoja") keep
 * the sheet's value unless the person chooses the proposal's beside the cell;
 * a banner counts them (a click goes to the next one), and «Avisar al
 * asistente» sends the assistant which ones, to look at them again: into its T3
 * chat when the app can, else copied to paste there.
 * The columns go as the person chooses on the card (lib/proposalColumns): the
 * sheet's order, the notebook's (a notebook page's proposal, or any
 * Collection_data table: a wild-caught butterfly's columns), or their own
 * (ordered and hidden in ColumnChooser), remembered per person. A notebook
 * page's rows go in the sheet's order or the notebook's (the photo's lines),
 * remembered per person too; the table tells where its rows are not
 * continuous in the sheet, and a summary above says which repeated IDs
 * (A0E.1) have their rows away from their series (lib/proposalRows).
 * «Aplicar» while Google Sheets does not answer as usual asks first: the save
 * then waits in the app (status queued) and the card says so until written.
 */
const props = defineProps<{
  proposal: Proposal
  busy?: boolean
  /** Shown beside its photo (PhotoReview): its thumbnails pick the photo there. */
  reviewing?: boolean
}>()
const emit = defineEmits<{
  /** doubtful: what to do with the unreviewed doubtful cells (the person chose it in the dialog). */
  apply: [indexes: number[], revision: number | undefined, doubtful?: 'confirm' | 'skip']
  discard: []
  replace: [proposal: Proposal]
  /** A photo's thumbnail clicked: shown with the table (PhotoReview). */
  photo: [n: number]
  /** The row selected in the table: its photo and line on the page (null: not on the photos). */
  row: [photo: number | null, line: number | null]
}>()
const session = useSession()
const live = useLive()

const pending = computed(() => props.proposal.status === 'pending')
/** Repeated IDs (A0E.1) whose rows are away from their series: said in plain words above the table. */
const repeatNotice = computed(() => repeatSummary(props.proposal.changes))
/**
 * A notebook page whose lines go another way in the sheet (within a photo): said above the table, rows
 * marked ↕ (those a repeat explains are said by its summary).
 */
const orderNotice = computed(() => {
  const notes = unexplained(props.proposal.outOfOrder ?? [], props.proposal.changes)
  const photos = new Set(props.proposal.changes.flatMap(c => (c.page ? [c.page.photo] : [])))
  return notes.length ? orderText(notes, photos.size > 1) : ''
})
const editable = computed(() => pending.value && session.canEdit)
/** Other pending proposals (anyone's) with the same rows or the same new IDs and clutches: said above the table. */
const overlapNotices = computed(() =>
  (props.proposal.overlaps ?? []).map(o =>
    t('También en la propuesta pendiente de {name} ({id}): {rows}', {
      name: o.by,
      id: o.proposalId.slice(0, 8),
      rows: o.rows.join(', ') + (o.count > o.rows.length ? '…' : ''),
    }),
  ),
)
/** Why its last apply failed, in plain words (Google's refusal, a cell changed meanwhile…). */
const lastErrorText = computed(() => {
  const e = props.proposal.lastError
  if (!e) return ''
  const first = e.items?.[0]
  return [tx(e.message, e.messageMsg), first ? tx(first.message, first.messageMsg) : ''].filter(Boolean).join(' · ')
})

// ------------------------------------------------------------ the person's edits, saved to the proposal
/** Typed and not yet saved (laid over the server's copy). */
const local = ref(new Map<string, LocalCell>())
/** Doubtful cells marked (or unmarked) as reviewed and not yet saved. */
const localChecks = ref(new Map<string, boolean>())
/** Cells edited in the sheet: the sheet's value kept or the proposal's chosen, not yet saved. */
const localChoices = ref(new Map<string, 'sheet' | 'proposal'>())
/** The proposal as the server sends it (lean) made whole for the table. */
const full = computed(() => expandProposal(props.proposal))
const shown = computed(() => withLocal(full.value, local.value, localChecks.value, localChoices.value))
const queue = new Map<string, CellEdit>()
const checkQueue = new Map<string, { key: string; field: string; checked: boolean }>()
const choiceQueue = new Map<string, { key: string; field: string; use: 'sheet' | 'proposal' }>()
let removes: string[] = []
let adds: { sheet: string }[] = []
/** What was sent lately: its echo from the server must not flash as the assistant's change. */
const sent = new Map<string, { value: CellValue; at: number }>()
const saving = ref(false)
const failed = ref(false)
let timer: number | undefined
let running: Promise<void> | null = null

function onEdit(cells: CellEdit[]) {
  const next = new Map(local.value)
  for (const c of cells) {
    const id = cellId(c.key, c.field)
    next.set(id, c.use ? { value: c.value, use: c.use } : { value: c.value })
    // The first "before" is what the person saw.
    queue.set(id, { ...c, before: queue.get(id)?.before ?? c.before })
  }
  local.value = next
  later()
}
/** «Marcar revisadas» (or an undo of it, `checked: false`): shown at once, saved with the next edits. */
function onCheck(cells: { key: string; field: string; checked?: boolean }[]) {
  const next = new Map(localChecks.value)
  for (const { key, field, checked = true } of cells) {
    next.set(cellId(key, field), checked)
    checkQueue.set(cellId(key, field), { key, field, checked })
  }
  localChecks.value = next
  later(0)
}
/** A cell edited in the sheet: the sheet's value kept, or the proposal's written over it (shown at once, saved now). */
function onSheet(cells: { key: string; field: string; use: 'sheet' | 'proposal' }[]) {
  const next = new Map(localChoices.value)
  for (const c of cells) {
    next.set(cellId(c.key, c.field), c.use)
    choiceQueue.set(cellId(c.key, c.field), c)
  }
  localChoices.value = next
  later(0)
}
function later(ms = 500) {
  window.clearTimeout(timer)
  timer = window.setTimeout(() => void save(), ms)
}
/** Sends what waits, one request at a time. */
async function save(): Promise<void> {
  window.clearTimeout(timer)
  if (running) {
    await running
    return save()
  }
  if (!queue.size && !removes.length && !adds.length && !checkQueue.size && !choiceQueue.size) return
  const cells = [...queue.values()]
  const checks = [...checkQueue.values()]
  const choices = [...choiceQueue.values()]
  const body = { cells, remove: removes, add: adds, check: checks, sheet: choices }
  checkQueue.clear()
  choiceQueue.clear()
  queue.clear()
  removes = []
  adds = []
  saving.value = true
  running = (async () => {
    try {
      const out = await api<{
        proposal: Proposal
        rejected: { label?: string; field?: string; message: string }[]
        overrode: { label?: string; field: string; ai: CellValue }[]
      }>(`chat/proposals/${props.proposal.id}/edit`, { method: 'POST', body })
      failed.value = false
      const now = Date.now()
      for (const c of cells) sent.set(cellId(c.key, c.field), { value: c.value, at: now })
      dropSaved(cells)
      dropChecks(checks)
      dropChoices(choices)
      savedRevision = Math.max(savedRevision, out.proposal.revision ?? 0)
      emit('replace', out.proposal)
      for (const r of out.rejected.slice(0, 3)) {
        const what = [r.field, r.label ? `(${r.label})` : ''].filter(Boolean).join(' ')
        const message = t(r.message)
        notify(what ? t('No se guardó {what}: {message}', { what, message }) : t('No se guardó: {message}', { message }), 'error')
      }
      for (const o of out.overrode.slice(0, 3))
        notify(
          t('Escribiste encima de un cambio de la IA en {field}: proponía {value}', {
            field: `${o.field}${o.label ? ` (${o.label})` : ''}`,
            value: show(o.field, o.ai) || t('vacío'),
          }),
        )
    } catch (e) {
      const code = (e as { code?: string }).code
      if (code === 'OFFLINE' || (e as { status?: number }).status === 0) {
        // Kept, and tried again: nothing typed is lost.
        failed.value = true
        for (const c of cells) if (!queue.has(cellId(c.key, c.field))) queue.set(cellId(c.key, c.field), c)
        for (const c of checks) if (!checkQueue.has(cellId(c.key, c.field))) checkQueue.set(cellId(c.key, c.field), c)
        for (const c of choices) if (!choiceQueue.has(cellId(c.key, c.field))) choiceQueue.set(cellId(c.key, c.field), c)
        removes.push(...body.remove)
        adds.push(...body.add)
        later(5000)
      } else {
        dropSaved(cells)
        dropChecks(checks)
        dropChoices(choices)
        notify(errorText(e), 'error')
      }
    } finally {
      saving.value = false
      running = null
    }
  })()
  return running
}
function dropSaved(cells: CellEdit[]) {
  const next = new Map(local.value)
  for (const c of cells) {
    const id = cellId(c.key, c.field)
    const mine = next.get(id)
    if (!queue.has(id) && mine?.use === c.use && JSON.stringify(mine?.value) === JSON.stringify(c.value)) next.delete(id)
  }
  local.value = next
}
function dropChecks(checks: { key: string; field: string }[]) {
  if (!checks.length) return
  const next = new Map(localChecks.value)
  for (const c of checks) if (!checkQueue.has(cellId(c.key, c.field))) next.delete(cellId(c.key, c.field))
  localChecks.value = next
}
function dropChoices(choices: { key: string; field: string }[]) {
  if (!choices.length) return
  const next = new Map(localChoices.value)
  for (const c of choices) if (!choiceQueue.has(cellId(c.key, c.field))) next.delete(cellId(c.key, c.field))
  localChoices.value = next
}
onBeforeUnmount(() => {
  // Leaving the page (or the panel) still saves what was typed.
  if (queue.size || removes.length || adds.length || checkQueue.size || choiceQueue.size) void save()
})

function removeRow(key: string) {
  removes.push(key)
  later(0)
}
function addRow(sheet: string) {
  adds.push({ sheet })
  later(0)
}

// ------------------------------------------------------------ what the assistant changes, live
const flash = ref(new Set<string>())
const flashText = ref('')
let flashTimer: number | undefined
watch(
  () => props.proposal,
  (next, prev) => {
    const now = Date.now()
    for (const [id, s] of sent) if (now - s.at > 30000) sent.delete(id)
    /** What the cell holds now; a value the person sent that is the formula's own (left to it) counts as kept. */
    const isMine = (id: string, mine: CellValue) => {
      const [key, field] = id.split('\u0000')
      const c = next.changes.find(x => rowKey(x) === key)
      const now = c && field in c.values ? c.values[field] : undefined
      const gives = c?.formulaGives?.[field]
      if (now === undefined && gives !== undefined && gives !== null && sameAsFormula(mine, gives)) return true
      return JSON.stringify(mine ?? null) === JSON.stringify(now ?? null)
    }
    const cells = changedCells(prev, next).filter(id => {
      if (local.value.has(id)) return false
      const mine = sent.get(id)
      return !mine || !isMine(id, mine.value)
    })
    if (!cells.length) return
    flash.value = new Set(cells)
    flashText.value = changedText(cells.length)
    window.clearTimeout(flashTimer)
    flashTimer = window.setTimeout(() => {
      flash.value = new Set()
      flashText.value = ''
    }, 2600)
  },
)
onBeforeUnmount(() => window.clearTimeout(flashTimer))

// ------------------------------------------------------------ columns, apply
/** The rows "Aplicar" writes: what the table shows (a row set back to the sheet in every cell is left out). */
const chosen = computed(() => rowsToWrite(shown.value))
const setAside = computed(() => notApplied(shown.value))

/** Columns the person added to a sheet's table (kept while the tab is open). */
const extra = persistentRef<Record<string, string[]>>(`proposal-columns:${props.proposal.id}`, {})
const fieldsOf = (sheet: string) => session.module(sheet)?.fields
const groups = computed(() => sheetGroups(shown.value, extra.value, sheet => fieldsOf(sheet)?.map(f => f.key)))

// ------------------------------------------------------------ a notebook page
/** "Solo cambios": the page's lines that write nothing hidden (a line to look at, not found or refused, stays). */
const changesOnly = persistentRef('proposal-changes-only', false)
const lookAt = (c: ProposalChange) => !!c.page?.error || (!!c.placeholder && c.page?.status !== 'crossed')
const rowsOf = (changes: ProposalChange[]) => (changesOnly.value ? changes.filter(c => !c.context || lookAt(c)) : changes)
const quietRows = (changes: ProposalChange[]) => changes.filter(c => c.context && !lookAt(c)).length
/** The template's columns (only NA / NOT_COLLECTED), folded unless opened. */
const templatesOpen = ref(false)
const columnsOf = (g: { template: string[] }, fields: string[]) =>
  templatesOpen.value ? fields : fields.filter(f => !g.template.includes(f))
const page = computed(() => props.proposal.page)

// ------------------------------------------------------------ the columns' order: the sheet's, the notebook's, the person's
const choices = computed(() => columnChoices(session.user?.username ?? ''))
/** The notebook's columns of a sheet's table (a notebook page's, in the page's order; else the sheet's own: Collection_data). */
const notebookOf = (sheet: string) => notebookColumns(props.proposal, sheet)
const hasNotebook = computed(() => groups.value.some(g => notebookOf(g.sheet).length > 0))
const view = computed(() => viewFor(choices.value.view, hasNotebook.value))
const views = computed(() =>
  (
    [
      ['sheet', t('Hoja'), t('Todas las columnas en el orden de la hoja, como en Google Sheets')],
      ['notebook', t('Cuaderno'), t('Las columnas del cuaderno primero, en su orden de izquierda a derecha; luego las demás')],
      ['custom', t('Personal'), t('Tu propio orden y columnas ocultas (se guardan para ti en este navegador)')],
    ] as [ColumnView, string, string][]
  ).filter(([v]) => v !== 'notebook' || hasNotebook.value),
)
const sheetOrder = (sheet: string) => fieldsOf(sheet)?.map(f => f.key) ?? []
/** Columns where the proposal writes or marks something: shown in every view, even hidden by the person. */
const marked = (changes: ProposalChange[]) =>
  new Set(
    changes
      .filter(c => !c.context || c.page?.error)
      .flatMap(c => [
        ...Object.keys(c.values),
        ...Object.keys(c.personEdits ?? {}),
        ...Object.keys(c.unreadable ?? {}),
        ...Object.keys(c.warnings ?? {}),
        ...Object.keys(c.doubts ?? {}),
        ...Object.keys(c.sheetChanged ?? {}),
      ]),
  )
/** A sheet's columns in the view chosen (before the template's are folded). */
function viewColumns(g: { sheet: string; fields: string[]; changes: ProposalChange[] }, as: ColumnView = view.value) {
  return orderColumns(as, g.fields, {
    sheetOrder: sheetOrder(g.sheet),
    notebook: notebookOf(g.sheet),
    implied: g.changes.flatMap(c => c.inferred ?? []),
    custom: as === 'custom' ? choices.value.custom(g.sheet) : null,
    keep: marked(g.changes),
  })
}
/** The rows' order of a notebook page's table: the sheet's or the notebook's (remembered per person). */
const rowOrder = computed<RowOrder>(() => choices.value.rowOrder)
const rowOrders = computed(
  () =>
    [
      ['sheet', t('Hoja'), t('Las filas en el orden de la hoja; una fila fina dice dónde no van seguidas')],
      ['notebook', t('Cuaderno'), t('Las filas en el orden de las líneas de la foto; una fila fina dice cuánto salta la hoja')],
    ] as [RowOrder, string, string][],
)
const pagedTable = (changes: ProposalChange[]) => changes.some(c => c.page)
function setView(v: ColumnView) {
  choices.value.setView(v)
  if (v !== 'custom') choosing.value = null
}
/** The sheet whose own columns are being chosen (ColumnChooser open). */
const choosing = ref<string | null>(null)
function openChooser(sheet: string) {
  if (view.value !== 'custom') choices.value.setView('custom')
  choosing.value = choosing.value === sheet ? null : sheet
}
/** The person's columns for the chooser: shown (in order), the others that can be shown, those always shown. */
function chooserOf(g: { sheet: string; fields: string[]; changes: ProposalChange[] }) {
  const shown = viewColumns(g, 'custom')
  const kept = marked(g.changes)
  const others = orderColumns('sheet', [...new Set([...g.fields, ...addable(g.sheet, shown)])], { sheetOrder: sheetOrder(g.sheet) }).filter(
    f => !shown.includes(f),
  )
  return { shown, others, kept: shown.filter(f => kept.has(f)) }
}
function customOrder(g: { sheet: string; fields: string[]; changes: ProposalChange[] }, list: string[]) {
  choices.value.setCustom(g.sheet, { order: list, hidden: choices.value.custom(g.sheet)?.hidden ?? [] })
}
function customToggle(g: { sheet: string; fields: string[]; changes: ProposalChange[] }, field: string, on: boolean) {
  const shown = viewColumns(g, 'custom')
  const hidden = new Set(choices.value.custom(g.sheet)?.hidden ?? [])
  if (on) hidden.delete(field)
  else hidden.add(field)
  const order = on ? [...shown.filter(f => f !== field), field] : shown.filter(f => f !== field)
  choices.value.setCustom(g.sheet, { order, hidden: [...hidden] })
}
/** Per photo of the page: its lines, how many change, how many are as the sheet has them. */
function photosOf(changes: ProposalChange[]) {
  const summaries = photoSummaries(changes)
  for (let n = 0; n < (page.value?.photos ?? 0); n++)
    if (!summaries.some(s => s.photo === n)) summaries.push({ photo: n, from: 0, to: 0, change: 0, same: 0, other: 0 })
  return summaries.sort((a, b) => a.photo - b.photo)
}
/**
 * Each sheet's table as shown: its rows ("solo cambios"), its columns (the
 * template folded), its page's photos. The rows around the page with the same
 * error (not on the photo) go in a table of their own under the page's.
 */
const tables = computed(() =>
  groups.value.flatMap(g => {
    const near = g.changes.filter(c => c.sameErrorAs !== undefined)
    const own = near.length ? g.changes.filter(c => c.sameErrorAs === undefined) : g.changes
    // The rows in the order chosen, with the slim rows that say where the sheet is not continuous.
    const paged = pagedTable(own)
    const laid = shownRows(layRows(own, paged ? rowOrder.value : 'sheet', paged), new Set(rowsOf(own).map(rowKey)), rowKey)
    const table = {
      ...g,
      id: g.sheet,
      near: false,
      paged,
      laid,
      rows: laid.flatMap(item => (item.change ? [item.change] : [])),
      columns: columnsOf(g, viewColumns(g)),
      quiet: quietRows(own),
      photos: page.value?.sheet === g.sheet ? photosOf(own) : [],
    }
    return near.length
      ? [table, { ...table, id: `${g.sheet}:near`, near: true, paged: false, laid: undefined, rows: near, quiet: 0, photos: [] }]
      : [table]
  }),
)
/** A sheet's rows `from`–`to` as they are now: a slim row of its table opened with a click. */
const sheetRows = (sheet: string) => (from: number, to: number) =>
  api<SheetRows>(`chat/proposals/${props.proposal.id}/rows?sheet=${encodeURIComponent(sheet)}&from=${from}&to=${to}`)
/** The assistant's few words on why a photo is there ('' for none). */
const photoNote = (n: number) => page.value?.photoNotes?.[n] ?? ''
const photoUrl = (n: number, size: 'thumb' | 'view') => `api/proposals/${props.proposal.id}/photos/${n}?size=${size}${props.proposal.page?.photoKey ? `&v=${props.proposal.page.photoKey}` : ''}`
/** A thumbnail that would not load (an old proposal's photo gone): hidden. */
const brokenPhotos = ref(new Set<number>())
const typesOf = (sheet: string) => ({
  ...Object.fromEntries((fieldsOf(sheet) ?? []).map(f => [f.key, f.type])),
  ...props.proposal.types,
})
/** Columns that can still be added to a sheet's table. */
const addable = (sheet: string, fields: string[]) => {
  // Columns beyond the ones the team handles (after Notes_Insectary_data) are never offered (server/proposal-columns.mjs).
  const hidden = props.proposal.shownColumns?.[sheet]?.hidden ?? []
  return (fieldsOf(sheet) ?? []).filter(f => !f.readonly && !f.unavailable && !fields.includes(f.key) && !hidden.includes(f.key)).map(f => f.key)
}
function addColumn(sheet: string, event: Event) {
  const select = event.target as HTMLSelectElement
  const group = groups.value.find(g => g.sheet === sheet)
  // In the person's own view it joins their columns of the sheet (every proposal); else this table's.
  if (select.value && view.value === 'custom' && group) customToggle(group, select.value, true)
  else if (select.value) extra.value = { ...extra.value, [sheet]: [...(extra.value[sheet] ?? []), select.value] }
  select.value = ''
}

/** The revision the last save of the person's edits produced (the list may not show it yet). */
let savedRevision = 0
/** Doubtful cells nobody reviewed yet (in the whole table, and in the rows "Aplicar" writes). */
const doubtful = computed(() => uncheckedDoubts(shown.value))
const doubtfulToWrite = computed(() => uncheckedDoubts(shown.value, chosen.value))
/** The CAM or tube a preserved butterfly would be left without: marked in the table until filled. */
const noSample = computed(() => sampleWarnings(shown.value))
/** Unreadable cells nobody filled yet: applying leaves them as the sheet has them. */
const unreadable = computed(() => unfilledUnreadable(shown.value))
/** Cells edited in the sheet since the proposal (and new rows whose pre-made row was used): the banner's. */
const edited = computed(() => sheetEdits(shown.value))
const editedCells = computed(
  () => edited.value.filter(e => e.field && e.edit) as { key: string; field: string; index: number; edit: SheetEdit }[],
)
const overwritten = computed(() => editedCells.value.filter(e => e.edit.use === 'proposal' && !e.edit.again).length)
const takenRows = computed(() => edited.value.filter(e => e.taken).length)
/** The dialog "Aplicar" opens while doubtful cells are unreviewed, or unreadable ones empty. */
const asking = ref(false)
/** «Aplicar» while Google Sheets is busy or slow: what was being applied, until the person confirms. */
const waitAsk = ref<{ how?: 'confirm' | 'skip'; leave: boolean } | null>(null)
/**
 * how: what to do with unreviewed doubtful cells; `leave`: the person saw the
 * empty unreadable cells and applies anyway (they stay as the sheet has them);
 * `waiting`: they agreed to keep the save in the app while Google does not answer.
 */
async function apply(how?: 'confirm' | 'skip', leave = false, waiting = false) {
  // What was just typed goes into the proposal first.
  await save()
  await nextTick()
  if ((!how && doubtfulToWrite.value.length) || (!how && !leave && unreadable.value.length)) {
    asking.value = true
    return
  }
  asking.value = false
  // Google recalculating (server/workbook-health.mjs): the save would wait in the app; said first.
  if (live.busy && !waiting) {
    waitAsk.value = { how, leave }
    return
  }
  waitAsk.value = null
  // With the rows whose cells the sheet keeps (nothing of theirs is written): the answer says what stayed.
  const rows = [...new Set([...chosen.value, ...edited.value.map(e => e.index)])].filter(i => i >= 0)
  emit('apply', rows, Math.max(props.proposal.revision ?? 1, savedRevision) || undefined, how)
}
/** The tables, to bring a doubtful cell into view. */
const sheets = new Map<string, { focusCell: (key: string, field: string) => boolean }>()
const sheetRef = (id: string) => (el: unknown) => {
  if (el) sheets.set(id, el as { focusCell: (key: string, field: string) => boolean })
  else sheets.delete(id)
}
/** The tables' cells in the order shown, to go from one cell to review to the next. */
const order = computed(() => cellOrder(tables.value.map(g => ({ keys: g.rows.map(rowKey), fields: g.columns }))))
/** The cell gone to last, to go on from. */
let lastReviewed: { key: string; field: string } | null = null
/** Selects the doubtful (unreadable, edited in the sheet) cell after `from` in the tables' order (after the last, the first). */
function reviewNext(
  which: 'doubtful' | 'unreadable' | 'sheet' = doubtful.value.length ? 'doubtful' : 'unreadable',
  from = lastReviewed,
  /** After a choice beside a cell edited in the sheet: only those nobody chose for yet (none left: it stays). */
  open = false,
) {
  asking.value = false
  // Cells edited in the sheet: those still to choose for first, then all of them again.
  const undecided = editedCells.value.filter(e => !e.edit.use || e.edit.again)
  const edits = undecided.length || open ? undecided : editedCells.value
  const cells = which === 'doubtful' ? doubtful.value : which === 'sheet' ? edits : unreadable.value
  const next = nextCell(cells, order.value, from)
  if (!next) return
  lastReviewed = next
  const id = tables.value.find(g => g.rows.some(c => rowKey(c) === next.key))?.id
  if (id) sheets.get(id)?.focusCell(next.key, next.field)
}

/**
 * «Avisar al asistente»: which cells the sheet changed since its proposal, to look
 * at them again and update it. Sent into the proposal's T3 chat when the app can
 * (it is idle, and the app reaches T3), else copied to paste there.
 */
const telling = ref(false)
async function tell() {
  const text = tellText(shown.value, show)
  telling.value = true
  try {
    const out = await api<{ sent: boolean; reason?: string }>(`chat/proposals/${props.proposal.id}/tell`, { method: 'POST', body: { text } })
    if (out.sent) return notify(t('Enviado al chat del asistente'), 'success')
    await copy(text, out.reason === 'busy' ? t('El asistente está respondiendo: el mensaje se copió para pegarlo en su chat cuando termine') : '')
  } catch {
    await copy(text, '')
  } finally {
    telling.value = false
  }
}
async function copy(text: string, why: string) {
  try {
    await navigator.clipboard.writeText(text)
    notify(why || t('Mensaje copiado: pégalo en el chat del asistente'))
  } catch {
    // No clipboard here (an address without https): the text to copy by hand.
    window.prompt(t('Copia este mensaje y pégalo en el chat del asistente'), text)
  }
}

/** The key that checks a doubtful cell and goes on (ProposalSheet). */
const reviewKey = typeof navigator !== 'undefined' && /Mac|iPhone|iPad/.test(navigator.platform) ? '⌘ Enter' : 'Ctrl+Enter'

const show = (field: string, value: CellValue | undefined) =>
  displayValue(value, { key: field, type: (props.proposal.types[field] ?? 'text') as 'text' })
const created = computed(() => props.proposal.changes.filter(c => c.create).length)
const personCells = computed(() => shown.value.changes.reduce((n, c) => n + Object.keys(c.personEdits ?? {}).length, 0))
/** The row selected in a table: its photo and line, for the photo beside it. */
function onSelect(key: string) {
  const c = shown.value.changes.find(x => rowKey(x) === key)
  emit('row', c?.page ? c.page.photo : null, c?.page ? c.page.line : null)
}
/** A thumbnail: beside the table in the same tab («Revisar con la foto»); Ctrl/⌘ or the middle button, a new tab. */
function openPhoto(n: number, e: MouseEvent) {
  if (e.ctrlKey || e.metaKey || e.shiftKey || e.button !== 0) return
  e.preventDefault()
  emit('photo', n)
}
const statusText = computed(
  () =>
    ({
      applied: t('Aplicado en la hoja ({n} filas)', { n: props.proposal.applied?.length ?? props.proposal.changes.length }),
      needs_review: t('No se pudo aplicar: revisa las filas en la hoja'),
      discarded: t('Descartado'),
      applying: t('Aplicando…'),
      queued: t('Esperando a Google Sheets: se escribe solo cuando responda'),
    })[props.proposal.status as string] ?? '',
)
</script>

<template>
  <div class="mt-2 rounded-md border border-stone-300 bg-white text-stone-800">
    <p class="flex flex-wrap items-center gap-x-2 border-b border-stone-200 px-2 py-1.5 text-xs font-medium">
      <span>
        {{ $t('Cambios propuestos') }} · {{ (proposal.sheets ?? [proposal.changes[0]?.sheet]).join(', ') }}
        <template v-if="created"> · {{ $tn(created, '{n} fila nueva', '{n} filas nuevas') }}</template>
        <span class="font-normal text-stone-500">— {{ proposal.reason }}</span>
      </span>
      <span
        v-if="flashText"
        class="flex items-center gap-1 rounded bg-amber-100 px-1.5 py-0.5 font-normal text-amber-900"
        role="status"
      >
        <Sparkles :size="12" /> {{ flashText }}
      </span>
      <button
        v-if="pending && doubtful.length"
        type="button"
        class="doubt-count"
        :title="$t('La IA no está segura de estas celdas: en cada una, «Correcta» u otra lectura junto a la celda, o edítala ({key} en la tabla: correcta y siguiente). Clic: ir a la siguiente', { key: reviewKey })"
        @click="reviewNext('doubtful')"
      >
        <CircleHelp :size="12" />
        {{ $tn(doubtful.length, '{n} celda dudosa por revisar', '{n} celdas dudosas por revisar') }}
        <span class="font-semibold">· {{ $t('siguiente') }}</span><ArrowRight :size="12" />
      </button>
      <button
        v-if="pending && unreadable.length"
        type="button"
        class="unread-count"
        :title="$t('La IA no pudo leer estas celdas: escribe su valor en la tabla (la barra de arriba dice por qué y lo que se leyó). Vacías no se escriben. Clic: ir a la siguiente')"
        @click="reviewNext('unreadable')"
      >
        <SquarePen :size="12" />
        {{ $tn(unreadable.length, '{n} celda ilegible por rellenar', '{n} celdas ilegibles por rellenar') }}
      </button>
      <span
        v-if="pending && noSample.length"
        class="warn-count"
        :title="$t('Estas filas dejan una mariposa preservada sin CAM_ID o Tube_1_id (celdas en ámbar): pregunta al equipo y escríbelos aquí')"
      >
        <AlertTriangle :size="12" />
        {{ $tn(new Set(noSample.map(w => w.key)).size, '{n} preservada sin CAM o tubo', '{n} preservadas sin CAM o tubo') }}
      </span>
    </p>
    <!-- Where the page and the sheet differ: repeated IDs with their rows away from their series, and lines of a
         photo the sheet has the other way round (an ID misread?). -->
    <div v-if="pending && (repeatNotice || orderNotice)" class="order-notice flex-wrap" role="status">
      <AlertTriangle :size="12" class="shrink-0" />
      <span class="min-w-0 flex-1">
        <span
          v-if="repeatNotice"
          class="block"
          :title="$t('El mismo ID se escribió en dos mariposas: la repetida va en su propia fila (con .1, .2…) después de la serie, no en la fila de su ID')"
          >{{ repeatNotice }}</span
        >
        <span
          v-if="orderNotice"
          class="block"
          :title="$t('Las filas van en el orden de la hoja. Estas líneas de una misma foto están al revés en la hoja (marcadas con ↕ en «Fila»): revisa que el ID esté bien leído')"
          >{{ orderNotice }}</span
        >
      </span>
      <button
        v-if="rowOrder === 'sheet' && tables.some(g => g.paged)"
        type="button"
        class="font-semibold underline decoration-dotted hover:text-amber-950"
        :title="$t('Las filas en el orden de las líneas de la foto; una fila fina dice cuánto salta la hoja')"
        @click="choices.setRowOrder('notebook')"
      >
        {{ $t('Ver en el orden del cuaderno') }}
      </button>
    </div>
    <!-- Why the last «Aplicar» failed (nothing was written), and the same rows in someone else's pending proposal. -->
    <div v-if="pending && (lastErrorText || overlapNotices.length)" class="order-notice flex-wrap" role="status">
      <AlertTriangle :size="12" class="shrink-0" />
      <span class="min-w-0 flex-1">
        <span v-if="lastErrorText" class="block">{{
          $t('No se aplicó ({when}): {why}', { when: whenText(proposal.lastError?.at), why: lastErrorText })
        }}</span>
        <span
          v-for="(line, i) in overlapNotices"
          :key="i"
          class="block"
          :title="$t('Aplicar las dos escribiría lo mismo dos veces: aplica una y descarta o corrige la otra')"
          >{{ line }}</span
        >
      </span>
    </div>
    <!-- Edited in the sheet after this proposal: how many, what applying does, to the next one; and telling the assistant. -->
    <div v-if="pending && edited.length" class="sheet-banner" role="status">
      <button
        v-if="editedCells.length"
        type="button"
        class="sheet-banner-go"
        :title="$t('Alguien editó estas celdas en la hoja después de la propuesta. Junto a cada una: mantener el valor de la hoja o usar el de la propuesta. Clic: ir a la siguiente')"
        @click="reviewNext('sheet')"
      >
        <Table2 :size="13" class="shrink-0" />
        {{
          $tn(
            editedCells.length,
            '{n} celda se editó en la hoja después de esta propuesta',
            '{n} celdas se editaron en la hoja después de esta propuesta',
          )
        }}
        ·
        <template v-if="!overwritten">{{ $t('se mantienen los valores de la hoja') }}</template>
        <template v-else-if="overwritten === editedCells.length">{{ $t('se escriben los de la propuesta encima') }}</template>
        <template v-else>{{ $tn(overwritten, '{n} con el valor de la propuesta', '{n} con el valor de la propuesta') }}</template>
        · <span class="font-semibold">{{ $t('revisar') }}</span><ArrowRight :size="12" />
      </button>
      <span v-if="takenRows">
        {{
          $tn(
            takenRows,
            '{n} fila nueva sin escribir: su fila sin usar ya se usó en la hoja',
            '{n} filas nuevas sin escribir: su fila sin usar ya se usó en la hoja',
          )
        }}
      </span>
      <button
        v-if="editable"
        type="button"
        class="sheet-banner-tell"
        :disabled="telling"
        :title="$t('Le dice al asistente qué celdas cambió la hoja, para que las vuelva a mirar y corrija la propuesta (en su chat, o copiado para pegarlo)')"
        @click="tell"
      >
        <Send :size="12" /> {{ $t('Avisar al asistente') }}
      </button>
    </div>
    <div v-for="g in tables" :key="g.id" class="border-b border-stone-100 last:border-b-0">
      <!-- Rows off the photo where the page's error repeats: their own table, each row's note says which line it follows. -->
      <p
        v-if="g.near"
        class="flex items-center gap-1 px-2 pt-1.5 text-[11px] font-medium text-amber-900"
        :title="$t('Filas cerca de la página con el mismo error de tecleo que una de sus líneas; no están en la foto. Revisa cada celda dudosa')"
      >
        <AlertTriangle :size="12" />
        {{ $tn(g.rows.length, 'Mismo error cerca (no en la foto) · {n} fila', 'Mismo error cerca (no en la foto) · {n} filas') }}
      </p>
      <!-- A notebook page: per photo, its thumbnail (opens upright in a new tab), why it is there and how its lines compare with the sheet. -->
      <div v-if="page && g.photos.length" class="flex flex-wrap gap-2 px-2 pt-1.5">
        <div
          v-for="p in g.photos"
          :key="p.photo"
          class="flex items-center gap-2 rounded border border-stone-200 bg-stone-50 py-1 pr-2 pl-1 text-[11px] text-stone-600"
        >
          <a
            v-if="p.photo < page.photos && !brokenPhotos.has(p.photo)"
            :href="photoUrl(p.photo, 'view')"
            target="_blank"
            rel="noopener"
            class="shrink-0"
            :title="reviewing ? $t('Ver esta foto') : $t('Revisar con esta foto (Ctrl+clic: en una pestaña nueva)')"
            @click="openPhoto(p.photo, $event)"
          >
            <img
              :src="photoUrl(p.photo, 'thumb')"
              :alt="$t('Foto {n} del cuaderno', { n: p.photo + 1 })"
              class="h-14 w-auto max-w-24 rounded border border-stone-300 bg-white object-contain"
              loading="lazy"
              @error="brokenPhotos = new Set([...brokenPhotos, p.photo])"
            />
          </a>
          <span>
            <b v-if="g.photos.length > 1 || photoNote(p.photo)" class="font-medium text-stone-700">{{
              $t('Foto {n}', { n: p.photo + 1 })
            }}</b>
            <template v-if="photoNote(p.photo)"> · {{ photoNote(p.photo) }}</template>
            <template v-if="p.to"
              ><template v-if="g.photos.length > 1 || photoNote(p.photo)"> · </template
              >{{ $t('Líneas {from}–{to}', { from: p.from, to: p.to }) }} ·
              {{ $tn(p.change, '{n} cambia', '{n} cambian') }} · {{ $tn(p.same, '{n} igual', '{n} iguales') }}
              <template v-if="p.other"> · {{ $tn(p.other, '{n} sin escribir', '{n} sin escribir') }}</template>
            </template>
          </span>
        </div>
      </div>
      <!-- The table and its bar, where ProposalSheet adds the buttons for the selected cells (Valor de la hoja / de la IA). -->
      <ProposalSheet
        :ref="sheetRef(g.id)"
        :sheet="g.sheet"
        :changes="g.rows"
        :laid="g.laid"
        :notebook-order="g.paged && rowOrder === 'notebook'"
        :load-rows="sheetRows(g.sheet)"
        :fields="g.columns"
        :types="typesOf(g.sheet)"
        :new-row-formulas="proposal.newRowFormulas?.[g.sheet] ?? []"
        :editable="editable"
        :applied="proposal.status === 'applied' ? (proposal.applied ?? []) : null"
        :flash="flash"
        :history-key="`${proposal.id}:${g.id}`"
        @edit="onEdit"
        @remove="removeRow"
        @check="onCheck"
        @sheet="onSheet"
        @next="(from, which) => reviewNext(which ?? 'doubtful', from, which === 'sheet')"
        @notice="m => notify(m)"
        @select="onSelect"
      >
        <template v-if="!g.near" #default>
          <span v-if="groups.length > 1" class="font-medium text-stone-700">{{ g.sheet }}</span>
          <!-- A notebook page's rows: the sheet's order or the photo's lines (remembered for them). -->
          <template v-if="g.paged">
            <span>{{ $t('Filas') }}</span>
            <span class="view-switch" role="group" :aria-label="$t('Orden de las filas')">
              <button
                v-for="[o, label, tip] in rowOrders"
                :key="o"
                type="button"
                :class="{ 'is-on': rowOrder === o }"
                :aria-pressed="rowOrder === o"
                :title="tip"
                @click="choices.setRowOrder(o)"
              >
                {{ label }}
              </button>
            </span>
            <span>{{ $t('Columnas') }}</span>
          </template>
          <!-- The columns' order: the sheet's, the notebook's, the person's own (remembered for them). -->
          <span class="view-switch" role="group" :aria-label="$t('Orden de las columnas')">
            <button
              v-for="[v, label, tip] in views"
              :key="v"
              type="button"
              :class="{ 'is-on': view === v }"
              :aria-pressed="view === v"
              :title="tip"
              @click="setView(v)"
            >
              {{ label }}
            </button>
          </span>
          <button
            v-if="view === 'custom'"
            type="button"
            class="flex items-center gap-0.5 hover:text-stone-800"
            :class="{ 'text-emerald-800': choosing === g.sheet }"
            :title="$t('Elegir y ordenar tus columnas')"
            :aria-expanded="choosing === g.sheet"
            @click="openChooser(g.sheet)"
          >
            <Settings2 :size="12" /> {{ $t('Columnas…') }}
          </button>
          <label
            v-if="g.quiet"
            class="flex cursor-pointer items-center gap-1"
            :title="$t('Oculta las líneas de la página que no escriben nada (iguales a la hoja o tachadas)')"
          >
            <input v-model="changesOnly" type="checkbox" class="h-3 w-3" />
            <ListFilter :size="12" /> {{ $t('Solo cambios') }}
          </label>
          <button
            v-if="g.template.length"
            type="button"
            class="flex items-center gap-0.5 hover:text-stone-800"
            :title="$t('Columnas que solo llevan NA o NOT_COLLECTED de la plantilla: se escriben igual, aunque estén plegadas')"
            @click="templatesOpen = !templatesOpen"
          >
            <Columns3 :size="12" />
            {{
              templatesOpen
                ? $t('Plegar la plantilla')
                : $tn(g.template.length, '+{n} columna de plantilla', '+{n} columnas de plantilla')
            }}
          </button>
          <button
            v-if="editable && g.changes.some(c => c.create)"
            class="flex items-center gap-0.5 hover:text-emerald-800"
            :title="$t('Añadir una fila nueva vacía a esta hoja')"
            @click="addRow(g.sheet)"
          >
            <Plus :size="12" /> {{ $t('Fila') }}
          </button>
          <select
            v-if="editable && addable(g.sheet, [...g.fields, ...g.columns]).length"
            class="rounded border border-stone-200 bg-white px-1 py-0.5 text-[11px]"
            :aria-label="$t('Añadir columna')"
            @change="addColumn(g.sheet, $event)"
          >
            <option value="">{{ $t('+ Columna…') }}</option>
            <option v-for="f in addable(g.sheet, [...g.fields, ...g.columns])" :key="f" :value="f">{{ f }}</option>
          </select>
        </template>
        <template v-if="!g.near && view === 'custom' && choosing === g.sheet" #panel>
          <ColumnChooser
            :sheet="g.sheet"
            v-bind="chooserOf(g)"
            @order="list => customOrder(g, list)"
            @toggle="(f, on) => customToggle(g, f, on)"
            @reset="choices.setCustom(g.sheet, null)"
            @close="choosing = null"
          />
        </template>
        <template v-if="editable && !g.near" #end>
          <span class="ml-auto flex items-center gap-2">
            <span class="legend is-proposed" :title="$t('Valor de la IA: se escribe al aplicar')">{{ $t('IA') }}</span>
            <span class="legend is-person" :title="$t('Escrito por ti: se escribe al aplicar')">{{ $t('tú') }}</span>
            <span class="legend is-sheet" :title="$t('Valor actual de la hoja: no cambia')">{{ $t('hoja') }}</span>
            <span class="legend is-reverted" :title="$t('Vuelto al valor de la hoja: la sugerencia de la IA no se aplica')">{{
              $t('IA sin aplicar')
            }}</span>
            <span
              v-if="g.changes.some(c => c.doubts)"
              class="legend is-doubtful"
              :title="$t('La IA no está segura: revísala antes de aplicar')"
              >{{ $t('dudosa') }}</span
            >
            <span
              v-if="g.changes.some(c => c.unreadable)"
              class="legend is-unreadable"
              :title="$t('La IA no pudo leerla: escribe el valor; vacía no se escribe')"
              >{{ $t('ilegible') }}</span
            >
            <span
              v-if="g.changes.some(c => c.inferred?.length)"
              class="legend is-inferred"
              :title="$t('No está escrito en la línea: sale de la página, de la nota o de lo que el equipo escribe siempre')"
              >{{ $t('deducida') }}</span
            >
            <span
              v-if="g.changes.some(c => c.sheetChanged || c.rowTaken)"
              class="legend is-sheet-edit"
              :title="$t('Editada en la hoja después de la propuesta: se mantiene el valor de la hoja salvo que elijas el de la propuesta')"
              >{{ $t('editada en la hoja') }}</span
            >
            <span
              v-if="g.changes.some(c => c.formulaCells?.length)"
              class="legend is-formula-write"
              :title="$t('La propuesta cambia la fórmula de la celda: se ve lo que dará; la fórmula, al pasar el ratón y en la barra')"
              >{{ $t('cambia la fórmula') }}</span
            >
            <span
              v-if="g.changes.some(c => c.formulaGives || c.formulaFallback || c.formulas?.length)"
              class="legend is-formula-gives"
              :title="$t('Calculado por la fórmula de la hoja con los valores propuestos: no se escribe')"
              >{{ $t('fórmula') }}</span
            >
            <span
              v-if="g.changes.some(c => c.context && !c.page?.error)"
              class="legend is-context"
              :title="
                g.changes.some(c => c.context && !c.gap)
                  ? $t('Línea de la página que no escribe nada: solo para seguirla')
                  : $t('Fila de la hoja que la propuesta no cambia: se muestra para leer en orden')
              "
              >{{ $t('sin cambios') }}</span
            >
            <span
              v-if="g.changes.some(c => c.page?.error)"
              class="legend is-line-error"
              :title="$t('La hoja no aceptaría esta línea: su nota dice por qué')"
              >{{ $t('rechazada') }}</span
            >
            <span
              v-if="g.changes.some(c => c.highlight)"
              class="legend is-marked"
              :title="$t('Fila marcada por la IA para que la mires')"
              >{{ $t('marcada') }}</span
            >
          </span>
        </template>
      </ProposalSheet>
    </div>
    <div class="flex flex-wrap items-center gap-2 px-2 py-1.5">
      <template v-if="pending">
        <button class="btn-primary bg-emerald-700 hover:bg-emerald-800" :disabled="busy || !chosen.length" @click="apply()">
          <Check :size="15" /> {{ $tn(chosen.length, 'Aplicar {n} fila', 'Aplicar {n} filas') }}
        </button>
        <button class="btn" :disabled="busy" @click="emit('discard')"><X :size="15" /> {{ $t('Descartar') }}</button>
        <span class="hint">
          <template v-if="saving">{{ $t('Guardando tus cambios…') }}</template>
          <template v-else-if="failed">{{ $t('Sin conexión: tus cambios se guardarán al volver') }}</template>
          <template v-else>
            <template v-if="personCells > setAside"
              >{{ $tn(personCells - setAside, '{n} celda editada por ti', '{n} celdas editadas por ti') }} ·
            </template>
            <template v-if="setAside"
              >{{ $tn(setAside, '{n} sugerencia de la IA sin aplicar', '{n} sugerencias de la IA sin aplicar') }} ·
            </template>
          </template>
          {{ $t('Corrige en la tabla o díselo al asistente; también puedes responder «sí, aplícalo» en el chat.') }}
        </span>
      </template>
      <span
        v-else
        class="flex items-center gap-1 text-xs"
        :class="proposal.status === 'applied' ? 'text-brand-700' : 'text-amber-800'"
        :role="proposal.status === 'queued' ? 'status' : undefined"
      >
        <Clock v-if="proposal.status === 'queued'" :size="13" class="shrink-0" />
        {{ statusText }}
        <template v-if="proposal.status === 'needs_review' && lastErrorText"> · {{ lastErrorText }}</template>
      </span>
    </div>
    <!-- "Aplicar" while Google Sheets does not answer as usual: the changes wait in the app until it does. -->
    <div v-if="waitAsk && pending" class="doubt-ask" role="alertdialog" :aria-label="$t('Google Sheets no responde')">
      <p class="flex items-start gap-1.5 font-medium">
        <Clock :size="14" class="mt-0.5 shrink-0" />
        {{
          $t(
            'Google Sheets no está respondiendo como siempre (está recalculando). Los cambios se guardarán en la app y se escribirán cuando responda. ¿Aplicar ahora y guardarlos aquí?',
          )
        }}
      </p>
      <div class="mt-1.5 flex flex-wrap gap-2">
        <button class="btn" :disabled="busy" @click="apply(waitAsk.how, waitAsk.leave, true)">
          <Check :size="14" /> {{ $t('Aplicar y guardarlos aquí') }}
        </button>
        <button class="btn" @click="waitAsk = null"><X :size="14" /> {{ $t('Cancelar') }}</button>
      </div>
    </div>
    <!-- "Aplicar" with doubtful cells nobody reviewed, or unreadable ones still empty: the person decides what happens to them. -->
    <div
      v-if="asking && pending"
      class="doubt-ask"
      :class="{ 'is-unread': !doubtfulToWrite.length }"
      role="alertdialog"
      :aria-label="doubtfulToWrite.length ? $t('Celdas dudosas sin revisar') : $t('Celdas ilegibles sin rellenar')"
    >
      <template v-if="doubtfulToWrite.length">
        <p class="font-medium">
          {{
            $tn(
              doubtfulToWrite.length,
              '{n} celda dudosa sin revisar: ¿aplicarla como la leyó la IA?',
              '{n} celdas dudosas sin revisar: ¿aplicarlas como las leyó la IA?',
            )
          }}
        </p>
        <p class="text-stone-600">
          {{ $t('Revísalas en la tabla (bordes ámbar con «?»): «Correcta» u otra lectura junto a cada una, o edítala.') }}
        </p>
      </template>
      <p v-if="unreadable.length" :class="doubtfulToWrite.length ? 'unread-line' : 'font-medium'">
        {{
          $tn(
            unreadable.length,
            '{n} celda ilegible sigue vacía: al aplicar no se escribe (queda como está en la hoja).',
            '{n} celdas ilegibles siguen vacías: al aplicar no se escriben (quedan como están en la hoja).',
          )
        }}
      </p>
      <div class="mt-1.5 flex flex-wrap gap-2">
        <template v-if="doubtfulToWrite.length">
          <button class="btn" :disabled="busy" @click="reviewNext('doubtful')"><CircleHelp :size="14" /> {{ $t('Revisarlas') }}</button>
          <button class="btn" :disabled="busy" @click="apply('skip')">{{ $t('Aplicar sin las dudosas') }}</button>
          <button class="btn" :disabled="busy" @click="apply('confirm')">{{ $t('Aplicar todo igualmente') }}</button>
        </template>
        <template v-else>
          <button class="btn" :disabled="busy" @click="reviewNext('unreadable')"><SquarePen :size="14" /> {{ $t('Rellenarlas') }}</button>
          <button class="btn" :disabled="busy || !chosen.length" @click="apply(undefined, true)">{{ $t('Aplicar sin ellas') }}</button>
        </template>
        <button class="btn" @click="asking = false"><X :size="14" /> {{ $t('Cancelar') }}</button>
      </div>
    </div>
  </div>
</template>
