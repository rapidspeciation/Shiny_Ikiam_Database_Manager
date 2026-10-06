<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref, shallowRef, watch } from 'vue'
import { TabulatorFull as Tabulator } from 'tabulator-tables'
import type { CellComponent, ColumnDefinition, RowComponent } from 'tabulator-tables'
import 'tabulator-tables/dist/css/tabulator_simple.min.css'
import { Check, CheckCheck, FileSpreadsheet, Redo2, Sparkles, Table2, Undo2 } from 'lucide-vue-next'
import { displayValue, editText, normalizeInput } from '../../lib/cells'
import { copyText } from '../../lib/clipboard'
import {
  activeCell,
  attachColumnFit,
  attachCopyMarker,
  attachFillHandle,
  attachPendingCut,
  cutCellId,
  attachTouchSheet,
  plainCopy,
  backToGrid,
  choiceEditor,
  editingKeys,
  followSelection,
  longText,
  openList,
  selectedCell,
  setFromBar,
  spread,
  spreadsheetKeys,
  textEditor,
  tileToSelection,
  typingPending,
  watchSize,
  type CanEdit,
  type CellBarInfo,
  type CellBarNote,
  type Direction,
  type PendingCut,
} from '../../lib/gridKit'
import { parseBlock } from '../../lib/paste'
import {
  ID_COLUMN,
  cellId,
  cellComments,
  cellOf,
  editedBy,
  isFormulaError,
  keptOver,
  pageNote,
  pageOnly,
  readOnlyRow,
  rowKey,
  selectionActions,
  whenText,
  writtenFields,
  type CellComment,
  type CellInfo,
  type ProposalChange,
} from '../../lib/proposals'
import {
  baseRowKey,
  hiddenRange,
  markerKey,
  markerText,
  offPhoto,
  peekText,
  repeatChip,
  withPeeks,
  type Laid,
  type Marker,
  type Peek,
  type PeekState,
  type SheetRows,
} from '../../lib/proposalRows'
import { StepBuilder, UndoHistory, historyFor, snapCell, type Step } from '../../lib/proposalUndo'
import { errorText } from '../../lib/notice'
import { isSumField, sumTotal } from '../../lib/sums'
import type { CellValue, Field } from '../../lib/types'
import { listProblem, verificationsFor } from '../../lib/verifications'
import { locale, t, tn } from '../../lib/i18n'
import CellBar from '../CellBar.vue'

/**
 * The rows of one sheet of a proposal as a spreadsheet, like the Colecta list:
 * Enter/Tab move, typing replaces, the fill handle and Ctrl+D copy down, blocks
 * paste from Excel or Sheets, list columns open the sheet's list, dates are
 * typed day first. The assistant's values are green, the person's blue, an
 * existing row's other cells grey; cells the assistant just changed flash;
 * counts written as sums show their total apart (=12+15 then "= 27"). The
 * selected cells can go back to the sheet's value (the assistant's is kept
 * aside, dashed, and not written) or take the assistant's value again. The ID
 * stays at the left and the column names at the top while scrolling (see
 * columns() and the table's maxHeight). Edits go out through `edit` (the
 * parent saves them to the proposal). Cells the assistant read with a doubt are
 * amber, dashed, with a "?" until someone reviews them: edits one, picks one
 * of its other readings beside it (or in the cell bar), takes it as correct
 * there or with Ctrl+Enter (then on to the next, `next`), or marks the
 * selection checked (`check`). Values the notebook line does not write (a
 * template, the note's words, the page's room) are in italics. What the
 * assistant says about a cell (a doubt, where a value comes from, why it was
 * unreadable) gives it a corner mark: its tooltip and the bar say it. Its note
 * on the row is the "Nota IA" column beside the ID.
 * Cells the assistant could not read at all are hatched red with an
 * "unreadable" tag, empty: the person types them (the bar gives why and what
 * of it was read, to complete); left empty, applying does not write them.
 * The CAM or tube a preserved butterfly would be left without (server/preserved.mjs)
 * are amber with a "missing" tag until someone fills them.
 * A notebook page's lines that write nothing are grey and read-only (as the
 * sheet has them, or as written when the sheet has no such row; a line the
 * save refused is red, with why); a "Línea" column gives each row's line.
 * What the SPECIES formula will give (from the clutch) shows grey, in
 * italics, tagged "fórmula": it is never written.
 * A cell someone edited in the sheet after the proposal read it is violet,
 * tagged "hoja": the sheet's value stays (the proposal's struck through after
 * it) unless the person chose the proposal's; its comment says what was read
 * and what the sheet has now, by whom and when, and the buttons beside it
 * (`sheet`) keep the sheet's value or use the proposal's. A new row whose
 * pre-made row was used meanwhile is violet too, and is not written.
 * The rows go as `laid` says (lib/proposalRows): slim rows between them tell
 * where they are not continuous in the sheet (or, in the notebook's order, how
 * far the next line's row jumps), and which rows are not on the photo. A
 * repeated ID (A0E.1) has a chip «repeat of A0E (row …)», which goes to that
 * row when the table shows it. A click on a slim row that stands for sheet
 * rows the table does not show (`loadRows`) opens them under it, grey and to
 * read only (up to PEEK_ROWS, then a slim row for the rest); another click
 * folds them.
 * Ctrl+X only marks the cells (dashed) until they are pasted: Ctrl+V then moves
 * them in one go; Esc or another copy leaves them (gridKit's attachPendingCut).
 * Ctrl+Z / Ctrl+Shift+Z (Ctrl+Y), or the buttons above, undo and redo the
 * person's own edits in the table, a paste or a fill as one step
 * (lib/proposalUndo); cells the assistant changed since stay as it left them.
 */
export interface CellEdit {
  key: string
  field: string
  value: CellValue
  /** What the cell showed when the person started typing. */
  before: CellValue
  /** One of the buttons: back to the sheet's value, or the assistant's again (`value` then holds it). */
  use?: 'sheet' | 'ai'
}
const props = defineProps<{
  sheet: string
  changes: ProposalChange[]
  fields: string[]
  types: Record<string, string>
  /** Formula columns of the pre-made rows new rows go into. */
  newRowFormulas: string[]
  editable: boolean
  /** An applied proposal: the rows written (a ✓ beside them); null otherwise. */
  applied: number[] | null
  /** Cells to flash (the assistant just changed them). */
  flash: Set<string>
  /** The rows in the order shown, with the markers between them (lib/proposalRows); else `changes` as they are. */
  laid?: Laid[]
  /** The rows go as the notebook has them (no ↕ marks: the jumps say it). */
  notebookOrder?: boolean
  /** The sheet's rows `from`–`to` as they are now, for a slim row opened with a click (none: they do not open). */
  loadRows?: (from: number, to: number) => Promise<SheetRows>
  /** Where the table's undo steps are kept while the page is open (the proposal and the table); none: while it is shown. */
  historyKey?: string
}>()
const emit = defineEmits<{
  edit: [cells: CellEdit[]]
  remove: [key: string]
  notice: [message: string]
  /** Doubtful cells the person reviewed and leaves as they are («Marcar revisadas»); `checked: false` (an undo) unmarks them. */
  check: [cells: { key: string; field: string; checked?: boolean }[]]
  /** On to the doubtful cell (or the cell edited in the sheet) after this one (null: from the top), in any of the proposal's tables. */
  next: [from: { key: string; field: string } | null, which?: 'doubtful' | 'sheet']
  /** Cells edited in the sheet: the sheet's value kept, or the proposal's written over it. */
  sheet: [cells: { key: string; field: string; use: 'sheet' | 'proposal' }[]]
  /** The row of the selected cell (its key), when it changes: «Revisar con la foto» shows its photo. */
  select: [key: string]
}>()

type Row = Record<string, CellValue> & {
  __key: string
  __done: string
  __row: string
  __label: string
  __note: string
  __line: string
  __state: string
  /** A slim row between the rows (lib/proposalRows): its kind. */
  __marker?: string
}

const host = ref<HTMLDivElement>()
let table: Tabulator | null = null
let built = false
let byKey = new Map<string, ProposalChange>()
const touch = typeof window !== 'undefined' && window.matchMedia('(pointer: coarse)').matches
// On a phone only the ID stays in place (as the first column): row and ID together took half the screen.
const wide = typeof window !== 'undefined' && window.matchMedia('(min-width: 768px)').matches
const rules = computed(() => verificationsFor(props.sheet))
const fieldSet = computed(() => new Set(props.fields))
const typeOf = (field: string) => (props.types[field] ?? 'text') as Field['type']
const show = (field: string, value: CellValue | undefined) => displayValue(value, { key: field, type: typeOf(field) })
/** What a count written as a sum (=12+15) adds up to, shown after it; null for other cells. */
const totalOf = (field: string, value: CellValue | undefined) => (isSumField(props.sheet, field) ? sumTotal(value) : null)
/** The cell's text with a sum's total apart after it: "=12+15" then "= 27" in its own colour. */
function withTotal(field: string, value: CellValue | undefined, text: string): Node {
  const total = totalOf(field, value)
  if (total === null) return document.createTextNode(text)
  const box = document.createElement('span')
  const sum = document.createElement('span')
  sum.className = 'sum-total'
  sum.textContent = `= ${total}`
  box.append(text, sum)
  return box
}
/** The same as plain text, to size the column (the total's box counts as a few letters more). */
const textWithTotal = (field: string, value: CellValue | undefined) => {
  const total = totalOf(field, value)
  return show(field, value) + (total === null ? '' : `  = ${total}  `)
}

function info(key: string, field: string) {
  const change = byKey.get(key)
  return change ? cellOf(change, field, props.newRowFormulas) : null
}
const canEditCell = (key: string, field: string) => {
  const change = byKey.get(key)
  return props.editable && !!change && !readOnlyRow(change) && fieldSet.value.has(field) && info(key, field)?.kind !== 'locked'
}
/** Why a row is only to read: a page line that writes nothing, or a sheet row between the proposal's rows. */
const readOnlyText = (change: ProposalChange) =>
  change.gap
    ? t('Fila de la hoja que la propuesta no cambia: se muestra para leer en orden')
    : t('Línea de la página que no escribe nada: solo para seguirla')
/** The rows come from a notebook page: a "Línea" column (photo and line, when the page has several photos). */
const paged = () => props.changes.some(c => c.page)
const severalPhotos = () => new Set(props.changes.map(c => c.page?.photo ?? 0)).size > 1
const lineText = (c: ProposalChange) => (!c.page ? '' : severalPhotos() ? `${c.page.photo + 1}·${c.page.line}` : String(c.page.line))
const canEdit: CanEdit = (row, field) => canEditCell((row.getData() as Row).__key, field)
/** A list to pick from: the sheet's dropdown, except for identifiers (typed or pasted). */
const choicesOf = (field: string) => (ID_COLUMN.test(field) ? null : rules.value?.lists[field]?.values)
const hasChoices = (field: string) => !!choicesOf(field)?.size

function toRow(c: ProposalChange): Row {
  const key = rowKey(c)
  const out = {
    __key: key,
    __done: props.applied?.includes(c.index) ? '✓' : '',
    // A page line with no sheet row: no row number (it is not a new row either).
    __row: c.row ? String(c.row) : c.placeholder ? '—' : t('nueva'),
    __label: c.label,
    // A new row whose pre-made row someone used meanwhile: why it is not written, then its note.
    __note: c.rowTaken
      ? [t('Su fila sin usar ({row}) ya se usó en la hoja: esta fila no se escribe', { row: c.rowTaken.row }), pageNote(c)]
          .filter(Boolean)
          .join(' · ')
      : pageNote(c),
    __line: lineText(c),
  } as Row
  let state = ''
  for (const f of props.fields) {
    const cell = cellOf(c, f, props.newRowFormulas)
    out[f] = cell.value
    state += cell.kind[0] + (cell.doubtful ? '?' : '') + (cell.fromFormula ? 'f' : '') + (props.flash.has(cellId(key, f)) ? '*' : '')
  }
  // Markers can change without the value (whose edit it is, a flash, a doubt checked): part of the row's signature.
  out.__state =
    state +
    JSON.stringify(c.personEdits ?? null) +
    JSON.stringify(c.doubts ?? null) +
    JSON.stringify(c.unreadable ?? null) +
    JSON.stringify(c.warnings ?? null) +
    JSON.stringify(c.formulaNotes ?? null) +
    JSON.stringify(c.checks ?? null) +
    JSON.stringify(c.sheetChanged ?? null) +
    JSON.stringify(c.rowTaken ?? null) +
    JSON.stringify(c.outOfOrder ?? null) +
    JSON.stringify(c.repeatOf ?? null) +
    (props.notebookOrder ? 'n' : '') +
    (props.editable ? 'e' : '') +
    (c.context ? 'c' : '') +
    (c.page?.error ? 'x' : '') +
    Object.keys(c.values).length
  return out
}

/** A slim row between the rows: its text in the first column that stays at the left. */
const markerField = () => (wide ? '__row' : '__label')
type MarkerState = Marker & { peek?: PeekState }
function markerRow(m: Marker, next: string, peek?: PeekState): Row {
  return {
    __key: `mark:${markerKey(m)}:${next}`,
    __marker: m.kind,
    __done: '',
    __row: '',
    __label: '',
    __note: '',
    __line: '',
    __state: JSON.stringify({ ...m, ...(peek ? { peek } : {}) }),
  } as Row
}
/** A slim row a click opens: it stands for sheet rows the table does not show, and they can be read. */
const peekable = (m: Marker) => !!props.loadRows && !!hiddenRange(m)
/** A marker row's cell: its text (it overflows the columns beside it), the rest empty. */
function markerCell(cell: CellComponent): Node | string {
  const row = cell.getData() as Row
  const el = cell.getElement()
  const here = cell.getField() === markerField()
  el.classList.toggle('is-marker-cell', here)
  if (!here) return ''
  const m = JSON.parse(row.__state) as MarkerState
  const { text, title } = markerText(m)
  const peek = peekable(m) ? peekText(m, m.peek) : null
  el.title = [title, peek?.title].filter(Boolean).join('\n')
  const box = document.createElement('span')
  box.className = `marker-text is-${m.kind}${m.kind === 'jump' ? (m.by > 0 ? '-down' : '-up') : ''}`
  box.textContent = text
  if (!peek?.action) return box
  // Opened (or on its way): «hide», and how many of its rows the table shows already.
  const both = document.createElement('span')
  const action = document.createElement('span')
  action.className = `marker-action is-${m.peek?.state ?? 'closed'}`
  action.textContent = peek.action
  both.append(box, ' ', action)
  return both
}

/** The ID, with a repeat's chip («repeat of A0E (row …)»: a click goes to that row when the table shows it). */
function labelFormatter(cell: CellComponent) {
  const row = cell.getData() as Row
  if (row.__marker) return markerCell(cell)
  const change = byKey.get(row.__key)
  const chip = change ? repeatChip(change) : null
  if (!chip) return document.createTextNode(row.__label)
  const box = document.createElement('span')
  const mark = document.createElement('span')
  mark.className = 'repeat-chip'
  mark.textContent = chip.text
  const target = baseRowKey(change!, props.changes, rowKey)
  mark.title = target ? `${chip.title}. ${t('Clic: ir a esa fila')}` : chip.title
  if (target) {
    mark.classList.add('is-link')
    mark.addEventListener('click', e => {
      e.stopPropagation()
      focusCell(target, '__label')
    })
  }
  box.append(row.__label, ' ', mark)
  return box
}
/** The page's line; a row with none, in a page's table, says it is not on the photo. */
function lineFormatter(cell: CellComponent) {
  const row = cell.getData() as Row
  if (row.__marker) return ''
  const change = byKey.get(row.__key)
  if (!change || !offPhotoRows() || !offPhoto(change)) return document.createTextNode(row.__line)
  const mark = document.createElement('span')
  mark.className = 'off-photo'
  mark.textContent = t('no en la foto')
  mark.title = t('Esta fila no está en las fotos de la página: añadida a mano o encontrada fuera de la página')
  return mark
}
/** Rows not on the photo in a page's table, in the sheet's order: the line column makes room for their tag (in the notebook's, a heading says it). */
const offPhotoRows = () => paged() && !props.notebookOrder && props.changes.some(offPhoto)
/** Rows marked ↕ (sheet order): the row column makes room for the mark. */
const markedRows = () => !props.notebookOrder && props.changes.some(c => c.outOfOrder)
/** Repeated IDs: the ID column makes room for their chip. */
const repeatRows = () => props.changes.some(c => c.repeatOf)

/**
 * The cell as the table shows it: the value, and in an existing row the sheet's
 * value struck through; a corner mark when the assistant says something about it
 * (its tooltip, and the bar when selected, say what).
 */
function formatter(field: string) {
  return (cell: CellComponent) => {
    const row = cell.getData() as Row
    const el = cell.getElement()
    const c = info(row.__key, field)
    if (!c) return ''
    const comments = cellComments(c, v => show(field, v))
    el.classList.toggle('has-comment', comments.length > 0)
    const content = drawn(cell, field, c, comments)
    if (!comments.length) return content
    const box = document.createElement('span')
    const mark = document.createElement('span')
    mark.className = 'comment-mark'
    mark.setAttribute('aria-hidden', 'true')
    box.append(content, mark)
    return box
  }
}
/** The cell's content, its look (classes) and its tooltip, which leads with the assistant's `comments`. */
function drawn(cell: CellComponent, field: string, c: CellInfo, comments: CellComment[]): Node {
  const row = cell.getData() as Row
  const el = cell.getElement()
  const change = byKey.get(row.__key)!
  const changed = c.kind === 'proposed' || c.kind === 'person'
  const problem = changed ? listProblem(rules.value, field, c.value) : null
  el.classList.toggle('is-proposed', c.kind === 'proposed')
  el.classList.toggle('is-person', c.kind === 'person')
  el.classList.toggle('is-reverted', c.kind === 'reverted')
  el.classList.toggle('is-sheet', c.kind === 'sheet')
  // A formula cell, as it is or with what it will give once applied: the sheet's formula grey, never written.
  el.classList.toggle('is-formula', c.kind === 'locked' || !!c.fromFormula || !!c.formulaFallback)
  el.classList.toggle('is-formula-error', (c.kind === 'locked' || !!c.fromFormula) && isFormulaError(c.value))
  el.classList.toggle('is-formula-stale', !!c.formulaFallback && !c.fromFormula)
  el.classList.toggle('is-invalid', !!problem)
  el.classList.toggle('is-flash', props.flash.has(cellId(row.__key, field)))
  el.classList.toggle('has-choices', canEditCell(row.__key, field) && hasChoices(field))
  el.classList.toggle('is-doubtful', c.doubtful)
  el.classList.toggle('is-inferred', c.inferred)
  el.classList.toggle('is-unreadable', c.kind === 'unreadable')
  el.classList.toggle('is-warned', !!c.warning)
  el.classList.toggle('is-formula-gives', !!c.fromFormula || !!c.formulaWrite)
  el.classList.toggle('is-formula-write', !!c.formulaWrite)
  el.classList.toggle('is-sheet-edit', !!c.sheetEdit)
  el.classList.toggle('is-kept', c.kind === 'kept')
  el.classList.toggle('is-again', !!c.sheetEdit?.again)
  const was = c.was === undefined ? '' : show(field, c.was) || t('vacío')
  const ai = show(field, c.ai) || t('vacío')
  const before = change.replaceFormula?.includes(field) ? 'Antes: {value} (fórmula)' : 'Antes: {value}'
  // What the assistant says about it first, then what the cell is.
  el.title = [
    ...comments.map(n => `${n.label}: ${n.text}`),
    c.doubtful && c.doubt?.alternatives?.length
      ? t('otras lecturas: {values}', { values: c.doubt.alternatives.map(a => show(field, a) || t('vacío')).join(' / ') })
      : '',
    c.kind === 'unreadable' && c.unreadable?.partial?.length
      ? t('leído en parte: {values}', { values: c.unreadable.partial.join(' / ') })
      : '',
    problem,
    c.formulaWrite ? t('Cambia la fórmula de la celda: {formula}', { formula: String(c.value ?? '') }) : '',
    c.formulaWrite && c.computed !== undefined ? t('Dará: {value}', { value: show(field, c.computed) || t('vacío') }) : '',
    c.formulaWrite && c.oldFormula ? t('Fórmula actual: {formula}', { formula: c.oldFormula }) : '',
    c.kind === 'proposed' && !change.create ? t(before, { value: was }) : '',
    c.kind === 'person'
      ? [
          t('Editado por ti'),
          c.aiProposed ? t('la IA proponía: {value}', { value: ai }) : '',
          change.create ? '' : t('en la hoja: {value}', { value: was }),
        ]
          .filter(Boolean)
          .join(' · ')
      : '',
    c.kind === 'reverted'
      ? [
          change.create ? t('Vacía: no se escribe') : t('Valor de la hoja: no cambia'),
          t('la IA proponía: {value}', { value: ai }),
        ].join(' · ')
      : '',
    c.kind === 'kept'
      ? t('Se mantiene el valor de la hoja; la propuesta decía: {value}', { value: show(field, c.proposalValue) || t('vacío') })
      : '',
    c.sheetEdit && props.editable ? t('Elige junto a la celda: el valor de la hoja o el de la propuesta') : '',
    (c.kind === 'locked' || c.fromFormula) && isFormulaError(c.value)
      ? t('La fórmula de la hoja da un error con estos valores: revísalo antes de aplicar')
      : '',
    c.fromFormula
      ? t('Calculado por la fórmula de la hoja con los valores propuestos: no se escribe')
      : c.formulaFallback
        ? t('No se pudo calcular aquí: es el valor actual de la hoja, que la fórmula puede cambiar al aplicar')
        : c.kind === 'locked'
          ? t('Calculado por la fórmula de la hoja: no se escribe')
          : '',
    c.kind === 'unreadable' && props.editable ? t('escribe el valor; vacía no se escribe') : '',
    c.kind === 'sheet' && !c.fromFormula && canEditCell(row.__key, field) ? t('Valor actual de la hoja; escribe para cambiarlo') : '',
    readOnlyRow(change) ? readOnlyText(change) : '',
  ]
    .filter(Boolean)
    .join('\n')
  const text = show(field, c.value)
  // Edited in the sheet after the proposal: a "sheet" tag, then the cell as it is written.
  if (c.sheetEdit) return sheetTagged(field, c, text)
  // A preserved butterfly would be left without it: an amber "missing" tag, then what the cell holds.
  if (c.warning && c.kind !== 'unreadable') {
    const box = document.createElement('span')
    const mark = document.createElement('span')
    mark.className = 'warn-mark'
    mark.textContent = t('falta')
    box.append(mark)
    if (text) box.append(' ', withTotal(field, c.value, text))
    return box
  }
  // Nobody could read it: its tag, then the sheet's value if the row has one (it stays).
  if (c.kind === 'unreadable') {
    const box = document.createElement('span')
    const mark = document.createElement('span')
    mark.className = 'unread-mark'
    mark.textContent = t('ilegible')
    box.append(mark)
    if (text) box.append(' ', withTotal(field, c.value, text))
    return box
  }
  // A formula the proposal writes: an "ƒx" tag, then what it will give (its text in the tooltip and the cell bar).
  if (c.formulaWrite) {
    const box = document.createElement('span')
    const mark = document.createElement('span')
    mark.className = 'fx-mark'
    mark.textContent = 'ƒx'
    box.append(mark, ' ', c.computed !== undefined ? show(field, c.computed) || t('vacío') : text)
    return box
  }
  // What the formula will give (grey, tagged): then the sheet's older value, struck through, if it had one.
  if (c.fromFormula) {
    const box = document.createElement('span')
    const mark = document.createElement('span')
    mark.className = 'formula-mark'
    mark.textContent = t('fórmula')
    box.append(withTotal(field, c.value, text), ' ', mark)
    if (c.was !== undefined && c.was !== null && c.was !== '' && show(field, c.was) !== text) {
      const old = document.createElement('s')
      old.className = 'was'
      old.textContent = show(field, c.was)
      box.append(' ', old)
    }
    // The assistant's species set aside for the formula's: struck through, not written.
    if (c.kind === 'reverted') {
      const aside = document.createElement('s')
      aside.className = 'aside'
      aside.append(withTotal(field, c.ai, ai))
      box.append(' ', aside)
    }
    return box
  }
  // Set back to the sheet: its value, then the assistant's struck through (kept aside, not written).
  if (c.kind === 'reverted') {
    const box = document.createElement('span')
    if (text) box.append(withTotal(field, c.value, text), ' ')
    const aside = document.createElement('s')
    aside.className = 'aside'
    aside.append(withTotal(field, c.ai, ai))
    box.append(aside)
    return box
  }
  if (!changed || change.create || c.was === undefined || show(field, c.was) === text) return marked(c, withTotal(field, c.value, text))
  const box = document.createElement('span')
  // Emptied on purpose ({ clear: true } or the person deleted it): red, not a quiet "vacío".
  if (text) box.append(withTotal(field, c.value, text))
  else box.textContent = t('vaciar')
  box.classList.toggle('is-clear', !text)
  const old = document.createElement('s')
  old.className = 'was'
  old.append(withTotal(field, c.was, show(field, c.was) || t('vacío')))
  box.append(' ', old)
  return marked(c, box)
}

/**
 * A cell edited in the sheet since the proposal read it: its "hoja" tag, then the
 * sheet's value with the proposal's struck through (kept), or the proposal's
 * with the sheet's struck through (written over it).
 */
function sheetTagged(field: string, c: CellInfo, text: string): Node {
  const box = document.createElement('span')
  const mark = document.createElement('span')
  mark.className = 'sheet-mark'
  mark.textContent = t('hoja')
  box.append(mark, ' ')
  if (c.kind === 'kept') {
    if (text) box.append(withTotal(field, c.value, text), ' ')
    const aside = document.createElement('s')
    aside.className = 'aside'
    aside.append(withTotal(field, c.proposalValue, show(field, c.proposalValue) || t('vacío')))
    box.append(aside)
    return box
  }
  if (text) box.append(withTotal(field, c.value, text))
  else box.append(t('vaciar'))
  const old = document.createElement('s')
  old.className = 'was'
  old.append(withTotal(field, c.was, show(field, c.was) || t('vacío')))
  box.append(' ', old)
  return box
}

/** A doubtful cell's content after its "?" mark (the cell's dashed amber edge is its class). */
function marked(c: CellInfo, content: Node): Node {
  if (!c.doubtful) return content
  const box = document.createElement('span')
  const mark = document.createElement('span')
  mark.className = 'doubt-mark'
  mark.textContent = '?'
  box.append(mark, content)
  return box
}
/** The row number; a row with nothing left to write (every cell back to the sheet's value) is struck through. */
function rowFormatter(cell: CellComponent) {
  const row = cell.getData() as Row
  if (row.__marker) return markerCell(cell)
  const change = byKey.get(row.__key)
  // (A row whose only cells are unreadable ones still to fill is not: it waits for them.)
  const waiting = !!change && Object.keys(change.unreadable ?? {}).some(f => !(f in change.values))
  // (A page line that writes nothing is grey already: it was never to be written.)
  const skipped = props.editable && !!change && !change.context && !writtenFields(change).length && !waiting
  const el = cell.getElement()
  el.classList.toggle('is-skipped', skipped)
  // A page line that comes before the previous line of its photo in the sheet: marked, with which line.
  const after = props.notebookOrder ? undefined : change?.outOfOrder
  el.classList.toggle('is-out-of-order', !!after)
  const why = after
    ? t('En el cuaderno va después de la línea {line} ({id}), pero en la hoja va antes', { line: after.line, id: after.id })
    : ''
  el.title = [
    change?.gap
      ? readOnlyText(change)
      : !skipped
        ? ''
        : change?.rowTaken
          ? t('Esta fila no se escribe: su fila sin usar ya se usó en la hoja')
          : t('Esta fila no se escribe: no le queda ningún cambio'),
    why,
  ]
    .filter(Boolean)
    .join(' · ')
  return after ? `↕ ${row.__row}` : row.__row
}

/** A cell as drawn, as text: its value, then the sheet's value struck through or the assistant's set aside. */
function drawnText(change: ProposalChange, field: string) {
  const cell = cellOf(change, field, props.newRowFormulas)
  const beside =
    cell.kind === 'reverted'
      ? ` ${textWithTotal(field, cell.ai)}`
      : cell.was !== undefined && cell.kind !== 'sheet'
        ? ` ${textWithTotal(field, cell.was)}`
        : ''
  // The "?" of a doubtful cell takes about two letters; an unreadable cell's tag about its word.
  if (cell.kind === 'kept') return `${t('hoja')}   ${textWithTotal(field, cell.value)} ${textWithTotal(field, cell.proposalValue)}`
  if (cell.sheetEdit) return `${t('hoja')}   ${textWithTotal(field, cell.value)} ${textWithTotal(field, cell.was)}`
  if (cell.kind === 'unreadable') return `${t('ilegible')}   ${textWithTotal(field, cell.value)}`
  if (cell.warning) return `${t('falta')}   ${textWithTotal(field, cell.value)}`
  if (cell.formulaWrite) return `ƒx   ${cell.computed !== undefined ? textWithTotal(field, cell.computed) : textWithTotal(field, cell.value)}`
  if (cell.fromFormula)
    return `${textWithTotal(field, cell.value)}  ${t('fórmula')}  ${cell.was ? textWithTotal(field, cell.was) : ''}${cell.kind === 'reverted' ? ` ${textWithTotal(field, cell.ai)}` : ''}`
  return (cell.doubtful ? '?  ' : '') + textWithTotal(field, cell.value) + beside
}

/**
 * The rows a column's width is measured on: up to 200 of those that write
 * something and up to 100 of the others (page lines, the sheet's rows in
 * between), each spread over the table, so hundreds of rows by some thirty
 * columns are still sized at once.
 */
function measured() {
  if (props.changes.length <= 200) return props.changes
  return [...spread(props.changes.filter(c => !c.context), 200), ...spread(props.changes.filter(c => c.context), 100)]
}
function widthOf(field: string, rows = measured()) {
  let chars = field.length + 2
  for (const c of rows) chars = Math.max(chars, drawnText(c, field).length)
  return Math.max(70, Math.min(260, Math.round(chars * 7.2 + 28)))
}

function columns(): ColumnDefinition[] {
  const cols: ColumnDefinition[] = []
  // The row's identity stays at the left while scrolling right, with its row number on a computer.
  // Frozen columns go first (Tabulator keeps them in line only at the edge): on a phone, the ID alone.
  const id = {
    title: 'ID',
    field: '__label',
    frozen: true,
    headerSort: false,
    cssClass: 'proposal-label',
    formatter: labelFormatter as never,
    // Room for a repeat's chip («repeat of A0E (row 13263)»).
    ...(repeatRows() ? { width: Math.min(240, 7.2 * Math.max(4, ...props.changes.map(c => c.label.length)) + 150) } : {}),
  } as ColumnDefinition
  if (!wide) cols.push(id)
  if (props.applied)
    cols.push({
      title: '✓',
      field: '__done',
      width: 34,
      frozen: wide,
      hozAlign: 'center',
      headerHozAlign: 'center',
      headerTooltip: t('Filas escritas en la hoja'),
    })
  cols.push({
    title: t('Fila'),
    field: '__row',
    width: markedRows() ? 70 : 54,
    frozen: wide,
    hozAlign: 'right',
    cssClass: 'row-number',
    headerSort: false,
    formatter: rowFormatter as never,
  })
  // The notebook line, to follow the page row by row.
  if (paged())
    cols.push({
      title: t('Línea'),
      field: '__line',
      width: offPhotoRows() ? 92 : severalPhotos() ? 58 : 50,
      frozen: wide,
      formatter: lineFormatter as never,
      hozAlign: 'right',
      cssClass: 'row-number',
      headerSort: false,
      headerTooltip: severalPhotos() ? t('Foto · línea del cuaderno') : t('Línea del cuaderno'),
    })
  if (wide) cols.push(id)
  // The assistant's note on the row (where its values come from, what a line says), beside its ID:
  // cut short, whole under the pointer and in the bar when selected.
  cols.push({
    title: t('Nota IA'),
    field: '__note',
    headerSort: false,
    width: 150,
    minWidth: 70,
    cssClass: 'proposal-note',
    headerTooltip: t('Nota de la IA sobre la fila: de dónde salen sus valores'),
    formatter: (cell: CellComponent) => {
      if ((cell.getData() as Row).__marker) return ''
      const note = String(cell.getValue() ?? '')
      cell.getElement().title = note
      return note
    },
  } as ColumnDefinition)
  const rows = measured()
  for (const field of props.fields) {
    const choices = hasChoices(field)
    cols.push({
      title: field,
      field,
      width: widthOf(field, rows),
      minWidth: 70,
      headerSort: false,
      formatter: formatter(field) as never,
      editable: (cell: CellComponent) => canEditCell((cell.getData() as Row).__key, field),
      ...(choices
        ? choiceEditor(() => [...(choicesOf(field) ?? [])])
        : // Dates open day first (26/05/2026), not as the sheet's serial number.
          textEditor(value => editText(value as CellValue, { key: field, type: typeOf(field) }))),
    } as ColumnDefinition)
  }
  if (props.editable)
    cols.push({
      title: '',
      field: '__remove',
      width: 34,
      hozAlign: 'center',
      headerSort: false,
      cssClass: 'row-remove',
      // A page line with no row of its own has nothing to take out.
      formatter: (cell: CellComponent) => (removable((cell.getData() as Row).__key) ? '✕' : ''),
      tooltip: (_e: MouseEvent, cell: CellComponent) =>
        removable((cell.getData() as Row).__key) ? t('Quitar esta fila de la propuesta') : '',
      cellClick: (_e: UIEvent, cell: CellComponent) => {
        const key = (cell.getData() as Row).__key
        if (removable(key)) emit('remove', key)
      },
    } as ColumnDefinition)
  return cols
}
const removable = (key: string) => {
  const change = byKey.get(key)
  return !!change && !pageOnly(change)
}
/** A page line's row: grey when it writes nothing, red when the save refused it. */
function rowLook(row: RowComponent) {
  const data = row.getData() as Row
  const change = byKey.get(data.__key)
  const el = row.getElement()
  el.classList.toggle('is-marker-row', !!data.__marker)
  const m = data.__marker ? (JSON.parse(data.__state) as MarkerState) : null
  el.classList.toggle('is-peekable', !!m && peekable(m))
  el.classList.toggle('is-context-row', !!change?.context && !change.page?.error)
  el.classList.toggle('is-placeholder-row', !!change?.placeholder)
  el.classList.toggle('is-error-row', !!change?.page?.error)
  el.classList.toggle('is-taken-row', !!change?.rowTaken)
}

// ------------------------------------------------------------ a slim row opened
/** The slim rows opened (by markerKey): their sheet rows, read from the server. */
const peeks = shallowRef(new Map<string, Peek>())
/** The last ask of each slim row: a fold or a newer click makes an older answer late. */
const peekAsks = new Map<string, number>()
let peekAsked = 0
function setPeek(key: string, peek: Peek | null) {
  const next = new Map(peeks.value)
  if (peek) next.set(key, peek)
  else next.delete(key)
  peeks.value = next
}
/** A click on a slim row: its sheet rows open under it (read now), or fold if open. */
async function togglePeek(m: Marker) {
  const range = hiddenRange(m)
  if (!range || !props.loadRows) return
  const key = markerKey(m)
  const ask = ++peekAsked
  peekAsks.set(key, ask)
  const now = peeks.value.get(key)
  if (now && now.state !== 'error') return setPeek(key, null)
  setPeek(key, { state: 'loading' })
  try {
    const rows = await props.loadRows(range.from, range.to)
    if (peekAsks.get(key) === ask) setPeek(key, { state: 'open', rows })
  } catch (e) {
    if (peekAsks.get(key) === ask) setPeek(key, { state: 'error', message: errorText(e) })
  }
}

// ------------------------------------------------------------ the cell bar
/** The selected cell as the bar above the table shows it, with the sheet's and the assistant's values under it. */
const bar = ref<CellBarInfo | null>(null)
const editText$ = (field: string, value: CellValue | undefined) => editText(value ?? null, { key: field, type: typeOf(field) })
function describe(cell: CellComponent | null): CellBarInfo | null {
  if (!cell) return null
  const row = cell.getData() as Row
  const field = cell.getField()
  const base = { index: row.__key, field, row: row.__label, editable: false, multiline: false }
  // The row's own columns: its note (where the values come from), its ID and row number.
  if (field === '__note') return { ...base, column: t('Nota IA'), text: row.__note }
  if (field === '__label') return { ...base, column: 'ID', text: row.__label }
  if (field === '__row') return { ...base, column: t('Fila'), text: row.__row }
  if (field === '__line') return { ...base, column: t('Línea'), text: row.__line }
  const c = fieldSet.value.has(field) ? info(row.__key, field) : null
  const change = byKey.get(row.__key)
  if (!c || !change) return null
  const notes: CellBarNote[] = []
  const total = totalOf(field, c.value)
  if (total !== null) notes.push({ text: `= ${total}`, kind: 'total' })
  const editable = canEditCell(row.__key, field)
  // What the assistant says about it (as the cell's tooltip): a doubt, where it comes from, why unreadable.
  for (const n of cellComments(c, v => show(field, v)))
    notes.push(
      n.kind === 'unreadable' && editable ? { ...n, text: `${n.text} · ${t('escribe el valor; vacía no se escribe')}` } : n,
    )
  // What the sheet has now (an existing row), and what the assistant proposed when the person changed it.
  if (!change.create && (c.kind === 'proposed' || c.kind === 'person'))
    notes.push({ label: t('Hoja'), text: editText$(field, c.was) || t('vacío'), kind: 'sheet' })
  if (c.aiProposed && (c.kind === 'person' || c.kind === 'reverted'))
    notes.push({ label: t('IA'), text: editText$(field, c.ai) || t('vacío'), kind: 'ai' })
  // Edited in the sheet: the proposal's value, set aside while the sheet's stays.
  if (c.kind === 'kept') notes.push({ label: t('Propuesta'), text: editText$(field, c.proposalValue) || t('vacío'), kind: 'ai' })
  if (c.fromFormula)
    notes.push({ label: t('Fórmula'), text: t('Calculado por la fórmula de la hoja con los valores propuestos: no se escribe'), kind: 'hint' })
  else if (c.formulaFallback)
    notes.push({ label: t('Fórmula'), text: t('No se pudo calcular aquí: es el valor actual de la hoja, que la fórmula puede cambiar al aplicar'), kind: 'hint' })
  // What of an unreadable cell was read (as written): a click puts it in the bar to complete.
  const partial = c.kind === 'unreadable' ? (c.unreadable?.partial ?? []) : []
  // The doubt's other readings (and the assistant's own value, once the person changed it).
  const readings = c.doubt
    ? [...(c.doubt.alternatives ?? []), ...(c.kind === 'person' && c.aiProposed ? [c.ai] : [])].filter(
        (a, i, all) => a !== undefined && JSON.stringify(a) !== JSON.stringify(c.value) && all.findIndex(b => JSON.stringify(b) === JSON.stringify(a)) === i,
      )
    : []
  return {
    ...base,
    column: field,
    text: editText$(field, c.value),
    editable,
    multiline: longText(field) && !hasChoices(field),
    readonly: editable
      ? ''
      : c.kind === 'locked'
        ? t('Calculado por la fórmula de la hoja: no se escribe')
        : readOnlyRow(change)
          ? readOnlyText(change)
          : '',
    notes,
    choices: partial.length
      ? partial.map(text => ({ label: text, text }))
      : readings.map(a => ({ label: show(field, a as CellValue) || t('vacío'), text: editText$(field, a as CellValue) })),
    ...(partial.length ? { choicesLabel: t('Leído en parte'), choicesComplete: true } : {}),
  }
}
let selectedKey = ''
function showBar() {
  const cell = table ? selectedCell(table) : null
  bar.value = describe(cell)
  const key = cell ? String((cell.getData() as Row).__key ?? '') : ''
  if (key && key !== selectedKey) emit('select', key)
  selectedKey = key
}
/** The bar and the doubtful cell's choices follow the selection (once per frame). */
let follow: (() => void) | null = null
function saveFromBar(target: CellBarInfo, text: string, move: Direction | 'here' | null) {
  if (!table) return
  if (!setFromBar(table, target, text, canEdit)) emit('notice', t('Esa celda ya no se puede editar'))
  if (move) backToGrid(table, move)
  showBar()
}
/** Another reading picked in the bar: written as if typed (the person's value from then on, so reviewed). */
const pickFromBar = (target: CellBarInfo, text: string) => saveFromBar(target, text, 'here')

// ------------------------------------------------------------ edits
let normalizing = false
let outgoing: CellEdit[] = []
/** The cells the edits going out together change, as they were before: one undo step. */
const step = new StepBuilder()
function send() {
  if (!outgoing.length) return
  const cells = outgoing
  outgoing = []
  record(step.take())
  emit('edit', cells)
}
function onCellEdited(cell: CellComponent) {
  if (normalizing) return
  const field = cell.getField()
  if (!fieldSet.value.has(field)) return
  const row = cell.getData() as Row
  const before = (cell.getOldValue() ?? null) as CellValue
  const result = normalizeInput(cell.getValue(), { key: field, type: typeOf(field) }, props.sheet)
  if (!result.ok) {
    emit('notice', result.message)
    normalizing = true
    cell.restoreOldValue()
    normalizing = false
    return
  }
  if (result.value !== cell.getValue()) {
    normalizing = true
    cell.setValue(result.value)
    normalizing = false
  }
  if (JSON.stringify(before) === JSON.stringify(result.value)) return
  const list = rules.value?.lists[field]
  const problem = list && listProblem(rules.value, field, result.value)
  if (problem) emit('notice', list.strict ? problem : t('{problem}: se guarda igual; corrígelo si es un error', { problem }))
  // `before` tells the server what change the person typed over (to say when the assistant changed it
  // meanwhile): the sheet's value, or what the formula will give, is no change of anyone's.
  const was = info(row.__key, field)
  const over = was?.kind === 'proposed' || was?.kind === 'person' ? before : null
  const change = latest.value.get(row.__key)
  if (change) step.add(change, field)
  outgoing.push({ key: row.__key, field, value: result.value, before: over })
  // A paste or a fill sets many cells at once: they go out together.
  if (outgoing.length === 1) queueMicrotask(send)
}

// ------------------------------------------------------------ the sheet's value or the assistant's, for the selection
/** What the buttons can do with the selected cells (counts, for their labels). */
const actions = ref({ sheet: 0, ai: 0, check: 0 })
function selected() {
  const out: { key: string; field: string; cell: NonNullable<ReturnType<typeof info>> }[] = []
  for (const range of table?.getRanges() ?? [])
    for (const cell of range.getCells().flat() as CellComponent[]) {
      const field = cell.getField()
      const key = (cell.getData() as Row).__key
      const c = fieldSet.value.has(field) && canEditCell(key, field) ? info(key, field) : null
      if (c) out.push({ key, field, cell: c })
    }
  return out
}
function updateActions() {
  const next = props.editable && table ? selectionActions(selected().map(s => s.cell)) : { sheet: 0, ai: 0, check: 0 }
  if (next.sheet !== actions.value.sheet || next.ai !== actions.value.ai || next.check !== actions.value.check) actions.value = next
}
/**
 * "Valor de la hoja": the selected cells go back to what the sheet has (a new
 * row's to empty); the assistant's value is kept aside, marked, and not
 * written. "Valor de la IA": they take the assistant's value again.
 */
function use(which: 'sheet' | 'ai') {
  const cells: CellEdit[] = []
  for (const { key, field, cell } of selected()) {
    if (which === 'sheet' && (cell.kind === 'proposed' || cell.kind === 'person'))
      cells.push({ key, field, value: null, before: cell.value, use: 'sheet' })
    if (which === 'ai' && cell.aiProposed && (cell.kind === 'reverted' || cell.kind === 'person'))
      cells.push({ key, field, value: cell.ai ?? null, before: cell.value, use: 'ai' })
  }
  if (!cells.length) return
  recordCells(cells)
  emit('edit', cells)
}
/** «Marcar revisadas»: the selected doubtful cells were looked at and stay as they are. */
function markChecked() {
  const cells = selected()
    .filter(s => s.cell.doubtful)
    .map(({ key, field }) => ({ key, field }))
  if (!cells.length) return
  recordCells(cells)
  emit('check', cells)
}

// ------------------------------------------------------------ undo, redo
/** The rows as the table has them now (the person's edits not saved yet included). */
const latest = computed(() => new Map(props.changes.map(c => [rowKey(c), c])))
const history = props.historyKey ? historyFor(props.historyKey) : new UndoHistory()
/** How many steps there are to undo and to redo (the buttons). */
const steps = ref({ undo: history.done.length, redo: history.undone.length })
const countSteps = () => (steps.value = { undo: history.done.length, redo: history.undone.length })
function record(cells: Step) {
  if (!cells.length) return
  history.record(cells)
  countSteps()
}
/** One step: the cells as they are before the person's edit. */
function recordCells(cells: { key: string; field: string }[]) {
  const builder = new StepBuilder()
  for (const { key, field } of cells) {
    const change = latest.value.get(key)
    if (change) builder.add(change, field)
  }
  record(builder.take())
}
/**
 * Ctrl+Z (⌘Z) undoes the last step; Ctrl+Shift+Z or Ctrl+Y redoes it: the cells
 * go back through the same edits as typing (saved to the proposal). Cells the
 * assistant changed since stay as it left them, and the person is told.
 */
function undoRedo(which: 'undo' | 'redo') {
  if (!props.editable || !table) return
  const out = which === 'undo' ? history.undo(key => latest.value.get(key)) : history.redo(key => latest.value.get(key))
  countSteps()
  if (!out) return
  if (out.edits.length) emit('edit', out.edits)
  if (out.checks.length) emit('check', out.checks)
  if (out.sheets.length) emit('sheet', out.sheets)
  const done = out.inverse.length
  if (out.revised)
    emit(
      'notice',
      which === 'undo'
        ? done
          ? tn(out.revised, 'Deshecho, salvo {n} celda que la IA cambió después: queda como la dejó', 'Deshecho, salvo {n} celdas que la IA cambió después: quedan como las dejó')
          : tn(out.revised, 'No se deshizo: la IA cambió esa celda después y queda como la dejó', 'No se deshizo: la IA cambió esas {n} celdas después y quedan como las dejó')
        : done
          ? tn(out.revised, 'Rehecho, salvo {n} celda que la IA cambió después: queda como la dejó', 'Rehecho, salvo {n} celdas que la IA cambió después: quedan como las dejó')
          : tn(out.revised, 'No se rehízo: la IA cambió esa celda después y queda como la dejó', 'No se rehízo: la IA cambió esas {n} celdas después y quedan como las dejó'),
    )
  if (out.gone)
    emit('notice', tn(out.gone, '{n} celda quedó como está: su fila ya no está en la propuesta', '{n} celdas quedaron como están: su fila ya no está en la propuesta'))
  selectCells(out.inverse)
}
/** The cells an undo put back, selected (the block around them) and brought into view. */
function selectCells(cells: { key: string; field: string }[]) {
  if (!table || !cells.length) return
  const rows = table.getRows('active')
  const columns = table.getColumns().filter(c => c.isVisible())
  const at = cells
    .map(c => {
      const row = table!.getRow(c.key)
      return { r: row ? rows.indexOf(row) : -1, c: columns.findIndex(col => col.getField() === c.field) }
    })
    .filter(p => p.r >= 0 && p.c >= 0)
  if (!at.length) return
  const top = Math.min(...at.map(p => p.r))
  const bottom = Math.max(...at.map(p => p.r))
  const left = Math.min(...at.map(p => p.c))
  const right = Math.max(...at.map(p => p.c))
  const from = rows[top].getCell(columns[left].getField())
  const to = rows[bottom].getCell(columns[right].getField())
  if (!from || !to) return
  try {
    ;(table as unknown as { addRange: (a: CellComponent, b: CellComponent) => void }).addRange(from, to)
  } catch {
    /* Range selection is off on touch screens. */
  }
  from.getElement().scrollIntoView({ block: 'nearest', inline: 'nearest' })
}
/** Ctrl+Z / ⌘Z, Ctrl+Shift+Z / ⌘⇧Z and Ctrl+Y on the grid (in a cell being edited, the box's own undo). */
function onUndoKey(event: KeyboardEvent) {
  if (!(event.ctrlKey || event.metaKey) || event.altKey || !props.editable) return
  const key = event.key.toLowerCase()
  const which = key === 'z' ? (event.shiftKey ? 'redo' : 'undo') : key === 'y' && !event.shiftKey ? 'redo' : null
  if (!which || (event.target as HTMLElement).closest('input, textarea, select, .tabulator-editing')) return
  event.preventDefault()
  undoRedo(which)
}

// ------------------------------------------------------------ a doubtful cell, checked where it is
/**
 * The selected doubtful cell's choices, beside it: «Correcta» keeps the
 * assistant's reading (checked), another reading writes it (the person's value,
 * so checked too); either goes on to the next doubtful cell (`next`), as does
 * Ctrl+Enter (⌘+Enter) on the grid. Big enough for a finger on a phone.
 */
const quick = ref<{
  key: string
  field: string
  left: number
  top: number
  above: boolean
  choices: { label: string; text: string }[]
  /** A cell edited in the sheet: the sheet's value or the proposal's (`use`: the one it has now). */
  sheet?: { use: 'sheet' | 'proposal'; now: string; proposal: string; who: string }
} | null>(null)
const quickBox = ref<HTMLDivElement>()
const mac = typeof navigator !== 'undefined' && /Mac|iPhone|iPad/.test(navigator.platform)
const reviewKey = mac ? '⌘ Enter' : 'Ctrl+Enter'
/** The undo and redo keys, for the buttons' tooltips. */
const undoKey = mac ? '⌘Z' : 'Ctrl+Z'
const redoKey = mac ? '⌘⇧Z' : 'Ctrl+Y'
/** The cell edited in the sheet just chosen for: its choices stay away until another cell is selected. */
let chosenHere: string | null = null
function placeQuick() {
  quick.value = null
  if (!table || !props.editable || busy()) return
  const cell = activeCell(table)
  if (!cell) return
  const key = (cell.getData() as Row).__key
  const field = cell.getField()
  if (chosenHere === cellId(key, field)) return
  chosenHere = null
  const c = fieldSet.value.has(field) ? info(key, field) : null
  if (!(c?.doubtful || c?.sheetEdit) || !canEditCell(key, field)) return
  const el = cell.getElement()
  const container = host.value?.parentElement
  const view = host.value?.querySelector('.tabulator-tableholder')?.getBoundingClientRect()
  if (!container || !view || !el.isConnected) return
  const r = el.getBoundingClientRect()
  // Scrolled out of the grid's view: nothing to point at.
  if (r.bottom < view.top || r.top > view.bottom || r.right < view.left || r.left > view.right) return
  const origin = container.getBoundingClientRect()
  // Under the cell, or over it near the grid's bottom edge.
  const above = r.bottom + 44 > view.bottom && r.top - 44 > view.top
  quick.value = {
    key,
    field,
    left: Math.max(0, r.left - origin.left),
    // (Clear of the round handle a finger drags, on a touch screen.)
    top: above ? r.top - origin.top - 4 : r.bottom - origin.top + (touch ? 16 : 5),
    above,
    choices: c.sheetEdit
      ? []
      : (c.doubt?.alternatives ?? [])
          .filter(
            (a, i, all) =>
              JSON.stringify(a) !== JSON.stringify(c.value) && all.findIndex(b => JSON.stringify(b) === JSON.stringify(a)) === i,
          )
          .map(a => ({ label: show(field, a) || t('vacío'), text: editText$(field, a) })),
    ...(c.sheetEdit
      ? {
          sheet: {
            use: keptOver(c.sheetEdit) ? 'proposal' : 'sheet',
            now: show(field, c.sheetEdit.now) || t('vacío'),
            proposal: show(field, c.kind === 'kept' ? c.proposalValue : c.value) || t('vacío'),
            who: [editedBy(c.sheetEdit), whenText(c.sheetEdit.at)].filter(Boolean).join(', '),
          },
        }
      : {}),
  }
  // Kept inside the grid's box at its right edge.
  nextTick(() => {
    const box = quickBox.value
    if (box && quick.value) quick.value.left = Math.max(0, Math.min(quick.value.left, container.clientWidth - box.offsetWidth - 4))
  })
}
/** The sheet's value kept, or the proposal's written over it; then on to the next cell edited in the sheet. */
function chooseQuick(key: string, field: string, use: 'sheet' | 'proposal') {
  chosenHere = cellId(key, field)
  quick.value = null
  recordCells([{ key, field }])
  emit('sheet', [{ key, field, use }])
  emit('next', { key, field }, 'sheet')
}
function confirmQuick(key: string, field: string) {
  recordCells([{ key, field }])
  emit('check', [{ key, field }])
  emit('next', { key, field })
}
function pickQuick(key: string, field: string, text: string) {
  if (!table || !setFromBar(table, { index: key, field }, text, canEdit)) return emit('notice', t('Esa celda ya no se puede editar'))
  emit('next', { key, field })
}
/** Ctrl+Enter (⌘+Enter) on the grid: the selected doubtful cell is correct, on to the next; elsewhere, to the next doubtful cell. */
function onReviewKey(event: KeyboardEvent) {
  if (!(event.ctrlKey || event.metaKey) || event.key !== 'Enter' || event.shiftKey || event.altKey || !table || !props.editable) return
  if ((event.target as HTMLElement).closest('input, textarea, select, .tabulator-editing')) return
  event.preventDefault()
  // Before the grid's own Enter (which opens the editor).
  event.stopImmediatePropagation()
  const cell = activeCell(table)
  const key = cell ? (cell.getData() as Row).__key : null
  const field = cell?.getField() ?? ''
  if (key && fieldSet.value.has(field) && info(key, field)?.doubtful && canEditCell(key, field)) confirmQuick(key, field)
  else emit('next', key ? { key, field } : null)
}

/** Selects a cell and brings it into view (the panel's "review" jumps to the first doubtful cell). */
function focusCell(key: string, field: string) {
  const row = table?.getRow(key)
  const cell = row?.getCell(field)
  if (!table || !row || !cell) return false
  try {
    ;(table as unknown as { addRange: (a: CellComponent, b: CellComponent) => void }).addRange(cell, cell)
  } catch {
    /* Range selection is off on touch screens. */
  }
  cell.getElement().scrollIntoView({ block: 'nearest', inline: 'nearest' })
  // Keys go where Tabulator listens for them (its rows), as after a click.
  ;(table as unknown as { rowManager: { element: HTMLElement } }).rowManager.element.focus({ preventScroll: true })
  showBar()
  return true
}
defineExpose({ focusCell })

/** The cut being pasted (its cells are emptied once the paste is written). */
let moving: PendingCut | null = null
/**
 * The copied block as rows of { field: text }, from the first selected column on (as SheetGrid).
 * The cells cut in this table go as they were cut (not repeated over a bigger selection).
 */
function pasteParser(text: string) {
  const range = table?.getRanges()[0]
  moving = null
  if (!table || !range) return false
  const block = parseBlock(text) ?? [[text.replace(/\r?\n$/, '')]]
  moving = cuts?.take(text) ?? null
  const tiled = moving ? block : tileToSelection(block, range.getRows().length, range.getColumns().length)
  const visible = table.getColumns().filter(c => c.isVisible())
  const first = range.getColumns()[0]?.getField()
  const start = visible.findIndex(c => c.getField() === first)
  if (start < 0) return false
  const fields = visible.slice(start, start + (tiled[0]?.length ?? 0)).map(c => c.getField())
  return tiled.map(line => Object.fromEntries(fields.map((f, j) => [f, line[j]])))
}
function pasteRange(rowsData: Record<string, unknown>[]) {
  const move = moving
  moving = null
  if (!table || !rowsData.length) return []
  const selected = table.getRanges()[0]?.getRows() || []
  if (!selected.length) return []
  // (The slim rows between the rows take no line of the block.)
  const active = table.getRows('active').filter(r => r === selected[0] || !(r.getData() as Row).__marker)
  const start = active.indexOf(selected[0])
  if (start < 0) return []
  let skipped = 0
  const touched: RowComponent[] = []
  /** The cells the paste covers: a cut's cells among them are not emptied after. */
  const written = new Set<string>()
  for (const [offset, row] of active.slice(start, start + rowsData.length).entries()) {
    for (const [field, raw] of Object.entries(rowsData[offset])) {
      if (!fieldSet.value.has(field)) continue
      written.add(cutCellId(row.getIndex() as string, field))
      if (!canEdit(row, field)) {
        skipped++
        continue
      }
      row.getCell(field).setValue(raw === undefined ? null : String(raw))
    }
    touched.push(row)
  }
  // A cut pasted: the cells it came from are emptied now, with the paste (one step to undo).
  if (move && cuts) {
    cuts.finish(move, written)
    copied?.clear()
  }
  if (skipped) emit('notice', t('{n} celdas de solo lectura no se modificaron', { n: skipped }))
  return touched
}

// ------------------------------------------------------------ drawing
let shown = new Map<string, string>()
let shownOrder = ''
let shownColumns = ''
let stale = false
let retry: number | undefined
/** A cell is being edited, or keys typed wait for its editor: redrawing now would throw away what is typed. */
const busy = () => !!host.value?.querySelector('.tabulator-editing') || typingPending()
const layoutKey = () =>
  // The language is part of it: the column titles and tooltips are in it.
  [
    props.fields.join('|'),
    props.editable,
    props.applied ? 1 : 0,
    rules.value ? 1 : 0,
    locale.value,
    paged() ? (severalPhotos() ? 'pp' : 'p') : '',
    offPhotoRows() ? 'o' : '',
    markedRows() ? 'm' : '',
    repeatRows() ? `r${Math.max(0, ...props.changes.map(c => c.label.length))}` : '',
  ].join('\n')
function sync() {
  if (!table || !built) return
  if (busy()) {
    stale = true
    window.clearTimeout(retry)
    retry = window.setTimeout(sync, 250)
    return
  }
  stale = false
  // The slim rows opened, with their sheet rows under them.
  const laid = withPeeks(props.laid ?? props.changes.map(change => ({ change })), peeks.value, props.sheet)
  byKey = new Map(laid.flatMap(item => (item.change ? [[rowKey(item.change), item.change] as const] : [])))
  const rows = laid.map((item, i) => {
    if (item.change) return toRow(item.change)
    const next = laid.slice(i + 1).find(x => x.change)?.change
    return markerRow(item.marker, next ? rowKey(next) : 'end', item.peek)
  })
  const layout = layoutKey()
  if (layout !== shownColumns) {
    shownColumns = layout
    table.setColumns(columns())
    shownOrder = ''
  }
  const order = rows.map(r => r.__key).join('|')
  if (order !== shownOrder) table.replaceData(rows)
  else {
    const changed = rows.filter(r => shown.get(r.__key) !== JSON.stringify(r))
    if (changed.length)
      table.updateData(changed).then(() => {
        for (const r of changed) table?.getRow(r.__key)?.reformat()
      })
  }
  shownOrder = order
  shown = new Map(rows.map(r => [r.__key, JSON.stringify(r)]))
  updateActions()
  // A cell checked or edited meanwhile: its choices go (or move with the rows).
  follow?.()
}

/** A cell as copied: its value as shown (a formula's too), dates as 2026-10-04 (lib/clipboard). */
function copyCell(cell: CellComponent) {
  const field = cell.getField()
  return fieldSet.value.has(field) ? copyText(cell.getValue(), { key: field, type: typeOf(field) }) : String(cell.getValue() ?? '')
}
/** A formula cell of the sheet (grey in the grid): the copy notice counts them. */
function formulaCell(cell: CellComponent) {
  const c = fieldSet.value.has(cell.getField()) ? info((cell.getData() as Row).__key, cell.getField()) : null
  return !!c && (c.kind === 'locked' || !!c.fromFormula || !!c.formulaFallback)
}

const onKeydown = spreadsheetKeys(
  () => table,
  canEdit,
  message => emit('notice', message),
  undefined,
  // Ctrl+X marks the cells; the paste moves them.
  { cut: () => cuts?.start() },
)
const onEditingKey = editingKeys(() => table)
let fill: { destroy: () => void } | null = null
let copied: ReturnType<typeof attachCopyMarker> | null = null
let cuts: ReturnType<typeof attachPendingCut> | null = null
let sizeWatch: { disconnect: () => void } | null = null
let fit: { destroy: () => void } | null = null

onMounted(() => {
  if (!host.value) return
  byKey = new Map(props.changes.map(c => [rowKey(c), c]))
  shownColumns = layoutKey()
  table = new Tabulator(host.value, {
    data: [],
    index: '__key',
    columns: columns(),
    layout: 'fitData',
    // At most the height of the panel it is in (less its title and buttons), so the column names stay
    // in sight while scrolling the rows (the panel or chat around it is a size container: 100cqh).
    // (Less the cell bar above it.)
    maxHeight: 'max(10rem, calc(100cqh - 10rem))',
    autoResize: false,
    placeholder: t('Sin filas'),
    selectableRange: 1,
    selectableRangeColumns: true,
    selectableRangeRows: true,
    selectableRangeClearCells: false,
    editTriggerEvent: 'dblclick',
    clipboard: true,
    // Copied as plain text, without the struck-through old value (lib/clipboard).
    ...plainCopy(() => table, copyCell),
    clipboardPasteParser: pasteParser,
    clipboardPasteAction: pasteRange,
    // Widths change from the header's borders only (see gridKit): a finger on the rows scrolls.
    columnDefaults: { headerSort: false, resizable: 'header' },
    rowFormatter: rowLook,
  } as unknown as ConstructorParameters<typeof Tabulator>[1])
  table.on('tableBuilt', () => {
    built = true
    sync()
  })
  table.on('cellEdited', onCellEdited)
  table.on('cellEditCancelled', () => stale && sync())
  table.on('cellEdited', () => stale && setTimeout(sync))
  for (const event of ['rangeAdded', 'rangeChanged', 'rangeRemoved'] as const) table.on(event as 'dataChanged', updateActions)
  // The ▾ arrow opens the list at once.
  table.on('cellClick', (event: UIEvent, cell: CellComponent) => {
    const el = cell.getElement()
    if (!el.classList.contains('has-choices') || !(event instanceof MouseEvent)) return
    if (event.clientX >= el.getBoundingClientRect().right - 22) openList(cell)
  })
  const notice = (message: string) => emit('notice', message)
  const container = host.value.parentElement!
  fill = touch
    ? attachTouchSheet(table, container, { canEdit, notice })
    : attachFillHandle(table, container, {
        canEdit,
        onFilled: rows => notice(tn(rows, 'Copiado a {n} fila', 'Copiado a {n} filas')),
      })
  copied = attachCopyMarker(table, container, notice, formulaCell)
  cuts = attachPendingCut(table, container, canEdit)
  fit =attachColumnFit(table, host.value, {
    text: (data, field) => {
      const change = byKey.get(String(data.__key))
      return fieldSet.value.has(field) && change ? drawnText(change, field) : String(data[field] ?? '')
    },
  })
  follow = followSelection(table, () => {
    showBar()
    placeQuick()
  })
  for (const event of ['scrollVertical', 'scrollHorizontal', 'columnResized'] as const) table.on(event as 'renderComplete', follow)
  table.on('cellEditing', () => (quick.value = null))
  // A slim row that stands for sheet rows not shown: they open under it, or fold.
  table.on('cellClick', (_event: UIEvent, cell: CellComponent) => {
    const data = cell.getData() as Row
    if (data.__marker) void togglePeek(JSON.parse(data.__state) as Marker)
  })
  // The cell just chosen for, clicked again: its choices come back (to change the choice).
  table.on('cellClick', (_event: UIEvent, cell: CellComponent) => {
    if (chosenHere !== cellId((cell.getData() as Row).__key, cell.getField())) return
    chosenHere = null
    placeQuick()
  })
  table.on('cellEditCancelled', follow)
  host.value.addEventListener('keydown', onReviewKey, true)
  host.value.addEventListener('keydown', onUndoKey)
  host.value.addEventListener('keydown', onKeydown)
  host.value.addEventListener('keydown', onEditingKey, true)
  sizeWatch = watchSize(() => table, host.value)
  // Built while the panel was hidden (closed, or the tab in the background): drawn when it shows.
  let hidden = !host.value.offsetWidth
  shownWatch = new ResizeObserver(([entry]) => {
    const zero = !entry.contentRect.width
    if (hidden && !zero && table && built && !busy()) table.redraw(true)
    hidden = zero
  })
  shownWatch.observe(host.value)
})
let shownWatch: ResizeObserver | null = null
onBeforeUnmount(() => {
  window.clearTimeout(retry)
  fill?.destroy()
  copied?.destroy()
  cuts?.destroy()
  fit?.destroy()
  sizeWatch?.disconnect()
  shownWatch?.disconnect()
  host.value?.removeEventListener('keydown', onReviewKey, true)
  host.value?.removeEventListener('keydown', onUndoKey)
  host.value?.removeEventListener('keydown', onKeydown)
  host.value?.removeEventListener('keydown', onEditingKey, true)
  table?.destroy()
  table = null
})
watch(
  () => [
    props.changes,
    props.laid,
    peeks.value,
    props.fields,
    props.flash,
    props.editable,
    props.applied,
    rules.value,
    locale.value,
  ],
  sync,
)
</script>

<template>
  <div>
    <div v-if="$slots.default || editable" class="flex flex-wrap items-center gap-2 px-2 pt-1.5 text-[11px] text-stone-500">
      <slot />
      <template v-if="editable">
        <!-- Kept from taking the focus: the grid keeps its selection while the button is pressed. -->
        <span class="flex items-center gap-1">
          <button
            class="proposal-use"
            :disabled="!steps.undo"
            :title="$t('Deshacer tu último cambio en la tabla ({key})', { key: undoKey })"
            :aria-label="$t('Deshacer')"
            @mousedown.prevent
            @click="undoRedo('undo')"
          >
            <Undo2 :size="12" />
          </button>
          <button
            class="proposal-use"
            :disabled="!steps.redo"
            :title="$t('Rehacer lo deshecho ({key})', { key: redoKey })"
            :aria-label="$t('Rehacer')"
            @mousedown.prevent
            @click="undoRedo('redo')"
          >
            <Redo2 :size="12" />
          </button>
        </span>
        <button
          class="proposal-use"
          :disabled="!actions.sheet"
          :title="
            actions.sheet
              ? $t('Las celdas elegidas vuelven al valor de la hoja (en una fila nueva, vacías): la sugerencia de la IA queda marcada y no se aplica')
              : $t('Elige celdas cambiadas: arrastra sobre ellas o pulsa el nombre de una columna')
          "
          @mousedown.prevent
          @click="use('sheet')"
        >
          <FileSpreadsheet :size="12" /> {{ $t('Valor de la hoja') }}<template v-if="actions.sheet > 1"> ({{ actions.sheet }})</template>
        </button>
        <button
          class="proposal-use"
          :disabled="!actions.ai"
          :title="
            actions.ai
              ? $t('Las celdas elegidas vuelven a tomar el valor que propuso la IA')
              : $t('Elige celdas con una sugerencia de la IA que cambiaste o no se aplica')
          "
          @mousedown.prevent
          @click="use('ai')"
        >
          <Sparkles :size="12" /> {{ $t('Valor de la IA') }}<template v-if="actions.ai > 1"> ({{ actions.ai }})</template>
        </button>
        <button
          class="proposal-use"
          :disabled="!actions.check"
          :title="
            actions.check
              ? $t('Las celdas dudosas elegidas quedan como revisadas, con el valor que tienen')
              : $t('Elige celdas dudosas (bordes ámbar con «?»): sus otras lecturas salen junto a la celda')
          "
          @mousedown.prevent
          @click="markChecked"
        >
          <CheckCheck :size="12" /> {{ $t('Marcar revisadas') }}<template v-if="actions.check > 1"> ({{ actions.check }})</template>
        </button>
      </template>
      <slot name="end" />
    </div>
    <slot name="panel" />
    <div class="sheet-grid proposal-sheet">
      <CellBar :info="bar" notes-line @save="saveFromBar" @pick="pickFromBar" @back="move => table && backToGrid(table, move)" />
      <!-- The grid's own box: the fill handle and the copied cells' border are placed in it, below the bar. -->
      <div class="relative">
        <div ref="host" tabindex="-1" />
        <!-- The selected doubtful cell's choices, beside it (the grid keeps the selection and the keys). -->
        <div
          v-if="quick"
          ref="quickBox"
          class="doubt-quick"
          :class="{ 'is-above': quick.above, 'is-sheet': !!quick.sheet }"
          :style="{ left: `${quick.left}px`, top: `${quick.top}px` }"
          role="group"
          :aria-label="quick.sheet ? $t('Celda editada en la hoja') : $t('Revisar la celda dudosa')"
          @pointerdown.prevent
          @mousedown.prevent
        >
          <!-- Edited in the sheet after the proposal: keep the sheet's value, or write the proposal's over it. -->
          <template v-if="quick.sheet">
            <span class="sheet-quick-what">{{ $t('Editada en la hoja') }} · {{ quick.sheet.who }}</span>
            <button
              type="button"
              class="sheet-quick-choice"
              :class="{ 'is-chosen': quick.sheet.use === 'sheet' }"
              :aria-pressed="quick.sheet.use === 'sheet'"
              :title="$t('No se escribe esta celda: queda {value}, como en la hoja', { value: quick.sheet.now })"
              @click="chooseQuick(quick.key, quick.field, 'sheet')"
            >
              <Table2 :size="13" /> {{ $t('Mantener el de la hoja') }} <b>{{ quick.sheet.now }}</b>
            </button>
            <button
              type="button"
              class="sheet-quick-choice"
              :class="{ 'is-chosen': quick.sheet.use === 'proposal' }"
              :aria-pressed="quick.sheet.use === 'proposal'"
              :title="$t('Se escribe {value} encima de lo que tiene la hoja', { value: quick.sheet.proposal })"
              @click="chooseQuick(quick.key, quick.field, 'proposal')"
            >
              <Sparkles :size="13" /> {{ $t('Usar el de la propuesta') }} <b>{{ quick.sheet.proposal }}</b>
            </button>
          </template>
          <button
            v-if="!quick.sheet"
            type="button"
            class="doubt-quick-ok"
            :title="$t('La lectura de la IA es correcta: queda revisada y pasa a la siguiente dudosa ({key})', { key: reviewKey })"
            @click="confirmQuick(quick.key, quick.field)"
          >
            <Check :size="13" /> {{ $t('Correcta') }}
          </button>
          <button
            v-for="(choice, i) in quick.choices"
            :key="i"
            type="button"
            class="doubt-quick-choice"
            :title="$t('Escribir {value} en la celda y pasar a la siguiente dudosa', { value: choice.label })"
            @click="pickQuick(quick.key, quick.field, choice.text)"
          >
            {{ choice.label }}
          </button>
          <kbd v-if="!touch && !quick.sheet" class="doubt-quick-key">{{ reviewKey }}</kbd>
        </div>
      </div>
    </div>
  </div>
</template>

<style>
/* A notebook page's lines that write nothing: grey (as the sheet has them), or as written (no such row). */
.proposal-sheet .tabulator-row.is-context-row .tabulator-cell,
.legend.is-context {
  background: #fafaf9;
  color: #a8a29e;
}
.proposal-sheet .tabulator-row.is-placeholder-row .tabulator-cell {
  background: #fafaf9;
  color: #a8a29e;
  font-style: italic;
}
.proposal-sheet .tabulator-row.is-placeholder-row .tabulator-cell.proposal-note,
.proposal-sheet .tabulator-row.is-context-row .tabulator-cell.proposal-note {
  color: #78716c;
}
/* A line the save refused: red, with why in its note. */
.proposal-sheet .tabulator-row.is-error-row .tabulator-cell,
.legend.is-line-error {
  background: #fef2f2;
  color: #991b1b;
}
/* A formula cell (grey, as in every grid), and what it will give once applied (tagged): never written. */
.proposal-sheet .tabulator-cell.is-formula-gives,
.legend.is-formula-gives {
  color: #57534e;
  background: #f3f4f1;
  font-style: italic;
}
/* A formula the proposal writes: the formula tint with the proposal's green edge, and an "ƒx" tag. */
.proposal-sheet .tabulator-cell.is-formula-write {
  box-shadow: inset 3px 0 0 #15803d;
}
.proposal-sheet .tabulator-cell .fx-mark,
.legend.is-formula-write::before {
  display: inline-block;
  padding: 0 3px;
  border: 1px solid #15803d;
  border-radius: 3px;
  color: #15803d;
  font-style: normal;
  font-size: 11px;
  line-height: 14px;
}
.legend.is-formula-write::before {
  content: 'ƒx';
  margin-right: 4px;
}
/* The formula gives an error with the proposed values (#N/A): red, the only colour for errors. */
.proposal-sheet .tabulator-cell.is-formula-error {
  color: #b91c1c;
  background: #fef2f2;
}
/* A formula that could not be calculated here: the sheet's value, underlined dotted. */
.proposal-sheet .tabulator-cell.is-formula-stale {
  text-decoration: underline dotted #a8a29e;
}
/* The assistant says something about the cell (hover, or select it to read it in the bar): a corner, as a comment in Google Sheets. */
.proposal-sheet .tabulator-cell.has-comment {
  position: relative;
}
.proposal-sheet .tabulator-cell .comment-mark {
  position: absolute;
  top: 0;
  right: 0;
  border-style: solid;
  border-width: 0 7px 7px 0;
  border-color: transparent #ea8600 transparent transparent;
  pointer-events: none;
}
/* Beside the red corner of a value outside the list. */
.proposal-sheet .tabulator-cell.is-invalid .comment-mark {
  right: 8px;
}
/* The selected doubtful cell's choices, under it (over it near the grid's bottom). */
.proposal-sheet .doubt-quick {
  position: absolute;
  z-index: 30;
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 4px;
  width: max-content;
  max-width: calc(100% - 8px);
  border: 1px solid #fcd34d;
  border-radius: 6px;
  background: #fffbeb;
  padding: 3px;
  box-shadow: 0 2px 8px rgb(0 0 0 / 0.15);
  font-size: 12px;
  color: #78350f;
}
.proposal-sheet .doubt-quick.is-above {
  transform: translateY(-100%);
}
.proposal-sheet .doubt-quick button {
  display: inline-flex;
  align-items: center;
  gap: 3px;
  min-height: 24px;
  border: 1px solid #fcd34d;
  border-radius: 4px;
  background: white;
  padding: 0 8px;
  font-variant-numeric: tabular-nums;
}
.proposal-sheet .doubt-quick button:hover {
  background: #fde68a;
}
.proposal-sheet .doubt-quick .doubt-quick-ok {
  border-color: #6ee7b7;
  background: #ecfdf5;
  color: #065f46;
  font-weight: 600;
}
.proposal-sheet .doubt-quick .doubt-quick-ok:hover {
  background: #d1fae5;
}
.proposal-sheet .doubt-quick-key {
  padding: 0 4px;
  color: #a16207;
  font-family: inherit;
  font-size: 10px;
}
@media (pointer: coarse) {
  .proposal-sheet .doubt-quick button {
    min-height: 36px;
    padding: 0 12px;
    font-size: 14px;
  }
}
/*
 * A slim row between the rows: where they are not continuous in the sheet, a jump of the notebook's
 * lines, «not on the photo». Drawn as a tear: the rows above and below end in teeth over a grey gap.
 */
.proposal-sheet .tabulator-row.is-marker-row,
.proposal-sheet .tabulator-row.is-marker-row .tabulator-cell {
  min-height: 0;
  border-color: transparent;
  background: #e7e5e4;
}
.proposal-sheet .tabulator-row.is-marker-row .tabulator-cell {
  padding-top: 7px;
  padding-bottom: 7px;
  font-size: 11px;
  line-height: 16px;
}
.proposal-sheet .tabulator-row.is-marker-row::before,
.proposal-sheet .tabulator-row.is-marker-row::after {
  content: '';
  position: absolute;
  left: 0;
  right: 0;
  height: 6px;
  z-index: 13;
  pointer-events: none;
  background-repeat: repeat-x;
  background-size: 12px 6px;
}
.proposal-sheet .tabulator-row.is-marker-row::before {
  top: 0;
  background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='12' height='6'%3E%3Cpath d='M0 0L6 5.5L12 0Z' fill='white'/%3E%3Cpath d='M0 0L6 5.5L12 0' fill='none' stroke='%23a8a29e' stroke-width='0.8'/%3E%3C/svg%3E");
}
.proposal-sheet .tabulator-row.is-marker-row::after {
  bottom: 0;
  background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='12' height='6'%3E%3Cpath d='M0 6L6 0.5L12 6Z' fill='white'/%3E%3Cpath d='M0 6L6 0.5L12 6' fill='none' stroke='%23a8a29e' stroke-width='0.8'/%3E%3C/svg%3E");
}
/* One that stands for sheet rows not shown: a click opens them under it (the tear stays). */
.proposal-sheet .tabulator-row.is-marker-row.is-peekable,
.proposal-sheet .tabulator-row.is-marker-row.is-peekable .tabulator-cell {
  cursor: pointer;
}
.proposal-sheet .tabulator-row.is-marker-row.is-peekable:hover,
.proposal-sheet .tabulator-row.is-marker-row.is-peekable:hover .tabulator-cell {
  background: #d6d3d1;
}
.proposal-sheet .tabulator-row.is-marker-row.is-peekable:hover .marker-text:not(.is-jump-down) {
  color: #44403c;
}
.proposal-sheet .marker-action {
  color: #57534e;
  font-size: 10px;
  white-space: nowrap;
}
.proposal-sheet .marker-action.is-open {
  padding: 0 5px;
  border: 1px solid #a8a29e;
  border-radius: 3px;
  background: #fafaf9;
}
.proposal-sheet .marker-action.is-error {
  color: #b91c1c;
}
/* Its text runs over the empty cells beside it, and stays at the left while scrolling. */
.proposal-sheet .tabulator-row.is-marker-row .tabulator-cell.is-marker-cell {
  overflow: visible;
  z-index: 12;
}
.proposal-sheet .marker-text {
  white-space: nowrap;
  color: #78716c;
  font-style: italic;
}
.proposal-sheet .marker-text.is-jump-down,
.proposal-sheet .marker-text.is-jump-up {
  display: inline-block;
  padding: 0 6px;
  border-radius: 3px;
  font-style: normal;
  font-weight: 600;
  font-variant-numeric: tabular-nums;
}
.proposal-sheet .marker-text.is-jump-down {
  background: #e0f2fe;
  color: #075985;
}
.proposal-sheet .marker-text.is-jump-up {
  background: #fef3c7;
  color: #92400e;
}
.proposal-sheet .marker-text.is-apart {
  font-style: normal;
  font-weight: 600;
  color: #57534e;
}
/* A repeated ID (A0E.1): which ID it repeats and that ID's row. */
.proposal-sheet .repeat-chip,
.proposal-sheet .off-photo {
  display: inline-block;
  padding: 0 4px;
  border: 1px solid #d6d3d1;
  border-radius: 3px;
  color: #57534e;
  background: #f5f5f4;
  font-size: 10px;
  line-height: 13px;
  white-space: nowrap;
}
.proposal-sheet .repeat-chip {
  border-color: #99f6e4;
  background: #f0fdfa;
  color: #115e59;
}
.proposal-sheet .repeat-chip.is-link {
  cursor: pointer;
  text-decoration: underline dotted;
}
.proposal-sheet .tabulator-cell .formula-mark {
  display: inline-block;
  padding: 0 4px;
  border: 1px solid #d6d3d1;
  border-radius: 3px;
  color: #78716c;
  font-size: 10px;
  font-style: normal;
  line-height: 13px;
}
</style>
