import type { CellValue } from './types'
import type { TableRow } from './rowsTable'
import { t, tn, tx, type Msg } from './i18n'

/**
 * The assistant's proposed changes as the Asistente tab shows them: a live
 * table both the assistant (update_proposal) and the person (typing in it)
 * edit. Pure helpers, kept apart from the grid so they can be tested.
 */
export interface PersonEdit {
  /** What the assistant had proposed there (absent: no change to that cell). */
  ai?: CellValue
  by?: string
  at?: string
}
/**
 * A cell the assistant read with a doubt (match_notebook): its value goes in,
 * highlighted, with how sure the reading was, the other readings and why. It
 * counts as reviewed once the person edits it, takes one of the other readings,
 * keeps the sheet's value, or marks it checked.
 */
export interface Doubt {
  confidence?: number
  alternatives?: CellValue[]
  reason?: string | null
  /** The reason's descriptor, to show it in the chosen language (the reader's own words come without one). */
  reasonMsg?: Msg
  checked?: { by?: string; at?: string; how?: string }
}
/**
 * A cell the assistant could not read at all (match_notebook's null): it has no
 * value, so applying never writes it, until the person types one. `reason`: why
 * (the reader's words, e.g. "smudged"); `partial`: what of it was read, as
 * written (e.g. "1?/9"), to complete.
 */
export interface Unreadable {
  reason?: string | null
  reasonMsg?: Msg
  partial?: string[]
}
/** Where a value the line does not write comes from (a template, a word of the note). */
export interface Hint {
  text: string
  msg?: Msg
}
/**
 * Where a row is on the notebook page (match_notebook): its photo (0 = the
 * first) and line. A line without a row of its own also gives the line as
 * written and its state: `match` (in the sheet, nothing to write), `missing`
 * (not found), `crossed`, `ambiguous`, `duplicate`, `nokey`, `new`; `error`:
 * why the save would refuse it; `near`: IDs it may be.
 */
export interface PageLine {
  photo: number
  line: number
  raw?: string
  status?: string
  error?: string
  message?: string
  near?: { value: CellValue; row: number | null }[]
}
/**
 * A cell edited in the sheet after the assistant read it (server/sheet-edits.mjs):
 * what was read then and what the sheet has now, who changed it (a person in the
 * app, or the sheet as the app saw it: 'sheets', its edit trigger; 'sync', a
 * read) and when. The sheet's value is kept unless the person chose the
 * proposal's (`use`), a choice that holds while the sheet keeps `now`; `again`:
 * edited again after they chose (applying waits for a new choice).
 */
export interface SheetEdit {
  read: CellValue
  now: CellValue
  source?: 'app' | 'sheets' | 'sync'
  by?: string
  at?: string
  use?: 'sheet' | 'proposal'
  decidedBy?: string | null
  decidedAt?: string | null
  again?: boolean
}
/** A new row whose pre-made row (its Insectary ID) someone used meanwhile: left out when applying. */
export interface RowTaken {
  row: number
  label?: string
  source?: 'app' | 'sheets' | 'sync'
  by?: string
  at?: string
}
/**
 * The notebook page a proposal was read from: its notebook, the sheet, its
 * columns in the page's order, its key columns (the row's own ID), how many
 * photos (0 when only the proposal's reason names the notebook).
 */
export interface ProposalPage {
  kind: string
  sheet: string
  columns: string[]
  keys: string[]
  photos: number
}
export interface ProposalChange {
  index: number
  /** Stable while the proposal is revised: a new row's clientId, an edited row's recordId. */
  key: string
  /** null for a new row not written yet. */
  recordId: string | null
  sheet: string
  /** null for a new row not written yet. */
  row: number | null
  label: string
  create?: boolean
  clientId?: string
  values: Record<string, CellValue>
  /** The sheet's values of the changed columns (existing rows of a proposal no longer pending). */
  current?: Record<string, CellValue>
  /** An existing row's values (pending proposals): its changed columns and the rest, for columns added to the table. */
  rowValues?: Record<string, CellValue>
  /** Formula columns of an existing row: not editable. */
  formulas?: string[]
  replaceFormula?: string[]
  /** Cells the person typed in the table. */
  personEdits?: Record<string, PersonEdit>
  note?: string
  /** Doubtful cells (match_notebook), by column. */
  doubts?: Record<string, Doubt>
  /** Cells nobody could read (match_notebook), by column: empty until someone fills them. */
  unreadable?: Record<string, Unreadable>
  /** Columns the notebook line does not write: the page's room, a template, the note's words. */
  inferred?: string[]
  hints?: Record<string, Hint>
  /**
   * Cells the row would leave empty on a butterfly preserved without its CAM or
   * tube (server/preserved.mjs), with why: marked until someone fills them.
   */
  warnings?: Record<string, Hint>
  /** What the data checks (Revisión) say about the sheet's value of a cell, by column. */
  checks?: Record<string, Hint[]>
  /** A notebook line shown only for context: never written. */
  context?: boolean
  /** A page line with no sheet row (not found, crossed out): shown as written, never written. */
  placeholder?: boolean
  /**
   * What the row's formula cells will give once it is written, those that
   * differ from what the sheet shows now (server/formula-gives.mjs; an error as
   * its code, #N/A): shown, never written.
   */
  formulaGives?: Record<string, CellValue>
  /** Formula cells the proposal reaches that could not be calculated here: the sheet's value is shown, marked. */
  formulaFallback?: string[]
  /** Its place on the notebook page. */
  page?: PageLine
  /** A row off the photo with the same error as this line of the page (match_notebook): shown apart, after the page. */
  sameErrorAs?: number
  /** Cells edited in the sheet since they were read, by column (pending proposals). */
  sheetChanged?: Record<string, SheetEdit>
  /** A new row whose pre-made row was used meanwhile. */
  rowTaken?: RowTaken
  /**
   * A context row that is a row of the sheet between the proposal's rows (a
   * proposal without a notebook page, shown in sheet order): never written nor editable.
   */
  gap?: boolean
  /** A notebook line that comes before this line of its photo in the sheet, though after it on the page. */
  outOfOrder?: { line: number; id: string }
  /** A row not written yet: the sheet row it will go to (x.5: inserted below row x). */
  place?: number
  /**
   * An Insectary ID written as a repeat (A0E.1): its base ID, that ID's row (and
   * whether it is still an empty pre-made row), the ID of the row just above it.
   */
  repeatOf?: RepeatOf
}
export interface RepeatOf {
  id: string
  row?: number
  empty?: boolean
  above?: string
}
/** A notebook page's line whose sheet row goes another way than the page (within its photo). */
export interface OrderNote {
  photo: number
  line: number
  id: string
  /** The line before it on the page, which comes after it in the sheet. */
  after: { line: number; id: string }
}
export interface Proposal {
  id: string
  /** For a table (show_rows): its title. */
  reason: string
  /** A table of rows the assistant shows (show_rows) is 'shown', then 'closed': never pending, never applied. */
  /** queued: applied while Google did not answer; its save waits in the app and is written when it does. */
  status: 'pending' | 'applying' | 'queued' | 'applied' | 'needs_review' | 'discarded' | 'shown' | 'closed'
  /** 'table': rows of the sheet to read (lib/rowsTable), not changes. */
  kind?: 'table'
  /** A table's rows, with the sheet's current values of its columns (`fields`). */
  rows?: TableRow[]
  sheets?: string[]
  /** The conversation it comes from (T3 Code, Revisión de datos, a chat). */
  source?: string
  createdAt?: string
  /** Goes up on every change, by the assistant or the person. */
  revision?: number
  updatedAt?: string | null
  lastBy?: 'ai' | 'person' | null
  fields: string[]
  types: Record<string, string>
  /** Formula columns of the pre-made rows new rows go into, per sheet. */
  newRowFormulas?: Record<string, string[]>
  /** An existing row's formula columns, per sheet (a row's own `formulas` when they differ). */
  sheetFormulas?: Record<string, string[]>
  /** The cells' hints, once (the server sends a row's hints as indexes into it; see expandProposal). */
  hintTable?: { text?: string; msg?: Msg }[]
  /** A notebook page's proposal: the page. */
  page?: ProposalPage
  /**
   * The columns each sheet's table shows whatever the proposal changes, in order
   * (reviewColumns in server/notebook.mjs); `keys`: the row's own ID, shown in the
   * table's ID column (its own column only when the proposal changes it);
   * `hidden`: columns never shown, not even added (server/proposal-columns.mjs).
   */
  shownColumns?: Record<string, { fields: string[]; keys: string[]; hidden?: string[] }>
  /** Changes when the sheet's rows of a pending proposal change (an edit in the sheet): the list redraws it. */
  sheetStamp?: string
  /** The rows are listed in the sheet's order: a notebook page's lines that go another way there. */
  outOfOrder?: OrderNote[]
  /** A hash of it as the server sent it: the list asks again with it and gets only { id, digest, same } while it holds. */
  digest?: string
  same?: boolean
  applied: number[] | null
  changes: ProposalChange[]
}

/**
 * A proposal as the server sends it (lean, for slow connections) made whole
 * for the table: each row's hints from the proposal's table, its formula
 * columns from its sheet's. Already whole: returned as it is.
 */
export function expandProposal(p: Proposal): Proposal {
  if (!p.hintTable && !p.sheetFormulas) return p
  return {
    ...p,
    changes: p.changes.map(c => {
      const hints = c.hints as Record<string, Hint | number> | undefined
      const out = { ...c }
      if (hints)
        out.hints = Object.fromEntries(
          Object.entries(hints).map(([f, h]) => {
            const entry = typeof h === 'number' ? p.hintTable?.[h] : h
            return [f, { text: entry?.text ?? '', ...(entry?.msg ? { msg: entry.msg } : {}) }]
          }),
        )
      const checks = c.checks as Record<string, (Hint | number)[]> | undefined
      if (checks)
        out.checks = Object.fromEntries(
          Object.entries(checks).map(([f, list]) => [
            f,
            list.map(h => {
              const entry = typeof h === 'number' ? p.hintTable?.[h] : h
              return { text: entry?.text ?? '', ...(entry?.msg ? { msg: entry.msg } : {}) }
            }),
          ]),
        )
      if (!c.formulas && !c.create && !c.placeholder && p.sheetFormulas?.[c.sheet]) out.formulas = p.sheetFormulas[c.sheet]
      return out
    }),
  }
}

/** A row the person only reads (never written): a page line as the sheet has it, or as written. A line the save refused can be corrected. */
export const readOnlyRow = (c: Pick<ProposalChange, 'context' | 'placeholder' | 'page' | 'recordId'>) =>
  !!c.placeholder || (!!c.context && !(c.page?.error && c.recordId))
/** A page line shown without a row of its own in the proposal (nothing to take out). */
export const pageOnly = (c: Pick<ProposalChange, 'index'>) => c.index < 0

/** What a page line that writes nothing says in the Nota column: the line as written and why. */
export function pageNote(c: Pick<ProposalChange, 'page' | 'context' | 'note'>): string {
  const p = c.page
  if (!p || (!c.context && !p.error)) return c.note ?? ''
  const near = (p.near ?? []).map(n => (n.row ? t('{id} (fila {row})', { id: String(n.value), row: n.row }) : String(n.value))).join(', ')
  const why = p.error
    ? t('No se puede escribir: {error}', { error: t(p.error) })
    : (
        {
          match: t('Ya está así en la hoja'),
          missing: near ? t('No está en la hoja; ¿quisiste decir {ids}?', { ids: near }) : t('No está en la hoja: ¿está bien leído?'),
          crossed: t('Tachada en el cuaderno: no se usa'),
          ambiguous: t('Varias filas de la hoja podrían ser esta'),
          duplicate: t('El mismo ID está en otra línea de la página'),
          nokey: t('Sin ID legible'),
        } as Record<string, string>
      )[p.status ?? ''] ?? ''
  const line = p.raw !== undefined ? t('Línea {n}: «{raw}»', { n: p.line, raw: p.raw }) : ''
  // A context row from match_notebook already says the line (and why) in its note.
  return c.note ? [c.note, p.error ? why : ''].filter(Boolean).join(' · ') : [line, why].filter(Boolean).join(' · ')
}

/** A page's photos as their headers say them: the lines on each, how many write something, how many are as the sheet has them. */
export function photoSummaries(changes: Pick<ProposalChange, 'page' | 'context' | 'placeholder'>[]) {
  const out = new Map<number, { photo: number; from: number; to: number; change: number; same: number; other: number }>()
  for (const c of changes) {
    if (!c.page) continue
    const s = out.get(c.page.photo) ?? { photo: c.page.photo, from: c.page.line, to: c.page.line, change: 0, same: 0, other: 0 }
    s.from = Math.min(s.from, c.page.line)
    s.to = Math.max(s.to, c.page.line)
    if (c.placeholder || c.page.error) s.other++
    else if (c.context) s.same++
    else s.change++
    out.set(c.page.photo, s)
  }
  return [...out.values()].sort((a, b) => a.photo - b.photo)
}

export const rowKey = (c: Pick<ProposalChange, 'key' | 'clientId' | 'recordId' | 'index'>) =>
  c.key ?? c.clientId ?? c.recordId ?? `i${c.index}`
export const cellId = (key: string, field: string) => `${key}\u0000${field}`
const same = (a: CellValue | undefined, b: CellValue | undefined) => JSON.stringify(a ?? null) === JSON.stringify(b ?? null)

/**
 * How a cell of the table looks: `proposed` (green, the assistant's), `person`
 * (typed by the person), `reverted` (the person set it back to the sheet's
 * value, or emptied a new row's cell: the assistant's value is kept aside, not
 * written), `sheet` (an existing row's value, unchanged), `empty` (a new row's
 * cell with nothing yet), `locked` (a formula), `unreadable` (the assistant
 * could not read it and nobody filled it yet: not written), `kept` (edited in
 * the sheet after the proposal: the sheet's value stays, the proposal's is
 * set aside, not written).
 */
export type CellKind = 'proposed' | 'person' | 'reverted' | 'sheet' | 'empty' | 'locked' | 'unreadable' | 'kept'
export interface CellInfo {
  value: CellValue
  kind: CellKind
  was?: CellValue
  ai?: CellValue
  aiProposed: boolean
  /** The assistant's doubt about this cell, if it had one. */
  doubt?: Doubt
  /** A doubtful value of the assistant's nobody has reviewed yet (amber, dashed: check it before applying). */
  doubtful: boolean
  /** The value is not written on the notebook line (italic): where it comes from is in `hint`. */
  inferred: boolean
  hint?: Hint
  /** The assistant could not read this cell (still empty when `kind` is 'unreadable', filled otherwise). */
  unreadable?: Unreadable
  /** A preserved butterfly would be left without this cell (its CAM or tube): ask for it before applying. */
  warning?: Hint
  /** What the data checks say about the sheet's value of this cell (a tube with a digit missing…). */
  checks?: Hint[]
  /** The value is what the sheet's formula will give once applied (SPECIES from the clutch): shown grey, never written. */
  fromFormula?: boolean
  /** A formula cell that could not be calculated here: the sheet's current value, marked as such. */
  formulaFallback?: boolean
  /** Edited in the sheet since the proposal read it (violet): kept from the sheet, or the proposal's written over it. */
  sheetEdit?: SheetEdit
  /** A `kept` cell's value in the proposal (set aside, not written). */
  proposalValue?: CellValue
}
export function cellOf(change: ProposalChange, field: string, newRowFormulas: string[] = []): CellInfo {
  const mark = change.personEdits?.[field]
  const was = change.create ? undefined : (change.current?.[field] ?? change.rowValues?.[field] ?? null)
  const ai = mark && 'ai' in mark ? mark.ai : undefined
  const aiProposed = !!mark && 'ai' in mark
  const doubt = change.doubts?.[field]
  const unreadable = change.unreadable?.[field]
  const warning = change.warnings?.[field]
  const checks = change.checks?.[field]
  const extra = {
    doubt,
    hint: change.hints?.[field],
    ...(unreadable ? { unreadable } : {}),
    ...(warning ? { warning } : {}),
    ...(checks?.length ? { checks } : {}),
  }
  const sheetEdit = !change.create && field in change.values ? change.sheetChanged?.[field] : undefined
  // Edited in the sheet since it was read, and nobody chose the proposal's: the sheet's value stays.
  if (sheetEdit && !keptOver(sheetEdit))
    return {
      value: sheetEdit.now,
      kind: 'kept',
      was: sheetEdit.now,
      ai,
      aiProposed,
      ...extra,
      sheetEdit,
      proposalValue: change.values[field],
      doubtful: false,
      inferred: false,
    }
  if (field in change.values) {
    const kind: CellKind = mark ? 'person' : 'proposed'
    return {
      value: change.values[field],
      kind,
      // Written over what the sheet has now.
      was: sheetEdit ? sheetEdit.now : was,
      ai,
      aiProposed,
      ...extra,
      ...(sheetEdit ? { sheetEdit } : {}),
      doubtful: kind === 'proposed' && !!doubt && !doubt.checked,
      inferred: kind === 'proposed' && !!change.inferred?.includes(field),
    }
  }
  const quiet = { ...extra, doubtful: false, inferred: false }
  // Nobody could read it and nobody filled it: shown empty (or with the sheet's value), never written.
  if (unreadable && !mark) return { value: change.create ? null : (was ?? null), kind: 'unreadable', was, aiProposed, ...quiet }
  // What the formula will give once the row is written, in place of the sheet's (blank or older) value.
  const gives = change.formulaGives?.[field]
  const fallback = !!change.formulaFallback?.includes(field)
  const formula =
    gives !== undefined && (gives !== null && gives !== '' ? true : !change.create && !(field in change.values))
      ? { value: gives ?? null, fromFormula: true }
      : fallback
        ? { formulaFallback: true }
        : null
  if (mark)
    return { value: change.create ? null : (was ?? null), kind: aiProposed ? 'reverted' : 'person', was, ai, aiProposed, ...quiet, ...formula }
  const locked = change.create ? newRowFormulas.includes(field) : !!change.formulas?.includes(field)
  if (change.create) return { value: null, kind: locked ? 'locked' : 'empty', aiProposed, ...quiet, ...formula }
  return { value: was ?? null, kind: locked ? 'locked' : 'sheet', was, aiProposed, ...quiet, ...formula }
}

/** A formula's error as the sheet shows it (#N/A, #REF!…): the cell is marked so the person sees the problem. */
export const isFormulaError = (v: CellValue | undefined) =>
  typeof v === 'string' && /^#(N\/A|REF!|VALUE!|DIV\/0!|NAME\?|NUM!|NULL!|ERROR!)$/.test(v)

/** The person chose to write the proposal's value over the sheet's edit (and nobody edited it again since). */
export const keptOver = (e: SheetEdit) => e.use === 'proposal' && !e.again
/** The cells of a row the save writes: those with a value, but the ones edited in the sheet whose value stays. */
export function writtenFields(c: Pick<ProposalChange, 'values' | 'sheetChanged' | 'rowTaken'>): string[] {
  if (c.rowTaken) return []
  return Object.keys(c.values).filter(f => !c.sheetChanged?.[f] || keptOver(c.sheetChanged[f]))
}

/** When something was done, day first, in Ecuador's time: 2/10/26 14:05. */
export function whenText(iso: string | null | undefined): string {
  if (!iso) return ''
  const at = new Date(iso)
  if (Number.isNaN(at.getTime())) return ''
  const parts = Object.fromEntries(
    new Intl.DateTimeFormat('en-US', {
      timeZone: 'America/Guayaquil',
      year: '2-digit',
      month: 'numeric',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
      hourCycle: 'h23',
    })
      .formatToParts(at)
      .map(p => [p.type, p.value]),
  )
  return `${Number(parts.day)}/${Number(parts.month)}/${parts.year} ${parts.hour}:${parts.minute}`
}
/** Who edited a cell (or a row) in the sheet: the person of the app, else the sheet itself. */
export const editedBy = (e: Pick<SheetEdit, 'by' | 'source'>) => e.by || (e.source === 'app' ? t('la app') : 'Google Sheets')

/** The same text as a formula gives, whatever the spacing or capitals (as sameAsFormula in server/batch.mjs). */
export const sameAsFormula = (a: CellValue | undefined, b: CellValue | undefined) => {
  const text = (v: CellValue | undefined) => String(v ?? '').trim().replace(/\s+/g, ' ').toLowerCase()
  return text(a) === text(b)
}

/**
 * What the assistant says about a cell, as the bar under the table's cell bar
 * shows it and the cell's tooltip leads with: a preserved butterfly left
 * without it, a doubtful reading (or one reviewed), where a value the line does
 * not write comes from, why a cell was unreadable. A cell with any gets a
 * corner mark in the table.
 */
export interface CellComment {
  label: string
  text: string
  kind: 'doubt' | 'hint' | 'unreadable' | 'edited'
}
/** `show`: a cell value as the table writes it (dates day first). */
export function cellComments(c: CellInfo, show: (value: CellValue | undefined) => string = v => String(v ?? '')): CellComment[] {
  const out: CellComment[] = []
  // Edited in the sheet after the proposal: what was read, what the sheet has, and what applying does.
  if (c.sheetEdit) {
    const e = c.sheetEdit
    const who = [editedBy(e), whenText(e.at)].filter(Boolean).join(', ')
    const read = t('Se leyó {read} al proponer; la hoja tiene ahora {now} ({who})', {
      read: show(e.read) || t('vacío'),
      now: show(e.now) || t('vacío'),
      who,
    })
    const then = e.again
      ? t('editada otra vez después de elegir: elige de nuevo')
      : keptOver(e)
        ? e.decidedBy
          ? t('se escribe el valor de la propuesta encima (eligió {who})', { who: e.decidedBy })
          : t('se escribe el valor de la propuesta encima')
        : t('se mantiene el de la hoja')
    out.push({ label: t('Editada en la hoja'), text: `${read} · ${then}`, kind: 'edited' })
  }
  if (c.warning) out.push({ label: t('Falta'), text: tx(c.warning.text, c.warning.msg), kind: 'doubt' })
  // The checks are about the sheet's value: said while the cell keeps it.
  if (['sheet', 'locked', 'kept', 'reverted'].includes(c.kind))
    for (const h of c.checks ?? []) out.push({ label: t('Revisión'), text: tx(h.text, h.msg), kind: 'doubt' })
  if (c.doubt) {
    const reason = c.doubt.reason ? tx(c.doubt.reason, c.doubt.reasonMsg) : t('Lectura dudosa')
    if (c.doubtful) out.push({ label: t('Dudosa'), text: reason, kind: 'doubt' })
    else if (c.kind === 'proposed' || c.kind === 'person' || c.kind === 'reverted')
      out.push({
        label: t('Revisada'),
        text: c.doubt.checked?.by ? t('{reason} (por {who})', { reason, who: c.doubt.checked.by }) : reason,
        kind: 'hint',
      })
  }
  if (c.hint && c.kind === 'proposed') out.push({ label: t('No escrito en la línea'), text: tx(c.hint.text, c.hint.msg), kind: 'hint' })
  if (c.unreadable) {
    const reason = c.unreadable.reason ? tx(c.unreadable.reason, c.unreadable.reasonMsg) : t('La IA no pudo leerla')
    if (c.kind === 'unreadable') out.push({ label: t('Ilegible'), text: reason, kind: 'unreadable' })
    else out.push({ label: t('Ilegible en el cuaderno'), text: t('{reason} (rellenada a mano)', { reason }), kind: 'hint' })
  }
  return out
}

/**
 * The cells of the tables in the order they are shown (table by table, row by
 * row, column by column), as a position to sort by; a cell not shown goes last.
 */
export function cellOrder(tables: { keys: string[]; fields: string[] }[]) {
  const at = new Map<string, number>()
  let n = 0
  for (const { keys, fields } of tables) for (const key of keys) for (const field of fields) at.set(cellId(key, field), n++)
  return (key: string, field: string) => at.get(cellId(key, field)) ?? Number.MAX_SAFE_INTEGER
}

/**
 * The cell to review after `from` (a doubtful or unreadable one, as listed by
 * uncheckedDoubts or unfilledUnreadable), in the tables' order: the next one
 * after it, else the first again; without `from`, the first. `from` itself (it
 * may still be listed while its check is saved) is never the next one.
 */
export function nextCell<T extends { key: string; field: string }>(
  cells: T[],
  position: (key: string, field: string) => number,
  from?: { key: string; field: string } | null,
): T | null {
  const others = from ? cells.filter(c => c.key !== from.key || c.field !== from.field) : cells
  if (!others.length) return null
  const sorted = [...others].sort((a, b) => position(a.key, a.field) - position(b.key, b.field))
  if (!from) return sorted[0]
  const here = position(from.key, from.field)
  return sorted.find(c => position(c.key, c.field) > here) ?? sorted[0]
}

/**
 * The doubtful cells "Aplicar" would write without anyone reviewing them, in
 * the rows chosen (as rowKey + field, with the row's index): the same rule as
 * the server's (server/doubts.mjs), so the dialog and the server agree.
 */
export function uncheckedDoubts(p: Pick<Proposal, 'changes'>, indexes?: number[]) {
  const out: { key: string; field: string; index: number }[] = []
  const chosen = indexes ? new Set(indexes) : null
  for (const c of p.changes) {
    if (c.context || (chosen && !chosen.has(c.index))) continue
    const written = new Set(writtenFields(c))
    for (const [field, doubt] of Object.entries(c.doubts ?? {}))
      if (written.has(field) && !doubt.checked && !c.personEdits?.[field]) out.push({ key: rowKey(c), field, index: c.index })
  }
  return out
}

/**
 * The cells edited in the sheet since the proposal read them (as rowKey + field,
 * with the row's index and what applying does), and the new rows whose pre-made
 * row was used meanwhile (field null).
 */
export function sheetEdits(p: Pick<Proposal, 'changes'>) {
  const out: { key: string; field: string | null; index: number; label: string; edit?: SheetEdit; taken?: RowTaken }[] = []
  for (const c of p.changes) {
    if (c.context) continue
    if (c.rowTaken) out.push({ key: rowKey(c), field: null, index: c.index, label: c.label, taken: c.rowTaken })
    for (const [field, edit] of Object.entries(c.sheetChanged ?? {}))
      if (field in c.values) out.push({ key: rowKey(c), field, index: c.index, label: c.label, edit })
  }
  return out
}

/**
 * What the person tells the assistant about the cells edited in the sheet since
 * its proposal (pasted, or sent to its chat): which, what it read and what the
 * sheet has now, to look at them again and update the proposal.
 */
export function tellText(p: Pick<Proposal, 'id' | 'reason' | 'changes'>, show: (field: string, value: CellValue | undefined) => string) {
  const cells = sheetEdits(p).map(e =>
    e.field && e.edit
      ? t('{row} {field}: leído {read}, ahora {now}', {
          row: e.label,
          field: e.field,
          read: show(e.field, e.edit.read) || t('vacío'),
          now: show(e.field, e.edit.now) || t('vacío'),
        })
      : t('{row}: su fila sin usar {n} ya se usó en la hoja', { row: e.label, n: e.taken?.row ?? '' }),
  )
  return t(
    'La hoja cambió después de tu propuesta {id} ({reason}): {cells}. Vuelve a mirar esas celdas (la foto, get_proposal) y actualiza la propuesta.',
    { id: p.id, reason: p.reason, cells: cells.join('; ') },
  )
}

/** The cells marked as missing on a preserved butterfly (its CAM or tube), as rowKey + field, with the row's index. */
export function sampleWarnings(p: Pick<Proposal, 'changes'>) {
  const out: { key: string; field: string; index: number }[] = []
  for (const c of p.changes)
    if (!c.context) for (const field of Object.keys(c.warnings ?? {})) out.push({ key: rowKey(c), field, index: c.index })
  return out
}

/**
 * The unreadable cells still empty (as rowKey + field, with the row's index):
 * the same rule as the server's (unfilledUnreadable in server/doubts.mjs).
 * Applying leaves them as the sheet has them.
 */
export function unfilledUnreadable(p: Pick<Proposal, 'changes'>) {
  const out: { key: string; field: string; index: number }[] = []
  for (const c of p.changes) {
    if (c.context) continue
    for (const field of Object.keys(c.unreadable ?? {})) if (!(field in c.values)) out.push({ key: rowKey(c), field, index: c.index })
  }
  return out
}

/** Values a template fills (no news): a column holding only these is folded away. */
const TEMPLATE_VALUE = new Set(['NA', 'NOT_COLLECTED'])

/**
 * The proposal's rows split by sheet (one table each, with that sheet's
 * columns). A sheet with columns always shown (`shownColumns`: for
 * Insectary_data the notebook's, then the sheet's others up to
 * Notes_Insectary_data) has those first, unchanged ones too, so the person
 * reads each row whole; then the columns it changes and those the person
 * added beyond them, in the sheet's order when known. Without such columns,
 * only the changed and added ones, and a notebook page's sheet follows the
 * notebook: its columns in the page's order, then the implied ones, then the
 * rest. On a notebook page, changed columns beyond those shown
 * that hold only a template's NA / NOT_COLLECTED go last, listed in
 * `template` (the table can fold them).
 */
export function sheetGroups(
  p: Pick<Proposal, 'changes' | 'fields' | 'page' | 'shownColumns'>,
  extra: Record<string, string[]> = {},
  order: (sheet: string) => string[] | undefined = () => undefined,
) {
  const sheets = [...new Set(p.changes.map(c => c.sheet))]
  return sheets.map(sheet => {
    const changes = p.changes.filter(c => c.sheet === sheet)
    const used = new Set(
      changes.flatMap(c => [
        ...Object.keys(c.values),
        ...Object.keys(c.personEdits ?? {}),
        ...Object.keys(c.unreadable ?? {}),
        ...Object.keys(c.warnings ?? {}),
      ]),
    )
    const added = new Set(extra[sheet] ?? [])
    // The columns it changes and those the person adds, at their place in the sheet.
    const sorted = (order(sheet) ?? p.fields).filter(f => used.has(f) || added.has(f))
    // Columns the sheet no longer lists still show.
    for (const f of [...used, ...added]) if (!sorted.includes(f)) sorted.push(f)
    const base = p.shownColumns?.[sheet]
    const paged = p.page?.sheet === sheet
    if (!base && !paged) return { sheet, fields: sorted, changes, template: [] as string[] }
    // The columns always shown first (a notebook page's in the page's order, the ID being the row's own
    // column), so the person reads each row beside its line; even those with nothing to write (SPECIES, a formula).
    const keys = base?.keys ?? p.page?.keys ?? []
    const notebook = (base?.fields ?? p.page?.columns ?? []).filter(f => !keys.includes(f) || used.has(f))
    const template = sorted.filter(
      f =>
        paged &&
        used.has(f) &&
        !notebook.includes(f) &&
        changes.every(c => {
          if (c.personEdits?.[f] || c.unreadable?.[f] || c.doubts?.[f]) return false
          const v = c.values[f]
          return v === undefined || (typeof v === 'string' && TEMPLATE_VALUE.has(v))
        }),
    )
    const implied = new Set(changes.flatMap(c => c.inferred ?? []))
    const rest = sorted.filter(f => !notebook.includes(f) && !template.includes(f))
    const fields = [...notebook, ...rest.filter(f => implied.has(f)), ...rest.filter(f => !implied.has(f)), ...template]
    return { sheet, fields, changes, template }
  })
}

/** Cells whose value changed between two versions of a proposal (to flash them), as cellId(key, field). */
export function changedCells(prev: Proposal | undefined, next: Proposal): string[] {
  if (!prev || prev.id !== next.id) return []
  const before = new Map(prev.changes.map(c => [rowKey(c), c]))
  const out: string[] = []
  for (const c of next.changes) {
    const key = rowKey(c)
    const old = before.get(key)
    const fields = new Set([...Object.keys(c.values), ...Object.keys(old?.values ?? {})])
    for (const f of fields) {
      const now = f in c.values ? c.values[f] : undefined
      const then = old && f in old.values ? old.values[f] : undefined
      if (!old || !same(now, then)) out.push(cellId(key, f))
    }
  }
  return out
}

/**
 * A cell the person changed and not yet saved: a value typed or pasted, or one
 * of the two buttons: `use: 'sheet'` (back to the sheet's value; in a new row,
 * empty) or `use: 'ai'` (the assistant's value again, given as `value`).
 */
export interface LocalCell {
  value: CellValue
  use?: 'sheet' | 'ai'
}

/**
 * The person's edits not yet saved, laid over the server's copy: a revision
 * arriving from the assistant meanwhile must not undo what was just typed.
 * Marked as the server will mark them (reviseChanges in server/assistant.mjs).
 */
export function withLocal(
  p: Proposal,
  local: Map<string, LocalCell>,
  checks: Map<string, boolean> = new Map(),
  /** Choices on cells edited in the sheet not saved yet. */
  choices: Map<string, 'sheet' | 'proposal'> = new Map(),
): Proposal {
  if (!local.size && !checks.size && !choices.size) return p
  return {
    ...p,
    changes: p.changes.map(c => {
      const key = rowKey(c)
      // The sheet's value kept, or the proposal's written over it; a value typed there is the proposal's.
      const chosen = [
        ...[...choices].map(([id, use]) => [...id.split('\u0000'), use]),
        ...[...local].filter(([, cell]) => cell.use !== 'sheet').map(([id]) => [...id.split('\u0000'), 'proposal']),
      ].filter(([k, field]) => k === key && c.sheetChanged?.[field])
      if (chosen.length) {
        const edited = { ...c.sheetChanged }
        for (const [, field, use] of chosen) {
          const { again: _, ...rest } = edited[field]
          edited[field] = { ...rest, use: use as 'sheet' | 'proposal' }
        }
        c = { ...c, sheetChanged: edited }
      }
      // Doubtful cells marked checked (or unmarked) and not saved yet.
      let doubts: Record<string, Doubt> | null = null
      for (const [id, checked] of checks) {
        const [k, field] = id.split('\u0000')
        if (k !== key || !c.doubts?.[field]) continue
        doubts ??= { ...c.doubts }
        const { checked: _, ...rest } = doubts[field]
        doubts[field] = checked ? { ...rest, checked: { how: 'table' } } : rest
      }
      if (doubts) c = { ...c, doubts }
      let values: Record<string, CellValue> | null = null
      let marks: Record<string, PersonEdit> | null = null
      for (const [id, cell] of local) {
        const [k, field] = id.split('\u0000')
        if (k !== key) continue
        values ??= { ...c.values }
        marks ??= { ...c.personEdits }
        // What the assistant proposed there stays with the person's mark.
        const mark: PersonEdit = marks[field] ?? (field in c.values ? { ai: c.values[field] } : {})
        // The species the formula gives, typed: left to the formula, as the server will leave it.
        const gives = c.formulaGives?.[field]
        const toFormula = !cell.use && gives !== undefined && gives !== null && gives !== '' && sameAsFormula(cell.value, gives)
        if (cell.use === 'ai') {
          values[field] = cell.value
          delete marks[field]
        } else if (cell.use === 'sheet' || (cell.value === null && c.create) || toFormula) {
          delete values[field]
          if ('ai' in mark) marks[field] = mark
          else delete marks[field]
        } else {
          values[field] = cell.value
          marks[field] = mark
        }
      }
      if (!values) return c
      return { ...c, values, personEdits: Object.keys(marks!).length ? marks! : undefined }
    }),
  }
}

/** Rows to apply: those with something to write (a row whose every cell went back to the sheet is left out). */
export function rowsToWrite(p: Pick<Proposal, 'changes'>): number[] {
  return p.changes.filter(c => writtenFields(c).length && !c.context).map(c => c.index)
}

/** How many of the assistant's values the person set back to the sheet's (kept aside, not written). */
export function notApplied(p: Pick<Proposal, 'changes'>): number {
  let n = 0
  for (const c of p.changes)
    for (const [field, mark] of Object.entries(c.personEdits ?? {})) if ('ai' in mark && !(field in c.values)) n++
  return n
}

/**
 * What the "Valor de la hoja" and "Valor de la IA" buttons can do with the
 * selected cells: how many hold a change that can go back to the sheet's value
 * (the assistant's or the person's), and how many can take the assistant's
 * value again (set back, or typed over by the person).
 */
export function selectionActions(cells: Pick<CellInfo, 'kind' | 'aiProposed' | 'doubtful'>[]) {
  let sheet = 0
  let ai = 0
  // Doubtful cells that «Marcar revisadas» would mark.
  let check = 0
  for (const c of cells) {
    if (c.kind === 'proposed' || c.kind === 'person') sheet++
    if (c.aiProposed && (c.kind === 'reverted' || c.kind === 'person')) ai++
    if (c.doubtful) check++
  }
  return { sheet, ai, check }
}

/** "la IA cambió 3 celdas" */
export function changedText(n: number) {
  return tn(n, 'La IA cambió {n} celda', 'La IA cambió {n} celdas')
}

/**
 * The notice of a page whose lines go another way in the sheet: up to three
 * cases ("línea 7 (A4E) después de la línea 6 (X9C)"), then how many more;
 * with several photos each line says its photo.
 */
export function orderText(notes: OrderNote[], severalPhotos: boolean, shown = 3): string {
  const line = (photo: number, n: number, id: string) => {
    const name = id ? `${n} (${id})` : String(n)
    return severalPhotos ? t('foto {photo}, línea {line}', { photo: photo + 1, line: name }) : t('línea {line}', { line: name })
  }
  const cases = notes
    .slice(0, shown)
    .map(o =>
      t('{line} va después de {after} en el cuaderno, pero antes en la hoja', {
        line: line(o.photo, o.line, o.id),
        after: line(o.photo, o.after.line, o.after.id),
      }),
    )
  const more = notes.length > shown ? ` ${t('y {n} más', { n: notes.length - shown })}` : ''
  return `${t('El orden no es el del cuaderno')}: ${cases.join('; ')}${more}`
}

/** The panel's share of the screen while dragging its divider: kept between 20 % and 80 %. */
export function panelShare(pointer: number, start: number, size: number, fromEnd = true) {
  if (size <= 0) return 40
  const share = ((fromEnd ? start + size - pointer : pointer - start) / size) * 100
  return Math.round(Math.min(80, Math.max(20, share)))
}

/** Identifier columns: typed or pasted, never picked from a (long) list. */
export const ID_COLUMN = /_(ID|id)$|CAM_ID|Tube_\d|FieldMark/
