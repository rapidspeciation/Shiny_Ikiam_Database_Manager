import type { CellValue } from './types'
import { t, tn, type Msg } from './i18n'

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
/** The notebook page a proposal was read from: its notebook, the sheet, its columns in the page's order, how many photos. */
export interface ProposalPage {
  kind: string
  sheet: string
  columns: string[]
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
  /** A notebook line shown only for context: never written. */
  context?: boolean
  /** A page line with no sheet row (not found, crossed out): shown as written, never written. */
  placeholder?: boolean
  /** What a formula column will give once the row is written (SPECIES from its clutch): shown, never written. */
  formulaGives?: Record<string, CellValue>
  /** Its place on the notebook page. */
  page?: PageLine
}
export interface Proposal {
  id: string
  reason: string
  status: 'pending' | 'applying' | 'applied' | 'needs_review' | 'discarded'
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
 * could not read it and nobody filled it yet: not written).
 */
export type CellKind = 'proposed' | 'person' | 'reverted' | 'sheet' | 'empty' | 'locked' | 'unreadable'
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
  /** The value is what the sheet's formula will give (SPECIES from the clutch): shown grey, never written. */
  fromFormula?: boolean
}
export function cellOf(change: ProposalChange, field: string, newRowFormulas: string[] = []): CellInfo {
  const mark = change.personEdits?.[field]
  const was = change.create ? undefined : (change.current?.[field] ?? change.rowValues?.[field] ?? null)
  const ai = mark && 'ai' in mark ? mark.ai : undefined
  const aiProposed = !!mark && 'ai' in mark
  const doubt = change.doubts?.[field]
  const unreadable = change.unreadable?.[field]
  const extra = { doubt, hint: change.hints?.[field], ...(unreadable ? { unreadable } : {}) }
  if (field in change.values) {
    const kind: CellKind = mark ? 'person' : 'proposed'
    return {
      value: change.values[field],
      kind,
      was,
      ai,
      aiProposed,
      ...extra,
      doubtful: kind === 'proposed' && !!doubt && !doubt.checked,
      inferred: kind === 'proposed' && !!change.inferred?.includes(field),
    }
  }
  const quiet = { ...extra, doubtful: false, inferred: false }
  // Nobody could read it and nobody filled it: shown empty (or with the sheet's value), never written.
  if (unreadable && !mark) return { value: change.create ? null : (was ?? null), kind: 'unreadable', was, aiProposed, ...quiet }
  if (mark) return { value: change.create ? null : (was ?? null), kind: aiProposed ? 'reverted' : 'person', was, ai, aiProposed, ...quiet }
  const locked = change.create ? newRowFormulas.includes(field) : !!change.formulas?.includes(field)
  // What the formula will give once the row is written, in place of the sheet's (blank or older) value.
  const gives = change.formulaGives?.[field]
  const formula = gives !== undefined && gives !== null && gives !== '' ? { value: gives, fromFormula: true } : null
  if (change.create) return { value: null, kind: locked ? 'locked' : 'empty', aiProposed, ...quiet, ...formula }
  return { value: was ?? null, kind: locked ? 'locked' : 'sheet', was, aiProposed, ...quiet, ...formula }
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
    for (const [field, doubt] of Object.entries(c.doubts ?? {}))
      if (field in c.values && !doubt.checked && !c.personEdits?.[field]) out.push({ key: rowKey(c), field, index: c.index })
  }
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
 * columns): the columns it changes, in the sheet's order when known, then the
 * columns the person added. A notebook page's sheet follows the notebook: its
 * columns in the page's order, then the implied ones, then the rest; columns
 * holding only a template's NA / NOT_COLLECTED go last, listed in `template`
 * (the table can fold them).
 */
export function sheetGroups(
  p: Pick<Proposal, 'changes' | 'fields' | 'page'>,
  extra: Record<string, string[]> = {},
  order: (sheet: string) => string[] | undefined = () => undefined,
) {
  const sheets = [...new Set(p.changes.map(c => c.sheet))]
  return sheets.map(sheet => {
    const changes = p.changes.filter(c => c.sheet === sheet)
    const used = new Set(
      changes.flatMap(c => [...Object.keys(c.values), ...Object.keys(c.personEdits ?? {}), ...Object.keys(c.unreadable ?? {})]),
    )
    const columns = order(sheet)
    const sorted = columns ? columns.filter(f => used.has(f)) : p.fields.filter(f => used.has(f))
    // Columns the sheet no longer lists still show.
    for (const f of used) if (!sorted.includes(f)) sorted.push(f)
    const added = (extra[sheet] ?? []).filter(f => !sorted.includes(f))
    if (p.page?.sheet !== sheet) return { sheet, fields: [...sorted, ...added], changes, template: [] as string[] }
    const notebook = p.page.columns.filter(f => used.has(f))
    const template = sorted.filter(
      f =>
        !notebook.includes(f) &&
        changes.every(c => {
          if (c.personEdits?.[f] || c.unreadable?.[f] || c.doubts?.[f]) return false
          const v = c.values[f]
          return v === undefined || (typeof v === 'string' && TEMPLATE_VALUE.has(v))
        }),
    )
    const implied = new Set(changes.flatMap(c => c.inferred ?? []))
    const rest = sorted.filter(f => !notebook.includes(f) && !template.includes(f))
    const fields = [...notebook, ...rest.filter(f => implied.has(f)), ...rest.filter(f => !implied.has(f)), ...added, ...template]
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
export function withLocal(p: Proposal, local: Map<string, LocalCell>, checks: Map<string, boolean> = new Map()): Proposal {
  if (!local.size && !checks.size) return p
  return {
    ...p,
    changes: p.changes.map(c => {
      const key = rowKey(c)
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
        if (cell.use === 'ai') {
          values[field] = cell.value
          delete marks[field]
        } else if (cell.use === 'sheet' || (cell.value === null && c.create)) {
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
  return p.changes.filter(c => Object.keys(c.values).length && !c.context).map(c => c.index)
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

/** The panel's share of the screen while dragging its divider: kept between 20 % and 80 %. */
export function panelShare(pointer: number, start: number, size: number, fromEnd = true) {
  if (size <= 0) return 40
  const share = ((fromEnd ? start + size - pointer : pointer - start) / size) * 100
  return Math.round(Math.min(80, Math.max(20, share)))
}

/** Identifier columns: typed or pasted, never picked from a (long) list. */
export const ID_COLUMN = /_(ID|id)$|CAM_ID|Tube_\d|FieldMark/
