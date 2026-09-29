import type { CellValue } from './types'
import { tn } from './i18n'

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
  before: Record<string, CellValue>
  values: Record<string, CellValue>
  /** The sheet's values of the changed columns (existing rows). */
  current: Record<string, CellValue>
  /** The rest of an existing row (pending proposals), for columns added to the table. */
  rowValues?: Record<string, CellValue>
  /** Formula columns of an existing row: not editable. */
  formulas?: string[]
  replaceFormula?: string[]
  /** Cells the person typed in the table. */
  personEdits?: Record<string, PersonEdit>
  note?: string
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
  applied: number[] | null
  changes: ProposalChange[]
}

export const rowKey = (c: Pick<ProposalChange, 'key' | 'clientId' | 'recordId' | 'index'>) =>
  c.key ?? c.clientId ?? c.recordId ?? `i${c.index}`
export const cellId = (key: string, field: string) => `${key}\u0000${field}`
const same = (a: CellValue | undefined, b: CellValue | undefined) => JSON.stringify(a ?? null) === JSON.stringify(b ?? null)

/**
 * How a cell of the table looks: `proposed` (green, the assistant's), `person`
 * (typed by the person), `sheet` (an existing row's value, unchanged), `empty`
 * (a new row's cell with nothing yet), `locked` (a formula).
 */
export type CellKind = 'proposed' | 'person' | 'sheet' | 'empty' | 'locked'
export function cellOf(
  change: ProposalChange,
  field: string,
  newRowFormulas: string[] = [],
): { value: CellValue; kind: CellKind; was?: CellValue; ai?: CellValue; aiProposed: boolean } {
  const mark = change.personEdits?.[field]
  const was = change.create ? undefined : (change.current[field] ?? change.rowValues?.[field] ?? null)
  const ai = mark && 'ai' in mark ? mark.ai : undefined
  const aiProposed = !!mark && 'ai' in mark
  if (field in change.values) return { value: change.values[field], kind: mark ? 'person' : 'proposed', was, ai, aiProposed }
  if (mark) return { value: change.create ? null : (was ?? null), kind: 'person', was, ai, aiProposed }
  const locked = change.create ? newRowFormulas.includes(field) : !!change.formulas?.includes(field)
  if (change.create) return { value: null, kind: locked ? 'locked' : 'empty', aiProposed }
  return { value: was ?? null, kind: locked ? 'locked' : 'sheet', was, aiProposed }
}

/**
 * The proposal's rows split by sheet (one table each, with that sheet's
 * columns): the columns it changes, in the sheet's order when known, then the
 * columns the person added.
 */
export function sheetGroups(
  p: Pick<Proposal, 'changes' | 'fields'>,
  extra: Record<string, string[]> = {},
  order: (sheet: string) => string[] | undefined = () => undefined,
) {
  const sheets = [...new Set(p.changes.map(c => c.sheet))]
  return sheets.map(sheet => {
    const changes = p.changes.filter(c => c.sheet === sheet)
    const used = new Set(changes.flatMap(c => [...Object.keys(c.values), ...Object.keys(c.personEdits ?? {})]))
    const columns = order(sheet)
    const fields = columns ? columns.filter(f => used.has(f)) : p.fields.filter(f => used.has(f))
    // Columns the sheet no longer lists still show.
    for (const f of used) if (!fields.includes(f)) fields.push(f)
    for (const f of extra[sheet] ?? []) if (!fields.includes(f)) fields.push(f)
    return { sheet, fields, changes }
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
 * The person's edits not yet saved, laid over the server's copy: a revision
 * arriving from the assistant meanwhile must not undo what was just typed.
 */
export function withLocal(p: Proposal, local: Map<string, CellValue>): Proposal {
  if (!local.size) return p
  return {
    ...p,
    changes: p.changes.map(c => {
      const key = rowKey(c)
      let values: Record<string, CellValue> | null = null
      let marks: Record<string, PersonEdit> | null = null
      for (const [id, value] of local) {
        const [k, field] = id.split('\u0000')
        if (k !== key) continue
        values ??= { ...c.values }
        marks ??= { ...c.personEdits }
        if (value === null && c.create) delete values[field]
        else values[field] = value
        marks[field] = marks[field] ?? (field in c.values ? { ai: c.values[field] } : {})
      }
      return values ? { ...c, values, personEdits: marks! } : c
    }),
  }
}

/** Rows to apply: ticked (ticks are kept per row key, so they survive revisions) and with something to write. */
export function chosenIndexes(p: Pick<Proposal, 'changes'>, unticked: Set<string>): number[] {
  return p.changes.filter(c => !unticked.has(rowKey(c)) && Object.keys(c.values).length).map(c => c.index)
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
