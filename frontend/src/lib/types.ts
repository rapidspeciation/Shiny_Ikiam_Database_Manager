import type { Msg } from './i18n'

export type CellValue = string | number | boolean | null

export interface Field {
  key: string
  label: string
  type: 'text' | 'number' | 'date'
  readonly?: boolean
  /** Its column is missing from the live sheet: last known values, read-only. */
  unavailable?: boolean
}

/**
 * A difference between a sheet's live header and the fields the app knows:
 * a known column missing, a new column (ignored), a header written twice, or a
 * header row the app cannot read. `blocking` ones stop reads and saves of the sheet.
 */
export interface HeaderProblem {
  kind: 'missing' | 'new' | 'duplicate' | 'header'
  field: string | null
  column?: string
  columns?: string[]
  blocking?: boolean
}

export interface Module {
  id: string
  label: string
  group: string
  sheet: string
  sheetId: number
  headerRow: number
  fields: Field[]
  identityFields: string[]
  recordCount: number
}

export interface User {
  id: string
  username: string
  displayName: string
  role: 'observer' | 'editor' | 'reviewer' | 'admin'
  active: boolean
  email?: string | null
}

export interface Settings {
  language: string
  /** The team's workbook; null in the offline lab copy (LOCAL_MODE), which has no sheet. */
  sheetUrl: string | null
  localMode?: boolean
  basePath: string
}

export interface SyncStatus {
  state: string
  lastSync?: string | null
  error?: string | null
}

/** One sheet row as delivered by GET /api/table. */
export interface TableRow {
  id: string
  row: number
  version: number
  observed: boolean
  values: Record<string, CellValue>
  /** Keys of cells that contain a formula (read-only in the app). */
  formulas: string[]
}

export interface Table {
  module: string
  revision: string
  columns: Field[]
  rows: TableRow[]
  /** How the live header differs from the fields the app knows (server/columns.mjs). */
  headerProblems: HeaderProblem[]
  /** Newest change in the server's copy, to ask for later changes only. */
  latest?: string
  /** Column keys of the wire rows, in sheet order (may repeat). */
  keys?: string[]
}

export interface Change {
  id: string
  recordId: string
  sheet: string
  row: number
  field: string
  before: CellValue | { formula: string }
  after: CellValue | { formula: string }
  label?: string
}

export interface Action {
  id: string
  actor: string
  actorName?: string
  source: string
  createdAt: string
  status: string
  reason: string | null
  reverses: string | null
  reversedBy?: string | null
  changes: Change[]
}

export interface HistoryChange extends Change {
  /** The save made this row (a new row). */
  isNew?: boolean
  /** Already put back by a later undo. */
  undone?: boolean
}

/** One save inside a group of the Historial (GET history/groups/:id). */
export interface HistoryAction {
  id: string
  createdAt: string
  source: string
  status: string
  reason: string | null
  reverses: string | null
  reversedBy: string | null
  undoable: boolean
  changes: HistoryChange[]
}

/** Saves by one person with one purpose close in time (server/history.mjs). */
export interface HistoryGroup {
  id: string
  purpose: string
  purposeLabel: string
  actor: string
  actorName: string | null
  start: string
  end: string
  counts: { actions: number; rows: number; newRows: number; cells: number }
  sheets: string[]
  fields: string[]
  labels: string[]
  summary: string
  /** The summary's descriptor, for the interface language (server/messages.mjs). */
  summaryMsg?: Msg
  reasons: string[]
  statuses: Record<string, number>
  undone: 'all' | 'some' | null
  undoable: boolean
  actionIds: string[]
  matched?: string[]
  link: string
  /** Only in the detail of a group. */
  actions?: HistoryAction[]
}

/** What an undo would write: each cell's value now (before) and the value it goes back to (after). */
export interface UndoPreviewItem {
  recordId: string
  field: string
  before: Change['before']
  after: Change['after']
  label?: string | null
  sheet?: string | null
  row?: number | null
  reason?: string
}

export interface UndoPreview {
  changes: UndoPreviewItem[]
  conflicts: UndoPreviewItem[]
  eligible: boolean
  selection: { actionIds: string[]; changeIds: string[] }
}

export interface ApiErrorBody {
  code: string
  message: string
  /** The message's descriptor when it has values in it (server/messages.mjs). */
  messageMsg?: Msg
  details?: unknown
}
