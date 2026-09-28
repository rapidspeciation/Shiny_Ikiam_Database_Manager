export type CellValue = string | number | boolean | null

export interface Field {
  key: string
  label: string
  type: 'text' | 'number' | 'date'
  readonly?: boolean
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
  sandbox: boolean
  sandboxLabel: string
  sheetUrl: string
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
  /** Columns whose header no longer matches the live Sheet; saving there is blocked. */
  headerProblems: { field: string; found: string | null }[]
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

export interface ApiErrorBody {
  code: string
  message: string
  details?: unknown
}
