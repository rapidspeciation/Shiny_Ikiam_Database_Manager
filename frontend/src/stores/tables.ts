import { defineStore } from 'pinia'
import { markRaw } from 'vue'
import { api } from '../lib/api'
import type { Table, TableRow } from '../lib/types'

/** A record as returned by the write endpoints. */
export interface ServerRecord {
  id: string
  sheet: string
  row: number
  version: number
  observed?: boolean
  values: TableRow['values']
  formulas: Record<string, string>
}

export function toRow(record: ServerRecord): TableRow {
  return {
    id: record.id,
    row: record.row,
    version: record.version,
    observed: record.observed ?? true,
    values: record.values,
    formulas: Object.keys(record.formulas || {}),
  }
}

type WireRow = { id: string; row: number; version: number; observed: boolean; v: TableRow['values'][string][]; f: number[] }

/** GET /api/table sends rows as arrays in column order to keep the payload small. */
interface TableWire {
  module: string
  revision: string
  latest?: string
  columns: Table['columns']
  headerProblems: { field: string; found: string | null }[]
  rows: WireRow[]
}

/** GET /api/table/changes: rows changed or removed since an earlier `latest`. */
interface TableDelta {
  revision: string
  latest: string
  count: number
  rows: WireRow[]
  removed: string[]
}

function rowFromWire(keys: string[], r: WireRow): TableRow {
  const values: TableRow['values'] = {}
  keys.forEach((key, i) => {
    // Duplicate header names keep the first column's value.
    if (!(key in values)) values[key] = r.v[i] ?? null
  })
  return { id: r.id, row: r.row, version: r.version, observed: r.observed, values, formulas: r.f.map(i => keys[i]) }
}

function fromWire(wire: TableWire): Table {
  const keys = wire.columns.map(c => c.key)
  return {
    module: wire.module,
    revision: wire.revision,
    latest: wire.latest,
    keys,
    columns: wire.columns.filter((c, i) => keys.indexOf(c.key) === i),
    headerProblems: wire.headerProblems,
    rows: wire.rows.map(r => rowFromWire(keys, r)),
  }
}

const FOLLOW_MS = 10000
let followTimer: ReturnType<typeof setTimeout> | undefined

/**
 * Whole sheets are loaded once and kept in memory; the grid filters and sorts
 * them locally. Tables are marked raw so Vue does not make 13k rows reactive.
 */
export const useTables = defineStore('tables', {
  state: () => ({
    tables: {} as Record<string, Table>,
    loading: {} as Record<string, boolean>,
    /** Bumped when any sheet changes (for views that read several). */
    version: 0,
    /**
     * Bumped when that sheet changes. A grid follows only its own sheet, so
     * loading or refreshing another one does not redraw 13k rows.
     */
    versions: {} as Record<string, number>,
  }),
  actions: {
    changed(module: string) {
      this.versions[module] = (this.versions[module] || 0) + 1
      this.version++
    },
    async load(module: string, force = false): Promise<Table> {
      if (this.tables[module] && !force) return this.tables[module]
      this.loading[module] = true
      try {
        const wire = await api<TableWire>(`table?module=${encodeURIComponent(module)}`)
        const table = fromWire(wire)
        this.tables[module] = markRaw(table)
        this.changed(module)
        return table
      } finally {
        this.loading[module] = false
      }
    },
    /**
     * Brings a loaded sheet up to date with the server's copy (edits from other
     * people or made directly in Google Sheets) by fetching only what changed.
     */
    async refresh(module: string) {
      const table = this.tables[module]
      if (!table?.latest || !table.keys || this.loading[module]) return
      const delta = await api<TableDelta>(
        `table/changes?module=${encodeURIComponent(module)}&since=${encodeURIComponent(table.latest)}`,
      )
      if (delta.revision === table.revision || this.tables[module] !== table) return
      const removed = new Set(delta.removed)
      const rows = table.rows.filter(r => !removed.has(r.id))
      const byId = new Map(rows.map((r, i) => [r.id, i]))
      let moved = false
      for (const wire of delta.rows) {
        const row = rowFromWire(table.keys, wire)
        const index = byId.get(row.id)
        if (index === undefined) {
          rows.push(row)
          moved = true
        } else {
          if (rows[index].row !== row.row) moved = true
          rows[index] = row
        }
      }
      // Rows can also shift without being edited; then only a full reload is exact.
      if (rows.length !== delta.count) return void (await this.load(module, true))
      if (moved) rows.sort((a, b) => a.row - b.row)
      this.tables[module] = markRaw({ ...table, rows, revision: delta.revision, latest: delta.latest })
      this.changed(module)
    },
    /** Keeps every loaded sheet following the server while the page is visible. */
    follow() {
      clearTimeout(followTimer)
      followTimer = setTimeout(async () => {
        if (document.visibilityState === 'visible' && navigator.onLine)
          for (const module of Object.keys(this.tables)) await this.refresh(module).catch(() => {})
        this.follow()
      }, FOLLOW_MS)
    },
    /** Replace or append rows after a verified save. */
    merge(records: ServerRecord[]) {
      const touched = new Set<string>()
      for (const record of records) {
        const table = this.tables[record.sheet]
        if (!table) continue
        const row = toRow(record)
        const index = table.rows.findIndex(r => r.id === row.id)
        if (index >= 0) table.rows[index] = row
        else table.rows.push(row)
        touched.add(record.sheet)
      }
      for (const module of touched) this.changed(module)
    },
    row(module: string, id: string): TableRow | undefined {
      return this.tables[module]?.rows.find(r => r.id === id)
    },
  },
})
