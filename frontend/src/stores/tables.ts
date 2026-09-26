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

/** GET /api/table sends rows as arrays in column order to keep the payload small. */
interface TableWire {
  module: string
  revision: string
  columns: Table['columns']
  headerProblems: { field: string; found: string | null }[]
  rows: { id: string; row: number; version: number; observed: boolean; v: TableRow['values'][string][]; f: number[] }[]
}

function fromWire(wire: TableWire): Table {
  const keys = wire.columns.map(c => c.key)
  return {
    module: wire.module,
    revision: wire.revision,
    columns: wire.columns.filter((c, i) => keys.indexOf(c.key) === i),
    headerProblems: wire.headerProblems,
    rows: wire.rows.map(r => {
      const values: TableRow['values'] = {}
      keys.forEach((key, i) => {
        // Duplicate header names keep the first column's value.
        if (!(key in values)) values[key] = r.v[i] ?? null
      })
      return { id: r.id, row: r.row, version: r.version, observed: r.observed, values, formulas: r.f.map(i => keys[i]) }
    }),
  }
}

/**
 * Whole sheets are loaded once and kept in memory; the grid filters and sorts
 * them locally. Tables are marked raw so Vue does not make 13k rows reactive.
 */
export const useTables = defineStore('tables', {
  state: () => ({
    tables: {} as Record<string, Table>,
    loading: {} as Record<string, boolean>,
    version: 0,
  }),
  actions: {
    async load(module: string, force = false): Promise<Table> {
      if (this.tables[module] && !force) return this.tables[module]
      this.loading[module] = true
      try {
        const wire = await api<TableWire>(`table?module=${encodeURIComponent(module)}`)
        const table = fromWire(wire)
        this.tables[module] = markRaw(table)
        this.version++
        return table
      } finally {
        this.loading[module] = false
      }
    },
    /** Replace or append rows after a verified save. */
    merge(records: ServerRecord[]) {
      for (const record of records) {
        const table = this.tables[record.sheet]
        if (!table) continue
        const row = toRow(record)
        const index = table.rows.findIndex(r => r.id === row.id)
        if (index >= 0) table.rows[index] = row
        else table.rows.push(row)
      }
      this.version++
    },
    row(module: string, id: string): TableRow | undefined {
      return this.tables[module]?.rows.find(r => r.id === id)
    },
  },
})
