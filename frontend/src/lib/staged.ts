import { t, tn } from './i18n'
import type { CellValue, Table, TableRow } from './types'

/**
 * Emergidos and Clutches entries kept in the app until someone presses
 * «Guardar en Google Sheets» (server/staged.mjs): everyone's, seen by everyone.
 * Those two tabs show the sheet with them on top (overlayTable), marked as not
 * yet in Google Sheets, with who entered them; every other tab shows the sheet.
 */
export interface StagedItem {
  id: string
  /** One press of a tab's Save: undone together. */
  entryId: string
  purpose: 'emergidos' | 'clutches' | 'censo'
  /** A new row, or cells of a sheet row. */
  kind: 'create' | 'edit'
  sheet: string
  recordId: string | null
  clientId: string | null
  /** The row as these tabs show it: the record, or staged:<clientId> for a new row. */
  rowId: string
  label: string | null
  /** A count kept as a sum is { formula: '=12+15' }. */
  values: Record<string, StagedValue>
  /** Per cell, what the sheet holds under the entries (what the save checks). */
  base?: Record<string, StagedValue>
  actor: string
  actorName: string
  editedByName?: string
  createdAt: string
  updatedAt: string
  /** Being written to Google Sheets now (or waiting for it): not changed or undone meanwhile. */
  status: 'staged' | 'sent'
  /** Why the last save left it out (a cell changed in the sheet…), to correct or undo. */
  error?: { code: string; message: string; field?: string } | null
}
export type StagedValue = CellValue | { formula: string }

/** An identifier held by an entry: nobody else is offered it. */
export interface StagedClaim {
  kind: 'insectary' | 'cam' | 'tube' | 'clutch'
  value: string
  /** The entry holding it, or null for a card's hold (server/holds.mjs). */
  itemId: string | null
  /** The card of Emergidos holding this Insectary ID from its tap (its key). */
  hold?: string
  actor: string
  actorName: string
}

/** What a cell shows: a sum by its total (=12+15 → 27). */
export function shownValue(value: StagedValue | undefined): CellValue {
  if (value && typeof value === 'object') return formulaTotal(value.formula)
  return value ?? null
}
export const formulaTotal = (formula: string): number =>
  (String(formula).match(/[+-]?\s*\d+(?:\.\d+)?/g) ?? []).reduce((n, t) => n + Number(t.replace(/\s+/g, '')), 0)
/** A value as the sum editors read it: the formula's text for a sum. */
export const sumText = (value: StagedValue | undefined): CellValue => (value && typeof value === 'object' ? value.formula : (value ?? null))

/** Per row id, the cells entries changed (or every cell of a new row), who and whether it is being written. */
export interface StagedMark {
  fields: string[]
  who: string[]
  create: boolean
  sent: boolean
  error: string | null
  items: string[]
}

/**
 * A sheet with the entries on top: an edit's values over its row; a new
 * Insectary_data butterfly over the empty pre-made row of its Insectary ID
 * (the row it will be written in), other new rows after the sheet's last row.
 * Those rows get the id staged:<clientId>, so a change to them changes the
 * entry. Returns the table (a new object, or the same when there are none),
 * the marks per row and, per row, the sums (=12+15) the clutch editors read.
 */
export function overlayTable(
  table: Table | undefined,
  items: StagedItem[],
): { table: Table | undefined; marks: Record<string, StagedMark>; sums: Record<string, Record<string, string>> } {
  const marks: Record<string, StagedMark> = {}
  const sums: Record<string, Record<string, string>> = {}
  if (!table) return { table, marks, sums }
  const mine = items.filter(i => i.sheet === table.module)
  if (!mine.length) return { table, marks, sums }
  const mark = (rowId: string, item: StagedItem, fields: string[]) => {
    const m = (marks[rowId] ??= { fields: [], who: [], create: item.kind === 'create', sent: false, error: null, items: [] })
    for (const f of fields) if (!m.fields.includes(f)) m.fields.push(f)
    const who = item.editedByName && item.editedByName !== item.actorName ? [item.actorName, item.editedByName] : [item.actorName]
    for (const w of who) if (!m.who.includes(w)) m.who.push(w)
    m.sent ||= item.status === 'sent'
    m.error ||= item.error?.message ?? null
    m.items.push(item.id)
  }
  const apply = (row: TableRow, values: Record<string, StagedValue>): TableRow => {
    const out = { ...row.values }
    for (const [field, value] of Object.entries(values)) {
      out[field] = shownValue(value)
      if (value && typeof value === 'object') (sums[row.id] ??= {})[field] = value.formula
    }
    return { ...row, values: out }
  }
  const rows = [...table.rows]
  const index = new Map(rows.map((r, i) => [r.id, i]))
  const premade = new Map<string, number>()
  if (table.module === 'Insectary_data')
    rows.forEach((r, i) => {
      if (!r.observed && r.values.Insectary_ID) premade.set(String(r.values.Insectary_ID).trim().toUpperCase(), i)
    })
  let next = rows.reduce((max, r) => Math.max(max, r.row), 0)
  for (const item of mine) {
    if (item.kind === 'edit') {
      const i = item.recordId ? index.get(item.recordId) : undefined
      if (i === undefined) continue
      rows[i] = apply(rows[i], item.values)
      mark(rows[i].id, item, Object.keys(item.values))
      continue
    }
    const id = String(item.values.Insectary_ID ?? '').trim().toUpperCase()
    const at = id ? premade.get(id) : undefined
    const fields = Object.keys(item.values)
    if (at !== undefined) {
      // Its pre-made row: written there, so shown there (with the row's formulas).
      const base = rows[at]
      const formulas = base.formulas.filter(f => !(f in item.values) || f === 'Insectary_ID')
      rows[at] = { ...apply({ ...base, id: item.rowId }, item.values), observed: true, formulas, version: 0 }
      premade.delete(id)
      mark(item.rowId, item, fields)
      continue
    }
    const row: TableRow = { id: item.rowId, row: ++next, version: 0, observed: true, values: {}, formulas: [] }
    rows.push(apply(row, item.values))
    mark(item.rowId, item, fields)
  }
  return { table: { ...table, rows }, marks, sums }
}

/** The sums of the sheet's counts (clutchState), with the entries' sums on top. */
export function overlaySums(base: Record<string, Record<string, string>>, staged: Record<string, Record<string, string>>) {
  if (!Object.keys(staged).length) return base
  const out = { ...base }
  for (const [rowId, fields] of Object.entries(staged)) out[rowId] = { ...(base[rowId] ?? {}), ...fields }
  return out
}

/** «Guardar en Google Sheets» shows what it writes: per sheet, each row (its label) with its cells and who. */
export interface SummaryRow {
  sheet: string
  label: string
  isNew: boolean
  cells: { field: string; value: string }[]
  who: string[]
  items: StagedItem[]
  error: string | null
}
export function stagedSummary(items: StagedItem[], status: 'staged' | 'sent' = 'staged'): { sheet: string; rows: SummaryRow[] }[] {
  const bySheet = new Map<string, Map<string, SummaryRow>>()
  for (const item of items.filter(i => i.status === status)) {
    const rows = bySheet.get(item.sheet) ?? bySheet.set(item.sheet, new Map()).get(item.sheet)!
    const row = rows.get(item.rowId) ?? {
      sheet: item.sheet,
      label: item.label || String(item.values.Insectary_ID ?? item.values['CLUTCH NUMBER'] ?? ''),
      isNew: item.kind === 'create',
      cells: [],
      who: [],
      items: [],
      error: null,
    }
    for (const [field, value] of Object.entries(item.values)) {
      const text = value === null || value === '' ? '—' : String(sumText(value))
      const at = row.cells.findIndex(c => c.field === field)
      if (at >= 0) row.cells[at] = { field, value: text }
      else row.cells.push({ field, value: text })
    }
    if (!row.who.includes(item.actorName)) row.who.push(item.actorName)
    row.items.push(item)
    row.error ||= item.error?.message ?? null
    rows.set(item.rowId, row)
  }
  return [...bySheet].map(([sheet, rows]) => ({ sheet, rows: [...rows.values()] }))
}

/** Rows changed (a new row, or a sheet row however many times it was edited): a census of 221 butterflies is 221, not its cells. */
export function changeCount(items: StagedItem[], status: 'staged' | 'sent' = 'staged') {
  // Rows, not entries: two changes to the same row (a count changed twice) are one row to save.
  return new Set(items.filter(i => i.status === status).map(i => i.recordId ?? i.clientId ?? i.id)).size
}

/** Who holds an identifier in an entry (A4E — Ana), or null. */
export function claimHolder(claims: StagedClaim[], kind: StagedClaim['kind'], value: string): StagedClaim | null {
  const v = String(value ?? '').trim().toUpperCase()
  return (v && claims.find(c => c.kind === kind && c.value === v)) || null
}

/**
 * The banner everyone sees while Google does not answer normally, or while saves
 * kept in the app are being written: { kind, text }, or null when all is normal.
 */
export function googleNotice(state: 'ok' | 'slow' | 'busy', waiting: number): { kind: 'busy' | 'slow' | 'writing'; text: string } | null {
  if (state === 'busy')
    return {
      kind: 'busy',
      text: waiting
        ? tn(
            waiting,
            'Google Sheets no responde (está recalculando la hoja): los guardados se conservan aquí y se escriben cuando responda; {n} esperando',
            'Google Sheets no responde (está recalculando la hoja): los guardados se conservan aquí y se escriben cuando responda; {n} esperando',
          )
        : t('Google Sheets no responde (está recalculando la hoja): los guardados se conservan aquí y se escriben cuando responda'),
    }
  if (state === 'slow')
    return {
      kind: 'slow',
      text: waiting
        ? tn(
            waiting,
            'Google Sheets responde lento (está recalculando la hoja): los guardados se conservan aquí y se escriben en orden; {n} esperando',
            'Google Sheets responde lento (está recalculando la hoja): los guardados se conservan aquí y se escriben en orden; {n} esperando',
          )
        : t('Google Sheets responde lento (está recalculando la hoja): los guardados se conservan aquí y se escriben en orden'),
    }
  if (waiting)
    return {
      kind: 'writing',
      text: tn(waiting, 'Escribiendo en Google Sheets {n} guardado que esperaba', 'Escribiendo en Google Sheets {n} guardados que esperaban'),
    }
  return null
}

/** Where a used CAM or tube is (GET /api/ids?check=…): a sheet row, or someone's entry kept in the app. */
export interface UsedHolder {
  sheet: string | null
  row: number | null
  label: string | null
  claimedBy?: string
}
export function usedWhere(h: UsedHolder): string {
  if (h.claimedBy) return t('{name}, en la app (aún no en Google Sheets)', { name: h.claimedBy })
  return t('{sheet} fila {row}{label}', { sheet: h.sheet ?? '', row: h.row ?? '', label: h.label ? ` (${h.label})` : '' })
}
