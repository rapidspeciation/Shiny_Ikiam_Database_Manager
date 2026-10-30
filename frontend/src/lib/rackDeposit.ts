/**
 * A rack deposit at the LBI freezer as «Cambios propuestos» shows it (server/rack-deposit.mjs): one
 * proposal per chat holding every rack read there, each with its wells A1..H12 (Anexo1's grid), the
 * deposit sheet's fields and the tube-locations rows it implies. Each well's state follows the
 * proposal's rows as they are now: a row the person took out of the table is no longer «nuevo».
 */
import type { ProposalChange } from './proposals'
import { t, tn } from './i18n'

export const ROWS = 'ABCDEFGH'.split('')
export const COLS = Array.from({ length: 12 }, (_, k) => k + 1)

export interface RackPlace {
  rack: string | null
  well: string | null
}
/** A well as the server worked it out: tube, state, the sheet's row, where the sheet had it, no row in the workbook (n). */
export interface RackWell {
  t?: string
  s: 'new' | 'moved' | 'same' | 'check' | 'empty'
  r?: string
  was?: RackPlace | RackPlace[]
  n?: 1
}
export interface DepositRack {
  reading: string
  rack_id: string | null
  rack_id_read?: boolean
  code_beside: string | null
  code_options?: string[]
  code_read?: boolean
  assumed?: boolean
  confirmed?: boolean
  checked_by_ids?: boolean
  estanteria?: string
  fila_de_rack?: string
  caja?: string
  fill?: Record<string, string>
  empty?: string[]
  wells: Record<string, RackWell>
  missing: string[]
  disputed?: string[]
  problems: string[]
  counts?: { new: number; moved: number; same: number; check: number; empty: number; missing: number }
  need_rows: string[]
  elsewhere: { tube: string; well: string | null }[]
  /** Wells where the sheet records another tube of this rack (one the photos do not show there). */
  sheet_here?: Record<string, string>
  duplicates: string[]
  unset: string[]
  tubes_read?: number
  /** Wells the person set in the table's rows (a Tube_ID typed, a tube moved; null: moved away), over the reading. */
  set?: Record<string, string | null>
  /**
   * A tube moved in its row onto a well holding another tube: nothing overwritten, both wells flagged, the row
   * keeps the person's well until they resolve it here (swap the two, or keep them as the photos have them).
   */
  conflicts?: RackConflict[]
}
export interface RackConflict {
  tube: string
  from: string
  to: string
  other: string
}
export interface RackFile {
  file: string
  at: string
  download?: string
  link?: string
}
export interface RackDeposit {
  responsable: string
  responsable_default?: boolean
  ultracongelador: string
  batch?: string
  racks: DepositRack[]
  sheet: string | null
  unset: string[]
  protected?: boolean
  /** Tubes whose rows the person took out of the table: they stay out until brought back. */
  left_out?: string[]
  xlsx?: RackFile
  drive?: RackFile
  /** The tube-locations changes as an .xlsx to paste by hand (the last one made, and who made it). */
  changes_xlsx?: RackFile & { by?: string }
  /** Pasted into the sheet by hand («Aplicado a mano»): who and when; the app wrote nothing. */
  manual?: { by: string; at: string; file?: string }
}

/**
 * What a well shows: the server's states, 'missing' (nobody said what is there), 'left' (its row taken out of the
 * proposal), 'off' (its row edited in the table to another rack or well than the photos have), 'conflict' (a row
 * moves its tube onto a well holding another tube: both wells, until the person resolves it).
 */
export type CellState = RackWell['s'] | 'missing' | 'left' | 'off' | 'conflict'
export interface RackCell {
  well: string
  tube: string | null
  state: CellState
  /** Where the sheet had the tube (a moved one), or its rows (a tube on two rows). */
  was: RackPlace[]
  /** No sheet of the workbook has the tube: it needs its rows. */
  needsRows: boolean
  /** Its tube typed by the person (side label), not read. */
  byHand: boolean
  /** The photos disagree on it. */
  disputed: boolean
  /** Where its row in the table puts the tube now, when not here ('off'). */
  rowPlace: RackPlace | null
  /** Another tube the sheet records in this well of this rack. */
  sheetHere: string | null
  /** The conflict this well is part of (either end). */
  conflict: RackConflict | null
}

/** The tube a proposal row places: a new row's Tube_ID, an existing row's ID (its label). */
const tubeOf = (c: ProposalChange) => String((c.create ? c.values?.Tube_ID : c.label) ?? '').trim().toUpperCase()

/** The rack's 96 wells (A1..H12) as shown now, with the proposal's rows. */
export function rackCells(rack: DepositRack, changes: ProposalChange[]): Map<string, RackCell> {
  const created = new Map(changes.filter(c => c.create && !c.context).map(c => [tubeOf(c), c]))
  const edited = new Map(changes.filter(c => !c.create && !c.context).map(c => [c.recordId, c]))
  // Where a row puts its tube now (the table's value, the person's edits included).
  const text = (v: unknown) => String(v ?? '').trim().toUpperCase()
  // A well as the server tidies it (A01, a 1 → A1).
  const wellOf = (v: unknown) => {
    const m = /^([A-H])\s*0?(\d{1,2})$/.exec(text(v))
    return m ? `${m[1]}${Number(m[2])}` : text(v)
  }
  const placeOf = (c: ProposalChange): RackPlace => ({
    rack: text(c.values?.Rack_ID ?? c.current?.Rack_ID ?? c.rowValues?.Rack_ID) || null,
    well: wellOf(c.values?.Tube_location ?? c.current?.Tube_location ?? c.rowValues?.Tube_location) || null,
  })
  const disputed = new Set(rack.disputed ?? [])
  const conflictAt = new Map<string, RackConflict>()
  for (const c of rack.conflicts ?? []) for (const w of [c.from, c.to]) conflictAt.set(w, c)
  const out = new Map<string, RackCell>()
  for (const row of ROWS)
    for (const col of COLS) {
      const well = `${row}${col}`
      const w = rack.wells[well]
      let state: CellState = w?.s ?? 'missing'
      const line = state === 'new' && w?.t ? created.get(w.t) : state === 'moved' && w?.r ? edited.get(w.r) : undefined
      let rowPlace: RackPlace | null = null
      const conflict = conflictAt.get(well) ?? null
      if (conflict) state = 'conflict'
      else if ((state === 'new' || state === 'moved') && !line) state = 'left'
      else if (line) {
        const at = placeOf(line)
        if (at.rack !== text(rack.rack_id) || at.well !== well) {
          state = 'off'
          rowPlace = at
        }
      }
      out.set(well, {
        well,
        tube: w?.t ?? null,
        state,
        was: w?.was ? (Array.isArray(w.was) ? w.was : [w.was]) : [],
        needsRows: !!w?.n,
        byHand: !!rack.fill?.[well],
        disputed: disputed.has(well),
        rowPlace,
        sheetHere: rack.sheet_here?.[well] ?? null,
        conflict,
      })
    }
  return out
}

/** How many wells of each state (the section's legend). */
export function cellCounts(cells: Map<string, RackCell>) {
  const n: Record<CellState, number> = { new: 0, moved: 0, same: 0, check: 0, empty: 0, missing: 0, left: 0, off: 0, conflict: 0 }
  for (const c of cells.values()) n[c.state]++
  return n
}

/** The rack of the deposit a proposal row belongs to (its tube among the rack's wells), or null. */
export function rackOfChange(deposit: RackDeposit, change: ProposalChange): string | null {
  const tube = tubeOf(change)
  for (const r of deposit.racks)
    for (const w of Object.values(r.wells))
      if ((change.create ? w.t === tube : w.r === change.recordId) && w.t) return r.reading
  return null
}

/** A rack's wells not settled yet, in the person's words: no tube said, a problem, a conflict. */
function wellsNotReady(r: DepositRack): string[] {
  const rack = r.rack_id ?? '?'
  const out: string[] = []
  const open = r.missing.length
  const wells = `${r.missing.slice(0, 6).join(', ')}${open > 6 ? '…' : ''}`
  if (open) out.push(tn(open, '{rack}: {n} pocillo sin tubo ({wells})', '{rack}: {n} pocillos sin tubo ({wells})', { rack, wells }))
  for (const p of r.problems) if (!/code was not read/.test(p)) out.push(`${rack}: ${problemText(p)}`)
  for (const c of r.conflicts ?? []) out.push(`${rack}: ${conflictText(c)}`)
  return out
}

/** Why the deposit sheet (.xlsx) cannot be made yet, in the person's words (empty: it can). */
export function notReady(deposit: RackDeposit): string[] {
  const out: string[] = []
  if (!deposit.racks.length) out.push(t('ningún rack leído'))
  if (!deposit.responsable?.trim()) out.push(t('falta el responsable'))
  if (!deposit.ultracongelador?.trim()) out.push(t('falta el ultracongelador'))
  for (const r of deposit.racks) {
    const rack = r.rack_id ?? '?'
    if (!r.rack_id) out.push(t('{rack}: falta el código del rack', { rack }))
    const unset = (['estanteria', 'fila_de_rack', 'caja'] as const).filter(f => !String(r[f] ?? '').trim())
    if (unset.length) out.push(t('{rack}: falta {fields}', { rack, fields: unset.map(f => t(FIELD_LABELS[f])).join(', ') }))
    out.push(...wellsNotReady(r))
  }
  return out
}

/**
 * Why the tube-locations changes (.xlsx to paste by hand) cannot be made yet (empty: they can): the deposit sheet's
 * rule for the wells (every well read, filled or said empty; no conflict, no problem, the rack code known), without
 * its own fields (shelf, rack row, box: the tube-locations sheet has none); and rows to paste at all.
 */
export function changesNotReady(deposit: RackDeposit, changes: ProposalChange[]): string[] {
  const out: string[] = []
  if (!deposit.racks.length) out.push(t('ningún rack leído'))
  for (const r of deposit.racks) {
    if (!r.rack_id) out.push(t('{rack}: falta el código del rack', { rack: '?' }))
    out.push(...wellsNotReady(r))
  }
  if (!out.length && !changes.some(c => !c.context && !c.applied && c.sheet === deposit.sheet)) out.push(t('no cambia nada en la hoja de tubos'))
  return out
}

/** A conflict in the person's words. */
export const conflictText = (c: RackConflict) =>
  t('{tube} va a {to} según su fila, pero {to} tiene {other}: resuélvelo en la cuadrícula', { tube: c.tube, to: c.to, other: c.other })

/**
 * The tube-locations columns the rack decides: read-only in its rows, with where they change instead. Tube_ID and
 * Tube_location stay editable (they go into the grid).
 */
export const RACK_LOCKED: Record<string, string> = {
  Rack_ID: 'Se cambia en los datos del rack (el campo «Rack» junto a su cuadrícula)',
  Sort: 'Automático: se numera solo',
}

/** A tube ID as the grid shows it: its series and the digits the person compares, apart. */
export function splitTube(id: string | null): [string, string] {
  if (!id) return ['', '']
  return id.length > 4 ? [id.slice(0, -4), id.slice(-4)] : ['', id]
}

/** A field left empty (or assumed) the person still has to give: amber in the section. */
export const FIELD_LABELS: Record<string, string> = {
  estanteria: 'Estantería',
  fila_de_rack: 'Fila de rack',
  caja: 'Nro. Caja',
}

/**
 * A rack's problem as the server says it to the assistant (English), in the person's words for the common ones;
 * others as they are.
 */
export function problemText(p: string): string {
  let m: RegExpExecArray | null
  if (/code was not read/.test(p)) return t('Falta el código del rack: escríbelo en «Rack»')
  if ((m = /^(\S+) is on (.+)\.$/.exec(p))) return t('{tube} está en varios racks: {racks}', { tube: m[1], racks: m[2].replaceAll(' and ', ', ') })
  if ((m = /^(.+) on two wells\.$/.exec(p))) return t('En dos pocillos: {tubes}', { tubes: m[1] })
  if ((m = /^(\w+) was read as (\S+), not (\S+)\.$/.exec(p))) return t('{well}: las fotos leyeron {read}, no {given}', { well: m[1], read: m[2], given: m[3] })
  if ((m = /^In these photos the rack code can only be beside (\S+) or (\S+), not (\S+)\.$/.exec(p)))
    return t('En estas fotos el código solo puede estar junto a {a} o {b}, no {c}', { a: m[1], b: m[2], c: m[3] })
  if (/^A1 is not known/.test(p)) return t('No se sabe dónde está A1: dile al Asistente qué tubo está en A1 (mirando el rack desde arriba)')
  if ((m = /^(\S+) is not a rack code/.exec(p))) return t('{code} no es un código de rack (FK…, FH…, FE…, SA… y 8 cifras)', { code: m[1] })
  if (/reading is gone/.test(p)) return t('Su lectura ya no está: vuelve a leer las fotos')
  return p
}
