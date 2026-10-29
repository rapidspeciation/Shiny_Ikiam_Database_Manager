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
  duplicates: string[]
  unset: string[]
  tubes_read?: number
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
  xlsx?: RackFile
  drive?: RackFile
}

/** What a well shows: the server's states, 'missing' (nobody said what is there), 'left' (its row taken out of the proposal). */
export type CellState = RackWell['s'] | 'missing' | 'left'
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
}

/** The tube a proposal row places: a new row's Tube_ID, an existing row's ID (its label). */
const tubeOf = (c: ProposalChange) => String((c.create ? c.values?.Tube_ID : c.label) ?? '').trim().toUpperCase()

/** The rack's 96 wells (A1..H12) as shown now, with the proposal's rows. */
export function rackCells(rack: DepositRack, changes: ProposalChange[]): Map<string, RackCell> {
  const created = new Set(changes.filter(c => c.create && !c.context).map(tubeOf))
  const edited = new Set(changes.filter(c => !c.create && !c.context).map(c => c.recordId))
  const disputed = new Set(rack.disputed ?? [])
  const out = new Map<string, RackCell>()
  for (const row of ROWS)
    for (const col of COLS) {
      const well = `${row}${col}`
      const w = rack.wells[well]
      let state: CellState = w?.s ?? 'missing'
      if (state === 'new' && w?.t && !created.has(w.t)) state = 'left'
      if (state === 'moved' && w?.r && !edited.has(w.r)) state = 'left'
      out.set(well, {
        well,
        tube: w?.t ?? null,
        state,
        was: w?.was ? (Array.isArray(w.was) ? w.was : [w.was]) : [],
        needsRows: !!w?.n,
        byHand: !!rack.fill?.[well],
        disputed: disputed.has(well),
      })
    }
  return out
}

/** How many wells of each state (the section's legend). */
export function cellCounts(cells: Map<string, RackCell>) {
  const n: Record<CellState, number> = { new: 0, moved: 0, same: 0, check: 0, empty: 0, missing: 0, left: 0 }
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

/** Why the deposit sheet (.xlsx) cannot be made yet, in the person's words (empty: it can). */
export function notReady(deposit: RackDeposit): string[] {
  const out: string[] = []
  if (!deposit.racks.length) out.push(t('ningún rack leído'))
  if (!deposit.responsable?.trim()) out.push(t('falta el responsable'))
  for (const r of deposit.racks) {
    const rack = r.rack_id ?? '?'
    if (!r.rack_id) out.push(t('{rack}: falta el código del rack', { rack }))
    const open = r.missing.length
    const wells = `${r.missing.slice(0, 6).join(', ')}${open > 6 ? '…' : ''}`
    if (open) out.push(tn(open, '{rack}: {n} pocillo sin tubo ({wells})', '{rack}: {n} pocillos sin tubo ({wells})', { rack, wells }))
    for (const p of r.problems) if (!/A1 is not known|code was not read/.test(p)) out.push(`${rack}: ${p}`)
  }
  return out
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
