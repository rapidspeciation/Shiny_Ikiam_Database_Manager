import type { OrderNote, ProposalChange } from './proposals'
import type { RowOrder } from './proposalColumns'
import { t, tn } from './i18n'
import type { CellValue } from './types'

/**
 * The rows of a proposal's table as laid out, between the sheet and the
 * notebook page (Cambios propuestos):
 * - in the sheet's order (as the server sends them), a slim row where two rows
 *   are not next to each other in the sheet: how many rows are not shown
 *   between them, and which;
 * - in the notebook's order (a page's table), the photo's lines one after the
 *   other, and where the next line's row is not the row after the previous
 *   one, how far it goes down or back in the sheet. A row with no line (added
 *   by hand) goes after the line of its ID (A3E.1 after the line read A3E);
 *   the others under «Not on the photo», at the end.
 * Nothing is added while the rows go one after the other.
 */

export type { RowOrder }

const idKey = (v: unknown) => String(v ?? '').replace(/\s+/g, '').toUpperCase()
/** A0E.1 → A0E: an Insectary ID written as a repeat (the same ID on a second butterfly). Null for any other. */
export function baseOf(id: unknown): string | null {
  const m = /^([A-Z0-9]+)\.[1-9]\d*$/.exec(idKey(id))
  return m ? m[1] : null
}
/** The row's ID: its label, or a new row's Insectary ID. */
const idOf = (c: ProposalChange) => c.label || String(c.values?.Insectary_ID ?? '')
/** Where a row stands in the sheet: its row, or where it will be written (x.5: below row x); null when unknown. */
export const sheetAt = (c: ProposalChange): number | null => c.row ?? c.place ?? null
/** A row of a page's table that is not on its photos (added by hand, or found off the page); not a sheet row shown in between. */
export const offPhoto = (c: ProposalChange) => !c.page && !c.gap

export type Marker =
  /** Sheet order: rows of the sheet between two rows of the table that it does not show. */
  | { kind: 'gap'; count: number; from: number; to: number; paged: boolean }
  /** Notebook order: the next line's row is `by` rows down (or back, < 0) from the previous line's. */
  | { kind: 'jump'; by: number; from: number; to: number }
  /** Notebook order: the rows not on the photo follow. */
  | { kind: 'apart'; count: number }
export type Laid =
  | { change: ProposalChange; marker?: undefined }
  /** `peek`: the sheet's rows it stands for, opened with a click (withPeeks). */
  | { marker: Marker; change?: undefined; peek?: PeekState }

/**
 * A page's rows as the notebook has them: by photo and line; a row without a
 * line after the line of its ID (or of its base ID: A3E.1 after A3E), else
 * `apart`, in the order given. The sheet's rows in between are left out.
 */
export function notebookOrder(changes: ProposalChange[]): { rows: ProposalChange[]; apart: ProposalChange[] } {
  const lineOf = new Map<string, { photo: number; line: number }>()
  for (const c of changes) {
    if (!c.page) continue
    const id = idKey(idOf(c))
    for (const key of [id, baseOf(id)]) if (key && !lineOf.has(key)) lineOf.set(key, c.page)
  }
  const placed: { c: ProposalChange; photo: number; line: number; added: number; i: number }[] = []
  const apart: ProposalChange[] = []
  changes.forEach((c, i) => {
    if (c.gap) return
    if (c.page) return void placed.push({ c, photo: c.page.photo, line: c.page.line, added: 0, i })
    const id = idKey(idOf(c))
    const base = baseOf(id)
    const at = (id && lineOf.get(id)) || (base && lineOf.get(base)) || null
    if (at) placed.push({ c, photo: at.photo, line: at.line, added: 1, i })
    else apart.push(c)
  })
  placed.sort((a, b) => a.photo - b.photo || a.line - b.line || a.added - b.added || a.i - b.i)
  return { rows: placed.map(p => p.c), apart }
}

/** How far the next row is from the previous one in the sheet: 0 when it is the next row (or a row inserted below it). */
function step(from: number, to: number) {
  const by = to - from
  if (by > 0 && by <= 1) return 0
  return by > 0 ? Math.ceil(by) : -Math.ceil(-by)
}

/** The table's rows (all of them, before «Solo cambios») in the order chosen, with the markers between them. */
export function layRows(changes: ProposalChange[], order: RowOrder, paged: boolean): Laid[] {
  const out: Laid[] = []
  let prev: number | null = null
  if (order === 'notebook' && paged) {
    const { rows, apart } = notebookOrder(changes)
    for (const change of rows) {
      const at = sheetAt(change)
      const by = at != null && prev != null ? step(prev, at) : 0
      if (by) out.push({ marker: { kind: 'jump', by, from: prev!, to: at! } })
      if (at != null) prev = at
      out.push({ change })
    }
    if (apart.length) out.push({ marker: { kind: 'apart', count: apart.length } }, ...apart.map(change => ({ change })))
    return out
  }
  for (const change of changes) {
    const at = sheetAt(change)
    if (at != null && prev != null) {
      const from = Math.floor(prev) + 1
      const to = Math.ceil(at) - 1
      if (to >= from) out.push({ marker: { kind: 'gap', count: to - from + 1, from, to, paged } })
    }
    if (at != null) prev = Math.max(prev ?? at, at)
    out.push({ change })
  }
  return out
}

/** The laid rows with only those shown (`keys`); a marker stays when a shown row follows it. */
export function shownRows(laid: Laid[], keys: Set<string>, keyOf: (c: ProposalChange) => string): Laid[] {
  const out: Laid[] = []
  let markers: Laid[] = []
  for (const item of laid) {
    if (item.marker) {
      markers.push(item)
      continue
    }
    if (!keys.has(keyOf(item.change))) continue
    out.push(...markers, item)
    markers = []
  }
  return out
}

const range = (from: number, to: number) => (from === to ? String(from) : `${from}–${to}`)

/** A marker's text, and what its tooltip says. */
export function markerText(m: Marker): { text: string; title: string } {
  if (m.kind === 'gap')
    return m.paged
      ? {
          text: tn(m.count, '⋯ {n} fila que no es de esta página ({range})', '⋯ {n} filas que no son de esta página ({range})', {
            range: range(m.from, m.to),
          }),
          title: t('Las filas de la tabla no van seguidas en la hoja: aquí hay filas de la hoja que no son de esta página'),
        }
      : {
          text: tn(m.count, '⋯ {n} fila de la hoja entre medias ({range})', '⋯ {n} filas de la hoja entre medias ({range})', {
            range: range(m.from, m.to),
          }),
          title: t('Las filas de la tabla no van seguidas en la hoja: aquí hay filas de la hoja que la propuesta no toca'),
        }
  if (m.kind === 'jump')
    return m.by > 0
      ? {
          text: tn(m.by, '↓ +{n} fila en la hoja', '↓ +{n} filas en la hoja', { n: m.by }),
          title: t('La línea siguiente del cuaderno está {n} filas más abajo en la hoja (fila {to}, no {next})', {
            n: m.by,
            to: m.to,
            next: Math.floor(m.from) + 1,
          }),
        }
      : {
          text: tn(-m.by, '↑ {n} fila atrás en la hoja', '↑ {n} filas atrás en la hoja', { n: -m.by }),
          title: t('La línea siguiente del cuaderno está más arriba en la hoja (fila {to}), antes que la línea anterior (fila {from})', {
            to: m.to,
            from: m.from,
          }),
        }
  return {
    text: tn(m.count, 'No está en la foto · {n} fila', 'No están en la foto · {n} filas'),
    title: t('Filas sin línea en las fotos de la página: añadidas a mano o encontradas fuera de la página'),
  }
}

// ------------------------------------------------------------ the sheet's rows a marker stands for

/** At most this many of the sheet's rows open at once under a marker (PEEK_ROWS in server/proposal-view.mjs). */
export const PEEK_ROWS = 50

/**
 * The sheet's rows a marker stands for and the table does not show: a gap's,
 * and those a jump down of the notebook's lines goes over; null for the others
 * (a jump back goes over rows the table shows above it).
 */
export function hiddenRange(m: Marker): { from: number; to: number } | null {
  if (m.kind === 'gap') return { from: m.from, to: m.to }
  if (m.kind !== 'jump' || m.by <= 0) return null
  const from = Math.floor(m.from) + 1
  const to = Math.ceil(m.to) - 1
  return to >= from ? { from, to } : null
}
/** A marker's own name, the same in either order of the rows. */
export const markerKey = (m: Marker) => (m.kind === 'apart' ? 'apart' : `${m.kind}:${m.from}:${m.to}`)

/** A sheet row as GET chat/proposals/:id/rows sends it: its values as the sheet has them now. */
export interface SheetRow {
  recordId: string
  row: number
  label: string
  values: Record<string, CellValue>
}
/** The rows asked for (up to PEEK_ROWS); `rest`: the rows after them, when there were more. */
export interface SheetRows {
  sheet: string
  from: number
  to: number
  rows: SheetRow[]
  rest?: { from: number; to: number }
}
/** A marker opened: its rows on their way, shown, or not read. */
export type Peek = { state: 'loading' } | { state: 'error'; message: string } | { state: 'open'; rows: SheetRows }
/** What the marker's row says of it: opened, and how many of its rows the table shows elsewhere. */
export type PeekState = { state: 'loading' | 'error' | 'open'; inTable?: number; message?: string }

/** A sheet row shown under its marker: grey, to read only, never written (as the rows in between the server sends). */
export function peekChange(sheet: string, r: SheetRow): ProposalChange {
  return {
    index: -1,
    key: `peek:${r.recordId}`,
    recordId: r.recordId,
    sheet,
    row: r.row,
    label: r.label,
    values: {},
    rowValues: r.values,
    context: true,
    gap: true,
    note: '',
  }
}

/**
 * The laid rows with the markers opened (`peeks`, by markerKey) followed by
 * their sheet rows, those the table does not show already (they are counted
 * on the marker); past PEEK_ROWS, a marker for the rest, which opens the same way.
 */
export function withPeeks(laid: Laid[], peeks: ReadonlyMap<string, Peek>, sheet: string): Laid[] {
  if (!peeks.size) return laid
  const seen = new Set(laid.flatMap(item => (item.change?.recordId ? [item.change.recordId] : [])))
  const out: Laid[] = []
  const add = (marker: Marker) => {
    const peek = peeks.get(markerKey(marker))
    if (!peek || !hiddenRange(marker)) return void out.push({ marker })
    if (peek.state === 'loading') return void out.push({ marker, peek: { state: 'loading' } })
    if (peek.state === 'error') return void out.push({ marker, peek: { state: 'error', message: peek.message } })
    const rows = peek.rows.rows.filter(r => !seen.has(r.recordId))
    const inTable = peek.rows.rows.length - rows.length
    out.push({ marker, peek: { state: 'open', ...(inTable ? { inTable } : {}) } })
    for (const r of rows) {
      seen.add(r.recordId)
      out.push({ change: peekChange(sheet, r) })
    }
    const rest = peek.rows.rest
    const paged = marker.kind === 'gap' && marker.paged
    if (rest) add({ kind: 'gap', count: rest.to - rest.from + 1, from: rest.from, to: rest.to, paged })
  }
  for (const item of laid) {
    if (item.marker) add(item.marker)
    else out.push(item)
  }
  return out
}

/** What a marker says besides its text: that a click opens or folds its rows, or that they are coming. */
export function peekText(m: Marker, peek?: PeekState): { action: string; title: string } {
  const range = hiddenRange(m)
  if (!range) return { action: '', title: '' }
  const count = range.to - range.from + 1
  if (!peek)
    return {
      action: '',
      title:
        count > PEEK_ROWS
          ? t('Clic: ver las primeras {n} de estas filas de la hoja, solo para leer', { n: PEEK_ROWS })
          : t('Clic: ver estas filas de la hoja, solo para leer'),
    }
  if (peek.state === 'loading') return { action: t('cargando…'), title: '' }
  if (peek.state === 'error')
    return { action: t('no se pudo leer · reintentar'), title: peek.message ?? '' }
  return {
    action: [peek.inTable ? t('{n} ya en la tabla', { n: peek.inTable }) : '', t('ocultar')].filter(Boolean).join(' · '),
    title: t('Clic: ocultar estas filas de la hoja'),
  }
}

// ------------------------------------------------------------ repeated IDs

/** Repeats next to each other in the sheet whose rows are away from their IDs' series (A0E.1–A8E.1 after Z9D). */
export interface RepeatRun {
  ids: string[]
  rows: [number, number]
  /** The ID of the sheet row just above the first. */
  above?: string
  bases: string[]
  /** Their base IDs' rows, when all are known. */
  baseRows?: [number, number]
  /** Their base IDs' rows are all empty pre-made rows. */
  empty: boolean
}

/**
 * The table's repeats (rows with `repeatOf`) in runs of rows next to each other
 * in the sheet; only the runs not right below their ID's own rows (the
 * curators' way: W2B, W2B.1, W2B.2), which is what makes the page and the
 * sheet differ.
 */
export function repeatRuns(changes: ProposalChange[]): RepeatRun[] {
  const repeats = changes
    .filter(c => c.repeatOf && !c.gap && sheetAt(c) != null)
    .sort((a, b) => sheetAt(a)! - sheetAt(b)!)
  const runs: ProposalChange[][] = []
  for (const c of repeats) {
    const last = runs.at(-1)
    const prev = last?.at(-1)
    if (last && prev && sheetAt(c)! - sheetAt(prev)! <= 1) last.push(c)
    else runs.push([c])
  }
  return runs
    .filter(run => {
      const { id, above } = run[0].repeatOf!
      return !above || (idKey(above) !== id && baseOf(above) !== id)
    })
    .map(run => {
      const rows = run.map(c => Math.ceil(sheetAt(c)!))
      const baseRows = run.map(c => c.repeatOf!.row)
      const known = baseRows.every((r): r is number => r != null)
      return {
        ids: run.map(idOf),
        rows: [rows[0], rows.at(-1)!],
        ...(run[0].repeatOf!.above ? { above: run[0].repeatOf!.above } : {}),
        bases: run.map(c => c.repeatOf!.id),
        ...(known ? { baseRows: [Math.min(...baseRows), Math.max(...baseRows)] as [number, number] } : {}),
        empty: known && run.every(c => c.repeatOf!.empty),
      }
    })
}

const span = (list: string[]) => (list.length > 1 ? `${list[0]}–${list.at(-1)}` : (list[0] ?? ''))

/** A run in plain words: «A0E.1–A8E.1 are repeats: their rows are 13522–13530, after Z9D, not with A0E–A8E (13263–13271, still empty)». */
export function repeatText(run: RepeatRun): string {
  const n = run.ids.length
  const head = tn(n, '{ids} es una repetición', '{ids} son repeticiones', { ids: span(run.ids) })
  const where = tn(n, 'su fila es la {rows}', 'sus filas son {rows}', { rows: range(...run.rows) })
  const after = run.above ? `, ${t('después de {id}', { id: run.above })}` : ''
  const bases = span(run.bases)
  const notWith = !run.baseRows
    ? t('no con {bases}', { bases })
    : run.empty
      ? tn(n, 'no con la fila de {bases} ({rows}, aún vacía)', 'no con las filas de {bases} ({rows}, aún vacías)', {
          bases,
          rows: range(...run.baseRows),
        })
      : t('no con {bases} ({rows})', { bases, rows: range(...run.baseRows) })
  return `${head}: ${where}${after}, ${notWith}`
}

/** The summary of a table's repeats (up to two runs, then how many more); '' when none is away from its series. */
export function repeatSummary(changes: ProposalChange[], shown = 2): string {
  const runs = repeatRuns(changes)
  if (!runs.length) return ''
  const more = runs.length > shown ? ` ${t('y {n} más', { n: runs.length - shown })}` : ''
  return runs.slice(0, shown).map(repeatText).join('; ') + more
}

/** The page's lines told out of order that a repeat explains (a repeat's line, or the line before it): said once, by the summary. */
export function unexplained(notes: OrderNote[], changes: ProposalChange[]): OrderNote[] {
  const told = new Set(repeatRuns(changes).flatMap(r => r.ids.map(idKey)))
  return notes.filter(o => !told.has(idKey(o.id)) && !told.has(idKey(o.after.id)))
}

/** A repeat's chip: «repeat of A0E (row 13263)», and its tooltip. */
export function repeatChip(c: ProposalChange): { text: string; title: string } | null {
  const r = c.repeatOf
  if (!r) return null
  return {
    text: r.row ? t('repite {id} (fila {row})', { id: r.id, row: r.row }) : t('repite {id}', { id: r.id }),
    title: [
      t('El mismo ID ({id}) se escribió en dos mariposas: la repetida va en su propia fila, después de la serie, no en la fila de {id}', {
        id: r.id,
      }),
      r.row ? (r.empty ? t('La fila de {id} es la {row}, aún vacía', { id: r.id, row: r.row }) : t('La fila de {id} es la {row}', { id: r.id, row: r.row })) : '',
    ]
      .filter(Boolean)
      .join('. '),
  }
}

/** The table's row of a repeat's base ID (to go to it), when the table shows it. */
export function baseRowKey(c: ProposalChange, changes: ProposalChange[], keyOf: (c: ProposalChange) => string): string | null {
  const id = c.repeatOf?.id
  const hit = id ? changes.find(o => o !== c && idKey(idOf(o)) === id) : null
  return hit ? keyOf(hit) : null
}
