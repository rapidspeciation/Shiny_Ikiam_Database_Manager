import { describe, expect, it } from 'vitest'
import type { OrderNote, ProposalChange } from '../proposals'
import { rowKey } from '../proposals'
import {
  PEEK_ROWS,
  baseOf,
  deathDay,
  deathMark,
  hiddenRange,
  layRows,
  markerKey,
  markerText,
  notebookOrder,
  peekText,
  repeatChip,
  repeatRuns,
  repeatSummary,
  shownRows,
  unexplained,
  withPeeks,
  type Laid,
  type Peek,
  type SheetRows,
} from '../proposalRows'
import { readRowOrder, writeRowOrder, type KeptStorage } from '../proposalColumns'

/** A row of the table: its ID, sheet row and page line (photo 0). */
function row(label: string, at: number | null, line?: number, extra: Partial<ProposalChange> = {}): ProposalChange {
  return {
    index: 0,
    key: `k:${label}`,
    recordId: `r:${label}`,
    sheet: 'Insectary_data',
    row: at,
    label,
    values: {},
    ...(line ? { page: { photo: 0, line } } : {}),
    ...extra,
  }
}
/** The laid rows as text: IDs, and the markers' kind and numbers. */
const shape = (laid: Laid[]) =>
  laid.map(item =>
    item.change
      ? item.change.label
      : item.marker.kind === 'gap'
        ? `gap ${item.marker.count} ${item.marker.from}-${item.marker.to}`
        : item.marker.kind === 'jump'
          ? `jump ${item.marker.by}`
          : `apart ${item.marker.count}`,
  )

/**
 * The page of the real case: lines Y8D–Z9D, then A0E–A8E written in the sheet
 * as repeats A0E.1–A8E.1 right after Z9D (rows 13522–13530), their pre-made
 * rows (13263–13271) empty 260 rows higher; then A9E and B0E in their own rows.
 */
function realPage() {
  const ids = ['Y8D', 'Y9D', 'Z0D', 'Z1D', 'Z2D', 'Z3D', 'Z4D', 'Z5D', 'Z6D', 'Z7D', 'Z8D', 'Z9D']
  const head = ids.map((id, i) => row(id, 13510 + i, i + 1))
  const repeats = Array.from({ length: 9 }, (_, i) =>
    row(`A${i}E.1`, 13522 + i, ids.length + 1 + i, {
      repeatOf: { id: `A${i}E`, row: 13263 + i, empty: true, above: i ? `A${i - 1}E.1` : 'Z9D' },
    }),
  )
  const tail = [row('A9E', 13272, 22), row('B0E', 13273, 23)]
  // As the server sends them: the sheet's order.
  return [...tail, ...head, ...repeats]
}

describe('the rows in the sheet order', () => {
  it('a slim row only where two rows are not next to each other in the sheet', () => {
    const laid = layRows(realPage(), 'sheet', true)
    expect(shape(laid).slice(0, 4)).toEqual(['A9E', 'B0E', 'gap 236 13274-13509', 'Y8D'])
    expect(laid.filter(i => i.marker)).toHaveLength(1)
    expect(markerText(laid[2].marker!).text).toBe('⋯ 236 filas que no son de esta página (13274–13509)')
  })

  it('a continuous page has none; a proposal without a page says the rows in between', () => {
    expect(layRows([row('A1E', 2, 1), row('A2E', 3, 2), row('A3E', 4, 3)], 'sheet', true).every(i => i.change)).toBe(true)
    const scattered = layRows([row('A1E', 2), row('K5B', 40)], 'sheet', false)
    expect(shape(scattered)).toEqual(['A1E', 'gap 37 3-39', 'K5B'])
    expect(markerText(scattered[1].marker!).text).toBe('⋯ 37 filas de la hoja entre medias (3–39)')
    expect(markerText({ kind: 'gap', count: 1, from: 7, to: 7, paged: true }).text).toBe('⋯ 1 fila que no es de esta página (7)')
  })

  it('a new row counts where it will be written (below a row: right after it); rows with no place say nothing', () => {
    const laid = layRows(
      [row('A1E', 2, 1), row('A1E.1', null, 2, { create: true, place: 2.5 }), row('A2E', 3, 3), row('X', null, 4, { placeholder: true }), row('A5E', 6, 5)],
      'sheet',
      true,
    )
    expect(shape(laid)).toEqual(['A1E', 'A1E.1', 'A2E', 'X', 'gap 2 4-5', 'A5E'])
  })

  it('the rows «Solo cambios» hides are left out, the markers kept before the next row shown', () => {
    const all = [row('A1E', 2, 1), row('A2E', 3, 2, { context: true }), row('A3E', 4, 3), row('K1B', 90, 4)]
    const shown = new Set([all[0], all[2], all[3]].map(rowKey))
    expect(shape(shownRows(layRows(all, 'sheet', true), shown, rowKey))).toEqual(['A1E', 'A3E', 'gap 85 5-89', 'K1B'])
  })
})

/** The sheet rows from–to as the server sends them (row n: ID S<n>). */
function sheetRows(from: number, to: number, total = to): SheetRows {
  const rows = Array.from({ length: to - from + 1 }, (_, i) => ({
    recordId: `r:S${from + i}`,
    row: from + i,
    label: `S${from + i}`,
    values: { Sex: 'NA' },
  }))
  return { sheet: 'Insectary_data', from, to, rows, ...(total > to ? { rest: { from: to + 1, to: total } } : {}) }
}

describe('a slim row opened: the sheet rows it stands for', () => {
  it('a gap and a jump down stand for rows not shown; a jump back and «not on the photo» do not', () => {
    expect(hiddenRange({ kind: 'gap', count: 8, from: 12878, to: 12885, paged: true })).toEqual({ from: 12878, to: 12885 })
    expect(hiddenRange({ kind: 'jump', by: 10, from: 100, to: 110 })).toEqual({ from: 101, to: 109 })
    // A new row to go below row 101 (101.5): the jump goes over row 101.
    expect(hiddenRange({ kind: 'jump', by: 2, from: 100, to: 101.5 })).toEqual({ from: 101, to: 101 })
    expect(hiddenRange({ kind: 'jump', by: -258, from: 13530, to: 13272 })).toBeNull()
    expect(hiddenRange({ kind: 'apart', count: 3 })).toBeNull()
  })

  it('opened, its rows follow it, grey and read-only; closed or loading, only the marker', () => {
    const laid = layRows([row('A1E', 2), row('K5B', 8)], 'sheet', false)
    const gap = laid[1].marker!
    expect(withPeeks(laid, new Map(), 'Insectary_data')).toBe(laid)
    const loading = withPeeks(laid, new Map<string, Peek>([[markerKey(gap), { state: 'loading' }]]), 'Insectary_data')
    expect(shape(loading)).toEqual(['A1E', 'gap 5 3-7', 'K5B'])
    expect(loading[1].marker && loading[1].peek).toEqual({ state: 'loading' })
    const opened = new Map<string, Peek>([[markerKey(gap), { state: 'open', rows: sheetRows(3, 7) }]])
    const open = withPeeks(laid, opened, 'Insectary_data')
    expect(shape(open)).toEqual(['A1E', 'gap 5 3-7', 'S3', 'S4', 'S5', 'S6', 'S7', 'K5B'])
    const first = open[2].change!
    expect(first).toMatchObject({ context: true, gap: true, row: 3, values: {}, rowValues: { Sex: 'NA' }, index: -1 })
    expect(rowKey(first)).toBe('peek:r:S3')
    expect(peekText(gap, open[1].marker ? open[1].peek : undefined).action).toBe('ocultar')
  })

  it('past the cap, a slim row for the rest, which opens the same way', () => {
    const laid = layRows([row('A1E', 2), row('K5B', 200)], 'sheet', false)
    const gap = laid[1].marker!
    const first = sheetRows(3, 2 + PEEK_ROWS, 199)
    const peeks = new Map<string, Peek>([[markerKey(gap), { state: 'open', rows: first }]])
    let out = withPeeks(laid, peeks, 'Insectary_data')
    expect(out).toHaveLength(2 + 1 + PEEK_ROWS + 1)
    const rest = out.at(-2)!.marker!
    expect(rest).toEqual({ kind: 'gap', count: 199 - PEEK_ROWS - 2, from: 3 + PEEK_ROWS, to: 199, paged: false })
    expect(peekText(gap).title).toBe(`Clic: ver las primeras ${PEEK_ROWS} de estas filas de la hoja, solo para leer`)
    peeks.set(markerKey(rest), { state: 'open', rows: sheetRows(3 + PEEK_ROWS, 2 + 2 * PEEK_ROWS, 199) })
    out = withPeeks(laid, peeks, 'Insectary_data')
    expect(out.filter(i => i.change?.gap)).toHaveLength(2 * PEEK_ROWS)
  })

  it('in the notebook order: rows the table shows elsewhere are not shown twice, and counted', () => {
    // Lines A1E (row 2), A9E (row 10), then A5E (row 6) back up: the jump down goes over A5E's row.
    const changes = [row('A1E', 2, 1), row('A9E', 10, 2), row('A5E', 6, 3)]
    const laid = layRows(changes, 'notebook', true)
    expect(shape(laid)).toEqual(['A1E', 'jump 8', 'A9E', 'jump -4', 'A5E'])
    const jump = laid[1].marker!
    const rows = sheetRows(3, 9)
    rows.rows[3] = { recordId: 'r:A5E', row: 6, label: 'A5E', values: {} }
    const out = withPeeks(laid, new Map<string, Peek>([[markerKey(jump), { state: 'open', rows }]]), 'Insectary_data')
    expect(shape(out)).toEqual(['A1E', 'jump 8', 'S3', 'S4', 'S5', 'S7', 'S8', 'S9', 'A9E', 'jump -4', 'A5E'])
    expect(out[1].marker && out[1].peek).toEqual({ state: 'open', inTable: 1 })
    expect(peekText(jump, { state: 'open', inTable: 1 }).action).toBe('1 ya en la tabla · ocultar')
    // A jump back opens nothing.
    expect(peekText(laid[3].marker!).title).toBe('')
  })
})

describe('the rows in the notebook order', () => {
  it('by photo and line, with how far the sheet jumps where the next row does not follow', () => {
    const laid = layRows(realPage(), 'notebook', true)
    expect(shape(laid)).toEqual([
      ...['Y8D', 'Y9D', 'Z0D', 'Z1D', 'Z2D', 'Z3D', 'Z4D', 'Z5D', 'Z6D', 'Z7D', 'Z8D', 'Z9D'],
      ...Array.from({ length: 9 }, (_, i) => `A${i}E.1`),
      'jump -258',
      'A9E',
      'B0E',
    ])
    const jump = laid.find(i => i.marker)!.marker!
    expect(markerText(jump).text).toBe('↑ 258 filas atrás en la hoja')
    expect(markerText({ kind: 'jump', by: 251, from: 13271, to: 13522 }).text).toBe('↓ +251 filas en la hoja')
  })

  it('photos one after the other; a row added by hand goes after its ID’s line (A3E.1 after A3E), the others under «not on the photo»', () => {
    const changes = [
      row('A1E', 2, 1),
      row('A3E', 4, 3),
      row('A3E.1', 40, undefined, { repeatOf: { id: 'A3E', row: 4 } }),
      row('K9Z', 70),
      row('B1E', 12, 1, { page: { photo: 1, line: 1 } }),
      row('A2E', 3, 2),
      row('G1A', 80, undefined, { gap: true, context: true }),
    ]
    const { rows, apart } = notebookOrder(changes)
    expect(rows.map(c => c.label)).toEqual(['A1E', 'A2E', 'A3E', 'A3E.1', 'B1E'])
    expect(apart.map(c => c.label)).toEqual(['K9Z'])
    const laid = layRows(changes, 'notebook', true)
    expect(shape(laid)).toEqual(['A1E', 'A2E', 'A3E', 'jump 36', 'A3E.1', 'jump -28', 'B1E', 'apart 1', 'K9Z'])
    expect(markerText(laid.at(-2)!.marker!).text).toBe('Sin línea en la foto · 1 fila, debajo')
  })

  it('a table without a page keeps the sheet order', () => {
    expect(shape(layRows([row('A1E', 2), row('A2E', 3)], 'notebook', false))).toEqual(['A1E', 'A2E'])
  })
})

describe('repeated IDs', () => {
  it('a suffixed ID’s base', () => {
    expect(baseOf('a0e.1')).toBe('A0E')
    expect(baseOf('W2B.12')).toBe('W2B')
    expect(baseOf('A0E')).toBeNull()
    expect(baseOf('A0E.0')).toBeNull()
  })

  it('the summary says in plain words where the repeats are and where their IDs are', () => {
    const page = realPage()
    expect(repeatRuns(page)).toEqual([
      {
        ids: Array.from({ length: 9 }, (_, i) => `A${i}E.1`),
        rows: [13522, 13530],
        above: 'Z9D',
        bases: Array.from({ length: 9 }, (_, i) => `A${i}E`),
        baseRows: [13263, 13271],
        empty: true,
      },
    ])
    expect(repeatSummary(page)).toBe(
      'A0E.1–A8E.1 son repeticiones: sus filas son 13522–13530, después de Z9D, no con las filas de A0E–A8E (13263–13271, aún vacías)',
    )
  })

  it('a repeat right below its ID’s rows (the curators’ way) is not told; one away from them is', () => {
    expect(repeatSummary([row('W2B', 204, 1), row('W2B.1', 205, 2, { repeatOf: { id: 'W2B', row: 204, above: 'W2B' } })])).toBe('')
    expect(repeatSummary([row('W2B.2', 206, 1, { repeatOf: { id: 'W2B', row: 204, above: 'W2B.1' } })])).toBe('')
    expect(repeatSummary([row('A3E.1', 90, 1, { repeatOf: { id: 'A3E', row: 4, above: 'K2C' } })])).toBe(
      'A3E.1 es una repetición: su fila es la 90, después de K2C, no con A3E (4)',
    )
  })

  it('the lines out of order that a repeat explains are not told again', () => {
    const notes: OrderNote[] = [
      { photo: 0, line: 22, id: 'A9E', after: { line: 21, id: 'A8E.1' } },
      { photo: 0, line: 5, id: 'Q1C', after: { line: 4, id: 'Q7C' } },
    ]
    expect(unexplained(notes, realPage()).map(o => o.id)).toEqual(['Q1C'])
  })

  it('its chip names its ID and that ID’s row', () => {
    const [chip] = realPage()
      .filter(c => c.repeatOf)
      .map(repeatChip)
    expect(chip?.text).toBe('repite A0E (fila 13263)')
    expect(chip?.title).toMatch(/La fila de A0E es la 13263, aún vacía/)
    expect(repeatChip(row('A1E', 2))).toBeNull()
  })
})

describe('the rows’ order is remembered per person', () => {
  it('in this browser, sheet order unless they chose the notebook’s', () => {
    const data = new Map<string, string>()
    const storage: KeptStorage = { getItem: k => data.get(k) ?? null, setItem: (k, v) => void data.set(k, v), removeItem: k => void data.delete(k) }
    expect(readRowOrder(storage, 'ana')).toBe('sheet')
    writeRowOrder(storage, 'ana', 'notebook')
    expect(readRowOrder(storage, 'ana')).toBe('notebook')
    expect(readRowOrder(storage, 'franz')).toBe('sheet')
  })
})

describe('butterflies dead in the sheet, or dying through the proposal', () => {
  const SEP_28_2026 = 46293
  it('a death day as the paper says it: day and month this year, with the year another', () => {
    expect(deathDay(SEP_28_2026, '2026-10-06')).toBe('28-Sep')
    expect(deathDay(SEP_28_2026 - 365, '2026-10-06')).toBe('28-Sep-25')
    expect(deathDay('28/9', '2026-10-06')).toBe('28/9')
  })
  it('the mark on its ID: dead in the sheet (context rows too) or dying here, with when and why', () => {
    const today = '2026-10-06'
    expect(deathMark(row('5VB', 3, 1, { context: true, sheetDeath: { date: SEP_28_2026, cause: 'Unknown' } }), today)).toEqual({
      kind: 'dead',
      text: 'muerta en la hoja: 28-Sep, Unknown',
    })
    expect(deathMark(row('6VB', 4, 2, { sheetDeath: { date: SEP_28_2026, cause: null } }), today)?.text).toBe('muerta en la hoja: 28-Sep')
    expect(deathMark(row('7VB', 5, 3, { diesHere: { date: SEP_28_2026 + 2, cause: 'Eaten' } }), today)).toEqual({
      kind: 'dies',
      text: 'muere en esta propuesta: 30-Sep, Eaten',
    })
    // Independent of the assistant's highlight; nothing on a living butterfly.
    expect(deathMark(row('8VB', 6, 4, { highlight: true }), today)).toBeNull()
  })
})
