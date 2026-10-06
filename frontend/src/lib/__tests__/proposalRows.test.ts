import { describe, expect, it } from 'vitest'
import type { OrderNote, ProposalChange } from '../proposals'
import { rowKey } from '../proposals'
import { baseOf, layRows, markerText, notebookOrder, repeatChip, repeatRuns, repeatSummary, shownRows, unexplained, type Laid } from '../proposalRows'
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
    expect(markerText(laid.at(-2)!.marker!).text).toBe('No está en la foto · 1 fila')
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
      'A0E.1–A8E.1 son repeticiones: sus filas son 13522–13530, después de Z9D, no con las filas sin usar de A0E–A8E (13263–13271, vacías)',
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
    expect(chip?.title).toMatch(/La fila de A0E es la 13263: sin usar, vacía/)
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
