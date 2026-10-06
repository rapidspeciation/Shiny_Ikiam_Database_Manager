import { reactive } from 'vue'

/**
 * How a proposal's table orders its columns (Cambios propuestos), chosen on
 * each card and remembered per person in this browser:
 * - 'sheet': every column the table has, in the sheet's own order (left to
 *   right, as Google Sheets shows them);
 * - 'notebook': a notebook page's proposal follows the notebook, its columns
 *   left to right as the page has them (server/notebook.mjs KINDS, sent as
 *   `page.columns`), then the others in the sheet's order; a Collection_data
 *   table without a page, the columns a wild-caught butterfly is written down
 *   with (server/proposal-columns.mjs NOTEBOOK_COLUMNS);
 * - 'custom': the person's own order and hidden columns, per sheet.
 */
export type ColumnView = 'sheet' | 'notebook' | 'custom'
export const COLUMN_VIEWS: ColumnView[] = ['sheet', 'notebook', 'custom']

/**
 * A person's own columns for a sheet: `order`, the columns placed (shown, in
 * this order; one the proposal does not bring is added to the table);
 * `hidden`, those taken out. Columns neither placed nor hidden (new ones) go
 * after the placed ones, in the sheet's order.
 */
export interface CustomColumns {
  order: string[]
  hidden: string[]
}

/** The columns in the sheet's order; those the sheet does not list keep their order at the end. */
export function inSheetOrder(fields: string[], sheetOrder: string[] = []): string[] {
  const at = new Map(sheetOrder.map((f, i) => [f, i]))
  return fields
    .map((f, i) => ({ f, i, at: at.get(f) ?? sheetOrder.length + i }))
    .sort((a, b) => a.at - b.at)
    .map(x => x.f)
}

/**
 * A notebook's columns first, as the page has them (left to right), then those
 * its lines imply (`implied`: a death's template, the note's words) and the
 * others, each in the sheet's order.
 */
export function inNotebookOrder(fields: string[], notebook: string[], sheetOrder: string[] = [], implied: Iterable<string> = []): string[] {
  const has = new Set(fields)
  const first = [...new Set(notebook)].filter(f => has.has(f))
  const rest = fields.filter(f => !first.includes(f))
  const then = new Set(implied)
  return [
    ...first,
    ...inSheetOrder(rest.filter(f => then.has(f)), sheetOrder),
    ...inSheetOrder(rest.filter(f => !then.has(f)), sheetOrder),
  ]
}

/**
 * The person's own columns: those placed first (in their order; one the table
 * does not bring is added when the sheet has it), then the rest in the
 * sheet's order, without the hidden ones. `keep`: columns shown even when
 * hidden (the proposal writes them, or marks a cell there).
 */
export function inCustomOrder(
  fields: string[],
  custom: CustomColumns | null | undefined,
  sheetOrder: string[] = [],
  keep: Iterable<string> = [],
): string[] {
  if (!custom) return inSheetOrder(fields, sheetOrder)
  const has = new Set(fields)
  const known = new Set(sheetOrder)
  const hidden = new Set(custom.hidden)
  for (const f of keep) hidden.delete(f)
  const placed = [...new Set(custom.order)].filter(f => (has.has(f) || known.has(f)) && !hidden.has(f))
  const rest = inSheetOrder(
    fields.filter(f => !placed.includes(f) && !hidden.has(f)),
    sheetOrder,
  )
  return [...placed, ...rest]
}

/** The columns a table shows in a view; 'notebook' without a notebook's columns is the sheet's order. */
export function orderColumns(
  view: ColumnView,
  fields: string[],
  { sheetOrder = [], notebook = [], implied = [], custom = null, keep = [] }: {
    sheetOrder?: string[]
    notebook?: string[]
    implied?: Iterable<string>
    custom?: CustomColumns | null
    keep?: Iterable<string>
  } = {},
): string[] {
  if (view === 'notebook' && notebook.length) return inNotebookOrder(fields, notebook, sheetOrder, implied)
  if (view === 'custom') return inCustomOrder(fields, custom, sheetOrder, keep)
  return inSheetOrder(fields, sheetOrder)
}

/** The view a card shows: the one chosen, unless it is the notebook's and the proposal has no notebook columns. */
export const viewFor = (chosen: ColumnView, paged: boolean): ColumnView => (chosen === 'notebook' && !paged ? 'sheet' : chosen)

// ------------------------------------------------------------ the person's choices, kept in this browser

/** Where the choices are kept (localStorage in the app; a stand-in in tests). */
export interface KeptStorage {
  getItem(key: string): string | null
  setItem(key: string, value: string): void
  removeItem(key: string): void
}
const viewKey = (user: string) => `ithomiini:proposal-view:${user}`
const rowOrderKey = (user: string) => `ithomiini:proposal-row-order:${user}`
const customKey = (user: string, sheet: string) => `ithomiini:proposal-columns-custom:${user}:${sheet}`

export function readView(storage: KeptStorage, user: string): ColumnView {
  try {
    const v = storage.getItem(viewKey(user))
    return v && (COLUMN_VIEWS as string[]).includes(v) ? (v as ColumnView) : 'sheet'
  } catch {
    return 'sheet'
  }
}
export function writeView(storage: KeptStorage, user: string, view: ColumnView) {
  try {
    storage.setItem(viewKey(user), view)
  } catch {
    /* storage full or blocked: the choice lasts while the page is open */
  }
}
/**
 * How a notebook page's table orders its rows: 'sheet', as the sheet has them;
 * 'notebook', as the photo's lines (lib/proposalRows). Remembered per person.
 */
export type RowOrder = 'sheet' | 'notebook'
export function readRowOrder(storage: KeptStorage, user: string): RowOrder {
  try {
    return storage.getItem(rowOrderKey(user)) === 'notebook' ? 'notebook' : 'sheet'
  } catch {
    return 'sheet'
  }
}
export function writeRowOrder(storage: KeptStorage, user: string, order: RowOrder) {
  try {
    storage.setItem(rowOrderKey(user), order)
  } catch {
    /* as for the view */
  }
}
export function readCustom(storage: KeptStorage, user: string, sheet: string): CustomColumns | null {
  try {
    const raw = storage.getItem(customKey(user, sheet))
    if (!raw) return null
    const v = JSON.parse(raw) as Partial<CustomColumns>
    const strings = (list: unknown) => (Array.isArray(list) ? list.filter((x): x is string => typeof x === 'string') : [])
    return { order: strings(v.order), hidden: strings(v.hidden) }
  } catch {
    return null
  }
}
export function writeCustom(storage: KeptStorage, user: string, sheet: string, custom: CustomColumns | null) {
  try {
    if (custom) storage.setItem(customKey(user, sheet), JSON.stringify(custom))
    else storage.removeItem(customKey(user, sheet))
  } catch {
    /* as above */
  }
}

/** A column moved in the person's list (drag or arrows): the new order. */
export function moveColumn(list: string[], from: number, to: number): string[] {
  if (from === to || from < 0 || from >= list.length) return list
  const out = [...list]
  const [f] = out.splice(from, 1)
  out.splice(Math.max(0, Math.min(out.length, to)), 0, f)
  return out
}

/**
 * The choices as every card of the page shares them (one card's change shows
 * on all), for the person signed in.
 */
const shared = reactive({
  user: '',
  view: 'sheet' as ColumnView,
  rowOrder: 'sheet' as RowOrder,
  custom: {} as Record<string, CustomColumns | null>,
})
const storage = (): KeptStorage | null => (typeof localStorage === 'undefined' ? null : localStorage)
function forUser(user: string) {
  if (shared.user === user) return
  shared.user = user
  const s = storage()
  shared.view = s ? readView(s, user) : 'sheet'
  shared.rowOrder = s ? readRowOrder(s, user) : 'sheet'
  shared.custom = {}
}
export function columnChoices(user: string) {
  forUser(user)
  return {
    get view() {
      return shared.view
    },
    setView(view: ColumnView) {
      shared.view = view
      const s = storage()
      if (s) writeView(s, user, view)
    },
    get rowOrder() {
      return shared.rowOrder
    },
    setRowOrder(order: RowOrder) {
      shared.rowOrder = order
      const s = storage()
      if (s) writeRowOrder(s, user, order)
    },
    custom(sheet: string): CustomColumns | null {
      // Read from the browser until changed here (a change is kept in `shared`, so every card follows it).
      const changed = shared.custom[sheet]
      if (changed !== undefined) return changed
      const s = storage()
      return s ? readCustom(s, user, sheet) : null
    },
    setCustom(sheet: string, custom: CustomColumns | null) {
      shared.custom[sheet] = custom
      const s = storage()
      if (s) writeCustom(s, user, sheet, custom)
    },
  }
}
