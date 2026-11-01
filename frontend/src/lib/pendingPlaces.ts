/** One row with unsaved changes, as the save bar names it: the sheet, the row (its label and number), the columns. */
export interface PendingPlace {
  module: string
  label: string
  /** The sheet's row number; null for a new row. */
  row: number | null
  fields: string[]
}

/**
 * The rows the save bar names (the first `limit`, edits before new rows) and how many more there are, so the bar
 * says what is unsaved and not only how much («Insectary_data · R6E · fila 13698: Insectary_ID»).
 */
export function pendingPlaces(
  edits: { module: string; label: string; row: number; values: Record<string, unknown> }[],
  creates: { module: string; label: string }[],
  limit = 2,
): { shown: PendingPlace[]; more: number } {
  const all: PendingPlace[] = [
    ...edits.map(e => ({ module: e.module, label: e.label, row: e.row, fields: Object.keys(e.values) })),
    ...creates.map(c => ({ module: c.module, label: c.label, row: null, fields: [] })),
  ]
  return { shown: all.slice(0, limit), more: Math.max(0, all.length - limit) }
}
