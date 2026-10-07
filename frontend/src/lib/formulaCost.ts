/**
 * What the formulas a proposal writes cost the Google Sheet's recalculation
 * (server/formula-cost.mjs), in plain words for the notice above its table:
 * cells × rows each one scans ≈ comparisons per full recalculation, and the
 * formulas elsewhere that read the written column whole (each write makes them
 * recalculate). `heavy` (amber) is decided by the server's thresholds.
 */
import { intlLocale, t, tn } from './i18n'

export interface FormulaCost {
  sheet: string
  column: string
  formula: string
  cells: number
  perCell: number
  comparisons: number
  scans: { range: string; rows: number; lookup?: boolean; times?: number }[]
  dependents: { cells: number; comparisons: number; sheets: [string, number][] } | null
  flags: string[]
  heavy: boolean
  tips: string[]
  bounded?: string
}

/** A count as people read it: 1.000 / 1,000; millions as «8 M». */
export function bigNumber(n: number): string {
  const whole = (v: number, digits = 0) =>
    new Intl.NumberFormat(intlLocale(), {
      useGrouping: 'always',
      maximumFractionDigits: digits,
    } as Intl.NumberFormatOptions).format(v)
  if (n >= 1e6) return `${whole(n / 1e6, n >= 1e7 ? 0 : 1)} M`
  return whole(Math.round(n))
}

/** The main line of one formula: «1.000 celdas × 9.757 filas ≈ 9,8 M comparaciones por recálculo · cada escritura aquí recalcula …». */
export function costLine(p: FormulaCost): string {
  const own = t('{cells} × {rows} ≈ {total} comparaciones por recálculo', {
    cells: tn(p.cells, '{n} celda', '{n} celdas', { n: bigNumber(p.cells) }),
    rows: tn(p.perCell, '{n} fila', '{n} filas', { n: bigNumber(p.perCell) }),
    total: bigNumber(p.comparisons),
  })
  if (!p.dependents) return own
  const sheets = p.dependents.sheets.map(([sheet]) => sheet).join(', ')
  return `${own} · ${t('cada escritura aquí recalcula {n} búsquedas de {sheets}', { n: bigNumber(p.dependents.cells), sheets })}`
}

/** What would make it lighter, in one line (empty when it is light). */
export function costTip(p: FormulaCost): string {
  if (!p.heavy && !p.flags.length) return ''
  const tips: Record<string, () => string> = {
    helper: () =>
      t('el mismo rango se busca varias veces por fila: una columna auxiliar con XMATCH una vez por fila, luego INDEX'),
    bounded: () =>
      t('un rango que acabe en la última fila usada ({range}) en vez de la columna entera; hay que alargarlo al añadir filas', {
        range: p.bounded ?? '',
      }),
    guard: () => t('saltar las filas que no pueden coincidir antes de buscar (IF(clave="","",…) o el tipo de fila)'),
    batch: () => t('escribirla de una vez, no celda por celda: cada escritura recalcula esas búsquedas'),
  }
  return p.tips
    .map(tip => tips[tip]?.())
    .filter(Boolean)
    .join('; ')
}
