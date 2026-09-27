/**
 * Shared pieces of the small SVG charts used in the monitoring report.
 * Colours are the validated reference palette (categorical order is fixed:
 * a series keeps its colour whatever else is filtered out).
 */
export const SERIES = ['#2a78d6', '#eb6834', '#1baf7a', '#eda100', '#e87ba4', '#008300', '#4a3aa7', '#e34948']
export const OTHER = '#a8a7a1'
/** Sequential blue, light → dark, for magnitude (heat cells). */
export const BLUES = [
  '#cde2fb',
  '#b7d3f6',
  '#9ec5f4',
  '#86b6ef',
  '#6da7ec',
  '#5598e7',
  '#3987e5',
  '#2a78d6',
  '#256abf',
  '#1c5cab',
  '#184f95',
  '#104281',
]

export interface Series {
  key: string
  label: string
  color: string
  values: number[]
}

/** A rounded axis top and 3–5 clean ticks (0, 5, 10, 15 …). */
export function niceTicks(max: number, target = 4): number[] {
  if (!(max > 0)) return [0, 1]
  const raw = max / target
  const power = 10 ** Math.floor(Math.log10(raw))
  const step = [1, 2, 2.5, 5, 10].map(m => m * power).find(s => s >= raw) || power * 10
  const top = Math.ceil(max / step) * step
  const ticks = []
  for (let v = 0; v <= top + step / 2; v += step) ticks.push(Math.round(v * 100) / 100)
  return ticks
}

export const format = (n: number | null | undefined) =>
  typeof n !== 'number' || !Number.isFinite(n)
    ? '—'
    : Number.isInteger(n)
      ? n.toLocaleString('es-EC')
      : n.toLocaleString('es-EC', { maximumFractionDigits: 1 })

/** Heat-cell fill for a value in [0, max]; text colour that stays legible on it. */
export function heat(value: number, max: number) {
  if (!value) return { fill: 'transparent', ink: '#898781' }
  const i = Math.min(BLUES.length - 1, Math.floor((value / Math.max(max, 1)) * (BLUES.length - 1)))
  return { fill: BLUES[i], ink: i >= 6 ? '#ffffff' : '#0b0b0b' }
}
