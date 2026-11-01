/**
 * Shared look of the report's ECharts figures. Colours are the validated
 * reference palette; the categorical order is fixed, so a series keeps its
 * colour whatever else is filtered out. Each series is at least 3:1 on the
 * chart's surface and every label 4.5:1, in light and in dark: the options
 * are written in the light colours, and darkOption() swaps them for the dark
 * ones (EChartCanvas does it, and draws again when the theme changes).
 */
import { intlLocale } from '../../lib/i18n'
import { cssColor, theme } from '../../lib/theme'

export const SERIES = ['#2a78d6', '#eb6834', '#00a571', '#cb8100', '#d86d96', '#008300', '#4a3aa7', '#e34948']
export const OTHER = '#91908a'
/** Sequential blue, light → dark, for magnitude (heat cells). */
export const BLUES = ['#eef5fd', '#cde2fb', '#9ec5f4', '#6da7ec', '#3987e5', '#2670cc', '#1c5cab', '#104281']
const INK = '#0b0b0b'
/** Value labels, legends and the species names on an axis. */
export const LABEL = '#52514e'
/** Axis numbers and units: the subtle text step (4.5:1 on white). */
export const MUTED = '#6f6964'
const GRID = '#e1e0d9'
const AXIS = '#c3c2b7'
/** The pale part of a bar of three (unknown sex): an edge-like grey. */
export const PALE = '#e7e5e4'

/** The same colours on the dark surface: series solved again for 3:1 there, the blues from dark (low) to light (high). */
const SERIES_DARK = ['#2a78d6', '#eb6834', '#00a571', '#cb8100', '#d86d96', '#008300', '#655aca', '#e34948']
const BLUES_DARK = ['#16263b', '#1a3556', '#204779', '#2a5c9c', '#3473c0', '#4f8fdc', '#7aaee8', '#a9cdf2']
function darkColours(): Record<string, string> {
  const map: Record<string, string> = {
    [INK]: cssColor('--fg', '#e9ece9'),
    '#1c1917': cssColor('--fg', '#e9ece9'),
    [LABEL]: cssColor('--fg-muted', '#b2b9b4'),
    [MUTED]: cssColor('--fg-subtle', '#959d98'),
    [GRID]: '#2b332e',
    [AXIS]: '#4b544f',
    [PALE]: '#4b544f',
    [OTHER]: '#a8a7a1',
    'rgba(0,0,0,.04)': 'rgba(255,255,255,.06)',
    'rgba(255,255,255,.9)': 'rgba(21,26,24,.9)',
  }
  SERIES.forEach((c, i) => (map[c] = SERIES_DARK[i]!))
  BLUES.forEach((c, i) => (map[c] = BLUES_DARK[i]!))
  return map
}
const WHITE = new Set(['#fff', '#ffffff'])
/** Keys whose white is the surface under the chart (gaps between bars, the tooltip), not a label on a bar. */
const SURFACE_KEYS = new Set(['borderColor', 'backgroundColor'])

/** An option written in the light colours, in the dark ones (a copy; functions and the rest kept as they are). */
export function darkOption<T>(option: T): T {
  const map = darkColours()
  const surface = cssColor('--surface', '#151a18')
  const raised = cssColor('--surface-raised', '#202723')
  const walk = (value: unknown, key: string): unknown => {
    if (typeof value === 'string') {
      const low = value.toLowerCase()
      if (WHITE.has(low) && SURFACE_KEYS.has(key)) return key === 'backgroundColor' ? raised : surface
      return map[low] ?? map[value] ?? value
    }
    if (Array.isArray(value)) return value.map(v => walk(v, key))
    if (value && typeof value === 'object' && Object.getPrototypeOf(value) === Object.prototype)
      return Object.fromEntries(Object.entries(value).map(([k, v]) => [k, walk(v, k)]))
    return value
  }
  return walk(option, '') as T
}
/** A chart colour as the current theme shows it, for the HTML beside a chart (swatches, heat cells). */
export const themed = (color: string) => (theme.value === 'dark' ? (darkColours()[color.toLowerCase()] ?? color) : color)

/** A number in the interface language's style (1.234,5 in Spanish, 1,234.5 in English). */
export const format = (n: number | null | undefined) =>
  typeof n !== 'number' || !Number.isFinite(n)
    ? '—'
    : Number.isInteger(n)
      ? n.toLocaleString(intlLocale())
      : n.toLocaleString(intlLocale(), { maximumFractionDigits: 1 })

/** Heat-cell fill for a value in [0, max] and a legible text colour on it (for HTML tables). */
export function heat(value: number, max: number) {
  if (!value) return { fill: 'transparent', ink: 'var(--fg-subtle)' }
  const i = Math.min(BLUES.length - 1, 1 + Math.floor((value / Math.max(max, 1)) * (BLUES.length - 2)))
  // Dark: the scale runs from deep (low) to pale (high), so the ink flips at the same step.
  if (theme.value === 'dark') return { fill: BLUES_DARK[i], ink: i >= 5 ? INK : '#ffffff' }
  return { fill: BLUES[i], ink: i >= 5 ? '#ffffff' : INK }
}

const escape = (s: unknown) => String(s ?? '').replace(/[&<>"']/g, ch => `&#${ch.charCodeAt(0)};`)
/** Tooltip row: a short line key in the series colour, the value first, then the name. */
export const tipRow = (color: string, value: string, label: string) =>
  `<div style="display:flex;align-items:center;gap:6px"><span style="display:inline-block;width:12px;height:2px;background:${color}"></span><b>${escape(value)}</b><span style="color:var(--fg-muted)">${escape(label)}</span></div>`
export const tipTitle = (title: string) => `<div style="color:var(--fg-muted);font-weight:500;margin-bottom:2px">${escape(title)}</div>`

/**
 * Beside the pointer, at the top of the chart: the tooltip never covers the
 * column or point being read.
 */
function besidePointer(
  point: number[],
  _params: unknown,
  _dom: unknown,
  _rect: unknown,
  size: { contentSize: number[]; viewSize: number[] },
) {
  const [w] = size.contentSize
  const [W] = size.viewSize
  const left = point[0] + 18 + w > W ? point[0] - w - 18 : point[0] + 18
  return [Math.max(0, left), 4]
}

export function base(extra: { legend?: boolean; zoom?: boolean } = {}) {
  return {
    color: SERIES,
    animationDuration: 300,
    textStyle: { fontFamily: "'Fira Sans', system-ui, sans-serif", color: INK },
    aria: { enabled: true },
    grid: { left: 8, right: 12, top: extra.legend ? 34 : 12, bottom: extra.zoom ? 44 : 8, containLabel: true },
    legend: extra.legend
      ? { top: 0, left: 0, icon: 'roundRect', itemWidth: 10, itemHeight: 10, textStyle: { color: LABEL, fontSize: 12 } }
      : { show: false },
    tooltip: {
      confine: true,
      position: besidePointer,
      backgroundColor: '#ffffff',
      borderColor: PALE,
      borderWidth: 1,
      padding: [6, 10],
      textStyle: { color: INK, fontSize: 12 },
      extraCssText: 'box-shadow:0 2px 8px rgba(0,0,0,.08);border-radius:6px',
    },
    dataZoom: extra.zoom
      ? [
          { type: 'inside', zoomOnMouseWheel: 'shift' },
          { type: 'slider', height: 18, bottom: 6, borderColor: GRID, fillerColor: 'rgba(42,120,214,.12)', showDetail: false },
        ]
      : [],
  }
}

export function axis(type: 'category' | 'value', data?: string[]) {
  return {
    type,
    data,
    axisLine: { show: type === 'category', lineStyle: { color: AXIS } },
    axisTick: { show: false },
    axisLabel: { color: MUTED, fontSize: 12, hideOverlap: true },
    splitLine: { show: type === 'value', lineStyle: { color: GRID, type: 'solid' } },
  }
}

/** Bars: thin, rounded at the data end only, 2px surface gap between stacked parts. */
export const barStyle = (color: string, top = true, horizontal = false) => ({
  color,
  borderColor: '#ffffff',
  borderWidth: 1,
  borderRadius: top ? (horizontal ? [0, 4, 4, 0] : [4, 4, 0, 0]) : 0,
})
