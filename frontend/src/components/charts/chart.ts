/**
 * Shared look of the report's ECharts figures. Colours are the validated
 * reference palette; the categorical order is fixed, so a series keeps its
 * colour whatever else is filtered out.
 */
export const SERIES = ['#2a78d6', '#eb6834', '#1baf7a', '#eda100', '#e87ba4', '#008300', '#4a3aa7', '#e34948']
export const OTHER = '#a8a7a1'
/** Sequential blue, light → dark, for magnitude (heat cells). */
export const BLUES = ['#eef5fd', '#cde2fb', '#9ec5f4', '#6da7ec', '#3987e5', '#2a78d6', '#1c5cab', '#104281']
const INK = '#0b0b0b'
const MUTED = '#898781'
const GRID = '#e1e0d9'
const AXIS = '#c3c2b7'

export const format = (n: number | null | undefined) =>
  typeof n !== 'number' || !Number.isFinite(n)
    ? '—'
    : Number.isInteger(n)
      ? n.toLocaleString('es-EC')
      : n.toLocaleString('es-EC', { maximumFractionDigits: 1 })

/** Heat-cell fill for a value in [0, max] and a legible text colour on it (for HTML tables). */
export function heat(value: number, max: number) {
  if (!value) return { fill: 'transparent', ink: MUTED }
  const i = Math.min(BLUES.length - 1, 1 + Math.floor((value / Math.max(max, 1)) * (BLUES.length - 2)))
  return { fill: BLUES[i], ink: i >= 5 ? '#ffffff' : INK }
}

const escape = (s: unknown) => String(s ?? '').replace(/[&<>"']/g, ch => `&#${ch.charCodeAt(0)};`)
/** Tooltip row: a short line key in the series colour, the value first, then the name. */
export const tipRow = (color: string, value: string, label: string) =>
  `<div style="display:flex;align-items:center;gap:6px"><span style="display:inline-block;width:12px;height:2px;background:${color}"></span><b>${escape(value)}</b><span style="color:#52514e">${escape(label)}</span></div>`
export const tipTitle = (title: string) => `<div style="color:#52514e;font-weight:500;margin-bottom:2px">${escape(title)}</div>`

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
      ? { top: 0, left: 0, icon: 'roundRect', itemWidth: 10, itemHeight: 10, textStyle: { color: '#52514e', fontSize: 12 } }
      : { show: false },
    tooltip: {
      confine: true,
      position: besidePointer,
      backgroundColor: '#ffffff',
      borderColor: '#e7e5e4',
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
    axisLabel: { color: MUTED, fontSize: 11, hideOverlap: true },
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
