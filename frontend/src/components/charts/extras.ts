/**
 * Whether a chart uses a part beyond bars, lines, axes, legend and tooltip: a heat map, its
 * colour scale (visualMap), zoom bars (dataZoom) or a marked area or line. Those parts are
 * loaded on their own (echartsExtras.ts), so Inicio does not wait for them.
 */
export function needsExtras(option: Record<string, unknown>): boolean {
  const list = (value: unknown) => (Array.isArray(value) ? value : value ? [value] : [])
  if (list(option.visualMap).length || list(option.dataZoom).length) return true
  return list(option.series).some(
    s => !!s && typeof s === 'object' && ((s as { type?: string }).type === 'heatmap' || 'markArea' in s || 'markLine' in s),
  )
}
