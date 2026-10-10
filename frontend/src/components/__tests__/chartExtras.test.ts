import { describe, expect, it } from 'vitest'
import { needsExtras } from '../charts/extras'
import { base } from '../charts/chart'

describe('needsExtras', () => {
  it('Inicio’s bars and lines load only the common parts', () => {
    expect(needsExtras({ ...base({ legend: true }), series: [{ type: 'bar' }, { type: 'line' }] })).toBe(false)
    expect(needsExtras({ ...base(), series: { type: 'bar', data: [1] } })).toBe(false)
  })
  it('a heat map, a colour scale, zoom bars or marked areas load the extra parts', () => {
    expect(needsExtras({ ...base({ zoom: true }), series: [{ type: 'bar' }] })).toBe(true)
    expect(needsExtras({ visualMap: { min: 0, max: 1 }, series: [{ type: 'heatmap' }] })).toBe(true)
    expect(needsExtras({ series: [{ type: 'heatmap' }] })).toBe(true)
    expect(needsExtras({ series: [{ type: 'bar', markArea: { data: [] } }] })).toBe(true)
    expect(needsExtras({ series: [{ type: 'line', markLine: { data: [] } }] })).toBe(true)
  })
})
