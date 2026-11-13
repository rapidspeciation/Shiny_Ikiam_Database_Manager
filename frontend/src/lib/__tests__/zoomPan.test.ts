import { describe, expect, it } from 'vitest'
import { clampView, IDENTITY, MAX_SCALE, panBy, pinch, TAP_SCALE, toggleAt, wheel, zoomAt } from '../zoomPan'

// The photo viewer's zoom: the point under the finger stays put, a zoomed photo never shows a gap at an edge.
const W = 400
const H = 300
const under = (v: { s: number; x: number; y: number }, px: number, py: number) => [(px - v.x) / v.s, (py - v.y) / v.s]

describe('zoomPan', () => {
  it('a double tap zooms in on the point tapped, a second one goes back to the whole photo', () => {
    const v = toggleAt(IDENTITY, 100, 80, W, H)
    expect(v.s).toBe(TAP_SCALE)
    expect(under(v, 100, 80)).toEqual([100, 80])
    expect(toggleAt(v, 10, 10, W, H)).toEqual(IDENTITY)
  })
  it('zoom keeps the point under the pointer, within the bounds', () => {
    const v = zoomAt(IDENTITY, 3, 200, 150, W, H)
    const [x, y] = under(v, 200, 150)
    expect(x).toBeCloseTo(200)
    expect(y).toBeCloseTo(150)
    expect(zoomAt(IDENTITY, 50, 0, 0, W, H).s).toBe(MAX_SCALE)
    expect(zoomAt(v, 0.2, 0, 0, W, H)).toEqual(IDENTITY)
  })
  it('a pan stops at the edges; at full size nothing moves', () => {
    expect(panBy(IDENTITY, 50, 50, W, H)).toEqual(IDENTITY)
    const v = zoomAt(IDENTITY, 2, 0, 0, W, H)
    expect(panBy(v, 999, 999, W, H)).toMatchObject({ x: 0, y: 0 })
    expect(panBy(v, -9999, -9999, W, H)).toMatchObject({ x: W - 2 * W, y: H - 2 * H })
    expect(clampView({ s: 2, x: 10, y: -1000 }, W, H)).toEqual({ s: 2, x: 0, y: -H })
  })
  it('a pinch follows the fingers\' distance; the wheel zooms in on scroll up', () => {
    expect(pinch(IDENTITY, 100, 250, 200, 150, W, H).s).toBeCloseTo(2.5)
    expect(wheel(IDENTITY, -200, 200, 150, W, H).s).toBeGreaterThan(1)
    expect(wheel(IDENTITY, 200, 200, 150, W, H)).toEqual(IDENTITY)
  })
})
