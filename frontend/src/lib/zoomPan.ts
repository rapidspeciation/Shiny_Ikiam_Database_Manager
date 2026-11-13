/**
 * The photo viewer's zoom and pan (components/identify/PhotoViewer.vue), as pure arithmetic so it is tested without a
 * browser. The photo's frame is `w` × `h` CSS px; the content is drawn with `translate(x, y) scale(s)` from its top
 * left corner. At scale 1 nothing moves; zoomed in, the content never leaves a gap at the frame's edges.
 */
export interface View {
  s: number
  x: number
  y: number
}
export const MIN_SCALE = 1
export const MAX_SCALE = 6
/** The scale a double tap (or double click) goes to, centred on the point tapped. */
export const TAP_SCALE = 2.5

export const IDENTITY: View = { s: 1, x: 0, y: 0 }

/** The view kept inside the frame: scale within its bounds, no empty band at an edge. */
export function clampView(v: View, w: number, h: number): View {
  const s = Math.min(MAX_SCALE, Math.max(MIN_SCALE, v.s))
  const x = Math.min(0, Math.max(w - w * s, v.x))
  const y = Math.min(0, Math.max(h - h * s, v.y))
  return { s, x: s === 1 ? 0 : x, y: s === 1 ? 0 : y }
}

/** Zoom to scale `to` keeping the point (px, py) of the frame under the finger or the pointer. */
export function zoomAt(v: View, to: number, px: number, py: number, w: number, h: number): View {
  const s = Math.min(MAX_SCALE, Math.max(MIN_SCALE, to))
  const k = s / v.s
  return clampView({ s, x: px - (px - v.x) * k, y: py - (py - v.y) * k }, w, h)
}

/** A double tap: in to TAP_SCALE at the point, or back to the whole photo when zoomed in already. */
export function toggleAt(v: View, px: number, py: number, w: number, h: number): View {
  return v.s > 1.01 ? IDENTITY : zoomAt(v, TAP_SCALE, px, py, w, h)
}

/** Moved by (dx, dy) px, within the frame. */
export function panBy(v: View, dx: number, dy: number, w: number, h: number): View {
  return clampView({ s: v.s, x: v.x + dx, y: v.y + dy }, w, h)
}

/** A pinch: the scale follows the fingers' distance (from d0 to d1), centred between them. */
export function pinch(start: View, d0: number, d1: number, cx: number, cy: number, w: number, h: number): View {
  return zoomAt(start, start.s * (d1 / Math.max(1, d0)), cx, cy, w, h)
}

/** A mouse wheel step: about 15 % per notch. */
export function wheel(v: View, deltaY: number, px: number, py: number, w: number, h: number): View {
  return zoomAt(v, v.s * Math.exp(-deltaY * 0.0015), px, py, w, h)
}
