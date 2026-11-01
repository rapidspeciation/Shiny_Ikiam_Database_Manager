import { readFileSync } from 'node:fs'
import { resolve } from 'node:path'
import { describe, expect, it } from 'vitest'

// The role tokens of style.css (:root and [data-theme='dark']), held to their contrast bars
// (.work/ui-overhaul-20261031/PRINCIPLES.md §P1, §P2, §5): text 4.5:1 (body 7:1), edges and controls 3:1.
const css = readFileSync(resolve(import.meta.dirname, '../../style.css'), 'utf8')

function block(selector: string): Record<string, string> {
  const start = css.indexOf(`${selector} {`)
  const body = css.slice(start, css.indexOf('}', start))
  return Object.fromEntries([...body.matchAll(/--([a-z-]+):\s*([^;]+);/g)].map(m => [m[1], m[2].trim()]))
}
const themes = { light: block(':root'), dark: block("[data-theme='dark']") }

type Rgba = [number, number, number, number]
function parse(value: string): Rgba {
  const hex = /^#([0-9a-f]{6})$/i.exec(value)
  if (hex) return [0, 2, 4].map(i => parseInt(hex[1].slice(i, i + 2), 16)).concat(1) as Rgba
  const rgb = /^rgb\((\d+) (\d+) (\d+)(?: \/ ([\d.]+))?\)$/.exec(value)
  if (rgb) return [+rgb[1], +rgb[2], +rgb[3], rgb[4] === undefined ? 1 : +rgb[4]]
  throw new Error(`not a colour: ${value}`)
}
const over = (fg: Rgba, bg: Rgba): Rgba => [0, 1, 2].map(i => fg[i] * fg[3] + bg[i] * (1 - fg[3])).concat(1) as Rgba
const lum = (c: Rgba) => {
  const ch = (v: number) => ((v /= 255) <= 0.04045 ? v / 12.92 : ((v + 0.055) / 1.055) ** 2.4)
  return 0.2126 * ch(c[0]) + 0.7152 * ch(c[1]) + 0.0722 * ch(c[2])
}
/** The ratio of token `fg` drawn on token `bg` (translucent tokens composited on `base`, the surface). */
function ratio(theme: keyof typeof themes, fg: string, bg: string, base = 'surface') {
  const t = themes[theme]
  const get = (name: string) => {
    if (!(name in t)) throw new Error(`${theme} has no --${name}`)
    return parse(t[name])
  }
  const under = get(base)
  const b = over(get(bg), under)
  const f = over(get(fg), b)
  const [hi, lo] = [lum(f), lum(b)].sort((x, y) => y - x)
  return (hi + 0.05) / (lo + 0.05)
}

// [foreground, background, bar]
const pairs: [string, string, number][] = [
  ['fg', 'canvas', 7],
  ['fg', 'surface', 7],
  ['fg', 'surface-sunken', 7],
  ['fg', 'surface-raised', 7],
  ['fg-muted', 'surface', 4.5],
  ['fg-muted', 'surface-sunken', 4.5],
  ['fg-muted', 'cell-formula', 4.5],
  ['fg-subtle', 'surface', 4.5],
  ['fg-subtle', 'canvas', 4.5],
  ['fg-subtle', 'surface-sunken', 4.5],
  ['fg-subtle', 'surface-raised', 4.5],
  ['border-control', 'surface', 3],
  ['border-control', 'canvas', 3],
  ['on-brand', 'brand', 4.5],
  ['on-brand', 'brand-hover', 4.5],
  ['on-brand-muted', 'brand', 4.5],
  ['brand-text', 'surface', 4.5],
  ['brand-text', 'canvas', 4.5],
  ['brand-text', 'brand-soft', 4.5],
  ['brand-text', 'select', 4.5],
  ['focus', 'surface', 3],
  ['focus', 'canvas', 3],
  ['focus', 'select', 3],
  ['focus-on-brand', 'brand', 3],
  ['fg', 'cell-pending', 7],
  ['fg', 'cell-error', 7],
  ['fg', 'select', 7],
  ['fg', 'cell-match', 7],
  ['fg', 'cell-match-strong', 7],
  ['fg', 'range', 7],
  ['focus', 'cell-match-strong', 3],
  ['match-edge', 'cell-match', 3],
  ['pending-text', 'cell-pending', 4.5],
  ['pending-edge', 'cell-pending', 3],
  ['pending-edge', 'surface', 3],
  ['error-text', 'cell-error', 4.5],
  ['error-text', 'surface', 4.5],
  ['error-edge', 'cell-error', 3],
  ['error-edge', 'surface', 3],
  ['match-edge', 'surface', 3],
  ['range-edge', 'surface', 3],
  ['fg', 'range-head', 4.5],
  ['success-text', 'surface', 4.5],
  ['on-danger', 'danger', 4.5],
  ['on-inverse', 'inverse', 4.5],
  ['on-photo', 'photo-mat', 7],
  ['on-photo-muted', 'photo-mat', 4.5],
  ['on-photo-warn', 'photo-mat', 4.5],
  ['cell-ai-text', 'cell-ai', 4.5],
  ['cell-ai-edge', 'surface', 3],
  ['cell-person-text', 'cell-person', 4.5],
  ['cell-person-edge', 'surface', 3],
  ['cell-sheet-edit-text', 'cell-sheet-edit', 4.5],
  ['cell-sheet-edit-edge', 'surface', 3],
]

describe('theme tokens', () => {
  it('define the same roles in light and dark', () => {
    expect(Object.keys(themes.dark).filter(k => k !== 'color-scheme').sort()).toEqual(Object.keys(themes.light).sort())
  })
  for (const theme of ['light', 'dark'] as const) {
    it(`meet their contrast bars in ${theme}`, () => {
      const fails = pairs
        .map(([fg, bg, bar]) => ({ pair: `${fg} on ${bg}`, ratio: +ratio(theme, fg, bg).toFixed(2), bar }))
        .filter(p => p.ratio < p.bar)
      expect(fails).toEqual([])
    })
  }
  it('keep the canvas darkest and each raised surface a step lighter in dark', () => {
    const l = (name: string) => lum(parse(themes.dark[name]))
    expect(l('canvas')).toBeLessThan(l('surface'))
    expect(l('surface')).toBeLessThan(l('surface-sunken'))
    expect(l('surface-sunken')).toBeLessThan(l('surface-raised'))
  })
  it('paint the page before the styles load with the canvas of each theme', () => {
    const html = readFileSync(resolve(import.meta.dirname, '../../../index.html'), 'utf8')
    expect(html).toContain(`background: ${themes.light.canvas}`)
    expect(html).toContain(`background: ${themes.dark.canvas}`)
  })
})
