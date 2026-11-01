import { describe, expect, it } from 'vitest'
import { HIDDEN_TABS, fitTabs, splitTabs } from '../headerTabs'
import { nextPref } from '../theme'

const tabs = ['/inicio', '/tablas', '/colecta', '/monitoreo', '/muertes', '/tubos', '/emergidos', '/clutches', '/censo', '/historial', '/revision'].map(path => ({ path }))
const paths = (list: { path: string }[]) => list.map(t => t.path)

describe('splitTabs', () => {
  it('keeps the hidden tabs in «Más», in their order', () => {
    const { row, menu } = splitTabs(tabs, '/muertes')
    expect(paths(row)).toEqual(['/inicio', '/tablas', '/monitoreo', '/muertes', '/historial'])
    expect(paths(menu)).toEqual(HIDDEN_TABS)
  })
  it('shows an open hidden tab in the row, where it belongs, and not in the menu', () => {
    const { row, menu } = splitTabs(tabs, '/colecta')
    expect(paths(row)).toEqual(['/inicio', '/tablas', '/colecta', '/monitoreo', '/muertes', '/historial'])
    expect(paths(menu)).not.toContain('/colecta')
  })
  it('takes another list', () => {
    expect(paths(splitTabs(tabs, '/inicio', ['/historial']).menu)).toEqual(['/historial'])
  })
})

describe('fitTabs', () => {
  const w = [60, 80, 90, 70, 75]
  it('keeps them all when they fit beside «Más»', () => {
    expect(fitTabs(w, 500, 70, true)).toEqual([0, 1, 2, 3, 4])
  })
  it('keeps them all without «Más» when nothing else needs it', () => {
    expect(fitTabs(w, 380, 70, false)).toEqual([0, 1, 2, 3, 4])
  })
  it('moves the last ones into «Más» when the row is short', () => {
    expect(fitTabs(w, 280, 70, false)).toEqual([0, 1])
  })
  it('never moves the open tab, even when it is last', () => {
    expect(fitTabs(w, 300, 70, true, 4)).toEqual([0, 1, 4])
    expect(fitTabs(w, 100, 70, true, 4)).toEqual([4])
  })
})

describe('nextPref', () => {
  it('cycles System → Light → Dark → System', () => {
    expect(nextPref('system')).toBe('light')
    expect(nextPref('light')).toBe('dark')
    expect(nextPref('dark')).toBe('system')
  })
})
