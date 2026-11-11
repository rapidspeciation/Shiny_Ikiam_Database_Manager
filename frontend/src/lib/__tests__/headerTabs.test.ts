import { describe, expect, it } from 'vitest'
import { HIDDEN_TABS, fitTabs, headerLayout, splitTabs } from '../headerTabs'

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

describe('headerLayout', () => {
  // The tabs a team member sees, with their widths (px) in Spanish on a phone: Fira Sans 14 px, as measured, plus
  // 12 px of padding; «Más» plain 54 px, or a tab's name and its ▾ (+16 px).
  const all = ['/inicio', '/tablas', '/colecta', '/monitoreo', '/muertes', '/tubos', '/emergidos', '/clutches', '/censo', '/historial', '/identificar', '/asistente', '/revision'].map(
    path => ({ path }),
  )
  const text: Record<string, number> = {
    '/inicio': 35, '/tablas': 60, '/colecta': 48, '/monitoreo': 65, '/muertes': 52, '/tubos': 37, '/emergidos': 67,
    '/clutches': 55, '/censo': 39, '/historial': 54, '/identificar': 65, '/asistente': 60, '/revision': 55,
  }
  const w = (t: { path: string }) => text[t.path]! + 12
  const lay = (active: string, room: number) => headerLayout(all, active, room, w, 54, t => w(t) + 16)
  /** A 360 px phone and a 390 px one: the row's room. */
  const P360 = 352
  const P390 = 382

  it('keeps the usual row where it fits whole (a laptop)', () => {
    const l = lay('/muertes', 1200)
    expect(paths(l.row)).toEqual(splitTabs(all, '/muertes').row.map(t => t.path))
    expect(l.moreLabel).toBeNull()
  })
  it('on a phone shows the same four daily tasks on every page, the rest in «Más»', () => {
    for (const room of [P360, P390])
      for (const t of all) {
        const l = lay(t.path, room)
        expect(paths(l.row)).toEqual(['/muertes', '/asistente', '/identificar', '/tablas'])
        expect([...l.row, ...l.menu]).toHaveLength(all.length)
      }
  })
  it('names the open page on «Más» when it waits there, cut short with «…» where the whole name does not fit', () => {
    expect(lay('/censo', P360)).toMatchObject({ moreLabel: { path: '/censo' }, moreActive: true, moreRoom: null })
    expect(lay('/inicio', P360).moreLabel?.path).toBe('/inicio')
    // Never a bare «Más» while the open page is in it: at 360 px Emergidos, Monitoreo, Historial keep Buscador.
    for (const path of ['/emergidos', '/monitoreo', '/historial']) {
      const l = lay(path, P360)
      expect(l.moreLabel?.path).toBe(path)
      expect(paths(l.row)).toEqual(['/muertes', '/asistente', '/identificar', '/tablas'])
      expect(l.moreRoom).toBe(P360 - (64 + 72 + 77 + 72))
    }
    // At 390 px every name fits.
    for (const t of all.filter(t => !['/muertes', '/asistente', '/identificar', '/tablas'].includes(t.path)))
      expect(lay(t.path, P390).moreLabel?.path).toBe(t.path)
    expect(lay('/muertes', P390)).toMatchObject({ moreLabel: null, moreActive: false, moreRoom: null })
    expect(lay('/emergidos', P390).moreRoom).toBeNull()
  })
  it('lists the daily tabs in «Más» before the hidden ones', () => {
    const l = lay('/muertes', P390)
    expect(paths(l.menu.slice(0, l.firstHidden))).toEqual(['/inicio', '/monitoreo', '/historial'])
    expect(paths(l.menu.slice(l.firstHidden))).toEqual(['/colecta', '/tubos', '/emergidos', '/clutches', '/censo', '/revision'])
  })
  it('only tabs the person can see: a visitor keeps the usual row', () => {
    const open = [{ path: '/inicio' }, { path: '/identificar' }]
    expect(paths(headerLayout(open, '/inicio', P360, w, 54, t => w(t) + 16).row)).toEqual(['/inicio', '/identificar'])
  })
  it('a narrower row still keeps the open daily task and drops the last ones', () => {
    expect(paths(lay('/tablas', 270).row)).toEqual(['/muertes', '/asistente', '/tablas'])
  })
})
