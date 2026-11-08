/**
 * Which tabs the header shows in its row and which wait in «Más ▾». HIDDEN_TABS is the one list to change: tabs
 * not used day to day. A hidden tab still opens from its link (#/colecta) and shows in the row while it is open.
 * When the usual row does not fit (phones, a half screen), the row is PHONE_ROW instead (headerLayout): the same
 * tabs on every page, the rest in «Más», which then carries the open page's name. No label is ever cut off and the
 * row never scrolls.
 */
export const HIDDEN_TABS: readonly string[] = ['/colecta', '/tubos', '/emergidos', '/clutches', '/censo', '/revision']

/** The row's tabs (in their order, the open hidden one where it belongs) and the menu's (hidden ones after the rest). */
export function splitTabs<T extends { path: string }>(tabs: readonly T[], active: string | undefined, hidden: readonly string[] = HIDDEN_TABS) {
  const row = tabs.filter(t => !hidden.includes(t.path) || t.path === active)
  const menu = tabs.filter(t => hidden.includes(t.path) && t.path !== active)
  return { row, menu }
}

/**
 * How many of the row's tabs fit in `room` pixels, given each one's width: all of them when they fit with the
 * «Más» button (or without it, when `menuAnyway` is false and nothing is left over); otherwise as many as fit
 * beside «Más», in order, always keeping the open one (`active`, an index). Returns the indices kept.
 */
export function fitTabs(widths: readonly number[], room: number, moreWidth: number, menuAnyway: boolean, active = -1): number[] {
  const all = widths.map((_, i) => i)
  const total = widths.reduce((a, b) => a + b, 0)
  if (total <= room - (menuAnyway ? moreWidth : 0)) return all
  let left = room - moreWidth - (active >= 0 ? widths[active]! : 0)
  const kept: number[] = []
  for (const i of all) {
    if (i === active) {
      kept.push(i)
      continue
    }
    if (widths[i]! > left) break
    left -= widths[i]!
    kept.push(i)
  }
  if (active >= 0 && !kept.includes(active)) kept.push(active)
  return kept.sort((a, b) => a - b)
}

/**
 * The row when the usual one does not fit: the most used tasks, in this order (USAGE.md, 7 Nov 2026: Muertes 195
 * saves, Asistente 70 messages and 103 proposals applied, then Identificar and Buscador). Inicio is the logo.
 */
export const PHONE_ROW: readonly string[] = ['/muertes', '/asistente', '/identificar', '/tablas']

export interface HeaderLayout<T> {
  row: T[]
  menu: T[]
  /** Where the hidden tabs begin in the menu (a line above them). */
  firstHidden: number
  /** The open page when it is in «Más» and its name fits there: the button shows it («Censo ▾»). */
  moreLabel: T | null
  /** The open page is in «Más» (the button is marked as the open one, named or not). */
  moreActive: boolean
}

/**
 * The header's row and «Más» for `room` pixels. The usual row (every tab but the hidden ones, fitTabs) while it fits
 * whole beside «Más»; otherwise PHONE_ROW, whatever page is open: the open page, when it is not one of those, is
 * named on «Más» instead of pushing a daily tab out (on the narrowest phones a long name does not fit beside them:
 * «Más» is then only marked as the open one). `width` is a tab's width in the row, `moreAs` the width of «Más»
 * carrying that tab's name.
 */
export function headerLayout<T extends { path: string }>(
  tabs: readonly T[],
  active: string | undefined,
  room: number,
  width: (tab: T) => number,
  moreWidth: number,
  moreAs: (tab: T) => number,
  hidden: readonly string[] = HIDDEN_TABS,
  priority: readonly string[] = PHONE_ROW,
): HeaderLayout<T> {
  const total = (list: T[]) => list.reduce((n, t) => n + width(t), 0)
  const usual = splitTabs(tabs, undefined, hidden)
  const first = priority.flatMap(p => tabs.filter(t => t.path === p))
  if (!first.length || total(usual.row) <= room - (usual.menu.length ? moreWidth : 0)) {
    const split = splitTabs(tabs, active, hidden)
    const at = split.row.findIndex(t => t.path === active)
    const kept = fitTabs(split.row.map(width), room, moreWidth, split.menu.length > 0, at)
    const row = split.row.filter((_, i) => kept.includes(i))
    const left = split.row.filter((_, i) => !kept.includes(i))
    return { row, menu: [...left, ...split.menu], firstHidden: left.length, moreLabel: null, moreActive: false }
  }
  const open = tabs.find(t => t.path === active)
  const outside = !!open && !first.includes(open)
  const at = first.findIndex(t => t.path === active)
  const sizes = first.map(width)
  let kept = fitTabs(sizes, room, outside ? Math.max(moreWidth, moreAs(open!)) : moreWidth, true, at)
  let named = outside
  if (outside && kept.length < first.length) {
    // The name does not fit beside the daily tabs: «Más» unnamed rather than one of them in the menu.
    const plain = fitTabs(sizes, room, moreWidth, true, at)
    if (plain.length > kept.length) [kept, named] = [plain, false]
  }
  const row = first.filter((_, i) => kept.includes(i))
  const rest = tabs.filter(t => !row.includes(t))
  const daily = rest.filter(t => !hidden.includes(t.path))
  return {
    row,
    menu: [...daily, ...rest.filter(t => hidden.includes(t.path))],
    firstHidden: daily.length,
    moreLabel: named ? open! : null,
    moreActive: outside,
  }
}
