/**
 * Which tabs the header shows in its row and which wait in «Más ▾». HIDDEN_TABS is the one list to change: tabs
 * not used day to day. A hidden tab still opens from its link (#/colecta) and shows in the row while it is open.
 * Then, when the row is too narrow (phones, a half screen), the last tabs that do not fit move into «Más» too,
 * never the open one, so no label is ever cut off and the row never scrolls.
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
