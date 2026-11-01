export type MoreSide = 'none' | 'start' | 'end' | 'both'

/** Which edges of a sideways-scrolling row hide more of it (ScrollRow.vue): 1 px of slack for rounding. */
export function moreSide(scrollLeft: number, clientWidth: number, scrollWidth: number): MoreSide {
  const before = scrollLeft > 1
  const after = scrollLeft + clientWidth < scrollWidth - 1
  return before && after ? 'both' : before ? 'start' : after ? 'end' : 'none'
}
