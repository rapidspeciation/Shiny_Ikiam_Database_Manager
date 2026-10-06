import { describe, expect, it } from 'vitest'
import { chipEvents, rebaseChips, struckTerms, todaySplit, toggleStrike, type ClutchEvent } from '../clutches'

describe("a count's chips: tap one to strike it out of the sum, tap again to put it back", () => {
  const base = [2, 3, 9, 1, 8, 23]

  it('a chip struck out leaves the sum without it, in place', () => {
    const r = toggleStrike(base, [], 5)
    expect(r).toEqual({ ok: true, struck: [5], terms: [2, 3, 9, 1, 8] })
    expect(struckTerms(base, [5])).toEqual([2, 3, 9, 1, 8])
  })

  it('tapped again, it is back where it was', () => {
    const r = toggleStrike(base, [1, 5], 1)
    expect(r).toEqual({ ok: true, struck: [5], terms: [2, 3, 9, 1, 8] })
  })

  it('never a sum that starts with a loss or goes below 0', () => {
    expect(toggleStrike([10, -3], [], 0)).toEqual({ ok: false, reason: 'first' })
    expect(toggleStrike([4, 3, -6], [], 1)).toEqual({ ok: false, reason: 'negative' })
    // Every chip struck: the count is empty.
    expect(toggleStrike([4], [], 0)).toEqual({ ok: true, struck: [0], terms: [] })
  })

  it('the struck chips stay while the sum is the same or grows after them; anything else starts again', () => {
    // +5 added after striking the 3.
    expect(rebaseChips([2, 3, 9], [1], [2, 9, 5])).toEqual({ base: [2, 3, 9, 5], struck: [1] })
    // The same sum (the edit came back from the server).
    expect(rebaseChips([2, 3, 9], [1], [2, 9])).toEqual({ base: [2, 3, 9], struck: [1] })
    // Undo put the 3 back: no chip struck.
    expect(rebaseChips([2, 3, 9], [1], [2, 3, 9])).toEqual({ base: [2, 3, 9], struck: [] })
    // Another person's sum.
    expect(rebaseChips([2, 3, 9], [1], [7, 1])).toEqual({ base: [7, 1], struck: [] })
  })
})

describe("before and after today's changes", () => {
  it("this morning's sum at the start; today's terms after it", () => {
    expect(todaySplit([11, -1, 4], [11, -1, 4, 5, -4])).toEqual({ kept: 3, added: [5, -4], removed: [] })
  })
  it('a term of this morning taken out today', () => {
    expect(todaySplit([11, -1, 4, 5], [11, -1, 5])).toEqual({ kept: 2, added: [5], removed: [4, 5] })
  })
  it('nothing counted this morning: everything is today', () => {
    expect(todaySplit([], [12])).toEqual({ kept: 0, added: [12], removed: [] })
  })
})

describe('the event behind each chip (for its photos)', () => {
  const ev = (id: string, kind: ClutchEvent['kind'], count: number, day: string, at = '10:00'): Pick<ClutchEvent, 'id' | 'kind' | 'count' | 'stage' | 'day' | 'createdAt'> => ({
    id,
    kind,
    count,
    stage: 'larva',
    day,
    createdAt: `${day}T${at}:00Z`,
  })

  it('a + is the hatching of that many, a − a death or disappearance of that many', () => {
    const events = [ev('h1', 'hatched', 3, '2026-10-01'), ev('d1', 'died', 1, '2026-10-03'), ev('x1', 'disappeared', 2, '2026-10-04')]
    expect(chipEvents([20, 3, -1, -2], events, 'larva', false)).toEqual([null, 'h1', 'd1', 'x1'])
  })

  it('the same number twice: the newest chip takes the newest event', () => {
    const events = [ev('a', 'died', 1, '2026-10-01'), ev('b', 'died', 1, '2026-10-05')]
    expect(chipEvents([10, -1, -1], events, 'larva', false)).toEqual([null, 'a', 'b'])
  })

  it('preserved ones are a chip only when the team takes them off the count', () => {
    const events = [ev('p', 'preserved', 4, '2026-10-02')]
    expect(chipEvents([10, -4], events, 'larva', false)).toEqual([null, null])
    expect(chipEvents([10, -4], events, 'larva', true)).toEqual([null, 'p'])
  })

  it("another stage's events are not this count's", () => {
    expect(chipEvents([5], [{ ...ev('e', 'pupated', 5, '2026-10-02'), stage: 'pupa' }], 'larva', false)).toEqual([null])
  })
})
