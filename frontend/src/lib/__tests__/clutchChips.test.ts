import { describe, expect, it } from 'vitest'
import { preservedApart, readCount, todaySplit } from '../clutches'

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

describe('a count read with its groups', () => {
  it('one parenthesized sub-sum per group; the terms in order; the total of all', () => {
    const c = readCount('=(6-2)+(5+3)')
    expect(c.groups).toEqual([
      [6, -2],
      [5, 3],
    ])
    expect(c.terms).toEqual([6, -2, 5, 3])
    expect(c.text).toBeNull()
  })
  it('a plain sum or a number is one group; NA and text are not sums', () => {
    expect(readCount('=27-2-11-3').groups).toEqual([[27, -2, -11, -3]])
    expect(readCount(12).groups).toEqual([[12]])
    expect(readCount('NA')).toMatchObject({ na: true, groups: [] })
    expect(readCount('=(6)-(2)')).toMatchObject({ text: '=(6)-(2)', groups: [] })
  })
})

describe('preserved ones kept counted: shown apart from the sum', () => {
  const tallies = {
    larva: { gained: 20, died: 2, disappeared: 0, preserved: 3 },
    egg: { gained: 0, died: 0, disappeared: 0, preserved: 1 },
  }

  it("the team keeps them counted: the stage's preserved ones, beside its sum", () => {
    expect(preservedApart(tallies, 'larva', false)).toBe(3)
    expect(preservedApart(tallies, 'egg', false)).toBe(1)
  })

  it('the team takes them off: they are a − in the sum, nothing apart', () => {
    expect(preservedApart(tallies, 'larva', true)).toBe(0)
  })

  it('none preserved, no events, or no stage (the dissections): nothing apart', () => {
    expect(preservedApart(tallies, 'pupa', false)).toBe(0)
    expect(preservedApart(undefined, 'larva', false)).toBe(0)
    expect(preservedApart(tallies, null, false)).toBe(0)
  })
})
