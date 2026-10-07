import { describe, expect, it } from 'vitest'
import {
  adoption,
  alignTerms,
  countState,
  formulaOfGroups,
  groupsOf,
  leftOf,
  nextLabel,
  parseCounts,
  parseFormula,
  parseTerms,
  planCorrection,
  planFromTerm,
  planMoveOn,
  planRegroup,
  planRemove,
  planSplit,
  planTerm,
  regroupTargets,
  toggleSelection,
  type GroupRow,
  type Plan,
} from '../clutchGroups'
import { simpleSum, sumTotal } from '../sums'

const row = (id: string, field: string, position: number, label: string | null, originId: string | null = null): GroupRow => ({
  id,
  field,
  stage: field === 'NUMBER OF EGGS' ? 'egg' : field === 'NUMBER OF LARVAE' ? 'larva' : 'pupa',
  position,
  label,
  originId,
})
const formulaOf = (plan: Plan, field: keyof Plan['counts']) => formulaOfGroups(plan.counts[field]!)
const ok = <T extends { ok: boolean }>(r: T) => {
  if (!r.ok) throw new Error(`refused: ${JSON.stringify(r)}`)
  return r as Extract<T, { ok: true }>
}

describe('typing terms: a space is a plus, «-» a minus', () => {
  it('27 3 5 → 27+3+5; 27 -3 and 27-3 → 27−3; =12+15 as written', () => {
    expect(parseTerms('27 3 5')).toEqual({ ok: true, terms: [27, 3, 5] })
    expect(parseTerms('27 -3')).toEqual({ ok: true, terms: [27, -3] })
    expect(parseTerms('27-3')).toEqual({ ok: true, terms: [27, -3] })
    expect(parseTerms('27 − 3')).toEqual({ ok: true, terms: [27, -3] })
    expect(parseTerms('=12+15')).toEqual({ ok: true, terms: [12, 15] })
  })
  it('two spaces a phone turned into ". " are still a plus', () => {
    expect(parseTerms('27. 3')).toEqual({ ok: true, terms: [27, 3] })
  })
  it('refused: nothing, letters, a first term that is a loss', () => {
    expect(parseTerms('  ')).toEqual({ ok: false, reason: 'empty' })
    expect(parseTerms('12 a')).toEqual({ ok: false, reason: 'invalid' })
    expect(parseTerms('-3 5')).toEqual({ ok: false, reason: 'first' })
  })
  it('counts only (a regroup «6 5»): no minus', () => {
    expect(parseCounts('6 5')).toEqual([6, 5])
    expect(parseCounts('6+5')).toEqual([6, 5])
    expect(parseCounts('6 -5')).toBeNull()
    expect(regroupTargets('6 5', 11)).toEqual({ ok: true, targets: [6, 5] })
    expect(regroupTargets('6 4', 11)).toEqual({ ok: false, reason: 'sum', sum: 10 })
  })
})

describe('formulas with groups: one parenthesized sub-sum per group', () => {
  it('read and written back; one group is the plain sum', () => {
    expect(groupsOf('=(6-2)+(5+3)')).toEqual([
      [6, -2],
      [5, 3],
    ])
    expect(groupsOf('=27-2-11-3')).toEqual([[27, -2, -11, -3]])
    expect(groupsOf('=(6)-(2)')).toBeNull()
    expect(formulaOfGroups([[6, -2], [5, 3]])).toBe('=(6-2)+(5+3)')
    expect(formulaOfGroups([[27, -2, -11, -3]])).toBe('=27-2-11-3')
    expect(formulaOfGroups([])).toBeNull()
  })
  it('the sheet-side parser and total accept them too', () => {
    expect(simpleSum('( 6 - 2 ) + ( 5 + 3 )')).toBe('=(6-2)+(5+3)')
    expect(sumTotal('=(6-2)+(5+3)')).toBe(12)
    expect(simpleSum('=(6-2)-(5)')).toBeNull()
  })
  it('a formula typed by hand: spaces are plus inside a group, groups side by side', () => {
    expect(parseFormula('(6 -2) (5 3)')).toEqual({ ok: true, groups: [[6, -2], [5, 3]] })
    expect(parseFormula('=(6-2)+(5+3)')).toEqual({ ok: true, groups: [[6, -2], [5, 3]] })
    expect(parseFormula('27 -2 -11')).toEqual({ ok: true, groups: [[27, -2, -11]] })
    expect(parseFormula('(5 -7)')).toEqual({ ok: false, reason: 'negative' })
    expect(parseFormula('(5 3')).toEqual({ ok: false, reason: 'invalid' })
    expect(parseFormula('(-2 5)')).toEqual({ ok: false, reason: 'first' })
  })
})

describe('the event behind each term', () => {
  const ev = (id: string, term: number, groupId: string | null, at: string) => ({ id, term, groupId, createdAt: `2026-10-0${at}T10:00:00Z` })
  it("each group's terms with its own events, newest back; a term without one is the sheet's", () => {
    const groups = [
      [6, -2],
      [5, 3],
    ]
    const events = [ev('h6', 6, 'A', '1'), ev('d2', -2, 'A', '2'), ev('h3', 3, 'B', '3'), ev('x', 5, 'A', '4')]
    expect(alignTerms(groups, events, ['A', 'B'])).toEqual([
      ['h6', 'd2'],
      [null, 'h3'],
    ])
  })
  it('equal terms told apart by where each sits (its place in the parentheses)', () => {
    const at = (id: string, term: number, termAt: number, day: string) => ({ ...ev(id, term, null, day), termAt })
    // The second +5 is newer, but the first one's event says it is the first.
    expect(alignTerms([[5, 5]], [at('first', 5, 0, '2'), ev('second', 5, null, '1')], [null])).toEqual([['first', 'second']])
    // A place that no longer holds its value is only a hint: matched by value then.
    expect(alignTerms([[5, 3]], [at('x', 5, 1, '1')], [null])).toEqual([['x', null]])
  })
  it('the first group also takes the events recorded before there were groups', () => {
    expect(alignTerms([[10, -1, -1]], [ev('a', -1, null, '1'), ev('b', -1, null, '2')], [null])).toEqual([[null, 'a', 'b']])
  })
})

describe('selecting groups', () => {
  it('a tap toggles one; several at once; `only` keeps one', () => {
    expect(toggleSelection([], 1)).toEqual([1])
    expect(toggleSelection([1], 0)).toEqual([0, 1])
    expect(toggleSelection([0, 1], 1)).toEqual([0])
    expect(toggleSelection([0, 1], 1, true)).toEqual([1])
    expect(nextLabel(['A', 'C'])).toBe('B')
  })
})

describe('steps written into the groups', () => {
  const larvae = (formula: string, rows: GroupRow[] = []) => countState('NUMBER OF LARVAE', 'larva', groupsOf(formula)!, rows)
  const AB = [row('A', 'NUMBER OF LARVAE', 0, 'A'), row('B', 'NUMBER OF LARVAE', 1, 'B')]

  it('a loss in the group selected goes inside its parentheses, with its group', () => {
    const r = ok(planTerm(larvae('=(6)+(5+3)', AB), 0, { stage: 'larva', kind: 'died', count: 2, term: -2 }))
    expect(formulaOf(r.plan, 'NUMBER OF LARVAE')).toBe('=(6-2)+(5+3)')
    expect(r.plan.events).toEqual([{ stage: 'larva', kind: 'died', count: 2, term: -2, groupId: 'A', termAt: 1 }])
  })
  it('no group said: the last one; one group: the plain sum', () => {
    expect(formulaOf(ok(planTerm(larvae('=(6)+(5)', AB), null, { stage: 'larva', kind: 'hatched', count: 3, term: 3 })).plan, 'NUMBER OF LARVAE')).toBe('=(6)+(5+3)')
    expect(formulaOf(ok(planTerm(larvae('=27-2'), null, { stage: 'larva', kind: 'died', count: 1, term: -1 })).plan, 'NUMBER OF LARVAE')).toBe('=27-2-1')
  })
  it('a group cannot go below 0; preserved ones kept counted write no term', () => {
    expect(planTerm(larvae('=(6)+(5)', AB), 1, { stage: 'larva', kind: 'died', count: 9, term: -9 })).toEqual({ ok: false, reason: 'negative' })
    const kept = ok(planTerm(larvae('=(6)+(5)', AB), 1, { stage: 'larva', kind: 'preserved', count: 2, term: null }))
    expect(kept.plan.counts).toEqual({})
    expect(kept.plan.events[0]).toMatchObject({ kind: 'preserved', groupId: 'B', term: null })
  })
  it('a corrected total is its own term: counted 10, not 11 → −1, in the group chosen', () => {
    const r = ok(planCorrection(larvae('=27-2-11-3'), null, 10, 'recount'))
    expect(formulaOf(r.plan, 'NUMBER OF LARVAE')).toBe('=27-2-11-3-1')
    expect(r.plan.events[0]).toMatchObject({ kind: 'correction', count: 1, term: -1, note: 'recount' })
    expect(formulaOf(ok(planCorrection(larvae('=(6)+(5)', AB), 0, 12)).plan, 'NUMBER OF LARVAE')).toBe('=(6+1)+(5)')
    expect(planCorrection(larvae('=11'), null, 11)).toEqual({ ok: false, reason: 'unchanged' })
  })
  it('a term taken out; a group left empty goes, its place too', () => {
    const r = ok(planRemove(larvae('=(6-2)+(5)', AB), 1, 0))
    expect(formulaOfGroups(r.groups)).toBe('=6-2')
    expect(r.list).toEqual([{ id: 'A', label: 'A' }])
    expect(planRemove(larvae('=(6-2)+(5)', AB), 0, 0)).toEqual({ ok: false, reason: 'first' })
  })
})

describe('regrouping, written as transfers', () => {
  const larvae = (formula: string, rows: GroupRow[] = []) => countState('NUMBER OF LARVAE', 'larva', groupsOf(formula)!, rows)
  const AB = [row('A', 'NUMBER OF LARVAE', 0, 'A'), row('B', 'NUMBER OF LARVAE', 1, 'B')]

  it('«6+5» from one group of 11: the terms wrapped in its parentheses, 5 moved to a new group', () => {
    const r = ok(planRegroup(larvae('=27-2-11-3'), [6, 5]))
    expect(formulaOf(r.plan, 'NUMBER OF LARVAE')).toBe('=(27-2-11-3-5)+(5)')
    expect(r.plan.groups[0].list.map(g => g.label)).toEqual(['A', 'B'])
    const [out, into] = r.plan.events
    expect(out).toMatchObject({ kind: 'transfer', count: 5, term: -5, groupKey: r.plan.groups[0].list[0].key })
    expect(into).toMatchObject({ kind: 'transfer', count: 5, term: 5, groupKey: r.plan.groups[0].list[1].key, fromGroupKey: r.plan.groups[0].list[0].key })
    expect(r.plan.log).toEqual([{ kind: 'regroup', field: 'NUMBER OF LARVAE', before: '=27-2-11-3', after: '=(27-2-11-3-5)+(5)' }])
  })
  it('2 of A split into a group of their own, right after it', () => {
    const r = ok(planSplit(larvae('=(6-2)+(5+3)', AB), 0, 2))
    expect(formulaOf(r.plan, 'NUMBER OF LARVAE')).toBe('=(6-2-2)+(2)+(5+3)')
    expect(r.plan.groups[0].list).toEqual([{ id: 'A', label: 'A' }, expect.objectContaining({ label: 'C' }), { id: 'B', label: 'B' }])
  })
  it("B merged into A: B's total into A's parentheses, B's go", () => {
    const r = ok(planRegroup(larvae('=(6-2)+(5+3)', AB), [12]))
    expect(formulaOf(r.plan, 'NUMBER OF LARVAE')).toBe('=6-2+8')
    expect(r.plan.groups[0].list).toEqual([{ id: 'A', label: 'A' }])
    expect(r.plan.events).toEqual([{ stage: 'larva', kind: 'transfer', count: 8, term: 8, groupId: 'A', fromGroupId: 'B' }])
  })
  it('the total stays; anything else is refused', () => {
    const r = ok(planRegroup(larvae('=(6-2)+(5+3)', AB), [6, 6]))
    expect(formulaOf(r.plan, 'NUMBER OF LARVAE')).toBe('=(6-2+2)+(5+3-2)')
    expect(planRegroup(larvae('=(6-2)+(5+3)', AB), [6, 5])).toEqual({ ok: false, reason: 'sum' })
    expect(planRegroup(larvae('=(6-2)+(5+3)', AB), [4, 8])).toEqual({ ok: false, reason: 'unchanged' })
  })
})

describe('hatched (pupated) from groups', () => {
  const eggs = countState('NUMBER OF EGGS', 'egg', [[10], [8]], [row('EA', 'NUMBER OF EGGS', 0, 'A'), row('EB', 'NUMBER OF EGGS', 1, 'box B')])
  it('each egg group selected gives larvae to a group that mirrors it; leftover eggs did not hatch', () => {
    const r = ok(
      planMoveOn(eggs, countState('NUMBER OF LARVAE', 'larva', [], []), [
        { index: 0, count: 8, notHatched: 2 },
        { index: 1, count: 8 },
      ]),
    )
    expect(formulaOf(r.plan, 'NUMBER OF LARVAE')).toBe('=(8)+(8)')
    const list = r.plan.groups[0].list
    expect(list.map(g => [g.label, g.originId])).toEqual([
      ['A', 'EA'],
      ['box B', 'EB'],
    ])
    expect(r.plan.events).toEqual([
      expect.objectContaining({ stage: 'larva', kind: 'hatched', count: 8, term: 8, groupKey: list[0].key, fromGroupId: 'EA' }),
      { stage: 'egg', kind: 'not_hatched', count: 2, term: null, groupId: 'EA' },
      expect.objectContaining({ stage: 'larva', kind: 'hatched', count: 8, term: 8, groupKey: list[1].key, fromGroupId: 'EB' }),
    ])
  })
  it('a later hatch from the same eggs goes into the same larvae group', () => {
    const larvae = countState('NUMBER OF LARVAE', 'larva', [[8], [8]], [row('LA', 'NUMBER OF LARVAE', 0, 'A', 'EA'), row('LB', 'NUMBER OF LARVAE', 1, 'box B', 'EB')])
    const r = ok(planMoveOn(eggs, larvae, [{ index: 0, count: 2 }], { day: '2026-10-05' }))
    expect(formulaOf(r.plan, 'NUMBER OF LARVAE')).toBe('=(8+2)+(8)')
    expect(r.plan.groups).toEqual([])
    expect(r.plan.events[0]).toMatchObject({ groupId: 'LA', fromGroupId: 'EA', day: '2026-10-05' })
  })
  it('eggs without groups to larvae without groups: the plain +N; the hatch day may be unknown', () => {
    const plain = countState('NUMBER OF EGGS', 'egg', [[12]], [])
    const r = ok(planMoveOn(plain, countState('NUMBER OF LARVAE', 'larva', [[3]], []), [{ index: 0, count: 9 }], { dayKnown: false }))
    expect(formulaOf(r.plan, 'NUMBER OF LARVAE')).toBe('=3+9')
    expect(r.plan.events[0]).toMatchObject({ kind: 'hatched', term: 9, groupId: null, dayKnown: false })
  })
  it('a new group beside larvae counted without groups: those are wrapped as the first group', () => {
    const r = ok(planMoveOn(eggs, countState('NUMBER OF LARVAE', 'larva', [[3, -1]], []), [{ index: 1, count: 5 }]))
    expect(formulaOf(r.plan, 'NUMBER OF LARVAE')).toBe('=(3-1)+(5)')
    expect(r.plan.groups[0].list.map(g => g.label)).toEqual(['A', 'box B'])
  })
  it('what is left of each group: its total less those hatched from it and those that did not hatch', () => {
    const events = [
      { stage: 'larva' as const, kind: 'hatched' as const, count: 6, groupId: null, fromGroupId: 'EA' },
      { stage: 'egg' as const, kind: 'not_hatched' as const, count: 1, groupId: 'EB', fromGroupId: null },
    ]
    expect(leftOf(eggs, events, 'larva')).toEqual([4, 7])
  })
  it('nothing chosen: refused', () => {
    expect(planMoveOn(eggs, countState('NUMBER OF LARVAE', 'larva', [], []), [{ index: 0, count: 0 }])).toEqual({ ok: false, reason: 'empty' })
  })
})

describe('a term written before the app: its event, and a loss taken from it', () => {
  const HATCH = 46290 // 25-Sep-2026
  const plain = countState('NUMBER OF LARVAE', 'larva', groupsOf('=27-2-11-3')!, [])
  it('the first + term gets the stage’s first date; the others and the minus terms an unknown day; the formula is not touched', () => {
    expect(adoption(plain, 0, 0, HATCH)).toEqual({
      stage: 'larva',
      kind: 'hatched',
      count: 27,
      term: 27,
      groupId: null,
      termAt: 0,
      adopted: true,
      dayKnown: true,
      day: '2026-09-25',
    })
    expect(adoption(plain, 0, 1, HATCH)).toMatchObject({ kind: 'correction', count: 2, term: -2, termAt: 1, dayKnown: false })
    expect(adoption(plain, 0, 1, HATCH)).not.toHaveProperty('day')
    const two = countState('NUMBER OF EGGS', 'egg', groupsOf('=12+15')!, [])
    expect(adoption(two, 0, 1, HATCH)).toMatchObject({ kind: 'laid', term: 15, dayKnown: false })
    expect(adoption(two, 0, 0, null)).toMatchObject({ kind: 'laid', term: 12, dayKnown: false })
  })
  it('without groups, the loss goes at the end of the plain sum, linked to the term (=27-2-11-3 → =27-2-11-3-1)', () => {
    const r = ok(planFromTerm(plain, 0, 0, 'e27', { kind: 'died', count: 1, term: -1 }))
    expect(formulaOf(r.plan, 'NUMBER OF LARVAE')).toBe('=27-2-11-3-1')
    expect(r.plan.events).toEqual([{ stage: 'larva', kind: 'died', count: 1, term: -1, groupId: null, termAt: 4, ofEventId: 'e27' }])
  })
  it('with groups, inside the parentheses of the group holding the term', () => {
    const s = countState('NUMBER OF LARVAE', 'larva', groupsOf('=(6-2)+(5+3)')!, [row('A', 'NUMBER OF LARVAE', 0, 'A'), row('B', 'NUMBER OF LARVAE', 1, 'B')])
    const r = ok(planFromTerm(s, 1, 0, 'e5', { kind: 'disappeared', count: 2, term: -2 }))
    expect(formulaOf(r.plan, 'NUMBER OF LARVAE')).toBe('=(6-2)+(5+3-2)')
    expect(r.plan.events[0]).toMatchObject({ groupId: 'B', ofEventId: 'e5', termAt: 2 })
  })
  it('preserved ones kept counted are linked without a term; never more than the term has left', () => {
    const kept = ok(planFromTerm(plain, 0, 0, 'e27', { kind: 'preserved', count: 3, term: null }))
    expect(kept.plan.counts).toEqual({})
    expect(kept.plan.events[0]).toMatchObject({ kind: 'preserved', ofEventId: 'e27', term: null })
    expect(planFromTerm(plain, 0, 0, 'e27', { kind: 'died', count: 3, term: -3 }, 25)).toEqual({ ok: false, reason: 'negative' })
    expect(planFromTerm(plain, 0, 1, 'e2', { kind: 'died', count: 1, term: -1 })).toEqual({ ok: false, reason: 'first' })
  })
})
