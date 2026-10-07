import { describe, expect, it } from 'vitest'
import type { DeathChoice } from '../deaths'
import {
  DEFAULT_ORDER,
  addCards,
  addPhraseTo,
  applyToAll,
  cardsForIds,
  clickCard,
  commonChoice,
  compareIds,
  differing,
  focusAfterPick,
  keepDefaults,
  namedIds,
  nextDefaults,
  readDefaults,
  readOrder,
  recordedOn,
  removeCards,
  setCardField,
  sortRecorded,
  type HistoryAction,
} from '../deathsCart'

const heat: DeathChoice = { date: '2026-10-06', cause: 'Heat stroke', preserved: false, note: '' }
const unknown: DeathChoice = { date: '2026-10-05', cause: 'Unknown', preserved: false, note: '' }

describe('«Seleccionadas»: each butterfly with its own death', () => {
  it('picked IDs get a copy of the panel\'s values; one already there keeps its own', () => {
    const one = addCards([], ['G7D'], heat)
    expect(one).toEqual({ cards: [{ id: 'G7D', choice: heat }], added: ['G7D'] })
    // A copy: changing the panel afterwards does not change the card.
    expect(one.cards[0].choice).not.toBe(heat)
    const edited = setCardField(one.cards, 'G7D', 'cause', 'Eaten')
    const more = addCards(edited, ['g7d', 'A1E', 'A2E', 'A1E'], unknown)
    expect(more.added).toEqual(['A1E', 'A2E'])
    expect(more.cards.map(c => [c.id, c.choice.cause])).toEqual([
      ['G7D', 'Eaten'],
      ['A1E', 'Unknown'],
      ['A2E', 'Unknown'],
    ])
  })

  it('choose the cause once, then pick ten IDs: all ten carry it', () => {
    const ids = Array.from({ length: 10 }, (_, i) => `B${i}D`)
    const { cards } = addCards([], ids, heat)
    expect(cards).toHaveLength(10)
    expect(cards.every(c => c.choice.cause === 'Heat stroke' && c.choice.date === '2026-10-06')).toBe(true)
  })

  it('a card\'s value changes that card only; × takes it out without touching the rest', () => {
    const { cards } = addCards([], ['A1E', 'A2E'], heat)
    const next = setCardField(cards, 'a2e', 'preserved', true)
    expect(next[0].choice.preserved).toBe(false)
    expect(next[1].choice.preserved).toBe(true)
    expect(removeCards(next, ['A1E']).map(c => c.id)).toEqual(['A2E'])
    expect(removeCards(next, ['nope'])).toHaveLength(2)
  })

  it('the panel\'s value given to the cards that differ', () => {
    let { cards } = addCards([], ['A1E', 'A2E'], unknown)
    cards = setCardField(cards, 'A1E', 'cause', 'Heat stroke')
    expect(differing(cards, heat, 'cause')).toEqual(['A2E'])
    const all = applyToAll(cards, 'cause', 'Heat stroke')
    expect(all.map(c => c.choice.cause)).toEqual(['Heat stroke', 'Heat stroke'])
    // A card already so is the same object.
    expect(all[0]).toBe(cards[0])
    expect(differing(all, heat, 'cause')).toEqual([])
  })

  it('the table\'s list of IDs: cards kept with their values, new ones with the panel\'s, in its order', () => {
    const { cards } = addCards([], ['A1E', 'A2E'], unknown)
    const next = cardsForIds(setCardField(cards, 'A2E', 'cause', 'Eaten'), ['A3E', 'a2e', 'A3E'], heat)
    expect(next.map(c => [c.id, c.choice.cause])).toEqual([
      ['A3E', 'Heat stroke'],
      ['A2E', 'Eaten'],
    ])
  })

  it('the panel shows the lone butterfly picked, or one picked again; several, or one more beside others, keep the panel\'s values', () => {
    const one = addCards([], ['G7D'], heat).cards
    expect(focusAfterPick([], one, ['G7D'], false)).toBe('G7D')
    expect(focusAfterPick([], one, ['g7d'], false)).toBe('G7D')
    // «Seleccionar varias», a list or a range.
    expect(focusAfterPick([], one, ['G7D'], true)).toBeNull()
    const two = addCards(one, ['A1E'], heat).cards
    expect(focusAfterPick(one, two, ['A1E', 'G7D'], false)).toBeNull()
    expect(focusAfterPick(one, two, ['A1E'], false)).toBeNull()
    // Picked again while others are there: that one, to change it.
    expect(focusAfterPick(two, two, ['g7d'], false)).toBe('G7D')
    expect(focusAfterPick(two, two, ['NOPE'], false)).toBeNull()
  })
})

describe('several cards in the panel', () => {
  // The cards as shown, newest first.
  const order = ['G9D', 'G8D', 'G7D', 'A1E', 'A2E']
  const none = { ids: [], anchor: null }

  it('a plain click opens that card alone; on the card already open it stays open and pulses', () => {
    const one = clickCard(none, order, 'g8d')
    expect(one).toEqual({ ids: ['G8D'], anchor: 'G8D', again: false })
    expect(clickCard(one, order, 'G8D')).toEqual({ ids: ['G8D'], anchor: 'G8D', again: true })
    // One of several: that one alone, no pulse.
    expect(clickCard({ ids: ['G8D', 'G7D'], anchor: 'G7D' }, order, 'G8D')).toEqual({ ids: ['G8D'], anchor: 'G8D', again: false })
    // Not a card (gone since): nothing changes.
    expect(clickCard(one, order, 'NOPE')).toEqual({ ...one, again: false })
  })

  it('Ctrl/⌘+click adds or takes out one card, kept in the cards\' order', () => {
    let s = clickCard(none, order, 'A1E')
    s = clickCard(s, order, 'G9D', { toggle: true })
    expect(s.ids).toEqual(['G9D', 'A1E'])
    s = clickCard(s, order, 'G8D', { toggle: true })
    expect(s.ids).toEqual(['G9D', 'G8D', 'A1E'])
    s = clickCard(s, order, 'g9d', { toggle: true })
    expect(s).toEqual({ ids: ['G8D', 'A1E'], anchor: 'G9D', again: false })
    expect(clickCard(clickCard(none, order, 'A1E'), order, 'A1E', { toggle: true }).ids).toEqual([])
  })

  it('Shift+click: the cards from the last one clicked to this one, either way; with Ctrl, beside the others', () => {
    const start = clickCard(none, order, 'G8D')
    expect(clickCard(start, order, 'A1E', { range: true })).toEqual({ ids: ['G8D', 'G7D', 'A1E'], anchor: 'G8D', again: false })
    expect(clickCard(start, order, 'G9D', { range: true }).ids).toEqual(['G9D', 'G8D'])
    // Shift again counts from the same card.
    const range = clickCard(start, order, 'A1E', { range: true })
    expect(clickCard(range, order, 'G7D', { range: true }).ids).toEqual(['G8D', 'G7D'])
    // Ctrl+Shift keeps what was there.
    const apart = clickCard(clickCard(none, order, 'A2E'), order, 'G9D', { toggle: true })
    expect(clickCard(apart, order, 'G7D', { range: true, toggle: true }).ids).toEqual(['G9D', 'G8D', 'G7D', 'A2E'])
    // Nothing clicked before: a plain click.
    expect(clickCard(none, order, 'G7D', { range: true })).toEqual({ ids: ['G7D'], anchor: 'G7D', again: false })
  })

  it('the panel shows each field they share, and which differ («varios»)', () => {
    let cards = addCards([], ['A1E', 'A2E', 'G7D'], heat).cards
    expect(commonChoice(cards, ['A1E', 'a2e'])).toEqual({ choice: heat, mixed: [] })
    cards = setCardField(cards, 'A2E', 'date', '2026-10-05')
    cards = setCardField(cards, 'A2E', 'note', 'Head eaten')
    const { choice, mixed } = commonChoice(cards, ['A1E', 'A2E'])
    expect(mixed).toEqual(['date', 'note'])
    expect(choice).toEqual({ date: '', cause: 'Heat stroke', preserved: false, note: '' })
    // G7D not selected: its values do not count.
    expect(commonChoice(cards, ['A1E', 'G7D']).mixed).toEqual([])
    expect(commonChoice(cards, []).mixed).toEqual([])
  })

  it('a change in the panel goes to every card selected, and only to them', () => {
    let cards = addCards([], ['A1E', 'A2E', 'G7D'], heat).cards
    cards = setCardField(cards, 'A2E', 'note', 'Head eaten')
    const set = setCardField(cards, ['a1e', 'A2E'], 'cause', 'Eaten')
    expect(set.map(c => c.choice.cause)).toEqual(['Eaten', 'Eaten', 'Heat stroke'])
    expect(setCardField(set, ['A1E', 'A2E'], 'preserved', true).map(c => c.choice.preserved)).toEqual([true, true, false])
    // A quick phrase after each one's own note; the note typed replaces them all.
    const phrased = addPhraseTo(set, ['A1E', 'A2E'], 'With fungi')
    expect(phrased.map(c => c.choice.note)).toEqual(['With fungi', 'Head eaten; With fungi', ''])
    expect(setCardField(phrased, ['A1E', 'A2E'], 'note', 'Only wings found').map(c => c.choice.note)).toEqual([
      'Only wings found',
      'Only wings found',
      '',
    ])
    expect(commonChoice(setCardField(phrased, ['A1E', 'A2E'], 'note', 'x'), ['A1E', 'A2E']).mixed).toEqual([])
  })

  it('the title names two IDs and counts the rest', () => {
    expect(namedIds(['G7D'])).toEqual({ shown: ['G7D'], more: 0 })
    expect(namedIds(['G7D', 'G8D'])).toEqual({ shown: ['G7D', 'G8D'], more: 0 })
    expect(namedIds(['G7D', 'G8D', 'A1E', 'A2E', 'A3E'])).toEqual({ shown: ['G7D', 'G8D'], more: 3 })
  })
})

describe('the values for the next butterflies follow the last death recorded', () => {
  const eaten: DeathChoice = { date: '2026-10-05', cause: 'Eaten', preserved: true, note: 'Head eaten' }

  it('after recording, the next ones start with that death; a group that differed, with its last card\'s', () => {
    expect(nextDefaults([eaten], heat)).toEqual(eaten)
    expect(nextDefaults([eaten], heat)).not.toBe(eaten)
    expect(nextDefaults([heat, eaten], unknown)).toEqual(eaten)
    // Nothing recorded (all refused): as they were.
    expect(nextDefaults([], heat)).toBe(heat)
    // A run of the same cause: picked after, a card carries it with no tap.
    const next = nextDefaults([eaten], heat)
    expect(addCards([], ['B1D'], next).cards[0].choice).toEqual(eaten)
  })

  it('kept in the browser with its day: the same day the date stays, another day it is today\'s', () => {
    const kept = keepDefaults(eaten, '2026-10-06')
    expect(kept).toEqual({ ...eaten, day: '2026-10-06' })
    expect(readDefaults(kept, '2026-10-06')).toEqual(eaten)
    expect(readDefaults(kept, '2026-10-07')).toEqual({ ...eaten, date: '2026-10-07' })
    // The date emptied on purpose stays empty that day.
    expect(readDefaults(keepDefaults({ ...eaten, date: '' }, '2026-10-06'), '2026-10-06').date).toBe('')
    // Kept before the day was (cause, preservation and note only), or nothing: today, empty.
    expect(readDefaults({ cause: 'Eaten', preserved: false, note: '' }, '2026-10-06')).toEqual({
      date: '2026-10-06',
      cause: 'Eaten',
      preserved: false,
      note: '',
    })
    expect(readDefaults(null, '2026-10-06')).toEqual({ date: '2026-10-06', cause: '', preserved: false, note: '' })
  })
})

describe('«Registradas hoy»', () => {
  const change = (id: string, recordId: string, label: string, field = 'Death_date', sheet = 'Insectary_data') => ({
    id,
    recordId,
    label,
    sheet,
    field,
    before: null,
    after: 1,
  })
  const action = (over: Partial<HistoryAction>): HistoryAction => ({
    id: 'a',
    actor: 'u1',
    actorName: 'Franz',
    createdAt: '2026-10-06T15:00:00.000Z',
    status: 'verified',
    purpose: 'muertes',
    changes: [],
    ...over,
  })

  it('one line per butterfly saved from Muertes that day (Ecuador\'s day), with all its changes and who saved them', () => {
    const actions = [
      action({ id: 'a2', actor: 'u2', actorName: 'Ana', createdAt: '2026-10-06T18:00:00.000Z', changes: [change('c4', 'r1', 'G7D', 'Death_cause')] }),
      action({ id: 'a1', changes: [change('c1', 'r1', 'G7D'), change('c2', 'r1', 'G7D', 'Death_cause'), change('c3', 'r2', 'A1E')] }),
      // 23:30 in Ecuador on the 5th is 04:30 UTC on the 6th: yesterday's.
      action({ id: 'old', createdAt: '2026-10-06T04:30:00.000Z', changes: [change('c5', 'r3', 'B2E')] }),
      // 21:00 in Ecuador on the 6th is the 7th in UTC: today's.
      action({ id: 'late', createdAt: '2026-10-07T02:00:00.000Z', changes: [change('c6', 'r4', 'C3E')] }),
      action({ id: 'other', purpose: 'tubos', changes: [change('c7', 'r5', 'D4E')] }),
      action({ id: 'failed', status: 'failed', changes: [change('c8', 'r6', 'E5E')] }),
      action({ id: 'clutch', changes: [change('c9', 'r7', '12', 'NOTES', 'Insectary_stocks')] }),
    ]
    const out = recordedOn(actions, '2026-10-06')
    expect(out.map(r => r.label)).toEqual(['G7D', 'A1E', 'C3E'])
    expect(out[0]).toEqual({
      recordId: 'r1',
      label: 'G7D',
      at: '2026-10-06T18:00:00.000Z',
      actors: [
        { id: 'u1', name: 'Franz' },
        { id: 'u2', name: 'Ana' },
      ],
      changeIds: ['c1', 'c2', 'c4'],
    })
  })

  it('sorts by Insectary ID, emergence date or sheet row, up or down', () => {
    const items = [
      { id: 'A10E', emergence: 46290, row: 30 },
      { id: 'A2E', emergence: null, row: 10 },
      { id: 'b1d', emergence: 46280, row: 20 },
      { id: 'A9E', emergence: 46290, row: 5 },
    ]
    const ids = (by: 'id' | 'emergence' | 'row', desc = false) => sortRecorded(items, { by, desc }, x => x).map(x => x.id)
    expect(ids('id')).toEqual(['A2E', 'A9E', 'A10E', 'b1d'])
    expect(ids('id', true)).toEqual(['b1d', 'A10E', 'A9E', 'A2E'])
    expect(ids('row')).toEqual(['A9E', 'A2E', 'b1d', 'A10E'])
    expect(ids('row', true)).toEqual(['A10E', 'b1d', 'A2E', 'A9E'])
    // The same day by sheet row; no emergence date last, either way.
    expect(ids('emergence')).toEqual(['b1d', 'A9E', 'A10E', 'A2E'])
    expect(ids('emergence', true)).toEqual(['A10E', 'A9E', 'b1d', 'A2E'])
    // The list given is not changed.
    expect(items[0].id).toBe('A10E')
  })

  it('compares IDs as people read them; a sort read back from storage, else by sheet row', () => {
    expect(compareIds('A2E', 'a10e')).toBeLessThan(0)
    expect(compareIds('g7d', 'G7D')).toBe(0)
    expect(readOrder({ by: 'id', desc: true })).toEqual({ by: 'id', desc: true })
    expect(readOrder({ by: 'nope' })).toEqual(DEFAULT_ORDER)
    expect(readOrder(null)).toEqual({ by: 'row', desc: false })
  })
})
