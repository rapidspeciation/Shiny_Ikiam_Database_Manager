import { describe, expect, it } from 'vitest'
import { HoldQueue, type HoldAnswer } from '../holds'
import { nextId } from '../emerged'

/**
 * A round of quick taps against a server that answers after a while and where
 * someone else may hold an ID already: the cards, the requests in the order
 * they reached the server, and the queue.
 */
function round({ order = ['A9E', 'B0E', 'B1E', 'B2E', 'B3E', 'B4E', 'B5E', 'B6E'], others = [] as string[], used = [] as string[], delay = 5 } = {}) {
  const cards: { key: string; id: string; state?: string }[] = []
  const held = new Map<string, string>()
  for (const id of others) held.set(id, 'someone')
  const requests: string[] = []
  let inFlight = 0
  let overlapped = false
  const notices: string[] = []
  const queue = new HoldQueue({
    async hold(key, id): Promise<HoldAnswer> {
      requests.push(id)
      inFlight++
      if (inFlight > 1) overlapped = true
      await new Promise(r => setTimeout(r, delay))
      inFlight--
      const holder = held.get(id)
      if (holder && holder !== key) return { held: false, value: id, code: 'CLAIMED', holder: 'Ana' }
      if (used.includes(id)) return { held: false, value: id, code: 'USED' }
      for (const [k, v] of held) if (v === key) held.delete(k)
      held.set(id, key)
      return { held: true, value: id }
    },
    idOf: key => cards.find(c => c.key === key)?.id ?? null,
    pick(key, refused) {
      const at = cards.findIndex(c => c.key === key)
      return nextId(order, order[0], [...cards.slice(0, at).map(c => c.id), ...refused])
    },
    assign(key, id, from) {
      const c = cards.find(x => x.key === key)!
      c.id = id
      if (from.holder) notices.push(`${from.id}→${id} (${from.holder})`)
    },
    settled(key, state) {
      cards.find(c => c.key === key)!.state = state
    },
  })
  /** One tap: the card at once, with the next ID the cards leave free. */
  function tap() {
    const key = `k${cards.length}`
    const id = nextId(order, order[0], cards.map(c => c.id))!
    cards.push({ key, id })
    void queue.push(key)
    return key
  }
  return { cards, queue, tap, requests, notices, overlapped: () => overlapped, held }
}

describe('quick taps hold their IDs in tap order', () => {
  it('six taps get six consecutive IDs at once, held one after another in the same order', async () => {
    const r = round()
    for (let i = 0; i < 6; i++) r.tap()
    // Shown at once, before any answer.
    expect(r.cards.map(c => c.id)).toEqual(['A9E', 'B0E', 'B1E', 'B2E', 'B3E', 'B4E'])
    await r.queue.idle()
    expect(r.requests).toEqual(['A9E', 'B0E', 'B1E', 'B2E', 'B3E', 'B4E'])
    expect(r.overlapped()).toBe(false)
    expect(r.cards.every(c => c.state === 'held')).toBe(true)
  })

  it('an ID someone took a moment before: that card and the ones after it move on, still in tap order', async () => {
    const r = round({ others: ['B0E'] })
    for (let i = 0; i < 3; i++) r.tap()
    await r.queue.idle()
    expect(r.cards.map(c => c.id)).toEqual(['A9E', 'B1E', 'B2E'])
    expect(r.cards.map(c => c.state)).toEqual(['held', 'held', 'held'])
    expect(r.notices).toEqual(['B0E→B1E (Ana)'])
    // Never two cards with the same ID, nor a request for the ID that was refused twice.
    expect(r.requests).toEqual(['A9E', 'B0E', 'B1E', 'B2E'])
  })

  it('later taps while the earlier ones wait never show the ID an earlier card moved to', async () => {
    const r = round({ others: ['A9E'], delay: 20 })
    r.tap()
    r.tap()
    // While the first waits for its answer, a third tap.
    await new Promise(res => setTimeout(res, 25))
    r.tap()
    const ids = r.cards.map(c => c.id)
    expect(new Set(ids).size).toBe(ids.length)
    await r.queue.idle()
    expect(r.cards.map(c => c.id)).toEqual(['B0E', 'B1E', 'B2E'])
  })

  it('a card kept from before (its ID on the wing already) is not moved: it says it is not held', async () => {
    const r = round({ others: ['B0E'] })
    r.cards.push({ key: 'old', id: 'B0E' })
    await r.queue.push('old', { move: false })
    expect(r.cards[0]).toMatchObject({ id: 'B0E', state: 'refused' })
  })

  it('no answer (no signal): the card keeps its ID and says so', async () => {
    const r = round()
    const queue = new HoldQueue({
      hold: () => Promise.reject(new Error('offline')),
      idOf: key => r.cards.find(c => c.key === key)?.id ?? null,
      pick: () => null,
      assign: () => {},
      settled: (key, state) => (r.cards.find(c => c.key === key)!.state = state),
    })
    r.cards.push({ key: 'a', id: 'A9E' })
    await queue.push('a')
    expect(r.cards[0]).toMatchObject({ id: 'A9E', state: 'offline' })
  })

  it('a card taken away while its hold is asked: its answer changes nothing', async () => {
    const r = round()
    const key = r.tap()
    r.cards.splice(0, 1)
    await r.queue.idle()
    expect(r.cards).toEqual([])
    expect(r.queue.waiting).toEqual([])
    void key
  })
})
