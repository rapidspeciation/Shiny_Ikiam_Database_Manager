import { describe, expect, it } from 'vitest'
import { AlreadyListed, BACKOFF, backoff, createOutbox, memoryStore, OutboxFull, type Answer, type Draft, type Send } from '../contribOutbox'
import { MAX_PENDING } from '../contribConfig'

// The phone's outbox for photos sent to train the AI: what each server answer does to an item.

const draft = (n = 1): Draft => ({
  photo: `cGhvdG8${n}`,
  sha256: 'a'.repeat(63) + n,
  thumb: 'dGh1bWI',
  mini: 'data:image/jpeg;base64,bWluaQ',
  payload: {
    createdAt: `2026-11-11T10:00:0${n % 10}.000Z`,
    takenAt: '2026-11-10',
    labels: { sex: 'female', photoKind: 'hand', species: { name: 'Mechanitis polymnia', rank: 'species', confirmed: true, freeText: false } },
    prediction: { source: 'online' },
    gps: null,
    consent: { version: 'v1-draft' },
  },
})
const OK = { contribution: { status: 'received' } }
const answer = (status: number, body: Record<string, unknown> | null = {}, retryAfter: number | null = null): Answer => ({ status, body, retryAfter })

/** A server that answers from `next` (default 201) and records each call. */
function setup(next: (method: string, path: string, body: unknown) => Answer = () => answer(201, { contribution: { status: 'received' } })) {
  const calls: { method: string; path: string; body: Record<string, unknown> }[] = []
  const send: Send = async (method, path, body) => {
    calls.push({ method, path, body: body as Record<string, unknown> })
    return next(method, path, body)
  }
  const clock = { t: 1_000_000 }
  let n = 0
  const store = memoryStore()
  const outbox = createOutbox({ store, send, now: () => clock.t, rand: () => 0.5, uuid: () => `00000000-0000-4000-8000-00000000000${++n}` })
  return { outbox, calls, clock, store }
}

describe('contribOutbox', () => {
  it('sent with its id, its photo and labels; then the photo leaves the phone and the status stays', async () => {
    const { outbox, calls, store } = setup()
    const item = await outbox.add('ana', draft())
    expect(await outbox.pending('ana')).toBe(1)
    expect(await outbox.flush('ana')).toEqual({ sent: 1, stopped: null, already: [] })
    expect(calls).toHaveLength(1)
    expect(calls[0]).toMatchObject({ method: 'POST', path: 'identify/contributions', body: { id: item.id, photo: 'cGhvdG81', sha256: draft().sha256, consent: { version: 'v1-draft' } } })
    expect((calls[0].body.labels as { sex: string }).sex).toBe('female')
    const kept = store.items.get(item.id)!
    expect(kept).toMatchObject({ state: 'sent', photo: null, thumb: null, serverStatus: 'received' })
    expect(kept.mini).toBeTruthy()
    expect(await outbox.pending('ana')).toBe(0)
    // Nothing more to send.
    await outbox.flush('ana', { force: true })
    expect(calls).toHaveLength(1)
  })

  it('without signal it waits with a backoff, then goes once the signal is back (the same id)', async () => {
    let up = false
    const { outbox, calls, clock, store } = setup(() => (up ? answer(201, OK) : answer(0, null)))
    const item = await outbox.add('ana', draft())
    expect(await outbox.flush('ana')).toEqual({ sent: 0, stopped: 'offline', already: [] })
    expect(store.items.get(item.id)).toMatchObject({ state: 'pending', attempts: 1, nextAt: clock.t + BACKOFF[0] })
    // The timer before the backoff ends: nothing sent.
    await outbox.flush('ana')
    expect(calls).toHaveLength(1)
    // The signal comes back (`online`): sent at once, under the same id.
    up = true
    expect(await outbox.flush('ana', { force: true })).toEqual({ sent: 1, stopped: null, already: [] })
    expect(calls.map(c => c.body.id)).toEqual([item.id, item.id])
    expect(store.items.get(item.id)!.state).toBe('sent')
  })

  it('backoff: 30 s, 1, 2, 5, 15 min, then every hour, ±20 %', () => {
    expect([1, 2, 3, 4, 5, 6, 9].map(a => backoff(a, () => 0.5))).toEqual([30_000, 60_000, 120_000, 300_000, 900_000, 3_600_000, 3_600_000])
    expect(backoff(1, () => 0)).toBe(24_000)
    expect(backoff(1, () => 1)).toBe(36_000)
  })

  it('200 already with another id: that id is kept, with the labels the server holds, and is the one deleted', async () => {
    const first = { id: 'first-id', status: 'received', sex: 'male', photoKind: 'live', species: null }
    const { outbox, calls, store } = setup((method) => (method === 'POST' ? answer(200, { already: true, id: 'first-id', contribution: first }) : answer(200, { deleted: 'first-id' })))
    const item = await outbox.add('ana', draft())
    await outbox.flush('ana')
    expect(store.items.get(item.id)).toMatchObject({ state: 'sent', serverId: 'first-id', payload: { labels: { sex: 'male', photoKind: 'live', species: null } } })
    await outbox.remove(item.id)
    expect(await outbox.list('ana')).toEqual([])
    await outbox.flush('ana')
    expect(calls[1]).toMatchObject({ method: 'DELETE', path: 'identify/contributions/first-id' })
    expect(store.items.size).toBe(0)
  })

  it('the same photo sent again from this phone (already listed here): the new one is dropped and said', async () => {
    let n = 0
    const { outbox, store } = setup(() => (++n === 1 ? answer(201, OK) : answer(200, { already: true, id: '00000000-0000-4000-8000-000000000001', contribution: { status: 'received' } })))
    const a = await outbox.add('ana', draft())
    await outbox.flush('ana')
    // (The server knows it by its bytes; here a copy whose hash this phone did not keep.)
    const b = await outbox.add('ana', { ...draft(), sha256: 'b'.repeat(64), payload: { ...draft().payload, labels: { ...draft().payload.labels, sex: 'male' } } })
    expect(await outbox.flush('ana')).toEqual({ sent: 0, stopped: null, already: [b.id] })
    expect([...store.items.keys()]).toEqual([a.id])
  })

  it('the same photo again while this phone lists it (also without signal): refused at Enviar; after «Borrar» it may go again', async () => {
    let up = false
    const { outbox, calls, store } = setup((method) => (!up ? answer(0, null) : method === 'DELETE' ? answer(200, {}) : answer(201, OK)))
    const a = await outbox.add('ana', draft())
    await expect(outbox.add('ana', draft())).rejects.toBeInstanceOf(AlreadyListed)
    up = true
    await outbox.flush('ana', { force: true })
    await expect(outbox.add('ana', draft())).rejects.toBeInstanceOf(AlreadyListed)
    // «Borrar», then Enviar again with signal: the delete goes first, then the new item.
    await outbox.remove(a.id)
    const again = await outbox.add('ana', draft())
    await outbox.flush('ana', { force: true })
    expect(calls.map(c => c.method)).toEqual(['POST', 'DELETE', 'POST'])
    expect(store.items.get(again.id)!.state).toBe('sent')
  })

  it('«Borrar» then Enviar again under weak signal: the new one waits for the delete instead of being lost', async () => {
    // The timer reaches the new item while the delete still waits: the server answers `already` with the first id.
    const answers: Answer[] = [answer(201, OK), answer(0, null), answer(200, { already: true, id: '00000000-0000-4000-8000-000000000001', contribution: { status: 'received' } })]
    const { outbox, calls, clock, store } = setup(() => answers.shift() ?? (calls.at(-1)!.method === 'DELETE' ? answer(200, {}) : answer(201, OK)))
    const a = await outbox.add('ana', draft())
    await outbox.flush('ana')
    await outbox.remove(a.id)
    await outbox.flush('ana') // the DELETE: no signal, backs off
    const again = await outbox.add('ana', draft())
    await outbox.flush('ana') // only the new one is due: `already`, the first is still being deleted
    expect(store.items.get(again.id)).toMatchObject({ state: 'pending' })
    clock.t += 3_600_000
    await outbox.flush('ana')
    expect(calls.map(c => c.method)).toEqual(['POST', 'DELETE', 'POST', 'DELETE', 'POST'])
    expect(store.items.get(again.id)!.state).toBe('sent')
    expect(store.items.has(a.id)).toBe(false)
  })

  it('a 200 without the stored item (a proxy, a captive portal) is not taken for sent', async () => {
    const { outbox, store } = setup(() => answer(200, null))
    const item = await outbox.add('ana', draft())
    await outbox.flush('ana')
    expect(store.items.get(item.id)).toMatchObject({ state: 'pending', photo: 'cGhvdG81', attempts: 1 })
  })

  it('410 (deleted on the server): dropped from the phone', async () => {
    const { outbox, store } = setup(() => answer(410, { error: { code: 'CONTRIBUTION_DELETED' } }))
    await outbox.add('ana', draft())
    await outbox.flush('ana')
    expect(store.items.size).toBe(0)
  })

  it('401: paused, kept for after the sign-in; 422: failed with the reason', async () => {
    const auth = setup(() => answer(401, { error: { message: 'Sign in required' } }))
    const a = await auth.outbox.add('ana', draft())
    await auth.outbox.add('ana', draft(2))
    expect(await auth.outbox.flush('ana')).toEqual({ sent: 0, stopped: 'auth', already: [] })
    expect(auth.calls).toHaveLength(1)
    expect(auth.store.items.get(a.id)).toMatchObject({ state: 'pending', attempts: 0 })
    const bad = setup(() => answer(422, { error: { message: 'The photo must be at most 1600 px on its longest side' } }))
    const b = await bad.outbox.add('ana', draft())
    await bad.outbox.flush('ana')
    expect(bad.store.items.get(b.id)).toMatchObject({ state: 'failed', photo: null, lastError: 'The photo must be at most 1600 px on its longest side' })
    // A failed one is deleted on the phone only (the server never kept it).
    await bad.outbox.remove(b.id)
    expect(bad.store.items.size).toBe(0)
    expect(bad.calls).toHaveLength(1)
  })

  it('429 waits for Retry-After; 507 (the server full) backs off and keeps the item', async () => {
    const rate = setup(() => answer(429, {}, 120))
    const r = await rate.outbox.add('ana', draft())
    expect((await rate.outbox.flush('ana')).stopped).toBe('rate')
    expect(rate.store.items.get(r.id)!.nextAt).toBe(rate.clock.t + 120_000)
    const full = setup(() => answer(507, { error: { message: 'The server has no room' } }, 3600))
    const f = await full.outbox.add('ana', draft())
    await full.outbox.flush('ana')
    expect(full.store.items.get(f.id)).toMatchObject({ state: 'pending', attempts: 1, nextAt: full.clock.t + BACKOFF[0], photo: 'cGhvdG81' })
  })

  it('«Borrar»: one never sent goes at once; a sent one is deleted on the server, also after a time without signal', async () => {
    let up = true
    const { outbox, calls, store } = setup((method) => (!up ? answer(0, null) : method === 'POST' ? answer(201, OK) : answer(200, { deleted: 'x' })))
    const never = await outbox.add('ana', draft())
    await outbox.remove(never.id)
    expect(store.items.size).toBe(0)
    expect(calls).toHaveLength(0)
    const sent = await outbox.add('ana', draft(2))
    await outbox.flush('ana')
    up = false
    await outbox.remove(sent.id)
    expect(store.items.get(sent.id)!.state).toBe('deleting')
    expect(await outbox.list('ana')).toEqual([])
    await outbox.flush('ana', { force: true })
    expect(store.items.get(sent.id)!.state).toBe('deleting')
    up = true
    await outbox.flush('ana', { force: true })
    expect(store.items.size).toBe(0)
    expect(calls.map(c => c.method)).toEqual(['POST', 'DELETE', 'DELETE'])
  })

  it('«Borrar» while its send is on its way: the delete follows', async () => {
    let release: (a: Answer) => void = () => {}
    const { outbox, calls, store } = setup((method) => (method === 'POST' ? (undefined as unknown as Answer) : answer(200, {})))
    // The POST answers only when released.
    const slowSend: Send = async (method, path, body) => {
      calls.push({ method, path, body: body as Record<string, unknown> })
      return method === 'POST' ? new Promise<Answer>(ok => (release = ok)) : answer(200, {})
    }
    const slow = createOutbox({ store, send: slowSend, now: () => 1, rand: () => 0.5, uuid: () => '00000000-0000-4000-8000-000000000009' })
    const item = await slow.add('ana', draft())
    const flushing = slow.flush('ana')
    await new Promise(r => setTimeout(r, 0))
    await slow.remove(item.id)
    release(answer(201, OK))
    await flushing
    expect(store.items.get(item.id)!.state).toBe('deleting')
    await slow.flush('ana')
    expect(calls.map(c => c.method)).toEqual(['POST', 'DELETE'])
    expect(store.items.size).toBe(0)
    void outbox
  })

  it("another person signed in on the phone never sends or sees someone else's items", async () => {
    const { outbox, calls } = setup()
    await outbox.add('ana', draft())
    await outbox.flush('bob', { force: true })
    expect(calls).toHaveLength(0)
    expect(await outbox.list('bob')).toEqual([])
    expect(await outbox.list('ana')).toHaveLength(1)
  })

  it(`at most ${MAX_PENDING} wait for signal`, async () => {
    const { outbox } = setup(() => answer(0, null))
    for (let k = 0; k < MAX_PENDING; k++) await outbox.add('ana', draft(k))
    await expect(outbox.add('ana', draft(99))).rejects.toBeInstanceOf(OutboxFull)
  })

  it('«Mis envíos» as the server has them: items from another phone added, items deleted elsewhere gone', async () => {
    let listed: object[] = []
    const { outbox, store } = setup((method) => (method === 'GET' ? answer(200, { contributions: listed }) : answer(201, OK)))
    const here = await outbox.add('ana', draft())
    await outbox.flush('ana')
    listed = [
      { id: 'other-phone', status: 'received', createdAt: '2026-11-01T09:00:00.000Z', sex: 'female', photoKind: 'hand', species: null },
    ]
    await outbox.refresh('ana')
    const items = await outbox.list('ana')
    // The one sent from here is gone from the server (deleted on another phone); the other phone's is listed.
    expect(items.map(i => i.id)).toEqual(['other-phone'])
    expect(items[0]).toMatchObject({ state: 'sent', mini: '', payload: { labels: { sex: 'female' } } })
    expect(store.items.has(here.id)).toBe(false)
    // A refresh that fails changes nothing.
    await createOutbox({ store, send: async () => answer(0, null) }).refresh('ana')
    expect((await outbox.list('ana')).length).toBe(1)
  })

  it('sent items are not pruned: every one stays deletable', async () => {
    const { outbox } = setup()
    for (let k = 0; k < 120; k++) {
      await outbox.add('ana', draft(k))
      await outbox.flush('ana')
    }
    expect((await outbox.list('ana')).length).toBe(120)
  })

  it("the server's status is shown once listed", async () => {
    const { outbox, store } = setup((method) =>
      method === 'GET' ? answer(200, { contributions: [{ id: '00000000-0000-4000-8000-000000000001', status: 'accepted' }] }) : answer(201, { contribution: { status: 'received' } }),
    )
    const item = await outbox.add('ana', draft())
    await outbox.flush('ana')
    await outbox.refresh('ana')
    expect(store.items.get(item.id)!.serverStatus).toBe('accepted')
  })
})
