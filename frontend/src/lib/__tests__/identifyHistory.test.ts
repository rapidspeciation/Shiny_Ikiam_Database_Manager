import { afterEach, describe, expect, it } from 'vitest'
import { memoryHistory, outdated, prune, versionDay } from '../identifyHistory'
import { readLog, writeLog, writeLogAll, type LogEntry } from '../identifyLog'

// «Mis identificaciones»: Reprocesar only when a newer model reads photos now; the list keeps an item's place.
afterEach(() => localStorage.clear())

describe('identifyHistory', () => {
  it('outdated only when the versions\' own days say the stored one is older', () => {
    expect(outdated({ id: 'head_a.json', sex: null }, { id: 'head_a.json', sex: null })).toBe(false)
    // the identifier's head is a hash: which came first is unknown, so no button (also for a hash never seen here)
    expect(outdated({ id: 'head_a.json', sex: null }, { id: 'head_b.json', sex: null })).toBe(false)
    expect(outdated({ id: 'head_old_never_seen.json', sex: null }, { id: 'head_b.json', sex: null })).toBe(false)
    // no version kept: unknown
    expect(outdated({ id: null, sex: null }, { id: 'head_b.json', sex: null })).toBe(false)
    expect(outdated(undefined, { id: 'head_b.json', sex: 'B 2026-11-08' })).toBe(false)
    // sex models carry their day: a newer one now → Reprocesar; a rollback to an older one → none
    expect(outdated({ id: 'head_b.json', sex: 'B 2026-11-08' }, { id: 'head_b.json', sex: 'C 2026-11-20' })).toBe(true)
    expect(outdated({ id: 'head_b.json', sex: 'C 2026-11-20' }, { id: 'head_b.json', sex: 'B 2026-11-08' })).toBe(false)
    // dated identifiers (the small model's names)
    expect(outdated({ id: 'small-L1-20261031', sex: null }, { id: 'small-SB-20261111', sex: null })).toBe(true)
    // nothing known now (no signal, no model): no button
    expect(outdated({ id: 'head_a.json', sex: 'B 2026-11-08' }, { id: null, sex: null })).toBe(false)
    expect(versionDay('small-SB-20261111')).toBe(20261111)
    expect(versionDay('sha256-9c0845ecc74a')).toBeNull()
  })
  it('prune drops the photos of items that left the list', async () => {
    const h = memoryHistory()
    for (const at of ['a', 'b', 'c']) await h.put({ at, photo: new Blob(), result: '{}', compared: null, pick: null, takenAt: null, country: null, versions: { id: null, sex: null } })
    await prune(h, ['a', 'c'])
    expect(await h.keys()).toEqual(['a', 'c'])
  })
  it('writeLog: a new item first, a changed one in its place', () => {
    const e = (at: string, pick: string | null = null): LogEntry => ({ at, thumb: '', ai: 'X y', p: 0.5, pick, side: null })
    let log = writeLog(e('1'), [])
    log = writeLog(e('2'), log)
    expect(log.map(x => x.at)).toEqual(['2', '1'])
    log = writeLog(e('1', 'X z'), log)
    expect(log.map(x => x.at)).toEqual(['2', '1'])
    expect(log[1].pick).toBe('X z')
    writeLogAll([log[0]])
    expect(readLog().map(x => x.at)).toEqual(['2'])
  })
})
