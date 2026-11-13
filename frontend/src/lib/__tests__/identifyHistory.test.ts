import { afterEach, describe, expect, it } from 'vitest'
import { memoryHistory, noteSeen, outdated, prune, readSeen, versionDay } from '../identifyHistory'
import { readLog, writeLog, writeLogAll, type LogEntry } from '../identifyLog'

// «Mis identificaciones»: Reprocesar only when a newer model reads photos now; the list keeps an item's place.
afterEach(() => localStorage.clear())

describe('identifyHistory', () => {
  it('outdated only when the stored version is known to be older', () => {
    const seen = { 'head_a.json': '2026-11-01T00:00:00Z', 'head_b.json': '2026-11-10T00:00:00Z' }
    expect(outdated({ id: 'head_a.json', sex: null }, { id: 'head_a.json', sex: null }, seen)).toBe(false)
    // a newer head came later to this phone: older
    expect(outdated({ id: 'head_a.json', sex: null }, { id: 'head_b.json', sex: null }, seen)).toBe(true)
    // the server went back to an older head: the item is newer, no button
    expect(outdated({ id: 'head_b.json', sex: null }, { id: 'head_a.json', sex: null }, seen)).toBe(false)
    // a version never seen, or none kept: unknown, no button
    expect(outdated({ id: 'head_x.json', sex: null }, { id: 'head_b.json', sex: null }, seen)).toBe(false)
    expect(outdated({ id: null, sex: null }, { id: 'head_b.json', sex: null }, seen)).toBe(false)
    expect(outdated(undefined, { id: 'head_b.json', sex: null }, seen)).toBe(false)
    // sex models carry their day
    expect(outdated({ id: 'head_b.json', sex: 'B 2026-11-08' }, { id: 'head_b.json', sex: 'C 2026-11-20' }, seen)).toBe(true)
    expect(outdated({ id: 'head_b.json', sex: 'C 2026-11-20' }, { id: 'head_b.json', sex: 'B 2026-11-08' }, seen)).toBe(false)
    // nothing known now (no signal, no model): no button
    expect(outdated({ id: 'head_a.json', sex: 'B 2026-11-08' }, { id: null, sex: null }, seen)).toBe(false)
  })
  it('the versions seen are kept with the time they first came', () => {
    noteSeen({ id: 'head_a.json', sex: null }, '2026-11-01T00:00:00Z')
    noteSeen({ id: 'head_a.json', sex: 'B 2026-11-08' }, '2026-11-05T00:00:00Z')
    expect(readSeen()).toEqual({ 'head_a.json': '2026-11-01T00:00:00Z', 'B 2026-11-08': '2026-11-05T00:00:00Z' })
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
