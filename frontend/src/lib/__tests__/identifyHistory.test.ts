import { afterEach, describe, expect, it } from 'vitest'
import { memoryHistory, outdated, prune } from '../identifyHistory'
import { readLog, writeLog, writeLogAll, type LogEntry } from '../identifyLog'

// «Mis identificaciones»: Reprocesar only when a newer model reads photos now; the list keeps an item's place.
afterEach(() => localStorage.clear())

describe('identifyHistory', () => {
  it('outdated: a stored version that differs from the current one (both known)', () => {
    expect(outdated({ id: 'head_a.json', sex: 'v1' }, { id: 'head_a.json', sex: 'v1' })).toBe(false)
    expect(outdated({ id: 'head_a.json', sex: null }, { id: 'head_b.json', sex: 'v1' })).toBe(true)
    expect(outdated({ id: 'head_a.json', sex: 'v1' }, { id: 'head_a.json', sex: 'v2' })).toBe(true)
    // nothing known now (no signal, no model): no Reprocesar
    expect(outdated({ id: 'head_a.json', sex: 'v1' }, { id: null, sex: null })).toBe(false)
    // read before versions were kept: older once the current one is known
    expect(outdated(undefined, { id: 'head_a.json', sex: null })).toBe(true)
    // no sex estimate kept: a sex model alone does not ask for Reprocesar
    expect(outdated({ id: 'head_a.json', sex: null }, { id: 'head_a.json', sex: 'v2' })).toBe(false)
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
