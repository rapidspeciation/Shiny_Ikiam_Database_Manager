import { afterEach, describe, expect, it } from 'vitest'
import { memoryHistory, outdated, prune, versionDay } from '../identifyHistory'
import { headRelease, IDENTIFIER_HEADS } from '../identifierReleases'
import { PUBLIC_FILES } from '../publicModels'
import { readLog, writeLog, writeLogAll, type LogEntry } from '../identifyLog'

// «Mis identificaciones»: Reprocesar only when a newer model reads photos now; the list keeps an item's place.
afterEach(() => localStorage.clear())

describe('identifyHistory', () => {
  it('outdated only when the versions\' own days say the stored one is older', () => {
    expect(outdated({ id: 'head_a.json', sex: null }, { id: 'head_a.json', sex: null })).toBe(false)
    // hashes this build does not list: which came first is unknown, so no button (also for one never seen here)
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
  it('the identifier\'s heads by the release list: an older listed head asks for Reprocesar; unlisted or newer does not', () => {
    const old = 'head_b8127dbf.json'
    const now = 'head_6fa6ab78.json'
    expect(outdated({ id: old, sex: null }, { id: now, sex: null })).toBe(true)
    // a server rolled back to the older head: the item is newer, no button
    expect(outdated({ id: now, sex: null }, { id: old, sex: null })).toBe(false)
    // a head this build does not list (older and never released here, or newer than the build): unknown, no button
    expect(outdated({ id: 'head_749fbffd.json', sex: null }, { id: now, sex: null })).toBe(false)
    expect(outdated({ id: now, sex: null }, { id: 'head_ffffffff.json', sex: null })).toBe(false)
    expect(outdated({ id: 'head_ffffffff.json', sex: null }, { id: now, sex: null })).toBe(false)
  })
  it('the release list ends with the head the app lists now, each file once, days in order', () => {
    const headMeta = PUBLIC_FILES.find(f => f.role === 'head-meta')!.name
    expect(IDENTIFIER_HEADS.at(-1)!.file).toBe(headMeta)
    expect(new Set(IDENTIFIER_HEADS.map(h => h.file)).size).toBe(IDENTIFIER_HEADS.length)
    const days = IDENTIFIER_HEADS.map(h => h.released)
    expect([...days].sort()).toEqual(days)
    expect(headRelease('head_6fa6ab78.json')).toBe(IDENTIFIER_HEADS.length - 1)
    expect(headRelease(null)).toBeNull()
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
