import { afterEach, describe, expect, it } from 'vitest'
import { memoryHistory, outdated, prune, reprocessable, versionDay } from '../identifyHistory'
import { activeHeadRelease, FUSION_HEAD_SLOT1, headRelease, IDENTIFIER_HEADS, MERGED_HEAD_6FA6AB78, onlineHeadFile } from '../identifierReleases'
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
    const prev = 'head_6fa6ab78.json'
    const now = MERGED_HEAD_6FA6AB78
    expect(outdated({ id: old, sex: null }, { id: prev, sex: null })).toBe(true)
    expect(outdated({ id: old, sex: null }, { id: now, sex: null })).toBe(true)
    // pre-merge head_6fa6ab78.json results trigger Reprocesar after Option 1A display-time merges
    expect(outdated({ id: prev, sex: null }, { id: now, sex: null })).toBe(true)
    // a server rolled back to the older head: the item is newer, no button
    expect(outdated({ id: prev, sex: null }, { id: old, sex: null })).toBe(false)
    expect(outdated({ id: now, sex: null }, { id: prev, sex: null })).toBe(false)
    expect(outdated({ id: now, sex: null }, { id: now, sex: null })).toBe(false)
    // a head this build does not list (older and never released here, or newer than the build): unknown, no button
    expect(outdated({ id: 'head_749fbffd.json', sex: null }, { id: now, sex: null })).toBe(false)
    expect(outdated({ id: now, sex: null }, { id: 'head_ffffffff.json', sex: null })).toBe(false)
    expect(outdated({ id: 'head_ffffffff.json', sex: null }, { id: now, sex: null })).toBe(false)
  })
  it('the release list includes the public head and ends with Slot 1 fusion, each file once, days in order', () => {
    const headMeta = PUBLIC_FILES.find(f => f.role === 'head-meta')!.name
    expect(IDENTIFIER_HEADS.map(h => h.file)).toContain(activeHeadRelease(headMeta))
    expect(IDENTIFIER_HEADS.at(-1)!.file).toBe(FUSION_HEAD_SLOT1)
    expect(new Set(IDENTIFIER_HEADS.map(h => h.file)).size).toBe(IDENTIFIER_HEADS.length)
    const days = IDENTIFIER_HEADS.map(h => h.released)
    expect([...days].sort()).toEqual(days)
    expect(headRelease('head_6fa6ab78.json')).toBe(IDENTIFIER_HEADS.length - 3)
    expect(headRelease(MERGED_HEAD_6FA6AB78)).toBe(IDENTIFIER_HEADS.length - 2)
    expect(headRelease(FUSION_HEAD_SLOT1)).toBe(IDENTIFIER_HEADS.length - 1)
    expect(outdated({ id: MERGED_HEAD_6FA6AB78, sex: null }, { id: FUSION_HEAD_SLOT1, sex: null })).toBe(true)
    expect(outdated({ id: FUSION_HEAD_SLOT1, sex: null }, { id: MERGED_HEAD_6FA6AB78, sex: null })).toBe(false)
    expect(activeHeadRelease('head_6fa6ab78.json')).toBe(MERGED_HEAD_6FA6AB78)
    expect(activeHeadRelease(FUSION_HEAD_SLOT1)).toBe(FUSION_HEAD_SLOT1)
    expect(onlineHeadFile('head_slot1_3bb_dist_rel_7d54b54f')).toBe(FUSION_HEAD_SLOT1)
    expect(onlineHeadFile('head_hier_6fa6ab78')).toBe('head_6fa6ab78.json')
    expect(onlineHeadFile(null)).toBeNull()
    expect(activeHeadRelease('head_b8127dbf.json')).toBe('head_b8127dbf.json')
    expect(activeHeadRelease('head_ffffffff.json')).toBe('head_ffffffff.json')
    expect(activeHeadRelease(null)).toBeNull()
    expect(headRelease(null)).toBeNull()
  })
  it('one gate for «Reprocesar»: a reader in the mode chosen, and an older model or a needed re-read', () => {
    // an item that cannot be shown (equal version) and something reads now: a re-read, not «older»
    expect(reprocessable({ saved: true, reader: true, older: false, unread: true })).toBe(true)
    // an older model read it, but nothing can read now (signed out, no phone model): no button
    expect(reprocessable({ saved: true, reader: false, older: true, unread: false })).toBe(false)
    // «En este teléfono» chosen with no model (canIdentify false), even signed in: no button
    expect(reprocessable({ saved: true, reader: false, older: true, unread: true })).toBe(false)
    expect(reprocessable({ saved: true, reader: true, older: true, unread: false })).toBe(true)
    expect(reprocessable({ saved: true, reader: true, older: false, unread: false })).toBe(false)
    expect(reprocessable({ saved: false, reader: true, older: true, unread: true })).toBe(false)
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
