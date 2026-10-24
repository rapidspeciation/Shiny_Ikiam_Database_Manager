import { describe, expect, it } from 'vitest'
import { excludeFileChanges, includeFileChanges, leftOutFileChanges } from '../photo-files'
import type { FileRow } from '../photoLinks'

const rows = [
  { change: 'rename', name: 'photo.jpg', folder: 'JPG', needs: [{ on: 1, why: 'raw' }] },
  { change: 'rename', name: 'photo.CR2', folder: 'RAW', raw: true, needs: [{ on: 0, why: 'raw' }] },
  { change: 'folder', name: 'new', folder: 'Photos' },
  { change: 'move', name: 'another.jpg', folder: 'JPG', needs: [{ on: 2, why: 'folder' }] },
  { change: 'mark', name: 'foreign.jpg', folder: 'JPG', needs: [{ on: 5, why: 'handover' }] },
  { change: 'sheet', name: 'handover', folder: 'Photos', needsAny: [4] },
  { change: 'rotate', name: 'turn.jpg', folder: 'JPG' },
] satisfies FileRow[]

describe('file change selection', () => {
  it('starts empty and selects rotation alone', () => {
    const selected = includeFileChanges(rows, new Set(), [6])
    expect([...selected]).toEqual([6])
    expect(leftOutFileChanges(rows, selected)).toEqual([0, 1, 2, 3, 4, 5])
  })

  it('includes RAW, folder and handoff dependencies', () => {
    expect([...includeFileChanges(rows, new Set(), [0])]).toEqual([0, 1])
    expect([...includeFileChanges(rows, new Set(), [3])]).toEqual([3, 2])
    expect([...includeFileChanges(rows, new Set(), [4])]).toEqual([4, 5])
  })

  it('removes changes when a required change is unchecked', () => {
    const selected = includeFileChanges(rows, new Set(), [0, 3, 4, 6])
    expect([...excludeFileChanges(rows, selected, [1])].sort()).toEqual([2, 3, 4, 5, 6])
    expect([...excludeFileChanges(rows, selected, [5])].sort()).toEqual([0, 1, 2, 3, 6])
    expect([...excludeFileChanges(rows, selected, [4])].sort()).toEqual([0, 1, 2, 3, 6])
  })
})
