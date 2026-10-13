import { describe, expect, it } from 'vitest'
import { filterRows, flagText, foundChecks, progressPercent, rowsToWrite, type PhotoChecks, type PhotoRow, type PhotoSync } from '../photoLinks'

const none = { count: 0, examples: [] }
const checks: PhotoChecks = {
  photos: 10,
  linked: 6,
  butterflies: 3,
  otherViews: 0,
  rename: { count: 3, examples: [{ name: 'CAM077986_d.jpg', to: 'CAM077986d.JPG', in: 'PANAMA_2025' }] },
  unknownId: none,
  unnamed: { count: 1, examples: [{ name: 'IMG_5069.JPG', in: 'Guyana_2025/jpg_2' }] },
  outOfOrder: none,
  otherFolder: none,
  unpaired: none,
  beforeCollection: none,
  otherCountry: none,
}

describe('a reading of the photo folders', () => {
  it('lists the checks that found something, the most frequent first', () => {
    expect(foundChecks(checks).map(c => [c.kind, c.count])).toEqual([
      ['rename', 3],
      ['unnamed', 1],
    ])
    expect(foundChecks(null)).toEqual([])
  })

  it('says what each flag means for its photo', () => {
    expect(flagText({ kind: 'rename', to: 'CAM077986d.JPG' })).toContain('CAM077986d.JPG')
    expect(flagText({ kind: 'unknownId', maybe: ['CAM073786'] })).toContain('CAM073786')
    expect(flagText({ kind: 'outOfOrder', around: ['M6354', 'M6495'] })).toContain('M6354 · M6495')
    expect(flagText({ kind: 'beforeCollection', days: 33, date: '2024-11-18' })).toContain('33')
    expect(flagText({ kind: 'otherCountry', country: 'Brazil', folder: 'Peru' })).toContain('Brazil')
  })

  it('counts the rows to write without the folders left for later', () => {
    const sync = {
      total: 512,
      newFolders: ['Guyana_2025/jpg_2', 'Guyana_2025/raw_2'],
      byFolder: [
        { folder: 'Guyana_2025/jpg_2', new: 253, files: 253 },
        { folder: 'Guyana_2025/raw_2', new: 253, files: 253 },
      ],
    } as PhotoSync
    expect(rowsToWrite(sync, new Set())).toBe(512)
    // The folder's 253 files and its own row.
    expect(rowsToWrite(sync, new Set(['Guyana_2025/raw_2']))).toBe(258)
  })

  it('shows how far the writing is', () => {
    expect(progressPercent(undefined)).toBe(0)
    expect(progressPercent({ state: 'writing', written: 400, total: 1600 })).toBe(25)
    expect(progressPercent({ state: 'done', written: 1600, total: 1600 })).toBe(100)
  })

  it('filters the rows by what there is to look at', () => {
    const rows: PhotoRow[] = [
      { change: 'new', folder: 'A', name: 'CAM000001d.JPG' },
      { change: 'new', folder: 'A', name: 'CAM000002_d.jpg', checks: [{ kind: 'rename', to: 'CAM000002d.JPG' }] },
      { change: 'new', folder: 'A', name: 'IMG_1.JPG', checks: [{ kind: 'unnamed' }] },
    ]
    expect(filterRows(rows, 'all')).toHaveLength(3)
    expect(filterRows(rows, 'flagged').map(r => r.name)).toEqual(['CAM000002_d.jpg', 'IMG_1.JPG'])
    expect(filterRows(rows, 'unnamed').map(r => r.name)).toEqual(['IMG_1.JPG'])
  })
})
