import { describe, expect, it } from 'vitest'
import { driveFileOf, fileCounts, pickRows, filterRows, flagText, foundChecks, handoverAccounts, progressPercent, rowsToWrite, rowsWanted, type PhotoChecks, type PhotoRow, type PhotoSync } from '../photoLinks'

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
      total: 508,
      newFolders: ['Guyana_2025/jpg_2', 'Guyana_2025/raw_2'],
      byFolder: [
        // folders not listed yet: their own row among their new ones
        { folder: 'Guyana_2025/jpg_2', new: 254, files: 253 },
        { folder: 'Guyana_2025/raw_2', new: 254, files: 253 },
      ],
    } as PhotoSync
    expect(rowsToWrite(sync, new Set())).toBe(508)
    // The folder's 253 files and its own row.
    expect(rowsToWrite(sync, new Set(['Guyana_2025/raw_2']))).toBe(254)
  })

  it('a folder\'s new rows and its changed rows are left for later each on its own; the rows shown follow', () => {
    const sync = {
      total: 10,
      newFolders: [],
      byFolder: [
        { folder: 'A', new: 3, files: 9, changed: 2 },
        { folder: 'B', new: 0, files: 4, changed: 5 },
      ],
    } as unknown as PhotoSync
    expect(rowsToWrite(sync, new Set(['A']), new Set())).toBe(7)
    expect(rowsToWrite(sync, new Set(), new Set(['A', 'B']))).toBe(3)
    const rows = [
      { change: 'new', folder: 'A', name: 'x.JPG' },
      { change: 'rename', folder: 'A', name: 'y.JPG', row: 4 },
      { change: 'remove', folder: 'B', name: 'z.JPG', row: 5 },
    ] as PhotoRow[]
    expect(rowsWanted(rows, new Set(['A']), new Set()).map(r => r.name)).toEqual(['y.JPG', 'z.JPG'])
    expect(rowsWanted(rows, new Set(), new Set(['A', 'B'])).map(r => r.name)).toEqual(['x.JPG'])
    expect(rowsWanted(rows, new Set(['A']), new Set(['A', 'B']))).toEqual([])
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

  it('says what a plan of file changes does, and which accounts are left a step', () => {
    expect(fileCounts({ rename: 3, mark: 2, sheet: 1 })).toBe('3 renombrar · 2 marcar …_todelete · 1 hoja de traspaso')
    expect(
      handoverAccounts({
        step: 'delete',
        button: 'Delete copied files',
        folder: 'PERU_2024',
        sheet: 'Transfer Ownership & Free Up Space',
        subfolders: true,
        by: [
          { account: 'a@example.org', files: 214 },
          { account: '', files: 3 },
        ],
      }),
    ).toBe('a@example.org (214) · cuenta desconocida (3)')
    expect(handoverAccounts(undefined)).toBe('')
  })
})

describe('pickRows', () => {
  const order = ['a', 'b', 'c', 'd']
  it('a click shows that row alone, a second click none', () => {
    expect(pickRows(['a', 'c'], 'b', {}, order, 'a')).toEqual(['b'])
    expect(pickRows(['b'], 'b', {}, order, 'b')).toEqual([])
  })
  it('Ctrl adds or takes out one row', () => {
    expect(pickRows(['a'], 'c', { add: true }, order, 'a')).toEqual(['a', 'c'])
    expect(pickRows(['a', 'c'], 'a', { add: true }, order, 'c')).toEqual(['c'])
  })
  it('Shift adds the rows from the last one clicked, either way', () => {
    expect(pickRows(['b'], 'd', { range: true }, order, 'b')).toEqual(['b', 'c', 'd'])
    expect(pickRows(['d'], 'b', { range: true }, order, 'd')).toEqual(['d', 'b', 'c'])
  })
  it('Shift with nothing clicked before adds the row alone', () => {
    expect(pickRows([], 'c', { range: true }, order, null)).toEqual(['c'])
  })
})

describe('driveFileOf', () => {
  it('takes the ID out of a Drive link', () => {
    expect(driveFileOf('https://drive.google.com/file/d/1AbCdEfGhIjKlMnOpQrStUvWxYz/view?usp=drivesdk')).toBe('1AbCdEfGhIjKlMnOpQrStUvWxYz')
    expect(driveFileOf(undefined)).toBe('')
  })
})

describe('what the wings show on the photos read', () => {
  it('lists the wing checks apart, each example in a few words', () => {
    const wings = {
      wingSide: { count: 2, examples: [{ name: 'CAM072274d.JPG', in: 'PERU_2024', side: 'ventral' as const, pair: 'suffix swapped' }] },
      wingTurned: { count: 1, examples: [{ name: 'CAM076802v.JPG', in: 'PERU_2024', turn: 'upside down', degrees: 180 }] },
    }
    expect(foundChecks(wings).map(c => [c.kind, c.count])).toEqual([['wingSide', 2], ['wingTurned', 1]])
    expect(foundChecks(null)).toEqual([])
    expect(flagText({ ...wings.wingSide.examples[0], kind: 'wingSide' })).toBe('muestra la ventral (d y v al revés)')
    expect(flagText({ kind: 'wingSide', side: 'dorsal', pair: 'both photos same side' })).toBe('muestra la dorsal (las dos fotos del mismo lado)')
    expect(flagText({ ...wings.wingTurned.examples[0], kind: 'wingTurned' })).toBe('girarla 180° a la derecha')
  })
})
