import { describe, expect, it } from 'vitest'
import { fixFor, renamedTo } from '../photoFixes'
import type { PhotoRow } from '../photoLinks'

const row = (name: string, checks: PhotoRow['checks'], folder = 'Peru/JPG'): PhotoRow => ({ change: 'new', folder, name, url: `https://drive.google.com/file/d/${name}/view`, checks })

describe("a reading's fixes", () => {
  it('renames to the names the sheets look up, and to a single likely ID', () => {
    expect(renamedTo('CAM07214_D 2.jpg', 'CAM072214')).toBe('CAM072214d2.JPG')
    expect(renamedTo('IMG_5001.JPG', 'CAM000001')).toBe(null)
    const rows = [
      row('CAM000003_v.jpg', [{ kind: 'rename', to: 'CAM000003v.JPG' }]),
      row('CAM07214d.JPG', [{ kind: 'unknownId', maybe: ['CAM072214'] }]),
      row('CAM99999d.JPG', [{ kind: 'unknownId', maybe: ['CAM099999', 'CAM999990'] }]),
      row('CAM55555d.JPG', [{ kind: 'unknownId' }]),
    ]
    expect(fixFor('rename', rows)?.operations).toEqual([{ action: 'rename', file: rows[0].url, name: 'CAM000003v.JPG' }])
    // Only the one with a single likely ID.
    expect(fixFor('unknownId', rows)?.operations).toEqual([{ action: 'rename', file: rows[1].url, name: 'CAM072214d.JPG' }])
  })
  it('removes only the copies of a photo in another folder; reads camera-named photos by folder; none for the rest', () => {
    const rows = [
      row('CAM1d.JPG', [{ kind: 'otherFolder', other: 'Panama', copy: true }]),
      row('CAM2d.JPG', [{ kind: 'otherFolder', other: 'Panama' }]),
      row('IMG_1.JPG', [{ kind: 'unnamed' }], 'Upload'),
      row('IMG_2.JPG', [{ kind: 'unnamed' }], 'Upload'),
      row('CAM3d.JPG', [{ kind: 'outOfOrder', around: ['CAM4'] }]),
    ]
    expect(fixFor('otherFolder', rows)?.operations).toEqual([{ action: 'remove', file: rows[0].url }])
    expect(fixFor('unnamed', rows)).toMatchObject({ folders: ['Upload'], count: 2 })
    expect(fixFor('outOfOrder', rows)).toBeNull()
  })
})
