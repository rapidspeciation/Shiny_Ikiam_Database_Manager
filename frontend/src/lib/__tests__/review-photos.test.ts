import { describe, expect, it } from 'vitest'
import { photoList } from '../review'

describe('card photos', () => {
  it('leaves out a raw file when its JPG is there', () => {
    const files = {
      a: { name: 'CAM074360d.JPG' },
      b: { name: 'CAM074360d.ORF' },
      c: { name: 'CAM074360v.JPG' },
      d: { name: 'CAM074361v.cr2' },
    }
    const list = photoList({ dorsal: ['a', 'b'], ventral: ['c', 'd'], files } as never)
    expect(list.map(p => p.name)).toEqual(['CAM074360d.JPG', 'CAM074360v.JPG', 'CAM074361v.cr2'])
  })
})
