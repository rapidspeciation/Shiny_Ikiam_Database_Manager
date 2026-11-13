import { afterEach, describe, expect, it } from 'vitest'
import { bestPhotos, bestThumb, photoKey, setCuratedOrder, viewerSrc } from '../bestPhotos'
import type { RefPhoto } from '../identifyRefs'

// The one place that orders a taxon's reference photos: the place order today; a curated order, once registered, first.
const ph = (src: string, country: string | null, level: RefPhoto['level'] = 'species'): RefPhoto => ({ src, credit: 'c', license: 'CC-BY', level, country })
const photos = [
  ph('/api/identify/ref-photo?u=https%3A%2F%2Finaturalist-open-data.s3.amazonaws.com%2Fphotos%2F111%2Fsmall.jpg', 'BR'),
  ph('https://static.inaturalist.org/photos/222/small.jpg', 'EC'),
  ph('packs/photos/333333-abcdef12.webp', 'PE'),
]
const place = { home: ['EC'], near: ['PE'], range: null }
afterEach(() => setCuratedOrder(null))

describe('bestPhotos', () => {
  it('without curation: the person\'s country first, then its neighbours, then the rest', () => {
    expect(bestPhotos('Mechanitis polymnia', photos, place as never).map(p => p.country)).toEqual(['EC', 'PE', 'BR'])
  })
  it('a curated order comes first (matched by src or iNaturalist photo id), the rest keep the place order', () => {
    setCuratedOrder(t => (t === 'Mechanitis polymnia' ? ['111', 'packs/photos/333333-abcdef12.webp'] : null))
    expect(bestPhotos('Mechanitis polymnia', photos, place as never).map(p => p.country)).toEqual(['BR', 'PE', 'EC'])
    expect(bestPhotos('Mechanitis lysimnia', photos, place as never).map(p => p.country)).toEqual(['EC', 'PE', 'BR'])
  })
  it('the thumbnail is the best photo of the taxon itself, not of its genus', () => {
    const g = [ph('/g.jpg', 'EC', 'genus'), ph('/s.jpg', 'BR')]
    expect(bestThumb('X y', g, place as never)).toBe('/s.jpg')
    expect(bestThumb('X y', undefined, place as never)).toBeUndefined()
  })
  it('photo keys and the viewer\'s larger size', () => {
    expect(photoKey(photos[0].src)).toBe('111')
    expect(photoKey(photos[2].src)).toBe('333333')
    expect(viewerSrc('https://static.inaturalist.org/photos/222/small.jpg')).toBe('https://static.inaturalist.org/photos/222/medium.jpg')
    expect(viewerSrc(photos[0].src)).toContain('%2F111%2Fmedium.jpg')
    expect(viewerSrc(photos[2].src)).toBe(photos[2].src)
  })
})
