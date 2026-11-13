import { afterEach, describe, expect, it } from 'vitest'
import { bestPhotos, bestThumb, curatedReady, fieldAndMuseum, viewerSrc } from '../bestPhotos'
import type { RefPhoto } from '../identifyRefs'

// Identificar's one order for a taxon's photos: by country until the curated list is in, then the curated order
// (lib/curatedPhotos.ts), the same for the comparison and the candidates' thumbnails.
const ph = (src: string, country: string | null, level: RefPhoto['level'] = 'species'): RefPhoto => ({ src, credit: 'c', license: 'CC-BY', level, country })
const photos = [
  ph('/api/identify/ref-photo?u=https%3A%2F%2Finaturalist-open-data.s3.amazonaws.com%2Fphotos%2F111%2Fsmall.jpg', 'BR'),
  ph('https://static.inaturalist.org/photos/222/small.jpg', 'EC'),
  ph('packs/photos/333333-abcdef12.webp', 'PE'),
]
const place = { home: ['EC'], near: ['PE'], range: null }
afterEach(() => (curatedReady.value = null))

describe('bestPhotos', () => {
  it('before the curated list: the person\'s country first, then its neighbours, then the rest', () => {
    expect(bestPhotos('Mechanitis polymnia', photos, place as never).map(p => p.country)).toEqual(['EC', 'PE', 'BR'])
  })
  it('with the curated list: photos judged good first (by quality), poor ones last; other species keep the country order', () => {
    curatedReady.value = {
      schema: 'ithomiini-curated/1',
      taxa: {
        'Mechanitis polymnia': {
          field: [
            { id: '111', s: [5, 5, 4, 5], hero: 'yes' },
            { id: '222', s: [3, 3, 4, 2], hero: 'no' },
            { id: '333333', s: [4, 4, 4, 4], hero: 'yes' },
          ],
        },
      },
    }
    expect(bestPhotos('Mechanitis polymnia', photos, place as never).map(p => p.country)).toEqual(['BR', 'PE', 'EC'])
    // a subspecies name uses its species' curation
    expect(bestThumb('Mechanitis polymnia casabranca', photos, place as never)).toBe(photos[0].src)
    expect(bestPhotos('Mechanitis lysimnia', photos, place as never).map(p => p.country)).toEqual(['EC', 'PE', 'BR'])
  })
  it('the thumbnail is a live photo of the taxon itself, not of its genus', () => {
    const g = [ph('/g.jpg', 'EC', 'genus'), ph('/s.jpg', 'BR')]
    expect(bestThumb('X y', g, place as never)).toBe('/s.jpg')
    expect(bestThumb('X y', undefined, place as never)).toBeUndefined()
  })
  it('the viewer\'s larger size for iNaturalist photos only', () => {
    expect(viewerSrc('https://static.inaturalist.org/photos/222/small.jpg')).toBe('https://static.inaturalist.org/photos/222/large.jpg')
    expect(viewerSrc('https://static.inaturalist.org/photos/222/medium.jpeg')).toBe('https://static.inaturalist.org/photos/222/large.jpeg')
    expect(viewerSrc(photos[0].src)).toContain('%2F111%2Flarge.jpg')
    expect(viewerSrc(photos[2].src)).toBe(photos[2].src)
  })
  it('the Field / Museum sets: live photos, then the wing crop first among the museum ones', () => {
    const wing = { ...ph('/identificar/wings/CAM1-aa.webp', null), kind: 'preserved' as const, specimen: 'CAM1' }
    const pres = { ...ph('/p.webp', null), kind: 'preserved' as const }
    const spec = ph('/s.jpg', 'EC', 'specimen')
    const s = fieldAndMuseum([photos[1], pres, spec, photos[2]], wing)
    expect(s.field).toEqual([photos[1], photos[2]])
    expect(s.museum).toEqual([wing, pres, spec])
    expect(fieldAndMuseum([photos[1]], null).museum).toEqual([])
  })
})
