/**
 * The curated photo order of a species (Franz, 13 Nov 2026: «the first photo shown is a bad photo compared to the
 * others ... is it because we prefer showing photos of Ecuador?»). identificar/curated.json
 * (.work/search-photos-20261113, scripts 01 to 04) holds, per Ithomiini species, its field photos (iNaturalist photo
 * ids) with four scores from a blind review of contact sheets (wings visible, angle, focus, crop; 1 to 5 each) and
 * whether each makes a good main photo, and the species' best Sanger photo cropped to its four wings
 * (identificar/wings/).
 *
 * The order: photos judged good as the main photo first, then those the review was unsure of, then those judged poor,
 * then photos it did not see (the server's extra iNaturalist photos, among them caterpillars and pupae, the team's
 * specimens, subspecies: round 1 review, scored photos first). Inside
 * each group by quality (the sum of the four scores, 4 to 20); the person's country (lib/photoCountry.ts) comes first
 * only among photos within SIMILAR points of the best one left. Photos it did not see keep the country order.
 * A species without curation keeps the country order alone (orderRefs).
 */
import { appRoot } from './appRoot'
import { countryRank, type Place } from './photoCountry'
import { orderRefs, type RefPhoto } from './identifyRefs'

export interface CuratedPhoto {
  /** iNaturalist photo id. */
  id: string
  /** [wings, angle, focus, crop], 1 to 5 each (absent: the only field photo, not scored). */
  s?: [number, number, number, number]
  hero?: 'yes' | 'no' | 'unsure'
}
export interface CuratedWing {
  /** Relative to identificar/wings/. */
  file: string
  specimen: string
  view: 'dorsal' | 'ventral'
  px?: [number, number]
  s?: [number, number, number, number]
}
export interface CuratedTaxon {
  field?: CuratedPhoto[]
  wing?: CuratedWing
}
export interface CuratedIndex {
  schema: 'ithomiini-curated/1'
  taxa: Record<string, CuratedTaxon>
}

/** Photos within this many points (of 20) of the best one left count as similar: the person's country goes first. */
export const SIMILAR = 2
export const WING_CREDIT = 'Ikiam / Wellcome Sanger Institute'

const CURATED_URL = () => new URL('identificar/curated.json', appRoot()).href
const WINGS_DIR = () => new URL('identificar/wings/', appRoot()).href
let memo: Promise<CuratedIndex | null> | null = null
/** The curated list (null: not reachable; the order then stays by country). Asked once, kept by the service worker. */
export function curatedIndex(): Promise<CuratedIndex | null> {
  memo ??= fetch(CURATED_URL(), { credentials: 'same-origin' })
    .then(r => (r.ok ? (r.json() as Promise<CuratedIndex>) : null))
    .then(d => (d?.schema === 'ithomiini-curated/1' && d.taxa ? d : null))
    .catch(() => null)
    .then(d => {
      if (!d) memo = null
      return d
    })
  return memo
}
/** For tests. */
export const resetCurated = () => void (memo = null)

/**
 * A photo's iNaturalist id from where it is served: a pack file (photos/<id>-<hash>.webp), the field set
 * (field/<id>.jpg), an iNaturalist URL (…/photos/<id>/…, also inside the team's photo proxy), or its page.
 */
export function photoId(p: Pick<RefPhoto, 'src' | 'source'>): string | null {
  let src = p.src
  try {
    const u = new URL(src, 'http://x')
    src = u.searchParams.get('u') ?? u.pathname
  } catch {
    /* kept as is */
  }
  const m = /\/photos\/(\d+)\//.exec(src) ?? /\/(\d+)-[0-9a-f]{6,}\.webp$/.exec(src) ?? /\/field\/(\d+)\.(?:jpe?g|webp)$/.exec(src) ?? /\/photos\/(\d+)\b/.exec(p.source ?? '')
  return m ? m[1] : null
}

const quality = (c: CuratedPhoto | undefined) => (c?.s ? c.s.reduce((a, b) => a + b, 0) : null)
/** 0 judged good, 1 unsure, 2 judged poor, 3 not seen. */
const band = (c: CuratedPhoto | undefined) => (!c?.s ? 3 : c.hero === 'yes' ? 0 : c.hero === 'no' ? 2 : 1)

/**
 * `photos` (one species' reference photos) in the curated order for `place`; without `entry` the country order.
 * Stable: photos the review did not see keep their country order among themselves.
 */
export function orderCurated(photos: RefPhoto[], entry: CuratedTaxon | null | undefined, place: Place): RefPhoto[] {
  const byPlace = orderRefs(photos, place)
  if (!entry?.field?.length) return byPlace
  const of = new Map(entry.field.map(c => [c.id, c]))
  const items = byPlace.map((p, i) => {
    const id = p.level === 'species' && p.kind !== 'preserved' ? photoId(p) : null
    const c = id ? of.get(id) : undefined
    return { p, i, b: band(c), q: quality(c), r: countryRank(p.country, place) }
  })
  const out: RefPhoto[] = []
  for (const b of [0, 1, 2, 3]) {
    const left = items.filter(x => x.b === b)
    if (b === 3) {
      out.push(...left.map(x => x.p))
      continue
    }
    while (left.length) {
      const best = Math.max(...left.map(x => x.q ?? 0))
      const near = left.filter(x => (x.q ?? 0) >= best - SIMILAR)
      near.sort((a, z) => a.r - z.r || (z.q ?? 0) - (a.q ?? 0) || a.i - z.i)
      out.push(near[0].p)
      left.splice(left.indexOf(near[0]), 1)
    }
  }
  return out
}

/** The species' Sanger wing crop as a reference photo (a preserved specimen with its ID), or null. */
export function wingPhoto(entry: CuratedTaxon | null | undefined): RefPhoto | null {
  const w = entry?.wing
  if (!w) return null
  return { src: new URL(w.file, WINGS_DIR()).href, credit: WING_CREDIT, license: '', level: 'species', source: null, country: null, kind: 'preserved', specimen: w.specimen }
}

const speciesOf = (name: string) => name.split(' ').slice(0, 2).join(' ')

/**
 * For a page that shows a species: its photos in the curated order, the best field photo (`hero`: the first live
 * photo of the species itself) and its Sanger wing crop (`wing`). `taxon` may be a subspecies: the species' curation
 * is used. `photos` as referencePhotos / savedPhotos give them; `place` the person's country (NO_PLACE: none).
 */
export async function curatedPhotos(taxon: string, photos: RefPhoto[], place: Place): Promise<{ photos: RefPhoto[]; hero: RefPhoto | null; wing: RefPhoto | null }> {
  const entry = (await curatedIndex())?.taxa[speciesOf(taxon)]
  const ordered = orderCurated(photos, entry, place)
  return { photos: ordered, hero: ordered.find(p => p.level === 'species' && p.kind !== 'preserved') ?? null, wing: wingPhoto(entry) }
}
