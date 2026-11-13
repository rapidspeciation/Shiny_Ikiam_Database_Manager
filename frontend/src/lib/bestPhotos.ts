/**
 * The order of a taxon's reference photos wherever one is shown first (Identificar's comparison and its candidates'
 * thumbnails): one function, so a curated order per species (lane «Search photo curation», 13 Nov 2026: photos scored
 * for wing visibility, angle, focus and crop, plus a museum wing crop) can feed every place at once.
 *
 * Today: the place order (lib/identifyRefs.ts orderRefs: the person's country, then neighbours, then the rest; the
 * named subspecies' photos after the species' own). With a curated order registered (setCuratedOrder), the curated
 * photos come first in that order and the rest keep the place order after them. A curated key matches a photo by its
 * `src`, or by the iNaturalist photo id in it (…/photos/<id>/… or a pack file «<id>-<hash>.webp»).
 */
import { orderRefs, type RefPhoto } from './identifyRefs'
import type { Place } from './photoCountry'

/** A taxon's curated photo keys, best first (srcs or iNaturalist photo ids); null or empty: no curation. */
export type CuratedOrder = (taxon: string) => readonly string[] | null | undefined

let curated: CuratedOrder = () => null

/** Registers the curated order (the curation's data file, once loaded). */
export function setCuratedOrder(order: CuratedOrder | null) {
  curated = order ?? (() => null)
}

/** The iNaturalist photo id in a photo's address, when there is one. */
export function photoKey(src: string): string | null {
  const m = /(?:\/|%2F)photos(?:\/|%2F)(\d+)(?:\/|%2F)/i.exec(src) ?? /(?:^|\/)(\d{5,})-[0-9a-f]{6,}\.webp$/i.exec(src)
  return m ? m[1] : null
}

/**
 * A taxon's photos, best first: the curated ones (when registered) in their order, then the rest in the place order.
 * `rankOf` orders the subspecies' photos (the model's closest subspecies first), as orderRefs takes it.
 */
export function bestPhotos(taxon: string, photos: RefPhoto[], place: Place, rankOf?: (taxon: string) => number): RefPhoto[] {
  const placed = orderRefs(photos, place, rankOf)
  const keys = curated(taxon)
  if (!keys?.length) return placed
  const at = new Map(keys.map((k, i) => [k, i]))
  const rank = (p: RefPhoto) => at.get(p.src) ?? at.get(photoKey(p.src) ?? '') ?? Infinity
  const first = placed.filter(p => rank(p) !== Infinity).sort((a, b) => rank(a) - rank(b))
  return [...first, ...placed.filter(p => rank(p) === Infinity)]
}

/** The photo shown small for a taxon (a candidate's thumbnail): its best photo of the taxon itself, not of its genus. */
export function bestThumb(taxon: string, photos: RefPhoto[] | undefined, place: Place): string | undefined {
  if (!photos?.length) return undefined
  return bestPhotos(taxon, photos, place).find(p => p.level !== 'genus')?.src
}

/**
 * The photo at a larger size for the full-screen viewer, where there is one: an iNaturalist «small» photo (about
 * 240 px, as lists show it) as its «medium» (about 500 px), also through the app's proxy (server/identify-photos.mjs
 * passes both sizes). Pack photos and the team's own keep their address.
 */
export function viewerSrc(src: string): string {
  return src.replace(/(\/photos\/\d+\/)small\.(jpe?g|png|webp)/i, '$1medium.$2').replace(/(%2Fphotos%2F\d+%2F)small\.(jpe?g|png|webp)/i, '$1medium.$2')
}
