/**
 * The order of a taxon's reference photos wherever Identificar shows one first (the comparison's reference photo and
 * the candidates' thumbnails): one place, fed by the curated order of lane «Search photo curation» (13 Nov 2026,
 * lib/curatedPhotos.ts: field photos scored for wing visibility, angle, focus and crop; the person's country first only
 * among photos of similar quality; a Sanger wing crop per species), the same order Buscar uses.
 *
 * The comparison takes its photos, hero and wing crop from curatedPhotos() itself (async, it loads curated.json once);
 * the thumbnails use bestThumb() here, which orders with the same rule (orderCurated) once the list is loaded
 * (`curatedReady`, reactive) and by country alone before that or without it (orderRefs, the named subspecies' photos
 * after the species' own).
 */
import { shallowRef } from 'vue'
import { orderRefs, type RefPhoto } from './identifyRefs'
import { curatedIndex, orderCurated, type CuratedIndex } from './curatedPhotos'
import type { Place } from './photoCountry'

/** The curated list once loaded (null: not yet, or not reachable). */
export const curatedReady = shallowRef<CuratedIndex | null>(null)
let asked = false
/** Loads the curated list once (the service worker keeps it for offline use). */
export function loadCurated(): void {
  if (asked) return
  asked = true
  curatedIndex().then(d => {
    if (d) curatedReady.value = d
    else asked = false
  })
}

const speciesOf = (name: string) => name.split(' ').slice(0, 2).join(' ')

/** A taxon's photos, best first: the curated order when loaded, else the place order (`rankOf`: subspecies order). */
export function bestPhotos(taxon: string, photos: RefPhoto[], place: Place, rankOf?: (taxon: string) => number): RefPhoto[] {
  const entry = curatedReady.value?.taxa[speciesOf(taxon)]
  return entry?.field?.length ? orderCurated(photos, entry, place) : orderRefs(photos, place, rankOf)
}

/** The photo shown small for a taxon (a candidate's thumbnail): its best live photo of the taxon itself, as curatedPhotos' hero. */
export function bestThumb(taxon: string, photos: RefPhoto[] | undefined, place: Place): string | undefined {
  if (!photos?.length) return undefined
  const ordered = bestPhotos(taxon, photos, place)
  return (ordered.find(p => p.level === 'species' && p.kind !== 'preserved') ?? ordered.find(p => p.level !== 'genus'))?.src
}

/**
 * The photo at a larger size for the full-screen viewer, where there is one: an iNaturalist «small» or «medium» photo
 * as its «large» (about 1024 px: the wings' detail when zoomed in), also through the app's proxy
 * (server/identify-photos.mjs passes it). The page keeps its own size; pack photos, wing crops and the team's own keep
 * their address. The viewer falls back to the page's address if the large one does not load.
 */
export function viewerSrc(src: string): string {
  return src.replace(/(\/photos\/\d+\/)(?:small|medium)\.(jpe?g|png|webp)/i, '$1large.$2').replace(/(%2Fphotos%2F\d+%2F)(?:small|medium)\.(jpe?g|png|webp)/i, '$1large.$2')
}

/** A museum photo: a preserved specimen (a Sanger wing crop, a pack's preserved photo) or the team's own specimen. */
export const isMuseum = (p: RefPhoto) => p.kind === 'preserved' || p.level === 'specimen'
/**
 * The comparison's two sets, as the Wings Gallery's Field / Museum tabs: the live photos in their order, and the
 * museum ones with the species' Sanger wing crop first (lib/curatedPhotos.ts wingPhoto).
 */
export function fieldAndMuseum(photos: RefPhoto[], wing: RefPhoto | null | undefined): { field: RefPhoto[]; museum: RefPhoto[] } {
  const museum = photos.filter(isMuseum).filter(p => p.src !== wing?.src)
  return { field: photos.filter(p => !isMuseum(p)), museum: wing ? [wing, ...museum] : museum }
}
