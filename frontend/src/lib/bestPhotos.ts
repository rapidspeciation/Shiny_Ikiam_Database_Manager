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
import { orderRefs, refPhotoSrc, type RefPhoto } from './identifyRefs'
import { curatedIndex, orderCurated, photoId, wingPhoto, type CuratedIndex } from './curatedPhotos'
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

/**
 * The photo shown small for a taxon (a candidate's thumbnail, 64 px: one butterfly must fill it). A curated species: its
 * best field photo judged good as a main photo (or its only field photo, which nobody judged poor); else its Sanger
 * wing crop (a plate of many specimens or a poor photo says nothing at 64 px); only then the old order.
 */
export function bestThumb(taxon: string, photos: RefPhoto[] | undefined, place: Place): string | undefined {
  const entry = curatedReady.value?.taxa[speciesOf(taxon)]
  if (entry) {
    const ok = new Set((entry.field ?? []).filter(c => c.hero === 'yes' || (!c.s && entry.field!.length === 1)).map(c => c.id))
    const live = orderCurated(photos ?? [], entry, place).find(p => p.level === 'species' && p.kind !== 'preserved' && ok.has(photoId(p) ?? ''))
    if (live) return live.src
    const wing = wingPhoto(entry)
    if (wing) return wing.src
  }
  if (!photos?.length) return undefined
  const ordered = bestPhotos(taxon, photos, place)
  return (ordered.find(p => p.level === 'species' && p.kind !== 'preserved') ?? ordered.find(p => p.level !== 'genus'))?.src
}

/**
 * The photo the full-screen viewer loads (Identificar's and Buscar's, one rule): for a photo with an iNaturalist id
 * (its proxied URL, its page, or a pack file «<id>-<hash>.webp»), signed in and online, iNaturalist's «large» (about
 * 1024 px: the wings' detail when zoomed in) through the app's photo proxy (server/identify-photos.mjs). Signed out
 * or offline, or a photo with no id (the team's own, a wing crop): its own address. `fallback`: the page's photo,
 * loaded if the large one does not come. Only the viewer's photo; the page keeps its sizes.
 */
export function viewerPhoto(p: Pick<RefPhoto, 'src' | 'source' | 'level' | 'kind'>, { signedIn, online }: { signedIn: boolean; online: boolean }): { src: string; fallback?: string } {
  const id = p.level !== 'specimen' && p.kind !== 'preserved' ? photoId(p) : null
  if (!id || !signedIn || !online) return { src: p.src }
  // the extension as the photo's own address names it (iNaturalist keeps jpg, jpeg or png); jpg for a pack file
  const ext = /photos(?:\/|%2F)\d+(?:\/|%2F)\w+\.(jpe?g|png)/i.exec(p.src)?.[1] ?? 'jpg'
  return { src: refPhotoSrc(`https://inaturalist-open-data.s3.amazonaws.com/photos/${id}/large.${ext}`), fallback: p.src }
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
