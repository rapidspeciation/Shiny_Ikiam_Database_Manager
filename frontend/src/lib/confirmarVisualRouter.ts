/**
 * Deterministic regional router for Confirmar visual field-ID cards in /identificar/.
 *
 * Strictly gated behind VITE_CONFIRM_VISUAL_CARDS=1 (default OFF).
 * Implements the exact Wave 2 + Wave 3A + Wave 3B regional & subspecies routing contract:
 *   1. Subspecies alias match (case-insensitive trimmed, a OR b); rejects conflicting
 *      subspecies or conflicting side+subspecies; returns explicit null for deferred rings
 *      and inseparable/held pairs (never silently substitutes another mimicry ring).
 *   2. Side-only match (East / West from user selector or conservative Ecuador GPS divide).
 *   3. Unspecified region fallback (pair.default_card_id with regionFallback=true), plus
 *      clean manual region switching when a pair has multiple active regional cards (e.g. p01).
 *
 * Pure TypeScript (no DOM requirement for routing logic; manifest fetch is lazy and optional).
 */

export type SideAndes = 'East' | 'West'
export type ConfirmarLocale = 'es' | 'en'

export type RouteRawStatus =
  | 'active'
  | 'no_unlocked_museum_pair'
  | 'inseparable_in_photos'
  | 'not_reliably_separable_in_photos'
  | 'held_sex_confounded_small_n'
  | 'held_suspect_reference_labels'
  | 'inseparable_or_unverified_refs'

export type RegionalRouteStatus =
  | 'active'
  | 'unavailable_deferred_ring'
  | 'inseparable_in_photos'
  | 'not_reliably_separable_in_photos'
  | 'held_sex_confounded_small_n'
  | 'held_suspect_reference_labels'
  | 'inseparable_or_unverified_refs'
  | 'conflicting_subspecies_inputs'
  | 'conflicting_side_and_subspecies'
  | 'unknown_subspecies'
  | 'no_card_for_side'
  | 'unknown_pair'

export interface ConfirmarCardAsset {
  path: string
  width: number
  height: number
  bytes: number
  sha256: string
  source_png_sha256: string
}

export interface ConfirmarRoute {
  card_id: string | null
  status: RouteRawStatus
  side_andes: SideAndes[]
  side_default_for: SideAndes[]
  region_bucket: 'occidente' | 'oriente' | 'both'
  region_es: string
  region_en: string
  region_detail_es?: string
  region_detail_en?: string
  view_badge_es?: string
  view_badge_en?: string
  taxon_a: string
  taxon_b: string
  subspecies_a_aliases: string[]
  subspecies_b_aliases: string[]
  assets?: {
    es: ConfirmarCardAsset
    en: ConfirmarCardAsset
  }
}

export interface ConfirmarPair {
  pair_id: string
  wave: string
  species_a: string
  species_b: string
  classification: string
  default_card_id: string | null
  routes: ConfirmarRoute[]
}

export interface ConfirmarVisualManifest {
  schema: 'confirmar-visual-cards/1'
  generated_at: string
  waves: { wave: string; map_version?: string | null; pairs: number; active_cards: number; non_active_pairs: number; audit_status: string }[]
  counts: {
    total_pairs: number
    active_pairs: number
    active_cards: number
    non_active_pairs: number
    deferred_routes: number
    assets_total: number
  }
  pairs: Record<string, ConfirmarPair>
}

export interface ConfirmarAlternateCard {
  cardId: string
  regionBadge: string
  regionDetail: string | null
  sideAndes: SideAndes[]
}

export interface RegionalRouteResolution {
  pairId: string
  cardId: string | null
  status: RegionalRouteStatus
  regionFallback: boolean
  imageUrl: string | null
  imageWidth: number | null
  imageHeight: number | null
  regionBadge: string | null
  regionDetail: string | null
  viewBadge: string | null
  taxonA: string
  taxonB: string
  altText: string | null
  honestNote: string | null
  alternateCards: ConfirmarAlternateCard[]
}

export interface PredictionSubspeciesInput {
  name: string
  p: number
}

export interface PredictionInput {
  name: string
  p: number
  subspecies?: PredictionSubspeciesInput[]
}

export interface ConfirmarCardMatch extends RegionalRouteResolution {
  /** Whether the UI should render a card or honest note for this resolution. */
  displayKind: 'card' | 'honest_note' | 'hidden'
}

/** Feature flag: strictly OFF unless built with VITE_CONFIRM_VISUAL_CARDS=1. */
export const CONFIRM_VISUAL_CARDS_ENABLED = import.meta.env.VITE_CONFIRM_VISUAL_CARDS === '1'

export function confirmVisualCardsEnabled(override?: boolean): boolean {
  return override !== undefined ? override : CONFIRM_VISUAL_CARDS_ENABLED
}

const norm = (value: string | null | undefined): string => (value ?? '').trim().replace(/\s+/g, ' ').toLowerCase()

function normalizeSide(side: string | null | undefined): SideAndes | null {
  const s = norm(side)
  if (s === 'east' || s === 'este' || s === 'oriente') return 'East'
  if (s === 'west' || s === 'oeste' || s === 'occidente') return 'West'
  return null
}

function mapNonActiveStatus(raw: RouteRawStatus): Exclude<RegionalRouteStatus, 'active'> {
  if (raw === 'no_unlocked_museum_pair') return 'unavailable_deferred_ring'
  if (raw === 'active') return 'no_card_for_side'
  return raw
}

function resolveAssetUrl(assetPath: string, rootUrl?: string): string {
  if (!rootUrl) return assetPath
  try {
    return new URL(assetPath, rootUrl).href
  } catch {
    return assetPath
  }
}

/** Plain-language honest field note (zero internal keys, CAM IDs, or jargon). */
export function honestNoteForStatus(
  status: RegionalRouteStatus,
  locale: ConfirmarLocale,
): string | null {
  if (status === 'inseparable_in_photos' || status === 'not_reliably_separable_in_photos' || status === 'inseparable_or_unverified_refs') {
    return locale === 'en'
      ? "These two look almost identical in field photos; a photo alone can't tell them apart."
      : 'Estas dos especies son casi idénticas en fotos de campo; una foto sola no basta para separarlas.'
  }
  if (
    status === 'unavailable_deferred_ring' ||
    status === 'held_sex_confounded_small_n' ||
    status === 'held_suspect_reference_labels'
  ) {
    return locale === 'en'
      ? 'No checked visual guide for this region yet.'
      : 'Aún no hay una guía visual revisada para esta región.'
  }
  return null
}

const HONEST_NOTE_STATUSES = new Set<RegionalRouteStatus>([
  'unavailable_deferred_ring',
  'inseparable_in_photos',
  'not_reliably_separable_in_photos',
  'inseparable_or_unverified_refs',
  'held_sex_confounded_small_n',
  'held_suspect_reference_labels',
])

function buildAlternates(pair: ConfirmarPair, locale: ConfirmarLocale): ConfirmarAlternateCard[] {
  return pair.routes
    .filter((r): r is ConfirmarRoute & { card_id: string } => r.status === 'active' && typeof r.card_id === 'string' && r.card_id.length > 0)
    .map(r => ({
      cardId: r.card_id,
      regionBadge: locale === 'en' ? r.region_en : r.region_es,
      regionDetail: (locale === 'en' ? r.region_detail_en : r.region_detail_es) ?? null,
      sideAndes: r.side_andes,
    }))
}

function buildActiveResolution(
  pair: ConfirmarPair,
  route: ConfirmarRoute,
  locale: ConfirmarLocale,
  regionFallback: boolean,
  rootUrl?: string,
): RegionalRouteResolution {
  const asset = route.assets?.[locale]
  const regionBadge = locale === 'en' ? route.region_en : route.region_es
  const regionDetail = (locale === 'en' ? route.region_detail_en : route.region_detail_es) ?? null
  const viewBadge = (locale === 'en' ? route.view_badge_en : route.view_badge_es) ?? null
  const altText =
    locale === 'en'
      ? `Visual field guide comparing ${route.taxon_a} and ${route.taxon_b} (${regionBadge})`
      : `Guía visual de campo para comparar ${route.taxon_a} y ${route.taxon_b} (${regionBadge})`

  return {
    pairId: pair.pair_id,
    cardId: route.card_id,
    status: 'active',
    regionFallback,
    imageUrl: asset ? resolveAssetUrl(asset.path, rootUrl) : null,
    imageWidth: asset?.width ?? null,
    imageHeight: asset?.height ?? null,
    regionBadge,
    regionDetail,
    viewBadge,
    taxonA: route.taxon_a,
    taxonB: route.taxon_b,
    altText,
    honestNote: null,
    alternateCards: buildAlternates(pair, locale),
  }
}

function buildNullResolution(
  pair: ConfirmarPair,
  status: Exclude<RegionalRouteStatus, 'active'>,
  locale: ConfirmarLocale,
  route?: ConfirmarRoute | null,
): RegionalRouteResolution {
  const regionBadge = route ? (locale === 'en' ? route.region_en : route.region_es) : null
  const regionDetail = route ? ((locale === 'en' ? route.region_detail_en : route.region_detail_es) ?? null) : null
  return {
    pairId: pair.pair_id,
    cardId: null,
    status,
    regionFallback: false,
    imageUrl: null,
    imageWidth: null,
    imageHeight: null,
    regionBadge,
    regionDetail,
    viewBadge: null,
    taxonA: route?.taxon_a ?? pair.species_a,
    taxonB: route?.taxon_b ?? pair.species_b,
    altText: null,
    honestNote: honestNoteForStatus(status, locale),
    alternateCards: buildAlternates(pair, locale),
  }
}

function aliasMatches(inputNorm: string, aliases: string[]): boolean {
  if (!inputNorm) return false
  return aliases.some(a => norm(a) === inputNorm)
}

/**
 * Deterministic Wave 2 / 3A / 3B route resolution for a known pair_id.
 */
export function resolveRegionalRoute(
  manifest: ConfirmarVisualManifest,
  input: {
    pairId: string
    sideAndes?: SideAndes | string | null
    subspeciesA?: string | null
    subspeciesB?: string | null
    locale: ConfirmarLocale
    preferredCardId?: string | null
    rootUrl?: string
  },
): RegionalRouteResolution {
  const pair = manifest.pairs[input.pairId]
  if (!pair) {
    return {
      pairId: input.pairId,
      cardId: null,
      status: 'unknown_pair',
      regionFallback: false,
      imageUrl: null,
      imageWidth: null,
      imageHeight: null,
      regionBadge: null,
      regionDetail: null,
      viewBadge: null,
      taxonA: '',
      taxonB: '',
      altText: null,
      honestNote: null,
      alternateCards: [],
    }
  }

  const locale = input.locale
  const side = normalizeSide(input.sideAndes)
  const sspA = norm(input.subspeciesA)
  const sspB = norm(input.subspeciesB)

  // Explicit user region toggle override (must match an active route of this pair)
  if (input.preferredCardId) {
    const chosen = pair.routes.find(r => r.status === 'active' && r.card_id === input.preferredCardId)
    if (chosen) return buildActiveResolution(pair, chosen, locale, false, input.rootUrl)
  }

  // Wave 3B contract Rule 1: pair explicitly classified as not_reliably_separable_in_photos
  if (pair.routes.length === 1 && pair.routes[0].status === 'not_reliably_separable_in_photos') {
    return buildNullResolution(pair, 'not_reliably_separable_in_photos', locale, pair.routes[0])
  }

  // Step 1: Subspecies match (a OR b alias, trimmed case-insensitive)
  const hasSspInput = sspA.length > 0 || sspB.length > 0
  if (hasSspInput) {
    const matchesA = sspA ? pair.routes.filter(r => aliasMatches(sspA, r.subspecies_a_aliases)) : []
    const matchesB = sspB ? pair.routes.filter(r => aliasMatches(sspB, r.subspecies_b_aliases)) : []

    if (matchesA.length > 0 && matchesB.length > 0) {
      const intersection = matchesA.filter(r => matchesB.includes(r))
      if (intersection.length === 0) {
        return buildNullResolution(pair, 'conflicting_subspecies_inputs', locale)
      }
      const route = intersection[0]
      if (side && !route.side_andes.includes(side)) {
        return buildNullResolution(pair, 'conflicting_side_and_subspecies', locale, route)
      }
      if (route.status !== 'active') {
        return buildNullResolution(pair, mapNonActiveStatus(route.status), locale, route)
      }
      return buildActiveResolution(pair, route, locale, false, input.rootUrl)
    }

    const matched = matchesA.length > 0 ? matchesA : matchesB
    if (matched.length > 0) {
      // If multiple routes matched one alias (rare), narrow by side if supplied
      const route = (side ? matched.find(r => r.side_andes.includes(side)) : null) ?? matched[0]
      if (side && !route.side_andes.includes(side)) {
        return buildNullResolution(pair, 'conflicting_side_and_subspecies', locale, route)
      }
      if (route.status !== 'active') {
        return buildNullResolution(pair, mapNonActiveStatus(route.status), locale, route)
      }
      return buildActiveResolution(pair, route, locale, false, input.rootUrl)
    }

    // Neither subspecies matched any alias: fail closed if side_andes is omitted
    if (!side) {
      return buildNullResolution(pair, 'unknown_subspecies', locale)
    }
  }

  // Step 2: Side-only match (when no subspecies matched and side_andes is supplied)
  if (side) {
    const activeForSide = pair.routes.filter(r => r.status === 'active' && r.side_default_for.includes(side))
    if (activeForSide.length === 1) {
      return buildActiveResolution(pair, activeForSide[0], locale, false, input.rootUrl)
    }
    const nonActiveForSide = pair.routes.find(r => r.status !== 'active' && r.side_andes.includes(side))
    if (nonActiveForSide) {
      return buildNullResolution(pair, mapNonActiveStatus(nonActiveForSide.status), locale, nonActiveForSide)
    }
    return buildNullResolution(pair, 'no_card_for_side', locale)
  }

  // Step 3: Unspecified region fallback (only when both subspecies and side_andes are omitted)
  if (pair.default_card_id) {
    const defRoute = pair.routes.find(r => r.status === 'active' && r.card_id === pair.default_card_id)
    if (defRoute) {
      return buildActiveResolution(pair, defRoute, locale, true, input.rootUrl)
    }
  }
  const firstNonActive = pair.routes.find(r => r.status !== 'active')
  if (firstNonActive) {
    return buildNullResolution(pair, mapNonActiveStatus(firstNonActive.status), locale, firstNonActive)
  }
  return buildNullResolution(pair, 'no_card_for_side', locale)
}

/**
 * Conservative continental-Ecuador East/West Andes classifier from GPS coordinates.
 * Returns null outside continental Ecuador or inside the ~70 km inter-Andean crest corridor
 * so valley sites (e.g. Quito, Cuenca) are never forced onto the wrong slope.
 */
export function ecuadorSideFromGps(lat: number, lon: number): SideAndes | null {
  if (!Number.isFinite(lat) || !Number.isFinite(lon)) return null
  if (lat > 1.5 || lat < -5.1 || lon < -81.2 || lon > -75.0) return null
  const ridgeLon = -78.4 + 0.24 * lat
  const bufferDeg = 0.35
  if (lon > ridgeLon + bufferDeg) return 'East'
  if (lon < ridgeLon - bufferDeg) return 'West'
  return null
}

/** Extract the strongest confident subspecies name for a species prediction, if any. */
function dominantSubspecies(pred: PredictionInput | undefined): string | null {
  const list = pred?.subspecies
  if (!list || list.length === 0) return null
  const best = list[0]
  if (!best?.name?.trim()) return null
  // Only propagate a subspecies prediction when it carries meaningful support
  if (best.p < 0.01 || (pred.p > 0 && best.p < 0.15 * pred.p)) return null
  return best.name.trim()
}

function parentSpeciesOf(taxon: string): string {
  const parts = taxon.trim().split(/\s+/)
  return parts.length >= 2 ? `${parts[0]} ${parts[1]}` : taxon.trim()
}

interface PairLookupMatch {
  pair: ConfirmarPair
  subspeciesA: string | null
  subspeciesB: string | null
}

function findPairForSpecies(
  manifest: ConfirmarVisualManifest,
  pred1: PredictionInput,
  pred2: PredictionInput,
): PairLookupMatch | null {
  const n1 = norm(pred1.name)
  const n2 = norm(pred2.name)
  if (!n1 || !n2 || n1 === n2) return null

  for (const pair of Object.values(manifest.pairs)) {
    const pa = norm(pair.species_a)
    const pb = norm(pair.species_b)
    if (n1 === pa && n2 === pb) {
      return { pair, subspeciesA: dominantSubspecies(pred1), subspeciesB: dominantSubspecies(pred2) }
    }
    if (n1 === pb && n2 === pa) {
      return { pair, subspeciesA: dominantSubspecies(pred2), subspeciesB: dominantSubspecies(pred1) }
    }
  }
  return null
}

/** Handle intraspecific subspecies pairs (e.g. p07 Hypothyris euclea intermedia vs pyrippe, p15 Godyris zavaleta). */
function findIntraspecificPair(manifest: ConfirmarVisualManifest, topPred: PredictionInput): PairLookupMatch | null {
  const topNorm = norm(topPred.name)
  if (!topNorm) return null
  const ssps = (topPred.subspecies ?? []).filter(s => s.p >= 0.01)

  for (const pair of Object.values(manifest.pairs)) {
    const parentA = norm(parentSpeciesOf(pair.species_a))
    const parentB = norm(parentSpeciesOf(pair.species_b))
    if (parentA !== parentB || parentA !== topNorm) continue
    // Species matches an intraspecific pair (e.g. Hypothyris euclea -> p07, Godyris zavaleta -> p15)
    const sspNames = ssps.map(s => norm(s.name))
    const pa = norm(pair.species_a)
    const pb = norm(pair.species_b)
    const hasA = sspNames.includes(pa)
    const hasB = sspNames.includes(pb)
    if (hasA && hasB) {
      return { pair, subspeciesA: pair.species_a, subspeciesB: pair.species_b }
    }
    // Even if only top species is known (e.g. older head without subspecies or single ssp), surface the pair
    if (ssps.length === 0 || hasA || hasB) {
      return {
        pair,
        subspeciesA: hasA ? pair.species_a : null,
        subspeciesB: hasB ? pair.species_b : null,
      }
    }
  }
  return null
}

/**
 * Resolve top AI predictions + side of Andes + locale into a Confirmar visual card match for /identificar/.
 */
export function resolveConfirmarVisualCard(
  manifest: ConfirmarVisualManifest,
  input: {
    topPredictions: PredictionInput[]
    comparedSpecies?: string | null
    sideAndes?: SideAndes | string | null
    locale: ConfirmarLocale
    preferredCardId?: string | null
    rootUrl?: string
  },
): ConfirmarCardMatch | null {
  const preds = input.topPredictions
  if (!preds || preds.length === 0) return null

  const top1 = preds[0]
  let found: PairLookupMatch | null = null

  // 1. If the user actively selected a runner-up candidate to compare against #1, check that pair first
  if (input.comparedSpecies && norm(input.comparedSpecies) !== norm(top1.name)) {
    const comparedPred = preds.find(p => norm(p.name) === norm(input.comparedSpecies)) ?? {
      name: input.comparedSpecies,
      p: 0,
    }
    found = findPairForSpecies(manifest, top1, comparedPred)
  }

  // 2. Check top-2 species pair
  if (!found && preds.length >= 2) {
    found = findPairForSpecies(manifest, top1, preds[1])
  }

  // 3. Check intraspecific subspecies pair on top-1 species (e.g. p07, p15)
  if (!found) {
    found = findIntraspecificPair(manifest, top1)
  }

  // 4. Check (top1, top3) when #3 is a plausible runner-up (>= 5%)
  if (!found && preds.length >= 3 && preds[2].p >= 0.05) {
    found = findPairForSpecies(manifest, top1, preds[2])
  }

  if (!found) return null

  const resolution = resolveRegionalRoute(manifest, {
    pairId: found.pair.pair_id,
    sideAndes: input.sideAndes,
    subspeciesA: found.subspeciesA,
    subspeciesB: found.subspeciesB,
    locale: input.locale,
    preferredCardId: input.preferredCardId,
    rootUrl: input.rootUrl,
  })

  const displayKind: ConfirmarCardMatch['displayKind'] =
    resolution.status === 'active' && resolution.imageUrl
      ? 'card'
      : HONEST_NOTE_STATUSES.has(resolution.status) && resolution.honestNote
        ? 'honest_note'
        : 'hidden'

  return {
    ...resolution,
    displayKind,
  }
}

export const CONFIRMAR_MANIFEST_PATH = 'identificar/confirmar/confirmar_visual_manifest.json'

let manifestMemo: Promise<ConfirmarVisualManifest> | null = null

/** Lazy-load the unified Confirmar visual manifest once per session (only when flag is ON). */
export function loadConfirmarManifest(rootUrl: string): Promise<ConfirmarVisualManifest> {
  manifestMemo ??= fetch(new URL(CONFIRMAR_MANIFEST_PATH, rootUrl).href, { credentials: 'same-origin' })
    .then(r => (r.ok ? (r.json() as Promise<ConfirmarVisualManifest>) : Promise.reject(new Error(`HTTP ${r.status}`))))
    .catch(err => {
      manifestMemo = null
      throw err
    })
  return manifestMemo
}

/** Reset cached manifest promise (for unit tests). */
export function resetConfirmarManifestCache(): void {
  manifestMemo = null
}
