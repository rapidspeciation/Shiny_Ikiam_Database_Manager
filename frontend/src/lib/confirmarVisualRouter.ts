/**
 * Deterministic regional router for Confirmar visual field-ID cards in /identificar/.
 *
 * Gated behind VITE_CONFIRM_VISUAL_CARDS=1 (OFF unless built with =1; production sets it in frontend/.env.production).
 * Implements the exact Waves 2, 3A, 3B, 4, 5, and 6 regional & subspecies routing contract:
 *   - Wave 3B / Wave 5 (`routing_mode: 'side_only'`): resolves strictly by `(pair_id, side_andes)`
 *     (never rejects on subspecies predictions); inseparable pairs return explicit `null`.
 *   - Wave 2 / Wave 3A / Wave 4 / Wave 6 (`routing_mode: 'subspecies_and_side'`):
 *     1. Subspecies alias match (case-insensitive trimmed, full trinomial OR infraspecific
 *        epithet against `a OR b` aliases); rejects conflicting subspecies or conflicting
 *        side+subspecies; returns explicit `null` for deferred rings and inseparable/held
 *        pairs (never silently substitutes another mimicry ring).
 *     2. Side-only match (`East` / `West` from user selector or conservative Ecuador GPS divide).
 *     3. Unspecified region fallback (`pair.default_card_id` with `regionFallback=true`),
 *        where manual region switching (`preferredCardId`) is permitted among active regional
 *        cards for that pair (e.g. `p01` Occidente vs Oriente · Napo y Amazonía).
 *   - With no side, a GPS point inside the ridge band but clearly on one side of the ridge line
 *     (`sideLean`) picks no card: before any of the steps, it hides a pair whose guides are all
 *     for the other side, whether or not the head names a subspecies.
 *   - Subspecies names are compared without rank markers (`f.`, `ssp.`, `subsp.`) or a trailing
 *     `?`, the same normal form scripts/build_confirmar_visual_bundle.py writes and checks.
 *   - Cross-wave superseding (`superseded_pairs`): when a newer wave rescues an earlier held
 *     pair (e.g. `w3a_03 -> w5_02`) or supersedes an earlier hold (`w3a_01 -> w5_01`), queries
 *     for either the old or new pair_id resolve cleanly to the canonical active/verified entry.
 *
 * Pure TypeScript (no DOM requirement for routing logic; manifest fetch is lazy and optional).
 */
import { SSP_MIN, SSP_SHARE } from './identifyResult'

export type SideAndes = 'East' | 'West'
export type ConfirmarLocale = 'es' | 'en'
export type RoutingMode = 'subspecies_and_side' | 'side_only'

export type RouteRawStatus =
  | 'active'
  | 'no_unlocked_museum_pair'
  | 'inseparable_in_photos'
  | 'not_reliably_separable_in_photos'
  | 'held_sex_confounded_small_n'
  | 'held_small_n_co_mimics'
  | 'held_suspect_reference_labels'
  | 'inseparable_or_unverified_refs'

export type RegionalRouteStatus =
  | 'active'
  | 'unavailable_deferred_ring'
  | 'inseparable_in_photos'
  | 'not_reliably_separable_in_photos'
  | 'held_sex_confounded_small_n'
  | 'held_small_n_co_mimics'
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
  routing_mode?: RoutingMode
  species_a: string
  species_b: string
  classification: string
  default_card_id: string | null
  supersedes?: string[]
  routes: ConfirmarRoute[]
}

export interface ConfirmarVisualManifest {
  schema: 'confirmar-visual-cards/1'
  waves: { wave: string; routing_mode?: RoutingMode; map_version?: string | null; pairs: number; active_cards: number; non_active_pairs: number; audit_status: string }[]
  verification?: {
    all_wave_receipts_passed: boolean
    locked_or_blind_overlap: number
    canonical_locked_cams_cross_checked?: number
    canonical_locked_inat_obs_cross_checked?: number
    canonical_locked_inat_photos_cross_checked?: number
    webp_quality?: number
    /** Whether the builder had the head's leaf list to check the subspecies aliases against. */
    head_subspecies_checked?: boolean
    subspecies_sides_reachable?: number
    subspecies_sides_without_head_subspecies?: number
  }
  counts: {
    total_pairs: number
    active_pairs: number
    active_cards: number
    non_active_pairs: number
    superseded_pairs?: number
    deferred_routes: number
    assets_total: number
  }
  superseded_pairs?: Record<string, string>
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
  /** The one side of the Andes the card shown is for; null for a card of both sides, and when no card is shown. */
  guideSide: SideAndes | null
  /** True only when region is unspecified and the pair offers >1 active regional cards to toggle between. */
  canSwitchRegion: boolean
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
  /** Why this pair was chosen (absent in matches built by hand, which read as a runner-up). */
  source?: ConfirmarGuideSource
  /** #1's guides to switch between: empty unless there are two or more. */
  guideOptions?: ConfirmarGuideOption[]
  /** A runner-up's note the card was shown ahead of. */
  sideNote?: ConfirmarSideNote | null
}

/** Feature flag: strictly OFF unless built with VITE_CONFIRM_VISUAL_CARDS=1. */
export const CONFIRM_VISUAL_CARDS_ENABLED = import.meta.env.VITE_CONFIRM_VISUAL_CARDS === '1'

export function confirmVisualCardsEnabled(override?: boolean): boolean {
  return override !== undefined ? override : CONFIRM_VISUAL_CARDS_ENABLED
}

const norm = (value: string | null | undefined): string => (value ?? '').trim().replace(/\s+/g, ' ').toLowerCase()

const RANK_MARKERS = new Set(['f.', 'ssp.', 'subsp.'])

/**
 * A subspecies name as it is compared: `norm`, without a rank marker (`f. ithomiola`, `Heliconius erato ssp. cyrbia`)
 * and without a trailing question mark (`gudruna?`). The same rule as norm_subspecies in
 * scripts/build_confirmar_visual_bundle.py, which writes the aliases and checks them against the head's names.
 */
export const normSubspecies = (value: string | null | undefined): string =>
  norm(value)
    .split(' ')
    .filter(word => !RANK_MARKERS.has(word))
    .join(' ')
    .replace(/(\s*\?)+$/, '')

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

/** Resolve asset URL with content-hash query string so sw.js cache-first updates cleanly when a card PNG changes. */
function resolveAssetUrl(asset: ConfirmarCardAsset, rootUrl?: string): string {
  const versioned = asset.sha256 ? `${asset.path}?v=${asset.sha256.slice(0, 10)}` : asset.path
  if (!rootUrl) return versioned
  try {
    return new URL(versioned, rootUrl).href
  } catch {
    return versioned
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
    status === 'held_small_n_co_mimics' ||
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
  'held_small_n_co_mimics',
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
  canSwitchRegion: boolean,
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
  const alternateCards = buildAlternates(pair, locale)

  return {
    pairId: pair.pair_id,
    cardId: route.card_id,
    status: 'active',
    regionFallback,
    guideSide: route.side_andes.length === 1 ? route.side_andes[0] : null,
    canSwitchRegion: canSwitchRegion && alternateCards.length > 1,
    imageUrl: asset ? resolveAssetUrl(asset, rootUrl) : null,
    imageWidth: asset?.width ?? null,
    imageHeight: asset?.height ?? null,
    regionBadge,
    regionDetail,
    viewBadge,
    taxonA: route.taxon_a,
    taxonB: route.taxon_b,
    altText,
    honestNote: null,
    alternateCards,
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
    guideSide: null,
    canSwitchRegion: false,
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

/** Extract the infraspecific epithet from a full trinomial (e.g. "mechanitis messenoides messenoides" -> "messenoides"). */
function infraspecificPart(taxonNorm: string): string | null {
  const words = taxonNorm.split(' ')
  return words.length >= 3 ? words.slice(2).join(' ') : null
}

/** Match a normalized subspecies input against route aliases, supporting both full trinomials and bare epithets. */
function aliasMatches(inputNorm: string, aliases: string[]): boolean {
  if (!inputNorm) return false
  const infra = infraspecificPart(inputNorm)
  return aliases.some(a => {
    const na = normSubspecies(a)
    return na === inputNorm || (infra !== null && na === infra)
  })
}

function isSideOnlyPair(pair: ConfirmarPair): boolean {
  return pair.routing_mode === 'side_only' || pair.wave === 'wave3b_non_ithomiini' || pair.wave === 'wave5_non_ithomiini'
}

/**
 * Deterministic Waves 2 / 3A / 3B / 4 / 5 / 6 route resolution for a known pair_id
 * (transparently following cross-wave `superseded_pairs` redirects like `w3a_03 -> w5_02`).
 */
export function resolveRegionalRoute(
  manifest: ConfirmarVisualManifest,
  input: {
    pairId: string
    sideAndes?: SideAndes | string | null
    /** With no `sideAndes`: the side a GPS point inside the ridge band clearly lies on (ecuadorSideLeanFromGps). */
    sideLean?: SideAndes | string | null
    subspeciesA?: string | null
    subspeciesB?: string | null
    locale: ConfirmarLocale
    preferredCardId?: string | null
    rootUrl?: string
  },
): RegionalRouteResolution {
  const canonicalPairId = manifest.pairs[input.pairId]
    ? input.pairId
    : (manifest.superseded_pairs?.[input.pairId] ?? input.pairId)
  const pair = manifest.pairs[canonicalPairId]
  if (!pair) {
    return {
      pairId: input.pairId,
      cardId: null,
      status: 'unknown_pair',
      regionFallback: false,
      guideSide: null,
      canSwitchRegion: false,
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
  const sspA = normSubspecies(input.subspeciesA)
  const sspB = normSubspecies(input.subspeciesB)

  // Wave 3B contract Rule 1: pair explicitly classified as not_reliably_separable_in_photos
  if (pair.routes.length === 1 && pair.routes[0].status === 'not_reliably_separable_in_photos') {
    return buildNullResolution(pair, 'not_reliably_separable_in_photos', locale, pair.routes[0])
  }

  // No side, and a point that leans to one: a pair whose guides are all for the other side shows none, whatever
  // subspecies the head names (as a known side does: `conflicting_side_and_subspecies`). The lean picks no card, so
  // a pair with a guide for that side, or for both, goes on as with no position; a pair with no guide keeps its note.
  const lean = side ? null : normalizeSide(input.sideLean)
  const guides = pair.routes.filter(r => r.status === 'active')
  if (lean && guides.length > 0 && !guides.some(r => r.side_andes.includes(lean))) {
    return buildNullResolution(pair, 'no_card_for_side', locale)
  }

  // Step 1: Subspecies match (Wave 2 / Wave 3A only — skipped for side-only Wave 3B contract)
  const hasSspInput = !isSideOnlyPair(pair) && (sspA.length > 0 || sspB.length > 0)
  if (hasSspInput) {
    const matchesA = sspA ? pair.routes.filter(r => aliasMatches(sspA, r.subspecies_a_aliases)) : []
    const matchesB = sspB ? pair.routes.filter(r => aliasMatches(sspB, r.subspecies_b_aliases)) : []

    if (matchesA.length > 0 && matchesB.length > 0) {
      const intersection = matchesA.filter(r => matchesB.includes(r))
      if (intersection.length === 0) {
        return buildNullResolution(pair, 'conflicting_subspecies_inputs', locale)
      }
      const route = (side ? intersection.find(r => r.side_andes.includes(side)) : null) ?? intersection[0]
      if (side && !route.side_andes.includes(side)) {
        return buildNullResolution(pair, 'conflicting_side_and_subspecies', locale, route)
      }
      if (route.status !== 'active') {
        return buildNullResolution(pair, mapNonActiveStatus(route.status), locale, route)
      }
      return buildActiveResolution(pair, route, locale, false, false, input.rootUrl)
    }

    const matched = matchesA.length > 0 ? matchesA : matchesB
    if (matched.length > 0) {
      const route = (side ? matched.find(r => r.side_andes.includes(side)) : null) ?? matched[0]
      if (side && !route.side_andes.includes(side)) {
        return buildNullResolution(pair, 'conflicting_side_and_subspecies', locale, route)
      }
      if (route.status !== 'active') {
        return buildNullResolution(pair, mapNonActiveStatus(route.status), locale, route)
      }
      return buildActiveResolution(pair, route, locale, false, false, input.rootUrl)
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
      return buildActiveResolution(pair, activeForSide[0], locale, false, false, input.rootUrl)
    }
    const nonActiveForSide = pair.routes.find(r => r.status !== 'active' && r.side_andes.includes(side))
    if (nonActiveForSide) {
      return buildNullResolution(pair, mapNonActiveStatus(nonActiveForSide.status), locale, nonActiveForSide)
    }
    return buildNullResolution(pair, 'no_card_for_side', locale)
  }

  // Step 3: Unspecified region fallback (only here may a manual region preference switch between active cards)
  if (input.preferredCardId) {
    const chosen = pair.routes.find(r => r.status === 'active' && r.card_id === input.preferredCardId)
    if (chosen) {
      return buildActiveResolution(pair, chosen, locale, false, true, input.rootUrl)
    }
  }
  if (pair.default_card_id) {
    const defRoute = pair.routes.find(r => r.status === 'active' && r.card_id === pair.default_card_id)
    if (defRoute) {
      return buildActiveResolution(pair, defRoute, locale, true, true, input.rootUrl)
    }
  }
  const firstNonActive = pair.routes.find(r => r.status !== 'active')
  if (firstNonActive) {
    return buildNullResolution(pair, mapNonActiveStatus(firstNonActive.status), locale, firstNonActive)
  }
  return buildNullResolution(pair, 'no_card_for_side', locale)
}

/** Half the width of the corridor along the ridge line where a point gets no side, in degrees of longitude. */
const RIDGE_BAND_DEG = 0.35
/** Inside that corridor, how far from the ridge line a point must be to lean to its side (about 22 km). */
const RIDGE_CLEAR_DEG = 0.2

/** The ridge line's longitude at a latitude; null for a point outside continental Ecuador. */
function ridgeLonAt(lat: number, lon: number): number | null {
  if (!Number.isFinite(lat) || !Number.isFinite(lon)) return null
  if (lat > 1.5 || lat < -5.1 || lon < -81.2 || lon > -75.0) return null
  // North of the equator the Andes curve more steeply NE toward Tulcán (0.81, -77.72)
  return lat > 0 ? -78.4 + 0.85 * lat : -78.4 + 0.24 * lat
}

/**
 * Conservative continental-Ecuador East/West Andes classifier from GPS coordinates.
 * Returns null outside continental Ecuador or inside the inter-Andean crest corridor
 * so valley towns (Quito, Tulcán, Cuenca, Loja, Otavalo, Riobamba) are never forced onto a slope.
 */
export function ecuadorSideFromGps(lat: number, lon: number): SideAndes | null {
  const ridgeLon = ridgeLonAt(lat, lon)
  if (ridgeLon === null) return null
  if (lon > ridgeLon + RIDGE_BAND_DEG) return 'East'
  if (lon < ridgeLon - RIDGE_BAND_DEG) return 'West'
  return null
}

/**
 * For a point `ecuadorSideFromGps` gives no side (inside the corridor): the side of the ridge line it clearly lies on,
 * as the west-slope cloud forest does (Tandayapa, Bellavista, Nanegalito); null near the line (Quito, Tulcán, Cuenca)
 * and wherever `ecuadorSideFromGps` answers. It is not sure enough to choose a guide, only to hide one made for the
 * other side alone.
 */
export function ecuadorSideLeanFromGps(lat: number, lon: number): SideAndes | null {
  const ridgeLon = ridgeLonAt(lat, lon)
  if (ridgeLon === null || ecuadorSideFromGps(lat, lon)) return null
  if (lon >= ridgeLon + RIDGE_CLEAR_DEG) return 'East'
  if (lon <= ridgeLon - RIDGE_CLEAR_DEG) return 'West'
  return null
}

/** Extract the strongest confident subspecies name using the app's SSP_MIN / SSP_SHARE thresholds. */
function dominantSubspecies(pred: PredictionInput | undefined): string | null {
  const list = pred?.subspecies
  if (!list || list.length === 0) return null
  const best = list[0]
  if (!best?.name?.trim()) return null
  if (best.p < SSP_MIN || (pred.p > 0 && best.p < SSP_SHARE * pred.p)) return null
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

  let fallbackMatch: PairLookupMatch | null = null
  for (const pair of Object.values(manifest.pairs)) {
    const pa = norm(pair.species_a)
    const pb = norm(pair.species_b)
    let match: PairLookupMatch | null = null
    if (n1 === pa && n2 === pb) {
      match = { pair, subspeciesA: dominantSubspecies(pred1), subspeciesB: dominantSubspecies(pred2) }
    } else if (n1 === pb && n2 === pa) {
      match = { pair, subspeciesA: dominantSubspecies(pred2), subspeciesB: dominantSubspecies(pred1) }
    }
    if (!match) continue
    // Prefer an active pair over a pure null hold so a legacy hold can never shadow an active card
    if (pair.routes.some(r => r.status === 'active')) return match
    fallbackMatch = match
  }
  return fallbackMatch
}

/** Handle intraspecific subspecies pairs (e.g. p07 Hypothyris euclea intermedia vs pyrippe, p15 Godyris zavaleta). */
function findIntraspecificPair(manifest: ConfirmarVisualManifest, topPred: PredictionInput): PairLookupMatch | null {
  const topNorm = norm(topPred.name)
  if (!topNorm) return null
  const ssps = topPred.subspecies ?? []
  const topSsp = dominantSubspecies(topPred)
  const topSspNorm = normSubspecies(topSsp)

  for (const pair of Object.values(manifest.pairs)) {
    const parentA = norm(parentSpeciesOf(pair.species_a))
    const parentB = norm(parentSpeciesOf(pair.species_b))
    if (parentA !== parentB || parentA !== topNorm) continue

    const pa = norm(pair.species_a)
    const pb = norm(pair.species_b)

    // When subspecies scores exist, require the dominant subspecies to belong to this intraspecific pair
    if (ssps.length > 0) {
      if (!topSspNorm) continue
      const topInfra = infraspecificPart(topSspNorm) ?? topSspNorm
      const isTopA = topSspNorm === pa || topInfra === infraspecificPart(pa)
      const isTopB = topSspNorm === pb || topInfra === infraspecificPart(pb)
      if (!isTopA && !isTopB) continue
      return {
        pair,
        subspeciesA: isTopA ? pair.species_a : null,
        subspeciesB: isTopB ? pair.species_b : null,
      }
    }

    // Older head without subspecies predictions: allow species-level match
    return { pair, subspeciesA: null, subspeciesB: null }
  }
  return null
}

/**
 * Why a guide is shown:
 * - ranked pairs: `compared` (tapped species), `runner_up` (#1 + runner-up), `runner_up_pair` (two ranked runner-ups
 *   when #1 has no active guide), `intraspecific` (two forms of one ranked species);
 * - unranked co-mimics: `top1_comimic`, `compared_comimic`, `runner_up_comimic` (partner not among the AI's ranked candidates).
 */
export type ConfirmarGuideSource =
  | 'compared'
  | 'compared_comimic'
  | 'runner_up'
  | 'runner_up_pair'
  | 'intraspecific'
  | 'top1_comimic'
  | 'runner_up_comimic'

/** One of the available guides the person can switch to (shown as pills when there are more than one). */
export interface ConfirmarGuideOption {
  pairId: string
  /** What tells the guides apart: the look-alike of the anchor (`Heliconius erato`), two forms of the anchor (`a / b`), or a full pair (`A vs. B`). */
  label: string
}

/** A runner-up pair with no guide (a plain note) that a guide was shown ahead of: said under the card. */
export interface ConfirmarSideNote {
  pairId: string
  status: RegionalRouteStatus
  taxonA: string
  taxonB: string
  regionBadge: string | null
  honestNote: string
}

/** The runner-ups after #2 checked for a guide when the person picked none: their index and the least score each needs. */
const LATER_RUNNER_UPS: [index: number, minP: number][] = [
  [2, 0.05], // #3
  [3, 0.01], // #4
]

function displayKindOf(resolution: RegionalRouteResolution): ConfirmarCardMatch['displayKind'] {
  if (resolution.status === 'active' && resolution.imageUrl) return 'card'
  return HONEST_NOTE_STATUSES.has(resolution.status) && resolution.honestNote ? 'honest_note' : 'hidden'
}

function buildSideNote(resolution: RegionalRouteResolution | null | undefined): ConfirmarSideNote | null {
  if (!resolution || displayKindOf(resolution) !== 'honest_note' || !resolution.honestNote) return null
  return {
    pairId: resolution.pairId,
    status: resolution.status,
    taxonA: resolution.taxonA,
    taxonB: resolution.taxonB,
    regionBadge: resolution.regionBadge,
    honestNote: resolution.honestNote,
  }
}

function optionLabel(pair: ConfirmarPair, anchorNorm: string): string {
  const a = norm(pair.species_a)
  const b = norm(pair.species_b)
  if (a === anchorNorm) return pair.species_b
  if (b === anchorNorm) return pair.species_a
  // Two forms of the anchor species: the forms are what differ.
  const parentA = norm(parentSpeciesOf(pair.species_a))
  const parentB = norm(parentSpeciesOf(pair.species_b))
  if (parentA === parentB && parentA === anchorNorm) {
    const fa = infraspecificPart(a) ?? pair.species_a
    const fb = infraspecificPart(b) ?? pair.species_b
    return `${fa} / ${fb}`
  }
  // Pair not containing the anchor species (e.g. non-#1 runner-up pairs): name both taxa without duplicating "vs.".
  return `${pair.species_a} vs. ${pair.species_b}`
}

/**
 * Resolve top AI predictions + side of Andes + locale into a Confirmar visual card match for /identificar/.
 *
 * With a species the person tapped (`comparedSpecies` = C):
 *   1. Check (#1, C); if it yields an active card, return it.
 *   2. Otherwise record any (#1, C) honest note as `sideNote` and search active guides for C: (C, other ranked
 *      candidates in rank order, #2 or p >= 1 %), intraspecific of C, and fail-closed co-mimics of C (`!regionFallback`).
 *   3. If no active card exists for C, return the (#1, C) resolution (honest note / hidden) or null.
 *
 * Without `comparedSpecies` (default view):
 *   1. Check (#1, #2), intraspecific of #1, (#1, #3 >= 5 %), (#1, #4 >= 1 %), then fail-closed co-mimics of #1.
 *   2. Only when #1 yields zero active cards: check ranked runner-up pairs (preds[i], preds[j]) for 1 <= i < j with
 *      preds[i].p >= 5 % and preds[j].p >= 1 %; if still zero and #2 is strong (p >= 15 % or p >= 0.5 * #1), check
 *      #2's intraspecific and fail-closed co-mimic guides (`!regionFallback`), keeping any #1 note as `sideNote`.
 *   3. With no active card at all, return the first #1 resolution's note (or null), as before.
 */
export function resolveConfirmarVisualCard(
  manifest: ConfirmarVisualManifest,
  input: {
    topPredictions: PredictionInput[]
    comparedSpecies?: string | null
    sideAndes?: SideAndes | string | null
    sideLean?: SideAndes | string | null
    locale: ConfirmarLocale
    preferredCardId?: string | null
    /** The guide the person switched to (a pill), when more than one guide is available. */
    preferredPairId?: string | null
    rootUrl?: string
  },
): ConfirmarCardMatch | null {
  const preds = input.topPredictions
  if (!preds || preds.length === 0) return null

  const top1 = preds[0]
  const top1Norm = norm(top1.name)
  const resolve = (found: PairLookupMatch, allowPreferredCard = true): RegionalRouteResolution =>
    resolveRegionalRoute(manifest, {
      pairId: found.pair.pair_id,
      sideAndes: input.sideAndes,
      sideLean: input.sideLean,
      subspeciesA: found.subspeciesA,
      subspeciesB: found.subspeciesB,
      locale: input.locale,
      preferredCardId: allowPreferredCard ? input.preferredCardId : null,
      rootUrl: input.rootUrl,
    })
  const finish = (
    resolution: RegionalRouteResolution,
    source: ConfirmarGuideSource,
    guideOptions: ConfirmarGuideOption[] = [],
    sideNote: ConfirmarSideNote | null = null,
  ): ConfirmarCardMatch => ({ ...resolution, displayKind: displayKindOf(resolution), source, guideOptions, sideNote })
  const pickCard = (
    cards: { resolution: RegionalRouteResolution; source: ConfirmarGuideSource }[],
    anchorNorm: string,
    sideNote: ConfirmarSideNote | null,
  ): ConfirmarCardMatch => {
    const chosen = cards.find(c => c.resolution.pairId === input.preferredPairId) ?? cards[0]
    const guideOptions: ConfirmarGuideOption[] =
      cards.length > 1
        ? cards.map(c => ({ pairId: c.resolution.pairId, label: optionLabel(manifest.pairs[c.resolution.pairId], anchorNorm) }))
        : []
    return finish(chosen.resolution, chosen.source, guideOptions, sideNote)
  }
  const findComimics = (anchor: PredictionInput, seenPairIds: Set<string>): RegionalRouteResolution[] => {
    const anchorNorm = norm(anchor.name)
    if (!anchorNorm) return []
    const anchorSsp = dominantSubspecies(anchor)
    const out: RegionalRouteResolution[] = []
    for (const pair of Object.values(manifest.pairs)) {
      if (seenPairIds.has(pair.pair_id)) continue
      const isA = norm(pair.species_a) === anchorNorm
      const isB = norm(pair.species_b) === anchorNorm
      if (isA === isB || !pair.routes.some(r => r.status === 'active')) continue
      seenPairIds.add(pair.pair_id)
      // Fail-closed: never allow preferredCardId to satisfy an unranked co-mimic without a side or subspecies.
      const resolution = resolve({ pair, subspeciesA: isA ? anchorSsp : null, subspeciesB: isB ? anchorSsp : null }, false)
      // Only a card the anchor's subspecies or the side chose: never a pair's default card, shown for want of a region.
      if (displayKindOf(resolution) !== 'card' || resolution.regionFallback) continue
      // If the partner is already in topPredictions with a confident subspecies (e.g. a sub-1 % Pastaza ring),
      // verify that its subspecies agrees with this card rather than bypassing a deferred ring or conflict.
      const partnerName = isA ? pair.species_b : pair.species_a
      const partnerSsp = dominantSubspecies(preds.find(p => norm(p.name) === norm(partnerName)))
      if (partnerSsp) {
        const check = resolve(
          { pair, subspeciesA: isA ? anchorSsp : partnerSsp, subspeciesB: isB ? anchorSsp : partnerSsp },
          false,
        )
        if (displayKindOf(check) !== 'card' || check.cardId !== resolution.cardId) continue
      }
      out.push(resolution)
    }
    return out
  }

  // 1. The person tapped a candidate C to compare: prefer (#1, C), else active guides for C (keeping any (#1, C) note).
  if (input.comparedSpecies && norm(input.comparedSpecies) !== top1Norm) {
    const comparedNorm = norm(input.comparedSpecies)
    const comparedPred = preds.find(p => norm(p.name) === comparedNorm) ?? {
      name: input.comparedSpecies,
      p: 0,
    }
    const directFound = findPairForSpecies(manifest, top1, comparedPred)
    const directRes = directFound ? resolve(directFound) : null
    if (directRes && displayKindOf(directRes) === 'card') return finish(directRes, 'compared')

    const seenPairIds = new Set<string>(directFound ? [directFound.pair.pair_id] : [])
    const comparedCards: { resolution: RegionalRouteResolution; source: ConfirmarGuideSource }[] = []
    for (let i = 1; i < preds.length; i++) {
      const other = preds[i]
      if (norm(other.name) === comparedNorm) continue
      if (i > 1 && other.p < 0.01) continue
      const found = findPairForSpecies(manifest, comparedPred, other)
      if (!found || seenPairIds.has(found.pair.pair_id)) continue
      seenPairIds.add(found.pair.pair_id)
      const resolution = resolve(found)
      if (displayKindOf(resolution) === 'card') comparedCards.push({ resolution, source: 'compared' })
    }
    const intra = findIntraspecificPair(manifest, comparedPred)
    if (intra && !seenPairIds.has(intra.pair.pair_id)) {
      seenPairIds.add(intra.pair.pair_id)
      const resolution = resolve(intra)
      if (displayKindOf(resolution) === 'card') comparedCards.push({ resolution, source: 'compared' })
    }
    for (const resolution of findComimics(comparedPred, seenPairIds)) {
      comparedCards.push({ resolution, source: 'compared_comimic' })
    }
    if (comparedCards.length > 0) return pickCard(comparedCards, comparedNorm, buildSideNote(directRes))
    return directRes ? finish(directRes, 'compared') : null
  }

  // 2. The runner-ups, in order: (#1, #2), two forms of #1, (#1, #3), (#1, #4).
  const candidates: { found: PairLookupMatch; source: ConfirmarGuideSource }[] = []
  const pushCandidate = (found: PairLookupMatch | null, source: ConfirmarGuideSource) => {
    if (found && !candidates.some(c => c.found.pair.pair_id === found.pair.pair_id)) candidates.push({ found, source })
  }
  if (preds.length >= 2) pushCandidate(findPairForSpecies(manifest, top1, preds[1]), 'runner_up')
  pushCandidate(findIntraspecificPair(manifest, top1), 'intraspecific')
  for (const [index, minP] of LATER_RUNNER_UPS) {
    const pred = preds[index]
    if (pred && pred.p >= minP) pushCandidate(findPairForSpecies(manifest, top1, pred), 'runner_up')
  }
  const resolved = candidates.map(c => ({ ...c, resolution: resolve(c.found) }))
  const firstNote = resolved.find(r => displayKindOf(r.resolution) === 'honest_note') ?? null
  const seenPairIds = new Set(candidates.map(c => c.found.pair.pair_id))

  // 3. Every guide of #1 with a look-alike, reached by #1's own subspecies or the side (never by a runner-up's).
  const comimics = findComimics(top1, seenPairIds)

  const cards: { resolution: RegionalRouteResolution; source: ConfirmarGuideSource }[] = [
    ...resolved.filter(r => displayKindOf(r.resolution) === 'card'),
    ...comimics.map(resolution => ({ resolution, source: 'top1_comimic' as const })),
  ]
  if (cards.length > 0) {
    return pickCard(cards, top1Norm, buildSideNote(firstNote?.resolution))
  }

  // 4. Only when #1 yields zero active cards: check ranked runner-up pairs (preds[i], preds[j]) for 1 <= i < j.
  const fallbackCards: { resolution: RegionalRouteResolution; source: ConfirmarGuideSource }[] = []
  for (let i = 1; i < preds.length; i++) {
    if (preds[i].p < 0.05) continue
    for (let j = i + 1; j < preds.length; j++) {
      if (preds[j].p < 0.01) continue
      const found = findPairForSpecies(manifest, preds[i], preds[j])
      if (!found || seenPairIds.has(found.pair.pair_id)) continue
      seenPairIds.add(found.pair.pair_id)
      const resolution = resolve(found)
      if (displayKindOf(resolution) === 'card') fallbackCards.push({ resolution, source: 'runner_up_pair' })
    }
  }

  // 5. If still zero active cards and #2 is strong, check #2's intraspecific and fail-closed co-mimic guides.
  if (
    fallbackCards.length === 0 &&
    preds.length >= 2 &&
    (preds[1].p >= 0.15 || (top1.p > 0 && preds[1].p >= 0.5 * top1.p))
  ) {
    const intra2 = findIntraspecificPair(manifest, preds[1])
    if (intra2 && !seenPairIds.has(intra2.pair.pair_id)) {
      seenPairIds.add(intra2.pair.pair_id)
      const resolution = resolve(intra2)
      if (displayKindOf(resolution) === 'card') fallbackCards.push({ resolution, source: 'intraspecific' })
    }
    for (const resolution of findComimics(preds[1], seenPairIds)) {
      fallbackCards.push({ resolution, source: 'runner_up_comimic' })
    }
  }

  if (fallbackCards.length > 0) {
    return pickCard(fallbackCards, top1Norm, buildSideNote(firstNote?.resolution))
  }

  const first = resolved[0]
  return first ? finish(first.resolution, first.source) : null
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
