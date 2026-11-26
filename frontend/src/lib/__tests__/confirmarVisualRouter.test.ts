import { createHash } from 'node:crypto'
import { existsSync, readFileSync } from 'node:fs'
import { resolve } from 'node:path'
import { describe, expect, it } from 'vitest'
import {
  CONFIRM_VISUAL_CARDS_ENABLED,
  confirmVisualCardsEnabled,
  ecuadorSideFromGps,
  ecuadorSideLeanFromGps,
  normSubspecies,
  resolveConfirmarVisualCard,
  resolveRegionalRoute,
  type ConfirmarVisualManifest,
} from '../confirmarVisualRouter'

const PUBLIC_DIR = resolve(import.meta.dirname, '../../../public')
const MANIFEST_PATH = resolve(PUBLIC_DIR, 'identificar/confirmar/confirmar_visual_manifest.json')
const manifest = JSON.parse(readFileSync(MANIFEST_PATH, 'utf8')) as ConfirmarVisualManifest
const ROOT_URL = 'https://example.test/ithomiini/'

describe('confirmarVisualRouter — bundle integrity & feature flag', () => {
  it('defaults feature flag OFF unless VITE_CONFIRM_VISUAL_CARDS=1', () => {
    expect(CONFIRM_VISUAL_CARDS_ENABLED).toBe(false)
    expect(confirmVisualCardsEnabled()).toBe(false)
    expect(confirmVisualCardsEnabled(true)).toBe(true)
    expect(confirmVisualCardsEnabled(false)).toBe(false)
  })

  it('ships 91 unified pairs, 64 active cards, 128 SHA-256 verified 1400px ES/EN WebP assets, and locked_overlap=0 across all 6 waves', () => {
    expect(manifest.schema).toBe('confirmar-visual-cards/1')
    expect(manifest.waves.map(w => w.wave)).toEqual([
      'wave2_regional',
      'wave3a_dv',
      'wave3b_non_ithomiini',
      'wave4_dv',
      'wave5_non_ithomiini',
      'wave6_dv',
    ])
    expect(manifest.verification?.all_wave_receipts_passed).toBe(true)
    expect(manifest.verification?.locked_or_blind_overlap).toBe(0)
    expect(manifest.counts.total_pairs).toBe(91)
    expect(manifest.counts.active_pairs).toBe(63)
    expect(manifest.counts.active_cards).toBe(64)
    expect(manifest.counts.assets_total).toBe(128)
    expect(manifest.counts.non_active_pairs).toBe(28)
    expect(manifest.counts.superseded_pairs).toBe(2)
    expect(manifest.counts.deferred_routes).toBe(2)
    expect(manifest.superseded_pairs).toEqual({
      w3a_01: 'w5_01',
      w3a_03: 'w5_02',
    })
    expect(manifest.pairs.w3a_01).toBeUndefined()
    expect(manifest.pairs.w3a_03).toBeUndefined()

    let checkedAssets = 0
    for (const pair of Object.values(manifest.pairs)) {
      for (const route of pair.routes) {
        if (route.status !== 'active') {
          expect(route.card_id).toBeNull()
          continue
        }
        expect(route.card_id).toBeTruthy()
        expect(route.assets).toBeDefined()
        for (const lang of ['es', 'en'] as const) {
          const asset = route.assets![lang]
          const absPath = resolve(PUBLIC_DIR, asset.path)
          expect(existsSync(absPath), `missing ${asset.path}`).toBe(true)
          const buf = readFileSync(absPath)
          expect(buf.byteLength).toBe(asset.bytes)
          const sha = createHash('sha256').update(buf).digest('hex')
          expect(sha, `sha mismatch for ${asset.path}`).toBe(asset.sha256)
          expect(asset.width).toBe(1400)
          checkedAssets++
        }
      }
    }
    expect(checkedAssets).toBe(128)
  })
})

describe('confirmarVisualRouter — deterministic Wave 2/3A/3B routing contract', () => {
  it('p01 routes West (cyrbia/cythera) and East Napo (lativitta/malleti) cleanly with content-hashed URLs', () => {
    const west = resolveRegionalRoute(manifest, {
      pairId: 'p01',
      subspeciesA: '  CYRBIA  ',
      locale: 'es',
      rootUrl: ROOT_URL,
    })
    const expectedWestHash = manifest.pairs.p01.routes[0].assets!.es.sha256.slice(0, 10)
    expect(west.status).toBe('active')
    expect(west.cardId).toBe('p01_west')
    expect(west.regionFallback).toBe(false)
    expect(west.canSwitchRegion).toBe(false)
    expect(west.regionBadge).toBe('Occidente')
    expect(west.imageUrl).toBe(`https://example.test/ithomiini/identificar/confirmar/cards/es/p01_west.webp?v=${expectedWestHash}`)
    expect(west.alternateCards.map(a => a.cardId)).toEqual(['p01_west', 'p01_east_napo'])

    const eastNapo = resolveRegionalRoute(manifest, {
      pairId: 'p01',
      subspeciesB: '  Heliconius melpomene MALLETI ',
      sideAndes: 'East',
      locale: 'en',
      rootUrl: ROOT_URL,
    })
    expect(eastNapo.status).toBe('active')
    expect(eastNapo.cardId).toBe('p01_east_napo')
    expect(eastNapo.canSwitchRegion).toBe(false)
    expect(eastNapo.regionBadge).toBe('East · Napo & Amazonia')
    expect(eastNapo.imageUrl).toContain('/cards/en/p01_east_napo.webp?v=')
  })

  it('matches full trinomial subspecies names against bare-epithet aliases (Fable R1-F1)', () => {
    const p05 = resolveRegionalRoute(manifest, {
      pairId: 'p05',
      subspeciesB: 'Mechanitis messenoides messenoides',
      locale: 'es',
    })
    expect(p05.status).toBe('active')
    expect(p05.cardId).toBe('p05')

    const w3a07 = resolveRegionalRoute(manifest, {
      pairId: 'w3a_07',
      subspeciesA: 'Hypothyris euclea peruviana',
      locale: 'en',
    })
    expect(w3a07.status).toBe('active')
    expect(w3a07.cardId).toBe('w3a_07')
  })

  it('applies Wave 3B side-only contract so subspecies predictions never hide valid W3B cards (Sol R1-F2 / Fable R1-F2)', () => {
    const w3b01 = resolveRegionalRoute(manifest, {
      pairId: 'w3b_01',
      subspeciesA: 'Adelpha irmina irmina',
      locale: 'es',
    })
    expect(w3b01.status).toBe('active')
    expect(w3b01.cardId).toBe('w3b_01')
    expect(w3b01.regionFallback).toBe(true)

    const w3b13 = resolveRegionalRoute(manifest, {
      pairId: 'w3b_13',
      subspeciesB: 'Morpho helenor theodorus',
      locale: 'en',
    })
    expect(w3b13.status).toBe('active')
    expect(w3b13.cardId).toBe('w3b_13')
    expect(w3b13.regionFallback).toBe(true)
  })

  it('p01 Pastaza foothills (notabilis/plesseni) and South (etylus/ecuadorensis) return explicit null unavailable_deferred_ring, NEVER overridden by preferredCardId (Sol R1-F3)', () => {
    for (const input of [
      { subspeciesA: 'notabilis', sideAndes: 'East' as const },
      { subspeciesA: 'notabilis', preferredCardId: 'p01_west' },
      { subspeciesB: 'Heliconius melpomene plesseni', preferredCardId: 'p01_east_napo' },
      { subspeciesA: 'etylus', sideAndes: 'East' as const },
      { subspeciesB: 'ecuadorensis' },
    ]) {
      const res = resolveRegionalRoute(manifest, { pairId: 'p01', ...input, locale: 'es' })
      expect(res.cardId).toBeNull()
      expect(res.status).toBe('unavailable_deferred_ring')
      expect(res.canSwitchRegion).toBe(false)
      expect(res.imageUrl).toBeNull()
      expect(res.honestNote).toBeTruthy()
      expect(res.honestNote).not.toMatch(/CAM\d+|unlocked|A1|B1/i)
    }
  })

  it('rejects conflicting subspecies inputs and conflicting side+subspecies inputs even if preferredCardId is set', () => {
    const conflictSsp = resolveRegionalRoute(manifest, {
      pairId: 'p01',
      subspeciesA: 'cyrbia',
      subspeciesB: 'malleti',
      preferredCardId: 'p01_west',
      locale: 'en',
    })
    expect(conflictSsp.cardId).toBeNull()
    expect(conflictSsp.status).toBe('conflicting_subspecies_inputs')

    const conflictSide = resolveRegionalRoute(manifest, {
      pairId: 'p01',
      sideAndes: 'East',
      subspeciesA: 'cyrbia',
      preferredCardId: 'p01_west',
      locale: 'en',
    })
    expect(conflictSide.cardId).toBeNull()
    expect(conflictSide.status).toBe('conflicting_side_and_subspecies')

    const unknownSsp = resolveRegionalRoute(manifest, {
      pairId: 'p01',
      subspeciesA: 'hydara',
      preferredCardId: 'p01_west',
      locale: 'en',
    })
    expect(unknownSsp.cardId).toBeNull()
    expect(unknownSsp.status).toBe('unknown_subspecies')
  })

  it('handles side-only match, no_card_for_side, unspecified default fallback, and manual region switch only on unspecified fallback', () => {
    const eastOnly = resolveRegionalRoute(manifest, { pairId: 'p01', sideAndes: 'East', preferredCardId: 'p01_west', locale: 'en' })
    expect(eastOnly.cardId).toBe('p01_east_napo')
    expect(eastOnly.canSwitchRegion).toBe(false)

    expect(resolveRegionalRoute(manifest, { pairId: 'p01', sideAndes: 'West', locale: 'en' }).cardId).toBe('p01_west')

    const fallback = resolveRegionalRoute(manifest, { pairId: 'p01', locale: 'en' })
    expect(fallback.cardId).toBe('p01_west')
    expect(fallback.regionFallback).toBe(true)
    expect(fallback.canSwitchRegion).toBe(true)

    const switched = resolveRegionalRoute(manifest, { pairId: 'p01', locale: 'en', preferredCardId: 'p01_east_napo' })
    expect(switched.cardId).toBe('p01_east_napo')
    expect(switched.regionFallback).toBe(false)
    expect(switched.canSwitchRegion).toBe(true)

    // East-only pair p02 queried with West -> no_card_for_side
    const p02West = resolveRegionalRoute(manifest, { pairId: 'p02', sideAndes: 'West', locale: 'es' })
    expect(p02West.cardId).toBeNull()
    expect(p02West.status).toBe('no_card_for_side')

    // West-only pair p06 queried with East -> no_card_for_side
    const p06East = resolveRegionalRoute(manifest, { pairId: 'p06', sideAndes: 'East', locale: 'es' })
    expect(p06East.cardId).toBeNull()
    expect(p06East.status).toBe('no_card_for_side')
  })

  it('returns explicit null and clean honest notes for all Wave 3B not_reliably_separable_in_photos and Wave 3A held/inseparable pairs', () => {
    const w3bInseparable = ['w3b_02', 'w3b_03', 'w3b_04', 'w3b_06', 'w3b_07', 'w3b_10', 'w3b_14', 'w3b_15']
    for (const pairId of w3bInseparable) {
      for (const locale of ['es', 'en'] as const) {
        const res = resolveRegionalRoute(manifest, { pairId, locale })
        expect(res.cardId).toBeNull()
        expect(res.status).toBe('not_reliably_separable_in_photos')
        expect(res.honestNote).toBeTruthy()
        expect(res.honestNote).not.toMatch(/CAM\d+|unlocked|A1|B1|ML gexp|Complete_but_verify/i)
      }
    }

    // w3a_01 was superseded by Wave 5 w5_01 (confirmed not_reliably_separable_in_photos)
    expect(resolveRegionalRoute(manifest, { pairId: 'w3a_01', sideAndes: 'East', locale: 'en' }).status).toBe('not_reliably_separable_in_photos')
    expect(resolveRegionalRoute(manifest, { pairId: 'w5_01', sideAndes: 'East', locale: 'en' }).status).toBe('not_reliably_separable_in_photos')
    expect(resolveRegionalRoute(manifest, { pairId: 'w3a_02', sideAndes: 'East', locale: 'en' }).status).toBe('held_suspect_reference_labels')
    expect(resolveRegionalRoute(manifest, { pairId: 'w3a_09', sideAndes: 'East', locale: 'en' }).status).toBe('inseparable_in_photos')
    expect(resolveRegionalRoute(manifest, { pairId: 'w3a_10', sideAndes: 'East', locale: 'en' }).status).toBe('inseparable_or_unverified_refs')
    expect(resolveRegionalRoute(manifest, { pairId: 'w3a_09', sideAndes: 'West', locale: 'en' }).status).toBe('no_card_for_side')
  })

  it('supersedes Wave 3A hold w3a_03 with active Wave 5 rescue w5_02 (Nessaea) and never lets a legacy null hold shadow it', () => {
    const directEs = resolveRegionalRoute(manifest, {
      pairId: 'w5_02',
      sideAndes: 'East',
      subspeciesA: 'Nessaea hewitsonii hewitsonii',
      locale: 'es',
      rootUrl: ROOT_URL,
    })
    expect(directEs.status).toBe('active')
    expect(directEs.pairId).toBe('w5_02')
    expect(directEs.cardId).toBe('w5_02')
    expect(directEs.taxonA).toBe('Nessaea hewitsonii hewitsonii')
    expect(directEs.taxonB).toBe('Nessaea obrinus obrinus')
    expect(directEs.viewBadge).toBe('Anverso machos · con cautela')
    expect(directEs.imageUrl).toContain('/cards/es/w5_02.webp?v=')

    // Querying legacy pair_id w3a_03 transparently redirects to active w5_02
    const legacyEn = resolveRegionalRoute(manifest, {
      pairId: 'w3a_03',
      sideAndes: 'East',
      locale: 'en',
      rootUrl: ROOT_URL,
    })
    expect(legacyEn.status).toBe('active')
    expect(legacyEn.pairId).toBe('w5_02')
    expect(legacyEn.cardId).toBe('w5_02')
    expect(legacyEn.imageUrl).toContain('/cards/en/w5_02.webp?v=')

    // Top-2 prediction matching for Nessaea resolves active w5_02 on East and hides on West
    const matched = resolveConfirmarVisualCard(manifest, {
      topPredictions: [
        { name: 'Nessaea obrinus', p: 0.56, subspecies: [{ name: 'Nessaea obrinus obrinus', p: 0.51 }] },
        { name: 'Nessaea hewitsonii', p: 0.39 },
      ],
      sideAndes: 'East',
      locale: 'es',
      rootUrl: ROOT_URL,
    })
    expect(matched?.displayKind).toBe('card')
    expect(matched?.cardId).toBe('w5_02')
    expect(matched?.taxonA).toBe('Nessaea hewitsonii hewitsonii')

    const westNessaea = resolveConfirmarVisualCard(manifest, {
      topPredictions: [
        { name: 'Nessaea hewitsonii', p: 0.6 },
        { name: 'Nessaea obrinus', p: 0.35 },
      ],
      sideAndes: 'West',
      locale: 'es',
    })
    expect(westNessaea?.status).toBe('no_card_for_side')
    expect(westNessaea?.displayKind).toBe('hidden')

    // Defense-in-depth: even in a synthetic manifest containing an older null hold before w5_02, active wins
    const synthetic: ConfirmarVisualManifest = {
      ...manifest,
      pairs: {
        w3a_03: {
          pair_id: 'w3a_03',
          wave: 'wave3a_dv',
          routing_mode: 'subspecies_and_side',
          species_a: 'Nessaea hewitsonii',
          species_b: 'Nessaea obrinus',
          classification: 'held_sex_confounded_small_n',
          default_card_id: null,
          routes: [
            {
              card_id: null,
              status: 'held_sex_confounded_small_n',
              side_andes: ['East'],
              side_default_for: [],
              region_bucket: 'oriente',
              region_es: 'Oriente',
              region_en: 'East Ecuador',
              taxon_a: 'Nessaea hewitsonii',
              taxon_b: 'Nessaea obrinus obrinus',
              subspecies_a_aliases: ['hewitsonii'],
              subspecies_b_aliases: ['obrinus'],
            },
          ],
        },
        w5_02: manifest.pairs.w5_02,
      },
    }
    const syntheticMatch = resolveConfirmarVisualCard(synthetic, {
      topPredictions: [
        { name: 'Nessaea hewitsonii', p: 0.6 },
        { name: 'Nessaea obrinus', p: 0.35 },
      ],
      sideAndes: 'East',
      locale: 'en',
      rootUrl: ROOT_URL,
    })
    expect(syntheticMatch?.displayKind).toBe('card')
    expect(syntheticMatch?.cardId).toBe('w5_02')
  })

  it('routes Wave 4 (w4_05 East, w4_10 West, w4_13 held_small_n_co_mimics) and Wave 6 (w6_01 H. numata vs M. menophilus, w6_16 West, w6_13 held)', () => {
    const w405 = resolveConfirmarVisualCard(manifest, {
      topPredictions: [
        { name: 'Hypothyris anastasia', p: 0.55, subspecies: [{ name: 'Hypothyris anastasia honesta', p: 0.5 }] },
        { name: 'Mechanitis messenoides', p: 0.38, subspecies: [{ name: 'Mechanitis messenoides phasianita', p: 0.34 }] },
      ],
      sideAndes: 'East',
      locale: 'es',
      rootUrl: ROOT_URL,
    })
    expect(w405?.displayKind).toBe('card')
    expect(w405?.cardId).toBe('w4_05')
    expect(w405?.regionBadge).toBe('Oriente')
    expect(w405?.imageUrl).toContain('/cards/es/w4_05.webp?v=')

    const w410West = resolveRegionalRoute(manifest, { pairId: 'w4_10', sideAndes: 'West', locale: 'en', rootUrl: ROOT_URL })
    expect(w410West.status).toBe('active')
    expect(w410West.cardId).toBe('w4_10')
    expect(w410West.taxonA).toBe('Aeria eurimedia latistriga')
    expect(w410West.taxonB).toBe('Scada zibia xanthina')
    expect(resolveRegionalRoute(manifest, { pairId: 'w4_10', sideAndes: 'East', locale: 'en' }).status).toBe('no_card_for_side')

    const w413Held = resolveConfirmarVisualCard(manifest, {
      topPredictions: [
        { name: 'Oleria padilla', p: 0.58 },
        { name: 'Pteronymia alissa', p: 0.35 },
      ],
      sideAndes: 'West',
      locale: 'es',
    })
    expect(w413Held?.displayKind).toBe('honest_note')
    expect(w413Held?.status).toBe('held_small_n_co_mimics')
    expect(w413Held?.honestNote).toBe('Aún no hay una guía visual revisada para esta región.')

    const w601 = resolveConfirmarVisualCard(manifest, {
      topPredictions: [
        { name: 'Melinaea menophilus', p: 0.57, subspecies: [{ name: 'Melinaea menophilus zaneka', p: 0.51 }] },
        { name: 'Heliconius numata', p: 0.36, subspecies: [{ name: 'Heliconius numata bicoloratus', p: 0.31 }] },
      ],
      sideAndes: 'East',
      locale: 'en',
      rootUrl: ROOT_URL,
    })
    expect(w601?.displayKind).toBe('card')
    expect(w601?.cardId).toBe('w6_01')
    expect(w601?.regionBadge).toBe('East Ecuador')
    expect(w601?.viewBadge).toBe('Upperside & underside')
    expect(w601?.imageUrl).toContain('/cards/en/w6_01.webp?v=')
    expect(resolveRegionalRoute(manifest, { pairId: 'w6_01', sideAndes: 'West', locale: 'es' }).status).toBe('no_card_for_side')

    const w616 = resolveRegionalRoute(manifest, { pairId: 'w6_16', sideAndes: 'West', locale: 'es', rootUrl: ROOT_URL })
    expect(w616.status).toBe('active')
    expect(w616.cardId).toBe('w6_16')
    expect(w616.imageUrl).toContain('/cards/es/w6_16.webp?v=')

    const w613 = resolveConfirmarVisualCard(manifest, {
      topPredictions: [
        { name: 'Godyris duillia', p: 0.54 },
        { name: 'Hypomenitis alphesiboea', p: 0.41 },
      ],
      sideAndes: 'East',
      locale: 'en',
    })
    expect(w613?.displayKind).toBe('honest_note')
    expect(w613?.status).toBe('held_small_n_co_mimics')
    expect(w613?.honestNote).toBe('No checked visual guide for this region yet.')

    // Wave 5 deduplicated Morpho achilles vs Morpho helenor -> honest_note
    const morpho = resolveConfirmarVisualCard(manifest, {
      topPredictions: [
        { name: 'Morpho achilles', p: 0.52 },
        { name: 'Morpho helenor', p: 0.44 },
      ],
      sideAndes: 'East',
      locale: 'en',
    })
    expect(morpho?.displayKind).toBe('honest_note')
    expect(morpho?.pairId).toBe('w5_01')
    expect(morpho?.status).toBe('not_reliably_separable_in_photos')
  })
})

describe('confirmarVisualRouter — /identificar/ prediction matching & GPS helper', () => {
  it('matches reversed top-2 predictions and aligns subspecies A/B properly', () => {
    const res = resolveConfirmarVisualCard(manifest, {
      topPredictions: [
        { name: 'Heliconius melpomene', p: 0.62, subspecies: [{ name: 'Heliconius melpomene malleti', p: 0.55 }] },
        { name: 'Heliconius erato', p: 0.31, subspecies: [{ name: 'Heliconius erato lativitta', p: 0.28 }] },
      ],
      locale: 'es',
      rootUrl: ROOT_URL,
    })
    expect(res).not.toBeNull()
    expect(res!.displayKind).toBe('card')
    expect(res!.cardId).toBe('p01_east_napo')
    expect(res!.imageUrl).toContain('/cards/es/p01_east_napo.webp?v=')
  })

  it('switches imageUrl and badges cleanly between ES and EN locales', () => {
    const base = {
      topPredictions: [
        { name: 'Adelpha irmina', p: 0.58 },
        { name: 'Adelpha saundersii', p: 0.36 },
      ],
      sideAndes: 'East' as const,
      rootUrl: ROOT_URL,
    }
    const es = resolveConfirmarVisualCard(manifest, { ...base, locale: 'es' })!
    const en = resolveConfirmarVisualCard(manifest, { ...base, locale: 'en' })!
    expect(es.cardId).toBe('w3b_01')
    expect(en.cardId).toBe('w3b_01')
    expect(es.imageUrl).toContain('/cards/es/w3b_01.webp?v=')
    expect(en.imageUrl).toContain('/cards/en/w3b_01.webp?v=')
    expect(es.viewBadge).toBe('Solo vista ventral')
    expect(en.viewBadge).toBe('Underside view only')
    expect(es.regionBadge).toBe('Oriente')
    expect(en.regionBadge).toBe('East Ecuador')
  })

  it('resolves ONLY the compared pair when comparedSpecies is chosen, returning null if unmatched (Sol R1-F4 / Fable R1-F8)', () => {
    const comparedMatch = resolveConfirmarVisualCard(manifest, {
      topPredictions: [
        { name: 'Mechanitis mazaeus', p: 0.52 },
        { name: 'Mechanitis lysimnia', p: 0.25 },
        { name: 'Mechanitis polymnia', p: 0.18 },
      ],
      comparedSpecies: 'Mechanitis polymnia',
      sideAndes: 'East',
      locale: 'en',
    })
    expect(comparedMatch?.cardId).toBe('w3a_04')

    const comparedUnmatched = resolveConfirmarVisualCard(manifest, {
      topPredictions: [
        { name: 'Heliconius erato', p: 0.55 },
        { name: 'Heliconius melpomene', p: 0.35 },
        { name: 'Danaus plexippus', p: 0.05 },
      ],
      comparedSpecies: 'Danaus plexippus',
      locale: 'en',
    })
    expect(comparedUnmatched).toBeNull()
  })

  it('supports intraspecific pairs (p07, p15) only when dominant subspecies belongs to the pair (Fable R1-F6)', () => {
    const p07 = resolveConfirmarVisualCard(manifest, {
      topPredictions: [
        {
          name: 'Hypothyris euclea',
          p: 0.88,
          subspecies: [
            { name: 'Hypothyris euclea intermedia', p: 0.51 },
            { name: 'Hypothyris euclea pyrippe', p: 0.34 },
          ],
        },
        { name: 'Hypothyris anastasia', p: 0.05 },
      ],
      sideAndes: 'East',
      locale: 'es',
    })
    expect(p07?.cardId).toBe('p07')

    // Dominant subspecies is valora (not intermedia/pyrippe) -> must NOT falsely surface p07
    const valoraDom = resolveConfirmarVisualCard(manifest, {
      topPredictions: [
        {
          name: 'Hypothyris euclea',
          p: 0.9,
          subspecies: [
            { name: 'Hypothyris euclea valora', p: 0.7 },
            { name: 'Hypothyris euclea intermedia', p: 0.18 },
          ],
        },
      ],
      sideAndes: 'East',
      locale: 'es',
    })
    expect(valoraDom).toBeNull()

    const p15 = resolveConfirmarVisualCard(manifest, {
      topPredictions: [{ name: 'Godyris zavaleta', p: 0.91 }],
      locale: 'en',
    })
    expect(p15?.cardId).toBe('p15')
  })

  it('surfaces honest_note for inseparable top-2 pairs and hides no_card_for_side', () => {
    const inseparable = resolveConfirmarVisualCard(manifest, {
      topPredictions: [
        { name: 'Gorgythion begga', p: 0.54 },
        { name: 'Gorgythion beggina', p: 0.41 },
      ],
      locale: 'en',
    })
    expect(inseparable?.displayKind).toBe('honest_note')
    expect(inseparable?.cardId).toBeNull()
    expect(inseparable?.status).toBe('not_reliably_separable_in_photos')

    const wrongSide = resolveConfirmarVisualCard(manifest, {
      topPredictions: [
        { name: 'Mechanitis messenoides', p: 0.6 },
        { name: 'Melinaea mothone', p: 0.3 },
      ],
      sideAndes: 'West',
      locale: 'es',
    })
    expect(wrongSide?.status).toBe('no_card_for_side')
    expect(wrongSide?.displayKind).toBe('hidden')
  })

  it('classifies clear East/West Ecuador GPS sites conservatively and returns null in inter-Andean towns incl. Quito & Tulcán (Fable R1-F5)', () => {
    expect(ecuadorSideFromGps(-0.64, -76.15)).toBe('East') // Tiputini
    expect(ecuadorSideFromGps(-0.95, -77.86)).toBe('East') // Tena / Ikiam
    expect(ecuadorSideFromGps(0.09, -76.89)).toBe('East') // Lago Agrio
    expect(ecuadorSideFromGps(-4.07, -78.95)).toBe('East') // Zamora
    expect(ecuadorSideFromGps(-0.05, -78.78)).toBe('West') // Mindo
    expect(ecuadorSideFromGps(-2.19, -79.89)).toBe('West') // Guayaquil
    expect(ecuadorSideFromGps(0.96, -79.65)).toBe('West') // Esmeraldas
    expect(ecuadorSideFromGps(-0.18, -78.47)).toBeNull() // Quito
    expect(ecuadorSideFromGps(0.81, -77.72)).toBeNull() // Tulcán
    expect(ecuadorSideFromGps(4.71, -74.07)).toBeNull() // Bogotá (outside Ecuador)
  })
})

describe('confirmarVisualRouter — subspecies names as the head sends them reach every card (audit 25 Nov 2026, 3.1)', () => {
  it('compares names without rank markers or a trailing question mark', () => {
    expect(normSubspecies('gudruna?')).toBe('gudruna')
    expect(normSubspecies('f. ithomiola')).toBe('ithomiola')
    expect(normSubspecies('  Heliconius  cydno\u00a0F. White ')).toBe('heliconius cydno white')
    expect(normSubspecies('Heliconius erato ssp. cyrbia')).toBe('heliconius erato cyrbia')
    expect(normSubspecies('Heliconius erato subsp. cyrbia ?')).toBe('heliconius erato cyrbia')
    // A name that only begins like a marker is a name.
    expect(normSubspecies('Hypoleria alema forma')).toBe('hypoleria alema forma')
    expect(normSubspecies(null)).toBe('')
  })

  it('the list carries no rank marker, question mark or unnamed form in a subspecies-routed pair', () => {
    const broken = /\?|(?:^|\s)(?:ssp|subsp|sp|f|nov)(?:[\s.-]|$)|-\s|\s-/
    let aliases = 0
    for (const pair of Object.values(manifest.pairs)) {
      if (pair.routing_mode !== 'subspecies_and_side') continue
      for (const route of pair.routes) {
        for (const alias of [...route.subspecies_a_aliases, ...route.subspecies_b_aliases]) {
          aliases++
          expect(broken.test(alias.toLowerCase()), `${pair.pair_id}: ${alias}`).toBe(false)
        }
      }
    }
    expect(aliases).toBeGreaterThan(250)
    // The builder checked every active side against the subspecies the head names.
    expect(manifest.verification?.head_subspecies_checked).toBe(true)
    expect(manifest.verification?.subspecies_sides_reachable).toBe(99)
    expect(manifest.verification?.subspecies_sides_without_head_subspecies).toBe(5)
  })

  // The head's leading subspecies, with no side known: before, each of these four sides gave `unknown_subspecies`.
  const card = (pairId: string, ssp: { subspeciesA?: string; subspeciesB?: string }) =>
    resolveRegionalRoute(manifest, { pairId, locale: 'es', rootUrl: ROOT_URL, ...ssp })

  it('p14 side A: Heliconius cydno alithea (West Ecuador) reaches the cydno / timareta card', () => {
    for (const subspeciesA of ['Heliconius cydno alithea', 'alithea', 'Heliconius cydno ssp. alithea']) {
      const r = card('p14', { subspeciesA })
      expect(r.status, subspeciesA).toBe('active')
      expect(r.cardId).toBe('p14')
      expect(r.regionFallback).toBe(false)
    }
    // A subspecies of another country still shows nothing rather than a guess.
    expect(card('p14', { subspeciesA: 'Heliconius cydno chioneus' }).status).toBe('unknown_subspecies')
    // The other side of the same card was reachable and stays so.
    expect(card('p14', { subspeciesB: 'Heliconius timareta timareta' }).cardId).toBe('p14')
  })

  it('w4_11 side A: Heliconius cydno alithea reaches the cydno / eleuchia card', () => {
    const r = card('w4_11', { subspeciesA: 'Heliconius cydno alithea' })
    expect(r.status).toBe('active')
    expect(r.cardId).toBe('w4_11')
    expect(r.guideSide).toBe('West')
    expect(card('w4_11', { subspeciesA: 'Heliconius cydno alithea', subspeciesB: 'Heliconius eleuchia primularis' }).cardId).toBe('w4_11')
  })

  it('w6_05 both sides: Eresia eunice gudruna and Eresia pelonia ithomiola reach the card', () => {
    expect(card('w6_05', { subspeciesA: 'Eresia eunice gudruna' }).cardId).toBe('w6_05')
    expect(card('w6_05', { subspeciesB: 'Eresia pelonia ithomiola' }).cardId).toBe('w6_05')
    expect(card('w6_05', { subspeciesB: 'Eresia pelonia pelonia' }).cardId).toBe('w6_05')
    // The head writes the third form «callonia»; the collection's «calonia» is kept beside it.
    expect(card('w6_05', { subspeciesB: 'Eresia pelonia callonia' }).cardId).toBe('w6_05')
    expect(card('w6_05', { subspeciesA: 'Eresia eunice gudruna', subspeciesB: 'Eresia pelonia ithomiola' }).cardId).toBe('w6_05')
  })

  it('the same four sides from the page: the two leading species with their leading subspecies', () => {
    const shown = (a: [string, string], b: [string, string]) =>
      resolveConfirmarVisualCard(manifest, {
        topPredictions: [
          { name: a[0], p: 0.55, subspecies: a[1] ? [{ name: a[1], p: 0.5 }] : [] },
          { name: b[0], p: 0.35, subspecies: b[1] ? [{ name: b[1], p: 0.3 }] : [] },
        ],
        sideAndes: null,
        locale: 'es',
        rootUrl: ROOT_URL,
      })
    for (const [a, b, id] of [
      [['Heliconius cydno', 'Heliconius cydno alithea'], ['Heliconius timareta', ''], 'p14'],
      [['Heliconius eleuchia', ''], ['Heliconius cydno', 'Heliconius cydno alithea'], 'w4_11'],
      [['Eresia eunice', 'Eresia eunice gudruna'], ['Eresia pelonia', ''], 'w6_05'],
      [['Eresia pelonia', 'Eresia pelonia ithomiola'], ['Eresia eunice', ''], 'w6_05'],
    ] as [[string, string], [string, string], string][]) {
      const match = shown(a, b)
      expect(match?.displayKind, id).toBe('card')
      expect(match?.cardId).toBe(id)
      expect(match?.imageUrl).toContain(`/cards/es/${id}.webp?v=`)
    }
  })

  it('names the head never writes that way are read too (a rank before the epithet)', () => {
    expect(card('p01', { subspeciesA: 'Heliconius erato ssp. cyrbia' }).cardId).toBe('p01_west')
    expect(card('p01', { subspeciesA: 'subsp. lativitta' }).cardId).toBe('p01_east_napo')
  })
})

describe('confirmarVisualRouter — no side known (audit 25 Nov 2026, 3.2)', () => {
  // Approximate positions. The first three are west-slope cloud forest inside the 0.35° corridor.
  const TANDAYAPA = [0.0044, -78.6769] as const
  const BELLAVISTA = [-0.0126, -78.6817] as const
  const NANEGALITO = [0.0655, -78.6781] as const

  it('a point inside the corridor gets no side, and leans only when clearly off the ridge line', () => {
    for (const [lat, lon] of [TANDAYAPA, BELLAVISTA, NANEGALITO]) {
      expect(ecuadorSideFromGps(lat, lon)).toBeNull()
      expect(ecuadorSideLeanFromGps(lat, lon)).toBe('West')
    }
    expect(ecuadorSideLeanFromGps(-3.69, -79.61)).toBe('West') // Zaruma
    expect(ecuadorSideLeanFromGps(-1.396, -78.42)).toBe('East') // Baños
    expect(ecuadorSideLeanFromGps(-0.37, -78.14)).toBe('East') // Papallacta
    // Near the line: no lean (the valley towns).
    expect(ecuadorSideLeanFromGps(-0.18, -78.47)).toBeNull() // Quito
    expect(ecuadorSideLeanFromGps(0.81, -77.72)).toBeNull() // Tulcán
    expect(ecuadorSideLeanFromGps(-2.9, -79.0)).toBeNull() // Cuenca
    expect(ecuadorSideLeanFromGps(-0.93, -78.61)).toBeNull() // Latacunga
    // Where the side is known there is nothing to lean: the side itself is the answer.
    expect(ecuadorSideLeanFromGps(-0.05, -78.78)).toBeNull() // Mindo (West)
    expect(ecuadorSideLeanFromGps(-0.95, -77.86)).toBeNull() // Tena (East)
    expect(ecuadorSideLeanFromGps(4.71, -74.07)).toBeNull() // Bogotá
    expect(ecuadorSideLeanFromGps(Number.NaN, -78.6)).toBeNull()
  })

  it('counts what a photo with no side shows: 63 pairs, 52 of them a guide for one side only', () => {
    let shown = 0
    let oneSide = 0
    for (const pair of Object.values(manifest.pairs)) {
      const r = resolveRegionalRoute(manifest, { pairId: pair.pair_id, locale: 'es', rootUrl: ROOT_URL })
      if (r.status !== 'active') continue
      shown++
      expect(r.regionFallback).toBe(true)
      if (r.guideSide && !r.canSwitchRegion) oneSide++
    }
    expect(shown).toBe(63)
    expect(oneSide).toBe(52)
  })

  it('Tandayapa: a guide for the East alone is hidden, a guide for the West is shown as a fallback, a guide for both stays', () => {
    const lean = ecuadorSideLeanFromGps(...TANDAYAPA)
    const at = (pairId: string) => resolveRegionalRoute(manifest, { pairId, sideAndes: ecuadorSideFromGps(...TANDAYAPA), sideLean: lean, locale: 'es', rootUrl: ROOT_URL })
    // East only (Eresia eunice / pelonia): without the lean it was shown with nothing but its badge to say so.
    expect(resolveRegionalRoute(manifest, { pairId: 'w6_05', locale: 'es', rootUrl: ROOT_URL }).cardId).toBe('w6_05')
    const east = at('w6_05')
    expect(east.status).toBe('no_card_for_side')
    expect(east.cardId).toBeNull()
    // West only (Heliconius cydno / eleuchia): shown, still as a fallback (the side is not certain), with its side.
    const west = at('w4_11')
    expect(west.cardId).toBe('w4_11')
    expect(west.regionFallback).toBe(true)
    expect(west.guideSide).toBe('West')
    // One card for both sides: no side to say, and it stays.
    const both = at('p14')
    expect(both.cardId).toBe('p14')
    expect(both.guideSide).toBeNull()
    // A guide per region: the lean chooses none, the switch stays.
    const regions = at('p01')
    expect(regions.cardId).toBe('p01_west')
    expect(regions.canSwitchRegion).toBe(true)
    expect(regions.guideSide).toBe('West')
  })

  it('the lean hides every one-sided guide of the other side and nothing else, and never overrides a side', () => {
    for (const lean of ['East', 'West'] as const) {
      const other = lean === 'East' ? 'West' : 'East'
      let hidden = 0
      for (const pair of Object.values(manifest.pairs)) {
        const plain = resolveRegionalRoute(manifest, { pairId: pair.pair_id, locale: 'es', rootUrl: ROOT_URL })
        const leaned = resolveRegionalRoute(manifest, { pairId: pair.pair_id, sideLean: lean, locale: 'es', rootUrl: ROOT_URL })
        const sides = new Set(pair.routes.filter(r => r.status === 'active').flatMap(r => r.side_andes))
        if (plain.status === 'active' && !sides.has(lean)) {
          expect(sides.has(other)).toBe(true)
          expect(leaned.status, pair.pair_id).toBe('no_card_for_side')
          hidden++
        } else {
          expect(leaned, pair.pair_id).toEqual(plain)
        }
      }
      // 39 pairs have a guide for the East alone, 13 for the West alone.
      expect(hidden).toBe(lean === 'West' ? 39 : 13)
    }
    // A side the person chose, or the GPS gave, is the answer; the lean is not asked.
    expect(resolveRegionalRoute(manifest, { pairId: 'w6_05', sideAndes: 'East', sideLean: 'West', locale: 'es', rootUrl: ROOT_URL }).cardId).toBe('w6_05')
  })

  it('a subspecies the head names does not bring back a guide made for the other side alone', () => {
    // Tandayapa, the head says Eresia eunice gudruna: the East-only guide stays hidden, as it is at Mindo (side West).
    const leaned = resolveRegionalRoute(manifest, { pairId: 'w6_05', subspeciesA: 'Eresia eunice gudruna', sideLean: 'West', locale: 'es', rootUrl: ROOT_URL })
    expect(leaned.status).toBe('no_card_for_side')
    expect(leaned.cardId).toBeNull()
    expect(resolveRegionalRoute(manifest, { pairId: 'w6_05', subspeciesA: 'Eresia eunice gudruna', sideAndes: 'West', locale: 'es', rootUrl: ROOT_URL }).status).toBe('conflicting_side_and_subspecies')
    // Every one-sided pair, every alias it lists, on either taxon: none shows with the lean on the other side.
    let tried = 0
    for (const pair of Object.values(manifest.pairs)) {
      const guides = pair.routes.filter(r => r.status === 'active')
      const sides = new Set(guides.flatMap(r => r.side_andes))
      if (guides.length === 0 || sides.size !== 1) continue
      const lean = sides.has('East') ? 'West' : 'East'
      for (const route of guides) {
        for (const [key, aliases] of [['subspeciesA', route.subspecies_a_aliases], ['subspeciesB', route.subspecies_b_aliases]] as const) {
          for (const alias of aliases) {
            tried++
            expect(resolveRegionalRoute(manifest, { pairId: pair.pair_id, [key]: alias, sideLean: lean, locale: 'es', rootUrl: ROOT_URL }).cardId, `${pair.pair_id} ${alias}`).toBeNull()
          }
        }
      }
    }
    expect(tried).toBeGreaterThan(150)
    // The lean on the guide's own side, or a pair with a guide per region: the subspecies routes as with no position.
    expect(resolveRegionalRoute(manifest, { pairId: 'w6_05', subspeciesA: 'Eresia eunice gudruna', sideLean: 'East', locale: 'es', rootUrl: ROOT_URL }).cardId).toBe('w6_05')
    expect(resolveRegionalRoute(manifest, { pairId: 'p01', subspeciesA: 'Heliconius erato lativitta', sideLean: 'West', locale: 'es', rootUrl: ROOT_URL }).cardId).toBe('p01_east_napo')
  })

  it('from the page: the two leading species at Tandayapa', () => {
    const at = (a: string, b: string) =>
      resolveConfirmarVisualCard(manifest, {
        topPredictions: [
          { name: a, p: 0.6 },
          { name: b, p: 0.3 },
        ],
        sideAndes: ecuadorSideFromGps(...TANDAYAPA),
        sideLean: ecuadorSideLeanFromGps(...TANDAYAPA),
        locale: 'es',
        rootUrl: ROOT_URL,
      })
    expect(at('Eresia eunice', 'Eresia pelonia')?.displayKind).toBe('hidden')
    const west = at('Heliconius cydno', 'Heliconius eleuchia')
    expect(west?.displayKind).toBe('card')
    expect(west?.regionFallback).toBe(true)
    expect(west?.guideSide).toBe('West')
  })
})
