import { createHash } from 'node:crypto'
import { existsSync, readFileSync } from 'node:fs'
import { resolve } from 'node:path'
import { describe, expect, it } from 'vitest'
import {
  CONFIRM_VISUAL_CARDS_ENABLED,
  confirmVisualCardsEnabled,
  ecuadorSideFromGps,
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

  it('ships 47 pairs, 35 active cards, and 70 SHA-256 verified ES/EN WebP assets', () => {
    expect(manifest.schema).toBe('confirmar-visual-cards/1')
    expect(manifest.counts.total_pairs).toBe(47)
    expect(manifest.counts.active_cards).toBe(35)
    expect(manifest.counts.assets_total).toBe(70)
    expect(manifest.counts.non_active_pairs).toBe(13)
    expect(manifest.counts.deferred_routes).toBe(2)

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
          expect(asset.width).toBeGreaterThanOrEqual(1200)
          checkedAssets++
        }
      }
    }
    expect(checkedAssets).toBe(70)
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

    expect(resolveRegionalRoute(manifest, { pairId: 'w3a_01', sideAndes: 'East', locale: 'en' }).status).toBe('held_sex_confounded_small_n')
    expect(resolveRegionalRoute(manifest, { pairId: 'w3a_02', sideAndes: 'East', locale: 'en' }).status).toBe('held_suspect_reference_labels')
    expect(resolveRegionalRoute(manifest, { pairId: 'w3a_09', sideAndes: 'East', locale: 'en' }).status).toBe('inseparable_in_photos')
    expect(resolveRegionalRoute(manifest, { pairId: 'w3a_10', sideAndes: 'East', locale: 'en' }).status).toBe('inseparable_or_unverified_refs')
    expect(resolveRegionalRoute(manifest, { pairId: 'w3a_09', sideAndes: 'West', locale: 'en' }).status).toBe('no_card_for_side')
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
