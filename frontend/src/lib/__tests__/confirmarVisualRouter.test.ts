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
  it('p01 routes West (cyrbia/cythera) and East Napo (lativitta/malleti) cleanly, case-insensitively and trimmed', () => {
    const west = resolveRegionalRoute(manifest, {
      pairId: 'p01',
      subspeciesA: '  CYRBIA  ',
      locale: 'es',
      rootUrl: ROOT_URL,
    })
    expect(west.status).toBe('active')
    expect(west.cardId).toBe('p01_west')
    expect(west.regionFallback).toBe(false)
    expect(west.regionBadge).toBe('Occidente')
    expect(west.imageUrl).toBe('https://example.test/ithomiini/identificar/confirmar/cards/es/p01_west.webp')
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
    expect(eastNapo.regionBadge).toBe('East · Napo & Amazonia')
    expect(eastNapo.imageUrl).toBe('https://example.test/ithomiini/identificar/confirmar/cards/en/p01_east_napo.webp')
  })

  it('p01 Pastaza foothills (notabilis/plesseni) and South (etylus/ecuadorensis) return explicit null unavailable_deferred_ring, NEVER substituting Napo', () => {
    for (const input of [
      { subspeciesA: 'notabilis', sideAndes: 'East' as const },
      { subspeciesB: 'Heliconius melpomene plesseni' },
      { subspeciesA: 'etylus', sideAndes: 'East' as const },
      { subspeciesB: 'ecuadorensis' },
    ]) {
      const res = resolveRegionalRoute(manifest, { pairId: 'p01', ...input, locale: 'es' })
      expect(res.cardId).toBeNull()
      expect(res.status).toBe('unavailable_deferred_ring')
      expect(res.imageUrl).toBeNull()
      expect(res.honestNote).toBeTruthy()
      expect(res.honestNote).not.toMatch(/CAM\d+|unlocked|A1|B1/i)
    }
  })

  it('rejects conflicting subspecies inputs and conflicting side+subspecies inputs', () => {
    const conflictSsp = resolveRegionalRoute(manifest, {
      pairId: 'p01',
      subspeciesA: 'cyrbia',
      subspeciesB: 'malleti',
      locale: 'en',
    })
    expect(conflictSsp.cardId).toBeNull()
    expect(conflictSsp.status).toBe('conflicting_subspecies_inputs')

    const conflictSide = resolveRegionalRoute(manifest, {
      pairId: 'p01',
      sideAndes: 'East',
      subspeciesA: 'cyrbia',
      locale: 'en',
    })
    expect(conflictSide.cardId).toBeNull()
    expect(conflictSide.status).toBe('conflicting_side_and_subspecies')

    const unknownSsp = resolveRegionalRoute(manifest, {
      pairId: 'p01',
      subspeciesA: 'hydara',
      locale: 'en',
    })
    expect(unknownSsp.cardId).toBeNull()
    expect(unknownSsp.status).toBe('unknown_subspecies')
  })

  it('handles side-only match, no_card_for_side, unspecified default fallback, and preferredCardId override', () => {
    expect(resolveRegionalRoute(manifest, { pairId: 'p01', sideAndes: 'East', locale: 'en' }).cardId).toBe('p01_east_napo')
    expect(resolveRegionalRoute(manifest, { pairId: 'p01', sideAndes: 'West', locale: 'en' }).cardId).toBe('p01_west')

    const fallback = resolveRegionalRoute(manifest, { pairId: 'p01', locale: 'en' })
    expect(fallback.cardId).toBe('p01_west')
    expect(fallback.regionFallback).toBe(true)

    const switched = resolveRegionalRoute(manifest, { pairId: 'p01', locale: 'en', preferredCardId: 'p01_east_napo' })
    expect(switched.cardId).toBe('p01_east_napo')
    expect(switched.regionFallback).toBe(false)

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
    // West side on East-only Wave 3A held pair -> no_card_for_side
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
    expect(res!.imageUrl).toContain('/cards/es/p01_east_napo.webp')
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
    expect(es.imageUrl).toContain('/cards/es/w3b_01.webp')
    expect(en.imageUrl).toContain('/cards/en/w3b_01.webp')
    expect(es.viewBadge).toBe('Solo vista ventral')
    expect(en.viewBadge).toBe('Underside view only')
    expect(es.regionBadge).toBe('Oriente')
    expect(en.regionBadge).toBe('East Ecuador')
  })

  it('prioritizes comparedSpecies over #2 and supports intraspecific pairs (p07, p15)', () => {
    const compared = resolveConfirmarVisualCard(manifest, {
      topPredictions: [
        { name: 'Mechanitis mazaeus', p: 0.52 },
        { name: 'Mechanitis lysimnia', p: 0.25 },
        { name: 'Mechanitis polymnia', p: 0.18 },
      ],
      comparedSpecies: 'Mechanitis polymnia',
      sideAndes: 'East',
      locale: 'en',
    })
    expect(compared?.cardId).toBe('w3a_04')

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

  it('classifies clear East/West Ecuador GPS sites conservatively and returns null in the inter-Andean valley or outside Ecuador', () => {
    expect(ecuadorSideFromGps(-0.64, -76.15)).toBe('East') // Tiputini
    expect(ecuadorSideFromGps(-0.95, -77.86)).toBe('East') // Tena / Ikiam
    expect(ecuadorSideFromGps(-4.07, -78.95)).toBe('East') // Zamora
    expect(ecuadorSideFromGps(-0.05, -78.78)).toBe('West') // Mindo
    expect(ecuadorSideFromGps(-2.19, -79.89)).toBe('West') // Guayaquil
    expect(ecuadorSideFromGps(0.96, -79.65)).toBe('West') // Esmeraldas
    expect(ecuadorSideFromGps(-0.18, -78.47)).toBeNull() // Quito (inter-Andean valley)
    expect(ecuadorSideFromGps(4.71, -74.07)).toBeNull() // Bogotá (outside Ecuador)
  })
})
