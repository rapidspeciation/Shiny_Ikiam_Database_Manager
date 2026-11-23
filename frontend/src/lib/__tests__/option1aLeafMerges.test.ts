import { describe, expect, it } from 'vitest'
import leafMergesDoc from '../../../../tools/identifier/leaf_merges_6fa6ab78.json'
import taxonomyDoc from '../../../../tools/identifier/taxonomy_6fa6ab78.json'
import {
  buildDisplaySpace,
  displayHeadMeta,
  LEAF_MERGES_6FA6AB78,
  leafScores,
  makeHead,
  speciesSides,
  speciesTop,
  subspeciesBySpecies,
  subspeciesTop,
  sumSpecies,
  type HeadMeta,
} from '../offlineId'
import { higherRanks, rank, type Taxonomy } from '../identifyResult'

// Encode a float32 in [-1, 1] to IEEE-754 float16 bits (exact for 0, +/-1, +/-0.5, +/-0.25, +/-0.125).
function f16(x: number): number {
  if (x === 0) return 0
  const sign = x < 0 ? 0x8000 : 0
  const ax = Math.abs(x)
  const exp = Math.floor(Math.log2(ax))
  const mant = Math.round((ax / 2 ** exp - 1) * 1024)
  return sign | ((exp + 15) << 10) | (mant & 0x3ff)
}

function buildAllPairsFixture(): { meta: HeadMeta; weights: ArrayBuffer; rawLeaves: string[] } {
  const merges = leafMergesDoc.merges as Record<string, string>
  const targets = [...new Set(Object.values(merges))].sort()
  const sources = Object.keys(merges).sort()
  // All 48 sources + all 42 existing targets (every target except Melinaea mothone messenina) + unmerged controls.
  const controls = [
    'Greta diaphanus quisqueya',
    'Mechanitis polymnia proceriformis',
    'Mechanitis polymnia eurydice',
    'Oleria onega janarilla',
  ]
  const rawSet = new Set<string>([...sources, ...targets.filter(t => t !== 'Melinaea mothone messenina'), ...controls])
  const rawLeaves = [...rawSet].sort()
  const species = [...new Set(rawLeaves.map(lf => lf.split(' ').slice(0, 2).join(' ')))].sort()
  const spIdx = new Map(species.map((s, i) => [s, i]))
  const leaf2sp = rawLeaves.map(lf => spIdx.get(lf.split(' ').slice(0, 2).join(' '))!)
  const isSsp = rawLeaves.map(lf => (lf.split(' ').length >= 3 ? '1' : '0')).join('')
  // Distinct geo bits: Melinaea mothone mothone = 7 (East Ecuador), Melinaea marsaeus/menophilus messenina = 1 (non-EC),
  // Greta diaphanus quisqueya = 1 (Hispaniola, non-EC), others = 7 (East) or b (West).
  const geo = rawLeaves
    .map(lf => {
      if (lf === 'Melinaea mothone mothone') return '7'
      if (lf === 'Melinaea marsaeus messenina' || lf === 'Melinaea menophilus messenina') return '1'
      if (lf === 'Greta diaphanus quisqueya') return '1'
      if (lf === 'Ithomia hymettia hymettia') return 'b'
      if (lf === 'Ithomia lagusa hymettia') return '7'
      return '7'
    })
    .join('')

  // 2-D unit weights with varied angles so every leaf gets deterministic, distinct non-zero softmax mass.
  const dim = 2
  const buf = new ArrayBuffer(rawLeaves.length * dim * 2)
  const weights = new Uint16Array(buf)
  for (let i = 0; i < rawLeaves.length; i++) {
    const t = (i % 7) - 3
    const a = t === 0 ? 1 : t > 0 ? 0.5 : -0.5
    const b = Math.sqrt(Math.max(0, 1 - a * a))
    weights[i * dim] = f16(a)
    weights[i * dim + 1] = f16(b)
  }

  const meta: HeadMeta = {
    schema: 'ithomiini-offline-head/2',
    head_sha256: LEAF_MERGES_6FA6AB78.head,
    weights: 'synthetic.bin',
    rows: rawLeaves.length,
    dim,
    K: 1,
    s: 30,
    ln_eps: 1e-5,
    ln_w: [1, 1],
    ln_b: [0, 0],
    species,
    leaf2sp,
    leaves: rawLeaves,
    is_ssp: isSsp,
    geo,
    full_photo_overrides: [],
  }
  return { meta, weights: buf, rawLeaves }
}

describe('Option 1A 48-pair display-time leaf merges (offlineId)', () => {
  it('bundles the exact approved 48-pair labels_q map with zero chains and quisqueya kept separate', () => {
    const merges = leafMergesDoc.merges as Record<string, string>
    expect(leafMergesDoc.schema).toBe('ithomiini-leaf-merges/1')
    expect(leafMergesDoc.source_sha256).toBe('05e847e69d187106955955cef615efd83aefe07a441014024f59bea3fca43603')
    expect(leafMergesDoc.count).toBe(48)
    expect(Object.keys(merges)).toHaveLength(48)
    expect(merges['Greta diaphanus quisqueya']).toBeUndefined()
    expect(merges['Greta diaphanus diaphanus']).toBe('Greta diaphanus')
    expect(merges['Mechanitis polymnia proceriformis']).toBeUndefined()
    expect(merges['Mechanitis polymnia eurydice']).toBeUndefined()
    expect(merges['Melinaea marsaeus mothone']).toBe('Melinaea mothone mothone')
    expect(merges['Melinaea menophilus mothone']).toBe('Melinaea mothone mothone')
    expect(merges['Melinaea marsaeus messenina']).toBe('Melinaea mothone messenina')
    expect(merges['Melinaea menophilus messenina']).toBe('Melinaea mothone messenina')
    for (const [oldLeaf, newLeaf] of Object.entries(merges)) {
      expect(oldLeaf).not.toBe(newLeaf)
      expect(merges[newLeaf]).toBeUndefined()
      // Every target species must exist in taxonomy_6fa6ab78.json so tribe/subfamily roll-up covers it.
      const sp = newLeaf.split(' ').slice(0, 2).join(' ')
      expect((taxonomyDoc.species as Record<string, unknown>)[sp]).toBeDefined()
    }
  })

  it('conserves probability exactly (sum == 1.0) and matches raw-leaf sum on all 48 pairs and unmerged controls', () => {
    const { meta, weights, rawLeaves } = buildAllPairsFixture()
    const rawHead = makeHead(meta, weights, null)
    const mergedHead = makeHead(meta, weights)

    expect(mergedHead.rawRows).toBe(rawLeaves.length)
    expect(mergedHead.old2new).not.toBeNull()
    // 48 old leaves removed, 1 brand-new leaf (Melinaea mothone messenina) added => -47 leaves.
    expect(mergedHead.meta.leaves!).toHaveLength(rawLeaves.length - 47)
    expect(mergedHead.meta.leaves!).toContain('Melinaea mothone messenina')
    expect(mergedHead.meta.leaves!).toContain('Greta diaphanus quisqueya')
    for (const oldLeaf of Object.keys(leafMergesDoc.merges)) {
      expect(mergedHead.meta.leaves!).not.toContain(oldLeaf)
    }
    // Emptied species never appear in display species list.
    for (const gone of ['Greta a', 'Hypothyris noinonia', 'Ithomia jacunda', 'Godyris gonussa', 'Mechanitis isthmia', 'Napeogenes cyrianassa']) {
      expect(mergedHead.meta.species).not.toContain(gone)
    }

    const feat = new Float32Array([0.8, 0.6])
    const rawP = leafScores(rawHead, feat, null)
    const mergedP = leafScores(mergedHead, feat, null)

    const rawSum = rawP.reduce((a, b) => a + b, 0)
    const mergedSum = mergedP.reduce((a, b) => a + b, 0)
    expect(Math.abs(rawSum - 1)).toBeLessThan(1e-12)
    expect(Math.abs(mergedSum - 1)).toBeLessThan(1e-12)

    // Check every display leaf equals the exact sum of its contributing raw leaves.
    const rawByName = new Map(rawLeaves.map((n, i) => [n, rawP[i]]))
    const mergedByName = new Map(mergedHead.meta.leaves!.map((n, i) => [n, mergedP[i]]))
    const expectedByTarget = new Map<string, number>()
    const merges = leafMergesDoc.merges as Record<string, string>
    for (const lf of rawLeaves) {
      const tgt = merges[lf] ?? lf
      expectedByTarget.set(tgt, (expectedByTarget.get(tgt) ?? 0) + rawByName.get(lf)!)
    }
    for (const [tgt, expVal] of expectedByTarget) {
      expect(Math.abs(mergedByName.get(tgt)! - expVal)).toBeLessThan(1e-14)
    }

    // Greta diaphanus quisqueya stays strictly unmerged.
    expect(mergedByName.get('Greta diaphanus quisqueya')).toBeCloseTo(rawByName.get('Greta diaphanus quisqueya')!, 14)

    // Melinaea mothone mothone sums all three source leaves.
    expect(mergedByName.get('Melinaea mothone mothone')).toBeCloseTo(
      rawByName.get('Melinaea mothone mothone')! + rawByName.get('Melinaea marsaeus mothone')! + rawByName.get('Melinaea menophilus mothone')!,
      14,
    )
    // Melinaea mothone messenina sums both source leaves and inherits Melinaea mothone's East geo bit (0x7).
    expect(mergedByName.get('Melinaea mothone messenina')).toBeCloseTo(
      rawByName.get('Melinaea marsaeus messenina')! + rawByName.get('Melinaea menophilus messenina')!,
      14,
    )
    const messIdx = mergedHead.meta.leaves!.indexOf('Melinaea mothone messenina')
    expect(mergedHead.geo![messIdx]).toBe(0x7)
    expect(mergedHead.ssp![messIdx]).toBe(1)

    // Geographic prior conservation on East and West, and target geo bits used (Ithomia hymettia hymettia keeps West 0xb, not lagusa's East 0x7).
    const hymIdx = mergedHead.meta.leaves!.indexOf('Ithomia hymettia hymettia')
    expect(mergedHead.geo![hymIdx]).toBe(0xb)
    for (const side of ['East', 'West'] as const) {
      const pSide = leafScores(mergedHead, feat, side)
      expect(Math.abs(pSide.reduce((a, b) => a + b, 0) - 1)).toBeLessThan(1e-12)
      const spScores = sumSpecies(mergedHead, pSide)
      expect(Math.abs(spScores.reduce((a, b) => a + b, 0) - 1)).toBeLessThan(1e-12)
      const ranked = rank(spScores, mergedHead.meta.species, 10)
      const genusSum = [...ranked.genus.values()].reduce((a, b) => a + b, 0)
      expect(Math.abs(genusSum - 1)).toBeLessThan(1e-12)
      const higher = higherRanks(spScores, mergedHead.meta.species, taxonomyDoc as unknown as Taxonomy)
      expect(higher.tribe.get('Ithomiini')!).toBeCloseTo(1, 10)
      expect(higher.subfamily.get('Danainae')!).toBeCloseTo(1, 10)
    }

    // Subspecies & species helpers never return old names.
    const topSsp = subspeciesTop(mergedHead, mergedP, 100)
    expect(topSsp.some(([n]) => n === 'Melinaea mothone messenina')).toBe(true)
    expect(topSsp.some(([n]) => n === 'Greta diaphanus quisqueya')).toBe(true)
    for (const [n] of topSsp) expect(merges[n]).toBeUndefined()

    const mothoneSpIdx = mergedHead.meta.species.indexOf('Melinaea mothone')
    const bySp = subspeciesBySpecies(mergedHead, mergedP, [mothoneSpIdx])
    expect(bySp.get(mothoneSpIdx)!.map(x => x.name).sort()).toEqual(['Melinaea mothone messenina', 'Melinaea mothone mothone'])

    // speciesTop, speciesSides, displayHeadMeta and buildDisplaySpace non-mutation.
    expect(speciesTop(mergedHead, feat, 'East', 5).length).toBe(5)
    expect(speciesSides(mergedHead).get('Melinaea mothone')).toEqual({ East: true, West: false })
    const dispMeta = displayHeadMeta(meta)
    expect(dispMeta.leaves).toEqual(mergedHead.meta.leaves)
    expect(meta.leaves).toHaveLength(rawLeaves.length) // original meta untouched
    expect(buildDisplaySpace(meta, null).old2new).toBeNull()
  })
})
