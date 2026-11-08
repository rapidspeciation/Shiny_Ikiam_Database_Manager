/**
 * The head's tribe and subfamily per species for the phone's tribe / subfamily answer (lib/identifyResult.ts
 * higherRanks, lab flag TRIBE_RUNG): tools/identifier/taxonomy_6fa6ab78.json, the file the server identifier reads,
 * shipped with the page as its own small script (loaded only with the flag on). Null when it is not this head's.
 */
import type { Taxonomy } from './identifyResult'

let memo: Promise<Taxonomy | null> | null = null
export function loadTaxonomy(headSha: string): Promise<Taxonomy | null> {
  memo ??= import('../../../tools/identifier/taxonomy_6fa6ab78.json').then(m => m.default as unknown as Taxonomy).catch(() => null)
  return memo.then(t => (t && t.head === headSha ? t : null))
}
