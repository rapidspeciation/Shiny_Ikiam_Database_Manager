/**
 * The head's tribe and subfamily per species (tools/identifier/taxonomy_6fa6ab78.json, the file the server identifier
 * reads), shipped with the page as its own small script, loaded once a photo has a result. Two uses
 * (lib/identifyResult.ts): the phone's tribe / subfamily totals (higherRanks: only for the head the file was made
 * for, `forHead`), and the tribe and subfamily named by a species or genus answer, read by name (any head, the
 * server's too: a species' tribe does not depend on the model). Null when the file cannot be loaded.
 */
import type { Taxonomy } from './identifyResult'

let memo: Promise<Taxonomy | null> | null = null
export function loadTaxonomy(): Promise<Taxonomy | null> {
  memo ??= import('../../../tools/identifier/taxonomy_6fa6ab78.json').then(m => m.default as unknown as Taxonomy).catch(() => null)
  return memo
}

/** The file, only when it was made for this head (or its compat live head SHA-256 for fusion heads). */
export const forHead = (tax: Taxonomy | null, headSha: string | undefined | null, compatSha?: string | null) =>
  tax && ((headSha && tax.head === headSha) || (compatSha && tax.head === compatSha)) ? tax : null
