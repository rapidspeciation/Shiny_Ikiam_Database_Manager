/**
 * The general sex line on Identificar (team only, «En línea» only, experimental): live butterflies of six species
 * outside the Ithomiini. The server computes it (tools/identifier/field_sex.py, from the BioCLIP feature it already
 * makes) and sends a `fieldSex` block with the answer; this decides whether the page shows it. Pure, tested on its own.
 *
 * Shown only when all hold: the page is in «En línea» mode and the photo was read online; the species named on the page is one of the six and is the
 * species the server's block is for (the server gates on the species score without the geographic prior, >= 0.8);
 * the in-hand Ithomiini line does not apply (sexApplies 'no'); the server's call is Supported, not suppressed, and the
 * wings were found (status 'call'); and the person said the photo is of a live butterfly (one tap per photo).
 * The 13 Ithomiini genera and the other Ithomiini keep the in-hand line (SexEstimate.vue) exactly as before.
 *
 * Model and test: WingsClassificator reports/general_sex_20261111 (PASS_RULE.md, RESULTS.md, BUILD.md).
 */
import { sexApplies } from './sexModel'
import type { Headline } from './identifyResult'

/** The six species that passed the fresh test (RESULTS.md, 11 Nov 2026), as the app's head names them. */
export const FIELD_SEX_SPECIES = ['Danaus plexippus', 'Hypolimnas bolina', 'Hypolimnas misippus', 'Papilio polyxenes', 'Papilio troilus', 'Poanes zabulon'] as const

/** The server's block (server/identify-photos.mjs fieldSexBlock). */
export interface FieldSexAnswer {
  species: string
  status: 'call' | 'below_gate' | 'not_supported' | 'no_wings' | 'suppressed'
  version?: string | null
  sex?: 'female' | 'male'
  pct?: number
}

export type LiveAnswer = 'yes' | 'no'

export type FieldSexState =
  | { kind: 'ask'; species: string }
  | { kind: 'call'; species: string; sex: 'female' | 'male'; pct: number }
  | { kind: 'declined'; species: string }

export interface FieldSexInput {
  /** The page's mode now: switching to «En este teléfono» hides the line, even for a result read online. */
  mode: 'online' | 'phone'
  source: 'online' | 'phone'
  headline: Headline | null
  answer: FieldSexAnswer | null | undefined
  live: LiveAnswer | undefined
  ithomiiniGenera: readonly string[]
}

/** What the line shows, or null for nothing (the usual case). */
export function fieldSexState({ mode, source, headline, answer, live, ithomiiniGenera }: FieldSexInput): FieldSexState | null {
  if (mode !== 'online' || source !== 'online' || !answer || headline?.kind !== 'species') return null
  const name = headline.name
  if (sexApplies(name, ithomiiniGenera) !== 'no') return null
  if (!(FIELD_SEX_SPECIES as readonly string[]).includes(name) || answer.species !== name) return null
  if (answer.status !== 'call' || (answer.sex !== 'female' && answer.sex !== 'male')) return null
  const pct = Number(answer.pct)
  if (!Number.isInteger(pct) || pct < 0 || pct > 99) return null
  if (live === 'yes') return { kind: 'call', species: name, sex: answer.sex, pct }
  if (live === 'no') return { kind: 'declined', species: name }
  return { kind: 'ask', species: name }
}
