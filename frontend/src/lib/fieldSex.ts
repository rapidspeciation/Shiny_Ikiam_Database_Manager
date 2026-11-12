/**
 * The general sex line on Identificar (team only, «En línea» only, experimental): butterflies outside the 13 Ithomiini
 * genera of the in-hand line. The server computes it (tools/identifier/field_sex.py, from the BioCLIP feature it
 * already makes) and sends a `fieldSex` block with the answer; this decides whether and how the page shows it. Pure,
 * tested on its own.
 *
 * Shown whenever the block is for the species the page names, the in-hand Ithomiini line does not apply (sexApplies
 * 'no', or 'genus' for the other Ithomiini, whose «No aplica» note it replaces), the photo was read online and the
 * person has not picked «En este teléfono». It is a bar like the in-hand line's: macho on the left, hembra on the
 * right, the «no seguro» band in the middle (P(female) 0.15 to 0.85), a marker at the photo's P(female).
 * Two kinds of species (BUILD.md «All species», 11 Nov 2026):
 * - Verified, the six that passed the fresh test, unchanged:
 *   - 'call' (a Supported answer outside the band): the sex and its % (Franz's per-sex rule, capped by the fresh test).
 *   - 'band': «No seguro», and the side it leans to; no %.
 *   - 'cell_under_90': the bar and marker only (that species and sex was under 90 % on the fresh test); no answer.
 *   - 'species_unsure' and 'no_wings': the bar, the side or «No seguro», a note that the sex may be wrong; no %.
 * - Estimated, every other species of the model (verified false): the marker at the calibrated P(female); 'estimate'
 *   shows the sex and % outside the band, «No seguro (se inclina a hembra, 62 %)» inside it; 'species_unsure' and
 *   'no_wings' always «No seguro (se inclina a …)» (the server sends sure false), the note, no %. Always with
 *   «estimado, sin verificar para esta especie».
 * No question about live butterflies (Franz, 11 Nov 2026). The 13 Ithomiini genera keep the in-hand line
 * (SexEstimate.vue) exactly as before.
 *
 * Model and test: WingsClassificator reports/general_sex_20261111 (PASS_RULE.md, RESULTS.md, BUILD.md).
 */
import { sexApplies } from './sexModel'

/** The six species that passed the fresh test (RESULTS.md, 11 Nov 2026), as the app's head names them. */
export const FIELD_SEX_SPECIES = ['Danaus plexippus', 'Hypolimnas bolina', 'Hypolimnas misippus', 'Papilio polyxenes', 'Papilio troilus', 'Poanes zabulon'] as const

/** The «no seguro» band in P(female): confidence max(p, 1 - p) under 0.85 (field_sex.py's t). */
export const FIELD_SEX_BAND = { lo: 0.15, hi: 0.85 } as const

/** An estimated species' % is never above this (field_sex.py ESTIMATE_CAP_MAX). */
export const FIELD_SEX_ESTIMATE_MAX = 95

export type FieldSexStatus = 'call' | 'band' | 'cell_under_90' | 'species_unsure' | 'no_wings' | 'estimate'
/** Where an estimated species' calibration comes from (calibration.json): its own photos, its genus, family or all. */
export type FieldSexLevel = 'species' | 'genus' | 'family' | 'global'

/** The server's block (server/identify-photos.mjs fieldSexBlock). */
export interface FieldSexAnswer {
  species: string
  status: FieldSexStatus
  /** P(female), 0 to 1: where the marker sits (calibrated and capped for an estimated species). */
  p: number
  /** The side it leans to (female when p >= 0.5). */
  sex: 'female' | 'male'
  /** Outside the «no seguro» band. */
  sure: boolean
  /** 'call': 90 to 99; 'estimate': 50 to 95. */
  pct?: number
  version?: string | null
  /** false for an estimated species; absent for the six verified ones. */
  verified?: boolean
  level?: FieldSexLevel
  trainPhotos?: number | null
}

export interface FieldSexState {
  species: string
  status: FieldSexStatus
  p: number
  sex: 'female' | 'male'
  sure: boolean
  /** Only for 'call' and 'estimate'. */
  pct: number | null
  /** The note under the bar for an answer the species or the crop makes doubtful. */
  note: 'species' | 'wings' | null
  /** One of the six verified on fresh photos. */
  verified: boolean
  /** Estimated species only. */
  level: FieldSexLevel | null
  trainPhotos: number | null
}

export interface FieldSexInput {
  /** The mode the person chose: picking «En este teléfono» hides the line, even for a result read online (a signal
   * drop, which only changes the effective mode, does not). */
  mode: 'online' | 'phone'
  source: 'online' | 'phone'
  /** The species the page names (its top species), whether or not the headline says it is sure. */
  topName: string | undefined
  answer: FieldSexAnswer | null | undefined
  ithomiiniGenera: readonly string[]
}

const VERIFIED_STATUSES: readonly FieldSexStatus[] = ['call', 'band', 'cell_under_90', 'species_unsure', 'no_wings']
const ESTIMATE_STATUSES: readonly FieldSexStatus[] = ['estimate', 'species_unsure', 'no_wings']
const LEVELS: readonly FieldSexLevel[] = ['species', 'genus', 'family', 'global']

/** What the line shows, or null for nothing (species outside the model, and the 13 Ithomiini genera). */
export function fieldSexState({ mode, source, topName, answer, ithomiiniGenera }: FieldSexInput): FieldSexState | null {
  if (mode !== 'online' || source !== 'online' || !answer || !topName) return null
  if (sexApplies(topName, ithomiiniGenera) === 'yes' || answer.species !== topName) return null
  if (answer.sex !== 'female' && answer.sex !== 'male') return null
  const p = Number(answer.p)
  if (!Number.isFinite(p) || p < 0 || p > 1) return null
  const pct = Number(answer.pct)
  const note = answer.status === 'species_unsure' ? 'species' : answer.status === 'no_wings' ? 'wings' : null
  if (answer.verified === false) {
    if (!ESTIMATE_STATUSES.includes(answer.status) || !LEVELS.includes(answer.level as FieldSexLevel)) return null
    // the six are always shown as verified, whatever a block says
    if ((FIELD_SEX_SPECIES as readonly string[]).includes(topName)) return null
    const shown = answer.status === 'estimate' && Number.isInteger(pct) && pct >= 50 && pct <= FIELD_SEX_ESTIMATE_MAX
    const n = Number(answer.trainPhotos)
    return { species: topName, status: answer.status, p, sex: answer.sex, sure: answer.sure === true, pct: shown ? pct : null, note, verified: false,
      level: answer.level as FieldSexLevel, trainPhotos: Number.isInteger(n) && n > 0 ? n : null }
  }
  if (sexApplies(topName, ithomiiniGenera) !== 'no') return null
  if (!(FIELD_SEX_SPECIES as readonly string[]).includes(topName)) return null
  if (!VERIFIED_STATUSES.includes(answer.status)) return null
  const call = answer.status === 'call' && Number.isInteger(pct) && pct >= 90 && pct <= 99
  return {
    species: topName,
    status: answer.status === 'call' && !call ? 'cell_under_90' : answer.status,
    p,
    sex: answer.sex,
    sure: answer.sure === true,
    pct: call ? pct : null,
    note,
    verified: true,
    level: null,
    trainPhotos: null,
  }
}
