/**
 * Sex estimate on Identificar (team only, experimental: in-hand photos). A DINOv2-S/14 fine-tuned on composites of
 * our museum wings (WingsClassificator `v2s_ft_full`, exported to fp16 ONNX, 43 MB), run in the browser after the
 * identifier's wing detector: the same box and the same 224 px grey-padded square (offlineId.ts cropBox and
 * letterboxCrop), fed as raw 0 to 255 RGB (the model normalises itself). Output: one logit per photo.
 *
 * The file is not public: the server sends it to a signed-in person only (server/offline-id.mjs MODELS, role
 * `sex`), never to Hugging Face (its training composites were built on iNaturalist template photos).
 */
import type { FileRole } from './offlineId'

/** The shipped file, pinned: the server's list must give these bytes (sha256 of the file and of each 32 MB part). */
export const SEX_FILE = {
  name: 'sex_v2s_ft_full_fp16.onnx',
  role: 'sex' as FileRole,
  bytes: 43409800,
  sha256: 'd6d5fb9d92af9a5b4d85a7fac3ead016e2764c5b1964a064fdaf0e98df7c412d',
  parts: ['2b96e1abb88fdf21efc39a41d6b503e88b8fd3d9b1bc9b4874c5941d4fbe89a1', '9f4a2f50b7787ab20153b3e781e684ece74697851400074db7cd67e91910cb24'],
}

/** The files the estimate needs on the phone: the identifier's wing detector and the sex model. */
export const SEX_ROLES: FileRole[] = ['detector', 'sex']

/**
 * The cutoff and the «no seguro» band, in logit space, fitted on the team's in-hand monitoring photos
 * (WingsClassificator reports/field_sex_plan_20261104/PLAN.md, Amendment 8, 8 Nov 2026; numbers in
 * reports/field_sex_explore_20261106/INAPP.md): t is the balanced-accuracy cutoff on the 103 team specimens
 * (2.9805, P 0.952); lo and hi are the 5th and 95th percentiles of 2,000 bootstrap refits of it (P 0.834 and 0.992),
 * rounded to 4 decimals. The model scores in-hand males high too, so the cutoff is far above P = 0.5.
 */
export const SEX_CUT = { t: 2.9805, lo: 1.6157, hi: 4.8722 }

/** The genera of the training composites: no estimate for any other genus. */
export const SEX_GENERA = ['Dircenna', 'Forbestra', 'Godyris', 'Hyposcada', 'Hypothyris', 'Ithomia', 'Mechanitis', 'Methona', 'Napeogenes', 'Oleria', 'Pteronymia', 'Scada', 'Tithorea']

export type SexCall = 'hembra' | 'macho' | 'no seguro'

/** The model's input: the 224 px RGB square as float32 CHW, raw 0 to 255 (no /255, no mean or std here). */
export function sexInput(rgb: Uint8Array, size = 224): Float32Array {
  const plane = size * size, x = new Float32Array(3 * plane)
  for (let i = 0; i < plane; i++) for (let c = 0; c < 3; c++) x[c * plane + i] = rgb[i * 3 + c]
  return x
}

export const sigmoid = (z: number) => 1 / (1 + Math.exp(-z))

/** The call for a score: female at or above HI, male below LO, otherwise not sure. */
export function sexCall(z: number, cut = SEX_CUT): SexCall {
  if (z >= cut.hi) return 'hembra'
  if (z < cut.lo) return 'macho'
  return 'no seguro'
}

/**
 * The % shown, for display only (the calls stay on SEX_CUT): Platt scaling of the logit, P(hembra) = sigmoid(a·z + b),
 * fitted on the same 103 team in-hand photos as the cutoff (WingsClassificator
 * reports/sex_display_calibration_20261108/CALIBRATION_DISPLAY.md, 8 Nov 2026). The raw sigmoid(z) is far too high
 * for males (a male call could read «P(hembra) 83 %»); calibrated, the cutoff shows 53.7 % and the band 39.7 to 71.8 %
 * female, so a male call always shows more than 60 % male.
 */
export const SEX_DISPLAY = { a: 0.415, b: -1.0887 }

/** The calibrated P(hembra) shown for a score. */
export const sexShownFemale = (z: number) => sigmoid(SEX_DISPLAY.a * z + SEX_DISPLAY.b)

/** The call and the calibrated % of the sex called; «no seguro»: the side it leans to (the likelier one). */
export function sexShown(z: number, cut = SEX_CUT): { call: SexCall; sign: '♀' | '♂'; p: number } {
  const call = sexCall(z, cut)
  const female = sexShownFemale(z)
  const leansFemale = call === 'hembra' || (call === 'no seguro' && female >= 0.5)
  return { call, sign: leansFemale ? '♀' : '♂', p: leansFemale ? female : 1 - female }
}

/**
 * Whether the estimate applies to the identifier's top species: 'yes' (a trained genus), 'genus' (an Ithomiini
 * outside them: «no aplica»), 'no' (not an Ithomiini, or unknown: nothing shown).
 */
export function sexApplies(species: string | undefined, ithomiiniGenera: readonly string[]): 'yes' | 'genus' | 'no' {
  const g = (species ?? '').split(' ')[0]
  if (!g) return 'no'
  if (SEX_GENERA.includes(g)) return 'yes'
  return ithomiiniGenera.includes(g) ? 'genus' : 'no'
}
