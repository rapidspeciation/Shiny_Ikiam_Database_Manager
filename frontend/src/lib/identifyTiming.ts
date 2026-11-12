/**
 * Identificar's timing line under a result (small grey text): how long the photo took.
 * - On this phone: «Tiempo: 1,4 s en este teléfono», from the engine's own times (offlineEngine lastMs: the photo, plus
 *   any model load this photo needed), «, sexo 0,4 s» when the sex model ran on it, and «(primera vez, cargando
 *   modelos)» when a load time is included.
 * - Online: «Tiempo: 2,1 s en el servidor» (the result's ms: sent to answered).
 * Seconds with one decimal in the page's language (a comma in Spanish). Pure, tested on its own.
 */

/** This photo's times on the phone (ms). Loads count only when this photo needed them. */
export interface PhoneTiming {
  /** Model loads (encoder, detector) this photo needed. */
  loadMs: number
  /** The sex model's run on this photo, when it ran. */
  sexMs?: number | null
  /** The sex model's load, when this photo needed it. */
  sexLoadMs?: number
}

/** The sum of the `*_load` times (the sex model's apart) that are new or changed since `before`: the loads one photo
 * needed. `only` limits it to one key (e.g. 'sex_load'). */
export function loadsSince(before: Record<string, number>, after: Record<string, number>, only?: string): number {
  let ms = 0
  for (const [k, v] of Object.entries(after)) {
    if (!k.endsWith('_load') || (only ? k !== only : k === 'sex_load')) continue
    if (before[k] !== v && Number.isFinite(v)) ms += v
  }
  return ms
}

type Translate = (es: string, vars?: Record<string, string | number>) => string

/** Seconds with one decimal, in the page's language: 1400 ms is «1,4» in Spanish, "1.4" in English. */
export function seconds(ms: number, lang: string): string {
  return new Intl.NumberFormat(lang === 'en' ? 'en' : 'es', { minimumFractionDigits: 1, maximumFractionDigits: 1 }).format(Math.max(0, ms) / 1000)
}

/** The line for a result, or null without a usable time. */
export function timingLine(r: { source: 'online' | 'phone'; ms?: number | null; timing?: PhoneTiming | null }, t: Translate, lang: string): string | null {
  const ms = Number(r.ms)
  if (!Number.isFinite(ms) || ms <= 0) return null
  if (r.source === 'online') return t('Tiempo: {s} s en el servidor', { s: seconds(ms, lang) })
  const load = Math.max(0, Number(r.timing?.loadMs) || 0)
  let line = t('Tiempo: {s} s en este teléfono', { s: seconds(ms + load, lang) })
  const sex = Number(r.timing?.sexMs)
  const sexLoad = Math.max(0, Number(r.timing?.sexLoadMs) || 0)
  if (Number.isFinite(sex) && sex > 0) line += ', ' + t('sexo {s} s', { s: seconds(sex + sexLoad, lang) })
  if (load > 0 || sexLoad > 0) line += ' (' + t('primera vez, cargando modelos') + ')'
  return line
}
