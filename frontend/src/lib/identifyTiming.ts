/**
 * Identificar's timing line under a result (small grey text): how long the photo took.
 * - On this phone: «Tiempo: 1,4 s en este teléfono»: the engine's photo time (offlineEngine lastMs.photo) or, when the
 *   photo had to load something (the runtime, the detector, the encoder, the head: Engine.loadGeneration moved), the
 *   whole phase measured once by `timed`; «, sexo 0,4 s» when the sex model ran on it (its load included the same way),
 *   and «(primera vez, cargando modelos)» when a load is included.
 * - Online: «Tiempo: 2,1 s en el servidor» (the result's ms: sent to answered).
 * Seconds with one decimal in the page's language (a comma in Spanish). Pure, tested on its own.
 */

/** This photo's times on the phone (ms). Loads count only when this photo needed them. */
export interface PhoneTiming {
  /** What loading added to the photo's own time (the phase's wall time minus the photo's), when it loaded. */
  loadMs: number
  /** The photo had to load something (whatever the durations: two loads can take the same ms). */
  fresh?: boolean
  /** The sex model's run on this photo, when it ran. */
  sexMs?: number | null
  /** What loading the sex model (and the runtime, if needed) added, when this photo needed it. */
  sexLoadMs?: number
  sexFresh?: boolean
}

/** Runs `fn` and measures it once, start to end: its wall time, and whether anything was read into memory meanwhile
 * (`generation`: Engine.loadGeneration). */
export async function timed<T>(generation: () => number, fn: () => Promise<T>): Promise<{ value: T; ms: number; fresh: boolean }> {
  const g0 = generation(), t0 = performance.now()
  const value = await fn()
  return { value, ms: Math.max(0, Math.round(performance.now() - t0)), fresh: generation() !== g0 }
}

/** A photo's phone timing from its timed phase (load if needed, then the run) and the engine's photo time. */
export function phoneTiming(phase: { ms: number; fresh: boolean }, photoMs: number): PhoneTiming {
  return phase.fresh ? { loadMs: Math.max(0, phase.ms - photoMs), fresh: true } : { loadMs: 0, fresh: false }
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
  if (load > 0 || sexLoad > 0 || r.timing?.fresh || r.timing?.sexFresh) line += ' (' + t('primera vez, cargando modelos') + ')'
  return line
}
