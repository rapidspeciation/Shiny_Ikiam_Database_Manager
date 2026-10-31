import { reactive } from 'vue'
import { api } from './api'
import { t } from './i18n'
import { blobBase64, renameFrame, type RenameState } from './renameHost'

/**
 * The «Fotos» tab's sex check (server/sex-check.mjs, docs/sex-check.md): before the batch goes to Drive, each
 * butterfly's d and v photos, as they will be once turned, are sent to the server one butterfly at a time, which
 * compares three sexes: the sex model's (only its Supported calls count as sure), the envelope's (the reader's
 * full text) and the database's. The answers stay for the session, kept by CAM and the two files.
 */
export type SexFlag = 'disagree' | 'check' | 'agree' | 'uncertain' | 'none'
export type Sex = 'male' | 'female'
export interface SexAnswer {
  cam: string
  flag: SexFlag
  reasons: string[]
  /** unreliable: why the model is not trusted on this species (server/sex-model-unreliable.json) */
  model: { sex: Sex | null; p?: number; supported?: boolean; status: string; unreliable?: string | true }
  envelope: { sex: Sex; conf: number; sure: boolean; both?: boolean; alone?: boolean; line: string | null } | null
  database: { sex: Sex | null; rows: { sheet: string; row: number; value: string }[]; differ?: boolean }
  suggest?: Sex
  species: string | null
  ms: number
}
export interface SexPair {
  cam: string
  d: string
  v: string
}
export interface SexResult extends SexPair {
  answer?: SexAnswer
  error?: string
}

const CAM = /^CAM\d{6}$/

/** The batch's butterflies with a d and a v photo (the first of each, skipped photos left out), in the order taken. */
export function sexPairs(state: RenameState | null): SexPair[] {
  const by = new Map<string, { d?: string; v?: string }>()
  for (const r of state?.rows ?? []) {
    const cam = r.id.trim().toUpperCase()
    if (r.skip || !CAM.test(cam) || (r.view !== 'd' && r.view !== 'v')) continue
    const pair = by.get(cam) ?? by.set(cam, {}).get(cam)!
    pair[r.view] ??= r.file
  }
  return [...by].filter(([, p]) => p.d && p.v).map(([cam, p]) => ({ cam, d: p.d!, v: p.v! }))
}
export const pairKey = (p: SexPair) => `${p.cam}|${p.d}|${p.v}`

export const sexCheck = reactive({
  /** Answers by pairKey. */
  results: {} as Record<string, SexResult>,
  running: false,
  done: 0,
  total: 0,
  stop: false,
})

/** The order the panel lists them in: the likely errors first. */
export const FLAG_ORDER: SexFlag[] = ['disagree', 'check', 'uncertain', 'agree', 'none']

/** Counts by flag of these pairs' answers (errors: a butterfly the check could not read). */
export function sexCounts(pairs: SexPair[]) {
  const out = { disagree: 0, check: 0, agree: 0, uncertain: 0, none: 0, errors: 0, checked: 0 }
  for (const p of pairs) {
    const r = sexCheck.results[pairKey(p)]
    if (!r) continue
    out.checked++
    if (r.answer) out[r.answer.flag]++
    else if (r.error) out.errors++
  }
  return out
}

const photoOf = async (file: string) => {
  const out = await renameFrame.ask!('view', { files: [file], part: 'photo', side: 1600 })
  const blob = (out.data as { images?: { blob: Blob }[] } | undefined)?.images?.[0]?.blob
  if (!out.ok || !blob) throw new Error(out.error || t('La app de fotos no dio la foto {file}', { file }))
  return blobBase64(blob)
}

/** Checks the pairs not checked yet (all of them with `again`), one at a time, until done or stopped. */
export async function runSexCheck(pairs: SexPair[], { again = false } = {}) {
  if (sexCheck.running || !renameFrame.ask) return
  const todo = pairs.filter(p => again || !sexCheck.results[pairKey(p)]?.answer)
  Object.assign(sexCheck, { running: true, stop: false, done: 0, total: todo.length })
  try {
    for (const pair of todo) {
      if (sexCheck.stop) break
      const key = pairKey(pair)
      try {
        const [d, v] = [await photoOf(pair.d), await photoOf(pair.v)]
        const answer = await api<SexAnswer>('sex-check', { method: 'POST', body: { cam: pair.cam, d, v } })
        sexCheck.results[key] = { ...pair, answer }
      } catch (e) {
        sexCheck.results[key] = { ...pair, error: e instanceof Error ? e.message : String(e) }
      }
      sexCheck.done++
    }
  } finally {
    sexCheck.running = false
  }
}

const SIGN: Record<Sex, string> = { male: '♂', female: '♀' }
export const sexSign = (sex: Sex | null | undefined) => (sex ? SIGN[sex] : '–')

/** The model's call in a few words: «♂ 99 % · Supported», «♀ 78 % · inseguro», «sin lectura». */
export function modelText(a: SexAnswer): string {
  if (!a.model.sex) return t('sin lectura')
  // never «100 %»: a model's call is not a certainty
  const p = (a.model.p ?? 0) >= 0.995 ? '>99' : String(Math.round((a.model.p ?? 0) * 100))
  if (a.model.unreliable) return t('{sign} {p} % · no fiable en esta especie', { sign: sexSign(a.model.sex), p })
  return a.model.supported ? t('{sign} {p} % · Supported', { sign: sexSign(a.model.sex), p }) : t('{sign} {p} % · inseguro', { sign: sexSign(a.model.sex), p })
}
export function envelopeText(a: SexAnswer): string {
  if (!a.envelope) return t('sin signo')
  return a.envelope.sure ? sexSign(a.envelope.sex) : t('{sign} (lectura dudosa)', { sign: sexSign(a.envelope.sex) })
}
export function databaseText(a: SexAnswer): string {
  if (a.database.differ) return a.database.rows.map(r => `${r.value || '–'} (${r.sheet})`).join(' / ')
  return a.database.sex ? sexSign(a.database.sex) : t('vacío')
}

/** What the flag means for this butterfly, in a sentence. */
export function flagText(a: SexAnswer): string {
  const r = new Set(a.reasons)
  const m = sexSign(a.model.sex)
  const db = sexSign(a.database.sex)
  switch (a.flag) {
    case 'disagree':
      return r.has('envelope_agrees_with_model')
        ? t('El modelo (Supported) y el sobre dicen {m}, la base dice {db}: probablemente la base está mal.', { m, db })
        : r.has('envelope_agrees_with_database')
          ? t('El modelo (Supported) dice {m}, la base y el sobre dicen {db}: mira las alas.', { m, db })
          : t('El modelo (Supported) dice {m}, la base dice {db}: mira las alas o el sobre.', { m, db })
    case 'check':
      if (r.has('database_rows_differ')) return t('Las filas de la base no tienen el mismo sexo.')
      if (r.has('envelope_vs_database')) return t('El sobre dice {e}, la base dice {db}; el modelo no está seguro. El lector a veces inventa ♂/♀: mira el sobre.', { e: sexSign(a.envelope?.sex), db })
      if (r.has('model_vs_envelope')) return t('La base no tiene sexo; el modelo (Supported) dice {m} y el sobre {e}.', { m, e: sexSign(a.envelope?.sex) })
      return r.has('only_envelope')
        ? t('La base no tiene sexo; el sobre dice {e} (el modelo no está seguro).', { e: sexSign(a.suggest) })
        : t('La base no tiene sexo; el modelo (Supported) dice {m}.', { m: sexSign(a.suggest) })
    case 'agree':
      return r.has('envelope_misread_likely') ? t('El modelo (Supported) y la base coinciden; el lector probablemente leyó mal el sobre.') : t('El modelo (Supported) y la base coinciden.')
    case 'uncertain':
      if (r.has('model_unreliable_species')) return t('El modelo no es fiable para esta especie: no se marca como error.')
      return a.model.sex ? t('El modelo no está seguro (no es una llamada Supported): no se marca como error.') : t('El modelo no pudo leer las alas de estas fotos.')
    default:
      return t('Nada que comparar.')
  }
}
