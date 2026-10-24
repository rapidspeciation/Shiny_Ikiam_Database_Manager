/**
 * The photo archive review (server/photo-curation.mjs): the shape of a card's cases and the pure rules
 * of how a case reads on it — the change in one line, and the evidence as chips that say whether each
 * source supports the change (ok), argues against it (against) or only informs (info). Kept apart from
 * the component so they are tested without a browser.
 */
import type { Box } from './review'
import { t } from './i18n'

export type CurationKind = 'species' | 'sex' | 'gallery' | 'aisex' | 'camid' | 'side' | 'turn'
export type CurationTier = 'clear' | 'reason' | 'look'
export type CurationDecision = 'accepted' | 'kept' | 'deferred' | 'queued' | 'planned'
export const TIERS: CurationTier[] = ['clear', 'reason', 'look']
export const KINDS: CurationKind[] = ['camid', 'turn', 'side', 'species', 'gallery', 'sex', 'aisex']

export interface CurationEntry {
  id: string
  kind: CurationKind
  cam: string
  tier: CurationTier
  folder?: string | null
  year?: number | null
  /** The database row a fix writes. */
  sheet?: string
  row?: number
  why?: string[]
  conf?: number
  evidence: Record<string, unknown>
  fix?: { field?: string; from?: string; to?: string; drive?: { action: string; from?: string; name?: string; degrees?: number }[] } | null
  choices?: string[]
  photos?: {
    files?: { id: string; name: string; wings?: Box }[]
    envelope?: { fileId: string; name: string; bbox: Box; turned?: number; aspect?: number }
  }
  idLines?: { fileId: string; name: string; read: string; conf?: number; box?: Box; aspect?: number; turn?: number }[]
  note?: string
  /** The envelope and database rows of the butterflies the case involves. */
  records?: RecordFacts[]
  checked?: boolean
  decision?: CurationDecision | null
  /** Why its Drive change was not made (it is to decide again). */
  problem?: string
  chosen?: string
}

export interface PhotoCuration {
  type: 'curation'
  title: string
  entries: CurationEntry[]
  offset?: number
  totalInSection?: number
  leftInSection?: number
  filters?: { tier?: string; kind?: string; folder?: string; year?: string }
  result?: {
    written?: number
    kept?: number
    deferred?: number
    planned?: number
    queued?: number
    skipped?: string[]
    failed?: string[]
    drive?: { id?: string; plan?: string; link?: string; error?: string }[]
    driveProgress?: { plan: string; state: string; written?: number; total?: number; failed?: number; errors?: string[] }
  }
}

export interface CurationSummary {
  total: number
  sandboxDrive?: boolean
  tiers: Record<string, number>
  kinds: Record<string, { n: number; clear: number; reason: number; look: number }>
  folders: { name: string; n: number }[]
  years: { name: string | number; n: number }[]
  settledBefore?: number
}

export const kindLabel = (kind: CurationKind | string) =>
  ({
    camid: t('ID CAM'),
    turn: t('Rotación'),
    side: t('Dorsal / ventral'),
    species: t('Especie del sobre'),
    gallery: t('Especie del modelo'),
    sex: t('Sexo del sobre'),
    aisex: t('Sexo del modelo'),
  })[kind] ?? kind

/** The three groups of a section, as their headers name them. */
export const tierLabel = (tier: CurationTier) => ({ clear: t('Seguros'), reason: t('A pensar'), look: t('Mirar la foto') })[tier]
export const tierHint = (tier: CurationTier) =>
  ({
    clear: t('fuentes independientes coinciden contra una; vienen marcados'),
    reason: t('las lecturas no coinciden entre sí; decide tú o pide a Claude'),
    look: t('solo la foto lo decide'),
  })[tier]

const pct = (v: unknown) => (typeof v === 'number' ? `${Math.round(v * 100)} %` : '')
const sexWord = (v: unknown) => {
  const s = String(v ?? '').toLowerCase()
  return s === 'male' ? t('macho') : s === 'female' ? t('hembra') : String(v ?? '')
}
const sideWord = (v: unknown) => (v === 'dorsal' ? t('dorsal') : v === 'ventral' ? t('ventral') : v === 'mixed' ? t('mixta') : String(v ?? '?'))
const binomial = (v: unknown) =>
  String(v ?? '')
    .toLowerCase()
    .split(/\s+/)
    .slice(0, 2)
    .join(' ')
const short = (name: unknown) => {
  const [g, ...rest] = String(name ?? '').split(/\s+/)
  return rest.length ? `${g[0]}. ${rest.join(' ')}` : g
}
/** A photo's view, from the end of its name (CAM070046d2 → d2). */
const viewOf = (name: string) => /([dv]\d*)$/i.exec(name)?.[1]?.toLowerCase() ?? name

/** The change a case proposes, as the card's bold line shows it: { from, to } (to null: nothing proposed). */
export function curationChange(e: CurationEntry, chosen?: string): { field: string; from: string; to: string | null } {
  if (e.kind === 'turn') {
    const deg = e.fix?.drive?.[0]?.degrees
    return { field: String(e.evidence.photo ?? e.cam), from: '', to: deg ? t('girar {n}°', { n: deg }) : null }
  }
  if (e.kind === 'side') return { field: '', from: '', to: e.fix?.drive?.length ? t('intercambiar d y v') : null }
  if (e.kind === 'camid') return { field: '', from: e.cam, to: e.fix?.to ?? null }
  const field = e.fix?.field === 'Sex' || e.kind === 'sex' || e.kind === 'aisex' ? t('Sexo') : t('Especie')
  const show = (v: unknown) => (field === t('Sexo') ? sexWord(v) : String(v ?? ''))
  return { field, from: show(e.fix?.from ?? e.evidence.database ?? '—'), to: chosen || e.fix?.to ? show(chosen || e.fix?.to) : null }
}

export type Chip = { text: string; tone: 'ok' | 'against' | 'info' }

/** The evidence of a case as chips, the sources that disagree first. */
export function curationChips(e: CurationEntry): Chip[] {
  const ev = e.evidence as Record<string, unknown[] | string | undefined>
  const out: Chip[] = []
  const to = e.fix?.to
  const arr = (v: unknown) => (Array.isArray(v) ? v : [])
  if (e.kind === 'species' || e.kind === 'gallery') {
    const [envName, envConf, seen] = arr(ev.envelope)
    const [modelName, modelConf] = arr(ev.model)
    const agrees = (name: unknown) => (to ? binomial(name) === binomial(to) : false)
    if (envName) out.push({ text: `${t('Sobre')}: ${short(envName)}${envConf != null ? ` · ${pct(envConf)}` : ''}${Number(seen) > 1 ? ` · ${t('en {n} fotos', { n: seen as number })}` : ''}`, tone: agrees(envName) ? 'ok' : binomial(envName) === binomial(ev.database) ? 'against' : 'info' })
    if (modelName) out.push({ text: `${t('Modelo')}: ${short(modelName)} · ${pct(modelConf)}`, tone: agrees(modelName) ? 'ok' : binomial(modelName) === binomial(ev.database) ? 'against' : 'info' })
  } else if (e.kind === 'sex' || e.kind === 'aisex') {
    const [envSex, envConf, seen] = arr(ev.envelope)
    const [modelSex, modelConf] = arr(ev.model)
    if (envSex) out.push({ text: `${t('Sobre')}: ${sexWord(envSex)}${envConf != null ? ` · ${pct(envConf)}` : ''}${Number(seen) > 1 ? ` · ${t('en {n} fotos', { n: seen as number })}` : ''}`, tone: envSex === to ? 'ok' : 'against' })
    if (modelSex) out.push({ text: `${t('Modelo de sexo')}: ${sexWord(modelSex)} · ${pct(modelConf)}`, tone: modelSex === to ? 'ok' : 'against' })
  } else if (e.kind === 'camid') {
    const [target, conf, seen, of] = arr(ev.envelope)
    out.push({ text: `${t('Sobre')}: ${target} · ${pct(conf)}${of ? ` · ${t('{seen} de {of} fotos', { seen: seen as number, of: of as number })}` : ''}`, tone: Number(seen) && seen === of ? 'ok' : 'info' })
    const [modelName, modelConf] = arr(ev.model)
    const why = e.why ?? []
    if (modelName && ev.target) {
      const fits = binomial(modelName) === binomial(ev.target)
      out.push({
        text: `${t('Modelo')}: ${short(modelName)} · ${pct(modelConf)} · ${fits ? t('= especie de {cam}', { cam: String(target) }) : t('≠ {species} de {cam}', { species: short(ev.target), cam: String(target) })}`,
        // A model under 60 % only informs.
        tone: fits ? 'ok' : Number(modelConf) >= 0.6 ? 'against' : 'info',
      })
    }
    if (why.includes('ownIdAlsoRead')) out.push({ text: t('El sobre también lee {cam}', { cam: e.cam }), tone: 'against' })
    if (why.includes('targetHasPhotos')) out.push({ text: t('{cam} ya tiene fotos', { cam: String(target) }), tone: 'against' })
    if (why.includes('targetNoRow')) out.push({ text: t('{cam} no está en la base de datos', { cam: String(target) }), tone: 'against' })
    else if (!why.includes('targetHasPhotos')) out.push({ text: t('{cam} sin fotos', { cam: String(target) }), tone: 'info' })
  } else if (e.kind === 'side') {
    for (const p of arr(ev.photos)) {
      const [name, side, pDorsal] = arr(p)
      const view = viewOf(String(name))
      const sure = typeof pDorsal === 'number' ? (side === 'ventral' ? 1 - pDorsal : pDorsal) : null
      const wrong = (view.startsWith('d') && side === 'ventral') || (view.startsWith('v') && side === 'dorsal')
      out.push({ text: `${view}: ${t('alas')} ${sideWord(side)}${sure != null ? ` · ${pct(sure)}` : ''}`, tone: wrong ? 'ok' : 'info' })
    }
    if ((e.why ?? []).includes('sameSide')) out.push({ text: t('Las dos fotos muestran el mismo lado'), tone: 'against' })
  } else if (e.kind === 'turn') {
    out.push({ text: `${t('Alas')}: ${String(ev.turn ?? '')}${e.conf != null ? ` · ${pct(e.conf)}` : ''}`, tone: 'ok' })
  }
  if ((e.why ?? []).includes('formula')) out.push({ text: t('La celda es una fórmula'), tone: 'against' })
  if ((e.why ?? []).includes('notListed')) out.push({ text: t('No está en la lista de especies'), tone: 'against' })
  return out
}

/** The section a card shows, in words: its kind, folder and year. */
export function sectionLabel(card: PhotoCuration) {
  const f = card.filters ?? {}
  const kinds = String(f.kind ?? '')
    .split(',')
    .filter(Boolean)
  return [kinds.length ? kinds.map(kindLabel).join(', ') : t('Todos los problemas'), f.tier ? tierLabel(f.tier as CurationTier) : '', f.folder || '', f.year ? String(f.year) : '']
    .filter(Boolean)
}

/** A case still to decide on this card. */
export const undecided = (e: CurationEntry) => !e.decision || e.decision === 'deferred'

/** What the person ticked and marked on each card, kept while the page is open (the card is drawn again as it changes). */
export const cardChoices = new Map<string, { ticked: Set<string>; wrong: Set<string>; values: Record<string, string> }>()

// ------------------------------------------------------------ a butterfly's envelope beside its database rows
export interface RecordFacts {
  cam: string
  envelope?: {
    species?: [string, number]
    sex?: [string, number]
    locality?: [string, number, string?]
    dates?: [string, string, number][]
    camid?: [string, number]
    code?: [string, number]
  }
  database?: {
    sheet: string
    row: number
    species: string | null
    sex: string | null
    locality: string | null
    dates?: Record<string, string>
    insectaryId?: string
    note?: string
    notes?: { field: 'species' | 'sex' | 'camid'; from?: string | null; to?: string | null; checked?: boolean; text: string }[]
  }[]
}

const fold = (v: unknown) =>
  String(v ?? '')
    .normalize('NFD')
    .replace(/[̀-ͯ]/g, '')
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, ' ')
    .trim()
// Words a place name can leave out without naming another place.
const PLACE_FILLERS = new Set(['de', 'del', 'la', 'las', 'los', 'el', 'y', 'rio', 'r'])
/**
 * The envelope's place is the database's when one name holds the other's words: envelopes write the
 * short name ("Arajuno", "BoanaExplora") of the database's longer one ("San Pedro de Arajuno 2",
 * "BoanaExplora, Pachijal"), a word cut short ("Sta." for "Santa"), or run together ("Tamiayura").
 */
export function samePlace(a: unknown, b: unknown) {
  const [x, y] = [fold(a), fold(b)]
  if (!x || !y) return false
  if (x.replace(/ /g, '') === y.replace(/ /g, '')) return true
  const [short, long] = x.length <= y.length ? [x.split(' '), y.split(' ')] : [y.split(' '), x.split(' ')]
  const named = short.filter(w => !PLACE_FILLERS.has(w))
  return named.length > 0 && named.every(w => long.some(t => t === w || (w.length >= 3 && t.startsWith(w))))
}
/** dd/mm/yyyy, as the team writes dates. */
const dayFirst = (iso: unknown) => (/^\d{4}-\d{2}-\d{2}$/.test(String(iso)) ? String(iso).split('-').reverse().join('/') : String(iso ?? ''))
/** Which database date an envelope's date is (a plain date on a field envelope: the collection day). */
const DATE_OF: Record<string, string[]> = { date: ['collected', 'intoInsectary'], death: ['death'], preserved: ['preserved'], emerged: ['intoInsectary'], wingClip: ['intoInsectary'] }
/**
 * A reared butterfly's envelope (it carries an insectary code, or its row is in Insectary_data): its plain date
 * is the day it died, and a wing clip is dated the day it came into the insectary.
 */
const REARED_DATE_OF: Record<string, string[]> = { ...DATE_OF, date: ['death', 'preserved', 'collected'] }
export const dateKindLabel = (k: string) =>
  ({ date: t('Fecha'), collected: t('Colecta'), intoInsectary: t('Ingreso al insectario'), death: t('Muerte'), preserved: t('Preservación'), emerged: t('Emergencia'), wingClip: t('Corte de ala') })[k] ?? k

export type RecordLine = { label: string; envelope: string; database: string; differ: boolean }
/** The envelope's fields beside the database's, one line each; `differ` when both have a value and they disagree. */
export function recordLines(r: RecordFacts, sheet?: string): RecordLine[] {
  const env = r.envelope ?? {}
  const row = r.database?.find(d => d.sheet === sheet) ?? r.database?.[0]
  const read = (v?: [string, number, ...unknown[]]) => (v ? `${v[0]} · ${pct(v[1])}` : '')
  const lines: RecordLine[] = []
  const add = (label: string, envelope: string, database: string, same: boolean) =>
    (envelope || database) && lines.push({ label, envelope, database, differ: !!envelope && !!database && !same })
  add('CAM', read(env.camid), r.cam, !env.camid || fold(env.camid[0]) === fold(r.cam))
  add(t('Especie'), read(env.species), row?.species ?? '', !env.species || binomial(env.species[0]) === binomial(row?.species))
  add(t('Sexo'), env.sex ? `${sexWord(env.sex[0])} · ${pct(env.sex[1])}` : '', sexWord(row?.sex ?? ''), !env.sex || String(row?.sex ?? '').toLowerCase().startsWith(env.sex[0]))
  add(t('Localidad'), env.locality ? `${env.locality[2] ? `${env.locality[2]} → ` : ''}${read(env.locality)}` : '', row?.locality ?? '', !env.locality || samePlace(env.locality[0], row?.locality))
  const dates = row?.dates ?? {}
  const used = new Set<string>()
  const reared = !!env.code || !!r.database?.some(d => d.sheet === 'Insectary_data')
  for (const [kind, value, conf] of env.dates ?? []) {
    const match = ((reared ? REARED_DATE_OF : DATE_OF)[kind] ?? []).find(k => dates[k] && !used.has(k))
    if (match) used.add(match)
    const label = reared && kind === 'date' && match ? dateKindLabel(match) : dateKindLabel(kind)
    add(label, `${dayFirst(value)} · ${pct(conf)}`, match ? dayFirst(dates[match]) : '', !match || dates[match] === value)
  }
  for (const [kind, value] of Object.entries(dates)) if (!used.has(kind)) add(dateKindLabel(kind), '', dayFirst(value), true)
  return lines
}
const FIELD_OF: Partial<Record<CurationKind, 'species' | 'sex' | 'camid'>> = { species: 'species', gallery: 'species', sex: 'sex', aisex: 'sex', camid: 'camid' }
/**
 * The notes that changed the field of a case after the photo, beside what its envelope reads: `outdated` when
 * the envelope still shows the value the note changed from (the envelope is out of date), `supports` when it
 * shows the value the note changed to.
 */
export function notesOnCase(e: CurationEntry) {
  const field = FIELD_OF[e.kind]
  const out: { note: NonNullable<NonNullable<RecordFacts['database']>[number]['notes']>[number]; verdict: 'outdated' | 'supports' | 'changed' }[] = []
  if (!field) return out
  for (const r of e.records ?? []) {
    const read = field === 'species' ? r.envelope?.species?.[0] : field === 'sex' ? r.envelope?.sex?.[0] : r.envelope?.camid?.[0]
    const same = (a: unknown, b: unknown) => !!a && !!b && (field === 'species' ? binomial(a) === binomial(b) : fold(a) === fold(b))
    for (const d of r.database ?? [])
      for (const n of d.notes ?? [])
        if (n.field === field && !n.checked) out.push({ note: n, verdict: same(read, n.from) ? 'outdated' : same(read, n.to) ? 'supports' : 'changed' })
  }
  return out
}
