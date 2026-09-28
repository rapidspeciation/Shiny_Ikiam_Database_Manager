// Explicit .ts extensions: the server loads this file too (Node strips the types), so the
// assistant builds monitoring rows exactly as the Monitoreo review does (server/walks.mjs).
import { formatSerial, isoToSerial, serialToIso } from './dates.ts'
import { MAX_SECTION_DISTANCE, nearestSection } from './transects.ts'
import type { CellValue, TableRow } from './types.ts'

/**
 * Ithomiini monitoring at Ikiam: reading Wikiloc GPX files, turning waypoint
 * notes such as "M1 Hyposcada illinissa ida hembra 9:20 0.5m NO id: B69" into
 * Collection_data values, and the summaries shown in the monthly reports.
 */

export const ZONE = 'America/Guayaquil'
/** Once a species has this many preserved individuals it is marked and released instead. */
export const MARK_THRESHOLD = 30

export const CLOUD = {
  CD: 'CD_(cloudy_dark)',
  CL: 'CL_(cloudy_light)',
  SC: 'S&C_(sun_&_cloud_patches)',
  S: 'S_(cloudless_sunny)',
} as const
export const RAIN = { DY: 'DY_(dry)', DZ: 'DZ_(drizzle)' } as const

// ---------------------------------------------------------------- GPX files

/** [latitude, longitude, elevation, time (ISO)] */
export type TrackPoint = [number, number, number | null, string | null]

export interface GpxWaypoint {
  lat: number
  lon: number
  ele: number | null
  time: string | null
  text: string
  /** IDs of photos stored in the app (walks read from Wikiloc pages). */
  photos?: string[]
}

export interface Gpx {
  name: string
  track: TrackPoint[]
  waypoints: GpxWaypoint[]
}

const ENTITIES: Record<string, string> = { amp: '&', lt: '<', gt: '>', quot: '"', apos: "'" }

function decode(text: string) {
  const cdata = /^\s*<!\[CDATA\[([\s\S]*?)\]\]>\s*$/.exec(text)
  if (cdata) return cdata[1].trim()
  return text
    .replace(/&(#x[0-9a-f]+|#\d+|\w+);/gi, (m, e: string) =>
      e[0] === '#'
        ? String.fromCodePoint(e[1] === 'x' || e[1] === 'X' ? parseInt(e.slice(2), 16) : Number(e.slice(1)))
        : (ENTITIES[e] ?? m),
    )
    .trim()
}
const tag = (body: string, name: string) => {
  const m = new RegExp(`<(?:\\w+:)?${name}(?:\\s[^>]*)?>([\\s\\S]*?)</(?:\\w+:)?${name}>`).exec(body)
  return m ? decode(m[1]) : ''
}
const attr = (attrs: string, name: string) => new RegExp(`\\b${name}\\s*=\\s*["']([^"']*)["']`).exec(attrs)?.[1] ?? null
const num = (text: string | null) => (text === null || text === '' || !Number.isFinite(Number(text)) ? null : Number(text))

function points(xml: string, name: string) {
  const out: { lat: number; lon: number; ele: number | null; time: string | null; body: string }[] = []
  const re = new RegExp(`<(?:\\w+:)?${name}\\b([^>]*?)(?:/>|>([\\s\\S]*?)</(?:\\w+:)?${name}>)`, 'g')
  for (const m of xml.matchAll(re)) {
    const lat = num(attr(m[1], 'lat'))
    const lon = num(attr(m[1], 'lon'))
    if (lat === null || lon === null || Math.abs(lat) > 90 || Math.abs(lon) > 180) continue
    const body = m[2] || ''
    out.push({ lat, lon, ele: num(tag(body, 'ele')), time: tag(body, 'time') || null, body })
  }
  return out
}

/**
 * Reads the track and waypoints of a GPX file (as exported by Wikiloc).
 * A small reader of its own: GPX is simple, and this behaves the same in
 * every browser and in tests.
 */
export function parseGpx(xml: string): Gpx {
  if (!/<(?:\w+:)?gpx[\s>]/.test(xml)) throw new Error('El archivo no es un GPX')
  const track: TrackPoint[] = points(xml, 'trkpt').map(p => [p.lat, p.lon, p.ele, p.time])
  const waypoints: GpxWaypoint[] = []
  for (const p of points(xml, 'wpt')) {
    // Wikiloc repeats the name in <cmt>; a longer description, if any, is in <desc>.
    const parts = [tag(p.body, 'name'), tag(p.body, 'desc')].filter(Boolean)
    const text = [...new Set(parts)].join(' ').replace(/\s+/g, ' ').trim()
    if (text) waypoints.push({ lat: p.lat, lon: p.lon, ele: p.ele, time: p.time, text })
  }
  const trk = /<(?:\w+:)?trk\b[^>]*>([\s\S]*?)<\/(?:\w+:)?trk>/.exec(xml)?.[1] || ''
  const name = tag(trk.split(/<(?:\w+:)?trkseg/)[0], 'name') || tag(xml.split(/<(?:\w+:)?(?:wpt|trk)\b/)[0], 'name')
  return { name, track, waypoints }
}

/** Date (ISO) and minutes after midnight of an instant, in Ecuador. */
export function localTime(iso: string): { date: string; minutes: number } | null {
  const ms = Date.parse(iso)
  if (!Number.isFinite(ms)) return null
  const parts = Object.fromEntries(
    new Intl.DateTimeFormat('en-CA', {
      timeZone: ZONE,
      year: 'numeric',
      month: '2-digit',
      day: '2-digit',
      hour: '2-digit',
      minute: '2-digit',
      hourCycle: 'h23',
    })
      .formatToParts(new Date(ms))
      .map(p => [p.type, p.value]),
  )
  return { date: `${parts.year}-${parts.month}-${parts.day}`, minutes: Number(parts.hour) * 60 + Number(parts.minute) }
}

/** Start and end of a track in Ecuador time. */
export function trackSpan(track: TrackPoint[]) {
  const times = track.map(p => p[3]).filter((t): t is string => !!t)
  if (!times.length) return null
  const start = localTime(times[0])
  const end = localTime(times[times.length - 1])
  return start && end ? { date: start.date, start: start.minutes, end: end.minutes } : null
}

export function trackLength(track: TrackPoint[]) {
  let total = 0
  for (let i = 1; i < track.length; i++) {
    const [a, b] = [track[i - 1], track[i]]
    const lat = ((a[0] + b[0]) / 2) * (Math.PI / 180)
    const dx = (b[1] - a[1]) * (Math.PI / 180) * 6371000 * Math.cos(lat)
    const dy = (b[0] - a[0]) * (Math.PI / 180) * 6371000
    total += Math.hypot(dx, dy)
  }
  return total
}

// ------------------------------------------------------------ waypoint notes

/** Known species with their subspecies, most used first. */
export type Taxa = Map<string, string[]>

export function taxaFrom(rows: TableRow[]): Taxa {
  const counts = new Map<string, Map<string, number>>()
  for (const row of rows) {
    if (!row.observed) continue
    const species = String(row.values.SPECIES ?? '').trim()
    if (!/^[A-Z][a-z]+ [a-z-]+$/.test(species)) continue
    const subs = counts.get(species) || new Map<string, number>()
    const sub = String(row.values.Subspecies_Form ?? '').trim()
    if (sub && !/^(NA|N\/A)$/i.test(sub)) subs.set(sub, (subs.get(sub) || 0) + 1)
    counts.set(species, subs)
  }
  const out: Taxa = new Map()
  for (const [species, subs] of counts)
    out.set(
      species,
      [...subs].sort((a, b) => b[1] - a[1]).map(([s]) => s),
    )
  return out
}

function levenshtein(a: string, b: string) {
  if (a === b) return 0
  let prev = Array.from({ length: b.length + 1 }, (_, i) => i)
  for (let i = 1; i <= a.length; i++) {
    const cur = [i]
    for (let j = 1; j <= b.length; j++)
      cur[j] = Math.min(prev[j] + 1, cur[j - 1] + 1, prev[j - 1] + (a[i - 1] === b[j - 1] ? 0 : 1))
    prev = cur
  }
  return prev[b.length]
}

/** Typos allowed in a name of this length. */
const tolerance = (word: string) => (word.length >= 8 ? 2 : word.length >= 5 ? 1 : 0)

export interface Capture {
  text: string
  /** The M number of the waypoint (butterfly 1, 2, … of the day). */
  seq: number | null
  species: string | null
  subspecies: string | null
  /** False when the name was not found among the species already in the sheet. */
  known: boolean
  sex: 'female' | 'male' | null
  /** Time of capture, minutes after midnight. */
  minutes: number | null
  height: number | null
  cloud: string | null
  rain: string | null
  markId: string | null
  /** The note itself says it is a recapture. */
  recaptureNote: boolean
  /** Words that were not understood; kept for the notes column. */
  rest: string
  /** Butterflies noted at this one point ("Mariposa 1 y 2" is two). */
  count: number
}

const WEATHER: [RegExp, string][] = [
  [/\bnublado oscuro\b|\bno\b|\bcd\b/, CLOUD.CD],
  [/\bnublado claro\b|\bnc\b|\bcl\b/, CLOUD.CL],
  [/\bparches?\b|\bs&c\b|\bsc\b/, CLOUD.SC],
  [/\bsoleado\b|\bdespejado\b|\bsol\b/, CLOUD.S],
]

/** Understands one waypoint note. Order and case of the parts do not matter. */
export function parseCapture(input: string, taxa: Taxa): Capture {
  let text = ` ${input.toLowerCase().replace(/\s+/g, ' ')} `
  const take = (re: RegExp) => {
    const m = re.exec(text)
    if (m) text = text.slice(0, m.index) + ' ' + text.slice(m.index + m[0].length)
    return m
  }
  // "Mariposa 1 y 2", "Marip 3 4 y 5": several butterflies noted at one point.
  const many = take(/^\s*(?:mariposas?|marip\.?)\s+((?:\d{1,3}(?:\s*(?:,|y|e)\s*|\s+))*\d{1,3})\b(?!\s*(?:[.,:]\d|m\b|cm\b))/)
  const numbers = many ? many[1].match(/\d+/g)!.map(Number) : []
  const seq = numbers.length ? null : take(/^\s*m\s?(\d{1,3})\b/)
  // "id: B69", or a mark written on its own as some collectors do ("B51 9:51 female …").
  // An M mark later in the note also counts ("… t4 M61"); at the start, "M1" is the point number.
  const mark =
    take(/\bid\s*[:#.]?\s*([a-z]{1,3})\s*-?\s*(\d{1,4})\b/) ||
    take(/(?<=\S.*)\b([abm])\s?(\d{1,3})\b(?![.,:]\d)|\b([ab])\s?(\d{1,3})\b(?![.,:]\d)/)
  // "9:20", "9h20", or "10.19" (a dot, when it cannot be a height: hour 6–18, two-digit minutes, no unit).
  const time = take(/\b([01]?\d|2[0-3])\s?[:h]\s?([0-5]\d)\b/) || take(/\b(0?[6-9]|1[0-8])\.([0-5]\d)\b(?!\s*(?:m|cm)\b)/)
  const height = take(/\b(\d+(?:[.,]\d+)?)\s*(cm|m)\b/)
  const recapture = take(/\b(recap\w*|recatch\w*)\b/)
  let sex: Capture['sex'] = null
  if (take(/\b(hembra|female|fem)\b/)) sex = 'female'
  else if (take(/\b(macho|male)\b/)) sex = 'male'
  let cloud: string | null = null
  for (const [re, code] of WEATHER) if (!cloud && take(re)) cloud = code
  let rain: string | null = null
  if (take(/\b(llovizna|garua|garúa|drizzle|dz)\b/)) rain = RAIN.DZ
  else if (take(/\b(seco|dry|dy)\b/)) rain = RAIN.DY

  const words = text.split(/[\s,;:]+/).filter(w => /^[a-záéíóúñ-]+\.?$/.test(w))
  const taxon = matchTaxon(words, taxa)
  const rest = words.slice(taxon.used).join(' ')
  const h = height ? Number(height[1].replace(',', '.')) / (height[2] === 'cm' ? 100 : 1) : null
  return {
    text: input,
    seq: numbers.length ? numbers[0] : seq ? Number(seq[1]) : null,
    species: taxon.species,
    subspecies: taxon.subspecies,
    known: taxon.known,
    sex,
    minutes: time ? Number(time[1]) * 60 + Number(time[2]) : null,
    height: h !== null && Number.isFinite(h) ? Math.round(h * 100) / 100 : null,
    cloud,
    rain,
    markId: mark ? `${(mark[1] || mark[3]).toUpperCase()}${Number(mark[2] || mark[4])}` : null,
    recaptureNote: !!recapture,
    rest,
    count: Math.max(1, numbers.length),
  }
}

const capital = (w: string) => w.charAt(0).toUpperCase() + w.slice(1)

/** Finds "genus epithet [subspecies]" at the start of the words, allowing small typos. */
export function matchTaxon(words: string[], taxa: Taxa) {
  const none = { species: null, subspecies: null, known: false, used: 0 }
  if (words.length < 2) return none
  const [g, e] = [words[0].replace(/\.$/, ''), words[1]]
  let best: { species: string; cost: number } | null = null
  for (const species of taxa.keys()) {
    const [genus, epithet] = species.toLowerCase().split(' ')
    // "H. illinissa": a genus initial is enough when the epithet matches.
    const gc = g.length === 1 ? (genus.startsWith(g) ? 0 : 99) : levenshtein(g, genus)
    const ec = levenshtein(e, epithet)
    if ((g.length > 1 && gc > tolerance(genus)) || ec > tolerance(epithet)) continue
    if (!best || gc + ec < best.cost) best = { species, cost: gc + ec }
  }
  if (!best) {
    if (g.length < 3 || e.length < 3) return none
    const sub = words[2] && words[2].length > 2 ? words[2] : null
    return { species: `${capital(g)} ${e}`, subspecies: sub, known: false, used: sub ? 3 : 2 }
  }
  const subs = taxa.get(best.species) || []
  const word = words[2]
  // No subspecies written: left empty (the grid offers the known ones); guessing it is not safe.
  if (!word) return { species: best.species, subspecies: null, known: true, used: 2 }
  const sub = subs.find(s => levenshtein(word, s.toLowerCase()) <= tolerance(s))
  if (sub) return { species: best.species, subspecies: sub, known: true, used: 3 }
  // An unknown third word is taken as a new subspecies name (to be reviewed).
  return { species: best.species, subspecies: word, known: false, used: 3 }
}

// ------------------------------------------------------ rows for the sheet

const pad = (n: number) => String(n).padStart(2, '0')
export const formatMinutes = (m: number | null) => (m === null ? '' : `${Math.floor(m / 60)}:${pad(m % 60)}`)

export interface CaptureContext {
  date: string
  collector: string
  section: number | null
}

/** Collection_data values for a capture, following how monitoring rows are filled in the sheet. */
export function captureValues(c: Capture, ctx: CaptureContext): Record<string, CellValue> {
  const marked = !!c.markId
  const values: Record<string, CellValue> = {
    Release_Collect: marked ? 'Mark_Released' : 'Collected_Preserved',
    FieldMark_ID: c.markId || 'NA',
    Insectary_ID: 'NA',
    CAM_ID_insectary: 'NA',
    Purpose: 'Monitoring',
    SPECIES: c.species,
    Subspecies_Form: c.subspecies,
    Identifier: ctx.collector || null,
    ID_status: c.species && c.known ? 'COMPLETE' : null,
    Sex: c.sex,
    Country: 'Ecuador',
    Collection_location: 'Ikiam',
    Transect_section: ctx.section,
    Collection_date: isoToSerial(ctx.date),
    Collection_time: c.minutes === null ? null : c.minutes / 1440,
    Collector: ctx.collector || null,
    Rainfall: c.rain || RAIN.DY,
    Cloud_cover: c.cloud,
    Flight_height: c.height,
  }
  if (marked)
    Object.assign(values, {
      CAM_ID: 'NA',
      Tube_1_id: 'NA',
      Tube_1_tissue: 'NOT_COLLECTED',
      Butterfly_weight: 'NA',
      Preservation_date: 'NA',
      Preservation_medium: 'NOT_COLLECTED',
    })
  const initials = ctx.collector.split(' - ')[0]
  // A recapture needs no note: the repeated field mark already says it.
  const note = c.rest
  if (note) {
    const [y, m, d] = ctx.date.split('-').map(Number)
    values.Notes_Collection_data = `${d}/${m}/${y} ${initials}: ${note}`
  }
  return values
}

export interface ImportedCapture extends Capture {
  lat: number
  lon: number
  ele: number | null
  section: number | null
  sectionDistance: number
  photos: string[]
}

export function locateCapture(w: GpxWaypoint, taxa: Taxa): ImportedCapture {
  const c = parseCapture(w.text, taxa)
  const near = nearestSection(w.lat, w.lon)
  if (c.minutes === null && w.time) c.minutes = localTime(w.time)?.minutes ?? null
  return {
    ...c,
    lat: w.lat,
    lon: w.lon,
    ele: w.ele,
    section: near.distance <= MAX_SECTION_DISTANCE ? near.section : null,
    sectionDistance: Math.round(near.distance),
    photos: w.photos || [],
  }
}

/** Wikiloc names like "Monitoreo ithomidos FCH 26 SEP 2026" carry the collector's initials. */
export function collectorFromName(name: string, collectors: string[]): string | null {
  return collectors.find(c => new RegExp(`\\b${c.split(' - ')[0].trim()}\\b`).test(name)) || null
}

/** Rows per field mark (upper case), to tell a new mark from a recapture. */
export function markIndex(rows: TableRow[]): Map<string, TableRow[]> {
  const out = new Map<string, TableRow[]>()
  for (const row of rows.filter(hasMark)) {
    const id = String(row.values.FieldMark_ID).trim().toUpperCase()
    out.set(id, [...(out.get(id) || []), row])
  }
  return out
}

/** Rows with the same field mark from before `when` (ISO): if any, the capture may be a recapture. */
export function earlierMarks(c: Capture, marks: Map<string, TableRow[]>, when: string): TableRow[] {
  if (!c.markId || !when) return []
  const day = isoToSerial(when)
  return (marks.get(c.markId) || []).filter(r => typeof r.values.Collection_date === 'number' && r.values.Collection_date < day)
}

/** Earlier rows of the same mark on the same species: the capture is a recapture. */
export function sameIndividual(c: Capture, marks: Map<string, TableRow[]>, when: string): TableRow[] {
  return earlierMarks(c, marks, when).filter(
    r => !!c.species && String(r.values.SPECIES ?? '').toLowerCase() === c.species.toLowerCase(),
  )
}

export interface CaptureCheck {
  text: string
  kind: 'ok' | 'info' | 'warn'
}

export interface ReviewContext {
  /** Collection_data rows. */
  rows: TableRow[]
  /** Day of the walk (ISO). */
  date: string
  /** Every capture of the walk, to spot a mark noted twice. */
  captures: Capture[]
  marks: Map<string, TableRow[]>
  /** Preserved individuals per species up to the walk (preservedForRule). */
  preserved: Map<string, number>
  isIthomiini: (species: string | null | undefined) => boolean
  /** The row already holding the capture, when the caller matched it another way (e.g. matchWalk). */
  existing?: TableRow | null
}

/**
 * What the review says about one capture: already in the sheet, new mark or
 * recapture, a mark used for another species, the 30-preserved rule, and the
 * parts missing from the note. Shared by "Importar recorrido" and the assistant.
 */
export function reviewCapture(
  c: ImportedCapture,
  i: number,
  ctx: ReviewContext,
): { existing: TableRow | null; recapture: TableRow | null; list: CaptureCheck[] } {
  const out: CaptureCheck[] = []
  const existing = ctx.existing !== undefined ? ctx.existing : ctx.date ? existingRow(ctx.rows, ctx.date, c) : null
  // Already in the sheet: nothing will be written, so no further checks.
  if (existing) return { existing, recapture: null, list: [{ kind: 'info', text: `Ya está en la hoja (fila ${existing.row})` }] }
  let recapture: TableRow | null = null
  if (c.markId) {
    const same = sameIndividual(c, ctx.marks, ctx.date)
    const others = earlierMarks(c, ctx.marks, ctx.date).filter(r => !same.includes(r))
    recapture = same[0] || null
    if (recapture)
      out.push({
        kind: 'ok',
        text: `Recaptura de ${c.markId} (marcada ${formatSerial(recapture.values.Collection_date as number)})`,
      })
    else out.push({ kind: 'ok', text: `Nueva marca ${c.markId}` })
    for (const r of others)
      out.push({ kind: 'warn', text: `${c.markId} ya se usó para ${r.values.SPECIES} (fila ${r.row}): ¿ID repetida?` })
    if (!recapture && c.recaptureNote && !others.length)
      out.push({ kind: 'warn', text: `Dice recaptura, pero ${c.markId} no está en la hoja` })
    if (ctx.captures.some((o, j) => j !== i && o.markId === c.markId))
      out.push({ kind: 'warn', text: `${c.markId} aparece dos veces en este recorrido` })
  } else {
    out.push({ kind: 'info', text: 'Preservado (sin marca)' })
    const preserved = c.species ? ctx.preserved.get(c.species) || 0 : 0
    if (preserved >= MARK_THRESHOLD && ctx.isIthomiini(c.species))
      out.push({ kind: 'warn', text: `${c.species} ya tiene ${preserved} preservados: ¿no debía marcarse?` })
  }
  if (!c.species) out.push({ kind: 'warn', text: 'Sin especie' })
  else if (!c.known) out.push({ kind: 'warn', text: 'Nombre no encontrado en la hoja: revisar' })
  if (!c.sex) out.push({ kind: 'warn', text: 'Sin sexo' })
  if (c.minutes === null) out.push({ kind: 'warn', text: 'Sin hora' })
  if (c.height === null) out.push({ kind: 'warn', text: 'Sin altura' })
  if (!c.cloud) out.push({ kind: 'warn', text: 'Sin clima' })
  if (c.section === null) out.push({ kind: 'warn', text: `Lejos del sendero (${c.sectionDistance} m)` })
  if (c.rest) out.push({ kind: 'info', text: `A notas: “${c.rest}”` })
  return { existing, recapture, list: out }
}

// ------------------------------------------------------------ summaries

const text = (v: CellValue | undefined) => (v === null || v === undefined ? '' : String(v).trim())

/** Tribe of each species, from the (formula) Tribe column of Collection_data. */
export function tribesFrom(rows: TableRow[]): Map<string, string> {
  const out = new Map<string, string>()
  for (const row of rows) {
    const species = text(row.values.SPECIES)
    const tribe = text(row.values.Tribe)
    if (species && tribe && !out.has(species)) out.set(species, tribe)
  }
  return out
}

/** Key of a collector's day: "46284|AA". */
export const dayKey = (serial: number, collector: string) => `${serial}|${collector.split(' - ')[0].trim().toUpperCase()}`

/** Collector-days recorded as Ikiam monitoring in SamplingDay_data. */
export function monitoringDays(dayRows: TableRow[]): Set<string> {
  const out = new Set<string>()
  for (const r of dayRows) {
    const d = r.values.Date
    if (r.observed && typeof d === 'number' && /^monitor/i.test(text(r.values.Purpose)) && /ikiam/i.test(text(r.values.Location)))
      out.add(dayKey(d, text(r.values.Collectors_initials)))
  }
  return out
}

/** Purpose left empty or "NA". */
export const noPurpose = (row: TableRow) => /^(|na|n\/a|none)$/i.test(text(row.values.Purpose))

/**
 * Rows of the Ikiam monitoring (the only ones counted in the summaries):
 * Purpose "Monitoring…", or no Purpose on a day the collector recorded as
 * monitoring in SamplingDay_data (some early rows were entered with "NA").
 */
export function isMonitoringRow(row: TableRow, days?: Set<string>) {
  if (!row.observed || !/^ikiam$/i.test(text(row.values.Collection_location))) return false
  if (/^monitoring/i.test(text(row.values.Purpose))) return true
  const d = dateOf(row)
  return !!days && d !== null && noPurpose(row) && days.has(dayKey(d, text(row.values.Collector)))
}

/**
 * Places whose preserved individuals count towards the 30-preserved rule. The
 * team counted every preserved butterfly from Ikiam and nearby Casa de Lin,
 * whatever its purpose: with that count every species had reached 30 when its
 * marking started (e.g. Oleria gunilla, 31 in July 2024).
 */
export const RULE_LOCATIONS = ['Ikiam', 'Casa de Lin']
export const isRuleRow = (row: TableRow) =>
  row.observed && RULE_LOCATIONS.some(l => l.toLowerCase() === text(row.values.Collection_location).toLowerCase())

/** Preserved individuals per species for the 30-preserved rule (optionally only before a date serial). */
export function preservedForRule(rows: TableRow[], before?: number): Map<string, number> {
  const out = new Map<string, number>()
  for (const row of rows)
    if (
      isRuleRow(row) &&
      text(row.values.Release_Collect) === 'Collected_Preserved' &&
      (before === undefined || (dateOf(row) ?? Infinity) < before)
    ) {
      const species = text(row.values.SPECIES)
      if (species) out.set(species, (out.get(species) || 0) + 1)
    }
  return out
}

export const hasMark = (row: TableRow) => {
  const id = text(row.values.FieldMark_ID)
  return !!id && !/^(NA|N\/A|not given)$/i.test(id)
}
const markOf = (row: TableRow) => text(row.values.FieldMark_ID).toUpperCase()

const dateOf = (row: TableRow) => (typeof row.values.Collection_date === 'number' ? row.values.Collection_date : null)
const byDate = (a: TableRow, b: TableRow) => (dateOf(a) ?? 0) - (dateOf(b) ?? 0) || a.row - b.row

/** Rows per field mark, oldest first. */
function markGroups(rows: TableRow[]) {
  const groups = new Map<string, TableRow[]>()
  for (const row of [...rows].filter(hasMark).sort(byDate)) groups.set(markOf(row), [...(groups.get(markOf(row)) || []), row])
  return groups
}

/**
 * Recaptures: rows whose field mark was seen before on the same species. The
 * same mark on another species is a reused ID, not a recapture (see markConflicts).
 */
export function recaptureIds(rows: TableRow[]): Set<string> {
  const out = new Set<string>()
  for (const list of markGroups(rows).values()) {
    const seen = new Set<string>()
    for (const row of list) {
      const species = text(row.values.SPECIES).toLowerCase()
      if (seen.has(species)) out.add(row.id)
      seen.add(species)
    }
  }
  return out
}

/** Field marks recorded on more than one species: an ID given twice, or a wrong species. */
export function markConflicts(rows: TableRow[]): { id: string; rows: TableRow[] }[] {
  const out: { id: string; rows: TableRow[] }[] = []
  for (const [id, list] of markGroups(rows))
    if (new Set(list.map(r => text(r.values.SPECIES).toLowerCase())).size > 1) out.push({ id, rows: list })
  return out.sort((a, b) => (dateOf(b.rows.at(-1)!) ?? 0) - (dateOf(a.rows.at(-1)!) ?? 0))
}

export interface SpeciesStat {
  key: string
  species: string
  subspecies: string
  preserved: number
  marked: number
  recaptured: number
  other: number
  female: number
  male: number
  total: number
}

/** Counts per species (or subspecies) as in the "Monitoring Samples Summary" tables. */
export function speciesStats(rows: TableRow[], bySubspecies: boolean, recaptures = recaptureIds(rows)): SpeciesStat[] {
  const stats = new Map<string, SpeciesStat>()
  for (const row of rows) {
    const species = text(row.values.SPECIES) || 'Sin especie'
    const subspecies = bySubspecies ? text(row.values.Subspecies_Form) : ''
    const key = subspecies ? `${species} ${subspecies}` : species
    const s = stats.get(key) || {
      key,
      species,
      subspecies,
      preserved: 0,
      marked: 0,
      recaptured: 0,
      other: 0,
      female: 0,
      male: 0,
      total: 0,
    }
    const kind = text(row.values.Release_Collect)
    if (recaptures.has(row.id)) s.recaptured++
    else if (kind === 'Collected_Preserved') s.preserved++
    else if (kind === 'Mark_Released') s.marked++
    else s.other++
    const sex = text(row.values.Sex).toLowerCase()
    if (sex.startsWith('female')) s.female++
    else if (sex.startsWith('male')) s.male++
    s.total++
    stats.set(key, s)
  }
  return [...stats.values()].sort((a, b) => b.total - a.total || a.key.localeCompare(b.key))
}

export interface MarkHistory {
  id: string
  species: string
  sex: string
  events: { row: TableRow; date: number | null; collector: string; section: string; minutes: number | null }[]
}

/** Every marked individual seen more than once (same mark and species), with its captures. */
export function markHistories(rows: TableRow[]): MarkHistory[] {
  const out: MarkHistory[] = []
  for (const [id, list] of markGroups(rows)) {
    const bySpecies = new Map<string, TableRow[]>()
    for (const row of list) {
      const species = text(row.values.SPECIES).toLowerCase()
      bySpecies.set(species, [...(bySpecies.get(species) || []), row])
    }
    for (const same of bySpecies.values()) {
      if (same.length < 2) continue
      out.push({
        id,
        species: [text(same[0].values.SPECIES), text(same[0].values.Subspecies_Form)].filter(Boolean).join(' '),
        sex: text(same[0].values.Sex),
        events: same.map(row => ({
          row,
          date: dateOf(row),
          collector: text(row.values.Collector).split(' - ')[0],
          section: text(row.values.Transect_section),
          minutes: typeof row.values.Collection_time === 'number' ? Math.round(row.values.Collection_time * 1440) : null,
        })),
      })
    }
  }
  return out.sort((a, b) => (b.events.at(-1)?.date ?? 0) - (a.events.at(-1)?.date ?? 0))
}

/** The next free mark in the series currently in use (e.g. B68 → B69). */
export function nextMarkId(rows: TableRow[]): string | null {
  const marks = rows
    .filter(hasMark)
    .sort(byDate)
    .map(r => /^([A-Z]+)(\d+)$/.exec(text(r.values.FieldMark_ID).toUpperCase()))
    .filter((m): m is RegExpExecArray => !!m)
  const latest = marks.at(-1)
  if (!latest) return null
  const series = latest[1]
  const max = Math.max(...marks.filter(m => m[1] === series).map(m => Number(m[2])))
  if (max < 99) return `${series}${max + 1}`
  const next = series === 'M' ? 'A' : String.fromCharCode(series.charCodeAt(0) + 1)
  return `${next}1`
}

export const monthOf = (row: TableRow) => {
  const d = dateOf(row)
  return d === null ? null : serialToIso(d).slice(0, 7)
}

/** Individuals per month and transect section (index 0 = no section). */
export function sectionsByMonth(rows: TableRow[]) {
  const out = new Map<string, number[]>()
  for (const row of rows) {
    const month = monthOf(row)
    if (!month) continue
    const counts = out.get(month) || [0, 0, 0, 0, 0]
    const section = Number(text(row.values.Transect_section))
    counts[section >= 1 && section <= 4 ? section : 0]++
    out.set(month, counts)
  }
  return new Map([...out].sort((a, b) => b[0].localeCompare(a[0])))
}

/** Per calendar month and year: individuals, new marks and recaptures. */
export function monthsByYear(rows: TableRow[], recaptures = recaptureIds(rows)) {
  const out = new Map<string, { total: number; marked: number; recaptured: number }>()
  for (const row of rows) {
    const month = monthOf(row)
    if (!month) continue
    const cell = out.get(month) || { total: 0, marked: 0, recaptured: 0 }
    cell.total++
    if (recaptures.has(row.id)) cell.recaptured++
    else if (text(row.values.Release_Collect) === 'Mark_Released') cell.marked++
    out.set(month, cell)
  }
  return out
}

/**
 * Whether a capture is already in the sheet: same day and mark, or same day,
 * species and minute (±2). A point noted without a species (identified later
 * from its photo) matches on the day and minute, and on the sex if both have one.
 */
export function existingRow(rows: TableRow[], date: string, c: Capture): TableRow | null {
  const serial = isoToSerial(date)
  const sameDay = rows.filter(row => dateOf(row) === serial)
  const minuteOf = (row: TableRow) =>
    typeof row.values.Collection_time === 'number' ? Math.round(row.values.Collection_time * 1440) : null
  const near = (row: TableRow) => c.minutes !== null && minuteOf(row) !== null && Math.abs(minuteOf(row)! - c.minutes) <= 2
  // A mark, or "M84" at the start of a note (read as the point number) when the sheet has that mark.
  const mark = c.markId || (c.seq !== null ? `M${c.seq}` : null)
  if (mark) {
    const marked = sameDay.find(row => text(row.values.FieldMark_ID).toUpperCase() === mark)
    if (marked) return marked
  }
  if (c.species)
    return sameDay.find(row => text(row.values.SPECIES).toLowerCase() === c.species!.toLowerCase() && near(row)) || null
  const sexOf = (row: TableRow) =>
    text(row.values.Sex)
      .toLowerCase()
      .replace(/\s*\?$/, '')
  return sameDay.find(row => near(row) && (!c.sex || !sexOf(row) || sexOf(row) === c.sex)) || null
}

/**
 * For the map, a capture that is already a sheet row takes the row's curated
 * species, subspecies, sex, mark and section (the note may lack them).
 */
export function withSheetValues<T extends Capture & { section: number | null }>(
  c: T,
  row: TableRow | null,
): T & { row?: number } {
  if (!row) return c
  const v = row.values
  const section = Number(text(v.Transect_section))
  return {
    ...c,
    species: text(v.SPECIES) || c.species,
    subspecies: text(v.Subspecies_Form) || c.subspecies,
    sex: /^female/i.test(text(v.Sex)) ? 'female' : /^male/i.test(text(v.Sex)) ? 'male' : c.sex,
    markId: hasMark(row) ? text(v.FieldMark_ID).toUpperCase() : c.markId,
    section: section >= 1 && section <= 4 ? section : c.section,
    row: row.row,
  }
}

// ------------------------------------------- recaptures written only in notes

export interface NoteRecapture {
  row: TableRow
  /** The part of the note that describes the recapture. */
  note: string
  date: string | null
  minutes: number | null
  height: number | null
  cloud: string | null
  rain: string | null
  initials: string | null
  section: number | null
}

const NOTE_CLOUD: [RegExp, string][] = [
  [/cloudy[\s_-]*dark|\bCD\b|CD_|nublado oscuro/i, CLOUD.CD],
  [/cloudy[\s_-]*light|\bCL\b|CL_|nublado claro/i, CLOUD.CL],
  [/sun[\s_&and-]*cloud|S&C|parches/i, CLOUD.SC],
  [/\bsun(ny)?\b|\bsol\b|S_\(|cloudless/i, CLOUD.S],
]

function noteDate(text: string): string | null {
  const m = /date\s*=\s*(\d{1,2})\/(\d{1,2})\/(\d{2,4})/i.exec(text) || /^\s*(\d{1,2})[/-](\d{1,2})[/-](\d{2,4})\b/.exec(text)
  if (!m) return null
  const year = Number(m[3]) < 100 ? 2000 + Number(m[3]) : Number(m[3])
  return `${year}-${String(m[2]).padStart(2, '0')}-${String(m[1]).padStart(2, '0')}`
}

/**
 * Recaptures that were written in the notes of the marking row instead of as
 * their own row (e.g. "7/7/24 AA: recatch&realease transect=4, time=9:59,
 * height=0.5m"). Each becomes a proposal for a new Mark_Released row.
 */
export function noteRecaptures(rows: TableRow[]): NoteRecapture[] {
  const out: NoteRecapture[] = []
  const groups = markGroups(rows)
  for (const row of rows.filter(hasMark)) {
    for (const part of text(row.values.Notes_Collection_data).split('|')) {
      if (!/recap|recatch|re-catch/i.test(part)) continue
      const date = noteDate(part)
      // A note on the recapture row itself ("Butterfly recatch") needs no new row.
      if (!date || isoToSerial(date) === dateOf(row)) continue
      const already = (groups.get(markOf(row)) || []).some(
        r => dateOf(r) === isoToSerial(date) && text(r.values.SPECIES) === text(row.values.SPECIES),
      )
      if (already) continue
      const time = /\b([01]?\d|2[0-3]):([0-5]\d)/.exec(part)
      const height =
        /height\s*=?\s*(\d+(?:[.,]\d+)?)\s*m?\b/i.exec(part) ||
        /fligh\w*\s*h\w*\s*(\d+(?:[.,]\d+)?)/i.exec(part) ||
        /\b(\d+(?:[.,]\d+)?)\s*m\b/.exec(part)
      const initials = /collector\s*=\s*([A-Z]{2,4})\b/.exec(part) || /^\s*[\d/-]+\s+([A-Z]{2,4})\s*:/.exec(part)
      const section = /transect\s*[=#]?\s*#?\s*([1-4])\b/i.exec(part)
      out.push({
        row,
        note: part.trim(),
        date,
        minutes: time ? Number(time[1]) * 60 + Number(time[2]) : null,
        height: height ? Number(height[1].replace(',', '.')) : null,
        cloud: NOTE_CLOUD.find(([re]) => re.test(part))?.[1] ?? null,
        rain: /drizzle|llovizna|\bDZ\b/i.test(part) ? RAIN.DZ : /\bDY\b|\bdry\b|seco/i.test(part) ? RAIN.DY : null,
        initials: initials ? initials[1] : null,
        section: section ? Number(section[1]) : null,
      })
    }
  }
  return out
}

/** A new Mark_Released row for a recapture found in notes, copying the marked individual. */
export function noteRecaptureValues(r: NoteRecapture, collectors: string[]): Record<string, CellValue> {
  const v = r.row.values
  const collector = (r.initials && collectors.find(c => c.split(' - ')[0].trim() === r.initials)) || r.initials
  const [y, m, d] = (r.date || '').split('-').map(Number)
  return {
    Release_Collect: 'Mark_Released',
    FieldMark_ID: v.FieldMark_ID,
    Insectary_ID: 'NA',
    CAM_ID_insectary: 'NA',
    CAM_ID: 'NA',
    Tube_1_id: 'NA',
    Tube_1_tissue: 'NOT_COLLECTED',
    Purpose: v.Purpose,
    SPECIES: v.SPECIES,
    Subspecies_Form: v.Subspecies_Form,
    Identifier: collector,
    ID_status: v.ID_status,
    Sex: v.Sex,
    Country: v.Country,
    Collection_location: v.Collection_location,
    Transect_section: r.section,
    Collection_date: r.date ? isoToSerial(r.date) : null,
    Collection_time: r.minutes === null ? null : r.minutes / 1440,
    Collector: collector,
    Rainfall: r.rain || RAIN.DY,
    Cloud_cover: r.cloud,
    Flight_height: r.height,
    Butterfly_weight: 'NA',
    Preservation_date: 'NA',
    Preservation_medium: 'NOT_COLLECTED',
    Notes_Collection_data: `${d}/${m}/${y} ${r.initials || ''}: Recapture, moved from the note of row ${r.row.row}`,
  }
}

// ------------------------------------------------------- the live report

/** Every month from `from` to `to` (YYYY-MM), inclusive. */
export function monthRange(from: string, to: string): string[] {
  const out: string[] = []
  let [y, m] = from.split('-').map(Number)
  const [ty, tm] = to.split('-').map(Number)
  while ((y < ty || (y === ty && m <= tm)) && out.length < 600) {
    out.push(`${y}-${String(m).padStart(2, '0')}`)
    m++
    if (m > 12) {
      m = 1
      y++
    }
  }
  return out
}

export type Kind = 'preserved' | 'marked' | 'recaptured' | 'other'
export function kindOf(row: TableRow, recaptures: Set<string>): Kind {
  if (recaptures.has(row.id)) return 'recaptured'
  const kind = text(row.values.Release_Collect)
  return kind === 'Collected_Preserved' ? 'preserved' : kind === 'Mark_Released' ? 'marked' : 'other'
}

/** Individuals per month, split by what happened to them. */
export function kindsByMonth(rows: TableRow[], months: string[], recaptures: Set<string>) {
  const index = new Map(months.map((m, i) => [m, i]))
  const out: Record<Kind, number[]> = {
    preserved: months.map(() => 0),
    marked: months.map(() => 0),
    recaptured: months.map(() => 0),
    other: months.map(() => 0),
  }
  for (const row of rows) {
    const i = index.get(monthOf(row) || '')
    if (i !== undefined) out[kindOf(row, recaptures)][i]++
  }
  return out
}

const initialsOf = (collector: string) => collector.split(' - ')[0].trim().toUpperCase()

/**
 * Monitoring effort: one "day" per collector and date, from SamplingDay_data
 * (Ikiam monitoring) plus any day with captures. Returns keys "YYYY-MM-DD|INI".
 */
export function effortDays(dayRows: TableRow[], captures: TableRow[]): Set<string> {
  const out = new Set<string>()
  for (const r of dayRows) {
    const d = r.values.Date
    if (!r.observed || typeof d !== 'number' || d < 40000 || d > 60000) continue
    if (!/^monitor/i.test(text(r.values.Purpose)) || !/ikiam/i.test(text(r.values.Location))) continue
    out.add(`${serialToIso(d)}|${text(r.values.Collectors_initials).toUpperCase()}`)
  }
  for (const r of captures) {
    const d = dateOf(r)
    if (d !== null) out.add(`${serialToIso(d)}|${initialsOf(text(r.values.Collector))}`)
  }
  return out
}

/** Captures per hour of the day (index = hour). */
export function byHour(rows: TableRow[], from = 7, to = 15) {
  const out = Array.from({ length: to - from + 1 }, () => 0)
  for (const r of rows) {
    const t = r.values.Collection_time
    if (typeof t !== 'number' || t <= 0 || t >= 1) continue
    const h = Math.floor(t * 24)
    if (h >= from && h <= to) out[h - from]++
  }
  return out
}

export const HEIGHT_CLASSES = ['< 0,5', '0,5–1', '1–1,5', '1,5–2', '2–3', '≥ 3']
/** Captures per flight-height class (metres at first sight). */
export function byHeight(rows: TableRow[]) {
  const out = HEIGHT_CLASSES.map(() => 0)
  for (const r of rows) {
    const h = Number(r.values.Flight_height)
    if (r.values.Flight_height === null || r.values.Flight_height === '' || !Number.isFinite(h)) continue
    out[h < 0.5 ? 0 : h < 1 ? 1 : h < 1.5 ? 2 : h < 2 ? 3 : h < 3 ? 4 : 5]++
  }
  return out
}

export const CLOUD_CLASSES: [string, string][] = [
  ['S', 'Soleado'],
  ['S&C', 'Sol y nubes'],
  ['CL', 'Nublado claro'],
  ['CD', 'Nublado oscuro'],
]
/** Captures per cloud-cover code. */
export function byCloud(rows: TableRow[]) {
  const out = CLOUD_CLASSES.map(() => 0)
  for (const r of rows) {
    const code = text(r.values.Cloud_cover).split('_')[0]
    const i = CLOUD_CLASSES.findIndex(([c]) => c === code)
    if (i >= 0) out[i]++
  }
  return out
}

export const median = (values: number[]) => {
  if (!values.length) return null
  const s = [...values].sort((a, b) => a - b)
  const mid = Math.floor(s.length / 2)
  return s.length % 2 ? s[mid] : (s[mid - 1] + s[mid]) / 2
}

/**
 * Pairs a walk's points with the collector's rows of that day: point by point
 * (mark, species and minute, or minute and sex), then, when notes are too short
 * ("Marip 3"), in order, if the remaining butterflies and rows are as many.
 * A title one day off is tolerated when the points match the next or previous day.
 */
export function matchWalk<T extends Capture>(rows: TableRow[], date: string, collector: string, captures: T[]) {
  const attempt = (serial: number) => {
    const day = rows.filter(r => dateOf(r) === serial && dayKey(serial, text(r.values.Collector)) === dayKey(serial, collector))
    const iso = serialToIso(serial)
    const used = new Set<string>()
    const pairs: { capture: T; row: TableRow }[] = []
    const left: T[] = []
    for (const c of captures) {
      const row = existingRow(
        day.filter(r => !used.has(r.id)),
        iso,
        c,
      )
      if (row) {
        used.add(row.id)
        pairs.push({ capture: c, row })
      } else left.push(c)
    }
    const rest = day.filter(r => !used.has(r.id)).sort(byTime)
    const wanted = left.reduce((n, c) => n + c.count, 0)
    let ordered = false
    if (left.length && wanted === rest.length) {
      let i = 0
      for (const c of left) for (let k = 0; k < c.count; k++) pairs.push({ capture: c, row: rest[i++] })
      ordered = true
      left.length = 0
    }
    return { date: iso, pairs, left, ordered }
  }
  const base = isoToSerial(date)
  let best = attempt(base)
  for (const shift of [-1, 1]) {
    if (best.pairs.length === captures.length) break
    const other = attempt(base + shift)
    // Another day only when its points match by content (not just by order).
    if (!other.ordered && other.pairs.length > best.pairs.length) best = other
  }
  return best
}
const byTime = (a: TableRow, b: TableRow) =>
  (typeof a.values.Collection_time === 'number' ? a.values.Collection_time : 9) -
    (typeof b.values.Collection_time === 'number' ? b.values.Collection_time : 9) || a.row - b.row

/**
 * Species accumulation: after each monitoring day (in date order), how many
 * species have been seen. Levelling off means the assemblage is well sampled.
 */
export function speciesAccumulation(rows: TableRow[]) {
  const byDay = new Map<number, Set<string>>()
  for (const r of rows) {
    const d = dateOf(r)
    const sp = text(r.values.SPECIES)
    if (d === null || !sp) continue
    byDay.set(d, (byDay.get(d) || new Set()).add(sp))
  }
  const seen = new Set<string>()
  return [...byDay.entries()]
    .sort((a, b) => a[0] - b[0])
    .map(([d, species], i) => {
      species.forEach(s => seen.add(s))
      return { day: i + 1, date: d, species: seen.size }
    })
}

/** Species seen only once or twice (rare in the samples). */
export function rareSpecies(rows: TableRow[]) {
  const n = new Map<string, number>()
  for (const r of rows) if (text(r.values.SPECIES)) n.set(text(r.values.SPECIES), (n.get(text(r.values.SPECIES)) || 0) + 1)
  return { once: [...n.values()].filter(v => v === 1).length, twice: [...n.values()].filter(v => v === 2).length }
}

/**
 * Seasonality: individuals per monitoring day for each species and calendar
 * month (all years pooled), so months with more walks do not look richer.
 */
export function seasonality(rows: TableRow[], effort: Iterable<string>, species: string[]) {
  const daysPerMonth = Array.from({ length: 12 }, () => 0)
  for (const key of effort) daysPerMonth[Number(key.slice(5, 7)) - 1]++
  const counts = species.map(() => Array.from({ length: 12 }, () => 0))
  for (const r of rows) {
    const i = species.indexOf(text(r.values.SPECIES))
    const m = monthOf(r)
    if (i >= 0 && m) counts[i][Number(m.slice(5)) - 1]++
  }
  return {
    daysPerMonth,
    values: counts.map(row => row.map((n, m) => (daysPerMonth[m] ? Math.round((n / daysPerMonth[m]) * 100) / 100 : 0))),
    counts,
  }
}

/** Individuals of each species per transect section (1–4). */
export function speciesBySection(rows: TableRow[], species: string[]) {
  const out = species.map(() => [0, 0, 0, 0])
  const other = [0, 0, 0, 0]
  for (const r of rows) {
    const t = Number(text(r.values.Transect_section))
    if (!(t >= 1 && t <= 4)) continue
    const i = species.indexOf(text(r.values.SPECIES))
    if (i >= 0) out[i][t - 1]++
    else other[t - 1]++
  }
  return { bySpecies: out, other }
}

/** Distances (m) between successive GPS positions of the same marked individual. */
export function recaptureDistances(
  captures: { markId: string | null; species: string | null; date: string; lat: number; lon: number }[],
  distance: (a: [number, number], b: [number, number]) => number,
) {
  const groups = new Map<string, typeof captures>()
  for (const c of captures) {
    if (!c.markId || !c.species) continue
    const key = `${c.markId}|${c.species}`
    groups.set(key, [...(groups.get(key) || []), c])
  }
  const out: { id: string; species: string; from: string; to: string; metres: number }[] = []
  for (const list of groups.values()) {
    list.sort((a, b) => a.date.localeCompare(b.date))
    for (let i = 1; i < list.length; i++)
      if (list[i].date !== list[i - 1].date)
        out.push({
          id: list[i].markId!,
          species: list[i].species!,
          from: list[i - 1].date,
          to: list[i].date,
          metres: Math.round(distance([list[i - 1].lat, list[i - 1].lon], [list[i].lat, list[i].lon])),
        })
  }
  return out
}
