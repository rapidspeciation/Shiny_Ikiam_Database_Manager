import { isoToSerial, serialToIso } from './dates'
import { MAX_SECTION_DISTANCE, nearestSection } from './transects'
import type { CellValue, TableRow } from './types'

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
  const seq = take(/^\s*m\s?(\d{1,3})\b/)
  const mark = take(/\bid\s*[:#.]?\s*([a-z]{1,3})\s*-?\s*(\d{1,4})\b/)
  const time = take(/\b([01]?\d|2[0-3])\s?[:h]\s?([0-5]\d)\b/)
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
    seq: seq ? Number(seq[1]) : null,
    species: taxon.species,
    subspecies: taxon.subspecies,
    known: taxon.known,
    sex,
    minutes: time ? Number(time[1]) * 60 + Number(time[2]) : null,
    height: h !== null && Number.isFinite(h) ? Math.round(h * 100) / 100 : null,
    cloud,
    rain,
    markId: mark ? `${mark[1].toUpperCase()}${Number(mark[2])}` : null,
    recaptureNote: !!recapture,
    rest,
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
  // With no subspecies written, a species only ever recorded with one subspecies gets that one.
  if (!word) return { species: best.species, subspecies: subs.length === 1 ? subs[0] : null, known: true, used: 2 }
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
  const note = [c.recaptureNote ? 'Recapture' : '', c.rest].filter(Boolean).join('; ')
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
  }
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

/** Rows of the Ikiam monitoring (the only ones counted in the summaries). */
export function isMonitoringRow(row: TableRow) {
  return row.observed && /^monitoring/i.test(text(row.values.Purpose)) && /^ikiam$/i.test(text(row.values.Collection_location))
}

export const hasMark = (row: TableRow) => {
  const id = text(row.values.FieldMark_ID)
  return !!id && !/^(NA|N\/A|not given)$/i.test(id)
}

const dateOf = (row: TableRow) => (typeof row.values.Collection_date === 'number' ? row.values.Collection_date : null)
const byDate = (a: TableRow, b: TableRow) => (dateOf(a) ?? 0) - (dateOf(b) ?? 0) || a.row - b.row

/**
 * Mark_Released rows whose field mark was already seen earlier: the first
 * row of a mark is the marking, every later row with the same mark a recapture.
 */
export function recaptureIds(rows: TableRow[]): Set<string> {
  const seen = new Set<string>()
  const out = new Set<string>()
  for (const row of [...rows].filter(hasMark).sort(byDate)) {
    const id = text(row.values.FieldMark_ID).toUpperCase()
    if (seen.has(id)) out.add(row.id)
    seen.add(id)
  }
  return out
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

/** Every field mark that was seen more than once, with its capture history. */
export function markHistories(rows: TableRow[]): MarkHistory[] {
  const groups = new Map<string, TableRow[]>()
  for (const row of rows.filter(hasMark)) {
    const id = text(row.values.FieldMark_ID).toUpperCase()
    groups.set(id, [...(groups.get(id) || []), row])
  }
  const out: MarkHistory[] = []
  for (const [id, list] of groups) {
    if (list.length < 2) continue
    list.sort(byDate)
    out.push({
      id,
      species: [text(list[0].values.SPECIES), text(list[0].values.Subspecies_Form)].filter(Boolean).join(' '),
      sex: text(list[0].values.Sex),
      events: list.map(row => ({
        row,
        date: dateOf(row),
        collector: text(row.values.Collector).split(' - ')[0],
        section: text(row.values.Transect_section),
        minutes: typeof row.values.Collection_time === 'number' ? Math.round(row.values.Collection_time * 1440) : null,
      })),
    })
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

/** Whether a capture is already in the sheet (same day, species and minute ±2). */
export function existingRow(rows: TableRow[], date: string, c: Capture): TableRow | null {
  const serial = isoToSerial(date)
  return (
    rows.find(row => {
      if (dateOf(row) !== serial) return false
      if (c.markId && text(row.values.FieldMark_ID).toUpperCase() === c.markId) return true
      const minutes = typeof row.values.Collection_time === 'number' ? Math.round(row.values.Collection_time * 1440) : null
      return (
        !!c.species &&
        text(row.values.SPECIES).toLowerCase() === c.species.toLowerCase() &&
        c.minutes !== null &&
        minutes !== null &&
        Math.abs(minutes - c.minutes) <= 2
      )
    }) || null
  )
}
