// Explicit .ts extensions: the server loads this file too (Node strips the types), so the
// assistant builds monitoring rows exactly as the Monitoreo review does (server/walks.mjs).
import { formatSerial, isoToSerial, serialToIso } from './dates.ts'
import { estimatedSection } from './transects.ts'
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
  /** Wikiloc's <author>: the person's name and profile number, which say whose walk it is. */
  author?: { name: string; id: string | null } | null
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
  const head = xml.split(/<(?:\w+:)?(?:wpt|trk)\b/)[0]
  const name =
    tag(trk.split(/<(?:\w+:)?trkseg/)[0], 'name') ||
    tag(head.replace(/<(?:\w+:)?author\b[\s\S]*?<\/(?:\w+:)?author>/, ''), 'name')
  const authorBlock = /<(?:\w+:)?author\b[^>]*>([\s\S]*?)<\/(?:\w+:)?author>/.exec(head)?.[1] || ''
  const author = authorBlock
    ? { name: tag(authorBlock, 'name'), id: /user\.do\?id=(\d{3,12})/.exec(authorBlock)?.[1] ?? null }
    : null
  return { name, track, waypoints, author }
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
  // More letters apart than any tolerance: not worth the table (names are read for every word of a note).
  if (Math.abs(a.length - b.length) > 2) return 3
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
  /** How a subspecies not written in full was chosen: the only one at Ikiam, or the start written ("id" → ida). */
  subspeciesGuess: 'ikiam' | 'prefix' | null
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
  /** The species is kept as written: it is not among the known names (not used to pair with rows). */
  unknownSpecies?: boolean
}

const WEATHER: [RegExp, string][] = [
  [/\bnublado oscuro\b|\bno\b|\bcd\b/, CLOUD.CD],
  [/\bnublado claro\b|\bnc\b|\bcl\b/, CLOUD.CL],
  // Taken whole, so nothing of it is left for the notes: "parches nube y sol", "sol y nubes", "parches de sol".
  [
    /\b(?:parches?|intervalos?)(?:\s+(?:de\s+)?(?:nubes?|sol))?(?:\s+(?:y|e|con)\s+(?:nubes?|sol))?\b|\b(?:sol|nubes?)\s+(?:y|e|con)\s+(?:nubes?|sol)\b|\bs&(?:amp;)?[cp]\b|\bsc\b/,
    CLOUD.SC,
  ],
  [/\bsoleado\b|\bdespejado\b|\bsol\b|\bsun(?:ny)?\b/, CLOUD.S],
  // MJS writes "seco obscuro", "claro seco": read after the phrases above, and before
  // "obscuro" could be taken for a name (Harjesia obscura).
  [/\bob?scuro\b/, CLOUD.CD],
  [/\bclaro\b/, CLOUD.CL],
]

/** Point numbers written as words at the start of a note ("Uno.", "Dos,"). */
const NUMBER_WORDS = ['uno', 'dos', 'tres', 'cuatro', 'cinco', 'seis', 'siete', 'ocho', 'nueve', 'diez']
const HOUR_WORDS: Record<string, number> = { seis: 6, siete: 7, ocho: 8, nueve: 9, diez: 10, once: 11, doce: 12 }

/**
 * Understands one waypoint note. Order and case of the parts do not matter.
 * `local` holds the species and subspecies seen at Ikiam: abbreviations and a
 * missing subspecies are resolved against them first.
 */
export function parseCapture(input: string, taxa: Taxa, local?: Taxa): Capture {
  // "O.gunilla", "H.a": a dot between letters separates an abbreviation from the next word.
  let text = ` ${input
    .toLowerCase()
    .replace(/([a-záéíóúñ])\.(?=[a-záéíóúñ])/g, '$1. ')
    .replace(/\s+/g, ' ')} `
  const take = (re: RegExp) => {
    const m = re.exec(text)
    if (m) text = text.slice(0, m.index) + ' ' + text.slice(m.index + m[0].length)
    return m
  }
  // "Mariposa 1 y 2", "Marip 3 4 y 5": several butterflies noted at one point.
  const many = take(/^\s*(?:mariposas?|marip\.?)\s+((?:\d{1,3}(?:\s*(?:,|y|e)\s*|\s+))*\d{1,3})\b(?!\s*(?:[.,:]\d|m\b|cm\b))/)
  // MJS starts with the point number, often twice ("4 4 50cm", "4 y 5 4 y 5 9:39", "3-4 10:47").
  const leading = many ? null : take(/^\s*(\d{1,3}(?:\s*(?:y|-)\s*\d{1,3})*)(?:\s+\1)?\b(?!\s*(?:[.,:h]\s?\d|m\b|cm\b|con\b))/)
  const numbers = many ? many[1].match(/\d+/g)!.map(Number) : leading ? leading[1].match(/\d+/g)!.map(Number) : []
  const mSeq = numbers.length ? null : take(/^\s*m\s?(\d{1,3})\b/)
  const seq =
    mSeq ||
    (numbers.length
      ? null
      : take(/^\s*(\d{1,2})\s?(?:ra|da|ro|do|ta|to|er|°|ª|º)\b/) || take(new RegExp(`^\\s*(${NUMBER_WORDS.join('|')})\\b`)))
  // "id: B69", or a mark written on its own as some collectors do ("B51 9:51 female …").
  // An M mark later in the note also counts ("… t4 M61"); at the start, "M1" is the point number.
  const mark =
    take(/\bid\s*[:#.]?\s*([a-z]{1,3})\s*-?\s*(\d{1,4})\b/) ||
    // ("2 m 10 03" is a height and a time: an m after a number is the unit.)
    take(/(?<=\S.*)\b([ab]|(?<!\s\d+(?:[.,]\d+)?\s?)m)\s?(\d{1,3})\b(?![.,:]\d)|\b([ab])\s?(\d{1,3})\b(?![.,:]\d)/)
  // An M number at the start is FCH's point number (M1–M15), but a mark in AA's 2024
  // notes ("M45 recatch 0,5m 9:59"): above 30, or when the note says recapture, and no other mark.
  const leadingMark =
    !mark && mSeq && (Number(mSeq[1]) > 30 || /\b(recap|recatch|reencontr)/.test(text)) ? `M${Number(mSeq[1])}` : null
  // "9:20", "9h20", or "10.19" (a dot, when it cannot be a height: hour 6–18, two-digit minutes, no unit);
  // dictated times: "10, 43", "10 con 09", "10 05", "nueve, 55".
  const time =
    take(/\b([01]?\d|2[0-3])\s?[:h]\s?([0-5]\d)\b/) ||
    take(/\b(0?[6-9]|1[0-8])\.([0-5]\d)\b(?!\s*(?:m|cm)\b)/) ||
    take(/\b(0?[6-9]|1[0-8])(?:\s*,\s*(?:con\s+)?|\s+con\s+|\s)([0-5]\d)\b(?![.,]\d|\s*(?:m|cm)\b)/) ||
    take(new RegExp(`\\b(${Object.keys(HOUR_WORDS).join('|')})\\s*(?:,|con|y)?\\s*([0-5]\\d)\\b`))
  const height = take(/\b(\d+(?:[.,]\d+)?)\s*(cm|m)\b/)
  const recapture = take(/\b(recap\w*|recatch\w*|reencontrad\w*)\b/)
  let sex: Capture['sex'] = null
  if (take(/\b(hembra|female|fem)\b/)) sex = 'female'
  else if (take(/\b(macho|male)\b/)) sex = 'male'
  let cloud: string | null = null
  for (const [re, code] of WEATHER) if (!cloud && take(re)) cloud = code
  // Weather words left over ("sol, soleado, obscuro") are not names.
  while (take(/\b(?:ob?scuro|claro|nublado|soleado|nubes?)\b/));
  let rain: string | null = null
  if (take(/\b(llovizna|llov|garua|garúa|drizzle|drizzel|dz)\b/)) rain = RAIN.DZ
  else if (take(/\b(seco|dry|dy)\b/)) rain = RAIN.DY

  const words = text.split(/[\s,;:?¿!()]+/).filter(w => /^[a-záéíóúñ-]+\.?$/.test(w))
  const taxon = findTaxon(words, taxa, local, input)
  const rest = [...words.slice(0, taxon.start), ...words.slice(taxon.start + taxon.used)].join(' ')
  const h = height ? Number(height[1].replace(',', '.')) / (height[2] === 'cm' ? 100 : 1) : null
  const hour = time ? (HOUR_WORDS[time[1]] ?? Number(time[1])) : null
  return {
    text: input,
    seq: numbers.length
      ? numbers[0]
      : seq && !leadingMark
        ? NUMBER_WORDS.includes(seq[1])
          ? NUMBER_WORDS.indexOf(seq[1]) + 1
          : Number(seq[1])
        : null,
    species: taxon.species,
    subspecies: taxon.subspecies,
    known: taxon.known,
    subspeciesGuess: taxon.subspeciesGuess,
    sex,
    minutes: time ? hour! * 60 + Number(time[2]) : null,
    height: h !== null && Number.isFinite(h) ? Math.round(h * 100) / 100 : null,
    cloud,
    rain,
    markId: mark ? `${(mark[1] || mark[3]).toUpperCase()}${Number(mark[2] || mark[4])}` : leadingMark,
    recaptureNote: !!recapture,
    rest,
    count: Math.max(1, numbers.length),
    ...(taxon.unknownSpecies ? { unknownSpecies: true } : {}),
  }
}

const capital = (w: string) => w.charAt(0).toUpperCase() + w.slice(1)
/** Species split into lower-case genus and epithet, once per list of names. */
const splitNames = new WeakMap<Taxa, [string, string, string][]>()
function namesOf(taxa: Taxa) {
  let out = splitNames.get(taxa)
  if (!out) {
    out = [...taxa.keys()].map(s => [s, ...(s.toLowerCase().split(' ') as [string, string])])
    splitNames.set(taxa, out)
  }
  return out
}
const bare = (w: string) => w.replace(/\.$/, '')
/** "f. travella" is written "travella". */
const subName = (s: string) => s.toLowerCase().replace(/^f\.\s*/, '')

interface TaxonHit {
  species: string | null
  subspecies: string | null
  known: boolean
  subspeciesGuess: 'ikiam' | 'prefix' | null
  /** Where the name starts among the words, and how many words it takes. */
  start: number
  used: number
  /** Only set when the name is not among the known species ("Genus epithet" as written). */
  unknownSpecies?: boolean
}

/**
 * The subspecies written after a name: in full (small typos allowed), or its
 * start when only one subspecies of the species (at Ikiam first) starts so
 * ("id" → ida, "m" → matronalis). Nothing written: the only one seen at Ikiam.
 * `free` takes an unknown word as a new subspecies (a name written in full).
 */
function subspeciesAfter(species: string, word: string | undefined, taxa: Taxa, local: Taxa | undefined, free: boolean) {
  const subs = taxa.get(species) || []
  const here = local?.get(species) || []
  const only = {
    subspecies: here.length === 1 ? here[0] : null,
    used: 0,
    known: true,
    guess: here.length === 1 ? ('ikiam' as const) : null,
  }
  if (!word) return only
  const w = bare(word)
  const full = subs.find(s => levenshtein(w, subName(s)) <= tolerance(subName(s)))
  if (full) return { subspecies: full, used: 1, known: true, guess: null }
  for (const list of [here, subs]) {
    const starting = list.filter(s => subName(s).startsWith(w))
    if (starting.length === 1) return { subspecies: starting[0], used: 1, known: true, guess: 'prefix' as const }
    if (starting.length > 1) break
  }
  // An unknown word right after a name written in full is taken as a new subspecies (to be reviewed).
  if (free && w.length >= 5) return { subspecies: w, used: 1, known: false, guess: null }
  return only
}

/**
 * Finds a species name anywhere among the words of a note, allowing small typos
 * and the abbreviations the collectors use. Abbreviations are read against the
 * names seen at Ikiam (`local`) and must point to one species:
 * - genus and epithet: "Hyposcada illinissa", "H. illinissa", "hyp anast"
 *   (Hypothyris anastasia), "god zav m", "O.gunilla", "ithomia s. s.";
 * - the epithet alone: "Numata", "onega janarilla", "Eucle Intermedia", and
 *   "Pol p" (three letters only with a subspecies that fits: polymnia proceriformis);
 * - a genus with a single species at Ikiam ("ceratinia"), or a subspecies
 *   found in one species only ("deceptus", "bicolora").
 * The cheapest reading wins (typos and abbreviations cost), the first one on a tie.
 */
export function findTaxon(words: string[], taxa: Taxa, local?: Taxa, original?: string): TaxonHit {
  const none: TaxonHit = { species: null, subspecies: null, known: false, used: 0, start: 0, subspeciesGuess: null }
  const here = local && local.size ? local : taxa
  let best: (TaxonHit & { cost: number }) | null = null
  const offer = (hit: TaxonHit & { cost: number }) => {
    if (hit.cost <= 3 && (!best || hit.cost < best.cost - 1e-9)) best = hit
  }
  /** The single cheapest species, or null when two tie (an abbreviation that says nothing). */
  const unique = (found: { species: string; cost: number }[]) => {
    const min = Math.min(...found.map(f => f.cost))
    const at = found.filter(f => f.cost <= min + 1e-9)
    return at.length === 1 ? at[0] : null
  }
  for (let k = 0; k < words.length; k++) {
    const g = bare(words[k])
    if (g.length < 1) continue
    const next = words[k + 1] ? bare(words[k + 1]) : ''
    // Genus and epithet. A dot or at most four letters: the start of a genus ("h.", "god.", "mech").
    if (next) {
      const abbreviated = words[k].endsWith('.') || g.length <= 4
      const found: { species: string; cost: number; short: boolean }[] = []
      for (const [species, genus, epithet] of namesOf(taxa)) {
        const gd = g.length >= 4 && genus[0] === g[0] ? levenshtein(g, genus) : 99
        // A typo in the genus costs half: the epithet says more ("hyposaca anchiala" is not "anchiala" alone).
        const gc = gd <= tolerance(genus) ? gd / 2 : abbreviated && genus.startsWith(g) ? 0.3 : -1
        if (gc < 0) continue
        const ed = levenshtein(next, epithet)
        // The start of an epithet: three letters, or one after a genus written in full ("ithomia s.").
        const ec =
          ed <= tolerance(epithet)
            ? ed
            : epithet.startsWith(next) && (next.length >= 3 || (gc < 0.3 && next.length >= 1))
              ? next.length >= 3
                ? 0.3
                : 0.6
              : -1
        if (ec < 0) continue
        const short = gc === 0.3 || (ec >= 0.3 && ed > tolerance(epithet))
        // Abbreviations are read against the names seen at Ikiam; ties go to those.
        if (short && local?.size && !local.has(species)) continue
        found.push({ species, cost: gc + ec + (local?.size && !local.has(species) ? 0.5 : 0), short })
      }
      const hit = found.length ? (found.some(f => f.short) ? unique(found) : found.sort((a, b) => a.cost - b.cost)[0]) : null
      if (hit) {
        const full = !(found.find(f => f.species === hit.species)?.short ?? true)
        const sub = subspeciesAfter(hit.species, words[k + 2], taxa, local, full)
        offer({
          species: hit.species,
          subspecies: sub.subspecies,
          known: sub.known,
          subspeciesGuess: sub.guess,
          start: k,
          used: 2 + sub.used,
          cost: hit.cost,
        })
      }
    }
    if (g.length < 3) continue
    // The epithet alone ("Numata", "Salapia", "Pol p").
    const alone: { species: string; cost: number }[] = []
    for (const [species, , epithet] of namesOf(here)) {
      const d = g.length >= 5 ? levenshtein(g, epithet) : 99
      if (d <= (epithet.length >= 8 ? 2 : 1)) alone.push({ species, cost: 1 + d })
      else if (g.length >= 5 && epithet.startsWith(g)) alone.push({ species, cost: 1.3 })
      else if (
        g.length === 3 &&
        epithet.startsWith(g) &&
        next &&
        (here.get(species) || []).some(s => subName(s).startsWith(next))
      )
        alone.push({ species, cost: 1.5 })
    }
    const epithetHit = alone.length ? unique(alone) : null
    if (epithetHit) {
      const sub = subspeciesAfter(epithetHit.species, words[k + 1], taxa, local, false)
      offer({
        species: epithetHit.species,
        subspecies: sub.subspecies,
        known: sub.known,
        subspeciesGuess: sub.guess,
        start: k,
        used: 1 + sub.used,
        cost: epithetHit.cost,
      })
    }
    if (g.length < 5) continue
    // A genus with one species at Ikiam ("ceratinia" → Ceratinia tutia).
    const ofGenus = namesOf(here)
      .filter(([, genus]) => (g.length >= 7 ? levenshtein(g, genus) <= 1 : g === genus))
      .map(([species]) => species)
    if (ofGenus.length === 1) {
      const sub = subspeciesAfter(ofGenus[0], words[k + 1], taxa, local, false)
      offer({
        species: ofGenus[0],
        subspecies: sub.subspecies,
        known: sub.known,
        subspeciesGuess: sub.guess,
        start: k,
        used: 1 + sub.used,
        cost: 1.7,
      })
    }
    // A subspecies found in one species only ("deceptus", "bicolora").
    const bySub: { species: string; sub: string; cost: number }[] = []
    for (const [species, subs] of here)
      for (const s of subs) {
        const name = subName(s)
        if (name.length < 5) continue
        if (name === g) bySub.push({ species, sub: s, cost: 2 })
        else if (g.length >= 5 && name.startsWith(g)) bySub.push({ species, sub: s, cost: 2.3 })
      }
    const subHit = bySub.length ? unique(bySub) : null
    if (subHit)
      offer({
        species: subHit.species,
        subspecies: (subHit as (typeof bySub)[number]).sub,
        known: true,
        subspeciesGuess: 'prefix',
        start: k,
        used: 1,
        cost: subHit.cost,
      })
  }
  if (best) {
    const { cost: _cost, ...hit } = best as TaxonHit & { cost: number }
    return hit
  }
  // A species not in the sheet yet, written in full at the start: kept as written, to be reviewed
  // (when its genus is known or it is written with a capital, not "nubkado srci").
  const [g, e] = [bare(words[0] || ''), words[1] || '']
  const named =
    [...taxa.keys()].some(s => s.toLowerCase().startsWith(`${g} `)) ||
    (!!original && new RegExp(`\\b${capital(g)}\\b`).test(original))
  if (g.length < 3 || e.length < 3 || !named) return none
  const sub = words[2] && words[2].length > 2 ? words[2] : null
  return {
    species: `${capital(g)} ${e}`,
    subspecies: sub,
    known: false,
    used: sub ? 3 : 2,
    start: 0,
    subspeciesGuess: null,
    unknownSpecies: true,
  }
}

// ------------------------------------------------------ rows for the sheet

const pad = (n: number) => String(n).padStart(2, '0')
export const formatMinutes = (m: number | null) => (m === null ? '' : `${Math.floor(m / 60)}:${pad(m % 60)}`)

export interface CaptureContext {
  date: string
  collector: string
  section: number | null
  /** Next CAM and tube for a preserved capture (the suggestions Colecta uses). */
  cam?: string | null
  tube?: string | null
  /** Preservation_medium of a preserved capture (Flash frozen unless said otherwise). */
  medium?: string | null
}

/**
 * Collection_data values for a capture, as the team fills monitoring rows (the
 * 21–23 Sep 2026 rows): a preserved butterfly is frozen alive that day, whole,
 * at Ikiam; a marked one is released, so its sample columns say NA or
 * NOT_COLLECTED. A capture without a species is To_identify, with no Identifier.
 */
export function captureValues(c: Capture, ctx: CaptureContext): Record<string, CellValue> {
  const marked = !!c.markId
  const date = isoToSerial(ctx.date)
  const values: Record<string, CellValue> = {
    Release_Collect: marked ? 'Mark_Released' : 'Collected_Preserved',
    FieldMark_ID: c.markId || 'NA',
    Insectary_ID: 'NA',
    CAM_ID_insectary: 'NA',
    Tube_2_id: 'NA',
    Tube_2_tissue: 'NOT_COLLECTED',
    Tube_3_id: 'NA',
    Tube_3_tissue: 'NOT_COLLECTED',
    Tube_4_id_LEGS: 'NA',
    Purpose: 'Monitoring',
    SPECIES: c.species,
    Subspecies_Form: c.subspecies,
    Identifier: c.species ? ctx.collector || null : null,
    ID_status: c.species ? 'COMPLETE' : 'To_identify',
    Sex: c.sex,
    Collection_location: 'Ikiam',
    Transect_section: ctx.section,
    Bait: 'NA',
    Forest_stratum: 'NA',
    Collection_date: date,
    Collection_time: c.minutes === null ? null : c.minutes / 1440,
    Collector: ctx.collector || null,
    Rainfall: c.rain || RAIN.DY,
    Cloud_cover: c.cloud,
    Flight_height: c.height,
    Splitted_body: 'No',
  }
  const at = (place: string) =>
    Object.fromEntries(
      ['Location_Head', 'Location_Torax', 'Location_abdomen', 'Location_Legs', 'Location_wings'].map(k => [k, place]),
    )
  if (marked)
    Object.assign(values, {
      CAM_ID: 'NA',
      Tube_1_id: 'NA',
      Tube_1_tissue: 'NOT_COLLECTED',
      Butterfly_weight: 'NA',
      Death_date: 'NA',
      Preservation_date: 'NA',
      Preservation_medium: 'NOT_COLLECTED',
      Preserved_dead_alive: 'NOT_PRESERVED',
      ...at('NA'),
    })
  else
    // The weight is taken later in the lab, so it stays empty.
    Object.assign(values, {
      CAM_ID: ctx.cam || null,
      Tube_1_id: ctx.tube || null,
      Tube_1_tissue: 'WHOLE_ORGANISM',
      Death_date: date,
      Preservation_date: date,
      Preservation_medium: ctx.medium || 'Flash frozen',
      Preserved_dead_alive: 'Alive',
      ...at('Ikiam'),
    })
  const initials = ctx.collector.split(' - ')[0]
  // A recapture needs no note: the repeated field mark already says it.
  const note = c.rest
  if (note) {
    const [y, m, d] = ctx.date.split('-').map(Number)
    values.Notes_Collection_data = `${d}/${m}/${y} ${initials}: ${note}`
  }
  for (const [k, v] of Object.entries(values)) if (v === null || v === '') delete values[k]
  return values
}

/**
 * ID_status and Identifier follow the species while a new row is edited: a
 * species typed in makes it COMPLETE (identified by the collector unless
 * someone else is written); emptied, it is To_identify again. Returns only the
 * cells that change.
 */
export function identificationFollows(values: Record<string, CellValue>): Record<string, CellValue> {
  const species = text(values.SPECIES)
  const status = text(values.ID_status)
  const out: Record<string, CellValue> = {}
  if (species && species !== 'NA') {
    if (!status || status === 'To_identify') out.ID_status = 'COMPLETE'
    if (!text(values.Identifier) && text(values.Collector)) out.Identifier = values.Collector
  } else if (!status || status === 'COMPLETE') out.ID_status = 'To_identify'
  return out
}

export interface ImportedCapture extends Capture {
  lat: number
  lon: number
  ele: number | null
  section: number | null
  sectionDistance: number
  photos: string[]
  /** The note had no time: it was read from where the GPS track passed the point. */
  timeFromTrack?: boolean
}

export function locateCapture(w: GpxWaypoint, taxa: Taxa, local?: Taxa): ImportedCapture {
  const c = parseCapture(w.text, taxa, local)
  const near = estimatedSection(w.lat, w.lon)
  if (c.minutes === null && w.time) c.minutes = localTime(w.time)?.minutes ?? null
  return {
    ...c,
    lat: w.lat,
    lon: w.lon,
    ele: w.ele,
    section: near.section,
    sectionDistance: near.distance,
    photos: w.photos || [],
  }
}

const metres = (a: [number, number], b: [number, number]) => {
  const lat = ((a[0] + b[0]) / 2) * (Math.PI / 180)
  return Math.hypot((b[1] - a[1]) * (Math.PI / 180) * 6371000 * Math.cos(lat), (b[0] - a[0]) * (Math.PI / 180) * 6371000)
}

/**
 * Points noted without a time get the time the GPS track passed closest to
 * them, between the times of the points before and after it (captures in walk
 * order). Only GPX files have track times; points more than 60 m from the
 * track are left without.
 */
export function timesFromTrack<T extends ImportedCapture>(captures: T[], track: TrackPoint[]): T[] {
  const timed = track.filter(p => p[3])
  if (!timed.length || !captures.some(c => c.minutes === null)) return captures
  // Ecuador has no summer time: one offset turns every GPS time into local minutes.
  const first = localTime(timed[0][3]!)
  const t0 = Date.parse(timed[0][3]!)
  if (!first) return captures
  const points = timed.map(p => ({
    at: [p[0], p[1]] as [number, number],
    minutes: first.minutes + Math.round((Date.parse(p[3]!) - t0) / 60000),
  }))
  return captures.map((c, i) => {
    if (c.minutes !== null) return c
    const before = captures.slice(0, i).reduce((m, o) => (o.minutes !== null && o.minutes > m ? o.minutes : m), -Infinity)
    const after = captures.slice(i + 1).reduce((m, o) => (o.minutes !== null && o.minutes < m ? o.minutes : m), Infinity)
    // The trail is walked out and back: the window between the neighbours picks the right pass.
    let inWindow: { minutes: number; distance: number } | null = null
    let anywhere: { minutes: number; distance: number } | null = null
    for (const p of points) {
      const distance = metres(p.at, [c.lat, c.lon])
      if (!anywhere || distance < anywhere.distance) anywhere = { minutes: p.minutes, distance }
      if (p.minutes >= before && p.minutes <= after && (!inWindow || distance < inWindow.distance))
        inWindow = { minutes: p.minutes, distance }
    }
    // No pass in the window (the GPS clock and the notes disagree): the nearest pass, kept between the neighbours.
    const best = inWindow && inWindow.distance <= 60 ? inWindow : anywhere
    if (!best || best.distance > 60) return c
    return { ...c, minutes: Math.min(Math.max(best.minutes, before), after), timeFromTrack: true }
  })
}

const plain = (s: string) => s.normalize('NFD').replace(/\p{M}/gu, '').toLowerCase()

/**
 * The collector named in a Wikiloc title or GPX author: the full name ("Franz
 * Chandi") or the initials ("Monitoreo ithomidos FCH 26 SEP 2026"). Initials
 * two people share (CR) say nothing.
 */
export function collectorFromName(name: string, collectors: string[]): string | null {
  const people = collectors.filter(c => / - /.test(c) && !/^NA\b/.test(c))
  const named = people.filter(c => {
    const full = plain(c.split(' - ').slice(1).join(' - ').trim())
    return full.length > 3 && plain(name).includes(full)
  })
  if (named.length === 1) return named[0]
  const initials = people.filter(c => new RegExp(`\\b${c.split(' - ')[0].trim()}\\b`).test(name))
  return initials.length === 1 ? initials[0] : null
}

/**
 * The people who walk the monitoring now (monitoring rows or SamplingDay_data
 * days in the last year of records, most first), then everyone else;
 * "NA - Missing data" is left out.
 */
export function monitoringCollectors(
  options: string[],
  rows: TableRow[],
  dayRows: TableRow[] = [],
): { usual: string[]; others: string[] } {
  const people = [...new Set(options)].filter(c => / - /.test(c) && !/^NA\b/.test(c))
  const latest = rows.reduce((m, r) => Math.max(m, dateOf(r) ?? 0), 0)
  const counts = new Map<string, number>()
  const count = (who: string) => counts.set(who, (counts.get(who) || 0) + 1)
  for (const r of rows) {
    const d = dateOf(r)
    const who = text(r.values.Collector)
    if (d !== null && d > latest - 365 && people.includes(who)) count(who)
  }
  // A day walked without captures still makes its collector one of the team.
  for (const key of monitoringDays(dayRows)) {
    const [d, initials] = key.split('|')
    const matches = people.filter(c => c.split(' - ')[0].trim().toUpperCase() === initials)
    if (Number(d) > latest - 365 && validDay(Number(d)) && matches.length === 1) count(matches[0])
  }
  const usual = [...counts].sort((a, b) => b[1] - a[1]).map(([c]) => c)
  return { usual, others: people.filter(c => !usual.includes(c)).sort((a, b) => a.localeCompare(b)) }
}

/** A collector's short name: the initials, or the full entry when two people share them (CR). */
export function collectorLabel(collector: string, collectors: string[]): string {
  const initials = collector.split(' - ')[0].trim()
  return collectors.filter(c => c.split(' - ')[0].trim() === initials).length > 1 ? collector : initials
}

// ------------------------------------------------------- marks and recaptures

/** Sex without the doubt mark ("female ?" → female); blanks and NOT_COLLECTED say nothing. */
export function sexOf(value: CellValue | undefined): 'female' | 'male' | null {
  const s = text(value)
    .toLowerCase()
    .replace(/[_\s]*\?$/, '')
    .trim()
  return s === 'female' || s === 'male' ? s : null
}
const binomial = (value: CellValue | undefined) => text(value).toLowerCase().split(/\s+/).slice(0, 2).join(' ')

interface MarkItem {
  key: string
  mark: string
  series: string | null
  number: number | null
  /** Lower-case binomial ('' when not identified). */
  species: string
  sex: 'female' | 'male' | null
  row: TableRow | null
}

export interface MarkRole {
  /** unsure: the capture has no species, so it cannot be told from the earlier butterfly. */
  role: 'new' | 'recapture' | 'reused' | 'unsure'
  /** Recapture: the first and the latest earlier row of the same butterfly. */
  first: TableRow | null
  of: TableRow | null
  /** Earlier rows of the mark that are other butterflies. */
  others: TableRow[]
  /** A new mark continuing the series being handed out (B58 after B57), although the number was used before. */
  continues: boolean
  /** Before this, the mark was already on two or more species: listed in Revisión de datos, not warned again. */
  known: boolean
}

/** Same butterfly: same species and sex (an unknown sex does not tell them apart). */
const sameAnimal = (a: MarkItem, b: MarkItem) => !!a.species && a.species === b.species && (!a.sex || !b.sex || a.sex === b.sex)

function markItem(
  key: string,
  mark: string,
  species: CellValue | undefined,
  sex: CellValue | undefined,
  row: TableRow | null,
): MarkItem {
  const m = /^([A-Z]+)(\d+)$/.exec(mark)
  return { key, mark, series: m?.[1] ?? null, number: m ? Number(m[2]) : null, species: binomial(species), sex: sexOf(sex), row }
}

/**
 * Marks handed out day by day. New marks follow a series (B55, B56, …); the
 * "head" of each series is the last number handed out. A mark above the head is
 * a new butterfly even if its number was used long ago; one at or below it is a
 * recapture when an earlier row with that mark is the same species and sex, and
 * otherwise an ID used twice. The numbering sometimes starts again (Aug 2026:
 * B40 although B40–B61 had been used in May–July): two or more consecutive
 * numbers on one day, most of them not recaptures, restart the series there.
 */
class MarkLedger {
  private heads = new Map<string, number>()
  private byMark = new Map<string, MarkItem[]>()

  /** One day's (or one walk's) marks, in time order; undated rows go without the series (`series` false). */
  day(items: MarkItem[], series = true): Map<string, MarkRole> {
    const roles = new Map<string, MarkRole>()
    const before = new Map(this.heads)
    for (const item of items) {
      const earlier = this.byMark.get(item.mark) || []
      const head = series && item.series ? before.get(item.series) : undefined
      const same = earlier.filter(e => sameAnimal(e, item))
      const others = earlier.filter(e => !same.includes(e)).flatMap(e => (e.row ? [e.row] : []))
      const known = new Set(earlier.map(e => e.species).filter(Boolean)).size >= 2
      const base = { first: null, of: null, others, continues: false, known }
      let role: MarkRole
      if (head !== undefined && item.number !== null && item.number > head)
        role = { ...base, role: 'new', others: earlier.flatMap(e => (e.row ? [e.row] : [])), continues: true }
      else if (same.length) role = { ...base, role: 'recapture', first: same[0].row, of: same.at(-1)!.row }
      else role = { ...base, role: !earlier.length ? 'new' : item.species ? 'reused' : 'unsure' }
      roles.set(item.key, role)
      this.byMark.set(item.mark, [...earlier, item])
    }
    if (series) this.advance(items, before, roles)
    return roles
  }

  private advance(items: MarkItem[], before: Map<string, number>, roles: Map<string, MarkRole>) {
    const bySeries = new Map<string, MarkItem[]>()
    for (const item of items)
      if (item.series && item.number !== null) bySeries.set(item.series, [...(bySeries.get(item.series) || []), item])
    for (const [series, list] of bySeries) {
      const numbers = [...new Set(list.map(i => i.number!))].sort((a, b) => a - b)
      const head = before.get(series)
      const above = head === undefined ? numbers : numbers.filter(n => n > head)
      if (above.length) {
        this.heads.set(series, above.at(-1)!)
        continue
      }
      const runs: number[][] = []
      for (const n of numbers) {
        const last = runs.at(-1)
        if (last && n === last.at(-1)! + 1) last.push(n)
        else runs.push([n])
      }
      const recaptured = (n: number) => list.some(i => i.number === n && roles.get(i.key)?.role === 'recapture')
      const restart = runs.filter(r => r.length >= 2 && r.filter(recaptured).length * 2 <= r.length).at(-1)
      if (!restart) continue
      this.heads.set(series, restart.at(-1)!)
      for (const i of list) {
        const r = roles.get(i.key)!
        if (restart.includes(i.number!))
          roles.set(i.key, {
            ...r,
            role: 'new',
            first: null,
            of: null,
            others: [...(r.of ? [r.of] : []), ...r.others],
            continues: true,
          })
      }
    }
  }
}

/** Every marked row's role, going through the days in order (optionally only days before a date serial). */
function ledgerOf(rows: TableRow[], before?: number) {
  const ledger = new MarkLedger()
  const roles = new Map<string, MarkRole>()
  const marked = rows.filter(r => hasMark(r) && (before === undefined || (dateOf(r) ?? Infinity) < before)).sort(byDate)
  const days = new Map<number | null, MarkItem[]>()
  for (const r of marked) {
    const d = dateOf(r)
    days.set(d, [...(days.get(d) || []), markItem(r.id, markOf(r), r.values.SPECIES, r.values.Sex, r)])
  }
  for (const [d, items] of days) for (const [k, v] of ledger.day(items, d !== null)) roles.set(k, v)
  return { ledger, roles }
}

/** The role of every marked row: new mark, recapture (same mark, species and sex) or an ID used twice. */
export function markRoles(rows: TableRow[]): Map<string, MarkRole> {
  return ledgerOf(rows).roles
}

/** The role of each capture of a walk, from the sheet's marks before the walk's day (null for unmarked captures). */
export function walkMarkRoles(rows: TableRow[], date: string, captures: Capture[]): (MarkRole | null)[] {
  if (!/^\d{4}-\d{2}-\d{2}$/.test(date)) return captures.map(() => null)
  const { ledger } = ledgerOf(rows, isoToSerial(date))
  const items = captures.map((c, i) => (c.markId ? markItem(`c${i}`, c.markId.toUpperCase(), c.species, c.sex, null) : null))
  const inOrder = items
    .map((item, i) => ({ item, minutes: captures[i].minutes ?? Infinity }))
    .filter((x): x is { item: MarkItem; minutes: number } => !!x.item)
    .sort((a, b) => a.minutes - b.minutes)
    .map(x => x.item)
  const roles = ledger.day(inOrder)
  return items.map(item => (item ? roles.get(item.key)! : null))
}

/** Rows per field mark (upper case). */
export function markIndex(rows: TableRow[]): Map<string, TableRow[]> {
  const out = new Map<string, TableRow[]>()
  for (const row of rows.filter(hasMark)) {
    const id = String(row.values.FieldMark_ID).trim().toUpperCase()
    out.set(id, [...(out.get(id) || []), row])
  }
  return out
}

/**
 * A message as its Spanish text (the key of the translations) and the values
 * that fill it. This file also runs on the server, without the interface
 * language, so it gives the phrase and the interface translates it (phraseText
 * with lib/i18n's t); `text` is the Spanish.
 */
export type PhraseValue = string | number | Phrase | Phrase[]
export interface Phrase {
  key: string
  vars?: Record<string, PhraseValue>
}
const phrase = (key: string, vars?: Record<string, PhraseValue>): Phrase => ({ key, vars })
/** A phrase as text; `translate` gives each key in the interface language (Spanish, the key itself, by default). */
export function phraseText(p: PhraseValue, translate: (key: string) => string = key => key): string {
  if (typeof p !== 'object') return String(p)
  if (Array.isArray(p)) return p.map(x => phraseText(x, translate)).join(', ')
  return translate(p.key).replace(/\{(\w+)\}/g, (all, k: string) =>
    p.vars && k in p.vars ? phraseText(p.vars[k], translate) : all,
  )
}

export interface CaptureCheck {
  text: string
  kind: 'ok' | 'info' | 'warn'
  /** The same message, to show in the interface language. */
  phrase?: Phrase
}
const check = (kind: CaptureCheck['kind'], key: string, vars?: Record<string, PhraseValue>): CaptureCheck => {
  const p = phrase(key, vars)
  return { kind, text: phraseText(p), phrase: p }
}

export interface ReviewContext {
  /** Collection_data rows. */
  rows: TableRow[]
  /** Day of the walk (ISO). */
  date: string
  /** Every capture of the walk, to spot a mark noted twice. */
  captures: Capture[]
  /** walkMarkRoles of the captures (computed when not given). */
  roles?: (MarkRole | null)[]
  /** Preserved individuals per species up to the walk (preservedForRule). */
  preserved: Map<string, number>
  isIthomiini: (species: string | null | undefined) => boolean
  /** The row already holding the capture, when the caller matched it another way (e.g. matchWalk). */
  existing?: TableRow | null
}

const who = (r: TableRow): Phrase => {
  const species = text(r.values.SPECIES) || phrase('sin especie')
  const sex = sexOf(r.values.Sex)
  return sex
    ? phrase('{species} {sex} (fila {n})', { species, sex: phrase(sex === 'female' ? 'hembra' : 'macho'), n: r.row })
    : phrase('{species} (fila {n})', { species, n: r.row })
}
let rolesCache: { rows: TableRow[]; date: string; captures: Capture[]; roles: (MarkRole | null)[] } | null = null

/**
 * What the review says about one capture: already in the sheet, new mark or
 * recapture, a mark used for another butterfly, the 30-preserved rule, and the
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
  if (existing) return { existing, recapture: null, list: [check('info', 'Ya está en la hoja (fila {n})', { n: existing.row })] }
  let recapture: TableRow | null = null
  if (c.markId) {
    let roles = ctx.roles
    if (!roles) {
      if (rolesCache?.rows !== ctx.rows || rolesCache.date !== ctx.date || rolesCache.captures !== ctx.captures)
        rolesCache = {
          rows: ctx.rows,
          date: ctx.date,
          captures: ctx.captures,
          roles: walkMarkRoles(ctx.rows, ctx.date, ctx.captures),
        }
      roles = rolesCache.roles
    }
    const role = roles[i]
    if (role?.role === 'recapture') {
      recapture = role.first
      const since = role.first?.values.Collection_date
      out.push(
        typeof since === 'number'
          ? check('ok', 'Recaptura de {mark} (marcada {date})', { mark: c.markId, date: formatSerial(since) })
          : check('ok', 'Recaptura de {mark}', { mark: c.markId }),
      )
    } else
      out.push(
        role?.continues && role.others.length
          ? check('ok', 'Nueva marca {mark} (sigue la serie)', { mark: c.markId })
          : check('ok', 'Nueva marca {mark}', { mark: c.markId }),
      )
    // An ID used twice before is already in Revisión de datos: warning on every import is noise.
    if (role?.role === 'reused' && !role.known)
      out.push(
        check('warn', '{mark} ya se usó para {rows}: ¿ID repetida?', { mark: c.markId, rows: role.others.slice(-2).map(who) }),
      )
    if (role?.role === 'unsure')
      out.push(
        check('warn', '{mark} era {rows}: identifica la especie para saber si es recaptura', {
          mark: c.markId,
          rows: role.others.slice(-1).map(who),
        }),
      )
    if (role?.role !== 'recapture' && c.recaptureNote)
      out.push(
        role?.others.length
          ? check('warn', 'Dice recaptura, pero {mark} era {row}', { mark: c.markId, row: who(role.others.at(-1)!) })
          : check('warn', 'Dice recaptura, pero {mark} no está en la hoja', { mark: c.markId }),
      )
    if (ctx.captures.some((o, j) => j !== i && o.markId === c.markId))
      out.push(check('warn', '{mark} aparece dos veces en este recorrido', { mark: c.markId }))
  } else {
    out.push(check('info', 'Preservado (sin marca)'))
    const preserved = c.species ? ctx.preserved.get(c.species) || 0 : 0
    if (preserved >= MARK_THRESHOLD && ctx.isIthomiini(c.species))
      out.push(check('warn', '{species} ya tiene {n} preservados: ¿no debía marcarse?', { species: c.species!, n: preserved }))
  }
  if (!c.species) out.push(check('warn', 'Sin especie'))
  else if (!c.known) out.push(check('warn', 'Nombre no encontrado en la hoja: revisar'))
  if (c.subspeciesGuess === 'ikiam') out.push(check('info', 'Subespecie {name}: la única en Ikiam', { name: c.subspecies ?? '' }))
  if (!c.sex) out.push(check('warn', 'Sin sexo'))
  if (c.timeFromTrack) out.push(check('warn', 'Hora del GPS ({time}): la nota no la dice', { time: formatMinutes(c.minutes) }))
  else if (c.minutes === null) out.push(check('warn', 'Sin hora'))
  if (c.height === null) out.push(check('warn', 'Sin altura'))
  if (!c.cloud) out.push(check('warn', 'Sin clima'))
  if (c.section === null) out.push(check('warn', 'Lejos del sendero ({n} m)', { n: String(c.sectionDistance) }))
  if (c.rest) out.push(check('info', 'A notas: “{note}”', { note: c.rest }))
  return { existing, recapture, list: out }
}

// ------------------------------------------------------- SamplingDay_data

/** A plausible date serial (2000–2100); 375004 (typed for 21/9/2026) is not. */
const validDay = (v: CellValue | undefined) => typeof v === 'number' && v >= 36526 && v <= 73051

/**
 * The SamplingDay_data row of a collector's walk: the row with that date and
 * initials; otherwise a row of the same collector whose Date is broken (e.g.
 * 375004) but whose note starts with the walk's day ("21/9/2026 AA: …") or
 * whose start and end times are within 10 minutes of the GPS. Such a row is
 * returned as `broken`, to flag rather than add the day twice.
 */
export function samplingDayRow(
  dayRows: TableRow[],
  date: string,
  initials: string,
  span?: { start: number; end: number } | null,
): { row: TableRow; broken: boolean } | null {
  const serial = isoToSerial(date)
  const mine = dayRows.filter(r => r.observed && text(r.values.Collectors_initials).toUpperCase() === initials.toUpperCase())
  const exact = mine.find(r => r.values.Date === serial)
  if (exact) return { row: exact, broken: false }
  const near = (v: CellValue | undefined, m: number) => typeof v === 'number' && Math.abs(v * 1440 - m) <= 10
  const broken = mine.find(
    r =>
      !validDay(r.values.Date) &&
      (noteDate(text(r.values.Notes)) === date ||
        (!!span && near(r.values.Start_time, span.start) && near(r.values.End_time, span.end))),
  )
  return broken ? { row: broken, broken: true } : null
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
 * marking started (e.g. Oleria gunilla, 31 in July 2024). Mariposario Ikiam (a
 * wild butterfly found in the campus butterfly garden) counts too, as in the
 * alerts (server/alerts.mjs RULE_LOCATIONS).
 */
export const RULE_LOCATIONS = ['Ikiam', 'Casa de Lin', 'Mariposario Ikiam']
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
 * The butterflies behind the marked rows: a recapture joins the butterfly it
 * recaptures (same mark, species and sex, see MarkLedger); every other row is a
 * butterfly of its own. Rows per mark, oldest first, each with its butterfly's number.
 */
function individuals(rows: TableRow[]) {
  const roles = markRoles(rows)
  const of = new Map<string, number>()
  const groups = new Map<string, { row: TableRow; individual: number }[]>()
  let next = 0
  for (const row of rows.filter(hasMark).sort(byDate)) {
    const role = roles.get(row.id)
    const individual = role?.role === 'recapture' && role.of && of.has(role.of.id) ? of.get(role.of.id)! : next++
    of.set(row.id, individual)
    groups.set(markOf(row), [...(groups.get(markOf(row)) || []), { row, individual }])
  }
  return groups
}

/**
 * Recaptures: rows whose field mark was seen before on the same species and
 * sex. The same mark on another butterfly is a reused ID, not a recapture (see
 * markConflicts); a mark continuing the series is new even if its number was
 * used long ago.
 */
export function recaptureIds(rows: TableRow[]): Set<string> {
  const out = new Set<string>()
  for (const [id, role] of markRoles(rows)) if (role.role === 'recapture') out.add(id)
  return out
}

/** Field marks on more than one butterfly (another species or sex): an ID given twice, or a wrong species. */
export function markConflicts(rows: TableRow[]): { id: string; rows: TableRow[] }[] {
  const out: { id: string; rows: TableRow[] }[] = []
  for (const [id, list] of individuals(rows))
    if (new Set(list.map(x => x.individual)).size > 1) out.push({ id, rows: list.map(x => x.row) })
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

/** Every marked individual seen more than once (same mark, species and sex), with its captures. */
export function markHistories(rows: TableRow[]): MarkHistory[] {
  const out: MarkHistory[] = []
  for (const [id, list] of individuals(rows)) {
    const byIndividual = new Map<number, TableRow[]>()
    for (const x of list) byIndividual.set(x.individual, [...(byIndividual.get(x.individual) || []), x.row])
    for (const same of byIndividual.values()) {
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

// ------------------------------------------- pairing walk points with sheet rows

/**
 * How a walk point was paired with its sheet row: by its field mark; `sure`,
 * the only best row by minute, species and sex; `tie`, another row (or point)
 * fits exactly as well; `order`, placed between its neighbours because the
 * note has no time (or nothing else fits); `none`, no row.
 */
export type MatchConfidence = 'mark' | 'sure' | 'tie' | 'order' | 'none'
/** Where the note and its row disagree. */
export type MatchConflict = 'sexo' | 'especie' | 'marca' | 'hora'

export interface PointMatch {
  /** Its rows: a note of several butterflies ("Mariposa 1 y 2") has several. */
  rows: TableRow[]
  confidence: MatchConfidence
  /** Rows it could also be: those that fit as well (tie), or the free rows between its neighbours (order, none). */
  candidates: TableRow[]
  conflicts: MatchConflict[]
  /** Chosen by a person: matching never changes it. */
  manual: boolean
}

export interface MatchOptions {
  /** Links chosen by a person, by capture index: record ids, or null for "No es ninguna". */
  fixed?: Map<number, string[] | null>
  /** Also try the day before and after when the title's date is off (default true). */
  shift?: boolean
}

/**
 * Minutes a note's time may differ from its row's. Measured on the 218 marked
 * points with a time on the map (Sep 2026): 210 differ by 0 minutes, one by 2,
 * two by 3 (a mark written on the next row of the sheet) and five by 10–70
 * (a wrong hour, found only by the mark). Rows are typed from the notes, so a
 * wider window would mostly catch the wrong butterfly: beyond 2 minutes a point
 * is only placed by its order in the walk, and listed as a doubt.
 */
export const TIME_TOLERANCE = 2
/** A pairing that costs this much is worse than no row (species and sex both disagree, say). */
const UNPAIRED = 7
/** Order within equal costs: rows in time order follow the points in walk order (a tiebreak only). */
const ORDER_WEIGHT = 1e-4
const BIG = 1e9

const minuteOfRow = (row: TableRow) =>
  typeof row.values.Collection_time === 'number' ? Math.round(row.values.Collection_time * 1440) : null

interface RowFacts {
  row: TableRow
  minute: number | null
  mark: string | null
  species: string
  sex: 'female' | 'male' | null
  rank: number
}
interface PointFacts {
  index: number
  /** Time written in the note; a time read from the GPS track is only `approx`. */
  minute: number | null
  approx: number | null
  mark: string | null
  /** Its mark is on another row of the day (a slip in the note), so a row with another mark may still be it. */
  markElsewhere: boolean
  species: string
  sex: 'female' | 'male' | null
  count: number
  rank: number
}

function rowFacts(rows: TableRow[]): RowFacts[] {
  return [...rows]
    .sort((a, b) => (minuteOfRow(a) ?? 1e6) - (minuteOfRow(b) ?? 1e6) || a.row - b.row)
    .map((row, rank) => ({
      row,
      minute: minuteOfRow(row),
      mark: hasMark(row) ? baseMark(markOf(row)) : null,
      species: binomial(row.values.SPECIES),
      sex: sexOf(row.values.Sex),
      rank,
    }))
}

/**
 * The walk's order: by the point numbers when most points have one ("M3",
 * "4 4 50cm"), otherwise as listed. Points with a time are ranked by it.
 */
function walkOrder(captures: Capture[]): number[] {
  const numbered = captures.filter(c => c.seq !== null).length * 2 >= captures.length
  const order = captures.map((_, i) => i)
  return numbered ? order.sort((a, b) => (captures[a].seq ?? 1e6) - (captures[b].seq ?? 1e6) || a - b) : order
}

/** What a pairing costs (minutes apart plus disagreements), or null when it cannot be. */
function pairCost(p: PointFacts, r: RowFacts): { cost: number; conflicts: MatchConflict[] } | null {
  if (p.minute === null || r.minute === null) return null
  const d = Math.abs(p.minute - r.minute)
  if (d > TIME_TOLERANCE) return null
  let cost = d
  const conflicts: MatchConflict[] = []
  if (p.species && r.species && p.species !== r.species) {
    cost += 4
    conflicts.push('especie')
  }
  if (p.sex && r.sex && p.sex !== r.sex) {
    cost += 3
    conflicts.push('sexo')
  }
  if (p.mark && r.mark && p.mark !== r.mark) {
    // Two marks are two butterflies (a recapture not entered, next to a new mark), unless the note's mark is taken.
    if (!p.markElsewhere && !sameNumber(p.mark, r.mark)) return null
    cost += 4
    conflicts.push('marca')
  } else if (p.mark && !r.mark) {
    cost += 1
    conflicts.push('marca')
  } else if (!p.mark && r.mark) cost += 0.5 // a marked row usually has its mark in the note
  return cost < UNPAIRED ? { cost, conflicts } : null
}

/** A mark without the suffix a row may add when it was given twice ("A13.1", "A13.2" → A13). */
const baseMark = (mark: string) => /^[A-Z]+\d+/.exec(mark)?.[0] ?? mark
/** The same number with another letter (MJS's "a66" for the sheet's M66): a slip, not another butterfly. */
const sameNumber = (a: string, b: string) => /\d+/.exec(a)?.[0] === /\d+/.exec(b)?.[0]

/** Whether a point placed by order can be this row: species, sex and mark do not disagree (a mark taken by another row may be a slip: listed as a doubt). */
function fits(p: PointFacts, r: RowFacts) {
  return (
    !(p.species && r.species && p.species !== r.species) &&
    !(p.sex && r.sex && p.sex !== r.sex) &&
    !(p.mark && r.mark && p.mark !== r.mark && !p.markElsewhere && !sameNumber(p.mark, r.mark))
  )
}
const markConflict = (p: PointFacts, r: RowFacts): MatchConflict[] => (p.mark && p.mark !== r.mark ? ['marca'] : [])

/** Minimum-cost assignment of each row of `cost` to a distinct column (Hungarian method; rows ≤ columns). */
function hungarian(cost: number[][]): number[] {
  const n = cost.length
  const m = cost[0]?.length ?? 0
  const u = new Array(n + 1).fill(0)
  const v = new Array(m + 1).fill(0)
  const p = new Array(m + 1).fill(0)
  const way = new Array(m + 1).fill(0)
  for (let i = 1; i <= n; i++) {
    p[0] = i
    let j0 = 0
    const minv = new Array(m + 1).fill(Infinity)
    const used = new Array(m + 1).fill(false)
    do {
      used[j0] = true
      const i0 = p[j0]
      let delta = Infinity
      let j1 = 0
      for (let j = 1; j <= m; j++)
        if (!used[j]) {
          const cur = cost[i0 - 1][j - 1] - u[i0] - v[j]
          if (cur < minv[j]) {
            minv[j] = cur
            way[j] = j0
          }
          if (minv[j] < delta) {
            delta = minv[j]
            j1 = j
          }
        }
      for (let j = 0; j <= m; j++)
        if (used[j]) {
          u[p[j]] += delta
          v[j] -= delta
        } else minv[j] -= delta
      j0 = j1
    } while (p[j0] !== 0)
    do {
      const j1 = way[j0]
      p[j0] = p[j1]
      j0 = j1
    } while (j0)
  }
  const out = new Array(n).fill(-1)
  for (let j = 1; j <= m; j++) if (p[j]) out[p[j] - 1] = j - 1
  return out
}

interface Slot {
  point: PointFacts
  row: RowFacts | null
  confidence: MatchConfidence
  conflicts: MatchConflict[]
  candidates: RowFacts[]
}

/**
 * Timed points and free rows within the time tolerance: the assignment that
 * pairs the most points at the least cost (minutes apart, species, sex, mark),
 * solved per group of points and rows that could be exchanged. A point is a
 * tie when another assignment of the same cost gives it another row or none.
 */
function assignTimed(slots: Slot[], rows: RowFacts[], taken: Set<string>) {
  const open = slots.filter(s => !s.row && s.point.minute !== null)
  const free = rows.filter(r => !taken.has(r.row.id) && r.minute !== null)
  const edges = open.map(s => free.map(r => pairCost(s.point, r)))
  // Groups of slots and rows linked by possible pairings (union-find over slots, then rows).
  const parent = [...open.map((_, i) => i), ...free.map((_, j) => open.length + j)]
  const find = (x: number): number => (parent[x] === x ? x : (parent[x] = find(parent[x])))
  edges.forEach((list, i) => list.forEach((e, j) => e && (parent[find(i)] = find(open.length + j))))
  const groups = new Map<number, { slots: number[]; rows: number[] }>()
  open.forEach((_, i) => {
    if (!edges[i].some(Boolean)) return
    const g = groups.get(find(i)) || { slots: [], rows: [] }
    g.slots.push(i)
    groups.set(find(i), g)
  })
  free.forEach((_, j) => groups.get(find(open.length + j))?.rows.push(j))
  for (const g of groups.values()) {
    const solve = (forbid: (slot: number, row: number) => boolean, force: number | null = null) => {
      const matrix = g.slots.map(i => [
        ...g.rows.map(j => {
          const e = edges[i][j]
          if (!e || forbid(i, j)) return BIG
          return e.cost + ORDER_WEIGHT * Math.abs(open[i].point.rank - free[j].rank)
        }),
        ...g.slots.map(() => (i === force ? BIG : UNPAIRED)),
      ])
      const pick = hungarian(matrix)
      let real = 0
      pick.forEach((col, k) => {
        const e = col < g.rows.length ? edges[g.slots[k]][g.rows[col]] : null
        real += matrix[k][col] >= BIG ? BIG : e ? e.cost : UNPAIRED
      })
      return { pick: pick.map(col => (col < g.rows.length ? g.rows[col] : -1)), real }
    }
    const best = solve(() => false)
    const samePoint = (a: number, b: number) => open[a].point === open[b].point
    g.slots.forEach((i, k) => {
      const j = best.pick[k]
      const slot = open[i]
      const alternatives = new Set<number>()
      // Another assignment as good without this pairing (a slot of the same point taking it is not another reading).
      if (j >= 0) {
        const alt = solve((s, r) => samePoint(s, i) && r === j)
        if (alt.real <= best.real + 1e-6) alt.pick.forEach((r, kk) => samePoint(g.slots[kk], i) && r >= 0 && alternatives.add(r))
        if (alt.real <= best.real + 1e-6 && !alternatives.size) alternatives.add(-1)
      } else {
        const alt = solve(() => false, i)
        if (alt.real <= best.real + 1e-6) alt.pick.forEach((r, kk) => g.slots[kk] === i && r >= 0 && alternatives.add(r))
      }
      // Rows it could as well be: within the minutes, species and sex not disagreeing.
      const candidates = g.rows
        .filter(r => edges[i][r] && !edges[i][r]!.conflicts.some(c => c === 'especie' || c === 'sexo'))
        .sort((a, b) => edges[i][a]!.cost - edges[i][b]!.cost || free[a].rank - free[b].rank)
        .map(r => free[r])
      if (j >= 0) {
        slot.row = free[j]
        slot.conflicts = edges[i][j]!.conflicts
        slot.confidence = alternatives.size ? 'tie' : 'sure'
        taken.add(free[j].row.id)
      } else slot.confidence = alternatives.size ? 'tie' : 'none'
      slot.candidates = slot.confidence === 'tie' ? candidates : []
    })
  }
}

/**
 * Points without a time (or whose note has none that fits), in walk order
 * between two placed neighbours: paired in order with the free rows between
 * their neighbours' rows, skipping rows whose species, sex or mark disagree.
 */
function placeByOrder(slots: Slot[], order: number[], rows: RowFacts[], taken: Set<string>) {
  const byPoint = new Map<number, Slot[]>()
  for (const s of slots) byPoint.set(s.point.index, [...(byPoint.get(s.point.index) || []), s])
  /** A point's place in time: its row's minute, else its note's; null for points without either. */
  const anchor = (i: number) => {
    const own = byPoint.get(i) || []
    const times = own.map(s => s.row?.minute).filter((m): m is number => m !== null && m !== undefined)
    if (times.length) return Math.min(...times)
    return own[0]?.point.minute ?? null
  }
  let q = 0
  while (q < order.length) {
    if (anchor(order[q]) !== null || !byPoint.has(order[q])) {
      q++
      continue
    }
    // A run of points without a time, between the nearest neighbours that have one.
    let end = q
    while (end + 1 < order.length && anchor(order[end + 1]) === null) end++
    let lo = -Infinity
    for (let b = q - 1; b >= 0; b--) {
      const t = anchor(order[b])
      if (t !== null) {
        lo = t
        break
      }
    }
    let hi = Infinity
    for (let a = end + 1; a < order.length; a++) {
      const t = anchor(order[a])
      if (t !== null) {
        hi = t
        break
      }
    }
    if (lo > hi) [lo, hi] = [hi, lo]
    const window = rows.filter(r => !taken.has(r.row.id) && (r.minute === null || (r.minute >= lo && r.minute <= hi)))
    const run = order.slice(q, end + 1).flatMap(i => (byPoint.get(i) || []).filter(s => !s.row && s.confidence !== 'tie'))
    // Alignment of the run (in walk order) with the window (in time order): most pairs, then fewest penalties.
    const n = run.length
    const m = window.length
    const score = (pairs: number, penalty: number) => pairs * 1000 - penalty
    const dp = Array.from({ length: n + 1 }, () => new Array(m + 1).fill(0))
    const cost = (s: Slot, r: RowFacts) =>
      (r.mark && !s.point.mark ? 0.5 : 0) +
      (s.point.mark && r.mark !== s.point.mark ? 4 : 0) +
      (s.point.approx !== null && r.minute !== null ? Math.abs(s.point.approx - r.minute) / 100 : 0)
    for (let i = 1; i <= n; i++)
      for (let j = 1; j <= m; j++) {
        dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1])
        if (fits(run[i - 1].point, window[j - 1]))
          dp[i][j] = Math.max(dp[i][j], dp[i - 1][j - 1] + score(1, cost(run[i - 1], window[j - 1])))
      }
    let [i, j] = [n, m]
    while (i > 0 && j > 0) {
      if (dp[i][j] === dp[i - 1][j]) i--
      else if (dp[i][j] === dp[i][j - 1]) j--
      else {
        const s = run[i - 1]
        s.row = window[j - 1]
        s.confidence = 'order'
        s.conflicts = markConflict(s.point, s.row)
        taken.add(s.row.row.id)
        i--
        j--
      }
    }
    for (const s of run) s.candidates = window.slice(0, 12)
    q = end + 1
  }
}

function matchDay<T extends Capture>(rows: TableRow[], captures: T[], fixed: Map<number, string[] | null>) {
  const facts = rowFacts(rows)
  const byId = new Map(facts.map(r => [r.row.id, r]))
  const order = walkOrder(captures)
  const rankOf = new Map(
    captures
      .map((c, i) => ({ i, t: (c as { timeFromTrack?: boolean }).timeFromTrack ? null : c.minutes }))
      .filter(x => x.t !== null)
      .sort((a, b) => a.t! - b.t! || order.indexOf(a.i) - order.indexOf(b.i))
      .map((x, rank) => [x.i, rank]),
  )
  const slots: Slot[] = []
  const manual = new Map<number, RowFacts[]>()
  const taken = new Set<string>()
  captures.forEach((c, index) => {
    const fromTrack = !!(c as { timeFromTrack?: boolean }).timeFromTrack
    const point: PointFacts = {
      index,
      minute: fromTrack ? null : c.minutes,
      approx: fromTrack ? c.minutes : null,
      mark: c.markId ? baseMark(c.markId.toUpperCase()) : null,
      markElsewhere: !!c.markId && facts.some(r => r.mark === baseMark(c.markId!.toUpperCase())),
      species: c.species && !c.unknownSpecies ? binomial(c.species) : '',
      sex: c.sex,
      count: Math.max(1, c.count || 1),
      rank: rankOf.get(index) ?? 0,
    }
    if (fixed.has(index)) {
      const chosen = (fixed.get(index) || []).map(id => byId.get(id)).filter((r): r is RowFacts => !!r)
      chosen.forEach(r => taken.add(r.row.id))
      manual.set(index, chosen)
      // Its rows still place the points around it.
      for (const r of chosen) slots.push({ point, row: r, confidence: 'sure', conflicts: [], candidates: [] })
      return
    }
    for (let k = 0; k < point.count; k++) slots.push({ point, row: null, confidence: 'none', conflicts: [], candidates: [] })
  })
  // 1. Field marks: the day's row with the point's mark, whatever its minute (a wrong hour is common), unless the species disagrees.
  for (const i of order) {
    const slot = slots.find(s => s.point.index === i && !s.row)
    const p = slot?.point
    if (!slot || !p?.mark) continue
    const options = facts.filter(
      r => !taken.has(r.row.id) && r.mark === p.mark && !(p.species && r.species && p.species !== r.species),
    )
    const delta = (r: RowFacts) => (p.minute !== null && r.minute !== null ? Math.abs(p.minute - r.minute) : 1e6)
    const r = options.sort((a, b) => delta(a) - delta(b))[0]
    if (!r) continue
    slot.row = r
    slot.confidence = 'mark'
    slot.conflicts = [
      ...(p.sex && r.sex && p.sex !== r.sex ? ['sexo' as const] : []),
      ...(p.minute !== null && r.minute !== null && Math.abs(p.minute - r.minute) > TIME_TOLERANCE ? ['hora' as const] : []),
    ]
    taken.add(r.row.id)
  }
  // 2. Points with a time: the best assignment by minute, species, sex and mark.
  assignTimed(slots, facts, taken)
  // 3. Points without a time, placed in walk order between their neighbours.
  placeByOrder(slots, order, facts, taken)
  // 4. As many points left as rows (short old notes such as "Marip 3"): all of them in order.
  const leftSlots = order.flatMap(i => slots.filter(s => s.point.index === i && !s.row && s.confidence !== 'tie'))
  const leftRows = facts.filter(r => !taken.has(r.row.id))
  if (leftSlots.length && leftSlots.length === leftRows.length && leftSlots.every((s, k) => fits(s.point, leftRows[k])))
    leftSlots.forEach((s, k) => {
      s.row = leftRows[k]
      s.confidence = 'order'
      s.conflicts = markConflict(s.point, s.row)
      s.candidates = leftRows
      taken.add(s.row.row.id)
    })
  const weakest: MatchConfidence[] = ['none', 'tie', 'order', 'sure', 'mark']
  return captures.map((_, index): PointMatch => {
    if (manual.has(index)) {
      const chosen = manual.get(index)!
      return {
        rows: chosen.map(r => r.row),
        confidence: chosen.length ? 'sure' : 'none',
        candidates: [],
        conflicts: [],
        manual: true,
      }
    }
    const own = slots.filter(s => s.point.index === index)
    const placed = own.filter(s => s.row)
    const confidence = placed.length
      ? weakest[Math.min(...placed.map(s => weakest.indexOf(s.confidence)))]
      : own.some(s => s.confidence === 'tie')
        ? 'tie'
        : 'none'
    const candidates = [...new Set(own.flatMap(s => s.candidates))].filter(r => !placed.some(s => s.row === r))
    return {
      rows: placed.map(s => s.row!.row),
      confidence,
      candidates: candidates.map(r => r.row),
      conflicts: [...new Set(placed.flatMap(s => s.conflicts))],
      manual: false,
    }
  })
}

const pairedByContent = (matches: PointMatch[]) =>
  matches.filter(m => m.confidence === 'mark' || m.confidence === 'sure' || m.confidence === 'tie').length

/**
 * Pairs a walk's points with the collector's rows of that day. Marks first;
 * then the assignment of timed points to rows (±2 minutes) with the least
 * total difference that respects species and sex; then points without a time,
 * in walk order between their neighbours; and when the notes are too short
 * ("Marip 3"), all in order if the butterflies and rows left are as many.
 * Each point says how sure its pairing is (see MatchConfidence). A title one
 * day off is tolerated when the points match the next or previous day by content.
 * Used by Importar recorrido, the server (stored walks) and the assistant.
 */
export function matchWalk<T extends Capture>(
  rows: TableRow[],
  date: string,
  collector: string,
  captures: T[],
  options: MatchOptions = {},
) {
  const attempt = (serial: number) => {
    // Without a collector (an old GPX), every row of the day.
    const day = rows.filter(
      r => dateOf(r) === serial && (!collector || dayKey(serial, text(r.values.Collector)) === dayKey(serial, collector)),
    )
    const matches = matchDay(day, captures, options.fixed || new Map())
    return {
      date: serialToIso(serial),
      matches,
      pairs: captures.flatMap((capture, i) => matches[i].rows.map(row => ({ capture, row }))),
      left: captures.filter((_, i) => !matches[i].rows.length),
      ordered: matches.some(m => m.confidence === 'order'),
    }
  }
  const base = isoToSerial(date)
  let best = attempt(base)
  if (options.shift === false) return best
  for (const shift of [-1, 1]) {
    if (!best.left.length) break
    const other = attempt(base + shift)
    // Another day only when more of its points match by content (not just by order).
    if (pairedByContent(other.matches) > pairedByContent(best.matches)) best = other
  }
  return best
}

/** What a stored capture keeps (see server/monitoring.mjs). */
export interface StoredPoint {
  text: string
  lat: number
  lon: number
  minutes: number | null
}

/**
 * The points of a stored walk, read again from their notes (the stored species,
 * sex and mark are those of the row it was paired with). A time the note does
 * not have came from the GPS track. A note of several butterflies was stored
 * once per row ("Mariposa 1 y 2"): those copies are one point again, and
 * `groups` gives the stored captures of each point.
 */
export function storedPoints(captures: StoredPoint[], taxa: Taxa, local?: Taxa) {
  const groups = new Map<string, number[]>()
  captures.forEach((c, i) => {
    const key = `${c.text}|${c.lat}|${c.lon}`
    groups.set(key, [...(groups.get(key) || []), i])
  })
  const points = [...groups.values()].map(indexes => {
    const c = captures[indexes[0]]
    const p = parseCapture(c.text, taxa, local)
    return {
      ...p,
      minutes: p.minutes ?? c.minutes,
      timeFromTrack: p.minutes === null && c.minutes !== null,
      count: indexes.length,
    }
  })
  return { points, groups: [...groups.values()] }
}

/** Doubtful pairings, to be looked at with the photos: a tie, placed by order, or a note that disagrees with its row. */
export const doubtfulMatch = (m: PointMatch) =>
  !m.manual && (m.confidence === 'tie' || m.confidence === 'order' || m.conflicts.some(c => c !== 'hora'))

/**
 * Whether a single capture is already in the sheet: the day's row with its
 * mark, or the closest by minute (±2) whose species and sex agree. A point
 * noted without a species (identified later from its photo) matches on the
 * minute, and on the sex if both have one. A whole walk is paired with matchWalk.
 */
export function existingRow(rows: TableRow[], date: string, c: Capture): TableRow | null {
  const serial = isoToSerial(date)
  const day = rowFacts(rows.filter(row => dateOf(row) === serial))
  const p: PointFacts = {
    index: 0,
    minute: c.minutes,
    approx: null,
    mark: c.markId ? baseMark(c.markId.toUpperCase()) : null,
    markElsewhere: false,
    species: c.species && !c.unknownSpecies ? binomial(c.species) : '',
    sex: c.sex,
    count: 1,
    rank: 0,
  }
  if (p.mark) {
    const marked = day.filter(r => r.mark === p.mark && !(p.species && r.species && p.species !== r.species))
    const delta = (r: RowFacts) => (p.minute !== null && r.minute !== null ? Math.abs(p.minute - r.minute) : 1e6)
    if (marked.length) return marked.sort((a, b) => delta(a) - delta(b))[0].row
  }
  const fitting = day
    .map(r => ({ r, e: pairCost(p, r) }))
    .filter(x => x.e && !x.e.conflicts.some(k => k !== 'marca'))
    .sort((a, b) => a.e!.cost - b.e!.cost || a.r.rank - b.r.rank)
  return fitting[0]?.r.row ?? null
}

/**
 * For the map, a capture that is already a sheet row takes the row's curated
 * species, subspecies, sex, mark and section (the note may lack them).
 */
export function withSheetValues<T extends Capture & { section: number | null }>(
  c: T,
  row: TableRow | null,
): T & { row?: number; recordId?: string } {
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
    recordId: row.id,
  }
}

/**
 * A walk's points as they are stored on the map. A point paired by its mark or
 * surely takes its rows (one copy per row). A doubtful one (a tie, placed only
 * by order, a note that disagrees with its row) is stored without a row, so the
 * map and Recapturas show nothing wrong, and flagged `doubt`: Dudas and
 * Revisión de datos list it until a person pairs it. A point without any row
 * is flagged too when `unpaired` (Pasar al mapa adds no rows); an import that
 * adds its row leaves it plain, linked once the row is saved. A note of several
 * butterflies keeps one copy per butterfly.
 */
export function capturesToStore<T extends Capture & { section: number | null }>(
  captures: T[],
  matches: PointMatch[],
  { unpaired = true }: { unpaired?: boolean } = {},
): (T & { row?: number; recordId?: string; doubt?: boolean })[] {
  return captures.flatMap((c, i) => {
    const m = matches[i]
    if (m.rows.length && !doubtfulMatch(m)) return m.rows.map(row => withSheetValues(c, row))
    if (!m.rows.length && !unpaired) return [c]
    return Array.from({ length: Math.max(1, c.count || 1, m.rows.length) }, () => ({ ...c, doubt: true }))
  })
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

/** A new Mark_Released row for a recapture found in notes (or only as a Wikiloc point: `note` empty), copying the marked individual. */
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
    // Without a note: a recapture only its Wikiloc point holds (its walk's pairing board).
    Notes_Collection_data: r.note
      ? `${d}/${m}/${y} ${r.initials || ''}: Recapture, moved from the note of row ${r.row.row}`
      : `${d}/${m}/${y} ${r.initials || ''}: Recapture of the butterfly marked in row ${r.row.row}, from its Wikiloc point`,
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
