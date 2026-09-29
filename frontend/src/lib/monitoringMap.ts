import { isoToSerial } from './dates'
import { hasMark, noteRecaptures, sexOf as sexOfValue, type MarkHistory } from './monitoring'
import { distance } from './transects'
import type { TableRow } from './types'

/** A capture point stored with a walk on the map (the fields the map needs). */
export interface MapCapture {
  lat: number
  lon: number
  species: string | null
  subspecies: string | null
  sex: string | null
  minutes: number | null
  markId: string | null
  section: number | null
  photos?: string[]
  recapture: boolean
  row?: number | null
  /** Stored without a row because its pairing was doubtful (not shown until paired). */
  doubt?: boolean
}

/** A walk on the map: its date, collector and captures. */
export interface MapWalk {
  id: string
  date: string
  collector: string | null
  captures: MapCapture[]
}

/** One capture of a point on the map, with the walk it belongs to. */
export interface MapPoint<W extends MapWalk = MapWalk> {
  walk: W
  capture: W['captures'][number]
}

export const collectorOf = (walk: { collector: string | null }) => (walk.collector || '').split(' - ')[0].trim()

/** What a capture was: preserved, marked and released, or a recapture. */
export type PointKind = 'preserved' | 'marked' | 'recapture'
export const kindOfCapture = (c: MapCapture): PointKind => (c.recapture ? 'recapture' : c.markId ? 'marked' : 'preserved')

/** Map filters; an empty list means "all". */
export interface MapFilters {
  years: string[]
  dates: string[]
  collectors: string[]
  species: string[]
  sections: string[]
  sexes: string[]
  kinds: string[]
  /** One marked individual, "B39|Mechanitis messenoides". */
  individual: string
}

export type FacetKey = Exclude<keyof MapFilters, 'individual'>

export const individualKey = (markId: string, species: string) => `${markId.toUpperCase()}|${species}`

export const sexOf = (c: MapCapture) => (c.sex === 'female' || c.sex === 'male' ? c.sex : 'unknown')
export const sectionOf = (c: MapCapture) => (c.section ? String(c.section) : 'none')

/** The value of one filter for a point. */
function facetValue(key: FacetKey, p: MapPoint): string {
  switch (key) {
    case 'years':
      return p.walk.date.slice(0, 4)
    case 'dates':
      // A day, not a walk: choosing a date shows every walk of that day.
      return p.walk.date
    case 'collectors':
      return collectorOf(p.walk)
    case 'species':
      return p.capture.species || ''
    case 'sections':
      return sectionOf(p.capture)
    case 'sexes':
      return sexOf(p.capture)
    case 'kinds':
      return kindOfCapture(p.capture)
  }
}

const FACETS: FacetKey[] = ['years', 'dates', 'collectors', 'species', 'sections', 'sexes', 'kinds']

/** Whether a point passes every filter, ignoring the one being counted. */
export function passes(p: MapPoint, f: MapFilters, except?: FacetKey) {
  if (f.individual) {
    const c = p.capture
    if (!c.markId || !c.species || individualKey(c.markId, c.species) !== f.individual) return false
  }
  return FACETS.every(key => key === except || !f[key].length || f[key].includes(facetValue(key, p)))
}

/**
 * How many points each value of a filter would show, given the other filters
 * (as in the atlas: the counts of a list follow what is chosen elsewhere).
 */
export function facetCounts(points: MapPoint[], f: MapFilters, key: FacetKey): Map<string, number> {
  const out = new Map<string, number>()
  for (const p of points) if (passes(p, f, key)) out.set(facetValue(key, p), (out.get(facetValue(key, p)) || 0) + 1)
  return out
}

/**
 * Colours for the most common species; the rest share "other". Ranked over all
 * walks so a species keeps its colour whatever the filters.
 */
export function rankColors(counts: Map<string, number>, palette: string[]): Map<string, string> {
  const ranked = [...counts].filter(([k]) => k).sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0]))
  return new Map(ranked.slice(0, palette.length).map(([k], i) => [k, palette[i]]))
}

/** One capture of a marked individual, with its photos and position when it is on the map. */
export interface IndividualEvent {
  /** Its sheet row; none for a recapture written only in notes or only in Wikiloc. */
  row: TableRow | null
  date: number | null
  collector: string
  section: string
  minutes: number | null
  photos: string[]
  position: [number, number] | null
  /** Days and metres since the previous capture. */
  days: number | null
  metres: number | null
  /** A recapture that is not a row of the sheet: where it is written. */
  outside?: OutsideSource
  /** The note (Notes_Collection_data of the marking row) or the Wikiloc note that tells it. */
  note?: string | null
}

export interface Individual {
  key: string
  id: string
  species: string
  sex: string
  events: IndividualEvent[]
  photos: number
  /** Days between the first and the last capture. */
  span: number | null
}

// ------------------------------------------------ recaptures that are not rows

/** Where a recapture that is not a row of the sheet is written. */
export type OutsideSource = 'nota' | 'wikiloc' | 'nota y Wikiloc'

/** A Wikiloc point without a sheet row (stored on the map, or in a walk waiting for review). */
export interface LoosePoint {
  date: string
  collector: string | null
  text: string
  markId: string | null
  species: string | null
  sex: 'female' | 'male' | null
  minutes: number | null
  section: number | null
  lat: number
  lon: number
  photos: string[]
  /** Where it is: "trackId|capture index" on the map, "walkId|point index" in a waiting walk. */
  ref?: string
}

/**
 * A recapture the team decided not to add as a row (2024 and a few later):
 * written only in the notes of the marking row ("7/7/24 AA: recatch&realease
 * transect=4, time=9:59"), or only as a Wikiloc point whose mark was given on
 * an earlier day. Shown in Recapturas and on the map, never written.
 */
export interface OutsideRecapture {
  key: string
  mark: string
  /** The marking row (the first capture). */
  first: TableRow
  date: string
  minutes: number | null
  section: number | null
  collector: string
  source: OutsideSource
  note: string | null
  point: LoosePoint | null
}

const text = (v: unknown) => (v === null || v === undefined ? '' : String(v).trim())
const binomial = (v: unknown) => text(v).toLowerCase().split(/\s+/).slice(0, 2).join(' ')
const rowDate = (r: TableRow) => (typeof r.values.Collection_date === 'number' ? r.values.Collection_date : null)
const initials = (collector: string | null) => (collector || '').split(' - ')[0].trim()

/**
 * Recaptures written only in notes, and Wikiloc points of a mark given on an
 * earlier day that have no row (points of the same mark and day as a note are
 * one recapture). A point belongs to the latest earlier row with its mark whose
 * species and sex do not disagree; a mark with a row that day is in the sheet.
 */
export function recapturesOutsideSheet(rows: TableRow[], points: LoosePoint[]): OutsideRecapture[] {
  const byMark = new Map<string, TableRow[]>()
  for (const r of rows.filter(hasMark).sort((a, b) => (rowDate(a) ?? 0) - (rowDate(b) ?? 0) || a.row - b.row)) {
    const mark = text(r.values.FieldMark_ID).toUpperCase()
    byMark.set(mark, [...(byMark.get(mark) || []), r])
  }
  const out = new Map<string, OutsideRecapture>()
  for (const n of noteRecaptures(rows)) {
    if (!n.date) continue
    const mark = text(n.row.values.FieldMark_ID).toUpperCase()
    const key = individualKey(mark, text(n.row.values.SPECIES))
    out.set(`${key}|${n.date}`, {
      key,
      mark,
      first: n.row,
      date: n.date,
      minutes: n.minutes,
      section: n.section,
      collector: n.initials || initials(text(n.row.values.Collector)),
      source: 'nota',
      note: n.note,
      point: null,
    })
  }
  for (const p of points) {
    if (!p.markId) continue
    const mark = p.markId.toUpperCase()
    const day = isoToSerial(p.date)
    const same = (r: TableRow) =>
      (!p.species || !text(r.values.SPECIES) || binomial(r.values.SPECIES) === binomial(p.species)) &&
      (!p.sex || !sexOfValue(r.values.Sex) || sexOfValue(r.values.Sex) === p.sex)
    const list = (byMark.get(mark) || []).filter(same)
    // Entered that day: not outside the sheet. Never marked before: a new mark, not a recapture.
    if (list.some(r => rowDate(r) === day)) continue
    // Marks are handed out again over the years: the latest butterfly with it before that day.
    const first = list.filter(r => (rowDate(r) ?? Infinity) < day).at(-1)
    if (!first) continue
    const key = individualKey(mark, text(first.values.SPECIES))
    const noted = out.get(`${key}|${p.date}`)
    out.set(`${key}|${p.date}`, {
      key,
      mark,
      first: noted?.first ?? first,
      date: p.date,
      minutes: p.minutes ?? noted?.minutes ?? null,
      section: p.section ?? noted?.section ?? null,
      collector: initials(p.collector) || noted?.collector || '',
      source: noted ? 'nota y Wikiloc' : 'wikiloc',
      note: noted?.note ?? null,
      point: p,
    })
  }
  return [...out.values()].sort((a, b) => a.date.localeCompare(b.date))
}

/** A row as a capture of an individual (before its photos are joined). */
const rowEvent = (row: TableRow) => ({
  row,
  date: rowDate(row),
  collector: initials(text(row.values.Collector)),
  section: text(row.values.Transect_section),
  minutes: typeof row.values.Collection_time === 'number' ? Math.round(row.values.Collection_time * 1440) : null,
})

/**
 * Marked individuals caught more than once, each capture joined to the walk
 * point it was stored with (its photos and GPS position). Recaptures outside
 * the sheet join their individual (or make one with its marking row).
 */
export function individuals(histories: MarkHistory[], walks: MapWalk[], outside: OutsideRecapture[] = []): Individual[] {
  const byRow = new Map<number, MapCapture>()
  const byDayMark = new Map<string, MapCapture>()
  for (const w of walks)
    for (const c of w.captures) {
      if (c.row) byRow.set(c.row, c)
      if (c.markId && c.species && !c.doubt) byDayMark.set(`${isoToSerial(w.date)}|${individualKey(c.markId, c.species)}`, c)
    }
  type Raw = Omit<IndividualEvent, 'days' | 'metres'>
  const groups = new Map<string, { id: string; species: string; sex: string; events: Raw[] }>()
  const fromRow = (key: string, e: ReturnType<typeof rowEvent>): Raw => {
    const c = byRow.get(e.row.row) ?? (e.date !== null ? byDayMark.get(`${e.date}|${key}`) : undefined)
    return { ...e, photos: c?.photos || [], position: c ? [c.lat, c.lon] : null }
  }
  for (const h of histories) {
    const key = individualKey(h.id, String(h.events[0].row.values.SPECIES ?? ''))
    groups.set(key, { id: h.id, species: h.species, sex: h.sex, events: h.events.map(e => fromRow(key, e)) })
  }
  for (const o of outside) {
    const group =
      groups.get(o.key) ||
      groups
        .set(o.key, {
          id: o.mark,
          species: [text(o.first.values.SPECIES), text(o.first.values.Subspecies_Form)].filter(Boolean).join(' '),
          sex: text(o.first.values.Sex),
          events: [fromRow(o.key, rowEvent(o.first))],
        })
        .get(o.key)!
    group.events.push({
      row: null,
      date: isoToSerial(o.date),
      collector: o.collector,
      section: o.section ? String(o.section) : '',
      minutes: o.minutes,
      photos: o.point?.photos || [],
      position: o.point ? [o.point.lat, o.point.lon] : null,
      outside: o.source,
      note: o.note ?? o.point?.text ?? null,
    })
  }
  return [...groups].map(([key, g]) => {
    const events: IndividualEvent[] = []
    for (const e of [...g.events].sort((a, b) => (a.date ?? 0) - (b.date ?? 0))) {
      const before = events.at(-1)
      events.push({
        ...e,
        days: before && before.date !== null && e.date !== null ? e.date - before.date : null,
        metres: before?.position && e.position ? Math.round(distance(before.position, e.position)) : null,
      })
    }
    const first = events[0].date
    const last = events.at(-1)!.date
    return {
      key,
      id: g.id,
      species: g.species,
      sex: g.sex,
      events,
      photos: events.reduce((n, e) => n + e.photos.length, 0),
      span: first !== null && last !== null ? last - first : null,
    }
  })
}
