import { isoToSerial } from './dates'
import type { MarkHistory } from './monitoring'
import { distance } from './transects'

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
  row: MarkHistory['events'][number]['row']
  date: number | null
  collector: string
  section: string
  minutes: number | null
  photos: string[]
  position: [number, number] | null
  /** Days and metres since the previous capture. */
  days: number | null
  metres: number | null
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

/**
 * Marked individuals caught more than once, each capture joined to the walk
 * point it was stored with (its photos and GPS position).
 */
export function individuals(histories: MarkHistory[], walks: MapWalk[]): Individual[] {
  const byRow = new Map<number, MapCapture>()
  const byDayMark = new Map<string, MapCapture>()
  for (const w of walks)
    for (const c of w.captures) {
      if (c.row) byRow.set(c.row, c)
      if (c.markId && c.species) byDayMark.set(`${isoToSerial(w.date)}|${individualKey(c.markId, c.species)}`, c)
    }
  return histories.map(h => {
    const species = String(h.events[0].row.values.SPECIES ?? '')
    const key = individualKey(h.id, species)
    const events: IndividualEvent[] = []
    for (const e of h.events) {
      const c = byRow.get(e.row.row) ?? (e.date !== null ? byDayMark.get(`${e.date}|${key}`) : undefined)
      const position: [number, number] | null = c ? [c.lat, c.lon] : null
      const before = events.at(-1)
      events.push({
        ...e,
        photos: c?.photos || [],
        position,
        days: before && before.date !== null && e.date !== null ? e.date - before.date : null,
        metres: before?.position && position ? Math.round(distance(before.position, position)) : null,
      })
    }
    const first = events[0].date
    const last = events.at(-1)!.date
    return {
      key,
      id: h.id,
      species: h.species,
      sex: h.sex,
      events,
      photos: events.reduce((n, e) => n + e.photos.length, 0),
      span: first !== null && last !== null ? last - first : null,
    }
  })
}
