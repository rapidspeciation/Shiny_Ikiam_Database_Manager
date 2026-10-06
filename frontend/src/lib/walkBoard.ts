import { isoToSerial } from './dates.ts'
import {
  TIME_TOLERANCE,
  dayKey,
  doubtfulMatch,
  hasMark,
  locateCapture,
  matchWalk,
  noteRecaptureValues,
  noteRecaptures,
  sexOf,
  walkMarkRoles,
  type ImportedCapture,
  type MatchConfidence,
  type MatchConflict,
  type NoteRecapture,
  type Taxa,
} from './monitoring.ts'
import { estimatedSection } from './transects.ts'
import type { CellValue, TableRow } from './types.ts'

/**
 * The pairing board of one Wikiloc walk (Monitoreo → Dudas): its points in walk
 * order beside the collector's rows of that day, with the pairs the matcher
 * suggests (matchWalk) and what a person decides for each point: its rows, "never
 * entered" (a field mistake: the point stays on the map without a row) or a new
 * recapture row (a recapture written only in the notes of its marking row).
 * Rows may be marked as having no point. The walk is stored, through the same
 * path as Pasar al mapa, once every point is decided.
 */

// Spanish; t() where shown.
/** How sure a suggested pair is, in a word (Dudas explains the doubtful ones at length). */
export const CONFIDENCE: Record<MatchConfidence, string> = {
  mark: 'Por marca',
  sure: 'Segura',
  tie: 'Empate',
  order: 'Por orden',
  none: 'Sin fila',
}
/** Where a point's note and its row disagree. */
export const CONFLICT: Record<MatchConflict, string> = {
  sexo: 'el sexo no coincide',
  especie: 'la especie no coincide',
  marca: 'la marca no coincide',
  hora: 'otra hora',
}

/** A Wikiloc walk as the app keeps it (composables/useMonitoring WikilocWalk). */
export interface BoardWalk {
  id: string
  name: string
  url: string
  date: string | null
  collector?: string | null
  status: 'waiting' | 'imported'
  trackId: string | null
  waypoints: { lat: number; lon: number; ele: number | null; text: string; photos: string[] }[]
}

/** The stored track of an imported walk, its captures linked as read now (server listTracks). */
export interface BoardTrack {
  id: string
  date: string
  collector: string | null
  captures: { text: string; lat: number; lon: number; recordId?: string | null; link?: 'manual' | 'none' | null }[]
  rowsWithoutPoint?: string[]
}

export interface BoardPoint {
  index: number
  capture: ImportedCapture
  /** The section its GPS position lies in (null: off the trail), and its distance to the trail in metres. */
  estimate: { section: number | null; distance: number }
  /** The rows the matcher pairs it with, how sure, and whether it is decided without a person (by mark or sure, nothing disagreeing). */
  suggestion: { ids: string[]; confidence: MatchConfidence; conflicts: MatchConflict[]; sure: boolean }
  /**
   * A recapture that is not a row: written only in the notes of its marking row
   * (`note` holds it), or only this point with the mark of an earlier butterfly
   * (`note` empty). It can become its own row.
   */
  recapture: NoteRecapture | null
}

export interface BoardRow {
  id: string
  row: number
  species: string | null
  subspecies: string | null
  sex: 'female' | 'male' | null
  minutes: number | null
  section: number | null
  markId: string | null
  kind: string | null
}

export interface Board {
  walk: BoardWalk
  trackId: string | null
  /** The walk's day (a title one day off is corrected by matching) and collector. */
  date: string
  collector: string
  points: BoardPoint[]
  /** The collector's rows of that day, by time. */
  rows: BoardRow[]
  initial: BoardState
}

/**
 * What is decided for a point: its rows (suggested by the app, kept from the
 * stored walk, or chosen by a person), never entered in the sheet, or a new
 * recapture row waiting in the unsaved changes (its client id).
 */
export type Decision =
  { kind: 'rows'; ids: string[]; by: 'app' | 'stored' | 'person' } | { kind: 'none' } | { kind: 'new'; clientId: string }

export type Selection = { point: number } | { row: string }

export interface BoardState {
  decisions: (Decision | null)[]
  /** Rows a person says have no point in the walk. */
  rowsWithoutPoint: string[]
  selected: Selection | null
}

const clean = (v: CellValue | undefined) => (v === null || v === undefined ? '' : String(v).trim())
const minutesOf = (r: TableRow) =>
  typeof r.values.Collection_time === 'number' ? Math.round(r.values.Collection_time * 1440) : null

function boardRow(r: TableRow): BoardRow {
  const section = Number(clean(r.values.Transect_section))
  return {
    id: r.id,
    row: r.row,
    species: clean(r.values.SPECIES) || null,
    subspecies: clean(r.values.Subspecies_Form) || null,
    sex: sexOf(r.values.Sex),
    minutes: minutesOf(r),
    section: section >= 1 && section <= 4 ? section : null,
    markId: hasMark(r) ? clean(r.values.FieldMark_ID).toUpperCase() : null,
    kind: clean(r.values.Release_Collect) || null,
  }
}

const pointKey = (p: { text: string; lat: number; lon: number }) => `${p.text}|${p.lat}|${p.lon}`
const sameMark = (a: string, b: string) => a.toUpperCase().replace(/\..*$/, '') === b.toUpperCase().replace(/\..*$/, '')

/** The note-only recapture of the walk's day each point is: the same mark, or, for a note without one, the same minute. */
function noteRecaptureOf(points: ImportedCapture[], found: NoteRecapture[]): (NoteRecapture | null)[] {
  const left = [...found]
  const take = (test: (n: NoteRecapture) => boolean) => {
    const k = left.findIndex(test)
    return k < 0 ? null : left.splice(k, 1)[0]
  }
  const byMark = points.map(c => (c.markId ? take(n => sameMark(clean(n.row.values.FieldMark_ID), c.markId!)) : null))
  return points.map(
    (c, i) =>
      byMark[i] ??
      (!c.markId && c.minutes !== null
        ? take(n => n.minutes !== null && Math.abs(n.minutes - c.minutes!) <= TIME_TOLERANCE)
        : null),
  )
}

/**
 * The board of a walk: its points (read from the notes, as Pasar al mapa reads
 * them), the collector's rows of the day, and the matcher's pairs. An imported
 * walk starts from its stored links (kept as they are) and suggests rows only
 * for the points without one. A new recapture row already waiting to be saved
 * (same mark and day) is that point's decision. Null without a date or collector.
 */
export function buildBoard(
  rows: TableRow[],
  walk: BoardWalk,
  taxa: Taxa,
  local: Taxa,
  {
    track = null,
    pending = [],
  }: { track?: BoardTrack | null; pending?: { clientId: string; values: Record<string, CellValue> }[] } = {},
): Board | null {
  const collector = track?.collector || walk.collector || ''
  const date = track?.date || walk.date
  if (!date || !collector) return null
  const sheet = rows.filter(r => r.observed)
  const captures = walk.waypoints.map(p => locateCapture({ ...p, time: null }, taxa, local))
  // The stored links of an imported walk, by point (the copies of a note of several butterflies together).
  const stored = new Map<string, BoardTrack['captures']>()
  for (const c of track?.captures || []) stored.set(pointKey(c), [...(stored.get(pointKey(c)) || []), c])
  const current = captures.map(c => {
    const copies = stored.get(pointKey(c))
    if (!copies) return null
    const ids = copies.flatMap(k => (k.recordId ? [k.recordId] : []))
    if (ids.length) return ids
    return copies.every(k => k.link === 'none') ? [] : null
  })
  const fixed = new Map<number, string[] | null>()
  current.forEach((ids, i) => {
    if (ids) fixed.set(i, ids.length ? ids : null)
  })
  const match = matchWalk(sheet, date, collector, captures, { fixed, shift: !track })
  const serial = isoToSerial(match.date)
  const day = sheet
    .filter(r => r.values.Collection_date === serial && dayKey(serial, clean(r.values.Collector)) === dayKey(serial, collector))
    .map(boardRow)
    .sort((a, b) => (a.minutes ?? 1e6) - (b.minutes ?? 1e6) || a.row - b.row)
  const notes = noteRecaptureOf(
    captures,
    noteRecaptures(sheet).filter(n => n.date === match.date),
  )
  // A mark seen before on the same species and sex, and no row that day: a recapture only Wikiloc has.
  const roles = walkMarkRoles(sheet, match.date, captures)
  const recaptures = captures.map((_, i): NoteRecapture | null => {
    if (notes[i]) return notes[i]
    const role = roles[i]
    const marked = role?.role === 'recapture' ? (role.of ?? role.first) : null
    if (!marked || match.matches[i].rows.length) return null
    const empty = { minutes: null, height: null, cloud: null, rain: null, initials: null, section: null }
    return { row: marked, note: '', date: match.date, ...empty }
  })
  const points: BoardPoint[] = captures.map((capture, index) => {
    const m = match.matches[index]
    return {
      index,
      capture,
      estimate: estimatedSection(capture.lat, capture.lon),
      suggestion: {
        ids: m.rows.map(r => r.id),
        confidence: m.confidence,
        conflicts: m.conflicts,
        sure: (m.confidence === 'mark' || m.confidence === 'sure') && !doubtfulMatch(m),
      },
      recapture: recaptures[index],
    }
  })
  const decisions = points.map((p, i): Decision | null => {
    const ids = current[i]
    if (ids) return ids.length ? { kind: 'rows', ids, by: 'stored' } : { kind: 'none' }
    const waiting = p.recapture && pendingRecapture(pending, p.recapture, match.date)
    if (waiting) return { kind: 'new', clientId: waiting.clientId }
    return p.suggestion.ids.length ? { kind: 'rows', ids: p.suggestion.ids, by: 'app' } : null
  })
  const ids = new Set(day.map(r => r.id))
  return {
    walk,
    trackId: track?.id ?? null,
    date: match.date,
    collector,
    points,
    rows: day,
    initial: { decisions, rowsWithoutPoint: (track?.rowsWithoutPoint || []).filter(id => ids.has(id)), selected: null },
  }
}

/** The unsaved new row of a note-only recapture: the same mark on the same day. */
function pendingRecapture(pending: { clientId: string; values: Record<string, CellValue> }[], n: NoteRecapture, date: string) {
  const mark = clean(n.row.values.FieldMark_ID).toUpperCase()
  return (
    pending.find(c => clean(c.values.FieldMark_ID).toUpperCase() === mark && c.values.Collection_date === isoToSerial(date)) ??
    null
  )
}

// ------------------------------------------------------------ state

/** The point a row is paired with, or -1. */
export const pointOfRow = (state: BoardState, id: string) =>
  state.decisions.findIndex(d => d?.kind === 'rows' && d.ids.includes(id))

/** Each row once: taken from any other point (a point left without rows is undecided again) and from "no point". */
function freeRow(state: BoardState, id: string, keep = -1): BoardState {
  return {
    ...state,
    decisions: state.decisions.map((d, i) => {
      if (i === keep || d?.kind !== 'rows' || !d.ids.includes(id)) return d
      const ids = d.ids.filter(x => x !== id)
      return ids.length ? { ...d, ids, by: 'person' } : null
    }),
    rowsWithoutPoint: state.rowsWithoutPoint.filter(x => x !== id),
  }
}

/**
 * Pairs a point with a row, or unpairs them when they already are. A note of
 * several butterflies takes several rows; any other point has one, so a new row
 * replaces its row.
 */
export function toggleLink(board: Board, state: BoardState, point: number, id: string): BoardState {
  const d = state.decisions[point]
  if (d?.kind === 'rows' && d.ids.includes(id)) {
    const ids = d.ids.filter(x => x !== id)
    return { ...state, decisions: with_(state.decisions, point, ids.length ? { kind: 'rows', ids, by: 'person' } : null) }
  }
  const next = freeRow(state, id, point)
  const several = board.points[point].capture.count > 1 && d?.kind === 'rows'
  return {
    ...next,
    decisions: with_(next.decisions, point, { kind: 'rows', ids: several ? [...d.ids, id] : [id], by: 'person' }),
  }
}

const with_ = <T>(list: T[], i: number, value: T) => list.map((x, k) => (k === i ? value : x))

/**
 * A click on a point or a row: chooses it, or, with one of the other side
 * chosen, pairs (or unpairs) the two. Clicking the chosen one again lets it go.
 */
export function click(board: Board, state: BoardState, target: Selection): BoardState {
  const s = state.selected
  if (!s) return { ...state, selected: target }
  if ('point' in s && 'point' in target) return { ...state, selected: s.point === target.point ? null : target }
  if ('row' in s && 'row' in target) return { ...state, selected: s.row === target.row ? null : target }
  const point = 'point' in s ? s.point : (target as { point: number }).point
  const row = 'row' in s ? s.row : (target as { row: string }).row
  return { ...toggleLink(board, state, point, row), selected: null }
}

/** Sets what a point is (null: undecided again); "never entered" and a new row free its rows. */
export function decide(state: BoardState, point: number, decision: Decision | null): BoardState {
  return { ...state, decisions: with_(state.decisions, point, decision), selected: null }
}

/** A suggested pair kept as it is by a person. */
export function confirm(state: BoardState, point: number): BoardState {
  const d = state.decisions[point]
  return d?.kind === 'rows' ? decide(state, point, { ...d, by: 'person' }) : state
}

/** A row that has no point in the walk (the waypoint was never added), or back to unpaired. */
export function toggleRowWithoutPoint(state: BoardState, id: string): BoardState {
  if (state.rowsWithoutPoint.includes(id)) return { ...state, rowsWithoutPoint: state.rowsWithoutPoint.filter(x => x !== id) }
  const next = freeRow(state, id)
  return { ...next, rowsWithoutPoint: [...next.rowsWithoutPoint, id], selected: null }
}

/** Decided: by a person, kept from the stored walk, or suggested surely by the app. */
export function isDecided(board: Board, state: BoardState, point: number) {
  const d = state.decisions[point]
  if (!d) return false
  return d.kind !== 'rows' || d.by !== 'app' || board.points[point].suggestion.sure
}

export function boardSummary(board: Board, state: BoardState) {
  const undecided = board.points.filter(p => !isDecided(board, state, p.index)).map(p => p.index)
  const paired = new Set(state.decisions.flatMap(d => (d?.kind === 'rows' ? d.ids : [])))
  const unpairedRows = board.rows.filter(r => !paired.has(r.id) && !state.rowsWithoutPoint.includes(r.id)).map(r => r.id)
  return { undecided, unpairedRows, ready: !undecided.length }
}

/** Client ids of new recapture rows the state no longer holds (to remove from the unsaved changes). */
export function droppedRows(before: BoardState, after: BoardState): string[] {
  const kept = new Set(after.decisions.flatMap(d => (d?.kind === 'new' ? [d.clientId] : [])))
  return before.decisions.flatMap(d => (d?.kind === 'new' && !kept.has(d.clientId) ? [d.clientId] : []))
}

/**
 * The rows of each point for the store endpoint (server storeReviewedWalk): record
 * ids, none ([]: never entered), or 'new' (its row is linked once saved).
 */
export function linksToStore(board: Board, state: BoardState): (string[] | 'new')[] {
  return board.points.map(p => {
    const d = state.decisions[p.index]
    if (!d || !isDecided(board, state, p.index)) throw new Error(`Point ${p.index + 1} is not decided`)
    return d.kind === 'rows' ? d.ids : d.kind === 'none' ? [] : 'new'
  })
}

/**
 * The new Mark_Released row of a recapture that is not a row (noteRecaptureValues):
 * the individual of its marking row, with the point's day, time, section (from
 * its GPS position), height and weather where the point has them, else what the
 * note says.
 */
export function recaptureRowValues(board: Board, point: BoardPoint, collectors: string[]): Record<string, CellValue> | null {
  const n = point.recapture
  if (!n) return null
  const c = point.capture
  return noteRecaptureValues(
    {
      ...n,
      date: board.date,
      minutes: c.minutes ?? n.minutes,
      height: c.height ?? n.height,
      cloud: c.cloud ?? n.cloud,
      rain: c.rain ?? n.rain,
      initials: board.collector.split(' - ')[0].trim(),
      section: point.estimate.section ?? n.section,
    },
    collectors.includes(board.collector) ? collectors : [...collectors, board.collector],
  )
}
