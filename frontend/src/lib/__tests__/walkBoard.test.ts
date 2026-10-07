import { describe, expect, it } from 'vitest'
import { isoToSerial } from '../dates'
import { taxaFrom } from '../monitoring'
import { SECTIONS } from '../transects'
import type { CellValue, TableRow } from '../types'
import {
  boardSummary,
  buildBoard,
  click,
  confirm,
  decide,
  droppedRows,
  isDecided,
  linksToStore,
  pointOfRow,
  recaptureRowValues,
  toggleLink,
  toggleRowWithoutPoint,
  type Board,
  type BoardWalk,
} from '../walkBoard'

let n = 100
function row(values: Record<string, CellValue>): TableRow {
  n++
  return {
    id: `r${n}`,
    row: n,
    version: 1,
    observed: true,
    values: { Purpose: 'Monitoring', Collection_location: 'Ikiam', ...values },
    formulas: [],
  }
}
const mid = (section: number) => {
  const s = SECTIONS.find(x => x.section === section)!
  return s.path[Math.floor(s.path.length / 2)]
}
const at = (time: string) => {
  const [h, m] = time.split(':').map(Number)
  return (h * 60 + m) / 1440
}

// --------------------------------------------------------------- a walk and its day
const DATE = '2025-09-10'
const day = isoToSerial(DATE)
const AA = 'AA - Alex Arias'
const sheet = [
  row({
    SPECIES: 'Hypothyris euclea',
    Subspecies_Form: 'intermedia',
    Sex: 'female',
    Collection_date: day,
    Collection_time: at('9:12'),
    Collector: AA,
    FieldMark_ID: 'B10',
    Release_Collect: 'Mark_Released',
    Transect_section: 4,
  }),
  row({
    SPECIES: 'Mechanitis polymnia',
    Subspecies_Form: 'proceriformis',
    Sex: 'male',
    Collection_date: day,
    Collection_time: at('9:40'),
    Collector: AA,
    FieldMark_ID: 'NA',
    Transect_section: 3,
  }),
  row({
    SPECIES: 'Methona confusa',
    Subspecies_Form: 'psamathe',
    Sex: 'male',
    Collection_date: day,
    Collection_time: at('10:05'),
    Collector: AA,
    FieldMark_ID: 'NA',
    Transect_section: 2,
  }),
  // Another collector's row that day: not on the board.
  row({
    SPECIES: 'Methona confusa',
    Sex: 'male',
    Collection_date: day,
    Collection_time: at('10:05'),
    Collector: 'FCH - Franz Chandi',
    FieldMark_ID: 'NA',
  }),
]
const [b10, mech, methona] = sheet
const taxa = taxaFrom(sheet)
const point = (text: string, section: number) => {
  const [lat, lon] = mid(section)
  return { lat, lon, ele: null, text, photos: ['123'] }
}
const walk = (waypoints: BoardWalk['waypoints'], extra: Partial<BoardWalk> = {}): BoardWalk => ({
  id: 'w1',
  name: 'Monitoreo AA 10/9/25',
  url: 'https://es.wikiloc.com/x-1234567',
  date: DATE,
  collector: AA,
  status: 'waiting',
  trackId: null,
  waypoints,
  ...extra,
})
const board = () =>
  buildBoard(
    sheet,
    walk([
      point('B10 9:12 hembra sol 1m', 4),
      point('Mech polymnia macho 9:40 sol', 2),
      point('Planta 9:50', 1),
      point('Methona 10:05 macho', 2),
    ]),
    taxa,
    taxa,
  )!

describe('buildBoard', () => {
  it("lists the points in walk order with their GPS section, the collector's rows by time and the suggested pairs", () => {
    const b = board()
    expect(b.date).toBe(DATE)
    expect(b.rows.map(r => r.id)).toEqual([b10.id, mech.id, methona.id])
    expect(b.rows[0]).toMatchObject({ row: b10.row, markId: 'B10', section: 4, minutes: 552, kind: 'Mark_Released' })
    expect(b.points.map(p => p.estimate.section)).toEqual([4, 2, 1, 2])
    expect(b.points.map(p => p.capture.minutes)).toEqual([552, 580, 590, 605])
    expect(b.points[0].suggestion).toMatchObject({ ids: [b10.id], confidence: 'mark', sure: true })
    expect(b.points[1].suggestion).toMatchObject({ ids: [mech.id], sure: true })
    expect(b.points[2].suggestion.ids).toEqual([])
    // Suggested pairs start as the app's; the sure ones count as decided.
    expect(b.initial.decisions.map(d => d && (d.kind === 'rows' ? d.ids : d.kind))).toEqual([
      [b10.id],
      [mech.id],
      null,
      [methona.id],
    ])
    expect(b.points.map(p => isDecided(b, b.initial, p.index))).toEqual([true, true, false, true])
    expect(boardSummary(b, b.initial)).toEqual({ undecided: [2], unpairedRows: [], ready: false })
  })
  it('needs a date and a collector', () => {
    expect(buildBoard(sheet, walk([], { collector: null }), taxa, taxa)).toBeNull()
  })
  it('an imported walk starts from its stored links', () => {
    const w = walk([point('B10 9:12 hembra sol 1m', 4), point('Mech polymnia macho 9:40 sol', 2), point('Planta 9:50', 1)], {
      status: 'imported',
      trackId: 't1',
    })
    const captures = w.waypoints.map((p, i) => ({
      text: p.text,
      lat: p.lat,
      lon: p.lon,
      recordId: i === 1 ? methona.id : null,
      link: i === 1 ? ('manual' as const) : i === 2 ? ('none' as const) : null,
    }))
    const b = buildBoard(sheet, w, taxa, taxa, {
      track: { id: 't1', date: DATE, collector: AA, captures, rowsWithoutPoint: [mech.id, 'gone'] },
    })!
    expect(b.trackId).toBe('t1')
    expect(b.initial.decisions).toEqual([
      { kind: 'rows', ids: [b10.id], by: 'app' },
      { kind: 'rows', ids: [methona.id], by: 'stored' },
      { kind: 'none' },
    ])
    expect(b.initial.rowsWithoutPoint).toEqual([mech.id])
    expect(boardSummary(b, b.initial)).toEqual({ undecided: [], unpairedRows: [], ready: true })
  })
})

describe('pairing on the board', () => {
  it('a point then a row pairs them; the same pair again separates them', () => {
    const b = board()
    let s = click(b, b.initial, { point: 2 })
    expect(s.selected).toEqual({ point: 2 })
    s = click(b, s, { row: methona.id })
    expect(s.selected).toBeNull()
    // The row leaves the point it had, which is undecided again.
    expect(s.decisions[2]).toEqual({ kind: 'rows', ids: [methona.id], by: 'person' })
    expect(s.decisions[3]).toBeNull()
    expect(pointOfRow(s, methona.id)).toBe(2)
    s = click(b, click(b, s, { row: methona.id }), { point: 2 })
    expect(s.decisions[2]).toBeNull()
    expect(pointOfRow(s, methona.id)).toBe(-1)
  })
  it('clicking the chosen one again lets it go; another of the same side takes its place', () => {
    const b = board()
    expect(click(b, click(b, b.initial, { point: 1 }), { point: 1 }).selected).toBeNull()
    expect(click(b, click(b, b.initial, { point: 1 }), { point: 3 }).selected).toEqual({ point: 3 })
    expect(click(b, click(b, b.initial, { row: mech.id }), { row: b10.id }).selected).toEqual({ row: b10.id })
  })
  it('a point has one row, a note of several butterflies more', () => {
    const b = board()
    const s = toggleLink(b, b.initial, 0, mech.id)
    expect(s.decisions[0]).toEqual({ kind: 'rows', ids: [mech.id], by: 'person' })
    expect(s.decisions[1]).toBeNull()
    const two = buildBoard(sheet, walk([point('Mariposa 1 y 2 9:40', 3)]), taxa, taxa)!
    expect(two.points[0].capture.count).toBe(2)
    const both = toggleLink(two, toggleLink(two, { ...two.initial, decisions: [null] }, 0, mech.id), 0, methona.id)
    expect(both.decisions[0]).toEqual({ kind: 'rows', ids: [mech.id, methona.id], by: 'person' })
  })
  it('a doubtful suggestion waits for a person to keep it', () => {
    const b = board()
    const s = {
      ...b.initial,
      decisions: b.initial.decisions.map((d, i) => (i === 2 ? { kind: 'rows' as const, ids: ['x'], by: 'app' as const } : d)),
    }
    expect(isDecided(b, s, 2)).toBe(false)
    expect(isDecided(b, confirm(s, 2), 2)).toBe(true)
  })
  it('never entered, rows without a point, and the links to store', () => {
    const b = board()
    let s = decide(b.initial, 2, { kind: 'none' })
    expect(boardSummary(b, s).ready).toBe(true)
    // A paired row marked without a point leaves its point.
    s = toggleRowWithoutPoint(s, methona.id)
    expect(s.decisions[3]).toBeNull()
    expect(boardSummary(b, s)).toEqual({ undecided: [3], unpairedRows: [], ready: false })
    expect(() => linksToStore(b, s)).toThrow()
    s = decide(s, 3, { kind: 'new', clientId: 'c1' })
    expect(linksToStore(b, s)).toEqual([[b10.id], [mech.id], [], 'new'])
    expect(s.rowsWithoutPoint).toEqual([methona.id])
    expect(toggleRowWithoutPoint(s, methona.id).rowsWithoutPoint).toEqual([])
    // Leaving the new row (another decision, or undecided) drops its unsaved row.
    expect(droppedRows(s, decide(s, 3, null))).toEqual(['c1'])
    expect(droppedRows(s, toggleLink(b, s, 3, methona.id))).toEqual(['c1'])
    expect(droppedRows(s, s)).toEqual([])
  })
})

describe('recaptures written only in notes', () => {
  const marked = row({
    SPECIES: 'Hyposcada illinissa',
    Subspecies_Form: 'ida',
    Sex: 'male',
    Release_Collect: 'Mark_Released',
    FieldMark_ID: 'M45',
    Collection_date: isoToSerial('2024-06-12'),
    Collector: 'FCH - Franz Chandi',
    Country: 'Ecuador',
    Notes_Collection_data:
      '12/6/24 FCH: Monitoring | 7/72024 AA: recatch&realease transect=4, date=7/7/24, time=9:59, collector=AA, Rainfall=DY, cloud_cover=CL_(cloudy_light), height=0.5m',
  })
  const other = row({
    SPECIES: 'Oleria onega',
    Sex: 'female',
    Collection_date: isoToSerial('2024-07-07'),
    Collection_time: at('9:30'),
    Collector: AA,
    FieldMark_ID: 'NA',
  })
  const rows = [marked, other]
  const recaptureWalk = walk([point('9:30 hembra sol', 3), point('M45 9:59 sol 0.4m', 2)], { date: '2024-07-07' })

  it("finds the point of the note's recapture and makes its row with the point's day, time and section", () => {
    const b = buildBoard(rows, recaptureWalk, taxa, taxa)!
    expect(b.points[0].recapture).toBeNull()
    expect(b.points[1].recapture?.row.id).toBe(marked.id)
    expect(b.points[1].suggestion.ids).toEqual([])
    const v = recaptureRowValues(b, b.points[1], ['AA - Alex Arias', 'FCH - Franz Chandi'])!
    expect(v).toMatchObject({
      Release_Collect: 'Mark_Released',
      FieldMark_ID: 'M45',
      SPECIES: 'Hyposcada illinissa',
      Subspecies_Form: 'ida',
      Sex: 'male',
      Collection_date: isoToSerial('2024-07-07'),
      Collection_time: at('9:59'),
      Transect_section: 2,
      Flight_height: 0.4,
      Collector: AA,
    })
    expect(recaptureRowValues(b, b.points[0], [])).toBeNull()
  })
  it('a point without a mark is its recapture by the minute', () => {
    const b = buildBoard(rows, walk([point('9:59 macho sol', 4)], { date: '2024-07-07' }), taxa, taxa)!
    expect(b.points[0].recapture?.row.id).toBe(marked.id)
  })
  it('a mark of an earlier butterfly with no row that day is a recapture only Wikiloc has', () => {
    const b35 = row({
      SPECIES: 'Oleria gunilla',
      Subspecies_Form: 'lota',
      Sex: 'male',
      Release_Collect: 'Mark_Released',
      FieldMark_ID: 'B35',
      Collection_date: isoToSerial('2026-03-12'),
      Collector: AA,
    })
    const names = taxaFrom([b35])
    const later = walk([point('B35 Oleria gunilla lota male 10:17 NO', 4)], { date: '2026-04-12' })
    const b = buildBoard([b35], later, names, names)!
    expect(b.points[0].recapture).toMatchObject({ note: '' })
    expect(b.points[0].recapture?.row.id).toBe(b35.id)
    const v = recaptureRowValues(b, b.points[0], [AA])!
    expect(v).toMatchObject({ FieldMark_ID: 'B35', SPECIES: 'Oleria gunilla', Collection_time: at('10:17'), Transect_section: 4 })
    expect(v.Notes_Collection_data).toBe(
      '12/4/2026 AA: Recapture of the butterfly marked in row ' + b35.row + ', from its Wikiloc point',
    )
    // Once its row is in the sheet, the point is paired with it instead.
    const saved = row({ ...v, Collection_date: isoToSerial('2026-04-12') })
    const again = buildBoard([b35, saved], later, names, names)!
    expect(again.points[0].recapture).toBeNull()
    expect(again.points[0].suggestion).toMatchObject({ ids: [saved.id], confidence: 'mark' })
  })
  it('its unsaved new row is the decision when the board opens again', () => {
    const pending = [{ clientId: 'c9', values: { FieldMark_ID: 'M45', Collection_date: isoToSerial('2024-07-07') } }]
    const b: Board = buildBoard(rows, recaptureWalk, taxa, taxa, { pending })!
    expect(b.initial.decisions[1]).toEqual({ kind: 'new', clientId: 'c9' })
    expect(isDecided(b, b.initial, 1)).toBe(true)
  })
})
