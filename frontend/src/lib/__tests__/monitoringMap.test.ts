import { describe, expect, it } from 'vitest'
import { markHistories } from '../monitoring'
import {
  facetCounts,
  individuals,
  passes,
  recapturesOutsideSheet,
  type LoosePoint,
  rankColors,
  type MapCapture,
  type MapFilters,
  type MapPoint,
  type MapWalk,
} from '../monitoringMap'
import type { CellValue, TableRow } from '../types'

const capture = (c: Partial<MapCapture>): MapCapture => ({
  lat: 0,
  lon: 0,
  species: 'Oleria onega',
  subspecies: null,
  sex: 'female',
  minutes: null,
  markId: null,
  section: 4,
  recapture: false,
  ...c,
})
const walks: MapWalk[] = [
  {
    id: 'w1',
    date: '2026-07-22',
    collector: 'AA - Alex Arias',
    captures: [capture({ markId: 'B51' }), capture({ species: 'Godyris zavaleta', sex: 'male' })],
  },
  // Two walks on one day: choosing the date shows both.
  { id: 'w2', date: '2026-07-22', collector: 'MJS - María José Sánchez', captures: [capture({ section: 1 })] },
  { id: 'w3', date: '2026-09-21', collector: 'AA - Alex Arias', captures: [capture({ markId: 'B51', recapture: true })] },
]
const points: MapPoint[] = walks.flatMap(walk => walk.captures.map(capture => ({ walk, capture })))
const none: MapFilters = { years: [], dates: [], collectors: [], species: [], sections: [], sexes: [], kinds: [], individual: '' }

describe('map filters', () => {
  it('shows everything when nothing is chosen', () => {
    expect(points.filter(p => passes(p, none))).toHaveLength(4)
  })
  it('shows every walk of a chosen date', () => {
    const shown = points.filter(p => passes(p, { ...none, dates: ['2026-07-22'] }))
    expect(new Set(shown.map(p => p.walk.id))).toEqual(new Set(['w1', 'w2']))
  })
  it('counts each list given the other filters', () => {
    const f = { ...none, collectors: ['AA'] }
    expect(facetCounts(points, f, 'dates')).toEqual(
      new Map([
        ['2026-07-22', 2],
        ['2026-09-21', 1],
      ]),
    )
    // The collector list itself ignores the collector filter.
    expect(facetCounts(points, f, 'collectors')).toEqual(
      new Map([
        ['AA', 3],
        ['MJS', 1],
      ]),
    )
    expect(facetCounts(points, f, 'kinds')).toEqual(
      new Map([
        ['marked', 1],
        ['preserved', 1],
        ['recapture', 1],
      ]),
    )
  })
  it('follows one individual: same mark and species', () => {
    const shown = points.filter(p => passes(p, { ...none, individual: 'B51|Oleria onega' }))
    expect(shown.map(p => p.walk.id)).toEqual(['w1', 'w3'])
  })
  it('colours the most common species and leaves the rest grey', () => {
    const colors = rankColors(
      new Map([
        ['a', 5],
        ['b', 9],
        ['c', 1],
      ]),
      ['red', 'blue'],
    )
    expect([...colors]).toEqual([
      ['b', 'red'],
      ['a', 'blue'],
    ])
  })
})

let n = 0
function row(values: Record<string, CellValue>): TableRow {
  n++
  return { id: `r${n}`, row: 100 + n, version: 1, observed: true, values: { Purpose: 'Monitoring', ...values }, formulas: [] }
}

describe('recaptured individuals', () => {
  it('joins each capture to its photos and measures time and distance between captures', () => {
    const rows = [
      row({
        SPECIES: 'Hyposcada illinissa',
        FieldMark_ID: 'B51',
        Release_Collect: 'Mark_Released',
        Collection_date: 46253,
        Collector: 'MJS - María José Sánchez',
      }),
      row({
        SPECIES: 'Hyposcada illinissa',
        FieldMark_ID: 'B51',
        Release_Collect: 'Mark_Released',
        Collection_date: 46286,
        Collector: 'AA - Alex Arias',
      }),
    ]
    const stored: MapWalk[] = [
      // Joined by the stored sheet row.
      {
        id: 'a',
        date: '2026-08-19',
        collector: 'MJS',
        captures: [capture({ row: rows[0].row, lat: 0, lon: 0, photos: ['p1'], species: 'Hyposcada illinissa', markId: 'B51' })],
      },
      // Joined by day and mark when the row was not stored.
      {
        id: 'b',
        date: '2026-09-21',
        collector: 'AA',
        captures: [capture({ lat: 0, lon: 0.0001, photos: ['p2', 'p3'], species: 'Hyposcada illinissa', markId: 'B51' })],
      },
    ]
    const [b51] = individuals(markHistories(rows), stored)
    expect(b51.key).toBe('B51|Hyposcada illinissa')
    expect(b51.events.map(e => e.photos)).toEqual([['p1'], ['p2', 'p3']])
    expect(b51.events[1].days).toBe(33)
    expect(b51.events[1].metres).toBe(11)
    expect(b51.photos).toBe(3)
    expect(b51.span).toBe(33)
  })
})

describe('recaptures that are not rows of the sheet', () => {
  const rows = [
    row({
      SPECIES: 'Hyposcada illinissa',
      Sex: 'male',
      FieldMark_ID: 'M45',
      Release_Collect: 'Mark_Released',
      Collection_date: 45455,
      Collector: 'FCH - Franz Chandi',
      Notes_Collection_data:
        '12/6/24 FCH: Monitoring by FCH butterfly M3 | 7/72024 AA: recatch&realease transect=4, date=7/7/24, time=9:59, collector=AA, Rainfall=DY, height=0.5m  | 16/8/2024 AA: recatch&realease transect=4, date=16/8/24, time=10:38, collector=AA',
    }),
    row({
      SPECIES: 'Hyposcada illinissa',
      FieldMark_ID: 'B32',
      Release_Collect: 'Mark_Released',
      Collection_date: 46089,
      Collector: 'AA - Alex Arias',
      Notes_Collection_data: '12/4/26 AA: Recapture Cloudy Dark/10:10am/flight height 1,5m',
    }),
    row({
      SPECIES: 'Oleria gunilla',
      Sex: 'male',
      FieldMark_ID: 'B35',
      Collection_date: 46100,
      Collector: 'MJS - María José Sánchez',
    }),
    row({ SPECIES: 'Oleria onega', FieldMark_ID: 'B36', Collection_date: 46100, Collector: 'MJS - María José Sánchez' }),
    row({ SPECIES: 'Oleria onega', FieldMark_ID: 'B36', Collection_date: 46124, Collector: 'AA - Alex Arias' }),
  ]
  const point = (p: Partial<LoosePoint>): LoosePoint => ({
    date: '2026-04-12',
    collector: 'AA - Alex Arias',
    text: '',
    markId: null,
    species: null,
    sex: null,
    minutes: null,
    section: null,
    lat: 0,
    lon: 0,
    photos: [],
    ...p,
  })
  const points = [
    point({ text: 'NO B32 10:10 1,5m', markId: 'B32', minutes: 610, photos: ['w1'] }),
    point({
      text: 'B35 Oleria gunilla lota male 10:17 NO',
      markId: 'B35',
      species: 'Oleria gunilla',
      sex: 'male',
      minutes: 617,
      photos: ['w2'],
    }),
    // Its row that day is in the sheet: not outside.
    point({ markId: 'B36', species: 'Oleria onega' }),
    // A mark never given before is a new butterfly, not a recapture; another species is another butterfly.
    point({ markId: 'B99' }),
    point({ markId: 'B35', species: 'Hyposcada anchiala' }),
  ]
  const outside = recapturesOutsideSheet(rows, points)
  it('finds recaptures written only in notes and Wikiloc points of earlier marks, as one per mark and day', () => {
    expect(outside.map(o => [o.mark, o.date, o.source, o.minutes, o.point?.photos ?? []])).toEqual([
      ['M45', '2024-07-07', 'nota', 599, []],
      ['M45', '2024-08-16', 'nota', 638, []],
      ['B32', '2026-04-12', 'nota y Wikiloc', 610, ['w1']],
      ['B35', '2026-04-12', 'wikiloc', 617, ['w2']],
    ])
    expect(outside[0].first.values.FieldMark_ID).toBe('M45')
  })
  it('shows them as captures of their individual, with the photos of the recapture', () => {
    const list = individuals(markHistories(rows), [], outside)
    const m45 = list.find(i => i.id === 'M45')!
    expect(m45.events.map(e => [e.row?.row ?? null, e.outside ?? null])).toEqual([
      [rows[0].row, null],
      [null, 'nota'],
      [null, 'nota'],
    ])
    expect(m45.events[1].days).toBe(25)
    const b35 = list.find(i => i.id === 'B35')!
    expect(b35.events.map(e => e.photos)).toEqual([[], ['w2']])
    // B36 has two rows and nothing outside: as before.
    expect(list.find(i => i.id === 'B36')!.events.every(e => e.row && !e.outside)).toBe(true)
  })
})
