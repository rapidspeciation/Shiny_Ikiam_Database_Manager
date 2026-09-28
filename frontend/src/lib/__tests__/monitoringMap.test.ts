import { describe, expect, it } from 'vitest'
import { markHistories } from '../monitoring'
import {
  facetCounts,
  individuals,
  passes,
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
