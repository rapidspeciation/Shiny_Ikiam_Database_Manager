import { describe, expect, it } from 'vitest'
import {
  CLOUD,
  captureValues,
  locateCapture,
  byCloud,
  byHeight,
  byHour,
  effortDays,
  existingRow,
  kindsByMonth,
  rareSpecies,
  recaptureDistances,
  seasonality,
  speciesAccumulation,
  speciesBySection,
  median,
  monthRange,
  markConflicts,
  withSheetValues,
  markHistories,
  noteRecaptureValues,
  noteRecaptures,
  preservedForRule,
  nextMarkId,
  parseCapture,
  parseGpx,
  recaptureIds,
  sectionsByMonth,
  speciesStats,
  taxaFrom,
  trackSpan,
} from '../monitoring'
import type { CellValue, TableRow } from '../types'

let n = 0
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

const taxa = taxaFrom([
  row({ SPECIES: 'Hyposcada illinissa', Subspecies_Form: 'ida' }),
  row({ SPECIES: 'Hyposcada anchiala', Subspecies_Form: 'ecuadorina' }),
  row({ SPECIES: 'Oleria onega', Subspecies_Form: 'janarilla' }),
  row({ SPECIES: 'Ithomia salapia', Subspecies_Form: 'salapia' }),
  row({ SPECIES: 'Ithomia salapia', Subspecies_Form: 'derasa' }),
  row({ SPECIES: 'Godyris zavaleta', Subspecies_Form: 'matronalis' }),
  row({ SPECIES: 'Godyris dircenna', Subspecies_Form: 'dircenna' }),
  row({ SPECIES: 'Heliconius numata', Subspecies_Form: 'NA' }),
])

// Waypoints from the Wikiloc export of 26 Sep 2026 (FCH).
const GPX = `<?xml version="1.0" encoding="UTF-8"?><gpx creator="Wikiloc" version="1.1" xmlns="http://www.topografix.com/GPX/1/1">
<wpt lat="-0.950925" lon="-77.869495"><ele>605.214</ele><name><![CDATA[M1 Hyposcada illinissa ida hembra 9:20 0.5m NO id: B69]]></name><cmt><![CDATA[M1 Hyposcada illinissa ida hembra 9:20 0.5m NO id: B69]]></cmt><desc><![CDATA[]]></desc></wpt>
<wpt lat="-0.952142" lon="-77.867853"><ele>598.232</ele><name><![CDATA[M6 Hyposcada anchiala ecuadorina hembra 9:48 2.20m NC id B64 recaptura]]></name></wpt>
<trk><name>Monitoreo</name><trkseg>
<trkpt lat="-0.950528" lon="-77.869962"><ele>601</ele><time>2026-09-26T14:11:55Z</time></trkpt>
<trkpt lat="-0.950562" lon="-77.869942"><ele>601</ele><time>2026-09-26T16:01:37Z</time></trkpt>
</trkseg></trk></gpx>`

describe('GPX files', () => {
  it('reads waypoints and the track, with times in Ecuador', () => {
    const gpx = parseGpx(GPX)
    expect(gpx.name).toBe('Monitoreo')
    expect(gpx.waypoints).toHaveLength(2)
    expect(gpx.waypoints[0].text).toBe('M1 Hyposcada illinissa ida hembra 9:20 0.5m NO id: B69')
    expect(gpx.track).toHaveLength(2)
    expect(trackSpan(gpx.track)).toEqual({ date: '2026-09-26', start: 9 * 60 + 11, end: 11 * 60 + 1 })
  })
  it('rejects files that are not GPX', () => {
    expect(() => parseGpx('<html></html>')).toThrow()
  })
})

describe('waypoint notes', () => {
  it.each([
    ['M1 Hyposcada illinissa ida hembra 9:20 0.5m NO id: B69', 'Hyposcada illinissa', 'ida', 'female', 560, 0.5, CLOUD.CD, 'B69'],
    ['M2 oleria onega janarilla macho 9:25 1.25m NO id b70', 'Oleria onega', 'janarilla', 'male', 565, 1.25, CLOUD.CD, 'B70'],
    ['M3 ithomia salapia salapia hembra 9:31 NO 1.70m id B71', 'Ithomia salapia', 'salapia', 'female', 571, 1.7, CLOUD.CD, 'B71'],
    [
      'M4 hyposcada illinisa ida hembra 9:37 NO 1.70m id B56 recaptura',
      'Hyposcada illinissa',
      'ida',
      'female',
      577,
      1.7,
      CLOUD.CD,
      'B56',
    ],
    [
      'M5 Hyposcada anchiala ecuadorina hembra 9:44 NC ID B72',
      'Hyposcada anchiala',
      'ecuadorina',
      'female',
      584,
      null,
      CLOUD.CL,
      'B72',
    ],
    [
      'M8 Godyris zavaleta matronalis hembra 10:54 parches 1.5m id B73',
      'Godyris zavaleta',
      'matronalis',
      'female',
      654,
      1.5,
      CLOUD.SC,
      'B73',
    ],
    ['M9 Godyris dircenna female 10:58 parches 1.7m', 'Godyris dircenna', null, 'female', 658, 1.7, CLOUD.SC, null],
  ])('%s', (note, species, subspecies, sex, minutes, height, cloud, mark) => {
    const c = parseCapture(note, taxa)
    expect(c).toMatchObject({ species, subspecies, sex, minutes, height, cloud, markId: mark, known: true, rest: '' })
  })
  it('flags recaptures and names it does not know', () => {
    expect(parseCapture('M4 hyposcada illinisa ida hembra 9:37 id B56 recaptura', taxa).recaptureNote).toBe(true)
    const c = parseCapture('M7 heliconius numata bicoloratus macho 10:37 2.5m parches', taxa)
    expect(c).toMatchObject({ species: 'Heliconius numata', subspecies: 'bicoloratus', known: false, sex: 'male' })
  })
})

describe('locating a capture', () => {
  it('a point far from the trail is on no section', () => {
    const far = locateCapture({ lat: -0.96, lon: -77.87, ele: null, time: null, text: 'M1 Oleria onega macho 9:00' }, taxa)
    expect(far.section).toBeNull()
  })
})

describe('sheet values', () => {
  it('fills a marked capture like the existing monitoring rows', () => {
    const c = parseCapture('M4 hyposcada illinisa ida hembra 9:37 NO 1.70m id B56 recaptura', taxa)
    const v = captureValues(c, { date: '2026-09-26', collector: 'FCH - Franz Chandi', section: 4 })
    expect(v).toMatchObject({
      Release_Collect: 'Mark_Released',
      FieldMark_ID: 'B56',
      SPECIES: 'Hyposcada illinissa',
      Subspecies_Form: 'ida',
      Transect_section: 4,
      Collection_date: 46291,
      Cloud_cover: CLOUD.CD,
      Rainfall: 'DY_(dry)',
      Flight_height: 1.7,
      Tube_1_tissue: 'NOT_COLLECTED',
    })
    expect(v.Collection_time).toBeCloseTo(577 / 1440)
  })
  it('leaves sample fields open for preserved captures', () => {
    const v = captureValues(parseCapture('M9 Godyris dircenna female 10:58 parches 1.7m', taxa), {
      date: '2026-09-26',
      collector: 'FCH - Franz Chandi',
      section: 4,
    })
    expect(v.Release_Collect).toBe('Collected_Preserved')
    expect(v.FieldMark_ID).toBe('NA')
    expect('CAM_ID' in v).toBe(false)
  })
})

describe('summaries', () => {
  const rows = [
    row({
      SPECIES: 'Oleria onega',
      Release_Collect: 'Mark_Released',
      FieldMark_ID: 'B55',
      Collection_date: 46223,
      Transect_section: 4,
      Sex: 'male',
    }),
    row({
      SPECIES: 'Oleria onega',
      Release_Collect: 'Mark_Released',
      FieldMark_ID: 'B55',
      Collection_date: 46284,
      Transect_section: 3,
      Sex: 'male',
    }),
    row({
      SPECIES: 'Oleria onega',
      Release_Collect: 'Collected_Preserved',
      FieldMark_ID: 'NA',
      Collection_date: 46284,
      Sex: 'female',
    }),
    row({
      SPECIES: 'Godyris zavaleta',
      Release_Collect: 'Mark_Released',
      FieldMark_ID: 'B68',
      Collection_date: 46288,
      Transect_section: 4,
      Sex: 'female',
    }),
  ]
  it('counts a repeated field mark as a recapture', () => {
    expect([...recaptureIds(rows)]).toEqual([rows[1].id])
    const onega = speciesStats(rows, false).find(s => s.species === 'Oleria onega')!
    expect(onega).toMatchObject({ preserved: 1, marked: 1, recaptured: 1, female: 1, male: 2, total: 3 })
    const history = markHistories(rows)
    expect(history).toHaveLength(1)
    expect(history[0].events.map(e => e.section)).toEqual(['4', '3'])
  })
  it('suggests the next mark of the current series', () => {
    expect(nextMarkId(rows)).toBe('B69')
    expect(nextMarkId([row({ FieldMark_ID: 'A99', Collection_date: 1 })])).toBe('B1')
  })
  it('counts individuals per month and section', () => {
    const table = sectionsByMonth(rows)
    expect(table.get('2026-09')).toEqual([1, 0, 0, 1, 1])
    expect(table.get('2026-07')).toEqual([0, 0, 0, 0, 1])
  })
})

describe('marks and the 30 rule', () => {
  it('treats a mark reused on another species as a conflict, not a recapture', () => {
    const rows = [
      row({ SPECIES: 'Godyris zavaleta', Release_Collect: 'Mark_Released', FieldMark_ID: 'B56', Collection_date: 46223 }),
      row({ SPECIES: 'Hyposcada illinissa', Release_Collect: 'Mark_Released', FieldMark_ID: 'B56', Collection_date: 46284 }),
      row({ SPECIES: 'Hyposcada illinissa', Release_Collect: 'Mark_Released', FieldMark_ID: 'B56', Collection_date: 46291 }),
    ]
    expect([...recaptureIds(rows)]).toEqual([rows[2].id])
    expect(markConflicts(rows).map(c => c.id)).toEqual(['B56'])
    expect(markHistories(rows).map(h => h.species)).toEqual(['Hyposcada illinissa'])
  })
  it('counts preserved butterflies from Ikiam, Casa de Lin and Mariposario Ikiam, whatever the purpose', () => {
    const counts = preservedForRule([
      row({ SPECIES: 'Oleria gunilla', Release_Collect: 'Collected_Preserved' }),
      row({ SPECIES: 'Oleria gunilla', Release_Collect: 'Collected_Preserved', Purpose: 'Ikiam trapping inventory' }),
      row({
        SPECIES: 'Oleria gunilla',
        Release_Collect: 'Collected_Preserved',
        Collection_location: 'Casa de Lin',
        Purpose: 'NA',
      }),
      row({ SPECIES: 'Oleria gunilla', Release_Collect: 'Collected_Preserved', Collection_location: 'Mariposario Ikiam' }),
      row({ SPECIES: 'Oleria gunilla', Release_Collect: 'Collected_Preserved', Collection_location: 'Apuya' }),
      row({ SPECIES: 'Oleria gunilla', Release_Collect: 'Mark_Released', FieldMark_ID: 'B1' }),
    ])
    expect(counts.get('Oleria gunilla')).toBe(4)
  })
})

describe('recaptures written only in notes', () => {
  const marked = row({
    SPECIES: 'Hyposcada illinissa',
    Subspecies_Form: 'ida',
    Sex: 'male',
    Release_Collect: 'Mark_Released',
    FieldMark_ID: 'M45',
    Collection_date: 45455,
    Notes_Collection_data:
      '12/6/24 FCH: Monitoring by FCH butterfly M3 | 7/72024 AA: recatch&realease transect=4, date=7/7/24, time=9:59, collector=AA, Rainfall=DY, cloud_cover=CL_(cloudy_light), height=0.5m  | 16/8/2024 AA: recatch&realease transect=4, date=16/8/24, time=10:38, collector=AA, Rainfall=drizzle, cloud_cover=CD_(cloudy_dark), height=0.3m',
  })
  const others = [
    row({
      SPECIES: 'Hyposcada anchiala',
      FieldMark_ID: 'B2',
      Release_Collect: 'Mark_Released',
      Collection_date: 46332,
      Notes_Collection_data: '12-11-25 MJS: Recapture at 9:27 1m dry and sun ',
    }),
    row({
      SPECIES: 'Oleria gunilla',
      FieldMark_ID: 'B41',
      Release_Collect: 'Mark_Released',
      Collection_date: 46160,
      Notes_Collection_data: '20/5/26 AA: recatched -cloudy light-10:49- fligh H 1,5',
    }),
    // A note on the recapture row itself needs no new row.
    row({
      SPECIES: 'Hyposcada illinissa',
      FieldMark_ID: 'B39',
      Release_Collect: 'Mark_Released',
      Collection_date: 46227,
      Notes_Collection_data: '24/7/2026 MJS: Butterfly recatch, collected in transect #4 ',
    }),
  ]
  it('finds each recapture with its date, time, weather and height', () => {
    const found = noteRecaptures([marked, ...others])
    expect(
      found.map(f => [f.row.values.FieldMark_ID, f.date, f.minutes, f.height, f.cloud, f.rain, f.initials, f.section]),
    ).toEqual([
      ['M45', '2024-07-07', 599, 0.5, CLOUD.CL, 'DY_(dry)', 'AA', 4],
      ['M45', '2024-08-16', 638, 0.3, CLOUD.CD, 'DZ_(drizzle)', 'AA', 4],
      ['B2', '2025-11-12', 567, 1, CLOUD.S, 'DY_(dry)', 'MJS', null],
      ['B41', '2026-05-20', 649, 1.5, CLOUD.CL, null, 'AA', null],
    ])
  })
  it('proposes a new Mark_Released row that copies the marked individual', () => {
    const [first] = noteRecaptures([marked])
    const v = noteRecaptureValues(first, ['AA - Alex Arias', 'FCH - Franz Chandi'])
    expect(v).toMatchObject({
      Release_Collect: 'Mark_Released',
      FieldMark_ID: 'M45',
      SPECIES: 'Hyposcada illinissa',
      Subspecies_Form: 'ida',
      Sex: 'male',
      Collection_date: 45480,
      Collector: 'AA - Alex Arias',
      Transect_section: 4,
      Flight_height: 0.5,
    })
    expect(v.Notes_Collection_data).toContain('moved from the note of row')
  })
  it('skips recaptures that already have their own row', () => {
    const copy = row({
      SPECIES: 'Hyposcada illinissa',
      FieldMark_ID: 'M45',
      Release_Collect: 'Mark_Released',
      Collection_date: 45480,
    })
    expect(noteRecaptures([marked, copy]).map(f => f.date)).toEqual(['2024-08-16'])
  })
})

describe("other collectors' notes", () => {
  it('reads a mark written on its own', () => {
    const c = parseCapture('B51 9:51 female sol 1,5m Hyposcada illinissa ida', taxa)
    expect(c).toMatchObject({
      markId: 'B51',
      minutes: 591,
      height: 1.5,
      cloud: CLOUD.S,
      species: 'Hyposcada illinissa',
      sex: 'female',
    })
    expect(parseCapture('M1 Hyposcada illinissa ida hembra 9:20 0.5m NO id: B69', taxa).seq).toBe(1)
  })
  it('matches a point without species to its row by time and sex, and takes the sheet values for the map', () => {
    const sheet = [
      row({
        SPECIES: 'Godyris dircenna',
        Subspecies_Form: 'dircenna',
        Sex: 'female',
        Collection_date: 46286,
        Collection_time: 562 / 1440,
        Transect_section: 1,
      }),
      row({ SPECIES: 'Oleria onega', Sex: 'male', Collection_date: 46286, Collection_time: 567 / 1440 }),
    ]
    const c = { ...parseCapture('9:22 sol 2m female', taxa), section: 2 }
    const found = existingRow(sheet, '2026-09-21', c)
    expect(found?.values.SPECIES).toBe('Godyris dircenna')
    expect(withSheetValues(c, found)).toMatchObject({
      species: 'Godyris dircenna',
      subspecies: 'dircenna',
      sex: 'female',
      section: 1,
    })
    expect(existingRow(sheet, '2026-09-21', { ...parseCapture('9:27 sol male 1,5m', taxa) })?.values.SPECIES).toBe('Oleria onega')
  })
})

describe('live report', () => {
  const rows = [
    row({
      SPECIES: 'Oleria onega',
      Release_Collect: 'Collected_Preserved',
      Collection_date: 46284,
      Collection_time: 0.4,
      Flight_height: 0.3,
      Cloud_cover: 'CL_(cloudy_light)',
      Collector: 'AA - Alex Arias',
    }),
    row({
      SPECIES: 'Oleria onega',
      Release_Collect: 'Mark_Released',
      FieldMark_ID: 'B1',
      Collection_date: 46284,
      Collection_time: 0.45,
      Flight_height: 1.2,
      Cloud_cover: 'S&C_(sun_&_cloud_patches)',
      Collector: 'AA - Alex Arias',
    }),
    row({
      SPECIES: 'Oleria onega',
      Release_Collect: 'Mark_Released',
      FieldMark_ID: 'B1',
      Collection_date: 46300,
      Collection_time: 0.4,
      Flight_height: '3',
      Collector: 'MJS - María José Sánchez',
    }),
  ]
  it('lists every month of a range', () => {
    expect(monthRange('2025-11', '2026-02')).toEqual(['2025-11', '2025-12', '2026-01', '2026-02'])
  })
  it('splits each month by preserved, marked and recaptured', () => {
    const k = kindsByMonth(rows, ['2026-09', '2026-10'], recaptureIds(rows))
    expect([k.preserved, k.marked, k.recaptured]).toEqual([
      [1, 0],
      [1, 0],
      [0, 1],
    ])
  })
  it('counts monitoring days per collector from SamplingDay_data and captures', () => {
    const day = (values: Record<string, CellValue>) => ({ ...row(values), values })
    const days = effortDays(
      [
        day({ Date: 46284, Location: 'Ikiam', Purpose: 'Monitoring', Collectors_initials: 'AA' }),
        day({ Date: 46290, Location: 'Ikiam', Purpose: 'Monitoring', Collectors_initials: 'FCH' }),
        day({ Date: 375004, Location: 'Ikiam', Purpose: 'Monitoring', Collectors_initials: 'AA' }),
      ],
      rows,
    )
    expect([...days].sort()).toEqual(['2026-09-19|AA', '2026-09-25|FCH', '2026-10-05|MJS'])
  })
  it('counts captures by hour, flight height and cloud cover', () => {
    expect(byHour(rows).slice(2, 4)).toEqual([2, 1])
    expect(byHeight(rows)).toEqual([1, 0, 1, 0, 0, 1])
    expect(byCloud(rows)).toEqual([0, 1, 1, 0])
    expect(median([5, 1, 9, 3])).toBe(4)
  })
})

describe('report figures', () => {
  const r = (sp: string, date: number, extra: Record<string, CellValue> = {}) =>
    row({ SPECIES: sp, Collection_date: date, ...extra })
  const rows = [
    r('A a', 46000, { Transect_section: 4 }),
    r('B b', 46000, { Transect_section: 4 }),
    r('A a', 46010),
    r('C c', 46040, { Transect_section: 1 }),
  ]
  it('accumulates species day by day', () => {
    expect(speciesAccumulation(rows).map(p => p.species)).toEqual([2, 2, 3])
    expect(rareSpecies(rows)).toEqual({ once: 2, twice: 1 })
  })
  it('gives individuals per monitoring day by calendar month', () => {
    const s = seasonality(rows, ['2025-12-09|FCH', '2025-12-10|AA', '2026-01-18|AA'], ['A a'])
    // Two individuals over two December days; none in January.
    expect(s.values[0][11]).toBe(1)
    expect(s.values[0][0]).toBe(0)
  })
  it('splits species by transect section', () => {
    expect(speciesBySection(rows, ['A a'])).toEqual({ bySpecies: [[0, 0, 0, 1]], other: [1, 0, 0, 1] })
  })
  it('measures how far a marked individual moved between captures', () => {
    const d = recaptureDistances(
      [
        { markId: 'B56', species: 'H i', date: '2026-07-20', lat: 0, lon: 0 },
        { markId: 'B56', species: 'H i', date: '2026-09-26', lat: 0, lon: 0.001 },
        { markId: 'B56', species: 'G z', date: '2026-08-01', lat: 1, lon: 1 },
      ],
      (a, b) => Math.hypot(a[0] - b[0], a[1] - b[1]) * 111_000,
    )
    expect(d).toEqual([{ id: 'B56', species: 'H i', from: '2026-07-20', to: '2026-09-26', metres: 111 }])
  })
})

describe('marks anywhere in the note', () => {
  it('reads an M mark at the end, but an M number at the start is the point number', () => {
    expect(parseCapture('Oleria onega j macho 1.2m nc dry t4 M61', taxa)).toMatchObject({ markId: 'M61', height: 1.2 })
    expect(parseCapture('M84 Hembra m84 1m', taxa)).toMatchObject({ seq: 84, markId: 'M84' })
    expect(parseCapture('M2 oleria onega janarilla macho 9:25 1.25m NO id b70', taxa)).toMatchObject({ seq: 2, markId: 'B70' })
    expect(parseCapture('M4 hyposcada illinisa ida hembra 9:37 NO 1.70m id B56 recaptura', taxa).markId).toBe('B56')
  })
})
