import { describe, expect, it } from 'vitest'
import {
  CLOUD,
  captureValues,
  locateCapture,
  markHistories,
  nextMarkId,
  parseCapture,
  parseGpx,
  recaptureIds,
  sectionsByMonth,
  speciesStats,
  taxaFrom,
  trackSpan,
} from '../monitoring'
import { nearestSection } from '../transects'
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
    ['M9 Godyris dircenna female 10:58 parches 1.7m', 'Godyris dircenna', 'dircenna', 'female', 658, 1.7, CLOUD.SC, null],
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

describe('transect sections', () => {
  it('places captures on the trail section they were taken in', () => {
    expect(nearestSection(-0.950925, -77.869495).section).toBe(4)
    expect(nearestSection(-0.952142, -77.867853).section).toBe(3)
    expect(nearestSection(-0.9528, -77.8655).section).toBe(2)
    expect(nearestSection(-0.9521, -77.86432).section).toBe(1)
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
      Notes_Collection_data: '26/9/2026 FCH: Recapture',
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
