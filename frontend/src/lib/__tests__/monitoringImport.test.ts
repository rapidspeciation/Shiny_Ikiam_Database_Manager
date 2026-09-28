import { describe, expect, it } from 'vitest'
import {
  CLOUD,
  captureValues,
  collectorFromName,
  collectorLabel,
  identificationFollows,
  locateCapture,
  markConflicts,
  markHistories,
  monitoringCollectors,
  parseCapture,
  parseGpx,
  recaptureIds,
  reviewCapture,
  samplingDayRow,
  taxaFrom,
  timesFromTrack,
  walkMarkRoles,
  type TrackPoint,
} from '../monitoring'
import { isoToSerial } from '../dates'
import type { CellValue, TableRow } from '../types'

/** Checks behind the 28 Sep 2026 test of importing the 21 and 23 Sep walks. */
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
const day = (iso: string) => isoToSerial(iso)

// Names in the whole sheet, and those seen at Ikiam (fewer subspecies).
const sheet = [
  row({ SPECIES: 'Hyposcada illinissa', Subspecies_Form: 'ida' }),
  row({ SPECIES: 'Hyposcada illinissa', Subspecies_Form: 'idoides', Collection_location: 'Apuya' }),
  row({ SPECIES: 'Godyris zavaleta', Subspecies_Form: 'matronalis' }),
  row({ SPECIES: 'Godyris zavaleta', Subspecies_Form: 'telesilla', Collection_location: 'Mindo' }),
  row({ SPECIES: 'Godyris dircenna', Subspecies_Form: 'dircenna' }),
  row({ SPECIES: 'Methona confusa', Subspecies_Form: 'psamathe' }),
  row({ SPECIES: 'Methona confusa', Subspecies_Form: 'confusa', Collection_location: 'Tarapoto' }),
  row({ SPECIES: 'Mechanitis messenoides', Subspecies_Form: 'deceptus' }),
  row({ SPECIES: 'Oleria gunilla', Subspecies_Form: 'lota' }),
  row({ SPECIES: 'Oleria onega', Subspecies_Form: 'janarilla' }),
  row({ SPECIES: 'Hyposcada anchiala', Subspecies_Form: 'ecuadorina' }),
]
const taxa = taxaFrom(sheet)
const ikiam = taxaFrom(sheet.filter(r => r.values.Collection_location === 'Ikiam'))

describe('notes of the 21 and 23 Sep 2026 walks', () => {
  it('resolves an abbreviated genus against the names seen at Ikiam, with the start of a subspecies', () => {
    expect(parseCapture('B62 11:07 sol female 1m god. Zavaleta m', taxa, ikiam)).toMatchObject({
      species: 'Godyris zavaleta',
      subspecies: 'matronalis',
      known: true,
      rest: '',
      markId: 'B62',
    })
    expect(parseCapture('11:01 sol male 1,6m m. Confusa', taxa, ikiam)).toMatchObject({
      species: 'Methona confusa',
      known: true,
    })
  })
  it('completes a subspecies written by its start ("id" → ida)', () => {
    const c = parseCapture('B59 10:14 parches male 1,5m Hyposcada illinissa id', taxa, ikiam)
    expect(c).toMatchObject({ species: 'Hyposcada illinissa', subspecies: 'ida', known: true, subspeciesGuess: 'prefix' })
  })
  it('fills the subspecies when the species has only one at Ikiam', () => {
    expect(parseCapture('11:01 sol male 1,6m m. Confusa', taxa, ikiam)).toMatchObject({
      subspecies: 'psamathe',
      subspeciesGuess: 'ikiam',
    })
    expect(parseCapture('M3 godyris zavaleta hembra 9:54 Nublado claro 2m id:B66', taxa, ikiam).subspecies).toBe('matronalis')
    // Without the Ikiam names nothing is guessed.
    expect(parseCapture('M3 godyris zavaleta hembra 9:54 Nublado claro 2m id:B66', taxa).subspecies).toBeNull()
  })
  it('takes a whole weather phrase, leaving nothing for the notes', () => {
    const c = parseCapture('M1 Hyposcada anchiala ecuadorina hembra 2m 9:40 parches nube y sol ID: B64', taxa, ikiam)
    expect(c).toMatchObject({ cloud: CLOUD.SC, rest: '', markId: 'B64', subspecies: 'ecuadorina' })
    expect(parseCapture('M2 Oleria gunilla macho 1m 9:47 sol y nubes', taxa).cloud).toBe(CLOUD.SC)
    expect(parseCapture('M2 Oleria gunilla macho 1m 9:47 sol y nubes', taxa).rest).toBe('')
  })
  it('reads a missing time from the GPS track, on the pass between the neighbouring points', () => {
    const at = (lat: number, lon: number, text: string) => locateCapture({ lat, lon, ele: null, time: null, text }, taxa, ikiam)
    // Out and back on the same trail: the point is passed at 9:30 and at 10:12.
    const track: TrackPoint[] = [
      [-0.95, -77.87, null, '2026-09-23T14:00:00Z'],
      [-0.951, -77.869, null, '2026-09-23T14:30:00Z'],
      [-0.952, -77.868, null, '2026-09-23T14:50:00Z'],
      [-0.951, -77.869, null, '2026-09-23T15:12:00Z'],
      [-0.95, -77.87, null, '2026-09-23T15:40:00Z'],
    ]
    const captures = timesFromTrack(
      [
        at(-0.952, -77.868, 'M4 Hyposcada illinissa ida hembra 10:00 NC id: B67 1m'),
        at(-0.95101, -77.86901, 'M5 Hyposcada illinisa ida macho NC 1m id: B68'),
        at(-0.95, -77.87, 'M6 macho NC 1m 10:52'),
      ],
      track,
    )
    expect(captures[1]).toMatchObject({ minutes: 10 * 60 + 12, timeFromTrack: true })
    expect(captures[0].timeFromTrack).toBeUndefined()
    const review = reviewCapture(captures[1], 1, {
      rows: [],
      date: '2026-09-23',
      captures,
      preserved: new Map(),
      isIthomiini: () => true,
    })
    expect(review.list.map(k => k.text)).toContain('Hora del GPS (10:12): la nota no la dice')
    // No GPS times (a Wikiloc page): left without.
    expect(
      timesFromTrack(
        captures.slice(1, 2).map(c => ({ ...c, minutes: null, timeFromTrack: undefined })),
        [],
      )[0].minutes,
    ).toBeNull()
  })
  it('reads the GPX author', () => {
    const gpx = parseGpx(
      '<gpx><metadata><name>Monitoreo 21/sep/2026</name><author><name>Alex Paul Arias Cruz</name><link href="https://www.wikiloc.com/wikiloc/user.do?id=13756119"/></author></metadata><trk><name>Monitoreo 21/sep/2026</name><trkseg><trkpt lat="-0.95" lon="-77.87"/></trkseg></trk></gpx>',
    )
    expect(gpx.name).toBe('Monitoreo 21/sep/2026')
    expect(gpx.author).toEqual({ name: 'Alex Paul Arias Cruz', id: '13756119' })
  })
})

describe('collectors', () => {
  const people = [
    'AA - Alex Arias',
    'FCH - Franz Chandi',
    'MJS - María José Sánchez',
    'NA - Missing data',
    'CR - Carlos Robalino',
    'CR - Cesar Ramirez',
  ]
  it('finds the collector by initials or name, never by initials two people share', () => {
    expect(collectorFromName('Monitoreo ithomidos 23 sep 2026 FCH', people)).toBe('FCH - Franz Chandi')
    expect(collectorFromName('Monitoreo Maria Jose Sanchez', people)).toBe('MJS - María José Sánchez')
    expect(collectorFromName('Monitoreo CR 2 oct', people)).toBeNull()
    expect(collectorFromName('Monitoreo NA', people)).toBeNull()
  })
  it('lists the monitoring collectors first, without NA, and names shared initials in full', () => {
    const rows = [
      row({ Collector: 'AA - Alex Arias', Collection_date: day('2026-09-21') }),
      row({ Collector: 'AA - Alex Arias', Collection_date: day('2026-09-19') }),
      row({ Collector: 'MJS - María José Sánchez', Collection_date: day('2026-08-19') }),
      row({ Collector: 'CR - Carlos Robalino', Collection_date: day('2019-01-01') }),
    ]
    const days = [row({ Date: day('2026-09-23'), Purpose: 'Monitoring', Location: 'Ikiam', Collectors_initials: 'FCH' })]
    const list = monitoringCollectors(people, rows, days)
    expect(list.usual).toEqual(['AA - Alex Arias', 'MJS - María José Sánchez', 'FCH - Franz Chandi'])
    expect(list.others).toEqual(['CR - Carlos Robalino', 'CR - Cesar Ramirez'])
    expect(collectorLabel('CR - Cesar Ramirez', people)).toBe('CR - Cesar Ramirez')
    expect(collectorLabel('AA - Alex Arias', people)).toBe('AA')
  })
})

describe('recaptures: same mark, species and sex; the series goes on', () => {
  const mark = (FieldMark_ID: string, date: string, SPECIES: string, Sex: string) =>
    row({ Release_Collect: 'Mark_Released', FieldMark_ID, Collection_date: day(date), SPECIES, Sex })
  // May–July: B40–B61. August: the numbering restarts at B40.
  const sheetMarks = [
    mark('B40', '2026-05-18', 'Oleria onega', 'female'),
    mark('B41', '2026-05-18', 'Oleria gunilla', 'male'),
    mark('B42', '2026-05-20', 'Oleria onega', 'male'),
    mark('B51', '2026-06-24', 'Ithomia salapia', 'female'),
    mark('B58', '2026-07-22', 'Oleria onega', 'male'),
    mark('B59', '2026-07-24', 'Oleria onega', 'male'),
    mark('B60', '2026-07-24', 'Hyposcada illinissa', 'female'),
    mark('B61', '2026-07-24', 'Hyposcada illinissa', 'male'),
    mark('B40', '2026-08-17', 'Hyposcada illinissa', 'female'),
    mark('B41', '2026-08-17', 'Oleria gunilla', 'male'),
    mark('B42', '2026-08-17', 'Hyposcada illinissa', 'male'),
    mark('B51', '2026-08-19', 'Hyposcada illinissa', 'female'),
    mark('B57', '2026-09-19', 'Hyposcada illinissa', 'female'),
  ]
  const walk = [
    parseCapture('B51 9:51 female sol 1,5m Hyposcada illinissa ida', taxa, ikiam),
    parseCapture('B58 10:05 parches 0,5m male Oleria gunilla lota', taxa, ikiam),
    parseCapture('B59 10:14 parches male 1,5m Hyposcada illinissa id', taxa, ikiam),
    parseCapture('B60 10:35 parches male 1m Hyposcada illinissa ida', taxa, ikiam),
  ]
  it('tells recaptures from marks continuing the series and from IDs used twice', () => {
    const roles = walkMarkRoles(sheetMarks, '2026-09-21', walk)
    expect(roles.map(r => r?.role)).toEqual(['recapture', 'new', 'new', 'new'])
    expect(roles[0]?.first?.values.Collection_date).toBe(day('2026-08-19'))
    // B58–B60 follow B57: new butterflies although the numbers were used in July.
    expect(roles.slice(1).every(r => r?.continues)).toBe(true)
  })
  it('does not repeat "¿ID repetida?" for marks already on two species', () => {
    const at = (text: string) => ({
      ...parseCapture(text, taxa, ikiam),
      lat: -0.95,
      lon: -77.87,
      ele: null,
      section: 4,
      sectionDistance: 0,
      photos: [],
    })
    const captures = [at('B51 9:51 female sol 1,5m Hyposcada illinissa ida'), at('B40 9:58 female sol 1,5m Oleria onega')]
    const ctx = { rows: sheetMarks, date: '2026-09-21', captures, preserved: new Map(), isIthomiini: () => true }
    expect(reviewCapture(captures[0], 0, ctx).list.map(k => k.text)).toEqual(['Recaptura de B51 (marcada 19-Aug-26)'])
    // B40 was Oleria onega in May and H. illinissa in August: a recapture of May's female, no warning.
    const b40 = reviewCapture(captures[1], 1, ctx)
    expect(b40.recapture?.values.Collection_date).toBe(day('2026-05-18'))
    expect(b40.list.some(k => /ID repetida/.test(k.text))).toBe(false)
  })
  it('a different sex is another butterfly', () => {
    const at = (text: string) => ({
      ...parseCapture(text, taxa, ikiam),
      lat: -0.95,
      lon: -77.87,
      ele: null,
      section: 4,
      sectionDistance: 0,
      photos: [],
    })
    const captures = [at('B57 10:00 male sol 1m Hyposcada illinissa ida')]
    const review = reviewCapture(captures[0], 0, {
      rows: sheetMarks,
      date: '2026-09-21',
      captures,
      preserved: new Map(),
      isIthomiini: () => true,
    })
    expect(review.recapture).toBeNull()
    expect(review.list.map(k => k.text)).toContain(
      'B57 ya se usó para Hyposcada illinissa hembra (fila ' + sheetMarks[12].row + '): ¿ID repetida?',
    )
  })
  it('the report counts the same way', () => {
    const rows = [
      ...sheetMarks,
      mark('B51', '2026-09-21', 'Hyposcada illinissa', 'female'),
      mark('B60', '2026-09-21', 'Hyposcada illinissa', 'male'),
    ]
    const recaptured = recaptureIds(rows)
    expect(rows.filter(r => recaptured.has(r.id)).map(r => r.values.FieldMark_ID)).toEqual(['B51'])
    // The restarted B41 (Oleria gunilla male again) is a new butterfly, not May's.
    expect(recaptured.has(sheetMarks[9].id)).toBe(false)
    expect(markHistories(rows).map(h => `${h.id} ${h.events.length}`)).toEqual(['B51 2'])
    expect(markConflicts(rows).map(c => c.id)).toContain('B60')
  })
})

describe('new rows', () => {
  it('fills preserved and marked rows as the team does, with the next CAM and tube', () => {
    const preserved = captureValues(parseCapture('9:15 sol 3m male methona confusa psamathe', taxa, ikiam), {
      date: '2026-09-21',
      collector: 'AA - Alex Arias',
      section: 1,
      cam: 'CAM079881',
      tube: 'FS96926183',
    })
    const d = day('2026-09-21')
    expect(preserved).toMatchObject({
      Release_Collect: 'Collected_Preserved',
      FieldMark_ID: 'NA',
      CAM_ID: 'CAM079881',
      Tube_1_id: 'FS96926183',
      Tube_1_tissue: 'WHOLE_ORGANISM',
      Tube_2_id: 'NA',
      Tube_2_tissue: 'NOT_COLLECTED',
      Tube_3_id: 'NA',
      Tube_3_tissue: 'NOT_COLLECTED',
      Tube_4_id_LEGS: 'NA',
      Identifier: 'AA - Alex Arias',
      ID_status: 'COMPLETE',
      Bait: 'NA',
      Forest_stratum: 'NA',
      Death_date: d,
      Preservation_date: d,
      Preservation_medium: 'Flash frozen',
      Preserved_dead_alive: 'Alive',
      Splitted_body: 'No',
      Location_Head: 'Ikiam',
      Location_wings: 'Ikiam',
    })
    expect('Butterfly_weight' in preserved).toBe(false)
    expect('Country' in preserved).toBe(false)
    const marked = captureValues(parseCapture('B58 10:05 parches 0,5m male Oleria gunilla lota', taxa, ikiam), {
      date: '2026-09-21',
      collector: 'AA - Alex Arias',
      section: 3,
    })
    expect(marked).toMatchObject({
      Release_Collect: 'Mark_Released',
      CAM_ID: 'NA',
      Tube_1_id: 'NA',
      Tube_1_tissue: 'NOT_COLLECTED',
      Butterfly_weight: 'NA',
      Death_date: 'NA',
      Preservation_date: 'NA',
      Preservation_medium: 'NOT_COLLECTED',
      Preserved_dead_alive: 'NOT_PRESERVED',
      Splitted_body: 'No',
      Location_Head: 'NA',
      Location_Legs: 'NA',
    })
  })
  it('leaves an unidentified capture To_identify without Identifier, and follows the species typed later', () => {
    const v = captureValues(parseCapture('9:22 sol 2m female', taxa, ikiam), {
      date: '2026-09-21',
      collector: 'AA - Alex Arias',
      section: 1,
    })
    expect(v.ID_status).toBe('To_identify')
    expect('Identifier' in v).toBe(false)
    expect(identificationFollows({ ...v, SPECIES: 'Oleria tigilla' })).toEqual({
      ID_status: 'COMPLETE',
      Identifier: 'AA - Alex Arias',
    })
    expect(
      identificationFollows({ ...v, SPECIES: 'Oleria tigilla', ID_status: 'COMPLETE', Identifier: 'AA - Alex Arias' }),
    ).toEqual({})
    expect(identificationFollows({ ...v, SPECIES: null, ID_status: 'COMPLETE' })).toEqual({ ID_status: 'To_identify' })
  })
})

describe('SamplingDay_data', () => {
  const days = [
    row({
      Date: 375004,
      Location: 'Ikiam',
      Purpose: 'Monitoring',
      Start_time: 0.3819,
      End_time: 0.4722,
      Collectors_initials: 'AA',
      Notes: '21/9/2026 AA: Sunny',
    }),
    row({ Date: day('2026-09-19'), Location: 'Ikiam', Purpose: 'Monitoring', Collectors_initials: 'AA' }),
  ]
  it('finds the day of a collector, and a row whose date was typed wrong instead of adding it twice', () => {
    expect(samplingDayRow(days, '2026-09-19', 'AA')).toEqual({ row: days[1], broken: false })
    expect(samplingDayRow(days, '2026-09-21', 'AA')).toEqual({ row: days[0], broken: true })
    // By the GPS times when the note does not say the day.
    const noNote = [{ ...days[0], values: { ...days[0].values, Notes: null } }]
    expect(samplingDayRow(noNote, '2026-09-21', 'AA', { start: 552, end: 682 })?.broken).toBe(true)
    expect(samplingDayRow(noNote, '2026-09-21', 'AA', { start: 600, end: 700 })).toBeNull()
    expect(samplingDayRow(days, '2026-09-21', 'FCH')).toBeNull()
  })
})
