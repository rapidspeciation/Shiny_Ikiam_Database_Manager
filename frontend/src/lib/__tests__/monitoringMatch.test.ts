import { describe, expect, it } from 'vitest'
import { doubtfulMatch, existingRow, matchWalk, parseCapture, storedPoints, taxaFrom } from '../monitoring'
import type { CellValue, TableRow } from '../types'

let n = 6866
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
  row({ SPECIES: 'Hypothyris euclea', Subspecies_Form: 'intermedia' }),
  row({ SPECIES: 'Hypothyris anastasia', Subspecies_Form: 'bicolora' }),
  row({ SPECIES: 'Mechanitis polymnia', Subspecies_Form: 'proceriformis' }),
  row({ SPECIES: 'Mechanitis messenoides', Subspecies_Form: 'deceptus' }),
  row({ SPECIES: 'Methona confusa', Subspecies_Form: 'psamathe' }),
  row({ SPECIES: 'Eresia eunice' }),
  row({ SPECIES: 'Ithomia salapia', Subspecies_Form: 'salapia' }),
  row({ SPECIES: 'Ithomia amarilla' }),
  row({ SPECIES: 'Heliconius numata', Subspecies_Form: 'bicoloratus' }),
  row({ SPECIES: 'Godyris zavaleta', Subspecies_Form: 'matronalis' }),
  row({ SPECIES: 'Oleria onega', Subspecies_Form: 'janarilla' }),
  row({ SPECIES: 'Oleria gunilla', Subspecies_Form: 'lota' }),
  row({ SPECIES: 'Harjesia obscura' }),
])

describe('abbreviations in the notes', () => {
  it.each([
    ['Pol p male 9:16 sol 2m', 'Mechanitis polymnia', 'proceriformis'],
    ['M2 mech polymnia? Hembra 9:55 1.5 m parches', 'Mechanitis polymnia', 'proceriformis'],
    ['M5 hembra hyp anast bicolora 9:55 parches 2m', 'Hypothyris anastasia', 'bicolora'],
    ['Numata nc 10:16 1.6m', 'Heliconius numata', 'bicoloratus'],
    ['A77 O.gunilla lota male 10:21 sol 0,5m', 'Oleria gunilla', 'lota'],
    ['B22 sol 10:26 1m female god zav m', 'Godyris zavaleta', 'matronalis'],
    ['B18 Eucle Intermedia seco, sol, 9:46, 1 m macho', 'Hypothyris euclea', 'intermedia'],
    ['Decep female 10:08 parches 1,8m', 'Mechanitis messenoides', 'deceptus'],
    ['M11 bicolora hembra 10:53 parches 0.5m', 'Hypothyris anastasia', 'bicolora'],
    ['Onega 0,4m parches male 10:30 A62', 'Oleria onega', 'janarilla'],
  ])('%s', (note, species, subspecies) => {
    expect(parseCapture(note, taxa, taxa)).toMatchObject({ species, subspecies, known: true })
  })
  it('does not read weather or other words as names', () => {
    // "obscuro" is not Harjesia obscura, "flor" is not a Pseudoscada.
    expect(parseCapture('B33 hembra seco obscuro 0.5m 10:06', taxa, taxa)).toMatchObject({ species: null, cloud: 'CD_(cloudy_dark)' })
    expect(parseCapture('Eresia 0.5m 10:23 sun', taxa, taxa).species).toBe('Eresia eunice')
    expect(parseCapture('1 Seco claro 1m 8:50 fuera del monitoreo', taxa, taxa).species).toBeNull()
  })
  it('reads point numbers at the start and dictated times', () => {
    expect(parseCapture('4 4 50cm', taxa)).toMatchObject({ seq: 4, height: 0.5, minutes: null })
    expect(parseCapture('10 10 1m parches seco', taxa)).toMatchObject({ seq: 10, minutes: null })
    expect(parseCapture('3-4 10:47 seco claro 50cm', taxa)).toMatchObject({ seq: 3, count: 2, minutes: 647 })
    expect(parseCapture('Dos, 1.20 seco oscuro, 10.19', taxa)).toMatchObject({ seq: 2, minutes: 619 })
    expect(parseCapture('3ra mariposa', taxa).seq).toBe(3)
    expect(parseCapture('2 Seco, soleado, macho 2 m 10 03', taxa)).toMatchObject({ seq: 2, minutes: 603, height: 2, markId: null })
    expect(parseCapture('B42 recatch Macho seco, soleado 10 con 09', taxa)).toMatchObject({ minutes: 609, recaptureNote: true })
    expect(parseCapture('Seco, sol, nueve, 55,2 m', taxa).minutes).toBe(595)
  })
})

// Monitoreo 8/9/2025 (AA): the collector's rows of that day and the Wikiloc notes, in Wikiloc's order.
const day = (time: string, species: string | null, sex: string, extra: Record<string, CellValue> = {}) => {
  const [h, m] = time.split(':').map(Number)
  return row({
    Collector: 'AA - Alex Arias',
    Collection_date: 45908,
    Collection_time: (h * 60 + m) / 1440,
    SPECIES: species,
    Sex: sex,
    FieldMark_ID: 'NA',
    ...extra,
  })
}
n = 6866
const AA = [
  day('9:03', 'Hypothyris euclea', 'female'),
  day('9:06', 'Hypothyris euclea', 'female'),
  day('9:10', 'Ithomia amarilla', 'female', { FieldMark_ID: 'A68' }),
  day('9:12', 'Hypothyris euclea', 'male'),
  day('9:12', 'Hypothyris euclea', 'female'),
  day('9:14', 'Eresia eunice', 'female'),
  day('9:14', 'Hypothyris euclea', 'female'),
  day('9:15', 'Methona confusa', 'male'),
  day('9:16', 'Mechanitis polymnia', 'male'),
  day('9:23', 'Ithomia salapia', 'female', { FieldMark_ID: 'A69' }),
  day('9:27', 'Hypothyris euclea', 'female'),
  day('9:31', null, 'male'),
  day('9:40', 'Hypothyris euclea', 'female'),
  day('9:45', 'Melinaea satevis', 'female'),
  day('9:55', 'Hyposcada anchiala', 'female'),
  day('9:58', 'Ithomia salapia', 'male', { FieldMark_ID: 'A70' }),
  day('10:01', 'Hypothyris euclea', 'female', { FieldMark_ID: 'A71' }),
  day('10:05', 'Hypothyris euclea', 'female'),
  day('10:14', 'Hypothyris anastasia', 'female'),
  day('10:30', 'Hypothyris euclea', 'male'),
  day('10:30', 'Pseudoscada florula', 'male'),
  day('10:30', 'Hypothyris euclea', 'female'),
  day('10:38', 'Hypothyris euclea', 'female'),
  day('10:46', 'Pteronymia vestilla', 'female'),
  day('10:59', 'Hypothyris euclea', 'female'),
  day('10:59', 'Lycorea halia', 'female'),
  day('11:05', 'Heliconius numata', 'NOT_COLLECTED'),
  day('11:11', 'Hypothyris euclea', 'female'),
  day('11:14', 'Pseudoscada florula', 'female'),
  day('11:23', 'Hypothyris anastasia', 'male'),
  // Another collector the same day and minute: never used.
  row({ Collector: 'CR - Carlos Robalino', Collection_date: 45908, Collection_time: 674 / 1440, SPECIES: 'Greta andromica', Sex: 'female' }),
]
const NOTES = [
  'Female 9:03 sol 1m',
  'Female 9:06 sol 1.5m',
  'A68 sol female 9:10 1.7m',
  '10:12 sol 1.9m female',
  '9:14 sol female 0.3m',
  '9:15 sol 0.4m',
  'Pol p male 9:16 sol 2m',
  'A69 sol female 9:23 1m',
  '9:31 sol 1m male',
  '9:40 sol female 1.5m',
  '9:45 sol female 0.5m',
  '9:55 sol 1.5m A70 female',
  'A71 sol 9:58 male 1m',
  '11:01 sol 1m female',
  '10:30 sol 0.5m male',
  '10:30 sol 0,5m male',
  '10:30 sol 0,8m female',
  'Female 10:38 sol 1.5m',
  '10:46 sol 0,3m female',
  '10:59 sol 1m female',
  '11:05 sol 2.5m',
  '11:11 sol 0.5m female',
  '11:14 parches 0.5m female',
  '11:23 parches male 0,5m',
]

describe('pairing a walk with its rows (8 Sep 2025, AA)', () => {
  const points = NOTES.map(t => parseCapture(t, taxa, taxa))
  const m = matchWalk(AA, '2025-09-08', 'AA - Alex Arias', points)
  const at = (note: string) => m.matches[NOTES.indexOf(note)]
  const time = (r: TableRow) => Math.round((r.values.Collection_time as number) * 1440)

  it('takes the closest row, not the first within two minutes', () => {
    // The old pairing took the 9:12 row for "9:14 female", although two rows are at 9:14.
    expect(time(at('9:14 sol female 0.3m').rows[0])).toBe(554)
    expect(at('9:14 sol female 0.3m').confidence).toBe('tie')
    expect(at('9:14 sol female 0.3m').candidates.map(r => r.values.SPECIES)).toContain('Hypothyris euclea')
  })
  it('reads "Pol p" as Mechanitis polymnia and pairs it with that row, not with Methona at 9:15', () => {
    expect(at('Pol p male 9:16 sol 2m')).toMatchObject({ confidence: 'sure', conflicts: [] })
    expect(at('Pol p male 9:16 sol 2m').rows[0].values.SPECIES).toBe('Mechanitis polymnia')
    expect(at('9:15 sol 0.4m').rows[0].values.SPECIES).toBe('Methona confusa')
  })
  it('pairs marks first, and lists a mark whose row says another sex', () => {
    expect(at('A68 sol female 9:10 1.7m').confidence).toBe('mark')
    expect(at('9:55 sol 1.5m A70 female')).toMatchObject({ confidence: 'mark', conflicts: ['sexo', 'hora'] })
    expect(doubtfulMatch(at('9:55 sol 1.5m A70 female'))).toBe(true)
  })
  it('says which points are ties, and uses each row once', () => {
    expect(at('10:30 sol 0.5m male').confidence).toBe('tie')
    expect(at('10:30 sol 0,5m male').confidence).toBe('tie')
    expect(at('10:30 sol 0,8m female')).toMatchObject({ confidence: 'sure' })
    expect(at('10:59 sol 1m female').confidence).toBe('tie')
    const used = m.matches.flatMap(x => x.rows.map(r => r.id))
    expect(new Set(used).size).toBe(used.length)
    expect(m.matches.every(x => x.rows.every(r => r.values.Collector === 'AA - Alex Arias'))).toBe(true)
    expect(m.left).toHaveLength(0)
  })
  it('keeps a pairing chosen by a person and pairs the rest around it', () => {
    const chosen = AA.find(r => r.values.SPECIES === 'Hypothyris euclea' && time(r) === 554)!
    const fixed = new Map([[NOTES.indexOf('9:14 sol female 0.3m'), [chosen.id]]])
    const again = matchWalk(AA, '2025-09-08', 'AA - Alex Arias', points, { fixed })
    expect(again.matches[NOTES.indexOf('9:14 sol female 0.3m')]).toMatchObject({ manual: true, rows: [chosen] })
    expect(again.matches.filter(x => x.rows.includes(chosen))).toHaveLength(1)
  })
})

describe('ties and order', () => {
  const rows = [
    row({ Collector: 'FCH - Franz Chandi', Collection_date: 45342, Collection_time: 550 / 1440, SPECIES: 'Oleria gunilla', Sex: 'male' }),
    row({ Collector: 'FCH - Franz Chandi', Collection_date: 45342, Collection_time: 550 / 1440, SPECIES: 'Hypothyris anastasia', Sex: 'male' }),
    row({ Collector: 'FCH - Franz Chandi', Collection_date: 45342, Collection_time: 575 / 1440, SPECIES: 'Oleria onega', Sex: 'female' }),
    row({ Collector: 'FCH - Franz Chandi', Collection_date: 45342, Collection_time: 590 / 1440, SPECIES: 'Ithomia salapia', Sex: 'female' }),
    row({ Collector: 'FCH - Franz Chandi', Collection_date: 45342, Collection_time: 600 / 1440, SPECIES: 'Oleria onega', Sex: 'male' }),
  ]
  it('marks two identical notes at the same minute as a tie, in walk order', () => {
    const points = ['M1 macho 9:10 parches 1m', 'M2 macho 9:10 1.5m parches'].map(t => parseCapture(t, taxa, taxa))
    const m = matchWalk(rows, '2024-02-20', 'FCH - Franz Chandi', points)
    expect(m.matches.map(x => x.confidence)).toEqual(['tie', 'tie'])
    expect(m.matches.map(x => x.rows[0].row)).toEqual([rows[0].row, rows[1].row])
  })
  it('places a point without a time between its neighbours, skipping rows of another sex', () => {
    const points = ['M1 macho 9:10', 'M2 macho 1m', 'M3 hembra', 'M4 macho 10:00'].map(t => parseCapture(t, taxa, taxa))
    const m = matchWalk(rows, '2024-02-20', 'FCH - Franz Chandi', points)
    // M2 and M3 have no time: placed in order among the free rows between 9:10 and 10:00, by sex.
    expect(m.matches[1]).toMatchObject({ confidence: 'order' })
    expect(m.matches[1].rows[0].values.Sex).toBe('male')
    expect(m.matches[2]).toMatchObject({ confidence: 'order' })
    expect(m.matches[2].rows[0].values.Sex).toBe('female')
    expect(m.matches[2].candidates.length).toBeGreaterThan(0)
    expect(doubtfulMatch(m.matches[2])).toBe(true)
  })
  it('reads a stored walk again: copies of one note are one point', () => {
    const stored = [
      { text: 'Mariposa 1 y 2', lat: 1, lon: 2, minutes: null },
      { text: 'Mariposa 1 y 2', lat: 1, lon: 2, minutes: null },
      { text: 'Marip 3', lat: 1, lon: 3, minutes: 580 },
    ]
    const { points, groups } = storedPoints(stored, taxa)
    expect(groups).toEqual([[0, 1], [2]])
    expect(points[0].count).toBe(2)
    // The time of "Marip 3" came from the GPS track: only a hint for the order.
    expect(points[1]).toMatchObject({ minutes: 580, timeFromTrack: true })
  })
  it('finds a single capture by its closest minute', () => {
    const c = parseCapture('9:11 macho', taxa)
    expect(existingRow(rows, '2024-02-20', c)?.row).toBe(rows[0].row)
    expect(existingRow(rows, '2024-02-20', parseCapture('9:59 hembra', taxa))).toBeNull()
  })
})
