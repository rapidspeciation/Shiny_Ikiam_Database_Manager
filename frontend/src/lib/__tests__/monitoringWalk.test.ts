import { describe, expect, it } from 'vitest'
import { captureValues, fillInOrder, locateCapture, parseCapture, reviewCapture, sequenceFrom, taxaFrom, walkSpan } from '../monitoring'
import type { CellValue, TableRow } from '../types'

/** The notes of the October 2026 walks by the two collectors who started then (Wikiloc, 19 Oct 2026 test). */
let n = 0
function row(values: Record<string, CellValue>): TableRow {
  n++
  return { id: `r${n}`, row: n, version: 1, observed: true, values: { Purpose: 'Monitoring', Collection_location: 'Ikiam', ...values }, formulas: [] }
}
const sheet = [
  row({ SPECIES: 'Hyposcada illinissa', Subspecies_Form: 'ida' }),
  row({ SPECIES: 'Hyposcada illinissa', Subspecies_Form: 'idoides', Collection_location: 'Apuya' }),
  row({ SPECIES: 'Hyposcada anchiala', Subspecies_Form: 'ecuadorina' }),
  row({ SPECIES: 'Godyris zavaleta', Subspecies_Form: 'matronalis' }),
  row({ SPECIES: 'Godyris dircenna', Subspecies_Form: 'dircenna' }),
  row({ SPECIES: 'Methona confusa', Subspecies_Form: 'psamathe' }),
  row({ SPECIES: 'Oleria gunilla', Subspecies_Form: 'lota' }),
  row({ SPECIES: 'Oleria tigilla', Subspecies_Form: 'tigilla' }),
]
const taxa = taxaFrom(sheet)
const ikiam = taxaFrom(sheet.filter(r => r.values.Collection_location === 'Ikiam'))
const read = (note: string) => parseCapture(note, taxa, ikiam)

describe('notes of the October 2026 walks', () => {
  it('reads "ida 0.3m" as the subspecies and the height, not as the mark "id: A0"', () => {
    expect(read('10:04 B75 Hembra Hyposcada illinissa ida 0.3m parches')).toMatchObject({
      markId: 'B75',
      species: 'Hyposcada illinissa',
      subspecies: 'ida',
      height: 0.3,
      sex: 'female',
      minutes: 10 * 60 + 4,
    })
    expect(read('10:35 Hiposcada illinisa ida 1.0m B60 Macho Parches')).toMatchObject({ markId: 'B60', height: 1 })
    // The id prefix some collectors write still marks.
    expect(read('M1 Hyposcada illinissa hembra 9:05 0.5m NO id: B68').markId).toBe('B68')
    expect(read('id B12 Oleria gunilla').markId).toBe('B12')
  })

  it('takes a lone decimal as the height once the time is taken', () => {
    expect(read('Godyris dircena 1.70 10:58 parches Hembra')).toMatchObject({ height: 1.7, minutes: 10 * 60 + 58, species: 'Godyris dircenna' })
    expect(read('Hiposcada illinisa ida B51 Soleado 9:52 1.5')).toMatchObject({ height: 1.5, minutes: 9 * 60 + 52, markId: 'B51' })
    // A time written with a dot is still a time.
    expect(read('Oleria gunilla 10.19 1.2m')).toMatchObject({ minutes: 10 * 60 + 19, height: 1.2 })
  })

  it('leaves out the labels written before each part', () => {
    const c = read('Hora: 9:23 Altura de vuelo: 2.5 female Soleado')
    expect(c).toMatchObject({ species: null, minutes: 9 * 60 + 23, height: 2.5, sex: 'female', rest: '' })
    expect(read('Hora 9:15 Clima: soleado Altura de vuela: 3.10m Especie: metona confusa')).toMatchObject({
      species: 'Methona confusa',
      height: 3.1,
      rest: '',
    })
  })

  it('a moth is no butterfly name', () => {
    expect(read('10:20 Polilla mimetica?')).toMatchObject({ species: null, rest: 'polilla mimetica' })
  })

  it('a misspelt genus before a known epithet is part of the name', () => {
    expect(read('9:56 Godiriys Zavaleta Female 2m Parches claro')).toMatchObject({ species: 'Godyris zavaleta', rest: '' })
  })

  it('a "?" after the name says which part the collector doubted', () => {
    expect(read('10:15 Methona confusa? Hembra 1.8m parches').unsure).toBe('species')
    expect(read('10:45 B47 recapture macho Hyposcada illinissa ida? 0.5m parches').unsure).toBe('subspecies')
    expect(read('10:52 1m Male Oleria tiguilla? Nublado claro').unsure).toBe('species')
    expect(read('9:20 B74 hembra Oleria gunilla lota 0.5m Parches').unsure).toBeUndefined()
    // Written as the team does: complete, to verify.
    const c = locateCapture({ lat: -0.950925, lon: -77.869495, ele: null, time: null, text: '10:15 Methona confusa? Hembra 1.8m parches' }, taxa, ikiam)
    expect(captureValues(c, { date: '2026-10-14', collector: 'AO - Collector Two', section: c.section }).ID_status).toBe('Complete_but_verify')
    const review = reviewCapture(c, 0, { rows: sheet, date: '2026-10-14', captures: [c], preserved: new Map(), isIthomiini: () => true })
    expect(review.list.some(k => /duda de la especie/.test(k.text))).toBe(true)
  })

  it('a note without a time takes when its point was saved in Wikiloc', () => {
    const c = locateCapture(
      { lat: -0.950925, lon: -77.869495, ele: null, time: '2026-10-14T15:20:30.000Z', text: 'B69 Recaptura hembra Hyposcada illinissa ida 1m parches' },
      taxa,
      ikiam,
    )
    expect(c).toMatchObject({ minutes: 10 * 60 + 20, timeFromPoint: true })
    const review = reviewCapture(c, 0, { rows: sheet, date: '2026-10-14', captures: [c], preserved: new Map(), isIthomiini: () => true })
    expect(review.list.some(k => /Hora del punto en Wikiloc \(10:20\)/.test(k.text))).toBe(true)
    // A note with its own time keeps it.
    expect(locateCapture({ lat: 0, lon: 0, ele: null, time: '2026-10-14T15:20:30.000Z', text: '10:04 B75 Oleria gunilla' }, taxa, ikiam)).toMatchObject({
      minutes: 10 * 60 + 4,
    })
  })
})

describe('IDs handed out in order', () => {
  it('counts up from the first, keeping its prefix and width', () => {
    expect(sequenceFrom('CAM078900', 3)).toEqual(['CAM078900', 'CAM078901', 'CAM078902'])
    expect(sequenceFrom('FS90415349', 2)).toEqual(['FS90415349', 'FS90415350'])
    expect(sequenceFrom('CAM079999', 2)).toEqual(['CAM079999', 'CAM080000'])
    expect(sequenceFrom(' 12 ', 2)).toEqual(['12', '13'])
    expect(sequenceFrom('CAM', 2)).toBeNull()
    expect(sequenceFrom('CAM1', 0)).toEqual([])
  })

  it('gives them to the items that take one, skipping the rest (a marked one, one that escaped)', () => {
    const walk = ['B74', null, 'B75', null, null]
    const escaped = new Set([3])
    expect(fillInOrder(walk, (m, i) => !m && !escaped.has(i), 'CAM079944')).toEqual([null, 'CAM079944', null, null, 'CAM079945'])
    expect(fillInOrder(walk, m => !m, 'x')).toBeNull()
  })
})

describe('the walk day from its public Wikiloc page', () => {
  it('starts when the recording started and ends after Wikiloc\'s «Tiempo»', () => {
    expect(walkSpan({ started: '2026-10-14T14:18:51.000Z', minutes: 111 }, '2026-10-14')).toEqual({ start: 9 * 60 + 18, end: 11 * 60 + 9 })
    expect(walkSpan({ started: '2026-10-16T14:22:17.000Z', minutes: null }, '2026-10-16')).toEqual({ start: 9 * 60 + 22, end: null })
    // Recorded on another day than the walk's (a typo in the title): not used.
    expect(walkSpan({ started: '2026-10-15T14:00:00.000Z', minutes: 60 }, '2026-10-14')).toBeNull()
    expect(walkSpan({}, '2026-10-14')).toBeNull()
  })
})
