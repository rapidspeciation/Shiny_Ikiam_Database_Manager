import { afterEach, describe, expect, it } from 'vitest'
import { locale, tm } from '../i18n'
import { dayText, momentText, monthNames, parseDateInput, recentText, serialToIso, unbroken } from '../dates'
import { dateLabel } from '../summary'
import { whenText } from '../proposals'
import { formatWhen } from '../history'
import { displayValue, readingValue } from '../cells'

// One date shape outside the grids (Inicio's tiles, alerts, proposals): the grid's own, 7-Nov-26, with the month in
// the interface language.
afterEach(() => {
  locale.value = 'en'
})

describe('a day in sentences, tiles and lists', () => {
  it('is day-month-year with a short month, from an ISO day, a timestamp or a Sheets serial', () => {
    locale.value = 'en'
    expect(dayText('2026-11-07')).toBe('7-Nov-26')
    expect(dayText('2026-11-07T15:30:00Z')).toBe('7-Nov-26')
    expect(dayText(45883)).toBe('14-Aug-25')
    expect(dayText('2026-01-31')).toBe('31-Jan-26')
  })
  it('names the month in the interface language', () => {
    locale.value = 'es'
    expect(dayText('2026-01-07')).toBe('7-Ene-26')
    expect(dayText('2026-08-14')).toBe('14-Ago-26')
    expect(dayText('2026-12-25')).toBe('25-Dic-26')
    expect(monthNames()[3]).toBe('Abr')
    locale.value = 'en'
    expect(dayText('2026-01-07')).toBe('7-Jan-26')
    expect(monthNames()[3]).toBe('Apr')
  })
  it('says there is no date with a dash, never NaN or undefined', () => {
    for (const none of [null, undefined, '', 'nope', '2026-13-40', Number.NaN]) expect(dayText(none)).toBe('—')
    expect(dayText(null, '')).toBe('')
  })
})

describe('a moment', () => {
  it("is the day and the time in Ecuador (UTC−5), on a 24 hour clock", () => {
    locale.value = 'en'
    expect(momentText('2026-09-28T19:05:00Z')).toBe('28-Sep-26 14:05')
    // 04:30 UTC is still the day before in Ecuador.
    expect(momentText('2026-09-29T04:30:00Z')).toBe('28-Sep-26 23:30')
    expect(momentText('nope')).toBe('')
    expect(momentText(null)).toBe('')
    locale.value = 'es'
    expect(momentText('2026-01-02T19:05:00Z')).toBe('2-Ene-26 14:05')
  })
  it("drops the day when it is today's (a list of what just arrived)", () => {
    locale.value = 'en'
    const now = new Date('2026-11-25T23:00:00Z') // 18:00 on 25 Nov in Ecuador
    expect(recentText('2026-11-25T13:53:00Z', now)).toBe('08:53')
    expect(recentText('2026-11-24T13:53:00Z', now)).toBe('24-Nov-26 08:53')
    // 03:00 UTC on the 26th is 22:00 on the 25th in Ecuador: still today.
    expect(recentText('2026-11-26T03:00:00Z', now)).toBe('22:00')
    expect(recentText('', now)).toBe('')
  })
})

describe('the same shape wherever the app says a date', () => {
  const plain = (text: string) => text.replace(/‑/g, '-')
  it('Inicio, proposals and the Historial agree', () => {
    locale.value = 'en'
    expect(plain(dateLabel('2026-11-07'))).toBe('7-Nov-26')
    expect(dateLabel(null)).toBe('—')
    expect(plain(whenText('2026-11-07T19:05:00Z'))).toBe('7-Nov-26 14:05')
    expect(whenText(undefined)).toBe('')
    expect(formatWhen('2026-11-07T19:05:00Z')).toBe('7-Nov-26 14:05')
    locale.value = 'es'
    expect(plain(dateLabel('2026-04-07'))).toBe('7-Abr-26')
    expect(plain(whenText('2026-04-07T19:05:00Z'))).toBe('7-Abr-26 14:05')
    expect(formatWhen('2026-04-07T19:05:00Z')).toBe('7-Abr-26 14:05')
  })
  it("an alert's day is written in the reader's language from the day the server names", () => {
    const m = { key: '{species} llegó a {limit} preservadas el {date}', vars: { species: 'Oleria onega', limit: 30, date: { date: '2026-04-07' } } }
    locale.value = 'es'
    expect(plain(tm(m))).toBe('Oleria onega llegó a 30 preservadas el 7-Abr-26')
    locale.value = 'en'
    expect(plain(tm(m))).toContain('7-Apr-26')
    // Several days: each one, joined.
    expect(plain(tm({ key: '{dates}', vars: { dates: [{ date: '2026-10-01' }, { date: '2026-10-02' }] } }))).toBe('1-Oct-26, 2-Oct-26')
  })
  it('a date in a sentence is never cut at its hyphens, and still reads as a date when copied into a cell', () => {
    locale.value = 'en'
    const text = unbroken(dayText('2026-09-22'))
    expect(text).not.toContain('-')
    expect(text).toBe('22‑Sep‑26')
    expect(dateLabel('2026-09-22')).toBe(text)
    const serial = parseDateInput(text)
    expect(serial === null ? null : serialToIso(serial)).toBe('2026-09-22')
  })
})

describe('a date cell read outside a grid (a row\'s line, a proposal\'s stacked cells)', () => {
  const date = { key: 'Death_date', type: 'date' as const }
  it('has the month in the interface language; the grid keeps the sheet\'s own', () => {
    locale.value = 'es'
    expect(readingValue(45883, date)).toBe('14-Ago-25')
    expect(displayValue(45883, date)).toBe('14-Aug-25')
    locale.value = 'en'
    expect(readingValue(45883, date)).toBe('14-Aug-25')
  })
  it('anything that is not a date in range reads as the cell shows it', () => {
    locale.value = 'es'
    expect(readingValue('NA', date)).toBe('NA')
    expect(readingValue(null, date)).toBe('')
    expect(readingValue(5, date)).toBe(displayValue(5, date))
    expect(readingValue(45883, { key: 'CLUTCH NUMBER', type: 'number' })).toBe('45883')
  })
})
