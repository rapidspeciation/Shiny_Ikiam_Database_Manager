import { describe, expect, it } from 'vitest'
import {
  calendarDays,
  dayLabel,
  formatSerial,
  isoToSerial,
  parseDateInput,
  serialFromIso,
  serialToIso,
  shiftMonth,
} from '../dates'

const iso = (text: string, today?: string) => {
  const serial = parseDateInput(text, today)
  return serial === null ? null : serialToIso(serial)
}

describe('Sheets serial numbers', () => {
  it('round-trip', () => {
    expect(isoToSerial('2025-08-14')).toBe(45883)
    expect(serialToIso(45883)).toBe('2025-08-14')
    expect(formatSerial(45883)).toBe('14-Aug-25')
  })
  it('refuse a year typed as 92026 and never show NaN', () => {
    expect(serialFromIso('2026-09-21')).toBe(46286)
    expect(serialFromIso('92026-09-21')).toBeNull()
    expect(serialFromIso('1899-12-30')).toBeNull()
    expect(serialFromIso('')).toBeNull()
    expect(formatSerial(Number.NaN)).toBe('fecha no válida')
    expect(parseDateInput('92026')).toBeNull()
  })
})

describe('dates typed day first', () => {
  it('reads the formats people type', () => {
    for (const input of ['14-Aug-25', '14-ago-2025', '2025-08-14', '14/08/2025', '45883'])
      expect(parseDateInput(input)).toBe(45883)
    expect(parseDateInput('mañana')).toBeNull()
  })
  it('reads day/month/year in its usual forms', () => {
    expect(iso('28/09/2026')).toBe('2026-09-28')
    expect(iso('28-9-26')).toBe('2026-09-28')
    expect(iso('3/10/2026')).toBe('2026-10-03')
    expect(iso('28-Sep-26')).toBe('2026-09-28')
  })
  it('reads digits only, as typed on a phone number pad', () => {
    expect(iso('280926')).toBe('2026-09-28')
    expect(iso('28092026')).toBe('2026-09-28')
  })
  it('reads day and month without the year, as the notebook writes them', () => {
    for (const text of ['27/9', '27-9', '27.9', '27 9', '27-sep', '27-Sep', '27 sept', '27sep', '27 septiembre', '27 de septiembre', '27-set'])
      expect(iso(text, '2026-10-06'), text).toBe('2026-09-27')
    expect(iso('3-ago', '2026-10-06')).toBe('2026-08-03')
    expect(iso('5 dic', '2026-10-06')).toBe('2026-12-05')
    // More than two months ahead: last year's.
    expect(iso('27/12', '2027-01-10')).toBe('2026-12-27')
    expect(iso('27 sep 25', '2026-10-06')).toBe('2025-09-27')
    expect(iso('31/9', '2026-10-06')).toBeNull()
    expect(iso('27 xyz', '2026-10-06')).toBeNull()
  })
  it('refuses impossible days and months', () => {
    expect(iso('31/09/2026')).toBeNull()
    expect(iso('13/13/2026')).toBeNull()
    expect(iso('31/02/2025')).toBeNull()
  })
})

describe('a date box', () => {
  it('shows the weekday and how long ago, so a wrong day stands out', () => {
    expect(dayLabel('2026-09-27', '2026-09-28')).toBe('domingo 27-Sep-26 · ayer')
    expect(dayLabel('2026-09-28', '2026-09-28')).toBe('lunes 28-Sep-26 · hoy')
    expect(dayLabel('2026-09-21', '2026-09-28')).toBe('lunes 21-Sep-26 · hace 7 días')
  })
  it('its calendar: months Monday first, six weeks, the days around greyed', () => {
    const october = calendarDays(2026, 10)
    expect(october).toHaveLength(42)
    // 1 Oct 2026 is a Thursday: Monday 28 Sep starts the grid.
    expect(october[0]).toEqual({ iso: '2026-09-28', day: 28, inMonth: false })
    expect(october[3]).toEqual({ iso: '2026-10-01', day: 1, inMonth: true })
    expect(october.filter(d => d.inMonth)).toHaveLength(31)
    expect(shiftMonth(2026, 1, -1)).toEqual({ year: 2025, month: 12 })
    expect(shiftMonth(2026, 12, 1)).toEqual({ year: 2027, month: 1 })
  })
})
