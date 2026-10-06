import { describe, expect, it } from 'vitest'
import { parseDateInput, serialToIso } from '../dates'

const iso = (text: string) => {
  const serial = parseDateInput(text)
  return serial === null ? null : serialToIso(serial)
}

describe('dates typed day first', () => {
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
    const on = (text: string, today: string) => {
      const serial = parseDateInput(text, today)
      return serial === null ? null : serialToIso(serial)
    }
    for (const text of ['27/9', '27-9', '27.9', '27 9', '27-sep', '27-Sep', '27 sept', '27sep', '27 septiembre', '27 de septiembre', '27-set'])
      expect(on(text, '2026-10-06'), text).toBe('2026-09-27')
    expect(on('3-ago', '2026-10-06')).toBe('2026-08-03')
    expect(on('5 dic', '2026-10-06')).toBe('2026-12-05')
    // More than two months ahead: last year's.
    expect(on('27/12', '2027-01-10')).toBe('2026-12-27')
    expect(on('27 sep 25', '2026-10-06')).toBe('2025-09-27')
    expect(on('31/9', '2026-10-06')).toBeNull()
    expect(on('27 xyz', '2026-10-06')).toBeNull()
  })
  it('refuses impossible days and months', () => {
    expect(iso('31/09/2026')).toBeNull()
    expect(iso('13/13/2026')).toBeNull()
  })
})
