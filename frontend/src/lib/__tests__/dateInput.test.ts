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
  it('refuses impossible days and months', () => {
    expect(iso('31/09/2026')).toBeNull()
    expect(iso('13/13/2026')).toBeNull()
  })
})
