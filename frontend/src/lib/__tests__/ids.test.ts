import { describe, expect, it } from 'vitest'
import { idTokens, resolveIds } from '../ids'
import { dayLabel, formatSerial, parseDateInput, serialFromIso } from '../dates'
import { initialsOf } from '../rows'

// Insectary IDs in the sheet's pre-made row order: N9D is followed by O0D.
const order = ['B9', 'N7D', 'N8D', 'N9D', 'O0D', 'O1D', 'O2D', 'B0D', 'B1D', 'B2D']

describe('ID picker lists', () => {
  it('reads IDs and ranges, with any dash and spaces', () => {
    expect(idTokens('N1D N2D, N3D;N4D')).toEqual(['N1D', 'N2D', 'N3D', 'N4D'])
    expect(idTokens('B0D-B9D E9D – F8D\nX1A —X2A')).toEqual(['B0D-B9D', 'E9D-F8D', 'X1A-X2A'])
  })
  it('expands a range in pre-made order, across letters', () => {
    expect(resolveIds(['n8d-o1d'], order)).toEqual({ found: ['N8D', 'N9D', 'O0D', 'O1D'], missing: [] })
    // Typed backwards, still the same run.
    expect(resolveIds(['O1D-N8D'], order).found).toEqual(['N8D', 'N9D', 'O0D', 'O1D'])
    expect(resolveIds(['B0D', 'Z1Z', 'B0D-Z9Z'], order)).toEqual({ found: ['B0D'], missing: ['Z1Z', 'B0D-Z9Z'] })
  })
})

describe('dates typed in date boxes', () => {
  it('refuses a year typed as 92026 and never shows NaN', () => {
    expect(serialFromIso('2026-09-21')).toBe(46286)
    expect(serialFromIso('92026-09-21')).toBeNull()
    expect(serialFromIso('1899-12-30')).toBeNull()
    expect(serialFromIso('')).toBeNull()
    expect(formatSerial(Number.NaN)).toBe('fecha no válida')
    expect(parseDateInput('92026')).toBeNull()
  })
  it('shows the weekday and how long ago, so a wrong day stands out', () => {
    expect(dayLabel('2026-09-27', '2026-09-28')).toBe('domingo 27-Sep-26 · ayer')
    expect(dayLabel('2026-09-28', '2026-09-28')).toBe('lunes 28-Sep-26 · hoy')
    expect(dayLabel('2026-09-21', '2026-09-28')).toBe('lunes 21-Sep-26 · hace 7 días')
  })
})

describe('initials for notes', () => {
  const collectors = ['FCH - Franz Chandi', 'PAS - Patricio Salazar', 'KN - Kimberly Nuñez']
  it('uses the Collector list code, found by name or by username', () => {
    expect(initialsOf('Franz Chandi', collectors)).toBe('FCH')
    expect(initialsOf('Laboratorio (compartido)', collectors, 'pas')).toBe('PAS')
  })
  it('otherwise takes letters only, never brackets', () => {
    expect(initialsOf('Prueba web (DF)', collectors)).toBe('PWD')
    expect(initialsOf('', collectors, 'lab-1')).toBe('LAB')
  })
})
