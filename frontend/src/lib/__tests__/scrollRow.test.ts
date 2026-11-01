import { describe, expect, it } from 'vitest'
import { moreSide } from '../scrollRow'

describe('moreSide', () => {
  it('is none when the row fits', () => {
    expect(moreSide(0, 390, 390)).toBe('none')
    expect(moreSide(0, 390, 391)).toBe('none')
  })
  it('is end at the start of a row wider than the screen (the Monitoreo tabs at 360 px)', () => {
    expect(moreSide(0, 360, 430)).toBe('end')
  })
  it('is both in the middle, start at the end', () => {
    expect(moreSide(30, 360, 430)).toBe('both')
    expect(moreSide(70, 360, 430)).toBe('start')
    expect(moreSide(69.5, 360, 430)).toBe('start')
  })
})
