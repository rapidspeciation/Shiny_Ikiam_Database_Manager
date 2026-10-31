import { describe, expect, it } from 'vitest'
import { menuLeft } from '../menuPlace'

describe('menuLeft', () => {
  it('ends at the button when that fits (the name at the right of the bar)', () => {
    expect(menuLeft({ left: 270, right: 380 }, 210, 390)).toBe(-100)
  })
  it('starts inside the screen when the button is at the left (the name wrapped under the title)', () => {
    // Before: the menu ended at x 128 and started at -80.
    expect(menuLeft({ left: 12, right: 128 }, 208, 360)).toBe(-4)
  })
  it('stays inside on both sides when the button is in the middle of a row', () => {
    const button = { left: 110, right: 230 }
    const left = button.left + menuLeft(button, 267, 360)
    expect(left).toBeGreaterThanOrEqual(8)
    expect(left + 267).toBeLessThanOrEqual(352)
  })
  it('keeps the left margin when the menu is as wide as the screen allows', () => {
    expect(menuLeft({ left: 100, right: 200 }, 344, 360)).toBe(-92)
  })
})
