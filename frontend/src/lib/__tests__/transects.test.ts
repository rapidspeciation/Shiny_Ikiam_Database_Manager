import { describe, expect, it } from 'vitest'
import { SECTIONS, TRAIL_LENGTH, nearestSection, trailPosition } from '../transects'

describe('trailPosition', () => {
  it('agrees with nearestSection and measures along the trail from the T4 end', () => {
    const start = SECTIONS[0].path[0]
    const p = trailPosition(start[0], start[1])
    expect(p.section).toBe(4)
    expect(p.along).toBeCloseTo(0, 5)
    const t1 = SECTIONS[3].path.at(-1)!
    expect(trailPosition(t1[0], t1[1]).along).toBeCloseTo(TRAIL_LENGTH, 3)
    expect(TRAIL_LENGTH).toBeGreaterThan(900)
    for (const s of SECTIONS) {
      const mid = s.path[Math.floor(s.path.length / 2)]
      const pos = trailPosition(mid[0], mid[1])
      expect(pos.section).toBe(s.section)
      expect(pos.section).toBe(nearestSection(mid[0], mid[1]).section)
      expect(pos.margin).toBeGreaterThan(20)
    }
  })
  it('the margin is the distance along the trail to the nearest section boundary', () => {
    const boundary = SECTIONS[1].path[0]
    expect(trailPosition(boundary[0], boundary[1]).margin).toBeCloseTo(0, 5)
    // A point beside the trail: its distance is to the line, its margin along it.
    const p = trailPosition(-0.951082 + 0.0001, -77.869329)
    expect(p.section).toBe(4)
    expect(p.distance).toBeGreaterThan(5)
    expect(p.distance).toBeLessThan(15)
  })
})
