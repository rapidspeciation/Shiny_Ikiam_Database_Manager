import { describe, expect, it } from 'vitest'
import { SECTIONS, TRAIL_LENGTH, estimatedSection, nearestSection, sectionDiffers, trailPosition } from '../transects'

const mid = (section: number) => {
  const s = SECTIONS.find(x => x.section === section)!
  return s.path[Math.floor(s.path.length / 2)]
}

describe('nearestSection', () => {
  it('places captures on the trail section they were taken in', () => {
    expect(nearestSection(-0.950925, -77.869495).section).toBe(4)
    expect(nearestSection(-0.952142, -77.867853).section).toBe(3)
    expect(nearestSection(-0.9528, -77.8655).section).toBe(2)
    expect(nearestSection(-0.9521, -77.86432).section).toBe(1)
  })
})

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

describe('estimatedSection', () => {
  it('gives the section of a point on the trail, and none off it with the distance', () => {
    for (const s of [1, 2, 3, 4]) expect(estimatedSection(...mid(s)).section).toBe(s)
    const [lat, lon] = mid(2)
    const off = estimatedSection(lat + 0.001, lon)
    expect(off.section).toBeNull()
    expect(off.distance).toBeGreaterThan(40)
    expect(Number.isInteger(off.distance)).toBe(true)
  })
  it('differs from the row only when both are known', () => {
    expect(sectionDiffers({ section: 3 }, '4')).toBe(true)
    expect(sectionDiffers({ section: 3 }, 3)).toBe(false)
    expect(sectionDiffers({ section: 3 }, 'NA')).toBe(false)
    expect(sectionDiffers({ section: 3 }, null)).toBe(false)
    expect(sectionDiffers({ section: null }, '2')).toBe(false)
  })
})
