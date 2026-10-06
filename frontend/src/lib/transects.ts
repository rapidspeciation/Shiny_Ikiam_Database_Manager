/**
 * The Ikiam monitoring trail and its four sections (Transect_section 1–4).
 *
 * Reconstructed on 26 Sep 2026: the section colours and boundaries of the QGIS
 * map in the monitoring reports were fitted onto the GPS track of that day's
 * monitoring (median distance about 1 m). The walked part follows the GPS
 * track; the last ~80 m of T1 towards the campus comes from the QGIS map only.
 * Coordinates are [latitude, longitude], ordered from the far (T4) end.
 */
export interface Section {
  section: number
  color: string
  path: [number, number][]
}

// prettier-ignore
export const SECTIONS: Section[] = [
  {
    section: 4,
    color: '#73a66d',
    path: [
      [-0.950528, -77.869962], [-0.95055, -77.869954], [-0.95067, -77.869699], [-0.95082, -77.86952],
      [-0.950867, -77.869536], [-0.950905, -77.869517], [-0.951082, -77.869329], [-0.951252, -77.869209],
      [-0.951362, -77.869207], [-0.95144, -77.869166], [-0.951498, -77.869039], [-0.951523, -77.869006],
      [-0.951577, -77.868959], [-0.951758, -77.868672], [-0.951825, -77.868389], [-0.951959, -77.868315],
    ],
  },
  {
    section: 3,
    color: '#e6b333',
    path: [
      [-0.951959, -77.868315], [-0.952017, -77.868203], [-0.952076, -77.868148], [-0.952064, -77.868054],
      [-0.952091, -77.86801], [-0.952092, -77.867927], [-0.95215, -77.867895], [-0.952141, -77.867835],
      [-0.952199, -77.867778], [-0.952262, -77.867649], [-0.952276, -77.867502], [-0.952363, -77.867395],
      [-0.952389, -77.867257], [-0.952488, -77.867111], [-0.95249, -77.867008], [-0.952566, -77.86681],
      [-0.952698, -77.866637], [-0.952707, -77.866587],
    ],
  },
  {
    section: 2,
    color: '#eea1ab',
    path: [
      [-0.952707, -77.866587], [-0.952794, -77.866371], [-0.952831, -77.866188], [-0.952915, -77.866016],
      [-0.952849, -77.865748], [-0.952796, -77.865668], [-0.952791, -77.865505], [-0.952752, -77.865437],
      [-0.952729, -77.865306], [-0.952739, -77.865047], [-0.952773, -77.864896], [-0.952733, -77.864814],
      [-0.952729, -77.864699], [-0.952794, -77.864485], [-0.952759, -77.864337], [-0.952747, -77.864141],
    ],
  },
  {
    section: 1,
    color: '#9bc5d5',
    path: [
      [-0.952747, -77.864141], [-0.952731, -77.864079], [-0.9527, -77.864065], [-0.952551, -77.864189],
      [-0.952483, -77.864209], [-0.952413, -77.864266], [-0.952268, -77.864282], [-0.952217, -77.864321],
      [-0.952158, -77.864309], [-0.951874, -77.86443], [-0.951708, -77.864404], [-0.951569, -77.864495],
      [-0.951449, -77.864472], [-0.951393, -77.864283], [-0.951308, -77.864165], [-0.951252, -77.864038],
    ],
  },
]

/** Points further than this from the trail get no automatic section. */
export const MAX_SECTION_DISTANCE = 40

const EARTH = 6371000
const RAD = Math.PI / 180

/** Metres east/north of a reference latitude; accurate enough over a 1 km trail. */
function project([lat, lon]: [number, number], lat0: number): [number, number] {
  return [lon * RAD * EARTH * Math.cos(lat0 * RAD), lat * RAD * EARTH]
}

function segmentDistance(p: [number, number], a: [number, number], b: [number, number]) {
  const [dx, dy] = [b[0] - a[0], b[1] - a[1]]
  const length = dx * dx + dy * dy
  const t = length ? Math.max(0, Math.min(1, ((p[0] - a[0]) * dx + (p[1] - a[1]) * dy) / length)) : 0
  return Math.hypot(p[0] - a[0] - t * dx, p[1] - a[1] - t * dy)
}

/** The nearest section to a point and its distance in metres. */
export function nearestSection(lat: number, lon: number): { section: number; distance: number } {
  const p = project([lat, lon], lat)
  let best = { section: 0, distance: Infinity }
  for (const s of SECTIONS)
    for (let i = 1; i < s.path.length; i++) {
      const d = segmentDistance(p, project(s.path[i - 1], lat), project(s.path[i], lat))
      if (d < best.distance) best = { section: s.section, distance: d }
    }
  return best
}

/** The section a GPS point lies in (null: off the trail, further than MAX_SECTION_DISTANCE) and its distance to the trail in whole metres. */
export function estimatedSection(lat: number, lon: number): { section: number | null; distance: number } {
  const near = nearestSection(lat, lon)
  return { section: near.distance <= MAX_SECTION_DISTANCE ? near.section : null, distance: Math.round(near.distance) }
}

/** Whether a point's estimated section and its row's Transect_section are both known and not the same. */
export function sectionDiffers(estimate: { section: number | null }, rowSection: string | number | null | undefined) {
  const row = Number(String(rowSection ?? '').trim())
  return estimate.section !== null && row >= 1 && row <= 4 && row !== estimate.section
}

/** Distance in metres between two points. */
export function distance(a: [number, number], b: [number, number]) {
  const lat0 = (a[0] + b[0]) / 2
  const [x1, y1] = project(a, lat0)
  const [x2, y2] = project(b, lat0)
  return Math.hypot(x2 - x1, y2 - y1)
}

/** The trail as one line from the far (T4) end, each segment with its section and its start in metres along the trail. */
const TRAIL = (() => {
  const out: { a: [number, number]; b: [number, number]; section: number; start: number; length: number }[] = []
  let along = 0
  for (const s of SECTIONS)
    for (let i = 1; i < s.path.length; i++) {
      const length = distance(s.path[i - 1], s.path[i])
      out.push({ a: s.path[i - 1], b: s.path[i], section: s.section, start: along, length })
      along += length
    }
  return out
})()
/** Metres along the trail where each section after the first (T3, T2, T1) begins. */
const BOUNDARIES = TRAIL.filter((seg, i) => i && seg.section !== TRAIL[i - 1].section).map(seg => seg.start)
/** Length of the trail in metres. */
export const TRAIL_LENGTH = TRAIL.reduce((sum, seg) => sum + seg.length, 0)

export interface TrailPosition {
  /** The nearest section (as nearestSection). */
  section: number
  /** Metres from the trail. */
  distance: number
  /** Metres along the trail from the far (T4) end. */
  along: number
  /** Metres along the trail to the nearest section boundary: how far a GPS error would have to move the point to change its section. */
  margin: number
}

/** Where a point lies on the trail: its section, how far from the trail, and how far from the nearest section boundary. */
export function trailPosition(lat: number, lon: number): TrailPosition {
  const p = project([lat, lon], lat)
  let best = { section: 0, distance: Infinity, along: 0 }
  for (const seg of TRAIL) {
    const a = project(seg.a, lat)
    const b = project(seg.b, lat)
    const [dx, dy] = [b[0] - a[0], b[1] - a[1]]
    const length = dx * dx + dy * dy
    const t = length ? Math.max(0, Math.min(1, ((p[0] - a[0]) * dx + (p[1] - a[1]) * dy) / length)) : 0
    const d = Math.hypot(p[0] - a[0] - t * dx, p[1] - a[1] - t * dy)
    if (d < best.distance) best = { section: seg.section, distance: d, along: seg.start + t * seg.length }
  }
  const margin = Math.min(...BOUNDARIES.map(b => Math.abs(best.along - b)))
  return { ...best, margin }
}
