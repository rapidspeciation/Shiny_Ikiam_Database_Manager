// Which country a coordinate lies in, from the maps site's outlines (data/country_boundaries.json, Natural Earth
// 1:50m, built by ithomiini_maps scripts/build_country_boundaries.mjs). Host-plant records carry coordinates and no
// country; the maps app places them with the same outlines and the same rule (src/utils/countryLookup.js), so a
// count here is what its country filter shows.

// A record this close (degrees, about 5 km) to a country's coast or border, and in no country, counts in it.
export const COAST_TOLERANCE_DEG = 0.05;

// Each ring's edges by latitude band (BAND degrees), built once: a point is tested only against the edges that cross
// its latitude.
const BAND = 0.25;
const bandIndex = new WeakMap();
function bandsOf(ring) {
  let bands = bandIndex.get(ring);
  if (bands) return bands;
  bands = new Map();
  for (let i = 0, j = ring.length - 1; i < ring.length; j = i++) {
    const low = Math.floor(Math.min(ring[i][1], ring[j][1]) / BAND);
    const high = Math.floor(Math.max(ring[i][1], ring[j][1]) / BAND);
    for (let b = low; b <= high; b++) (bands.get(b) ?? bands.set(b, []).get(b)).push(j, i);
  }
  bandIndex.set(ring, bands);
  return bands;
}

function inRing(lng, lat, ring) {
  let inside = false;
  const edges = bandsOf(ring).get(Math.floor(lat / BAND)) ?? [];
  for (let k = 0; k < edges.length; k += 2) {
    const [xj, yj] = ring[edges[k]];
    const [xi, yi] = ring[edges[k + 1]];
    if (yi > lat !== yj > lat && lng < ((xj - xi) * (lat - yi)) / (yj - yi) + xi) inside = !inside;
  }
  return inside;
}

const inPolygon = (lng, lat, polygon) => inRing(lng, lat, polygon[0]) && !polygon.slice(1).some(hole => inRing(lng, lat, hole));
// `lngPad`: the margin in longitude (wider towards the poles for the same distance).
export const inBox = (lng, lat, [w, s, e, n], pad = 0, lngPad = pad) => lng >= w - lngPad && lng <= e + lngPad && lat >= s - pad && lat <= n + pad;
export const lngPadAt = lat => COAST_TOLERANCE_DEG / Math.max(0.05, Math.cos((lat * Math.PI) / 180));

/** Squared distance (degrees, longitude scaled by latitude) from a point to a ring's edges. */
function ringDistance2(lng, lat, ring) {
  const k = Math.cos((lat * Math.PI) / 180);
  let best = Infinity;
  // Only edges in the bands within the coast margin can be that close.
  const bands = bandsOf(ring);
  const edges = [];
  for (let b = Math.floor((lat - COAST_TOLERANCE_DEG) / BAND); b <= Math.floor((lat + COAST_TOLERANCE_DEG) / BAND); b++) edges.push(...(bands.get(b) ?? []));
  for (let e = 0; e < edges.length; e += 2) {
    const [j, i] = [edges[e], edges[e + 1]];
    const [ax, ay, bx, by, px] = [ring[j][0] * k, ring[j][1], ring[i][0] * k, ring[i][1], lng * k];
    const [dx, dy] = [bx - ax, by - ay];
    const len = dx * dx + dy * dy;
    const t = len ? Math.max(0, Math.min(1, ((px - ax) * dx + (lat - ay) * dy) / len)) : 0;
    const [ex, ey] = [ax + t * dx - px, ay + t * dy - lat];
    best = Math.min(best, ex * ex + ey * ey);
  }
  return best;
}

/** The country a point lies in; the nearest one within COAST_TOLERANCE_DEG when it lies in none; else null. */
export function countryOfPoint(lng, lat, outlines) {
  const countries = outlines?.countries ?? [];
  for (const c of countries)
    if (inBox(lng, lat, c.bbox) && c.polygons.some((p, i) => (!c.boxes || inBox(lng, lat, c.boxes[i])) && inPolygon(lng, lat, p))) return c.name;
  let nearest = null;
  let best = COAST_TOLERANCE_DEG ** 2;
  const lngPad = lngPadAt(lat);
  for (const c of countries) {
    if (!inBox(lng, lat, c.bbox, COAST_TOLERANCE_DEG, lngPad)) continue;
    for (const [i, p] of c.polygons.entries()) {
      if (c.boxes && !inBox(lng, lat, c.boxes[i], COAST_TOLERANCE_DEG, lngPad)) continue;
      const d = ringDistance2(lng, lat, p[0]);
      if (d <= best) [best, nearest] = [d, c.name];
    }
  }
  return nearest;
}
