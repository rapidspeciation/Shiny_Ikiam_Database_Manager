// Which country a coordinate lies in, from the maps site's outlines (data/country_boundaries.json, Natural Earth
// 1:50m, built by ithomiini_maps scripts/build_country_boundaries.mjs). Host-plant records carry coordinates and no
// country; the maps app places them with the same outlines and the same rule (src/utils/countryLookup.js), so a
// count here is what its country filter shows.

// A record this close (degrees, about 5 km) to a country's coast or border, and in no country, counts in it.
export const COAST_TOLERANCE_DEG = 0.05;

function inRing(lng, lat, ring) {
  let inside = false;
  for (let i = 0, j = ring.length - 1; i < ring.length; j = i++) {
    const [xi, yi] = ring[i];
    const [xj, yj] = ring[j];
    if (yi > lat !== yj > lat && lng < ((xj - xi) * (lat - yi)) / (yj - yi) + xi) inside = !inside;
  }
  return inside;
}

const inPolygon = (lng, lat, polygon) => inRing(lng, lat, polygon[0]) && !polygon.slice(1).some(hole => inRing(lng, lat, hole));
export const inBox = (lng, lat, [w, s, e, n], pad = 0) => lng >= w - pad && lng <= e + pad && lat >= s - pad && lat <= n + pad;

/** Squared distance (degrees, longitude scaled by latitude) from a point to a ring's edges. */
function ringDistance2(lng, lat, ring) {
  const k = Math.cos((lat * Math.PI) / 180);
  let best = Infinity;
  for (let i = 0, j = ring.length - 1; i < ring.length; j = i++) {
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
  for (const c of countries) if (inBox(lng, lat, c.bbox) && c.polygons.some(p => inPolygon(lng, lat, p))) return c.name;
  let nearest = null;
  let best = COAST_TOLERANCE_DEG ** 2;
  for (const c of countries) {
    if (!inBox(lng, lat, c.bbox, COAST_TOLERANCE_DEG)) continue;
    for (const p of c.polygons) {
      const d = ringDistance2(lng, lat, p[0]);
      if (d <= best) [best, nearest] = [d, c.name];
    }
  }
  return nearest;
}
