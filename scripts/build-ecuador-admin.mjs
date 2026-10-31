#!/usr/bin/env node
// Builds server/ecuador-admin.json: Ecuador's cantons (INEC 2019, through geoBoundaries gbOpen ECU ADM2, CC BY 3.0
// IGO) simplified to about 200 m, each with its province. A province is the union of its cantons, so a point is in
// a province when it is in one of them (no dissolve needed, and provinces and cantons share the same edges). The
// province of a canton is read from geoBoundaries ECU ADM1 (CC0) at points inside the canton. Run again only to
// update the outlines:
//   node scripts/build-ecuador-admin.mjs [ADM2.geojson ADM1.geojson]   (downloads them when not given)

import { readFileSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const RELEASE = '9469f09';
const url = level => `https://github.com/wmgeolab/geoBoundaries/raw/${RELEASE}/releaseData/gbOpen/ECU/${level}/geoBoundaries-ECU-${level}.geojson`;
const TOLERANCE = 0.002;
const out = join(dirname(fileURLToPath(import.meta.url)), '..', 'server', 'ecuador-admin.json');

async function load(file, level) {
  if (file) return JSON.parse(readFileSync(file, 'utf8'));
  const r = await fetch(url(level));
  if (!r.ok) throw new Error(`${level}: ${r.status}`);
  return r.json();
}
const [adm2, adm1] = await Promise.all([load(process.argv[2], 'ADM2'), load(process.argv[3], 'ADM1')]);

const polygonsOf = g => (g.type === 'Polygon' ? [g.coordinates] : g.type === 'MultiPolygon' ? g.coordinates : []);

function inRing(x, y, ring) {
  let inside = false;
  for (let i = 0, j = ring.length - 1; i < ring.length; j = i++) {
    const [xi, yi] = ring[i];
    const [xj, yj] = ring[j];
    if (yi > y !== yj > y && x < ((xj - xi) * (y - yi)) / (yj - yi) + xi) inside = !inside;
  }
  return inside;
}
const inPolygon = (x, y, p) => inRing(x, y, p[0]) && !p.slice(1).some(h => inRing(x, y, h));

/** Douglas-Peucker on one ring (closed), keeping at least 4 points. */
function simplify(ring, tol) {
  if (ring.length <= 4) return ring;
  const keep = new Uint8Array(ring.length);
  keep[0] = keep[ring.length - 1] = 1;
  const stack = [[0, ring.length - 1]];
  while (stack.length) {
    const [a, b] = stack.pop();
    const [ax, ay] = ring[a];
    const [bx, by] = ring[b];
    const dx = bx - ax;
    const dy = by - ay;
    const len = Math.hypot(dx, dy);
    let best = -1;
    let at = -1;
    for (let i = a + 1; i < b; i++) {
      const [px, py] = ring[i];
      const d = len ? Math.abs(dy * px - dx * py + bx * ay - by * ax) / len : Math.hypot(px - ax, py - ay);
      if (d > best) [best, at] = [d, i];
    }
    if (best > tol) {
      keep[at] = 1;
      stack.push([a, at], [at, b]);
    }
  }
  const kept = ring.filter((_, i) => keep[i]).map(([x, y]) => [Math.round(x * 1e4) / 1e4, Math.round(y * 1e4) / 1e4]);
  return kept.length >= 4 ? kept : ring.map(([x, y]) => [Math.round(x * 1e4) / 1e4, Math.round(y * 1e4) / 1e4]);
}
const area = ring => ring.reduce((s, [x, y], i) => s + (i ? ring[i - 1][0] * y - x * ring[i - 1][1] : 0), 0) / 2;
const bboxOf = rings => {
  const xs = rings.flatMap(r => r.map(p => p[0]));
  const ys = rings.flatMap(r => r.map(p => p[1]));
  return [Math.min(...xs), Math.min(...ys), Math.max(...xs), Math.max(...ys)];
};

/** A few points inside a polygon: the middles of its inside stretches along lines across it. */
function insidePoints(polygon, n = 7) {
  const [w, s, e, nn] = bboxOf([polygon[0]]);
  const pts = [];
  for (let k = 1; k <= n; k++) {
    const y = s + ((nn - s) * k) / (n + 1);
    const xs = [];
    for (const ring of polygon)
      for (let i = 0, j = ring.length - 1; i < ring.length; j = i++) {
        const [xi, yi] = ring[i];
        const [xj, yj] = ring[j];
        if (yi > y !== yj > y) xs.push(((xj - xi) * (y - yi)) / (yj - yi) + xi);
      }
    xs.sort((a, b) => a - b);
    let widest = null;
    for (let i = 0; i + 1 < xs.length; i += 2) if (!widest || xs[i + 1] - xs[i] > widest[1] - widest[0]) widest = [xs[i], xs[i + 1]];
    if (widest) pts.push([(widest[0] + widest[1]) / 2, y]);
  }
  return pts.filter(([x, y]) => x >= w && x <= e && inPolygon(x, y, polygon));
}

const provinces = adm1.features.map(f => ({ name: f.properties.shapeName, iso: f.properties.shapeISO, polygons: polygonsOf(f.geometry) }));
// geoBoundaries' ADM1 spellings without their accents in places; the INEC names.
const PROVINCE_NAMES = { Manabi: 'Manabí', Sucumbios: 'Sucumbíos' };
const provinceAt = (x, y) => provinces.find(p => p.polygons.some(poly => inPolygon(x, y, poly)));

const cantons = [];
for (const f of adm2.features) {
  const polys = polygonsOf(f.geometry);
  // The largest part decides the province.
  const main = [...polys].sort((a, b) => Math.abs(area(b[0])) - Math.abs(area(a[0])))[0];
  const votes = new Map();
  for (const [x, y] of insidePoints(main)) {
    const p = provinceAt(x, y);
    if (p) votes.set(p.name, (votes.get(p.name) ?? 0) + 1);
  }
  const province = [...votes].sort((a, b) => b[1] - a[1])[0]?.[0] ?? null;
  const polygons = polys
    .map(poly => poly.map(ring => simplify(ring, TOLERANCE)))
    // Islets smaller than about 0.1 km² dropped.
    .filter(poly => Math.abs(area(poly[0])) > 1e-5);
  if (!polygons.length) continue;
  cantons.push({
    name: f.properties.shapeName,
    province: PROVINCE_NAMES[province] ?? province,
    bbox: bboxOf(polygons.map(p => p[0])).map(v => Math.round(v * 1e4) / 1e4),
    polygons,
  });
}
const missing = cantons.filter(c => !c.province).map(c => c.name);
if (missing.length) throw new Error(`No province for ${missing.join(', ')}`);
cantons.sort((a, b) => a.province.localeCompare(b.province, 'es') || a.name.localeCompare(b.name, 'es'));
const data = {
  source: `Cantons: INEC 2019 via geoBoundaries gbOpen ECU ADM2 (release ${RELEASE}), CC BY 3.0 IGO. Provinces: each canton's province from geoBoundaries gbOpen ECU ADM1 (CC0). Simplified to ${TOLERANCE}° (about 200 m) by scripts/build-ecuador-admin.mjs.`,
  attribution: 'INEC (Instituto Nacional de Estadística y Censos), OCHA ROLAC; geoBoundaries (Runfola et al. 2020)',
  tolerance_deg: TOLERANCE,
  cantons,
};
writeFileSync(out, JSON.stringify(data));
const verts = cantons.reduce((n, c) => n + c.polygons.reduce((m, p) => m + p.reduce((k, r) => k + r.length, 0), 0), 0);
console.log(`${cantons.length} cantons in ${new Set(cantons.map(c => c.province)).size} provinces, ${verts} points, ${(JSON.stringify(data).length / 1024).toFixed(0)} KB -> ${out}`);
