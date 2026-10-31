// Ecuador's provinces and cantons for the maps tools' `area` option: whether a record lies in one, and how far
// from it. The outlines are downloaded once from geoBoundaries (gbOpen ECU: ADM2 = INEC's 2019 cantons, CC BY 3.0
// IGO; ADM1 = provinces, CC0; their simplified files, about 3.5 MB), made compact and kept in `dir`
// (ecuador-admin.json), then read from there; in memory after the first question. A province is the union of its
// cantons, so provinces and cantons share the same edges; a canton's province is read from ADM1 at points inside it.

import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { writeCacheFile } from './cache-write.mjs';
import { countryOfPoint, inBox } from './country-outlines.mjs';

const RELEASE = '9469f09';
export const BOUNDARY_URLS = {
  ADM2: `https://github.com/wmgeolab/geoBoundaries/raw/${RELEASE}/releaseData/gbOpen/ECU/ADM2/geoBoundaries-ECU-ADM2_simplified.geojson`,
  ADM1: `https://github.com/wmgeolab/geoBoundaries/raw/${RELEASE}/releaseData/gbOpen/ECU/ADM1/geoBoundaries-ECU-ADM1_simplified.geojson`,
};
const FILE = 'ecuador-admin-v1.json';
const ATTRIBUTION = 'INEC cantons 2019 (via OCHA ROLAC, CC BY 3.0 IGO) and provinces (CC0), from geoBoundaries gbOpen ECU (Runfola et al. 2020)';
// A point in no canton (between two simplified outlines, or just past the border) counts in the nearest canton
// within this many degrees (about 600 m): the simplification's error.
export const ADMIN_TOLERANCE_DEG = 0.0055;
// geoBoundaries' ADM1 spellings without their accents; the INEC names.
const PROVINCE_NAMES = { Manabi: 'Manabí', Sucumbios: 'Sucumbíos' };

export const fold = v =>
  String(v ?? '')
    .normalize('NFD')
    .replace(/\p{M}/gu, '')
    .toLowerCase()
    .replace(/[^a-z0-9 ,]/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();

const polygonsOf = g => (g?.type === 'Polygon' ? [g.coordinates] : g?.type === 'MultiPolygon' ? g.coordinates : []);
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
const ringArea = ring => Math.abs(ring.reduce((s, [x, y], i) => s + (i ? ring[i - 1][0] * y - x * ring[i - 1][1] : 0), 0) / 2);
const boxOf = rings => {
  let [w, s, e, n] = [Infinity, Infinity, -Infinity, -Infinity];
  for (const ring of rings) for (const [x, y] of ring) [w, s, e, n] = [Math.min(w, x), Math.min(s, y), Math.max(e, x), Math.max(n, y)];
  return [w, s, e, n];
};
const r4 = v => Math.round(v * 1e4) / 1e4;

/** A few points inside a polygon: the middles of the widest inside stretch along lines across it. */
function insidePoints(polygon, n = 7) {
  const [, s, , north] = boxOf([polygon[0]]);
  const pts = [];
  for (let k = 1; k <= n; k++) {
    const y = s + ((north - s) * k) / (n + 1);
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
  return pts.filter(([x, y]) => inPolygon(x, y, polygon));
}

/** The compact outlines from geoBoundaries' ADM2 and ADM1 GeoJSON: { source, cantons: [{ name, province, bbox, polygons }] }. */
export function buildAdmin(adm2, adm1) {
  const provinces = (adm1.features ?? []).map(f => ({ name: f.properties.shapeName, polygons: polygonsOf(f.geometry) }));
  const provinceAt = (x, y) => provinces.find(p => p.polygons.some(poly => inPolygon(x, y, poly)));
  const cantons = [];
  for (const f of adm2.features ?? []) {
    const polys = polygonsOf(f.geometry).filter(p => p[0]?.length >= 4);
    if (!polys.length) continue;
    const main = [...polys].sort((a, b) => ringArea(b[0]) - ringArea(a[0]))[0];
    const votes = new Map();
    for (const [x, y] of insidePoints(main)) {
      const p = provinceAt(x, y);
      if (p) votes.set(p.name, (votes.get(p.name) ?? 0) + 1);
    }
    const province = [...votes].sort((a, b) => b[1] - a[1])[0]?.[0];
    if (!province) continue;
    const polygons = polys.map(poly => poly.map(ring => ring.map(([x, y]) => [r4(x), r4(y)])));
    cantons.push({ name: f.properties.shapeName, province: PROVINCE_NAMES[province] ?? province, bbox: boxOf(polygons.map(p => p[0])).map(r4), polygons });
  }
  return { source: `geoBoundaries gbOpen ECU ADM2 + ADM1 (release ${RELEASE}, simplified files)`, attribution: ATTRIBUTION, cantons };
}

/** Lookups on the compact outlines: { cantons, provinces, outlines (countryOfPoint's shape, cantons by key) }. */
export function indexAdmin(json) {
  const cantons = json.cantons.map(c => ({ ...c, key: `${c.name}|${c.province}`, boxes: c.polygons.map(p => boxOf([p[0]])) }));
  const provinces = new Map();
  for (const c of cantons) {
    const k = fold(c.province);
    const p = provinces.get(k) ?? provinces.set(k, { name: c.province, cantons: [] }).get(k);
    p.cantons.push(c);
  }
  for (const p of provinces.values()) p.bbox = boxOf(p.cantons.map(c => [[c.bbox[0], c.bbox[1]], [c.bbox[2], c.bbox[3]]]));
  return { attribution: json.attribution ?? ATTRIBUTION, cantons, provinces, outlines: { countries: cantons.map(c => ({ name: c.key, bbox: c.bbox, polygons: c.polygons, boxes: c.boxes })) } };
}

/** The canton a point lies in ({ name, province }), or null outside Ecuador's cantons. */
export function cantonOf(lng, lat, data) {
  if (!data || !Number.isFinite(lng) || !Number.isFinite(lat)) return null;
  const key = countryOfPoint(lng, lat, data.outlines, ADMIN_TOLERANCE_DEG);
  if (!key) return null;
  const [name, province] = key.split('|');
  return { name, province };
}

/** km from a point to a segment, on a plane scaled at the point's latitude (fine at these distances). */
function segmentKm(px, py, ax, ay, bx, by) {
  const k = Math.cos((py * Math.PI) / 180);
  const [x1, y1, x2, y2, x] = [ax * k, ay, bx * k, by, px * k];
  const [dx, dy] = [x2 - x1, y2 - y1];
  const len = dx * dx + dy * dy;
  const t = len ? Math.max(0, Math.min(1, ((x - x1) * dx + (py - y1) * dy) / len)) : 0;
  return Math.hypot(x1 + t * dx - x, y1 + t * dy - py) * 111.32;
}

/**
 * The area asked: a name ("Napo", "provincia de Napo", "cantón Tena", "Tena, Napo") or { province } / { canton,
 * province? }. A province wins over a canton of the same name (Pastaza, Loja…) unless a canton is asked. ->
 * { level, name, province, cantons, bbox, also?, test(lng, lat), distanceKm(lng, lat) } or { error, suggestions }.
 */
export function findArea(asked, data) {
  let text;
  let wantsCanton = false;
  let wantsProvince = false;
  if (asked && typeof asked === 'object') {
    if (asked.canton) [text, wantsCanton] = [`${asked.canton}${asked.province ? `, ${asked.province}` : ''}`, true];
    else [text, wantsProvince] = [asked.province ?? '', true];
  } else text = String(asked ?? '');
  const raw = fold(text);
  if (!raw) return { error: 'Name a province or canton of Ecuador: area {province: "Napo"} or {canton: "Tena"}.' };
  wantsCanton ||= /\bcanton\b/.test(raw);
  wantsProvince ||= /\bprovincia\b|\bprovince\b|\bprov\b/.test(raw);
  // The words around a name ("provincia de", "cantón"), taken out of what was asked and of the names alike, so
  // "Baños de Agua Santa" or "La Joya de los Sachas" still match.
  const clean = v =>
    fold(v)
      .replace(/\b(provincia|province|prov|canton|del|de la|de|of|the|ecuador)\b/g, ' ')
      .replace(/\s+/g, ' ')
      .trim();
  const [first, second] = clean(raw).split(',').map(s => s.trim());
  const province = [...data.provinces.values()].find(p => clean(p.name) === first);
  let cantons = data.cantons.filter(c => clean(c.name) === first);
  if (second) cantons = cantons.filter(c => clean(c.province) === second);
  const asProvince = p => ({ level: 'province', name: p.name, province: p.name, own: p.cantons, bbox: p.bbox });
  const asCanton = c => ({ level: 'canton', name: c.name, province: c.province, own: [c], bbox: c.bbox });
  let area = null;
  let also = null;
  if (province && !wantsCanton && !(second && cantons.length)) {
    area = asProvince(province);
    if (cantons.length) also = `${cantons[0].name} is also a canton (of ${cantons.map(c => c.province).join(', ')}): ask {canton: "${cantons[0].name}"} for it.`;
  } else if (cantons.length === 1 && (!wantsProvince || wantsCanton || second)) area = asCanton(cantons[0]);
  else if (cantons.length > 1) return { error: `Several cantons are named ${cantons[0].name}: ${cantons.map(c => `${c.name}, ${c.province}`).join('; ')}. Give its province too.` };
  else if (province) area = asProvince(province);
  if (!area) {
    const starts = [...data.provinces.values()].filter(p => clean(p.name).startsWith(first));
    const cantonStarts = data.cantons.filter(c => clean(c.name).startsWith(first)).map(c => `${c.name} (${c.province})`);
    if (starts.length === 1 && !wantsCanton) area = asProvince(starts[0]);
    else return { error: `No province or canton of Ecuador is called ${text}.`, suggestions: [...starts.map(p => p.name), ...cantonStarts].slice(0, 8) };
  }
  const keys = new Set(area.own.map(c => c.key));
  const own = { countries: area.own.map(c => ({ name: c.key, bbox: c.bbox, polygons: c.polygons, boxes: c.boxes })) };
  const test = (lng, lat) => {
    if (!Number.isFinite(lng) || !Number.isFinite(lat) || !inBox(lng, lat, area.bbox, ADMIN_TOLERANCE_DEG)) return false;
    // In one of its own cantons (most points); else the canton the point belongs to, within the tolerance.
    if (countryOfPoint(lng, lat, own, 0)) return true;
    return keys.has(countryOfPoint(lng, lat, data.outlines, ADMIN_TOLERANCE_DEG));
  };
  // The area's outer edges: the edges of its cantons that no other of its cantons shares.
  let edges = null;
  const edgesOf = () => {
    if (edges) return edges;
    const seen = new Map();
    for (const c of area.own)
      for (const poly of c.polygons)
        for (const ring of poly)
          for (let i = 1; i < ring.length; i++) {
            const [a, b] = [ring[i - 1], ring[i]];
            const key = a[0] < b[0] || (a[0] === b[0] && a[1] < b[1]) ? `${a}|${b}` : `${b}|${a}`;
            seen.set(key, seen.has(key) ? null : [a[0], a[1], b[0], b[1]]);
          }
    edges = [...seen.values()].filter(Boolean);
    return edges;
  };
  /** 0 inside; else the km to the area's edge. */
  const distanceKm = (lng, lat) => {
    if (test(lng, lat)) return 0;
    let best = Infinity;
    for (const [ax, ay, bx, by] of edgesOf()) best = Math.min(best, segmentKm(lng, lat, ax, ay, bx, by));
    return best;
  };
  return { level: area.level, name: area.name, province: area.province, cantons: area.own.map(c => c.name), bbox: area.bbox, ...(also ? { also } : {}), test, distanceKm, source: data.attribution };
}

/**
 * The outlines for one app: read from `dir` (made there on the first question from the downloaded boundary files),
 * kept in memory. `fetchImpl`: for tests. -> { data(): Promise<index>, area(asked): Promise<area | {error}> }
 */
export function createAdminAreas({ dir = null, fetchImpl = fetch } = {}) {
  let loading = null;
  async function load() {
    const file = dir ? join(dir, FILE) : null;
    if (file) {
      try {
        return indexAdmin(JSON.parse(readFileSync(file, 'utf8')));
      } catch {
        /* not made yet */
      }
    }
    const get = async url => {
      const r = await fetchImpl(url, { headers: { 'user-agent': 'ithomiini-database' }, signal: AbortSignal.timeout(60_000) });
      if (!r.ok) throw new Error(`The boundary file answered ${r.status}`);
      return r.json();
    };
    const [adm2, adm1] = await Promise.all([get(BOUNDARY_URLS.ADM2), get(BOUNDARY_URLS.ADM1)]);
    const json = buildAdmin(adm2, adm1);
    if (!json.cantons.length) throw new Error('The boundary files hold no cantons');
    if (file) writeCacheFile(file, JSON.stringify(json));
    return indexAdmin(json);
  }
  const data = () => {
    loading ??= load().catch(e => {
      loading = null;
      throw e;
    });
    return loading;
  };
  return {
    data,
    async area(asked) {
      try {
        return findArea(asked, await data());
      } catch (e) {
        return { error: `Ecuador's province and canton outlines could not be read: ${e.message}` };
      }
    },
  };
}
