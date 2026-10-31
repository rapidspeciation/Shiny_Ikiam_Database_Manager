// Ecuador's provinces and cantons by name, and whether a point lies in one: the outlines of server/ecuador-admin.json
// (INEC cantons through geoBoundaries, simplified to about 200 m: scripts/build-ecuador-admin.mjs). A province is
// the union of its cantons. Read once per process, on the first question that names an area.

import { readFileSync } from 'node:fs';
import { countryOfPoint, inBox } from './country-outlines.mjs';

const FILE = new URL('./ecuador-admin.json', import.meta.url);
// A point in no canton (between two simplified outlines, or just past the border) counts in the nearest canton
// within this many degrees (about 600 m): the simplification's error, not the 5 km of the country outlines.
export const ADMIN_TOLERANCE_DEG = 0.0055;

const fold = v =>
  String(v ?? '')
    .normalize('NFD')
    .replace(/\p{M}/gu, '')
    .toLowerCase()
    .replace(/[^a-z0-9 ,]/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();

let loaded = null;
/** { cantons, provinces: Map(folded name -> province), outlines } (countryOfPoint's shape, cantons by key). */
export function adminData(file = FILE) {
  if (loaded && loaded.file === String(file)) return loaded;
  const json = JSON.parse(readFileSync(file, 'utf8'));
  const cantons = json.cantons.map(c => ({ ...c, key: `${c.name}|${c.province}`, boxes: c.polygons.map(p => boxOf(p[0])) }));
  const provinces = new Map();
  for (const c of cantons) {
    const p = provinces.get(fold(c.province)) ?? provinces.set(fold(c.province), { name: c.province, cantons: [] }).get(fold(c.province));
    p.cantons.push(c);
  }
  for (const p of provinces.values()) p.bbox = joinBoxes(p.cantons.map(c => c.bbox));
  loaded = { file: String(file), source: json.source, attribution: json.attribution, cantons, provinces, outlines: { countries: cantons.map(c => ({ name: c.key, bbox: c.bbox, polygons: c.polygons, boxes: c.boxes })) } };
  return loaded;
}

function boxOf(ring) {
  let [w, s, e, n] = [Infinity, Infinity, -Infinity, -Infinity];
  for (const [x, y] of ring) [w, s, e, n] = [Math.min(w, x), Math.min(s, y), Math.max(e, x), Math.max(n, y)];
  return [w, s, e, n];
}
const joinBoxes = boxes => [Math.min(...boxes.map(b => b[0])), Math.min(...boxes.map(b => b[1])), Math.max(...boxes.map(b => b[2])), Math.max(...boxes.map(b => b[3]))];

/** The canton a point lies in ({ name, province }), or null outside Ecuador's cantons. */
export function cantonOf(lng, lat, data = adminData()) {
  if (!Number.isFinite(lng) || !Number.isFinite(lat)) return null;
  const key = countryOfPoint(lng, lat, data.outlines, ADMIN_TOLERANCE_DEG);
  if (!key) return null;
  const [name, province] = key.split('|');
  return { name, province };
}

/**
 * The area a name gives: "Napo", "provincia de Napo", "Tena", "cantón Tena", "Tena, Napo". A province wins over a
 * canton of the same name (Pastaza, Loja…) unless the words say canton. -> { level, name, province, cantons, bbox,
 * also, test(lng, lat) } or { error, suggestions }.
 */
export function findArea(asked, data = adminData()) {
  const raw = fold(asked);
  if (!raw) return { error: 'Name a province or canton of Ecuador.' };
  const wantsCanton = /\bcanton\b/.test(raw);
  const wantsProvince = /\bprovincia\b|\bprovince\b|\bprov\b/.test(raw);
  const clean = raw
    .replace(/\b(provincia|province|prov|canton|del|de la|de|of|the|ecuador)\b/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();
  const [first, second] = clean.split(',').map(s => s.trim());
  const cantonsNamed = name => data.cantons.filter(c => fold(c.name) === name);
  const province = data.provinces.get(first);
  let cantons = cantonsNamed(first);
  if (second) cantons = cantons.filter(c => fold(c.province) === second);
  const asProvince = p => ({ level: 'province', name: p.name, province: p.name, cantons: p.cantons, bbox: p.bbox });
  const asCanton = c => ({ level: 'canton', name: c.name, province: c.province, cantons: [c], bbox: c.bbox });
  let area = null;
  let also = null;
  if (province && !wantsCanton && !(second && cantons.length)) {
    area = asProvince(province);
    if (cantons.length) also = `${cantons[0].name} is also a canton (of ${cantons.map(c => c.province).join(', ')}): say "cantón ${cantons[0].name}, <province>" for it.`;
  } else if (cantons.length === 1 && !wantsProvince) area = asCanton(cantons[0]);
  else if (cantons.length > 1) return { error: `Several cantons are named ${cantons[0].name}: ${cantons.map(c => `${c.name}, ${c.province}`).join('; ')}. Name it with its province.` };
  else if (province) area = asProvince(province);
  if (!area) {
    // A start of a name: "Santo Domingo", "Morona".
    const starts = [...data.provinces.values()].filter(p => fold(p.name).startsWith(first)).map(p => p.name);
    const cantonStarts = data.cantons.filter(c => fold(c.name).startsWith(first)).map(c => `${c.name} (${c.province})`);
    if (starts.length === 1 && !wantsCanton) area = asProvince(data.provinces.get(fold(starts[0])));
    else return { error: `No province or canton of Ecuador is called ${asked}.`, suggestions: [...starts, ...cantonStarts].slice(0, 8) };
  }
  const keys = new Set(area.cantons.map(c => c.key));
  const [w, s, e, n] = area.bbox;
  const pad = ADMIN_TOLERANCE_DEG;
  const test = (lng, lat) => {
    if (!inBox(lng, lat, [w, s, e, n], pad)) return false;
    // Inside one of its own cantons first (most points), else the canton the point belongs to.
    for (const c of area.cantons) if (inBox(lng, lat, c.bbox) && countryOfPoint(lng, lat, { countries: [{ name: c.key, bbox: c.bbox, polygons: c.polygons, boxes: c.boxes }] }, 0) === c.key) return true;
    return keys.has(countryOfPoint(lng, lat, data.outlines, ADMIN_TOLERANCE_DEG));
  };
  return { ...area, cantons: area.cantons.map(c => c.name), ...(also ? { also } : {}), test, source: data.attribution };
}
