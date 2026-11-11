// What the three maps-render test files share (tests/maps-render.test.mjs, maps-render-checks.test.mjs,
// maps-render-worker.test.mjs): temp folders and the processes the fakes start (none may be left when a file
// ends), the hand-made PNGs, the app's answer and what the tool expects of it, and the Playwright stand-in.
// Not a test file itself (node --test runs tests/*.test.mjs).
import test from 'node:test';
import assert from 'node:assert/strict';
import { existsSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { deflateSync } from 'node:zlib';
import { EXPORT, OFFICIAL_MAPS_URL, fitBoundsView, procStat } from '../../server/maps-render.mjs';
import { mapsUrl } from '../../server/maps-tools.mjs';
import { OVERLAYS, withGeometry } from '../fixtures/export-geometry.mjs';

const FAKE_PW = new URL('../fixtures/fake-playwright.mjs', import.meta.url).href;
export const made = [];
export const dirOf = name => (made.push(mkdtempSync(join(tmpdir(), `maps-render-${name}-`))), made.at(-1));
/** A process that runs (a zombie waiting for its parent counts as gone). */
export const alive = pid => {
  const s = procStat(pid);
  return Boolean(s) && s.state !== 'Z' && s.state !== 'X';
};
export const waitFor = async (test, ms, step = 50) => {
  const until = Date.now() + ms;
  while (!test()) {
    if (Date.now() > until) return false;
    await new Promise(r => setTimeout(r, step));
  }
  return true;
};
// Every process the fakes or the tests start is listed here; after the tests none may still run (any that does is
// killed, so no fixture lingers, and the run fails).
export const fakes = [];
export const started = [];
const startedPids = () => [...started, ...fakes.flatMap(f => ['worker', 'browser', 'renderer', 'grandchild'].flatMap(k => f.get(k)))];
test.after(async () => {
  await new Promise(r => setTimeout(r, 300));
  const left = startedPids().filter(alive);
  for (const pid of left) {
    try {
      process.kill(pid, 'SIGKILL');
    } catch {
      // gone
    }
  }
  made.forEach(d => rmSync(d, { recursive: true, force: true }));
  assert.deepEqual(left, [], 'processes left running by the tests');
});

// ---------- PNGs: written with each PNG filter, so the decoder is tested on all five ----------
const crcTable = Array.from({ length: 256 }, (_, n) => {
  let c = n;
  for (let k = 0; k < 8; k++) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1;
  return c >>> 0;
});
export const crc = buf => {
  let c = 0xffffffff;
  for (const b of buf) c = crcTable[(c ^ b) & 255] ^ (c >>> 8);
  return (c ^ 0xffffffff) >>> 0;
};
export const chunk = (type, data) => {
  const len = Buffer.alloc(4);
  len.writeUInt32BE(data.length);
  const body = Buffer.concat([Buffer.from(type, 'latin1'), data]);
  const c = Buffer.alloc(4);
  c.writeUInt32BE(crc(body));
  return Buffer.concat([len, body, c]);
};
/** An RGB PNG; `pixel(x, y)` → [r, g, b]; each row's filter is row % 5 unless `filter` is given. */
export function encodePng(width, height, pixel, filter = null) {
  const raw = Buffer.alloc(width * 3 * height);
  for (let y = 0; y < height; y++) for (let x = 0; x < width; x++) raw.set(pixel(x, y), (y * width + x) * 3);
  const stride = width * 3;
  const out = Buffer.alloc((stride + 1) * height);
  for (let y = 0; y < height; y++) {
    const f = filter ?? y % 5;
    out[y * (stride + 1)] = f;
    for (let i = 0; i < stride; i++) {
      const v = raw[y * stride + i];
      const a = i >= 3 ? raw[y * stride + i - 3] : 0;
      const b = y ? raw[(y - 1) * stride + i] : 0;
      const c = y && i >= 3 ? raw[(y - 1) * stride + i - 3] : 0;
      const p = a + b - c;
      const paeth = Math.abs(p - a) <= Math.abs(p - b) && Math.abs(p - a) <= Math.abs(p - c) ? a : Math.abs(p - b) <= Math.abs(p - c) ? b : c;
      const pred = [0, a, b, (a + b) >> 1, paeth][f];
      out[y * (stride + 1) + 1 + i] = (v - pred) & 255;
    }
  }
  const ihdr = Buffer.alloc(13);
  ihdr.writeUInt32BE(width, 0);
  ihdr.writeUInt32BE(height, 4);
  ihdr[8] = 8;
  ihdr[9] = 2;
  return Buffer.concat([Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]), chunk('IHDR', ihdr), chunk('IDAT', deflateSync(out)), chunk('IEND', Buffer.alloc(0))]);
}
export const GREEN = [0x12, 0x86, 0x3d];
/** A map-like picture: a calm gradient; `border` a dashed accent frame 3 px wide; `dots` isolated accent pixels on the edge. */
const pngs = new Map();
export const mapPng = (o = {}) => pngs.get(JSON.stringify(o)) ?? pngs.set(JSON.stringify(o), drawPng(o)).get(JSON.stringify(o));
export const drawPng = ({ border = false, dots = false, blank = false, width = 1600, height = 1200 } = {}) =>
  encodePng(width, height, (x, y) => {
    const edge = x < 3 || y < 3 || x >= width - 3 || y >= height - 3;
    if (border && edge && Math.floor((x + y) / 12) % 3 !== 2) return GREEN;
    if (dots && ((x === 0 && y % 300 === 7) || (y === height - 1 && x % 400 === 11))) return GREEN;
    if (blank) return [240, 240, 238];
    return [200 + ((x >> 4) % 40), 210 - ((y >> 4) % 50), 190];
  });
// ---------- checkExport on hand-made app answers ----------
export const S = 'Sanger Institute';
export const REC = [
  { source: S, id: 'CAM1', sex: 'female', country: 'Ecuador', year: 2022, lng: -77.81, lat: -0.99, subspecies: 'hermias' },
  { source: S, id: 'CAM2', sex: 'female', country: 'Ecuador', year: 2023, lng: -77.812, lat: -0.991, subspecies: null },
  { source: S, id: 'CAM3', sex: 'female', country: 'Ecuador', year: 2022, lng: -77.19, lat: -0.71, subspecies: 'hermias' },
];
export const BBOX = [-77.9, -1.05, -77.1, -0.65];
export const PNG_OK = mapPng();
/** A hand-made app answer; with the picture geometry (`overlays`, default OVERLAYS) unless `geometry: false`. */
export function answer({ records = REC, bbox = BBOX, png = PNG_OK, view = null, legend = null, sites = null, geometry = true, overlays = OVERLAYS, ...rest } = {}) {
  const fitted = fitBoundsView(bbox, EXPORT.frame);
  const base = {
    version: 1,
    png_base64: png.toString('base64'),
    width: 1600,
    height: 1200,
    params: { applied: {}, ignored: [] },
    basemap: 'light',
    color_by: 'subspecies',
    title: 'Tithorea harmonia · females · Ecuador',
    view: view ?? { center: fitted.center, zoom: fitted.zoom, bbox: fitted.bbox, frame: { ...EXPORT.frame }, fit: { padding: EXPORT.padding, max_zoom: EXPORT.maxZoom } },
    records,
    legend: legend ?? { unit: 'records', rows: [{ label: 'hermias', records: records.filter(r => r.subspecies).length, unknown: false }, { label: 'Unknown subspecies', records: records.filter(r => !r.subspecies).length, unknown: true }] },
    sites: sites ?? [
      { label: 'Tena +1 site', merged_sites: 1, members: [{ key: 'a', locality: 'Tena', record_ids: [`${S}|CAM1`] }, { key: 'b', locality: 'Tena 2', record_ids: [`${S}|CAM2`] }] },
      { label: 'Loreto', merged_sites: 0, members: [{ key: 'c', locality: 'Loreto', record_ids: [`${S}|CAM3`] }] },
    ],
  };
  return { ...(geometry ? withGeometry(base, { overlays }) : base), ...rest };
}
export const EXPECT = {
  records: REC.map(({ subspecies, ...r }) => r),
  bbox: BBOX,
  explicit: false,
  style: 'light',
  colorBy: 'subspecies',
  sites: [
    { rank: 1, site: 'Tena', lat: -0.9905, lng: -77.811, ids: [`${S}|CAM1`, `${S}|CAM2`] },
    { rank: 2, site: 'Loreto', lat: -0.71, lng: -77.19, ids: [`${S}|CAM3`] },
  ],
};
/** A Playwright stand-in for one scenario: { module (PLAYWRIGHT_MODULE), log() (what the worker did) }. */
export function fakeModule(scenario) {
  const dir = dirOf('pw');
  const logFile = join(dir, 'log.jsonl');
  writeFileSync(join(dir, 'scenario.json'), JSON.stringify({ log: logFile, ...scenario }));
  const module = join(dir, 'pw.mjs');
  writeFileSync(module, `export * from ${JSON.stringify(`${FAKE_PW}?s=${encodeURIComponent(join(dir, 'scenario.json'))}`)};\n`);
  // Complete lines only: the worker, the browser and its helper append to the log while it is read, so the file may
  // be there but still empty, or a line being written seen in part (no newline yet).
  const log = () => (existsSync(logFile) ? readFileSync(logFile, 'utf8').split('\n').slice(0, -1).map(l => JSON.parse(l)) : []);
  const fake = { module, log, get: k => log().filter(e => k in e).map(e => e[k]) };
  fakes.push(fake);
  return fake;
}
export const URL_OK = mapsUrl({ species: ['Tithorea harmonia'], sources: ['sanger'], sex: 'female', sites: true, style: 'light', colorBy: 'subspecies', bbox: BBOX, exportMode: true }, OFFICIAL_MAPS_URL);
