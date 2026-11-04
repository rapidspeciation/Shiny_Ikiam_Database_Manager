// maps_snapshot's app picture (server/maps-render.mjs, server/maps-render-worker.mjs): the checks of what the app
// returns, the worker process (with a stand-in Playwright module: tests/fixtures/fake-playwright.mjs, so the real
// worker runs, but no browser), and maps_snapshot choosing the app or Pillow. MAPS_RENDER_LIVE=1: one render of the
// deployed app (needs PLAYWRIGHT_MODULE, a deployed export interface and network).
import test from 'node:test';
import assert from 'node:assert/strict';
import { spawn, spawnSync } from 'node:child_process';
import { existsSync, mkdirSync, mkdtempSync, readFileSync, readdirSync, rmSync, utimesSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { Worker } from 'node:worker_threads';
import { deflateSync } from 'node:zlib';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createMapsTools, mapsUrl, sexOf, yearOf } from '../server/maps-tools.mjs';
import { EXPORT, OFFICIAL_MAPS_URL, checkExport, createMapRenderer, decodePng, fitBoundsView, pixelChecks, procStat, procTable, processTree, projectPoint, renderUrlProblem, treeRss, unsupportedFilter, visibleBounds } from '../server/maps-render.mjs';
import { LEGEND_BOTTOM_LEFT, OVERLAYS, withGeometry } from './fixtures/export-geometry.mjs';

const FAKE_PW = new URL('./fixtures/fake-playwright.mjs', import.meta.url).href;
const PILOT = new URL('../reports/assistant_maps_photos_20261102/map-pilot/', import.meta.url);
const pillow = spawnSync('python3', ['-c', 'import PIL'], { stdio: 'ignore' }).status === 0;
const made = [];
const dirOf = name => (made.push(mkdtempSync(join(tmpdir(), `maps-render-${name}-`))), made.at(-1));
/** A process that runs (a zombie waiting for its parent counts as gone). */
const alive = pid => {
  const s = procStat(pid);
  return Boolean(s) && s.state !== 'Z' && s.state !== 'X';
};
const waitFor = async (test, ms, step = 50) => {
  const until = Date.now() + ms;
  while (!test()) {
    if (Date.now() > until) return false;
    await new Promise(r => setTimeout(r, step));
  }
  return true;
};
// Every process the fakes or the tests start is listed here; after the tests none may still run (any that does is
// killed, so no fixture lingers, and the run fails).
const fakes = [];
const started = [];
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
const crc = buf => {
  let c = 0xffffffff;
  for (const b of buf) c = crcTable[(c ^ b) & 255] ^ (c >>> 8);
  return (c ^ 0xffffffff) >>> 0;
};
const chunk = (type, data) => {
  const len = Buffer.alloc(4);
  len.writeUInt32BE(data.length);
  const body = Buffer.concat([Buffer.from(type, 'latin1'), data]);
  const c = Buffer.alloc(4);
  c.writeUInt32BE(crc(body));
  return Buffer.concat([len, body, c]);
};
/** An RGB PNG; `pixel(x, y)` → [r, g, b]; each row's filter is row % 5 unless `filter` is given. */
function encodePng(width, height, pixel, filter = null) {
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
const GREEN = [0x12, 0x86, 0x3d];
/** A map-like picture: a calm gradient; `border` a dashed accent frame 3 px wide; `dots` isolated accent pixels on the edge. */
const pngs = new Map();
const mapPng = (o = {}) => pngs.get(JSON.stringify(o)) ?? pngs.set(JSON.stringify(o), drawPng(o)).get(JSON.stringify(o));
const drawPng = ({ border = false, dots = false, blank = false, width = 1600, height = 1200 } = {}) =>
  encodePng(width, height, (x, y) => {
    const edge = x < 3 || y < 3 || x >= width - 3 || y >= height - 3;
    if (border && edge && Math.floor((x + y) / 12) % 3 !== 2) return GREEN;
    if (dots && ((x === 0 && y % 300 === 7) || (y === height - 1 && x % 400 === 11))) return GREEN;
    if (blank) return [240, 240, 238];
    return [200 + ((x >> 4) % 40), 210 - ((y >> 4) % 50), 190];
  });

test('the PNG decoder reads all five row filters, RGB and RGBA', () => {
  const pixel = (x, y) => [(x * 7 + y) & 255, (y * 13) & 255, (x ^ y) & 255];
  const img = decodePng(encodePng(37, 23, pixel));
  assert.equal(img.width, 37);
  assert.equal(img.height, 23);
  for (const [x, y] of [[0, 0], [36, 22], [5, 4], [17, 9], [30, 3]]) assert.deepEqual([...img.data.subarray((y * 37 + x) * 3, (y * 37 + x) * 3 + 3)], pixel(x, y));
  assert.throws(() => decodePng(Buffer.from('not a png')), /not a PNG/);
});

const PNG_SIG = Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]);
const ihdrOf = (width, height) => {
  const b = Buffer.alloc(13);
  b.writeUInt32BE(width, 0);
  b.writeUInt32BE(height, 4);
  b[8] = 8;
  b[9] = 2;
  return b;
};
/** A PNG whose IHDR says `width` x `height` and whose IDAT is `raw` deflated (small: `raw` is mostly zeros). */
const pngOf = (width, height, raw) => Buffer.concat([PNG_SIG, chunk('IHDR', ihdrOf(width, height)), chunk('IDAT', deflateSync(raw)), chunk('IEND', Buffer.alloc(0))]);

test('PNG: the size is checked in IHDR before inflating; the data is inflated to exactly its rows; bad chunks refused', () => {
  const rows = (EXPORT.png.width * 3 + 1) * EXPORT.png.height;
  // 100000 x 100000 declared (30 GB of pixels) with 10 bytes of data: refused from IHDR, nothing inflated or allocated.
  const huge = pngOf(100_000, 100_000, Buffer.alloc(10));
  assert.throws(() => decodePng(huge, EXPORT.png), e => e.code === 'PNG_SIZE' && e.width === 100_000);
  assert.throws(() => decodePng(huge), e => e.code === 'PNG_SIZE');
  assert.equal(checkExport(answer({ png: huge }), EXPECT).reason, 'png_size');
  assert.deepEqual(checkExport(answer({ png: huge }), EXPECT).detail, { width: 100_000, height: 100_000 });
  // The right size, but data that inflates past its 1200 rows (4 kB more): refused at the expected length.
  const long = pngOf(EXPORT.png.width, EXPORT.png.height, Buffer.alloc(rows + 4096));
  assert.ok(long.length < 64 * 1024);
  assert.throws(() => decodePng(long, EXPORT.png), /PNG data longer than its rows/);
  assert.equal(checkExport(answer({ png: long }), EXPECT).reason, 'png_invalid');
  assert.throws(() => decodePng(pngOf(EXPORT.png.width, EXPORT.png.height, Buffer.alloc(rows - 1)), EXPORT.png), /PNG data short/);
  assert.equal(decodePng(pngOf(EXPORT.png.width, EXPORT.png.height, Buffer.alloc(rows)), EXPORT.png).width, EXPORT.png.width);
  // A chunk length past the end of the file, an IHDR of the wrong length or not first: refused.
  const good = encodePng(8, 6, () => [1, 2, 3]);
  const past = Buffer.from(good);
  past.writeUInt32BE(0x7fff_ffff, 33);
  assert.throws(() => decodePng(past), /PNG chunk past the end/);
  assert.throws(() => decodePng(Buffer.concat([PNG_SIG, chunk('IHDR', Buffer.alloc(12)), chunk('IEND', Buffer.alloc(0))])), /PNG IHDR/);
  assert.throws(() => decodePng(Buffer.concat([PNG_SIG, chunk('IDAT', deflateSync(Buffer.alloc(7))), chunk('IHDR', ihdrOf(2, 1)), chunk('IEND', Buffer.alloc(0))])), /PNG IHDR/);
});

test('border check: a dashed accent frame is rejected, isolated accent pixels on the edge are not, a blank picture shows', () => {
  assert.equal(pixelChecks(decodePng(mapPng())).stripe, null);
  assert.equal(pixelChecks(decodePng(mapPng({ dots: true }))).stripe, null);
  const dashed = pixelChecks(decodePng(mapPng({ border: true })));
  assert.ok(dashed.stripe && dashed.stripe.share > 0.25, JSON.stringify(dashed));
  assert.ok(pixelChecks(decodePng(mapPng())).sd > 8);
  assert.ok(pixelChecks(decodePng(mapPng({ blank: true }))).sd <= 8);
});

test('border check on the pilots\' real app exports: pilot 1\'s dashed green edge rejected, pilot A\'s fixed export passes', { skip: !existsSync(new URL('pilotA-border-transparent.png', PILOT)) && 'pilot PNGs not here' }, () => {
  const before = pixelChecks(decodePng(readFileSync(new URL('tithorea-harmonia-female-ecuador-app-export.png', PILOT))));
  assert.equal(before.stripe?.edge, 'col', JSON.stringify(before));
  // Pilot A: 4 accent pixels on row 1194 (map content), nothing border-like.
  const after = pixelChecks(decodePng(readFileSync(new URL('pilotA-border-transparent.png', PILOT))));
  assert.equal(after.stripe, null);
  assert.ok(after.sd > 8);
});

test('fitBounds as MapLibre: centre in Mercator, zoom from the frame less the padding, capped at 12', () => {
  // Expected values computed apart (Python, the same Mercator formulas): 760x570 frame, 32 px padding, 512 px tiles.
  const a = fitBoundsView([-1, -1, 1, 1], EXPORT.frame);
  assert.ok(Math.abs(a.center[0]) < 1e-9 && Math.abs(a.center[1]) < 1e-9);
  assert.ok(Math.abs(a.zoom - 7.474773422298291) < 1e-9, a.zoom);
  const b = fitBoundsView([-78.2, -1.3, -76.9, -0.5], EXPORT.frame);
  assert.ok(Math.abs(b.center[0] - -77.55) < 1e-9 && Math.abs(b.center[1] - -0.9000219347037713) < 1e-9);
  assert.ok(Math.abs(b.zoom - 8.556284968924691) < 1e-9);
  const c = fitBoundsView([-77.81, -0.991, -77.8, -0.99], EXPORT.frame);
  assert.equal(c.zoom, 12);
  assert.ok(c.capped);
  // What the view shows holds the bbox, wider on the axis that does not bind.
  const [w, s, e, n] = b.bbox;
  assert.ok(w < -78.2 && e > -76.9 && s <= -1.3 + 1e-9 && n >= -0.5 - 1e-9);
  assert.deepEqual(visibleBounds(b.center, b.zoom, EXPORT.frame), b.bbox);
});

test('only the official app, closed parameters and values; unsupported filters named', () => {
  const ok = `${OFFICIAL_MAPS_URL}?sp=Tithorea%20harmonia&source=Sanger%20Institute&sex=female&sites=1&style=light&color_by=subspecies&bbox=-78,-1.3,-76.9,-0.5&export=1`;
  assert.equal(renderUrlProblem(ok), null);
  assert.equal(renderUrlProblem(ok.replace('rapidspeciation.github.io', 'evil.test')), 'not the official maps app');
  assert.equal(renderUrlProblem(`${ok}&onload=alert(1)`), 'parameter onload');
  assert.equal(renderUrlProblem(ok.replace('style=light', 'style=javascript:alert(1)')), 'style');
  assert.equal(renderUrlProblem(ok.replace('color_by=subspecies', 'color_by=%3Cscript%3E')), 'value of color_by');
  assert.equal(renderUrlProblem(ok.replace('bbox=-78,-1.3,-76.9,-0.5', 'bbox=1,2,3')), 'bbox');
  assert.equal(renderUrlProblem(ok.replace('&export=1', '')), 'export=1 missing');
  assert.equal(renderUrlProblem(`${ok}#x`), 'not the official maps app');
  assert.equal(unsupportedFilter({ area: { province: 'Napo' } }), 'area');
  assert.equal(unsupportedFilter({ elevation_min: 500 }), 'elevation');
  assert.equal(unsupportedFilter({ years: { from: 2020 } }), 'years');
  assert.equal(unsupportedFilter({ near: { lat: 0, lng: 0, km: 5 } }), 'near');
  assert.equal(unsupportedFilter({ site_km: 5 }), 'site_km');
  assert.equal(unsupportedFilter({ site_km: 3, country: ['Ecuador'], sex: 'female' }), null);
  // The render URL from maps-tools carries the frame as bbox= (no map_view) and export=1 only when asked.
  const u = new URL(mapsUrl({ species: ['Tithorea harmonia'], sources: ['sanger'], sex: 'female', sites: true, style: 'light', colorBy: 'subspecies', bbox: [-78.2, -1.3, -76.9, -0.5], view: { lng: 0, lat: 0, zoom: 3 }, exportMode: true }, OFFICIAL_MAPS_URL));
  assert.equal(renderUrlProblem(u.href), null);
  assert.equal(u.searchParams.get('bbox'), '-78.20000,-1.30000,-76.90000,-0.50000');
  assert.equal(u.searchParams.has('map_view'), false);
  assert.deepEqual([u.searchParams.get('sites'), u.searchParams.get('site_labels'), u.searchParams.get('site_min')], ['1', 'records', '1']);
});

// ---------- checkExport on hand-made app answers ----------
const S = 'Sanger Institute';
const REC = [
  { source: S, id: 'CAM1', sex: 'female', country: 'Ecuador', year: 2022, lng: -77.81, lat: -0.99, subspecies: 'hermias' },
  { source: S, id: 'CAM2', sex: 'female', country: 'Ecuador', year: 2023, lng: -77.812, lat: -0.991, subspecies: null },
  { source: S, id: 'CAM3', sex: 'female', country: 'Ecuador', year: 2022, lng: -77.19, lat: -0.71, subspecies: 'hermias' },
];
const BBOX = [-77.9, -1.05, -77.1, -0.65];
const PNG_OK = mapPng();
/** A hand-made app answer; with the picture geometry (`overlays`, default OVERLAYS) unless `geometry: false`. */
function answer({ records = REC, bbox = BBOX, png = PNG_OK, view = null, legend = null, sites = null, geometry = true, overlays = OVERLAYS, ...rest } = {}) {
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
const EXPECT = {
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

test('the contract fixture for the app matches the renderer\'s constants and the answers the tests use', () => {
  const contract = JSON.parse(readFileSync(new URL('./fixtures/maps-export-contract.json', import.meta.url), 'utf8'));
  assert.equal(contract.page, OFFICIAL_MAPS_URL);
  assert.deepEqual([contract.fit.padding_css_px, contract.fit.max_zoom], [EXPORT.padding, EXPORT.maxZoom]);
  assert.deepEqual(contract.browser.viewport, EXPORT.viewport);
  assert.equal(contract.result.version, EXPORT.version);
  assert.deepEqual([contract.result.width, contract.result.height], [EXPORT.png.width, EXPORT.png.height]);
  assert.deepEqual(Object.keys(answer()).sort(), Object.keys(contract.result).sort());
  assert.deepEqual(Object.keys(answer().view).sort(), Object.keys(contract.result.view).sort());
});

test('checks: a good answer gives the picture, its view, what is in it and how the sites are labelled', () => {
  const ok = checkExport(answer(), EXPECT);
  assert.equal(ok.ok, true, JSON.stringify(ok));
  assert.equal(ok.visible.records, 3);
  assert.deepEqual(ok.picture_labels, [
    { rank: 1, site: 'Tena', label_on_picture: 'Tena +1 site', merged_sites: 1 },
    { rank: 2, site: 'Loreto', label_on_picture: 'Loreto', merged_sites: 0 },
  ]);
  assert.equal(ok.legend_unknown, 1);
  // Coordinates equal once rounded to 1e-5 degrees pass.
  assert.equal(checkExport(answer({ records: REC.map(r => ({ ...r, lng: r.lng + 3e-6 })) }), EXPECT).ok, true);
});

test('checks: the same IDs with moved coordinates, another sex, country or year are a parity mismatch', () => {
  const moved = checkExport(answer({ records: REC.map((r, i) => (i === 1 ? { ...r, lat: r.lat + 0.0002 } : r)) }), EXPECT);
  assert.equal(moved.reason, 'parity_mismatch');
  assert.deepEqual(moved.detail.fields, { sex: 0, country: 0, year: 0, coords: 1 });
  assert.equal(moved.detail.only_app + moved.detail.only_tool, 0);
  const other = checkExport(answer({ records: REC.map((r, i) => (i === 0 ? { ...r, sex: 'male', year: 2019 } : i === 2 ? { ...r, country: 'Peru' } : r)) }), EXPECT);
  assert.deepEqual(other.detail.fields, { sex: 1, country: 1, year: 1, coords: 0 });
  const extra = checkExport(answer({ records: [...REC, { source: S, id: 'CAM9', sex: 'female', country: 'Ecuador', year: 2022, lng: -77.5, lat: -0.8, subspecies: 'hermias' }] }), EXPECT);
  assert.equal(extra.reason, 'parity_mismatch');
  assert.equal(extra.detail.only_app, 1);
  // The app's sex spelled its own way is the same sex.
  assert.equal(checkExport(answer({ records: REC.map(r => ({ ...r, sex: 'Female' })) }), EXPECT).ok, true);
});

test('checks: with a bbox asked, the tool\'s records are the app\'s inside it; the legend counts the app\'s whole set', () => {
  const box = [-77.9, -1.05, -77.7, -0.95];
  const expect = { ...EXPECT, bbox: box, explicit: true, records: EXPECT.records.slice(0, 2) };
  // The app shows all three filtered records (CAM3 outside the box); the tool's two are the ones inside.
  const ok = checkExport(answer({ bbox: box }), expect);
  assert.equal(ok.ok, true, JSON.stringify(ok));
  assert.equal(ok.app_records, 3);
  // A record inside the box that the tool does not have is still a mismatch.
  const inside = { source: S, id: 'CAM8', sex: 'female', country: 'Ecuador', year: 2022, lng: -77.8, lat: -1.0, subspecies: 'hermias' };
  const bad = checkExport(answer({ bbox: box, records: [...REC, inside] }), expect);
  assert.equal(bad.reason, 'parity_mismatch');
  assert.equal(bad.detail.only_app, 1);
  // Without a bbox asked, CAM3 missing from the tool's set is a mismatch, not left out.
  assert.equal(checkExport(answer({ bbox: box }), { ...expect, explicit: false }).reason, 'parity_mismatch');
});

test('checks: what is on the picture comes from the view returned, not from the bbox asked', () => {
  // A narrow, tall bbox: the 4:3 frame shows more to the east and west than was asked.
  const box = [-77.82, -1.05, -77.8, -0.95];
  const recs = [...REC.slice(0, 2), { ...REC[2], lng: -77.76, lat: -1.0 }];
  const fitted = fitBoundsView(box, EXPORT.frame);
  assert.ok(fitted.bbox[2] > -77.76 && -77.76 > box[2], 'the third record is inside the view, outside the bbox');
  const out = checkExport(answer({ bbox: box, records: recs }), { ...EXPECT, bbox: box, records: recs.map(({ subspecies, ...r }) => r), sites: [] });
  assert.equal(out.ok, true, JSON.stringify(out));
  assert.equal(out.visible.records, 3);
});

test('checks: an app that ignored bbox=, a view that only contains it, or other fit options fall back', () => {
  // The app's own default view (bbox ignored): wrong centre and zoom.
  const own = fitBoundsView([-80, -4, -75, 2], EXPORT.frame);
  const ignored = checkExport(answer({ view: { center: own.center, zoom: own.zoom, bbox: own.bbox, frame: EXPORT.frame, fit: { padding: 32, max_zoom: 12 } } }), EXPECT);
  assert.equal(ignored.reason, 'view_mismatch');
  // Zoomed out by 0.5 around the right centre: it contains the bbox, but is not the fit.
  const right = fitBoundsView(BBOX, EXPORT.frame);
  const wider = { center: right.center, zoom: right.zoom - 0.5, bbox: visibleBounds(right.center, right.zoom - 0.5, EXPORT.frame), frame: EXPORT.frame, fit: { padding: 32, max_zoom: 12 } };
  assert.equal(checkExport(answer({ view: wider }), EXPECT).reason, 'view_mismatch');
  // Bounds that do not belong to the centre and zoom given.
  assert.equal(checkExport(answer({ view: { ...wider, zoom: right.zoom } }), EXPECT).reason, 'view_bbox_inconsistent');
  assert.equal(checkExport(answer({ view: { ...wider, zoom: right.zoom, bbox: right.bbox, fit: { padding: 20, max_zoom: 12 } } }), EXPECT).reason, 'view_fit_options');
  assert.equal(checkExport(answer({ view: { ...wider, frame: { width: 827, height: 620 } } }), EXPECT).reason, 'frame_size');
  // A zoom capped at 12 is the expected view.
  const tiny = [-77.811, -0.9911, -77.809, -0.9899];
  const capped = fitBoundsView(tiny, EXPORT.frame);
  assert.equal(capped.zoom, 12);
  const tinyRecs = REC.slice(0, 1).map(({ subspecies, ...r }) => r);
  assert.equal(checkExport(answer({ bbox: tiny, records: REC.slice(0, 1), sites: [] }), { ...EXPECT, bbox: tiny, records: tinyRecs, sites: [] }).ok, true);
});

test('checks: ignored params, other settings, legend, sites, size and pixels', () => {
  assert.equal(checkExport(answer({ params: { applied: {}, ignored: ['bbox'] } }), EXPECT).reason, 'params_ignored');
  assert.equal(checkExport(answer({ basemap: 'terrain' }), EXPECT).reason, 'settings_differ');
  assert.equal(checkExport(answer({ version: 2 }), EXPECT).reason, 'contract_version');
  assert.equal(checkExport(answer({ legend: { unit: 'records', rows: [{ label: 'hermias', records: 2, unknown: false }] } }), EXPECT).reason, 'legend_sum');
  assert.equal(checkExport(answer({ legend: { unit: 'sites', rows: [] } }), EXPECT).reason, 'legend_unit');
  assert.equal(checkExport(answer({ sites: [] }), EXPECT).reason, 'sites_not_drawn');
  assert.equal(checkExport(answer({ png: mapPng({ width: 800, height: 600 }), width: 800, height: 600 }), EXPECT).reason, 'png_size');
  assert.equal(checkExport(answer({ png: Buffer.from('nope') }), EXPECT).reason, 'png_invalid');
  assert.equal(checkExport(answer({ png: mapPng({ border: true }) }), EXPECT).reason, 'border_stripe');
  assert.equal(checkExport(answer({ png: mapPng({ dots: true }) }), EXPECT).ok, true);
  assert.equal(checkExport(answer({ png: mapPng({ blank: true }) }), EXPECT).reason, 'blank');
});

/** The latitude drawn `y` CSS px from the top of `view` (bisection on our own projection). */
function latAtY(view, y, lng = view.center[0]) {
  let [lo, hi] = [view.bbox[1], view.bbox[3]];
  for (let i = 0; i < 60; i++) {
    const mid = (lo + hi) / 2;
    if (projectPoint([lng, mid], view).y > y) lo = mid;
    else hi = mid;
  }
  return (lo + hi) / 2;
}

test('geometry: a ranked site behind the legend fails with ranked_site_hidden; a site the answer does not name is left out', () => {
  // Tena (rank 1) sits at y = 407; the legend in its default bottom-left corner starts at y = 409.
  const covered = [OVERLAYS[0], LEGEND_BOTTOM_LEFT, OVERLAYS[2], OVERLAYS[3]];
  const hidden = checkExport(answer({ overlays: covered }), EXPECT);
  assert.equal(hidden.reason, 'ranked_site_hidden');
  assert.deepEqual(hidden.detail.sites, [{ rank: 1, site: 'Tena', occluded_by: ['legend'] }]);
  // Only Loreto named by the answer: Tena is left out, explicitly, and its records are not counted visible.
  const ok = checkExport(answer({ overlays: covered }), { ...EXPECT, required: [2] });
  assert.equal(ok.ok, true, JSON.stringify(ok));
  assert.deepEqual(ok.picture_labels.map(l => l.site), ['Loreto']);
  assert.deepEqual(ok.hidden_sites, [{ rank: 1, site: 'Tena', occluded_by: ['legend'] }]);
  assert.deepEqual(ok.visible, { records: 1, asked_records: 1, sites: 1 });
  // With the legend moved off it, all three records and both sites are visible.
  assert.deepEqual(checkExport(answer(), EXPECT).visible, { records: 3, asked_records: 3, sites: 2 });
});

test('geometry: a marker 12 px from the top is under the title strip, also when only its disc reaches the strip', () => {
  const fitted = fitBoundsView(BBOX, EXPORT.frame);
  const view = { center: fitted.center, zoom: fitted.zoom, bbox: fitted.bbox, frame: EXPORT.frame };
  const top = (id, y) => ({ source: S, id, sex: 'female', country: 'Ecuador', year: 2022, lng: -77.5, lat: latAtY(view, y), subspecies: 'hermias' });
  const site = (id, label) => ({ label, merged_sites: 0, members: [{ key: id, locality: label, record_ids: [`${S}|${id}`] }] });
  // Anangu in the integration bbox: 12.1 px from the top. A second site whose centre (y 40) is below the 31 px strip
  // but whose 10 px disc reaches into it.
  const recs = [...REC, top('CAM4', 12.1), top('CAM5', 40)];
  const sites = [...answer().sites, site('CAM4', 'Anangu'), site('CAM5', 'Near')];
  const expect = { ...EXPECT, records: recs.map(({ subspecies, ...r }) => r), required: [1, 2],
    sites: [...EXPECT.sites, { rank: 3, site: 'Anangu', lat: recs[3].lat, lng: -77.5, ids: [`${S}|CAM4`] }, { rank: 4, site: 'Near', lat: recs[4].lat, lng: -77.5, ids: [`${S}|CAM5`] }] };
  const out = checkExport(answer({ records: recs, sites }), expect);
  assert.equal(out.ok, true, JSON.stringify(out));
  assert.deepEqual(out.hidden_sites, [{ rank: 3, site: 'Anangu', occluded_by: ['title'] }, { rank: 4, site: 'Near', occluded_by: ['title'] }]);
  assert.equal(out.visible.records, 3);
  assert.deepEqual(out.picture_labels.map(l => l.site), ['Tena', 'Loreto']);
  // Named by the answer: the picture is refused.
  assert.equal(checkExport(answer({ records: recs, sites }), { ...expect, required: [1, 2, 3] }).reason, 'ranked_site_hidden');
});

test('geometry: missing, malformed or optimistic picture geometry is not trusted', () => {
  const ok = answer();
  const edit = fn => {
    const a = structuredClone(ok);
    fn(a);
    return checkExport(a, EXPECT);
  };
  // An app without the geometry (the 16bd1f2 interface).
  assert.equal(checkExport(answer({ geometry: false }), EXPECT).reason, 'geometry_missing');
  assert.equal(edit(a => delete a.records[0].in_picture).reason, 'geometry_missing');
  assert.equal(edit(a => delete a.sites[0].radius).reason, 'geometry_missing');
  assert.equal(edit(a => { a.records[0].x = null; }).reason, 'geometry_missing');
  // Overlays well formed, with the title strip and the legend present.
  assert.equal(edit(a => { a.view.overlays[1].width = -5; }).reason, 'geometry_invalid');
  assert.equal(edit(a => { a.view.overlays[0].kind = 'banner'; }).reason, 'geometry_invalid');
  assert.deepEqual(edit(a => { a.view.overlays = a.view.overlays.filter(o => o.kind !== 'title'); }).detail, { missing_overlay: 'title' });
  assert.deepEqual(edit(a => { a.view.overlays = a.view.overlays.filter(o => o.kind !== 'legend'); }).detail, { missing_overlay: 'legend' });
  assert.equal(edit(a => { a.sites[0].label_boxes = [{ x: 1, y: 1, width: 'wide', height: 2 }]; }).reason, 'geometry_invalid');
  // Positions not where the view puts them.
  assert.equal(edit(a => { a.records[2].x += 5; }).reason, 'geometry_inconsistent');
  assert.equal(edit(a => { a.sites[1].y -= 3; }).reason, 'geometry_inconsistent');
  // Claims of visibility the geometry does not support.
  const legendOver = { kind: 'legend', x: 600, y: 120, width: 150, height: 100 };
  const claimed = answer({ overlays: [OVERLAYS[0], legendOver, OVERLAYS[2], OVERLAYS[3]] });
  claimed.sites[1] = { ...claimed.sites[1], occluded: false, occluded_by: [] };
  claimed.records[2] = { ...claimed.records[2], in_picture: true };
  assert.equal(checkExport(claimed, EXPECT).reason, 'geometry_inconsistent');
  assert.equal(edit(a => { a.records[0].in_picture = false; }).reason, 'geometry_inconsistent');
  // A label listed though its box is under an overlay.
  assert.equal(edit(a => { a.sites[1].label_boxes = [{ x: 0, y: 0, width: 120, height: 22 }]; }).reason, 'geometry_inconsistent');
});

test('the contract fixture example is a valid answer for its own request', () => {
  const contract = JSON.parse(readFileSync(new URL('./fixtures/maps-export-contract.json', import.meta.url), 'utf8'));
  const ex = contract.example;
  const out = checkExport({ ...ex.result, png_base64: PNG_OK.toString('base64') }, ex.expect);
  assert.equal(out.ok, true, JSON.stringify(out));
  assert.deepEqual(out.visible, ex.checked.visible);
  assert.deepEqual(out.hidden_sites, ex.checked.hidden_sites);
  assert.deepEqual(out.picture_labels.map(l => l.site), ex.checked.picture_labels);
});

// ---------- the worker process ----------
/** A Playwright stand-in for one scenario: { module (PLAYWRIGHT_MODULE), log() (what the worker did) }. */
function fakeModule(scenario) {
  const dir = dirOf('pw');
  const logFile = join(dir, 'log.jsonl');
  writeFileSync(join(dir, 'scenario.json'), JSON.stringify({ log: logFile, ...scenario }));
  const module = join(dir, 'pw.mjs');
  writeFileSync(module, `export * from ${JSON.stringify(`${FAKE_PW}?s=${encodeURIComponent(join(dir, 'scenario.json'))}`)};\n`);
  const log = () => (existsSync(logFile) ? readFileSync(logFile, 'utf8').trim().split('\n').map(l => JSON.parse(l)) : []);
  const fake = { module, log, get: k => log().filter(e => k in e).map(e => e[k]) };
  fakes.push(fake);
  return fake;
}
const URL_OK = mapsUrl({ species: ['Tithorea harmonia'], sources: ['sanger'], sex: 'female', sites: true, style: 'light', colorBy: 'subspecies', bbox: BBOX, exportMode: true }, OFFICIAL_MAPS_URL);

test('renderer off: no PLAYWRIGHT_MODULE or MAPS_RENDER=pillow, said once, nothing started', async () => {
  const logs = [];
  const r = createMapRenderer({ env: {}, log: m => logs.push(m), procTable: () => assert.fail('nothing to watch') });
  const a = await r.render(URL_OK);
  assert.equal(a.ok, false);
  assert.equal(a.reason, 'renderer_unavailable');
  await r.render(URL_OK);
  assert.equal(logs.length, 1);
  const fake = fakeModule({ result: answer() });
  const off = await createMapRenderer({ env: { PLAYWRIGHT_MODULE: fake.module, MAPS_RENDER: 'pillow' }, log: () => {} }).render(URL_OK);
  assert.equal(off.reason, 'renderer_off');
  assert.deepEqual(fake.log(), []);
});

test('startup check: a module without chromium.launch turns the app picture off', async () => {
  const dir = dirOf('badmod');
  const module = join(dir, 'pw.mjs');
  writeFileSync(module, 'export const firefox = {};\n');
  const out = await createMapRenderer({ env: { PLAYWRIGHT_MODULE: module }, log: () => {} }).render(URL_OK);
  assert.equal(out.reason, 'renderer_unavailable');
  assert.match(out.detail.message, /no chromium\.launch/);
  // MAPS_RENDER_CHROMIUM: used only when it is an existing executable file; nothing is installed.
  const fake = fakeModule({ result: answer() });
  const notExec = join(dir, 'chrome-headless-shell');
  writeFileSync(notExec, '');
  for (const [path, why] of [[join(dir, 'nope'), 'MAPS_RENDER_CHROMIUM missing'], [notExec, 'MAPS_RENDER_CHROMIUM not executable'], [dir, 'MAPS_RENDER_CHROMIUM not a file']]) {
    const off = await createMapRenderer({ env: { PLAYWRIGHT_MODULE: fake.module, MAPS_RENDER_CHROMIUM: path }, log: () => {} }).render(URL_OK);
    assert.deepEqual([off.reason, off.detail], ['renderer_unavailable', why]);
  }
  const ok = await createMapRenderer({ env: { PLAYWRIGHT_MODULE: fake.module, MAPS_RENDER_CHROMIUM: process.execPath }, memAvailable: () => 8 * 1024 ** 3, log: () => {} }).render(URL_OK);
  assert.equal(ok.ok, true, JSON.stringify(ok.detail));
  assert.deepEqual(fake.get('url').length, 1);
});

test('worker: opens only the URL given, evaluates only the export call, returns its answer; TMPDIR removed', async () => {
  const fake = fakeModule({ result: answer() });
  const r = createMapRenderer({ env: { PLAYWRIGHT_MODULE: fake.module, PATH: process.env.PATH }, memAvailable: () => 8 * 1024 ** 3, log: () => {} });
  const out = await r.render(URL_OK);
  assert.equal(out.ok, true, JSON.stringify({ ...out, result: undefined }));
  assert.equal(checkExport(out.result, EXPECT).ok, true);
  assert.deepEqual(fake.get('url'), [URL_OK]);
  assert.deepEqual(fake.get('evaluate'), ['() => window.ithomiiniMapsExport()']);
  assert.equal(fake.get('launch')[0].headless, true);
  assert.deepEqual(fake.get('context')[0].viewport, EXPORT.viewport);
  assert.equal(fake.get('context')[0].acceptDownloads, false);
  const [tmp] = fake.get('tmpdir');
  assert.ok(tmp && !existsSync(tmp), `temp dir ${tmp} removed`);
  assert.ok(out.timing.render >= 0 && out.timing.total >= out.timing.render);
  // A URL off the official app never reaches the worker.
  const evil = await r.render(URL_OK.replace('rapidspeciation.github.io', 'evil.test'));
  assert.equal(evil.reason, 'url_rejected');
  assert.equal(fake.get('url').length, 1);
});

test('clean: week-old app pictures and stale render temp folders go, recent ones stay', async () => {
  const dir = dirOf('clean');
  const tmp = dirOf('tmproot');
  const old = (Date.now() - 8 * 86400_000) / 1000;
  const hourAgo = (Date.now() - 3600_000) / 1000;
  for (const [where, name, t] of [[dir, 'app-old.png', old], [dir, 'app-new.png', null], [dir, 'snap-x.jpg', old], [tmp, 'maps-render-stale1', hourAgo], [tmp, 'maps-render-fresh1', null], [tmp, 'maps-render-pw-abcdef', hourAgo]]) {
    const f = join(where, name);
    if (name.startsWith('maps-render-')) mkdirSync(f);
    else writeFileSync(f, 'x');
    if (t) utimesSync(f, t, t);
  }
  const out = await createMapRenderer({ dir, tmpRoot: () => tmp, env: { PLAYWRIGHT_MODULE: fakeModule({ result: answer() }).module }, memAvailable: () => 8 * 1024 ** 3, log: () => {} }).render(URL_OK);
  assert.equal(out.ok, true);
  assert.deepEqual(readdirSync(dir).sort(), ['app-new.png', 'snap-x.jpg']);
  assert.deepEqual(readdirSync(tmp).sort(), ['maps-render-fresh1', 'maps-render-pw-abcdef']);
});

test('worker: a launch failure and a missing export interface fall back, the temp dir removed', async () => {
  const failing = fakeModule({ launchFail: 'no display' });
  const a = await createMapRenderer({ env: { PLAYWRIGHT_MODULE: failing.module }, memAvailable: () => 8 * 1024 ** 3, log: () => {} }).render(URL_OK);
  assert.equal(a.reason, 'launch_failed');
  assert.match(a.detail.message, /no display/);
  assert.ok(!existsSync(failing.get('tmpdir')[0]));
  const old = fakeModule({ noInterface: true });
  const b = await createMapRenderer({ env: { PLAYWRIGHT_MODULE: old.module }, memAvailable: () => 8 * 1024 ** 3, log: () => {} }).render(URL_OK);
  assert.equal(b.reason, 'no_export_interface');
  assert.ok(!existsSync(old.get('tmpdir')[0]));
});

test('deadline: a hung export is killed with everything started under it, the temp dir removed; others survive', async () => {
  const fake = fakeModule({ hang: 'export', grandchild: true, browserGroup: true });
  const running = createMapRenderer({ env: { PLAYWRIGHT_MODULE: fake.module }, memAvailable: () => 8 * 1024 ** 3, deadlineMs: 2500, log: () => {} }).render(URL_OK);
  // A process that only names the run's temp folder (an `ls` of it, a log tail) is not the render's: it is left alone.
  assert.ok(await waitFor(() => fake.get('tmpdir').length, 2000));
  const onlooker = spawn(process.execPath, ['-e', 'setInterval(() => {}, 1000)', '--', join(fake.get('tmpdir')[0], 'profile')], { stdio: 'ignore' });
  started.push(onlooker.pid);
  const out = await running;
  assert.ok(alive(onlooker.pid), 'a process naming the temp folder, not started by the render, still runs');
  onlooker.kill('SIGKILL');
  assert.equal(out.reason, 'deadline');
  assert.ok(out.timing.render < 2500 + 1500, `killed ${out.timing.render} ms after spawn`);
  const [pid] = fake.get('grandchild');
  const [browser] = fake.get('browser');
  const [renderer] = fake.get('renderer');
  assert.ok(pid && !alive(pid), `grandchild ${pid} killed`);
  // The browser in its own process group, and its helper in another, found as descendants of the worker and killed.
  assert.ok(browser && !alive(browser), `browser ${browser} killed`);
  assert.ok(renderer && !alive(renderer), `helper ${renderer} killed`);
  assert.ok(!existsSync(fake.get('tmpdir')[0]));
  // Hung before the worker's own timer can answer (in the browser launch): the parent's deadline kills the group.
  const stuck = fakeModule({ hang: 'launch', grandchild: true });
  const late = await createMapRenderer({ env: { PLAYWRIGHT_MODULE: stuck.module }, memAvailable: () => 8 * 1024 ** 3, deadlineMs: 2000, log: () => {} }).render(URL_OK);
  assert.equal(late.reason, 'deadline');
  assert.ok(late.timing.render < 2000 + 1500, `killed ${late.timing.render} ms after spawn`);
  await new Promise(r => setTimeout(r, 200));
  assert.ok(!alive(stuck.get('grandchild')[0]));
  assert.ok(!existsSync(stuck.get('tmpdir')[0]));
});

// ---------- who cleans up when the one who started the render is gone ----------
const HOST = new URL('./fixtures/maps-render-host.mjs', import.meta.url);
const WORKER_FILE = new URL('../server/maps-render-worker.mjs', import.meta.url);
/** The render's processes once the export hangs: { worker, browser, helper, grandchild, tmp }. */
async function hungRender(fake) {
  assert.ok(await waitFor(() => fake.get('evaluate').length && fake.get('renderer').length && fake.get('grandchild').length, 10_000), 'render reached the export');
  const [worker] = fake.get('worker');
  const run = { worker, browser: fake.get('browser')[0], helper: fake.get('renderer')[0], grandchild: fake.get('grandchild')[0], tmp: fake.get('tmpdir')[0] };
  for (const k of ['worker', 'browser', 'helper', 'grandchild']) assert.ok(alive(run[k]), `${k} runs`);
  assert.notEqual(procStat(run.browser).pgrp, procStat(worker).pgrp, 'the browser has a process group of its own');
  assert.notEqual(procStat(run.helper).pgrp, procStat(run.browser).pgrp, 'its helper another');
  return run;
}
const allGone = run => ['worker', 'browser', 'helper', 'grandchild'].every(k => !alive(run[k])) && !existsSync(run.tmp);
const which = run => JSON.stringify(Object.fromEntries(['worker', 'browser', 'helper', 'grandchild'].map(k => [k, alive(run[k])]).concat([['tmp', existsSync(run.tmp)]])));

test('lifecycle: the tools thread terminated mid-export: its worker loses the lease and kills the browser and helpers', async () => {
  const fake = fakeModule({ hang: 'export', grandchild: true, browserGroup: true });
  // A long deadline: only the end of the thread can stop this render in time.
  const thread = new Worker(HOST, { workerData: { module: fake.module, deadlineMs: 60_000, url: URL_OK } });
  const run = await hungRender(fake);
  assert.equal(procStat(run.worker).ppid, process.pid, 'the worker is a child of this process, whose pid outlives the thread');
  const t0 = Date.now();
  await thread.terminate();
  assert.ok(await waitFor(() => allGone(run), 5000), `left after the thread ended: ${which(run)}`);
  assert.ok(Date.now() - t0 < 5000);
});

test('lifecycle: the server process killed mid-export: the worker sees the lease end and kills the browser and helpers', async () => {
  const fake = fakeModule({ hang: 'export', grandchild: true, browserGroup: true });
  const server = spawn(process.execPath, [fileURLToPath(HOST), JSON.stringify({ module: fake.module, deadlineMs: 60_000, url: URL_OK })], { stdio: 'ignore' });
  started.push(server.pid);
  const run = await hungRender(fake);
  assert.equal(procStat(run.worker).ppid, server.pid);
  server.kill('SIGKILL');
  assert.ok(await waitFor(() => allGone(run), 5000), `left after the server died: ${which(run)}`);
});

test('lifecycle: the worker on its own (lease held, nobody killing it) stops a hung launch at its deadline', async () => {
  const fake = fakeModule({ hang: 'launch', grandchild: true, browserGroup: true });
  const tmp = mkdtempSync(join(tmpdir(), 'maps-render-'));
  made.push(tmp);
  const worker = spawn(process.execPath, [fileURLToPath(WORKER_FILE)], { detached: true, cwd: tmp, env: { PATH: process.env.PATH, TMPDIR: tmp, PLAYWRIGHT_MODULE: fake.module }, stdio: ['pipe', 'pipe', 'ignore'] });
  started.push(worker.pid);
  worker.stdout.resume();
  // The job, and stdin kept open: the lease is held the whole time.
  worker.stdin.write(`${JSON.stringify({ url: URL_OK, viewport: EXPORT.viewport, deadline_ms: 1000 })}\n`);
  const exited = new Promise(r => worker.on('exit', (code, signal) => r({ code, signal })));
  assert.ok(await waitFor(() => fake.get('renderer').length && fake.get('grandchild').length, 5000));
  const run = { worker: worker.pid, browser: fake.get('browser')[0], helper: fake.get('renderer')[0], grandchild: fake.get('grandchild')[0], tmp };
  const t0 = Date.now();
  const { code } = await exited;
  assert.equal(code, 124);
  assert.ok(Date.now() - t0 < 4000);
  assert.ok(await waitFor(() => allGone(run), 2000), `left after the worker's deadline: ${which(run)}`);
  worker.stdin.destroy();
});

test('process tree: kills only what it saw start under the root and is still the same process; no group kill unproven', async () => {
  // A /proc of hand-written stat files and a recorded kill(): nothing real is signalled.
  const proc = dirOf('proc');
  const stat = (pid, ppid, pgrp, start, state = 'S') => {
    mkdirSync(join(proc, String(pid)), { recursive: true });
    writeFileSync(join(proc, String(pid), 'stat'), `${pid} (n ame) ${state} ${ppid} ${pgrp} ${'0 '.repeat(16)}${start} 0 0\n`);
  };
  const gone = pid => rmSync(join(proc, String(pid)), { recursive: true, force: true });
  const sent = [];
  const sig = (pid, s) => {
    sent.push([pid, s]);
    if (s === 'SIGKILL' && pid > 0) gone(pid);
  };
  let rootAlive = true;
  stat(100, 1, 100, 10); // the root (a worker leading its group)
  stat(200, 100, 200, 20); // the browser, a group of its own
  stat(300, 200, 300, 30); // its helper, another group
  stat(400, 1, 400, 40); // unrelated, its command line could name anything
  stat(500, 100, 100, 50); // a child in the root's group
  const tree = processTree({ pid: 100, alive: () => rootAlive, group: true }, { proc, sig });
  assert.equal(tree.observe(), 3);
  // The browser dies; its helper, now a child of init, is still known by pid and start time.
  gone(200);
  stat(300, 1, 300, 30);
  // pid 500 reused by another process (another start time): no longer the render's.
  stat(500, 1, 500, 99);
  rootAlive = false;
  const out = await tree.reap({ withRoot: true, waitMs: 200 });
  assert.deepEqual(out.left, []);
  // Stopped, then killed: the helper and its group (it leads it, identity checked); nothing of 400 or of the reused 500,
  // and no group kill of the root (reaped: its pid is not proof any more).
  assert.deepEqual(sent, [[300, 'SIGSTOP'], [-300, 'SIGKILL'], [300, 'SIGKILL']]);
  assert.ok(existsSync(join(proc, '400')) && existsSync(join(proc, '500')));
  // With the root still the same process: it is stopped first, its group killed.
  sent.length = 0;
  stat(600, 1, 600, 60);
  const tree2 = processTree({ pid: 600, alive: () => true, group: true }, { proc, sig });
  stat(700, 600, 600, 70);
  await tree2.reap({ withRoot: true, waitMs: 200 });
  assert.deepEqual(sent, [[600, 'SIGSTOP'], [700, 'SIGSTOP'], [700, 'SIGKILL'], [-600, 'SIGKILL'], [600, 'SIGKILL']]);
});

// ---------- the worker's answer as bytes ----------
const SPLIT = fileURLToPath(new URL('./fixtures/split-utf8-worker.mjs', import.meta.url));
test('output: an answer cut inside multibyte characters arrives whole; the output cap counts bytes', async () => {
  const fake = fakeModule({});
  const r = createMapRenderer({ env: { PLAYWRIGHT_MODULE: fake.module }, workerPath: SPLIT, memAvailable: () => 8 * 1024 ** 3, log: () => {} });
  const out = await r.render(URL_OK);
  assert.equal(out.ok, true, JSON.stringify(out));
  assert.equal(out.result.title, 'Tithorea harmonia · hembras ♀ · Perú');
  assert.equal(out.result.records[0].country, 'Perú');
  assert.ok(!JSON.stringify(out.result).includes('�'));
  // 600 'é' are 1200 bytes: over a 1000-byte cap, though 600 characters.
  const big = await createMapRenderer({ env: { PLAYWRIGHT_MODULE: fake.module }, workerPath: SPLIT, workerArgs: ['big'], stdoutMax: 1000, memAvailable: () => 8 * 1024 ** 3, log: () => {} }).render(URL_OK);
  assert.equal(big.reason, 'output_too_large');
  const fits = await createMapRenderer({ env: { PLAYWRIGHT_MODULE: fake.module }, workerPath: SPLIT, workerArgs: ['big'], stdoutMax: 1300, memAvailable: () => 8 * 1024 ** 3, log: () => {} }).render(URL_OK);
  assert.equal(fits.result.title, 'é'.repeat(600));
});

test('memory: low MemAvailable starts nothing; the RSS cap of the worker and its descendants kills the group', async () => {
  const fake = fakeModule({ hang: 'export' });
  const low = await createMapRenderer({ env: { PLAYWRIGHT_MODULE: fake.module }, memAvailable: () => 1024 ** 3, log: () => {} }).render(URL_OK);
  assert.equal(low.reason, 'low_memory');
  assert.equal(low.detail.mem_available_mb, 1024);
  assert.deepEqual(fake.get('url'), []);
  // The real process table, with the render worker's RSS made 3 GB.
  const swollen = () => {
    const t = procTable();
    for (const e of t.values()) if (e.cmd.includes('maps-render-worker') && !e.cmd.includes('--check')) e.rss = 3 * 1024 ** 3;
    return t;
  };
  const capped = await createMapRenderer({ env: { PLAYWRIGHT_MODULE: fake.module }, memAvailable: () => 8 * 1024 ** 3, procTable: swollen, pollMs: 100, deadlineMs: 20_000, log: () => {} }).render(URL_OK);
  assert.equal(capped.reason, 'memory_cap');
  assert.ok(capped.timing.render < 3000, `killed ${capped.timing.render} ms after spawn`);
  assert.ok(!existsSync(fake.get('tmpdir').at(-1)));
  assert.ok(treeRss(procTable(), process.pid) > 0);
});

test('queue: one render at a time per thread (wait at most queueMs), and one per server process across threads', async () => {
  const fake = fakeModule({ hang: 'export' });
  const r = createMapRenderer({ env: { PLAYWRIGHT_MODULE: fake.module }, memAvailable: () => 8 * 1024 ** 3, deadlineMs: 3000, queueMs: 400, log: () => {} });
  await r.availability();
  const first = r.render(URL_OK);
  await new Promise(res => setTimeout(res, 50));
  const second = await r.render(URL_OK);
  assert.equal(second.reason, 'queue_wait');
  assert.equal(second.detail, 'this thread was rendering');
  assert.equal((await first).reason, 'deadline');
  // Another thread's render worker (a child of this process in /proc): this one waits, then falls back.
  const other = () => new Map([...procTable(), [2 ** 22 + 1, { ppid: process.pid, start: 0, rss: 0, cmd: `${process.execPath} server/maps-render-worker.mjs` }]]);
  const r2 = createMapRenderer({ env: { PLAYWRIGHT_MODULE: fakeModule({ result: answer() }).module }, memAvailable: () => 8 * 1024 ** 3, queueMs: 400, procTable: other, log: () => {} });
  const busy = await r2.render(URL_OK);
  assert.equal(busy.reason, 'queue_wait');
  assert.equal(busy.detail, 'another tools thread was rendering');
});

// ---------- maps_snapshot: the app picture or Pillow ----------
const MAPS = 'https://maps.test/ithomiini_maps/';
const pt = (id, name, ssp, sex, lat, lng, source, place, date) => ({ id, scientific_name: name, genus: name.split(' ')[0], species: name.split(' ')[1], subspecies: ssp, sex, lat, lng, source, collection_location: place, observation_date: date, country: 'Ecuador' });
const POINTS = {
  'map_points_sanger.json': [
    pt('CAM000011', 'Tithorea harmonia', 'hermias', 'female', -0.99, -77.81, 'Sanger Institute', 'Tena', '2022-01-02'),
    pt('CAM000012', 'Tithorea harmonia', null, 'female', -0.991, -77.812, 'Sanger Institute', 'Tena', '2023-05-01'),
    pt('CAM000013', 'Tithorea harmonia', 'hermias', 'female', -0.71, -77.19, 'Sanger Institute', 'Loreto', '2022-07-01'),
    pt('CAM000014', 'Tithorea harmonia', 'hermias', 'male', -0.99, -77.81, 'Sanger Institute', 'Tena', '2022-01-02'),
    // Loja: over 150 km from Tena.
    pt('CAM000015', 'Tithorea harmonia', 'hermias', 'female', -3.99, -79.2, 'Sanger Institute', 'Loja', '2021-01-01'),
    // No sex in the maps; the workbook says female.
    pt('CAM000016', 'Oleria onega', null, null, -0.99, -77.81, 'Sanger Institute', 'Tena', '2022-01-02'),
  ],
  'map_points_inaturalist.json': [pt('9001', 'Tithorea harmonia', null, null, -0.95, -77.86, 'iNaturalist', 'Ikiam', '2019-08-12T13:15')],
};
const FILES = {
  'data_manifest.json': { generated_at: '2026-10-01T00:00:00Z', sources: { 'Sanger Institute': { file: 'map_points_sanger.json' }, iNaturalist: { file: 'map_points_inaturalist.json' } } },
  ...POINTS,
  'taxonomy_synonyms.json': { synonyms: [] },
};
const APP_RECORDS = Object.values(POINTS)
  .flat()
  .map(p => ({ source: p.source, id: p.id, name: p.scientific_name, ssp: p.subspecies, sex: sexOf(p.sex), country: p.country, year: yearOf(p.observation_date), lng: p.lng, lat: p.lat, place: p.collection_location }));
const fetchImpl = async url => {
  if (url.startsWith('https://tiles.test/')) return new Response(encodePng(1, 1, () => [200, 200, 200]));
  const path = new URL(url).pathname.replace('/ithomiini_maps/data/', '');
  return path in FILES ? Response.json(FILES[path]) : new Response('missing', { status: 404 });
};
const appPngFile = (() => {
  let file = null;
  return () => {
    if (!file) writeFileSync((file = join(dirOf('png'), 'app.png')), mapPng());
    return file;
  };
})();
async function snapshotTools(app = {}, env = null) {
  // CAM000011: the workbook says male, the maps female; the maps' sex wins, so the app's filter agrees with the tool.
  const seed = { Collection_data: [{ row: 2, values: { CAM_ID: 'CAM000016', SPECIES: 'Oleria onega', Sex: 'female', Country: 'Ecuador' } }, { row: 3, values: { CAM_ID: 'CAM000014', SPECIES: 'Tithorea harmonia', Sex: 'male', Country: 'Ecuador' } }, { row: 4, values: { CAM_ID: 'CAM000011', SPECIES: 'Tithorea harmonia', Sex: 'male', Country: 'Ecuador' } }] };
  const store = new Store({ localMode: true }, { sheets: new LocalSheets(seed) });
  await store.sync({ sheets: Object.keys(seed) });
  const fake = fakeModule({ app: { records: APP_RECORDS, png: appPngFile(), ...app } });
  const renderOptions = env ?? { env: { PLAYWRIGHT_MODULE: fake.module }, memAvailable: () => 8 * 1024 ** 3, log: () => {} };
  const tools = createMapsTools({ store, fetchImpl, mapsBase: MAPS, snapshotDir: dirOf('snap'), snapshotOptions: { tileUrl: 'https://tiles.test/{z}/{x}/{y}' }, renderOptions });
  return { tools, fake };
}

test('maps_snapshot, app picture: best site + 150 km, parity checked, labels and caption from what the frame shows', async () => {
  const { tools, fake } = await snapshotTools();
  const out = await tools.run('maps_snapshot', { species: ['Tithorea harmonia'], sex: 'female', country: ['Ecuador'], lang: 'es' });
  assert.equal(out.renderer, 'app', JSON.stringify(out.degraded));
  assert.equal(out.degraded, null);
  assert.equal(out.image_type, 'png');
  assert.equal(out.image_size, '1600x1200');
  assert.ok(existsSync(out.image) && readFileSync(out.image).length === out.image_bytes);
  // The app was asked the tool's frame (Tena and Loreto, not Loja) by bbox=, with export=1, the sites layer and settings.
  const asked = new URL(fake.get('url')[0]);
  assert.equal(`${asked.origin}${asked.pathname}`, OFFICIAL_MAPS_URL);
  assert.equal(asked.searchParams.get('export'), '1');
  assert.deepEqual([asked.searchParams.get('style'), asked.searchParams.get('color_by'), asked.searchParams.get('sex')], ['light', 'subspecies', 'female']);
  const box = asked.searchParams.get('bbox').split(',').map(Number);
  assert.ok(box[0] < -77.81 && box[2] > -77.19 && box[1] > -3.99, box);
  assert.deepEqual(out.framed.asked, box);
  // Four females in all (Loja's too), three in the frame; one without a subspecies.
  assert.equal(out.records.on_picture, 3);
  assert.equal(out.records.app_total, 4);
  assert.equal(out.legend_unknown, 1);
  assert.deepEqual(out.picture_labels.map(l => [l.rank, l.site, l.label_on_picture]), [[1, 'Tena', 'Tena'], [2, 'Loreto', 'Loreto']]);
  assert.deepEqual(out.sites.map(s => s.site), ['Tena', 'Loreto']);
  assert.ok(out.sites.every(s => !('on_map' in s)), 'no numbers promised on the app picture');
  assert.deepEqual(out.elsewhere.map(s => s.site), ['Loja']);
  assert.equal(out.caption, 'Tithorea harmonia · hembras · Ecuador; 4 registros en 3 sitios, 3 en el mapa; 1 sin subespecie; 2021–2023');
  // The link: the same filters, sites layer, basemap, colour and frame, without export mode.
  const link = new URL(out.map_url);
  assert.equal(link.searchParams.has('export'), false);
  for (const k of ['sp', 'source', 'country', 'sex', 'sites', 'site_labels', 'site_min', 'style', 'color_by', 'bbox']) assert.equal(link.searchParams.get(k), asked.searchParams.get(k), k);
});

test('maps_snapshot, app picture: a bbox asked frames the app\'s whole filtered set; the tool\'s records are the ones inside it', async () => {
  const { tools } = await snapshotTools();
  const out = await tools.run('maps_snapshot', { species: ['Tithorea harmonia'], sex: 'female', bbox: [-77.9, -1.05, -77.7, -0.95] });
  assert.equal(out.renderer, 'app', JSON.stringify(out.degraded));
  assert.equal(out.records.female, 2);
  assert.equal(out.records.app_total, 4);
  assert.equal(out.framed.by, 'bbox asked');
  // frame: 'all' frames every record, Loja too.
  const all = await tools.run('maps_snapshot', { species: ['Tithorea harmonia'], sex: 'female', frame: 'all' });
  assert.equal(all.renderer, 'app', JSON.stringify(all.degraded));
  assert.equal(all.records.on_picture, 4);
  assert.equal(all.framed.by, 'all records');
});

test('maps_snapshot, app picture: a named site under the legend falls back with ranked_site_hidden; an app without geometry too', async () => {
  // The legend left in its default bottom-left corner, over Tena (rank 1).
  const covered = await (await snapshotTools({ geometry: { overlays: [OVERLAYS[0], LEGEND_BOTTOM_LEFT, OVERLAYS[2], OVERLAYS[3]] } })).tools
    .run('maps_snapshot', { species: ['Tithorea harmonia'], sex: 'female', country: ['Ecuador'] });
  assert.equal(covered.renderer, 'pillow');
  assert.equal(covered.degraded.reason, 'ranked_site_hidden');
  assert.deepEqual(covered.degraded.detail.sites, [{ rank: 1, site: 'Tena', occluded_by: ['legend'] }]);
  // The Pillow picture still numbers every site it frames.
  assert.deepEqual(covered.picture_labels.map(l => l.site), ['Tena', 'Loreto']);
  // The app interface of 16bd1f2 (no picture geometry): not trusted.
  const legacy = await (await snapshotTools({ geometry: false })).tools.run('maps_snapshot', { species: ['Tithorea harmonia'], sex: 'female', country: ['Ecuador'] });
  assert.equal(legacy.renderer, 'pillow');
  assert.equal(legacy.degraded.reason, 'geometry_missing');
});

test('maps_snapshot, app picture with a bbox: the caption separates the area asked from what the frame shows around it', async () => {
  const { tools } = await snapshotTools();
  // One female record (Loreto) in the box; the 4:3 frame is wider than the box.
  const one = await tools.run('maps_snapshot', { species: ['Tithorea harmonia'], sex: 'female', bbox: [-77.25, -0.75, -77.15, -0.68], lang: 'es' });
  assert.equal(one.renderer, 'app', JSON.stringify(one.degraded));
  assert.equal(one.records.asked_on_picture, 1);
  assert.match(one.caption, /; 1 registro en 1 sitio en la zona pedida, 1 visible en la imagen; /);
  // Tena's two in the box; the frame also shows records around it, counted apart.
  const box = [-77.9, -1.05, -77.7, -0.95];
  const two = await tools.run('maps_snapshot', { species: ['Tithorea harmonia'], sex: 'female', bbox: box, lang: 'en' });
  assert.equal(two.renderer, 'app', JSON.stringify(two.degraded));
  assert.equal(two.records.asked_on_picture, 2);
  const around = two.records.on_picture - two.records.asked_on_picture;
  assert.match(two.caption, /; 2 records at 1 site in the area asked, 2 visible in the picture(; |$)/);
  if (around > 0) assert.match(two.caption, new RegExp(`the picture also shows ${around} records? just outside the area`));
  else assert.doesNotMatch(two.caption, /just outside/);
});

test('maps_snapshot, no sex asked: map-native sexes compared, the workbook not consulted for a fallback', async () => {
  const { tools } = await snapshotTools();
  const out = await tools.run('maps_snapshot', { species: ['Tithorea harmonia', 'Oleria onega'] });
  assert.equal(out.renderer, 'app', JSON.stringify(out.degraded));
  assert.equal(new URL(out.map_url).searchParams.get('color_by'), 'species');
});

test('maps_snapshot falls back to Pillow with the reason and the same records, frame and ranking', { skip: !pillow && 'no python3 with Pillow' }, async () => {
  const pillowOnly = (await snapshotTools({}, false)).tools;
  const plain = await pillowOnly.run('maps_snapshot', { species: ['Tithorea harmonia'], sex: 'female' });
  assert.equal(plain.renderer, 'pillow');
  assert.equal(plain.degraded.reason, 'renderer_unavailable');
  assert.deepEqual(plain.degraded.not_kept, ['basemap', 'color_by', 'site_labels', 'legend_style']);
  assert.deepEqual(plain.picture_labels.map(l => l.label_on_picture), ['1', '2']);
  const same = out => {
    assert.deepEqual(out.sites.map(s => [s.on_map, s.rank, s.site, s.records]), plain.sites.map(s => [s.on_map, s.rank, s.site, s.records]));
    assert.deepEqual(out.records, plain.records);
    assert.deepEqual(out.framed, plain.framed);
  };
  // Coordinates moved by the app: parity mismatch.
  const moved = await (await snapshotTools({ shift: 0.001 })).tools.run('maps_snapshot', { species: ['Tithorea harmonia'], sex: 'female' });
  assert.equal(moved.renderer, 'pillow');
  assert.equal(moved.degraded.reason, 'parity_mismatch');
  assert.equal(moved.degraded.detail.fields.coords, 4);
  same(moved);
  // The app ignored bbox=: wrong view.
  const ignored = await (await snapshotTools({ ignoreBbox: true })).tools.run('maps_snapshot', { species: ['Tithorea harmonia'], sex: 'female' });
  assert.equal(ignored.degraded.reason, 'view_mismatch');
  same(ignored);
  // A filter the app does not have: no browser started.
  const { tools, fake } = await snapshotTools();
  const years = await tools.run('maps_snapshot', { species: ['Tithorea harmonia'], sex: 'female', years: { from: 2022 } });
  assert.equal(years.degraded.reason, 'unsupported_filter:years');
  assert.equal(years.records.female, (await tools.run('maps_localities', { taxa: ['Tithorea harmonia'], sex: 'female', years: { from: 2022 } })).records.female);
  // A sex asked that the workbook fills in for one of the project's records.
  const filled = await tools.run('maps_snapshot', { species: ['Oleria onega'], sex: 'female' });
  assert.equal(filled.degraded.reason, 'sex_from_workbook');
  assert.equal(filled.degraded.detail.records_affected, 1);
  assert.deepEqual(fake.get('url'), []);
});

test('live: one render of the deployed maps app', { skip: process.env.MAPS_RENDER_LIVE !== '1' && 'MAPS_RENDER_LIVE=1 to run' }, async () => {
  const r = createMapRenderer({});
  const url = mapsUrl({ species: ['Tithorea harmonia'], sources: ['sanger', 'gbif_unam', 'gbif_other', 'dore', 'inaturalist'], country: ['Ecuador'], sex: 'female', sites: true, style: 'light', colorBy: 'subspecies', bbox: [-77.95, -1.2, -77.1, -0.6], exportMode: true }, OFFICIAL_MAPS_URL);
  const out = await r.render(url);
  console.log(JSON.stringify({ ok: out.ok, reason: out.reason, detail: out.detail, timing: out.timing, version: out.result?.version, records: out.result?.records?.length }));
  assert.equal(out.ok, true);
  assert.equal(out.result.version, 1);
});


test('process tree revalidates newly discovered identities before stopping them', async () => {
  const proc = dirOf('rescan-reuse');
  const stat = (pid, ppid, start) => {
    mkdirSync(join(proc, String(pid)), { recursive: true });
    writeFileSync(join(proc, String(pid), 'stat'), `${pid} (fake) S ${ppid} ${pid} ${'0 '.repeat(16)}${start} 0 0\n`);
  };
  stat(100, 1, 10);
  stat(200, 100, 20);
  const sent = [];
  const tree = processTree({ pid: 100, alive: () => true, group: true }, { proc, sig: (pid, signal) => {
    sent.push([pid, signal]);
    if (pid === 200 && signal === 'SIGSTOP') {
      stat(300, 200, 30);
      stat(400, 200, 40);
    }
    // Both new children were in the rescan snapshot. Reuse the second PID
    // between its discovery and the callback which would send SIGSTOP.
    if (pid === 300 && signal === 'SIGSTOP') stat(400, 1, 99);
    if (pid > 0 && signal === 'SIGKILL') rmSync(join(proc, String(pid)), { recursive: true, force: true });
  } });
  tree.observe();
  await tree.reap({ waitMs: 100 });
  assert.ok(sent.some(([pid, signal]) => pid === 300 && signal === 'SIGSTOP'));
  assert.ok(!sent.some(([pid]) => Math.abs(pid) === 400), 'replacement must receive no signal');
  assert.equal(procStat(400, proc).start, 99);
});
