// maps_snapshot's app picture (server/maps-render.mjs): the checks of what the app returns, on hand-made answers
// and PNGs (tests/helpers/maps-render.mjs). The worker process: maps-render-worker.test.mjs; maps_snapshot:
// maps-render.test.mjs.
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
import { EXPORT, OFFICIAL_MAPS_URL, checkExport, clearingFrame, createMapRenderer, decodePng, fitBoundsView, pixelChecks, procStat, procTable, processTree, projectPoint, renderUrlProblem, treeRss, unsupportedFilter, visibleBounds } from '../server/maps-render.mjs';
import { LEGEND_BOTTOM_LEFT, OVERLAYS, withGeometry } from './fixtures/export-geometry.mjs';
import { BBOX, EXPECT, GREEN, PNG_OK, REC, S, URL_OK, alive, answer, chunk, crc, dirOf, drawPng, encodePng, fakeModule, fakes, made, mapPng, started, waitFor } from './helpers/maps-render.mjs';

const PILOT = new URL('../reports/assistant_maps_photos_20261102/map-pilot/', import.meta.url);

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
  const ok = `${OFFICIAL_MAPS_URL}?sp=Tithorea+harmonia&source=sanger&sex=female&sites=1&style=light&color_by=subspecies&bbox=-78,-1.3,-76.9,-0.5&export=1`;
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

test('geometry: a hidden named site comes with a wider frame that clears it from the legend in either bottom corner', () => {
  const covered = [OVERLAYS[0], LEGEND_BOTTOM_LEFT, OVERLAYS[2], OVERLAYS[3]];
  const hidden = checkExport(answer({ overlays: covered }), EXPECT);
  assert.equal(hidden.reason, 'ranked_site_hidden');
  const next = hidden.detail.reframe;
  assert.ok(Array.isArray(next) && next.length === 4, JSON.stringify(hidden.detail));
  assert.ok(next[0] <= BBOX[0] && next[1] <= BBOX[1] && next[2] >= BBOX[2] && next[3] >= BBOX[3], 'the frame asked stays inside');
  // Drawn in that frame, Tena and Loreto are clear with the legend bottom-left (where it was) or bottom-right.
  for (const legend of [LEGEND_BOTTOM_LEFT, OVERLAYS[1]]) {
    const out = checkExport(answer({ bbox: next, overlays: [OVERLAYS[0], legend, OVERLAYS[2], OVERLAYS[3]] }), { ...EXPECT, bbox: next });
    assert.equal(out.ok, true, JSON.stringify({ legend, reason: out.reason, detail: out.detail }));
    assert.deepEqual(out.picture_labels.map(l => l.site), ['Tena', 'Loreto']);
  }
  // Not wider than needed: it zooms out less than the old 15 % padding did, or the same.
  const [w, s, e, n] = BBOX;
  const padded = fitBoundsView([w - (e - w) * 0.15, s - (n - s) * 0.15, e + (e - w) * 0.15, n + (n - s) * 0.15], EXPORT.frame);
  assert.ok(fitBoundsView(next, EXPORT.frame).zoom >= padded.zoom - 1e-9);
  // A site with nothing to move: no frame. A site that cannot clear (under the title whatever the frame): none either.
  assert.equal(clearingFrame({ bbox: BBOX, frame: EXPORT.frame, overlays: OVERLAYS, footprints: [] }), null);
  const wall = [{ kind: 'title', x: 0, y: 0, width: 760, height: 570 }];
  assert.equal(clearingFrame({ bbox: BBOX, frame: EXPORT.frame, overlays: wall, footprints: [{ lng: -77.5, lat: -0.8, boxes: [{ dx: -10, dy: -10, width: 20, height: 20 }] }] }), null);
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
