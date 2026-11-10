// maps_snapshot's app picture: the worker process (server/maps-render-worker.mjs), with a stand-in Playwright
// module (tests/fixtures/fake-playwright.mjs), so the real worker runs, but no browser. The checks of what the app
// returns: maps-render-checks.test.mjs; maps_snapshot choosing the app or Pillow: maps-render.test.mjs.
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

// ---------- the worker process ----------
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
