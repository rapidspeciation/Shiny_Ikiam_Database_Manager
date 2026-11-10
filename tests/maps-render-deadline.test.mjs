// maps_snapshot's app picture: the render's deadline and who cleans up when the one who started it is gone (the worker
// process, server/maps-render-worker.mjs, with the stand-in Playwright module tests/fixtures/fake-playwright.mjs). Split
// from maps-render-worker.test.mjs so the three run side by side.
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

test('deadline: a hung export is killed with everything started under it, the temp dir removed; others survive', async () => {
  const fake = fakeModule({ hang: 'export', grandchild: true, browserGroup: true });
  const running = createMapRenderer({ env: { PLAYWRIGHT_MODULE: fake.module }, memAvailable: () => 8 * 1024 ** 3, deadlineMs: 2500, log: () => {} }).render(URL_OK);
  // A process that only names the run's temp folder (an `ls` of it, a log tail) is not the render's: it is left alone.
  // Up to 10 s for the worker to start: deploy.sh runs four test jobs at once on claudeclaw's 4 cores (once over 2 s here).
  assert.ok(await waitFor(() => fake.get('tmpdir').length, 10_000));
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
