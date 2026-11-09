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
  const r = createMapRenderer({ env: { PLAYWRIGHT_MODULE: fake.module }, memAvailable: () => 8 * 1024 ** 3, deadlineMs: 1500, queueMs: 400, log: () => {} });
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
