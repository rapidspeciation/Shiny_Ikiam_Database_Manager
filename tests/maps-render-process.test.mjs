// maps_snapshot's app picture: the process tree the renderer kills, the worker's answer as bytes, the memory cap and the
// queue (server/maps-render.mjs, with the stand-in Playwright module tests/fixtures/fake-playwright.mjs). Split from
// maps-render-worker.test.mjs so the three run side by side.
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
