import test from 'node:test';
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { mkdtempSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { ATTRIBUTION, TILE_URL, USER_AGENT, createSnapshots } from '../server/map-snapshot.mjs';

const pillow = spawnSync('python3', ['-c', 'import PIL'], { stdio: 'ignore' }).status === 0;
const png = Buffer.from('iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAAAAAA6fptVAAAACklEQVQI12P4DwABAQEAWk1v8QAAAABJRU5ErkJggg==', 'base64');

test('the basemap is OpenTopoMap, credited as its licence asks', () => {
  assert.equal(TILE_URL, 'https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png');
  assert.equal(ATTRIBUTION, '© OpenStreetMap contributors, SRTM | © OpenTopoMap (CC-BY-SA)');
  assert.match(USER_AGENT, /^ithomiini-database\/\S+ \(.*\+https:\/\/github\.com\//);
});

test('tiles: the app\'s User-Agent, a/b/c subdomains, one at a time, kept a week then fetched again', { skip: !pillow && 'no python3 with Pillow' }, async () => {
  // From now: the tiles' age is the clock minus their files' real times (a fixed date stops working a week after it).
  let clock = Date.now();
  const asked = [];
  let inFlight = 0;
  let most = 0;
  const fetchImpl = async (url, o) => {
    asked.push({ url, ua: o.headers['user-agent'] });
    most = Math.max(most, ++inFlight);
    await new Promise(r => setTimeout(r, 5));
    inFlight--;
    return new Response(png);
  };
  const dir = mkdtempSync(join(tmpdir(), 'map-snapshot-'));
  const snaps = createSnapshots({ dir, fetchImpl, now: () => clock });
  const bbox = [-78, -1.2, -77.3, -0.6];
  const first = await snaps.render({ bbox });
  assert.ok(first.tiles.asked > 4);
  assert.equal(first.tiles.fetched, first.tiles.asked);
  assert.equal(most, 1, `one at a time, saw ${most}`);
  assert.ok(asked.every(a => a.ua === USER_AGENT));
  assert.ok(asked.every(a => /^https:\/\/[abc]\.tile\.opentopomap\.org\/11\/\d+\/\d+\.png$/.test(a.url)));
  assert.equal(new Set(asked.map(a => a.url[8])).size, 3);
  // Within the week: from the disk.
  const again = await snaps.render({ bbox });
  assert.equal(again.tiles.fetched, 0);
  // A week and more later: the old tiles deleted by the hourly clean, fetched again.
  clock += 8 * 86400_000;
  const later = await snaps.render({ bbox });
  assert.equal(later.tiles.fetched, later.tiles.asked);
});

test('tiles: a stalled tile fetch respects deadlineMs, throws DEADLINE, and releases the fetch slot', { skip: !pillow && 'no python3 with Pillow' }, async () => {
  let stall = true;
  const fetchImpl = (_url, { signal } = {}) => {
    if (!stall) return Promise.resolve(new Response(png));
    return new Promise((_resolve, reject) => {
      signal?.addEventListener('abort', () => reject(signal.reason ?? new Error('aborted')), { once: true });
    });
  };
  const dir = mkdtempSync(join(tmpdir(), 'map-snapshot-deadline-'));
  const snaps = createSnapshots({ dir, fetchImpl });
  const bbox = [-78, -1.2, -77.3, -0.6];
  const t0 = Date.now();
  await assert.rejects(() => snaps.render({ bbox, deadlineMs: 150 }), err => err?.code === 'DEADLINE');
  assert.ok(Date.now() - t0 < 2000, 'aborted promptly at deadlineMs');
  // The slot was released: a subsequent render succeeds cleanly.
  stall = false;
  const ok = await snaps.render({ bbox, deadlineMs: 5000 });
  assert.ok(ok.bytes > 0 && ok.tiles.fetched > 0);
});

test('tiles: concurrent caller with longer deadline is not aborted by a short-deadline caller sharing loading', { skip: !pillow && 'no python3 with Pillow' }, async () => {
  const fetchImpl = (_url, { signal } = {}) =>
    new Promise((resolve, reject) => {
      const t = setTimeout(() => resolve(new Response(png)), 25);
      signal?.addEventListener('abort', () => (clearTimeout(t), reject(signal.reason ?? new Error('aborted'))), { once: true });
    });
  const dir = mkdtempSync(join(tmpdir(), 'map-snapshot-shared-'));
  const snaps = createSnapshots({ dir, fetchImpl });
  const bbox = [-78, -1.2, -77.3, -0.6];
  const [shortRun, longRun] = await Promise.allSettled([
    snaps.render({ bbox, deadlineMs: 40 }),
    snaps.render({ bbox, deadlineMs: 5000 }),
  ]);
  assert.equal(shortRun.status, 'rejected');
  assert.equal(shortRun.reason?.code, 'DEADLINE');
  assert.equal(longRun.status, 'fulfilled', String(longRun.reason));
  assert.ok(longRun.value.bytes > 0);
});

test('pillow: tight deadline with >64 KB stdin payload rejects cleanly with DEADLINE without unhandled EPIPE', { skip: !pillow && 'no python3 with Pillow' }, async () => {
  const fetchImpl = async () => new Response(png);
  const dir = mkdtempSync(join(tmpdir(), 'map-snapshot-epipe-'));
  const snaps = createSnapshots({ dir, fetchImpl });
  const bbox = [-78, -1.2, -77.3, -0.6];
  // Warm tiles first so all remaining budget goes into the Python Pillow invocation.
  await snaps.render({ bbox, deadlineMs: 5000 });
  const points = Array.from({ length: 20_000 }, (_, i) => ({ lat: -0.9 - (i % 100) * 0.001, lng: -77.7 + (i % 100) * 0.001, color: i % 5 }));
  await assert.rejects(() => snaps.render({ bbox, points, deadlineMs: 15 }), err => err?.code === 'DEADLINE');
});
