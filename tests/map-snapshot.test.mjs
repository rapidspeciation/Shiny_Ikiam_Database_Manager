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

test('tiles: the app\'s User-Agent, a/b/c subdomains, two at a time, kept a week then fetched again', { skip: !pillow && 'no python3 with Pillow' }, async () => {
  let clock = Date.parse('2026-11-01T12:00:00Z');
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
  assert.ok(most <= 2, `at most two at once, saw ${most}`);
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
