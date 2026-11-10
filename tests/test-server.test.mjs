// scripts/test-server.mjs: which test files may share a process, and the run's plan (every file once).
import test from 'node:test';
import assert from 'node:assert/strict';
import { readdirSync } from 'node:fs';
import { aloneReasons, plan } from '../scripts/test-server.mjs';

const files = readdirSync(new URL('.', import.meta.url)).filter(f => f.endsWith('.test.mjs')).sort().map(f => `tests/${f}`);
const src = lines => lines.join('\n');

test('a file that changes what other files in its process would see runs alone', () => {
  const alone = [
    "process.env.TZ = 'UTC';",
    "process.on('exit', () => {});",
    "test('x', t => { t.mock.timers.enable(); });",
    "afterEach(() => {});",
    "test.after(() => {});",
    "describe('x', () => { before(() => {}); });",
    'globalThis.fetch = async () => null;',
    'Date.now = () => 0;',
    'setPasswordHashCost(16);',
    'String.prototype.shout = function () {};',
    "import { MODELS } from '../server/offline-id.mjs';\nMODELS[0].sha256 = 'x';",
    "import { CACHE } from '../server/x.mjs';\nCACHE.set('k', 1);",
    "import * as cfg from '../server/config.mjs';\ndelete cfg.thing;",
  ];
  for (const s of alone) assert.notDeepEqual(aloneReasons('tests/x.test.mjs', s), [], s);
  assert.match(aloneReasons('tests/offline-id.test.mjs', '').join(), /MODELS/);
  // A helper's root hook (tests/helpers/maps-render.mjs: test.after() checks no render process survived).
  assert.match(aloneReasons('tests/x.test.mjs', "import { renderTest } from './helpers/maps-render.mjs';").join(), /helpers\/maps-render\.mjs: a file's hooks/);
  const shareable = src([
    "import test from 'node:test';",
    "import assert from 'node:assert/strict';",
    "import { Store } from '../server/store.mjs';",
    "test('x', async t => { const store = new Store({}); t.after(() => store.close()); store.db.prepare('SELECT 1').get(); const out = {}; out.a = 1; assert.equal(out.a, 1); });",
    "test('y', async () => { const { thing } = await import('../server/thing.mjs'); assert.ok(thing); });",
  ]);
  assert.deepEqual(aloneReasons('tests/x.test.mjs', shareable), []);
});

test('the plan runs every test file exactly once, the longest first; --isolated: each alone', () => {
  const durations = Object.fromEntries(files.map((f, i) => [f, { tests_ms: (i * 37) % 900, wall_ms: ((i * 37) % 900) + 300 }]));
  durations['tests/maps-render-worker.test.mjs'] = { tests_ms: 20000, wall_ms: 21000 };
  for (const options of [{ durations, concurrency: 4 }, { durations, concurrency: 12 }, { concurrency: 4 }, { durations, concurrency: 4, seed: 7 }]) {
    const units = plan(files, options);
    assert.deepEqual(units.flatMap(u => u.files).sort(), files);
    assert.ok(units.some(u => u.files.length > 1), 'some files share a process');
    for (const u of units) if (u.files.length > 1) for (const f of u.files) assert.deepEqual(aloneReasons(f), [], f);
    if (options.seed === undefined) assert.deepEqual(units.map(u => u.ms), [...units.map(u => u.ms)].sort((a, b) => b - a));
  }
  assert.equal(plan(files, { durations, concurrency: 4 })[0].files[0], 'tests/maps-render-worker.test.mjs');
  const isolated = plan(files, { durations, isolated: true });
  assert.equal(isolated.length, files.length);
  // The guard's own self-check always runs alone (it reads the environment the guard set).
  assert.ok(plan(files, { durations }).some(u => u.files.length === 1 && u.files[0] === 'tests/no-ai-guard.test.mjs'));
});
