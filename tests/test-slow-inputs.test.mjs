// scripts/test-slow.mjs --if-changed skips the slow suite (the real reads of the rack photo) only when every input is
// byte for byte as at its last green run: a stamp written by another checkout or for another commit lets it skip only
// when the reader, its library, the photos, the saved reading, the tests and the tools are all the same. The hash is
// computed on small stand-in checkouts in a temp folder; the slow suite itself never runs here.
import test from 'node:test';
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { cpSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { INPUTS, inputsHash, lastGreen } from '../scripts/test-slow.mjs';

const script = fileURLToPath(new URL('../scripts/test-slow.mjs', import.meta.url));
const VERSIONS = ['node v24.0.0 linux-x64', 'ffmpeg version 6.1.1\nlibavcodec 60.31.102'];

/** A checkout holding one small file for every input (a folder input gets two files). */
function checkout(parent, name) {
  const root = join(parent, name);
  for (const p of INPUTS) {
    const files = /\.\w+$/.test(p) ? [p] : [`${p}/a.js`, `${p}/b/c.wasm`];
    for (const f of files) {
      mkdirSync(dirname(join(root, f)), { recursive: true });
      writeFileSync(join(root, f), `${f}\n`);
    }
  }
  return root;
}

test('slow suite inputs: the same bytes in another checkout hash the same; any changed input, tool or missing file does not', () => {
  const dir = mkdtempSync(join(tmpdir(), 'slow-inputs-'));
  try {
    const a = checkout(dir, 'a');
    const b = checkout(dir, 'other-checkout');
    const base = inputsHash(a, VERSIONS).hash;
    assert.match(base, /^[0-9a-f]{64}$/);
    assert.equal(inputsHash(b, VERSIONS).hash, base, 'where the checkout is does not count');
    const changes = {
      'the reader': () => writeFileSync(join(b, 'server/rack-reader.mjs'), 'server/rack-reader.mjs\n// changed\n'),
      'one byte of the barcode library': () => writeFileSync(join(b, 'server/vendor/zxing-wasm/b/c.wasm'), 'server/vendor/zxing-wasm/b/c.wasM\n'),
      'a file added to the library': () => writeFileSync(join(b, 'server/vendor/zxing-wasm/new.js'), 'x\n'),
      'the photo': () => writeFileSync(join(b, 'tests/fixtures/rack-bottom-fk00274230.jpg'), 'another photo\n'),
      'the saved reading': () => writeFileSync(join(b, 'tests/fixtures/rack-reading-fk00274230.json'), '{}\n'),
      'a slow test': () => writeFileSync(join(b, 'tests/slow/b/c.wasm'), 'changed\n'),
      'a missing input': () => rmSync(join(b, 'server/rack-tools.mjs')),
    };
    for (const [what, change] of Object.entries(changes)) {
      rmSync(b, { recursive: true });
      cpSync(a, b, { recursive: true });
      assert.equal(inputsHash(b, VERSIONS).hash, base);
      change();
      assert.notEqual(inputsHash(b, VERSIONS).hash, base, what);
    }
    assert.notEqual(inputsHash(a, ['node v24.1.0 linux-x64', VERSIONS[1]]).hash, base, 'another node');
    assert.notEqual(inputsHash(a, ['node v24.0.0 linux-arm64', VERSIONS[1]]).hash, base, 'another platform');
    assert.notEqual(inputsHash(a, [VERSIONS[0], 'ffmpeg version 6.1.1\nlibavcodec 60.31.103']).hash, base, 'another ffmpeg build');
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test('slow suite stamp: a last green run counts only for exactly its inputs (another checkout or commit with a changed reader runs it)', () => {
  const dir = mkdtempSync(join(tmpdir(), 'slow-stamp-'));
  try {
    const a = checkout(dir, 'a');
    const stamp = join(dir, 'slow-suite.json');
    const green = inputsHash(a, VERSIONS).hash;
    writeFileSync(stamp, JSON.stringify({ hash: green, at: '2026-11-09T00:00:00.000Z' }));
    assert.equal(lastGreen(stamp, green)?.at, '2026-11-09T00:00:00.000Z');
    const b = checkout(dir, 'b');
    writeFileSync(join(b, 'server/rack-reader.mjs'), 'a reader that reads less\n');
    assert.equal(lastGreen(stamp, inputsHash(b, VERSIONS).hash), null);
    // No stamp, an unreadable one, or one without a hash: never green here.
    assert.equal(lastGreen(join(dir, 'none.json'), green), null);
    writeFileSync(stamp, 'not json');
    assert.equal(lastGreen(stamp, green), null);
    writeFileSync(stamp, JSON.stringify({ at: 'x' }));
    assert.equal(lastGreen(stamp, undefined), null);
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test('test-slow.mjs --if-changed with a stamp of these exact inputs: not run, and its result for the record says "unchanged"', () => {
  const dir = mkdtempSync(join(tmpdir(), 'slow-cli-'));
  try {
    const inputs = JSON.parse(spawnSync(process.execPath, [script, '--inputs'], { encoding: 'utf8' }).stdout);
    const stamp = join(dir, 'slow-suite.json');
    const result = join(dir, 'result.json');
    writeFileSync(stamp, JSON.stringify({ hash: inputs.hash, at: '2026-11-09T00:00:00.000Z' }));
    const r = spawnSync(process.execPath, [script, '--if-changed', '--stamp', stamp, '--result', result], { encoding: 'utf8', timeout: 20_000 });
    assert.equal(r.status, 0, r.stderr);
    assert.match(r.stdout, /Slow tests not needed/);
    assert.doesNotMatch(r.stdout, /ℹ tests/);
    assert.deepEqual(JSON.parse(readFileSync(result, 'utf8')), { slow: 'unchanged', inputs: inputs.hash, lastGreenAt: '2026-11-09T00:00:00.000Z' });
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});
