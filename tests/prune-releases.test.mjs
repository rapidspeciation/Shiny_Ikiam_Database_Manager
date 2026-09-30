import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, mkdirSync, readdirSync, rmSync, symlinkSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { execFileSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

const script = fileURLToPath(new URL('../scripts/prune-releases.sh', import.meta.url));

test('old releases are deleted; the newest, the current and the previous one stay', () => {
  const app = mkdtempSync(join(tmpdir(), 'ithomiini-releases-'));
  try {
    const names = Array.from({ length: 9 }, (_, i) => `2026092${i}T120000Z`);
    for (const name of names) mkdirSync(join(app, 'releases', name), { recursive: true });
    mkdirSync(join(app, 'releases', 'notes'));
    mkdirSync(join(app, 'shared'));
    // Rolled back to an old release; the one before it is older still.
    symlinkSync(`releases/${names[2]}`, join(app, 'current'));
    writeFileSync(join(app, 'shared', 'previous-release'), `releases/${names[1]}\n`);
    execFileSync('bash', [script, app, '3']);
    assert.deepEqual(readdirSync(join(app, 'releases')).sort(), [names[1], names[2], names[6], names[7], names[8], 'notes']);
    // Nothing more to delete the second time.
    execFileSync('bash', [script, app, '3']);
    assert.equal(readdirSync(join(app, 'releases')).length, 6);
    assert.throws(() => execFileSync('bash', [script, app, '1'], { stdio: 'pipe' }));
  } finally {
    rmSync(app, { recursive: true, force: true });
  }
});
