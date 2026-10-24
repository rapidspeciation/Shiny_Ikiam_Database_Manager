import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, writeFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { configFromEnv } from '../server/index.mjs';
import { driveFor, driveWriterFor, LocalDrive } from '../server/drive.mjs';
import { SANDBOX_ID } from '../server/workbook.mjs';

test('the recorded Drive sandbox requires an explicit lab and a non-production database', () => {
  const enabled = { ITHOMIINI_LAB_DRIVE_SANDBOX: '1', ITHOMIINI_LAB_URL: 'http://127.0.0.1:8795' };
  assert.equal(configFromEnv(enabled).driveSandbox, false);
  assert.equal(configFromEnv({ ...enabled, WORKBOOK_ID: SANDBOX_ID }).driveSandbox, true);
  assert.equal(configFromEnv({ WORKBOOK_ID: SANDBOX_ID, ITHOMIINI_LAB_DRIVE_SANDBOX: '1' }).driveSandbox, false);
});

test('the lab reader and writer share only an in-memory recorded Drive', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'lab-drive-'));
  try {
    const file = join(dir, 'listing.json');
    writeFileSync(file, JSON.stringify({ files: [] }));
    const store = { localMode: false };
    const config = { driveSandbox: true, driveListingFile: file };
    const reader = driveFor(store, config);
    const writer = driveWriterFor(store, config);
    assert.ok(reader instanceof LocalDrive);
    assert.equal(reader, writer);
    assert.equal(driveWriterFor(store, {}), null, 'without the sandbox there is no Drive-writing credential');
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});
