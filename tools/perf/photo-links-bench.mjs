#!/usr/bin/env node
// Times sync_photo_links on a COPY of the team's workbook (never the real one): the app's own code
// (server/photo-links-tool.mjs → server/batch.mjs) reading the real photo folders on Drive and writing
// the copy's Photo_links, then undoing what it wrote.
//
//   node tools/perf/photo-links-bench.mjs --id <copy id> [--only "Guyana_2025"] [--keep] [--read]
//
// --read: only the reading of Drive and its comparison with the copy's sheet; nothing is written.
//
// Credentials (on claudeclaw): --creds for the copy (default ~/.config/ithomiini/google.jmithominii.json),
// --drive-creds for Drive (default ~/.config/ithomiini/google.json, the app's login).
import os from 'node:os';
import fs from 'node:fs';
import path from 'node:path';
import { Store } from '../../server/store.mjs';
import { GoogleSheets } from '../../server/sheets.mjs';
import { GoogleDrive } from '../../server/drive.mjs';
import { createPhotoLinksTool } from '../../server/photo-links-tool.mjs';
import { REAL_ID } from '../../server/workbook.mjs';

const args = {};
const argv = process.argv.slice(2);
for (let i = 0; i < argv.length; i++) if (argv[i].startsWith('--')) args[argv[i].slice(2)] = argv[i + 1]?.startsWith('--') || argv[i + 1] === undefined ? true : argv[++i];
if (!args.id) throw new Error('--id <copy spreadsheet id> is required');
if (args.id === REAL_ID) throw new Error("Refusing: that is the team's real workbook. Use the PERF TEST copy.");

const home = os.homedir();
const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'photo-links-bench-'));
const sheets = new GoogleSheets({ spreadsheetId: args.id, googleCredentialsFile: args.creds || `${home}/.config/ithomiini/google.jmithominii.json` });
const driveLogin = new GoogleSheets({ spreadsheetId: args.id, googleCredentialsFile: args['drive-creds'] || `${home}/.config/ithomiini/google.json`, readOnly: true });
const store = new Store({ databasePath: path.join(dir, 'bench.sqlite'), spreadsheetId: args.id }, { sheets });
const drive = new GoogleDrive({ token: () => driveLogin.accessToken() });
const user = { id: 'bench', username: 'bench', role: 'editor' };
const seconds = ms => `${(ms / 1000).toFixed(1)} s`;
const timed = async (label, work) => {
  const t = Date.now();
  const out = await work();
  console.log(`${label}: ${seconds(Date.now() - t)}`);
  return out;
};

try {
  await timed('The copy read into the app (Photo_links and the three sheets with CAM IDs)', () =>
    store.sync({ sheets: ['Photo_links', 'Collection_data', 'Insectary_data', 'CRISPR'] }),
  );
  const tool = createPhotoLinksTool({ store, config: { drive }, waitMs: 30 * 60_000 });
  const only = args.only ? String(args.only).split(',') : [];
  const read = await timed(`Drive read${only.length ? ` (${only.join(', ')})` : ' (every listed folder)'} and compared`, () => tool.run({ only }, user));
  if (read.error) throw new Error(read.error);
  console.log(`  ${read.drive.files} files in ${read.drive.folders} folders; new files: ${read.changes?.newFiles ?? 0}; requests to Drive: ${drive.requests}`);
  if (read.upToDate || args.read) process.exit(0);
  const before = Date.now();
  const done = await timed('Written to Photo_links and confirmed', () => tool.run({ confirmed: true, scan: read.scan }, user));
  console.log(`  ${JSON.stringify({ status: done.status, rowsWritten: done.rowsWritten, saves: done.saves, error: done.error })}`);
  const actions = store.db.prepare("SELECT id, created_at, result_json FROM actions WHERE status='verified' ORDER BY created_at").all();
  console.log(`  whole run after the reading: ${seconds(Date.now() - before)}; saves: ${actions.length}`);
  if (!args.keep && actions.length) {
    await timed('Undone (the copy as it was, but for the empty rows added at the end)', async () => {
      for (const a of actions.reverse()) await store.undo({ actionIds: [a.id], requestId: `bench-undo-${a.id}` }, user);
    });
  }
} finally {
  store.close();
  fs.rmSync(dir, { recursive: true, force: true });
}
