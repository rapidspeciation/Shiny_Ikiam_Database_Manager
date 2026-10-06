#!/usr/bin/env node
// How long a whole-workbook sync keeps other requests waiting (6 Oct 2026: right
// after a restart, "Sync: 25 sheets read in 162.1 s … 0 changed" with the event
// loop blocked up to 5.9 s). The lab's snapshot of the team's workbook
// ($LAB/snapshot.json, tools/lab/snapshot.sh) is served as Google answers it (each
// range as JSON text, parsed by the app's own GoogleSheets.readSheet) after a
// short wait instead of the network's. A Store on a database file: a first sync
// fills it (not timed), then, as after a restart (a new Store on the same file),
// the sync timed with
// - the longest stretch the event loop got no turn and its delay as /health reports it,
// - a "person": a small request every 100 ms (a row read and a save, as «Marcar
//   revisado» in Clutches) timed from when it was due to when it ended,
// - then the sheets' copy for `query` (server/replica.mjs) rebuilt, and the checks
//   (Revisión) found again, as after a sync in the server.
// Also a periodic sync (nothing changed) and one after a row inserted at the top of
// Insectary_data (every row below it moved) and some cells edited in the sheet.
//   node tools/lab/sync-load.mjs [--latency 30] [--only restart]
//   node tools/lab/sync-load.mjs --profile /tmp/prof   a CPU profile of each timed step there
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { Session } from 'node:inspector/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { performance } from 'node:perf_hooks';
import { Store } from '../../server/store.mjs';
import { GoogleSheets } from '../../server/sheets.mjs';
import { REAL_ID } from '../../server/workbook.mjs';
import { moduleMap } from '../../server/schema.mjs';
import { watchEventLoop } from '../../server/event-loop.mjs';
import { createSheetsCopy } from '../../server/replica.mjs';
import { allIssues } from '../../server/checks.mjs';
import { loadSnapshot } from './lib.mjs';

const argv = process.argv.slice(2);
const option = (name, fallback) => {
  const i = argv.indexOf(name);
  return i < 0 ? fallback : argv[i + 1];
};
const LATENCY = Number(option('--latency', 30));
const ONLY = option('--only', null);
const PROFILE = option('--profile', null);

const sleep = ms => new Promise(resolve => setTimeout(resolve, ms));
const ms = n => `${n.toFixed(0)} ms`;

const t0 = performance.now();
const snapshot = loadSnapshot();
console.log(`snapshot: ${Object.values(snapshot).reduce((n, rows) => n + rows.length, 0)} rows in ${ms(performance.now() - t0)}`);

/** The workbook as Google serves it: GoogleSheets with its requests answered from the snapshot. */
function fakeGoogle(workbook) {
  const google = Object.create(GoogleSheets.prototype);
  Object.assign(google, { spreadsheetId: REAL_ID, requestCount: 0, gridRows: new Map(), readTimes: [] });
  google.revision = async () => null;
  google.loadMetadata = async () =>
    new Map(
      Object.entries(workbook).map(([title, rows]) => [
        title,
        { properties: { sheetId: moduleMap.get(title)?.sheetId ?? 0, title, gridProperties: { rowCount: rows.at(-1)?.row ?? 0 } } },
      ]),
    );
  google.request = async path => {
    google.requestCount++;
    const range = new URLSearchParams(path.slice(1)).get('ranges');
    const [, title, start, end] = /^'(.*)'!(\d+):(\d+)$/.exec(range);
    await sleep(LATENCY);
    const rows = workbook[title.replaceAll("''", "'")] || [];
    const rowData = [];
    for (const r of rows) if (r.row >= +start && r.row <= +end) rowData[r.row - +start] = { values: r.cells };
    // Google sends every row of the range up to its last one with data.
    for (let i = 0; i < rowData.length; i++) rowData[i] ??= {};
    return JSON.stringify({ sheets: [{ data: [{ startRow: +start - 1, rowData }] }] });
  };
  return google;
}

const home = mkdtempSync(join(tmpdir(), 'ithomiini-sync-'));
const databasePath = join(home, 'app.sqlite');
const open = () => new Store({ databasePath }, { sheets: fakeGoogle(snapshot) });

let store = open();
let s = performance.now();
await store.sync();
console.log(`first sync (fills the copy): ${ms(performance.now() - s)}`);
store.close();

// The longest stretch the event loop got no turn (a 1 ms timer that keeps asking).
let longest = 0;
let last = performance.now();
// With --profile: when the stretches over 150 ms happened (ms from the step's start), to find them in its profile.
let gaps = [];
let stepStart = 0;
const beat = setInterval(() => {
  const now = performance.now();
  longest = Math.max(longest, now - last);
  if (PROFILE && now - last > 150) gaps.push(`${(last - stepStart).toFixed(0)}+${(now - last).toFixed(0)}`);
  last = now;
}, 1);

/** A person's small request every 100 ms: [ms from due to done]. */
function person() {
  const waits = [];
  let stop = false;
  const ids = store.db.prepare("SELECT id FROM records WHERE sheet='Insectary_stocks' AND missing=0 LIMIT 50").all().map(r => r.id);
  store.db.exec('CREATE TABLE IF NOT EXISTS probe(at TEXT)');
  const save = store.db.prepare('INSERT INTO probe(at) VALUES(?)');
  (async () => {
    let due = performance.now() + 100;
    while (!stop) {
      await sleep(Math.max(0, due - performance.now()));
      if (stop) break;
      store.getRecord(ids[waits.length % ids.length]);
      save.run(new Date().toISOString());
      waits.push(performance.now() - due);
      due += 100;
      if (due < performance.now()) due = performance.now() + 100;
    }
  })();
  return () => ((stop = true), waits);
}
const quantile = (list, q) => {
  const sorted = [...list].sort((a, b) => a - b);
  return sorted.length ? sorted[Math.min(sorted.length - 1, Math.floor(sorted.length * q))] : 0;
};

let session = null;
if (PROFILE) {
  mkdirSync(PROFILE, { recursive: true });
  session = new Session();
  session.connect();
  await session.post('Profiler.enable');
  await session.post('Profiler.setSamplingInterval', { interval: 500 });
}
let step = 0;
async function measure(what, fn) {
  await sleep(50);
  if (session) await session.post('Profiler.start');
  const lag = watchEventLoop({ sampleMs: 10, windowMs: 3_600_000 });
  const stop = person();
  longest = 0;
  gaps = [];
  last = stepStart = performance.now();
  const start = performance.now();
  const out = await fn();
  const took = performance.now() - start;
  await sleep(150);
  if (session) {
    const { profile } = await session.post('Profiler.stop');
    const file = join(PROFILE, `${++step}-${what.replace(/[^a-z]+/gi, '-')}.cpuprofile`);
    writeFileSync(file, JSON.stringify(profile));
    if (gaps.length) console.log(`  stretches over 150 ms (at+ms): ${gaps.join(' ')}`);
  }
  const waits = stop();
  const { p99Ms } = lag.stats();
  lag.stop();
  console.log(
    `${what}: ${ms(took)}; longest block ${ms(longest)}, p99 lag ${ms(p99Ms)}; ` +
      `person's request (${waits.length}): median ${ms(quantile(waits, 0.5))}, p99 ${ms(quantile(waits, 0.99))}, max ${ms(Math.max(0, ...waits))}`,
  );
  return out;
}

const syncOnly = status =>
  `${status.state}, ${status.added} added, ${status.changed} changed, ${status.moved} moved, ${status.missing} missing, ${status.sheetsUnchanged} unchanged sheets; ${status.requests} requests`;

// As after a restart: a new Store on the same database (it has not seen the sheets' digests).
store = open();
const copy = createSheetsCopy({ store, path: join(home, 'sheets.sqlite'), delayMs: 3_600_000, startMs: 3_600_000, log: { log() {}, error: console.error } });
let status = await measure('sync after a restart', () => store.sync());
console.log(`  ${syncOnly(status)}; Google waits ${ms(status.requests * LATENCY)} of it`);
await measure('the sheets copy rebuilt', () => copy.rebuild());
await measure('the checks found again', async () => allIssues(store));

if (ONLY !== 'restart') {
  status = await measure('periodic sync (nothing changed)', () => store.sync());
  console.log(`  ${syncOnly(status)}`);
  // A row inserted at the top of Insectary_data in the sheet, and cells edited in a few others.
  const insectary = snapshot.Insectary_data;
  const header = insectary[0];
  const notes = header.cells.findIndex(c => c?.userEnteredValue?.stringValue === 'Notes_Insectary_data');
  const moved = insectary.slice(1).map(r => ({ ...r, row: r.row + 1 }));
  for (const r of moved.filter((_, i) => i % 100 === 0)) {
    r.cells = [...r.cells];
    r.cells[notes] = { userEnteredValue: { stringValue: `edited ${r.row}` }, effectiveValue: { stringValue: `edited ${r.row}` } };
  }
  snapshot.Insectary_data = [header, { row: 2, cells: [] }, ...moved];
  store.sheets = fakeGoogle(snapshot);
  status = await measure('sync after a row inserted at the top of Insectary_data', () => store.sync());
  console.log(`  ${syncOnly(status)}, ${status.cells} cells`);
  await measure('the sheets copy rebuilt', () => copy.rebuild());
}
clearInterval(beat);
copy.close();
store.close();
rmSync(home, { recursive: true, force: true });
