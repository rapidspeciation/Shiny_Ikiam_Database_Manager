#!/usr/bin/env node
// How long people wait while the Revisión checks scan the whole local copy (server/checks.mjs,
// about 1.6 s on the team's 107k rows), in the app's thread (--mode inline, as before) and in the
// checks' worker (--mode worker, server/checks-host.mjs). A copy of the lab app's database
// ($LAB/app/app.sqlite, or --db) opened by a Store as the app opens it, and timed with
// - the longest stretch the event loop got no turn and its p99 delay as /health reports it,
// - a "person": a row read and a save of a cell every 100 ms (each save changes the copy),
// for: a page asking for the issues after a restart (nothing scanned yet), after a data change,
// and pages asking every 250 ms for 5 s while the person saves. Then, with no one saving, the
// worker's issues are compared with a scan in the app's thread of the same copy.
//   node tools/lab/checks-load.mjs                 both modes, one after the other
//   node tools/lab/checks-load.mjs --mode worker   one mode
import { spawnSync } from 'node:child_process';
import { deepStrictEqual } from 'node:assert';
import { mkdtempSync, rmSync } from 'node:fs';
import { DatabaseSync } from 'node:sqlite';
import { join } from 'node:path';
import { performance } from 'node:perf_hooks';
import { fileURLToPath } from 'node:url';
import { labPath } from './lib.mjs';

const argv = process.argv.slice(2);
const option = (name, fallback) => {
  const i = argv.indexOf(name);
  return i < 0 ? fallback : argv[i + 1];
};
const MODE = option('--mode', 'both');
const SOURCE = option('--db', labPath('app', 'app.sqlite'));

if (MODE === 'both') {
  // Each mode in its own process, on its own copy: nothing kept from the other.
  const rest = argv.filter((a, i) => a !== '--mode' && argv[i - 1] !== '--mode');
  for (const mode of ['inline', 'worker']) {
    const run = spawnSync(process.execPath, [fileURLToPath(import.meta.url), '--mode', mode, ...rest], { stdio: 'inherit' });
    if (run.status) process.exit(run.status);
  }
  process.exit(0);
}

const { Store } = await import('../../server/store.mjs');
const { allIssues, freshIssues } = await import('../../server/checks.mjs');
const { createChecksHost } = await import('../../server/checks-host.mjs');
const { watchEventLoop } = await import('../../server/event-loop.mjs');

const sleep = ms => new Promise(resolve => setTimeout(resolve, ms));
const ms = n => `${n.toFixed(0)} ms`;
const quantile = (list, q) => {
  const sorted = [...list].sort((a, b) => a - b);
  return sorted.length ? sorted[Math.min(sorted.length - 1, Math.floor(sorted.length * q))] : 0;
};

// In the lab folder (on disk): the copy is some 400 MB, more than /tmp may hold.
const home = mkdtempSync(labPath('checks-load-'));
const databasePath = join(home, 'app.sqlite');
let s = performance.now();
const source = new DatabaseSync(SOURCE, { readOnly: true });
source.exec(`VACUUM INTO '${databasePath.replaceAll("'", "''")}'`);
source.close();
console.log(`[${MODE}] copy of ${SOURCE}: ${ms(performance.now() - s)}`);

const store = new Store({ databasePath, localMode: true }, {});
const host = MODE === 'worker' ? createChecksHost({ store, config: {} }) : null;
if (MODE === 'worker' && host.mode !== 'worker') throw new Error(`No worker: ${host.status().why}`);

// The longest stretch the event loop got no turn (a 1 ms timer that keeps asking).
let longest = 0;
let last = performance.now();
const beat = setInterval(() => {
  const now = performance.now();
  longest = Math.max(longest, now - last);
  last = now;
}, 1);

const rows = store.db
  .prepare("SELECT id FROM records WHERE sheet='Insectary_stocks' AND missing=0 AND observed=1 LIMIT 50")
  .all()
  .map(r => r.id);
let saves = 0;
/** A save of one cell, as the app writes it: the copy changes (its stamp too). */
function save() {
  const id = rows[saves++ % rows.length];
  const record = store.getRecord(id);
  const values = { ...record.values, Notes: `checks-load ${saves}` };
  store.db
    .prepare('UPDATE records SET values_json=?, version=version+1, updated_at=? WHERE id=?')
    .run(JSON.stringify(values), new Date().toISOString(), id);
}

/** The person: a row read and a save every 100 ms; [ms from due to done]. */
function person() {
  const waits = [];
  let stop = false;
  (async () => {
    let due = performance.now() + 100;
    while (!stop) {
      await sleep(Math.max(0, due - performance.now()));
      if (stop) break;
      store.getRecord(rows[waits.length % rows.length]);
      save();
      waits.push(performance.now() - due);
      due += 100;
      if (due < performance.now()) due = performance.now() + 100;
    }
  })();
  return () => ((stop = true), waits);
}

async function measure(what, fn) {
  await sleep(100);
  const lag = watchEventLoop({ sampleMs: 10, windowMs: 3_600_000 });
  const stop = person();
  longest = 0;
  last = performance.now();
  const start = performance.now();
  const out = await fn();
  const took = performance.now() - start;
  await sleep(150);
  const waits = stop();
  const { p99Ms } = lag.stats();
  lag.stop();
  console.log(
    `[${MODE}] ${what}: ${ms(took)}; longest block ${ms(longest)}, p99 lag ${ms(p99Ms)}; ` +
      `person (${waits.length}): median ${ms(quantile(waits, 0.5))}, p99 ${ms(quantile(waits, 0.99))}, max ${ms(Math.max(0, ...waits))}`,
  );
  return out;
}

const first = await measure('a page after a restart (nothing scanned yet)', () => freshIssues(store));
console.log(`  ${first.issues.length} issues`);
save();
await measure('a page after a data change', () => freshIssues(store));
await measure('pages every 250 ms for 5 s, the person saving', async () => {
  const asked = [];
  const until = performance.now() + 5000;
  while (performance.now() < until) {
    const at = performance.now();
    asked.push(freshIssues(store).then(() => performance.now() - at));
    await sleep(250);
  }
  const waited = await Promise.all(asked);
  console.log(`  pages (${waited.length}): median ${ms(quantile(waited, 0.5))}, max ${ms(Math.max(...waited))}`);
});

if (MODE === 'worker') {
  // The same copy, nobody saving: the worker's issues and a scan here, side by side.
  await sleep(200);
  const fromWorker = await freshIssues(store);
  const here = new Store({ databasePath, localMode: true }, {});
  const inline = allIssues(here);
  deepStrictEqual(fromWorker.stamp, inline.stamp);
  deepStrictEqual(fromWorker.issues, inline.issues);
  console.log(`[${MODE}] the worker's ${fromWorker.issues.length} issues are the same as a scan in the app's thread`);
  console.log(`[${MODE}] /health checks:`, JSON.stringify(host.status()));
  here.close();
}
clearInterval(beat);
host?.close();
store.close();
rmSync(home, { recursive: true, force: true });
