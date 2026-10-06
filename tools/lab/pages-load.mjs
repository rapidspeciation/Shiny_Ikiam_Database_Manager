#!/usr/bin/env node
// Which requests of the main pages keep the app from answering anyone else, and for how long: the
// whole app on a copy of the lab app's database ($LAB/app/app.sqlite, or --db), over HTTP, each
// request the pages make (Inicio, Tablas, Revisión, Clutches, Emergidos, Muertes, Censo, Monitoreo,
// Cambios propuestos) asked a first time, right after a cell of --sheet (default Insectary_data)
// was saved (what the app kept for that sheet is out of date: "after a change"), and again (what
// it kept). For each: the time it took and, in brackets, the longest stretch the event loop got no
// turn meanwhile. Requests that blocked longer than --over ms (default 200) are listed at the end.
// The answers are read as bytes, not parsed, so the time is the app's.
//   node tools/lab/pages-load.mjs
//   node tools/lab/pages-load.mjs --only table,review --over 100
import { mkdtempSync, rmSync } from 'node:fs';
import { DatabaseSync } from 'node:sqlite';
import { join } from 'node:path';
import { performance } from 'node:perf_hooks';
import { labPath } from './lib.mjs';

const argv = process.argv.slice(2);
const option = (name, fallback) => {
  const i = argv.indexOf(name);
  return i < 0 ? fallback : argv[i + 1];
};
const SOURCE = option('--db', labPath('app', 'app.sqlite'));
// Where the saves go: Insectary_data is what Emergidos and Muertes write all day.
const SHEET = option('--sheet', 'Insectary_data');
const OVER = Number(option('--over', 200));
const ONLY = option('--only', '').split(',').filter(Boolean);

const { createApp } = await import('../../server/index.mjs');
const { LocalSheets } = await import('../../server/sheets.mjs');
const { createUser } = await import('../../server/auth.mjs');
const { moduleMap, modules } = await import('../../server/schema.mjs');

const ms = n => `${n.toFixed(0)} ms`;
const sleep = n => new Promise(resolve => setTimeout(resolve, n));

// In the lab folder (on disk): the copy is some 400 MB, more than /tmp may hold.
const home = mkdtempSync(labPath('pages-load-'));
const databasePath = join(home, 'app.sqlite');
let s = performance.now();
const source = new DatabaseSync(SOURCE, { readOnly: true });
source.exec(`VACUUM INTO '${databasePath.replaceAll("'", "''")}'`);
source.close();
console.log(`copy of ${SOURCE}: ${ms(performance.now() - s)}`);

s = performance.now();
const app = await createApp(
  { databasePath, sheetsCopyPath: null, localMode: true, secureCookies: false, syncIntervalMs: 0 },
  { sheets: new LocalSheets({}), skipInitialSync: true },
);
await app.ready;
const { port } = await app.listen(0, '127.0.0.1');
const base = `http://127.0.0.1:${port}/ithomiini`;
console.log(`app started: ${ms(performance.now() - s)}`);
const store = app.store;
createUser(store, { username: 'pagesload', password: 'pages-load-123', role: 'admin', displayName: 'Pages load' });
const login = await fetch(`${base}/api/auth/login`, {
  method: 'POST',
  headers: { 'content-type': 'application/json', origin: base },
  body: JSON.stringify({ username: 'pagesload', password: 'pages-load-123' }),
});
if (login.status !== 200) throw new Error(`login: ${login.status} ${await login.text()}`);
const cookie = login.headers
  .getSetCookie()
  .map(c => c.split(';')[0])
  .join('; ');

// The longest stretch the event loop got no turn (a 1 ms timer that keeps asking).
let longest = 0;
let last = performance.now();
const beat = setInterval(() => {
  const now = performance.now();
  longest = Math.max(longest, now - last);
  last = now;
}, 1);

const rows = store.db
  .prepare('SELECT id FROM records WHERE sheet=? AND missing=0 AND observed=1 LIMIT 50')
  .all(SHEET)
  .map(r => r.id);
const NOTES = moduleMap.get(SHEET).fields.find(f => /note/i.test(f.key))?.key ?? 'Notes';
let saves = 0;
/** A save of one cell, as the app writes it: the copy changes, and what the app kept with it. */
function save() {
  const id = rows[saves++ % rows.length];
  const record = store.getRecord(id);
  store.db
    .prepare('UPDATE records SET values_json=?, version=version+1, updated_at=? WHERE id=?')
    .run(JSON.stringify({ ...record.values, [NOTES]: `pages-load ${saves}` }), new Date().toISOString(), id);
}

/** One request: { status, bytes, took, block }. */
async function ask(path) {
  await sleep(30);
  longest = 0;
  last = performance.now();
  const start = performance.now();
  const response = await fetch(base + path, { headers: { cookie } });
  const bytes = (await response.arrayBuffer()).byteLength;
  const took = performance.now() - start;
  await sleep(5);
  return { status: response.status, bytes, took, block: longest };
}

const day = new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' }).format(new Date());
const sheets = modules.map(m => m.id);
/** What each page asks when it opens (frontend/src: views, composables, stores). */
const PAGES = {
  shell: ['/api/auth/session', '/api/bootstrap', '/api/pulse', '/api/staged', '/api/outbox'],
  home: ['/api/summary', '/api/alerts'],
  table: sheets.flatMap(sheet => [
    `/api/table?module=${encodeURIComponent(sheet)}`,
    `/api/verifications?module=${encodeURIComponent(sheet)}`,
  ]),
  search: ['/api/search?q=Mechanitis', '/api/history/groups?limit=30'],
  review: [
    '/api/review?limit=50',
    '/api/alerts',
    '/api/suggested-edits?limit=50',
    '/api/solved?limit=50',
    '/api/checks?limit=50',
  ],
  clutches: ['/api/clutches/state', `/api/clutches/day?day=${day}`, '/api/clutches/notebook', '/api/clutches/photos'],
  emerged: [
    '/api/ids?kind=insectary&count=1',
    '/api/ids?kind=cam',
    '/api/ids?kind=tube',
    '/api/verifications?module=Insectary_data',
  ],
  deaths: ['/api/ids?kind=cam', '/api/ids?kind=tube'],
  census: ['/api/census', '/api/census/lookalikes'],
  monitoring: [
    '/api/monitoring/tracks',
    '/api/monitoring/wikiloc',
    '/api/monitoring/wikiloc-data',
    '/api/monitoring/wikiloc/jobs',
  ],
  proposals: ['/api/chat/proposals?all=1&chat=all', '/api/t3/status'],
};

const found = [];
for (const [page, paths] of Object.entries(PAGES)) {
  if (ONLY.length && !ONLY.includes(page)) continue;
  console.log(`\n${page}`);
  for (const path of paths) {
    const first = await ask(path);
    save();
    const cold = await ask(path);
    const warm = await ask(path);
    const cell = r => `${ms(r.took).padStart(8)} (${ms(r.block).padStart(7)})`;
    console.log(
      `  ${path.padEnd(58)} ${String(cold.status).padEnd(4)} ${String(Math.round(cold.bytes / 1024)).padStart(6)} kB` +
        `  first ${cell(first)}  after a change ${cell(cold)}  again ${cell(warm)}`,
    );
    for (const [when, r] of [
      ['first', first],
      ['after a change', cold],
      ['again', warm],
    ])
      if (r.block > OVER) found.push({ page, path, when, ...r });
  }
}

console.log(`\nBlocked over ${OVER} ms:`);
for (const f of found.sort((a, b) => b.block - a.block))
  console.log(`  ${ms(f.block).padStart(8)}  ${f.page.padEnd(11)} ${f.path} (${f.when}, ${ms(f.took)})`);
if (!found.length) console.log('  none');
console.log('/health checks:', JSON.stringify((await (await fetch(`${base}/health`)).json()).checks));
clearInterval(beat);
await app.close();
rmSync(home, { recursive: true, force: true });
