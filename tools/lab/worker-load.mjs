#!/usr/bin/env node
// How long people wait while a chat drafts big proposals, with the assistant's work in its worker
// threads (ASSISTANT_WORKER=auto, server/assistant-host.mjs) and without (0). The whole app on a
// database file, over HTTP: Franz's Cambios propuestos open (its long poll), a chat (MCP with his
// token) drafting five formula proposals of 2,000 rows down CAM_ID_CollData beside 15 smaller ones,
// and Ana checking clutches (a check posted and taken back every 100 ms). Reported: Ana's request
// times (p50, p99, longest) while the chat works, and how late the app's event loop got.
//   node tools/lab/worker-load.mjs                 both modes, one after the other
//   node tools/lab/worker-load.mjs --mode auto     one mode
//   node tools/lab/worker-load.mjs --rows 23000 --big 5 --big-rows 2000 --pending 15 --every 100
import { spawnSync } from 'node:child_process';
import { createHash, randomUUID } from 'node:crypto';
import { mkdtempSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { performance } from 'node:perf_hooks';
import { fileURLToPath } from 'node:url';

const argv = process.argv.slice(2);
const flag = (name, fallback) => {
  const i = argv.indexOf(name);
  return i < 0 ? fallback : argv[i + 1];
};
const MODE = flag('--mode', 'both');

if (MODE === 'both') {
  // Each mode in its own process: nothing kept from the other.
  const rest = argv.filter((a, i) => a !== '--mode' && argv[i - 1] !== '--mode');
  for (const mode of ['0', 'auto']) {
    const run = spawnSync(process.execPath, [fileURLToPath(import.meta.url), '--mode', mode, ...rest], {
      stdio: 'inherit',
    });
    if (run.status) process.exit(run.status);
  }
  process.exit(0);
}

const { createApp } = await import('../../server/index.mjs');
const { LocalSheets } = await import('../../server/sheets.mjs');
const { columnOf } = await import('../../server/schema.mjs');
const { watchEventLoop } = await import('../../server/event-loop.mjs');

const ROWS = Number(flag('--rows', 23000));
const BIG = Number(flag('--big', 5));
const BIG_ROWS = Number(flag('--big-rows', 2000));
const PENDING = Number(flag('--pending', 15));
const EVERY = Number(flag('--every', 100));
// Rows up to here already hold the formula; the proposals write it below.
const FILLED = 12792;
const formula = row => `=XLOOKUP(A${row},Collection_data!D:D,Collection_data!E:E,"NA")`;
const id = i => `${String.fromCharCode(65 + (i % 26))}${String.fromCharCode(65 + (Math.floor(i / 26) % 26))}${i}`;
const ms = n => `${n.toFixed(0)} ms`;
const pct = (list, p) => {
  const sorted = [...list].sort((a, b) => a - b);
  return sorted.length ? sorted[Math.min(sorted.length - 1, Math.floor(sorted.length * p))] : 0;
};

const home = mkdtempSync(join(tmpdir(), 'ithomiini-worker-load-'));
const insectary = [];
const collection = [];
for (let row = 2; row <= ROWS; row++) {
  insectary.push({
    row,
    values: {
      Insectary_ID: id(row),
      Wild_Reared: row % 3 ? 'Reared' : 'Wild',
      Sex: row % 2 ? 'female' : 'male',
      SPECIES: 'Mechanitis lysimnia',
    },
  });
  if (row % 3 === 0)
    collection.push({
      row: collection.length + 2,
      values: { Insectary_ID: id(row), CAM_ID_insectary: `CAM${String(row).padStart(6, '0')}` },
    });
}
const stocks = Array.from({ length: 40 }, (_, i) => ({
  row: i + 2,
  values: { 'CLUTCH NUMBER': 1000 + i, SPECIES: 'Mechanitis lysimnia', 'DATE LAID': 46280 },
}));
const sheets = new LocalSheets({ Insectary_data: insectary, Collection_data: collection, Insectary_stocks: stocks });
const cam = columnOf('Insectary_data', 'CAM_ID_CollData');
for (const r of sheets.rows.get('Insectary_data'))
  if (r.row > 1 && r.row <= FILLED)
    r.cells[cam] = { userEnteredValue: { formulaValue: formula(r.row) }, effectiveValue: { stringValue: 'NA' } };

const t0 = performance.now();
const app = await createApp(
  {
    databasePath: join(home, 'app.sqlite'),
    sheetsCopyPath: null,
    localMode: true,
    secureCookies: false,
    syncIntervalMs: 0,
    setupToken: 'load-setup',
    assistantWorker: MODE,
    proposalWaitMs: 20_000,
  },
  { sheets },
);
await app.ready;
const { port } = await app.listen(0, '127.0.0.1');
const base = `http://127.0.0.1:${port}/ithomiini`;

/** A signed-in person: their cookie and CSRF token. */
function person() {
  const me = { cookie: '', csrf: '' };
  me.call = async (path, method = 'GET', body) => {
    const response = await fetch(base + path, {
      method,
      headers: {
        ...(body ? { 'content-type': 'application/json' } : {}),
        ...(me.cookie ? { cookie: me.cookie, 'x-csrf-token': me.csrf } : {}),
      },
      body: body ? JSON.stringify(body) : undefined,
    });
    const data = await response.json();
    if (response.headers.get('set-cookie')) me.cookie = response.headers.get('set-cookie').split(';')[0];
    if (data.csrf) me.csrf = data.csrf;
    if (response.status >= 400)
      throw new Error(`${method} ${path}: ${response.status} ${JSON.stringify(data).slice(0, 300)}`);
    return data;
  };
  return me;
}
const franz = person();
await franz.call('/api/auth/setup', 'POST', { token: 'load-setup', username: 'franz', password: 'load-admin-123' });
await franz.call('/api/admin/users', 'POST', {
  requestId: randomUUID(),
  username: 'ana',
  role: 'editor',
  displayName: 'Ana Torres',
  password: 'load-pass-123',
});
const ana = person();
await ana.call('/api/auth/login', 'POST', { username: 'ana', password: 'load-pass-123' });
const franzId = app.store.db.prepare("SELECT id FROM users WHERE username = 'franz'").get().id;
app.store.db
  .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
  .run(createHash('sha256').update('token-franz').digest('hex'), franzId);
const clutches = app.store.db
  .prepare("SELECT id FROM records WHERE sheet = 'Insectary_stocks' AND missing = 0")
  .all()
  .map(r => r.id);
const records = app.store.db
  .prepare("SELECT id, row_num FROM records WHERE sheet = 'Insectary_data' AND missing = 0 ORDER BY row_num")
  .all();
console.log(
  `\n== ASSISTANT_WORKER=${MODE}: ${ROWS} Insectary_data rows, the app ready in ${ms(performance.now() - t0)}`,
);

/** A tool call of the chat (MCP over HTTP, as T3 Code makes it). */
async function tool(name, args) {
  const response = await fetch(`${base}/api/ai/mcp`, {
    method: 'POST',
    headers: { 'content-type': 'application/json', authorization: 'Bearer token-franz' },
    body: JSON.stringify({ jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } }),
  });
  const body = await response.json();
  if (body.error) throw new Error(`${name}: ${body.error.message}`);
  const out = JSON.parse(body.result.content[0].text);
  if (out.error) throw new Error(`${name}: ${out.error}`);
  return out;
}

// The chat's other pending proposals: 26–500 rows of plain values.
let at = 0;
for (const size of Array.from({ length: PENDING }, (_, i) => [26, 40, 80, 120, 200, 300, 500][i % 7])) {
  const changes = records
    .slice(at, at + size)
    .map(r => ({ recordId: r.id, values: { Sex: 'female', Notes_Insectary_data: `checked ${r.row_num}` } }));
  at += size;
  await tool('propose_changes', { reason: `pending ${size}`, changes });
}

// Franz's Cambios propuestos: its long poll, as the page keeps it (the proposals it holds by digest).
let page = { revision: '', stamp: '', proposals: [] };
let polling = true;
let pageAnswers = 0;
const poll = (async () => {
  while (polling) {
    const q = new URLSearchParams({
      all: '1',
      ...(page.revision ? { wait: '1', revision: page.revision, stamp: page.stamp } : {}),
    });
    const have = page.proposals
      .map(p => p.digest)
      .filter(Boolean)
      .join(',');
    if (have) q.set('have', have);
    const next = await franz
      .call(`/api/chat/proposals?${q}`)
      .catch(e => (polling && console.error('page:', e.message), null));
    if (!next) break;
    pageAnswers++;
    if (next.unchanged) continue;
    page = { ...next, proposals: next.proposals.map(p => (p.same ? page.proposals.find(q => q.id === p.id) : p)) };
  }
})();
await new Promise(resolve => setTimeout(resolve, 500));

// Ana at the clutches: a check posted and taken back every EVERY ms; each request timed.
const times = [];
let checking = true;
const checks = (async () => {
  let n = 0;
  while (checking) {
    const started = performance.now();
    const recordId = clutches[n++ % clutches.length];
    let s = performance.now();
    const { check } = await ana.call('/api/clutches/checks', 'POST', { requestId: randomUUID(), recordId });
    times.push(performance.now() - s);
    s = performance.now();
    await ana.call(`/api/clutches/checks/${check.id}`, 'DELETE');
    times.push(performance.now() - s);
    await new Promise(resolve => setTimeout(resolve, Math.max(0, EVERY - (performance.now() - started))));
  }
})();

// The chat drafts the big ones, one after the other.
const lag = watchEventLoop({ sampleMs: 10, windowMs: 3_600_000 });
const started = performance.now();
const drafts = [];
for (let i = 0; i < BIG; i++) {
  const from = FILLED + 1 + i * BIG_ROWS;
  const s = performance.now();
  await tool('propose_changes', {
    reason: 'CAM_ID_CollData formula',
    bulk: [
      {
        sheet: 'Insectary_data',
        rows: { from, to: from + BIG_ROWS - 1 },
        set: { CAM_ID_CollData: { formula: '=XLOOKUP(A{row},Collection_data!D:D,Collection_data!E:E,"NA")' } },
      },
    ],
  });
  drafts.push(performance.now() - s);
}
// The page catches up with the last one.
await new Promise(resolve => setTimeout(resolve, 3000));
const took = performance.now() - started;
checking = false;
await checks;
const { p99Ms, maxMs } = lag.stats();
lag.stop();
const health = await (await fetch(`http://127.0.0.1:${port}/health`)).json();
polling = false;
await app.close();
await poll.catch(() => {});
rmSync(home, { recursive: true, force: true });

console.log(
  `the chat: ${BIG} proposals of ${BIG_ROWS} rows in ${ms(took)} (each ${drafts.map(ms).join(', ')}); the page answered ${pageAnswers} times, holds ${page.proposals.length} proposals`,
);
console.log(
  `Ana (${times.length} requests): p50 ${ms(pct(times, 0.5))}, p99 ${ms(pct(times, 0.99))}, longest ${ms(Math.max(...times))}`,
);
console.log(
  `the app's event loop: p99 ${ms(p99Ms)} late, longest ${ms(maxMs)}; /health assistant: ${JSON.stringify(health.assistant)}`,
);
