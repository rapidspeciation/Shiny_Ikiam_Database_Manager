#!/usr/bin/env node
// How long the server is busy when a chat drafts big formula proposals while
// Cambios propuestos is open (6 Oct 2026: five proposals of ~2,000 rows each,
// =XLOOKUP down CAM_ID_CollData, with ~20 others pending in the same chat).
// A local Store with a synthetic workbook (no Google, no browser): times each
// propose_changes call, the list the page asks for (first load, then each
// long-poll wake-up) and the longest stretch the event loop got no turn (the
// wait of any other request then); at the end, the delay /health would report.
//   node tools/lab/proposal-load.mjs                    the default sizes
//   node tools/lab/proposal-load.mjs --rows 23000 --big 5 --big-rows 2000 --pending 15
//   node --cpu-prof --cpu-prof-dir /tmp/prof tools/lab/proposal-load.mjs
import { createHash } from 'node:crypto';
import { mkdtempSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { performance } from 'node:perf_hooks';
import { Store } from '../../server/store.mjs';
import { LocalSheets } from '../../server/sheets.mjs';
import { columnOf } from '../../server/schema.mjs';
import { createAssistant } from '../../server/assistant.mjs';
import { allIssues } from '../../server/checks.mjs';
import { watchEventLoop } from '../../server/event-loop.mjs';

const argv = process.argv.slice(2);
const flag = (name, fallback) => {
  const i = argv.indexOf(name);
  return i < 0 ? fallback : Number(argv[i + 1]);
};
const ROWS = flag('--rows', 23000);
const BIG = flag('--big', 5);
const BIG_ROWS = flag('--big-rows', 2000);
const PENDING = flag('--pending', 15);
const THREAD = '11111111-2222-4333-8444-555555555555';
// Rows up to here already hold the formula; the proposals write it below.
const FILLED = 12792;

const formula = row => `=XLOOKUP(A${row},Collection_data!D:D,Collection_data!E:E,"NA")`;
const id = i => `${String.fromCharCode(65 + (i % 26))}${String.fromCharCode(65 + (Math.floor(i / 26) % 26))}${i}`;

async function setup() {
  const home = mkdtempSync(join(tmpdir(), 'ithomiini-load-'));
  const insectary = [];
  const collection = [];
  for (let row = 2; row <= ROWS; row++) {
    const values = { Insectary_ID: id(row), Wild_Reared: row % 3 ? 'Reared' : 'Wild', Sex: row % 2 ? 'female' : 'male', SPECIES: 'Mechanitis lysimnia' };
    insectary.push({ row, values });
    if (row % 3 === 0) collection.push({ row: collection.length + 2, values: { Insectary_ID: id(row), CAM_ID_insectary: `CAM${String(row).padStart(6, '0')}` } });
  }
  const sheets = new LocalSheets({ Insectary_data: insectary, Collection_data: collection });
  const cam = columnOf('Insectary_data', 'CAM_ID_CollData');
  for (const r of sheets.rows.get('Insectary_data'))
    if (r.row > 1 && r.row <= FILLED) r.cells[cam] = { userEnteredValue: { formulaValue: formula(r.row) }, effectiveValue: { stringValue: 'NA' } };
  const store = new Store({ localMode: true, dataDir: home }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Collection_data'] });
  const t3Chats = {
    available: true,
    threads: ids => new Map(ids.map(i => [i, { title: 'CAM_ID_CollData' }])),
    threadOfToolUse: () => THREAD,
    onlyRunning: () => THREAD,
    open: () => null,
    chatsOf: () => [],
    findProposals: async () => new Map(),
  };
  const assistant = createAssistant({ store, config: { t3: { home }, t3Chats, proposalWaitMs: 60000 } });
  store.db
    .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u-franz','franz','Franz','admin','s','h',1,'2026-01-01')")
    .run();
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,'u-franz','t3','2026-01-01')")
    .run(createHash('sha256').update('token-franz').digest('hex'));
  const call = async (name, args) =>
    JSON.parse(
      (
        await assistant.mcp(
          { authorization: 'Bearer token-franz' },
          { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args, _meta: { 'claudecode/toolUseId': 'toolu_1' } } },
        )
      ).body.result.content[0].text,
    );
  const franz = { id: 'u-franz', username: 'franz', displayName: 'Franz', role: 'admin' };
  const list = query => assistant.handle({ method: 'GET', path: '/api/chat/proposals', body: {}, user: franz, query, headers: {} });
  const post = (path, body) => assistant.handle({ method: 'POST', path, body, user: franz, query: {}, headers: {} });
  return { store, assistant, call, list, post, close: () => (store.close(), rmSync(home, { recursive: true, force: true })) };
}

const ms = n => `${n.toFixed(0)} ms`;
// The longest stretch the event loop got no turn (a 1 ms timer that keeps asking).
let longest = 0;
let last = performance.now();
const beat = setInterval(() => {
  const now = performance.now();
  longest = Math.max(longest, now - last);
  last = now;
}, 1);
const settle = () => new Promise(resolve => setTimeout(resolve, 20));
const timed = async (what, fn) => {
  await settle();
  longest = 0;
  last = performance.now();
  const s = performance.now();
  const out = await fn();
  const took = performance.now() - s;
  await settle();
  console.log(`${what}: ${ms(took)}; longest block ${ms(longest)}`);
  return out;
};

const t0 = performance.now();
const { store, call, list, post, close } = await setup();
console.log(`setup: ${ROWS} Insectary_data rows in ${ms(performance.now() - t0)}`);
// From here on, the event loop's delay as /health reports it.
const lag = watchEventLoop({ sampleMs: 10, windowMs: 3_600_000 });
const records = store.db.prepare("SELECT id, row_num FROM records WHERE sheet = 'Insectary_data' AND missing = 0 ORDER BY row_num").all();

// The chat's other pending proposals: 26–500 rows of plain values.
const sizes = Array.from({ length: PENDING }, (_, i) => [26, 40, 80, 120, 200, 300, 500][i % 7]);
let at = 0;
const t = performance.now();
let slowest = [0, 0];
for (const size of sizes) {
  const changes = records.slice(at, at + size).map(r => ({ recordId: r.id, values: { Sex: 'female', Notes_Insectary_data: `checked ${r.row_num}` } }));
  at += size;
  const s = performance.now();
  const out = await call('propose_changes', { reason: `pending ${size}`, changes });
  if (out.error) throw new Error(out.error);
  if (performance.now() - s > slowest[0]) slowest = [performance.now() - s, size];
}
console.log(
  `${PENDING} pending proposals (${sizes.reduce((a, b) => a + b, 0)} rows) in ${ms(performance.now() - t)}; the slowest ${ms(slowest[0])} (${slowest[1]} rows)`,
);

// The page: its first load, then each wake-up of its long poll (a revision it holds that is behind).
const query = (revision, have) => ({ all: '1', chat: 'auto', seen: THREAD, follow: THREAD, wait: revision ? '1' : '', revision, ...(have ? { have } : {}) });
let page = (await timed('list, first load', () => list(query('')))).body;
const digests = () => page.proposals.map(p => p.digest).join(',');

for (let i = 0; i < BIG; i++) {
  const from = FILLED + 1 + i * BIG_ROWS;
  const held = page.revision;
  // A formula down a column: one bulk group (`changes` take up to 500 rows).
  const cell = { formula: '=XLOOKUP(A{row},Collection_data!D:D,Collection_data!E:E,"NA")' };
  const out = await timed(`propose_changes #${i + 1} (${BIG_ROWS} formula rows ${from}–${from + BIG_ROWS - 1})`, () =>
    call('propose_changes', {
      reason: 'CAM_ID_CollData formula',
      bulk: [{ sheet: 'Insectary_data', rows: { from, to: from + BIG_ROWS - 1 }, set: { CAM_ID_CollData: cell } }],
    }),
  );
  if (out.error) throw new Error(out.error);
  const next = (await timed('  list, long-poll wake-up', () => list(query(held, digests())))).body;
  page = { ...next, proposals: next.proposals.map(p => (p.same ? page.proposals.find(q => q.id === p.id) : p)) };
}
await timed('list, reload with nothing held', () => list(query('')));
await timed('list, wake-up with every proposal held', () => list(query(`${page.revision}x`, digests())));
// A cell edited in the sheet, in a row of the first big proposal: the app saves it (its watchers
// look for the proposals it touches), then the page's list is built again with the sheet's value.
const edited = store.getRecordBySheetRow('Insectary_data', FILLED + 5);
await timed('a sheet edit saved (and its watchers)', async () => {
  store.persistRecord({ ...edited, values: { ...edited.values, Sex: 'male' }, version: edited.version + 1, updatedAt: new Date().toISOString() });
  await new Promise(resolve => setImmediate(resolve));
});
// The checks (Revisión) found again for the edited copy: run in the background after any save, and read by the list.
await timed('the checks found again after the edit', async () => allIssues(store));
await timed('list, wake-up after the sheet edit', () => list(query(`${page.revision}x`, digests())));
// The last big proposal: the assistant reads it, the person takes a row out of it in the table.
const big = page.proposals.find(p => p.changes?.length >= BIG_ROWS);
if (big) {
  await timed('get_proposal of a big one', () => call('get_proposal', { proposalId: big.id }));
  const row = big.changes[1];
  const edit = await timed('a person takes one row out of it', () =>
    post(`/api/chat/proposals/${big.id}/edit`, { remove: [row.key] }),
  );
  if (edit.status !== 200) throw new Error(JSON.stringify(edit.body));
  await timed('list, wake-up after that edit', () => list(query(`${page.revision}x`, digests())));
}
clearInterval(beat);
const { p99Ms, maxMs } = lag.stats();
lag.stop();
console.log(`total ${ms(performance.now() - t0)}; event loop after the setup (as /health): p99 ${ms(p99Ms)}, max ${ms(maxMs)}`);
close();
