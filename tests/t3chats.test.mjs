import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { DatabaseSync } from 'node:sqlite';
import { appendFileSync, mkdirSync, mkdtempSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { addReports, chatsOnScreen, createT3Chats, openChat } from '../server/t3chats.mjs';
import { signIn, until } from './helpers/assistant.mjs';

// Proposals by T3 Code chat: which chat drafted a proposal, and which chat the panel follows.

const ENV = '4e6c4765-8cfa-4adc-b761-3c3bae2ae7e0';
const A = '5c8de89d-bbaf-4328-b354-74733e099781';
const B = 'c2dffe41-61ba-4e41-b25d-a5feb3dd0eb0';
const OTHER = '1e5e595a-45c1-4416-956c-1cf308941fe9';
const iso = (ms = Date.now()) => new Date(ms).toISOString();

/** A line of T3's trace log: its page sending traces, from the chat it shows. */
const traceLine = (path, at = Date.now()) =>
  JSON.stringify({
    type: 'effect-span',
    name: 'http.server POST',
    startTimeUnixNano: String(BigInt(at) * 1_000_000n),
    attributes: {
      'url.path': '/api/observability/v1/traces',
      'http.request.header.referer': `https://t3.example.org${path}`,
    },
  });

/** A T3 home as a stock install keeps it: its state database (the tables read) and its trace log. */
function t3Home() {
  const home = mkdtempSync(join(tmpdir(), 't3-chats-'));
  mkdirSync(join(home, 'userdata', 'logs'), { recursive: true });
  const db = new DatabaseSync(join(home, 'userdata', 'state.sqlite'));
  db.exec(`
    CREATE TABLE projection_projects (project_id TEXT PRIMARY KEY, title TEXT, workspace_root TEXT, deleted_at TEXT);
    CREATE TABLE projection_threads (thread_id TEXT PRIMARY KEY, project_id TEXT, title TEXT, created_at TEXT, updated_at TEXT,
      latest_user_message_at TEXT, deleted_at TEXT, archived_at TEXT);
    CREATE TABLE projection_thread_activities (activity_id TEXT PRIMARY KEY, thread_id TEXT, kind TEXT, payload_json TEXT, created_at TEXT);
    CREATE INDEX idx_projection_thread_activities_thread_created ON projection_thread_activities(thread_id, created_at);
    CREATE TABLE projection_thread_sessions (thread_id TEXT PRIMARY KEY, status TEXT);
    INSERT INTO projection_projects VALUES ('p-franz', 'Ithomiini · Franz', '/srv/t3-workspaces/franz', NULL);
    INSERT INTO projection_projects VALUES ('p-ana', 'Ithomiini · Ana', '/srv/t3-workspaces/ana/', NULL);
  `);
  const thread = (id, project, title, lastUser) =>
    db.prepare('INSERT INTO projection_threads VALUES (?,?,?,?,?,?,NULL,NULL)').run(id, project, title, iso(), iso(), lastUser);
  thread(A, 'p-franz', 'Cuaderno de emergidos', iso(Date.now() - 60_000));
  thread(B, 'p-franz', 'Cuaderno de posturas', iso(Date.now() - 120_000));
  thread(OTHER, 'p-ana', 'Chat de Ana', iso());
  /** T3 records a tool call of a chat (and the chat's updated_at follows it). */
  const call = (threadId, kind, payload) => {
    db.prepare('INSERT INTO projection_thread_activities VALUES (?,?,?,?,?)').run(randomUUID(), threadId, kind, JSON.stringify(payload), iso());
    db.prepare('UPDATE projection_threads SET updated_at = ? WHERE thread_id = ?').run(iso(), threadId);
  };
  /** A T3 page shows this chat now (its last two reports, 2 s apart). */
  const screen = (path, at = Date.now()) =>
    appendFileSync(join(home, 'userdata', 'logs', 'server.trace.ndjson'), `${traceLine(path, at - 2000)}\n${traceLine(path, at)}\n`);
  const remove = () => {
    db.close();
    rmSync(home, { recursive: true, force: true });
  };
  return { home, db, call, screen, remove };
}

async function fixture({ withT3 = true } = {}) {
  const sheets = new LocalSheets({
    Collection_data: [{ row: 2, values: { Purpose: 'Monitoring', SPECIES: 'Oleria gunilla', Sex: 'male', CAM_ID: 'CAM000001' } }],
    Taxonomy_v18Jun25: [{ row: 2, values: { species: 'Oleria gunilla', tribe: 'Ithomiini' } }],
    Location_data: [{ row: 2, values: { Collection_location: 'Ikiam' } }],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Collection_data', 'Taxonomy_v18Jun25', 'Location_data'] });
  const t3 = withT3 ? t3Home() : null;
  // T3's chats on a clock the test moves on (the trace log is read at most once a second).
  let offset = 0;
  const later = ms => void (offset += ms);
  const t3Chats = t3 && createT3Chats({ home: t3.home, now: () => Date.now() + offset });
  const assistant = createAssistant({ store, config: { ...(t3 ? { t3Chats, followCheckMs: 10 } : {}) } });
  const { user, call } = signIn(store, assistant);
  const record = store.getRecordBySheetRow('Collection_data', 2);
  /** propose_changes from a T3 chat, as Claude Code calls it (its tool-use id in _meta). */
  const propose = async (note, toolUseId) => {
    const result = await call(
      'propose_changes',
      { reason: note, changes: [{ recordId: record.id, values: { Sex: 'female' }, note }] },
      toolUseId ? { 'claudecode/toolUseId': toolUseId } : undefined,
    );
    assert.ok(result.proposalId, JSON.stringify(result));
    return result.proposalId;
  };
  const list = (query = {}) => assistant.handle({ method: 'GET', path: '/api/chat/proposals', user, query: { all: '1', ...query } });
  const linked = id => store.db.prepare('SELECT t3_thread, t3_title FROM ai_proposals WHERE id = ?').get(id);
  const close = () => {
    store.close();
    t3Chats?.close();
    t3?.remove();
  };
  return { store, assistant, t3, user, propose, list, linked, later, close };
}

test("T3's trace log tells the chat on screen; a person's latest one is open, two pages keep the one shown", () => {
  const now = Date.now();
  const text = [
    '{"cut": "the first line of a tail"',
    traceLine(`/${ENV}/${A}`, now - 9000),
    traceLine(`/draft/${randomUUID()}`, now - 8000),
    traceLine(`/${ENV}/${B}`, now - 6000),
    traceLine(`/${ENV}/${A}`, now - 3000),
    traceLine(`/${ENV}/${OTHER}/diff?file=x`, now - 1000),
    JSON.stringify({ name: 'http.server GET', attributes: { 'url.path': '/api/other', 'http.request.header.referer': `https://x/${ENV}/${B}` } }),
  ].join('\n');
  const seen = chatsOnScreen(text);
  assert.deepEqual(
    seen.map(([thread, at]) => [thread, now - at]),
    [
      [A, 9000],
      [B, 6000],
      [A, 3000],
      [OTHER, 1000],
    ],
    'drafts and other requests are not chats',
  );
  const franz = id => id === A || id === B;
  const streaksOf = reports => addReports(new Map(), reports);
  const every2s = (thread, from, to) => Array.from({ length: Math.floor((from - to) / 2000) + 1 }, (_, i) => [thread, now - from + i * 2000]);
  const sorted = (...lists) => lists.flat().sort((a, b) => a[1] - b[1]);
  // One page on A, then the person clicks B: B once it has reported twice.
  const moved = sorted(every2s(A, 20_000, 8000), every2s(B, 6000, 0));
  assert.equal(openChat(streaksOf(moved), franz, { now }), B);
  assert.equal(openChat(streaksOf(sorted(every2s(A, 20_000, 8000), [[B, now]])), franz, { now }), A, 'one report is not enough yet');
  // A second page left on A keeps reporting it: B, opened later, is the chat the person is on.
  const twoPages = sorted(every2s(A, 20_000, 0), every2s(B, 6000, 0));
  // Its reports pause for a few seconds now and then: not a new streak.
  const paused = sorted(every2s(A, 60_000, 30_000), every2s(A, 24_000, 0), every2s(B, 40_000, 0));
  assert.equal(openChat(streaksOf(paused), franz, { now }), B);
  assert.equal(openChat(streaksOf(twoPages), franz, { now }), B);
  // Back from B to A on the same page: A starts again.
  const back = sorted(every2s(A, 50_000, 40_000), every2s(B, 38_000, 8000), every2s(A, 6000, 0));
  assert.equal(openChat(streaksOf(back), franz, { now }), A);
  // Ana's chats are not theirs; a tab in the background (a report a minute) is never open.
  assert.equal(openChat(streaksOf(sorted(every2s(OTHER, 6000, 0), every2s(A, 20_000, 0))), franz, { now }), A);
  const background = streaksOf(sorted(every2s(A, 20_000, 0)));
  addReports(background, [[B, now - 60_000]]);
  addReports(background, [[B, now]]);
  assert.equal(openChat(background, franz, { now }), A);
  // Streaks carry over between reads of the log: a later read without B's start keeps it.
  const kept = streaksOf(twoPages);
  addReports(kept, [
    [A, now + 1000],
    [B, now + 1500],
  ]);
  assert.equal(openChat(kept, franz, { now: now + 2000 }), B);
  // The page stopped reporting a little while ago: still the chat it showed.
  assert.equal(openChat(streaksOf(every2s(A, 21_000, 15_000)), franz, { now }), A);
  // A page closed a while ago says nothing.
  assert.equal(openChat(streaksOf(moved), franz, { now: now + 60_000 }), null);
  assert.equal(openChat(new Map(), franz, { now }), null);
});

test('a proposal is linked to the T3 chat that made it: by tool-use id, later if T3 records the call late, or by its result', async () => {
  const { t3, propose, list, linked, close } = await fixture();
  try {
    // T3 has recorded the call already: linked at once, with the chat's title.
    t3.call(A, 'tool.started', { itemType: 'mcp_tool_call', toolCallId: 'toolu_A1', status: 'inProgress' });
    const first = await propose('foto 1', 'toolu_A1');
    assert.deepEqual({ ...linked(first) }, { t3_thread: A, t3_title: 'Cuaderno de emergidos' });

    // T3 records the call after the tool ran: linked when the list is next asked for.
    const late = await propose('foto 2', 'toolu_B1');
    assert.equal(linked(late).t3_thread, null);
    t3.call(B, 'tool.started', { itemType: 'mcp_tool_call', toolCallId: 'toolu_B1' });
    // Codex (no tool-use id): found by the chat's tool result naming the proposal.
    const codex = await propose('foto 3');
    t3.call(B, 'tool.completed', { itemType: 'mcp_tool_call', detail: `{"proposalId":"${codex}","rows":1}` });
    await list();
    assert.equal(linked(late).t3_thread, B, 'by tool-use id, before the answer');
    await until(() => linked(codex).t3_thread === B);

    const body = (await list({ chat: 'all' })).body;
    assert.deepEqual(
      body.proposals.map(p => [p.chat, p.source]),
      [
        [B, 'Cuaderno de posturas'],
        [B, 'Cuaderno de posturas'],
        [A, 'Cuaderno de emergidos'],
      ],
    );
    assert.deepEqual(
      body.chats.map(c => [c.title, c.pending]),
      [
        ['Cuaderno de posturas', 2],
        ['Cuaderno de emergidos', 1],
      ],
    );
  } finally {
    close();
  }
});

test('the panel follows the chat open in T3, else the latest active one; other chats and those outside T3 stay reachable', async () => {
  const { store, t3, user, propose, list, later, close } = await fixture();
  try {
    t3.call(A, 'tool.started', { toolCallId: 'toolu_A1' });
    t3.call(B, 'tool.started', { toolCallId: 'toolu_B1' });
    const inA = await propose('foto 1', 'toolu_A1');
    await new Promise(resolve => setTimeout(resolve, 5));
    const inB = await propose('foto 2', 'toolu_B1');
    // A proposal made outside T3 (Revisión de datos).
    const thread = randomUUID();
    const outside = randomUUID();
    store.db.prepare("INSERT INTO ai_threads (id,owner_id,title,created_at,updated_at) VALUES (?,?,'Revisión de datos',?,?)").run(thread, user.id, iso(), iso());
    store.db
      .prepare("INSERT INTO ai_proposals (id,thread_id,owner_id,changes_json,reason,status,created_at,updated_at) VALUES (?,?,?,'[]','Revisión','pending',?,?)")
      .run(outside, thread, user.id, iso(Date.now() - 3600_000), iso(Date.now() - 3600_000));

    // No T3 page open: the chat active last (B's proposal is the newest).
    let body = (await list({ chat: 'auto' })).body;
    assert.equal(body.scope.how, 'recent');
    assert.deepEqual(body.proposals.map(p => p.id), [inB]);

    // A is open in T3: its proposals only, the others listed in the selector.
    t3.screen(`/${ENV}/${A}`);
    later(1000);
    body = (await list({ chat: 'auto' })).body;
    assert.deepEqual({ ...body.scope }, { chat: A, how: 'open', title: 'Cuaderno de emergidos' });
    assert.deepEqual(body.proposals.map(p => p.id), [inA]);
    assert.deepEqual(
      body.chats.map(c => [c.id, c.pending]),
      [
        [B, 1],
        [A, 1],
        ['app', 1],
      ],
    );
    assert.equal(body.follow.chat, A);
    // Another chat picked by hand, those outside T3, all of them.
    assert.deepEqual((await list({ chat: B })).body.proposals.map(p => p.id), [inB]);
    assert.deepEqual((await list({ chat: 'app' })).body.proposals.map(p => p.id), [outside]);
    assert.equal((await list({ chat: 'all' })).body.proposals.length, 3);
    // One proposal (#/propuestas/<id>), whatever the chat.
    assert.deepEqual((await list({ chat: 'auto', only: inB })).body.proposals.map(p => p.id), [inB]);

    // The panel waits for changes; opening B in T3 wakes it with B's proposals.
    const { revision } = (await list({ chat: 'auto' })).body;
    let woken = false;
    const waiting = list({ chat: 'auto', wait: '1', revision, follow: A }).then(out => ((woken = true), out));
    await new Promise(resolve => setTimeout(resolve, 30));
    assert.equal(woken, false, 'nothing changed yet');
    t3.screen(`/${ENV}/${B}`);
    later(1000);
    body = (await waiting).body;
    assert.equal(body.scope.chat, B);
    assert.deepEqual(body.proposals.map(p => p.id), [inB]);
  } finally {
    close();
  }
});

test("the chat the page's T3 frame shows (its bridge) wins over the trace log's guess", async () => {
  const { t3, propose, list, later, close } = await fixture();
  try {
    t3.call(A, 'tool.started', { toolCallId: 'toolu_A1' });
    t3.call(B, 'tool.started', { toolCallId: 'toolu_B1' });
    const inA = await propose('foto 1', 'toolu_A1');
    await new Promise(resolve => setTimeout(resolve, 5));
    const inB = await propose('foto 2', 'toolu_B1');
    // Another T3 tab left on B keeps reporting it; the frame beside the panel shows A.
    t3.screen(`/${ENV}/${B}`);
    later(1000);
    assert.equal((await list({ chat: 'auto' })).body.scope.chat, B, 'the guess');
    let body = (await list({ chat: 'auto', seen: A })).body;
    assert.deepEqual({ ...body.scope }, { chat: A, how: 'open', title: 'Cuaderno de emergidos' });
    assert.deepEqual(body.proposals.map(p => p.id), [inA]);
    // A chat picked by hand, while the frame shows A.
    body = (await list({ chat: B, seen: A })).body;
    assert.deepEqual([body.scope.chat, body.follow.chat], [B, A]);
    // A new chat (a draft): none of the other chats' proposals; they stay in the selector.
    body = (await list({ chat: 'auto', seen: 'draft' })).body;
    assert.deepEqual({ ...body.scope }, { chat: 'draft', how: 'open', title: null });
    assert.deepEqual(body.proposals, []);
    assert.equal(body.chats.length, 2);
    // No chat on screen (T3's settings): the latest active chat, not the other tab's.
    body = (await list({ chat: 'auto', seen: 'none' })).body;
    assert.deepEqual([body.scope.chat, body.scope.how], [B, 'recent']);
    assert.deepEqual(body.proposals.map(p => p.id), [inB]);
    // Anything else is ignored (the guess).
    assert.equal((await list({ chat: 'auto', seen: "x' OR 1" })).body.scope.chat, B);
    // The page asks again itself when its frame moves: the wait is for proposals only.
    const { revision } = body;
    let woken = false;
    const waiting = list({ chat: 'auto', seen: A, follow: A, wait: '1', revision }).then(out => ((woken = true), out));
    await new Promise(resolve => setTimeout(resolve, 30));
    assert.equal(woken, false, 'no proposal yet');
    await propose('foto 3', 'toolu_A1');
    body = (await waiting).body;
    assert.equal(body.proposals.length, 2);
  } finally {
    close();
  }
});

test('without T3 the panel shows every proposal, as before', async () => {
  const { propose, list, linked, close } = await fixture({ withT3: false });
  try {
    const id = await propose('foto 1', 'toolu_X');
    assert.equal(linked(id).t3_thread, null);
    const body = (await list({ chat: 'auto' })).body;
    assert.equal(body.scope.chat, 'all');
    assert.deepEqual(body.proposals.map(p => p.id), [id]);
    // T3 configured but its files missing: the same.
    const chats = createT3Chats({ home: join(tmpdir(), 'no-t3-here') });
    assert.equal(chats.available, false);
    assert.equal(chats.open('franz'), null);
    assert.deepEqual(chats.chatsOf('franz'), []);
    assert.equal(chats.threadOfToolUse('toolu_X', iso(0)), null);
  } finally {
    close();
  }
});

test("a person's T3 chats are those of their workspace folder", () => {
  const { home, db, remove } = t3Home();
  const chats = createT3Chats({ home });
  assert.deepEqual(chats.projectsOf('franz'), ['p-franz']);
  assert.deepEqual(chats.projectsOf('ana'), ['p-ana'], 'a trailing slash is the same folder');
  assert.deepEqual(chats.chatsOf('franz').map(c => c.id), [A, B], 'the latest written in first');
  db.prepare("INSERT INTO projection_thread_sessions VALUES (?, 'running')").run(A);
  assert.equal(chats.onlyRunning('franz'), A);
  db.prepare("INSERT INTO projection_thread_sessions VALUES (?, 'running')").run(B);
  assert.equal(chats.onlyRunning('franz'), null, 'two chats answering: unknown');
  chats.close();
  remove();
});
