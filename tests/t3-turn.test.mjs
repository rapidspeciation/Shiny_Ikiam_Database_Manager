// The WhatsApp bridge's turn helper (tools/whatsapp/t3-turn.mjs) against a stand-in T3: an in-memory copy of the
// projection tables it reads and a `send` that plays T3's part. No T3, no chat, no model: `turn()` is imported, never
// run as a script (as a script it refuses under node --test; tests/no-ai-subscription.test.mjs shows that).
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { DatabaseSync } from 'node:sqlite';
import { chmodSync, mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { modelSelection, senderRole, turn } from '../tools/whatsapp/t3-turn.mjs';

/** A stand-in T3: its state tables, the commands it got, and what it does on each command and each poll. */
function fakeT3({ onSend = () => {}, onTick = () => {} } = {}) {
  const db = new DatabaseSync(':memory:');
  db.exec(`
    CREATE TABLE orchestration_v2_projection_threads (thread_id TEXT, project_id TEXT, title TEXT, deleted_at TEXT, archived_at TEXT, provider_instance_id TEXT, payload_json TEXT);
    CREATE TABLE orchestration_v2_projection_runs (run_id TEXT, thread_id TEXT, status TEXT, ordinal INTEGER);
    CREATE TABLE orchestration_v2_projection_turn_items (run_id TEXT, thread_id TEXT, ordinal INTEGER, type TEXT, status TEXT, payload_json TEXT);
    CREATE TABLE orchestration_v2_projection_messages (run_id TEXT, role TEXT, streaming INTEGER, created_at TEXT, payload_json TEXT);
  `);
  const t3 = {
    db,
    sent: [],
    ticks: 0,
    thread: (id, instance = 'claudeAgent') =>
      db.prepare('INSERT INTO orchestration_v2_projection_threads VALUES (?, ?, ?, NULL, NULL, ?, ?)').run(id, 'P', 'Chat', instance, '{}'),
    run: (id, thread, status, ordinal) => db.prepare('INSERT INTO orchestration_v2_projection_runs VALUES (?, ?, ?, ?)').run(id, thread, status, ordinal),
    status: (id, status) => db.prepare('UPDATE orchestration_v2_projection_runs SET status = ? WHERE run_id = ?').run(status, id),
    item: (run, thread, ordinal, type, status, payload = {}) =>
      db.prepare('INSERT INTO orchestration_v2_projection_turn_items VALUES (?, ?, ?, ?, ?, ?)').run(run, thread, ordinal, type, status, JSON.stringify(payload)),
    itemStatus: (run, ordinal, status) => db.prepare('UPDATE orchestration_v2_projection_turn_items SET status = ? WHERE run_id = ? AND ordinal = ?').run(status, run, ordinal),
    answer: (run, text) =>
      db.prepare("INSERT INTO orchestration_v2_projection_messages VALUES (?, 'assistant', 0, ?, ?)").run(run, new Date().toISOString(), JSON.stringify({ text })),
  };
  t3.send = async (...commands) => {
    for (const c of commands) {
      t3.sent.push(c);
      onSend(c, t3);
    }
  };
  // Each poll of the helper is one step of the stand-in's world; no real waiting.
  t3.sleep = async () => {
    t3.ticks++;
    onTick(t3);
    await new Promise(r => setImmediate(r));
  };
  return t3;
}

const types = t3 => t3.sent.map(c => c.type);

test('steering: a message while the chat answers goes into that run (deliveryIntent steer) and returns at once', async () => {
  const t3 = fakeT3({
    onSend: (c, t) => {
      // T3 takes it into the run under way: its user_message item says steer.
      if (c.type === 'message.dispatch') t.item('R1', 'T1', 5, 'user_message', 'completed', { messageId: c.messageId, inputIntent: 'steer' });
    },
  });
  t3.thread('T1');
  t3.run('R1', 'T1', 'running', 1);
  const out = await turn({ op: 'turn', threadId: 'T1', text: 'y agrega el clima', steer: true, timeoutMs: 2000 }, t3);
  assert.deepEqual(out, { ok: true, threadId: 'T1', steered: true });
  assert.deepEqual(types(t3), ['message.dispatch']);
  const [d] = t3.sent;
  assert.equal(d.deliveryIntent, 'steer');
  assert.deepEqual(d.dispatchMode, { type: 'queue_after_active' });
  assert.equal(d.text, 'y agrega el clima');
  assert.equal(d.threadId, 'T1');
});

test('steering: when the run ended as the message came, T3 gives it a run of its own and the helper waits for that answer', async () => {
  const t3 = fakeT3({
    onSend: (c, t) => {
      if (c.type !== 'message.dispatch') return;
      t.status('R1', 'completed');
      t.run('R2', 'T1', 'running', 2);
      t.item('R2', 'T1', 1, 'user_message', 'completed', { messageId: c.messageId, inputIntent: 'send' });
    },
    onTick: t => {
      if (t.ticks === 3) {
        t.answer('R2', 'Respuesta aparte.');
        t.status('R2', 'completed');
      }
    },
  });
  t3.thread('T1');
  t3.run('R1', 'T1', 'running', 1);
  const out = await turn({ op: 'turn', threadId: 'T1', text: 'y esto', steer: true, timeoutMs: 2000 }, t3);
  assert.equal(out.ok, true);
  assert.equal(out.steered, false);
  assert.equal(out.status, 'completed');
  assert.equal(out.text, 'Respuesta aparte.');
  assert.deepEqual(types(t3), ['message.dispatch']);
});

test('steering: steer on an idle chat starts a turn as usual; without steer a busy chat is waited for, never cut into', async () => {
  // Idle with steer: a normal thread.turn.start.
  const idle = fakeT3({
    onSend: (c, t) => c.type === 'thread.turn.start' && t.run('R2', 'T1', 'running', 2),
    onTick: t => t.ticks === 2 && (t.answer('R2', 'Hola.'), t.status('R2', 'completed')),
  });
  idle.thread('T1');
  idle.run('R1', 'T1', 'completed', 1);
  const a = await turn({ op: 'turn', threadId: 'T1', text: 'hola', steer: true, timeoutMs: 2000 }, idle);
  assert.deepEqual(types(idle), ['thread.turn.start']);
  assert.deepEqual({ ok: a.ok, steered: a.steered, text: a.text }, { ok: true, steered: false, text: 'Hola.' });

  // Busy without steer: no dispatch; the turn starts only after the run under way ended.
  let startedWhileBusy = null;
  const busy = fakeT3({
    onSend: (c, t) => {
      if (c.type !== 'thread.turn.start') return;
      startedWhileBusy = t.db.prepare("SELECT status FROM orchestration_v2_projection_runs WHERE run_id = 'R1'").get().status;
      t.run('R2', 'T1', 'running', 2);
    },
    onTick: t => {
      if (t.ticks === 3) t.status('R1', 'completed');
      if (t.ticks === 6) (t.answer('R2', 'Listo.'), t.status('R2', 'completed'));
    },
  });
  busy.thread('T1');
  busy.run('R1', 'T1', 'running', 1);
  const b = await turn({ op: 'turn', threadId: 'T1', text: 'otra' }, busy);
  assert.deepEqual(types(busy), ['thread.turn.start']);
  assert.equal(startedWhileBusy, 'completed');
  assert.equal(b.text, 'Listo.');
  assert.equal('steered' in b, false);
});

test('stopAfterTools: the run is interrupted once its first k tool calls have settled, never cutting one; ok with stopped', async () => {
  let settledAt = null;
  const t3 = fakeT3({
    onSend: (c, t) => {
      if (c.type === 'thread.turn.start') t.run('R1', c.threadId, 'running', 1);
      if (c.type === 'run.interrupt') t.status(c.runId, 'interrupted');
    },
    onTick: t => {
      // The workspace setup (not counted), a tool that settles, a Bash call still running, then settling.
      if (t.ticks === 1) t.item('R1', 'N1', 1, 'command_execution', 'completed', { title: 'Workspace ready' });
      if (t.ticks === 2) t.item('R1', 'N1', 2, 'dynamic_tool', 'completed');
      if (t.ticks === 3) t.item('R1', 'N1', 3, 'command_execution', 'running', { title: 'Bash' });
      if (t.ticks === 6) {
        t.itemStatus('R1', 3, 'failed');
        settledAt = t.sent.length;
      }
    },
  });
  const out = await turn({ op: 'turn', threadId: null, newThreadId: 'N1', text: '¿cuántas hay?', stopAfterTools: 2, timeoutMs: 2000, create: { projectId: 'P' } }, t3);
  assert.deepEqual(types(t3), ['thread.create', 'thread.turn.start', 'run.interrupt']);
  assert.equal(t3.sent[0].threadId, 'N1'); // the id the caller chose
  assert.equal(settledAt, 2); // nothing was interrupted before the second call settled
  assert.equal(t3.sent[2].runId, 'R1');
  assert.match(t3.sent[2].reason, /stop after 2 tool calls/);
  assert.equal(out.ok, true);
  assert.equal(out.stopped, true);
  assert.equal(out.status, 'interrupted');
  assert.equal(out.threadId, 'N1');
});

test('stopAfterTools: a run that ignores the interrupt past graceMs is { ok: false, error: interrupt not taken }', async () => {
  const t3 = fakeT3({
    onSend: (c, t) => c.type === 'thread.turn.start' && t.run('R1', 'T1', 'running', 2),
    onTick: t => t.ticks === 1 && t.item('R1', 'T1', 1, 'dynamic_tool', 'completed'),
  });
  t3.thread('T1');
  t3.run('R0', 'T1', 'completed', 1);
  const out = await turn({ op: 'turn', threadId: 'T1', text: 'x', stopAfterTools: 1, graceMs: 30, timeoutMs: 2000 }, t3);
  assert.deepEqual(types(t3), ['thread.turn.start', 'run.interrupt']);
  assert.deepEqual({ ok: out.ok, error: out.error, status: out.status }, { ok: false, error: 'interrupt not taken', status: 'running' });
});

test("op stop interrupts the chat's runs under way and says whether it is idle", async () => {
  const t3 = fakeT3({ onSend: (c, t) => c.type === 'run.interrupt' && t.status(c.runId, 'interrupted') });
  t3.thread('T1');
  t3.run('R1', 'T1', 'completed', 1);
  t3.run('R2', 'T1', 'running', 2);
  const out = await turn({ op: 'stop', threadId: 'T1', graceMs: 1000 }, t3);
  assert.deepEqual(out, { ok: true, stopped: 1, busy: false });
  assert.deepEqual(t3.sent.map(c => c.runId), ['R2']);
});

test('without stopAfterTools or steer a turn is unchanged: the run ends on its own, no interrupt, no extra fields', async () => {
  const t3 = fakeT3({
    onSend: (c, t) => c.type === 'thread.turn.start' && (t.run('R1', 'T1', 'running', 1), t.item('R1', 'T1', 1, 'dynamic_tool', 'completed')),
    onTick: t => t.ticks === 4 && (t.answer('R1', 'Siete.'), t.status('R1', 'completed')),
  });
  t3.thread('T1');
  const out = await turn({ op: 'turn', threadId: 'T1', text: '¿cuántas?' }, t3);
  assert.deepEqual(types(t3), ['thread.turn.start']);
  assert.deepEqual(Object.keys(out).toSorted(), ['ms', 'ok', 'status', 'text', 'threadId']);
  assert.equal(out.text, 'Siete.');
});

test('the sender in the app: the private map links a JID to an account, the live user record says admin or guest', async () => {
  const dir = mkdtempSync(join(tmpdir(), 't3-turn-people-'));
  try {
    const database = join(dir, 'database.sqlite');
    const app = new DatabaseSync(database);
    app.exec(`CREATE TABLE users(id TEXT PRIMARY KEY, username TEXT UNIQUE NOT NULL, display_name TEXT NOT NULL, role TEXT NOT NULL, active INTEGER NOT NULL DEFAULT 1);
      INSERT INTO users VALUES ('u1','ana','Ana Pérez','admin',1), ('u2','luis','Luis','editor',1), ('u3','old','Old','admin',0);`);
    const map = join(dir, 'people.json');
    writeFileSync(map, JSON.stringify({ '111@lid': 'ana', '222@s.whatsapp.net': 'luis', '333@lid': 'old', '444@lid': 'nobody', self: 'ana' }), { mode: 0o600 });
    const who = extra => ({ map, database, ...extra });
    assert.equal(senderRole(who({ jid: '111@lid' })), 'app account ana (Ana Pérez), admin');
    assert.equal(senderRole(who({ jid: '111:7@lid' })), 'app account ana (Ana Pérez), admin', 'a device suffix is the same person');
    assert.equal(senderRole(who({ fromMe: true, jid: '999@lid' })), 'app account ana (Ana Pérez), admin', 'the bridge\'s own number is "self"');
    assert.equal(senderRole(who({ jid: '222@s.whatsapp.net' })), 'app account luis (Luis), editor: guest');
    assert.equal(senderRole(who({ jid: '222@s.whatsapp.net' }), { userOf: () => ({ display_name: 'X), admin', role: 'editor', active: 1 }) }).endsWith('(X admin), editor: guest'), true);
    assert.equal(senderRole(who({ jid: '333@lid' })), 'app account old (not an active account): guest');
    assert.equal(senderRole(who({ jid: '444@lid' })), 'app account nobody (not an active account): guest');
    assert.equal(senderRole(who({ jid: '555@lid' })), 'no linked app account: guest');
    assert.equal(senderRole(who({ jid: 'constructor' })), 'no linked app account: guest');
    // Live: a change in the app's records shows at the next turn, with no copy to refresh.
    app.prepare("UPDATE users SET role = 'editor' WHERE username = 'ana'").run();
    assert.equal(senderRole(who({ jid: '111@lid' })), 'app account ana (Ana Pérez), editor: guest');
    // The empty map as shipped: everyone is a guest. A missing or broken map, or no database: guest too.
    writeFileSync(map, '{}');
    assert.equal(senderRole(who({ jid: '111@lid' })), 'no linked app account: guest');
    assert.equal(senderRole(who({ fromMe: true })), 'no linked app account: guest');
    assert.equal(senderRole({ jid: '111@lid', map: join(dir, 'missing.json'), database }), 'app roles unreadable: guest');
    writeFileSync(map, '{not json');
    assert.equal(senderRole(who({ jid: '111@lid' })), 'app roles unreadable: guest');
    writeFileSync(map, JSON.stringify({ '111@lid': 'ana' }));
    chmodSync(map, 0o600);
    assert.equal(senderRole({ jid: '111@lid', map, database: join(dir, 'none.sqlite') }), 'app account ana (records unreadable): guest');
    app.close();

    // The helper puts it in the header at turn time, in place of the bridge's token; 'whois' only reads.
    const t3 = fakeT3({
      onSend: (c, t) => {
        if (c.type === 'thread.turn.start') t.run('R1', 'T1', 'running', 1);
      },
      onTick: t => {
        if (t.ticks === 2) {
          t.answer('R1', 'ok');
          t.status('R1', 'completed');
        }
      },
    });
    t3.thread('T1');
    const token = '[[sender-role 0f]]';
    const people = w => `looked up ${w.jid}`;
    await turn({ op: 'turn', threadId: 'T1', text: `[WhatsApp · group «G» · Ana · ${token} · 10:00]\nMessage to answer:\n  Ana: hola`, who: { token, jid: '111@lid' }, timeoutMs: 5000 }, { ...t3, people });
    const start = t3.sent.find(c => c.type === 'thread.turn.start');
    assert.equal(start.message.text, '[WhatsApp · group «G» · Ana · looked up 111@lid · 10:00]\nMessage to answer:\n  Ana: hola');
    assert.deepEqual(await turn({ op: 'whois', who: { jid: '111@lid' } }, { db: null, send: null, people }), { ok: true, line: 'looked up 111@lid' });
    // Without `who` the text goes as it is.
    const t4 = fakeT3({ onSend: (c, t) => c.type === 'thread.turn.start' && (t.run('R1', 'T1', 'completed', 1), t.answer('R1', 'x')) });
    t4.thread('T1');
    await turn({ op: 'turn', threadId: 'T1', text: 'plain [[sender-role 0f]]', timeoutMs: 5000 }, t4);
    assert.equal(t4.sent[0].message.text, 'plain [[sender-role 0f]]');
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test('modelSelection: Antigravity / Gemini uses thought_level; Claude instances keep effort + fastMode', async () => {
  assert.deepEqual(modelSelection({ model: 'gemini-4-neon', effort: 'high' }, 'antigravity'), {
    instanceId: 'antigravity',
    model: 'gemini-4-neon',
    options: [{ id: 'thought_level', value: 'high' }],
  });
  assert.deepEqual(modelSelection({ model: 'gemini-4-neon', effort: 'xhigh' }, 'antigravity-eval'), {
    instanceId: 'antigravity-eval',
    model: 'gemini-4-neon',
    options: [{ id: 'thought_level', value: 'xhigh' }],
  });
  assert.deepEqual(modelSelection({ effort: 'medium' }, 'antigravity'), {
    instanceId: 'antigravity',
    model: 'claude-opus-5-5',
    options: [{ id: 'thought_level', value: 'medium' }],
  });
  assert.deepEqual(modelSelection({ model: 'claude-opus-5-5', effort: 'high' }, 'claudeAgent'), {
    instanceId: 'claudeAgent',
    model: 'claude-opus-5-5',
    options: [
      { id: 'effort', value: 'high' },
      { id: 'fastMode', value: false },
    ],
  });
  assert.deepEqual(modelSelection({ model: 'claude-opus-5-5', effort: 'medium' }, 'claude-eval-main'), {
    instanceId: 'claude-eval-main',
    model: 'claude-opus-5-5',
    options: [
      { id: 'effort', value: 'medium' },
      { id: 'fastMode', value: false },
    ],
  });

  // End-to-end via turn() creating a new Antigravity chat.
  const t3 = fakeT3({
    onSend: (c, t) => c.type === 'thread.turn.start' && (t.run('R1', c.threadId, 'completed', 1), t.answer('R1', 'ok')),
  });
  const out = await turn(
    { op: 'turn', threadId: null, newThreadId: 'AG1', text: 'hola', instanceId: 'antigravity', create: { projectId: 'P', model: 'gemini-4-neon', effort: 'high' } },
    t3,
  );
  assert.equal(out.ok, true);
  assert.deepEqual(t3.sent[0].modelSelection.options, [{ id: 'thought_level', value: 'high' }]);
  assert.deepEqual(t3.sent[1].modelSelection.options, [{ id: 'thought_level', value: 'high' }]);
});
