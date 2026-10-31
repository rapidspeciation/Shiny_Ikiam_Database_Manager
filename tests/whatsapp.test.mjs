import test from 'node:test';
import assert from 'node:assert/strict';
import { DatabaseSync } from 'node:sqlite';
import { createHash } from 'node:crypto';
import { camMismatches, createWhatsAppRelay, groupWake, nameCall, normalizeEvent, protocolQuestion, quietHours, sign } from '../server/whatsapp.mjs';

const SECRET = 'test-secret-0123456789';
const GROUP = '120363000000000001@g.us';
const ANA = '593991111111@s.whatsapp.net';
const LUIS = '593992222222@s.whatsapp.net';
const STRANGER = '593999999999@s.whatsapp.net';
const BOT = '593980000000@s.whatsapp.net';
// 10:00 in Ecuador (15:00 UTC): outside the quiet hours.
const MORNING = Date.parse('2026-10-30T15:00:00Z');

function fixture({ mode = 'mention-only', options = {}, lookupCam = () => null, genera = () => new Set() } = {}) {
  const db = new DatabaseSync(':memory:');
  db.exec(`CREATE TABLE users(id TEXT PRIMARY KEY, username TEXT UNIQUE NOT NULL, display_name TEXT NOT NULL, role TEXT NOT NULL, salt TEXT NOT NULL, password_hash TEXT NOT NULL, active INTEGER NOT NULL DEFAULT 1, created_at TEXT NOT NULL);
    CREATE TABLE ai_tokens(token_hash TEXT PRIMARY KEY, user_id TEXT NOT NULL, label TEXT NOT NULL, created_at TEXT NOT NULL, revoked_at TEXT);`);
  for (const [id, name] of [['u1', 'whatsapp'], ['u2', 'lab']]) {
    db.prepare("INSERT INTO users VALUES (?,?,?,'editor','s','h',1,'2026-10-30')").run(id, name, name);
    db.prepare("INSERT INTO ai_tokens VALUES (?,?,'t3','2026-10-30',NULL)").run(createHash('sha256').update(`token-${name}`).digest('hex'), id);
  }
  const clock = { t: MORNING };
  const woken = [];
  const t3 = { busy: false };
  const relay = createWhatsAppRelay({
    db,
    secret: SECRET,
    botJids: [BOT],
    wake: async text => {
      woken.push(text);
      return { sent: true, threadId: 'thread-1' };
    },
    busy: () => t3.busy,
    lookupCam,
    genera,
    options,
    now: () => clock.t,
    log: {},
  });
  relay.setPerson({ phone: '+593991111111', name: 'Ana', role: 'admin' });
  relay.setPerson({ phone: '+593992222222', name: 'Luis' });
  relay.setGroup({ jid: GROUP, name: 'Insectario Ikiam', enabled: true, mode });
  let k = 0;
  const post = (fields, { signature } = {}) => {
    const raw = Buffer.from(JSON.stringify({ msg_id: `m-${++k}`, timestamp: Math.floor(clock.t / 1000), text: '', ...fields }));
    return relay.receive({ 'x-wacli-signature': signature ?? sign(SECRET, raw) }, raw);
  };
  const decision = out => out.body.results[0].decision;
  return { db, relay, clock, woken, t3, post, decision };
}

test('a body without the right signature is refused before anything is read', () => {
  const { post, db } = fixture();
  assert.equal(post({ chat_jid: ANA, sender_jid: ANA, text: 'hola' }, { signature: 'sha256=00' }).status, 403);
  assert.equal(post({ chat_jid: ANA, sender_jid: ANA, text: 'hola' }, { signature: sign('other-secret-0000000', 'x') }).status, 403);
  assert.equal(db.prepare('SELECT count(*) n FROM wa_inbox').get().n, 0);
});

test('a message delivered twice is handled once', () => {
  const { post, decision } = fixture();
  assert.equal(decision(post({ msg_id: 'same', chat_jid: ANA, sender_jid: ANA, text: '¿cuántas?' })), 'waiting');
  assert.equal(decision(post({ msg_id: 'same', chat_jid: ANA, sender_jid: ANA, text: '¿cuántas?' })), 'duplicate');
});

test('strangers are dropped silently and their text is not kept', async () => {
  const { post, decision, db, relay, clock, woken } = fixture();
  assert.equal(decision(post({ chat_jid: STRANGER, sender_jid: STRANGER, text: 'hola, quién eres?' })), 'dropped');
  const row = db.prepare('SELECT * FROM wa_inbox').get();
  assert.equal(row.text, null);
  assert.equal(row.reason, 'not on list');
  clock.t += 120_000;
  await relay.pump();
  assert.equal(woken.length, 0);
  assert.equal(db.prepare('SELECT count(*) n FROM wa_outbox').get().n, 0);
});

test('a disabled person and an unknown group are dropped; the group waits as pending', () => {
  const { post, decision, relay } = fixture();
  relay.setPerson({ phone: '+593992222222', name: 'Luis', enabled: false });
  assert.equal(decision(post({ chat_jid: LUIS, sender_jid: LUIS, text: 'hola' })), 'dropped');
  const other = '120363000000000099@g.us';
  assert.equal(decision(post({ chat_jid: other, sender_jid: ANA, text: '@asistente hola' })), 'dropped');
  assert.equal(relay.groups().find(g => g.jid === other).enabled, 0);
});

test('group messages wake only on a mention, a reply to the bot, its name or a command', () => {
  const { post, decision } = fixture();
  assert.equal(decision(post({ chat_jid: GROUP, sender_jid: LUIS, text: 'buenos días, ya salimos a T2' })), 'context');
  assert.equal(decision(post({ chat_jid: GROUP, sender_jid: LUIS, text: 'hey', mentions: [BOT] })), 'waiting');
  assert.equal(decision(post({ chat_jid: GROUP, sender_jid: LUIS, text: 'ok gracias', quoted_sender: BOT })), 'waiting');
  assert.equal(decision(post({ chat_jid: GROUP, sender_jid: LUIS, text: 'asistente, cuántas hay?' })), 'waiting');
  assert.equal(decision(post({ chat_jid: GROUP, sender_jid: LUIS, text: '/cam CAM026514' })), 'waiting');
  // Someone in the group but not on the list: kept as context, never a wake.
  assert.equal(decision(post({ chat_jid: GROUP, sender_jid: STRANGER, text: '@asistente hola', mentions: [BOT] })), 'context');
});

test('the wake rules, alone', () => {
  const msg = over => ({ text: '', mentions: [], quotedId: null, quotedSender: null, ...over });
  const bot = { jids: [BOT], name: /\basistente\b/i, outboxIds: id => id === 'out:7' };
  assert.equal(groupWake(msg({ text: 'mira @593980000000' }), bot), 'mention');
  assert.equal(groupWake(msg({ quotedId: 'out:7' }), bot), 'reply');
  assert.equal(groupWake(msg({ text: 'Asistente: ayuda' }), bot), 'name');
  assert.equal(groupWake(msg({ text: 'nada que ver' }), bot), null);
  const name = /\b(ithobot|asistente|assistant)\b/i;
  for (const t of ['@asistente el CAM026514 es Melinaea?', 'asistente, cuántas hay?', 'oye asistente ¿cuántas hay?', '¿cuántas hay, asistente?', 'gracias @ithobot', '@asistente Lo pregunto para mañana'])
    assert.ok(nameCall(t, name), t);
  for (const t of ['@Asistente de campo A ¿cuáles son tus iniciales?', 'Asistente de campo A trajo las mariposas', 'la asistente nueva llega mañana', 'my research assistant will check'])
    assert.ok(!nameCall(t, name), t);
  assert.ok(protocolQuestion('¿Marcamos o preservamos las Mechanitis de T2?'));
  assert.ok(!protocolQuestion('¿vamos a almorzar?'));
  assert.ok(!protocolQuestion('Marcamos 7 en T2.'));
  assert.ok(protocolQuestion('Oigan, si llueve, ¿se pospone o se cancela el monitoreo?'));
  assert.ok(!protocolQuestion('@Ana ¿marcamos en el ala anterior o en el abdomen?'));
});

test('a call by name brings the same chat\'s waiting question into its envelope', async () => {
  const { post, decision, clock, relay, woken } = fixture({ mode: 'helpful' });
  assert.equal(decision(post({ chat_jid: GROUP, sender_jid: LUIS, text: '¿Qué especies ya tienen las 30 preservadas y hay que marcar?' })), 'waiting');
  clock.t += 60_000;
  assert.equal(decision(post({ chat_jid: GROUP, sender_jid: LUIS, text: '@asistente lo pregunto para la salida de mañana' })), 'waiting');
  clock.t += 25_000;
  await relay.pump();
  assert.equal(woken.length, 1);
  assert.match(woken[0], /\[WA\] 2 new\n#m1 .*30 preservadas.*\n#m2 .*\(goes on from #m1\)/);
});

test('a CAM named with another genus or sex than the database is a mismatch', () => {
  const lookup = cam => (cam === 'CAM026514' ? { genus: 'Mechanitis', species: 'Mechanitis messenoides', sex: 'female' } : null);
  const genera = new Set(['Mechanitis', 'Melinaea']);
  assert.deepEqual(camMismatches('el CAM026514 es Melinaea', lookup, genera), [{ cam: 'CAM026514', said: 'Melinaea', database: 'Mechanitis Mechanitis messenoides' }]);
  assert.deepEqual(camMismatches('CAM026514 Mechanitis hembra', lookup, genera), []);
  assert.equal(camMismatches('CAM026514 es macho', lookup, genera)[0].said, 'male');
  assert.equal(camMismatches('CAM000001 listo', lookup, genera)[0].database, 'no such CAM');
});

test('helpful groups: a CAM mismatch wakes; an unanswered protocol question waits 20 min for a teammate', () => {
  const lookupCam = cam => (cam === 'CAM026514' ? { genus: 'Mechanitis', species: 'Mechanitis messenoides', sex: 'female' } : null);
  const { post, decision, db, clock, relay } = fixture({ mode: 'helpful', lookupCam, genera: () => new Set(['Mechanitis', 'Melinaea']) });
  assert.equal(decision(post({ chat_jid: GROUP, sender_jid: LUIS, text: 'el CAM026514 es Melinaea' })), 'waiting');
  assert.equal(decision(post({ chat_jid: GROUP, sender_jid: LUIS, text: 'CAM026514 Mechanitis, listo' })), 'context');
  const q = post({ msg_id: 'q1', chat_jid: GROUP, sender_jid: LUIS, text: '¿Después de 30 preservadas marcamos el resto?' });
  assert.equal(decision(q), 'waiting');
  assert.equal(db.prepare("SELECT ready_at FROM wa_inbox WHERE msg_id = 'q1'").get().ready_at, clock.t + 20 * 60_000);
  // Ana answers in the group: the question needs nobody else.
  post({ chat_jid: GROUP, sender_jid: ANA, text: 'Sí, se marcan las demás' });
  assert.equal(relay.message('q1').status, 'ignored');
});

test('unprompted group replies: none in the quiet hours, at most the daily cap', async () => {
  const lookupCam = () => null;
  const { post, decision, clock, relay, db } = fixture({ mode: 'helpful', lookupCam });
  clock.t = Date.parse('2026-10-31T03:00:00Z'); // 22:00 in Ecuador
  assert.ok(quietHours(clock.t));
  assert.equal(decision(post({ chat_jid: GROUP, sender_jid: LUIS, text: 'CAM000001 listo' })), 'context');
  clock.t = MORNING;
  for (let i = 0; i < 3; i++) {
    assert.equal(decision(post({ chat_jid: GROUP, sender_jid: LUIS, text: `CAM00000${i + 2} listo` })), 'waiting');
    db.prepare("UPDATE wa_inbox SET status = 'answered' WHERE status = 'waiting'").run();
  }
  assert.equal(decision(post({ chat_jid: GROUP, sender_jid: LUIS, text: 'CAM000009 listo' })), 'context');
  assert.equal(relay.message('m-5').reason, 'group daily cap');
});

test('per-number limit: past 20 an hour, one notice, then silence', () => {
  const { post, decision, db, clock } = fixture();
  for (let i = 0; i < 20; i++) {
    assert.equal(decision(post({ chat_jid: LUIS, sender_jid: LUIS, text: `pregunta ${i}` })), 'waiting');
    clock.t += 1000;
  }
  assert.equal(decision(post({ chat_jid: LUIS, sender_jid: LUIS, text: 'otra' })), 'limited');
  assert.equal(decision(post({ chat_jid: LUIS, sender_jid: LUIS, text: 'y otra' })), 'limited');
  assert.equal(db.prepare("SELECT count(*) n FROM wa_outbox WHERE by = 'relay'").get().n, 1);
  // Another person is not held back.
  assert.equal(decision(post({ chat_jid: ANA, sender_jid: ANA, text: 'hola' })), 'waiting');
  clock.t += 61 * 60_000;
  assert.equal(decision(post({ chat_jid: LUIS, sender_jid: LUIS, text: 'ya' })), 'waiting');
});

test('a burst goes as one turn after 20 s of quiet (60 s at most), and one turn at a time', async () => {
  const { post, relay, clock, woken, t3 } = fixture();
  post({ chat_jid: ANA, sender_jid: ANA, text: 'Hola' });
  clock.t += 10_000;
  post({ chat_jid: ANA, sender_jid: ANA, text: '¿cuántas Mechanitis marcamos ayer en T2?' });
  clock.t += 15_000;
  await relay.pump();
  assert.equal(woken.length, 0, 'still inside the 20 s after the last message');
  clock.t += 6_000;
  await relay.pump();
  assert.equal(woken.length, 1);
  assert.match(woken[0], /^\[WA\] 2 new\n#m1 DM Ana \(\+593991111111\) 10:00: Hola\n#m2 DM/);
  // A person's next line is marked as going on from their last one.
  assert.match(woken[0], /#m2 DM Ana \(\+593991111111\) 10:00 \(goes on from #m1\): ¿cuántas/);
  // The chat is answering: a new message waits for it.
  t3.busy = true;
  await relay.pump();
  post({ chat_jid: LUIS, sender_jid: LUIS, text: 'hola' });
  clock.t += 25_000;
  await relay.pump();
  assert.equal(woken.length, 1);
  t3.busy = false;
  await relay.pump();
  assert.equal(woken.length, 2);
  // The first turn ended without an answer to Ana's messages.
  assert.equal(relay.message('m-1').status, 'unanswered');

  // A steady stream still goes within 60 s of its first message.
  t3.busy = true;
  await relay.pump();
  t3.busy = false;
  for (let i = 0; i < 7; i++) {
    post({ chat_jid: ANA, sender_jid: ANA, text: `línea ${i}` });
    clock.t += 10_000;
    await relay.pump();
  }
  assert.equal(woken.length, 3);
  assert.match(woken[2], /\[WA\] [67] new/);
});

test('whatsapp_reply writes to the outbox, warns for a chat outside the turn, and only the WhatsApp token has it', async () => {
  const { post, relay, clock, db } = fixture();
  post({ chat_jid: ANA, sender_jid: ANA, text: '¿Cuántas marcamos?' });
  post({ chat_jid: LUIS, sender_jid: LUIS, text: 'hola' });
  db.prepare("UPDATE wa_inbox SET status = 'answered' WHERE chat = ?").run(LUIS);
  clock.t += 30_000;
  await relay.pump();
  const call = (token, name, args) =>
    relay.mcp({ authorization: `Bearer ${token}` }, { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } }, async () => ({ status: 200, body: { jsonrpc: '2.0', id: 1, result: { content: [{ type: 'text', text: '{"next":true}' }] } } }));
  const text = out => JSON.parse(out.body.result.content[0].text);

  assert.equal(text(await call('token-lab', 'whatsapp_reply', { replyTo: '#m1', text: 'Siete.' })).error, 'Only the WhatsApp chat sends WhatsApp answers.');
  assert.deepEqual(text(await call('token-whatsapp', 'whatsapp_reply', { replyTo: '#m1', text: 'Ayer en T2: 7 marcadas.' })), { sent: true, outbox: 1, via: 'outbox (test: nothing goes to WhatsApp)' });
  assert.equal(relay.message('m-1').status, 'answered');
  // A second answer to the same message: a warning first.
  assert.match(text(await call('token-whatsapp', 'whatsapp_reply', { replyTo: '#m1', text: 'Corrección: 8.' })).warning, /already has an answer/);
  // Luis's chat had nothing in this turn: a warning, then sent when confirmed.
  assert.match(text(await call('token-whatsapp', 'whatsapp_reply', { replyTo: '#m2', text: 'Hola Luis' })).warning, /no message in this turn/);
  assert.equal(text(await call('token-whatsapp', 'whatsapp_reply', { replyTo: '#m2', text: 'Hola Luis', confirm: true })).sent, true);
  assert.equal(text(await call('token-whatsapp', 'whatsapp_reply', { replyTo: '#m1', text: 'Corrección: 8.', confirm: true })).sent, true);
  // No applying from WhatsApp; other tools pass through.
  assert.match(text(await call('token-whatsapp', 'apply_proposal', { proposalId: 'x' })).error, /not available from WhatsApp/);
  assert.deepEqual(text(await call('token-whatsapp', 'find_records', {})), { next: true });

  const list = token =>
    relay.mcp({ authorization: `Bearer ${token}` }, { method: 'tools/list', id: 2 }, async () => ({ status: 200, body: { result: { tools: [{ name: 'apply_proposal' }, { name: 'find_records' }] } } }));
  assert.deepEqual((await list('token-whatsapp')).body.result.tools.map(t => t.name), ['find_records', 'whatsapp_reply']);
  assert.deepEqual((await list('token-lab')).body.result.tools.map(t => t.name), ['apply_proposal', 'find_records']);
});

test('admins add and remove people by DM, without the model', async () => {
  const { post, decision, relay, woken, clock } = fixture();
  assert.equal(decision(post({ chat_jid: ANA, sender_jid: ANA, text: '/add +593 993 333 333 Carla' })), 'handled');
  assert.equal(relay.people().find(p => p.phone === '+593993333333').name, 'Carla');
  // Not an admin: an ordinary message for the chat.
  assert.equal(decision(post({ chat_jid: LUIS, sender_jid: LUIS, text: '/add +593994444444 X' })), 'waiting');
  clock.t += 30_000;
  await relay.pump();
  assert.equal(woken.length, 1);
  assert.doesNotMatch(woken[0], /Carla/);
});

test('wacli bodies in other shapes read the same', () => {
  const m = normalizeEvent({ event: 'message', message: { ID: 'X1', Chat: GROUP, Sender: LUIS, PushName: 'Luis', Text: 'hola', Timestamp: '2026-10-30T15:00:00Z', media_type: 'image', local_path: '/tmp/a.jpg' } });
  assert.equal(m.id, 'X1');
  assert.equal(m.isGroup, true);
  assert.equal(m.at, MORNING);
  assert.deepEqual(m.media, { type: 'image', path: '/tmp/a.jpg', mime: null, bytes: null, name: null });
  assert.equal(normalizeEvent({ event: 'receipt', chat: ANA, id: 'r' }), null);
});
