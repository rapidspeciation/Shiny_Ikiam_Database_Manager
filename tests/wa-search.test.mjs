// whatsapp_search (server/wa-search.mjs) over a copy made by tools/whatsapp/search-export.mjs from a fixture wacli
// store: only the two team groups go in, with no phone number or JID; the search finds words, dates, senders and
// links and shows the messages around a hit; only active team accounts may search (observers, inactive accounts and
// callers without a token are refused). Nothing here reaches WhatsApp or an AI.
import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, readFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { DatabaseSync } from 'node:sqlite';
import { exportRows, linksOf, maskName, maskPhones, peopleIndex, searchExport, writeSearchFile } from '../tools/whatsapp/search-export.mjs';
import { createWhatsAppSearch, dayOf, parseQuery } from '../server/wa-search.mjs';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { mcpTools } from '../server/assistant.mjs';
import { addToken, addUser, mcpClient, signIn } from './helpers/assistant.mjs';

// Made-up numbers and JIDs (none is a real person's).
const GROUP = '120363000000000001@g.us';
const IKIAM = '120363000000000002@g.us';
const OTHER = '120363000000000003@g.us';
const ANA = '593990000001@s.whatsapp.net';
const ANA_LID = '111111111111111@lid';
const LUIS = '593990000002@s.whatsapp.net';
const GUEST = '593990000003@s.whatsapp.net';
const SELF = '593990000009@s.whatsapp.net';
const DIGITS = ['593990000001', '111111111111111', '593990000002', '593990000003', '593990000009', '120363000000000001', '120363000000000002'];
const at = iso => Math.floor(Date.parse(iso) / 1000);

function fixtureStore(dir) {
  const db = new DatabaseSync(join(dir, 'wacli.db'));
  db.exec(`CREATE TABLE messages (rowid INTEGER PRIMARY KEY, chat_jid TEXT, chat_name TEXT, msg_id TEXT, sender_jid TEXT, sender_name TEXT, ts INTEGER,
      from_me INTEGER DEFAULT 0, text TEXT, display_text TEXT, media_type TEXT, media_caption TEXT, filename TEXT, reaction_to_id TEXT,
      revoked INTEGER DEFAULT 0, deleted_at TEXT, quoted_msg_id TEXT, quoted_sender_jid TEXT);
    CREATE TABLE contacts (jid TEXT, phone TEXT, push_name TEXT, full_name TEXT, first_name TEXT, business_name TEXT);
    CREATE TABLE contact_aliases (jid TEXT, alias TEXT, notes TEXT, updated_at TEXT);`);
  const put = db.prepare(
    'INSERT INTO messages (chat_jid,msg_id,sender_jid,sender_name,ts,from_me,text,display_text,media_type,media_caption,filename,reaction_to_id,revoked,quoted_msg_id,quoted_sender_jid) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)',
  );
  const m = (chat, id, sender, name, iso, text, more = {}) =>
    put.run(chat, id, sender, name, at(iso), more.fromMe ? 1 : 0, text, more.display ?? null, more.media ?? null, more.caption ?? null, more.file ?? null, more.reaction ?? null, more.revoked ? 1 : 0, more.quoted ?? null, more.quotedSender ?? null);
  m(GROUP, 'A1', ANA_LID, 'Ana Ikiam', '2026-10-01T20:43:00Z', 'Hola! @593990000002 tal vez sabes dónde está el contrato marco vigente en el drive?');
  m(GROUP, 'A2', LUIS, 'Luis', '2026-10-01T22:16:00Z', 'https://drive.google.com/file/d/FAKEFILEID000/view?usp=drivesdk');
  m(GROUP, 'A3', LUIS, 'Luis', '2026-10-01T22:17:00Z', null, { display: 'Reacted 👍 to …', reaction: 'A1' });
  m(GROUP, 'A4', SELF, '+593 99 000 0009', '2026-10-02T15:00:00Z', 'Gracias Luis, mi número nuevo es 0990000009 y el de la oficina +593 99 000 0008', { fromMe: true });
  m(GROUP, 'A5', GUEST, '+593 99 000 0003', '2026-10-03T15:00:00Z', 'Les paso la carta de colectores', { media: 'document', file: 'Carta MAE salida campo Jul2025.pdf', caption: 'Les paso la carta de colectores', display: 'Sent document' });
  m(GROUP, 'A6', SELF, 'Franz', '2026-10-03T15:01:00Z', 'Claude: la carta cita el contrato 2023-0286, ya vencido.', { fromMe: true, quoted: 'A5', quotedSender: GUEST });
  m(GROUP, 'A7', ANA, 'Ana Ikiam', '2026-10-04T15:00:00Z', 'este mensaje se borró', { revoked: true });
  m(IKIAM, 'B0', ANA, 'Ana Ikiam', '2026-11-01T14:00:00Z', null, { display: 'Sent image', media: 'image' });
  m(IKIAM, 'B1', ANA, 'Ana Ikiam', '2026-11-02T14:00:00Z', 'Agenda for Thursday: the permit (contrato marco) renewal');
  m(OTHER, 'C1', ANA, 'Ana Ikiam', '2026-10-01T10:00:00Z', 'contrato marco en otro chat que no se exporta');
  db.prepare('INSERT INTO contacts (jid, push_name, full_name) VALUES (?,?,?)').run(GUEST, '+593 99 000 0003', null);
  db.prepare('INSERT INTO contacts (jid, push_name, full_name) VALUES (?,?,?)').run(LUIS, 'Luis', 'Luis Pérez');
  db.prepare('INSERT INTO contact_aliases (jid, alias) VALUES (?,?)').run(ANA, ANA_LID);
  db.close();
  return join(dir, 'wacli.db');
}
const people = { [ANA]: 'ana', self: 'franz', [SELF]: 'franz' };
const chats = { underground: { jid: GROUP, label: 'Ithomiini 🦋 (underground)' }, ikiam: { jid: IKIAM, label: 'Ithomiini Ikiam' } };

function exported() {
  const dir = mkdtempSync(join(tmpdir(), 'wa-search-'));
  const db = new DatabaseSync(fixtureStore(dir), { readOnly: true });
  const rows = exportRows(db, { chats, people: peopleIndex(people), transcripts: {} });
  db.close();
  const file = join(dir, 'wa-search.sqlite');
  writeSearchFile(file, rows, { chats: { underground: chats.underground.label, ikiam: chats.ikiam.label }, syncedAt: Date.parse('2026-11-18T12:00:00Z') });
  return { dir, rows, file };
}

test('export: only the two groups, no reactions or deleted messages, no phone number or JID anywhere in the file', () => {
  const { rows, file } = exported();
  assert.deepEqual(rows.map(r => r.id), ['A1', 'A2', 'A4', 'A5', 'A6', 'B0', 'B1']);
  // A photo with no caption: its attachment, not wacli's «Sent image».
  assert.deepEqual([rows[5].text, rows[5].media], [null, 'image']);
  const raw = readFileSync(file).toString('latin1');
  for (const d of DIGITS) assert.ok(!raw.includes(d), `the file holds ${d}`);
  assert.ok(!raw.includes('0990000009') && !raw.includes('000 0008'), 'phone numbers in a text are masked');
  const byId = Object.fromEntries(rows.map(r => [r.id, r]));
  // The LID sender is the same app account as her phone JID (contact_aliases); a mention becomes a name.
  assert.equal(byId.A1.sender_user, 'ana');
  assert.match(byId.A1.text, /@Luis tal vez/, 'their own WhatsApp name, not the address book label');
  // Franz's own messages: his account, never the number WhatsApp shows as his name; the bridge's own answer is Claude's.
  assert.deepEqual([byId.A4.sender_user, byId.A4.sender_name], ['franz', 'Franz']);
  assert.deepEqual([byId.A6.sender_user, byId.A6.sender_name], [null, 'Claude (asistente)']);
  // An unlinked sender whose WhatsApp name is a number: no name rather than the number.
  assert.deepEqual([byId.A5.sender_user, byId.A5.sender_name], [null, null]);
  assert.deepEqual([byId.A5.media, byId.A5.filename], ['document', 'Carta MAE salida campo Jul2025.pdf']);
  assert.deepEqual(JSON.parse(byId.A2.links), ['https://drive.google.com/file/d/FAKEFILEID000/view?usp=drivesdk']);
});

test('export helpers: links, phone masking outside links, the people map', () => {
  assert.deepEqual(linksOf('mira https://a.org/x?y=1. y https://a.org/x?y=1'), ['https://a.org/x?y=1']);
  assert.equal(maskPhones('llama al +593 99 123 4567 o 0991234567; https://inat.org/obs/123456789 CAM072174'), 'llama al [número] o [número]; https://inat.org/obs/123456789 CAM072174');
  assert.equal(maskPhones('el 18/11/2026 a las 9:30, 30 marcas, -0.95123'), 'el 18/11/2026 a las 9:30, 30 marcas, -0.95123');
  // Numbers in WhatsApp links, contact cards, file names and mixed names (review round 1).
  assert.equal(maskPhones('https://wa.me/593991234567 y wa.me/593991234567'), 'https://wa.me/numero-oculto y wa.me/[número]');
  assert.equal(maskPhones('https://api.whatsapp.com/send?phone=593991234567&text=hola'), 'https://api.whatsapp.com/send?phone=numero-oculto&text=hola');
  assert.equal(maskPhones('TEL;waid=593991234567:+593 99 123 4567'), 'TEL;waid=[número]:[número]');
  assert.equal(maskPhones('Cel.0991234567 o (099) 123-4567'), 'Cel.[número] o [número]');
  assert.equal(maskPhones('099.123.4567'), '[número]');
  // Written with one dot or comma, a prefix or a split group: still numbers, not coordinates (review round 3).
  for (const t of ['099.1234567', '099,1234567', '+593.991234567', '593.991234567', '2.991234567', '(099) 123.4567', '099 123.4567', '099-123.4567', '+593 99 123.4567', '0991-23-4567'])
    assert.equal(maskPhones(t), '[número]', t);
  assert.equal(maskPhones('cuadros 120, 130, 140, 150'), 'cuadros 120, 130, 140, 150');
  // GPS points and date-times are not phone numbers (review round 2).
  assert.equal(maskPhones('lat -0.9512345 lon -77.8123456; -1.0123456,-77.81234567'), 'lat -0.9512345 lon -77.8123456; -1.0123456,-77.81234567');
  assert.equal(maskPhones('2026-10-01 10:00 y 2026-10-01T10:00:00'), '2026-10-01 10:00 y 2026-10-01T10:00:00');
  assert.equal(maskPhones('-0.95123, -77.81234 y Lat: 0.9512345'), '-0.95123, -77.81234 y Lat: 0.9512345');
  assert.equal(maskName('Carta_0991234567.pdf'), 'Carta_[número].pdf');
  assert.equal(maskName('Juan 0991234567'), 'Juan [número]');
  assert.equal(maskName('+593 99 123 4567'), null);
  assert.equal(maskName('IMG-20261001-WA0003.jpg'), 'IMG-20261001-WA0003.jpg');
  assert.deepEqual([...peopleIndex({ 'x:3@s.whatsapp.net': 'a', self: 'f', y: '' })], [['x@s.whatsapp.net', 'a'], ['self', 'f']]);
  assert.deepEqual(parseQuery('Contrato "carta MAE" a|b'), [['contrato'], ['carta mae'], ['a', 'b']]);
  assert.equal(dayOf('01/10/2026'), Date.parse('2026-10-01T05:00:00Z'));
  assert.equal(dayOf('2026-10-01', true), Date.parse('2026-10-02T05:00:00Z'));
  assert.equal(dayOf('mañana'), null);
  assert.equal(dayOf('31/02/2026'), null);
});

// Round 4 (both critics' inputs): every way of writing a number they found leaking, and what must stay as written.
const LEAKS = [
  '099.1234567', '099,1234567', '+593.991234567', '593.991234567', 'tel 2.991234567', '(099) 123.4567', '099 123.4567', '099-123.4567',
  '+593 99 123.4567', 'Cel. 099.1234567', '0991-23-4567', 'https://api.whatsapp.com/send?phone=%2B593991234567', 'https://wa.me/+593-99-123-4567',
  'https://wa.me/c/593991234567', 'https://t.me/+593991234567', 'https://example.com/c?tel=0991234567', 'cel: 0991234567/0987654321',
  'cel: 099 123 4567', '+593 99 123 4567', '099 123 4567', '099‑123‑4567', '099–123–4567',
  'teléfono: 2991234', 'Cel: 99.1234567', 'Llámame al 2 2123456', '120363000000000001@g.us', '593990000001@s.whatsapp.net',
  '0991234567,0987654321', '0991234567.0987654321', '099 123 4567 098 765 4321', 'Cel0991234567', 'juan_0991234567', '0991234567_',
  '#0991234567', 'tel=0991234567', '1234567890123456', '@593991234567hola', 'tel:+593991234567',
];
const KEPT = [
  'conteos 100 200 300', 'cuentas 120 340 560 780', 'cuadros 120, 130, 140, 150', 'IMG_20261001_123456.jpg', 'CAM000201-000250',
  'CAM000201, 000202', '000201 000202 000203', 'lat -0.9512345 lon -77.8123456', '-0,63831234 -76,14971234', '-0.6383, -76.1497',
  '2026-10-01 10:00', '18/11/2026 9:30', 'el 18-11-2026', 'pagué 1.500000', '$1,500.00', 'número de jaulas: 12', '9:30-11:00',
  'https://www.inaturalist.org/observations/123456789', 'MAATE-DBI-CM-2024-0443', 'CAM072174',
];

test('masking: no phone number or JID left in a text or a name, ordinary numbers kept (round 4)', () => {
  for (const t of LEAKS) {
    assert.doesNotMatch(maskPhones(t), /\d{3}/, `text: ${JSON.stringify(t)} → ${JSON.stringify(maskPhones(t))}`);
    assert.doesNotMatch(maskName(`Contacto ${t}.pdf`) ?? '', /\d{3}/, `name: ${JSON.stringify(t)}`);
  }
  for (const t of KEPT) assert.equal(maskPhones(t), t);
});

test('searchExport: the config chats, the people map, the file pushed only when it changed', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'wa-search-run-'));
  fixtureStore(dir);
  const { writeFileSync } = await import('node:fs');
  const map = join(dir, 'people.json');
  writeFileSync(map, JSON.stringify(people));
  const target = join(dir, 'server', 'wa-search.sqlite');
  const config = {
    wacli: { store: dir },
    chats: { group: { jid: GROUP, label: 'Ithomiini 🦋 (underground)', route: 'here', people: { map } }, ikiam: { jid: IKIAM, label: 'Ithomiini Ikiam' } },
    routes: { here: {} },
    search: { target: { route: 'here', path: target } },
  };
  const state = join(dir, 'state');
  const log = { log() {} };
  const first = await searchExport({ config, stateDir: state, push: true, log });
  assert.deepEqual([first.rows, first.pushed], [7, true]);
  assert.equal((await searchExport({ config, stateDir: state, push: true, log })).pushed, false, 'unchanged: not copied again');
  const search = createWhatsAppSearch({ path: target });
  assert.equal(search.run({ query: 'contrato marco' }, { role: 'editor' }).total, 2);
});

async function app(file) {
  const store = new Store({ localMode: true }, { sheets: new LocalSheets({}) });
  const assistant = createAssistant({ store, config: { waSearchPath: file } });
  const editor = signIn(store, assistant, { id: 'u1', username: 'ana', displayName: 'Ana Pérez', role: 'editor' });
  addUser(store, { id: 'u2', username: 'franz', displayName: 'Franz Chandi', role: 'admin' });
  addToken(store, 'u2', 'franz-token');
  const observer = signIn(store, assistant, { id: 'u3', username: 'visita', displayName: 'Visita', role: 'observer' });
  return { store, assistant, editor, observer };
}

test('whatsapp_search: words, the answer in the next message (around), senders by their app name, dates, links', async () => {
  const { file } = exported();
  const { store, editor } = await app(file);
  try {
    const found = await editor.call('whatsapp_search', { query: 'contrato marco' });
    assert.equal(found.total, 2, JSON.stringify(found));
    assert.deepEqual(found.messages.map(m => m.id), ['B1', 'A1'], 'newest first');
    assert.equal(found.messages[1].sender, 'Ana Pérez');
    assert.equal(found.messages[1].chat, 'Ithomiini 🦋 (underground)');
    assert.equal(found.messages[1].date, '01/10/2026 15:43');
    assert.equal(found.synced_at, '18/11/2026 07:00');
    // The bare Drive link is the next message.
    const around = await editor.call('whatsapp_search', { around: 'A1', limit: 2 });
    assert.deepEqual(around.messages.map(m => [m.id, !!m.match]), [['A1', true], ['A2', false], ['A4', false]]);
    assert.deepEqual(around.messages[1].links, ['https://drive.google.com/file/d/FAKEFILEID000/view?usp=drivesdk']);
    assert.equal(around.messages[1].sender, 'Luis');
    assert.equal(around.messages[2].sender, 'Franz Chandi');
    // Accents and case ignored; file names searched; a|b; the reply shown.
    const letter = await editor.call('whatsapp_search', { query: 'CARTA salida|colectores', has: 'document' });
    assert.deepEqual(letter.messages.map(m => [m.id, m.attachment.name, m.sender]), [['A5', 'Carta MAE salida campo Jul2025.pdf', '(sin nombre)']]);
    const reply = await editor.call('whatsapp_search', { query: '2023-0286' });
    assert.deepEqual([reply.messages[0].sender, reply.messages[0].reply_to.sender], ['Claude (asistente)', '(sin nombre)']);
    assert.equal((await editor.call('whatsapp_search', { query: 'dónde' })).total, 1);
    assert.equal((await editor.call('whatsapp_search', { query: 'contrato', chat: 'ikiam' })).total, 1);
    assert.equal((await editor.call('whatsapp_search', { query: 'contrato', from: '02/10/2026', to: '2026-11-30' })).total, 2);
    assert.equal((await editor.call('whatsapp_search', { sender: 'franz' })).total, 1);
    assert.equal((await editor.call('whatsapp_search', { has: 'link' })).total, 1);
    assert.equal((await editor.call('whatsapp_search', { query: 'otro chat' })).total, 0, 'other chats are not in the copy');
    assert.match((await editor.call('whatsapp_search', {})).error, /query/);
    assert.match((await editor.call('whatsapp_search', { from: 'ayer' })).error, /from/);
    // No phone number or JID in any answer.
    const all = JSON.stringify(await editor.call('whatsapp_search', { query: 'a|e|i|o|u|http', limit: 100 }));
    for (const d of DIGITS) assert.ok(!all.includes(d), `an answer holds ${d}`);
  } finally {
    store.close();
  }
});

test('whatsapp_search: a page cut to the answer size goes on from the last message shown', () => {
  const dir = mkdtempSync(join(tmpdir(), 'wa-search-big-'));
  const file = join(dir, 'wa-search.sqlite');
  const rows = Array.from({ length: 100 }, (_, i) => ({
    id: `M${i}`, chat: 'underground', at: Date.parse('2026-10-01T12:00:00Z') + i * 60_000, sender_user: null, sender_name: 'Ana',
    text: `contrato ${i} ` + 'x'.repeat(1500), media: null, filename: null, links: '[]', transcript: null, reply_to: null, reply_user: null, reply_name: null, reply_text: null,
  }));
  writeSearchFile(file, rows, { chats: { underground: 'U' } });
  const search = createWhatsAppSearch({ path: file });
  const first = search.run({ query: 'contrato', limit: 100, order: 'oldest' }, { role: 'editor' });
  assert.ok(first.messages.length < 100 && first.messages.length > 5, String(first.messages.length));
  assert.equal(first.next_offset, first.messages.length);
  assert.ok(JSON.stringify(first).length < 38000);
  const next = search.run({ query: 'contrato', limit: 100, order: 'oldest', offset: first.next_offset }, { role: 'editor' });
  assert.equal(next.messages[0].id, `M${first.messages.length}`);
  // A page that fits says where the next one starts too (review round 2).
  const small = search.run({ query: 'contrato', limit: 3, order: 'oldest' }, { role: 'editor' });
  assert.deepEqual([small.total, small.messages.length, small.next_offset], [100, 3, 3]);
  assert.equal(search.run({ query: 'contrato', limit: 3, offset: 97 }, { role: 'editor' }).next_offset, undefined);
  // A window around a message too large for one answer narrows on both sides and keeps the message.
  const around = search.run({ around: 'M50', limit: 100 }, { role: 'editor' });
  assert.ok(around.truncated && around.messages.some(m => m.id === 'M50' && m.match), JSON.stringify(around).slice(0, 200));
  const at = around.messages.findIndex(m => m.match);
  assert.equal(at, around.messages.length - 1 - at, 'as many before as after');
});

test('whatsapp_search: team accounts only; observers, inactive accounts and callers without a token are refused', async () => {
  const { file } = exported();
  const { store, assistant, editor, observer } = await app(file);
  try {
    const refused = await observer.call('whatsapp_search', { query: 'contrato' });
    assert.match(refused.error, /team accounts only/);
    assert.doesNotMatch(JSON.stringify(refused), /contrato marco vigente/);
    const admin = mcpClient(assistant, 'franz-token');
    assert.equal((await admin.call('whatsapp_search', { query: 'contrato marco' })).total, 2);
    // Lowered to observer mid-chat: refused at once.
    store.db.prepare("UPDATE users SET role = 'observer' WHERE id = 'u1'").run();
    assert.match((await editor.call('whatsapp_search', { query: 'contrato' })).error, /team accounts only/);
    store.db.prepare("UPDATE users SET role = 'editor', active = 0 WHERE id = 'u1'").run();
    assert.equal((await editor.mcp('tools/call', { name: 'whatsapp_search', arguments: { query: 'contrato' } })).status, 401);
    // No token, or a wrong one: 401, nothing of the groups.
    for (const headers of [{}, { authorization: 'Bearer nope' }]) {
      const out = await assistant.mcp(headers, { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name: 'whatsapp_search', arguments: { query: 'contrato' } } });
      assert.equal(out.status, 401);
      assert.doesNotMatch(JSON.stringify(out.body), /contrato marco/);
    }
    // Without the copy on the server: says so.
    const empty = createWhatsAppSearch({ path: join(tmpdir(), 'no-such-wa-search.sqlite') });
    assert.match(empty.run({ query: 'x' }, { role: 'editor' }).error, /not searchable yet/);
  } finally {
    store.close();
  }
});

test('whatsapp_search is listed for T3 chats, deferred (not always loaded)', () => {
  const tool = mcpTools().find(t => t.name === 'whatsapp_search');
  assert.ok(tool);
  assert.equal(tool._meta, undefined);
  for (const d of DIGITS) assert.ok(!tool.description.includes(d));
});
