import { test } from 'node:test';
import assert from 'node:assert/strict';
import { createCipheriv, createHmac, hkdfSync, randomBytes } from 'node:crypto';
import { mkdtempSync, mkdirSync, readFileSync, writeFileSync, existsSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { command, decryptMedia, imageKind, ownAnswer, parsePayload, parseReply, sign, speakable, tagged, toWhatsApp, verify } from '../tools/whatsapp/bridge-lib.mjs';
import { createServer } from 'node:http';
import { createBridge } from '../tools/whatsapp/bridge.mjs';

test('@claude wakes only as a whole word', () => {
  assert.ok(tagged('@claude cuántos hay?'));
  assert.ok(tagged('oye @Claude, mira esto'));
  assert.ok(tagged('mira esto @CLAUDE.'));
  assert.ok(!tagged('claude cuántos hay?'));
  assert.ok(!tagged('@claudette hola'));
  assert.ok(!tagged('escribe a franz@claude.ai'));
});

test('the loop guard: the bridge\'s own answers', () => {
  assert.ok(ownAnswer('Claude: buscando…'));
  assert.ok(ownAnswer('  claude : hola @claude'));
  assert.ok(!ownAnswer('@claude: hola'));
});

test('commands', () => {
  assert.deepEqual(command('/status'), { name: 'status' });
  assert.deepEqual(command('@claude /whoami'), { name: 'whoami' });
  assert.deepEqual(command('@claude /foo'), { name: 'unknown', word: 'foo' });
  assert.equal(command('/foo'), null);
  assert.equal(command('hola /status'), null);
});

test('signature', () => {
  const raw = Buffer.from('{"a":1}');
  assert.ok(verify('s'.repeat(32), raw, sign('s'.repeat(32), raw)));
  assert.ok(!verify('s'.repeat(32), raw, sign('t'.repeat(32), raw)));
  assert.ok(!verify('s'.repeat(32), raw, undefined));
});

test('wacli payloads', () => {
  const m = parsePayload({ Chat: '1@g.us', ID: 'X', SenderJID: '2@s.whatsapp.net', Timestamp: '2026-10-31T10:00:00Z', FromMe: false, Text: '', Media: { Type: 'image', Caption: '@claude ¿qué es?' }, PushName: 'Ana' });
  assert.equal(m.text, '@claude ¿qué es?');
  assert.equal(m.media.type, 'image');
  assert.equal(m.at, Date.parse('2026-10-31T10:00:00Z'));
  assert.equal(parsePayload({ EventType: 'receipt', Chat: '1@g.us', ID: 'X' }), null);
  assert.equal(parsePayload({ Chat: '1@g.us', ID: 'X', ReactionToID: 'Y' }).skip, 'reaction');
});

test('reply blocks', () => {
  const r = parseReply('Aquí está.\n[images]\n/tmp/a.jpg | *Mechanitis* — Tena, 2024 — CC BY\nhttps://x.org/b.png\n[/images]\n[voice]');
  assert.equal(r.text, 'Aquí está.');
  assert.deepEqual(r.images, [
    { src: '/tmp/a.jpg', caption: '*Mechanitis* — Tena, 2024 — CC BY' },
    { src: 'https://x.org/b.png', caption: '' },
  ]);
  assert.ok(r.voice);
  assert.ok(parseReply('[silent]').silent);
  assert.equal(parseReply('[images]\n' + Array.from({ length: 8 }, (_, i) => `/tmp/${i}.jpg`).join('\n') + '\n[/images]').images.length, 5);
  assert.equal(toWhatsApp('**Sí**: [mapa](https://m.org)'), '*Sí*: mapa: https://m.org');
  assert.equal(speakable('*Hola* ver https://x.org ya'), 'Hola ver ya');
});

test('WhatsApp media decryption', () => {
  const key = randomBytes(32);
  const plain = Buffer.from('a photo of a butterfly');
  const keys = Buffer.from(hkdfSync('sha256', key, Buffer.alloc(32), 'WhatsApp Image Keys', 112));
  const iv = keys.subarray(0, 16);
  const c = createCipheriv('aes-256-cbc', keys.subarray(16, 48), iv);
  const data = Buffer.concat([c.update(plain), c.final()]);
  const mac = createHmac('sha256', keys.subarray(48, 80)).update(iv).update(data).digest().subarray(0, 10);
  assert.deepEqual(decryptMedia(Buffer.concat([data, mac]), key, 'image'), plain);
  assert.throws(() => decryptMedia(Buffer.concat([data, Buffer.alloc(10)]), key, 'image'));
  assert.equal(imageKind(Buffer.from([0xff, 0xd8, 0xff, 0xe0, 0, 0, 0, 0, 0, 0, 0, 0])), 'jpg');
  assert.equal(imageKind(Buffer.from('#!/bin/sh echo secret')), null);
});

function setup() {
  const dir = mkdtempSync(join(tmpdir(), 'wa-bridge-test-'));
  // A stand-in for t3-turn.mjs: answers with what the chat would.
  const script = join(dir, 'turn.mjs');
  writeFileSync(script, `let s='';for await (const c of process.stdin) s+=c; const r=JSON.parse(s); console.log(JSON.stringify({ok:true,threadId:'T1',status:'completed',text: r.text.split('Message to answer:')[1].includes('silencio') ? '[silent]' : 'Hay 12.'}));`);
  const config = {
    chats: {
      group: { jid: 'G@g.us', label: 'Grupo', route: 'r', enabled: true, dryRun: true, repliesHour: 2, contextLines: 15 },
      self: { jid: 'ME@s.whatsapp.net', label: 'yo', route: 'r', enabled: false, repliesHour: 60, contextLines: 0 },
    },
    routes: { r: { script, attachmentsDir: join(dir, 'att') } },
    wacli: { bin: '/bin/false', store: dir },
  };
  const bridge = createBridge({ config, stateDir: join(dir, 'state'), log: { error() {}, info() {} } });
  const outbox = () => (existsSync(join(dir, 'state', 'outbox.jsonl')) ? readFileSync(join(dir, 'state', 'outbox.jsonl'), 'utf8').trim().split('\n').map(l => JSON.parse(l)) : []);
  const log = () => readFileSync(join(dir, 'state', 'bridge.log.jsonl'), 'utf8');
  let n = 0;
  const post = (chat, text, extra = {}) => bridge.handle({ Chat: chat, ID: `M${++n}`, SenderJID: 'P@s.whatsapp.net', Timestamp: new Date().toISOString(), FromMe: false, Text: text, PushName: 'Ana', ...extra });
  const settle = async () => {
    for (let i = 0; i < 100 && !bridge.idle(); i++) await new Promise(r => setTimeout(r, 50));
    await new Promise(r => setTimeout(r, 100));
  };
  return { bridge, outbox, log, post, settle };
}

test('other chats are dropped before anything, with nothing of them kept', () => {
  const { post, log, outbox } = setup();
  assert.equal(post('STRANGER@s.whatsapp.net', '@claude secreto 123').decision, 'drop');
  assert.equal(post('ME@s.whatsapp.net', '@claude disabled chat').decision, 'drop');
  assert.ok(!log().includes('secreto') && !log().includes('STRANGER'));
  assert.deepEqual(outbox(), []);
});

test('the group: untagged, loop guard, tagged, limit', async () => {
  const { post, outbox, settle } = setup();
  assert.equal(post('G@g.us', 'hola a todos').reason, 'no @claude');
  assert.equal(post('G@g.us', 'Claude: algo @claude', { FromMe: true }).reason, 'loop guard');
  assert.equal(post('G@g.us', '@claude cuántos hay?').decision, 'queued');
  await settle();
  const sent = outbox();
  assert.deepEqual(sent.map(s => s.kind), ['react', 'reply', 'react']);
  assert.deepEqual(sent.map(s => s.react?.emoji ?? null), ['👀', null, '✅']);
  assert.equal(sent[1].text, 'Claude: Hay 12.');
  post('G@g.us', '@claude y ahora en silencio');
  await settle();
  assert.deepEqual(outbox().slice(3).map(s => s.react?.emoji), ['👀', '✅']); // no answer
  post('G@g.us', '@claude otra');
  await settle();
  assert.equal(post('G@g.us', '@claude y otra más').decision, 'limited');
});

test('the edit mode: one message that becomes the answer', async () => {
  const { post, outbox, settle } = setup();
  post('G@g.us', '@claude /ack edit', { FromMe: true });
  await settle();
  post('G@g.us', '@claude cuántos hay?');
  await settle();
  const sent = outbox().slice(1);
  assert.deepEqual(sent.map(s => s.kind), ['status', 'reply']);
  assert.equal(sent[0].text, 'Claude: pensando…');
  assert.equal(sent[1].edit, sent[0].id);
  assert.equal(sent[1].text, 'Claude: Hay 12.');
});

test('/stop and /start only from Franz; everything else waits while stopped', async () => {
  const { post, outbox, settle, bridge } = setup();
  post('G@g.us', '@claude /stop');
  await settle();
  assert.equal(bridge.state().stopped, false);
  assert.match(outbox().at(-1).text, /Solo Franz/);
  post('G@g.us', '@claude /stop', { FromMe: true });
  await settle();
  assert.equal(bridge.state().stopped, true);
  assert.equal(post('G@g.us', '@claude hola').reason, 'stopped');
  post('G@g.us', '@claude /start', { FromMe: true });
  await settle();
  assert.equal(bridge.state().stopped, false);
});

test('step lines name tools, not reasoning', async () => {
  const { stepLabel, statusMessage } = await import('../tools/whatsapp/bridge-lib.mjs');
  assert.equal(stepLabel({ tool: 'mcp__db__query' }), 'consultando la base');
  assert.equal(stepLabel({ tool: 'mcp__db__identify_species' }), 'identificando la especie');
  assert.equal(stepLabel({ tool: 'mcp__db__new_tool' }), 'usando new tool');
  assert.equal(stepLabel({ tool: 'ToolSearch' }), null);
  assert.equal(statusMessage(['consultando la base', 'consultando la base', null, 'dibujando el mapa']), 'Claude: pensando…\n- consultando la base\n- dibujando el mapa');
});

test('the self-chat answers at once when the tools are down, without a model turn', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'wa-bridge-tools-'));
  const script = join(dir, 'turn.mjs');
  writeFileSync(script, `import { writeFileSync } from 'node:fs'; writeFileSync(${JSON.stringify(join(dir, 'turn-ran'))}, '1'); console.log(JSON.stringify({ok:true,threadId:'T1',status:'completed',text:'Hay 12.'}));`);
  const tools = createServer((req, res) => res.end('{"status":"ok"}'));
  await new Promise(r => tools.listen(0, '127.0.0.1', r));
  const { port } = tools.address();
  const config = {
    chats: { self: { jid: 'ME@s.whatsapp.net', label: 'yo', route: 'r', enabled: true, dryRun: true, repliesHour: 60, contextLines: 0 } },
    routes: { r: { script, attachmentsDir: join(dir, 'att'), toolsHealth: `http://127.0.0.1:${port}/health` } },
    wacli: { bin: '/bin/false', store: dir },
  };
  const bridge = createBridge({ config, stateDir: join(dir, 'state'), log: { error() {}, info() {} } });
  const outbox = () => (existsSync(join(dir, 'state', 'outbox.jsonl')) ? readFileSync(join(dir, 'state', 'outbox.jsonl'), 'utf8').trim().split('\n').map(l => JSON.parse(l)) : []);
  const settle = async () => {
    for (let i = 0; i < 100 && !bridge.idle(); i++) await new Promise(r => setTimeout(r, 50));
    await new Promise(r => setTimeout(r, 100));
  };
  let n = 0;
  const post = text => bridge.handle({ Chat: 'ME@s.whatsapp.net', ID: `T${++n}`, SenderJID: 'ME@s.whatsapp.net', Timestamp: new Date().toISOString(), FromMe: true, Text: text });
  post('@claude hola');
  await settle();
  assert.ok(existsSync(join(dir, 'turn-ran'))); // tools up: the turn runs
  assert.ok(outbox().some(s => s.text === 'Claude: Hay 12.'));
  await new Promise(r => tools.close(r));
  const before = outbox().length;
  post('@claude otra');
  await settle();
  const sent = outbox().slice(before).filter(s => s.kind !== 'react');
  assert.deepEqual(sent.map(s => s.text), ['Claude: herramientas no disponibles ahora (la app de prueba está apagada); intenta en unos minutos.']);
  assert.equal(outbox().slice(before).filter(s => s.react).at(-1).react.emoji, '⚠️');
});

test('group sends: never without a @claude or reply-to-bot trigger', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'wa-bridge-guard-'));
  const script = join(dir, 'turn.mjs');
  writeFileSync(script, `console.log(JSON.stringify({ok:true,threadId:'T1',status:'completed',text:'Hay 12.\\n[images]\\n${join(dir, 'p.jpg')} | pie\\n[/images]\\n[voice]'}));`);
  writeFileSync(join(dir, 'p.jpg'), Buffer.from([0xff, 0xd8, 0xff, 0xe0, 1, 2, 3, 4]));
  const config = {
    chats: {
      group: { jid: 'G@g.us', label: 'Grupo', route: 'r', enabled: true, dryRun: true, repliesHour: 2, contextLines: 0 },
      self: { jid: 'ME@s.whatsapp.net', label: 'yo', route: 'r', enabled: true, dryRun: true, repliesHour: 60, contextLines: 0 },
    },
    routes: { r: { script, attachmentsDir: join(dir, 'att') } },
    wacli: { bin: '/bin/false', store: dir },
  };
  const bridge = createBridge({ config, stateDir: join(dir, 'state'), log: { error() {}, info() {} } });
  const outbox = () => (existsSync(join(dir, 'state', 'outbox.jsonl')) ? readFileSync(join(dir, 'state', 'outbox.jsonl'), 'utf8').trim().split('\n').map(l => JSON.parse(l)) : []);
  const log = () => readFileSync(join(dir, 'state', 'bridge.log.jsonl'), 'utf8');
  const settle = async () => {
    for (let i = 0; i < 100 && !bridge.idle(); i++) await new Promise(r => setTimeout(r, 50));
    await new Promise(r => setTimeout(r, 100));
  };
  let n = 0;
  const post = (chat, text, extra = {}) => bridge.handle({ Chat: chat, ID: `Q${++n}`, SenderJID: 'P@s.whatsapp.net', Timestamp: new Date().toISOString(), FromMe: false, Text: text, PushName: 'Ana', ...extra });
  // Image and voice need ffmpeg and a speech engine, so they are checked in the source: every send( call names its trigger.
  const src = readFileSync(new URL('../tools/whatsapp/bridge.mjs', import.meta.url), 'utf8');
  const calls = [...src.matchAll(/\bsend\(chat, \{[^\n]*/g)].map(m => m[0]);
  assert.ok(calls.length >= 8);
  assert.deepEqual(calls.filter(c => !c.includes('trigger')), []);
  assert.ok(calls.some(c => c.includes("kind: 'image'")) && calls.some(c => c.includes("kind: 'voice'")));
  const group = { key: 'group', jid: 'G@g.us', dryRun: true };

  // The unit: send() itself.
  const direct = bridge.send;
  await assert.rejects(direct(group, { kind: 'reply', text: 'Claude: hola' }), /no trigger/);
  await assert.rejects(direct(group, { kind: 'reply', text: 'Claude: hola', trigger: { id: 'x', text: 'hola a todos' } }), /no trigger/);
  await assert.rejects(direct(group, { kind: 'notice', text: 'Claude: aviso', trigger: { id: 'x', text: 'hola', replyToId: 'NOT-OURS' } }), /no trigger/);
  await assert.rejects(direct(group, { kind: 'react', react: { id: 'x', emoji: '👀' }, trigger: { id: 'x', text: 'sin etiqueta' } }), /no trigger/);
  await assert.rejects(direct(group, { kind: 'status', edit: 'OUT1', text: 'Claude: x', trigger: { id: 'x', text: 'sin etiqueta' } }), /no trigger/);
  assert.deepEqual(outbox(), []);
  assert.ok(log().includes('refused'));
  await direct(group, { kind: 'notice', text: 'Claude: hola', trigger: { id: 'x', text: 'oye @claude' } });
  assert.equal(outbox().length, 1);

  // Untagged group messages, commands included, send nothing at all.
  post('G@g.us', 'hola a todos');
  assert.equal(post('G@g.us', '/status', { FromMe: true }).reason, 'command without trigger');
  await settle();
  assert.equal(outbox().length, 1);

  // Tagged: reaction, answer, image and voice all pass the trigger; the rate-limit notice too.
  post('G@g.us', '@claude cuántos hay?');
  await settle();
  const kinds = outbox().slice(1).map(s => s.kind);
  assert.ok(['react', 'reply'].every(k => kinds.includes(k)), kinds.join());
  // A reply to one of the bridge's own messages counts as a trigger (also for a command); to someone else's it does not.
  const ours = outbox().find(s => s.kind === 'reply').id;
  post('G@g.us', '/status', { ReplyToID: ours });
  await settle();
  assert.ok(outbox().some(s => /Estado del puente/.test(s.text ?? '')));
  const before = outbox().length;
  post('G@g.us', '/status', { ReplyToID: 'SOMEONE-ELSE' });
  await settle();
  assert.equal(outbox().length, before);
  assert.equal(outbox().filter(s => s.kind === 'reply').length, 2);

  post('G@g.us', '@claude otra');
  await settle();
  assert.ok(outbox().some(s => s.kind === 'notice' && /límite/.test(s.text)));

  // The self chat needs no trigger.
  await direct({ key: 'self', jid: 'ME@s.whatsapp.net', dryRun: true }, { kind: 'reply', text: 'Claude: a mí' });
  assert.equal(outbox().at(-1).text, 'Claude: a mí');
});

test('group conversations: a tag opens a window, follow-ups inside it may be answered, outside nothing happens', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'wa-bridge-convo-'));
  const script = join(dir, 'turn.mjs');
  // A stubbed turn: silent when the follow-up is people talking among themselves.
  writeFileSync(script, `import { appendFileSync } from 'node:fs'; let s='';for await (const c of process.stdin) s+=c; const r=JSON.parse(s); appendFileSync(${JSON.stringify(join(dir, 'turns'))}, r.text.includes('UNTAGGED FOLLOW-UP') ? 'F\\n' : 'T\\n'); const m=r.text.split('Message to answer:')[1]; console.log(JSON.stringify({ok:true,threadId:'T1',status:'completed',text: m.includes('entre nosotros') ? '[silent]' : 'Para machos hay 5.'}));`);
  let clock = Date.now();
  const config = {
    chats: { group: { jid: 'G@g.us', label: 'Grupo', route: 'r', enabled: true, dryRun: true, repliesHour: 10, contextLines: 0 } },
    routes: { r: { script, attachmentsDir: join(dir, 'att') } },
    wacli: { bin: '/bin/false', store: dir },
  };
  const bridge = createBridge({ config, stateDir: join(dir, 'state'), now: () => clock, log: { error() {}, info() {} } });
  const outbox = () => (existsSync(join(dir, 'state', 'outbox.jsonl')) ? readFileSync(join(dir, 'state', 'outbox.jsonl'), 'utf8').trim().split('\n').map(l => JSON.parse(l)) : []);
  const turns = () => (existsSync(join(dir, 'turns')) ? readFileSync(join(dir, 'turns'), 'utf8').trim().split('\n') : []);
  const settle = async () => {
    for (let i = 0; i < 100 && !bridge.idle(); i++) await new Promise(r => setTimeout(r, 50));
    await new Promise(r => setTimeout(r, 100));
  };
  let n = 0;
  const post = (text, extra = {}) => bridge.handle({ Chat: 'G@g.us', ID: `C${++n}`, SenderJID: 'P@s.whatsapp.net', Timestamp: new Date(clock).toISOString(), FromMe: false, Text: text, PushName: 'Ana', ...extra });

  // No conversation: nothing is sent, no turn starts.
  assert.equal(post('¿y para machos?').reason, 'no @claude');
  await settle();
  assert.deepEqual(outbox(), []);
  assert.deepEqual(turns(), []);

  // A tag opens one; the follow-up inside it starts a turn (no reaction) and is answered, quoting it.
  post('@claude cuántas hembras hay?');
  await settle();
  assert.deepEqual(turns(), ['T']);
  const afterTag = outbox().length;
  clock += 5 * 60_000;
  assert.equal(post('¿y para machos?').decision, 'queued');
  await settle();
  assert.deepEqual(turns(), ['T', 'F']);
  const sent = outbox().slice(afterTag);
  assert.deepEqual(sent.map(s => s.kind), ['reply']);
  assert.equal(sent[0].text, 'Claude: Para machos hay 5.');

  // Teammates talking among themselves inside the window: the chat answers [silent], nothing goes out.
  clock += 5 * 60_000;
  const before = outbox().length;
  post('entre nosotros: ¿quién lleva las redes mañana?');
  await settle();
  assert.deepEqual(turns(), ['T', 'F', 'F']);
  assert.equal(outbox().length, before);

  // The silent exchange did not refresh the window: 15 minutes after Claude's last answer it is over.
  clock += 11 * 60_000;
  assert.equal(post('¿y para hembras?').reason, 'no @claude');
  await settle();
  assert.deepEqual(turns(), ['T', 'F', 'F']);
  assert.equal(outbox().length, before);
  // A reply to one of Claude's messages reopens it.
  post('gracias', { ReplyToID: sent[0].id });
  await settle();
  assert.deepEqual(turns(), ['T', 'F', 'F', 'T']);
});

function historySetup({ failing = false } = {}) {
  const dir = mkdtempSync(join(tmpdir(), 'wa-bridge-hist-'));
  const hist = join(dir, failing ? 'blocked' : 'ws', 'wa-history');
  if (failing) writeFileSync(join(dir, 'blocked'), 'a file where a folder should be');
  const script = join(dir, 'turn.mjs');
  // A stub turn: keeps the envelope it got, and answers from the history file when the envelope points to one and asks "planta".
  writeFileSync(
    script,
    `import { appendFileSync, readFileSync, existsSync } from 'node:fs'; let s='';for await (const c of process.stdin) s+=c; const r=JSON.parse(s);
appendFileSync(${JSON.stringify(join(dir, 'envelopes'))}, JSON.stringify(r.text)+'\\n');
let ans='Hay 12.'; const f=${JSON.stringify(join(hist, 'grupo.jsonl'))};
if (r.text.includes('wa-history/grupo.jsonl') && r.text.split('Message to answer:')[1].includes('planta') && existsSync(f)) { const hit=readFileSync(f,'utf8').split('\\n').filter(Boolean).map(x=>JSON.parse(x)).find(x=>/planta/i.test(x.text||'')); ans = hit ? 'La planta fue '+hit.text.split(': ').pop() : 'No la encuentro'; }
console.log(JSON.stringify({ok:true,threadId:'T1',status:'completed',text:ans}));`,
  );
  const config = {
    chats: {
      group: { jid: 'G@g.us', label: 'Grupo', route: 'r', enabled: true, dryRun: true, repliesHour: 50, contextLines: 15, history: { dir: hist } },
      other: { jid: 'O@g.us', label: 'Otro', route: 'r', enabled: true, dryRun: true, repliesHour: 50, contextLines: 15 },
    },
    routes: { r: { script, attachmentsDir: join(dir, 'att') } },
    wacli: { bin: '/bin/false', store: dir },
  };
  let clock = Date.now();
  const bridge = createBridge({ config, stateDir: join(dir, 'state'), now: () => clock, log: { error() {}, info() {} } });
  const settle = async () => {
    for (let i = 0; i < 100 && !bridge.idle(); i++) await new Promise(r => setTimeout(r, 50));
    await new Promise(r => setTimeout(r, 100));
    await bridge.flushHistory();
  };
  let n = 0;
  const post = (chat, text, extra = {}) => bridge.handle({ Chat: chat, ID: `H${++n}`, SenderJID: 'P@s.whatsapp.net', Timestamp: new Date(clock).toISOString(), FromMe: false, Text: text, PushName: 'Ana', ...extra });
  const rows = () => (existsSync(join(hist, 'grupo.jsonl')) ? readFileSync(join(hist, 'grupo.jsonl'), 'utf8').trim().split('\n').map(l => JSON.parse(l)) : []);
  const envelopes = () => (existsSync(join(dir, 'envelopes')) ? readFileSync(join(dir, 'envelopes'), 'utf8').trim().split('\n').map(l => JSON.parse(l)) : []);
  return { dir, hist, bridge, settle, post, rows, envelopes, tick: ms => (clock += ms) };
}

test('the enabled group is mirrored to its history file (all its messages, nothing from other chats), once each', async () => {
  const { post, settle, rows, hist, dir } = historySetup();
  post('G@g.us', 'hola a todos');
  post('G@g.us', '', { Media: { Type: 'image', Caption: '' }, PushName: 'Luis' });
  post('G@g.us', 'sí, en T2', { ReplyToID: 'H1' });
  post('O@g.us', 'secreto de otro chat');
  post('G@g.us', 'hola a todos', { ID: 'H1' }); // a duplicate delivery
  await settle();
  const got = rows();
  assert.deepEqual(got.map(r => r.text), ['hola a todos', null, 'sí, en T2']);
  assert.equal(got[1].media, 'foto');
  assert.equal(got[1].name, 'Luis');
  assert.equal(got[2].replyTo, 'H1');
  assert.equal(got[2].replyText, 'hola a todos');
  assert.ok(!readFileSync(join(hist, 'grupo.jsonl'), 'utf8').includes('secreto'));
  assert.ok(!existsSync(join(hist, 'otro.jsonl')));
  assert.equal(readFileSync(join(dir, 'state', 'history-queue.jsonl'), 'utf8'), ''); // sent, queue empty
});

test('a history that cannot be written keeps its lines queued instead of losing them', async () => {
  const { post, settle, dir } = historySetup({ failing: true });
  post('G@g.us', 'primero');
  post('G@g.us', 'segundo');
  await settle();
  const queued = readFileSync(join(dir, 'state', 'history-queue.jsonl'), 'utf8').trim().split('\n');
  assert.equal(queued.length, 2);
  assert.ok(readFileSync(join(dir, 'state', 'bridge.log.jsonl'), 'utf8').includes('"dir":"history"'));
});

test('context: last 15 messages or last 2 hours, whichever is more (cap 40), with replies and media', async () => {
  const { post, settle, envelopes, tick } = historySetup();
  for (let i = 0; i < 18; i++) {
    post('G@g.us', `viejo ${i}`, { Timestamp: new Date(Date.now() - 5 * 3_600_000 + i * 1000).toISOString() });
  }
  for (let i = 0; i < 25; i++) post('G@g.us', `reciente ${i}`, { Timestamp: new Date(Date.now() - 3_000_000 + i * 1000).toISOString() });
  post('G@g.us', '', { Media: { Type: 'image', Caption: 'una Mechanitis' } });
  post('G@g.us', 'respuesta', { ReplyToID: 'H19', PushName: 'Luis' });
  tick(1000);
  post('G@g.us', '@claude ¿qué dijimos?');
  await settle();
  const env = envelopes().at(-1);
  const ctx = env.split('Recent messages (oldest first):')[1].split('Message to answer:')[0].split('\n').filter(l => l.startsWith('  '));
  assert.equal(ctx.length, 27); // 25 recent + the photo + the reply: all inside 2 hours, more than 15
  assert.ok(!env.includes('viejo'));
  assert.ok(env.includes('[foto] una Mechanitis'));
  assert.match(env, /Luis \(en respuesta a Ana: «reciente 0»\): respuesta/);
  assert.ok(env.includes('wa-history/grupo.jsonl'));
});

test('context: with a quiet chat, the last 15 messages are kept even when older than 2 hours', async () => {
  const { post, settle, envelopes } = historySetup();
  for (let i = 0; i < 20; i++) post('G@g.us', `de ayer ${i}`, { Timestamp: new Date(Date.now() - 20 * 3_600_000 + i * 1000).toISOString() });
  post('G@g.us', '@claude hola');
  await settle();
  const ctx = envelopes().at(-1).split('Recent messages (oldest first):')[1].split('Message to answer:')[0];
  assert.equal((ctx.match(/de ayer/g) ?? []).length, 15);
});

test('a question that needs a 3-day-old message: the chat is pointed to the history and finds it', async () => {
  const { post, settle, hist, rows, envelopes } = historySetup();
  // The history already holds an old message (a backfill), not in the 15-message context.
  mkdirSync(hist, { recursive: true });
  writeFileSync(join(hist, 'grupo.jsonl'), JSON.stringify({ id: 'OLD1', at: new Date(Date.now() - 3 * 86_400_000).toISOString(), name: 'Luis', text: 'la planta de la semana: Solanum', media: null }) + '\n');
  for (let i = 0; i < 20; i++) post('G@g.us', `charla ${i}`);
  post('G@g.us', '@claude ¿cuál era la planta de la semana pasada?');
  await settle();
  const env = envelopes().at(-1);
  assert.ok(!env.includes('Solanum')); // not in the context lines
  assert.ok(env.includes('wa-history/grupo.jsonl'));
  assert.ok(rows().some(r => r.text?.includes('charla 0')));
  assert.ok(readFileSync(join(hist, '..', '..', 'state', 'outbox.jsonl'), 'utf8').includes('La planta fue Solanum'));
});
