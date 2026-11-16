// Voice notes in the WhatsApp bridge (12 Nov 2026): every note transcribed on arrival (Parakeet, one at a time), reused by
// the turns, upgraded once with Scribe for a turn; a note that calls Claude counts as @claude. No test here sends a WhatsApp
// message (dry-run chats: the outbox file only), starts a T3 turn (a stand-in script answers) or calls a speech engine
// (stand-in engines, counted).
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { chmodSync, mkdtempSync, readFileSync, writeFileSync, existsSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { basename, join } from 'node:path';
import { appendNew, applyWords, fixGenera, isVoice, loadWords, parakeetText, parseReply, speakable, voiceAddressed } from '../tools/whatsapp/bridge-lib.mjs';
import { createBridge } from '../tools/whatsapp/bridge.mjs';

test('voiceAddressed: Franz\'s chat takes the name anywhere, the group only in its first six words', () => {
  const personal = { personal: true };
  // Franz's own chat: anywhere.
  assert.ok(voiceAddressed('Claude, mándame la lista de Oleria', personal));
  assert.ok(voiceAddressed('estaba pensando que mañana podríamos, bueno, Claude revisa el gabinete tres', personal));
  assert.ok(voiceAddressed('y al final del audio: gracias claude', personal));
  // The group: the first six words only.
  assert.ok(voiceAddressed('Claude, ¿cuántos Mechanitis hay?'));
  assert.ok(voiceAddressed('Oye Claude, revisa la Oleria del CAM 234'));
  assert.ok(voiceAddressed('a ver a ver, ¿Claude? ¿me escuchas?'));
  assert.ok(voiceAddressed('uno dos tres cuatro cinco Claude'));
  assert.ok(!voiceAddressed('uno dos tres cuatro cinco seis Claude'));
  assert.ok(!voiceAddressed('Le estaba contando a Ana que Claude nos ayudó con las Methona'));
  assert.ok(voiceAddressed('Le estaba contando a Ana que Claude nos ayudó', personal));
  // Mishearings of the name.
  for (const name of ['Claud', 'Klaud', 'Clod', 'CLAUDE', 'cláude']) assert.ok(voiceAddressed(`${name}, ¿qué dice el reporte?`), name);
  // «Cloud» only in addressing position.
  assert.ok(voiceAddressed('Cloud, mándame el reporte'));
  assert.ok(voiceAddressed('oye cloud, mándame el reporte'));
  assert.ok(!voiceAddressed('ok cloud mándame el reporte', personal)); // no comma: could be a noun, so a miss
  assert.ok(!voiceAddressed('súbelo a la cloud porfa', personal));
  assert.ok(!voiceAddressed('the cloud is down again', personal));
  // «Cloud» as a noun never wakes (round 1 critics).
  for (const t of ['Cloud computing es lo que usamos', 'Y por favor cloud storage para las fotos', 'A cloud of butterflies', 'y cloud computing también', 'cloud storage, ya sabes'])
    assert.ok(!voiceAddressed(t, personal), t);
  assert.ok(voiceAddressed('¿Cloud? ¿me escuchas?'));
  // After a greeting too: only a call, followed by , ? or ! (round 2), and inside the group's six words.
  for (const t of ['ok cloud computing es lo que usamos', 'Hola cloud storage está lleno', 'Mira, cloud computing es caro', 'oye cloud mándame eso'])
    assert.ok(!voiceAddressed(t), t);
  assert.ok(!voiceAddressed('hola oye hey ok bueno mira cloud, revisa esto'));
  assert.ok(voiceAddressed('hola oye hey ok bueno mira cloud, revisa esto', personal));
  // Parakeet's «Claudia» for «Claude» (13 Nov): a leading call in Franz's chat only.
  for (const t of ['Claudia, ¿puedes ver cuántas mariposas ha colectado el colector Josué Ruiz?', 'Claudia, ¿me escuchaste?', 'Hola Claudia, revisa esto'])
    assert.ok(voiceAddressed(t, personal), t);
  for (const t of ['Claudia, ¿me escuchaste?', 'Hola Claudia, revisa esto']) assert.ok(!voiceAddressed(t), t);
  assert.ok(!voiceAddressed('le dije a Claudia, que venga mañana', personal));
  // Other names are not Claude.
  for (const t of ['Claudio, ¿vienes mañana?', 'Claudia ya llegó', 'claudette dice hola', 'Clodomiro trae las trampas', 'Oye Claudio'])
    assert.ok(!voiceAddressed(t, personal), t);
  assert.ok(!voiceAddressed('', personal));
  assert.ok(!voiceAddressed(null));
});

test('fixGenera: Parakeet\'s sound-alike genera become the genus; common words and names stay', () => {
  assert.equal(fixGenera('revisa si la olería del CAM 234, creo que es hiposcada'), 'revisa si la Oleria del CAM 234, creo que es Hyposcada');
  assert.equal(fixGenera('la lista de callitomia y las metonas'), 'la lista de Callithomia y las Methonas');
  assert.equal(fixGenera('vimos Godiris y una pteronimia, y la seratinia'), 'vimos Godyris y una Pteronymia, y la Ceratinia');
  assert.equal(fixGenera('cuántos mechanitis y melinaea'), 'cuántos Mechanitis y Melinaea');
  // Not a genus by sound: left as heard.
  assert.equal(fixGenera('las heliconias del jardín'), 'las heliconias del jardín');
  assert.equal(fixGenera('Patricia y greta trajeron las scada'), 'Patricia y greta trajeron las scada');
  assert.equal(fixGenera('hipoteris tircena gialiris'), 'hipoteris tircena gialiris');
  assert.equal(fixGenera('mañana revisamos todo el muestreo'), 'mañana revisamos todo el muestreo');
});

test('isVoice and the history: a transcript per note and engine', () => {
  assert.ok(isVoice('ptt') && isVoice('AUDIO') && !isVoice('image') && !isVoice(null));
  const file = join(mkdtempSync(join(tmpdir(), 'wa-voice-hist-')), 'h.jsonl');
  const line = o => JSON.stringify(o);
  assert.equal(appendNew(file, [line({ t: 'transcript', of: 'A', engine: 'parakeet', text: 'x' }), line({ t: 'transcript', of: 'A', engine: 'parakeet', text: 'x' })].join('\n')), 1);
  assert.equal(appendNew(file, line({ t: 'transcript', of: 'A', engine: 'scribe', text: 'y' })), 1);
  assert.equal(appendNew(file, line({ t: 'transcript', of: 'A', engine: 'scribe', text: 'y' })), 0);
  assert.equal(appendNew(file, line({ t: 'transcript', of: 'B', text: 'old line without engine' })), 1);
});

/**
 * A bridge with two dry-run chats (the group G and Franz's own chat S), a stand-in T3 turn that records the prompt it got,
 * and stand-in engines that answer from `heard` (file name → text) and count their calls.
 */
function setup({ engines: over = {}, repliesHour = 20, history = true, parakeetMs = 0, answer = "'Hay 12.'", voice: voiceConf = {}, tts = null, now = Date.now } = {}) {
  const dir = mkdtempSync(join(tmpdir(), 'wa-voice-'));
  const script = join(dir, 'turn.mjs');
  const prompts = join(dir, 'prompts');
  writeFileSync(script, `import { appendFileSync } from 'node:fs'; let s='';for await (const c of process.stdin) s+=c; const r=JSON.parse(s); appendFileSync(${JSON.stringify(prompts)}, JSON.stringify(r.text)+'\\n'); const t=r.text; console.log(JSON.stringify({ok:true,threadId:'T1',status:'completed',text:${answer}}));`);
  const config = {
    chats: {
      group: { jid: 'G@g.us', label: 'Grupo', route: 'r', enabled: true, dryRun: true, repliesHour, contextLines: 15, ack: 'reaction', ...(history ? { history: { dir: join(dir, 'hist') } } : {}) },
      self: { jid: 'S@s.whatsapp.net', label: 'yo', route: 'r', enabled: true, dryRun: true, repliesHour, contextLines: 0, ack: 'reaction' },
      ro: { jid: 'R@g.us', label: 'Solo lectura', route: 'r', enabled: true, readOnly: true, history: { dir: join(dir, 'hist') } },
    },
    routes: { r: { script, attachmentsDir: join(dir, 'att') } },
    wacli: { bin: '/bin/false', store: dir },
    voice: voiceConf,
  };
  const heard = {};
  const calls = { parakeet: [], scribe: [], whisper: [] };
  const engine = name => async file => {
    calls[name].push(basename(file));
    if (name === 'parakeet' && parakeetMs) await new Promise(r => setTimeout(r, parakeetMs));
    if (over[name]) return over[name](file);
    const t = heard[basename(file)];
    if (t == null) throw new Error('no such audio');
    return { text: name === 'scribe' ? `${t} [scribe]` : t, language: 'es' };
  };
  const engines = { parakeet: engine('parakeet'), scribe: engine('scribe'), whisper: engine('whisper'), ...(tts ? { tts } : {}) };
  const bridge = createBridge({ config, stateDir: join(dir, 'state'), log: { error() {}, info() {} }, engines, now });
  let n = 0;
  /** A voice note (ptt) in a chat; `said` is what the engines hear in it. */
  const voice = (chat, said, extra = {}) => {
    const id = extra.ID ?? `V${++n}`;
    const file = join(dir, `${id}.ogg`);
    writeFileSync(file, 'not really audio');
    heard[`${id}.ogg`] = said;
    const self = chat === 'S@s.whatsapp.net';
    return { id, ...bridge.handle({ Chat: chat, ID: id, SenderJID: self ? chat : 'P@s.whatsapp.net', Timestamp: new Date().toISOString(), FromMe: self, Text: '', Media: { Type: 'ptt' }, PushName: self ? 'Franz' : 'Ana', _labMedia: file, ...extra }) };
  };
  const text = (chat, t, extra = {}) => bridge.handle({ Chat: chat, ID: `M${++n}`, SenderJID: 'P@s.whatsapp.net', Timestamp: new Date().toISOString(), FromMe: chat === 'S@s.whatsapp.net', Text: t, PushName: 'Persona Tres', ...extra });
  const outbox = () => (existsSync(join(dir, 'state', 'outbox.jsonl')) ? readFileSync(join(dir, 'state', 'outbox.jsonl'), 'utf8').trim().split('\n').map(l => JSON.parse(l)) : []);
  const log = () => readFileSync(join(dir, 'state', 'bridge.log.jsonl'), 'utf8').trim().split('\n').map(l => JSON.parse(l));
  const turns = () => (existsSync(prompts) ? readFileSync(prompts, 'utf8').trim().split('\n').map(l => JSON.parse(l)) : []);
  const settle = async () => {
    for (let i = 0; i < 200 && !bridge.idle(); i++) await new Promise(r => setTimeout(r, 25));
    await new Promise(r => setTimeout(r, 150));
    for (let i = 0; i < 200 && !bridge.idle(); i++) await new Promise(r => setTimeout(r, 25));
    await new Promise(r => setTimeout(r, 100));
  };
  const historyLines = () => (existsSync(join(dir, 'hist', 'grupo.jsonl')) ? readFileSync(join(dir, 'hist', 'grupo.jsonl'), 'utf8').trim().split('\n').map(l => JSON.parse(l)) : []);
  return { bridge, dir, calls, heard, voice, text, outbox, log, turns, settle, historyLines };
}

test('every voice note is transcribed on arrival, once, with Parakeet; one not for Claude starts nothing', async () => {
  const s = setup();
  const a = s.voice('G@g.us', 'mañana salimos temprano a las trampas');
  assert.equal(a.decision, 'voice');
  await s.settle();
  assert.deepEqual(s.calls.parakeet, [`${a.id}.ogg`]);
  assert.deepEqual(s.calls.scribe, []); // on arrival a note never goes to Scribe
  assert.equal(s.bridge.state().transcripts[a.id], 'mañana salimos temprano a las trampas');
  assert.equal(s.bridge.state().voiceMeta[a.id].engine, 'parakeet');
  assert.deepEqual(s.outbox(), []); // no reaction, no answer
  assert.deepEqual(s.turns(), []);
  assert.ok(s.log().some(e => e.id === a.id && e.reason === 'voice: not addressed'));
  // The same webhook again (wacli retries): no second transcription.
  s.bridge.handle({ Chat: 'G@g.us', ID: a.id, SenderJID: 'P@s.whatsapp.net', Timestamp: new Date().toISOString(), Text: '', Media: { Type: 'ptt' }, _labMedia: join(s.dir, `${a.id}.ogg`) });
  await s.settle();
  assert.equal(s.calls.parakeet.length, 1);
  // Its transcript is in the history file (parakeet), next to the note's own line.
  await s.bridge.flushHistory();
  const lines = s.historyLines();
  assert.ok(lines.some(l => l.id === a.id && l.media === 'audio'));
  assert.ok(lines.some(l => l.t === 'transcript' && l.of === a.id && l.engine === 'parakeet' && l.text === 'mañana salimos temprano a las trampas'));
});

test('the transcript of an earlier note is in the context lines of a later @claude message (no new transcription)', async () => {
  const s = setup();
  const a = s.voice('G@g.us', 'encontré tres Oleria en la trampa dos');
  await s.settle();
  s.text('G@g.us', '@claude ¿qué encontró Ana?');
  await s.settle();
  const [prompt] = s.turns();
  assert.match(prompt, /Ana: \[audio: «encontré tres Oleria en la trampa dos»\]/);
  assert.equal(s.calls.parakeet.length, 1);
  assert.deepEqual(s.calls.scribe, []);
  assert.ok(a.id);
});

test('a group note that calls Claude in its first words is an @claude message: 👀, a turn with the transcript, «🎙️» line, ✅', async () => {
  const s = setup();
  const a = s.voice('G@g.us', 'Oye Claude, ¿cuántas Mechanitis tenemos de Tiputini?');
  await s.settle();
  assert.equal(s.calls.parakeet.length, 1);
  assert.deepEqual(s.calls.scribe, [`${a.id}.ogg`]); // the turn upgrades its own note once, for the names
  const [prompt] = s.turns();
  assert.match(prompt, /VOICE NOTE: this voice note does not tag @claude/);
  assert.match(prompt, /Ana: \[nota de voz: su transcripción está en Attachments\]/);
  assert.match(prompt, /voice note \(transcribed by scribe, es, may mishear names and codes\): "Oye Claude, ¿cuántas Mechanitis tenemos de Tiputini\? \[scribe\]"/);
  const out = s.outbox();
  // (No TTS in the test config, so the voice reply the bridge adds for a heard note fails and is only logged.)
  assert.deepEqual(out.map(o => o.kind), ['react', 'reply', 'react']);
  assert.ok(s.log().some(e => e.dir === 'out' && e.kind === 'voice' && e.ok === false));
  assert.deepEqual([out[0].react.emoji, out.at(-1).react.emoji], ['👀', '✅']);
  assert.equal(out[1].text, 'Claude: 🎙️ «Oye Claude, ¿cuántas Mechanitis tenemos de Tiputini? [scribe]»\nHay 12.');
  // The Scribe text is kept, so a later turn reuses it without any engine.
  assert.equal(s.bridge.state().voiceMeta[a.id].engine, 'scribe');
  await s.bridge.flushHistory();
  assert.deepEqual(s.historyLines().filter(l => l.t === 'transcript').map(l => l.engine), ['parakeet', 'scribe']);
  // The conversation is open: a quote of the note now wakes too, and nothing is transcribed again.
  s.text('G@g.us', '@claude repite lo de antes');
  await s.settle();
  assert.equal(s.calls.parakeet.length, 1);
  assert.equal(s.calls.scribe.length, 1);
});

test('a long «🎙️» line is cut at 80 characters', async () => {
  const s = setup();
  const said = 'Claude, ' + 'necesito que revises todas las fotos del gabinete tres y me digas cuáles faltan por identificar';
  s.voice('G@g.us', said);
  await s.settle();
  const reply = s.outbox().find(o => o.kind === 'reply');
  const line = reply.text.split('\n')[0];
  assert.equal(line, `Claude: 🎙️ «${(said + ' [scribe]').slice(0, 80)}…»`);
});

test('the group ignores a note that only talks about Claude; Franz\'s chat takes the name anywhere; Claudio wakes nobody', async () => {
  const s = setup();
  s.voice('G@g.us', 'Le estaba contando a Ana que Claude nos ayudó con las Methona');
  s.voice('G@g.us', 'Claudio, ¿traes las trampas mañana?');
  s.voice('S@s.whatsapp.net', 'Claudio dice que mañana no viene');
  await s.settle();
  assert.deepEqual(s.turns(), []);
  assert.deepEqual(s.outbox(), []);
  const b = s.voice('S@s.whatsapp.net', 'acuérdate de que mañana hay reunión, y Claude anótalo en la agenda');
  await s.settle();
  assert.equal(s.turns().length, 1);
  assert.match(s.turns()[0], /\[WhatsApp · Franz \(self-chat\)/);
  assert.match(s.outbox().find(o => o.kind === 'reply').text, /^Claude: 🎙️ «acuérdate de que mañana/);
  assert.equal(s.calls.parakeet.length, 4);
  assert.ok(b.id);
});

test('a tagged message with a voice note waits for the note\'s on-arrival transcription instead of making a second one', async () => {
  const s = setup({ parakeetMs: 300 });
  // A voice note with an @claude caption: the turn starts at once and needs the note while Parakeet is still on it.
  const a = s.voice('G@g.us', 'la Oleria del CAM 234', { Media: { Type: 'ptt', Caption: '@claude escucha esto' } });
  assert.equal(a.decision, 'queued');
  await s.settle();
  assert.deepEqual(s.calls.parakeet, [`${a.id}.ogg`]);
  assert.deepEqual(s.calls.scribe, [`${a.id}.ogg`]);
  assert.equal(s.turns().length, 1);
  // Tagged with text: no «🎙️» line (that is only for a note that woke Claude by itself).
  assert.equal(s.outbox().find(o => o.kind === 'reply').text, 'Claude: Hay 12.');
});

test('fallbacks: Parakeet down → whisper on arrival (never Scribe); Scribe down in a turn → the stored transcript', async () => {
  const s = setup({ engines: { parakeet: () => { throw new Error('device 0000:03:00.0 not found'); }, scribe: () => { throw new Error('scribe 503'); } } });
  const a = s.voice('G@g.us', 'Claude, ¿qué tal la Godyris?');
  await s.settle();
  assert.deepEqual(s.calls.parakeet, [`${a.id}.ogg`]);
  assert.deepEqual(s.calls.whisper, [`${a.id}.ogg`]); // the arrival's fallback
  assert.deepEqual(s.calls.scribe, [`${a.id}.ogg`]); // the turn's upgrade, which fails
  assert.match(s.turns()[0], /transcribed by whisper/);
  assert.equal(s.bridge.state().voiceMeta[a.id].engine, 'whisper');
  assert.ok(s.log().some(e => e.dir === 'voice' && e.engine === 'parakeet' && e.ok === false && /not found/.test(e.error)));
  assert.ok(s.log().some(e => e.dir === 'voice' && e.engine === 'scribe' && e.ok === false));
});

test('fallbacks: every engine down → no trigger, nothing stored, nothing sent', async () => {
  const fail = () => { throw new Error('down'); };
  const s = setup({ engines: { parakeet: fail, whisper: fail, scribe: fail } });
  const a = s.voice('G@g.us', 'Claude, hola');
  await s.settle();
  assert.equal(s.bridge.state().transcripts[a.id], undefined);
  assert.deepEqual(s.calls.scribe, []);
  assert.deepEqual(s.outbox(), []);
  assert.deepEqual(s.turns(), []);
});

test('a turn whose note has no stored transcript runs the turn chain: Scribe, then Parakeet, then whisper', async () => {
  const s = setup({ engines: { scribe: () => { throw new Error('scribe 500'); } } });
  // A read-only chat is never transcribed on arrival; here the turn chain is checked through transcribe() itself.
  const file = join(s.dir, 'x.ogg');
  writeFileSync(file, 'x');
  s.heard['x.ogg'] = 'hola';
  const t = await s.bridge.transcribe(file);
  assert.equal(t.engine, 'parakeet');
  assert.deepEqual([s.calls.scribe, s.calls.parakeet, s.calls.whisper], [['x.ogg'], ['x.ogg'], []]);
});

test('the limits still apply: /stop, the hourly limit; a read-only chat is not transcribed', async () => {
  const s = setup({ repliesHour: 1 });
  s.voice('G@g.us', 'Claude, primera pregunta');
  await s.settle();
  s.voice('G@g.us', 'Claude, segunda pregunta');
  await s.settle();
  assert.equal(s.turns().length, 1);
  assert.ok(s.outbox().some(o => o.kind === 'notice' && /límite de 1 respuestas/.test(o.text)));
  // /stop from Franz: notes are still transcribed (history) but wake nothing.
  const t = setup();
  t.text('S@s.whatsapp.net', '/stop');
  await t.settle();
  const before = t.outbox().length;
  t.voice('S@s.whatsapp.net', 'Claude, ¿estás ahí?');
  await t.settle();
  assert.equal(t.calls.parakeet.length, 1);
  assert.equal(t.outbox().length, before);
  assert.deepEqual(t.turns(), []);
  // Read-only source: mirrored, never transcribed.
  t.voice('R@g.us', 'Claude, hola');
  await t.settle();
  assert.equal(t.calls.parakeet.length, 1);
});

test('the bridge\'s own voice reply, coming back as Franz\'s audio, is not transcribed and cannot wake Claude', async () => {
  const s = setup();
  // A voice note sent by the bridge (dry run: the outbox gives it an id, kept in botIds like a real send's).
  const file = join(s.dir, 'reply.ogg');
  writeFileSync(file, 'x');
  const trigger = { id: 'T0', text: '@claude hola', fromMe: true };
  const id = await s.bridge.send({ key: 'self', jid: 'S@s.whatsapp.net', dryRun: true, route: 'r' }, { kind: 'voice', file, voice: true, trigger });
  s.voice('S@s.whatsapp.net', 'Claude dice que hay doce Oleria', { ID: id });
  await s.settle();
  assert.deepEqual(s.calls.parakeet, []);
  assert.deepEqual(s.turns(), []);
  assert.ok(s.log().some(e => e.id === id && e.skipped === 'own voice note'));
});

test('asr-parakeet picks the configured eGPU only: never the iGPU, the CPU or another device', async () => {
  const { pickDevice } = await import('../tools/whatsapp/asr-parakeet.mjs');
  const devices = [
    { name: 'Vulkan0', description: 'AMD Radeon 760M Graphics', deviceType: 'igpu', deviceId: '0000:66:00.0' },
    { name: 'Vulkan1', description: 'AMD Radeon RX 7600M XT', deviceType: 'gpu', deviceId: '0000:03:00.0' },
    { name: 'CPU', description: 'AMD Ryzen 5 7640U', deviceType: 'cpu', deviceId: null },
  ];
  assert.equal(pickDevice(devices, '0000:03:00.0').name, 'Vulkan1');
  assert.throws(() => pickDevice(devices, '0000:66:00.0'), /igpu, not the eGPU/);
  assert.throws(() => pickDevice(devices, null), /no device configured/);
  assert.throws(() => pickDevice(devices.slice(0, 1).concat(devices.slice(2)), '0000:03:00.0'), /not found/); // eGPU unplugged: no fallback
});

test('a group note that only talks about Claude early wakes the turn, which is told to end in [silent]', async () => {
  const s = setup();
  s.voice('G@g.us', 'Ayer Claude me dijo que faltaban tres');
  await s.settle();
  const [prompt] = s.turns();
  assert.match(prompt, /only talks about you to someone else .* your last message must be exactly \[silent\]/);
});

test('a note delivered late is transcribed (history) but wakes nothing', async () => {
  const s = setup();
  const old = new Date(Date.now() - 20 * 60_000).toISOString();
  const a = s.voice('G@g.us', 'Claude, ¿sigues ahí?', { Timestamp: old });
  await s.settle();
  assert.equal(a.decision, 'skip');
  assert.deepEqual(s.calls.parakeet, [`${a.id}.ogg`]);
  assert.equal(s.bridge.state().transcripts[a.id], 'Claude, ¿sigues ahí?');
  assert.deepEqual(s.turns(), []);
  // Older than two days: not even transcribed.
  s.voice('G@g.us', 'nota vieja', { Timestamp: new Date(Date.now() - 3 * 86_400_000).toISOString() });
  await s.settle();
  assert.equal(s.calls.parakeet.length, 1);
});

test('a failed Scribe upgrade is not retried by later turns needing the same note; two at once share one try', async () => {
  const s = setup({ engines: { scribe: () => { throw new Error('scribe 503'); } } });
  const a = s.voice('G@g.us', 'mañana revisamos las Oleria');
  await s.settle();
  const chat = { key: 'group', jid: 'G@g.us', dryRun: true, history: { dir: join(s.dir, 'hist') } };
  const row = { id: a.id, type: 'ptt', path: join(s.dir, `${a.id}.ogg`) };
  // Two turns needing the note at the same time, then a third later.
  const [t1, t2] = await Promise.all([s.bridge.voiceForTurn(chat, row, s.dir), s.bridge.voiceForTurn(chat, row, s.dir)]);
  const t3 = await s.bridge.voiceForTurn(chat, row, s.dir);
  assert.equal(s.calls.scribe.length, 1);
  assert.equal(s.calls.parakeet.length, 1);
  assert.deepEqual([t1.text, t2.text, t3.text], Array(3).fill('mañana revisamos las Oleria'));
  assert.equal(s.bridge.state().voiceMeta[a.id].scribeTried, true);
});

test('the on-arrival queue: a fresh note goes ahead of a backlog of late ones', async () => {
  const s = setup({ parakeetMs: 50 });
  const old = new Date(Date.now() - 60 * 60_000).toISOString();
  const ids = [1, 2, 3].map(i => s.voice('G@g.us', `nota vieja ${i}`, { Timestamp: old }).id);
  const fresh = s.voice('G@g.us', 'nota nueva').id;
  await s.settle();
  // The first late note was already being transcribed; the fresh one comes right after it, before the other late ones.
  assert.deepEqual(s.calls.parakeet, [ids[0], fresh, ids[1], ids[2]].map(id => `${id}.ogg`));
});

test('speakable: every number kept or dropped whole; links, code, coordinates, phones, codes and files go; [text] and [voice]', () => {
  // Kept as they are (reviews rounds 1-3): counts, thousands, decimals, signs, times, ranges, fractions, units, short answers.
  for (const t of ['Mide 1.05 mm y pesa 0.025 g.', 'El 95.02 % tiene foto.', 'Hay 10.050 registros.', 'Son 2.007 fotos y 1.005 sin foto.', 'p < 0.001', 'p > 0.05', 'Mide 2,05 mm.', 'Llegamos a las 9:05.', 'Son las 08:15.', 'Entre 1.250, 2.500 y 3.750 registros.', 'Altitud entre 1200-1800 m, en 2019-2024.', 'Vuela a 1500m, 3km, 2h, 5x, 1er, 2º, 1/3, 3km/h, 7mo.', '25°C y 50%', 'Sí.', 'No.', '42', 'Mide 1,234.5678 mm.', 'Hay 12.345,6789 kg', '-0.05 °C.', 'En 2024 hay 300 fotos', 'Altitud 2500.5 -0.5', 'Diferencia -0.5, 1.234.567 registros', 'Correlaciones de 0.45, -0.32 y 0.12', 'Valores: -1.5, -2.5, -3.5', '+12 % más', '35.5-42.0 mm', 'Hay 2345 Mechanitis, colectadas en 2024 a 1200 m; el 95.2 % son hembras, de 2,5 mm, entre 10-15 por trampa.'])
    assert.equal(speakable(t), t, t);
  const cases = [
    // Thousands grouped with spaces become one number.
    ['Hay 10 000 registros', 'Hay 10000 registros'], ['Hay 1 000 000 de registros', 'Hay 1000000 de registros'], ['Hay 1 050 fotos', 'Hay 1050 fotos'], ['Hay 12 000,5 kg', 'Hay 12000,5 kg'],
    ['Hay 10\u00A0000 registros', 'Hay 10000 registros'], ['Hay 10 000 000 4567 fotos', 'Hay 10000000 4567 fotos'], ['Son 1000 000', ''], ['1 000 | 123', '1000, 123'],
    // Signs between numbers keep their meaning; a glued word is split; removed spans never glue two numbers.
    ['Hay 12 machos/15 hembras', 'Hay 12 machos/ 15 hembras'], ['Total:12 fotos', 'Total: 12 fotos'], ['Rango 2~3 días', 'Rango 2-3 días'], ['Son 5*120 = 600', 'Son 5 × 120 = 600'],
    ['>1000 registros', '> 1000 registros'], ['Hay 12`x`34 fotos', 'Hay fotos'], ['Hay 12✅34 fotos', 'Hay 12, 34 fotos'], ['Tel: +593 99 123 4567\n12 fotos', 'Tel: 12 fotos'],
    ['tubo #123 listo', 'tubo listo'],
    // Signed or bracketed grouped thousands join whole; a grouped number next to a range, a slash or a sign goes whole (rounds 5-6).
    ['El saldo es -1 000 dólares.', 'El saldo es -1000 dólares.'], ['Subió +5 000 registros', 'Subió +5000 registros'], ['Hay (3 000 000 registros) en total', 'Hay (3000000 registros) en total'],
    ['Entre 1 000–1 500 m', 'Entre m'], ['Entre 10-15 000 registros', 'Entre registros'], ['Cuesta $1 500', ''], ['Hay 1  000 250 registros.', 'Hay registros.'], ['La distancia es 1 200 050m.', 'La distancia es.'],
    ['Altitud entre 1200–1800 m.', 'Altitud entre 1200-1800 m.'], ['It has 12 species—3 of them rare.', 'It has 12 species, 3 of them rare.'], ['Temperatura: –5 °C', 'Temperatura: °C'],
    ['Hembras: 12\n300 machos', 'Hembras: 12, 300 machos'], ['En 2024 *345* registros', 'En 2024, 345 registros'], ['Cambio: +12 000', 'Cambio: +12000'], ['192.168.100.200', ''], ['Probabilidades: 0.1234, 0.2345 y 0.3456', 'Probabilidades: 0.1234, 0.2345 y 0.3456'], ['0.6374° S, 76.1450° W', ''],
    // Coordinates go whole, also next to other numbers, without spaces, with the Unicode minus, after «coordenadas».
    ['Coordenadas: -0.64, -76.15.', ''], ['Coordenadas: -0.64,-76.15.', ''], ['Coordenadas: 0.64, 76.15.', ''], ['Unicode −0.64, −76.15 aquí', 'Unicode, aquí'],
    ['Elevación 1250.5, -0.6374, -76.1450', 'Elevación 1250.5'], ['Ubicación 0°38\'S 76°08\'W', ''], ['Está a 1234°38\' N', 'Está a'],
    ['Hay 12 Mechanitis en el CAM034567, cerca de -0.6374, -76.1450.', 'Hay 12 Mechanitis en el, cerca de.'],
    // Phones, e-mails, codes, dates, files, links, code, emojis.
    ['Llama al 099 123 4567 o al (02) 245-1234, o al +593 99 123 4567', 'Llama al o al, o al'], ['Escribe a franz@x.org', 'Escribe a'],
    ['Fecha 2026-11-12 y 12/11/2026, tubo B2, archivo informe_2026.xlsx', 'Fecha y, tubo, archivo'], ['Ver CAM034567', ''], ['Código CAM-123 listo', 'Código listo'],
    ['Ver https://x.org/a.', ''], ['*Hola* ver https://x.org ya', 'Hola ver ya'], ['Mira inaturalist.org/observations/123456 ahí', 'Mira ahí'],
    ['Son 3 Oleria y 145 Ithomia (ver www.inat.org).', 'Son 3 Oleria y 145 Ithomia.'], ['Usa `identify_species` así:\n```js\nx=1\n```\nlisto', 'Usa así: listo'],
    ['🎙️ Listo, son 3 ✅', 'Listo, son 3'], ['- primero\n- segundo', 'primero segundo'],
  ];
  for (const [t, want] of cases) assert.equal(speakable(t), want, t);
  assert.deepEqual([parseReply('Hola\n[text]').textOnly, parseReply('Hola\n[text]').text], [true, 'Hola']);
  assert.equal(parseReply('Hola\n[voice]\n[text]').voice, false); // text wins when both
  assert.equal(parseReply('Hola\n[voice]').voice, true);
});

/** A TTS stand-in: writes a tiny Ogg file and counts its calls (or throws `fail`). */
const fakeTts = (calls, fail = null) => async (spoken, lang, ogg) => {
  calls.push({ spoken, lang });
  if (fail) throw new Error(fail);
  writeFileSync(ogg, Buffer.concat([Buffer.from('OggS'), Buffer.alloc(60)]));
  return 7;
};
const ttsConf = dir => {
  writeFileSync(join(dir, 'el.env'), 'ELEVENLABS_API_KEY=stub\n');
  return { elevenlabsEnv: join(dir, 'el.env'), tts: { model: 'eleven_v4_turbo', format: 'opus_48000_64', voiceEs: 'ES_VOICE', voiceEn: 'EN_VOICE' } };
};

test('voice replies: a voice note in → text and a voice note out; [text] → text only; a text message → text unless [voice]', async () => {
  const calls = [];
  const tmp = mkdtempSync(join(tmpdir(), 'wa-tts-'));
  const answer = "t.includes('mándamelo por escrito') ? 'Son 12.\\n[text]' : t.includes('dímelo en audio') ? 'Son 12 Mechanitis, ver https://x.org\\n[voice]' : 'Son 12.'";
  const s = setup({ answer, voice: ttsConf(tmp), tts: fakeTts(calls) });
  s.voice('S@s.whatsapp.net', 'Claude, ¿cuántas Mechanitis hay?');
  await s.settle();
  let out = s.outbox();
  assert.deepEqual(out.map(o => o.kind), ['react', 'reply', 'voice', 'react']);
  assert.match(s.turns()[0], /Voice: they sent a voice note, so the bridge also sends your answer as a voice note .* add a line \[text\]/);
  assert.equal(calls.length, 1);
  assert.ok(s.log().some(e => e.kind === 'voice' && e.engine === 'elevenlabs' && e.cost === 7));
  // A voice note that asks for text: text only.
  s.voice('S@s.whatsapp.net', 'Claude, mándamelo por escrito');
  await s.settle();
  out = s.outbox().slice(4);
  assert.deepEqual(out.map(o => o.kind), ['react', 'reply', 'react']);
  assert.equal(calls.length, 1);
  // A text message: text, and the instruction says [voice] only when asked.
  s.text('S@s.whatsapp.net', '@claude ¿cuántas hay?');
  await s.settle();
  assert.equal(calls.length, 1);
  assert.match(s.turns()[2], /Voice: your answer goes as text\. Only if they ask to hear it .* add a line \[voice\]/);
  assert.doesNotMatch(s.turns()[2], /«dímelo»/);
  // Text asking for audio, answered with [voice]: text and voice, the link left out of the spoken part.
  s.text('S@s.whatsapp.net', '@claude dímelo en audio');
  await s.settle();
  assert.equal(calls.length, 2);
  assert.deepEqual(calls[1], { spoken: 'Son 12 Mechanitis, ver', lang: 'es' });
  const last = s.outbox().slice(-3);
  assert.deepEqual(last.map(o => o.kind), ['reply', 'voice', 'react']);
  assert.match(last[0].text, /https:\/\/x\.org/); // the written answer keeps the link
});

test('voice replies: ElevenLabs out of credits → Edge TTS, and ElevenLabs is left alone for an hour', async () => {
  const { createServer } = await import('node:http');
  const tmp = mkdtempSync(join(tmpdir(), 'wa-tts-'));
  // The bridge's own ElevenLabs request, against a local stub that answers 402 (quota exceeded) and counts its calls.
  const hits = [];
  const server = createServer((req, res) => {
    let body = '';
    req.on('data', d => (body += d));
    req.on('end', () => {
      hits.push({ url: req.url, body: JSON.parse(body) });
      res.writeHead(402, { 'content-type': 'application/json' });
      res.end('{"detail":{"status":"quota_exceeded"}}');
    });
  });
  await new Promise(r => server.listen(0, '127.0.0.1', r));
  // An Edge TTS stand-in that writes a short real mp3 (ffmpeg), so the bridge's own ffmpeg step runs.
  const edge = join(tmp, 'edge-tts');
  writeFileSync(edge, `#!/bin/sh\nwhile [ "$1" != "--write-media" ]; do shift; done\nffmpeg -v error -y -f lavfi -i anullsrc=r=24000:cl=mono -t 0.3 "$2"\n`);
  chmodSync(edge, 0o700);
  const conf = ttsConf(tmp);
  const voice = { ...conf, tts: { ...conf.tts, url: `http://127.0.0.1:${server.address().port}/v1/text-to-speech` }, edgeTts: edge, ttsVoiceEs: 'es-EC-AndreaNeural', ttsVoiceEn: 'en-US-AvaNeural' };
  const s = setup({ voice });
  s.voice('S@s.whatsapp.net', 'Claude, ¿cuántas hay?');
  await s.settle();
  s.voice('S@s.whatsapp.net', 'Claude, ¿y en Tiputini?');
  await s.settle();
  server.close();
  assert.deepEqual(s.outbox().map(o => o.kind), ['react', 'reply', 'voice', 'react', 'react', 'reply', 'voice', 'react']);
  assert.equal(hits.length, 1); // the second answer went straight to Edge TTS
  assert.equal(hits[0].url, '/v1/text-to-speech/ES_VOICE?output_format=opus_48000_64');
  assert.deepEqual(hits[0].body, { text: 'Hay 12.', model_id: 'eleven_v4_turbo' });
  assert.ok(s.log().some(e => e.kind === 'voice' && e.engine === 'elevenlabs' && e.ok === false && /402/.test(e.error)));
  assert.equal(s.log().filter(e => e.kind === 'voice' && e.engine === 'edge').length, 2);
});

test('voice replies: an ElevenLabs error of any kind → Edge TTS', async () => {
  const calls = [];
  const tmp = mkdtempSync(join(tmpdir(), 'wa-tts-'));
  const edge = join(tmp, 'edge-tts');
  writeFileSync(edge, `#!/bin/sh\nwhile [ "$1" != "--write-media" ]; do shift; done\nffmpeg -v error -y -f lavfi -i anullsrc=r=24000:cl=mono -t 0.3 "$2"\n`);
  chmodSync(edge, 0o700);
  const s = setup({ voice: { ...ttsConf(tmp), edgeTts: edge, ttsVoiceEs: 'es-EC-AndreaNeural', ttsVoiceEn: 'en-US-AvaNeural' }, tts: fakeTts(calls, 'boom') });
  s.voice('S@s.whatsapp.net', 'Claude, ¿cuántas hay?');
  await s.settle();
  assert.deepEqual(s.outbox().map(o => o.kind), ['react', 'reply', 'voice', 'react']);
  assert.ok(s.log().some(e => e.kind === 'voice' && e.engine === 'elevenlabs' && e.ok === false && e.chat === 'self' && /boom/.test(e.error)));
  assert.ok(s.log().some(e => e.kind === 'voice' && e.engine === 'edge'));
  assert.equal(calls.length, 1);
});

test('voice replies: an ElevenLabs server error → Edge TTS for 5 minutes, then ElevenLabs again', async () => {
  const { createServer } = await import('node:http');
  const tmp = mkdtempSync(join(tmpdir(), 'wa-tts-'));
  let hits = 0;
  const server = createServer((req, res) => {
    req.resume();
    req.on('end', () => {
      hits++;
      res.writeHead(500);
      res.end('oops');
    });
  });
  await new Promise(r => server.listen(0, '127.0.0.1', r));
  const edge = join(tmp, 'edge-tts');
  writeFileSync(edge, `#!/bin/sh\nwhile [ "$1" != "--write-media" ]; do shift; done\nffmpeg -v error -y -f lavfi -i anullsrc=r=24000:cl=mono -t 0.3 "$2"\n`);
  chmodSync(edge, 0o700);
  const conf = ttsConf(tmp);
  let skew = 0;
  const s = setup({ now: () => Date.now() + skew, voice: { ...conf, tts: { ...conf.tts, url: `http://127.0.0.1:${server.address().port}/v1/text-to-speech` }, edgeTts: edge, ttsVoiceEs: 'es-EC-AndreaNeural', ttsVoiceEn: 'en-US-AvaNeural' } });
  s.voice('S@s.whatsapp.net', 'Claude, uno');
  await s.settle();
  s.voice('S@s.whatsapp.net', 'Claude, dos');
  await s.settle();
  assert.equal(hits, 1); // the second reply skipped ElevenLabs
  skew = 6 * 60_000;
  s.voice('S@s.whatsapp.net', 'Claude, tres', { Timestamp: new Date(Date.now() + skew).toISOString() });
  await s.settle();
  server.close();
  assert.equal(hits, 2); // tried again after the 5 minutes
  assert.equal(s.log().filter(e => e.kind === 'voice' && e.engine === 'edge').length, 3);
});

test('speakable property: every number in the spoken text is a whole number of the answer (30 000 random answers)', () => {
  // Fragments of real answers, with punctuation and signs attached to the numbers at random, joined by spaces; fixed seed.
  const nums = ['12', '0', '05', '1.05', '10.050', '2,5', '-0.64', '−76.15', '+593', '99', '123', '4567', '000', '1 000', '10 000', '+5 000', '-10 000', '3 000 000', '12 000,5', '1 000–1 500', '1 500-2 000', '900–1 000', '10-15 000', '1 500/ha', '$1 500', '±1 500', '1 200 000…', '–5', '1  000 250', '1 200 050m', '0.6374', '76.1450', '9:05', '08:15', '10-15', '1200–1800', '2019-2024', '1/3', '1500m', '3km/h', '25°C', '50%', "0°38'S", 'CAM034567', 'CAM-123', 'B2', '2026-11-12', '12/11/2026'];
  const other = ['informe_2026.xlsx', 'https://x.org/a', 'www.inat.org', 'a@b.org', '`x`', '**', '*', '_', '~', '|', '#', '>', '<', '=', '✅', '🦋', '—', '\n', 'Hay', 'machos', 'fotos', 'Total', 'coordenadas', 'lat', 'y', 'de', 'km', 'Sur'];
  const pre = ['', '', '', '(', '«', '¿', '"', '*', '_', '+', '-'];
  const post = ['', '', '', ')', '»', '?', '.', ',', ';', ':', '*', '_', '%'];
  let seed = 12345;
  const rnd = n => ((seed = (seed * 1103515245 + 12345) % 2147483648), seed % n);
  const SP = '[ \\t\\u00A0\\u202F]';
  const GROUPED = new RegExp(`(?<!\\d[.,:\\-/]?|\\p{L}-?)[-−+]?[1-9]\\d{0,2}(?:${SP}\\d{3})+(?:[.,]\\d+)?(?!\\d)`, 'gu');
  const tokens = s => s.match(/(?<!\d|\d[.,:\-])[-−+]?\d+(?:[.,:\-]\d+)*/g) ?? [];
  const bad = [];
  for (let i = 0; i < 30000; i++) {
    const s = Array.from({ length: 2 + rnd(10) }, () => (rnd(2) ? pre[rnd(pre.length)] + nums[rnd(nums.length)] + post[rnd(post.length)] : other[rnd(other.length)])).join(' ');
    const t = s.replace(/(?<=\d)[~–](?=\d)/g, '-');
    // The numbers of the answer: whole tokens, and grouped thousands as one value; a group's head alone is not one of them.
    const whole = new Map();
    const add = x => whole.set(x.replace(/^[-−+]/, ''), (whole.get(x.replace(/^[-−+]/, '')) ?? 0) + 1);
    const ungrouped = t.replace(GROUPED, m => (add(m.replace(new RegExp(SP, 'g'), '')), ' '));
    for (const x of tokens(ungrouped)) add(x);
    // A group with no zero group and plain spaces may also be read as separate numbers («1 120»).
    for (const m of t.matchAll(GROUPED)) if (!/[  ]/.test(m[0]) && !/ 0/.test(m[0])) for (const x of tokens(m[0])) add(x);
    const out = speakable(s);
    const seen = new Map();
    for (const n of tokens(out)) {
      const key = n.replace(/^[-−+]/, '');
      seen.set(key, (seen.get(key) ?? 0) + 1);
      if (seen.get(key) > (whole.get(key) ?? 0)) bad.push([s, out, n]);
    }
  }
  assert.deepEqual(bad.slice(0, 5), []);
});

test('the shared word list: aliases and terms become the term, whole words only, wake rules untouched', () => {
  const file = new URL('../tools/whatsapp/dictation-words.json', import.meta.url);
  // The shipped list is lean (Claude only, Franz 14 Nov); the behaviour is tested on a fixture list.
  assert.deepEqual(loadWords([file]).map(w => w.term), ['Claude']);
  const words = [
    { term: 'Claude', aliases: ['Claudia', 'Cloud', 'Clod'] },
    { term: 'CAM', aliases: [] },
    { term: 'Ikiam', aliases: [] },
    { term: 'Oleria', aliases: ['olería'] },
    { term: 'María José Sánchez', aliases: ['Maria Jose Sanchez'] },
  ];
  assert.equal(parakeetText('claudia revisa la hiposcada del cam 234', words), 'Claude revisa la Hyposcada del CAM 234');
  assert.equal(applyWords('Cloud, ¿qué colectó maria josé sanchez en ikiam? 🦋 Olería y CAMARA', words), 'Claude, ¿qué colectó María José Sánchez en Ikiam? 🦋 Oleria y CAMARA');
  assert.equal(applyWords('Claudio y claudette, clodomiro', words), 'Claudio y claudette, clodomiro');
  assert.equal(applyWords('sin lista', []), 'sin lista');
  assert.deepEqual(loadWords(['/nonexistent/words.json']), []);
  // The bridge checks the wake word on the text before the word list (wakeText), so these stay as they were.
  assert.equal(voiceAddressed('ok cloud storage para las fotos'), false);
  assert.equal(voiceAddressed('claudia revisa la hiposcada', { personal: true }), false);
});
