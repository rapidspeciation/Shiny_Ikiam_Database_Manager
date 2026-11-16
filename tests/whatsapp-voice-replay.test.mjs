// The replay: real voice notes through the bridge's whole voice pipeline with the real Parakeet (this PC's eGPU), in dry
// run. Nothing leaves the PC and nothing reaches WhatsApp or an AI: the chats are dry runs (the outbox file), wacli is a
// stand-in that records any call (there must be none), the T3 turn is a stand-in script, and Scribe is a local stub
// server (the bridge's own Scribe code, pointed at it). Skipped unless WA_VOICE_REPLAY is set:
//   WA_VOICE_REPLAY=<folder with manifest.json and synth/truth.json> WA_VOICE_PARAKEET=<json: module, model, device> node --test tests/whatsapp-voice-replay.test.mjs
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, readFileSync, writeFileSync, existsSync, chmodSync } from 'node:fs';
import { createServer } from 'node:http';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { createBridge } from '../tools/whatsapp/bridge.mjs';

const REPLAY = process.env.WA_VOICE_REPLAY;

test('replay: real voice notes, real Parakeet, dry run (no WhatsApp, no AI, no ElevenLabs)', { skip: !REPLAY && 'set WA_VOICE_REPLAY', timeout: 600_000 }, async () => {
  const dir = mkdtempSync(join(tmpdir(), 'wa-voice-replay-'));
  const wacliCalls = join(dir, 'wacli-calls');
  const wacli = join(dir, 'wacli');
  writeFileSync(wacli, `#!/bin/sh\necho "$@" >> ${wacliCalls}\nexit 1\n`);
  chmodSync(wacli, 0o700);
  const prompts = join(dir, 'prompts');
  const script = join(dir, 'turn.mjs');
  writeFileSync(script, `import { appendFileSync } from 'node:fs'; let s='';for await (const c of process.stdin) s+=c; const r=JSON.parse(s); appendFileSync(${JSON.stringify(prompts)}, JSON.stringify(r.text)+'\\n'); console.log(JSON.stringify({ok:true,threadId:'T1',status:'completed',text:'Respuesta de prueba.'}));`);
  // The Scribe stub: answers «(scribe stub)» and counts the uploads.
  let scribeHits = 0;
  const server = createServer((req, res) => {
    scribeHits++;
    req.resume();
    req.on('end', () => res.end(JSON.stringify({ text: '(scribe stub)', language_code: 'spa' })));
  });
  await new Promise(r => server.listen(0, '127.0.0.1', r));
  const envFile = join(dir, 'el.env');
  writeFileSync(envFile, 'ELEVENLABS_API_KEY=stub\n');
  const config = {
    chats: {
      group: { jid: 'G@g.us', label: 'Grupo replay', route: 'r', enabled: true, dryRun: true, repliesHour: 100, contextLines: 15, ack: 'reaction', history: { dir: join(dir, 'hist') } },
      self: { jid: 'S@s.whatsapp.net', label: 'yo', route: 'r', enabled: true, dryRun: true, repliesHour: 100, contextLines: 0, ack: 'reaction' },
    },
    routes: { r: { script, attachmentsDir: join(dir, 'att') } },
    wacli: { bin: wacli, store: dir },
    voice: { parakeet: JSON.parse(process.env.WA_VOICE_PARAKEET), elevenlabsEnv: envFile, scribeUrl: `http://127.0.0.1:${server.address().port}/v1/speech-to-text` },
  };
  const bridge = createBridge({ config, stateDir: join(dir, 'state'), log: { error() {}, info() {} } });
  const files = [
    ...JSON.parse(readFileSync(join(REPLAY, 'manifest.json'), 'utf8')).map(m => ({ id: m.id, file: m.file })),
    ...JSON.parse(readFileSync(join(REPLAY, 'synth/truth.json'), 'utf8')).map(s => ({ id: s.id, file: join(REPLAY, 'synth', `${s.id}.ogg`) })),
  ];
  const settle = async () => {
    for (let i = 0; i < 2400 && !bridge.idle(); i++) await new Promise(r => setTimeout(r, 50));
    await new Promise(r => setTimeout(r, 300));
    for (let i = 0; i < 2400 && !bridge.idle(); i++) await new Promise(r => setTimeout(r, 50));
  };
  const t0 = Date.now();
  for (const [chat, jid] of [['group', 'G@g.us'], ['self', 'S@s.whatsapp.net']])
    for (const f of files)
      bridge.handle({ Chat: jid, ID: `${chat}-${f.id}`, SenderJID: chat === 'self' ? jid : 'P@s.whatsapp.net', Timestamp: new Date().toISOString(), FromMe: chat === 'self', Text: '', Media: { Type: 'ptt' }, PushName: chat === 'self' ? 'Franz' : 'Ana', _labMedia: f.file });
  await settle();
  await bridge.flushHistory();
  server.close();
  const state = bridge.state();
  const log = readFileSync(join(dir, 'state', 'bridge.log.jsonl'), 'utf8').trim().split('\n').map(l => JSON.parse(l));
  const outbox = existsSync(join(dir, 'state', 'outbox.jsonl')) ? readFileSync(join(dir, 'state', 'outbox.jsonl'), 'utf8').trim().split('\n').map(l => JSON.parse(l)) : [];
  const turns = existsSync(prompts) ? readFileSync(prompts, 'utf8').trim().split('\n').length : 0;
  const arrivals = log.filter(e => e.dir === 'voice' && e.on === 'arrival' && e.ok);
  const triggered = log.filter(e => e.decision === 'voice trigger').map(e => e.id);
  const summary = { notes: files.length * 2, arrivals: arrivals.length, engines: [...new Set(arrivals.map(e => e.engine))], triggered, turns, scribeHits, replies: outbox.filter(o => o.kind === 'reply').map(o => o.text.split('\n')[0]), ms: Date.now() - t0, wacliCalls: existsSync(wacliCalls) ? readFileSync(wacliCalls, 'utf8') : '' };
  writeFileSync(join(process.env.WA_VOICE_REPLAY_OUT ?? dir, 'replay-summary.json'), JSON.stringify({ ...summary, transcripts: Object.fromEntries(Object.entries(state.transcripts).map(([k, v]) => [k, { engine: state.voiceMeta[k]?.engine, text: v }])) }, null, 1), { mode: 0o600 });
  console.log(JSON.stringify(summary, null, 1));
  // Every note transcribed once on arrival, by Parakeet.
  assert.equal(arrivals.length, files.length * 2);
  assert.deepEqual(summary.engines, ['parakeet']);
  // Nothing went to WhatsApp (no wacli call at all) or to a real model (turns only through the stand-in).
  assert.equal(summary.wacliCalls, '');
  assert.ok(outbox.every(o => o.chat === 'group' || o.chat === 'self'));
  // Each trigger had one turn and one Scribe upgrade (to the stub), and its reply starts with the «🎙️» line.
  assert.equal(turns, triggered.length);
  assert.equal(scribeHits, triggered.length);
  assert.ok(summary.replies.every(r => r.startsWith('Claude: 🎙️ «')));
  // The synthetic notes: the ones that open with «Claude» / «Oye Claude» wake both chats; S8 (Claude as the 7th word) only Franz's.
  for (const id of ['S1', 'S2', 'S4', 'S6']) assert.ok(triggered.includes(`group-${id}`) && triggered.includes(`self-${id}`), id);
  assert.ok(!triggered.includes('group-S8') && triggered.includes('self-S8'));
  for (const id of ['S3', 'S5', 'S7']) assert.ok(!triggered.includes(`group-${id}`) && !triggered.includes(`self-${id}`), id);
  // The group's transcripts are in its history file.
  const hist = readFileSync(join(dir, 'hist', 'grupo-replay.jsonl'), 'utf8').trim().split('\n').map(l => JSON.parse(l));
  assert.equal(hist.filter(l => l.t === 'transcript' && l.engine === 'parakeet').length, files.length);
});
