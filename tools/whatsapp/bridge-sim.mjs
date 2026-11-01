#!/usr/bin/env node
// Posts WhatsApp messages to the running bridge (tools/whatsapp/bridge.mjs) as wacli would, signed with
// its secret, one after another, and prints what each one led to: the bridge's log lines and, for the
// lab's dry-run chat, its outbox. For the lab run before going live; nothing reaches WhatsApp from
// a dry-run chat.
//   node tools/whatsapp/bridge-sim.mjs <cases.json> <new output folder>
// A case: { name, chat: "<jid>", fromMe?, sender?, pushName?, text, media?: "/abs/photo.jpg" (dry-run
// chats only), replyTo?, waitTurn?: true (wait for the bridge to be idle afterwards) }.
import { readFileSync, writeFileSync, mkdirSync, existsSync, statSync } from 'node:fs';
import { homedir } from 'node:os';
import { join } from 'node:path';
import { randomBytes } from 'node:crypto';
import { sign } from './bridge-lib.mjs';

const [casesFile, outDir] = process.argv.slice(2);
if (!casesFile || !outDir) throw new Error('Usage: bridge-sim.mjs <cases.json> <new output folder>');
if (existsSync(outDir)) throw new Error('The output folder must be new');
mkdirSync(outDir, { recursive: true, mode: 0o700 });
const config = JSON.parse(readFileSync(process.env.WA_BRIDGE_CONFIG || join(homedir(), '.config/ithomiini-wa-bridge/config.json'), 'utf8'));
const secret = readFileSync(config.secretFile, 'utf8').trim();
const stateDir = process.env.WA_BRIDGE_STATE || join(homedir(), '.local/state/ithomiini-wa-bridge');
const base = `http://127.0.0.1:${config.port}`;
const size = f => (existsSync(f) ? statSync(f).size : 0);
const tail = (f, from) => (existsSync(f) ? readFileSync(f).subarray(from).toString('utf8').split('\n').filter(Boolean).map(l => JSON.parse(l)) : []);
const sleep = ms => new Promise(r => setTimeout(r, ms));

const results = [];
for (const c of JSON.parse(readFileSync(casesFile, 'utf8'))) {
  const logAt = size(join(stateDir, 'bridge.log.jsonl'));
  const outAt = size(join(stateDir, 'outbox.jsonl'));
  const id = 'SIM' + randomBytes(8).toString('hex').toUpperCase();
  const body = {
    Chat: c.chat,
    ID: id,
    SenderJID: c.sender ?? (c.fromMe ? config.chats.self.jid : '593900000001@s.whatsapp.net'),
    Timestamp: new Date().toISOString(),
    FromMe: Boolean(c.fromMe),
    Text: c.text,
    PushName: c.pushName ?? null,
    ReplyToID: c.replyTo ?? '',
    Media: c.media ? { Type: /\.(ogg|opus)$/i.test(c.media) ? 'audio' : 'image' } : null,
    ...(c.media ? { _labMedia: c.media } : {}),
  };
  const raw = Buffer.from(JSON.stringify(body));
  const started = Date.now();
  const res = await fetch(`${base}/wacli`, { method: 'POST', headers: { 'content-type': 'application/json', 'x-wacli-signature': sign(secret, raw) }, body: raw });
  await sleep(1500);
  if (c.waitTurn)
    for (let i = 0; i < 600; i++) {
      const h = await (await fetch(`${base}/health`)).json();
      if (h.idle) break;
      await sleep(2000);
    }
  const r = { name: c.name, status: res.status, seconds: Math.round((Date.now() - started) / 1000), log: tail(join(stateDir, 'bridge.log.jsonl'), logAt), outbox: tail(join(stateDir, 'outbox.jsonl'), outAt) };
  results.push(r);
  console.log(`\n## ${c.name} (${r.seconds} s)`);
  for (const l of r.log) console.log('  log', JSON.stringify(l));
  for (const o of r.outbox) console.log('  out', o.kind, o.file ? `[file ${o.file}]` : '', JSON.stringify(o.text ?? '').slice(0, 700));
}
writeFileSync(join(outDir, 'results.json'), JSON.stringify(results, null, 2), { mode: 0o600 });
