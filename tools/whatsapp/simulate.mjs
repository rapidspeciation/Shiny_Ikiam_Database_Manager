#!/usr/bin/env node
// Feeds WhatsApp test cases through the relay of the LAB app (server/whatsapp.mjs) as wacli would,
// and collects what the WhatsApp chat answered (the relay's outbox) or "no reply".
//   node tools/whatsapp/simulate.mjs <cases.jsonl> <new output folder> [--setup setup.json] [--fresh-thread] [--timeout 300]
// A case: { id, chat: "dm" | "<group>@g.us", chatName?, sender: "+593…", senderName?,
//   preceding?: [{ sender, senderName?, text }], message, image?: "/abs/path.jpg", quoted?: "out:<n>" }.
// setup.json: { people: [{ phone, name, role?, enabled? }], groups: [{ jid, name, enabled, mode? }] } (the whitelist).
// Writes <folder>/results.jsonl (one line per case) and <folder>/run.json.
// Only the lab: http://127.0.0.1:8795, whose relay says it is the lab and not on the team's workbook.
import { createHmac } from 'node:crypto';
import { existsSync, mkdirSync, readFileSync, readdirSync, writeFileSync, appendFileSync } from 'node:fs';
import { join, resolve, isAbsolute } from 'node:path';
import { homedir } from 'node:os';

const LAB_BASES = new Set(['http://127.0.0.1:8795', 'http://localhost:8795']);
const FINAL = new Set(['dropped', 'context', 'limited', 'handled', 'ignored']);

function usage(message) {
  if (message) console.error(message);
  console.error('Usage: simulate.mjs <cases.jsonl> <new output folder> [--setup setup.json] [--fresh-thread] [--timeout seconds] [--base http://127.0.0.1:8795]');
  process.exit(2);
}
const args = process.argv.slice(2);
const flag = name => {
  const i = args.indexOf(name);
  if (i < 0) return null;
  const value = args[i + 1];
  args.splice(i, 2);
  return value;
};
const fresh = args.includes('--fresh-thread');
if (fresh) args.splice(args.indexOf('--fresh-thread'), 1);
const setupFile = flag('--setup');
const timeoutS = Number(flag('--timeout') ?? 300);
const base = (flag('--base') ?? 'http://127.0.0.1:8795').replace(/\/+$/, '');
const [casesFile, outDir] = args;
if (!casesFile || !outDir) usage();
if (!LAB_BASES.has(base)) usage(`Refused: ${base} is not the lab app (http://127.0.0.1:8795).`);
if (!isAbsolute(outDir)) usage('Give the output folder as an absolute path.');
if (existsSync(outDir) && readdirSync(outDir).length) usage(`Refused: ${outDir} is not empty; give a new folder.`);

const lab = process.env.ITHOMIINI_LAB_DIR || join(homedir(), '.cache/ithomiini-lab');
const secret = readFileSync(join(lab, 'wa-webhook-secret'), 'utf8').trim();
const sign = buf => 'sha256=' + createHmac('sha256', secret).update(buf).digest('hex');

async function call(method, path, body) {
  const raw = body === undefined ? Buffer.alloc(0) : Buffer.from(JSON.stringify(body));
  const signed = path === '/api/wa/inbound' ? raw : Buffer.concat([Buffer.from(`${method} ${path}\n`), raw]);
  const r = await fetch(base + path, {
    method,
    headers: { 'x-wacli-signature': sign(signed), ...(body === undefined ? {} : { 'content-type': 'application/json' }) },
    ...(body === undefined ? {} : { body: raw }),
  });
  const text = await r.text();
  let json = null;
  try {
    json = JSON.parse(text);
  } catch {
    /* kept as text */
  }
  return { status: r.status, json, text };
}

const info = await call('GET', '/api/wa/info');
if (info.status !== 200 || !info.json?.lab || info.json.realWorkbook !== false)
  usage(`Refused: ${base} did not answer as the lab's relay (status ${info.status}, lab ${info.json?.lab}, real workbook ${info.json?.realWorkbook}).`);

const cases = readFileSync(resolve(casesFile), 'utf8')
  .split('\n')
  .filter(l => l.trim())
  .map((l, i) => {
    try {
      return JSON.parse(l);
    } catch {
      usage(`Line ${i + 1} of ${casesFile} is not JSON.`);
    }
  });
mkdirSync(outDir, { recursive: true });
const run = new Date().toISOString().replace(/[-:]/g, '').replace(/\.\d+Z$/, 'Z');
if (setupFile || fresh) {
  const setup = setupFile ? JSON.parse(readFileSync(resolve(setupFile), 'utf8')) : {};
  const out = await call('POST', '/api/wa/setup', { ...setup, resetThread: fresh });
  if (out.status !== 200) usage(`Setup failed: ${out.status} ${out.text.slice(0, 200)}`);
  console.log(`Whitelist: ${out.json.people} people, ${out.json.groups} groups${fresh ? '; a new WhatsApp chat on the next wake' : ''}`);
}
writeFileSync(join(outDir, 'run.json'), JSON.stringify({ run, base, cases: cases.length, casesFile: resolve(casesFile), setupFile: setupFile && resolve(setupFile), freshThread: fresh, startedAt: new Date().toISOString() }, null, 1));

const jidOf = phone => String(phone).replace(/^\+/, '') + '@s.whatsapp.net';
const sleep = ms => new Promise(r => setTimeout(r, ms));
const results = join(outDir, 'results.jsonl');
for (const c of cases) {
  const started = Date.now();
  const chat = !c.chat || c.chat === 'dm' ? jidOf(c.sender) : c.chat;
  const isGroup = chat.endsWith('@g.us');
  let k = 0;
  const event = (m, at) => ({
    msg_id: `sim-${run}-${c.id}-${++k}`,
    chat_jid: chat,
    chat_name: isGroup ? (c.chatName ?? null) : null,
    sender_jid: jidOf(m.sender ?? c.sender),
    sender_name: m.senderName ?? null,
    text: m.text ?? '',
    timestamp: Math.floor(at / 1000),
    ...(m.mentions ? { mentions: m.mentions } : {}),
    ...(m.quoted ? { quoted_id: m.quoted } : {}),
    ...(m.image ? { media_type: 'image', local_path: resolve(m.image) } : {}),
  });
  const preceding = c.preceding ?? [];
  // Every message of the case is followed, the preceding ones too: one of them may wake the chat
  // (an unanswered question in a `helpful` group), and its answer belongs to this case.
  const posted = [];
  for (const [i, p] of preceding.entries()) {
    const e = event(p, Date.now() - (preceding.length - i) * 60_000);
    posted.push({ e, r: await call('POST', '/api/wa/inbound', e) });
  }
  const last = event({ sender: c.sender, senderName: c.senderName, text: c.message, image: c.image, quoted: c.quoted, mentions: c.mentions }, Date.now());
  posted.push({ e: last, r: await call('POST', '/api/wa/inbound', last) });
  const done = s => s && (FINAL.has(s.status) || ((s.status === 'answered' || s.status === 'unanswered') && s.turnDone));
  let states = [];
  let outcome = 'timeout';
  while (Date.now() - started < timeoutS * 1000) {
    await sleep(3000);
    states = await Promise.all(posted.map(async p => (await call('GET', `/api/wa/messages/${encodeURIComponent(p.e.msg_id)}`)).json));
    if (states.every(done)) {
      outcome = states.at(-1).status;
      break;
    }
  }
  const state = states.at(-1) ?? null;
  const seen = new Set();
  const replies = states
    .flatMap(s => s?.replies ?? [])
    .filter(r => !seen.has(r.n) && seen.add(r.n))
    .map(r => ({ text: r.text, file: r.file, by: r.by, replyTo: r.reply_n ? `#m${r.reply_n}` : null }));
  const woken = states.filter(s => s?.wake).map(s => ({ id: s.id, wake: s.wake, status: s.status }));
  const line = {
    id: c.id,
    chat: isGroup ? 'group' : 'dm',
    message: state?.id ?? null,
    decision: posted.at(-1).r.json?.results?.[0]?.decision ?? `http ${posted.at(-1).r.status}`,
    reason: state?.reason ?? posted.at(-1).r.json?.results?.[0]?.reason ?? null,
    woken,
    outcome,
    reply: replies.length ? replies.map(r => r.text).join('\n---\n') : 'no reply',
    replies,
    proposalLinks: [...new Set(replies.flatMap(r => r.text.match(/\S*#\/propuestas\/[0-9a-f-]{36}/g) ?? []))],
    seconds: Math.round((Date.now() - started) / 1000),
  };
  appendFileSync(results, JSON.stringify(line) + '\n');
  console.log(`${c.id}: ${line.decision} → ${outcome}${replies.length ? `, ${replies.length} repl${replies.length > 1 ? 'ies' : 'y'}` : ', no reply'} (${line.seconds} s)`);
}
console.log(`Results: ${results}`);
