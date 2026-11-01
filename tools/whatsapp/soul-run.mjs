#!/usr/bin/env node
// Runs WhatsApp scenarios through the bridge (createBridge, dry-run chats only) on the lab T3 (the config's `labsim`
// route): each scenario gets its own state folder, so its own new chat, and nothing goes to WhatsApp. At most 4 chats
// run at once, and each chat is archived as soon as its answer is in (also after a failure or a timeout): T3 keeps an
// idle chat's Claude process for 30 minutes, so a burst of open chats fills this PC's memory.
//   node tools/whatsapp/soul-run.mjs <scenarios.json> <soul.md> <new output folder> [--round r0] [--tono normal] [--only S01,S02] [--parallel 4]
import { readFileSync, writeFileSync, mkdirSync, existsSync, chmodSync } from 'node:fs';
import { spawn } from 'node:child_process';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, '..', '..');
export const MAX_CHATS = 4;
const sleep = ms => new Promise(r => setTimeout(r, ms));

/** The --parallel value as a number of chats at once: 1 to MAX_CHATS, 4 when missing or unreadable. */
export const chatLimit = value => {
  const n = Math.floor(Number(value ?? MAX_CHATS));
  return Number.isFinite(n) ? Math.min(MAX_CHATS, Math.max(1, n)) : MAX_CHATS;
};

/** work(item) for every item, at most `size` at once (size is capped at MAX_CHATS); results in the items' order. */
export async function runPool(items, size, work) {
  const results = new Array(items.length);
  let next = 0;
  const worker = async () => {
    while (next < items.length) {
      const i = next++;
      results[i] = await work(items[i], i);
    }
  };
  await Promise.all(Array.from({ length: Math.min(chatLimit(size), items.length) }, worker));
  return results;
}

/** Runs run(), then archives the chat it made (threadId() after the run), whether run() returned or threw. */
export async function withArchive(run, threadId, archive, log = console) {
  try {
    return await run();
  } finally {
    const id = threadId();
    if (id) await archive(id).catch(e => log.error?.(`archive ${id}: ${e.message}`));
  }
}

/** Archives one chat through t3-turn.mjs's `archive` op on the route's T3. */
export function archiveThread(route, threadId) {
  return new Promise((done, fail) => {
    const child = spawn(process.execPath, [route.script], { stdio: ['pipe', 'pipe', 'ignore'] });
    let out = '';
    child.stdout.on('data', d => (out += d));
    child.on('error', fail);
    child.on('close', () => {
      try {
        const r = JSON.parse(out.trim().split('\n').pop());
        r.ok ? done(r) : fail(new Error(r.error ?? 'not archived'));
      } catch {
        fail(new Error('no answer from t3-turn'));
      }
    });
    child.stdin.end(JSON.stringify({ op: 'archive', home: route.home, local: route.local, tokenFile: route.tokenFile, commandsModule: route.commandsModule, threadId }));
  });
}

// Invented first names for the roles, so the chat sees names as it would on WhatsApp (the review page shows roles).
const NAMES = { 'técnica de insectario': 'Daniela', 'técnico B': 'Mateo', 'técnica C': 'Valeria', coordinadora: 'Paola', 'jefa de insectario': 'Lucía', 'investigador principal': 'Tom', 'colaboradora de Ikiam': 'Gabriela' };
const nameOf = role => NAMES[role] ?? role;

/** One scenario: posts its earlier lines and its message, waits for the turn (at most 15 min), reads the outbox; the chat is archived afterwards. */
export async function runScenario(s, { outDir, soulFile, round, tono, lab, createBridge, archive = archiveThread, waitMs = 900_000 }) {
  const stateDir = join(outDir, `state-${s.id}-${tono}`);
  const jid = s.chat === 'self' ? 'SOULSELF@s.whatsapp.net' : 'SOULLAB@g.us';
  const route = {
    home: lab.home,
    local: lab.local,
    tokenFile: lab.tokenFile,
    commandsModule: join(ROOT, 'server/t3commands.mjs'),
    script: join(HERE, 't3-turn.mjs'),
    projectId: lab.projectId,
    title: `WA soul lab ${round} ${s.id} ${tono}`,
    model: 'claude-opus-5-5',
    effort: 'medium',
    attachmentsDir: lab.attachmentsDir,
    rackReportsDir: lab.rackReportsDir,
  };
  const config = {
    soulFile,
    chats: { [s.chat]: { jid, label: s.chat === 'self' ? 'yo' : 'Ithomiini 🦋 (underground) [lab]', route: 'soul', enabled: true, dryRun: true, repliesHour: 60, contextLines: 15, tono } },
    routes: { soul: route },
    wacli: { bin: '/bin/false', store: stateDir },
  };
  const bridge = createBridge({ config, stateDir, log: { error: e => console.error(s.id, e), info() {} } });
  const t0 = Date.now();
  return withArchive(
    async () => {
      const ids = [];
      const post = (m, minsAgo) => {
        const id = `${s.id}${ids.length}${Math.random().toString(16).slice(2, 8)}`.toUpperCase();
        ids.push(id);
        const self = s.chat === 'self';
        bridge.handle({
          Chat: jid,
          ID: id,
          SenderJID: m.bot || self ? jid : 'P1@s.whatsapp.net',
          Timestamp: new Date(t0 - minsAgo * 60_000).toISOString(),
          FromMe: self && !m.bot,
          Text: m.text,
          PushName: m.bot ? 'Claude (you)' : nameOf(m.name ?? s.sender),
          ReplyToID: m.replyTo != null ? ids[m.replyTo] : '',
          Media: m.media ? { Type: 'image' } : null,
          ...(m.media ? { _labMedia: m.media } : {}),
        });
      };
      // Earlier lines are context only: their @claude was answered back then, so it is dropped here.
      (s.preceding ?? []).forEach((m, i) => post({ ...m, text: m.text.replace(/@claude\s*/gi, '') }, s.preceding.length - i + 1));
      post({ text: s.message, name: s.sender, media: s.media, replyTo: s.replyTo }, 0);
      await sleep(Math.min(500, waitMs));
      while (!bridge.idle() && Date.now() - t0 < waitMs) await sleep(Math.min(1000, waitMs));
      const lines = f => (existsSync(join(stateDir, f)) ? readFileSync(join(stateDir, f), 'utf8').trim().split('\n').filter(Boolean).map(l => JSON.parse(l)) : []);
      const turns = lines('bridge.log.jsonl').filter(l => l.dir === 'turn');
      const sent = lines('outbox.jsonl').filter(o => o.kind !== 'react');
      const timedOut = !bridge.idle();
      const r = {
        id: s.id,
        tono,
        reply: sent.filter(o => o.kind === 'reply' || o.kind === 'notice').map(o => o.text).join('\n\n') || (timedOut ? '[timeout]' : '[silent]'),
        extra: sent.filter(o => !['reply', 'notice'].includes(o.kind)).map(o => `[${o.kind}${o.text ? ': ' + o.text : ''}]`),
        seconds: Math.round((Date.now() - t0) / 1000),
        ok: !timedOut && turns.every(t => t.ok),
        steps: turns.reduce((a, t) => a + (t.steps ?? 0), 0),
      };
      console.log(`${s.id} ${tono} ${r.seconds}s ok=${r.ok}`);
      return r;
    },
    () => bridge.state().threads?.[s.chat] ?? null,
    id => archive(route, id),
  );
}

async function main(args) {
  const opt = (k, d) => (args.includes(k) ? args[args.indexOf(k) + 1] : d);
  const [scenFile, soulFile, outDir] = args;
  if (!outDir || existsSync(outDir)) throw new Error('Usage: soul-run.mjs <scenarios.json> <soul.md> <new output folder> [...]');
  mkdirSync(outDir, { recursive: true, mode: 0o700 });
  const only = opt('--only', null)?.split(',');
  const lab = JSON.parse(readFileSync(process.env.WA_BRIDGE_CONFIG || join(process.env.HOME, '.config/ithomiini-wa-bridge/config.json'), 'utf8')).routes.labsim;
  const { createBridge } = await import(join(HERE, 'bridge.mjs'));
  const scenarios = JSON.parse(readFileSync(scenFile, 'utf8')).filter(s => !only || only.includes(s.id));
  const options = { outDir, soulFile, round: opt('--round', 'r'), tono: opt('--tono', 'normal'), lab, createBridge };
  const results = await runPool(scenarios, chatLimit(opt('--parallel', MAX_CHATS)), s => runScenario(s, options));
  results.sort((a, b) => a.id.localeCompare(b.id));
  writeFileSync(join(outDir, 'results.json'), JSON.stringify(results, null, 1));
  chmodSync(join(outDir, 'results.json'), 0o600);
  console.log('done', results.length);
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url))
  main(process.argv.slice(2)).then(
    () => process.exit(0),
    e => (console.error(e.message), process.exit(1)),
  );
