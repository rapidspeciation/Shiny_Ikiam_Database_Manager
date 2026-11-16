#!/usr/bin/env node
// The live WhatsApp bridge on Franz's own WhatsApp (wacli on this PC). wacli's sync posts every
// live message to this receiver (127.0.0.1, HMAC-signed: `wacli sync --webhook … --webhook-secret …
// --webhook-allow-private`). In order, before anything else:
//   1. the signature; then the chat: only the chats in the config (the group «Ithomiini 🦋
//      (underground)» and Franz's own chat). Every other message is dropped here, with nothing of it
//      kept or logged but the fact;
//   2. old, duplicate, edited, reaction and deleted messages are skipped; a message starting with
//      «Claude:» is the bridge's own (loop guard);
//   3. the relay's commands (/help, /status, /whoami, /usage, /stop, /start) answered without a model;
//      /stop from Franz switches everything off until his /start (kept in the state file);
//   4. only a message with @claude goes on; the hourly reply limit per chat;
//   5. one turn at a time per chat: «Claude: buscando…» at once, the message (with the group's last
//      messages, photos and voice notes transcribed) to the chat's T3 chat, the turn's last message
//      back to WhatsApp, starting with «Claude: », with its pictures and, when asked, a voice note.
// Group → the T3 project «Ithomiini 🦋 (underground)» on claudeclaw (production tools; each turn says whether the sender
// is an app admin, and guests get proposals: bridge skill, «Who is asking»), over ssh with tools/whatsapp/t3-turn.mjs. Franz's chat → «Orchestrator wpp»
// in this PC's T3, in the project on the lab's workspace (the lab app's tools).
// Sends go through `wacli send`, and only to the group's and Franz's own JID (hard block in send()).
// In the group the bridge never speaks first: a conversation opens with an @claude message (or a reply
// to one of the bridge's messages) and stays active 15 minutes from the last message Claude answered or
// was replied to. Inside it, untagged messages go to the chat marked as follow-ups, and the chat answers
// [silent] unless one is addressed to Claude. Outside it nothing is sent and no turn starts; send()
// refuses a group send that names no incoming message or comes outside an active conversation.
//
//   node tools/whatsapp/bridge.mjs serve            the receiver (systemd: ithomiini-wa-bridge.service)
//   node tools/whatsapp/bridge.mjs backfill [days]  copy the group's last days (default 30) into its history file on claudeclaw
//   node tools/whatsapp/bridge.mjs install          copy the bridge to ~/.local/share/ithomiini-wa-bridge and write the unit
// Config: $WA_BRIDGE_CONFIG (default ~/.config/ithomiini-wa-bridge/config.json, mode 600); state and
// logs in ~/.local/state/ithomiini-wa-bridge. Design and test notes: tools/whatsapp/README.md.
import { createServer } from 'node:http';
import { execFile, spawn } from 'node:child_process';
import { copyFileSync, realpathSync, existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync, statSync, writeFileSync, appendFileSync, renameSync, chmodSync } from 'node:fs';
import { homedir, tmpdir } from 'node:os';
import { basename, dirname, isAbsolute, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { DatabaseSync } from 'node:sqlite';
import { createHash, randomUUID, scryptSync } from 'node:crypto';
import { appendNew, mediaTag, named, slug, stripHarnessBlocks, stripPreamble, ACK_MODES, TONES, KEYTERMS, PREFIX, bareJid, command, decryptMedia, fixGenera, imageKind, loadWords, parakeetText, isVoice, lastHour, ownAnswer, parsePayload, parseReply, speakable, statusMessage, stepLabel, tagged, toWhatsApp, verify, voiceAddressed, voiceLanguage } from './bridge-lib.mjs';

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, '..', '..');
const HOME = homedir();
const CONFIG_FILE = process.env.WA_BRIDGE_CONFIG || join(HOME, '.config/ithomiini-wa-bridge/config.json');
const STATE_DIR = process.env.WA_BRIDGE_STATE || join(HOME, '.local/state/ithomiini-wa-bridge');
const ECUADOR_MS = -5 * 3_600_000;
const STALE_MS = 15 * 60_000;
const sleep = ms => new Promise(r => setTimeout(r, ms));
const hhmm = t => new Date(t + ECUADOR_MS).toISOString().slice(11, 16);
const clip = (s, n) => (s == null ? '' : String(s).slice(0, n));
const dmhm = t => { const d = new Date(t + ECUADOR_MS).toISOString(); return `${d.slice(8, 10)}/${d.slice(5, 7)} ${d.slice(11, 16)}`; };
const CONTEXT_CAP = 40;
const IMAGE_PARALLEL = Number(process.env.WA_IMAGE_PARALLEL) || 3;
const CONTEXT_MS = 2 * 3_600_000;

/** One message as a history line (a WhatsApp row or a webhook message, both reduced to the same fields). */
const historyLine = ({ id, at, name, text, media, replyTo = null, replyName = null, replyText = null }) => ({
  id: String(id),
  at: new Date(at).toISOString(),
  name: name ?? null,
  text: clip(text, 3000) || null,
  media: mediaTag(media),
  ...(replyTo ? { replyTo, replyName, replyText: clip(replyText, 120) || null } : {}),
});

/**
 * Chats nothing is ever sent to, whatever the config says (Franz, 4 Nov 2026): «Ithomiini Ikiam», the team's group with
 * the UK side. Its history may be read (a read-only source); the agenda is shared there by Franz himself, never by the bridge.
 * Kept as slow (scrypt) hashes of the bare JID, so this public file holds no JID; each JID is hashed once.
 */
const NEVER_SEND_HASHES = new Set(['57a6516ac97d641afbd07a273ef89dcf31aa9d64dabeef2cd585fc3ddf784188']);
const neverSendSeen = new Map();
export const neverSend = jid => {
  const bare = bareJid(jid ?? '');
  if (!neverSendSeen.has(bare))
    neverSendSeen.set(bare, NEVER_SEND_HASHES.has(scryptSync(bare, 'ithomiini-wa-never-send', 32, { N: 1 << 15, r: 8, p: 1, maxmem: 64 << 20 }).toString('hex')));
  return neverSendSeen.get(bare);
};

/**
 * Where a chat's history is mirrored: `history` is one { dir, slug? } on the chat's route, or a list of
 * { route?, dir, slug? } (a read-only source mirrored into several workspaces). Only targets with a dir.
 */
export const historyTargets = (config, chat) =>
  (Array.isArray(chat.history) ? chat.history : chat.history ? [chat.history] : [])
    .filter(h => h?.dir)
    .map(h => ({ routeKey: h.route ?? chat.route, route: config.routes?.[h.route ?? chat.route] ?? {}, history: h }));
/** The history file's name in a workspace (wa-history/<slug>.jsonl). */
export const historySlug = (chat, h) => h?.slug ?? slug(chat.label);

/** Appends lines (JSON, one per line) to a chat's history file, on claudeclaw over ssh or in a local folder. */
async function historyAppend(route, history, chat, lines) {
  const file = join(history.dir, `${history.slug ?? slug(chat.label)}.jsonl`);
  const text = lines.map(x => JSON.stringify(x)).join('\n');
  if (!route.ssh) return appendNew(file, text);
  const src = `${appendNew.toString()};const fs=process.getBuiltinModule('node:fs');process.stdout.write(String(appendNew(process.argv[1],fs.readFileSync(0,'utf8'))));`;
  const out = await run('ssh', ['-o', 'BatchMode=yes', route.ssh, `${route.node ?? 'node'} -e "eval(Buffer.from('${Buffer.from(src).toString('base64')}','base64').toString())" '${file.replace(/'/g, '')}'`], { input: text, timeout: 60_000 });
  return Number(out) || 0;
}

/** The history rows of a chat from the wacli store (read-only), oldest first. */
function historyRows(config, chat, sinceMs) {
  const db = new DatabaseSync(join(config.wacli.store, 'wacli.db'), { readOnly: true, timeout: 5000 });
  try {
    return db
      .prepare(
        `SELECT m.msg_id id, m.ts, m.from_me, m.sender_name, coalesce(m.text, m.display_text, m.media_caption, '') text, m.media_type,
           m.quoted_msg_id,
           (SELECT coalesce(nullif(c.full_name, ''), nullif(c.push_name, '')) FROM contacts c WHERE c.jid = m.sender_jid) contact,
           (SELECT coalesce(q.text, q.display_text, q.media_caption, '') FROM messages q WHERE q.chat_jid = m.chat_jid AND q.msg_id = m.quoted_msg_id) qtext,
           (SELECT q.sender_name FROM messages q WHERE q.chat_jid = m.chat_jid AND q.msg_id = m.quoted_msg_id) qname
         FROM messages m WHERE m.chat_jid = ? AND m.ts >= ? AND m.revoked = 0 AND m.reaction_to_id IS NULL ORDER BY m.ts`,
      )
      .all(chat.jid, Math.floor(sinceMs / 1000))
      .map(r => ({ ...r, name: r.from_me ? (ownAnswer(r.text) ? 'Claude' : 'Franz') : r.sender_name || r.contact || null }));
  } finally {
    db.close();
  }
}

function run(bin, args, { input = null, timeout = 120_000, env = process.env } = {}) {
  return new Promise((resolve, reject) => {
    const child = execFile(bin, args, { timeout, env, maxBuffer: 16 * 1024 * 1024 }, (e, stdout, stderr) =>
      e ? reject(Object.assign(new Error(clip(String(stderr || e.message).trim(), 300)), { stdout })) : resolve(stdout),
    );
    if (input != null) child.stdin.end(input);
  });
}

export function createBridge({ config, stateDir = STATE_DIR, now = Date.now, log = console, engines = {} }) {
  mkdirSync(stateDir, { recursive: true, mode: 0o700 });
  const stateFile = join(stateDir, 'state.json');
  const logFile = join(stateDir, 'bridge.log.jsonl');
  const outboxFile = join(stateDir, 'outbox.jsonl');
  let state = { stopped: false, threads: {}, sends: {}, seen: [], lastTurn: {}, botIds: [], transcripts: {}, voiceMeta: {} };
  try {
    state = { ...state, ...JSON.parse(readFileSync(stateFile, 'utf8')) };
  } catch {
    /* first start */
  }
  const save = () => {
    state.seen = state.seen.slice(-500);
    state.botIds = (state.botIds ?? []).slice(-500);
    state.transcripts = Object.fromEntries(Object.entries(state.transcripts ?? {}).slice(-300));
    state.voiceMeta = Object.fromEntries(Object.entries(state.voiceMeta ?? {}).filter(([id]) => id in state.transcripts));
    writeFileSync(stateFile + '.tmp', JSON.stringify(state, null, 1), { mode: 0o600 });
    renameSync(stateFile + '.tmp', stateFile);
  };
  const record = entry => {
    try {
      appendFileSync(logFile, JSON.stringify({ at: new Date(now()).toISOString(), ...entry }) + '\n', { mode: 0o600 });
    } catch (e) {
      log.error?.('log:', e.message);
    }
  };

  const chats = Object.entries(config.chats ?? {}).map(([key, c]) => ({ key, ...c }));
  // A chat by its JID, or another JID WhatsApp gives it (Franz's own chat may come under his LID).
  const chatOf = jid => chats.find(c => (c.jid === bareJid(jid) || (c.aliases ?? []).includes(bareJid(jid))) && c.enabled) ?? null;
  // The hard block: wacli sends only to these two JIDs, whatever the config's other chats say.
  const SENDABLE = new Set([config.chats?.group?.jid, config.chats?.self?.jid].filter(Boolean));
  // A read-only source (`readOnly: true`, e.g. «Ithomiini Ikiam»): its messages are only mirrored into the history files of
  // the workspaces named in its `history`. It never starts a turn, answers no command and is never sent to (send() refuses it
  // too). It may not be the group or the self chat, nor share their JID.
  for (const c of chats)
    if (c.readOnly && (c.key === 'group' || c.key === 'self' || SENDABLE.has(c.jid) || !historyTargets(config, c).length))
      throw new Error(`Chat ${c.key}: a read-only source must be its own chat, with a history file`);
  for (const c of chats)
    if (neverSend(c.jid) && !c.readOnly) throw new Error(`Chat ${c.key}: «${c.label}» can only be a read-only source`);
  // Only these routes make real sends: Franz's own chat through his WhatsApp project on claudeclaw (route claudeclaw-franz-wa,
  // «Franz · WhatsApp», since 4 Nov 2026 evening; its voice in the workspace), his app project (route claudeclaw-franz, the
  // rollback) or «Orchestrator wpp» (route local, the main T3); the group through claudeclaw. Any other
  // chat or route (the lab's simulations, a lab T3 route, a renamed one) goes to the outbox, whatever its dryRun says, so a test
  // can never reach WhatsApp.
  const LIVE_ROUTES = { self: ['claudeclaw-franz-wa', 'claudeclaw-franz', 'local'], group: ['claudeclaw'] };
  const isLive = chat => (LIVE_ROUTES[chat.key] ?? []).includes(chat.route) && !/ithomiini-lab|:3775\b/.test(`${config.routes?.[chat.route]?.home ?? ''} ${config.routes?.[chat.route]?.local ?? ''}`);
  // A chat's T3 thread belongs to the route it was made on. When a chat moves to another route, its thread there (if it had
  // one) comes back, the old one is kept in `parkedThreads` for a move back, and the voice is sent again: the new thread starts
  // with the skill and the soul. State from before this rule (no `threadRoutes`) counts as made on the chat's current route.
  state.threadRoutes ??= {};
  for (const c of chats) if (state.threads?.[c.key] && !state.threadRoutes[c.key]) state.threadRoutes[c.key] = c.route;
  function followRoute(chat) {
    const was = state.threadRoutes[chat.key];
    if (!was || was === chat.route) return (state.threadRoutes[chat.key] = chat.route), false;
    const parked = (state.parkedThreads ??= {});
    if (state.threads[chat.key]) parked[`${chat.key}@${was}`] = state.threads[chat.key];
    const back = parked[`${chat.key}@${chat.route}`];
    if (back) state.threads[chat.key] = back;
    else delete state.threads[chat.key];
    delete parked[`${chat.key}@${chat.route}`];
    delete state.soulSent?.[chat.key];
    state.threadRoutes[chat.key] = chat.route;
    record({ dir: 'state', chat: chat.key, route: { from: was, to: chat.route }, thread: state.threads[chat.key] ?? null });
    save();
    return true;
  }
  const admin = msg => msg.fromMe;
  // The group's rule: a message wakes the bridge only if it tags @claude or replies to one of the bridge's own messages.
  const isGroup = chat => String(chat.jid ?? '').endsWith('@g.us');
  const CONVO_MS = 15 * 60_000;
  const active = chat => (state.convo?.[chat.key] ?? 0) > now();
  const openConvo = chat => ((state.convo ??= {})[chat.key] = now() + CONVO_MS);
  // A voice note whose transcript called Claude (msg.voiceTrigger, set by the bridge itself after the transcription) wakes it like @claude.
  const wakes = msg => Boolean(msg) && (tagged(msg.text) || msg.voiceTrigger === true || Boolean(msg.replyToId && (state.botIds ?? []).includes(msg.replyToId)));
  const ring = new Map(); // dry-run chats' recent messages (the lab's fake group has no wacli history)
  const queues = new Map();
  const busy = new Set();
  const running = new Map(); // chat key → the turn under way: { riders } (messages steered into it, reacted to with it)
  const steering = new Set(); // ids of steered messages still being handed to T3

  // --- the group's history, mirrored to the chat's own workspace (only chats with a `history` setting)
  const queueFile = join(stateDir, 'history-queue.jsonl');
  let flushing = false;
  let flushTimer = null;
  let flushDue = 0; // when flushTimer fires (Date.now()), for settled()
  async function flushHistory() {
    if (flushing || !existsSync(queueFile)) return 0;
    flushing = true;
    let done = 0;
    try {
      const entries = readFileSync(queueFile, 'utf8').split('\n').filter(Boolean).map(x => JSON.parse(x));
      const rest = [];
      for (const key of new Set(entries.map(e => e.chat))) {
        const chat = chats.find(c => c.key === key);
        const mine = entries.filter(e => e.chat === key);
        try {
          for (const t of historyTargets(config, chat)) await historyAppend(t.route, t.history, chat, mine.map(e => e.line));
          done += mine.length;
        } catch (e) {
          rest.push(...mine);
          record({ dir: 'history', chat: key, ok: false, queued: mine.length, error: clip(e.message, 120) });
        }
      }
      // Lines queued while this ran stay: the file is rewritten from what it holds now minus what was sent.
      const now2 = readFileSync(queueFile, 'utf8').split('\n').filter(Boolean);
      const left = now2.slice(entries.length);
      writeFileSync(queueFile, [...rest.map(e => JSON.stringify(e)), ...left].map(x => x + '\n').join(''), { mode: 0o600 });
      if (rest.length) {
        flushDue = Date.now() + 60_000;
        flushTimer = setTimeout(() => ((flushTimer = null), flushHistory().catch(() => {})), 60_000).unref();
      }
    } finally {
      flushing = false;
    }
    return done;
  }
  const queueHistory = (chat, line) => {
    appendFileSync(queueFile, JSON.stringify({ chat: chat.key, line }) + '\n', { mode: 0o600 });
    if (flushTimer) return;
    const ms = chat.dryRun && !config.routes[chat.route]?.ssh ? 0 : 3000;
    flushDue = Date.now() + ms;
    flushTimer = setTimeout(() => ((flushTimer = null), flushHistory().catch(e => log.error?.('history:', e.message))), ms).unref();
  };
  function mirror(chat, msg) {
    if (!historyTargets(config, chat).length || msg.skip || (!msg.text && !msg.media)) return;
    let reply = {};
    if (msg.replyToId) {
      let q = null;
      if (chat.dryRun) q = (ring.get(chat.key) ?? []).find(m => m.id === msg.replyToId);
      else
        try {
          const db = wacliDb();
          try {
            q = db.prepare(`SELECT sender_name name, coalesce(text, display_text, media_caption, '') text FROM messages WHERE chat_jid = ? AND msg_id = ?`).get(chat.jid, msg.replyToId);
          } finally {
            db.close();
          }
        } catch {
          /* not stored */
        }
      reply = { replyTo: msg.replyToId, replyName: q?.name ?? null, replyText: q?.text ?? null };
    }
    let name = msg.pushName ?? (msg.fromMe ? (ownAnswer(msg.text) ? 'Claude' : 'Franz') : null);
    if (!name && !chat.dryRun)
      try {
        const db = wacliDb();
        try {
          name = db.prepare(`SELECT coalesce(nullif(full_name, ''), nullif(push_name, '')) n FROM contacts WHERE jid = ?`).get(msg.sender ?? '')?.n ?? null;
        } finally {
          db.close();
        }
      } catch {
        /* none */
      }
    queueHistory(chat, historyLine({ id: msg.id, at: msg.at, name, text: msg.text, media: msg.media?.type ?? null, ...reply }));
  }

  // --- wacli
  const wacli = (args, opts) => run(config.wacli.bin, ['--store', config.wacli.store, ...args], opts);
  const wacliDb = () => new DatabaseSync(join(config.wacli.store, 'wacli.db'), { readOnly: true, timeout: 5000 });

  /**
   * One send; resolves to the sent message's id (null in a dry run). `kind`: reply (counted for the
   * hourly limit), status, notice, image, voice, react (`react`: { id, sender, emoji } on a message),
   * edit (`edit`: the id of one of the bridge's own messages, `text` its new text).
   * `trigger` is the incoming message being answered; in a group the send is refused unless it wakes the bridge.
   */
  // The bridge's own voice notes come back through the webhook as Franz's (fromMe) audio: while one is being sent to a chat, the
  // on-arrival transcription of that chat's fromMe audio waits for it, then skips the note if it is the one just sent (botIds).
  // A voice send that gives back no id (a timeout after delivery, a parse miss) mutes that chat's next fromMe audio (one note,
  // within 2 minutes).
  const voiceOut = new Map();
  const voiceMute = new Map(); // chat key → until
  function send(chat, opts) {
    const p = sendNow(chat, opts);
    // (Not on a send the bridge refused itself: nothing left, so nothing comes back.)
    if (opts?.voice)
      voiceOut.set(
        chat.key,
        p.then(
          id => void (id || voiceMute.set(chat.key, now() + 120_000)),
          e => void (e?.refused || voiceMute.set(chat.key, now() + 120_000)),
        ),
      );
    return p;
  }
  async function sendNow(chat, { kind, text = null, file = null, voice = false, quote = null, react = null, edit = null, trigger, postWait = null, document = null }) {
    const entry = { dir: 'out', chat: chat.key, kind, chars: text?.length ?? 0, file: Boolean(file), ...(edit ? { edit: true } : {}), ...(react ? { emoji: react.emoji } : {}) };
    let id = null;
    if (chat.readOnly || neverSend(chat.jid)) {
      record({ ...entry, refused: 'read-only source' });
      throw Object.assign(new Error('Refused: read-only source'), { refused: true });
    }
    if (isGroup(chat) && !(wakes(trigger) || (trigger && active(chat)))) {
      record({ ...entry, refused: 'group send outside an active conversation' });
      throw Object.assign(new Error('Refused: no trigger'), { refused: true });
    }
    if (chat.dryRun || !isLive(chat)) {
      // The file is kept for the test to look at (the turn's own copy is deleted after it).
      let kept = null;
      if (file) {
        mkdirSync(join(stateDir, 'outbox-media'), { recursive: true, mode: 0o700 });
        kept = join(stateDir, 'outbox-media', `${now()}-${file.split('/').pop()}`);
        copyFileSync(file, kept);
      }
      id = edit ?? `OUT${now()}${Math.random().toString(16).slice(2, 8)}`;
      appendFileSync(outboxFile, JSON.stringify({ at: new Date(now()).toISOString(), chat: chat.key, kind, id, text, file: kept, voice, quote: quote?.id ?? null, react, edit }) + '\n', { mode: 0o600 });
      record({ ...entry, via: 'outbox', ...(chat.dryRun ? {} : { notLive: true }) });
    } else {
      if (!SENDABLE.has(chat.jid) || neverSend(chat.jid)) {
        record({ ...entry, refused: 'not an allowed chat' });
        throw Object.assign(new Error('Refused: not an allowed chat'), { refused: true });
      }
      const quoteArgs = quote ? ['--reply-to', quote.id, ...(chat.jid.endsWith('@g.us') && quote.sender ? ['--reply-to-sender', quote.sender] : [])] : [];
      const argsFor = q => react
        ? ['send', 'react', '--to', chat.jid, '--id', react.id, '--reaction', react.emoji, ...(chat.jid.endsWith('@g.us') && react.sender ? ['--sender', react.sender] : [])]
        : edit
          ? ['messages', 'edit', '--chat', chat.jid, '--id', edit, '--message', text]
          : voice
            ? ['send', 'voice', '--to', chat.jid, '--file', file, ...q]
            : file
              ? ['send', 'file', '--to', chat.jid, '--file', file, ...(document ? ['--as', 'document', '--filename', document] : []), ...(text ? ['--caption', text] : []), ...(postWait ? ['--post-send-wait', postWait] : []), ...q]
              : ['send', 'text', '--to', chat.jid, '--message', text, ...(chat.key === 'self' ? ['--allow-self'] : []), ...q];
      // A reply to a message wacli has not stored yet («quoted message … not found in local store») must not be lost:
      // retry once with the quote after a short wait, then send it without the quote (same chat, same guards).
      const QUOTE_MISSING = /quoted message.*not found in local store/i;
      let quoted = quoteArgs.length ? 'quote' : null;
      try {
        let out;
        try {
          out = await wacli(['--json', ...argsFor(quoteArgs)], { timeout: 90_000 });
        } catch (e) {
          if (!quoteArgs.length || !QUOTE_MISSING.test(e.message)) throw e;
          await sleep(config.quoteRetryMs ?? 1500);
          try {
            out = await wacli(['--json', ...argsFor(quoteArgs)], { timeout: 90_000 });
            quoted = 'quote-after-retry';
          } catch (e2) {
            if (!QUOTE_MISSING.test(e2.message)) throw e2;
            out = await wacli(['--json', ...argsFor([])], { timeout: 90_000 });
            quoted = 'sent-without-quote';
          }
        }
        try {
          id = JSON.parse(out)?.data?.id ?? null;
        } catch {
          /* no id */
        }
        record({ ...entry, via: 'wacli', ok: true, ...(quoted && quoted !== 'quote' ? { quote: quoted } : {}) });
      } catch (e) {
        record({ ...entry, via: 'wacli', ok: false, error: clip(e.message, 200) });
        throw e;
      }
    }
    if (id && !edit) (state.botIds ??= []).push(String(id));
    if (isGroup(chat) && ['reply', 'image', 'voice', 'document'].includes(kind)) openConvo(chat); // an answer of Claude's keeps the conversation open
    if (kind === 'reply') (state.sends[chat.key] ??= []).push(now());
    state.sends[chat.key] = lastHour(state.sends[chat.key], now());
    save();
    return id;
  }
  // The reaction emojis (config `reactions`): while working, when Claude replied, when it woke but stayed silent.
  const EMOJI = { working: '👀', replied: '✅', silent: '☑️', failed: '⚠️', ...(config.reactions ?? {}) };
  /** How a chat shows it is working: Franz's /ack choice, else the config's (reaction by default). */
  const ackMode = chat => state.ack?.[chat.key] ?? chat.ack ?? (chat.key === 'self' ? 'edit' : 'reaction');
  /** The chat's humour level: its /tono choice, else the config's, else normal. */
  const toneOf = chat => state.tono?.[chat.key] ?? chat.tono ?? TONES[0];
  const reactTo = (chat, msg, emoji) =>
    send(chat, { kind: 'react', react: { id: msg.id, sender: msg.sender, emoji }, trigger: msg }).catch(e => log.error?.('react:', e.message));
  const say = (chat, text, kind = 'reply', trigger = null) => send(chat, { kind, text: text.startsWith(PREFIX) ? text : PREFIX + text, quote: kind === 'reply' ? trigger : null, trigger }).catch(e => log.error?.('send:', e.message));

  // --- the commands
  function statusText() {
    let heartbeat = null;
    try {
      heartbeat = Math.round((now() - Date.parse(readFileSync(join(config.wacli.store, 'HEARTBEAT'), 'utf8').trim())) / 60_000);
    } catch {
      /* none */
    }
    const lines = [`*Estado del puente*`, `- ${state.stopped ? 'Detenido (/start lo reanuda)' : 'Activo'}`];
    for (const c of chats.filter(c => c.enabled)) {
      const last = state.lastTurn[c.key];
      lines.push(
        `- ${c.label ?? c.key}: ${lastHour(state.sends[c.key], now()).length}/${c.repliesHour} respuestas esta hora; cola ${queues.get(c.key)?.length ?? 0}${busy.has(c.key) ? ', respondiendo' : ''}${last ? `; último turno ${hhmm(last.at)} (${Math.round(last.ms / 1000)} s${last.ok ? '' : ', falló'})` : ''}`,
      );
    }
    lines.push(`- wacli: ${heartbeat == null ? 'sin señal' : `última actividad hace ${heartbeat} min`}`);
    return lines.join('\n');
  }
  const HELP = [
    '*Asistente de prueba de Franz* (responde solo si le escriben @claude, o en una nota de voz que empieza con «Claude, …»)',
    '- @claude <pregunta>: consulta la base de datos, fotos, mapas, propuestas de cambio (con enlace)',
    '- Nota de voz que empieza con «Claude, …» (o «oye Claude…»): cuenta como @claude',
    '- Foto, y luego un mensaje con @claude (o responde a ella con @claude)',
    '- /status – estado del puente',
    '- /usage – uso de la cuenta de Claude',
    '- /whoami – quién eres para el asistente',
    '- /stop, /start – solo Franz: apagar o encender todo',
    '- /ack reaction | edit – solo Franz: reacciones en el mensaje (👀 trabajando, ✅ respondí, ☑️ sin respuesta), o un mensaje con los pasos',
    '- /tono normal | gracioso | picante – cuánto humor pone Claude en este chat',
  ].join('\n');
  const TONE_HELP = 'Opciones: /tono normal (cálido, casi sin bromas), /tono gracioso (con humor cuando encaja), /tono picante (juguetón y pícaro, siempre amable).';

  async function runCommand(chat, msg, cmd) {
    record({ dir: 'in', chat: chat.key, id: msg.id, decision: 'command', command: cmd.name });
    if (cmd.name === 'stop' || cmd.name === 'start') {
      if (!admin(msg)) return say(chat, 'Solo Franz puede usar /stop y /start.', 'reply', msg);
      state.stopped = cmd.name === 'stop';
      save();
      record({ dir: 'state', stopped: state.stopped, by: chat.key });
      return say(chat, state.stopped ? 'Detenido: no respondo a nada hasta que Franz escriba /start.' : 'Activo de nuevo.', 'reply', msg);
    }
    if (cmd.name === 'ack') {
      if (!admin(msg)) return say(chat, 'Solo Franz puede cambiar /ack.', 'reply', msg);
      const mode = String(cmd.arg ?? '').toLowerCase();
      if (!ACK_MODES.includes(mode)) return say(chat, `Aviso de trabajo aquí: ${ackMode(chat)}. Opciones: /ack reaction (👀 y luego ✅, o ☑️ si no hay respuesta), /ack edit (un mensaje que muestra los pasos y se vuelve la respuesta).`, 'reply', msg);
      (state.ack ??= {})[chat.key] = mode;
      save();
      record({ dir: 'state', ack: mode, chat: chat.key });
      return say(chat, `Aviso de trabajo aquí: ${mode}.`, 'reply', msg);
    }
    if (cmd.name === 'tono') {
      // A chat with one fixed voice (`toneLine: false`) has no tone levels: say so, change nothing.
      if (chat.toneLine === false) return say(chat, 'Este chat usa una sola voz ahora; /tono ya no cambia nada aquí.', 'reply', msg);
      const level = String(cmd.arg ?? '').toLowerCase();
      if (!TONES.includes(level)) return say(chat, `Tono aquí: ${toneOf(chat)}. ${TONE_HELP}`, 'reply', msg);
      (state.tono ??= {})[chat.key] = level;
      save();
      record({ dir: 'state', tono: level, chat: chat.key });
      return say(chat, `Tono aquí: ${level}.`, 'reply', msg);
    }
    if (overLimit(chat, msg)) return;
    if (cmd.name === 'help') return say(chat, HELP, 'reply', msg);
    if (cmd.name === 'status') return say(chat, statusText(), 'reply', msg);
    if (cmd.name === 'whoami') {
      const who = admin(msg) ? 'Franz (administrador)' : `${msg.pushName ?? 'sin nombre'}, miembro del grupo`;
      return say(chat, `Eres ${who}, en ${chat.label ?? chat.key}.${admin(msg) ? '' : ' Puedes consultar datos, fotos y mapas y pedir propuestas de cambio (se aplican en la app).'}`, 'reply', msg);
    }
    if (cmd.name === 'usage') {
      try {
        const { claudeUsage } = await import(join(ROOT, 'server/claude-usage.mjs'));
        const r = await claudeUsage().read({ es: true });
        return say(chat, r.text ?? r.error, 'reply', msg);
      } catch (e) {
        return say(chat, `No pude leer el uso: ${clip(e.message, 100)}`, 'reply', msg);
      }
    }
    return say(chat, `No conozco /${cmd.word}. Comandos: /help, /status, /whoami, /usage.`, 'reply', msg);
  }

  function overLimit(chat, msg) {
    const n = lastHour(state.sends[chat.key], now()).length;
    if (n < chat.repliesHour) return false;
    if (msg?.followUp) {
      record({ dir: 'in', chat: chat.key, decision: 'limited' });
      return true;
    }
    const hour = Math.floor(now() / 3_600_000);
    if (state.noticeHour?.[chat.key] !== hour) {
      (state.noticeHour ??= {})[chat.key] = hour;
      save();
      say(chat, `Llegué al límite de ${chat.repliesHour} respuestas por hora; vuelvo a leer en un rato.`, 'notice', msg);
    }
    record({ dir: 'in', chat: chat.key, decision: 'limited' });
    return true;
  }

  // --- incoming
  function handle(body) {
    const msg = parsePayload(body);
    if (!msg) return { decision: 'ignored' };
    const chat = chatOf(msg.chat);
    // Not an enabled chat: dropped before anything else, nothing of it kept.
    if (!chat) {
      record({ dir: 'in', decision: 'drop', reason: 'chat not enabled' });
      return { decision: 'drop' };
    }
    if (chat.dryRun && body._labMedia) msg.labMedia = String(body._labMedia);
    // A message sent from Franz's phone can reach the webhook before its text is stored: read it from wacli.db
    // (read-only); if it is still empty it is not marked seen below, so the later webhook with the text is handled.
    if (!msg.text && !msg.media) {
      let db = null;
      try {
        db = wacliDb();
        const row = db.prepare("SELECT coalesce(text, display_text, media_caption, '') t FROM messages WHERE chat_jid = ? AND msg_id = ?").get(msg.chat, msg.id);
        if (row?.t) msg.text = String(row.t);
      } catch (e) {
        log.error?.('text lookup:', e.message);
      } finally {
        db?.close();
      }
    }
    try {
      mirror(chat, msg);
    } catch (e) {
      log.error?.('mirror:', e.message);
    }
    if (chat.dryRun) {
      const list = ring.get(chat.key) ?? [];
      list.push({ at: msg.at, name: msg.fromMe ? 'Franz' : msg.pushName ?? 'alguien', text: msg.text, media: msg.media?.type ?? null, fromMe: msg.fromMe, id: msg.id, replyToId: msg.replyToId ?? null });
      ring.set(chat.key, list.slice(-60));
    }
    const out = (decision, reason) => {
      record({ dir: 'in', chat: chat.key, id: msg.id, decision, reason });
      return { decision, reason };
    };
    // A read-only source: mirrored above, nothing else (no turn, no command, no reaction, no media download).
    if (chat.readOnly) return out('history', 'read-only source');
    if (state.seen.includes(msg.id)) return out('skip', 'duplicate');
    if (msg.text || msg.media) state.seen.push(msg.id);
    save();
    if (msg.skip) return out('skip', msg.skip);
    // Every voice note is transcribed as it arrives (one at a time, on this PC), whether it wakes Claude or not: its transcript
    // goes into the context lines and the chat's history, and a turn that needs it later reuses it. Also a note delivered late
    // (the PC asleep, wacli reconnecting), up to two days old; only a fresh one can wake Claude (voiceLater checks).
    const heard = voiceOn(chat) && isVoice(msg.media?.type) && now() - msg.at < 2 * 86_400_000 ? enqueueVoice(chat, msg) : null;
    if (now() - msg.at > STALE_MS) return out('skip', 'old');
    if (ownAnswer(msg.text)) return out('skip', 'loop guard');
    if (chat.key === 'self' && !msg.fromMe) return out('skip', 'not from Franz');
    const cmd = command(msg.text);
    if (state.stopped && !(cmd && cmd.name === 'start' && admin(msg)) && !(cmd?.name === 'status' && admin(msg))) return out('skip', 'stopped');
    // In a group a command counts only if its own message tagged @claude or replied to the bridge.
    if (cmd && isGroup(chat) && !wakes(msg)) return out('skip', 'command without trigger');
    if (cmd) {
      runCommand(chat, msg, cmd).catch(e => log.error?.('command:', e.message));
      return { decision: 'command' };
    }
    // The group: a tag or a reply to the bridge opens or refreshes the conversation; an untagged message inside one is a follow-up only if it is addressed to Claude by name.
    if (isGroup(chat)) {
      if (tagged(msg.text)) openConvo(chat);
      else if (wakes(msg)) {
        // A quote of Claude's message may be aimed at the team: the chat decides whether an answer is expected.
        openConvo(chat);
        msg.followUp = true;
      }
      // Inside the window, an untagged message wakes Claude only when someone other than Franz quotes a Claude message
      // (handled above) or addresses it by name; being close in time is not enough, and Franz's own untagged posts never do.
      else if (active(chat) && !msg.fromMe && named(msg.text)) msg.followUp = true;
      else return heard ? voiceLater(chat, msg, heard, out) : out('skip', 'no @claude');
      save();
    } else if (!tagged(msg.text)) return heard ? voiceLater(chat, msg, heard, out) : out('skip', 'no @claude');
    return admit(chat, msg, out);
  }

  /**
   * An untagged voice note: once transcribed, it counts as an @claude message when its transcript calls Claude
   * (voiceAddressed: Franz's chat anywhere, the group only in its first words). Everything after the trigger is as for
   * @claude: /stop, the hourly limit, the ack, the queue.
   */
  function voiceLater(chat, msg, heard, out) {
    out('voice', 'transcribing');
    heard
      .then(t => {
        if (!t?.text) return;
        if (!voiceAddressed(t.wakeText ?? t.text, { personal: chat.key === 'self' })) return out('skip', 'voice: not addressed');
        if (state.stopped) return out('skip', 'stopped');
        if (now() - msg.at > STALE_MS) return out('skip', 'old');
        msg.voiceTrigger = true;
        msg.heard = t.text;
        if (isGroup(chat)) {
          openConvo(chat);
          save();
        }
        record({ dir: 'in', chat: chat.key, id: msg.id, decision: 'voice trigger', engine: t.engine });
        admit(chat, msg, out);
      })
      .catch(e => log.error?.('voice trigger:', e.message));
    return { decision: 'voice' };
  }

  /** A message that passed the gate: the hourly limit, then steering or the chat's queue. */
  function admit(chat, msg, out) {
    if (overLimit(chat, msg)) return { decision: 'limited' };
    // A chat with `steer: true` (Franz's own): a message while it answers goes into that answer (T3 steering), not to the queue.
    // Only into a thread whose id is known: a chat's first turn learns it at its end, and a steer without it would make a new chat.
    if (chat.steer && busy.has(chat.key) && running.has(chat.key) && state.threads[chat.key] && !msg.followUp) {
      out('steer', null);
      msg.seen = reactTo(chat, msg, EMOJI.working);
      steering.add(msg.id);
      turn(chat, msg, { steer: true })
        .catch(e => {
          log.error?.('steer:', e.message);
          record({ dir: 'turn', chat: chat.key, id: msg.id, ok: false, steer: true, error: clip(e.message, 200) });
        })
        .finally(() => steering.delete(msg.id));
      return { decision: 'steered' };
    }
    const q = queues.get(chat.key) ?? [];
    if (q.length >= 5) return out('skip', 'queue full');
    q.push(msg);
    queues.set(chat.key, q);
    out('queued', msg.followUp ? 'follow-up' : null);
    // The 👀 only here, after the gate: a message that never passed it gets no reaction at all. Both ack modes get it (the edit
    // mode's status message is on top of the reactions, not instead of them).
    msg.seen = reactTo(chat, msg, EMOJI.working);
    pump(chat);
    return { decision: 'queued' };
  }

  async function pump(chat) {
    if (busy.has(chat.key)) return;
    const q = queues.get(chat.key) ?? [];
    const msg = q.shift();
    if (!msg) return;
    busy.add(chat.key);
    try {
      await turn(chat, msg);
    } catch (e) {
      log.error?.('turn:', e.message);
      record({ dir: 'turn', chat: chat.key, id: msg.id, ok: false, error: clip(e.message, 200) });
      if (!msg.followUp) await say(chat, 'No pude responder ahora (error del puente). Prueba de nuevo en un rato.', 'notice', msg);
    } finally {
      busy.delete(chat.key);
      if (q.length) setImmediate(() => pump(chat));
    }
  }

  // --- context and media
  /** The chat's last `contextLines` messages or the last 2 hours, whichever is more, at most 40 (oldest first). */
  function recentMessages(chat, msg) {
    if (!chat.contextLines) return [];
    const keep = (rows, at) => rows.filter((r, i) => i >= rows.length - chat.contextLines || r.at >= at - CONTEXT_MS).slice(-CONTEXT_CAP);
    if (chat.dryRun) {
      const list = (ring.get(chat.key) ?? []).filter(m => m.id !== msg.id && m.at <= msg.at);
      return keep(list, msg.at).map(m => {
        const q = m.replyToId ? list.find(x => x.id === m.replyToId) : null;
        return { ...m, replyName: q?.name ?? null, replyText: q?.text ?? null, transcript: state.transcripts?.[m.id] ?? null };
      });
    }
    const db = wacliDb();
    try {
      const rows = db
        .prepare(
          `SELECT m.msg_id id, m.ts, m.from_me, m.sender_name, coalesce(m.text, m.display_text, m.media_caption, '') text, m.media_type, m.quoted_msg_id,
             (SELECT coalesce(nullif(c.full_name, ''), nullif(c.push_name, '')) FROM contacts c WHERE c.jid = m.sender_jid) contact,
             (SELECT coalesce(q.text, q.display_text, q.media_caption, '') FROM messages q WHERE q.chat_jid = m.chat_jid AND q.msg_id = m.quoted_msg_id) qtext,
             (SELECT q.sender_name FROM messages q WHERE q.chat_jid = m.chat_jid AND q.msg_id = m.quoted_msg_id) qname
           FROM messages m WHERE m.chat_jid = ? AND m.msg_id != ? AND m.ts <= ? AND m.revoked = 0 AND m.reaction_to_id IS NULL ORDER BY m.ts DESC LIMIT ?`,
        )
        .all(chat.jid, msg.id, Math.ceil(msg.at / 1000), CONTEXT_CAP)
        .reverse()
        .map(r => ({ id: r.id, at: r.ts * 1000, name: r.from_me ? (ownAnswer(r.text) ? 'Claude (you)' : 'Franz') : r.sender_name || r.contact || 'alguien', text: r.text, media: r.media_type, replyName: r.qname ?? null, replyText: r.qtext || null, transcript: state.transcripts?.[r.id] ?? null }));
      return keep(rows, msg.at);
    } finally {
      db.close();
    }
  }

  /** A context line: time (with the date when not today), sender, what it answers, media placeholder and text. */
  function contextLine(r, today) {
    const when = hhmm(r.at);
    const stamp = dmhm(r.at).slice(0, 5) === dmhm(today).slice(0, 5) ? when : dmhm(r.at);
    const tag = mediaTag(r.media);
    const media = tag ? (r.transcript ? `[${tag}: «${clip(r.transcript, 200)}»] ` : `[${tag}] `) : '';
    const to = r.replyName || r.replyText ? ` (en respuesta a ${r.replyName ?? 'alguien'}: «${clip(String(r.replyText ?? '').replace(/\s*\n\s*/g, ' '), 60)}»)` : '';
    return `  ${stamp} ${r.name}${to}: ${media}${clip(String(r.text || '').replace(/\s*\n\s*/g, ' / '), 400)}`;
  }

  /** Media to bring along: the message's own, the quoted message's, and the sender's untagged media of the last 3 minutes. */
  function mediaFor(chat, msg) {
    if (chat.dryRun) return msg.labMedia ? [{ id: msg.id, type: /\.(ogg|opus|mp3|m4a|wav)$/i.test(msg.labMedia) ? 'audio' : 'image', path: msg.labMedia }] : [];
    const db = wacliDb();
    try {
      const rows = db
        .prepare(
          `SELECT msg_id id, media_type type, mime_type mime, direct_path, media_key, file_length, local_path, ts FROM messages
           WHERE chat_jid = ? AND media_type IN ('image','audio','ptt','video','document','sticker') AND (
             msg_id = ? OR msg_id = ? OR (coalesce(sender_jid, '') = ? AND from_me = ? AND ts BETWEEN ? AND ?))
           ORDER BY ts`,
        )
        .all(chat.jid, msg.id, msg.replyToId ?? '', msg.sender ?? '', msg.fromMe ? 1 : 0, Math.floor(msg.at / 1000) - 180, Math.ceil(msg.at / 1000));
      // Not the bridge's own media (its voice replies are Franz's audio in the store).
      return rows.filter(r => (r.type === 'image' || isVoice(r.type)) && !(state.botIds ?? []).includes(r.id)).slice(-4);
    } finally {
      db.close();
    }
  }

  async function fetchMedia(row, dir) {
    if (row.path) return row.path;
    if (row.local_path && existsSync(row.local_path)) return row.local_path;
    if (!row.direct_path || !row.media_key) throw new Error('no media key');
    const res = await fetch(`https://mmg.whatsapp.net${row.direct_path}`, { signal: AbortSignal.timeout(60_000) });
    if (!res.ok) throw new Error(`media download ${res.status}`);
    const plain = decryptMedia(Buffer.from(await res.arrayBuffer()), Buffer.from(row.media_key), row.type);
    const file = join(dir, `${row.id.replace(/\W/g, '')}.${row.type === 'image' ? 'jpg' : 'ogg'}`);
    writeFileSync(file, plain, { mode: 0o600 });
    return file;
  }

  // --- voice notes: the engines, the on-arrival queue, the transcripts
  // Parakeet (transcribe.cpp on this PC's eGPU, config voice.parakeet) is local and fast but has no custom vocabulary, so it
  // misspells genera (fixGenera repairs the ones it can); Scribe v2 (ElevenLabs, the genera as keyterms) gets the names right
  // but is outside. So: on arrival, every note goes through Parakeet (then the local whisper if it fails), never Scribe; a turn
  // that needs a note (its own, the one it quotes, the sender's last minutes) upgrades it once with Scribe, as before
  // (then Parakeet, then whisper). The 12 Nov 2026 pilot: reports/wa_voice_20261112 in WingsClassificator.
  const ARRIVAL = ['parakeet', 'whisper'];
  const TURN = ['scribe', 'parakeet', 'whisper'];
  const ready = {
    parakeet: () => Boolean(config.voice?.parakeet?.model && config.voice?.parakeet?.device) || Boolean(engines.parakeet),
    scribe: () => Boolean(config.voice?.elevenlabsEnv) || Boolean(engines.scribe),
    whisper: () => Boolean(config.voice?.whisperPython) || Boolean(engines.whisper),
  };
  const voiceOn = chat => !chat.readOnly && chat.voiceOnArrival !== false && config.voice?.onArrival !== false && ARRIVAL.some(e => ready[e]());
  // One GPU job at a time (the on-arrival queue and a turn's fallback share the card).
  let gpuChain = Promise.resolve();
  const onGpu = fn => {
    const p = gpuChain.then(fn, fn);
    gpuChain = p.then(() => {}, () => {});
    return p;
  };

  /** The shared dictation word list (config voice.wordsFiles, default dictation-words.json and its untracked .local.json beside this file), read per note. */
  const wordsFiles = () => config.voice?.wordsFiles ?? [join(HERE, 'dictation-words.json'), join(HERE, 'dictation-words.local.json')];

  /** Parakeet: tools/whatsapp/asr-parakeet.mjs as a child process in its own scope (MemoryMax), on the configured eGPU only. */
  async function parakeetRun(file) {
    const p = config.voice.parakeet;
    const env = { ...process.env, PARAKEET_MODULE: p.module, PARAKEET_MODEL: p.model, PARAKEET_DEVICE: p.device };
    const args = ['--user', '--scope', '--quiet', '-p', `MemoryMax=${p.memoryMax ?? '3G'}`, '--', process.execPath, p.script ?? join(HERE, 'asr-parakeet.mjs'), file];
    let out;
    try {
      out = await run(p.systemdRun ?? 'systemd-run', args, { env, timeout: p.timeoutMs ?? 180_000 });
    } catch (e) {
      out = String(e.stdout ?? '');
      if (!out.trim()) throw e;
    }
    const j = JSON.parse(out.trim().split('\n').pop());
    if (j.error) throw new Error(j.error);
    // The wake check (voiceAddressed) reads `wakeText`, the text before the word list, so its rules stay as they were
    // («cloud computing» does not become a call); everything else gets `text`, with the shared word list applied.
    const wakeText = fixGenera(String(j.text ?? '').trim());
    const text = parakeetText(wakeText, loadWords(wordsFiles()));
    return { text, wakeText, language: text ? voiceLanguage(text) : null, audioMs: j.audioMs ?? null };
  }

  /** The ElevenLabs key file (KEY=value lines). */
  const elevenEnv = () =>
    Object.fromEntries(
      readFileSync(config.voice.elevenlabsEnv, 'utf8')
        .split('\n')
        .filter(l => l.includes('='))
        .map(l => [l.slice(0, l.indexOf('=')).trim(), l.slice(l.indexOf('=') + 1).trim().replace(/^"|"$/g, '')]),
    );
  async function scribeRun(file) {
    const audio = readFileSync(file);
    if (audio.length > 5 * 1024 * 1024) throw new Error('voice note over 5 MB');
    const env = elevenEnv();
    const form = new FormData();
    form.append('model_id', 'scribe_v2');
    form.append('tag_audio_events', 'false');
    for (const k of KEYTERMS) form.append('keyterms', k);
    form.append('file', new Blob([audio], { type: 'audio/ogg' }), 'voice.ogg');
    let status = null;
    for (let attempt = 0; attempt < 2; attempt++) {
      const res = await fetch(config.voice.scribeUrl ?? 'https://api.elevenlabs.io/v1/speech-to-text', { method: 'POST', headers: { 'xi-api-key': env.ELEVENLABS_API_KEY }, body: form, signal: AbortSignal.timeout(30_000) }).catch(e => ({ ok: false, status: e.name }));
      if (res.ok) {
        const j = await res.json();
        return { text: String(j.text ?? '').trim(), language: j.language_code ?? null };
      }
      status = res.status;
    }
    throw new Error(`scribe ${status}`);
  }

  /** The local fallback: faster-whisper small (int8, 2 threads, nice, its own MemoryMax scope), the genera as its prompt. */
  async function whisperRun(file) {
    const wav = file + '.wav';
    try {
      await run('ffmpeg', ['-v', 'error', '-y', '-i', file, '-ar', '16000', '-ac', '1', wav], { timeout: 60_000 });
      const py = `import sys,json\nfrom faster_whisper import WhisperModel\nm=WhisperModel("small",device="cpu",compute_type="int8",cpu_threads=2)\nseg,info=m.transcribe(sys.argv[1],beam_size=5,vad_filter=True,initial_prompt=sys.argv[2])\nprint(json.dumps({"text":" ".join(s.text.strip() for s in seg),"language":info.language}))`;
      // Without «Claude» in its prompt: whisper echoes its prompt on noise, and an echoed «Claude» would wake the chat.
      const prompt = KEYTERMS.filter(k => k !== 'Claude').join(', ');
      const out = await run('systemd-run', ['--user', '--scope', '--quiet', '-p', `MemoryMax=${config.voice.whisperMemoryMax ?? '2G'}`, '--', 'nice', '-n', '10', config.voice.whisperPython, '-c', py, wav, prompt], { timeout: 600_000 });
      const j = JSON.parse(out.trim().split('\n').pop());
      return { text: String(j.text ?? '').trim(), language: j.language ?? null };
    } finally {
      rmSync(wav, { force: true });
    }
  }
  const ENGINES = {
    parakeet: file => onGpu(() => (engines.parakeet ?? parakeetRun)(file)),
    scribe: file => (engines.scribe ?? scribeRun)(file),
    whisper: file => (engines.whisper ?? whisperRun)(file),
  };

  /** The first engine of `order` that answers: { text, language, engine, ms } (text '' for a note without speech), or { error }. */
  async function transcribe(file, order = TURN, ctx = {}) {
    for (const engine of order.filter(e => ready[e]())) {
      const t0 = now();
      try {
        const r = await ENGINES[engine](file);
        return { ...r, text: String(r.text ?? '').trim(), engine, ms: now() - t0 };
      } catch (e) {
        record({ dir: 'voice', ...ctx, engine, ok: false, error: clip(e.message, 120) });
      }
    }
    return { error: 'no pude transcribir la nota de voz' };
  }

  /** Keeps a transcript (state, and the chat's history files where its text history already goes; nowhere else). */
  function storeTranscript(chat, id, t) {
    if (!t?.text) return;
    (state.transcripts ??= {})[id] = clip(t.text, 3000);
    (state.voiceMeta ??= {})[id] = { engine: t.engine, language: t.language ?? null, at: new Date(now()).toISOString() };
    save();
    if (historyTargets(config, chat).length) queueHistory(chat, { t: 'transcript', of: id, engine: t.engine, at: new Date(now()).toISOString(), text: clip(t.text, 1500) });
  }
  const storedTranscript = id => (state.transcripts?.[id] ? { text: state.transcripts[id], engine: state.voiceMeta?.[id]?.engine ?? null, language: state.voiceMeta?.[id]?.language ?? null } : null);

  /** A message's own media row in the wacli store (read-only); the webhook can come before the row: a few tries. */
  async function mediaRow(chat, msg) {
    if (chat.dryRun) return msg.labMedia ? { id: msg.id, type: 'audio', path: msg.labMedia } : null;
    for (let i = 0; i < 5; i++) {
      const db = wacliDb();
      try {
        const row = db.prepare(`SELECT msg_id id, media_type type, direct_path, media_key, file_length, local_path FROM messages WHERE chat_jid = ? AND msg_id = ?`).get(chat.jid, msg.id);
        if (row?.media_key || row?.local_path) return row;
      } finally {
        db.close();
      }
      await sleep(config.voice?.rowWaitMs ?? 1500);
    }
    return null;
  }

  // The on-arrival queue: one note at a time. A turn that needs a note still in it waits for that job (voiceJobs) instead of
  // transcribing it a second time.
  const voiceJobs = new Map(); // message id → promise of its transcript
  const voiceQueue = [];
  let voiceBusy = false;
  function enqueueVoice(chat, msg) {
    if (voiceJobs.has(msg.id)) return voiceJobs.get(msg.id);
    const have = storedTranscript(msg.id);
    if (have) return Promise.resolve(have);
    let done;
    const p = new Promise(r => (done = r));
    voiceJobs.set(msg.id, p);
    // A fresh note goes ahead of a backlog of late ones (it may wake Claude; they are only history).
    const late = now() - msg.at > STALE_MS;
    const at = late ? -1 : voiceQueue.findIndex(j => j.late);
    if (at < 0) voiceQueue.push({ chat, msg, done, late });
    else voiceQueue.splice(at, 0, { chat, msg, done, late });
    pumpVoice();
    return p;
  }
  async function pumpVoice() {
    if (voiceBusy) return;
    voiceBusy = true;
    try {
      while (voiceQueue.length) {
        const job = voiceQueue.shift();
        let t = null;
        try {
          t = await arrival(job.chat, job.msg);
        } catch (e) {
          record({ dir: 'voice', on: 'arrival', chat: job.chat.key, id: job.msg.id, ok: false, error: clip(e.message, 120) });
        }
        job.done(t);
        voiceJobs.delete(job.msg.id);
      }
    } finally {
      voiceBusy = false;
    }
  }
  async function arrival(chat, msg) {
    // The bridge's own voice reply (it comes back as Franz's audio): wait for a send under way, then skip it.
    if (msg.fromMe) await voiceOut.get(chat.key);
    const muted = msg.fromMe && (voiceMute.get(chat.key) ?? 0) > now();
    if (muted) voiceMute.delete(chat.key); // one note only: the echo comes first
    if ((state.botIds ?? []).includes(msg.id) || muted) {
      record({ dir: 'voice', on: 'arrival', chat: chat.key, id: msg.id, skipped: 'own voice note' });
      return null;
    }
    const row = await mediaRow(chat, msg);
    if (!row) throw new Error('no media row');
    if (row.file_length > 25 * 1024 * 1024) throw new Error('voice note over 25 MB');
    const dir = mkdtempSync(join(tmpdir(), 'wa-voice-'));
    try {
      const file = await fetchMedia(row, dir);
      const t = await transcribe(file, ARRIVAL, { on: 'arrival', chat: chat.key, id: msg.id });
      storeTranscript(chat, msg.id, t);
      record({ dir: 'voice', on: 'arrival', chat: chat.key, id: msg.id, engine: t.engine ?? null, ok: !t.error, chars: t.text?.length ?? 0, ms: t.ms ?? null, audioMs: t.audioMs ?? null });
      return t;
    } finally {
      rmSync(dir, { recursive: true, force: true });
    }
  }

  /**
   * A voice note a turn needs: the transcript made on arrival (waited for if still in the queue) is reused; one not made by
   * Scribe is upgraded once with Scribe (the names), and the better one is kept. Without any, the turn's own chain.
   */
  const upgrades = new Map(); // note id → the Scribe upgrade under way (two turns needing the same note share it)
  async function voiceForTurn(chat, m, dir, onListen) {
    await voiceJobs.get(m.id);
    while (upgrades.has(m.id)) await upgrades.get(m.id).catch(() => {});
    const have = storedTranscript(m.id);
    // After a failed Scribe upgrade of a stored transcript, later turns reuse what is stored (no new Scribe try).
    if (have && (have.engine === 'scribe' || !ready.scribe() || state.voiceMeta?.[m.id]?.scribeTried)) return { ...have, reused: true };
    const p = upgrade(chat, m, dir, onListen, have);
    upgrades.set(m.id, p);
    try {
      return await p;
    } finally {
      upgrades.delete(m.id);
    }
  }
  async function upgrade(chat, m, dir, onListen, have) {
    onListen?.();
    const file = await fetchMedia(m, dir);
    const order = have ? TURN.filter(e => e === 'scribe') : TURN;
    const t = await transcribe(file, order, { on: 'turn', chat: chat.key, id: m.id });
    if (t.text) {
      storeTranscript(chat, m.id, t);
      return t;
    }
    if (have && state.voiceMeta?.[m.id]) {
      state.voiceMeta[m.id].scribeTried = true;
      save();
    }
    return have ? { ...have, reused: true } : t;
  }

  /** A photo where the chat's tools find it (the route's T3 attachments folder: identify_species takes its name). */
  async function placePhoto(route, file, id) {
    const name = `wa-${String(id).replace(/[^\w-]/g, '').slice(0, 60)}.jpg`;
    const target = `${route.attachmentsDir}/${name}`;
    if (route.ssh) await run('scp', ['-q', '-o', 'BatchMode=yes', file, `${route.ssh}:${target}`], { timeout: 120_000 });
    else {
      mkdirSync(route.attachmentsDir, { recursive: true, mode: 0o700 });
      copyFileSync(file, target);
      chmodSync(target, 0o600);
    }
    return { name, path: target };
  }

  // --- T3
  /** One turn (tools/whatsapp/t3-turn.mjs, over ssh for claudeclaw); `onStep` gets each tool step as it starts. */
  async function t3Turn(route, threadKey, text, onStep = null, { steer = false, who = null } = {}) {
    const req = {
      op: 'turn',
      home: route.home,
      local: route.local,
      tokenFile: route.tokenFile,
      commandsModule: route.commandsModule,
      threadId: state.threads[threadKey] ?? null,
      text,
      progress: Boolean(onStep),
      personalProgress: Boolean(onStep) && threadKey === 'self',
      ...(steer ? { steer: true } : {}),
      // The sender's app account and admin flag, looked up by the helper on the route's machine at turn time.
      ...(who ? { who } : {}),
      timeoutMs: route.timeoutMs ?? 12 * 60_000,
      // The chats' own Claude instance (config `instanceId`, e.g. claude-whatsapp): T3 settings made for it reach only them.
      ...(route.instanceId ? { instanceId: route.instanceId } : {}),
      create: { projectId: route.projectId, title: route.title, model: route.model, effort: route.effort },
    };
    // A turn over ssh would run on claudeclaw's real T3, where NODE_TEST_CONTEXT does not reach t3-turn.mjs: refused here under node --test.
    if (route.ssh && process.env.NODE_TEST_CONTEXT) return { ok: false, error: 'ssh turn refusing to run under node --test (NODE_TEST_CONTEXT is set): tests never use the AI subscriptions' };
    const [bin, args] = route.ssh
      ? ['ssh', ['-o', 'BatchMode=yes', '-o', 'ServerAliveInterval=30', route.ssh, `${route.node} ${route.script}`]]
      : [route.node ?? process.execPath, [route.script ?? join(HERE, 't3-turn.mjs')]];
    const lines = await new Promise(resolve => {
      const child = spawn(bin, args, { stdio: ['pipe', 'pipe', 'ignore'] });
      const got = [];
      let rest = '';
      const timer = setTimeout(() => child.kill('SIGTERM'), req.timeoutMs + 60_000);
      child.stdout.on('data', d => {
        rest += d;
        let i;
        while ((i = rest.indexOf('\n')) >= 0) {
          const line = rest.slice(0, i).trim();
          rest = rest.slice(i + 1);
          if (!line) continue;
          got.push(line);
          try {
            const j = JSON.parse(line);
            if (j.progress && onStep) onStep(j.progress);
          } catch {
            /* not JSON */
          }
        }
      });
      child.on('close', () => {
        clearTimeout(timer);
        if (rest.trim()) got.push(rest.trim());
        resolve(got);
      });
      child.on('error', () => resolve(got));
      child.stdin.end(JSON.stringify(req));
    });
    let r = { ok: false, error: 'no answer from the turn helper' };
    for (const line of lines.toReversed())
      try {
        const j = JSON.parse(line);
        if (!j.progress) {
          r = j;
          break;
        }
      } catch {
        /* not JSON */
      }
    if (r.threadId && r.threadId !== state.threads[threadKey]) {
      state.threads[threadKey] = r.threadId;
      save();
    }
    return r;
  }

  /** A picture to send: fetched (URL, this computer, or claudeclaw), checked to be one, at most 2048 px on its long side as JPEG (WhatsApp's HD size). */
  async function fetchImage(route, src, dir, i) {
    let buf;
    if (/^https?:\/\//.test(src)) {
      const res = await fetch(src, { signal: AbortSignal.timeout(60_000), redirect: 'follow' });
      if (!res.ok) throw new Error(`image ${res.status}`);
      buf = Buffer.from(await res.arrayBuffer());
    } else if (route.ssh) {
      const tmp = join(dir, `remote-${i}`);
      await run('scp', ['-q', '-o', 'BatchMode=yes', `${route.ssh}:${src}`, tmp], { timeout: 120_000 });
      buf = readFileSync(tmp);
      rmSync(tmp, { force: true });
    } else buf = readFileSync(src);
    if (buf.length > 32 * 1024 * 1024) throw new Error('image over 32 MB');
    const kind = imageKind(buf);
    if (!kind) throw new Error('not a picture');
    const raw = join(dir, `in-${i}.${kind}`);
    writeFileSync(raw, buf, { mode: 0o600 });
    const file = join(dir, `out-${i}.jpg`);
    // Smaller pictures keep their size; larger ones come down to 2048 px on the long side.
    await run('ffmpeg', ['-v', 'error', '-y', '-i', raw, '-frames:v', '1', '-vf', "scale='if(gt(iw,ih),min(2048,iw),-2)':'if(gt(iw,ih),-2,min(2048,ih))'", '-q:v', '3', file], { timeout: 60_000 });
    rmSync(raw, { force: true });
    return file;
  }

  // ElevenLabs refused or failed: left alone for an hour after 401/402 (key, credits), 5 minutes after anything else.
  let ttsDownUntil = 0;
  /** One ElevenLabs synthesis into `ogg` (Ogg Opus as it comes); → its credit cost (the character-cost header) or null. */
  async function elevenTts(spoken, lang, ogg) {
    const tts = config.voice.tts;
    const voice = (lang === 'en' && tts.voiceEn) || tts.voiceEs;
    const url = `${tts.url ?? 'https://api.elevenlabs.io/v1/text-to-speech'}/${encodeURIComponent(voice)}?output_format=${tts.format ?? 'opus_48000_64'}`;
    let status = null;
    try {
      // No language_code: the model reads the language from the text (voiceLanguage() is only a guess, for the voice).
      const res = await fetch(url, {
        method: 'POST',
        headers: { 'xi-api-key': elevenEnv().ELEVENLABS_API_KEY, 'content-type': 'application/json' },
        body: JSON.stringify({ text: spoken, model_id: tts.model ?? 'eleven_v4_turbo' }),
        signal: AbortSignal.timeout(tts.timeoutMs ?? 20_000),
      });
      status = res.status;
      const buf = Buffer.from(await res.arrayBuffer());
      if (!res.ok) throw new Error(`elevenlabs ${res.status} ${clip(buf.toString(), 80)}`);
      if (buf.subarray(0, 4).toString() !== 'OggS') throw new Error('elevenlabs: not an Ogg file');
      writeFileSync(ogg, buf, { mode: 0o600 });
      return Number(res.headers.get('character-cost')) || null;
    } catch (e) {
      ttsDownUntil = now() + ([401, 402].includes(status) ? 3_600_000 : 300_000);
      throw e;
    }
  }
  /**
   * The answer as a voice note (Ogg Opus, 48 kHz mono): ElevenLabs (config voice.tts: model eleven_v4_turbo, format
   * opus_48000_64, which WhatsApp takes as it is; voiceEs / voiceEn), else Edge TTS (ttsVoiceEs / ttsVoiceEn).
   * → { file, engine, cost, chars }, or null when nothing in the text can be spoken.
   */
  async function voiceNote(text, dir, chatKey = null) {
    const spoken = speakable(text);
    if (!spoken) return null;
    const ogg = join(dir, 'reply.ogg');
    const lang = voiceLanguage(spoken);
    if (config.voice.tts?.voiceEs && config.voice.elevenlabsEnv && now() >= ttsDownUntil)
      try {
        const cost = await (engines.tts ?? elevenTts)(spoken, lang, ogg);
        return { file: ogg, engine: 'elevenlabs', cost, chars: spoken.length };
      } catch (e) {
        record({ dir: 'out', chat: chatKey, kind: 'voice', engine: 'elevenlabs', ok: false, error: clip(e.message, 120) });
      }
    const mp3 = join(dir, 'reply.mp3');
    const voice = lang === 'en' ? config.voice.ttsVoiceEn : config.voice.ttsVoiceEs;
    await run(config.voice.edgeTts, ['--voice', voice, '--text', spoken, '--write-media', mp3], { timeout: 60_000 });
    await run('ffmpeg', ['-v', 'error', '-y', '-i', mp3, '-c:a', 'libopus', '-b:a', '32k', '-ar', '48000', '-ac', '1', ogg], { timeout: 60_000 });
    return { file: ogg, engine: 'edge', cost: null, chars: spoken.length };
  }

  /** The tools' /health answers within 2 s. */
  const toolsUp = url => fetch(url, { signal: AbortSignal.timeout(2000) }).then(r => r.ok, () => false);

  /** The reply's pictures, at most `IMAGE_PARALLEL` at a time; each keeps its own caption and goes through send() with the trigger. */
  async function sendImages(chat, msg, route, dir, images) {
    if (!images.length) return;
    const t0 = now();
    let next = 0;
    const worker = async () => {
      for (let i = next++; i < images.length; i = next++) {
        const img = images[i];
        try {
          const file = await fetchImage(route, img.src, dir, i);
          // wacli keeps its connection 2 s after each send (for retry receipts) and sends queue on its store lock, so
          // that wait is most of the 3 s per picture: all but the last picture skip it.
          await send(chat, { kind: 'image', file, text: img.caption ? PREFIX + toWhatsApp(img.caption).slice(0, 900) : PREFIX.trim(), trigger: msg, postWait: i < images.length - 1 ? '0s' : null });
        } catch (e) {
          record({ dir: 'out', chat: chat.key, kind: 'image', ok: false, error: clip(e.message, 100) });
        }
      }
    };
    await Promise.all(Array.from({ length: Math.min(IMAGE_PARALLEL, images.length) }, worker));
    record({ dir: 'images', chat: chat.key, n: images.length, ms: now() - t0 });
  }

  /**
   * A file a tool made for this request, ready to send: copied into `dir`. Only the route's rack-reports folder and its
   * `fileDirs` (real paths), only office/PDF/text types, at most 25 MB, written in the last 3 hours; the app's own
   * download link of a rack report counts as the file. Anything else throws (and is logged by the caller).
   */
  async function fetchFile(route, src, dir, i) {
    let p = String(src);
    if (/^https?:\/\//.test(p)) {
      const m = /\/api\/rack-reports\/([a-f0-9]{24})\/([\w.-]+)$/.exec(p);
      if (!m || !route.rackReportsDir) throw new Error('not a file of the tools');
      p = join(route.rackReportsDir, m[1], m[2]);
    }
    if (!isAbsolute(p) || /['\0\n\\]/.test(p)) throw new Error('not an allowed path');
    const allowed = [route.rackReportsDir, ...(route.fileDirs ?? [])].filter(Boolean).map(d => (route.ssh ? d : (() => { try { return realpathSync(d); } catch { return d; } })()).replace(/\/+$/, ''));
    let real, mtime, size;
    if (route.ssh) {
      const out = (await run('ssh', ['-o', 'BatchMode=yes', route.ssh, `realpath -e -- '${p}' && stat -L -c '%Y %s' -- '${p}'`], { timeout: 30_000 })).trim().split('\n');
      real = out[0];
      [mtime, size] = String(out[1] ?? '').split(' ').map(Number);
    } else {
      real = realpathSync(p);
      const st = statSync(real);
      mtime = Math.floor(st.mtimeMs / 1000);
      size = st.size;
    }
    if (!real || !allowed.some(d => real.startsWith(d + '/'))) throw new Error('outside the tools\' folders');
    const name = basename(real);
    if (!/^[\w][\w. ()-]{0,120}\.(xlsx|csv|pdf|docx|txt|png|jpg|jpeg|gpx|kml)$/i.test(name)) throw new Error('file type not allowed');
    if (!(size > 0) || size > 25 * 1024 * 1024) throw new Error('file size not allowed');
    if (now() / 1000 - mtime > 3 * 3600) throw new Error('not made for this request (older than 3 h)');
    const out = join(dir, `file${i}`);
    mkdirSync(out, { recursive: true, mode: 0o700 });
    const file = join(out, name);
    if (route.ssh) await run('scp', ['-q', '-o', 'BatchMode=yes', `${route.ssh}:${real}`, file], { timeout: 120_000 });
    else copyFileSync(real, file);
    return file;
  }

  /** The reply's [files], one after the other, as documents, each through send()'s trigger guard; a refused file is logged. */
  async function sendFiles(chat, msg, route, dir, files) {
    for (const [i, f] of files.entries()) {
      let file;
      try {
        file = await fetchFile(route, f.src, dir, i);
      } catch (e) {
        record({ dir: 'out', chat: chat.key, kind: 'document', ok: false, refused: clip(e.message, 80), src: clip(f.src, 120) });
        continue;
      }
      try {
        await send(chat, { kind: 'document', file, document: basename(file), text: f.caption ? PREFIX + toWhatsApp(f.caption).slice(0, 900) : PREFIX.trim(), trigger: msg });
      } catch (e) {
        record({ dir: 'out', chat: chat.key, kind: 'document', ok: false, error: clip(e.message, 100) });
      }
    }
  }

  const HEARD = 'For a voice note, write the answer to be heard: short sentences; counts, years and decimals are read aloud, but links, codes (CAM…), dates, coordinates, IDs and phone numbers are left out of the voice note, so say what they are in words if they matter (the written text keeps everything).';
  async function turn(chat, msg, { steer = false } = {}) {
    const route = config.routes[chat.route];
    const started = now();
    const dir = mkdtempSync(join(tmpdir(), 'wa-bridge-'));
    // A steered message rides on the turn under way: its reactions come with that turn's end, unless T3 gave it a run of its own.
    const own = steer ? null : { riders: [] };
    const hostTurn = own; // (another `own` below names the history target)
    if (own) running.set(chat.key, own);
    let rider = false;
    // An untagged message that may end in silence never gets the edit mode's «pensando…» message: it uses the reaction (👀, then ✅
    // also on [silent]); so does a steered one (the status message belongs to the answer under way).
    const mode = msg.followUp || steer ? 'reaction' : ackMode(chat);
    // The edit mode: one message that shows the chat's own short notes as it works («Revisando las actas…»; never its reasoning,
    // never the tools it uses: those are only counted) and becomes the answer.
    const status = { id: null, steps: [], notes: [], assistant: '', shown: '', editing: Promise.resolve(), last: 0 };
    const showSteps = () => {
      const text = statusMessage([...status.notes, status.assistant]);
      if (!status.id || text === status.shown || now() - status.last < 3000) return;
      status.shown = text;
      status.last = now();
      status.editing = status.editing.then(() => send(chat, { kind: 'status', edit: status.id, text, trigger: msg }).catch(e => log.error?.('edit:', e.message)));
    };
    let ok = false;
    let spoke = false;
    try {
      // The route's tools (the lab app's MCP endpoint, for Franz's chat) answer before a model turn starts; if not, say so at once.
      if (route.toolsHealth && !(await toolsUp(route.toolsHealth))) {
        record({ dir: 'turn', chat: chat.key, id: msg.id, ok: false, error: 'tools down' });
        if (!msg.followUp) await say(chat, 'herramientas no disponibles ahora (la app de prueba está apagada); intenta en unos minutos.', 'notice', msg);
        return;
      }
      if (mode === 'edit') status.id = await send(chat, { kind: 'status', text: statusMessage([]), quote: msg, trigger: msg }).catch(() => null);
      const attachments = [];
      let sentVoice = false; // the message being answered is a voice note: the answer goes as voice too
      let heardText = null;
      for (const m of mediaFor(chat, msg)) {
        try {
          if (m.type === 'image') {
            const file = await fetchMedia(m, dir);
            const placed = await placePhoto(route, file, m.id);
            attachments.push(`photo ${placed.path} (identify_species files: ["${placed.name}"])`);
          } else {
            const t = await voiceForTurn(chat, m, dir, () => {
              status.steps.push('escuchando la nota de voz');
              showSteps();
            });
            // Only the message being answered (also when its transcription failed): a recent note of theirs in the context
            // does not make a typed question a voice one.
            if (m.id === msg.id) sentVoice = true;
            if (m.id === msg.id && t.text) heardText = t.text;
            record({ dir: 'voice', on: 'turn', chat: chat.key, id: m.id, engine: t.engine ?? null, reused: Boolean(t.reused), ok: Boolean(t.text) });
            attachments.push(t.text ? `voice note (transcribed by ${t.engine ?? '?'}, ${t.language ?? '?'}, may mishear names and codes): "${t.text}"` : `voice note: ${t.error ?? 'no speech in it'}`);
          }
        } catch (e) {
          attachments.push(`${m.type} that could not be downloaded (${clip(e.message, 60)})`);
        }
      }
      // In the group the header says who the sender is in the app: the account the private map on the route's machine
      // links to their JID, and whether it is an active admin there (config `people`: { map, database }, read by
      // t3-turn.mjs at turn time; the token below becomes that part). No `people`, unlinked or unknown: a guest.
      const people = isGroup(chat) ? chat.people ?? null : null;
      const roleToken = people ? `[[sender-role ${randomUUID()}]]` : null;
      // The role part comes first and the WhatsApp name (set by the sender) is quoted and cleaned, so a name cannot pose as a role.
      const waName = String(msg.fromMe ? 'Franz' : msg.pushName ?? 'sin nombre').replace(/[\s«»[\]·,:()]+/g, ' ').trim().slice(0, 40) || 'sin nombre';
      const who = `${roleToken ?? 'no linked app account: guest'} · WhatsApp name «${waName}»`;
      const head = chat.key === 'self' ? `[WhatsApp · Franz (self-chat) · ${hhmm(msg.at)}]` : `[WhatsApp · group «${chat.label}» · ${who} · ${hhmm(msg.at)}]`;
      const recent = recentMessages(chat, msg);
      const lines = [head];
      if (recent.length) {
        lines.push('Recent messages (oldest first):');
        for (const r of recent) lines.push(contextLine(r, msg.at));
      }
      // Where the older messages are, for questions that reach back further than the lines above.
      const own = historyTargets(config, chat).find(t => t.routeKey === chat.route);
      if (own) lines.push(`History: the file wa-history/${historySlug(chat, own.history)}.jsonl in your workspace has every message of this group (one JSON per line: id, at, name, text, media, replyTo; a voice note's transcript is a line { t: 'transcript', of: <the note's id>, engine, text }; when a note has two, the engine scribe one is better on names); search it with Grep or Read when the question needs earlier messages.`);
      else if (chat.key === 'self' && !chat.dryRun && !route.ssh) lines.push(`History: your chat's older messages are in the WhatsApp store, read-only: wacli --store ${config.wacli.store} messages list --read-only --json --chat ${chat.jid} --after YYYY-MM-DD --limit N (this chat only).`);
      // Read-only sources mirrored into this chat's workspace (e.g. «Ithomiini Ikiam» for a meeting agenda).
      // Other chats mirrored into this chat's workspace too (e.g. the underground group into Franz's own workspace).
      for (const src of chats.filter(c => c.enabled && c.key !== chat.key)) {
        const t = historyTargets(config, src).find(x => x.routeKey === chat.route);
        if (t) lines.push(`Read-only history of «${src.label}» (another group: read it for context such as a meeting agenda; ${src.readOnly ? 'you cannot write there and nothing you say reaches it' : 'you answer there only when someone there calls you'}): wa-history/${historySlug(src, t.history)}.jsonl in your workspace.`);
      }
      if (msg.followUp) lines.push('UNTAGGED FOLLOW-UP: this message does not tag @claude; it quotes one of your messages or addresses you by name («Claude, …») inside a conversation you were in during the last 15 minutes. That does not mean it is for you: people also quote you to point something out to the team. The default is [silent]; answer only if an answer from you is clearly expected, or you add something clearly useful (for instance a fact you got wrong). If unsure, your last message must be exactly [silent].');
      // The voice (config `soulFile`, one file or a list read in order: the channel's header, then the shared core; a chat's own
      // `soulFile` replaces the global one for that chat): sent on a thread's first turn, when it changes, and every 20 turns
      // so it outlives compaction. A chat with `soulInWorkspace: true` has it in its workspace's brief instead (CLAUDE.md, the
      // system context: `bridge.mjs voice-push <chat>` puts it there), so its messages carry only the envelope.
      const threadKey = chat.key;
      followRoute(chat);
      const soul = steer || chat.soulInWorkspace ? '' : [chat.soulFile ?? config.soulFile ?? []].flat().map(f => readFileSync(f, 'utf8').trim()).join('\n\n');
      if (soul) {
        const hash = createHash('sha256').update(soul).digest('hex').slice(0, 16);
        const sent = (state.soulSent ??= {})[threadKey];
        const fresh = !state.threads[threadKey] || sent?.hash !== hash || (sent?.turns ?? 0) >= 20;
        if (fresh) lines.splice(1, 0, 'Your voice (soul.md; the skill still decides what you say and when):', soul, '---');
        state.soulSent[threadKey] = { hash, turns: fresh ? 1 : (sent.turns ?? 0) + 1 };
      }
      // A chat with `toneLine: false` (a voice without tone levels) gets no tone line, whatever its /tono says.
      if (chat.toneLine !== false) lines.push(`Tone in this chat (/tono): ${toneOf(chat)}`);
      if (msg.voiceTrigger) lines.push('VOICE NOTE: this voice note does not tag @claude, but its transcript calls you by name, so it counts as an @claude message. The bridge itself puts the line «🎙️ «what was heard»» above your answer: do not repeat it. If the note only talks about you to someone else (for example «ayer Claude me dijo…») or the name was misheard, it is not for you: your last message must be exactly [silent].');
      lines.push('Message to answer:', `  ${msg.fromMe ? 'Franz' : msg.pushName ?? 'alguien'}: ${msg.text || (isVoice(msg.media?.type) ? '[nota de voz: su transcripción está en Attachments]' : '')}`);
      if (attachments.length) lines.push('Attachments:', ...attachments.map(a => `  ${a}`));
      if (steer) lines.push('(Franz sent this while you were still answering his previous message. If you are still working on that one, answer both in that same reply.)');
      lines.push(sentVoice
        ? `Voice: they sent a voice note, so the bridge also sends your answer as a voice note (read from your text), unless they asked for text: then add a line [text]. ${HEARD}`
        : `Voice: your answer goes as text. Only if they ask to hear it (en audio, nota de voz, háblame, léemelo), add a line [voice]: the bridge then also sends it as a voice note. ${HEARD}`);
      lines.push('(Answer per the whatsapp-bridge skill: your last message is sent as it is.)');
      const onStep = mode === 'edit' ? step => {
        if (step.kind !== 'assistant') {
          const label = stepLabel(step);
          if (label) status.steps.push(label); // counted in the log, not shown
          return;
        }
        const note = chat.key === 'self' ? stripHarnessBlocks(String(step.text ?? '')).text.trim().slice(0, 600) : '';
        // A note that grows while it streams replaces itself; a new one keeps the earlier note above it.
        if (status.assistant && note && !note.startsWith(status.assistant.slice(0, 40))) status.notes.push(status.assistant);
        status.assistant = note;
        showSteps();
      } : null;
      const r = await t3Turn(route, threadKey, (state.threads[threadKey] ? '' : 'Read the whatsapp-bridge skill first.\n') + lines.join('\n'), onStep, {
        steer,
        who: roleToken ? { token: roleToken, jid: bareJid(msg.sender), fromMe: Boolean(msg.fromMe), map: people.map ?? null, database: people.database ?? null } : null,
      });
      if (steer && r.ok && r.steered) {
        record({ dir: 'turn', chat: chat.key, id: msg.id, ok: true, steered: true, ms: now() - started, attachments: attachments.length });
        const host = running.get(chat.key);
        if (host) {
          host.riders.push(msg);
          if (sentVoice) host.sentVoice = true; // the one answer covers a voice note too
          rider = true;
        } else ok = spoke = true; // the answer it rode on has already gone out
        return;
      }
      await status.editing;
      state.lastTurn[chat.key] = { at: started, ms: now() - started, ok: Boolean(r.ok) };
      save();
      record({ dir: 'turn', chat: chat.key, id: msg.id, ok: Boolean(r.ok), status: r.status ?? null, error: r.error ?? null, ms: now() - started, attachments: attachments.length, steps: status.steps.length });
      if (!r.ok && !r.text && msg.followUp) return;
      if (!r.ok && !r.text) {
        const sorry = `No pude responder ahora (${r.error ?? r.status ?? 'error'}). Prueba de nuevo en un rato.`;
        if (status.id) await send(chat, { kind: 'notice', edit: status.id, text: PREFIX + sorry, trigger: msg }).catch(() => say(chat, sorry, 'notice', msg));
        else await say(chat, sorry, 'notice', msg);
        return;
      }
      ok = true;
      const reply = parseReply(r.text);
      // Harness blocks (<system-reminder>, <thinking>, tool calls) anywhere in the reply are taken off (logged); nothing sensible left → ⚠️.
      const blocks = stripHarnessBlocks(reply.text);
      if (blocks.stripped) {
        record({ dir: 'turn', chat: chat.key, id: msg.id, harnessBlocks: blocks.stripped, chars: reply.text.length - blocks.text.length });
        reply.text = blocks.text;
        if (!/[\p{L}\p{N}]/u.test(reply.text) && !reply.images.length && !reply.files.length) {
          ok = false;
          if (status.id) await send(chat, { kind: 'status', edit: status.id, text: `${PREFIX}${EMOJI.failed}`, trigger: msg }).catch(() => null);
          return;
        }
      }
      // A note to the operator before the reply is taken off (logged); when that leaves nothing sure to send, nothing goes and the message gets ⚠️.
      const cut = stripPreamble(reply.text);
      if (cut.stripped) {
        record({ dir: 'turn', chat: chat.key, id: msg.id, preamble: cut.stripped, unsure: cut.unsure, chars: reply.text.length - cut.text.length });
        if (cut.unsure) {
          ok = false;
          if (status.id) await send(chat, { kind: 'status', edit: status.id, text: `${PREFIX}${EMOJI.failed}`, trigger: msg }).catch(() => null);
          return;
        }
        reply.text = cut.text;
      }
      spoke = Boolean(reply.text || reply.images.length || reply.files.length);
      if (reply.silent) {
        if (status.id) await send(chat, { kind: 'status', edit: status.id, text: `${PREFIX}✅`, trigger: msg }).catch(() => null);
        return;
      }
      // The text goes first; the pictures are fetched and sent meanwhile, three at a time.
      const textDone = (async () => {
      if (reply.text) {
        // A voice-triggered answer starts with what was heard, so a mishearing is visible.
        const said = String(heardText ?? msg.heard ?? '').replace(/\s+/g, ' ').trim();
        const heardLine = msg.voiceTrigger && said ? `🎙️ «${said.slice(0, 80)}${said.length > 80 ? '…' : ''}»\n` : '';
        const text = PREFIX + heardLine + toWhatsApp(reply.text.replace(/^\s*claude\s*:\s*/i, '')).slice(0, 3500); // the bridge adds the «Claude: » itself
        // WhatsApp lets a message be edited for about 15 minutes; past that, or when the edit fails, the answer goes as a new message.
        const edited = status.id && now() - started < 14 * 60_000 ? await send(chat, { kind: 'reply', edit: status.id, text, trigger: msg }).then(() => true, () => false) : false;
        if (!edited) await send(chat, { kind: 'reply', text, quote: msg, trigger: msg });
      } else if (status.id) await send(chat, { kind: 'status', edit: status.id, text: `${PREFIX}✅`, trigger: msg }).catch(() => null);
      })();
      textDone.catch(() => {});
      await sendImages(chat, msg, route, dir, reply.images);
      await sendFiles(chat, msg, route, dir, reply.files);
      await textDone;
      // Voice (Franz's rules, 12 Nov 2026): [voice] (asked for a spoken answer), or the person sent a voice note and did not
      // ask for text ([text]); otherwise text only. The written answer always goes too, above.
      if (reply.text && (reply.voice || ((sentVoice || hostTurn?.sentVoice) && !reply.textOnly))) {
        let note = null;
        try {
          note = await voiceNote(reply.text, dir, chat.key);
          if (note) {
            await send(chat, { kind: 'voice', file: note.file, voice: true, trigger: msg });
            record({ dir: 'out', chat: chat.key, kind: 'voice', engine: note.engine, cost: note.cost, chars: note.chars });
          }
        } catch (e) {
          record({ dir: 'out', chat: chat.key, kind: 'voice', ok: false, engine: note?.engine ?? null, cost: note?.cost ?? null, error: clip(e.message, 100) });
        }
      }
    } finally {
      rmSync(dir, { recursive: true, force: true });
      if (own && running.get(chat.key) === own) running.delete(chat.key);
      if (!rider) {
        const emoji = ok ? (spoke ? EMOJI.replied : EMOJI.silent) : EMOJI.failed;
        await msg.seen;
        await reactTo(chat, msg, emoji);
        // The messages steered into this answer get the same mark.
        for (const m of own?.riders ?? []) {
          await m.seen;
          await reactTo(chat, m, emoji);
        }
      }
    }
  }

  /** No turn, steered message or voice note running or queued. */
  const idle = () => busy.size === 0 && steering.size === 0 && !voiceBusy && !voiceQueue.length && [...queues.values()].every(q => !q.length);
  return {
    handle,
    send,
    flushHistory,
    sendImages,
    sendFiles,
    state: () => state,
    followRoute,
    idle,
    transcribe,
    voiceForTurn,
    /**
     * Resolves true once nothing is left to do soon: idle (no turn, steered message or voice note running or queued), no history flush running or due
     * within a second (a dry-run chat's flush starts at once, a failed one's retry in a minute does not count); false
     * after `maxMs`. For tests: they wait for this instead of a fixed time.
     */
    async settled(maxMs = 10_000) {
      for (const until = Date.now() + maxMs; Date.now() < until; await sleep(5)) {
        const flushSoon = flushTimer !== null && flushDue - Date.now() < 1000;
        if (idle() && !flushing && !flushSoon) return true;
      }
      return false;
    },
  };
}

function serve() {
  const config = JSON.parse(readFileSync(CONFIG_FILE, 'utf8'));
  const secret = readFileSync(config.secretFile, 'utf8').trim();
  if (secret.length < 16) throw new Error('The webhook secret is too short');
  const bridge = createBridge({ config });
  const server = createServer((req, res) => {
    if (req.method === 'GET' && req.url === '/health') {
      res.writeHead(200, { 'content-type': 'application/json' });
      return res.end(JSON.stringify({ ok: true, stopped: bridge.state().stopped, idle: bridge.idle() }));
    }
    if (req.method !== 'POST' || req.url !== '/wacli') return res.writeHead(404).end();
    const parts = [];
    let size = 0;
    req.on('data', d => {
      size += d.length;
      if (size > 1024 * 1024) req.destroy();
      else parts.push(d);
    });
    req.on('end', () => {
      const raw = Buffer.concat(parts);
      if (!verify(secret, raw, req.headers['x-wacli-signature'])) return res.writeHead(403).end();
      res.writeHead(202).end();
      let body;
      try {
        body = JSON.parse(raw.toString('utf8'));
      } catch {
        return;
      }
      try {
        bridge.handle(body);
      } catch (e) {
        console.error('handle:', e.message);
      }
    });
  });
  server.listen(config.port, '127.0.0.1', () => console.log(`WhatsApp bridge on 127.0.0.1:${config.port}`));
}

/** Copies the bridge (and the two app modules it uses) to a folder of its own, so the unit outlives this checkout. */
function install() {
  const target = process.env.WA_BRIDGE_HOME || join(HOME, '.local/share/ithomiini-wa-bridge');
  for (const f of ['tools/whatsapp/bridge.mjs', 'tools/whatsapp/bridge-lib.mjs', 'tools/whatsapp/asr-parakeet.mjs', 'tools/whatsapp/dictation-words.json', 'tools/whatsapp/t3-turn.mjs', 'tools/whatsapp/t3-progress.mjs', 'assistant/voice/whatsapp.md', 'assistant/voice/core.md', 'server/t3commands.mjs', 'server/claude-usage.mjs']) {
    mkdirSync(dirname(join(target, f)), { recursive: true, mode: 0o700 });
    copyFileSync(join(ROOT, f), join(target, f));
  }
  const unit = join(HOME, '.config/systemd/user/ithomiini-wa-bridge.service');
  writeFileSync(
    unit,
    `[Unit]
Description=Ithomiini WhatsApp bridge (wacli webhook to T3; tools/whatsapp/bridge.mjs)
After=network-online.target

[Service]
Type=simple
ExecStart=${process.execPath} ${join(target, 'tools/whatsapp/bridge.mjs')} serve
Environment=PATH=${dirname(process.execPath)}:${join(HOME, '.local/bin')}:/usr/local/bin:/usr/bin:/bin
Restart=on-failure
RestartSec=5
UMask=0077

[Install]
WantedBy=default.target
`,
  );
  console.log(`Installed in ${target}; unit ${unit}`);
}

/**
 * Puts a chat's voice (its `soulFile` list, else the global one) in its route's workspace (`route.workspace`, on `route.ssh`
 * when set): .claude/voice/<files> and voice.json (the order, the source, a hash), then rewrites that workspace's brief
 * (the app's scripts/t3-provision.mjs --refresh <folder>, `route.provision`: the node and the script). The brief is the
 * thread's CLAUDE.md: Claude Code reads it when a session starts (checked 4 Nov 2026: a running process keeps the copy it
 * started with), so a running thread gets the new voice with its next Claude process: after T3 stops it when idle, or at
 * once with `restart` (the chat's thread is archived, which stops its process; its next message resumes the same chat with
 * the new brief). See tools/whatsapp/README.md, «The voice in the workspace». `dry`: says what it would do.
 */
async function voicePush(key, { dry = false, restart = false } = {}) {
  const config = JSON.parse(readFileSync(CONFIG_FILE, 'utf8'));
  const chat = config.chats?.[key];
  const route = config.routes?.[chat?.route];
  if (!chat || !route?.workspace) throw new Error(`Chat ${key}: no chat, or its route has no workspace`);
  const files = [chat.soulFile ?? config.soulFile ?? []].flat();
  if (!files.length) throw new Error(`Chat ${key}: no soulFile`);
  const names = files.map(f => basename(f));
  if (new Set(names).size !== names.length) throw new Error(`Chat ${key}: two voice files share a name`);
  const texts = files.map(f => readFileSync(f, 'utf8'));
  const hash = createHash('sha256').update(texts.join('\0')).digest('hex').slice(0, 16);
  const manifest = JSON.stringify({ files: names, from: dirname(files[0]), hash, at: new Date().toISOString() }, null, 1) + '\n';
  const folder = join(route.workspace, '.claude', 'voice');
  const q = s => `'${String(s).replaceAll("'", "'\\''")}'`;
  const sh = (cmd, input = null) => (route.ssh ? run('ssh', [route.ssh, cmd], { input }) : run('sh', ['-c', cmd], { input }));
  const [node, script] = route.provision ?? [];
  console.log(`${key}: ${names.join(', ')} (${hash}) → ${route.ssh ? route.ssh + ':' : ''}${folder}`);
  if (dry) return hash;
  await sh(`mkdir -p ${q(folder)}`);
  for (const [i, name] of names.entries()) await sh(`cat > ${q(join(folder, name))}`, texts[i]);
  await sh(`cat > ${q(join(folder, 'voice.json'))}`, manifest);
  if (node && script) process.stdout.write(await sh(`${q(node)} ${q(script)} --refresh ${q(basename(route.workspace))}`));
  else console.log('No route.provision: the brief is rewritten at the next deploy (--refresh-all).');
  if (restart) {
    const threadId = JSON.parse(readFileSync(join(STATE_DIR, 'state.json'), 'utf8')).threads?.[key];
    if (!threadId) return console.log('No thread yet: its first turn starts with the new voice.'), hash;
    const ask = op => {
      const req = JSON.stringify({ op, home: route.home, local: route.local, tokenFile: route.tokenFile, commandsModule: route.commandsModule, threadId });
      return (route.ssh ? run('ssh', [route.ssh, `${route.node} ${route.script}`], { input: req }) : run(route.node ?? process.execPath, [route.script ?? join(HERE, 't3-turn.mjs')], { input: req })).then(out => JSON.parse(out.trim().split('\n').at(-1)));
    };
    if ((await ask('busy')).busy) console.log(`Thread ${threadId} is answering: not restarted (run voice-push ${key} --restart again when it is idle).`);
    else console.log(`Thread ${threadId} restarted: ${JSON.stringify(await ask('archive'))}`);
  }
  return hash;
}

/** Copies a chat's last `days` days from the wacli store (read-only) into its history file; idempotent. */
async function backfill(days = 30, only = null) {
  const config = JSON.parse(readFileSync(CONFIG_FILE, 'utf8'));
  let total = 0;
  for (const [key, c] of Object.entries(config.chats ?? {})) {
    const chat = { key, ...c };
    if (!historyTargets(config, chat).length || !c.enabled || (only && key !== only)) continue;
    const lines = historyRows(config, chat, Date.now() - days * 86_400_000).map(r =>
      historyLine({ id: r.id, at: r.ts * 1000, name: r.name, text: r.text, media: r.media_type, ...(r.quoted_msg_id ? { replyTo: r.quoted_msg_id, replyName: r.qname, replyText: r.qtext } : {}) }),
    );
    let wrote = 0;
    for (const t of historyTargets(config, chat))
      for (let i = 0; i < lines.length; i += 500) wrote += await historyAppend(t.route, t.history, chat, lines.slice(i, i + 500));
    console.log(`${key}: ${lines.length} messages read, ${wrote} new lines in the history file`);
    total += lines.length;
  }
  return total;
}

/** A local audio file through the configured engines (default Parakeet only): no WhatsApp, no turn, a throwaway state folder. */
async function voiceCheck(file, order) {
  const config = JSON.parse(readFileSync(CONFIG_FILE, 'utf8'));
  const stateDir = mkdtempSync(join(tmpdir(), 'wa-voice-check-'));
  try {
    const bridge = createBridge({ config: { ...config, chats: {} }, stateDir, log: console });
    const t = await bridge.transcribe(resolve(file), order.length ? order : ['parakeet']);
    console.log(JSON.stringify({ engine: t.engine ?? null, ms: t.ms ?? null, audioMs: t.audioMs ?? null, chars: t.text?.length ?? 0, error: t.error ?? null, text: t.text ?? null }));
    if (t.error) process.exitCode = 1;
  } finally {
    rmSync(stateDir, { recursive: true, force: true });
  }
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const cmd = process.argv[2];
  if (cmd === 'serve') serve();
  else if (cmd === 'install') install();
  else if (cmd === 'backfill') backfill(Number(process.argv[3]) || 30).catch(e => (console.error(e.message), process.exit(1)));
  else if (cmd === 'voice-push') voicePush(process.argv[3], { dry: process.argv.includes('--dry'), restart: process.argv.includes('--restart') }).catch(e => (console.error(e.message), process.exit(1)));
  else if (cmd === 'voice-check') voiceCheck(process.argv[3], process.argv.slice(4)).catch(e => (console.error(e.message), process.exit(1)));
  else console.error('Usage: bridge.mjs serve | install | backfill [days] | voice-push <chat> [--dry] [--restart] | voice-check <audio file> [engine…]');
}
