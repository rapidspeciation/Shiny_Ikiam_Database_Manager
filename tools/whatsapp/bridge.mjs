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
// Group → the T3 project «Ithomiini 🦋 (underground)» on claudeclaw (production tools, proposals
// only: its workspace's settings deny applying), over ssh with tools/whatsapp/t3-turn.mjs. Franz's chat → «Orchestrator wpp»
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
import { copyFileSync, existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync, statSync, writeFileSync, appendFileSync, renameSync, chmodSync } from 'node:fs';
import { homedir, tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { DatabaseSync } from 'node:sqlite';
import { appendNew, mediaTag, named, slug, ACK_MODES, KEYTERMS, PREFIX, bareJid, command, decryptMedia, imageKind, lastHour, ownAnswer, parsePayload, parseReply, speakable, statusMessage, stepLabel, tagged, toWhatsApp, verify, voiceLanguage } from './bridge-lib.mjs';

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

export function createBridge({ config, stateDir = STATE_DIR, now = Date.now, log = console }) {
  mkdirSync(stateDir, { recursive: true, mode: 0o700 });
  const stateFile = join(stateDir, 'state.json');
  const logFile = join(stateDir, 'bridge.log.jsonl');
  const outboxFile = join(stateDir, 'outbox.jsonl');
  let state = { stopped: false, threads: {}, sends: {}, seen: [], lastTurn: {}, botIds: [], transcripts: {} };
  try {
    state = { ...state, ...JSON.parse(readFileSync(stateFile, 'utf8')) };
  } catch {
    /* first start */
  }
  const save = () => {
    state.seen = state.seen.slice(-500);
    state.botIds = (state.botIds ?? []).slice(-500);
    state.transcripts = Object.fromEntries(Object.entries(state.transcripts ?? {}).slice(-200));
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
  const admin = msg => msg.fromMe;
  // The group's rule: a message wakes the bridge only if it tags @claude or replies to one of the bridge's own messages.
  const isGroup = chat => String(chat.jid ?? '').endsWith('@g.us');
  const CONVO_MS = 15 * 60_000;
  const active = chat => (state.convo?.[chat.key] ?? 0) > now();
  const openConvo = chat => ((state.convo ??= {})[chat.key] = now() + CONVO_MS);
  const wakes = msg => Boolean(msg) && (tagged(msg.text) || Boolean(msg.replyToId && (state.botIds ?? []).includes(msg.replyToId)));
  const ring = new Map(); // dry-run chats' recent messages (the lab's fake group has no wacli history)
  const queues = new Map();
  const busy = new Set();

  // --- the group's history, mirrored to the chat's own workspace (only chats with a `history` setting)
  const queueFile = join(stateDir, 'history-queue.jsonl');
  let flushing = false;
  let flushTimer = null;
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
          await historyAppend(config.routes[chat.route], chat.history, chat, mine.map(e => e.line));
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
      if (rest.length) flushTimer = setTimeout(() => ((flushTimer = null), flushHistory().catch(() => {})), 60_000).unref();
    } finally {
      flushing = false;
    }
    return done;
  }
  const queueHistory = (chat, line) => {
    appendFileSync(queueFile, JSON.stringify({ chat: chat.key, line }) + '\n', { mode: 0o600 });
    if (!flushTimer) flushTimer = setTimeout(() => ((flushTimer = null), flushHistory().catch(e => log.error?.('history:', e.message))), chat.dryRun && !config.routes[chat.route]?.ssh ? 0 : 3000).unref();
  };
  function mirror(chat, msg) {
    if (!chat.history?.dir || msg.skip || (!msg.text && !msg.media)) return;
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
  async function send(chat, { kind, text = null, file = null, voice = false, quote = null, react = null, edit = null, trigger, postWait = null }) {
    const entry = { dir: 'out', chat: chat.key, kind, chars: text?.length ?? 0, file: Boolean(file), ...(edit ? { edit: true } : {}), ...(react ? { emoji: react.emoji } : {}) };
    let id = null;
    if (isGroup(chat) && !(wakes(trigger) || (trigger && active(chat)))) {
      record({ ...entry, refused: 'group send outside an active conversation' });
      throw new Error('Refused: no trigger');
    }
    if (chat.dryRun) {
      // The file is kept for the test to look at (the turn's own copy is deleted after it).
      let kept = null;
      if (file) {
        mkdirSync(join(stateDir, 'outbox-media'), { recursive: true, mode: 0o700 });
        kept = join(stateDir, 'outbox-media', `${now()}-${file.split('/').pop()}`);
        copyFileSync(file, kept);
      }
      id = edit ?? `OUT${now()}${Math.random().toString(16).slice(2, 8)}`;
      appendFileSync(outboxFile, JSON.stringify({ at: new Date(now()).toISOString(), chat: chat.key, kind, id, text, file: kept, voice, quote: quote?.id ?? null, react, edit }) + '\n', { mode: 0o600 });
      record({ ...entry, via: 'outbox' });
    } else {
      if (!SENDABLE.has(chat.jid)) {
        record({ ...entry, refused: 'not an allowed chat' });
        throw new Error('Refused: not an allowed chat');
      }
      const q = quote ? ['--reply-to', quote.id, ...(chat.jid.endsWith('@g.us') && quote.sender ? ['--reply-to-sender', quote.sender] : [])] : [];
      const args = react
        ? ['send', 'react', '--to', chat.jid, '--id', react.id, '--reaction', react.emoji, ...(chat.jid.endsWith('@g.us') && react.sender ? ['--sender', react.sender] : [])]
        : edit
          ? ['messages', 'edit', '--chat', chat.jid, '--id', edit, '--message', text]
          : voice
            ? ['send', 'voice', '--to', chat.jid, '--file', file, ...q]
            : file
              ? ['send', 'file', '--to', chat.jid, '--file', file, ...(text ? ['--caption', text] : []), ...(postWait ? ['--post-send-wait', postWait] : []), ...q]
              : ['send', 'text', '--to', chat.jid, '--message', text, ...(chat.key === 'self' ? ['--allow-self'] : []), ...q];
      try {
        const out = await wacli(['--json', ...args], { timeout: 90_000 });
        try {
          id = JSON.parse(out)?.data?.id ?? null;
        } catch {
          /* no id */
        }
        record({ ...entry, via: 'wacli', ok: true });
      } catch (e) {
        record({ ...entry, via: 'wacli', ok: false, error: clip(e.message, 200) });
        throw e;
      }
    }
    if (id && !edit) (state.botIds ??= []).push(String(id));
    if (isGroup(chat) && ['reply', 'image', 'voice'].includes(kind)) openConvo(chat); // an answer of Claude's keeps the conversation open
    if (kind === 'reply') (state.sends[chat.key] ??= []).push(now());
    state.sends[chat.key] = lastHour(state.sends[chat.key], now());
    save();
    return id;
  }
  /** How a chat shows it is working: Franz's /ack choice, else the config's (reaction by default). */
  const ackMode = chat => state.ack?.[chat.key] ?? chat.ack ?? 'reaction';
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
    '*Asistente de prueba de Franz* (responde solo si le escriben @claude)',
    '- @claude <pregunta>: consulta la base de datos, fotos, mapas, propuestas de cambio (con enlace)',
    '- Nota de voz o foto, y luego un mensaje con @claude (o responde a ella con @claude)',
    '- /status – estado del puente',
    '- /usage – uso de la cuenta de Claude',
    '- /whoami – quién eres para el asistente',
    '- /stop, /start – solo Franz: apagar o encender todo',
    '- /ack reaction | edit – solo Franz: 👀/✅ en el mensaje, o un mensaje con los pasos',
  ].join('\n');

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
      if (!ACK_MODES.includes(mode)) return say(chat, `Aviso de trabajo aquí: ${ackMode(chat)}. Opciones: /ack reaction (👀 y luego ✅), /ack edit (un mensaje que muestra los pasos y se vuelve la respuesta).`, 'reply', msg);
      (state.ack ??= {})[chat.key] = mode;
      save();
      record({ dir: 'state', ack: mode, chat: chat.key });
      return say(chat, `Aviso de trabajo aquí: ${mode}.`, 'reply', msg);
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
    if (state.seen.includes(msg.id)) return out('skip', 'duplicate');
    if (msg.text || msg.media) state.seen.push(msg.id);
    save();
    if (msg.skip) return out('skip', msg.skip);
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
      else return out('skip', 'no @claude');
      save();
    } else if (!tagged(msg.text)) return out('skip', 'no @claude');
    if (overLimit(chat, msg)) return { decision: 'limited' };
    const q = queues.get(chat.key) ?? [];
    if (q.length >= 5) return out('skip', 'queue full');
    q.push(msg);
    queues.set(chat.key, q);
    out('queued', msg.followUp ? 'follow-up' : null);
    // The 👀 only here, after the gate: a message that never passed it gets no reaction at all.
    if (ackMode(chat) === 'reaction') msg.seen = reactTo(chat, msg, '👀');
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
      return rows.filter(r => r.type === 'image' || r.type === 'audio' || r.type === 'ptt').slice(-4);
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

  async function transcribe(file) {
    const audio = readFileSync(file);
    if (audio.length > 5 * 1024 * 1024) return { error: 'nota de voz de más de 5 MB' };
    try {
      const env = Object.fromEntries(
        readFileSync(config.voice.elevenlabsEnv, 'utf8')
          .split('\n')
          .filter(l => l.includes('='))
          .map(l => [l.slice(0, l.indexOf('=')).trim(), l.slice(l.indexOf('=') + 1).trim().replace(/^"|"$/g, '')]),
      );
      const form = new FormData();
      form.append('model_id', 'scribe_v2');
      form.append('tag_audio_events', 'false');
      for (const k of KEYTERMS) form.append('keyterms', k);
      form.append('file', new Blob([audio], { type: 'audio/ogg' }), 'voice.ogg');
      for (let attempt = 0; attempt < 2; attempt++) {
        const res = await fetch('https://api.elevenlabs.io/v1/speech-to-text', { method: 'POST', headers: { 'xi-api-key': env.ELEVENLABS_API_KEY }, body: form, signal: AbortSignal.timeout(30_000) }).catch(e => ({ ok: false, status: e.name }));
        if (res.ok) {
          const j = await res.json();
          return { text: String(j.text ?? '').trim(), language: j.language_code ?? null, engine: 'scribe' };
        }
        record({ dir: 'voice', engine: 'scribe', ok: false, status: res.status });
      }
    } catch (e) {
      record({ dir: 'voice', engine: 'scribe', ok: false, error: clip(e.message, 100) });
    }
    // The local fallback: faster-whisper small (int8, 2 threads), the genera as its prompt.
    const wav = file + '.wav';
    try {
      await run('ffmpeg', ['-v', 'error', '-y', '-i', file, '-ar', '16000', '-ac', '1', wav], { timeout: 60_000 });
      const py = `import sys,json\nfrom faster_whisper import WhisperModel\nm=WhisperModel("small",device="cpu",compute_type="int8",cpu_threads=2)\nseg,info=m.transcribe(sys.argv[1],beam_size=5,vad_filter=True,initial_prompt=sys.argv[2])\nprint(json.dumps({"text":" ".join(s.text.strip() for s in seg),"language":info.language}))`;
      const out = await run('nice', ['-n', '10', config.voice.whisperPython, '-c', py, wav, KEYTERMS.join(', ')], { timeout: 600_000 });
      const j = JSON.parse(out.trim().split('\n').pop());
      return { text: j.text, language: j.language, engine: 'whisper' };
    } catch (e) {
      return { error: `no pude transcribir (${clip(e.message, 80)})` };
    } finally {
      rmSync(wav, { force: true });
    }
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
  async function t3Turn(route, threadKey, text, onStep = null) {
    const req = {
      op: 'turn',
      home: route.home,
      local: route.local,
      tokenFile: route.tokenFile,
      commandsModule: route.commandsModule,
      threadId: state.threads[threadKey] ?? null,
      text,
      progress: Boolean(onStep),
      timeoutMs: route.timeoutMs ?? 12 * 60_000,
      create: { projectId: route.projectId, title: route.title, model: route.model, effort: route.effort },
    };
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

  async function voiceNote(text, dir) {
    const spoken = speakable(text);
    if (!spoken) return null;
    const mp3 = join(dir, 'reply.mp3');
    const ogg = join(dir, 'reply.ogg');
    const voice = voiceLanguage(spoken) === 'en' ? config.voice.ttsVoiceEn : config.voice.ttsVoiceEs;
    await run(config.voice.edgeTts, ['--voice', voice, '--text', spoken, '--write-media', mp3], { timeout: 60_000 });
    await run('ffmpeg', ['-v', 'error', '-y', '-i', mp3, '-c:a', 'libopus', '-b:a', '32k', '-ar', '48000', '-ac', '1', ogg], { timeout: 60_000 });
    return ogg;
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

  async function turn(chat, msg) {
    const route = config.routes[chat.route];
    const started = now();
    const dir = mkdtempSync(join(tmpdir(), 'wa-bridge-'));
    // An untagged message that may end in silence never gets the edit mode's «pensando…» message: it uses the reaction (👀, then ✅ also on [silent]).
    const mode = msg.followUp ? 'reaction' : ackMode(chat);
    // The edit mode: one message that lists the chat's steps (tool names, never its reasoning) and becomes the answer.
    const status = { id: null, steps: [], shown: '', editing: Promise.resolve(), last: 0 };
    const showSteps = () => {
      const text = statusMessage(status.steps);
      if (!status.id || text === status.shown || now() - status.last < 3000) return;
      status.shown = text;
      status.last = now();
      status.editing = status.editing.then(() => send(chat, { kind: 'status', edit: status.id, text, trigger: msg }).catch(e => log.error?.('edit:', e.message)));
    };
    let ok = false;
    try {
      // The route's tools (the lab app's MCP endpoint, for Franz's chat) answer before a model turn starts; if not, say so at once.
      if (route.toolsHealth && !(await toolsUp(route.toolsHealth))) {
        record({ dir: 'turn', chat: chat.key, id: msg.id, ok: false, error: 'tools down' });
        if (!msg.followUp) await say(chat, 'herramientas no disponibles ahora (la app de prueba está apagada); intenta en unos minutos.', 'notice', msg);
        return;
      }
      if (mode === 'edit') status.id = await send(chat, { kind: 'status', text: statusMessage([]), quote: msg, trigger: msg }).catch(() => null);
      const attachments = [];
      let heardVoice = false;
      for (const m of mediaFor(chat, msg)) {
        try {
          const file = await fetchMedia(m, dir);
          if (m.type === 'image') {
            const placed = await placePhoto(route, file, m.id);
            attachments.push(`photo ${placed.path} (identify_species files: ["${placed.name}"])`);
          } else {
            status.steps.push('escuchando la nota de voz');
            showSteps();
            const t = await transcribe(file);
            if (t.text) {
              (state.transcripts ??= {})[m.id] = clip(t.text, 400);
              if (chat.history?.dir) queueHistory(chat, { t: 'transcript', of: m.id, at: new Date(now()).toISOString(), text: clip(t.text, 1500) });
            }
            heardVoice = heardVoice || Boolean(t.text);
            record({ dir: 'voice', chat: chat.key, id: m.id, engine: t.engine ?? null, ok: Boolean(t.text) });
            attachments.push(t.text ? `voice note (transcribed, ${t.language ?? '?'}, may mishear names and codes): "${t.text}"` : `voice note: ${t.error}`);
          }
        } catch (e) {
          attachments.push(`${m.type} that could not be downloaded (${clip(e.message, 60)})`);
        }
      }
      const who = msg.fromMe ? 'Franz (admin)' : `member ${msg.pushName ?? 'sin nombre'}`;
      const head = chat.key === 'self' ? `[WhatsApp · Franz (self-chat) · ${hhmm(msg.at)}]` : `[WhatsApp · group «${chat.label}» · ${who} · ${hhmm(msg.at)}]`;
      const recent = recentMessages(chat, msg);
      const lines = [head];
      if (recent.length) {
        lines.push('Recent messages (oldest first):');
        for (const r of recent) lines.push(contextLine(r, msg.at));
      }
      // Where the older messages are, for questions that reach back further than the lines above.
      if (chat.history?.dir) lines.push(`History: the file wa-history/${chat.history.slug ?? slug(chat.label)}.jsonl in your workspace has every message of this group (one JSON per line: id, at, name, text, media, replyTo); search it with Grep or Read when the question needs earlier messages.`);
      else if (chat.key === 'self' && !chat.dryRun) lines.push(`History: your chat's older messages are in the WhatsApp store, read-only: wacli --store ${config.wacli.store} messages list --read-only --json --chat ${chat.jid} --after YYYY-MM-DD --limit N (this chat only).`);
      if (msg.followUp) lines.push('UNTAGGED FOLLOW-UP: this message does not tag @claude; it quotes one of your messages or addresses you by name («Claude, …») inside a conversation you were in during the last 15 minutes. That does not mean it is for you: people also quote you to point something out to the team. The default is [silent]; answer only if an answer from you is clearly expected, or you add something clearly useful (for instance a fact you got wrong). If unsure, your last message must be exactly [silent].');
      lines.push('Message to answer:', `  ${msg.fromMe ? 'Franz' : msg.pushName ?? 'alguien'}: ${msg.text}`);
      if (attachments.length) lines.push('Attachments:', ...attachments.map(a => `  ${a}`));
      lines.push('(Answer per the whatsapp-bridge skill: your last message is sent as it is.)');
      const threadKey = chat.key;
      const onStep = mode === 'edit' ? step => (status.steps.push(stepLabel(step)), showSteps()) : null;
      const r = await t3Turn(route, threadKey, (state.threads[threadKey] ? '' : 'Read the whatsapp-bridge skill first.\n') + lines.join('\n'), onStep);
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
      if (reply.silent) {
        if (status.id) await send(chat, { kind: 'status', edit: status.id, text: `${PREFIX}✅`, trigger: msg }).catch(() => null);
        return;
      }
      // The text goes first; the pictures are fetched and sent meanwhile, three at a time.
      const textDone = (async () => {
      if (reply.text) {
        const text = PREFIX + toWhatsApp(reply.text).slice(0, 3500);
        // WhatsApp lets a message be edited for about 15 minutes; past that, or when the edit fails, the answer goes as a new message.
        const edited = status.id && now() - started < 14 * 60_000 ? await send(chat, { kind: 'reply', edit: status.id, text, trigger: msg }).then(() => true, () => false) : false;
        if (!edited) await send(chat, { kind: 'reply', text, quote: msg, trigger: msg });
      } else if (status.id) await send(chat, { kind: 'status', edit: status.id, text: `${PREFIX}✅`, trigger: msg }).catch(() => null);
      })();
      textDone.catch(() => {});
      await sendImages(chat, msg, route, dir, reply.images);
      await textDone;
      if (reply.text && (reply.voice || heardVoice)) {
        try {
          const ogg = await voiceNote(reply.text, dir);
          if (ogg) await send(chat, { kind: 'voice', file: ogg, voice: true, trigger: msg });
        } catch (e) {
          record({ dir: 'out', chat: chat.key, kind: 'voice', ok: false, error: clip(e.message, 100) });
        }
      }
    } finally {
      rmSync(dir, { recursive: true, force: true });
      if (mode === 'reaction') {
        await msg.seen;
        await reactTo(chat, msg, ok ? '✅' : '⚠️');
      }
    }
  }

  return {
    handle,
    send,
    flushHistory,
    sendImages,
    state: () => state,
    idle: () => busy.size === 0 && [...queues.values()].every(q => !q.length),
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
  for (const f of ['tools/whatsapp/bridge.mjs', 'tools/whatsapp/bridge-lib.mjs', 'tools/whatsapp/t3-turn.mjs', 'server/t3commands.mjs', 'server/claude-usage.mjs']) {
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

/** Copies a chat's last `days` days from the wacli store (read-only) into its history file; idempotent. */
async function backfill(days = 30, only = null) {
  const config = JSON.parse(readFileSync(CONFIG_FILE, 'utf8'));
  let total = 0;
  for (const [key, c] of Object.entries(config.chats ?? {})) {
    if (!c.history?.dir || !c.enabled || (only && key !== only)) continue;
    const chat = { key, ...c };
    const lines = historyRows(config, chat, Date.now() - days * 86_400_000).map(r =>
      historyLine({ id: r.id, at: r.ts * 1000, name: r.name, text: r.text, media: r.media_type, ...(r.quoted_msg_id ? { replyTo: r.quoted_msg_id, replyName: r.qname, replyText: r.qtext } : {}) }),
    );
    let wrote = 0;
    for (let i = 0; i < lines.length; i += 500) wrote += await historyAppend(config.routes[chat.route], chat.history, chat, lines.slice(i, i + 500));
    console.log(`${key}: ${lines.length} messages read, ${wrote} new lines in the history file`);
    total += lines.length;
  }
  return total;
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const cmd = process.argv[2];
  if (cmd === 'serve') serve();
  else if (cmd === 'install') install();
  else if (cmd === 'backfill') backfill(Number(process.argv[3]) || 30).catch(e => (console.error(e.message), process.exit(1)));
  else console.error('Usage: bridge.mjs serve | install');
}
