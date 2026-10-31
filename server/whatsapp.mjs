// The WhatsApp relay (lab prototype; design in .work/whatsapp-assistant-20261030/bridge-design.md):
// wacli posts each live WhatsApp message here (signed with HMAC-SHA256), and the relay decides,
// without any model, whether the shared «WhatsApp» T3 chat should see it:
//   1. the signature, then duplicates dropped by message id;
//   2. the whitelist: a DM from an enabled person, or an enabled group (and the sender on the list
//      for a wake). Strangers are logged without their text and get no answer;
//   3. in groups: a mention of the bot, a reply to one of its messages, its name or a /command
//      wakes the chat; a CAM whose species or sex disagrees with the database, or a protocol
//      question nobody answered in 20 min, is a candidate (groups in `helpful` mode); the rest is context;
//   4. rate limits per number, per group (unprompted replies) and for all wakes;
//   5. a chat's messages wait 20 s after its last one (60 s after its first at most), then go to
//      T3 as one message, only while the WhatsApp chat is not answering (server/t3tell.mjs).
// The chat answers with the MCP tool whatsapp_reply, which only its own token (the service user's)
// has. In this prototype a reply goes to the outbox (wa_outbox and a JSONL file) and never to
// WhatsApp; WA_SENDER=wacli is the switch for sending with `wacli send` later.
// Everything in and out is kept in wa_inbox / wa_outbox and in a JSONL log beside the database.
import { createHash, createHmac, randomBytes, randomUUID, timingSafeEqual } from 'node:crypto';
import { appendFileSync, existsSync, statSync } from 'node:fs';
import { execFile } from 'node:child_process';
import { t3Commands } from './t3commands.mjs';

const MINUTE = 60_000;
const HOUR = 60 * MINUTE;
const DAY = 24 * HOUR;
/** Ecuador is UTC-5 all year. */
const ECUADOR_MS = -5 * HOUR;
export const DEFAULTS = {
  debounceMs: 20_000,
  maxWaitMs: 60_000,
  /** A protocol question waits this long for a teammate's answer before it goes to the chat. */
  questionWaitMs: 20 * MINUTE,
  /** No unprompted replies at night (quietHours); false only for lab runs. */
  quietHours: true,
  perNumberHour: 20,
  perNumberDay: 80,
  groupUnpromptedDay: 3,
  wakesDay: 150,
  sendsHour: 40,
  /** A turn that never showed as answering counts as done after this long. */
  turnGraceMs: 30_000,
  /** Context lines of a group before a woken message: this many, from this far back. */
  contextLines: 6,
  contextMs: 30 * MINUTE,
  maxVideoBytes: 20 * 1024 * 1024,
};
/** The shared chat's model (memory: Sonnet or Opus on the server, never Fable). */
const MODEL = {
  instanceId: 'claudeAgent',
  model: 'claude-sonnet-5-5',
  options: [
    { id: 'effort', value: 'medium' },
    { id: 'fastMode', value: false },
  ],
};
const PROTOCOL_WORDS =
  /\b(marc\w*|preserv\w*|transect\w*|monitor\w*|recaptur\w*|clima|weather|lluvi\w*|lluev\w*|rain\w*|protocol\w*|cam\w*|tubo\w*|ala\w*|wing\w*|insectario|insectary|colect\w*|collect\w*|muert\w*|emergid\w*|clutch\w*|huevo\w*|larva\w*|pupa\w*|T[1-4])\b|\b30\b/i;
const QUESTION_START =
  /^\s*(¿|qu[eé]|c[oó]mo|cu[aá]nd[oa]s?|cu[aá]nt[oa]s?|d[oó]nde|cu[aá]l(es)?|por qu[eé]|se puede|hay que|alguien|what|how|when|where|which|why|should|do we|does|is it|can we)\b/i;
const CAM = /\bCAM\d{6}\b/gi;
const SEX_WORDS = [
  [/\b(macho|male)\b|♂/i, 'male'],
  [/\b(hembra|female)\b|♀/i, 'female'],
];

const clip = (s, n) => (s == null ? '' : String(s).slice(0, n));
const fail = (code, message, status) => Object.assign(new Error(message), { code, status });

/** `sha256=<hex>` of `raw` with `secret`, as wacli's X-Wacli-Signature. */
export function sign(secret, raw) {
  return 'sha256=' + createHmac('sha256', secret).update(raw).digest('hex');
}
/** Whether `header` is the signature of `raw`. */
export function verifySignature(secret, raw, header) {
  if (!secret || typeof header !== 'string') return false;
  const want = Buffer.from(sign(secret, raw));
  const given = Buffer.from(header.trim());
  return want.length === given.length && timingSafeEqual(want, given);
}

/** +<digits> for a phone JID (593991234567@s.whatsapp.net, with or without :device), else null. */
export function phoneOf(jid) {
  const m = /^\+?(\d{6,16})(?::\d+)?(?:@s\.whatsapp\.net|@c\.us)?$/.exec(String(jid ?? '').trim());
  return m ? '+' + m[1] : null;
}
const pick = (o, ...keys) => {
  for (const k of keys) {
    const v = k.split('.').reduce((x, p) => (x == null ? x : x[p]), o);
    if (v !== undefined && v !== null && v !== '') return v;
  }
  return null;
};
function timeOf(v, fallback) {
  if (v == null) return fallback;
  if (typeof v === 'number' || /^\d+$/.test(String(v))) {
    const n = Number(v);
    return n < 1e12 ? n * 1000 : n;
  }
  const t = Date.parse(v);
  return Number.isNaN(t) ? fallback : t;
}

/**
 * A wacli webhook body as one message: { id, chat, chatName, isGroup, sender, senderName, text,
 * at (ms), fromMe, mentions[], quotedId, quotedSender, media } or null when it is no message.
 * Tolerant of field names (snake_case and Go's), since the pilot will record real bodies.
 */
export function normalizeEvent(body, now = Date.now()) {
  if (!body || typeof body !== 'object') return null;
  const kind = pick(body, 'event', 'type', 'event_type');
  if (kind && !/message/i.test(String(kind))) return null;
  const m = body.message && typeof body.message === 'object' ? { ...body, ...body.message } : body;
  const id = pick(m, 'msg_id', 'id', 'ID', 'message_id');
  const chat = pick(m, 'chat_jid', 'chat', 'Chat', 'chatJID');
  if (!id || !chat) return null;
  const isGroup = String(chat).endsWith('@g.us');
  const sender = pick(m, 'sender_jid', 'sender', 'Sender', 'participant') ?? (isGroup ? null : chat);
  const mediaType = pick(m, 'media_type', 'media.type', 'mediaType');
  const mediaPath = pick(m, 'media_path', 'local_path', 'media.path', 'file');
  const mentions = pick(m, 'mentions', 'mentioned_jids', 'context.mentioned_jid') ?? [];
  return {
    id: String(id),
    chat: String(chat),
    chatName: pick(m, 'chat_name', 'ChatName', 'group_name'),
    isGroup,
    sender: sender ? String(sender) : null,
    senderName: pick(m, 'sender_name', 'push_name', 'PushName', 'pushName'),
    text: String(pick(m, 'text', 'body', 'Text', 'content', 'caption') ?? ''),
    at: timeOf(pick(m, 'timestamp', 'ts', 'Timestamp', 'time', 'message_time'), now),
    fromMe: Boolean(pick(m, 'from_me', 'FromMe', 'fromMe')),
    mentions: Array.isArray(mentions) ? mentions.map(String) : [],
    quotedId: pick(m, 'quoted_id', 'reply_to', 'quoted.id', 'context.stanza_id', 'context.quoted_id'),
    quotedSender: pick(m, 'quoted_sender', 'quoted.sender', 'context.participant'),
    media: mediaType || mediaPath
      ? {
          type: String(mediaType ?? 'file').toLowerCase(),
          path: mediaPath ? String(mediaPath) : null,
          mime: pick(m, 'mime_type', 'mimetype', 'media.mime'),
          bytes: Number(pick(m, 'media_size', 'file_length', 'media.bytes') ?? 0) || null,
          name: pick(m, 'filename', 'file_name', 'media.name'),
        }
      : null,
  };
}

/**
 * Why a group message wakes the chat by itself: 'mention', 'reply', 'name', 'command' or null.
 * `bot`: { jids: [], name: RegExp, outboxIds: id => boolean }. Pure, for tests.
 */
export function groupWake(msg, bot) {
  const jids = new Set((bot.jids ?? []).filter(Boolean));
  const digits = [...jids].map(j => /^(\d+)/.exec(j)?.[1]).filter(Boolean);
  if (msg.mentions.some(j => jids.has(j)) || digits.some(d => msg.text.includes('@' + d))) return 'mention';
  if ((msg.quotedSender && jids.has(msg.quotedSender)) || (msg.quotedId && bot.outboxIds?.(msg.quotedId))) return 'reply';
  if (bot.name && nameCall(msg.text, bot.name)) return 'name';
  if (/^\s*\/\w/.test(msg.text)) return 'command';
  return null;
}

// A person's next line within this time reads as the same message.
const FOLLOW_ON_MS = 5 * 60 * 1000;

// The bot's name followed by more of a person's name or role ("@Asistente de campo A") is someone else.
const NAME_GOES_ON = /^\s+(?:de|del|of)\b/i;
const CALL_OPENER = /^\s*(?:(?:hola|oye|hey|hi|buenas|buenos días|gracias|thanks)[\s,!]*)?@?$/i;

/**
 * Whether the text calls the bot by name: "@asistente …" anywhere, or the name as a vocative at the
 * start ("Asistente: …", "oye asistente, …") or after a comma at the end ("…, asistente?").
 * A role or ordinary word inside a sentence ("la asistente llega mañana") is not a call. Pure, for tests.
 */
export function nameCall(text, name) {
  const t = String(text ?? '');
  const re = new RegExp(name.source, name.flags.includes('g') ? name.flags : name.flags + 'g');
  for (const m of t.matchAll(re)) {
    const before = t.slice(0, m.index);
    const after = t.slice(m.index + m[0].length);
    if (NAME_GOES_ON.test(after)) continue;
    if (before.endsWith('@') || CALL_OPENER.test(before)) return true;
    if (/,\s*@?$/.test(before) && /^\s*[?!.¿¡]*\s*$/.test(after)) return true;
  }
  return false;
}

/** Whether a message reads as a protocol question. Pure, for tests. */
export function protocolQuestion(text) {
  const t = String(text ?? '').trim();
  // A question that tags someone is theirs to answer.
  if (/(^|\s)@\S/.test(t)) return false;
  return (t.endsWith('?') || QUESTION_START.test(t)) && PROTOCOL_WORDS.test(t);
}

/**
 * CAMs in the text that disagree with the database: [{ cam, said, database }]. `lookup(cam)`
 * gives { genus, species, sex } or null; `genera` the genus names the database knows. Pure, for tests.
 */
export function camMismatches(text, lookup, genera = new Set()) {
  const out = [];
  const cams = [...new Set((String(text).match(CAM) ?? []).map(c => c.toUpperCase()))];
  if (!cams.length) return out;
  const words = String(text).match(/\b[A-Z][a-z]{3,}\b/g) ?? [];
  const namedGenus = words.find(w => genera.has(w));
  const namedSex = SEX_WORDS.find(([re]) => re.test(text))?.[1];
  for (const cam of cams) {
    const row = lookup(cam);
    if (!row) {
      out.push({ cam, said: 'exists', database: 'no such CAM' });
      continue;
    }
    if (namedGenus && row.genus && namedGenus !== row.genus)
      out.push({ cam, said: namedGenus, database: [row.genus, row.species].filter(Boolean).join(' ') });
    const sex = String(row.sex ?? '').toLowerCase();
    if (namedSex && /^(male|female)$/.test(sex) && namedSex !== sex) out.push({ cam, said: namedSex, database: sex });
  }
  return out;
}

/** The start of the Ecuador day holding `t` (ms). */
export const ecuadorDay = t => Math.floor((t + ECUADOR_MS) / DAY) * DAY - ECUADOR_MS;
/** No unprompted replies between 21:00 and 06:30 Ecuador time. */
export function quietHours(t) {
  const minutes = Math.floor(((t + ECUADOR_MS) % DAY) / MINUTE);
  return minutes >= 21 * 60 || minutes < 6 * 60 + 30;
}

function initTables(db) {
  db.exec(`CREATE TABLE IF NOT EXISTS wa_people (
    phone TEXT PRIMARY KEY, lid TEXT, username TEXT, name TEXT NOT NULL DEFAULT '', role TEXT NOT NULL DEFAULT 'member',
    language TEXT, enabled INTEGER NOT NULL DEFAULT 1, updated_at TEXT NOT NULL
  );
  CREATE INDEX IF NOT EXISTS wa_people_lid ON wa_people(lid);
  CREATE TABLE IF NOT EXISTS wa_groups (
    jid TEXT PRIMARY KEY, name TEXT NOT NULL DEFAULT '', enabled INTEGER NOT NULL DEFAULT 0,
    mode TEXT NOT NULL DEFAULT 'mention-only', daily_cap INTEGER NOT NULL DEFAULT 3, updated_at TEXT NOT NULL
  );
  CREATE TABLE IF NOT EXISTS wa_inbox (
    n INTEGER PRIMARY KEY AUTOINCREMENT, msg_id TEXT NOT NULL UNIQUE, chat TEXT NOT NULL, chat_name TEXT,
    is_group INTEGER NOT NULL, sender TEXT, sender_jid TEXT, sender_name TEXT, at INTEGER NOT NULL, received_at INTEGER NOT NULL,
    text TEXT, media_json TEXT, quoted_id TEXT, status TEXT NOT NULL, reason TEXT, wake TEXT,
    counted INTEGER NOT NULL DEFAULT 0, ready_at INTEGER, turn TEXT
  );
  CREATE INDEX IF NOT EXISTS wa_inbox_status ON wa_inbox(status, ready_at);
  CREATE INDEX IF NOT EXISTS wa_inbox_chat ON wa_inbox(chat, at);
  CREATE INDEX IF NOT EXISTS wa_inbox_sender ON wa_inbox(sender, counted, received_at);
  CREATE TABLE IF NOT EXISTS wa_outbox (
    n INTEGER PRIMARY KEY AUTOINCREMENT, at INTEGER NOT NULL, chat TEXT NOT NULL, reply_to TEXT, reply_n INTEGER,
    text TEXT NOT NULL, file TEXT, turn TEXT, tool_use TEXT, by TEXT NOT NULL, via TEXT NOT NULL, state TEXT NOT NULL, error TEXT,
    wa_id TEXT
  );
  CREATE INDEX IF NOT EXISTS wa_outbox_at ON wa_outbox(at);
  CREATE TABLE IF NOT EXISTS wa_turns (
    id TEXT PRIMARY KEY, thread TEXT, sent_at INTEGER NOT NULL, started INTEGER NOT NULL DEFAULT 0, done_at INTEGER, messages INTEGER NOT NULL
  );
  CREATE TABLE IF NOT EXISTS wa_state (key TEXT PRIMARY KEY, value TEXT)`);
}

/** The tool the WhatsApp chat answers with (only the service user's token lists it). */
export const REPLY_TOOL = {
  name: 'whatsapp_reply',
  description: [
    'Answers a WhatsApp message from the relay\'s envelope ("#m12 DM …"). `replyTo`: its #id; the answer goes to that message\'s chat, quoting it.',
    '`text`: what to say, in WhatsApp formatting. `file`: a picture or document on this computer to send with it (e.g. a proposal\'s screenshot), `text` as its caption.',
    'A chat with no message in this turn answers with a warning instead; `confirm: true` sends anyway.',
    'Messages that need no answer get none: call nothing for them.',
  ].join('\n'),
  inputSchema: {
    type: 'object',
    properties: {
      replyTo: { type: 'string', description: 'The message\'s #id from the envelope, e.g. #m12' },
      text: { type: 'string' },
      file: { type: 'string' },
      confirm: { type: 'boolean' },
    },
    required: ['replyTo', 'text'],
  },
};
/** Tools the WhatsApp chat does not get: changes are applied in the app by a person. */
export const WITHHELD_TOOLS = new Set(['apply_proposal', 'undo_edits']);

/**
 * The relay. `db`: the app's database (node:sqlite). `secret`: the webhook's HMAC secret.
 * `wake(text)` → { sent, reason } sends one message to the WhatsApp chat (or makes it);
 * `busy()` whether that chat is answering. `lookupCam(cam)` / `genera()` for the CAM check.
 * `send({ chat, replyTo, text, file })` the future wacli sender (used only when `sender` is 'wacli').
 */
export function createWhatsAppRelay({
  db,
  secret,
  serviceUser = 'whatsapp',
  botJids = [],
  botName = /\b(ithobot|asistente|assistant)\b/i,
  wake = async () => ({ sent: false, reason: 'no_t3' }),
  busy = () => false,
  lookupCam = () => null,
  genera = () => new Set(),
  sender = 'outbox',
  send = null,
  outboxFile = null,
  logFile = null,
  lab = false,
  options = {},
  now = Date.now,
  log = console,
}) {
  const o = { ...DEFAULTS, ...options };
  initTables(db);
  const iso = t => new Date(t).toISOString();
  const state = {
    get: key => db.prepare('SELECT value FROM wa_state WHERE key = ?').get(key)?.value ?? null,
    set: (key, value) =>
      value == null
        ? db.prepare('DELETE FROM wa_state WHERE key = ?').run(key)
        : db.prepare('INSERT INTO wa_state (key, value) VALUES (?, ?) ON CONFLICT(key) DO UPDATE SET value = excluded.value').run(key, String(value)),
  };
  const stats = { received: 0, duplicates: 0, badSignature: 0, dropped: 0, woken: 0, turns: 0, replies: 0 };

  function record(entry) {
    if (!logFile) return;
    try {
      appendFileSync(logFile, JSON.stringify({ at: iso(now()), ...entry }) + '\n', { mode: 0o600 });
    } catch (e) {
      log.error?.('WhatsApp log:', e.message);
    }
  }

  // --- the whitelist
  function person(jid) {
    if (!jid) return null;
    const phone = phoneOf(jid);
    const row = phone
      ? db.prepare('SELECT * FROM wa_people WHERE phone = ?').get(phone)
      : db.prepare('SELECT * FROM wa_people WHERE lid = ?').get(String(jid).replace(/:\d+(?=@)/, ''));
    return row ?? null;
  }
  const group = jid => db.prepare('SELECT * FROM wa_groups WHERE jid = ?').get(jid) ?? null;
  function setPerson({ phone, name = '', role = 'member', lid = null, username = null, language = null, enabled = true }) {
    const p = phoneOf(phone);
    if (!p) throw fail('WA_PHONE', `Not a phone number: ${clip(phone, 30)}`, 400);
    db.prepare(
      `INSERT INTO wa_people (phone, lid, username, name, role, language, enabled, updated_at) VALUES (?,?,?,?,?,?,?,?)
       ON CONFLICT(phone) DO UPDATE SET lid = excluded.lid, username = excluded.username, name = excluded.name, role = excluded.role,
         language = excluded.language, enabled = excluded.enabled, updated_at = excluded.updated_at`,
    ).run(p, lid, username, String(name), role === 'admin' ? 'admin' : 'member', language, enabled ? 1 : 0, iso(now()));
    return person(p);
  }
  function setGroup({ jid, name = '', enabled = false, mode = 'mention-only', dailyCap = o.groupUnpromptedDay }) {
    if (!String(jid ?? '').endsWith('@g.us')) throw fail('WA_GROUP', 'A group JID ends in @g.us', 400);
    db.prepare(
      `INSERT INTO wa_groups (jid, name, enabled, mode, daily_cap, updated_at) VALUES (?,?,?,?,?,?)
       ON CONFLICT(jid) DO UPDATE SET name = excluded.name, enabled = excluded.enabled, mode = excluded.mode,
         daily_cap = excluded.daily_cap, updated_at = excluded.updated_at`,
    ).run(jid, String(name), enabled ? 1 : 0, mode === 'helpful' ? 'helpful' : 'mention-only', Number(dailyCap) || 0, iso(now()));
    return group(jid);
  }

  // --- limits
  function overNumber(phone, t) {
    const hour = db.prepare('SELECT count(*) n FROM wa_inbox WHERE sender = ? AND counted = 1 AND received_at > ?').get(phone, t - HOUR).n;
    if (hour >= o.perNumberHour) return 'hour';
    const day = db.prepare('SELECT count(*) n FROM wa_inbox WHERE sender = ? AND counted = 1 AND received_at > ?').get(phone, t - DAY).n;
    return day >= o.perNumberDay ? 'day' : null;
  }
  function unpromptedToday(chat, t) {
    return db
      .prepare("SELECT count(*) n FROM wa_inbox WHERE chat = ? AND wake LIKE 'candidate:%' AND status IN ('delivered','answered','unanswered') AND received_at >= ?")
      .get(chat, ecuadorDay(t)).n;
  }
  const wakesToday = t => db.prepare('SELECT count(*) n FROM wa_turns WHERE sent_at >= ?').get(ecuadorDay(t)).n;
  const sendsLastHour = t => db.prepare("SELECT count(*) n FROM wa_outbox WHERE at > ? AND state != 'refused'").get(t - HOUR).n;

  // --- the queue
  function schedule(chat, t) {
    // Every waiting message of the chat goes together: 20 s after the last, 60 s after the first at most.
    const first = db.prepare("SELECT min(received_at) f FROM wa_inbox WHERE chat = ? AND status = 'waiting' AND wake NOT LIKE 'candidate:question'").get(chat).f ?? t;
    const ready = Math.min(t + o.debounceMs, first + o.maxWaitMs);
    db.prepare("UPDATE wa_inbox SET ready_at = ? WHERE chat = ? AND status = 'waiting' AND wake NOT LIKE 'candidate:question'").run(ready, chat);
  }
  function setStatus(n, status, reason = null) {
    db.prepare('UPDATE wa_inbox SET status = ?, reason = coalesce(?, reason) WHERE n = ?').run(status, reason, n);
  }

  /** Relay-made answers (limits, /add): through the same outbox as the chat's. */
  function relaySay(chat, replyTo, text) {
    return deliver({ chat, replyTo, replyN: null, text, file: null, turn: null, toolUse: null, by: 'relay' });
  }

  function adminCommand(msg, who) {
    const m = /^\s*\/(add|remove)\s+(\+?\d[\d\s-]{5,20})\s*(.*)$/i.exec(msg.text);
    if (!m) return false;
    const phone = phoneOf(m[2].replace(/[\s-]/g, ''));
    if (!phone) return relaySay(msg.chat, msg.id, 'Número no válido.'), true;
    if (m[1].toLowerCase() === 'add') {
      setPerson({ phone, name: m[3].trim() || phone, role: 'member', enabled: true });
      relaySay(msg.chat, msg.id, `Añadido: ${m[3].trim() || phone} (${phone}).`);
    } else {
      db.prepare('UPDATE wa_people SET enabled = 0, updated_at = ? WHERE phone = ?').run(iso(now()), phone);
      relaySay(msg.chat, msg.id, `Quitado: ${phone}.`);
    }
    record({ dir: 'admin', by: who.phone, command: m[1].toLowerCase(), phone });
    return true;
  }

  /** A webhook call: { status, body }. `raw`: the body's bytes as they came. */
  function receive(headers, raw) {
    if (!secret) return { status: 404, body: { error: 'WhatsApp relay is not configured' } };
    if (!verifySignature(secret, raw, headers['x-wacli-signature'])) {
      stats.badSignature++;
      record({ dir: 'in', decision: 'bad_signature' });
      return { status: 403, body: { error: 'Invalid signature' } };
    }
    let body;
    try {
      body = JSON.parse(Buffer.from(raw).toString('utf8'));
    } catch {
      return { status: 400, body: { error: 'Invalid JSON' } };
    }
    const events = Array.isArray(body) ? body : Array.isArray(body?.events) ? body.events : [body];
    const results = events.slice(0, 50).map(e => handle(normalizeEvent(e, now())));
    return { status: 202, body: { results } };
  }

  function handle(msg) {
    if (!msg) return { decision: 'ignored', reason: 'not a message' };
    const t = now();
    stats.received++;
    if (msg.fromMe) return { id: msg.id, decision: 'ignored', reason: 'own message' };
    const who = person(msg.sender);
    const grp = msg.isGroup ? group(msg.chat) : null;
    const listed = Boolean(who?.enabled);
    const chatOk = msg.isGroup ? Boolean(grp?.enabled) : listed;
    // Text only from people on the list (and their groups); a stranger's is never kept.
    const keepText = chatOk;
    const inserted = db
      .prepare(
        `INSERT OR IGNORE INTO wa_inbox (msg_id, chat, chat_name, is_group, sender, sender_jid, sender_name, at, received_at, text, media_json, quoted_id, status)
         VALUES (?,?,?,?,?,?,?,?,?,?,?,?, 'new')`,
      )
      .run(
        msg.id,
        msg.chat,
        keepText ? (msg.chatName ?? grp?.name ?? null) : null,
        msg.isGroup ? 1 : 0,
        who?.phone ?? phoneOf(msg.sender) ?? msg.sender,
        msg.sender,
        keepText ? (who?.name || msg.senderName) : null,
        msg.at,
        t,
        keepText ? clip(msg.text, 4000) : null,
        keepText && msg.media ? JSON.stringify(msg.media) : null,
        msg.quotedId,
      );
    if (!inserted.changes) {
      stats.duplicates++;
      return { id: msg.id, decision: 'duplicate' };
    }
    const n = Number(inserted.lastInsertRowid);
    const out = (status, reason, extra = {}) => {
      setStatus(n, status, reason);
      if (status === 'dropped') stats.dropped++;
      record({ dir: 'in', n, id: msg.id, chat: msg.chat, sender: who?.phone ?? phoneOf(msg.sender) ?? msg.sender, decision: status, reason, ...(keepText ? { text: clip(msg.text, 500) } : {}), ...extra });
      log.info?.(`[wa] in #m${n} ${msg.isGroup ? 'group' : 'DM'} → ${status}${reason ? ` (${reason})` : ''}`);
      return { id: msg.id, n, decision: status, reason };
    };

    if (msg.isGroup && !grp) {
      setGroup({ jid: msg.chat, name: msg.chatName ?? '', enabled: false });
      return out('dropped', 'group pending');
    }
    if (!chatOk) return out('dropped', msg.isGroup ? 'group not enabled' : 'not on list');
    // A teammate wrote in the group: the questions waiting for an answer there have one (or need none).
    if (msg.isGroup)
      db.prepare(
        "UPDATE wa_inbox SET status = 'ignored', reason = 'a teammate wrote after it' WHERE chat = ? AND status = 'waiting' AND wake = 'candidate:question' AND sender != ?",
      ).run(msg.chat, who?.phone ?? msg.sender ?? '');
    if (msg.isGroup && !listed) return out('context', 'not on list');

    let wakeReason = null;
    if (msg.isGroup) {
      // A quote of one of the bot's answers: its WhatsApp id once sent (wacli), or out:<n> in the outbox (the simulator).
      const outboxIds = id => Boolean(db.prepare("SELECT 1 FROM wa_outbox WHERE ('out:' || n) = ? OR wa_id = ?").get(id, id));
      wakeReason = groupWake(msg, { jids: botJids, name: botName, outboxIds });
      if (!wakeReason && grp.mode === 'helpful') {
        const mismatch = camMismatches(msg.text, lookupCam, genera());
        if (mismatch.length) wakeReason = 'candidate:cam';
        else if (!msg.mentions.length && protocolQuestion(msg.text)) wakeReason = 'candidate:question';
      }
      if (!wakeReason) return out('context', 'no wake rule');
      if (wakeReason.startsWith('candidate:')) {
        if (o.quietHours && quietHours(t)) return out('context', 'quiet hours');
        if (unpromptedToday(msg.chat, t) >= (grp.daily_cap ?? o.groupUnpromptedDay)) return out('context', 'group daily cap');
      }
    } else {
      wakeReason = 'dm';
      if (who.role === 'admin' && adminCommand(msg, who)) return out('handled', 'admin command');
    }

    const over = overNumber(who.phone, t);
    if (over) {
      const key = `notice:${who.phone}:${over}:${over === 'hour' ? Math.floor(t / HOUR) : ecuadorDay(t)}`;
      if (!state.get(key)) {
        state.set(key, iso(t));
        relaySay(msg.chat, msg.id, 'Demasiados mensajes por ahora; vuelvo a leer más tarde. / Too many messages for now; I will read again later.');
      }
      return out('limited', `per-number ${over} limit`);
    }
    db.prepare('UPDATE wa_inbox SET counted = 1, wake = ? WHERE n = ?').run(wakeReason, n);
    if (wakeReason === 'candidate:question') {
      db.prepare('UPDATE wa_inbox SET ready_at = ? WHERE n = ?').run(t + o.questionWaitMs, n);
      return out('waiting', wakeReason);
    }
    setStatus(n, 'waiting', wakeReason);
    schedule(msg.chat, t);
    return out('waiting', wakeReason);
  }

  // --- the envelope
  const label = r => `#m${r.n}`;
  const hhmm = t => new Date(t + ECUADOR_MS).toISOString().slice(11, 16);
  function mediaLine(media) {
    if (!media) return '';
    const m = typeof media === 'string' ? JSON.parse(media) : media;
    if (/image|photo|sticker/.test(m.type)) return m.path ? ` [photo ${m.path}]` : ' [photo, not downloaded]';
    if (/video/.test(m.type)) return m.bytes && m.bytes > o.maxVideoBytes ? ' [video over 20 MB, not opened]' : m.path ? ` [video ${m.path}]` : ' [video]';
    if (/audio|ptt|voice/.test(m.type)) return ' [voice note: ask for it in text]';
    return ` [document ${m.name ?? ''} ${m.path ? m.path : ''}, open only if asked]`.replace(/\s+,/, ',');
  }
  function line(r, extra = '') {
    const where = r.is_group ? `grp "${r.chat_name ?? r.chat}"` : 'DM';
    const tag = r.wake && r.wake !== 'dm' ? ` (${r.wake.replace('candidate:', 'unprompted ')})` : '';
    return `${label(r)} ${where} ${r.sender_name ?? ''} (${r.sender}) ${hhmm(r.at)}${tag}${extra}: ${String(r.text ?? '').replace(/\s*\n\s*/g, ' / ')}${mediaLine(r.media_json)}`;
  }
  /** " (goes on from #m33)" when the chat's previous message, a few minutes before, is the same person's. */
  function followsOn(r) {
    const prev = db.prepare('SELECT n, sender, at FROM wa_inbox WHERE chat = ? AND n < ? ORDER BY n DESC LIMIT 1').get(r.chat, r.n);
    return prev && prev.sender === r.sender && r.at - prev.at <= FOLLOW_ON_MS ? ` (goes on from ${label(prev)})` : '';
  }
  /** One T3 message for the messages ready now: their group context first, then them. */
  function envelope(rows) {
    const lines = [`[WA] ${rows.length} new`];
    const contextShown = new Set();
    for (const r of rows) {
      if (r.is_group) {
        const ctx = db
          .prepare(
            `SELECT * FROM wa_inbox WHERE chat = ? AND n < ? AND at > ? AND status IN ('context') ORDER BY n DESC LIMIT ?`,
          )
          .all(r.chat, r.n, r.at - o.contextMs, o.contextLines)
          .reverse()
          .filter(c => !contextShown.has(c.n));
        for (const c of ctx) {
          contextShown.add(c.n);
          lines.push(`  ctx ${line(c, c.reason === 'not on list' ? ' [not on list]' : '')}`);
        }
      }
      lines.push(line(r, followsOn(r)));
    }
    lines.push('Reply with whatsapp_reply (replyTo #id); the whatsapp skill has how. Say nothing to messages that need no answer.');
    return lines.join('\n');
  }

  // --- one turn at a time
  const openTurn = () => db.prepare('SELECT * FROM wa_turns WHERE done_at IS NULL ORDER BY sent_at DESC LIMIT 1').get() ?? null;
  const lastTurn = () => db.prepare('SELECT * FROM wa_turns ORDER BY sent_at DESC LIMIT 1').get() ?? null;
  let pumping = null;
  async function pumpOnce() {
    const t = now();
    const turn = openTurn();
    if (turn) {
      const answering = busy();
      if (answering && !turn.started) db.prepare('UPDATE wa_turns SET started = 1 WHERE id = ?').run(turn.id);
      if (answering || (!turn.started && t - turn.sent_at < o.turnGraceMs)) return { waiting: 'turn' };
      db.prepare('UPDATE wa_turns SET done_at = ? WHERE id = ?').run(t, turn.id);
      const quiet = db.prepare("UPDATE wa_inbox SET status = 'unanswered', reason = 'no reply this turn' WHERE turn = ? AND status = 'delivered'").run(turn.id);
      record({ dir: 'turn', turn: turn.id, done: true, unanswered: Number(quiet.changes) });
    }
    const rows = db.prepare("SELECT * FROM wa_inbox WHERE status = 'waiting' AND ready_at <= ? ORDER BY n").all(t);
    if (!rows.length) return { waiting: null };
    if (busy()) return { waiting: 'busy' };
    // Over the day's wakes, only admins' DMs go on; the rest waits for tomorrow.
    let ready = rows;
    if (wakesToday(t) >= o.wakesDay) {
      ready = rows.filter(r => !r.is_group && person(r.sender)?.role === 'admin');
      if (!ready.length) return { waiting: 'wake cap' };
    }
    const id = randomUUID();
    const text = envelope(ready);
    const sent = await wake(text);
    if (!sent.sent) {
      record({ dir: 'turn', turn: id, sent: false, reason: sent.reason });
      return { waiting: sent.reason ?? 'not sent' };
    }
    db.prepare('INSERT INTO wa_turns (id, thread, sent_at, messages) VALUES (?,?,?,?)').run(id, sent.threadId ?? null, t, ready.length);
    const mark = db.prepare("UPDATE wa_inbox SET status = 'delivered', turn = ? WHERE n = ?");
    for (const r of ready) mark.run(id, r.n);
    stats.turns++;
    stats.woken += ready.length;
    record({ dir: 'turn', turn: id, sent: true, messages: ready.map(label), envelope: text });
    log.info?.(`[wa] turn ${id.slice(0, 8)}: ${ready.length} message(s)`);
    return { turn: id, messages: ready.length };
  }
  function pump() {
    if (!pumping) pumping = pumpOnce().finally(() => (pumping = null));
    return pumping;
  }

  // --- out
  function deliver({ chat, replyTo, replyN, replySender = null, text, file, turn, toolUse, by }) {
    const t = now();
    const via = sender === 'wacli' && send ? 'wacli' : 'outbox';
    const row = db
      .prepare('INSERT INTO wa_outbox (at, chat, reply_to, reply_n, text, file, turn, tool_use, by, via, state) VALUES (?,?,?,?,?,?,?,?,?,?,?)')
      .run(t, chat, replyTo, replyN, text, file, turn, toolUse, by, via, via === 'wacli' ? 'sending' : 'outbox');
    const n = Number(row.lastInsertRowid);
    const entry = { n, at: iso(t), chat, replyTo, replyToN: replyN, text, file, turn, by, via };
    if (outboxFile)
      try {
        appendFileSync(outboxFile, JSON.stringify(entry) + '\n', { mode: 0o600 });
      } catch (e) {
        log.error?.('WhatsApp outbox file:', e.message);
      }
    record({ dir: 'out', ...entry });
    log.info?.(`[wa] out ${by} → ${chat.endsWith('@g.us') ? 'group' : 'DM'} (${via})`);
    if (via === 'wacli')
      Promise.resolve(send({ chat, replyTo, replySender, text, file }))
        .then(sent => db.prepare("UPDATE wa_outbox SET state = 'sent', wa_id = ? WHERE n = ?").run(sent?.id ?? null, n))
        .catch(e => db.prepare("UPDATE wa_outbox SET state = 'failed', error = ? WHERE n = ?").run(clip(e.message, 300), n));
    return n;
  }

  /** whatsapp_reply, called by the WhatsApp chat. */
  function reply(args, { toolUse = null } = {}) {
    const t = now();
    const ref = String(args?.replyTo ?? '').trim();
    const nMatch = /^#?m(\d+)$/i.exec(ref);
    const msg = nMatch
      ? db.prepare('SELECT * FROM wa_inbox WHERE n = ?').get(Number(nMatch[1]))
      : db.prepare('SELECT * FROM wa_inbox WHERE msg_id = ?').get(ref);
    if (!msg) return { error: `No message ${clip(ref, 40)}. Give the #id from the envelope.` };
    const text = String(args?.text ?? '').trim();
    if (!text) return { error: 'Give the text to send.' };
    if (text.length > 1500) return { error: `The text has ${text.length} characters; WhatsApp answers here are 1–4 short lines. Send a link or a screenshot for more.` };
    const ok = msg.is_group ? group(msg.chat)?.enabled : person(msg.sender)?.enabled;
    if (!ok) return { error: 'That chat is not on the WhatsApp list (anymore). Nothing was sent.' };
    const file = args?.file ? String(args.file) : null;
    if (file && !existsSync(file)) return { error: `No file ${clip(file, 200)} on this computer. Nothing was sent.` };
    if (file && statSync(file).size > 16 * 1024 * 1024) return { error: 'The file is over 16 MB. Nothing was sent.' };
    if (sendsLastHour(t) >= o.sendsHour) return { error: `${o.sendsHour} answers were sent in the last hour, the limit. Nothing was sent; try later.` };
    const turn = openTurn() ?? lastTurn();
    const inTurn = turn && db.prepare('SELECT 1 FROM wa_inbox WHERE turn = ? AND chat = ?').get(turn.id, msg.chat);
    if (!inTurn && !args?.confirm)
      return {
        sent: false,
        warning: `${label(msg)}'s chat (${msg.is_group ? `group "${msg.chat_name ?? msg.chat}"` : `DM with ${msg.sender_name ?? msg.sender}`}) had no message in this turn. Send anyway with confirm: true.`,
      };
    const n = deliver({ chat: msg.chat, replyTo: msg.msg_id, replyN: msg.n, replySender: msg.is_group ? msg.sender_jid : null, text, file, turn: turn?.id ?? null, toolUse, by: 'assistant' });
    db.prepare("UPDATE wa_inbox SET status = 'answered' WHERE n = ? AND status IN ('delivered','unanswered','waiting')").run(msg.n);
    stats.replies++;
    return { sent: true, outbox: n, via: sender === 'wacli' && send ? 'wacli' : 'outbox (test: nothing goes to WhatsApp)' };
  }

  // --- MCP: the WhatsApp chat's token gets whatsapp_reply and loses apply_proposal
  function tokenUser(headers) {
    const token = /^Bearer\s+(\S+)$/.exec(String(headers?.authorization ?? ''))?.[1];
    if (!token) return null;
    const hash = createHash('sha256').update(token).digest('hex');
    return (
      db
        .prepare('SELECT u.username FROM ai_tokens t JOIN users u ON u.id = t.user_id WHERE t.token_hash = ? AND t.revoked_at IS NULL AND u.active = 1')
        .get(hash)?.username ?? null
    );
  }
  /** Wraps the app's MCP answer (`next`) for the WhatsApp chat. */
  async function mcp(headers, body, next) {
    const method = String(body?.method ?? '');
    if (method !== 'tools/list' && method !== 'tools/call') return next();
    const mine = tokenUser(headers) === serviceUser;
    const name = String(body?.params?.name ?? '');
    const id = body?.id ?? null;
    const result = value => ({ status: 200, body: { jsonrpc: '2.0', id, result: value } });
    if (method === 'tools/list') {
      const out = await next();
      if (!mine || !out?.body?.result?.tools) return out;
      const tools = out.body.result.tools.filter(t => !WITHHELD_TOOLS.has(t.name));
      return { ...out, body: { ...out.body, result: { ...out.body.result, tools: [...tools, { ...REPLY_TOOL, _meta: { 'anthropic/alwaysLoad': true } }] } } };
    }
    if (name === REPLY_TOOL.name) {
      if (!mine) return result({ content: [{ type: 'text', text: JSON.stringify({ error: 'Only the WhatsApp chat sends WhatsApp answers.' }) }], isError: true });
      const out = reply(body.params?.arguments ?? {}, { toolUse: clip(body.params?._meta?.['claudecode/toolUseId'], 100) || null });
      return result({ content: [{ type: 'text', text: JSON.stringify(out) }], isError: Boolean(out.error) });
    }
    if (mine && WITHHELD_TOOLS.has(name))
      return result({
        content: [{ type: 'text', text: JSON.stringify({ error: `${name} is not available from WhatsApp: a person applies changes in the app (the proposal's link).` }) }],
        isError: true,
      });
    return next();
  }

  /** A message's state for the simulator: its decision, its replies, whether its turn is over. */
  function message(msgId) {
    const r = db.prepare('SELECT n, msg_id, chat, status, reason, wake, turn FROM wa_inbox WHERE msg_id = ?').get(msgId);
    if (!r) return null;
    const turn = r.turn ? db.prepare('SELECT * FROM wa_turns WHERE id = ?').get(r.turn) : null;
    const replies = db.prepare('SELECT n, at, chat, reply_n, text, file, by, via FROM wa_outbox WHERE reply_n = ? OR (by = ? AND reply_to = ?) ORDER BY n').all(r.n, 'relay', r.msg_id);
    // Answers in the same turn to the same chat (the chat may quote another message of the burst).
    const sameTurn = turn
      ? db.prepare('SELECT n, at, chat, reply_n, text, file, by, via FROM wa_outbox WHERE turn = ? AND chat = ? ORDER BY n').all(turn.id, r.chat)
      : [];
    const all = [...new Map([...replies, ...sameTurn].map(x => [x.n, x])).values()].map(x => ({ ...x, at: iso(x.at) }));
    return { ...r, id: `#m${r.n}`, turnDone: turn ? Boolean(turn.done_at) : null, replies: all };
  }

  let timer = null;
  return {
    receive,
    pump,
    reply,
    mcp,
    message,
    setPerson,
    setGroup,
    people: () => db.prepare('SELECT * FROM wa_people ORDER BY phone').all(),
    groups: () => db.prepare('SELECT * FROM wa_groups ORDER BY jid').all(),
    thread: () => state.get('thread'),
    setThread: id => state.set('thread', id),
    status: () => ({ ...stats, sender, lab, thread: state.get('thread'), openTurn: openTurn()?.id ?? null, waiting: db.prepare("SELECT count(*) n FROM wa_inbox WHERE status = 'waiting'").get().n }),
    start(intervalMs = 5000) {
      if (timer) return;
      timer = setInterval(() => pump().catch(e => log.error?.('WhatsApp relay:', e.message)), intervalMs);
      timer.unref?.();
    },
    close() {
      clearInterval(timer);
      timer = null;
    },
  };
}

/**
 * The glue to T3 (server/t3tell.mjs's way): the WhatsApp chat is a chat of the service user's own
 * T3 project (scripts/t3-provision.mjs). `relay.thread()` keeps its id; without one (or after a
 * reset) the first envelope makes it, on Sonnet 5.5.
 */
export function t3Glue({ t3, chats, provision, serviceUser, getThread, setThread, tell, log = console, wait = 500 }) {
  const send = t3 ? t3Commands({ local: t3.local, tokenFile: t3.tokenFile }) : null;
  async function project() {
    let own = chats?.projectOf(serviceUser) ?? null;
    if (own || !provision) return own;
    await provision(serviceUser).catch(e => log.error?.(`T3 project for ${serviceUser}:`, e.message));
    for (let i = 0; i < 20 && !own; i++) {
      own = chats.projectOf(serviceUser, { fresh: true });
      if (!own) await new Promise(r => setTimeout(r, wait));
    }
    return own;
  }
  return {
    busy() {
      const id = getThread();
      return Boolean(id && chats?.session(id)?.busy);
    },
    async wake(text) {
      if (!send || !chats?.available) return { sent: false, reason: 'no_t3' };
      const id = getThread();
      if (id && chats.session(id)) {
        const out = await tell({ t3, chats, threadId: id, username: serviceUser, text });
        return { ...out, threadId: id };
      }
      const own = await project();
      if (!own) return { sent: false, reason: 'no_project' };
      const threadId = randomUUID();
      // A new chat starts by reading its guide (the skill), once.
      text = `Read the whatsapp skill first, then answer.\n${text}`;
      try {
        await send(
          { type: 'thread.create', threadId, projectId: own.id, title: 'WhatsApp', modelSelection: MODEL, runtimeMode: 'full-access', interactionMode: 'default', branch: null, worktreePath: null },
          { type: 'thread.turn.start', threadId, message: { messageId: randomUUID(), role: 'user', text, attachments: [] }, modelSelection: MODEL, runtimeMode: 'full-access', interactionMode: 'default' },
        );
      } catch (e) {
        log.error?.('WhatsApp chat:', e.message);
        return { sent: false, reason: 'failed' };
      }
      setThread(threadId);
      return { sent: true, threadId };
    },
  };
}

/** The service user the WhatsApp chat's tools run as (made once; it never signs in: no password works). */
export function ensureServiceUser(db, username, displayName = 'WhatsApp') {
  const found = db.prepare('SELECT * FROM users WHERE username = ?').get(username);
  if (found) return found;
  db.prepare('INSERT INTO users (id, username, display_name, role, salt, password_hash, active, created_at) VALUES (?,?,?,?,?,?,1,?)').run(
    randomUUID(),
    username,
    displayName,
    'editor',
    randomBytes(16).toString('hex'),
    randomBytes(32).toString('hex'),
    new Date().toISOString(),
  );
  return db.prepare('SELECT * FROM users WHERE username = ?').get(username);
}

/** The future sender: `wacli send text|file` through the running sync's socket (off: WA_SENDER=outbox). */
export function wacliSender({ bin = 'wacli', account = null } = {}) {
  const run = args =>
    new Promise((resolve, reject) =>
      execFile(bin, [...(account ? ['--account', account] : []), ...args], { timeout: 60_000 }, (e, out, err) => (e ? reject(new Error(String(err || e.message).trim().slice(0, 300))) : resolve(out))),
    );
  const quote = (replyTo, replySender) => [...(replyTo ? ['--reply-to', replyTo] : []), ...(replyTo && replySender ? ['--reply-to-sender', replySender] : [])];
  return async ({ chat, replyTo, replySender, text, file }) => {
    const out = await run([
      '--json',
      'send',
      ...(file ? ['file', '--to', chat, '--file', file, '--caption', text] : ['text', '--to', chat, '--message', text]),
      ...quote(replyTo, replySender),
    ]);
    try {
      return { id: JSON.parse(out)?.id ?? JSON.parse(out)?.msg_id ?? null };
    } catch {
      return { id: null };
    }
  };
}
