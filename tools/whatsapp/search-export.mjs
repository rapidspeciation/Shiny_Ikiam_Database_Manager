// The team groups' WhatsApp history for the assistant's whatsapp_search (server/wa-search.mjs), built on this PC
// from wacli's store (read-only) and copied to the app's server, where every team chat can search it.
//
// Only the chats named in the config's `search.chats` (default: «Ithomiini 🦋 (underground)» and «Ithomiini Ikiam»)
// go in. The file has no phone number and no JID: each sender is their app account (the private people map on the
// server, `chats.group.people.map`, read over ssh) or their WhatsApp name, and a mention «@593…» in a text becomes
// «@Name». Phone-like numbers in the text itself are masked. Reactions and deleted messages are left out; a voice
// note carries its transcript when the bridge has one.
//
//   node tools/whatsapp/bridge.mjs search-export [--push]   build the file (and copy it to the server)
// systemd: ithomiini-wa-search.timer every 10 minutes (written by `bridge.mjs install`).
import { DatabaseSync } from 'node:sqlite';
import { execFile } from 'node:child_process';
import { createHash } from 'node:crypto';
import { existsSync, mkdirSync, readFileSync, renameSync, rmSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { bareJid, ownAnswer } from './bridge-lib.mjs';

export const SEARCH_DEFAULTS = {
  chats: { underground: 'group', ikiam: 'ikiam' },
  /** The file on the app's server (beside its database, where server/wa-search.mjs looks by default). */
  target: { route: 'claudeclaw', path: '/home/ubuntu/ithomiini/shared/wa-search/wa-search.sqlite' },
};
export const SCHEMA_VERSION = 1;

const clip = (s, n) => (s == null ? '' : String(s).slice(0, n));
/** A name that is only a phone number (WhatsApp shows the number when there is no name). */
const PHONE_NAME = /^[\s+()\d.-]{7,}$/;
/**
 * A phone number written in a text, outside links: +593 99 123 4567, 0991234567, (099) 123-4567, Cel.0991234567.
 * Any other bare number of 9 to 15 digits is masked too (an iNaturalist id written without its link, say): the
 * price of never letting a number through.
 */
const PHONE_IN_TEXT = /(?<![\w/@=#])(?<!\d[.,])\+?\(?\d(?:[ .()-]{0,2}\d){8,14}(?![\w/])/g;
/** The same in a file or a person's name, where a number may touch letters or «_»: Carta_0991234567.pdf. */
const PHONE_ANYWHERE = /\+?\(?\d(?:[ .()_-]{0,2}\d){8,14}/g;
/** A number inside a link or a contact card: wa.me/593…, api.whatsapp.com/send?phone=593…, waid=593…. */
const PHONE_IN_LINK = /(wa\.me\/|[?&]phone=|\bwaid=)\+?\d{6,}/gi;
const URL = /\bhttps?:\/\/[^\s<>"')\]]+/gi;
/** Kept as they are, like links: decimals (GPS points, -0.9512345) and date-times (2026-10-01 10:00). */
const KEEP = new RegExp(`${URL.source}|(?<![\\d.,])-?\\d{1,3}[.,]\\d{4,}(?![.,]?\\d)|\\b\\d{4}-\\d{2}-\\d{2}(?:[ T]\\d{1,2}:\\d{2}(?::\\d{2})?)?`, 'gi');
/** wacli's display text for a message with no text of its own («Sent image», «Reacted 👍 to …»). */
const PLACEHOLDER = /^(Sent (an? )?\w+|Reacted .*|Message deleted|This message was deleted)$/is;

/** The links in a text, in order, without repeats. */
export const linksOf = text => [...new Set((String(text ?? '').match(URL) ?? []).map(u => u.replace(/[.,;:!?]+$/, '')))];

/** A text with its phone numbers masked: in WhatsApp links and contact cards, and anywhere outside other links. */
export function maskPhones(text) {
  const s = String(text ?? '').replace(PHONE_IN_LINK, '$1numero-oculto');
  const links = [];
  const kept = s.replace(KEEP, u => `\u0000${links.push(u) - 1}\u0000`);
  return kept.replace(PHONE_IN_TEXT, '[número]').replace(/\u0000(\d+)\u0000/g, (_, i) => links[Number(i)]);
}
/** A file or person's name with any phone-like number masked; null when nothing else is left. */
export function maskName(name) {
  const masked = String(name ?? '').replace(PHONE_IN_LINK, '$1numero-oculto').replace(PHONE_ANYWHERE, '[número]').trim();
  return masked && !PHONE_NAME.test(masked) && masked.replace(/\[número\]/g, '').trim() ? masked : null;
}

/**
 * The people map as the export uses it: bare JID → app username, `self` → Franz's username. A JID key keeps its
 * digits only in memory here; nothing of it is written.
 */
export function peopleIndex(map) {
  const out = new Map();
  for (const [k, v] of Object.entries(map ?? {})) if (typeof v === 'string' && v) out.set(k === 'self' ? 'self' : bareJid(k), v);
  return out;
}

/**
 * The export's rows from a wacli store: one per message of the given chats, ready for the search file. `chats` is
 * { key: { jid, label, aliases? } }; `people` from peopleIndex; `transcripts` { msgId: text } (the bridge's).
 */
export function exportRows(db, { chats, people = new Map(), transcripts = {} }) {
  const contacts = new Map();
  for (const c of db.prepare("SELECT jid, coalesce(nullif(push_name, ''), nullif(full_name, ''), nullif(first_name, ''), nullif(business_name, '')) name FROM contacts").all())
    if (maskName(c.name)) contacts.set(bareJid(c.jid), clip(maskName(c.name), 80));
  try {
    // A LID's phone JID (or the other way round): one person under two JIDs.
    for (const a of db.prepare('SELECT jid, alias FROM contact_aliases').all()) {
      const [x, y] = [bareJid(a.jid), bareJid(a.alias)];
      if (contacts.has(x) && !contacts.has(y)) contacts.set(y, contacts.get(x));
      if (people.has(x) && !people.has(y)) people.set(y, people.get(x));
    }
  } catch {
    /* an older store without aliases */
  }
  const userOf = (jid, fromMe) => (fromMe ? people.get('self') : people.get(bareJid(jid))) ?? null;
  const nameOf = (jid, fromMe, given) => {
    if (fromMe) return 'Franz';
    const name = maskName(given);
    if (name) return clip(name, 80);
    return contacts.get(bareJid(jid)) ?? null;
  };
  // «@593991234567» or «@123456789012345» (a LID) in a text: the person's name, never the number.
  const mention = digits => {
    for (const jid of [`${digits}@s.whatsapp.net`, `${digits}@lid`]) {
      const user = people.get(jid);
      if (user) return `@{${user}}`;
      if (contacts.has(jid)) return '@' + contacts.get(jid);
    }
    return '@alguien';
  };
  const clean = text => maskPhones(String(text ?? '').replace(/@(\d{6,20})\b/g, (_, d) => mention(d)));
  const textOf = r => {
    const t = r.text || r.media_caption || (r.display_text && !PLACEHOLDER.test(r.display_text.trim()) ? r.display_text : '');
    return clean(t).trim();
  };
  const rows = [];
  for (const [key, chat] of Object.entries(chats)) {
    const jids = [chat.jid, ...(chat.aliases ?? [])].map(bareJid);
    const all = db
      .prepare(
        `SELECT msg_id, ts, from_me, sender_jid, sender_name, text, display_text, media_caption, media_type, filename, quoted_msg_id, quoted_sender_jid
         FROM messages WHERE chat_jid IN (${jids.map(() => '?').join(',')}) AND revoked = 0 AND reaction_to_id IS NULL AND deleted_at IS NULL ORDER BY ts, rowid`,
      )
      .all(...jids);
    const byId = new Map(all.map(r => [r.msg_id, r]));
    for (const r of all) {
      const text = textOf(r);
      const media = r.media_type ? String(r.media_type).toLowerCase() : null;
      const transcript = transcripts[r.msg_id] ? clean(clip(transcripts[r.msg_id], 3000)) : null;
      if (!text && !media && !transcript) continue;
      const bot = Boolean(r.from_me && ownAnswer(text));
      const q = r.quoted_msg_id ? byId.get(r.quoted_msg_id) : null;
      rows.push({
        id: String(r.msg_id),
        chat: key,
        at: Number(r.ts) * 1000,
        sender_user: bot ? null : userOf(r.sender_jid, r.from_me),
        sender_name: bot ? 'Claude (asistente)' : nameOf(r.sender_jid, r.from_me, r.sender_name),
        text: clip(text, 6000) || null,
        media,
        filename: r.filename ? clip(maskName(r.filename) ?? '[número]', 200) : null,
        links: JSON.stringify(linksOf(text)),
        transcript,
        reply_to: r.quoted_msg_id ? String(r.quoted_msg_id) : null,
        reply_user: q ? (q.from_me && ownAnswer(q.text) ? null : userOf(q.sender_jid, q.from_me)) : r.quoted_sender_jid ? userOf(r.quoted_sender_jid, false) : null,
        reply_name: q ? (q.from_me && ownAnswer(q.text) ? 'Claude (asistente)' : nameOf(q.sender_jid, q.from_me, q.sender_name)) : null,
        reply_text: q ? clip(textOf(q), 300) || null : null,
      });
    }
  }
  return rows;
}

/** Writes the search file (a fresh SQLite file, then renamed over the old one). Returns its content hash. */
export function writeSearchFile(file, rows, { chats, syncedAt = Date.now() }) {
  mkdirSync(dirname(file), { recursive: true, mode: 0o700 });
  const tmp = `${file}.tmp-${process.pid}`;
  rmSync(tmp, { force: true });
  const db = new DatabaseSync(tmp);
  try {
    db.exec(`PRAGMA journal_mode = DELETE;
      CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT NOT NULL);
      CREATE TABLE messages (id TEXT NOT NULL, chat TEXT NOT NULL, at INTEGER NOT NULL, sender_user TEXT, sender_name TEXT,
        text TEXT, media TEXT, filename TEXT, links TEXT NOT NULL DEFAULT '[]', transcript TEXT,
        reply_to TEXT, reply_user TEXT, reply_name TEXT, reply_text TEXT, PRIMARY KEY (chat, id));
      CREATE INDEX messages_at ON messages(at);`);
    db.exec('BEGIN');
    const put = db.prepare(
      'INSERT OR REPLACE INTO messages (id,chat,at,sender_user,sender_name,text,media,filename,links,transcript,reply_to,reply_user,reply_name,reply_text) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)',
    );
    const hash = createHash('sha256');
    for (const r of rows) {
      const values = [r.id, r.chat, r.at, r.sender_user, r.sender_name, r.text, r.media, r.filename, r.links, r.transcript, r.reply_to, r.reply_user, r.reply_name, r.reply_text];
      put.run(...values);
      hash.update(JSON.stringify(values));
    }
    const meta = db.prepare('INSERT INTO meta (key, value) VALUES (?, ?)');
    meta.run('schema', String(SCHEMA_VERSION));
    meta.run('synced_at', new Date(syncedAt).toISOString());
    meta.run('chats', JSON.stringify(chats));
    db.exec('COMMIT');
    db.close();
    renameSync(tmp, file);
    return hash.digest('hex');
  } catch (e) {
    try {
      db.close();
    } catch {
      /* closed */
    }
    rmSync(tmp, { force: true });
    throw e;
  }
}

function run(bin, args, { input = null, timeout = 120_000 } = {}) {
  return new Promise((resolve, reject) => {
    const child = execFile(bin, args, { timeout, maxBuffer: 64 * 1024 * 1024 }, (e, stdout, stderr) => (e ? reject(new Error(clip(String(stderr || e.message).trim(), 300))) : resolve(stdout)));
    if (input != null) child.stdin.end(input);
  });
}
const q = s => `'${String(s).replaceAll("'", "'\\''")}'`;

/**
 * The export as the timer runs it: the config's chats from the store, the people map from the server, the file
 * written in the state folder and, with `push`, copied to the server (only when it changed, or once an hour so the
 * server sees it is fresh). Returns { rows, hash, pushed }.
 */
export async function searchExport({ config, stateDir, push = false, now = Date.now, log = console }) {
  const opts = { ...SEARCH_DEFAULTS, ...(config.search ?? {}) };
  const chats = {};
  for (const [name, key] of Object.entries(opts.chats)) {
    const c = config.chats?.[key];
    if (!c?.jid) throw new Error(`search: chat ${key} is not in the config`);
    chats[name] = { jid: c.jid, aliases: c.aliases ?? [], label: c.label ?? name };
  }
  const target = opts.target;
  const route = config.routes?.[target.route] ?? {};
  // The people map: the group's (`chats.group.people.map`, on the group's route).
  const mapFile = opts.peopleMap ?? config.chats?.group?.people?.map ?? null;
  let people = new Map();
  if (mapFile) {
    const mapRoute = config.routes?.[config.chats?.group?.route] ?? {};
    const raw = mapRoute.ssh ? await run('ssh', ['-o', 'BatchMode=yes', mapRoute.ssh, `cat ${q(mapFile)}`]) : readFileSync(mapFile, 'utf8');
    people = peopleIndex(JSON.parse(raw));
  }
  // Franz's own JIDs (his chat with himself): a mention of him is his name too.
  const self = config.chats?.self;
  if (people.has('self') && self?.jid) for (const jid of [self.jid, ...(self.aliases ?? [])]) people.set(bareJid(jid), people.get('self'));
  let transcripts = {};
  try {
    transcripts = Object.fromEntries(Object.entries(JSON.parse(readFileSync(join(stateDir, 'state.json'), 'utf8')).transcripts ?? {}).map(([id, t]) => [id, typeof t === 'string' ? t : (t?.text ?? '')]).filter(([, t]) => t));
  } catch {
    /* no bridge state */
  }
  const store = new DatabaseSync(join(config.wacli.store, 'wacli.db'), { readOnly: true, timeout: 5000 });
  let rows;
  try {
    rows = exportRows(store, { chats, people, transcripts });
  } finally {
    store.close();
  }
  const labels = Object.fromEntries(Object.entries(chats).map(([k, c]) => [k, c.label]));
  const file = join(stateDir, 'wa-search.sqlite');
  const hash = writeSearchFile(file, rows, { chats: labels, syncedAt: now() });
  const sentFile = join(stateDir, 'wa-search.pushed.json');
  let last = {};
  try {
    last = JSON.parse(readFileSync(sentFile, 'utf8'));
  } catch {
    /* never pushed */
  }
  let pushed = false;
  if (push && (last.hash !== hash || now() - (last.at ?? 0) > 3_600_000)) {
    const dest = target.path;
    const tmp = `${dest}.incoming`;
    if (route.ssh) {
      await run('ssh', ['-o', 'BatchMode=yes', route.ssh, `mkdir -p -m 700 ${q(dirname(dest))} && cat > ${q(tmp)} && chmod 600 ${q(tmp)} && mv -f ${q(tmp)} ${q(dest)}`], { input: readFileSync(file) });
    } else {
      mkdirSync(dirname(dest), { recursive: true, mode: 0o700 });
      await run('cp', [file, tmp]);
      renameSync(tmp, dest);
    }
    writeFileSync(sentFile, JSON.stringify({ hash, at: now() }), { mode: 0o600 });
    pushed = true;
  }
  log.log?.(`search-export: ${rows.length} messages (${Object.keys(chats).join(', ')})${pushed ? `, copied to ${route.ssh ? route.ssh + ':' : ''}${target.path}` : push ? ', unchanged' : ''}`);
  return { rows: rows.length, hash, pushed, file: existsSync(file) ? file : null };
}
