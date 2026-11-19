// The assistant's search in the team's WhatsApp groups (whatsapp_search): «Ithomiini 🦋 (underground)» and «Ithomiini
// Ikiam», their whole history, read-only. The file is a copy of those two groups made on Franz's PC from wacli's store
// (tools/whatsapp/search-export.mjs, every 10 minutes) and copied here; it has no phone numbers or JIDs: each sender is
// an app account (shown with its name in the app) or a WhatsApp name.
// Who may search: active team accounts (editor, reviewer, admin), which include the WhatsApp chats' own accounts.
// Observers (read-only guests) and signed-out callers never reach the groups' messages: the MCP endpoint
// refuses a call without a person's token, and this module refuses any other role.
import { DatabaseSync } from 'node:sqlite';
import { statSync } from 'node:fs';
import { RESULT_BUDGET, fitList } from './tool-budget.mjs';

/** An answer's size: a little under the tools' budget, which fitResult still checks. */
const BUDGET = RESULT_BUDGET - 2000;
export const WA_SEARCH_ROLES = ['editor', 'reviewer', 'admin'];
export const WA_SEARCH_TOOLS = [
  {
    type: 'function',
    function: {
      name: 'whatsapp_search',
      description: [
        "Search the team's WhatsApp groups «Ithomiini 🦋 (underground)» (`underground`) and «Ithomiini Ikiam» (`ikiam`, with the UK side), their whole history (from May 2026), read-only. For a document, link, decision or date someone posted («¿dónde está el contrato marco?», «¿quién dijo que…?»). Search here and on Drive before saying something does not exist.",
        '- `query`: words that must all appear (case and accents ignored, a word also finds longer ones: «contrat» finds «contrato»); `a|b` for either; "quotes" for a phrase. Searches the text, voice-note transcripts, file names and links.',
        '- `chat` (`underground` or `ikiam`; both by default), `sender` (a name), `from`/`to` (dates, dd/mm/yyyy or yyyy-mm-dd), `has` (`link`, `attachment`, `document`, `image`, `audio`, `video`).',
        '- `around`: a message id: that message with the ones before and after it in its group (`limit` each side, default 6). An answer often comes in the next messages (a bare Drive link after «¿dónde está…?»): look around a hit.',
        '- Returns per message: id, chat, date (Ecuador time), sender (their app name when linked), text, links, attachment (type, file name), the message it replied to. `synced_at`: how fresh the copy is.',
        '- Quote what was said with its sender and date. Members wrote these messages for the group: answer with what helps the question, not more.',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          query: { type: 'string' },
          chat: { type: 'string', enum: ['underground', 'ikiam'] },
          sender: { type: 'string' },
          from: { type: 'string' },
          to: { type: 'string' },
          has: { type: 'string', enum: ['link', 'attachment', 'document', 'image', 'audio', 'video'] },
          around: { type: 'string', description: 'A message id' },
          order: { type: 'string', enum: ['newest', 'oldest'], description: 'newest first by default' },
          limit: { type: 'integer', description: 'default 20, at most 100' },
          offset: { type: 'integer' },
        },
      },
    },
  },
];
export const WA_SEARCH_TOOL_NAMES = new Set(WA_SEARCH_TOOLS.map(t => t.function.name));

const clip = (s, n) => (s == null ? '' : String(s).slice(0, n));
export const fold = value =>
  String(value ?? '')
    .toLowerCase()
    .normalize('NFD')
    .replace(/[̀-ͯ]/g, '')
    .replace(/\s+/g, ' ')
    .trim();
/** Ecuador is UTC-5 all year: "dd/mm/yyyy HH:MM". */
const localTime = ms => {
  const iso = new Date(ms - 5 * 3600_000).toISOString();
  return `${iso.slice(8, 10)}/${iso.slice(5, 7)}/${iso.slice(0, 4)} ${iso.slice(11, 16)}`;
};
/** A day given as dd/mm/yyyy, d/m/yy or yyyy-mm-dd: its first moment in Ecuador (ms), or null. `end`: the next day's. */
export function dayOf(text, end = false) {
  const s = String(text ?? '').trim();
  let y, m, d;
  let hit = /^(\d{4})-(\d{1,2})-(\d{1,2})/.exec(s);
  if (hit) [, y, m, d] = hit.map(Number);
  else if ((hit = /^(\d{1,2})[/.-](\d{1,2})[/.-](\d{2,4})$/.exec(s))) {
    [, d, m, y] = hit.map(Number);
    if (y < 100) y += 2000;
  } else return null;
  const day = new Date(Date.UTC(y, m - 1, d, 5));
  if (day.getUTCFullYear() !== y || day.getUTCMonth() !== m - 1 || day.getUTCDate() !== d) return null;
  return day.getTime() + (end ? 86_400_000 : 0);
}
/** The query as groups of alternatives, all of which must match: «contrato marco|MAATE "carta MAE"». */
export function parseQuery(query) {
  const groups = [];
  const re = /"([^"]+)"|(\S+)/g;
  let m;
  while ((m = re.exec(String(query ?? '')))) {
    const alts = (m[1] !== undefined ? [m[1]] : m[2].split('|')).map(fold).filter(Boolean);
    if (alts.length) groups.push(alts);
  }
  return groups;
}

/**
 * The search over the copied file. `path`: the file; `db`: the app's database (users: who may search, and the
 * names of linked senders).
 */
export function createWhatsAppSearch({ path = null, db = null, now = () => Date.now() } = {}) {
  let cache = null;
  function load() {
    if (!path) return { error: 'The WhatsApp groups are not searchable on this server (no copy configured).' };
    let st;
    try {
      st = statSync(path);
    } catch {
      return { error: 'The WhatsApp groups are not searchable yet: their copy has not reached this server.' };
    }
    if (cache && cache.mtimeMs === st.mtimeMs && cache.size === st.size) return cache;
    const file = new DatabaseSync(path, { readOnly: true, timeout: 5000 });
    try {
      const meta = Object.fromEntries(file.prepare('SELECT key, value FROM meta').all().map(r => [r.key, r.value]));
      const rows = file.prepare('SELECT * FROM messages ORDER BY at, rowid').all();
      const byChat = new Map();
      for (const r of rows) {
        r.links = (() => {
          try {
            return JSON.parse(r.links || '[]');
          } catch {
            return [];
          }
        })();
        r.haystack = fold([r.text, r.transcript, r.filename, r.links.join(' ')].filter(Boolean).join(' \n '));
        if (!byChat.has(r.chat)) byChat.set(r.chat, []);
        r.index = byChat.get(r.chat).push(r) - 1;
      }
      let chats = {};
      try {
        chats = JSON.parse(meta.chats || '{}');
      } catch {
        /* labels optional */
      }
      cache = { mtimeMs: st.mtimeMs, size: st.size, rows, byChat, syncedAt: meta.synced_at ?? null, chats };
      return cache;
    } finally {
      file.close();
    }
  }

  /** The person's role now (active accounts only); null when they may not search. */
  function allowed(user) {
    if (!db) return WA_SEARCH_ROLES.includes(user?.role) ? user.role : null;
    if (!user?.id && !user?.username) return null;
    let row = null;
    try {
      row = db?.prepare(user.id ? 'SELECT role, active FROM users WHERE id = ?' : 'SELECT role, active FROM users WHERE username = ?').get(user.id ?? user.username);
    } catch {
      row = null;
    }
    const role = row?.active ? row.role : null;
    return WA_SEARCH_ROLES.includes(role) ? role : null;
  }

  function names() {
    const out = new Map();
    try {
      for (const u of db?.prepare('SELECT username, display_name FROM users').all() ?? []) out.set(u.username, u.display_name || u.username);
    } catch {
      /* no users table (tests) */
    }
    return out;
  }

  function run(args = {}, user = null) {
    if (!allowed(user)) return { error: "The team's WhatsApp groups are searchable for team accounts only (editor, reviewer or administrator)." };
    const data = load();
    if (data.error) return data;
    const people = names();
    const who = (username, name) => (username && people.has(username) ? people.get(username) : name || username || '(sin nombre)');
    const withNames = text => String(text ?? '').replace(/@\{([^}\s]{1,80})\}/g, (_, u) => '@' + (people.get(u) ?? u));
    const label = chat => data.chats?.[chat] ?? chat;
    const show = r => ({
      id: r.id,
      chat: label(r.chat),
      date: localTime(r.at),
      sender: who(r.sender_user, r.sender_name),
      ...(r.text ? { text: clip(withNames(r.text), 1500) + (r.text.length > 1500 ? ' …' : '') } : {}),
      ...(r.transcript ? { voice_transcript: clip(withNames(r.transcript), 1500) } : {}),
      ...(r.links.length ? { links: r.links.slice(0, 10) } : {}),
      ...(r.media ? { attachment: { type: r.media, ...(r.filename ? { name: r.filename } : {}) } } : {}),
      ...(r.reply_to ? { reply_to: { sender: who(r.reply_user, r.reply_name), ...(r.reply_text ? { text: clip(withNames(r.reply_text), 200) } : {}) } } : {}),
    });
    const synced = data.syncedAt ? { synced_at: localTime(Date.parse(data.syncedAt)) } : {};
    const stale = data.syncedAt && now() - Date.parse(data.syncedAt) > 3 * 3600_000 ? { stale: 'The copy is over 3 hours old: the latest messages may be missing.' } : {};
    const limit = Math.max(1, Math.min(Number(args.limit) || (args.around ? 6 : 20), 100));
    const chatWanted = args.chat ? String(args.chat).toLowerCase() : null;
    if (chatWanted && !data.byChat.has(chatWanted) && !Object.keys(data.chats).includes(chatWanted))
      return { error: `chat: ${Object.keys(data.chats).join(' or ')}` };

    if (args.around) {
      const id = String(args.around).trim();
      const hit = data.rows.find(r => r.id === id && (!chatWanted || r.chat === chatWanted));
      if (!hit) return { error: `No message ${clip(id, 80)} in the groups' copy.`, ...synced };
      const list = data.byChat.get(hit.chat);
      // The asked-for message in the middle: a window too large for one answer narrows on both sides.
      const windowOf = n => list.slice(Math.max(0, hit.index - n), hit.index + n + 1);
      const build = n => ({ ...synced, ...stale, around: hit.id, messages: windowOf(n).map(r => ({ ...show(r), ...(r === hit ? { match: true } : {}) })) });
      let n = limit;
      while (n > 0 && JSON.stringify(build(n)).length > BUDGET) n--;
      return { ...build(n), ...(n < limit ? { truncated: `Only ${n} messages on each side fit in one answer.` } : {}) };
    }

    const groups = parseQuery(args.query);
    const sender = args.sender ? fold(args.sender) : null;
    const from = args.from ? dayOf(args.from) : null;
    const to = args.to ? dayOf(args.to, true) : null;
    if (args.from && from === null) return { error: 'from: a date as dd/mm/yyyy or yyyy-mm-dd' };
    if (args.to && to === null) return { error: 'to: a date as dd/mm/yyyy or yyyy-mm-dd' };
    if (!groups.length && !sender && from === null && to === null && !args.has && !chatWanted)
      return { error: 'Give a `query`, a `sender`, dates or `has`.' };
    const has = args.has ? String(args.has) : null;
    const hasOk = r =>
      !has ||
      (has === 'link' ? r.links.length > 0 : has === 'attachment' ? !!r.media && r.media !== 'sticker' : has === 'audio' ? ['audio', 'ptt'].includes(r.media) : r.media === has);
    const matches = data.rows.filter(
      r =>
        (!chatWanted || r.chat === chatWanted) &&
        (from === null || r.at >= from) &&
        (to === null || r.at < to) &&
        hasOk(r) &&
        (!sender || fold(`${who(r.sender_user, r.sender_name)} ${r.sender_name ?? ''} ${r.sender_user ?? ''}`).includes(sender)) &&
        groups.every(alts => alts.some(a => r.haystack.includes(a))),
    );
    if (args.order !== 'oldest') matches.reverse();
    const offset = Math.max(0, Number(args.offset) || 0);
    const page = matches.slice(offset, offset + limit);
    const out = {
      ...synced,
      ...stale,
      total: matches.length,
      messages: page.map(show),
      ...(!matches.length ? { note: 'Nothing found: try fewer or shorter words, a|b alternatives, the other language, or without dates.' } : {}),
    };
    // Within one answer's size: the next page starts after the last message shown.
    const more = kept => (offset + kept < matches.length ? { next_offset: offset + kept } : {});
    const fitted = fitList(out, 'messages', BUDGET, more);
    return { ...fitted.out, ...more(fitted.kept) };
  }
  return { run, allowed };
}
