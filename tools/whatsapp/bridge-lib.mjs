// The WhatsApp bridge's pure parts (tools/whatsapp/bridge.mjs), kept apart for tests: which
// message wakes the assistant, the relay's own commands, the reply's picture and voice blocks, the
// hourly limit, the payload of wacli's webhook and the decryption of WhatsApp media.
import { createDecipheriv, createHmac, hkdfSync, timingSafeEqual } from 'node:crypto';

/** `@claude` as a whole word, anywhere in the text (case ignored). */
export const TRIGGER = /(^|[^\w@.])@claude(?![\w@])/i;
/** The bridge's own answers start with this; such a message never wakes it (loop guard). */
export const PREFIX = 'Claude: ';
export const LOOP_GUARD = /^\s*claude\s*:/i;
export const COMMANDS = ['help', 'status', 'whoami', 'usage', 'stop', 'start', 'ack'];

/**
 * Claude addressed by name as a vocative, at the start or the end of the text: «Claude, …», «claude ¿y…?»,
 * «…, claude?». Not inside other words (claudette, claude.ai), not «Claude: …» (the bridge's own prefix).
 */
export const VOCATIVE = /^\s*[¿¡]*claude(?=[\s,¿¡?!])|[\s,]claude\s*[?!.]*\s*$/i;
export const named = text => VOCATIVE.test(String(text ?? '')) && !LOOP_GUARD.test(String(text ?? ''));

export const tagged = text => TRIGGER.test(String(text ?? ''));
export const ownAnswer = text => LOOP_GUARD.test(String(text ?? ''));

/**
 * The relay's command in a message: { name, arg } for /help, /status, /whoami, /usage, /stop, /start,
 * /ack at the start (after an optional @claude), { name: 'unknown', word } for another /word that
 * tags @claude, else null. Pure.
 */
export function command(text) {
  const t = String(text ?? '').replace(/^\s*@claude\b[\s,:]*/i, '');
  const m = /^\s*\/([a-z]+)\b\s*(\S*)/i.exec(t);
  if (!m) return null;
  const name = m[1].toLowerCase();
  if (COMMANDS.includes(name)) return m[2] ? { name, arg: m[2] } : { name };
  return tagged(text) ? { name: 'unknown', word: name.slice(0, 20) } : null;
}

/** sha256=<hex HMAC of the raw body>, as wacli's X-Wacli-Signature. */
export function sign(secret, raw) {
  return 'sha256=' + createHmac('sha256', secret).update(raw).digest('hex');
}
export function verify(secret, raw, header) {
  if (!secret || typeof header !== 'string') return false;
  const want = Buffer.from(sign(secret, raw));
  const given = Buffer.from(header.trim());
  return want.length === given.length && timingSafeEqual(want, given);
}

/**
 * A wacli webhook body (a stored live message: Chat, ID, SenderJID, Timestamp, FromMe, Text,
 * Media { Type, Caption, … }, PushName, ReplyToID, ReplyToSenderJID, ReactionToID, Revoked, Edited,
 * ChatName) as { id, chat, sender, fromMe, at, text, media, pushName, replyToId, replyToSender,
 * skip }; null when it is no message (receipts, presence). `skip`: a reaction, an edit or a deletion.
 */
export function parsePayload(body) {
  if (!body || typeof body !== 'object' || body.EventType) return null;
  const id = body.ID;
  const chat = typeof body.Chat === 'string' ? body.Chat : null;
  if (!id || !chat) return null;
  const media = body.Media && typeof body.Media === 'object' ? { type: String(body.Media.Type ?? '').toLowerCase(), mime: body.Media.MimeType ?? null, bytes: Number(body.Media.FileLength) || null } : null;
  const at = Date.parse(body.Timestamp);
  return {
    id: String(id),
    chat,
    sender: body.SenderJID ? String(body.SenderJID) : null,
    fromMe: Boolean(body.FromMe),
    at: Number.isNaN(at) ? Date.now() : at,
    text: String(body.Text || body.Media?.Caption || ''),
    media,
    pushName: body.PushName ? String(body.PushName) : null,
    replyToId: body.ReplyToID || null,
    replyToSender: body.ReplyToSenderJID || null,
    skip: body.ReactionToID ? 'reaction' : body.Revoked ? 'revoked' : body.Edited ? 'edited' : null,
  };
}

/** A JID without its device part (123:4@s.whatsapp.net → 123@s.whatsapp.net). */
export const bareJid = jid => String(jid ?? '').replace(/:\d+(?=@)/, '');

/**
 * The chat's final message split into what the bridge sends: { text, images: [{ src, caption }],
 * voice, silent }. Pictures come from an [images] … [/images] block, one per line as
 * "<path or https URL> | <caption>" (at most `max`); `[voice]` asks for a voice note; `[silent]`
 * alone means no answer. Pure.
 */
export function parseReply(raw, { max = 5 } = {}) {
  let text = String(raw ?? '');
  const images = [];
  text = text.replace(/\[images\]([\s\S]*?)(?:\[\/images\]|$)/gi, (_, block) => {
    for (const line of block.split('\n')) {
      const m = /^\s*[-*]?\s*(https?:\/\/\S+|\/\S+)\s*(?:\|\s*(.*))?$/.exec(line.trim());
      if (m && images.length < max) images.push({ src: m[1].replace(/^<|>$/g, ''), caption: (m[2] ?? '').trim() });
    }
    return '';
  });
  const voice = /^\s*\[voice\]\s*$/im.test(text);
  text = text.replace(/^\s*\[voice\]\s*$/gim, '').trim();
  const silent = /^\[silent\]$/i.test(text);
  return { text: silent ? '' : text, images, voice, silent };
}

/** Markdown the chat may write, in WhatsApp's own formatting. Pure. */
export function toWhatsApp(md) {
  return String(md ?? '')
    .replace(/\*\*(.+?)\*\*/g, '*$1*')
    .replace(/__(.+?)__/g, '_$1_')
    .replace(/^#{1,6}\s+(.+)$/gm, '*$1*')
    .replace(/\[([^\]]+)\]\((https?:\/\/[^)\s]+)\)/g, '$1: $2')
    .replace(/\n{3,}/g, '\n\n')
    .trim();
}

/** Text for a voice note: no formatting, no links. Pure. */
export function speakable(text, max = 600) {
  const t = String(text ?? '')
    .replace(/https?:\/\/\S+/g, '')
    .replace(/[*_~`#>]/g, '')
    .replace(/^\s*-\s+/gm, '')
    .replace(/\s+/g, ' ')
    .trim();
  return t.length > max ? t.slice(0, max).replace(/\s+\S*$/, '') + '…' : t;
}

/** A rough language guess for the voice: Spanish unless the text reads as English. Pure. */
export function voiceLanguage(text) {
  const t = ` ${String(text).toLowerCase()} `;
  const en = (t.match(/\s(the|and|is|are|of|with|you|this|that|it)\s/g) ?? []).length;
  const es = (t.match(/\s(el|la|los|las|de|que|y|es|en|con|un|una|por)\s/g) ?? []).length + (/[ñáéíóú¿¡]/.test(t) ? 2 : 0);
  return en > es ? 'en' : 'es';
}

/** Times in `list` within the last hour of `now`. Pure. */
export const lastHour = (list, now) => (list ?? []).filter(t => now - t < 3_600_000);

/** The genera (and words) the voice transcription is told to expect. */
export const KEYTERMS = [
  'Ithomiini', 'Aeria', 'Athesis', 'Athyrtis', 'Brevioleria', 'Callithomia', 'Ceratinia', 'Dircenna', 'Elzunia', 'Episcada',
  'Epityches', 'Forbestra', 'Godyris', 'Greta', 'Heterosais', 'Hyalenna', 'Hyalyris', 'Hypoleria', 'Hyposcada', 'Hypothyris',
  'Ithomia', 'Mcclungia', 'Mechanitis', 'Melinaea', 'Methona', 'Napeogenes', 'Oleria', 'Olyras', 'Pagyris', 'Patricia',
  'Placidina', 'Pseudoscada', 'Pteronymia', 'Sais', 'Scada', 'Thyridia', 'Tithorea', 'Veladyris', 'Velamysta', 'Heliconius',
  'Ikiam', 'CAM', 'Claude',
];

const MEDIA_INFO = {
  image: 'WhatsApp Image Keys',
  sticker: 'WhatsApp Image Keys',
  video: 'WhatsApp Video Keys',
  gif: 'WhatsApp Video Keys',
  audio: 'WhatsApp Audio Keys',
  ptt: 'WhatsApp Audio Keys',
  document: 'WhatsApp Document Keys',
};

/**
 * A WhatsApp media file from its encrypted download (`enc`) and the message's media key, as
 * whatsmeow does: HKDF-SHA256 to 112 bytes (IV, AES key, MAC key), the last 10 bytes a MAC of IV +
 * ciphertext, AES-256-CBC. Throws when the MAC does not match.
 */
export function decryptMedia(enc, mediaKey, type) {
  const info = MEDIA_INFO[type] ?? MEDIA_INFO.document;
  const keys = Buffer.from(hkdfSync('sha256', mediaKey, Buffer.alloc(32), info, 112));
  const iv = keys.subarray(0, 16);
  const cipherKey = keys.subarray(16, 48);
  const macKey = keys.subarray(48, 80);
  const data = enc.subarray(0, enc.length - 10);
  const mac = enc.subarray(enc.length - 10);
  const want = createHmac('sha256', macKey).update(iv).update(data).digest().subarray(0, 10);
  if (!timingSafeEqual(want, mac)) throw new Error('media MAC mismatch');
  const decipher = createDecipheriv('aes-256-cbc', cipherKey, iv);
  return Buffer.concat([decipher.update(data), decipher.final()]);
}

/** The picture formats sent out (by their first bytes): jpg, png, webp, gif; else null. */
export function imageKind(buf) {
  if (!buf || buf.length < 12) return null;
  if (buf[0] === 0xff && buf[1] === 0xd8 && buf[2] === 0xff) return 'jpg';
  if (buf.subarray(0, 8).equals(Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]))) return 'png';
  if (buf.subarray(0, 4).toString('latin1') === 'RIFF' && buf.subarray(8, 12).toString('latin1') === 'WEBP') return 'webp';
  if (buf.subarray(0, 4).toString('latin1') === 'GIF8') return 'gif';
  return null;
}

/** How the bridge shows it is working: a 👀 then ✅ reaction on the message, or one message edited with the steps. */
export const ACK_MODES = ['reaction', 'edit'];

const STEP_LABELS = [
  [/__(query|count_records|run_report)$/, 'consultando la base'],
  [/__(find_records|search_text|get_record|row_history|describe_sheet)$/, 'buscando registros'],
  [/__identify_species$/, 'identificando la especie'],
  [/__(reference_photos|photo_files_look|photo_facts|show_photos|drive_photos_look|gallery_specimens)$/, 'mirando fotos'],
  [/__(maps_snapshot)$/, 'dibujando el mapa'],
  [/__(maps_localities|maps_species)$/, 'mirando los mapas'],
  [/__host_plants$/, 'buscando plantas hospederas'],
  [/__(propose_changes|update_proposal|match_notebook)$/, 'preparando la propuesta'],
  [/__(get_proposal|list_proposals)$/, 'revisando propuestas'],
  [/__(review_issues|review_suggestions)$/, 'revisando problemas de la base'],
  [/__(list_history|get_history_group|preview_undo)$/, 'mirando el historial'],
  [/__(sync_documents)$/, 'leyendo los documentos'],
  [/^(Read|Glob|Grep)$/, 'leyendo archivos'],
  [/^(WebFetch|WebSearch)$/, 'buscando en la web'],
  [/^Skill$/, 'leyendo su guía'],
  [/^(Agent|Task|subagent)$/, 'pidiendo ayuda a un subagente'],
  [/^(Bash|command_execution)$/, 'ejecutando un comando'],
];
/** A short line for a tool step of the chat (its name, never its reasoning); null for steps not worth a line. Pure. */
export function stepLabel({ tool, title } = {}) {
  const t = String(tool ?? '');
  if (/^(TodoWrite|ToolSearch)$/.test(t)) return null;
  for (const [re, label] of STEP_LABELS) if (re.test(t)) return label;
  const name = t.replace(/^mcp__[^_]+__/, '').replace(/_/g, ' ');
  return name ? `usando ${name}` : title ? String(title).toLowerCase() : null;
}

/** The status message for the edit mode: the latest distinct steps under one line. Pure. */
export function statusMessage(steps, max = 6) {
  const lines = [];
  for (const s of steps) if (s && lines.at(-1) !== s) lines.push(s);
  return ['Claude: pensando…', ...lines.slice(-max).map(s => `- ${s}`)].join('\n');
}

/** A folder-safe name from a chat's label: «Ithomiini 🦋 (underground)» → ithomiini-underground. */
export const slug = label =>
  String(label ?? '')
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-+|-+$/g, '') || 'chat';

/** What a media message is called in the context and the history. */
export const mediaTag = type => ({ image: 'foto', audio: 'audio', ptt: 'audio', video: 'video', sticker: 'sticker', document: 'documento' })[String(type ?? '').toLowerCase()] ?? (type ? String(type).toLowerCase() : null);

/**
 * Appends the JSON lines whose "id" (or "of", for a transcript) is not in the file yet; returns how
 * many it wrote. Self-contained: it is also sent to claudeclaw as source (node -e), so it reaches
 * only for the built-in modules.
 */
export function appendNew(file, text) {
  const fs = process.getBuiltinModule('node:fs');
  const path = process.getBuiltinModule('node:path');
  fs.mkdirSync(path.dirname(file), { recursive: true, mode: 0o750 });
  const key = o => (o.t === 'transcript' ? 'T:' + o.of : 'M:' + o.id);
  const seen = new Set();
  if (fs.existsSync(file))
    for (const line of fs.readFileSync(file, 'utf8').split('\n'))
      try {
        seen.add(key(JSON.parse(line)));
      } catch {
        /* blank or partial */
      }
  let out = '';
  let n = 0;
  for (const line of String(text).split('\n')) {
    let o;
    try {
      o = JSON.parse(line);
    } catch {
      continue;
    }
    if (seen.has(key(o))) continue;
    seen.add(key(o));
    out += JSON.stringify(o) + '\n';
    n++;
  }
  if (out) fs.appendFileSync(file, out, { mode: 0o640 });
  return n;
}
