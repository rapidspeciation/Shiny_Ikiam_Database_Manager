// The WhatsApp bridge's pure parts (tools/whatsapp/bridge.mjs), kept apart for tests: which
// message wakes the assistant, the relay's own commands, the reply's picture and voice blocks, the
// hourly limit, the payload of wacli's webhook and the decryption of WhatsApp media.
import { createDecipheriv, createHmac, hkdfSync, timingSafeEqual } from 'node:crypto';
import { readFileSync } from 'node:fs';

/** `@claude` as a whole word, anywhere in the text (case ignored). */
export const TRIGGER = /(^|[^\w@.])@claude(?![\w@])/i;
/** The bridge's own answers start with this; such a message never wakes it (loop guard). */
export const PREFIX = 'Claude: ';
export const LOOP_GUARD = /^\s*claude\s*:/i;
export const COMMANDS = ['help', 'status', 'whoami', 'usage', 'stop', 'start', 'ack', 'tono'];
/** The humour levels a chat can choose with /tono (soul.md says what each means); the first is the default. */
export const TONES = ['normal', 'gracioso', 'picante'];

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
 * /ack, /tono at the start (after an optional @claude), { name: 'unknown', word } for another /word that
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
 * The chat's final message split into what the bridge sends: { text, images: [{ src, caption }], files (same shape),
 * voice, textOnly, silent }. Pictures come from an [images] … [/images] block, one per line as
 * "<path or https URL> | <caption>" (at most `max`); `[voice]` asks for a voice note; `[text]` asks for text only (it
 * wins over [voice] and over a voice note's default); `[silent]` alone means no answer. Pure.
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
  // [files] … [/files]: documents the tools made, "<path or app link> | caption", at most 3.
  const files = [];
  text = text.replace(/\[files\]([\s\S]*?)(?:\[\/files\]|$)/gi, (_, block) => {
    for (const line of block.split('\n')) {
      const m = /^\s*[-*]?\s*(https?:\/\/\S+|\/\S+)\s*(?:\|\s*(.*))?$/.exec(line.trim());
      if (m && files.length < 3) files.push({ src: m[1].replace(/^<|>$/g, ''), caption: (m[2] ?? '').trim() });
    }
    return '';
  });
  const voice = /^\s*\[voice\]\s*$/im.test(text);
  // [text]: the answer goes as text only, even to a voice note (the person asked for text).
  const textOnly = /^\s*\[text\]\s*$/im.test(text);
  text = text.replace(/^\s*\[(voice|text)\]\s*$/gim, '').trim();
  const silent = /^\[silent\]$/i.test(text);
  return { text: silent ? '' : text, images, files, voice: voice && !textOnly, textOnly, silent };
}

/**
 * A note to the operator before the reply, in Spanish or English, is taken off; real content never is. Two shapes:
 * - a line among the first few that hands the reply over with a colon («…así que va la respuesta:», «Respuesta:», «Aquí va:»,
 *   «Here's the reply:», «The message to send:»): only what follows it is the reply;
 * - a first paragraph about the requester in the third person («Lucía preguntó si…», «Daniela asked how many…»), when the
 *   reply follows in its own paragraph;
 * - a first paragraph of working lines, one short sentence each about searching or checking («Sigo buscando en los
 *   documentos…», «Voy a revisar la base.», «Let me check the sheet…»), when the reply follows in its own paragraph.
 * → { text, stripped: number of lines taken off, unsure: nothing left, or what is left still opens like such a note }. Pure.
 */
const HANDOVER = new RegExp(
  [
    String.raw`\b(va|aquí va|ahí va|te va|esta es|esto es|here is|here's|this is|below is)\b[^:]{0,40}\b(la respuesta|el mensaje|the reply|the answer|the response|the message)\b[^:]{0,25}`,
    String.raw`\b(va la respuesta|aquí va|ahí va|here it is|here goes)`,
    String.raw`\b(the )?(message|reply|answer|text) (to send|for whatsapp)`,
    String.raw`\b(mensaje|respuesta) (a|para) enviar`,
    String.raw`^(draft |final )?(reply|answer|response|respuesta|mensaje)( for whatsapp| para whatsapp| final)?`,
  ].map(p => `(?:${p})`).join('|').replace(/^/, '(?:') + String.raw`)\s*:\s*$`,
  'iu',
);
// «Lucía preguntó…», «Daniela asked…»: a name (one or two words, no comma: «Lucía, …» is someone being spoken to) and a verb of asking.
const ABOUT_ASKER = /^\s*\p{Lu}\p{L}+(?:\s\p{Lu}\p{L}+)?\s+(?:preguntó|pregunta|preguntaba|quiere saber|quería saber|pidió|pide|asked|asks|is asking|wants to know|wanted to know)\b/u;
// «Sigo buscando…», «Voy a revisar…», «Let me check…»: one clause about looking something up, nothing found in it yet
// (a comma or a second sentence may carry a finding, so such a line stays).
const WORKING = /^\s*(?:claude\s*:\s*)?(?:(?:sigo|estoy|ahora|primero|todavía|aún)\s+(?:buscando|revisando|consultando|mirando|verificando|leyendo|chequeando)|(?:voy a|déjame|dejame|permíteme|ahora|primero)\s+(?:buscar|revisar|consultar|mirar|verificar|leer|chequear)|(?:busco|reviso|consulto|verifico)\b|(?:let me|i'll|i will|i'm going to|now i'll|first i'll)\s+(?:check|look|search|query|read|verify|go through)|(?:still |now |first )?(?:checking|looking|searching|querying|reading|verifying))\b[^.!?\n…,;]{0,140}(?:,\s*(?:un momento|un segundo|ya vuelvo|one moment|one sec))?(?:…|\.{1,3}|:)?\s*$/iu;
const HANDOVER_WITHIN = 6; // lines: a hand-over colon further down belongs to the reply itself
export function stripPreamble(raw) {
  const original = String(raw ?? '');
  const lines = original.split('\n');
  const done = (rest, cut) => {
    const text = rest.join('\n').trim();
    const next = text.split('\n').find(l => l.trim()) ?? '';
    return { text, stripped: cut.filter(l => l.trim()).length, unsure: !text || HANDOVER.test(next.trim()) || ABOUT_ASKER.test(next) };
  };
  const handover = lines.findIndex((l, i) => i < HANDOVER_WITHIN && HANDOVER.test(l.trim()));
  if (handover >= 0) return done(lines.slice(handover + 1), lines.slice(0, handover + 1));
  const firstText = lines.findIndex(l => l.trim());
  if (firstText >= 0 && ABOUT_ASKER.test(lines[firstText])) {
    const blank = lines.findIndex((l, i) => i > firstText && !l.trim());
    const rest = blank > 0 ? lines.slice(blank) : [];
    if (rest.some(l => l.trim())) return done(rest, lines.slice(0, blank));
  }
  if (firstText >= 0 && WORKING.test(lines[firstText])) {
    const blank = lines.findIndex((l, i) => i > firstText && !l.trim());
    const head = blank > 0 ? lines.slice(0, blank) : lines;
    if (head.every(l => !l.trim() || WORKING.test(l))) return done(blank > 0 ? lines.slice(blank) : [], head);
  }
  return { text: original, stripped: 0, unsure: false };
}

/**
 * Blocks from the model's harness that can end up in a chat's last message (`<system-reminder>…</system-reminder>`,
 * `<thinking>`, `<function_calls>`, tool results), anywhere in the text: each is taken off whole, and an opening tag
 * that starts a line with no closing one is taken off to the end. Ordinary «<» and tag-like words in a sentence stay.
 * → { text, stripped: number of blocks taken off }. Pure.
 */
const HARNESS_TAGS = ['system-reminder', 'thinking', 'antml:thinking', 'function_calls', 'antml:function_calls', 'function_results', 'invoke', 'antml:invoke', 'tool_use', 'tool_result', 'search_quality_reflection', 'search_quality_score', 'reflection'];
const tagRe = t => t.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
const TAGS = HARNESS_TAGS.map(tagRe).join('|');
// A closed block anywhere; an unclosed one only when its tag opens a line (a tag named inside a sentence stays).
const HARNESS_BLOCK = new RegExp(`<(${TAGS})(?:\\s[^>]*)?>[\\s\\S]*?</\\1\\s*>|^[ \\t]*<(?:${TAGS})(?:\\s[^>]*)?>[\\s\\S]*$`, 'gm');
export function stripHarnessBlocks(raw) {
  let stripped = 0;
  const text = String(raw ?? '')
    .replace(HARNESS_BLOCK, () => (stripped++, ''))
    .replace(/\n{3,}/g, '\n\n')
    .trim();
  return { text: stripped ? text : String(raw ?? ''), stripped };
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

/** A word with a digit that reads fine aloud (its core, without surrounding punctuation): a number (1.05, 10.050, 2,5,
 * 0.001, -0.05, +12, 1,234.5678), a time (9:05, 08:15), a range (10-15, 2019-2024, 35.5-42.0), a fraction (1/3), or a
 * number with a short unit or ending (1500m, 3km, 3.5km/h, 2h, 5x, 1er, 2º, 7mo, 25°C, 50%). */
const NUM = String.raw`[-−+]?(?:0|[1-9]\d{0,5})(?:[.,]\d{3})*(?:[.,]\d+)?`;
const UNIT = String.raw`(?:mm|cm|m|km|km\/h|m\/s|g|kg|mg|ha|h|min|s|x|er|ra|ro|do|da|to|ta|vo|va|mo|ma|no|na|º|ª|°c|°|%)`;
const SPOKEN_NUMBER = [
  new RegExp(`^${NUM}${UNIT}?$`, 'i'),
  /^\d{1,2}:\d{2}$/,
  new RegExp(`^(?:0|[1-9]\\d{0,3})(?:[.,]\\d+)?-(?:0|[1-9]\\d{0,3})(?:[.,]\\d+)?${UNIT}?$`, 'i'),
  /^\d{1,2}\/\d{1,2}$/,
];
const COORD_WORDS = /^(coordenadas?|latitud|longitud|lat|lon|lng|ubicaci[oó]n|posici[oó]n|gps|coordinates?|latitude|longitude|location)$/i;
// A word's core: without the punctuation around it, but a «.» or «,» right before a digit stays («.25» is not «25»).
const EDGE = /^(?:[«»"'“”‘’()[\]¿¡;:!?…]|[.,](?!\d))+|[«»"'“”‘’()[\]¿¡,;:.!?…]+$/gu;
const core = w => w.replace(EDGE, '');
/** Marks where something was taken off, so a number touching it is dropped whole. */
const CUT = '';
/** Opening punctuation and a sign that may stand before the first group of a number («(3 000», «+5 000», «−10 000»). */
const HEAD = String.raw`^[«"'“‘(\[¿¡:;…]*[-−+]?`;
const decimals = c => (/^[-−+]?\d{1,3}[.,](\d+)°?$/.exec(c)?.[1]?.length ?? 0);

/**
 * The spoken part of an answer. Whatever is taken off becomes a space, so two numbers can never run together; then each
 * word is judged on its own, so a number is kept whole or dropped whole, never in part. Taken off: formatting, emojis,
 * code, links, e-mail addresses, file names, coordinates (pairs of decimals with 4+ places, both signed with 2+, or after
 * «coordenadas», «lat»…; lone signed decimals with 4+ places; degrees with minutes), phone numbers (+593 99 123 4567,
 * 099 123 4567) and every word whose digits do not read as a number (CAM034567, CAM-123, B2, 2026-11-12, 12/11/2026,
 * 0991234567). Thousands grouped with a no-break or thin space, or with a group starting with 0 («10 000»), are joined.
 * Nothing (empty) when a removal leaves a single word («Ver»). Pure.
 */
export function speakable(text, max = 600) {
  let cut = false;
  const gap = () => ((cut = true), CUT);
  const raw = String(text ?? '')
    .replace(/```[\s\S]*?```/g, gap)
    .replace(/`[^`\n]*`/g, gap)
    .replace(/(?:https?:\/\/|www\.)\S*[^\s)\]»".,;:!?]/g, gap)
    .replace(/[\w.+-]+@[\w-]+(?:\.[\w-]+)+/g, gap)
    .replace(/(?<![\w.,])[a-z][\w-]*(?:\.[a-z][\w-]*)*\.[a-z]{2,}\/\S*[^\s.,;:!?)]/gi, gap) // a bare domain with a path
    .replace(/(?<!\S)\S+\.(?:xlsx|xls|csv|pdf|docx?|txt|png|jpe?g|gpx|kml|json|zip)\b/gi, gap) // file names
    .replace(/[\p{Extended_Pictographic}\u{FE0F}\u{200D}]+/gu, ` ${CUT} `) // a word of its own: never glues two numbers
    .replace(/^[^\S\n]*>[^\S\n]+/gm, '') // a quote mark (not «>1000»)
    .replace(/^[^\S\n]*[-•*][^\S\n]+/gm, '') // list markers
    .replace(/^[^\S\n]*\|?[\s|:-]*-{3,}[\s|:-]*$/gm, ' ') // table rules
    // Signs between or before numbers keep their meaning as words: 2~3 → 2-3, 5*120 → 5 × 120, 12|15 → 12, 15, >1000 → > 1000,
    // «machos/15», «Total:12» → two words.
    .replace(/(?<=\d)[~–](?=\d)/g, '-')
    .replace(/(?<![\p{L}\d])–(?=\d)/gu, CUT) // an en dash before a number may be a minus or a pause: the number goes
    .replace(/\s*[–—]\s*/g, ', ')
    .replace(/(?<=\d)(?:[*×]|\s+[*×]\s+)(?=\d)/g, ' × ')
    .replace(/\s*\|\s*/g, ', ')
    .replace(/([<>≤≥=≈])(?=[-−+]?\d)/g, '$1 ')
    .replace(/(?<=\p{L})([:/])(?=\d)|(?<=\d)([:/])(?=\p{L})/gu, '$1$2 ')
    .replace(/#(?=\d)/g, CUT)
    .replace(/[*_~`#]+/g, ` ${CUT} `); // formatting: a word of its own, so it neither glues nor joins numbers
  // Words with the spaces between them (a no-break or thin space is kept as such, to tell grouped thousands).
  const parts = raw.split(/([^\S\n      ]+|\n)/);
  const words = [];
  for (let k = 0; k < parts.length; k += 2) words.push({ w: parts[k], sp: parts[k + 1] ?? '' });
  const drop = k => {
    if (words[k].w) cut = true;
    words[k].w = (/[.!?]$/.test(words[k].w) ? '.' : /[,;:]$/.test(words[k].w) ? ',' : '');
    words[k].done = true;
  };
  // Phone numbers first (their digit groups must not be joined as thousands).
  for (let k = 0; k < words.length; k++) {
    const c = core(words[k].w);
    if (!/\d/.test(c) || words[k].w.includes(CUT)) continue;
    // A phone number: «+593 …» or a number starting with 0, and the digit groups after it.
    // (A group like «000» right after a number is a thousands group, not a phone number.)
    const afterNumber = k > 0 && new RegExp(`${HEAD}\\d[\\d      ]*$`).test(words[k - 1].w) && /^[ \t\u00A0\u202F]$/.test(words[k - 1].sp);
    if (/^\+\d{2,}$/.test(c) || (!afterNumber && /^\(?0\d{1,3}\)?(?:-\d+)*$/.test(words[k].w.replace(/[,;:.]$/, '')))) {
      let n = k;
      while (n + 1 < words.length && !/\n/.test(words[n].sp) && /^\d{1,4}(?:-\d{1,4})*[,;:.]?$/.test(words[n + 1].w) && !/[,;:.]$/.test(words[n].w)) n++;
      const digits = words.slice(k, n + 1).map(x => x.w).join('').replace(/\D/g, '').length;
      if (digits >= (c.startsWith('+') ? 8 : 7)) {
        for (let x = k; x <= n; x++) drop(x);
      }
    }
  }
  for (const x of words) x.grp = /^\d{3}(?!\d)/.test(x.w); // may continue the number before it
  // Thousands grouped with spaces: «10 000», «1 050», «12 000,5», or with no-break/thin spaces inside one word.
  for (let k = 0; k < words.length; k++) {
    const thin = /^[-−+]?[1-9]\d{0,2}(?:[      ]\d{3})+(?:[.,]\d+)?$/.test(core(words[k].w));
    if (thin) {
      words[k].w = words[k].w.replace(/[      ]/g, '');
      words[k].joined = true;
    }
    if (words[k].done) continue;
    if (!new RegExp(`${HEAD}[1-9]\\d{0,2}$`).test(words[k].w) && !(words[k].joined && new RegExp(`${HEAD}[1-9]\\d{3,}$`).test(words[k].w))) continue;
    let n = k;
    // Only while the group so far ends in a digit (a comma or full stop after a group ends the number).
    while (n + 1 < words.length && /\d$/.test(words[n].w) && /^[ \t]$/.test(words[n].sp) && /^\d{3}(?:[.,]\d+)?[«»"'“”)\],;:.!?]*$/.test(words[n + 1].w) && !/[.,]\d/.test(core(words[n].w))) n++;
    if (n > k && (words[k].joined || words.slice(k + 1, n + 1).some(x => x.w.startsWith('0')))) {
      words[k].w = words.slice(k, n + 1).map(x => x.w).join('');
      words[k].sp = words[n].sp;
      words[k].joined = true;
      words.splice(k + 1, n - k);
    }
  }
  const orig = words.map(x => x.w);
  for (let k = 0; k < words.length; k++) {
    const c = core(words[k].w);
    if (words[k].done) continue;
    if (words[k].w.includes(CUT)) {
      if (/\d/.test(words[k].w)) drop(k);
      else (words[k].w = words[k].w.replaceAll(CUT, '')), (words[k].mark = true);
      continue;
    }
    if (!/\d/.test(c)) continue;
    // Coordinates: a pair of decimals.
    const d1 = decimals(c);
    const next = words[k + 1] ? core(words[k + 1].w) : '';
    const d2 = decimals(next);
    if (d1 && d2 && !/[.!?]$/.test(words[k].w)) {
      const signed = /^[-−+]/.test(c) && /^[-−+]/.test(next);
      const context = words.slice(Math.max(0, k - 4), k).some(x => COORD_WORDS.test(core(x.w)));
      const wide = /^[-−+]?\d{2,3}[.,]/.test(c) || /^[-−+]?\d{2,3}[.,]/.test(next); // a longitude (75–81 in Ecuador)
      if (((d1 >= 4 || d2 >= 4) && (wide || /^[-−+]/.test(c) || /^[-−+]/.test(next))) || (signed && d1 >= 2 && d2 >= 2) || context || /°$/.test(c)) {
        drop(k);
        drop(k + 1);
        words[k + 1].done = true;
        for (const x of [k + 2]) if (words[x] && /^[NSEWO][,.;]?$/.test(words[x].w)) drop(x);
        continue;
      }
    }
    if (d1 >= 4 && (/^[-−+]/.test(c) || /°$/.test(c) || words.slice(Math.max(0, k - 4), k).some(x => COORD_WORDS.test(core(x.w))))) {
      drop(k);
      if (words[k + 1] && /^[NSEWO][,.;]?$/.test(words[k + 1].w)) drop(k + 1);
      continue;
    }
    // A number followed by a group with a leading zero («1000 000») is one value we cannot read: both go.
    if (/^[-−+]?\d+$/.test(c) && words[k + 1] && /^[ \t]+$/.test(words[k].sp) && /^0\d/.test(words[k + 1].w)) {
      drop(k);
      drop(k + 1);
      words[k + 1].done = true;
      continue;
    }
    if (/^\d{1,3}(?:\.\d{1,3}){3}$/.test(c) || (!(words[k].joined && /^[-−+]?[1-9]\d{3,11}(?:[.,]\d+)?$/.test(c)) && !SPOKEN_NUMBER.some(r => r.test(c)))) {
      drop(k);
      // A degree value's hemisphere letter («1234°38' N») goes with it.
      if (/°/.test(c) && words[k + 1] && /^[NSEWO][,.;]?$/.test(words[k + 1].w)) drop(k + 1);
    }
  }
  // A word starting with a three-digit group right after a word ending in a digit may be one number with it
  // («1 500-2 000», «900-1 000», «$1 500», «1 500/ha»): when one of the two goes, the other goes too.
  for (let again = true; again; ) {
    again = false;
    for (let k = 1; k < words.length; k++) {
      if (!/^[ \t]$/.test(words[k - 1].sp) || !/\d$/.test(orig[k - 1]) || !words[k].grp) continue;
      if (!!words[k - 1].done === !!words[k].done) continue;
      drop(words[k].done ? k - 1 : k);
      again = true;
    }
  }
  // Two numbers that a removal or a line break now leaves side by side get a comma, so «12 300» is not read as one.
  let out = '';
  let prevDigit = false;
  let between = false;
  for (const x of words) {
    const w = x.w.replaceAll(CUT, '');
    if (/[\p{L}\d]/u.test(w)) {
      if (prevDigit && between && /^[-−+]?\d/.test(w)) out = out.replace(/\s*$/, ', ');
      prevDigit = /\d$/.test(w);
      between = false;
    } else if (x.w.includes(CUT) || x.done || x.mark) between = true;
    out += x.w + x.sp;
    if (/\n/.test(x.sp)) between = true;
  }
  let t = out
    .replace(/\(\s*(?:(?:ver|see|en|link|enlace|mapa|map)\s*:?)?\s*[,;:]?\s*\)/gi, ' ')
    .replaceAll(CUT, ' ')
    .replace(/\s+([,.;:!?])(?!\d)/g, '$1')
    .replace(/([,;:])\s*(?=[,;:.](?!\d))/g, '')
    .replace(/^[\s,;:.]+/, '')
    .replace(/\s+/g, ' ')
    .replace(/\s*[,;:]\s*$/, '')
    .trim();
  if (!/[\p{L}\d]/u.test(t) || (cut && (t.match(/[\p{L}\d]+/gu) ?? []).length < 2)) return '';
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

/**
 * The genera (and words) the voice transcription is told to expect: a hint for the speech model, not a list of what
 * is an Ithomiini (that is frontend/src/lib/ithomiiniGenera.ts; this file is copied alone by `install`, so it keeps
 * its own words). tests/ithomiini-genera.test.mjs checks that every genus here other than Heliconius is in that list.
 */
export const KEYTERMS = [
  'Ithomiini', 'Aeria', 'Athesis', 'Athyrtis', 'Brevioleria', 'Callithomia', 'Ceratinia', 'Dircenna', 'Elzunia', 'Episcada',
  'Epityches', 'Forbestra', 'Godyris', 'Greta', 'Heterosais', 'Hyalenna', 'Hyalyris', 'Hypoleria', 'Hyposcada', 'Hypothyris',
  'Ithomia', 'Mcclungia', 'Mechanitis', 'Melinaea', 'Methona', 'Napeogenes', 'Oleria', 'Olyras', 'Pagyris', 'Patricia',
  'Placidina', 'Pseudoscada', 'Pteronymia', 'Sais', 'Scada', 'Thyridia', 'Tithorea', 'Veladyris', 'Velamysta', 'Heliconius',
  'Ikiam', 'CAM', 'Claude',
];

/** A voice note (WhatsApp's `audio` and push-to-talk `ptt`). */
export const isVoice = type => ['audio', 'ptt'].includes(String(type ?? '').toLowerCase());

const fold = s => String(s ?? '').normalize('NFD').replace(/[̀-ͯ]/g, '').toLowerCase();
/** Claude's name and the ways speech engines mishear it. Not Claudio, Claudia, Clodomiro or claudette: whole words only. */
const CLAUDE_WORDS = new Set(['claude', 'claud', 'klaud', 'clod']);
/** «Cloud» (and, in Franz's own chat, «Claudia») counts only as a call: the first word, or right after greetings («oye cloud, …»), and followed by a comma, ? or ! («Cloud, …», «¿Cloud?»), never as a noun («cloud computing», «ok cloud storage», «a cloud of»). */
const GREETINGS = new Set(['oye', 'oiga', 'hey', 'hola', 'ok', 'okay', 'oki', 'ya', 'bueno', 'eh', 'mira', 'hi', 'hello', 'buenas']);

/**
 * Whether a voice note's transcript calls Claude, so it counts as an @claude mention. `personal` (Franz's own chat):
 * the name anywhere. The group: only among the first `window` words («Claude, …», «oye Claude …»); a note that talks
 * about Claude later («le dije a Ana que Claude…») does not wake it, and one that talks about it early («ayer Claude me
 * dijo…») wakes it but the turn is told to stay [silent] when the note is not for it. «Cloud» only as a call (above). Pure.
 */
export function voiceAddressed(transcript, { personal = false, window = 6 } = {}) {
  const folded = fold(transcript);
  const words = folded.match(/[a-z0-9]+/g) ?? [];
  for (const [i, w] of words.entries()) {
    if (!personal && i >= window) break;
    if (CLAUDE_WORDS.has(w)) return true;
  }
  const lead = words.findIndex(w => !GREETINGS.has(w));
  // Parakeet often hears «Claude» as «Claudia» (13 Nov): in Franz's own chat a leading «Claudia,» is a call too.
  const callWord = words[lead] === 'cloud' || (personal && words[lead] === 'claudia') ? words[lead] : null;
  if (!callWord || (!personal && lead >= window)) return false;
  return new RegExp(`^[^a-z0-9]*(?:[a-z0-9]+[^a-z0-9]+){${lead}}${callWord}\\s*[,?!]`).test(folded);
}

/** A spelling-free key: how a Spanish speaker says the word (h silent, y = i, th = t, ph = f, c/qu/k, ge/gi = je/ji, z = s, v = b, ll = l, no doubles, no plural s). */
const soundKey = w => {
  let s = fold(w).replace(/[^a-z]/g, '');
  s = s.replace(/ph/g, 'f').replace(/th/g, 't').replace(/ch/g, 'X').replace(/h/g, '').replace(/y/g, 'i');
  s = s.replace(/qu(?=[ei])/g, 'k').replace(/c(?=[ei])/g, 's').replace(/c/g, 'k').replace(/g(?=[ei])/g, 'j').replace(/z/g, 's').replace(/v/g, 'b');
  s = s.replace(/(.)\1+/g, '$1').replace(/s$/, '');
  return s;
};
/** Genera that are also common words or names (Patricia, Greta, Sais, Scada, Aeria): never «corrected». */
const GENUS_FIX_SKIP = new Set(['Patricia', 'Greta', 'Sais', 'Scada', 'Aeria', 'Ithomiini', 'Ikiam', 'CAM', 'Claude']);
const GENUS_BY_KEY = new Map(KEYTERMS.filter(g => !GENUS_FIX_SKIP.has(g) && g.length >= 6).map(g => [soundKey(g), g]));

/**
 * Parakeet has no custom vocabulary and writes genera as they sound («hiposcada», «callitomia», «olería»). A word of six
 * or more letters whose sound key is exactly a genus's becomes that genus (its plural «s» kept); anything else is left as
 * it was. Only for Parakeet's text; Scribe gets the genera as keyterms. Pure.
 */
export const fixGenera = text =>
  String(text ?? '').replace(/[\p{L}]{6,}/gu, w => {
    const g = GENUS_BY_KEY.get(soundKey(w));
    if (!g) return w;
    return /s$/i.test(w) && !/s$/i.test(g) ? g + 's' : g;
  });

/**
 * The shared dictation word list (tools/whatsapp/dictation-words.json, the same entries as T3's shared dictionary):
 * `[{ term, aliases: [...] }]`. Several files merge in order (the first spelling of a key wins); a missing or broken file
 * is skipped, so the bridge never fails on it. Person names live only in the untracked `dictation-words.local.json`
 * beside it (this repo is public).
 */
export function loadWords(files) {
  const out = [];
  for (const f of files ?? []) {
    try {
      const j = JSON.parse(readFileSync(f, 'utf8'));
      const list = Array.isArray(j) ? j : j?.words;
      if (Array.isArray(list)) out.push(...list.filter(w => w && typeof w.term === 'string' && w.term.trim()));
    } catch {}
  }
  return out;
}

const WORD_CHAR = '[\\p{L}\\p{M}\\p{N}_]';
/**
 * Applies a word list to a transcript: each alias, and each term in any case, written as a whole word or phrase (case and
 * accents ignored, the longest spelling first) becomes the term. «claudia revisa la hiposcada del cam 234» → «Claude
 * revisa la Hyposcada del CAM 234». Pure.
 */
export function applyWords(text, words) {
  const s = String(text ?? '');
  const lookup = new Map();
  for (const { term, aliases } of words ?? []) {
    for (const spelling of [term, ...(Array.isArray(aliases) ? aliases : [])]) {
      const k = fold(String(spelling ?? '').replace(/\s+/g, ' ').trim());
      if (k && !lookup.has(k)) lookup.set(k, term.trim());
    }
  }
  if (!lookup.size) return s;
  const esc = v => v.replace(/[.*+?^${}()|[\]\\]/g, '\\$&').replace(/ /g, '\\s+');
  const keys = [...lookup.keys()].sort((a, b) => b.length - a.length);
  // Matched on the accent-folded text (same length as the original for precomposed letters), cut from the original.
  const nfc = s.normalize('NFC');
  const folded = [...nfc].map(ch => {
    const f = fold(ch);
    return f.length === ch.length ? f : ch.length === 1 ? f[0] ?? ch : ch;
  }).join('');
  if (folded.length !== nfc.length) return s;
  const re = new RegExp(`(?<!${WORD_CHAR})(?:${keys.map(esc).join('|')})(?!${WORD_CHAR})`, 'gu');
  let out = '';
  let cursor = 0;
  for (const m of folded.matchAll(re)) {
    out += nfc.slice(cursor, m.index) + (lookup.get(m[0].replace(/\s+/g, ' ')) ?? nfc.slice(m.index, m.index + m[0].length));
    cursor = m.index + m[0].length;
  }
  return out + nfc.slice(cursor);
}

/** Parakeet's raw text as the bridge keeps it: sound-alike genera fixed (fixGenera), then the shared word list. Pure. */
export const parakeetText = (raw, words) => applyWords(fixGenera(String(raw ?? '').trim()), words);

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
  [/__weather_forecast$/, 'mirando el pronóstico del clima'],
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
 * Appends the JSON lines whose "id" (or "of" and "engine", for a transcript) is not in the file yet; returns how
 * many it wrote. Self-contained: it is also sent to claudeclaw as source (node -e), so it reaches
 * only for the built-in modules.
 */
export function appendNew(file, text) {
  const fs = process.getBuiltinModule('node:fs');
  const path = process.getBuiltinModule('node:path');
  fs.mkdirSync(path.dirname(file), { recursive: true, mode: 0o750 });
  // A transcript per note and engine: Scribe's upgrade of a Parakeet transcript is a second line, not a duplicate.
  const key = o => (o.t === 'transcript' ? 'T:' + o.of + (o.engine ? ':' + o.engine : '') : 'M:' + o.id);
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
