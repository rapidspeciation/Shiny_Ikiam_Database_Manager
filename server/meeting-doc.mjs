// The weekly 🇪🇨🇬🇧 meeting's minutes document, proposed by the assistant and made by a person.
//
// How the team does it (Nov 2026): on Wednesday the organiser (Patricio; Alex, Kim or Ale when
// he is away) asks the underground group for points, copies «## Meeting-##/##/2026 - TEMPLATE»
// in the Drive folder «Meetings», names the copy «NNN Meeting-DD/MM/YYYY», writes the Agenda as a
// checklist (an owner in brackets after each point; «Follow-ups:» and «Other/various:» in bold),
// and posts a screenshot of it in «Ithomiini Ikiam» with a short English line.
//
// Here the assistant drafts that agenda as a proposal (meeting_doc_propose): nothing is created on
// Drive. An administrator applies it, as any other proposal: by confirming in their chat
// (meeting_doc_apply, only on an explicit «sí, créalo») or with the button of its preview
// (/api/meeting-docs/<id>/preview). Applying copies the template into the configured folder (only
// folders in `folders`, the allowlist), renames it and fills its Agenda; then a picture of the made
// document's first page is written (meeting_doc_picture), for the chat that asked to send.
// Nothing here posts anywhere: the team's «Ithomiini Ikiam» group is never written to (Franz, 4 Nov 2026).
//
// Backends:
// - google: Drive files.copy, then one Docs batchUpdate (docsRequests below), with the project
//   account's token (GOOGLE_DRIVE_CREDENTIALS_FILE, jmithominii: scopes drive and documents; an editor
//   of «Meetings», checked 4 Nov 2026). The picture: the Doc exported as PDF, page 1 by pdftoppm.
// - sim: a folder on disk (the lab, tests). The template's structure is built in; copies are JSON;
//   the picture is the copy's page in headless Chromium (PLAYWRIGHT_MODULE, MAPS_RENDER_CHROMIUM).
//
// Tools: meeting_doc_context (the last minutes, the next number and date), meeting_doc_propose,
// meeting_doc_get, meeting_doc_apply (administrators), meeting_doc_picture.

import { randomUUID } from 'node:crypto';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { existsSync, mkdirSync, readFileSync, readdirSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';

export const TEMPLATE_TITLE = '## Meeting-##/##/2026 - TEMPLATE';
export const TITLE_PLACEHOLDER = 'Meeting-##/##/2026';
const MAX_ITEMS = 40;
const MAX_TEXT = 300;

// ------------------------------------------------------------ dates, numbers, names
const pad = n => String(n).padStart(2, '0');
/** 2026-11-05 → 05/11/2026 (the recent minutes' form). */
export const dmy = iso => {
  const [y, m, d] = String(iso).split('-');
  return `${d}/${m}/${y}`;
};
/** The meeting's day: the next Thursday after `today` (ISO date, Ecuador), or `today` itself when `sameDay`. */
export function nextMeetingDate(today, { weekday = 4, sameDay = false } = {}) {
  const [y, m, d] = String(today).split('-').map(Number);
  const t = new Date(Date.UTC(y, m - 1, d));
  let add = (weekday - t.getUTCDay() + 7) % 7;
  if (add === 0 && !sameDay) add = 7;
  t.setUTCDate(t.getUTCDate() + add);
  return `${t.getUTCFullYear()}-${pad(t.getUTCMonth() + 1)}-${pad(t.getUTCDate())}`;
}
/** 9:00 in Ecuador (UTC-5, no summer time) as UK time on that day: 3:00pm in BST, 2:00pm in GMT. */
export function ukTime(iso, ecHour = 9) {
  const [y, m, d] = String(iso).split('-').map(Number);
  const at = new Date(Date.UTC(y, m - 1, d, ecHour + 5, 0));
  const h = Number(new Intl.DateTimeFormat('en-GB', { timeZone: 'Europe/London', hour: 'numeric', hour12: false }).format(at));
  return `${h > 12 ? h - 12 : h}:00${h >= 12 ? 'pm' : 'am'}`;
}
export const docHeading = iso => `Meeting-${dmy(iso)}`;
export const docTitle = (number, iso) => `${number} Meeting-${dmy(iso)}`;
const TITLE_RE = /^(\d+)\s+Meeting-(\d{1,2})\/(\d{1,2})\/(\d{2,4})\s*$/;
/** «143 Meeting-29/10/2026» → { number: 143, date: '2026-10-29' } (null for another title). */
export function parseTitle(title) {
  const m = TITLE_RE.exec(String(title ?? '').trim());
  if (!m) return null;
  const year = m[4].length === 2 ? 2000 + Number(m[4]) : Number(m[4]);
  return { number: Number(m[1]), date: `${year}-${pad(m[3])}-${pad(m[2])}` };
}

/**
 * The minutes in the documents' copy (the app's KNOWLEDGE_DIR: one Markdown file per Drive document,
 * front matter `title:`, `sourceUrl:`, `driveId:`), newest first, and the template's id.
 */
export function minutesIndex(roots = []) {
  const list = [];
  let template = null;
  const walk = dir => {
    let names = [];
    try {
      names = readdirSync(dir, { withFileTypes: true });
    } catch {
      return;
    }
    for (const e of names) {
      const p = join(dir, e.name);
      if (e.isDirectory()) walk(p);
      else if (e.name.endsWith('.md')) {
        let head = '';
        try {
          head = readFileSync(p, 'utf8').slice(0, 800);
        } catch {
          continue;
        }
        const field = k => new RegExp(`^${k}:\\s*(.+)$`, 'm').exec(head)?.[1]?.trim() ?? null;
        const title = field('title');
        if (title === TEMPLATE_TITLE) template = { id: field('driveId'), title, file: p };
        const parsed = parseTitle(title);
        if (parsed) list.push({ ...parsed, title, id: field('driveId'), url: field('sourceUrl'), file: p, folder: field('folder') });
      }
    }
  };
  for (const r of roots) walk(r);
  list.sort((a, b) => b.number - a.number || b.date.localeCompare(a.date));
  return { minutes: list, template };
}

// ------------------------------------------------------------ the agenda
/** An item: a string, or { text, children }. Returns the clean tree, or throws a message for the assistant. */
function cleanItems(items, depth = 0, where = 'items') {
  if (items == null) return [];
  if (!Array.isArray(items)) throw new Error(`${where}: a list`);
  if (items.length > MAX_ITEMS) throw new Error(`${where}: at most ${MAX_ITEMS} points`);
  return items.map((it, i) => {
    const text = String(typeof it === 'string' ? it : it?.text ?? '').replace(/\s+/g, ' ').trim();
    if (text.length > MAX_TEXT) throw new Error(`${where}[${i}]: at most ${MAX_TEXT} characters`);
    if (!text && !(it?.children?.length)) throw new Error(`${where}[${i}]: empty`);
    const children = depth >= 2 ? [] : cleanItems(it?.children, depth + 1, `${where}[${i}].children`);
    return children.length ? { text, children } : { text };
  });
}
export function cleanAgenda(agenda = {}) {
  const out = { items: cleanItems(agenda.items), followUps: cleanItems(agenda.followUps, 1, 'followUps'), other: cleanItems(agenda.other, 1, 'other') };
  if (!out.items.length && !out.followUps.length && !out.other.length) throw new Error('The agenda has no points.');
  return out;
}
/**
 * The agenda as the template's checklist: one line per point, `depth` 0–3, `bold` for the two section
 * labels. «Follow-ups:» and «Other/various:» are always there, with an empty point under them when
 * they have none (as in the template).
 */
export function agendaLines(agenda) {
  const lines = [];
  const add = (items, depth) => {
    for (const it of items) {
      lines.push({ depth, text: it.text, bold: false });
      if (it.children) add(it.children, depth + 1);
    }
  };
  add(agenda.items, 0);
  for (const [label, items] of [['Follow-ups:', agenda.followUps], ['Other/various:', agenda.other]]) {
    lines.push({ depth: 0, text: label, bold: true });
    if (items.length) add(items, 1);
    else lines.push({ depth: 1, text: '', bold: false });
  }
  return lines;
}
/** `_word_` marks italics (species names, as Patricio writes them): the text without marks and the italic ranges. */
export function italics(text) {
  const ranges = [];
  let plain = '';
  let last = 0;
  for (const m of String(text).matchAll(/(^|[^\w])_([^_\n]+)_(?=$|[^\w])/g)) {
    plain += text.slice(last, m.index) + m[1];
    ranges.push([plain.length, plain.length + m[2].length]);
    plain += m[2];
    last = m.index + m[0].length;
  }
  return { plain: plain + text.slice(last), ranges };
}

/** The WhatsApp message (the organiser's usual line, then the agenda as text; WhatsApp's *bold* and _italic_). */
export function whatsappText({ date, agenda, docUrl = null, intro = true, today = null }) {
  const when = !today ? 'the' : today === date ? "today's" : dayAfter(today) === date ? "tomorrow's" : nextMeetingDate(today) === date ? "this Thursday's" : 'the';
  const head = intro
    ? `Hi everyone!!.. here is a proposed *Agenda* for ${when} 🇪🇨🇬🇧 meeting (${dmy(date)}).. 9:00am 🇪🇨, ${ukTime(date)} 🇬🇧. Let me know if you would like to add or modify something.`
    : null;
  const body = agendaLines(agenda)
    .filter(l => l.text)
    .map(l => `${'    '.repeat(l.depth)}${l.depth === 0 ? '☐' : '◦'} ${l.bold ? `*${l.text}*` : l.text}`);
  return [head, `*${docHeading(date)}*`, ...body, docUrl ? `\n${docUrl}` : null].filter(Boolean).join('\n');
}
function dayAfter(iso) {
  const [y, m, d] = iso.split('-').map(Number);
  const t = new Date(Date.UTC(y, m - 1, d + 1));
  return `${t.getUTCFullYear()}-${pad(t.getUTCMonth() + 1)}-${pad(t.getUTCDate())}`;
}

// ------------------------------------------------------------ the Google Docs edit
/**
 * The Docs batchUpdate that fills a fresh copy of the template (`doc`: documents.get of the copy):
 * the Agenda's checklist (every paragraph between the «Agenda» heading and «Resumen de la reunión»)
 * replaced by the agenda's lines, one checklist, nesting by leading tabs, the section labels in bold,
 * `_species_` in italics; then the title's «Meeting-##/##/2026» replaced by the meeting's date.
 * Requests run in order and each uses the indexes left by the previous one: the delete, insert and
 * styles all sit after the title, and the title's replacement goes last.
 */
export function docsRequests(doc, { date, agenda }) {
  const paras = (doc?.body?.content ?? []).filter(c => c.paragraph).map(c => ({
    start: c.startIndex,
    end: c.endIndex,
    text: (c.paragraph.elements ?? []).map(e => e.textRun?.content ?? '').join(''),
    bullet: c.paragraph.bullet ?? null,
  }));
  const at = paras.findIndex(p => p.text.trim() === 'Agenda');
  const resumen = paras.findIndex((p, i) => i > at && /^Resumen de la reuni[oó]n$/i.test(p.text.trim()));
  if (at < 0 || resumen < 0) throw new Error('The copy has no «Agenda» or «Resumen de la reunión» heading: is it the template?');
  // The checklist: from its first point to the heading. Lines before it (the template's blank line under «Agenda») stay.
  const first = paras.findIndex((p, i) => i > at && i < resumen && p.bullet);
  const body = paras.slice(first < 0 ? at + 1 : first, resumen);
  if (!body.length) throw new Error('No paragraph between «Agenda» and «Resumen de la reunión».');
  // The indents of the template's own checklist levels (36/18 pt, 72/54 pt…), so the new list sits where the old one did.
  const levels = doc?.lists?.[body.find(p => p.bullet)?.bullet?.listId]?.listProperties?.nestingLevels ?? [];
  const indent = depth => {
    const l = levels[depth];
    const pt = (v, d) => ({ magnitude: v?.magnitude ?? d, unit: 'PT' });
    return { indentStart: pt(l?.indentStart, 36 * (depth + 1)), indentFirstLine: pt(l?.indentFirstLine, 36 * (depth + 1) - 18) };
  };
  const start = body[0].start;
  const keepLast = body[body.length - 1].end - 1; // the last paragraph's newline stays: the new text goes in it
  const lines = agendaLines(agenda).map(l => ({ ...l, ...italics(l.text) }));
  let text = '';
  const bold = [];
  const ital = [];
  lines.forEach((l, i) => {
    const prefix = '\t'.repeat(l.depth);
    const from = start + text.length + prefix.length;
    if (l.bold) bold.push([from, from + l.plain.length]);
    for (const [a, b] of l.ranges) ital.push([from + a, from + b]);
    text += prefix + l.plain + (i < lines.length - 1 ? '\n' : '');
  });
  const range = (a, b) => ({ startIndex: a, endIndex: b });
  const requests = [];
  if (keepLast > start) requests.push({ deleteContentRange: { range: range(start, keepLast) } });
  requests.push({ insertText: { location: { index: start }, text } });
  const end = start + text.length;
  requests.push({ updateTextStyle: { range: range(start, end), textStyle: { bold: false, italic: false }, fields: 'bold,italic' } });
  for (const [a, b] of bold) if (b > a) requests.push({ updateTextStyle: { range: range(a, b), textStyle: { bold: true }, fields: 'bold' } });
  for (const [a, b] of ital) requests.push({ updateTextStyle: { range: range(a, b), textStyle: { italic: true }, fields: 'italic' } });
  requests.push({ deleteParagraphBullets: { range: range(start, end + 1) } });
  requests.push({ createParagraphBullets: { range: range(start, end + 1), bulletPreset: 'BULLET_CHECKBOX' } });
  // createParagraphBullets removed the tabs: each line's place without them, and its level's indent.
  let at2 = start;
  for (const l of lines) {
    requests.push({ updateParagraphStyle: { range: range(at2, at2 + l.plain.length + 1), paragraphStyle: indent(l.depth), fields: 'indentStart,indentFirstLine' } });
    at2 += l.plain.length + 1;
  }
  requests.push({ replaceAllText: { containsText: { text: TITLE_PLACEHOLDER, matchCase: true }, replaceText: docHeading(date) } });
  return requests;
}

/** Drive + Docs with a Google token (a function answering an access token). */
export function googleBackend({ token, fetch: fetchFn = fetch }) {
  const call = async (url, { method = 'GET', body } = {}) => {
    const res = await fetchFn(url, {
      method,
      headers: { authorization: `Bearer ${await token()}`, ...(body ? { 'content-type': 'application/json' } : {}) },
      ...(body ? { body: JSON.stringify(body) } : {}),
      signal: AbortSignal.timeout(60_000),
    });
    const out = await res.json().catch(() => ({}));
    if (!res.ok) throw new Error(`Google ${res.status}: ${out?.error?.message ?? 'error'}`);
    return out;
  };
  const files = 'https://www.googleapis.com/drive/v3/files';
  return {
    kind: 'google',
    async findByName(name, folder) {
      const q = `name = '${name.replace(/['\\]/g, '\\$&')}' and '${folder}' in parents and trashed = false`;
      const out = await call(`${files}?${new URLSearchParams({ q, fields: 'files(id,name,webViewLink)', supportsAllDrives: 'true', includeItemsFromAllDrives: 'true' })}`);
      return out.files?.[0] ?? null;
    },
    async copy(templateId, name, folder) {
      return call(`${files}/${encodeURIComponent(templateId)}/copy?supportsAllDrives=true&fields=id,name,webViewLink,parents`, { method: 'POST', body: { name, parents: [folder] } });
    },
    async exportPdf(docId) {
      const res = await fetchFn(`${files}/${encodeURIComponent(docId)}/export?mimeType=application/pdf`, { headers: { authorization: `Bearer ${await token()}` }, signal: AbortSignal.timeout(60_000) });
      if (!res.ok) throw new Error(`Google export ${res.status}`);
      return Buffer.from(await res.arrayBuffer());
    },
    async fill(docId, proposal) {
      const doc = await call(`https://docs.googleapis.com/v1/documents/${encodeURIComponent(docId)}`);
      await call(`https://docs.googleapis.com/v1/documents/${encodeURIComponent(docId)}:batchUpdate`, { method: 'POST', body: { requests: docsRequests(doc, proposal) } });
    },
  };
}

/**
 * The lab's stand-in for Drive and Docs: a folder with `files.json`. The template is there from the
 * start (id `templateId`, the real template's paragraphs, no team content); copy and fill change only
 * that file. A copy's link is the app's page of it (/api/meeting-docs/sim/<id>).
 */
export const TEMPLATE_PARAGRAPHS = [
  { text: TITLE_PLACEHOLDER, style: 'TITLE' },
  { text: 'Agenda', style: 'HEADING_1' },
  { text: '', depth: 0, check: true },
  { text: 'Follow-ups:', depth: 0, check: true, bold: true },
  { text: '', depth: 1, check: true },
  { text: 'Other/various:', depth: 0, check: true, bold: true },
  { text: '', depth: 1, check: true },
  { text: 'Resumen de la reunión', style: 'HEADING_1' },
  { text: '', depth: 0, bullet: true },
  { text: 'Follow-ups:', style: 'HEADING_2' },
  { text: '[Add items here]', depth: 0, bullet: true },
  { text: 'Next week(s)', style: 'HEADING_1' },
  { text: '', depth: 0, bullet: true },
  { text: 'Follow-ups:', depth: 0, bullet: true, bold: true },
];
export function simBackend({ dir, templateId, folders = [], publicUrl = '' }) {
  mkdirSync(dir, { recursive: true, mode: 0o700 });
  const file = join(dir, 'files.json');
  const load = () => (existsSync(file) ? JSON.parse(readFileSync(file, 'utf8')) : {});
  const save = all => writeFileSync(file, JSON.stringify(all, null, 1), { mode: 0o600 });
  const all = load();
  if (!all[templateId]) {
    all[templateId] = { id: templateId, name: TEMPLATE_TITLE, parents: folders.slice(0, 1), paragraphs: TEMPLATE_PARAGRAPHS, createdTime: new Date().toISOString() };
    save(all);
  }
  const link = id => `${String(publicUrl).replace(/\/+$/, '')}/api/meeting-docs/sim/${id}`;
  return {
    kind: 'sim',
    get: id => load()[id] ?? null,
    async findByName(name, folder) {
      const f = Object.values(load()).find(x => x.name === name && x.parents?.includes(folder) && !x.trashed);
      return f ? { id: f.id, name: f.name, webViewLink: link(f.id) } : null;
    },
    async copy(templateId2, name, folder) {
      const files = load();
      const src = files[templateId2];
      if (!src) throw new Error('Template not found');
      const id = `sim-${randomUUID().slice(0, 8)}`;
      files[id] = { id, name, parents: [folder], paragraphs: structuredClone(src.paragraphs), createdTime: new Date().toISOString(), copiedFrom: templateId2 };
      save(files);
      return { id, name, webViewLink: link(id), parents: [folder] };
    },
    async fill(docId, proposal) {
      const files = load();
      const f = files[docId];
      const at = f.paragraphs.findIndex(p => p.text === 'Agenda' && p.style);
      const resumen = f.paragraphs.findIndex(p => /^Resumen de la reuni/.test(p.text));
      if (at < 0 || resumen < 0) throw new Error('The copy has no «Agenda» or «Resumen de la reunión» heading');
      const lines = agendaLines(proposal.agenda).map(l => ({ text: l.text, depth: l.depth, check: true, ...(l.bold ? { bold: true } : {}) }));
      f.paragraphs.splice(at + 1, resumen - at - 1, ...lines);
      for (const p of f.paragraphs) if (p.text === TITLE_PLACEHOLDER) p.text = docHeading(proposal.date);
      save(files);
    },
  };
}

// ------------------------------------------------------------ the picture
const run = promisify(execFile);
/** A PDF's first page as PNG (pdftoppm, poppler-utils). */
export async function pdfFirstPage(pdf, out, { dpi = 110, bin = 'pdftoppm' } = {}) {
  const base = out.replace(/\.png$/, '');
  writeFileSync(`${base}.pdf`, pdf, { mode: 0o600 });
  await run(bin, ['-png', '-r', String(dpi), '-f', '1', '-l', '1', '-singlefile', `${base}.pdf`, base], { timeout: 60_000 });
  return `${base}.png`;
}
/** An HTML page's `.paper` as PNG, in headless Chromium (Playwright from `module`). */
export async function htmlPicture(html, out, { module = process.env.PLAYWRIGHT_MODULE, executablePath = process.env.MAPS_RENDER_CHROMIUM } = {}) {
  if (!module) throw new Error('No browser for pictures (PLAYWRIGHT_MODULE)');
  const mod = await import(module);
  const chromium = mod.chromium ?? mod.default?.chromium;
  const browser = await chromium.launch({ headless: true, ...(executablePath ? { executablePath } : {}) });
  try {
    const page = await browser.newPage({ viewport: { width: 860, height: 900 }, deviceScaleFactor: 2 });
    await page.setContent(html, { waitUntil: 'load' });
    await page.locator('.paper').screenshot({ path: out });
    return out;
  } finally {
    await browser.close();
  }
}

// ------------------------------------------------------------ pages
const esc = s => String(s ?? '').replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[c]);
const withItalics = s => {
  const { plain, ranges } = italics(s);
  let out = '';
  let last = 0;
  for (const [a, b] of ranges) (out += esc(plain.slice(last, a)) + `<i>${esc(plain.slice(a, b))}</i>`), (last = b);
  return out + esc(plain.slice(last));
};
/** The document's top as Google Docs shows it (title, «Agenda», the checklist): the preview's paper, and the picture for WhatsApp. */
export function paperHtml({ date, agenda }) {
  const rows = agendaLines(agenda)
    .map(l => `<li style="margin-left:${l.depth * 1.6 + 0.6}em"><span class="box"></span><span${l.bold ? ' class="b"' : ''}>${withItalics(l.text) || '&nbsp;'}</span></li>`)
    .join('');
  return `<div class="paper"><h1 class="t">${esc(docHeading(date))}</h1><h2>Agenda</h2><ul class="cl">${rows}</ul></div>`;
}
const PAPER_CSS = `
.paper{background:#fff;color:#000;font-family:Arial,Helvetica,sans-serif;padding:28px 36px 36px;max-width:760px;box-sizing:border-box}
.paper .t{font-weight:400;font-size:30px;text-align:center;margin:0 0 26px}
.paper h2{font-weight:400;font-size:24px;margin:0 0 14px}
.paper .cl{list-style:none;padding:0;margin:0;font-size:16px;line-height:1.55}
.paper .cl li{display:flex;gap:.55em;align-items:flex-start}
.paper .box{flex:none;width:.85em;height:.85em;border:1.5px solid #555;border-radius:2px;margin-top:.3em}
.paper .b{font-weight:700}`;
/** The picture's page (a screenshot of it is what goes to WhatsApp when a picture is wanted). */
export function paperPage(proposal) {
  return `<!doctype html><html><head><meta charset="utf-8"><title>${esc(docHeading(proposal.date))}</title><style>body{margin:0;background:#fff}${PAPER_CSS}</style></head><body>${paperHtml(proposal)}</body></html>`;
}
/** The proposal's page in the app: what apply will make, the paper, the sources, Apply / Discard for those allowed. */
export function previewPage(p, { canApply = false, basePath = '' } = {}) {
  const status =
    p.status === 'applied'
      ? `Aplicada ${esc(p.applied_at?.slice(0, 16).replace('T', ' '))} por ${esc(p.applied_by)}: <a href="${esc(p.doc_link)}" target="_blank" rel="noopener">${esc(p.title)}</a>`
      : p.status === 'discarded'
        ? `Descartada ${esc(p.closed_at?.slice(0, 16).replace('T', ' '))} por ${esc(p.closed_by)}`
        : p.status === 'failed'
          ? `No se pudo aplicar: ${esc(p.error)}`
          : canApply
            ? 'Pendiente: nada se ha creado en Drive todavía.'
            : 'Pendiente: nada se ha creado en Drive. Solo un administrador puede aplicarla (confirmando en su chat o aquí).';
  const sources = (p.sources ?? []).map(s => `<li>${esc(s)}</li>`).join('');
  const buttons = p.status === 'pending' && canApply
    ? `<div class="act"><button id="apply">Crear el documento</button><button id="discard" class="ghost">Descartar</button><span id="msg"></span></div>`
    : '';
  return `<!doctype html><html lang="es"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Propuesta: ${esc(p.title)}</title>
<style>
:root{--bg:#f3f4f2;--ink:#1d2321;--muted:#5d6764;--line:#d5dad7;--accent:#1f6f5c;--card:#fff}
@media (prefers-color-scheme: dark){:root{--bg:#161a19;--ink:#e6ebe9;--muted:#9aa5a1;--line:#2f3735;--accent:#5cc2a6;--card:#1f2523}}
body{margin:0;background:var(--bg);color:var(--ink);font:15px/1.5 system-ui,sans-serif}
main{max-width:840px;margin:0 auto;padding:20px 16px 48px}
h1{font-size:20px;margin:0 0 4px}.sub{color:var(--muted);margin:0 0 16px}
.box2{background:var(--card);border:1px solid var(--line);border-radius:8px;padding:12px 14px;margin:0 0 16px}
.box2 dt{color:var(--muted);font-size:13px}.box2 dd{margin:0 0 8px}
.state{font-weight:600}
.act{display:flex;gap:10px;align-items:center;margin:14px 0}
button{font:inherit;padding:8px 14px;border-radius:6px;border:1px solid var(--accent);background:var(--accent);color:#fff;cursor:pointer}
button.ghost{background:transparent;color:var(--accent)}
.wrap{overflow-x:auto;border:1px solid var(--line);border-radius:8px;background:#fff}.wrap .paper{max-width:none}
details{margin-top:16px}#msg{color:var(--muted)}
${PAPER_CSS}
</style></head><body><main>
<h1>Propuesta: crear «${esc(p.title)}»</h1>
<p class="sub">Propuesta por ${esc(p.created_by)} (${esc(p.created_at?.slice(0, 16).replace('T', ' '))}), revisión ${p.revision}.</p>
<div class="box2"><p class="state">${status}</p><dl>
<dt>Al aplicar</dt><dd>Se copia «${esc(TEMPLATE_TITLE)}» en la carpeta «${esc(p.folder_name || p.folder_id)}», se llama «${esc(p.title)}» y se llena su Agenda con estos puntos. El resto del documento queda como en la plantilla.</dd>
${p.note ? `<dt>Nota</dt><dd>${esc(p.note)}</dd>` : ''}</dl>${buttons}</div>
<div class="wrap">${paperHtml(p)}</div>
${sources ? `<details><summary>De dónde salen los puntos</summary><ul>${sources}</ul></details>` : ''}
</main>
${buttons ? `<script>
const base=${JSON.stringify(basePath)};
async function go(what){const m=document.getElementById('msg');m.textContent='…';
try{const b=await (await fetch(base+'/api/bootstrap',{credentials:'same-origin'})).json();
const r=await fetch(base+'/api/meeting-docs/${esc(p.id)}/'+what,{method:'POST',credentials:'same-origin',headers:{'content-type':'application/json','x-csrf-token':b.csrf},body:'{}'});
const o=await r.json();if(!r.ok)throw new Error(o.error?.message||o.message||r.status);location.reload();}catch(e){m.textContent='Error: '+e.message;}}
document.getElementById('apply').onclick=()=>go('apply');document.getElementById('discard').onclick=()=>go('discard');
</script>` : ''}</body></html>`;
}
/** A simulated document's page (the lab's stand-in for Google Docs). */
export function simDocPage(f) {
  const rows = f.paragraphs
    .map(p =>
      p.style === 'TITLE'
        ? `<h1 class="t">${esc(p.text)}</h1>`
        : p.style
          ? `<h2>${esc(p.text)}</h2>`
          : `<li style="margin-left:${(p.depth ?? 0) * 1.6 + 0.6}em">${p.check ? '<span class="box"></span>' : '•'}<span${p.bold ? ' class="b"' : ''}>${withItalics(p.text) || '&nbsp;'}</span></li>`,
    )
    .join('');
  return `<!doctype html><html><head><meta charset="utf-8"><title>${esc(f.name)}</title><style>body{margin:0;background:#e9ecea}${PAPER_CSS}.paper{margin:16px auto}</style></head><body><div class="paper"><p style="color:#777;font-size:12px;margin:0 0 12px">Documento simulado (laboratorio) · ${esc(f.name)}</p><ul class="cl">${rows}</ul></div></body></html>`;
}

// ------------------------------------------------------------ the tools
export const MEETING_DOC_TOOLS = [
  {
    type: 'function',
    function: {
      name: 'meeting_doc_context',
      description: [
        'Read-only. The weekly 🇪🇨🇬🇧 meeting minutes as the app knows them: the last minutes (`last`: title, number, date, link, file in the documents copy), the one before, the next meeting\'s number, date (next Thursday, 9:00 Ecuador) and UK time, the document title it would get, whether the template is known, and this meeting\'s pending or applied proposal if any.',
        'Use it before meeting_doc_propose (skill `meeting-agenda`).',
      ].join('\n'),
      parameters: { type: 'object', properties: { date: { type: 'string', description: 'The meeting date (YYYY-MM-DD) when it is not next Thursday.' } } },
    },
  },
  {
    type: 'function',
    function: {
      name: 'meeting_doc_propose',
      description: [
        "Proposes the next meeting's minutes document: a copy of the minutes template named «NNN Meeting-DD/MM/YYYY» with this Agenda. Nothing is created on Drive: an administrator or meeting organiser applies it from its preview (`link`).",
        'Called again for the same date, it revises the pending proposal (revision + 1). Returns `link` (the preview), `title`, `whatsapp` (the agenda as a WhatsApp message, in the organiser\'s usual words), `status`.',
        '- agenda.items: the points, each «Topic (Owner)» as in the minutes (English), or { text, children } for sub-points (two levels at most). `_Methona_` puts a name in italics.',
        '- agenda.followUps: points carried over from the last minutes («Next week(s)», open questions). agenda.other: announcements and the rest («Other/various:»).',
        '- sources: one short line per point on where it comes from (a WhatsApp message with date and sender, a minutes line, an alert).',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          date: { type: 'string', description: 'YYYY-MM-DD; default the next Thursday' },
          number: { type: 'integer', description: 'Default: the last minutes number + 1' },
          agenda: {
            type: 'object',
            properties: {
              items: { type: 'array', items: {} },
              followUps: { type: 'array', items: {} },
              other: { type: 'array', items: {} },
            },
            required: ['items'],
          },
          sources: { type: 'array', items: { type: 'string' } },
          note: { type: 'string', description: 'One line for the person who applies it' },
        },
        required: ['agenda'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'meeting_doc_get',
      description: "A meeting document proposal: status (pending, applied: the document's link, discarded), agenda, `whatsapp` text. Without id: this meeting's latest.",
      parameters: { type: 'object', properties: { id: { type: 'string' }, date: { type: 'string' } } },
    },
  },
  {
    type: 'function',
    function: {
      name: 'meeting_doc_apply',
      description: [
        "Creates the meeting's document from a pending proposal (copy of the template in «Meetings», named and filled), only when the person's latest message explicitly approves it ('sí, créalo', 'aplícalo') and only for an administrator.",
        'Returns the document `docLink` and `picture` (a PNG of its first page) to send to the chat that asked: [images] <picture> | <title> [/images].',
      ].join('\n'),
      parameters: { type: 'object', properties: { id: { type: 'string' } }, required: ['id'] },
    },
  },
  {
    type: 'function',
    function: {
      name: 'meeting_doc_picture',
      description: 'A PNG of the first page of an applied proposal\'s document, made again (after someone edited it): `picture`, a file path to send with [images].',
      parameters: { type: 'object', properties: { id: { type: 'string' } }, required: ['id'] },
    },
  },
];
export const MEETING_DOC_TOOL_NAMES = new Set(MEETING_DOC_TOOLS.map(t => t.function.name));

/**
 * The meeting documents of an app: `db` (node:sqlite), `config`: { templateId, folderId, folderName,
 * folders (allowlist; default [folderId]), knowledgeRoots, publicUrl, basePath, pictureDir, today() },
 * `backend` (googleBackend or simBackend), `picture` (for tests: (backend, docId, proposal, out) → path).
 */
export function createMeetingDocs({ db, config = {}, backend = null, picture: makePicture = null }) {
  db.exec(`CREATE TABLE IF NOT EXISTS meeting_doc_proposals (
    id TEXT PRIMARY KEY, created_at TEXT NOT NULL, created_by TEXT NOT NULL, updated_at TEXT NOT NULL,
    revision INTEGER NOT NULL DEFAULT 1, status TEXT NOT NULL, meeting_date TEXT NOT NULL, number INTEGER NOT NULL,
    title TEXT NOT NULL, template_id TEXT, folder_id TEXT, agenda TEXT NOT NULL, sources TEXT, note TEXT,
    applied_at TEXT, applied_by TEXT, doc_id TEXT, doc_link TEXT, closed_at TEXT, closed_by TEXT, error TEXT, picture TEXT)`);
  const now = () => new Date().toISOString();
  const today = config.today ?? (() => new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' }).format(new Date()));
  const folders = new Set((config.folders?.length ? config.folders : [config.folderId]).filter(Boolean));
  const base = String(config.publicUrl || '').replace(/\/+$/, '');
  const link = id => `${base}/api/meeting-docs/${id}/preview`;
  const row = r => r && { ...r, agenda: JSON.parse(r.agenda), sources: r.sources ? JSON.parse(r.sources) : [], folder_name: config.folderName ?? null, date: r.meeting_date };
  const get = id => row(db.prepare('SELECT * FROM meeting_doc_proposals WHERE id = ?').get(String(id)));
  const latestFor = date => row(db.prepare("SELECT * FROM meeting_doc_proposals WHERE meeting_date = ? ORDER BY CASE status WHEN 'applied' THEN 0 WHEN 'pending' THEN 1 ELSE 2 END, updated_at DESC LIMIT 1").get(date));
  const index = () => minutesIndex(config.knowledgeRoots ?? []);
  // Administrators only (Franz, 4 Nov 2026): by their confirmation in chat or the preview's button.
  const canApply = user => user?.role === 'admin';
  const view = p => p && {
    id: p.id,
    status: p.status,
    title: p.title,
    date: p.meeting_date,
    revision: p.revision,
    link: link(p.id),
    agenda: p.agenda,
    ...(p.doc_link ? { docLink: p.doc_link } : {}),
    ...(p.picture ? { picture: p.picture } : {}),
    ...(p.status === 'applied' ? { applied: { at: p.applied_at, by: p.applied_by } } : {}),
    ...(p.status === 'discarded' ? { discarded: { at: p.closed_at, by: p.closed_by } } : {}),
    whatsapp: whatsappText({ date: p.meeting_date, agenda: p.agenda, docUrl: p.doc_link, today: today() }),
  };

  function context(args = {}) {
    const { minutes, template } = index();
    const date = args.date && /^\d{4}-\d{2}-\d{2}$/.test(args.date) ? args.date : nextMeetingDate(today());
    const before = minutes.filter(m => m.date < date);
    const last = before[0] ?? null;
    const number = (last?.number ?? 0) + 1;
    const same = minutes.find(m => m.date === date) ?? null;
    return {
      today: today(),
      meeting: { date, number, title: docTitle(number, date), ecTime: '9:00am', ukTime: ukTime(date) },
      ...(same ? { alreadyExists: { title: same.title, url: same.url } } : {}),
      last: last && { title: last.title, date: last.date, url: last.url, file: last.file },
      previous: before[1] && { title: before[1].title, date: before[1].date, url: before[1].url, file: before[1].file },
      template: template ? { title: template.title, id: template.id } : null,
      proposal: view(latestFor(date)),
      configured: Boolean(backend && config.templateId && config.folderId),
    };
  }

  function propose(args, user) {
    const ctx = context(args);
    if (ctx.alreadyExists) return { error: `The minutes for ${dmy(ctx.meeting.date)} exist already: ${ctx.alreadyExists.title} (${ctx.alreadyExists.url}). Suggest the points as text instead.` };
    let agenda;
    try {
      agenda = cleanAgenda(args.agenda);
    } catch (e) {
      return { error: e.message };
    }
    const number = Number.isInteger(args.number) && args.number > 0 ? args.number : ctx.meeting.number;
    const title = docTitle(number, ctx.meeting.date);
    const sources = (Array.isArray(args.sources) ? args.sources : []).slice(0, 60).map(s => String(s).slice(0, 300));
    const note = args.note ? String(args.note).slice(0, 300) : null;
    const open = db.prepare("SELECT * FROM meeting_doc_proposals WHERE meeting_date = ? AND status = 'pending' ORDER BY updated_at DESC LIMIT 1").get(ctx.meeting.date);
    const applied = db.prepare("SELECT id FROM meeting_doc_proposals WHERE meeting_date = ? AND status = 'applied'").get(ctx.meeting.date);
    if (applied) return { error: 'This meeting\'s document was made already from a proposal; edit it in Google Docs.', ...view(get(applied.id)) };
    if (open) {
      db.prepare('UPDATE meeting_doc_proposals SET updated_at = ?, revision = revision + 1, number = ?, title = ?, agenda = ?, sources = ?, note = ? WHERE id = ?').run(
        now(), number, title, JSON.stringify(agenda), JSON.stringify(sources), note, open.id,
      );
      return { revised: true, ...view(get(open.id)) };
    }
    const id = `md-${randomUUID().slice(0, 8)}`;
    db.prepare(
      'INSERT INTO meeting_doc_proposals (id, created_at, created_by, updated_at, status, meeting_date, number, title, template_id, folder_id, agenda, sources, note) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)',
    ).run(id, now(), user?.username ?? 'assistant', now(), 'pending', ctx.meeting.date, number, title, config.templateId ?? null, config.folderId ?? null, JSON.stringify(agenda), JSON.stringify(sources), note);
    return { created: true, ...view(get(id)) };
  }

  async function apply(id, user) {
    if (!canApply(user)) throw Object.assign(new Error('Only an administrator can create the document'), { status: 403, code: 'FORBIDDEN' });
    const p = get(id);
    if (!p) throw Object.assign(new Error('No such proposal'), { status: 404, code: 'NOT_FOUND' });
    if (p.status !== 'pending') throw Object.assign(new Error(`The proposal is ${p.status}`), { status: 409, code: 'NOT_PENDING' });
    if (!backend || !p.template_id || !p.folder_id) throw Object.assign(new Error('Meeting documents are not set up (template, folder, Drive access)'), { status: 503, code: 'NOT_CONFIGURED' });
    if (!folders.has(p.folder_id)) throw Object.assign(new Error('That folder is not one the app may write in'), { status: 403, code: 'FOLDER_NOT_ALLOWED' });
    const claim = db.prepare("UPDATE meeting_doc_proposals SET status = 'applying', updated_at = ? WHERE id = ? AND status = 'pending'").run(now(), p.id);
    if (!claim.changes) throw Object.assign(new Error('Someone is applying it already'), { status: 409, code: 'NOT_PENDING' });
    try {
      const existing = await backend.findByName(p.title, p.folder_id);
      if (existing) throw new Error(`«${p.title}» exists already in the folder: ${existing.webViewLink ?? existing.id}`);
      const made = await backend.copy(p.template_id, p.title, p.folder_id);
      await backend.fill(made.id, p);
      const docLink = made.webViewLink ?? `https://docs.google.com/document/d/${made.id}/edit`;
      db.prepare("UPDATE meeting_doc_proposals SET status = 'applied', applied_at = ?, applied_by = ?, doc_id = ?, doc_link = ?, updated_at = ?, error = NULL WHERE id = ?").run(
        now(), user.username, made.id, docLink, now(), p.id,
      );
      const out = get(p.id);
      try {
        await picture(out);
      } catch (e) {
        // The document is made; only its picture failed (said in the answer, meeting_doc_picture tries again).
        return { ...view(get(p.id)), pictureError: String(e.message).slice(0, 200) };
      }
      return view(get(p.id));
    } catch (e) {
      // Back to pending, with the reason: a copy made before a failed fill is named in the error (its link), for a person to bin or finish.
      db.prepare("UPDATE meeting_doc_proposals SET status = 'pending', error = ?, updated_at = ? WHERE id = ?").run(String(e.message).slice(0, 400), now(), p.id);
      throw Object.assign(new Error(e.message), { status: 502, code: 'APPLY_FAILED' });
    }
  }

  /** The made document's first page as PNG in pictureDir; its path is kept on the proposal. */
  async function picture(p) {
    if (!p?.doc_id) throw new Error('Not applied yet');
    const dir = config.pictureDir;
    if (!dir) throw new Error('No folder for pictures (pictureDir)');
    mkdirSync(dir, { recursive: true, mode: 0o700 });
    const out = join(dir, `${p.id}-${Date.now()}.png`);
    let path;
    if (makePicture) path = await makePicture(backend, p.doc_id, p, out);
    else if (backend.kind === 'google') path = await pdfFirstPage(await backend.exportPdf(p.doc_id), out);
    else if (backend.kind === 'sim') path = await htmlPicture(simDocPage(backend.get(p.doc_id)), out);
    else throw new Error('No picture for this backend');
    db.prepare('UPDATE meeting_doc_proposals SET picture = ? WHERE id = ?').run(path, p.id);
    return path;
  }

  function discard(id, user) {
    if (!canApply(user) && get(id)?.created_by !== user?.username) throw Object.assign(new Error('Not allowed'), { status: 403, code: 'FORBIDDEN' });
    const r = db.prepare("UPDATE meeting_doc_proposals SET status = 'discarded', closed_at = ?, closed_by = ?, updated_at = ? WHERE id = ? AND status = 'pending'").run(now(), user.username, now(), String(id));
    if (!r.changes) throw Object.assign(new Error('Not pending'), { status: 409, code: 'NOT_PENDING' });
    return view(get(id));
  }

  async function run(name, args = {}, user) {
    if (name === 'meeting_doc_context') return context(args);
    if (name === 'meeting_doc_propose') {
      if (!user || !['editor', 'reviewer', 'admin'].includes(user.role)) return { error: 'Only editors can propose a meeting document.' };
      return propose(args, user);
    }
    if (name === 'meeting_doc_apply') {
      if (!canApply(user)) return { error: 'Only an administrator can create the meeting document; ask one to confirm it in their chat or with the preview button.' };
      try {
        return await apply(String(args.id ?? ''), user);
      } catch (e) {
        return { error: e.message };
      }
    }
    if (name === 'meeting_doc_picture') {
      const p = get(String(args.id ?? ''));
      if (!p || p.status !== 'applied') return { error: 'Not an applied proposal' };
      try {
        return { picture: await picture(p), title: p.title, docLink: p.doc_link };
      } catch (e) {
        return { error: e.message };
      }
    }
    if (name === 'meeting_doc_get') {
      const p = args.id ? get(args.id) : latestFor(args.date && /^\d{4}-\d{2}-\d{2}$/.test(args.date) ? args.date : nextMeetingDate(today()));
      return p ? view(p) : { error: 'No proposal' };
    }
    return { error: `Unknown tool ${name}` };
  }

  /** HTTP: GET /api/meeting-docs/<id>/preview (HTML), …/paper (the picture's HTML), GET /api/meeting-docs/<id> (JSON), POST …/apply, …/discard; GET /api/meeting-docs/sim/<docId> (lab). */
  async function route({ method, path, user, res, sendHtml, json }) {
    let m = /^\/api\/meeting-docs\/sim\/([\w-]+)$/.exec(path);
    if (m && method === 'GET') {
      const f = backend?.kind === 'sim' ? backend.get(m[1]) : null;
      if (!f) return json(res, 404, { error: { code: 'NOT_FOUND', message: 'No such document' } }), true;
      return sendHtml(res, simDocPage(f)), true;
    }
    m = /^\/api\/meeting-docs\/([\w-]+)(?:\/(preview|paper|apply|discard))?$/.exec(path);
    if (!m) return false;
    const [, id, what] = m;
    if (method === 'GET' && (what === 'preview' || what === 'paper' || !what)) {
      const p = get(id);
      if (!p) return json(res, 404, { error: { code: 'NOT_FOUND', message: 'No such proposal' } }), true;
      if (what === 'preview') return sendHtml(res, previewPage(p, { canApply: canApply(user), basePath: config.basePath ?? '' })), true;
      if (what === 'paper') return sendHtml(res, paperPage(p)), true;
      return json(res, 200, view(p)), true;
    }
    if (method === 'POST' && what === 'apply') return json(res, 200, await apply(id, user)), true;
    if (method === 'POST' && what === 'discard') return json(res, 200, discard(id, user)), true;
    return false;
  }

  return { run, route, context, propose, apply, discard, get, canApply, picture };
}

/** The settings from the environment (server/index.mjs). */
export function meetingDocsConfig(env = process.env) {
  const list = v => String(v ?? '').split(',').map(s => s.trim()).filter(Boolean);
  return {
    templateId: env.MEETING_DOC_TEMPLATE_ID || null,
    folderId: env.MEETING_DOC_FOLDER_ID || null,
    folderName: env.MEETING_DOC_FOLDER_NAME || 'Meetings',
    folders: list(env.MEETING_DOC_FOLDERS),
    pictureDir: env.MEETING_DOC_PICTURE_DIR || null,
    simDir: env.MEETING_DOC_SIM_DIR || null,
    google: env.MEETING_DOC_GOOGLE === '1',
  };
}
