// Project documents for the assistant: the hand-copied meeting notes at the top
// of KNOWLEDGE_DIR and the Drive mirror in KNOWLEDGE_DIR/drive (scripts/drive-sync.mjs).
// An in-memory BM25 index over ~1500-character chunks, rebuilt only when a file
// changes (checked with stat, at most every few seconds).
import { createHash } from 'node:crypto';
import { readFile, readdir, stat } from 'node:fs/promises';
import { basename, extname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const here = fileURLToPath(new URL('.', import.meta.url));
const clip = (value, length = 1200) => String(value ?? '').slice(0, length);

export const DOC_KINDS = ['meeting', 'protocol', 'presentation', 'report', 'document', 'transcript'];
const MAX_FILE_BYTES = 600_000;
const MAX_FILES_PER_DIR = 3000;
const CHUNK = 1500;
const CHECK_MS = 3000;
const DRIVE_ID = /^[A-Za-z0-9_-]{25,}$/;

/** Lowercase without accents: "Reunión" and "reunion" are the same word. */
export const fold = text =>
  String(text ?? '')
    .normalize('NFD')
    .replace(/\p{M}+/gu, '')
    .toLowerCase();

const STOP = new Set(
  (
    'de la el en y a los las del que se por con para un una es al lo su no o como mas pero sus le ya fue este esta son ' +
    'entre cuando muy sin sobre tambien me hasta hay donde han quien desde todo nos durante todos uno les ni contra otros ' +
    'ese eso ante ellos e esto mi antes algunos unos yo otro otras otra el tanto esa estos mucho nada cual poco ella estas ' +
    'the of and to in is for on with at by from an be this that are was it as or not have has but we they'
  ).split(' '),
);

/** Search terms of a text: accent-folded words, stop words out, a final plural "s" dropped. */
export function terms(text) {
  const out = [];
  for (const word of fold(text).split(/[^\p{L}\p{N}]+/u)) {
    if (word.length < 2 || STOP.has(word)) continue;
    out.push(word.length > 3 && word.endsWith('s') && !/\d/.test(word) ? word.slice(0, -1) : word);
  }
  return out;
}

const MONTHS = {
  jan: 1, ene: 1, feb: 2, mar: 3, apr: 4, abr: 4, may: 5, jun: 6, jul: 7, aug: 8, ago: 8,
  sep: 9, set: 9, oct: 10, nov: 11, dec: 12, dic: 12,
};
const iso = (y, m, d) => {
  const year = y < 100 ? 2000 + y : y;
  if (m < 1 || m > 12 || d < 1 || d > 31 || year < 1990 || year > 2100) return null;
  const date = new Date(Date.UTC(year, m - 1, d));
  if (date.getUTCDate() !== d) return null;
  return date.toISOString().slice(0, 10);
};

/**
 * The date in a title, day first: "137 Meeting-10/09/2026" → 2026-09-10; also
 * 10-09-26, 2026-09-10, "10 de septiembre de 2026", "Sept 10, 2026".
 */
export function dateFromTitle(title) {
  const text = fold(title);
  let m = /(?:^|\D)(\d{4})[-_.](\d{1,2})[-_.](\d{1,2})(?!\d)/.exec(text);
  if (m) return iso(+m[1], +m[2], +m[3]);
  m = /(?:^|\D)(\d{1,2})\s*[/.-]\s*(\d{1,2})\s*[/.-]\s*(\d{4}|\d{2})(?!\d)/.exec(text);
  if (m) return iso(+m[3], +m[2], +m[1]);
  m = /(?:^|\D)(\d{1,2})(?:\s+de)?\s+([a-z]{3,})\.?(?:\s+de)?,?\s+(\d{4})(?!\d)/.exec(text);
  if (m && MONTHS[m[2].slice(0, 3)]) return iso(+m[3], MONTHS[m[2].slice(0, 3)], +m[1]);
  m = /(?:^|[^a-z])([a-z]{3,})\.?\s+(\d{1,2}),?\s+(\d{4})(?!\d)/.exec(text);
  if (m && MONTHS[m[1].slice(0, 3)]) return iso(+m[3], MONTHS[m[1].slice(0, 3)], +m[2]);
  return null;
}

/** A date given by a person or a model: YYYY-MM-DD or dd/mm/yyyy. */
export function parseDay(value) {
  const text = String(value ?? '').trim();
  if (!text) return null;
  if (/^\d{4}-\d{2}-\d{2}$/.test(text)) return text;
  return dateFromTitle(text);
}

/** Front matter (`key: value` lines between ---) and the text after it. */
export function frontMatter(text) {
  const front = /^---\r?\n([\s\S]*?)\r?\n---\r?\n?/.exec(text);
  const meta = {};
  for (const line of (front?.[1] ?? '').split(/\r?\n/)) {
    const m = /^([A-Za-z][\w-]*):\s*(.*)$/.exec(line);
    if (!m) continue;
    let value = m[2].trim();
    if (/^".*"$/.test(value)) {
      try {
        value = JSON.parse(value);
      } catch {
        value = value.slice(1, -1);
      }
    } else value = value.replace(/^'|'$/g, '');
    meta[m[1]] = value;
  }
  return { meta, body: front ? text.slice(front[0].length) : text };
}

const driveIdOf = (meta, name) =>
  (DRIVE_ID.test(meta.driveId ?? '') && meta.driveId) ||
  /^([A-Za-z0-9_-]{25,})\.(md|txt)$/.exec(name)?.[1] ||
  /\/d\/([A-Za-z0-9_-]{25,})/.exec(meta.sourceUrl ?? '')?.[1] ||
  null;

const driveUrl = id => `https://docs.google.com/document/d/${id}/edit`;

/** Splits a text into ~1500-character pieces at headings and slides, then at paragraphs. */
export function chunkText(text, size = CHUNK) {
  const sections = [];
  const heading = /^#{1,6}\s+.*$/gm;
  let last = 0,
    m;
  while ((m = heading.exec(text))) {
    if (m.index > last) sections.push([last, m.index]);
    last = m.index;
  }
  sections.push([last, text.length]);
  const chunks = [];
  let start = null,
    end = 0;
  const flush = () => {
    if (start !== null && text.slice(start, end).trim()) chunks.push({ start, end });
    start = null;
  };
  for (const [a, b] of sections) {
    // Small sections join the previous piece while it stays under the size.
    if (start !== null && b - start <= size) {
      end = b;
      continue;
    }
    flush();
    let from = a;
    while (b - from > size) {
      // Cut at the last paragraph (or line, or space) before the size.
      const window = text.slice(from, from + size);
      let cut = window.lastIndexOf('\n\n');
      if (cut < size / 3) cut = window.lastIndexOf('\n');
      if (cut < size / 3) cut = window.lastIndexOf(' ');
      if (cut < size / 3) cut = size;
      chunks.push({ start: from, end: from + cut });
      from += cut;
    }
    start = from;
    end = b;
  }
  flush();
  return chunks;
}

/** Top-level `.md`/`.txt` files of each root (or the root itself if it is a file) and of its `drive/` folder. */
async function listFiles(roots) {
  const files = [];
  const scan = async (dir, drive) => {
    let items;
    try {
      items = await readdir(dir, { withFileTypes: true });
    } catch {
      return;
    }
    for (const item of items.slice(0, MAX_FILES_PER_DIR))
      if (item.isFile() && ['.md', '.txt'].includes(extname(item.name).toLowerCase()) && !item.name.startsWith('.'))
        files.push({ path: join(dir, item.name), drive });
  };
  for (const configured of roots) {
    const root = resolve(configured);
    let info;
    try {
      info = await stat(root);
    } catch {
      continue;
    }
    if (info.isFile()) files.push({ path: root, drive: false });
    else if (info.isDirectory()) {
      await scan(root, false);
      await scan(join(root, 'drive'), true);
    }
  }
  await Promise.all(
    files.map(async file => {
      try {
        const s = await stat(file.path);
        file.size = s.size;
        file.mtime = s.mtimeMs;
      } catch {
        file.size = -1;
      }
    }),
  );
  return files.filter(f => f.size >= 0 && f.size <= MAX_FILE_BYTES);
}

function inferKind(title) {
  if (/\b(meeting|reunion)\b/.test(fold(title))) return 'meeting';
  if (/\bprotocol/.test(fold(title))) return 'protocol';
  return 'document';
}

function makeDoc(file, text) {
  const { meta, body } = frontMatter(text);
  const name = basename(file.path);
  const driveId = driveIdOf(meta, name);
  const title = clip(meta.title || body.match(/^#\s+(.+)$/m)?.[1] || name.replace(/\.(md|txt)$/i, ''), 300).trim();
  const kind = DOC_KINDS.includes(meta.kind) ? meta.kind : inferKind(title);
  const date = parseDay(meta.date) ?? dateFromTitle(title);
  return {
    id: driveId ?? createHash('sha256').update(file.path).digest('hex').slice(0, 20),
    pathId: createHash('sha256').update(file.path).digest('hex').slice(0, 20),
    driveId,
    drive: file.drive,
    title,
    kind,
    date,
    modifiedTime: meta.modifiedTime || null,
    folder: meta.folder || null,
    sourceUrl: meta.sourceUrl || (driveId ? driveUrl(driveId) : `/api/knowledge/${createHash('sha256').update(file.path).digest('hex').slice(0, 20)}`),
    text: body,
  };
}

const effectiveDate = doc => doc.date ?? (doc.modifiedTime ? String(doc.modifiedTime).slice(0, 10) : null);

function buildIndex(docs) {
  const chunks = [];
  const postings = new Map();
  const titles = new Map();
  let total = 0;
  for (const doc of docs) {
    doc.titleTerms = new Set(terms(doc.title));
    for (const w of doc.titleTerms) titles.get(w)?.push(doc) ?? titles.set(w, [doc]);
    doc.chunks = [];
    for (const piece of chunkText(doc.text)) {
      const index = chunks.length;
      doc.chunks.push(index);
      const words = terms(doc.text.slice(piece.start, piece.end));
      const tf = new Map();
      for (const w of words) tf.set(w, (tf.get(w) ?? 0) + 1);
      for (const [w, n] of tf) {
        let list = postings.get(w);
        if (!list) postings.set(w, (list = []));
        list.push(index, n);
      }
      chunks.push({ doc, start: piece.start, end: piece.end, length: words.length });
      total += words.length;
    }
    // A document with no text still answers by its title.
    if (!doc.chunks.length) {
      doc.chunks.push(chunks.length);
      chunks.push({ doc, start: 0, end: 0, length: 0 });
    }
  }
  return { docs, chunks, postings, titles, avg: total / Math.max(1, chunks.length) };
}

/**
 * The knowledge base of the configured roots (a folder like KNOWLEDGE_DIR, or
 * single files). Without roots: the repo's docs/meetings.md and docs/workflows.md.
 */
export function createKnowledge(config = {}) {
  const roots =
    config.knowledgeRoots ??
    (config.knowledgeRoot
      ? [config.knowledgeRoot]
      : process.env.KNOWLEDGE_DIR
        ? [process.env.KNOWLEDGE_DIR]
        : [join(here, '..', 'docs', 'meetings.md'), join(here, '..', 'docs', 'workflows.md')]);
  const checkMs = config.knowledgeCheckMs ?? CHECK_MS;
  let current = null,
    signature = '',
    checkedAt = 0,
    pending = null;

  async function refresh() {
    const files = await listFiles(roots);
    const next = files.map(f => `${f.path}\u0000${f.size}\u0000${f.mtime}`).join('\n');
    if (current && next === signature) return current;
    const docs = new Map();
    for (const file of files) {
      let text;
      try {
        text = await readFile(file.path, 'utf8');
      } catch {
        continue;
      }
      const doc = makeDoc(file, text);
      const seen = docs.get(doc.id);
      // The same Drive document copied by hand and synced: the drive/ copy wins.
      if (seen && (seen.drive || !doc.drive)) continue;
      docs.set(doc.id, doc);
    }
    current = buildIndex([...docs.values()]);
    signature = next;
    return current;
  }

  async function index() {
    if (current && Date.now() - checkedAt < checkMs) return current;
    pending ??= refresh().finally(() => {
      checkedAt = Date.now();
      pending = null;
    });
    return pending;
  }

  const kindsOf = kind =>
    kind
      ? new Set(
          String(kind)
            .split(',')
            .map(k => k.trim().toLowerCase())
            .filter(Boolean),
        )
      : null;
  const filterFor = ({ kind, from, to }) => {
    const kinds = kindsOf(kind);
    const start = parseDay(from),
      end = parseDay(to);
    return doc => {
      if (kinds && !kinds.has(doc.kind)) return false;
      if (start || end) {
        const day = effectiveDate(doc);
        if (!day || (start && day < start) || (end && day > end)) return false;
      }
      return true;
    };
  };

  /** Chunks ranked by BM25 with a title boost and a mild boost for recent meetings. */
  async function search({ query, kind, from, to, limit = 8, perDoc = 2 } = {}) {
    const { chunks, postings, titles, avg } = await index();
    const words = [...new Set(terms(clip(query, 300)))].slice(0, 16);
    if (!words.length) return [];
    const keep = filterFor({ kind, from, to });
    const n = chunks.length;
    const k1 = 1.2,
      b = 0.75;
    const scores = new Map();
    for (const word of words) {
      const list = postings.get(word);
      const df = list ? list.length / 2 : 0;
      const idf = Math.log(1 + (n - df + 0.5) / (df + 0.5));
      if (list)
        for (let i = 0; i < list.length; i += 2) {
          const chunk = chunks[list[i]];
          if (!keep(chunk.doc)) continue;
          const tf = list[i + 1];
          const s = (idf * tf * (k1 + 1)) / (tf + k1 * (1 - b + (b * chunk.length) / avg));
          scores.set(list[i], (scores.get(list[i]) ?? 0) + s);
        }
      // Title words count for every chunk of the document.
      for (const doc of titles.get(word) ?? [])
        if (keep(doc)) for (const i of doc.chunks) scores.set(i, (scores.get(i) ?? 0) + idf * 0.8);
    }
    const today = Date.now();
    const ranked = [...scores]
      .map(([i, score]) => {
        const chunk = chunks[i];
        let boost = 1;
        if (chunk.doc.kind === 'meeting' && chunk.doc.date) {
          const days = Math.max(0, (today - Date.parse(chunk.doc.date)) / 864e5);
          boost += 0.2 * 2 ** (-days / 180);
        }
        return { chunk, score: score * boost };
      })
      .sort((x, y) => y.score - x.score);
    const out = [],
      perDocCount = new Map();
    const max = Math.min(Math.max(Number(limit) || 8, 1), 20);
    for (const { chunk, score } of ranked) {
      const count = perDocCount.get(chunk.doc.id) ?? 0;
      if (count >= perDoc) continue;
      perDocCount.set(chunk.doc.id, count + 1);
      out.push({ ...summary(chunk.doc), score: Math.round(score * 100) / 100, offset: chunk.start, snippet: chunk.doc.text.slice(chunk.start, chunk.end).trim() });
      if (out.length >= max) break;
    }
    return out;
  }

  const summary = doc => ({
    id: doc.id,
    type: 'document',
    title: doc.title,
    kind: doc.kind,
    date: doc.date,
    ...(doc.modifiedTime ? { modifiedTime: doc.modifiedTime } : {}),
    sourceUrl: doc.sourceUrl,
  });

  /** A document by its id, its Drive id, the old 20-character id or a Drive link. */
  async function get(id) {
    const { docs } = await index();
    const raw = String(id ?? '').trim();
    const key = /\/d\/([A-Za-z0-9_-]{25,})/.exec(raw)?.[1] ?? /[?&]id=([A-Za-z0-9_-]{25,})/.exec(raw)?.[1] ?? raw;
    return docs.find(doc => doc.id === key || doc.pathId === key || doc.driveId === key) ?? null;
  }

  async function read({ id, offset = 0, max = 8000 } = {}) {
    const doc = await get(id);
    if (!doc) return null;
    const start = Math.min(Math.max(Math.floor(Number(offset) || 0), 0), doc.text.length);
    const size = Math.min(Math.max(Math.floor(Number(max) || 8000), 200), 20000);
    const text = doc.text.slice(start, start + size);
    return {
      ...summary(doc),
      offset: start,
      length: doc.text.length,
      text,
      nextOffset: start + text.length < doc.text.length ? start + text.length : null,
    };
  }

  /** Documents newest first (meeting date or last edit), e.g. the last meeting. */
  async function list({ kind, from, to, query, limit = 20 } = {}) {
    const { docs } = await index();
    const keep = filterFor({ kind, from, to });
    const words = terms(clip(query, 200));
    const found = docs
      .filter(keep)
      .filter(doc => !words.length || words.every(w => doc.titleTerms.has(w) || fold(doc.title).includes(w)))
      .sort(
        (x, y) =>
          String(effectiveDate(y) ?? '').localeCompare(String(effectiveDate(x) ?? '')) ||
          String(y.modifiedTime ?? '').localeCompare(String(x.modifiedTime ?? '')) ||
          x.title.localeCompare(y.title),
      );
    const counts = {};
    for (const doc of docs) counts[doc.kind] = (counts[doc.kind] ?? 0) + 1;
    const max = Math.min(Math.max(Number(limit) || 20, 1), 100);
    return {
      total: found.length,
      counts,
      documents: found.slice(0, max).map(doc => ({ ...summary(doc), chars: doc.text.length })),
    };
  }

  return { search, read, list, get, index };
}

/** The document tools of the assistant (chat, Claude and T3 Code through MCP). */
export const KNOWLEDGE_TOOLS = [
  {
    type: 'function',
    function: {
      name: 'search_knowledge',
      description:
        "Search the project's documents: meeting notes, protocols, reports and presentations from the project Drive (Ithomiini_IKIAM) and the curated notes. Returns the best passages (snippet) with the document id, title, kind, date (day of the meeting) and sourceUrl (the Drive link: cite it when you answer from a document). Use read_document for the whole text.",
      parameters: {
        type: 'object',
        properties: {
          query: { type: 'string', description: 'Words to look for (Spanish or English; accents do not matter)' },
          kind: {
            type: 'string',
            description: 'Only these kinds (comma-separated): meeting, protocol, presentation, report, document, transcript',
          },
          from: { type: 'string', description: 'Only documents dated on or after this day (YYYY-MM-DD)' },
          to: { type: 'string', description: 'Only documents dated on or before this day (YYYY-MM-DD)' },
          limit: { type: 'integer', description: '1 to 20 passages, default 8' },
        },
        required: ['query'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'read_document',
      description:
        'Read a document found with search_knowledge or list_documents (id, Drive id or Drive link). Returns up to max characters from offset; nextOffset continues it.',
      parameters: {
        type: 'object',
        properties: {
          id: { type: 'string' },
          offset: { type: 'integer', description: 'Character to start at (e.g. a passage offset), default 0' },
          max: { type: 'integer', description: 'Characters to return, up to 20000, default 8000' },
        },
        required: ['id'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'list_documents',
      description:
        'List the project documents newest first (meeting date, or last edit), e.g. kind "meeting" limit 1 for the last meeting. Also gives counts per kind.',
      parameters: {
        type: 'object',
        properties: {
          kind: { type: 'string', description: 'meeting, protocol, presentation, report, document, transcript (comma-separated)' },
          from: { type: 'string', description: 'YYYY-MM-DD' },
          to: { type: 'string', description: 'YYYY-MM-DD' },
          query: { type: 'string', description: 'Words that must be in the title' },
          limit: { type: 'integer', description: '1 to 100, default 20' },
        },
      },
    },
  },
];

/** Runs a document tool; sources (for [id] citations) go into context.sources. */
export async function runKnowledgeTool(knowledge, name, args = {}, context) {
  const cite = doc => context?.sources.set(doc.id, { id: doc.id, type: 'document', title: doc.title, sourceUrl: doc.sourceUrl });
  if (name === 'search_knowledge') {
    const passages = await knowledge.search({
      query: args.query,
      kind: args.kind,
      from: args.from,
      to: args.to,
      limit: args.limit,
    });
    for (const p of passages) cite(p);
    return passages.length ? { passages } : { passages, note: 'Nothing found; try other words or list_documents.' };
  }
  if (name === 'read_document') {
    const doc = await knowledge.read({ id: clip(args.id, 300), offset: args.offset, max: args.max });
    if (!doc) return { error: 'Document not found; use search_knowledge or list_documents for its id' };
    cite(doc);
    return doc;
  }
  if (name === 'list_documents') {
    const out = await knowledge.list({ kind: args.kind, from: args.from, to: args.to, query: args.query, limit: args.limit });
    for (const doc of out.documents) cite(doc);
    return out;
  }
  return null;
}
