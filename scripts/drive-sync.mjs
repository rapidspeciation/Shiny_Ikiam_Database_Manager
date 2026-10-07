#!/usr/bin/env node
// Mirrors the project's Drive documents as text for the assistant (server/knowledge.mjs):
// KNOWLEDGE_DIR/drive/<fileId>.md plus manifest.json. Read-only on Drive (gog --readonly);
// only changed files are exported again, removed or trashed ones are deleted here.
// Docs → Markdown (images dropped), Slides and pptx → text slide by slide, docx → text,
// PDFs → pdftotext when installed, WebVTT transcripts → speaker paragraphs.
//   node scripts/drive-sync.mjs        (ithomiini-drive-sync.timer, twice a day)
// Environment: KNOWLEDGE_DIR, ITHOMIINI_GOG_BIN, GOG_ACCOUNT, GOG_CLIENT (default ithomiini),
// GOG_KEYRING_PASSWORD (gog.env); ITHOMIINI_DRIVE_SYNC_CONFIG (default deploy/drive-sync.json);
// ITHOMIINI_PDFTOTEXT (default: pdftotext if installed).
import { execFile } from 'node:child_process';
import { existsSync, readFileSync } from 'node:fs';
import { chmod, mkdir, mkdtemp, readFile, readdir, rename, rm, stat, unlink, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { dirname, join } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { dateFromTitle } from '../server/knowledge.mjs';

const release = join(dirname(fileURLToPath(import.meta.url)), '..');
/** Bumped when the text extraction changes: every file is exported again. */
const EXTRACTOR = 1;
const LOCK_STALE_MS = 3 * 3600_000;

const MIME = {
  folder: 'application/vnd.google-apps.folder',
  doc: 'application/vnd.google-apps.document',
  slides: 'application/vnd.google-apps.presentation',
  pptx: 'application/vnd.openxmlformats-officedocument.presentationml.presentation',
  docx: 'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
  pdf: 'application/pdf',
};

/** How a Drive file becomes text, or null when it is not read (Sheets, images, videos…). */
export function methodOf(file) {
  const name = String(file.name ?? '').toLowerCase();
  if (file.mimeType === MIME.doc) return 'doc';
  if (file.mimeType === MIME.slides) return 'slides';
  if (file.mimeType === MIME.pptx || name.endsWith('.pptx')) return 'pptx';
  if (file.mimeType === MIME.docx || name.endsWith('.docx')) return 'docx';
  if (file.mimeType === MIME.pdf || name.endsWith('.pdf')) return 'pdf';
  if (file.mimeType === 'text/vtt' || name.endsWith('.vtt')) return 'vtt';
  if (/^text\/(plain|markdown)$/.test(file.mimeType ?? '') || /\.(txt|md)$/.test(name)) return 'text';
  return null;
}

/** meeting | protocol | presentation | report | document | transcript */
export function kindOf(method, folderKind) {
  if (method === 'vtt') return 'transcript';
  if (folderKind === 'protocol' || folderKind === 'report') return folderKind;
  if (folderKind === 'meeting' && method === 'doc') return 'meeting';
  return method === 'slides' || method === 'pptx' ? 'presentation' : 'document';
}

export function loadConfig(path) {
  const raw = JSON.parse(
    readFileSync(path ?? process.env.ITHOMIINI_DRIVE_SYNC_CONFIG ?? join(release, 'deploy', 'drive-sync.json'), 'utf8'),
  );
  if (!Array.isArray(raw.folders) || !raw.folders.length) throw new Error('drive-sync config has no folders');
  const names = new Set((raw.excludeNames ?? []).map(n => String(n).trim().toLowerCase()));
  const pattern = raw.excludePattern ? new RegExp(raw.excludePattern, 'i') : null;
  return {
    folders: raw.folders,
    excluded: name => names.has(String(name).trim().toLowerCase()) || Boolean(pattern?.test(String(name))),
    maxFileBytes: (raw.maxFileMb ?? 30) * 1024 * 1024,
    maxTextBytes: (raw.maxTextKb ?? 400) * 1024,
    pdfPages: raw.pdfPages ?? 80,
    concurrency: Math.max(1, Math.min(raw.concurrency ?? 3, 8)),
  };
}

function run(bin, args, { timeout = 600_000, env = process.env } = {}) {
  return new Promise((resolve, reject) => {
    execFile(bin, args, { timeout, env, maxBuffer: 64 * 1024 * 1024, encoding: 'utf8' }, (error, stdout, stderr) => {
      if (error) {
        const message = String(stderr || error.message)
          .trim()
          .split('\n')
          .slice(-3)
          .join(' ');
        reject(Object.assign(new Error(message.slice(0, 300) || 'command failed'), { code: error.code }));
      } else resolve(stdout);
    });
  });
}

function gogFor(env, exec = run) {
  const bin = env.ITHOMIINI_GOG_BIN;
  if (!bin) throw new Error('ITHOMIINI_GOG_BIN is not set');
  const base = ['--readonly', ...(env.GOG_ACCOUNT ? ['--account', env.GOG_ACCOUNT] : []), '--client', env.GOG_CLIENT || 'ithomiini', '--no-input'];
  return (args, options) => exec(bin, [...base, ...args], { ...options, env });
}

const FIELDS = 'files(id,name,mimeType,modifiedTime,size,webViewLink,trashed,shortcutDetails),nextPageToken';

/** Every file of a folder (all pages). */
async function listFolder(gog, id) {
  const files = [];
  let page = '';
  for (let i = 0; i < 100; i++) {
    const out = await gog(['drive', 'ls', '--parent', id, '--max', '1000', '--json', '--fields', FIELDS, ...(page ? ['--page', page] : [])], { timeout: 120_000 });
    let data;
    try {
      data = JSON.parse(out);
    } catch {
      throw new Error(`gog drive ls returned no JSON for folder ${id}`);
    }
    const list = Array.isArray(data) ? data : data?.files;
    if (!Array.isArray(list)) throw new Error(`gog drive ls returned no file list for folder ${id}`);
    files.push(...list);
    page = Array.isArray(data) ? '' : (data.nextPageToken ?? '');
    if (!page) return files;
  }
  throw new Error(`folder ${id} has too many pages`);
}

/** The files to mirror, walking the configured folders (excluded names are never opened). */
export async function inventory(gog, config) {
  const found = new Map();
  let excluded = 0,
    ignored = 0;
  const walk = async (folderId, path, folder) => {
    for (const file of await listFolder(gog, folderId)) {
      if (file.trashed === true) continue;
      if (config.excluded(file.name)) {
        excluded++;
        continue;
      }
      if (file.mimeType === MIME.folder) {
        if (folder.recursive !== false) await walk(file.id, `${path}/${file.name}`, folder);
        continue;
      }
      const method = methodOf(file);
      if (!method || file.mimeType === 'application/vnd.google-apps.shortcut') {
        ignored++;
        continue;
      }
      if (!found.has(file.id)) found.set(file.id, { ...file, method, path, kind: kindOf(method, folder.kind) });
    }
  };
  for (const folder of config.folders) await walk(folder.id, folder.name, folder);
  return { files: [...found.values()], excluded, ignored };
}

const ENTITIES = { amp: '&', lt: '<', gt: '>', quot: '"', apos: "'", nbsp: ' ' };
const decode = text =>
  text.replace(/&(#x[0-9a-f]+|#\d+|[a-z]+);/gi, (all, e) =>
    e[0] === '#'
      ? String.fromCodePoint(e[1] === 'x' || e[1] === 'X' ? parseInt(e.slice(2), 16) : Number(e.slice(1)))
      : (ENTITIES[e.toLowerCase()] ?? all),
  );

/** The text of an Office XML part: one line per paragraph (<a:p>/<w:p>). */
export function xmlText(xml, word = false) {
  const para = word ? /<w:p[\s>][\s\S]*?<\/w:p>|<w:p\/>/g : /<a:p[\s>][\s\S]*?<\/a:p>|<a:p\/>/g;
  const lines = [];
  for (const p of xml.match(para) ?? []) {
    let line = '';
    const parts = word
      ? /<w:t(?:\s[^>]*)?>([\s\S]*?)<\/w:t>|<w:tab\/>|<w:br\/>/g
      : /<a:t(?:\s[^>]*)?>([\s\S]*?)<\/a:t>|<a:br\/>/g;
    let m;
    while ((m = parts.exec(p))) line += m[1] !== undefined ? decode(m[1]) : m[0].includes('tab') ? '\t' : '\n';
    const heading = word && /<w:pStyle w:val="(?:Heading|Titulo|Title)(\d?)"/i.exec(p);
    if (!line.trim()) continue;
    lines.push(heading ? `${'#'.repeat(Math.min(Number(heading[1]) || 1, 5) + 1)} ${line.trim()}` : line.trimEnd());
  }
  return lines.join('\n');
}

async function unzipParts(file, dir, patterns) {
  // Exit code 11: some pattern matched nothing (a deck without notes).
  await run('unzip', ['-o', '-qq', file, ...patterns, '-d', dir], { timeout: 120_000 }).catch(e => {
    if (e.code !== 11) throw e;
  });
}
const readIf = path => readFile(path, 'utf8').catch(() => '');

/** Slide by slide ("## Diapositiva N"), in the deck's order, with the speaker notes. */
export async function pptxText(file, work) {
  const dir = join(work, 'x');
  await rm(dir, { recursive: true, force: true });
  await unzipParts(file, dir, ['ppt/presentation.xml', 'ppt/_rels/presentation.xml.rels', 'ppt/slides/*', 'ppt/notesSlides/*']);
  const rels = await readIf(join(dir, 'ppt/_rels/presentation.xml.rels'));
  const targets = new Map([...rels.matchAll(/<Relationship\b[^>]*>/g)].map(m => [/Id="([^"]+)"/.exec(m[0])?.[1], /Target="([^"]+)"/.exec(m[0])?.[1]]));
  let order = [...(await readIf(join(dir, 'ppt/presentation.xml'))).matchAll(/<p:sldId\b[^>]*r:id="([^"]+)"/g)]
    .map(m => targets.get(m[1]))
    .filter(t => t && /slides\/slide\d+\.xml$/.test(t))
    .map(t => `ppt/${t.replace(/^\/?(ppt\/)?/, '')}`);
  if (!order.length) {
    const names = await readdir(join(dir, 'ppt/slides')).catch(() => []);
    order = names
      .filter(n => /^slide\d+\.xml$/.test(n))
      .sort((a, b) => Number(a.match(/\d+/)[0]) - Number(b.match(/\d+/)[0]))
      .map(n => `ppt/slides/${n}`);
  }
  const out = [];
  for (const [i, part] of order.entries()) {
    const text = xmlText(await readIf(join(dir, part)));
    const rel = await readIf(join(dir, part.replace(/slides\/(slide\d+\.xml)$/, 'slides/_rels/$1.rels')));
    const notesTarget = /Target="\.\.\/notesSlides\/(notesSlide\d+\.xml)"/.exec(rel)?.[1];
    const notes = notesTarget
      ? xmlText(await readIf(join(dir, 'ppt/notesSlides', notesTarget)))
          .split('\n')
          .filter(line => !/^\d+$/.test(line.trim()))
          .join('\n')
      : '';
    out.push(`## Diapositiva ${i + 1}\n\n${text}${notes.trim() ? `\n\nNotas: ${notes.trim()}` : ''}`);
  }
  return out.join('\n\n');
}

export async function docxText(file, work) {
  const dir = join(work, 'x');
  await rm(dir, { recursive: true, force: true });
  await unzipParts(file, dir, ['word/document.xml']);
  return xmlText(await readIf(join(dir, 'word/document.xml')), true);
}

/** Every `text` value in a JSON value (text boxes, table cells). */
const textsIn = value =>
  Array.isArray(value)
    ? value.flatMap(textsIn)
    : value && typeof value === 'object'
      ? Object.entries(value).flatMap(([k, v]) => (k === 'text' && typeof v === 'string' ? [v] : textsIn(v)))
      : [];

/**
 * A Google Slides deck slide by slide: exported as pptx, or, when Drive refuses
 * the export (over 10 MB with its images), read slide by slide with gog slides.
 */
export async function slidesText(gog, file, dir) {
  const target = join(dir, 'deck.pptx');
  await rm(target, { force: true });
  try {
    await gog(['drive', 'download', file.id, '--format', 'pptx', '--out', target, '--overwrite']);
    return await pptxText(target, dir);
  } catch (e) {
    if (!/exportSizeLimitExceeded|too large to be exported/i.test(e.message)) throw e;
  }
  const list = JSON.parse(await gog(['slides', 'list-slides', file.id, '--json'], { timeout: 120_000 }));
  const out = [];
  for (const [i, slide] of (list.slides ?? []).slice(0, 300).entries()) {
    const data = JSON.parse(await gog(['slides', 'read-slide', file.id, slide.objectId, '--json'], { timeout: 120_000 }));
    const text = [...textsIn(data.textElements ?? []), ...textsIn(data.tables ?? [])]
      .map(t => t.trim())
      .filter(Boolean)
      .join('\n');
    const notes = String(data.notes ?? '').trim();
    out.push(`## Diapositiva ${slide.number ?? i + 1}\n\n${text}${notes ? `\n\nNotas: ${notes}` : ''}`);
  }
  return out.join('\n\n');
}

/** A WebVTT transcript as speaker paragraphs, without cue numbers and times. */
export function vttText(raw) {
  const out = [];
  let last = null;
  for (const block of raw.replace(/\r/g, '').split(/\n{2,}/)) {
    const lines = block.split('\n').filter(Boolean);
    const at = lines.findIndex(l => l.includes('-->'));
    if (at < 0) continue;
    for (const line of lines.slice(at + 1)) {
      const voice = /^<v(?:\.[^\s>]*)?\s+([^>]+)>/.exec(line)?.[1]?.trim() ?? /^([^:<]{2,40}):\s/.exec(line)?.[1]?.trim() ?? null;
      const text = decode(line.replace(/<[^>]+>/g, '').replace(voice && !line.startsWith('<v') ? `${voice}: ` : '', '')).trim();
      if (!text) continue;
      if (last && last.voice === voice) last.text += ` ${text}`;
      else out.push((last = { voice, text }));
    }
  }
  return out.map(p => (p.voice ? `${p.voice}: ${p.text}` : p.text)).join('\n\n');
}

function pdftotextBin(env) {
  if (env.ITHOMIINI_PDFTOTEXT !== undefined) return existsSync(env.ITHOMIINI_PDFTOTEXT) ? env.ITHOMIINI_PDFTOTEXT : null;
  return ['/usr/bin/pdftotext', '/usr/local/bin/pdftotext'].find(p => existsSync(p)) ?? null;
}

const fmValue = value => {
  const text = String(value ?? '').replace(/[\r\n]+/g, ' ');
  return /^['"\s]|\s$|"/.test(text) ? JSON.stringify(text) : text;
};

/** Images embedded in a Docs Markdown export (base64 data URIs) are dropped; alt text stays. */
export const withoutImages = text =>
  text
    .replace(/^\[[^\]\n]+\]:\s*<?data:[^\n]*$/gm, '')
    .replace(/!\[([^\]\n]*)\](?:\[[^\]\n]*\]|\(data:[^)]*\))/g, (all, alt) => (alt.trim() ? `[imagen: ${alt.trim()}]` : ''))
    .replace(/data:[a-z]+\/[\w.+-]+;base64,[A-Za-z0-9+/=]+/gi, '')
    .replace(/\n{4,}/g, '\n\n\n');

/** Video-call links in meeting notes are left out (as in the hand-copied notes). */
export const withoutCallLinks = text =>
  text.replace(/https?:\/\/(?:meet\.google\.com|[\w-]+\.zoom\.us|zoom\.us|teams\.microsoft\.com|teams\.live\.com)\/(?:[^\s)\]>]*[^\s)\]>.,;:])?/gi, '[enlace de videollamada omitido]');

function capText(text, bytes) {
  const buffer = Buffer.from(text, 'utf8');
  if (buffer.length <= bytes) return text;
  return `${buffer.subarray(0, bytes).toString('utf8').replace(/�$/, '')}\n\n[… texto recortado a ${Math.round(bytes / 1024)} KB]`;
}

export function documentFile(file, text, status = 'ok') {
  const title = String(file.name ?? '').replace(/\.(pptx|docx|pdf|vtt|txt|md)$/i, '');
  const url = file.webViewLink || `https://drive.google.com/file/d/${file.id}/view`;
  const front = {
    title,
    sourceUrl: url,
    driveId: file.id,
    mimeType: file.mimeType,
    modifiedTime: file.modifiedTime,
    folder: file.path,
    kind: file.kind,
    date: dateFromTitle(title) ?? '',
    ...(status === 'ok' ? {} : { status }),
  };
  const lines = Object.entries(front).map(([k, v]) => `${k}: ${fmValue(v)}`);
  const body = text.trim() ? text.trim() : status === 'ok' ? '(Documento sin texto.)' : `(Sin texto extraído: ${status}. Ábrelo en Drive.)`;
  const heading = body.split('\n', 1)[0].replace(/^#\s+/, '').trim() === title ? '' : `# ${title}\n\n`;
  return `---\n${lines.join('\n')}\n---\n${heading}Fuente: ${url}\n\n${body}\n`;
}

async function writePrivate(path, text) {
  const tmp = join(dirname(path), `.${Date.now()}-${process.pid}-${Math.random().toString(36).slice(2)}.tmp`);
  await writeFile(tmp, text, { mode: 0o600 });
  await chmod(tmp, 0o600);
  await rename(tmp, path);
}

async function acquireLock(path) {
  for (let attempt = 0; attempt < 2; attempt++) {
    try {
      await writeFile(path, `${process.pid} ${new Date().toISOString()}\n`, { flag: 'wx', mode: 0o600 });
      return true;
    } catch (e) {
      if (e.code !== 'EEXIST') throw e;
      const [pid] = (await readIf(path)).split(' ');
      let alive = false;
      try {
        process.kill(Number(pid), 0);
        alive = Number(pid) > 0;
      } catch {
        /* Not running. */
      }
      const age = Date.now() - ((await stat(path).catch(() => null))?.mtimeMs ?? 0);
      if (alive && age < LOCK_STALE_MS) return false;
      await unlink(path).catch(() => {});
    }
  }
  return false;
}

/**
 * One sync run. Returns a summary; throws when the Drive listing fails (nothing is deleted then).
 * `exec(bin, args, options)` runs gog (resolves stdout, rejects with stderr's message); tests pass their own.
 */
export async function syncDrive({ env = process.env, configPath, log = () => {}, exec = run } = {}) {
  if (!env.KNOWLEDGE_DIR) throw new Error('KNOWLEDGE_DIR is not set');
  const config = loadConfig(configPath ?? env.ITHOMIINI_DRIVE_SYNC_CONFIG);
  const gog = gogFor(env, exec);
  const out = join(env.KNOWLEDGE_DIR, 'drive');
  await mkdir(out, { recursive: true, mode: 0o700 });
  const lock = join(out, '.sync.lock');
  if (!(await acquireLock(lock))) throw Object.assign(new Error('another drive sync is running'), { locked: true });
  const unlock = () => unlink(lock).catch(() => {});
  const onSignal = () => unlock().finally(() => process.exit(1));
  process.once('SIGTERM', onSignal);
  process.once('SIGINT', onSignal);
  const work = await mkdtemp(join(tmpdir(), 'ithomiini-drive-'));
  try {
    const manifestPath = join(out, 'manifest.json');
    const old = JSON.parse((await readIf(manifestPath)) || '{}');
    const files = { ...(old.files ?? {}) };
    const { files: current, excluded, ignored } = await inventory(gog, config);
    const pdftotext = pdftotextBin(env);
    const summary = { total: current.length, exported: 0, unchanged: 0, deleted: 0, failed: 0, excluded, ignored, kinds: {}, status: {} };
    const saveManifest = () =>
      writePrivate(manifestPath, `${JSON.stringify({ version: 1, syncedAt: new Date().toISOString(), files }, null, 1)}\n`);

    // Removed, trashed or now excluded: their text goes.
    const live = new Set(current.map(f => f.id));
    for (const id of Object.keys(files))
      if (!live.has(id)) {
        await unlink(join(out, `${id}.md`)).catch(() => {});
        delete files[id];
        summary.deleted++;
      }
    for (const name of await readdir(out)) {
      const id = /^([A-Za-z0-9_-]+)\.md$/.exec(name)?.[1];
      if ((id && !live.has(id)) || /^\..*\.tmp$/.test(name)) {
        await unlink(join(out, name)).catch(() => {});
        if (id) summary.deleted++;
      }
    }

    const todo = current.filter(file => {
      const prev = files[file.id];
      const same =
        prev &&
        prev.extractor === EXTRACTOR &&
        prev.modifiedTime === file.modifiedTime &&
        prev.title === file.name &&
        prev.folder === file.path &&
        prev.kind === file.kind &&
        prev.status !== 'error' &&
        !(prev.status === 'sin texto' && pdftotext) &&
        existsSync(join(out, `${file.id}.md`));
      if (same) summary.unchanged++;
      return !same;
    });

    const exportOne = async (file, slot) => {
      const dir = join(work, String(slot));
      await mkdir(dir, { recursive: true });
      const size = Number(file.size ?? 0);
      let text = '',
        status = 'ok';
      // Native Docs and Slides are exported as text; their Drive size (images) does not matter.
      if (size > config.maxFileBytes && file.method !== 'doc' && file.method !== 'slides') status = 'muy grande';
      else if (file.method === 'pdf' && !pdftotext) status = 'sin texto';
      else if (file.method === 'doc') {
        const target = join(dir, 'doc.md');
        await rm(target, { force: true });
        await gog(['drive', 'download', file.id, '--format', 'md', '--out', target, '--overwrite']);
        text = await readFile(target, 'utf8');
      } else if (file.method === 'slides') text = await slidesText(gog, file, dir);
      else {
        const target = join(dir, `file.${file.method}`);
        await rm(target, { force: true });
        await gog(['drive', 'download', file.id, '--out', target, '--overwrite']);
        if (file.method === 'pptx') text = await pptxText(target, dir);
        else if (file.method === 'docx') text = await docxText(target, dir);
        else if (file.method === 'pdf')
          text = await run(pdftotext, ['-layout', '-enc', 'UTF-8', '-l', String(config.pdfPages), target, '-'], { timeout: 180_000 });
        else if (file.method === 'vtt') text = vttText(await readFile(target, 'utf8'));
        else text = await readFile(target, 'utf8');
      }
      text = capText(withoutCallLinks(withoutImages(text).replace(/\r\n?/g, '\n').replace(/\f/g, '\n\n')), config.maxTextBytes);
      await writePrivate(join(out, `${file.id}.md`), documentFile(file, text, status));
      return { status, bytes: Buffer.byteLength(text) };
    };

    let next = 0,
      done = 0;
    const worker = async slot => {
      while (next < todo.length) {
        const file = todo[next++];
        const entry = {
          title: file.name,
          modifiedTime: file.modifiedTime,
          mimeType: file.mimeType,
          kind: file.kind,
          folder: file.path,
          sourceUrl: file.webViewLink ?? null,
          extractor: EXTRACTOR,
        };
        try {
          const result = await exportOne(file, slot);
          files[file.id] = { ...entry, ...result, exportedAt: new Date().toISOString() };
          summary.exported++;
        } catch (e) {
          // The earlier text (if any) stays until the export works again.
          files[file.id] = { ...entry, modifiedTime: files[file.id]?.modifiedTime ?? null, status: 'error', error: String(e.message).slice(0, 300) };
          summary.failed++;
          log(`failed: ${file.id} (${file.method}): ${String(e.message).slice(0, 200)}`);
        }
        if (++done % 20 === 0) await saveManifest();
      }
    };
    await Promise.all(Array.from({ length: config.concurrency }, (_, slot) => worker(slot)));
    await saveManifest();
    for (const file of current) {
      summary.kinds[file.kind] = (summary.kinds[file.kind] ?? 0) + 1;
      const status = files[file.id]?.status ?? 'error';
      summary.status[status] = (summary.status[status] ?? 0) + 1;
    }
    return summary;
  } finally {
    process.off('SIGTERM', onSignal);
    process.off('SIGINT', onSignal);
    await rm(work, { recursive: true, force: true });
    await unlock();
  }
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  syncDrive({ log: line => console.error(line) }).then(
    s => {
      const kinds = Object.entries(s.kinds)
        .map(([k, n]) => `${k} ${n}`)
        .join(', ');
      const status = Object.entries(s.status)
        .map(([k, n]) => `${k} ${n}`)
        .join(', ');
      console.log(
        `Drive sync: ${s.total} documents (${kinds || 'none'}); exported ${s.exported}, unchanged ${s.unchanged}, deleted ${s.deleted}, failed ${s.failed}; status: ${status || '-'}; excluded ${s.excluded}, not text ${s.ignored}`,
      );
      if (s.failed) {
        console.error(`Drive sync: ${s.failed} file(s) failed; they are retried next run (see drive/manifest.json)`);
        process.exitCode = 1;
      }
    },
    e => {
      console.error(`Drive sync failed: ${e.message}`);
      process.exitCode = e.locked ? 75 : 1;
    },
  );
}
