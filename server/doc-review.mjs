// Comments on the assistant's instructions (#/instrucciones) and the changes Claude proposes for them.
//
// - Threads: a quote of the rendered text (anchored as in Franz's md-review page: the quote, 32 characters
//   before and after it and its offset) with replies. Any signed-in person comments, replies, resolves and
//   reopens, and edits or deletes their own comments; the API names people by display_name only.
// - Changes («cambio de documento»): one section of an assistant/**/*.md file, before and after, proposed by
//   Claude over MCP (propose_doc_change, revise_doc_change: v1, v2… of a series). Admins apply, discard and undo.
// - Overlay: applying writes a doc_revisions row. effectiveText(path) is what the page (server/instructions.mjs),
//   the brief (server/brief.mjs) and the workspaces (scripts/t3-provision.mjs) read: the shipped file, unless a
//   chain of revisions starts at the shipped file's hash; then the chain's head. A release that has the commit
//   has the head's hash (the overlay goes quiet); one that changed the file otherwise stops the overlay.
// - Git: each revision is committed in a worktree of its own (docSync.worktree); pushed only when docSync.push.
//   Only assistant/**/*.md is written, and assistant/context-budget.json when a change changes a part's size.
// - Guards on apply (Aplicar skips deploy.sh): the context-size ratchet (scripts/context-budget.mjs) measured on
//   the effective text, and the pure checks the tests run on assistant/ (docChecks).
import { createHash, randomUUID } from 'node:crypto';
import { execFile } from 'node:child_process';
import { existsSync, mkdtempSync, readdirSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';

export const hashText = text => createHash('sha256').update(String(text)).digest('hex').slice(0, 20);
/** The files that take changes: Markdown under assistant/, no way out of it. */
export const DOC_PATH = /^assistant\/(?:[\w.-]+\/)*[\w.-]+\.md$/;
const okPath = path => DOC_PATH.test(String(path ?? '')) && !String(path).split('/').includes('..');
const MAX_BODY = 4000;
const MAX_TEXT = 60_000;
/** The brief as the page shows it is AGENTS.md and the app's voice (server/brief.mjs). */
const BRIEF = 'assistant/AGENTS.md';
const BRIEF_SOURCES = [BRIEF, 'assistant/voice/app.md', 'assistant/voice/core.md'];
export const sourcesOf = path => (path === BRIEF ? BRIEF_SOURCES : [path]);
const PLACEHOLDERS = new Set(['person', 'username', 'docs', 'knowledge', 'sheets']);

// ------------------------------------------------------------------ tables

export function initDocReview(db) {
  db.exec(`
    CREATE TABLE IF NOT EXISTS doc_threads (
      id TEXT PRIMARY KEY, path TEXT NOT NULL, quote TEXT NOT NULL, prefix TEXT NOT NULL DEFAULT '', suffix TEXT NOT NULL DEFAULT '',
      offset INTEGER NOT NULL DEFAULT -1, heading TEXT NOT NULL DEFAULT '', status TEXT NOT NULL DEFAULT 'open',
      created_by TEXT NOT NULL, created_at TEXT NOT NULL, resolved_by TEXT, resolved_at TEXT, resolved_by_change TEXT);
    CREATE INDEX IF NOT EXISTS doc_threads_path ON doc_threads(path, status);
    CREATE TABLE IF NOT EXISTS doc_comments (
      id TEXT PRIMARY KEY, thread_id TEXT, series_id TEXT, author TEXT NOT NULL, kind TEXT NOT NULL DEFAULT 'person',
      body TEXT NOT NULL, created_at TEXT NOT NULL, edited_at TEXT, deleted_at TEXT);
    CREATE INDEX IF NOT EXISTS doc_comments_thread ON doc_comments(thread_id);
    CREATE INDEX IF NOT EXISTS doc_comments_series ON doc_comments(series_id);
    CREATE TABLE IF NOT EXISTS doc_changes (
      id TEXT PRIMARY KEY, series_id TEXT NOT NULL, version INTEGER NOT NULL, path TEXT NOT NULL, heading TEXT NOT NULL,
      base_hash TEXT NOT NULL, before_part TEXT NOT NULL, after_part TEXT NOT NULL, before TEXT NOT NULL, after TEXT NOT NULL,
      diff TEXT, thread_ids TEXT NOT NULL DEFAULT '[]', status TEXT NOT NULL DEFAULT 'pending', chat_id TEXT,
      created_by TEXT, created_at TEXT NOT NULL, decided_by TEXT, decided_at TEXT, revision_id INTEGER);
    CREATE INDEX IF NOT EXISTS doc_changes_series ON doc_changes(series_id, version);
    CREATE TABLE IF NOT EXISTS doc_revisions (
      id INTEGER PRIMARY KEY AUTOINCREMENT, path TEXT NOT NULL, parent_hash TEXT NOT NULL, hash TEXT NOT NULL, text TEXT NOT NULL,
      change_id TEXT, kind TEXT NOT NULL DEFAULT 'apply', heading TEXT NOT NULL DEFAULT '', section_before TEXT, section_after TEXT,
      applied_by TEXT, applied_at TEXT NOT NULL, undone_by TEXT, undone_at TEXT,
      git_state TEXT NOT NULL DEFAULT 'pending', git_commit TEXT, git_error TEXT);
    CREATE INDEX IF NOT EXISTS doc_revisions_path ON doc_revisions(path, id);
    CREATE TABLE IF NOT EXISTS doc_asks (
      id TEXT PRIMARY KEY, asked_by TEXT NOT NULL, asked_at TEXT NOT NULL, chat_id TEXT, kind TEXT NOT NULL DEFAULT 'ask',
      thread_ids TEXT NOT NULL DEFAULT '[]', change_ids TEXT NOT NULL DEFAULT '[]');
  `);
}

// ------------------------------------------------------------------ the overlay

/**
 * The text in effect: `shipped` unless revisions (oldest first) chain from its hash; then the last one reached.
 * A revision whose parent is not the text reached so far is skipped (an older chain a release already has).
 */
export function effectiveFrom(shipped, revisions) {
  let hash = hashText(shipped);
  let text = shipped;
  const used = [];
  for (const r of revisions) {
    if (r.parent_hash !== hash) continue;
    hash = r.hash;
    text = r.text;
    used.push(r.id);
  }
  return { text, hash, used };
}

const tableThere = db => {
  try {
    return !!db.prepare("SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = 'doc_revisions'").get();
  } catch {
    return false;
  }
};

/**
 * A reader of the instructions' files with the overlay: read(relPath) → text. For scripts/t3-provision.mjs
 * and scripts/context-budget.mjs as well as the app (a database without the tables reads the files).
 */
export function overlayReader(db, root) {
  const has = db && tableThere(db);
  const revs = has ? db.prepare('SELECT id, parent_hash, hash, text FROM doc_revisions WHERE path = ? ORDER BY id') : null;
  return rel => {
    const shipped = readFileSync(join(root, rel), 'utf8');
    if (!revs || !okPath(rel)) return shipped;
    return effectiveFrom(shipped, revs.all(rel)).text;
  };
}

/** The files the overlay changes now: [{ path, text }] (scripts/t3-provision.mjs writes them over the copies). */
export function overlaidFiles(db, root) {
  if (!db || !tableThere(db)) return [];
  const read = overlayReader(db, root);
  const out = [];
  for (const { path } of db.prepare('SELECT DISTINCT path FROM doc_revisions').all()) {
    if (!okPath(path) || !existsSync(join(root, path))) continue;
    const text = read(path);
    if (text !== readFileSync(join(root, path), 'utf8')) out.push({ path, text });
  }
  return out;
}

// ------------------------------------------------------------------ sections and anchors

/** Where the front matter ends (0 without one). */
export function bodyStart(text) {
  const m = /^---\n[\s\S]*?\n---\n?/.exec(text);
  return m ? m[0].length : 0;
}

/** Strips Markdown marks for comparing a heading or a quote of the rendered text with the source. */
export const plain = s =>
  String(s ?? '')
    .replace(/\[([^\]]*)\]\([^)]*\)/g, '$1')
    .replace(/[`*_>#|\\]/g, '')
    .replace(/\s+/g, ' ')
    .trim();

/**
 * The sections of a Markdown source: [{ heading, level, start, end }]. A section runs from its heading to the
 * next heading of its level or higher; the text before the first heading is the section '' (level 0).
 * Headings in fenced code do not count.
 */
export function sections(text) {
  const start0 = bodyStart(text);
  const heads = [];
  let fence = null;
  let at = start0;
  for (const line of text.slice(start0).split('\n')) {
    const f = /^\s*(```|~~~)/.exec(line);
    if (f) fence = fence === f[1] ? null : (fence ?? f[1]);
    else if (!fence) {
      const h = /^(#{1,6})\s+(.*?)\s*#*\s*$/.exec(line);
      if (h) heads.push({ heading: h[2], level: h[1].length, start: at });
    }
    at += line.length + 1;
  }
  const end = text.length;
  const out = [];
  if ((heads[0]?.start ?? end) > start0 && text.slice(start0, heads[0]?.start ?? end).trim())
    out.push({ heading: '', level: 0, start: start0, end: heads[0]?.start ?? end });
  heads.forEach((h, i) => {
    const next = heads.slice(i + 1).find(n => n.level <= h.level);
    out.push({ ...h, end: next?.start ?? end });
  });
  return out;
}

/** The section named `heading` (compared without Markdown marks); `containing`: the one with that text, if several. */
export function findSection(text, heading, containing = '') {
  const all = sections(text);
  const want = plain(heading);
  const named = all.filter(s => plain(s.heading) === want);
  const pick = list => (containing ? list.find(s => text.slice(s.start, s.end).includes(containing)) : null) ?? list[0] ?? null;
  const s = named.length ? pick(named) : null;
  return s ? { ...s, text: text.slice(s.start, s.end) } : null;
}

/**
 * Where a quote of the rendered text sits in `text`: every match scored by its prefix and suffix agreeing,
 * ties to the nearest offset (md-review's locate). -1 when the quote is not there.
 */
export function locate(text, { quote, prefix = '', suffix = '', offset = -1 }) {
  if (!quote) return -1;
  let best = -1;
  let score = -Infinity;
  for (let i = text.indexOf(quote); i >= 0; i = text.indexOf(quote, i + 1)) {
    let s = 0;
    if (prefix && text.slice(Math.max(0, i - prefix.length), i) === prefix) s += 2;
    if (suffix && text.slice(i + quote.length, i + quote.length + suffix.length) === suffix) s += 2;
    s -= Math.abs(i - (offset ?? 0)) / 1e6;
    if (s > score) {
      score = s;
      best = i;
    }
  }
  return best;
}

/** The section of the source a thread is in: by its heading, else the one whose plain text holds the quote. */
export function threadSection(read, path, thread) {
  const quote = plain(thread.quote);
  for (const file of sourcesOf(path)) {
    let text;
    try {
      text = read(file);
    } catch {
      continue;
    }
    const all = sections(text).map(s => ({ ...s, text: text.slice(s.start, s.end) }));
    const named = all.filter(s => plain(s.heading) === plain(thread.heading));
    const hit = named.find(s => plain(s.text).includes(quote)) ?? (named.length === 1 && file === path ? named[0] : null);
    const found = hit ?? all.filter(s => plain(s.text).includes(quote)).sort((a, b) => a.text.length - b.text.length)[0];
    if (found) return { path: file, heading: found.heading, text: found.text, quoteFound: plain(found.text).includes(quote) };
  }
  return null;
}

/**
 * `part` replaced by `replacement` in a section: the part must be there exactly once (or be the whole section,
 * trailing blank lines aside). Returns the new section or null.
 */
export function replacePart(section, part, replacement) {
  if (!part) return null;
  if (section.trimEnd() === part.trimEnd()) return replacement.trimEnd() + section.slice(section.trimEnd().length);
  const at = section.indexOf(part);
  if (at < 0 || section.indexOf(part, at + 1) >= 0) return null;
  return section.slice(0, at) + replacement + section.slice(at + part.length);
}

// ------------------------------------------------------------------ checks (Aplicar skips deploy.sh)

/**
 * The pure checks the tests run on assistant/ (tests/t3-provision.test.mjs, tests/app-guide.test.mjs), on the
 * files as they would be: [{ code, ... }], [] passes. `read(rel)`: the files with the change; `root`: the release.
 */
export function docChecks(path, read, root) {
  const out = [];
  const text = read(path);
  const skill = /^assistant\/skills\/([^/]+)\/SKILL\.md$/.exec(path)?.[1];
  if (skill && !new RegExp(`^---\\nname: ${skill.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}\\ndescription: .+\\n---\\n`).test(text))
    out.push({ code: 'front_matter', file: path });
  if (path === BRIEF)
    for (const m of text.matchAll(/\{\{\s*([^}]*?)\s*\}\}/g)) if (!PLACEHOLDERS.has(m[1])) out.push({ code: 'placeholder', text: m[0] });
  if (path.startsWith('assistant/skills/app-guide/')) {
    const dir = 'assistant/skills/app-guide/';
    let refs = [];
    try {
      refs = listFiles(join(root, dir, 'reference'));
    } catch {
      /* no reference folder */
    }
    const main = read(`${dir}SKILL.md`);
    for (const f of refs) if (!main.includes(`(reference/${f})`)) out.push({ code: 'guide_link', file: `reference/${f}` });
    const routerFiles = ['frontend/src/router.ts', 'frontend/src/tabs.ts'].map(f => join(root, f));
    if (routerFiles.every(existsSync)) {
      const router = routerFiles.map(f => readFileSync(f, 'utf8')).join('\n');
      const routes = new Set([...router.matchAll(/path: '\/([a-z]*)(?:\/:[a-zA-Z]+\??)?'/g)].map(m => m[1]));
      const all = [main, ...refs.map(f => read(`${dir}reference/${f}`))].join('\n');
      for (const route of new Set([...all.matchAll(/#\/([a-z]+)/g)].map(m => m[1]))) if (!routes.has(route)) out.push({ code: 'route', route });
    }
  }
  return out;
}
const listFiles = dir => readdirSync(dir).filter(f => !f.startsWith('.')).sort();

/**
 * The parts that grow past what is recorded (or already accepted): [{ part, bytes }]. `recorded`: the budget
 * file's parts; `now` and `after`: measured without and with the change. Totals are left out (they repeat).
 */
export function budgetGrowth(recorded, now, after) {
  const out = [];
  for (const [part, bytes] of Object.entries(after ?? {})) {
    if (part.includes('(total)')) continue;
    const base = Math.max(recorded?.[part]?.bytes ?? 0, now?.[part] ?? 0);
    if (bytes > base) out.push({ part, bytes: bytes - base });
  }
  return out;
}

// ------------------------------------------------------------------ git

const git = (cwd, args, { timeout = 60_000, env } = {}) =>
  new Promise((resolve, reject) =>
    execFile('git', ['-C', cwd, ...args], { timeout, maxBuffer: 64 * 1024 * 1024, env: env ?? process.env }, (error, stdout, stderr) =>
      error ? reject(Object.assign(new Error(String(stderr || error.message).trim().split('\n').slice(-3).join(' ')), { stdout })) : resolve(String(stdout)),
    ),
  );

/** A word diff of two texts (git diff --no-index --word-diff=porcelain), from its first hunk; null without git. */
export async function wordDiff(before, after) {
  const dir = mkdtempSync(join(tmpdir(), 'doc-diff-'));
  try {
    writeFileSync(join(dir, 'a'), before);
    writeFileSync(join(dir, 'b'), after);
    const out = await new Promise(resolve =>
      execFile('git', ['diff', '--no-index', '--no-color', '--word-diff=porcelain', '-U100000', 'a', 'b'], { cwd: dir, maxBuffer: 16 * 1024 * 1024 }, (_e, stdout) =>
        resolve(String(stdout ?? '')),
      ),
    );
    const at = out.indexOf('\n@@');
    return at < 0 ? (before === after ? '' : null) : out.slice(at + 1).trimEnd();
  } catch {
    return null;
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
}

/**
 * Commits revisions in a worktree of the source (docSync): fetch and fast-forward first, then the file, the
 * budget file when a size changed, one commit; a push only when `push`, with up to 3 rebases.
 * commit({ path, text, parentHash, sectionBefore, sectionAfter, message, budgetReason }) → { state, commit, error }.
 */
export function createGitSync({ worktree, push = false, remote = 'origin', branch = 'main', node = process.execPath, author = null } = {}) {
  if (!worktree) return null;
  let queue = Promise.resolve();
  const identity = async () => {
    if (author) return ['-c', `user.name=${author.name}`, '-c', `user.email=${author.email}`];
    const email = await git(worktree, ['config', 'user.email']).catch(() => '');
    return email.trim() ? [] : ['-c', 'user.name=Ithomiini app', '-c', 'user.email=app@ithomiini.invalid'];
  };
  async function once(job) {
    if (!okPath(job.path)) return { state: 'failed', error: 'path' };
    if (!existsSync(worktree)) return { state: 'failed', error: 'no worktree' };
    if (push) {
      await git(worktree, ['fetch', '-q', remote, branch]);
      await git(worktree, ['merge', '-q', '--ff-only', `${remote}/${branch}`]).catch(async () => {
        await git(worktree, ['rebase', '-q', `${remote}/${branch}`]).catch(async e => {
          await git(worktree, ['rebase', '--abort']).catch(() => {});
          throw e;
        });
      });
    }
    const file = join(worktree, job.path);
    const was = existsSync(file) ? readFileSync(file, 'utf8') : null;
    let next;
    if (was === null) return { state: 'failed', error: 'no file' };
    if (hashText(was) === job.parentHash || was === job.text) next = job.text;
    else {
      // The worktree's file changed elsewhere: the section alone, when it is there as it was.
      const at = was.indexOf(job.sectionBefore);
      if (!job.sectionBefore || at < 0 || was.indexOf(job.sectionBefore, at + 1) >= 0) return { state: 'failed', error: 'conflict' };
      next = was.slice(0, at) + job.sectionAfter + was.slice(at + job.sectionBefore.length);
    }
    writeFileSync(file, next);
    const paths = [job.path];
    // The size ratchet: the next deploy's test needs the file to match (a growth with this change's reason).
    if (existsSync(join(worktree, 'scripts', 'context-budget.mjs')) && existsSync(join(worktree, 'assistant', 'context-budget.json'))) {
      await new Promise(resolve =>
        execFile(node, ['scripts/context-budget.mjs', '--write', '--reason', job.budgetReason || job.message], { cwd: worktree, timeout: 120_000 }, () => resolve()),
      );
      if ((await git(worktree, ['status', '--porcelain', '--', 'assistant/context-budget.json'])).trim()) paths.push('assistant/context-budget.json');
    }
    await git(worktree, ['add', '--', ...paths]);
    if (!(await git(worktree, ['diff', '--cached', '--name-only'])).trim()) {
      const head = (await git(worktree, ['rev-parse', 'HEAD'])).trim();
      return { state: push ? 'pushed' : 'committed', commit: head };
    }
    await git(worktree, [...(await identity()), 'commit', '-q', '-m', job.message, '--', ...paths]);
    const commit = (await git(worktree, ['rev-parse', 'HEAD'])).trim();
    if (!push) return { state: 'committed', commit };
    let error = '';
    for (let i = 0; i < 3; i++) {
      try {
        await git(worktree, ['push', '-q', remote, `HEAD:${branch}`]);
        return { state: 'pushed', commit: (await git(worktree, ['rev-parse', 'HEAD'])).trim() };
      } catch (e) {
        error = e.message;
        await git(worktree, ['fetch', '-q', remote, branch]).catch(() => {});
        const rebased = await git(worktree, ['rebase', '-q', `${remote}/${branch}`]).then(() => true, async () => {
          await git(worktree, ['rebase', '--abort']).catch(() => {});
          return false;
        });
        if (!rebased) break;
      }
    }
    return { state: 'failed', commit, error: error || 'push' };
  }
  return {
    worktree,
    push,
    commit(job) {
      const run = queue.then(() => once(job).catch(e => ({ state: 'failed', error: e.message })));
      queue = run.catch(() => {});
      return run;
    },
  };
}

// ------------------------------------------------------------------ the MCP tools

const WRITING =
  'Write the rule or fact as it is now, the smallest change; no history ("we decided", "new rule"), nothing about past mistakes, no capitals or never/always for emphasis.';
export const DOC_TOOLS = [
  {
    type: 'function',
    function: {
      name: 'doc_threads',
      description:
        "The team's open comments on the instructions (#/instrucciones), each with its quote and the section's current source, and the proposed changes with their comments.",
      parameters: { type: 'object', properties: { path: { type: 'string', description: 'Only this file' } } },
    },
  },
  {
    type: 'function',
    function: {
      name: 'reply_doc_thread',
      description: 'Reply in a comment thread or on a change (its id). Short: the answer, or why nothing changes.',
      parameters: {
        type: 'object',
        properties: { id: { type: 'string' }, text: { type: 'string' }, resolve: { type: 'boolean' } },
        required: ['id', 'text'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'propose_doc_change',
      description: `Propose a change to one section of an instructions file, for an admin to apply. \`before\`: exact current source from doc_threads (part or all of the section). ${WRITING}`,
      parameters: {
        type: 'object',
        properties: {
          path: { type: 'string' },
          heading: { type: 'string', description: "The section's heading ('' before the first)" },
          before: { type: 'string' },
          after: { type: 'string' },
          thread_ids: { type: 'array', items: { type: 'string' } },
        },
        required: ['path', 'heading', 'before', 'after'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'revise_doc_change',
      description: `A new version of a proposed change, after a comment on it. \`before\` as in propose_doc_change (default: the last version's). ${WRITING}`,
      parameters: {
        type: 'object',
        properties: { change_id: { type: 'string' }, after: { type: 'string' }, before: { type: 'string' } },
        required: ['change_id', 'after'],
      },
    },
  },
];
export const DOC_TOOL_NAMES = new Set(DOC_TOOLS.map(t => t.function.name));

/** The first message of an «Pedir a Claude» chat. */
export const ASK_TEXT = (n, url) =>
  [
    `Hay ${n === 1 ? '1 comentario abierto' : `${n} comentarios abiertos`} en las instrucciones${url ? ` (${url}/#/instrucciones)` : ''}.`,
    'Léelos con `doc_threads` y sigue el paso 3 de la habilidad `edit-instructions`.',
    'Para cada hilo: `propose_doc_change`, o `reply_doc_thread` con la respuesta o por qué no hace falta cambio. Respuestas cortas.',
  ].join(' ');
/** A comment on a change sent to its chat («Pedir versión nueva»), or a stale change to redo. */
export const REVISE_TEXT = (change, why) =>
  why === 'stale'
    ? `La sección de \`${change.path}\` § ${change.heading || '(inicio)'} cambió desde el cambio ${change.series_id} (v${change.version}). Léelo con \`doc_threads\` y rehazlo con \`revise_doc_change\` sobre el texto actual (paso 3 de \`edit-instructions\`).`
    : `Hay un comentario nuevo sobre el cambio ${change.series_id} (v${change.version}) de \`${change.path}\` § ${change.heading || '(inicio)'}. Léelo con \`doc_threads\` y responde con \`revise_doc_change\` (paso 3 de \`edit-instructions\`), o con \`reply_doc_thread\` si no cambia.`;

// ------------------------------------------------------------------ the service

const parse = (s, fallback) => {
  try {
    return JSON.parse(s);
  } catch {
    return fallback;
  }
};
const clip = (s, n) => String(s ?? '').slice(0, n);
const fail = (code, message, status = 400) => Object.assign(new Error(message), { code, status });

/**
 * The comments and changes. `root`: the release (the shipped files); `git`: createGitSync's, or null;
 * `refresh()`: the workspaces' refresh after an apply (t3-provision --refresh-all); `measure(read)`: the context
 * parts' bytes with a reader (scripts/context-budget.mjs measureParts), null to skip the ratchet; `budget()`: the
 * recorded parts; `chats`: server/t3chats.mjs (whether an ask's chat is still answering).
 */
export function createDocReview({ db, root, git = null, refresh = null, measure = null, budget = null, chats = null, now = () => new Date().toISOString(), log = console.error }) {
  initDocReview(db);
  const shippedText = path => readFileSync(join(root, path), 'utf8');
  const revisionsOf = db.prepare('SELECT * FROM doc_revisions WHERE path = ? ORDER BY id');
  function effective(path) {
    const shipped = shippedText(path);
    return { shipped, ...effectiveFrom(shipped, revisionsOf.all(path)) };
  }
  /** The text in effect of an instructions file (the page, the brief, the workspaces read it). */
  const effectiveText = path => (okPath(path) ? effective(path).text : shippedText(path));
  const read = rel => effectiveText(rel);
  const exists = path => okPath(path) && existsSync(join(root, path));

  const names = new Map();
  const nameOf = id => {
    if (!names.has(id)) names.set(id, db.prepare('SELECT display_name FROM users WHERE id = ?').get(id)?.display_name ?? '—');
    return names.get(id);
  };
  const isAdmin = user => {
    const row = user?.id ? db.prepare('SELECT role, active FROM users WHERE id = ?').get(user.id) : null;
    return !!row?.active && row.role === 'admin';
  };
  const requireAdmin = user => {
    if (!isAdmin(user)) throw fail('FORBIDDEN', 'Only administrators', 403);
  };
  const requireUser = user => {
    if (!user?.id) throw fail('UNAUTHORIZED', 'Sign in', 401);
  };
  const body = text => {
    const b = String(text ?? '').trim();
    if (!b) throw fail('NO_TEXT', 'Write something');
    return b.slice(0, MAX_BODY);
  };

  // ---------------------------------------------------------- views
  const commentView = (c, user) => ({
    id: c.id,
    body: c.deleted_at ? '' : c.body,
    createdAt: c.created_at,
    editedAt: c.edited_at,
    deleted: !!c.deleted_at,
    kind: c.kind,
    author: c.kind === 'person' ? nameOf(c.author) : null,
    own: c.kind === 'person' && c.author === user?.id,
  });
  const commentsOf = (column, id, user) =>
    db.prepare(`SELECT * FROM doc_comments WHERE ${column} = ? ORDER BY created_at, rowid`).all(id).map(c => commentView(c, user));
  function threadView(t, user) {
    return {
      id: t.id,
      path: t.path,
      quote: t.quote,
      prefix: t.prefix,
      suffix: t.suffix,
      offset: t.offset,
      heading: t.heading,
      status: t.status,
      createdAt: t.created_at,
      own: t.created_by === user?.id,
      resolvedAt: t.resolved_at,
      resolvedBy: t.resolved_by ? (t.resolved_by === 'claude' ? null : nameOf(t.resolved_by)) : null,
      resolvedByChange: t.resolved_by_change,
      comments: commentsOf('thread_id', t.id, user),
    };
  }

  /** A change's section now, and whether it still is what the change was made on. */
  function sectionNow(c) {
    try {
      return findSection(read(c.path), c.heading, c.before_part);
    } catch {
      return null;
    }
  }
  const revisionView = r =>
    r && { id: r.id, kind: r.kind, gitState: r.git_state, gitCommit: r.git_commit, gitError: r.git_error, appliedAt: r.applied_at, appliedBy: r.applied_by ? nameOf(r.applied_by) : null, undoneAt: r.undone_at };
  function seriesView(seriesId, user, { checks = true } = {}) {
    const versions = db.prepare('SELECT * FROM doc_changes WHERE series_id = ? ORDER BY version').all(seriesId);
    const c = versions.at(-1);
    if (!c) return null;
    const section = sectionNow(c);
    const current = section?.text ?? null;
    const stale = c.status === 'pending' && (current === null || hashText(current) !== c.base_hash);
    const revision = c.revision_id ? db.prepare('SELECT * FROM doc_revisions WHERE id = ?').get(c.revision_id) : null;
    // Applied, but not in the text in effect (a release changed the file without it): «Volver a proponer».
    const later = c.status === 'applied' && db.prepare("SELECT 1 FROM doc_changes WHERE path = ? AND heading = ? AND status = 'applied' AND decided_at > ? AND series_id <> ?").get(c.path, c.heading, c.decided_at, c.series_id);
    const lost = c.status === 'applied' && !later && !read(c.path).includes(c.after);
    const guard = checks && c.status === 'pending' && !stale ? cachedGuards(c) : null;
    return {
      id: c.id,
      seriesId,
      version: c.version,
      path: c.path,
      heading: c.heading,
      before: c.before,
      after: c.after,
      diff: c.diff,
      threadIds: parse(c.thread_ids, []),
      status: c.status,
      stale,
      lost,
      chat: c.chat_id,
      createdAt: c.created_at,
      decidedAt: c.decided_at,
      decidedBy: c.decided_by ? nameOf(c.decided_by) : null,
      revision: revisionView(revision),
      budget: guard?.budget ?? [],
      checks: guard?.checks ?? [],
      versions: versions.map(v => ({ id: v.id, version: v.version, before: v.before, after: v.after, diff: v.diff, createdAt: v.created_at, status: v.status })),
      comments: commentsOf('series_id', seriesId, user),
    };
  }

  /** The guards with a change in: the checks' failures and the parts that grow. */
  function guards(c, sectionText = null) {
    const text = read(c.path);
    const section = sectionText ?? findSection(text, c.heading, c.before_part);
    if (!section) return { checks: [{ code: 'stale' }], budget: [] };
    const changed = text.slice(0, section.start) + c.after + text.slice(section.end);
    const withChange = rel => (rel === c.path ? changed : read(rel));
    let checks;
    try {
      checks = docChecks(c.path, withChange, root);
    } catch (e) {
      checks = [{ code: 'check_failed', text: clip(e.message, 200) }];
    }
    return { checks, budget: measured(withChange), changed, section };
  }
  // The page asks often: a pending change's guards once per state of the overlay.
  const guardCache = new Map();
  function cachedGuards(c) {
    const key = `${c.id}:${db.prepare('SELECT max(id) AS m FROM doc_revisions').get()?.m ?? 0}`;
    if (!guardCache.has(key)) {
      if (guardCache.size > 200) guardCache.clear();
      const { checks, budget: grows } = guards(c);
      guardCache.set(key, { checks, budget: grows });
    }
    return guardCache.get(key);
  }
  // The ratchet measured once per state of the overlay.
  let measuredCache = { key: '', now: null };
  function measured(withChange) {
    if (!measure) return [];
    try {
      const key = String(db.prepare('SELECT max(id) AS m FROM doc_revisions').get()?.m ?? 0);
      if (measuredCache.key !== key) measuredCache = { key, now: measure(read) };
      return budgetGrowth(budget?.() ?? {}, measuredCache.now, measure(withChange));
    } catch (e) {
      log('Context budget on a doc change:', e.message);
      return [];
    }
  }

  function state(user) {
    const threads = db.prepare("SELECT * FROM doc_threads WHERE status <> 'deleted' ORDER BY created_at").all().map(t => threadView(t, user));
    const series = db.prepare("SELECT series_id FROM doc_changes GROUP BY series_id ORDER BY min(created_at)").all();
    const changes = series.map(s => seriesView(s.series_id, user)).filter(c => c && c.status !== 'discarded' && c.status !== 'superseded');
    const counts = {};
    for (const t of threads) if (t.status === 'open') counts[t.path] = (counts[t.path] ?? 0) + 1;
    for (const c of changes) if (c.status === 'pending') counts[c.path] = (counts[c.path] ?? 0) + 1;
    return { admin: isAdmin(user), threads, changes, counts, ask: askState() };
  }

  // ---------------------------------------------------------- threads and comments
  function addComment({ thread = null, series = null, author, kind = 'person', text }) {
    const id = randomUUID();
    db.prepare('INSERT INTO doc_comments (id, thread_id, series_id, author, kind, body, created_at) VALUES (?,?,?,?,?,?,?)').run(id, thread, series, author, kind, text, now());
    return id;
  }
  const threadRow = id => db.prepare("SELECT * FROM doc_threads WHERE id = ? AND status <> 'deleted'").get(String(id ?? ''));
  function createThread(user, input) {
    requireUser(user);
    const path = String(input?.path ?? '');
    if (!exists(path)) throw fail('BAD_PATH', 'Not an instructions file');
    const quote = clip(input?.quote, 2000).trim();
    if (!quote) throw fail('NO_QUOTE', 'Select some text');
    const text = body(input?.body);
    const id = randomUUID();
    db.prepare('INSERT INTO doc_threads (id, path, quote, prefix, suffix, offset, heading, created_by, created_at) VALUES (?,?,?,?,?,?,?,?,?)').run(
      id,
      path,
      quote,
      clip(input?.prefix, 64),
      clip(input?.suffix, 64),
      Number.isFinite(Number(input?.offset)) ? Number(input.offset) : -1,
      clip(input?.heading, 300),
      user.id,
      now(),
    );
    addComment({ thread: id, author: user.id, text });
    return threadView(threadRow(id), user);
  }
  function reply(user, threadId, text) {
    requireUser(user);
    const t = threadRow(threadId);
    if (!t) throw fail('NOT_FOUND', 'No such thread', 404);
    addComment({ thread: t.id, author: user.id, text: body(text) });
    return threadView(threadRow(t.id), user);
  }
  function setStatus(user, threadId, status) {
    requireUser(user);
    const t = threadRow(threadId);
    if (!t) throw fail('NOT_FOUND', 'No such thread', 404);
    if (status === 'resolved') db.prepare("UPDATE doc_threads SET status = 'resolved', resolved_by = ?, resolved_at = ? WHERE id = ?").run(user.id, now(), t.id);
    else db.prepare("UPDATE doc_threads SET status = 'open', resolved_by = NULL, resolved_at = NULL, resolved_by_change = NULL WHERE id = ?").run(t.id);
    return threadView(threadRow(t.id), user);
  }
  const ownComment = (user, id) => {
    requireUser(user);
    const c = db.prepare('SELECT * FROM doc_comments WHERE id = ? AND deleted_at IS NULL').get(String(id ?? ''));
    if (!c) throw fail('NOT_FOUND', 'No such comment', 404);
    if (c.kind !== 'person' || c.author !== user.id) throw fail('FORBIDDEN', 'Only your own comments', 403);
    return c;
  };
  function editComment(user, id, text) {
    const c = ownComment(user, id);
    db.prepare('UPDATE doc_comments SET body = ?, edited_at = ? WHERE id = ?').run(body(text), now(), c.id);
    return { ok: true };
  }
  function deleteComment(user, id) {
    const c = ownComment(user, id);
    db.prepare("UPDATE doc_comments SET deleted_at = ?, body = '' WHERE id = ?").run(now(), c.id);
    // A thread whose comments are all gone goes too.
    if (c.thread_id && !db.prepare('SELECT 1 FROM doc_comments WHERE thread_id = ? AND deleted_at IS NULL').get(c.thread_id))
      db.prepare("UPDATE doc_threads SET status = 'deleted' WHERE id = ?").run(c.thread_id);
    return { ok: true };
  }

  // ---------------------------------------------------------- changes
  const changeRow = id => db.prepare('SELECT * FROM doc_changes WHERE id = ? OR (series_id = ? AND version = (SELECT max(version) FROM doc_changes WHERE series_id = ?))').get(id, id, id);
  const latestOf = c => db.prepare('SELECT * FROM doc_changes WHERE series_id = ? ORDER BY version DESC LIMIT 1').get(c.series_id);

  /** A new change or version: `before` checked against the section now. { change } or { error }. */
  async function draft({ path, heading, before, after, threadIds = [], series = null, version = 1, chat = null, by = null }) {
    if (!exists(path)) return { error: `Not an instructions file: ${path}. Only assistant/**/*.md.` };
    const text = read(path);
    const section = findSection(text, heading, before);
    if (!section) return { error: `No section "${heading}" in ${path}: use the heading as doc_threads gives it.` };
    const part = String(before ?? '');
    const changedSection = replacePart(section.text, part, String(after ?? ''));
    if (changedSection === null)
      return { error: `before does not match the current source of § ${heading || '(start)'} exactly once. Copy it from doc_threads' section.` };
    if (section.start === 0 && bodyStart(text) > 0) return { error: 'The front matter is not changed here.' };
    if (changedSection === section.text) return { error: 'after is the same as before.' };
    if (changedSection.length > MAX_TEXT) return { error: 'The section is too long.' };
    const known = new Set(db.prepare('SELECT id FROM doc_threads').all().map(t => t.id));
    const ids = [...new Set((threadIds ?? []).map(String))].filter(id => known.has(id));
    const id = randomUUID();
    const seriesId = series ?? id;
    const diff = await wordDiff(section.text, changedSection);
    db.prepare(
      `INSERT INTO doc_changes (id, series_id, version, path, heading, base_hash, before_part, after_part, before, after, diff, thread_ids, chat_id, created_by, created_at)
       VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)`,
    ).run(id, seriesId, version, path, section.heading, hashText(section.text), part, String(after ?? ''), section.text, changedSection, diff, JSON.stringify(ids), chat, by, now());
    if (series) db.prepare("UPDATE doc_changes SET status = 'superseded' WHERE series_id = ? AND id <> ? AND status = 'pending'").run(seriesId, id);
    const c = db.prepare('SELECT * FROM doc_changes WHERE id = ?').get(id);
    const g = guards(c);
    return { change: c, checks: g.checks, budget: g.budget };
  }

  async function propose(args, { user, chat }) {
    if (!isAdmin(user)) return { error: "Only an administrator's chat proposes changes to the instructions." };
    const out = await draft({ path: String(args.path ?? ''), heading: String(args.heading ?? ''), before: args.before, after: args.after, threadIds: args.thread_ids, chat: chat ?? lastAskChat(), by: user.id });
    if (out.error) return out;
    if (out.checks.length) {
      db.prepare("UPDATE doc_changes SET status = 'discarded' WHERE id = ?").run(out.change.id);
      return { error: `Refused by the checks: ${JSON.stringify(out.checks)}` };
    }
    return answer(out);
  }
  async function revise(args, { user, chat }) {
    if (!isAdmin(user)) return { error: "Only an administrator's chat revises changes to the instructions." };
    const prev = changeRow(String(args.change_id ?? ''));
    if (!prev) return { error: 'No such change' };
    const last = latestOf(prev);
    if (!['pending', 'superseded'].includes(last.status) && last.status !== 'discarded')
      return { error: `The change is ${last.status}: propose a new one with propose_doc_change.` };
    const out = await draft({
      path: last.path,
      heading: last.heading,
      before: args.before ?? last.before_part,
      after: args.after,
      threadIds: parse(last.thread_ids, []),
      series: last.series_id,
      version: last.version + 1,
      chat: chat ?? last.chat_id,
      by: user.id,
    });
    if (out.error) return out;
    if (out.checks.length) {
      db.prepare('DELETE FROM doc_changes WHERE id = ?').run(out.change.id);
      db.prepare("UPDATE doc_changes SET status = 'pending' WHERE id = ?").run(last.id);
      return { error: `Refused by the checks: ${JSON.stringify(out.checks)}` };
    }
    return answer(out);
  }
  const answer = ({ change, budget: grows }) => ({
    changeId: change.series_id,
    version: change.version,
    ...(grows.length ? { grows: grows.map(g => `${g.part} +${g.bytes} bytes: an admin applies it anyway or not`) } : {}),
  });

  const pendingChange = id => {
    const c = changeRow(String(id ?? ''));
    if (!c) throw fail('NOT_FOUND', 'No such change', 404);
    return latestOf(c);
  };
  function discard(user, id) {
    requireAdmin(user);
    const c = pendingChange(id);
    if (c.status !== 'pending') throw fail('NOT_PENDING', 'The change is not pending', 409);
    db.prepare("UPDATE doc_changes SET status = 'discarded', decided_by = ?, decided_at = ? WHERE id = ?").run(user.id, now(), c.id);
    return { ok: true };
  }

  /** A revision of `path` to `text` (section `heading` from `before` to `after`), then git and the workspaces. */
  function writeRevision({ path, text, heading, before, after, changeId, kind, user, message }) {
    const { hash } = effective(path);
    const info = db
      .prepare('INSERT INTO doc_revisions (path, parent_hash, hash, text, change_id, kind, heading, section_before, section_after, applied_by, applied_at, git_state) VALUES (?,?,?,?,?,?,?,?,?,?,?,?)')
      .run(path, hash, hashText(text), text, changeId, kind, heading, before, after, user.id, now(), git ? 'pending' : 'off');
    const revisionId = Number(info.lastInsertRowid);
    const synced = sync(revisionId, message);
    refreshSoon();
    return { revisionId, synced };
  }
  const subject = (path, heading) => `Instrucciones: ${path.replace(/^assistant\//, '')} § ${heading || '(inicio)'}`;
  function sync(revisionId, message) {
    if (!git) return Promise.resolve({ state: 'off' });
    const r = db.prepare('SELECT * FROM doc_revisions WHERE id = ?').get(revisionId);
    db.prepare("UPDATE doc_revisions SET git_state = 'pending', git_error = NULL WHERE id = ?").run(revisionId);
    const job = git.commit({
      path: r.path,
      text: r.text,
      parentHash: r.parent_hash,
      sectionBefore: r.section_before,
      sectionAfter: r.section_after,
      message: message ?? subject(r.path, r.heading),
      budgetReason: `${subject(r.path, r.heading)} (doc change ${r.change_id ?? r.id})`,
    });
    return job.then(out => {
      db.prepare('UPDATE doc_revisions SET git_state = ?, git_commit = coalesce(?, git_commit), git_error = ? WHERE id = ?').run(out.state, out.commit ?? null, out.error ?? null, revisionId);
      if (out.state === 'failed') log(`Doc sync of revision ${revisionId}: ${out.error}`);
      return out;
    });
  }
  let refreshing = null;
  let again = false;
  function refreshSoon() {
    if (!refresh) return;
    if (refreshing) {
      again = true;
      return;
    }
    refreshing = Promise.resolve()
      .then(() => refresh())
      .catch(e => log('Workspaces refresh after a doc change:', e.message))
      .finally(() => {
        refreshing = null;
        if (again) {
          again = false;
          refreshSoon();
        }
      });
  }

  function apply(user, id, { anyway = false } = {}) {
    requireAdmin(user);
    const c = pendingChange(id);
    if (!['pending', 'undone'].includes(c.status)) throw fail('NOT_PENDING', 'The change is not pending', 409);
    const text = read(c.path);
    const section = findSection(text, c.heading, c.before_part);
    if (!section || hashText(section.text) !== hashText(c.before)) throw fail('STALE', 'The section changed since the proposal', 409);
    const g = guards(c, section);
    if (g.checks.length) throw Object.assign(fail('CHECKS', 'The checks refuse this change', 409), { details: { checks: g.checks } });
    if (g.budget.length && !anyway) throw Object.assign(fail('GROWS', 'The change grows the chats\' fixed context', 409), { details: { budget: g.budget } });
    const { revisionId, synced } = writeRevision({ path: c.path, text: g.changed, heading: c.heading, before: c.before, after: c.after, changeId: c.id, kind: 'apply', user, message: subject(c.path, c.heading) });
    db.prepare("UPDATE doc_changes SET status = 'applied', decided_by = ?, decided_at = ?, revision_id = ? WHERE id = ?").run(user.id, now(), revisionId, c.id);
    for (const t of parse(c.thread_ids, [])) {
      const row = threadRow(t);
      if (!row || row.status !== 'open') continue;
      db.prepare("UPDATE doc_threads SET status = 'resolved', resolved_by = ?, resolved_at = ?, resolved_by_change = ? WHERE id = ?").run(user.id, now(), c.series_id, t);
      addComment({ thread: t, author: 'system', kind: 'system', text: 'resolved_by_change' });
    }
    return { change: seriesView(c.series_id, user), synced };
  }

  /** Undo: the section back to its before-text when it still is the after-text; otherwise a new proposal that does it. */
  async function undo(user, id) {
    requireAdmin(user);
    const c = pendingChange(id);
    if (c.status !== 'applied') throw fail('NOT_APPLIED', 'The change is not applied', 409);
    const text = read(c.path);
    const section = findSection(text, c.heading, c.after_part) ?? findSection(text, c.heading);
    if (!section || section.text !== c.after) {
      const out = section ? await draft({ path: c.path, heading: c.heading, before: c.after_part, after: c.before_part, by: user.id }) : { error: 'gone' };
      if (out.error) throw fail('UNDO_CONFLICT', 'The section changed since: the undo cannot be made', 409);
      return { proposed: seriesView(out.change.series_id, user) };
    }
    const changed = text.slice(0, section.start) + c.before + text.slice(section.end);
    const { revisionId, synced } = writeRevision({ path: c.path, text: changed, heading: c.heading, before: c.after, after: c.before, changeId: c.id, kind: 'undo', user, message: `${subject(c.path, c.heading)} (deshecho)` });
    db.prepare('UPDATE doc_revisions SET undone_by = ?, undone_at = ? WHERE id = ?').run(user.id, now(), c.revision_id);
    db.prepare("UPDATE doc_changes SET status = 'undone', decided_by = ?, decided_at = ?, revision_id = ? WHERE id = ?").run(user.id, now(), revisionId, c.id);
    return { change: seriesView(c.series_id, user), synced };
  }

  /** «Volver a proponer»: an applied change the text in effect lost, as a new version on the section now. */
  async function repropose(user, id) {
    requireAdmin(user);
    const c = pendingChange(id);
    const out = await draft({ path: c.path, heading: c.heading, before: findSection(read(c.path), c.heading)?.text ?? c.before, after: c.after, threadIds: parse(c.thread_ids, []), series: c.series_id, version: c.version + 1, chat: c.chat_id, by: user.id });
    if (out.error) throw fail('REPROPOSE', out.error, 409);
    return { change: seriesView(c.series_id, user) };
  }

  function commentChange(user, id, text) {
    requireUser(user);
    const c = pendingChange(id);
    addComment({ series: c.series_id, author: user.id, text: body(text) });
    return seriesView(c.series_id, user);
  }

  async function retryGit(user, revisionId) {
    requireAdmin(user);
    const r = db.prepare('SELECT * FROM doc_revisions WHERE id = ?').get(Number(revisionId));
    if (!r) throw fail('NOT_FOUND', 'No such revision', 404);
    return sync(r.id, r.kind === 'undo' ? `${subject(r.path, r.heading)} (deshecho)` : subject(r.path, r.heading));
  }

  // ---------------------------------------------------------- asks
  const lastAsk = () => db.prepare('SELECT * FROM doc_asks ORDER BY asked_at DESC, rowid DESC LIMIT 1').get();
  const lastAskChat = () => lastAsk()?.chat_id ?? null;
  function askState() {
    const a = lastAsk();
    if (!a?.chat_id) return { state: 'idle', chat: null, at: a?.asked_at ?? null };
    const s = chats?.session?.(a.chat_id);
    return { state: s?.busy ? 'running' : s ? 'finished' : 'gone', chat: a.chat_id, at: a.asked_at };
  }
  function recordAsk(user, chat, kind, { threads = [], changes = [] } = {}) {
    db.prepare('INSERT INTO doc_asks (id, asked_by, asked_at, chat_id, kind, thread_ids, change_ids) VALUES (?,?,?,?,?,?,?)').run(randomUUID(), user.id, now(), chat, kind, JSON.stringify(threads), JSON.stringify(changes));
  }
  /** Open threads no pending change answers yet (what «Pedir a Claude» sends). */
  function openThreads() {
    const covered = new Set(db.prepare("SELECT thread_ids FROM doc_changes WHERE status = 'pending'").all().flatMap(c => parse(c.thread_ids, [])));
    return db.prepare("SELECT id FROM doc_threads WHERE status = 'open'").all().map(t => t.id).filter(id => !covered.has(id));
  }

  // ---------------------------------------------------------- MCP
  function toolThreads(args, user) {
    const only = args.path ? String(args.path) : null;
    const threads = db
      .prepare("SELECT * FROM doc_threads WHERE status = 'open' ORDER BY path, created_at")
      .all()
      .filter(t => !only || t.path === only)
      .map(t => {
        const s = threadSection(read, t.path, t);
        return {
          id: t.id,
          path: s?.path ?? t.path,
          heading: s?.heading ?? t.heading,
          quote: t.quote,
          ...(s ? (s.quoteFound ? {} : { textChanged: true }) : { textChanged: true }),
          section: s?.text ?? null,
          comments: commentsOf('thread_id', t.id, user).filter(c => !c.deleted).map(c => ({ by: c.kind === 'claude' ? 'Claude' : c.kind === 'system' ? 'app' : c.author, text: c.body })),
        };
      });
    const changes = db
      .prepare("SELECT series_id FROM doc_changes GROUP BY series_id HAVING max(CASE WHEN status = 'pending' THEN 1 ELSE 0 END) = 1")
      .all()
      .map(s => seriesView(s.series_id, user, { checks: false }))
      .filter(c => c && (!only || c.path === only))
      .map(c => ({
        id: c.seriesId,
        version: c.version,
        path: c.path,
        heading: c.heading,
        ...(c.stale ? { stale: true, section: sectionNow({ path: c.path, heading: c.heading, before_part: '' })?.text ?? null } : {}),
        before: c.versions.at(-1)?.before,
        after: c.after,
        comments: c.comments.filter(x => !x.deleted).map(x => ({ by: x.kind === 'claude' ? 'Claude' : x.author, text: x.body })),
      }));
    return { threads, changes };
  }
  function toolReply(args, user) {
    if (!isAdmin(user)) return { error: "Only an administrator's chat replies here." };
    const text = clip(String(args.text ?? '').trim(), MAX_BODY);
    if (!text) return { error: 'text is empty' };
    const id = String(args.id ?? '');
    const t = threadRow(id);
    if (t) {
      addComment({ thread: t.id, author: 'claude', kind: 'claude', text });
      if (args.resolve) db.prepare("UPDATE doc_threads SET status = 'resolved', resolved_by = 'claude', resolved_at = ? WHERE id = ?").run(now(), t.id);
      return { ok: true };
    }
    const c = changeRow(id);
    if (!c) return { error: 'No such thread or change' };
    addComment({ series: c.series_id, author: 'claude', kind: 'claude', text });
    return { ok: true };
  }
  async function runTool(name, args = {}, { user, chat = null } = {}) {
    if (name === 'doc_threads') return toolThreads(args, user);
    if (name === 'reply_doc_thread') return toolReply(args, user);
    if (name === 'propose_doc_change') return propose(args, { user, chat });
    if (name === 'revise_doc_change') return revise(args, { user, chat });
    return { error: `Unknown tool ${name}` };
  }

  /** The history tab's rows of a file: its revisions, newest first. */
  function revisions(path) {
    return db
      .prepare('SELECT * FROM doc_revisions WHERE path = ? ORDER BY id DESC')
      .all(path)
      .map(r => ({ revision: r.id, kind: r.kind, date: r.applied_at, author: r.applied_by ? nameOf(r.applied_by) : '', heading: r.heading, gitState: r.git_state }));
  }
  async function revisionDiff(path, id) {
    const r = db.prepare('SELECT * FROM doc_revisions WHERE path = ? AND id = ?').get(path, Number(id));
    if (!r) return null;
    return { revision: r.id, diff: (await wordDiff(r.section_before ?? '', r.section_after ?? '')) ?? '' };
  }

  return {
    effectiveText,
    read,
    state,
    createThread,
    reply,
    resolve: (user, id) => setStatus(user, id, 'resolved'),
    reopen: (user, id) => setStatus(user, id, 'open'),
    editComment,
    deleteComment,
    commentChange,
    apply,
    discard,
    undo,
    repropose,
    retryGit,
    runTool,
    revisions,
    revisionDiff,
    isAdmin,
    openThreads,
    recordAsk,
    askState,
    lastAskChat,
    change: id => {
      const c = changeRow(String(id ?? ''));
      return c ? latestOf(c) : null;
    },
    view: (id, user) => {
      const c = changeRow(String(id ?? ''));
      return c ? seriesView(c.series_id, user) : null;
    },
    /** For tests: the refresh and git jobs started. */
    idle: async () => {
      while (refreshing) await refreshing;
    },
  };
}
