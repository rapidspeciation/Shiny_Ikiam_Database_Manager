// The assistant's instructions as the team reads them in the app (#/instrucciones):
// the brief (assistant/AGENTS.md, filled in for a generic person as each T3
// workspace gets it: server/brief.mjs), the skills with their reference files,
// the Claude Code subagents and the MCP tools, each with its change history from git.
//
// History: a git checkout (the PC lab, a developer's copy) reads it from git,
// again whenever HEAD moves. A release has no .git (scripts/deploy.sh ships a
// tarball): `npm run build` writes instructions-history.json beside this file
// (scripts/instructions-history.mjs) and the release reads that.

import { execFile } from 'node:child_process';
import { existsSync, readFileSync, readdirSync, statSync, writeFileSync } from 'node:fs';
import { dirname, join, relative } from 'node:path';
import { fileURLToPath } from 'node:url';
import { GENERIC_PERSON, SERVER_PATHS, composeBrief } from './brief.mjs';

const here = dirname(fileURLToPath(import.meta.url));
export const REPO_ROOT = join(here, '..');
export const HISTORY_FILE = join(here, 'instructions-history.json');

/**
 * Where the tool descriptions are written: their history is the history of
 * these blocks (git log -L: only commits that changed them, only those lines).
 */
export const TOOL_RANGES = [
  ['server/assistant.mjs', '/^const VALUES_DOC/', '/^\\];/'],
  ['server/records-tool.mjs', '/^export const FILTERS_DOC/', '/^\\];/'],
  ['server/notebook-tool.mjs', '/^export const MATCH_NOTEBOOK_TOOL/', '/^};/'],
  ['server/history.mjs', '/^const selectionProps/', '/^\\];/'],
  ['server/knowledge.mjs', '/^export const KNOWLEDGE_TOOLS/', '/^\\];/'],
];
/** Characters of one commit's diff kept (a whole-file rewrite stays readable, not endless). */
const DIFF_LIMIT = 150_000;

const files = dir =>
  readdirSync(dir, { withFileTypes: true })
    .filter(d => !d.name.startsWith('.') && d.name !== '__pycache__')
    .flatMap(d => (d.isDirectory() ? files(join(dir, d.name)) : [join(dir, d.name)]))
    .sort();

/** A Markdown file's front matter (`key: value` lines between ---) and its body. */
export function frontMatter(text) {
  const m = /^---\n([\s\S]*?)\n---\n?/.exec(text);
  if (!m) return { meta: null, body: text };
  const meta = {};
  for (const line of m[1].split('\n')) {
    const kv = /^([\w-]+):\s*(.*)$/.exec(line);
    if (kv) meta[kv[1]] = kv[2];
  }
  return { meta, body: text.slice(m[0].length).replace(/^\n+/, '') };
}

/**
 * Every entry of the page, in its order: the brief, each skill (SKILL.md, then its other files), the subagents, the tools.
 * `history` says what git follows for it: one path (with renames), paths, or line ranges.
 */
export function instructionEntries(root = REPO_ROOT) {
  const rel = path => relative(root, path);
  const entries = [
    { id: 'assistant/AGENTS.md', group: 'brief', kind: 'markdown', title: 'AGENTS.md', history: { follow: 'assistant/AGENTS.md' } },
  ];
  const skills = join(root, 'assistant', 'skills');
  for (const name of existsSync(skills) ? readdirSync(skills).sort() : []) {
    const dir = join(skills, name);
    if (!statSync(dir).isDirectory()) continue;
    const all = files(dir);
    const main = all.find(f => rel(f) === rel(join(dir, 'SKILL.md')));
    for (const file of [main, ...all.filter(f => f !== main)].filter(Boolean)) {
      const id = rel(file);
      entries.push({
        id,
        group: 'skills',
        skill: name,
        kind: file.endsWith('.md') ? 'markdown' : 'code',
        title: file === main ? name : relative(dir, file),
        history: { follow: id },
      });
    }
  }
  const agents = join(root, 'assistant', 'agents');
  for (const file of existsSync(agents) ? files(agents).filter(f => f.endsWith('.md')) : []) {
    const id = rel(file);
    entries.push({ id, group: 'agents', kind: 'markdown', title: relative(agents, file).replace(/\.md$/, ''), history: { follow: id } });
  }
  entries.push({ id: 'tools', group: 'tools', kind: 'tools', title: 'MCP tools', history: { ranges: TOOL_RANGES } });
  return entries;
}

// ------------------------------------------------------------------ git

const run = (root, args) =>
  new Promise((resolve, reject) =>
    execFile('git', ['-C', root, ...args], { maxBuffer: 256 * 1024 * 1024, timeout: 60_000 }, (error, stdout) =>
      error ? reject(error) : resolve(String(stdout)),
    ),
  );
/** A git checkout (a worktree's .git is a file): not a release folder. */
export const isCheckout = root => existsSync(join(root, '.git'));

const FORMAT = '--format=%x1e%H%x1f%aI%x1f%an%x1f%s';
/**
 * Commits of `git log` with FORMAT and a word diff (--word-diff=porcelain:
 * a line per run of words, +/- for added and removed, ~ for a line break).
 */
export function parseLog(text) {
  return text
    .split('\x1e')
    .slice(1)
    .map(chunk => {
      const newline = chunk.indexOf('\n');
      const [commit, date, author, subject] = (newline < 0 ? chunk : chunk.slice(0, newline)).split('\x1f');
      let diff = newline < 0 ? '' : chunk.slice(newline + 1).replace(/^\n+/, '').replace(/^index .*\n/gm, '').trimEnd();
      if (diff.length > DIFF_LIMIT) diff = `${diff.slice(0, diff.lastIndexOf('\n', DIFF_LIMIT))}\n\\ (cut: the change is too long to show)`;
      return { commit, date, author, subject, diff };
    });
}

/** One entry's commits, newest first. */
async function entryHistory(root, entry) {
  const h = entry.history;
  const base = ['log', '--no-color', '--word-diff=porcelain', FORMAT];
  if (h.follow) return parseLog(await run(root, [...base, '-p', '--follow', '--', h.follow]));
  if (h.paths) return parseLog(await run(root, [...base, '-p', '--', ...h.paths]));
  const ranges = h.ranges.flatMap(([file, from, to]) => ['-L', `${from},${to}:${file}`]);
  return parseLog(await run(root, [...base, ...ranges]));
}

/** Every entry's history from git: { head, generatedAt, entries: { id: [commits] } }. */
export async function gitHistory(root = REPO_ROOT) {
  const head = (await run(root, ['rev-parse', 'HEAD'])).trim();
  const entries = {};
  for (const entry of instructionEntries(root)) entries[entry.id] = await entryHistory(root, entry);
  return { head, generatedAt: new Date().toISOString(), entries };
}

/** Writes the history a release ships (scripts/instructions-history.mjs, from `npm run build`). */
export async function writeHistory(root = REPO_ROOT, file = HISTORY_FILE) {
  const history = await gitHistory(root);
  writeFileSync(file, JSON.stringify(history));
  return history;
}

// ------------------------------------------------------------------ the page

/**
 * The instructions page's data. `tools`: the MCP tool list exactly as
 * tools/list gives it (server/assistant.mjs). The history comes from git in a
 * checkout (recomputed when HEAD moves), else from the release's file.
 */
export function createInstructions({ root = REPO_ROOT, tools = () => [], historyFile = HISTORY_FILE } = {}) {
  let cached = null; // { head, promise }
  let shipped = null; // { mtime, data }

  function fromFile() {
    try {
      const mtime = statSync(historyFile).mtimeMs;
      if (shipped?.mtime !== mtime) shipped = { mtime, data: JSON.parse(readFileSync(historyFile, 'utf8')) };
      return { ...shipped.data, source: 'release' };
    } catch {
      return null;
    }
  }
  async function history() {
    if (isCheckout(root)) {
      try {
        const head = (await run(root, ['rev-parse', 'HEAD'])).trim();
        if (cached?.head !== head) {
          const promise = gitHistory(root).then(h => ({ ...h, source: 'git' }));
          cached = { head, promise };
          promise.catch(() => (cached = null));
        }
        return await cached.promise;
      } catch {
        /* git is missing or failed: the shipped file, if any. */
      }
    }
    return fromFile();
  }

  function content(entry) {
    // The brief as a workspace on the server gets it, for a generic person.
    if (entry.id === 'assistant/AGENTS.md') return { meta: null, content: composeBrief(GENERIC_PERSON, { ...SERVER_PATHS, root }) };
    if (entry.kind === 'tools') return { tools: tools() };
    const text = readFileSync(join(root, entry.id), 'utf8');
    if (entry.kind !== 'markdown') return { content: text };
    const { meta, body } = frontMatter(text);
    return { meta, content: body };
  }

  /** Every entry with its text and its commits (without their diffs). */
  async function list() {
    const h = await history();
    return {
      historySource: h?.source ?? null,
      historyHead: h?.head ?? null,
      entries: instructionEntries(root).map(({ history: _, ...entry }) => {
        const commits = h?.entries?.[entry.id] ?? null;
        return {
          ...entry,
          ...content(entry),
          lastChanged: commits?.[0]?.date ?? null,
          history: commits?.map(({ diff: _diff, ...c }) => c) ?? null,
        };
      }),
    };
  }

  /** One commit's word diff of one entry. */
  async function diff(id, commit) {
    const found = (await history())?.entries?.[id]?.find(c => c.commit === commit);
    return found ? { commit: found.commit, diff: found.diff } : null;
  }

  return { list, diff, history };
}
