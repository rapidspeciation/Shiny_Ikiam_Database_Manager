#!/usr/bin/env node
// The project's Gmail, read-only, for the local test lab's chats and the assistant eval (16 Nov 2026: "can we at
// least allow read access? … specially for testing prompts in the lab").
//
// A Claude Code PreToolUse hook on Bash (scripts/lab-guard.mjs puts it in every lab workspace; the eval's workspace
// copies the lab's settings). Hooks run on this PC outside the Bash sandbox, so:
//  - the sandbox keeps gog's binary, its keyring and its OAuth files out of the chat's reach (lab-guard.mjs): the
//    project account's token can send mail (gmail.modify), and Google gives no read-only token from that grant;
//  - this hook runs a gog Gmail read itself, here, with gog's own guards (--readonly, --gmail-no-send, only these
//    commands enabled) on the project account, and hands the chat its output (`cat` of a file the sandbox reads
//    but cannot write, ~/.local/state/ithomiini-lab/gmail-out);
//  - any other gog command (send, reply, drafts, labels changes, trash, Drive, Calendar…) is refused with a reason.
// A command that does not run gog passes untouched.
// The hook runs from a read-only copy outside the sandbox's writable folders (lab-guard.mjs installs it), and it
// imports nothing but Node's own modules, so a chat cannot change what runs on the host.
//
// Reads:  gmail search | messages search | get | thread get | labels list | labels get   (and gog's aliases)
// Shape:  [set -a; . ~/.config/ithomiini/gog.env; set +a]  gog <flags> gmail <read> <args>  [2>&1] [| <filter>…]
//         The filter after `|` runs in the sandbox on the output; a second gog there finds no gog to run.
import { spawnSync } from 'node:child_process';
import { closeSync, constants, existsSync, mkdirSync, mkdtempSync, openSync, readdirSync, readFileSync, rmSync, statSync, writeFileSync, writeSync } from 'node:fs';
import { homedir } from 'node:os';
import { basename, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const HOME = homedir();
// The account of the google-account skill (assistant/skills/google-account/SKILL.md) and its OAuth client.
export const ACCOUNT = process.env.ITHOMIINI_LAB_GMAIL_ACCOUNT || 'jmithominii@gmail.com';
export const CLIENT = process.env.ITHOMIINI_LAB_GMAIL_CLIENT || 'ithomiini';
const GOG = process.env.ITHOMIINI_LAB_GMAIL_GOG || join(HOME, '.local', 'bin', 'gog');
const PASSWORD_FILE = process.env.ITHOMIINI_LAB_GMAIL_PASSWORD_FILE || join(HOME, '.config', 'gogcli', '.hermes-keyring-password');
// The output: a folder the sandbox reads but cannot write (not /tmp: a chat could swap the folder for a link while gog runs).
export const OUT_DEFAULT = join(HOME, '.local', 'state', 'ithomiini-lab', 'gmail-out');
const OUT_ROOT = process.env.ITHOMIINI_LAB_GMAIL_TMP || OUT_DEFAULT;
// Outside every folder the lab's sandbox may write (a link planted there would make this hook write elsewhere).
const LOG = process.env.ITHOMIINI_LAB_GMAIL_LOG || join(HOME, '.local', 'state', 'ithomiini-lab', 'gmail-reads.log');

// Canonical read command → gog's names for each word (gog gmail --help, v0.45).
const SERVICE = ['gmail', 'mail', 'email'];
const READS = [
  { path: ['search'], words: [['search', 'find', 'query', 'ls', 'list']] },
  { path: ['messages', 'search'], words: [['messages', 'message', 'msg', 'msgs'], ['search', 'find', 'query', 'ls', 'list']] },
  { path: ['get'], words: [['get', 'info', 'show']] },
  { path: ['thread', 'get'], words: [['thread', 'threads', 'read'], ['get', 'info', 'show']] },
  { path: ['labels', 'list'], words: [['labels', 'label'], ['list', 'ls']] },
  { path: ['labels', 'get'], words: [['labels', 'label'], ['get', 'info', 'show']] },
];
const ENABLED = READS.map(r => ['gmail', ...r.path].join('.')).join(',');
// Flags a read may carry: name → takes a value. Not --download / --out-dir (files written outside the sandbox),
// --access-token, --home, --enable-commands…, --force, --from-contact (another Google service).
const FLAGS = {
  '--readonly': false, '--gmail-no-send': false, '--no-input': false, '--json': false, '-j': false, '--plain': false, '-p': false,
  '--results-only': false, '--wrap-untrusted': false, '--select': true, '--color': true, '--account': true, '-a': true, '--client': true,
  '--max': true, '--page': true, '--all': false, '--count': false, '--oldest': false, '--timezone': true, '-z': true,
  '--local': false, '--include-body': false, '--body-format': true, '--full': false, '--include-attachments': false,
  '--include-recipients': false, '--sanitize-content': false, '--use-indexed-attachment-ids': false, '--format': true, '--headers': true,
  '--verbose': false, '-v': false, '--fields': true, '--help': false, '-h': false,
};

const REFUSE_GOG = 'In the lab and the assistant eval the project\'s Gmail is read-only: gog runs only `gog gmail search|get|thread get|labels list|labels get|messages search …` (one gog call per command, values written out, no $ or `…`; after it only `| head`, `tail`, `grep`, `jq`, `wc`, `cut` or `tr`). Sending, replies, drafts, label or message changes, attachments and the other Google services are off here.';
const REFUSE_SECRET = 'gog\'s keyring, tokens and OAuth files are off limits in the lab.';

/** Shell words and operators of `cmd` (a small POSIX lexer): {w: word, raw} | {op}. Unquoted or "…" `$`/backtick → {bad}. */
export function lex(cmd) {
  const out = [];
  let i = 0;
  let word = null;
  let raw = '';
  const push = () => {
    if (word != null) out.push({ w: word, raw });
    word = null;
    raw = '';
  };
  while (i < cmd.length) {
    const c = cmd[i];
    if (c === '\\' && cmd[i + 1] === '\n') { i += 2; continue; }
    if (c === ' ' || c === '\t') { push(); i++; continue; }
    if (c === '\n') { push(); out.push({ op: ';' }); i++; continue; }
    if (c === '#' && word == null) { while (i < cmd.length && cmd[i] !== '\n') i++; continue; }
    const two = cmd.slice(i, i + 2);
    if (['&&', '||', '>>', '2>', '&>', '$('].includes(two) && !(two === '2>' && word != null)) {
      push();
      if (two === '$(') return [{ bad: '$(' }];
      if (two === '2>' || two === '&>') {
        // 2>&1, 2>/dev/null, &>/dev/null
        const m = /^(2>&1|2>\s*\/dev\/null|&>\s*\/dev\/null)(?=\s|$|[|;&])/.exec(cmd.slice(i));
        if (!m) return [{ bad: 'redirect' }];
        out.push({ op: 'stderr', raw: m[1] });
        i += m[1].length;
        continue;
      }
      out.push({ op: two });
      i += 2;
      continue;
    }
    if (';|&()<>`'.includes(c)) {
      push();
      if (c === '`') return [{ bad: '`' }];
      out.push({ op: c });
      i++;
      continue;
    }
    if (c === '$') return [{ bad: '$' }];
    word ??= '';
    if (c === "'") {
      const j = cmd.indexOf("'", i + 1);
      if (j < 0) return [{ bad: 'quote' }];
      word += cmd.slice(i + 1, j);
      raw += cmd.slice(i, j + 1);
      i = j + 1;
      continue;
    }
    if (c === '"') {
      let j = i + 1;
      let s = '';
      while (j < cmd.length && cmd[j] !== '"') {
        if (cmd[j] === '$' || cmd[j] === '`') return [{ bad: '$' }];
        if (cmd[j] === '\\' && '"\\$`\n'.includes(cmd[j + 1])) { s += cmd[j + 1]; j += 2; continue; }
        s += cmd[j++];
      }
      if (j >= cmd.length) return [{ bad: 'quote' }];
      word += s;
      raw += cmd.slice(i, j + 1);
      i = j + 1;
      continue;
    }
    if (c === '\\') { word += cmd[i + 1] ?? ''; raw += cmd.slice(i, i + 2); i += 2; continue; }
    word += c;
    raw += c;
    i++;
  }
  push();
  return out;
}

const WRAPPERS = new Set(['env', 'command', 'exec', 'nohup', 'nice', 'time', 'timeout', 'sudo', 'xargs', 'stdbuf', 'setsid', 'unbuffer', 'doas']);
const SHELLS = new Set(['bash', 'sh', 'zsh', 'dash']);

/** Whether `cmd` runs gog anywhere (a command word gog or …/gog, also behind env/timeout/bash -c…). Errs towards yes. */
export function runsGog(cmd) {
  if (/(^|[\s;&|(`'"=/])gog(\s|$|[;&|)`'"])/.test(cmd) === false && !/\/gog\b/.test(cmd)) return false;
  const toks = lex(cmd);
  if (toks[0]?.bad) return true; // cannot read it and it names gog: treat as gog
  let start = true;
  let skipValue = false;
  for (let k = 0; k < toks.length; k++) {
    const t = toks[k];
    if (t.op) {
      if (t.op !== 'stderr' && t.op !== '>' && t.op !== '>>' && t.op !== '<') start = true;
      continue;
    }
    if (!start) continue;
    const w = t.w;
    if (/^[A-Za-z_][A-Za-z0-9_]*=/.test(w)) continue; // VAR=value before the command
    if (skipValue) { skipValue = false; continue; }
    const name = basename(w);
    if (name === 'gog') return true;
    if (WRAPPERS.has(name)) { if (name === 'timeout') skipValue = true; continue; }
    if (w.startsWith('-') && k > 0) continue; // a wrapper's option (env -i, nice -n…)
    if (SHELLS.has(name)) {
      const c = toks.slice(k + 1).find(x => x.w && !x.w.startsWith('-'));
      if (c && runsGog(c.w)) return true;
    }
    start = false;
  }
  return false;
}

/** Whether `cmd` reaches for gog's secrets by name (for workspaces without the sandbox, e.g. the UI audits'). */
export const touchesSecrets = cmd => /gogcli|hermes-keyring|GOG_KEYRING_PASSWORD|GOG_ACCESS_TOKEN|_gogcli_key_/i.test(cmd);

/**
 * The plan for a Bash command: {pass} (no gog), {deny: reason}, or {read: {argv, tail}} — argv for gog (the
 * guards and the project account added), tail the sandboxed rest after `|` (or ''), stderr whether 2>&1 was asked.
 */
export function classify(cmd) {
  cmd = String(cmd).replace(/\r\n/g, '\n').replace(/[\s;]+$/, '');
  if (touchesSecrets(cmd)) return { deny: REFUSE_SECRET };
  if (!runsGog(cmd)) return { pass: true };
  const toks = lex(cmd);
  if (toks[0]?.bad) return { deny: REFUSE_GOG };
  // The skill's prefix: set -a; . ~/.config/ithomiini/gog.env; set +a   (in the lab gog.env is not needed)
  let k = 0;
  const seg = [];
  for (;;) {
    const end = toks.findIndex((t, j) => j >= k && t.op && t.op !== 'stderr');
    const words = toks.slice(k, end < 0 ? toks.length : end);
    const ws = words.filter(t => !t.op).map(t => t.w);
    const prefix = ws.length === words.length - words.filter(t => t.op === 'stderr').length && ((ws.length === 2 && ws[0] === 'set' && /^[-+]a$/.test(ws[1])) || (ws.length === 2 && ['.', 'source'].includes(ws[0]) && /(^|\/)gog\.env$/.test(ws[1])) || ws.length === 0);
    if (prefix && end >= 0 && [';', '&&'].includes(toks[end].op)) { k = end + 1; continue; }
    seg.push(...words);
    k = end < 0 ? toks.length : end;
    break;
  }
  if (seg.some(t => t.op)) {
    // only 2>&1 / 2>/dev/null after the gog words
    const firstOp = seg.findIndex(t => t.op);
    if (seg.slice(firstOp).some(t => !t.op)) return { deny: REFUSE_GOG };
  }
  const words = seg.filter(t => !t.op).map(t => t.w);
  const stderr = seg.some(t => t.op === 'stderr' && t.raw === '2>&1');
  // The rest: nothing, or `| filter…` that runs in the sandbox (no gog in it: one gog read per command).
  let tail = '';
  if (k < toks.length) {
    if (toks[k].op !== '|') return { deny: REFUSE_GOG };
    const restStart = indexOfOp(cmd, k, toks);
    if (restStart < 0) return { deny: REFUSE_GOG };
    tail = cmd.slice(restStart).trim();
    if (!tail || !plainFilters(tail)) return { deny: REFUSE_GOG };
  }
  if (!words.length || basename(words[0]) !== 'gog') return { deny: REFUSE_GOG };
  // gog [flags] gmail [flags] <read words> [args and flags]
  const flags = [];
  const pos = [];
  for (let j = 1; j < words.length; j++) {
    const w = words[j];
    if (w === '--') { pos.push(...words.slice(j + 1)); break; }
    if (w.startsWith('-') && w.length > 1) {
      const [name, inline] = w.includes('=') ? [w.slice(0, w.indexOf('=')), w.slice(w.indexOf('=') + 1)] : [w, null];
      if (!Object.hasOwn(FLAGS, name)) return { deny: REFUSE_GOG };
      let value = inline;
      if (FLAGS[name] && value == null) {
        value = words[++j];
        if (value == null) return { deny: REFUSE_GOG };
      }
      if (!FLAGS[name] && inline != null) return { deny: REFUSE_GOG };
      flags.push([name, value]);
      continue;
    }
    pos.push(w);
  }
  if (!SERVICE.includes(pos[0])) return { deny: REFUSE_GOG };
  // --help (the skill says `gog <service> --help`): gmail's own, or a read's, with the same guards; no Google call.
  if (flags.some(([n]) => n === '--help' || n === '-h')) {
    const path = pos.length === 1 ? [] : READS.find(r => r.words.length === pos.length - 1 && r.words.every((alts, n) => alts.includes(pos[1 + n])))?.path;
    if (!path) return { deny: REFUSE_GOG };
    return { read: { argv: ['--readonly', '--gmail-no-send', '--no-input', `--enable-commands-exact=${ENABLED}`, 'gmail', ...path, '--help'], tail, stderr: true, command: ['gmail', ...path, '--help'].join(' ') } };
  }
  const read = READS.find(r => r.words.every((alts, n) => alts.includes(pos[1 + n])));
  if (!read) return { deny: REFUSE_GOG };
  const args = pos.slice(1 + read.words.length);
  for (const [name, value] of flags) {
    if ((name === '--account' || name === '-a') && String(value).toLowerCase() !== ACCOUNT.toLowerCase()) return { deny: `${REFUSE_GOG} Only the project's account (${ACCOUNT}).` };
    if (name === '--client' && value !== CLIENT) return { deny: REFUSE_GOG };
  }
  const keep = flags.filter(([n]) => !['--account', '-a', '--client', '--readonly', '--gmail-no-send', '--no-input'].includes(n)).flatMap(([n, v]) => (v == null ? [n] : [`${n}=${v}`]));
  const argv = ['--readonly', '--gmail-no-send', '--no-input', `--enable-commands-exact=${ENABLED}`, `--account=${ACCOUNT}`, `--client=${CLIENT}`, 'gmail', ...read.path, ...keep, '--', ...args];
  return { read: { argv, tail, stderr, command: ['gmail', ...read.path].join(' ') } };
}

// What may read the output after `|`: text filters only (the hook allows the rewritten command, so nothing that runs
// or writes anything: no sed/awk, no redirect).
// Each reads stdin and writes stdout only (sort -o, uniq's output operand, tee and cat's files are not here).
const FILTERS = new Set(['head', 'tail', 'grep', 'egrep', 'fgrep', 'jq', 'wc', 'cut', 'tr']);
function plainFilters(tail) {
  const toks = lex(tail);
  if (toks[0]?.bad) return false;
  let start = true;
  for (const t of toks) {
    if (t.op) {
      if (t.op === '|') { start = true; continue; }
      if (t.op === 'stderr') continue;
      return false;
    }
    if (start && !FILTERS.has(t.w)) return false;
    start = false;
  }
  return !start;
}

/** Where the `|` at token `k` sits in the command text (the lexer keeps no offsets, so count unquoted pipes). */
function indexOfOp(cmd, k, toks) {
  const nth = toks.slice(0, k + 1).filter(t => t.op === '|').length;
  let seen = 0;
  let q = null;
  for (let i = 0; i < cmd.length; i++) {
    const c = cmd[i];
    if (q) { if (c === q) q = null; else if (c === '\\' && q === '"') i++; continue; }
    if (c === "'" || c === '"') { q = c; continue; }
    if (c === '\\') { i++; continue; }
    if (c === '|' && cmd[i + 1] !== '|' && cmd[i - 1] !== '|' && ++seen === nth) return i + 1;
  }
  return -1;
}

const q = s => `'${String(s).replaceAll("'", "'\\''")}'`;

/** Runs the read with gog here and returns the sandboxed command that shows its output. */
export function runRead(plan, { workspace = '' } = {}) {
  mkdirSync(OUT_ROOT, { recursive: true, mode: 0o700 });
  sweep();
  const dir = mkdtempSync(join(OUT_ROOT, 'ithomiini-gmail-'));
  const file = join(dir, 'out.txt');
  const env = { ...process.env };
  if (!env.GOG_KEYRING_PASSWORD && existsSync(PASSWORD_FILE)) env.GOG_KEYRING_PASSWORD = readFileSync(PASSWORD_FILE, 'utf8').trim();
  delete env.GOG_ACCESS_TOKEN;
  const r = existsSync(GOG) ? spawnSync(GOG, plan.argv, { env, encoding: 'utf8', timeout: 90_000, maxBuffer: 32 << 20 }) : { status: 127, stdout: '', stderr: `gog not found (${GOG})\n` };
  const status = r.status ?? 124;
  // gog's errors go with the output when 2>&1 was asked or the read failed (the chat sees why).
  const text = (r.stdout ?? '') + (plan.stderr || status !== 0 ? (r.stderr ?? '') : '') + (r.error ? `\n${r.error.message}\n` : '');
  // wx: never through a link a sandboxed command planted in the (same user's) folder meanwhile.
  writeFileSync(file, text, { mode: 0o600, flag: 'wx' });
  try {
    mkdirSync(join(LOG, '..'), { recursive: true });
    const fd = openSync(LOG, constants.O_WRONLY | constants.O_APPEND | constants.O_CREAT | constants.O_NOFOLLOW, 0o600);
    writeSync(fd, `${new Date().toISOString()}\t${String(workspace).replace(/[\x00-\x1f;|&`$]/g, '_')}\t${plan.command}\texit ${status}\t${text.length} bytes\n`);
    closeSync(fd);
  } catch {}
  const show = `cat ${q(file)}`;
  // The call's status is gog's, also behind a filter (a grep that finds nothing does not fail a read that worked).
  return plan.tail ? `${show} | ${plan.tail}; exit ${status}` : `${show}; exit ${status}`;
}

/** Output folders older than a day go (they hold mail text). */
function sweep() {
  try {
    for (const d of readdirSync(OUT_ROOT)) {
      if (!d.startsWith('ithomiini-gmail-')) continue;
      const p = join(OUT_ROOT, d);
      if (Date.now() - statSync(p).mtimeMs > 864e5) rmSync(p, { recursive: true, force: true });
    }
  } catch {}
}

/** The hook's answer for one PreToolUse event (null: no opinion). */
export function decide(event, run = runRead) {
  if (!['Bash', 'Monitor'].includes(event?.tool_name)) return null;
  const command = String(event.tool_input?.command ?? '');
  // Monitor: no reads (one-off reads are Bash's); gog and its secrets refused there too.
  const plan = event.tool_name === 'Monitor' ? (touchesSecrets(command) ? { deny: REFUSE_SECRET } : runsGog(command) ? { deny: REFUSE_GOG } : { pass: true }) : classify(command);
  if (plan.pass) return null;
  if (plan.deny) return { hookSpecificOutput: { hookEventName: 'PreToolUse', permissionDecision: 'deny', permissionDecisionReason: plan.deny } };
  const shown = run(plan.read, { workspace: event.cwd ?? '' });
  return {
    hookSpecificOutput: {
      hookEventName: 'PreToolUse',
      permissionDecision: 'allow',
      permissionDecisionReason: `lab: read-only ${plan.read.command} on the project's Gmail`,
      updatedInput: { ...event.tool_input, command: shown },
    },
  };
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  let event = null;
  try {
    event = JSON.parse(readFileSync(0, 'utf8'));
  } catch {
    process.exit(0);
  }
  let answer;
  try {
    answer = decide(event);
  } catch (e) {
    // A hook that fails lets the command run as written: refuse instead (gog without the guards, where unsandboxed).
    answer = { hookSpecificOutput: { hookEventName: 'PreToolUse', permissionDecision: 'deny', permissionDecisionReason: `lab Gmail hook error: ${e.message}` } };
  }
  if (answer) process.stdout.write(JSON.stringify(answer));
  process.exit(0);
}
