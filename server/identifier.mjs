// The team's AI Identifier on this server (identify_species, server/drive-photos.mjs): a Python
// process (tools/identifier/worker.py) that the app starts with the first photo and that ends
// itself after some idle minutes, giving its memory back (about 2.3 GB while it runs). It reads
// one photo at a time (the team's, visitors' and a walk's photos in one queue), on 3 of the server's cores at a lower
// priority than the app (nice 10; IDENTIFIER_THREADS and IDENTIFIER_NICE, server/index.mjs); the app only waits on
// its pipe, so a photo being identified never holds up people's requests.
//
// Its files (the venv, the models) are outside the releases, in IDENTIFIER_DIR (shared/identifier
// beside the database by default; tools/identifier/setup.sh puts them there).

import { spawn } from 'node:child_process';
import { existsSync, mkdirSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { createInterface } from 'node:readline';

const SCRIPT = new URL('../tools/identifier/worker.py', import.meta.url).pathname;
/** A photo's longest wait: the first one also loads the models (about 4 s, longer on a busy server). */
const FIRST_MS = 120_000;
const PHOTO_MS = 60_000;
/**
 * A visitor's wait estimate (claudeclaw, 17 Nov 2026: 1.26 s a photo at 3 threads, 3.7 s more for the models' load on
 * the first photo after a quiet spell): a photo before any was timed, and the load added while the models are not in.
 */
const GUESS_MS = 1_500;
const LOAD_MS = 4_000;
/** How many recent photos the wait estimate averages. */
const RECENT = 10;

/**
 * `dir`: the identifier's files; `python` (default <dir>/venv/bin/python), `script`, `args` (for tests:
 * another program speaking the same lines), `threads`, `nice` (how much lower its priority; worker.py's own default, 5,
 * when null), `idleMinutes`, `encoder` (a path under dir).
 * Returns { identify({ path, side, box, first, priority, signal, ticket }) → the worker's answer or { error },
 * standing(ticket), queued(rank), status(), close() }.
 */
export function createIdentifier({ dir = null, python = null, script = SCRIPT, args = null, threads = 3, nice = null, idleMinutes = 10, encoder = null, firstMs = FIRST_MS, photoMs = PHOTO_MS, spawnImpl = spawn } = {}) {
  const exe = python ?? (dir ? join(dir, 'venv', 'bin', 'python') : null);
  let child = null;
  let loaded = false;
  let seq = 0;
  let lastMs = null;
  let photos = 0;
  /**
   * One photo at a time: each call waits for those before it. A photo a team member waits for (`first`, Identificar's
   * «En línea») goes ahead of the others, after the other such photos; then a visitor's (`priority` 1,
   * server/visitor-identify.mjs); then the background ones (a walk's photos). The same queue for everyone, so the
   * server never reads two photos at once.
   */
  const queue = [];
  let running = false;
  /** The photo being read: its queue entry and when it started (a visitor's wait counts from it). */
  let current = null;
  /** The last photos' times (a successful read with the models already in): the wait estimate. */
  const recent = [];
  async function pump() {
    if (running) return;
    running = true;
    while (queue.length) {
      const entry = queue.shift();
      entry.stop?.();
      current = { entry, started: Date.now() };
      const out = await run(entry.job).catch(e => ({ error: String(e?.message || e) }));
      current = null;
      entry.resolve(out);
    }
    running = false;
  }
  const meanMs = () => (recent.length ? recent.reduce((a, b) => a + b, 0) / recent.length : GUESS_MS);
  /** What the photo being read still needs, by the estimate (at least a second while it runs). */
  const leftMs = () => (current ? Math.max(1000, meanMs() + (loaded ? 0 : LOAD_MS) - (Date.now() - current.started)) : 0);
  /** The models' load still to come before the next photo (none being read, the models not in). */
  const loadMs = () => (current || loaded ? 0 : LOAD_MS);

  const missing = () => {
    if (!dir || !existsSync(dir)) return 'The identifier is not installed on this server (tools/identifier/setup.sh).';
    if (!existsSync(exe) || (!args && !existsSync(script))) return 'The identifier is not installed on this server (tools/identifier/setup.sh).';
    return null;
  };

  function start() {
    const scratch = join(tmpdir(), 'ithomiini-identifier');
    mkdirSync(scratch, { recursive: true, mode: 0o700 });
    const argv = args ?? ['-I', '-B', script, '--dir', dir, '--threads', String(threads), '--idle', String(Math.round(idleMinutes * 60)), ...(Number.isInteger(nice) ? ['--nice', String(nice)] : []), ...(encoder ? ['--encoder', encoder] : [])];
    const proc = spawnImpl(exe, argv, {
      stdio: ['pipe', 'pipe', 'pipe'],
      env: { ...process.env, YOLO_CONFIG_DIR: scratch, MPLCONFIGDIR: scratch, YOLO_OFFLINE: '1' },
    });
    const state = { proc, waiting: new Map(), exited: false, code: null };
    proc.stderr.on('data', d => {
      const line = String(d).trim();
      if (/error|traceback/i.test(line)) console.error('Identifier:', line.slice(0, 300));
    });
    proc.on('error', e => finish(state, null, e));
    proc.on('exit', code => finish(state, code));
    proc.stdin.on('error', () => {});
    createInterface({ input: proc.stdout }).on('line', line => {
      let out;
      try {
        out = JSON.parse(line);
      } catch {
        return;
      }
      if (out.id === undefined) return;
      const w = state.waiting.get(out.id);
      if (!w) return;
      state.waiting.delete(out.id);
      clearTimeout(w.timer);
      w.resolve(out);
    });
    return state;
  }
  function finish(state, code, error = null) {
    if (state.exited) return;
    state.exited = true;
    state.code = code;
    if (child === state) {
      child = null;
      loaded = false;
    }
    for (const [, w] of state.waiting) {
      clearTimeout(w.timer);
      // Ended on its own while idle as the photo was sent: sent again to a new one.
      w.resolve(code === 0 ? { again: true } : { error: error ? `The identifier could not start (${error.code ?? error.message}).` : `The identifier stopped (${code ?? 'killed'}).` });
    }
    state.waiting.clear();
  }

  function send(job) {
    child ??= start();
    const state = child;
    const ms = loaded ? photoMs : firstMs;
    return new Promise(resolve => {
      const timer = setTimeout(() => {
        state.waiting.delete(job.id);
        resolve({ error: `The identifier took more than ${Math.round(ms / 1000)} s; it was stopped. Call again.` });
        // The next photo goes to a new process.
        if (child === state) {
          child = null;
          loaded = false;
        }
        state.proc.kill('SIGKILL');
      }, ms);
      timer.unref?.();
      state.waiting.set(job.id, { resolve, timer });
      state.proc.stdin.write(`${JSON.stringify(job)}\n`);
    });
  }

  async function run(job) {
    const why = missing();
    if (why) return { error: why };
    const started = Date.now();
    const warm = loaded;
    let out = await send(job);
    if (out.again) out = await send(job);
    if (out.again) out = { error: 'The identifier stopped. Call again.' };
    if (!out.error) {
      loaded = true;
      photos += 1;
      lastMs = Date.now() - started;
      if (warm) recent.push(lastMs);
      if (recent.length > RECENT) recent.shift();
    }
    return out;
  }

  return {
    /** A photo on this server (`path`): { species, subspecies, genus, top, crop, wingBox, prior, ms } or { error }. */
    /**
     * `signal`: given up while it waits (a visitor who left): out of the queue, answered { error, cancelled }; once
     * it is being read it runs to its end (the caller drops the answer). `ticket`: any object, to ask its place with
     * standing().
     */
    identify({ path, side = null, box = null, first = false, priority = null, signal = null, ticket = null }) {
      const job = { id: String(++seq), path, ...(side ? { side } : {}), ...(box ? { box } : {}) };
      const rank = first ? 0 : Number.isInteger(priority) ? priority : 2;
      return new Promise(resolve => {
        if (signal?.aborted) return resolve({ error: 'Cancelled.', cancelled: true });
        const entry = { job, resolve, rank, ticket };
        if (signal) {
          const leave = () => {
            const i = queue.indexOf(entry);
            if (i < 0) return;
            queue.splice(i, 1);
            resolve({ error: 'Cancelled.', cancelled: true });
          };
          signal.addEventListener('abort', leave, { once: true });
          entry.stop = () => signal.removeEventListener('abort', leave);
        }
        const at = queue.findIndex(q => q.rank > rank);
        queue.splice(at < 0 ? queue.length : at, 0, entry);
        pump();
      });
    },
    /**
     * A ticket's place: { state: 'running' | 'waiting', ahead (photos before it, the one being read included),
     * waitMs (the estimate until its answer: the mean of the last photos' times) }, or null when it is not here.
     */
    standing(ticket) {
      if (!ticket) return null;
      if (current?.entry.ticket === ticket) return { state: 'running', ahead: 0, waitMs: leftMs() };
      const i = queue.findIndex(q => q.ticket === ticket);
      if (i < 0) return null;
      return { state: 'waiting', ahead: i + (current ? 1 : 0), waitMs: leftMs() + loadMs() + (i + 1) * meanMs() };
    },
    /** How many photos wait at `rank` or before it, the one being read included, and the wait for one more there. */
    queued(rank = 2) {
      const before = queue.filter(q => q.rank <= rank).length;
      return { ahead: before + (current ? 1 : 0), waitMs: leftMs() + loadMs() + (before + 1) * meanMs(), meanMs: meanMs() };
    },
    status: () => ({ installed: !missing(), running: !!child, loaded, photos, lastMs }),
    close() {
      child?.proc.stdin.end();
      child = null;
    },
  };
}
