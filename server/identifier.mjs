// The team's AI Identifier on this server (identify_species, server/drive-photos.mjs): a Python
// process (tools/identifier/worker.py) that the app starts with the first photo and that ends
// itself after some idle minutes, giving its memory back (about 2.3 GB while it runs). It reads
// one photo at a time, on 3 of the server's cores at a lower priority than the app; the app only
// waits on its pipe, so a photo being identified never holds up people's requests.
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
 * `dir`: the identifier's files; `python` (default <dir>/venv/bin/python), `script`, `args` (for tests:
 * another program speaking the same lines), `threads`, `idleMinutes`, `encoder` (a path under dir).
 * Returns { identify({ path, side, box }) → the worker's answer or { error }, status(), close() }.
 */
export function createIdentifier({ dir = null, python = null, script = SCRIPT, args = null, threads = 3, idleMinutes = 10, encoder = null, firstMs = FIRST_MS, photoMs = PHOTO_MS, spawnImpl = spawn } = {}) {
  const exe = python ?? (dir ? join(dir, 'venv', 'bin', 'python') : null);
  let child = null;
  let loaded = false;
  let seq = 0;
  let lastMs = null;
  let photos = 0;
  /** One photo at a time: each call waits for the one before. */
  let chain = Promise.resolve();

  const missing = () => {
    if (!dir || !existsSync(dir)) return 'The identifier is not installed on this server (tools/identifier/setup.sh).';
    if (!existsSync(exe) || (!args && !existsSync(script))) return 'The identifier is not installed on this server (tools/identifier/setup.sh).';
    return null;
  };

  function start() {
    const scratch = join(tmpdir(), 'ithomiini-identifier');
    mkdirSync(scratch, { recursive: true, mode: 0o700 });
    const argv = args ?? ['-I', '-B', script, '--dir', dir, '--threads', String(threads), '--idle', String(Math.round(idleMinutes * 60)), ...(encoder ? ['--encoder', encoder] : [])];
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
    let out = await send(job);
    if (out.again) out = await send(job);
    if (out.again) out = { error: 'The identifier stopped. Call again.' };
    if (!out.error) {
      loaded = true;
      photos += 1;
      lastMs = Date.now() - started;
    }
    return out;
  }

  return {
    /** A photo on this server (`path`): { species, subspecies, genus, top, crop, wingBox, prior, ms } or { error }. */
    identify({ path, side = null, box = null }) {
      const job = { id: String(++seq), path, ...(side ? { side } : {}), ...(box ? { box } : {}) };
      const next = chain.then(() => run(job));
      chain = next.catch(() => {});
      return next;
    },
    status: () => ({ installed: !missing(), running: !!child, loaded, photos, lastMs }),
    close() {
      child?.proc.stdin.end();
      child = null;
    },
  };
}
