import { spawn } from 'node:child_process';
import { existsSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const directory = dirname(fileURLToPath(import.meta.url));
const python = () => process.env.ITHOMIINI_WIKILOC_PYTHON || join(directory, 'venv', 'bin', 'python');
const REQUEST_TIMEOUT = 210_000;

/** Start a browser process for one job; page has the operations used by the parsers. */
export async function openBrowser({ timeoutMs = REQUEST_TIMEOUT } = {}) {
  if (!existsSync(python())) throw new Error(`Wikiloc Python is missing: ${python()}`);
  const child = spawn(python(), [join(directory, 'browser.py')], {
    stdio: ['pipe', 'pipe', 'pipe'],
    detached: process.platform !== 'win32',
  });
  const exited = new Promise(resolve => child.once('exit', resolve));
  const pending = new Map();
  let sequence = 0;
  let buffer = '';
  let ended = false;
  let closing = false;
  let exitReason = null;

  function terminateGroup() {
    try {
      if (process.platform !== 'win32' && child.pid) process.kill(-child.pid, 'SIGTERM');
      else child.kill('SIGTERM');
    } catch {
      // The process group has already exited.
    }
  }

  function fail(error) {
    if (ended) return;
    ended = true;
    exitReason = error;
    for (const entry of pending.values()) {
      clearTimeout(entry.timer);
      entry.reject(error);
    }
    pending.clear();
    if (!closing) terminateGroup();
  }

  child.stdout.setEncoding('utf8');
  child.stdout.on('data', chunk => {
    buffer += chunk;
    if (buffer.length > 10_000_000) {
      fail(new Error('Wikiloc browser response exceeded 10 MB'));
      return;
    }
    for (let newline; (newline = buffer.indexOf('\n')) >= 0;) {
      const line = buffer.slice(0, newline);
      buffer = buffer.slice(newline + 1);
      let response;
      try {
        response = JSON.parse(line);
      } catch {
        fail(new Error('Wikiloc browser sent invalid JSON'));
        return;
      }
      const entry = pending.get(response.id);
      if (!entry) continue;
      pending.delete(response.id);
      clearTimeout(entry.timer);
      if (response.error) entry.reject(new Error(`Wikiloc browser: ${response.error}`));
      else entry.resolve(response.data);
    }
  });
  child.on('error', error => fail(new Error(`Wikiloc browser failed to start: ${error.message}`)));
  child.on('exit', (code, signal) => {
    if (!closing) terminateGroup();
    fail(new Error(`Wikiloc browser exited (${signal || code})`));
  });
  child.stdin.on('error', error => fail(new Error(`Wikiloc browser input failed: ${error.message}`)));
  // Drain diagnostics so a full stderr pipe cannot stall the Python process.
  child.stderr.resume();

  function call(op, fields = {}, timeout = timeoutMs) {
    if (ended) return Promise.reject(exitReason);
    return new Promise((resolve, reject) => {
      const id = ++sequence;
      const timer = setTimeout(() => {
        pending.delete(id);
        const error = new Error(`Wikiloc browser ${op} timed out`);
        reject(error);
        fail(error);
      }, timeout);
      pending.set(id, { resolve, reject, timer });
      child.stdin.write(`${JSON.stringify({ id, op, ...fields })}\n`, error => {
        if (error && pending.delete(id)) {
          clearTimeout(timer);
          reject(error);
          fail(error);
        }
      });
    });
  }

  const page = {
    goto: url => call('goto', { url }),
    title: () => call('title'),
    evaluate: fn => call('evaluate', { code: fn.toString() }),
    waitForTimeout: ms => new Promise(resolve => setTimeout(resolve, ms)),
    async waitForFunction(fn, _arg, { timeout = 30_000 } = {}) {
      const end = Date.now() + timeout;
      while (Date.now() < end) {
        if (await this.evaluate(fn)) return;
        await this.waitForTimeout(500);
      }
      throw new Error('Wikiloc page data timed out');
    },
  };
  return {
    page,
    async close() {
      if (ended) return;
      try {
        await call('close', {}, 10_000);
        closing = true;
        child.stdin.end();
        let shutdownTimer;
        try {
          await Promise.race([
            exited,
            new Promise((_, reject) => {
              shutdownTimer = setTimeout(() => reject(new Error('Browser shutdown timed out')), 10_000);
            }),
          ]);
        } finally {
          clearTimeout(shutdownTimer);
        }
      } catch {
        terminateGroup();
      }
    },
  };
}
