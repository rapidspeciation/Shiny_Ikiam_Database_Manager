import { timingSafeEqual } from 'node:crypto';
import { moduleMap } from './schema.mjs';

const fail = (code, message, status) => Object.assign(new Error(message), { code, status });
// Row inserts and deletions shift everything below them, so only a sheet sync can place rows again.
const STRUCTURAL = new Set(['INSERT_ROW', 'REMOVE_ROW', 'INSERT_GRID', 'REMOVE_GRID', 'OTHER']);
const MAX_ROWS = 300;

/**
 * Edits made directly in Google Sheets, reported by the Apps Script trigger in
 * tools/apps-script. Reports arriving close together are merged, then only the
 * edited rows are read again (or the whole sheet after rows were inserted or deleted).
 */
export function createSheetHook(store, { secret, delayMs = 1500, log = console } = {}) {
  const queued = new Map();
  let timer = null;
  let running = Promise.resolve();
  const status = { received: 0, lastAt: null, lastSheet: null, lastMs: null, lastResult: null, lastError: null };

  function authorized(given) {
    if (!secret || typeof given !== 'string') return false;
    const a = Buffer.from(given),
      b = Buffer.from(secret);
    return a.length === b.length && timingSafeEqual(a, b);
  }

  function receive(headers, body) {
    if (!secret) throw fail('HOOK_DISABLED', 'Sheet hook is not configured', 404);
    if (!authorized(headers['x-hook-secret'])) throw fail('HOOK_FORBIDDEN', 'Invalid hook secret', 403);
    const events = Array.isArray(body.events) ? body.events : [body];
    let accepted = 0;
    for (const event of events.slice(0, 50)) {
      const sheet = String(event?.sheet || '');
      if (!moduleMap.has(sheet)) continue;
      const entry = queued.get(sheet) || { rows: new Set(), full: false };
      const start = Number(event.startRow),
        count = Math.min(Number(event.numRows) || 1, MAX_ROWS + 1);
      if (STRUCTURAL.has(event.change) || !Number.isInteger(start) || count > MAX_ROWS) entry.full = true;
      else for (let row = start; row < start + count; row++) entry.rows.add(row);
      if (entry.rows.size > MAX_ROWS) entry.full = true;
      queued.set(sheet, entry);
      accepted++;
    }
    status.received += accepted;
    if (accepted) {
      clearTimeout(timer);
      timer = setTimeout(() => (running = running.then(flush)), delayMs);
      timer.unref?.();
    }
    return { accepted };
  }

  async function flush() {
    const batch = [...queued];
    queued.clear();
    for (const [sheet, { rows, full }] of batch) {
      const started = Date.now();
      try {
        let result = full ? null : await store.refreshRows(sheet, [...rows]);
        if (!result || result.needsSync) result = { synced: true, ...(await store.sync({ sheets: [sheet] })) };
        Object.assign(status, {
          lastAt: new Date().toISOString(),
          lastSheet: sheet,
          lastMs: Date.now() - started,
          lastResult: result.synced ? 'sheet' : `${result.changed + result.added + result.removed} rows`,
          lastError: null,
        });
      } catch (e) {
        status.lastError = e.message;
        log.error?.(`Sheet hook for ${sheet} failed:`, e.message);
      }
    }
  }

  return { receive, status, flush: () => (running = running.then(flush)) };
}
