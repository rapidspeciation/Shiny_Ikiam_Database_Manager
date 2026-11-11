#!/usr/bin/env node
// The live app's own state, for the nightly run (scripts/nightly-tests.sh): read from its /health, one line in the
// night's log, and an exit code its `wikiloc` result is taken from. No login, no chat, no AI: /health is public.
//   node scripts/nightly-app-checks.mjs [health URL]   (default http://127.0.0.1:8794/health, the app's port on claudeclaw)
// Exit 0: ok. 3: a warning (a followed Wikiloc profile not looked at in 30 days, or the daily check on but last run more
// than 2 days ago). 1: failing (the latest Wikiloc import failed, a followed profile's latest check failed, or the worker
// stopped asking for work), or /health did not answer. In Oct 2026 three imports failed and no
// walk came in for six weeks while nothing outside Monitoreo → Importar said so (server/monitoring.mjs wikilocHealth).
//   node scripts/nightly-app-checks.mjs --sheet-ranges [--log <file>]   (GOOGLE_CREDENTIALS_FILE, WORKBOOK_ID)
// The workbook's fixed end rows kept ahead of its rows (server/sheet-ranges.mjs): one line per rule moved, each change
// with the rule as it was appended to the --log file (JSON lines, for the way back). A range protected from the app is
// left for its owner and kept in sheet-ranges-state.json beside the log: told once, again only when it changes (the
// app's alerts list it too, server/alerts.mjs). Exit 0: nothing near, or moved and read back, or only ranges already
// told; 3: a protected range told for the first time or changed; 1: an error or a rule not read back as sent.
import { appendFileSync, existsSync, readFileSync, renameSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { pathToFileURL } from 'node:url';

const url = process.argv[2] || 'http://127.0.0.1:8794/health';
const day = iso => (iso ? String(iso).slice(0, 10) : 'never');
/** Days from an Ecuador day (YYYY-MM-DD) to today's in Ecuador (UTC-5). */
const daysSince = (ecDay, at) => Math.floor((at - 5 * 3600_000 - Date.parse(`${ecDay}T00:00:00Z`)) / 864e5);

/** The line and exit code for /health's `wikiloc` (exported for tests/nightly-app-checks.test.mjs; `at`: the clock). */
export function wikilocLine(w, at = Date.now()) {
  if (!w) return { code: 1, line: 'Wikiloc imports: /health has no wikiloc section (an app older than 10 Nov 2026?)' };
  const detail = `last that worked ${day(w.lastOk)}, ${w.waiting} waiting, worker seen ${w.workerSeen || 'never'}, oldest profile check ${day(w.oldestProfileCheck)}`;
  if (w.status === 'failing')
    return {
      code: 1,
      line: `Wikiloc imports: FAILING: ${
        w.reason === 'worker'
          ? 'the worker has not asked for work in an hour'
          : `${w.failedSinceOk} failed since the last that worked${
              w.profilesFailing > 0 ? `, ${w.profilesFailing} followed profile${w.profilesFailing === 1 ? '' : 's'} whose latest check failed` : ''
            } (latest ${day(w.lastFailed)})`
      }; ${detail}. See Monitoreo → Importar and journalctl --user -u ithomiini-wikiloc.`,
    };
  if (w.status === 'stale')
    return { code: 3, line: `Wikiloc imports: WARNING: a followed profile has not been looked at in 30 days; ${detail}. Importar → «Buscar nuevos».` };
  // The daily check on but not run for over 2 days (the app's timer stopped): the profiles' «revisado» times in Importar.
  if (w.daily === 'on' && w.dailyQueued && daysSince(w.dailyQueued, at) > 2)
    return {
      code: 3,
      line: `Wikiloc imports: WARNING: the daily check is on but last ran ${w.dailyQueued}; ${detail}. Importar → each profile's «revisado».`,
    };
  if (w.status === 'ok' || w.status === 'unused') return { code: 0, line: `Wikiloc imports: ${w.status}; ${detail}` };
  // `unknown`: the app could not work it out (its log says why).
  return { code: 1, line: `Wikiloc imports: ${w.status}: the app could not tell (journalctl --user -u ithomiini)` };
}

/**
 * The --sheet-ranges run: `request` a GoogleSheets request, `log` the --log file. Before a write, the planned changes
 * (with the rules as they were) go into the log; without a log, or when it cannot be written, nothing is sent. After
 * it, the outcome (moved, left out, unconfirmed) goes in too.
 */
export async function sheetRangesRun(request, log, options = {}) {
  const { keepRangesAhead } = await import('../server/sheet-ranges.mjs');
  let planned = false;
  const keep = entry => {
    if (!log) throw new Error('no --log file: nothing sent');
    appendFileSync(log, `${JSON.stringify(entry)}\n`, { mode: 0o600 });
    if (entry.status === 'planned') planned = true;
  };
  const stateFile = log ? join(dirname(log), 'sheet-ranges-state.json') : null;
  const notes = [];
  try {
    let known = {};
    try {
      if (stateFile && existsSync(stateFile)) known = JSON.parse(readFileSync(stateFile, 'utf8')).leftOut || {};
    } catch (e) {
      notes.push(`Sheet ranges: ${stateFile} did not read (${String(e.message).slice(0, 120)}): its ranges are told again`);
    }
    const out = await keepRangesAhead(request, { ...options, known, beforeWrite: keep });
    out.lines.unshift(...notes);
    const { state, toldLeftOut, ...r } = out.record;
    // Every plan logged gets its outcome (a write refused as protected too: nothing landed).
    if (planned || r.changes.length || r.unconfirmed) keep({ ...r, status: r.unconfirmed ? 'unconfirmed' : r.changes.length ? 'sent' : 'refused' });
    else if (toldLeftOut) keep({ ...r, status: 'left out' });
    if (stateFile) {
      writeFileSync(`${stateFile}.new`, `${JSON.stringify({ at: r.at, leftOut: state }, null, 1)}\n`, { mode: 0o600 });
      renameSync(`${stateFile}.new`, stateFile);
    }
    return out;
  } catch (e) {
    return { code: 1, lines: [`Sheet ranges: FAILING: ${String(e.message).slice(0, 300)}`] };
  }
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href && process.argv[2] === '--sheet-ranges') {
  const i = process.argv.indexOf('--log');
  let out;
  try {
    const { GoogleSheets } = await import('../server/sheets.mjs');
    const sheets = new GoogleSheets({ spreadsheetId: process.env.WORKBOOK_ID || undefined });
    out = await sheetRangesRun((path, options) => sheets.request(path, { background: true, ...options }), i > 0 ? process.argv[i + 1] : null);
  } catch (e) {
    out = { code: 1, lines: [`Sheet ranges: FAILING: ${String(e.message).slice(0, 300)}`] };
  }
  console.log(out.lines.join('\n'));
  process.exitCode = out.code;
} else if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  let out;
  try {
    const r = await fetch(url, { signal: AbortSignal.timeout(15_000) });
    if (!r.ok) throw new Error(`answered ${r.status}`);
    out = wikilocLine((await r.json()).wikiloc);
  } catch (e) {
    out = { code: 1, line: `Wikiloc imports: ${url} did not answer (${e.message})` };
  }
  console.log(out.line);
  process.exitCode = out.code;
}
