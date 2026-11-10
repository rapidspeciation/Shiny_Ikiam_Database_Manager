#!/usr/bin/env node
// The live app's own state, for the nightly run (scripts/nightly-tests.sh): read from its /health, one line in the
// night's log, and an exit code its `wikiloc` result is taken from. No login, no chat, no AI: /health is public.
//   node scripts/nightly-app-checks.mjs [health URL]   (default http://127.0.0.1:8794/health, the app's port on claudeclaw)
// Exit 0: ok. 3: a warning (a followed Wikiloc profile not looked at in 30 days). 1: failing (the latest Wikiloc import
// failed, or its worker stopped asking for work), or /health did not answer. In Oct 2026 three imports failed and no
// walk came in for six weeks while nothing outside Monitoreo → Importar said so (server/monitoring.mjs wikilocHealth).
import { pathToFileURL } from 'node:url';

const url = process.argv[2] || 'http://127.0.0.1:8794/health';
const day = iso => (iso ? String(iso).slice(0, 10) : 'never');

/** The line and exit code for /health's `wikiloc` (exported for tests/nightly-app-checks.test.mjs). */
export function wikilocLine(w) {
  if (!w) return { code: 1, line: 'Wikiloc imports: /health has no wikiloc section (an app older than 10 Nov 2026?)' };
  const detail = `last that worked ${day(w.lastOk)}, ${w.waiting} waiting, worker seen ${w.workerSeen || 'never'}, oldest profile check ${day(w.oldestProfileCheck)}`;
  if (w.status === 'failing')
    return {
      code: 1,
      line: `Wikiloc imports: FAILING: ${
        w.reason === 'worker'
          ? 'the worker has not asked for work in an hour'
          : `${w.failedSinceOk} failed since the last that worked, ${w.profilesFailing ?? 0} followed profiles whose latest check failed (latest ${day(w.lastFailed)})`
      }; ${detail}. See Monitoreo → Importar and journalctl --user -u ithomiini-wikiloc.`,
    };
  if (w.status === 'stale')
    return { code: 3, line: `Wikiloc imports: WARNING: a followed profile has not been looked at in 30 days; ${detail}. Importar → «Buscar nuevos».` };
  if (w.status === 'ok' || w.status === 'unused') return { code: 0, line: `Wikiloc imports: ${w.status}; ${detail}` };
  // `unknown`: the app could not work it out (its log says why).
  return { code: 1, line: `Wikiloc imports: ${w.status}: the app could not tell (journalctl --user -u ithomiini)` };
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
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
