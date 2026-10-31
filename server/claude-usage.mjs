// The Claude account's use (the 5-hour window and the week), as the orchestrator's claude_usage.py reads it:
// Anthropic's OAuth usage endpoint with the account's own token from ~/.claude/.credentials.json. The token
// is read here, on the server, for that one request; it is never logged or returned. The endpoint
// rate-limits, so a reading is kept for 3 minutes in a cache file the other readers of this account share.
import { readFileSync, statSync, writeFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { join } from 'node:path';

const ENDPOINT = 'https://api.anthropic.com/api/oauth/usage';
const CACHE_MS = 180_000;
/** Ecuador is UTC-5 all year. */
const ECUADOR_MS = -5 * 3600_000;
const DAYS = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
const MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];

/** "Fri 30 Oct 22:00" in Ecuador time. */
export function ecuadorTime(iso) {
  const t = Date.parse(iso);
  if (Number.isNaN(t)) return null;
  const d = new Date(t + ECUADOR_MS);
  return `${DAYS[d.getUTCDay()]} ${String(d.getUTCDate()).padStart(2, '0')} ${MONTHS[d.getUTCMonth()]} ${d.toISOString().slice(11, 16)}`;
}

/** The reading as WhatsApp lines, in English or Spanish (`es`). `u`: the endpoint's answer. */
export function usageText(u, { cached = null, es = false } = {}) {
  const part = (label, v) => {
    if (!v) return `${label}: ${es ? 'sin lectura' : 'no reading'}`;
    const when = v.resets_at ? ecuadorTime(v.resets_at) : null;
    return `${label}: ${Math.round(Number(v.utilization ?? 0))} % ${es ? 'usado' : 'used'}${when ? `, ${es ? 'se reinicia' : 'resets'} ${when}` : ''}`;
  };
  return [
    es ? '*Uso de Claude* (la cuenta del bot)' : '*Claude use* (the bot\'s account)',
    `- ${part(es ? '5 horas' : '5 hours', u?.five_hour)}`,
    `- ${part(es ? 'Semana' : 'Week', u?.seven_day)}`,
    `_${es ? 'Hora de Ecuador' : 'Ecuador time'}${cached ? (es ? `; lectura de hace ${cached}` : `; reading from ${cached} ago`) : ''}_`,
  ].join('\n');
}

/**
 * `read({ es })` → { text } or { error }. `credentials`: the account's credentials file; `cacheFile`: shared with
 * claude_usage.py (/tmp/claude_usage_cache.json, the same JSON).
 */
export function claudeUsage({ credentials = join(homedir(), '.claude', '.credentials.json'), cacheFile = '/tmp/claude_usage_cache.json', fetchImpl = fetch, now = Date.now } = {}) {
  const fromCache = maxAge => {
    try {
      const age = now() - statSync(cacheFile).mtimeMs;
      if (age > maxAge) return null;
      return { u: JSON.parse(readFileSync(cacheFile, 'utf8')), age };
    } catch {
      return null;
    }
  };
  return {
    async read({ es = false } = {}) {
      const fresh = fromCache(CACHE_MS);
      if (fresh) return { text: usageText(fresh.u, { es }) };
      let token;
      try {
        token = JSON.parse(readFileSync(credentials, 'utf8'))?.claudeAiOauth?.accessToken;
      } catch {
        token = null;
      }
      if (!token) return { error: es ? 'No hay sesión de Claude en este servidor.' : 'No Claude login on this server.' };
      try {
        const r = await fetchImpl(ENDPOINT, { headers: { authorization: `Bearer ${token}`, 'anthropic-beta': 'oauth-2025-04-20' }, signal: AbortSignal.timeout(15_000) });
        if (!r.ok) throw Object.assign(new Error(`usage endpoint ${r.status}`), { status: r.status });
        const u = await r.json();
        try {
          writeFileSync(cacheFile, JSON.stringify(u), { mode: 0o600 });
        } catch {
          /* another reader's file: only a reading */
        }
        return { text: usageText(u, { es }) };
      } catch (e) {
        const old = fromCache(Infinity);
        if (old) return { text: usageText(old.u, { es, cached: `${Math.round(old.age / 60_000)} min` }) };
        return { error: es ? `La lectura de uso falló (${e.status ?? 'red'}); prueba en unos minutos.` : `The usage reading failed (${e.status ?? 'network'}); try again in a few minutes.` };
      }
    },
  };
}
