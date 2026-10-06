// Alerts: things the team must act on before they become a problem, shown in
// Revisión → Alertas, on Inicio and to the assistant (get_alerts).
//
// CAM pools. The CAM_ID columns take their values from pools in the Lists sheet
// (InsectaryWild&Reared_CAMid, Wild_indv_CAMid…, the dropdowns of
// server/verifications.mjs). A pool is made of ranges of consecutive CAMs that PAS
// or AA hand out; the team uses each range upwards. For every range: how many are
// used (any sheet), the highest used, how many are left above it (gaps below it
// are counted apart: they are usually skipped for good), and when it was last
// used. A range in use in the last year with fewer than 50 left, or less than
// 15 % of it, is an alert: time to ask PAS or AA for a new range.
//
// The 30-preserved rule (docs/monitoring.md): once an Ithomiini species has 30
// preserved individuals from Ikiam, Casa de Lin or Mariposario Ikiam
// (Collected_Preserved in Collection_data, whatever the Purpose; counted per
// species, genus + epithet, as the team does), it is marked and released
// instead. Species that reached 30 recently and species close to it (25+) are
// alerts; butterflies preserved after their species had 30 are listed as
// information, not as errors.
//
// Preserved without CAM or tube (server/preserved.mjs): an insectary butterfly
// whose cells say it was preserved, without its CAM_ID or Tube_1_id (or with
// Killed_Preserved but NA in them). The ones that died in the last 180 days
// are alerts, to ask the team; the notice goes once the cells are filled.

import { msg, msgn } from './messages.mjs';
import { iso, recordsStamp, sheetRows, todaySerial } from './checks.mjs';
import { moduleMap } from './schema.mjs';
import { LISTS } from './verifications.mjs';
import { sampleGap } from './preserved.mjs';

export const CAM_LOW_LEFT = 50;
export const CAM_LOW_SHARE = 0.15;
export const PRESERVE_LIMIT = 30;
export const PRESERVE_NEAR = 25;
export const RULE_LOCATIONS = ['Ikiam', 'Casa de Lin', 'Mariposario Ikiam'];
/** How long a species that reached 30 (or a range in use) stays news. */
const RECENT_DAYS = 60;
const ACTIVE_DAYS = 365;
/** How long a butterfly preserved without CAM or tube stays an alert (older ones are in Revisión). */
export const SAMPLE_DAYS = 180;

const text = value => (value === null || value === undefined ? '' : String(value).trim());
const isDate = value => typeof value === 'number' && Number.isFinite(value) && value > 0;
const CAM = /^([A-Z]{3})(\d{6})$/;

/** The pools the dropdowns use: Lists column → the sheet columns that take it. */
function camPools() {
  const pools = new Map();
  for (const [sheet, rules] of Object.entries(LISTS))
    for (const [field, rule] of Object.entries(rules))
      if (rule.from?.[0] === 'Lists' && /CAMid$/.test(rule.from[1]))
        (pools.get(rule.from[1]) || pools.set(rule.from[1], []).get(rule.from[1])).push(`${sheet} · ${field}`);
  return pools;
}

function camAlerts(sheets, today) {
  const pools = camPools();
  const lists = sheets.get('Lists') ?? [];
  // Every CAM written anywhere (sample IDs and manifests cite them too), with the latest day of its rows.
  const used = new Map();
  for (const [sheet, rows] of sheets) {
    if (sheet === 'Lists') continue;
    const fields = moduleMap.get(sheet).fields;
    const camFields = fields.map(f => f.key).filter(k => /cam|sample/i.test(k));
    const dateFields = fields.filter(f => f.type === 'date').map(f => f.key);
    if (!camFields.length) continue;
    for (const row of rows) {
      let day = null;
      for (const field of camFields) {
        const cam = text(row.values[field]).toUpperCase();
        if (!CAM.test(cam)) continue;
        day ??= Math.max(0, ...dateFields.map(f => row.values[f]).filter(v => isDate(v) && v <= today));
        const seen = used.get(cam);
        if (!seen || day > seen.day) used.set(cam, { day, sheet, row: row.row });
      }
    }
  }
  const out = [];
  for (const [pool, fields] of pools) {
    const ids = [
      ...new Set(
        lists
          .map(r => text(r.values[pool]).toUpperCase())
          .filter(v => CAM.test(v)),
      ),
    ]
      .map(v => {
        const [, prefix, digits] = CAM.exec(v);
        return { prefix, n: Number(digits) };
      })
      .sort((a, b) => a.prefix.localeCompare(b.prefix) || a.n - b.n);
    const ranges = [];
    for (const id of ids) {
      const last = ranges.at(-1);
      if (last && last.prefix === id.prefix && id.n === last.end + 1) last.end = id.n;
      else ranges.push({ prefix: id.prefix, start: id.n, end: id.n });
    }
    const name = (prefix, n) => `${prefix}${String(n).padStart(6, '0')}`;
    const described = ranges.map(r => {
      let count = 0,
        highest = null,
        lastDay = 0,
        lastCam = null;
      for (let n = r.start; n <= r.end; n++) {
        const u = used.get(name(r.prefix, n));
        if (!u) continue;
        count++;
        highest = n;
        if (u.day >= lastDay) [lastDay, lastCam] = [u.day, name(r.prefix, n)];
      }
      const size = r.end - r.start + 1;
      const left = highest === null ? size : r.end - highest;
      const active = !!lastDay && today - lastDay <= ACTIVE_DAYS;
      // A finished range is history; a range still in use running low is the alert.
      const low = active && left > 0 && (left < CAM_LOW_LEFT || left < size * CAM_LOW_SHARE);
      return {
        first: name(r.prefix, r.start),
        last: name(r.prefix, r.end),
        size,
        used: count,
        highest: highest === null ? null : name(r.prefix, highest),
        next: left ? name(r.prefix, (highest ?? r.start - 1) + 1) : null,
        left,
        gaps: highest === null ? 0 : highest - r.start + 1 - count,
        lastUsed: lastDay ? { cam: lastCam, date: iso(lastDay) } : null,
        active,
        level: !left ? 'done' : low ? 'low' : 'ok',
      };
    });
    // Shown: ranges with CAMs left, and full ones used in the last year (the run the team just finished).
    const shown = described.filter(r => r.left || r.active);
    const left = described.reduce((n, r) => n + r.left, 0);
    const current = [...described].filter(r => r.lastUsed).sort((a, b) => b.lastUsed.date.localeCompare(a.lastUsed.date))[0];
    // Out: in use this year and nothing left in any range.
    const empty = !left && described.some(r => r.active);
    out.push({
      pool,
      fields,
      left,
      ranges: shown,
      fullRanges: described.length - shown.length,
      current: current?.first ?? null,
      level: empty ? 'out' : shown.some(r => r.level === 'low') ? 'low' : 'ok',
    });
  }
  return out;
}

const binomial = value => text(value).split(/\s+/).slice(0, 2).join(' ');
/** The 30-preserved rule, per species. */
function preserveRule(sheets, today) {
  const rows = (sheets.get('Collection_data') ?? []).filter(r => r.observed);
  const places = new Set(RULE_LOCATIONS.map(l => l.toLowerCase()));
  // Tribe is a formula from the taxonomy; a blank one is read from other rows of the genus.
  const tribeOfGenus = new Map();
  for (const r of rows) {
    const tribe = text(r.values.Tribe);
    const genus = binomial(r.values.SPECIES).split(' ')[0].toLowerCase();
    if (tribe && genus && !tribeOfGenus.has(genus)) tribeOfGenus.set(genus, tribe);
  }
  const isIthomiini = r =>
    /^ithomiini$/i.test(text(r.values.Tribe) || tribeOfGenus.get(binomial(r.values.SPECIES).split(' ')[0].toLowerCase()) || '');
  const bySpecies = new Map();
  for (const r of rows) {
    if (text(r.values.Release_Collect) !== 'Collected_Preserved') continue;
    if (!places.has(text(r.values.Collection_location).toLowerCase()) || !isIthomiini(r)) continue;
    const name = binomial(r.values.SPECIES);
    if (!name || name.split(' ').length < 2) continue;
    const key = name.toLowerCase();
    (bySpecies.get(key) || bySpecies.set(key, []).get(key)).push(r);
  }
  const dayOf = r => (isDate(r.values.Collection_date) ? r.values.Collection_date : Infinity);
  const reached = [];
  const near = [];
  for (const list of bySpecies.values()) {
    list.sort((a, b) => dayOf(a) - dayOf(b) || a.row - b.row);
    // The spelling most rows use.
    const spellings = new Map();
    for (const r of list) spellings.set(binomial(r.values.SPECIES), (spellings.get(binomial(r.values.SPECIES)) ?? 0) + 1);
    const species = [...spellings].sort((a, b) => b[1] - a[1])[0][0];
    const last = list.at(-1);
    const lastPreserved = isDate(last.values.Collection_date) ? iso(last.values.Collection_date) : null;
    const pick = r => ({
      sheet: r.sheet,
      row: r.row,
      recordId: r.id,
      label: r.label,
      date: isDate(r.values.Collection_date) ? iso(r.values.Collection_date) : null,
    });
    if (list.length >= PRESERVE_LIMIT) {
      const at = list[PRESERVE_LIMIT - 1];
      const after = list.slice(PRESERVE_LIMIT);
      reached.push({
        species,
        preserved: list.length,
        reachedOn: isDate(at.values.Collection_date) ? iso(at.values.Collection_date) : null,
        reachedRow: pick(at),
        after: after.length,
        afterRows: after.slice(-10).map(pick),
        lastPreserved,
        recent: isDate(at.values.Collection_date) && today - at.values.Collection_date <= RECENT_DAYS,
        recentAfter: after.filter(r => isDate(r.values.Collection_date) && today - r.values.Collection_date <= RECENT_DAYS).length,
      });
    } else if (list.length >= PRESERVE_NEAR) near.push({ species, preserved: list.length, left: PRESERVE_LIMIT - list.length, lastPreserved });
  }
  reached.sort((a, b) => String(b.reachedOn).localeCompare(String(a.reachedOn)));
  near.sort((a, b) => b.preserved - a.preserved || a.species.localeCompare(b.species));
  return { limit: PRESERVE_LIMIT, near: PRESERVE_NEAR, locations: RULE_LOCATIONS, reached, close: near };
}

/**
 * Every insectary butterfly preserved without its CAM or tube, newest death
 * first: { recordId, sheet, row, id, species, date (of death, else of
 * preservation; YYYY-MM-DD), kind (missing_sample / preserved_na), missing }.
 */
export function missingSamples(store, sheets) {
  const out = [];
  for (const row of sheets.get('Insectary_data') ?? []) {
    if (!row.observed) continue;
    const gap = sampleGap(row.values, row.formulas);
    if (!gap) continue;
    const day = [row.values.Death_date, row.values.Preservation_date].find(isDate) ?? null;
    out.push({
      recordId: row.id,
      sheet: row.sheet,
      row: row.row,
      id: text(row.values.Insectary_ID) || row.label,
      species: text(row.values.SPECIES),
      date: day ? iso(day) : null,
      day,
      kind: gap.kind,
      missing: gap.missing,
    });
  }
  return out.sort((a, b) => (b.day ?? 0) - (a.day ?? 0) || b.row - a.row).map(({ day, ...s }) => s);
}

const LINK = '#/revision?vista=alertas';
/** A row opened in the Buscador. */
const rowLink = (sheet, search) => `#/tablas?${new URLSearchParams({ hoja: sheet, buscar: search })}`;
/** The alerts as a list, most urgent first: { id, level (warn / info), text, textMsg, link }. */
function alertList(cams, rule, samples, today) {
  const out = [];
  const add = (id, level, m, link) => out.push({ id, level, text: m.text, textMsg: m.msg, link });
  const day = date => (date ? date.split('-').reverse().join('/') : '—');
  for (const p of cams) {
    if (p.level === 'out')
      add(`cam:${p.pool}`, 'warn', msg('{pool}: no quedan CAM en ningún rango. Pidan a PAS o AA un rango nuevo.', { pool: p.pool }), LINK);
    for (const r of p.ranges)
      if (r.level === 'low')
        add(
          `cam:${p.pool}:${r.first}`,
          'warn',
          msgn(
            r.left,
            '{pool}: queda {n} CAM en {first}–{last} (último {cam}, {date}). Pidan a PAS o AA un rango nuevo.',
            '{pool}: quedan {n} CAM en {first}–{last} (último {cam}, {date}). Pidan a PAS o AA un rango nuevo.',
            { pool: p.pool, first: r.first, last: r.last, cam: r.lastUsed.cam, date: day(r.lastUsed.date) },
          ),
          LINK,
        );
  }
  for (const s of rule.reached.filter(s => s.recent))
    add(
      `thirty:${s.species}`,
      'warn',
      msg('{species} llegó a {limit} preservadas el {date}: desde ahora se marca y libera, no se preserva.', {
        species: s.species,
        limit: rule.limit,
        date: day(s.reachedOn),
      }),
      LINK,
    );
  const since = iso(today - SAMPLE_DAYS);
  for (const s of samples.filter(s => s.date && s.date >= since)) {
    const vars = { id: s.id, species: s.species || '—', date: day(s.date) };
    const m =
      s.kind === 'preserved_na'
        ? msg('{id} ({species}): Death_cause Killed_Preserved el {date}, pero CAM_ID y los tubos dicen NA — pregunta al equipo', vars)
        : msg('{id} ({species}) preservada el {date} sin CAM/tubo — pregunta al equipo', vars);
    add(`sample:${s.recordId}`, 'warn', m, rowLink(s.sheet, s.id));
  }
  for (const s of rule.reached.filter(s => s.recentAfter))
    add(
      `thirty-after:${s.species}`,
      'info',
      msgn(
        s.recentAfter,
        '{species}: {n} preservada en los últimos 60 días después de llegar a {limit} ({total} en total).',
        '{species}: {n} preservadas en los últimos 60 días después de llegar a {limit} ({total} en total).',
        { species: s.species, limit: rule.limit, total: s.preserved },
      ),
      LINK,
    );
  for (const s of rule.close)
    add(
      `thirty-near:${s.species}`,
      'info',
      msgn(s.left, '{species}: {preserved} preservadas, falta {n} para {limit}.', '{species}: {preserved} preservadas, faltan {n} para {limit}.', {
        species: s.species,
        preserved: s.preserved,
        limit: rule.limit,
      }),
      LINK,
    );
  return out;
}

const cache = new WeakMap();
export const alertsStamp = store => `${recordsStamp(store)}:${todaySerial()}`;
/**
 * Everything the alerts know, computed again only when the local copy or the day changed, here
 * in this thread. The app's requests ask freshAlerts instead (in the Revisión worker thread,
 * server/checks-host.mjs); this is for the workers, the tests and the assistant's workers.
 */
export function alerts(store) {
  return alertsEntry(store).value;
}
/** The alerts with the state of the copy they were computed from: { stamp, value }. */
export function alertsEntry(store) {
  const today = todaySerial();
  const stamp = alertsStamp(store);
  const hit = cache.get(store);
  if (hit?.stamp === stamp) return hit;
  const started = Date.now();
  const sheets = sheetRows(store);
  const cams = camAlerts(sheets, today);
  const rule = preserveRule(sheets, today);
  const samples = missingSamples(store, sheets);
  const value = {
    computedAt: new Date().toISOString(),
    ms: Date.now() - started,
    thresholds: { camLeft: CAM_LOW_LEFT, camShare: CAM_LOW_SHARE },
    alerts: alertList(cams, rule, samples, today),
    camPools: cams,
    preserveRule: rule,
    missingSamples: samples,
  };
  return keepAlerts(store, { stamp, value });
}
/** Alerts computed in the worker ({ stamp, value }): the cached answer while the copy is as they were computed. */
export function keepAlerts(store, entry) {
  if (entry.stamp === alertsStamp(store)) cache.set(store, entry);
  return entry;
}
/** The kept alerts when they are still up to date, else null: { stamp, value }. */
export function cachedAlerts(store) {
  const hit = cache.get(store);
  return hit?.stamp === alertsStamp(store) ? hit : null;
}

/** Who computes the alerts for the app's requests (server/checks-host.mjs: a worker thread), by store. */
const runners = new WeakMap();
export function useAlertsRunner(store, runner) {
  if (runner) runners.set(store, runner);
  else runners.delete(store);
}
/**
 * The alerts as of now, for the app's requests: the kept ones when nothing changed since, else
 * computed after this call (in the Revisión worker, where there is one). A promise.
 */
export async function freshAlerts(store) {
  const runner = runners.get(store);
  return (runner ? await runner.fresh() : alertsEntry(store)).value;
}
