// Alerts: things the team must act on before they become a problem, shown in
// Revisión → Alertas, on Inicio and to the assistant (review_issues show "alerts").
//
// CAM pools. The CAM_ID columns take their values from pools in the Lists sheet
// (InsectaryWild&Reared_CAMid, Wild_indv_CAMid…, the dropdowns of
// server/verifications.mjs). A pool is made of ranges of consecutive CAMs that PAS
// or AA hand out; the team uses each range upwards. For every range: how many are
// used (any sheet), the highest used, how many are left above it (gaps below it
// are counted apart: they are usually skipped for good), and when it was last
// used. A range in use in the last year with fewer than 50 left, or less than
// 15 % of it, is an alert: time to ask PAS or AA for a new range. Unless the pool
// already has its next range: a range on Lists with none of its CAMs used yet
// (PAS or AA added it). Then the low range is information that names the next
// one, not an alert. A range whose last CAM is older than about six months
// (IDLE_DAYS) is not being used: what is left in it is information with the date
// («sin uso desde…»), not a call to ask for a new range.
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
//
// Censuses (server/census.mjs). A census opened in the Censo tab that has no
// marks a week later (CENSUS_IDLE_DAYS): when its species already has census
// disappearances (Death_cause Disappearance) dated within a week of its day,
// entered by any route (Muertes, proposals, Sheets), the census was entered
// another way and the alert is information (the session can be cancelled);
// otherwise it is «abierto sin usar», a warn, to mark it from the notebook or
// cancel it. A proposal of the assistant's chats with census disappearances
// still pending two weeks after it was drafted (CENSUS_PROPOSAL_DAYS): an
// alert per chat, counting each butterfly once and only those still without
// a death date. When rows of those proposals changed since they were drafted
// (a record's version is not the one the proposal expected) applying them
// would overwrite newer data: the alert says to redo those disappearances from
// the notebook, not to apply them. Each alert leads with the outcome in the
// base and keeps who opened the census or the chat.
//
// Workbook ranges left for their owners (scripts/nightly-app-checks.mjs --sheet-ranges): a dropdown or a conditional
// format whose fixed end the nightly could not move because the range is protected from the app. Read from the
// nightly's state file; information at the end of the list, gone once the nightly no longer leaves it out.

import { readFileSync, statSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { msg, msgn } from './messages.mjs';
import { iso, recordsStamp, sheetRows, todaySerial } from './checks.mjs';
import { moduleMap } from './schema.mjs';
import { LISTS } from './verifications.mjs';
import { sampleGap } from './preserved.mjs';
import { DISAPPEARED, inTaxon, serialOf } from './census.mjs';

export const CAM_LOW_LEFT = 50;
export const CAM_LOW_SHARE = 0.15;
export const PRESERVE_LIMIT = 30;
export const PRESERVE_NEAR = 25;
export const RULE_LOCATIONS = ['Ikiam', 'Casa de Lin', 'Mariposario Ikiam'];
/** How long a species that reached 30 (or a range in use) stays news. */
const RECENT_DAYS = 60;
const ACTIVE_DAYS = 365;
/** A range whose last CAM is older than this is not in use: what is left is information, not an alert. */
export const IDLE_DAYS = 183;
/** A census open with no marks after this many days is an alert. */
export const CENSUS_IDLE_DAYS = 7;
/** Disappearances of a census's species dated this many days around its day cover it. */
export const CENSUS_COVER_DAYS = 7;
/** A proposal with census disappearances pending after this many days is an alert. */
export const CENSUS_PROPOSAL_DAYS = 14;
/** How long a butterfly preserved without CAM or tube stays an alert (older ones are in Revisión). */
export const SAMPLE_DAYS = 180;

/** The nightly's state file of ranges left for their owners: shared/nightly-tests, beside the database. */
export function sheetRangesStateFile(env = process.env) {
  return env.DATABASE_PATH ? join(dirname(resolve(env.DATABASE_PATH)), 'nightly-tests', 'sheet-ranges-state.json') : null;
}
/** The ranges in that file ([] when there is none or it does not read). */
export function leftOutRanges(file = sheetRangesStateFile()) {
  if (!file) return [];
  try {
    return Object.values(JSON.parse(readFileSync(file, 'utf8')).leftOut || {});
  } catch {
    return [];
  }
}
const fileStamp = file => {
  try {
    return String(statSync(file).mtimeMs);
  } catch {
    return '-';
  }
};

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
      const idle = active && today - lastDay > IDLE_DAYS;
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
        idle,
      };
    });
    // The pool's next range, already on Lists: a fresh one (nothing used), after the low range when there is one.
    const fresh = described.filter(r => r.used === 0 && r.size >= CAM_LOW_LEFT);
    for (const r of described)
      if (r.level === 'low') {
        const next = fresh.find(f => f.first > r.last) ?? fresh.find(f => f !== r) ?? null;
        if (next) Object.assign(r, { level: 'next', nextRange: { first: next.first, last: next.last, size: next.size } });
        // Not used for months (CRISPRonly_CAMid, last CAM 19/11/2025): nobody needs a new range yet.
        else if (r.idle) r.level = 'idle';
      }
    for (const r of described) delete r.idle;
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

/** The app's tables that exist in this database (a test's may lack the assistant's or the census's). */
const tablesIn = db =>
  new Set(
    db
      .prepare("SELECT name FROM sqlite_master WHERE type = 'table' AND name IN ('censuses', 'census_marks', 'ai_proposals', 'users')")
      .all()
      .map(t => t.name),
  );
const ECUADOR_DAY = new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' });
/** An instant (ISO) as the sheet's serial of its day in Ecuador. */
const serialAt = at => serialOf(ECUADOR_DAY.format(new Date(at)));

const blankCell = v => v === null || v === undefined || (typeof v === 'string' && v.trim() === '');

/**
 * The census disappearances (Death_cause Disappearance) of Insectary_data dated within
 * CENSUS_COVER_DAYS of `day` for the census's species (a species takes its subspecies, as the
 * census does): { count, dates (YYYY-MM-DD, oldest first) }, or null when there are none.
 */
function coveredBy(sheets, species, day) {
  const at = serialOf(day);
  const dates = new Map();
  let count = 0;
  for (const r of sheets.get('Insectary_data') ?? []) {
    const v = r.values;
    if (text(v.Death_cause) !== DISAPPEARED || !isDate(v.Death_date) || Math.abs(v.Death_date - at) > CENSUS_COVER_DAYS) continue;
    if (!inTaxon(species, v.SPECIES)) continue;
    count++;
    dates.set(v.Death_date, (dates.get(v.Death_date) ?? 0) + 1);
  }
  return count ? { count, dates: [...dates.keys()].sort((a, b) => a - b).map(iso) } : null;
}

/**
 * Censuses open in the Censo tab with no marks more than CENSUS_IDLE_DAYS after their day, oldest
 * first: { id, species, day (YYYY-MM-DD), expected, days, by, covered }. `covered` (with `sheets`):
 * the disappearances of its species already entered by another route ({ count, dates }), else null.
 */
export function idleCensuses(store, today = todaySerial(), sheets = null) {
  const tables = tablesIn(store.db);
  if (!tables.has('censuses') || !tables.has('census_marks')) return [];
  const who = tables.has('users') ? '(SELECT display_name FROM users u WHERE u.id = c.created_by)' : 'NULL';
  return store.db
    .prepare(
      `SELECT c.id, c.species, c.day, c.expected, coalesce(${who}, c.created_by) AS by FROM censuses c
       WHERE c.status = 'open' AND NOT EXISTS (SELECT 1 FROM census_marks m WHERE m.census_id = c.id) ORDER BY c.day, c.created_at`,
    )
    .all()
    .map(c => ({ id: c.id, species: c.species, day: c.day, expected: c.expected, days: today - serialOf(c.day), by: c.by }))
    .filter(c => c.days > CENSUS_IDLE_DAYS)
    .map(c => ({ ...c, covered: sheets ? coveredBy(sheets, c.species, c.day) : null }));
}

/**
 * The assistant's proposals with census disappearances (Death_cause Disappearance) still pending
 * (or to review) more than CENSUS_PROPOSAL_DAYS after they were drafted, by chat, oldest first:
 * { chat (T3 thread, else the app's thread), title, by, proposals (ids), censusDays (the Death_date
 * days of their disappearances, YYYY-MM-DD), disappearances (butterflies,
 * each once), open (of those, still without a Death_date), rows (rows the proposals write, each
 * once), changed (of those, changed since: their version is not the one expected, or gone), stale
 * (changed > 0), since (YYYY-MM-DD), days, link }.
 */
export function staleCensusProposals(store, today = todaySerial()) {
  const tables = tablesIn(store.db);
  if (!tables.has('ai_proposals')) return [];
  const who = tables.has('users') ? '(SELECT display_name FROM users u WHERE u.id = p.owner_id)' : 'NULL';
  const rows = store.db
    .prepare(
      `SELECT p.id, p.thread_id, p.t3_thread, p.t3_title, p.created_at, p.changes_json, coalesce(${who}, p.owner_id) AS by FROM ai_proposals p
       WHERE p.status IN ('pending', 'needs_review') AND instr(p.changes_json, ?) > 0 ORDER BY p.created_at, p.rowid`,
    )
    .all(DISAPPEARED);
  const chats = new Map();
  for (const p of rows) {
    const days = today - serialAt(p.created_at);
    if (!(days > CENSUS_PROPOSAL_DAYS)) continue;
    let changes = [];
    try {
      changes = JSON.parse(p.changes_json) ?? [];
    } catch {
      continue;
    }
    const writes = changes.filter(c => c && !c.context && c.recordId);
    const gone = writes.filter(c => text(c.values?.Death_cause) === DISAPPEARED);
    if (!gone.length) continue;
    const key = p.t3_thread || `app:${p.thread_id}`;
    let chat = chats.get(key);
    if (!chat)
      chats.set(
        key,
        (chat = {
          chat: key,
          title: text(p.t3_title) || 'T3 Code',
          by: p.by,
          proposals: [],
          gone: new Set(),
          censusDays: new Set(),
          rows: new Map(),
          since: iso(serialAt(p.created_at)),
          days,
          link: p.t3_thread ? `#/propuestas?${new URLSearchParams({ chat: p.t3_thread })}` : `#/propuestas/${encodeURIComponent(p.id)}`,
        }),
      );
    chat.proposals.push(p.id);
    for (const c of gone) {
      chat.gone.add(c.recordId);
      if (isDate(c.values?.Death_date)) chat.censusDays.add(c.values.Death_date);
    }
    for (const c of writes) (chat.rows.get(c.recordId) ?? chat.rows.set(c.recordId, []).get(c.recordId)).push(c.expectedVersion);
  }
  const record = store.db.prepare('SELECT version, values_json FROM records WHERE id = ? AND missing = 0');
  return [...chats.values()].map(({ gone, rows, censusDays, ...chat }) => {
    const now = new Map([...rows.keys()].map(id => [id, record.get(id)]));
    // A row in two proposals changed if either expected another version.
    const changed = [...rows].filter(([id, expected]) => {
      const r = now.get(id);
      return !r || expected.some(v => v !== undefined && v !== null && v !== r.version);
    }).length;
    const open = [...gone].filter(id => {
      const r = now.get(id);
      return r && blankCell(JSON.parse(r.values_json).Death_date);
    }).length;
    const days = [...censusDays].sort((a, b) => a - b).map(iso);
    return { ...chat, censusDays: days, disappearances: gone.size, open, rows: rows.size, changed, stale: changed > 0 };
  });
}

const LINK = '#/revision?vista=alertas';
const CENSUS_LINK = '#/censo';
/** A row opened in the Buscador. */
const rowLink = (sheet, search) => `#/tablas?${new URLSearchParams({ hoja: sheet, buscar: search })}`;
/**
 * The alerts as a list, most urgent first: { id, level (warn / info), text, textMsg, link, action? }.
 * `action` { kind: 'cancel-census', census }: the app offers to cancel that census session.
 */
export function alertList(cams, rule, samples, today, censuses = [], censusProposals = [], ranges = []) {
  const out = [];
  const add = (id, level, m, link, action) => out.push({ id, level, text: m.text, textMsg: m.msg, link, ...(action ? { action } : {}) });
  const day = date => (date ? date.split('-').reverse().join('/') : '—');
  for (const p of cams) {
    if (p.level === 'out')
      add(`cam:${p.pool}`, 'warn', msg('{pool}: no quedan CAM en ningún rango. Pidan a PAS o AA un rango nuevo.', { pool: p.pool }), LINK);
    for (const r of p.ranges) {
      if (r.level === 'next')
        add(
          `cam:${p.pool}:${r.first}`,
          'info',
          msgn(
            r.left,
            '{pool}: queda {n} CAM en {first}–{last} (último {cam}, {date}); el siguiente rango ya está en Lists: {nextFirst}–{nextLast}.',
            '{pool}: quedan {n} CAM en {first}–{last} (último {cam}, {date}); el siguiente rango ya está en Lists: {nextFirst}–{nextLast}.',
            { pool: p.pool, first: r.first, last: r.last, cam: r.lastUsed.cam, date: day(r.lastUsed.date), nextFirst: r.nextRange.first, nextLast: r.nextRange.last },
          ),
          LINK,
        );
      if (r.level === 'idle')
        add(
          `cam:${p.pool}:${r.first}`,
          'info',
          msgn(
            r.left,
            '{pool}: queda {n} CAM en {first}–{last}, sin uso desde {date} (último {cam}).',
            '{pool}: quedan {n} CAM en {first}–{last}, sin uso desde {date} (último {cam}).',
            { pool: p.pool, first: r.first, last: r.last, cam: r.lastUsed.cam, date: day(r.lastUsed.date) },
          ),
          LINK,
        );
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
  }
  const days = dates => dates.map(day).join(', ');
  for (const c of censuses) {
    const action = { kind: 'cancel-census', census: c.id };
    if (c.covered)
      add(
        `census-open:${c.id}`,
        'info',
        msgn(
          c.covered.count,
          'Censo de {species} del {date}: ya está en la base por otra vía ({n} desaparición de {species} del {dates}). La sesión de la pestaña Censo que abrió {by} quedó sin marcas; se puede cancelar.',
          'Censo de {species} del {date}: ya está en la base por otra vía ({n} desapariciones de {species} del {dates}). La sesión de la pestaña Censo que abrió {by} quedó sin marcas; se puede cancelar.',
          { species: c.species, date: day(c.day), dates: days(c.covered.dates), by: c.by },
        ),
        CENSUS_LINK,
        action,
      );
    else
      add(
        `census-open:${c.id}`,
        'warn',
        msg(
          'Censo de {species} del {date}: abierto sin usar. La sesión que abrió {by} en la pestaña Censo no tiene marcas ({expected} mariposas esperadas) y la base no tiene desapariciones de {species} de esa semana. Si se contó en papel, pasen las marcas; si no se hizo, cancélenlo.',
          { species: c.species, date: day(c.day), expected: c.expected, by: c.by },
        ),
        CENSUS_LINK,
        action,
      );
  }
  for (const c of censusProposals) {
    const vars = { gone: c.disappearances, open: c.open, date: day(c.since), census: days(c.censusDays) || '—', title: c.title, by: c.by, changed: c.changed, rows: c.rows };
    if (c.stale && c.open)
      add(
        `census-proposals:${c.chat}`,
        'warn',
        msgn(
          c.open,
          'Censo del {census} (propuestas del {date}, chat «{title}» de {by}): {n} de las {gone} mariposas que sus propuestas dan por desaparecidas sigue sin fecha de muerte y cuenta como viva. Las propuestas quedaron viejas ({changed} de sus {rows} filas cambiaron desde entonces): no las apliquen; rehagan esas desapariciones desde el cuaderno.',
          'Censo del {census} (propuestas del {date}, chat «{title}» de {by}): {n} de las {gone} mariposas que sus propuestas dan por desaparecidas siguen sin fecha de muerte y cuentan como vivas. Las propuestas quedaron viejas ({changed} de sus {rows} filas cambiaron desde entonces): no las apliquen; rehagan esas desapariciones desde el cuaderno.',
          vars,
        ),
        c.link,
      );
    else if (c.stale)
      add(
        `census-proposals:${c.chat}`,
        'info',
        msgn(
          c.proposals.length,
          'Censo del {census} (propuesta del {date}, chat «{title}» de {by}): las {gone} mariposas de su propuesta ya tienen fecha de muerte. La propuesta quedó vieja ({changed} de sus {rows} filas cambiaron desde entonces): descártenla.',
          'Censo del {census} (propuestas del {date}, chat «{title}» de {by}): las {gone} mariposas de sus {n} propuestas ya tienen fecha de muerte. Las propuestas quedaron viejas ({changed} de sus {rows} filas cambiaron desde entonces): descártenlas.',
          vars,
        ),
        c.link,
      );
    else
      add(
        `census-proposals:${c.chat}`,
        'warn',
        msgn(
          c.proposals.length,
          'Censo: {n} propuesta con {gone} desaparecidas espera desde el {date} sin aplicar (chat «{title}» de {by}). Revísenla y aplíquenla o descártenla; mientras tanto esas mariposas cuentan como vivas.',
          'Censo: {n} propuestas con {gone} desaparecidas esperan desde el {date} sin aplicar (chat «{title}» de {by}). Revísenlas y aplíquenlas o descártenlas; mientras tanto esas mariposas cuentan como vivas.',
          vars,
        ),
        c.link,
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
  // One alert per sheet and rule: dropdown runs of a sheet with the same list are named together.
  const groups = new Map();
  for (const r of ranges) {
    const key = [r.kind, r.sheet, r.before, r.after].join('\u0000');
    if (!groups.has(key)) groups.set(key, { ...r, cells: [] });
    const g = groups.get(key);
    // The label of a dropdown: «Sheet A2:A226»; a conditional format is named by its formula.
    g.cells.push(String(r.label).slice(String(r.sheet).length + 1).replace(/^([A-Z]+\d+):\1$/, '$1'));
    if (String(r.since) < String(g.since)) g.since = r.since;
  }
  for (const r of groups.values()) {
    const vars = { sheet: r.sheet, cells: r.cells.join(', '), rows: r.rows, before: r.before, after: r.after, date: day(String(r.since).slice(0, 10)) };
    add(
      `sheet-range:${r.key || `${r.kind}:${r.label}`}`,
      'info',
      r.kind === 'dropdown'
        ? msg(
            '{sheet} {cells}: el desplegable toma sus opciones de {before} y la lista ya llega a la fila {rows}; debería ser {after}. La app no puede cambiarlo (rango protegido): PAS puede hacerlo a mano (Datos → Validación de datos). Visto el {date}.',
            vars,
          )
        : msg(
            '{sheet}, formato condicional {before}: debería ser {after}. La app no puede cambiarlo (rango protegido): PAS puede hacerlo a mano (Formato → Formato condicional). Visto el {date}.',
            vars,
          ),
    );
  }
  return out;
}

const cache = new WeakMap();
/** What the census alerts read besides the copy: the open censuses, their marks, the pending proposals. */
function appStamp(store) {
  const tables = tablesIn(store.db);
  const census = tables.has('censuses') && tables.has('census_marks')
    ? store.db.prepare("SELECT (SELECT count(*) FROM censuses WHERE status = 'open') || '.' || (SELECT count(*) FROM census_marks) s").get().s
    : '-';
  const proposals = tables.has('ai_proposals')
    ? store.db.prepare("SELECT count(*) || '.' || total(revision) s FROM ai_proposals WHERE status IN ('pending', 'needs_review')").get().s
    : '-';
  return `${census}:${proposals}`;
}
export const alertsStamp = store => `${recordsStamp(store)}:${todaySerial()}:${appStamp(store)}:${fileStamp(sheetRangesStateFile())}`;
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
  const censuses = idleCensuses(store, today, sheets);
  const censusProposals = staleCensusProposals(store, today);
  const value = {
    computedAt: new Date().toISOString(),
    ms: Date.now() - started,
    thresholds: { camLeft: CAM_LOW_LEFT, camShare: CAM_LOW_SHARE },
    alerts: alertList(cams, rule, samples, today, censuses, censusProposals, leftOutRanges()),
    camPools: cams,
    preserveRule: rule,
    missingSamples: samples,
    openCensuses: censuses,
    censusProposals,
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
