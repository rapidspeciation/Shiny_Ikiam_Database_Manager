// The home page ("Inicio"): project-wide summaries computed from the local copy.
// Anyone can read the collection, monitoring, CRISPR and crossing summaries;
// the insectary's state (alive, clutches in progress, deaths) needs a session.
// Visitors without an account also get the monitoring report's rows, reduced
// to the columns its charts use (no coordinates, notes, tubes or CAM IDs).

import { moduleMap } from './schema.mjs';
import { tablePayload, tableRevision } from './grid.mjs';

const DAY = 86_400_000;
const EPOCH = Date.UTC(1899, 11, 30);
/** A clutch laid longer ago than this without an emergence date is treated as finished (never updated). */
const CLUTCH_DAYS = 120;
/** A butterfly without a death date counts as alive only this long after it entered the insectary. */
const ALIVE_DAYS = 90;

const text = value => (value === null || value === undefined ? '' : String(value).trim());
const blank = value => /^(|NA|N\/A|NULL|-|—)$/i.test(text(value));
const date = value => (typeof value === 'number' && value > 30000 && value < 60000 ? Math.floor(value) : null);
const month = serial => new Date(EPOCH + serial * DAY).toISOString().slice(0, 7);
const iso = serial => (serial === null ? null : new Date(EPOCH + serial * DAY).toISOString().slice(0, 10));
const num = value => (typeof value === 'number' && Number.isFinite(value) ? value : null);

/** Today in Ecuador as a Sheets date serial. */
export function todaySerial(now = new Date()) {
  const day = new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' }).format(now);
  return Math.round((Date.parse(`${day}T00:00:00Z`) - EPOCH) / DAY);
}

function rowsOf(store, sheet) {
  const mod = moduleMap.get(sheet);
  if (!mod) return [];
  return store.db
    .prepare('SELECT values_json FROM records WHERE sheet=? AND missing=0 AND observed=1 AND row_num>? AND row_num<2000000000')
    .all(sheet, mod.headerRow)
    .map(r => JSON.parse(r.values_json));
}

/** Counts by key, largest first, as [{ name, n }]. */
function tally(items, keyOf, limit = Infinity) {
  const counts = new Map();
  for (const item of items) {
    const key = keyOf(item);
    if (key) counts.set(key, (counts.get(key) || 0) + 1);
  }
  return [...counts]
    .map(([name, n]) => ({ name, n }))
    .sort((a, b) => b.n - a.n || a.name.localeCompare(b.name))
    .slice(0, limit);
}

/** The last `count` months ending at `today`, as "YYYY-MM". */
function lastMonths(today, count = 12) {
  const [y, m] = month(today).split('-').map(Number);
  return Array.from({ length: count }, (_, i) => {
    const d = new Date(Date.UTC(y, m - 1 - (count - 1 - i), 1));
    return d.toISOString().slice(0, 7);
  });
}

const speciesName = row => [text(row.SPECIES), text(row.Subspecies_Form)].filter(s => s && !blank(s)).join(' ');

// ----------------------------------------------------------------- public

function collections(rows, today) {
  const kinds = {
    Collected_Preserved: 'preserved',
    Collected_Sent2Insectary: 'insectary',
    Mark_Released: 'marked',
    Released_Unmarked: 'released',
  };
  const records = rows.filter(r => kinds[text(r.Release_Collect)] && date(r.Collection_date));
  const months = lastMonths(today);
  const byMonth = Object.fromEntries(Object.values(kinds).map(k => [k, months.map(() => 0)]));
  const years = new Map();
  for (const r of records) {
    const d = date(r.Collection_date);
    const i = months.indexOf(month(d));
    if (i >= 0) byMonth[kinds[text(r.Release_Collect)]][i]++;
    const y = month(d).slice(0, 4);
    years.set(y, (years.get(y) || 0) + 1);
  }
  const named = records.filter(r => !blank(r.SPECIES) && !/NOT_FOUND/i.test(text(r.SPECIES)));
  const places = new Map();
  for (const r of records) {
    const place = text(r.Collection_location);
    if (blank(place)) continue;
    const p = places.get(place) || { name: place, n: 0, species: new Set(), last: 0 };
    p.n++;
    if (!blank(r.SPECIES)) p.species.add(text(r.SPECIES));
    p.last = Math.max(p.last, date(r.Collection_date));
    places.set(place, p);
  }
  const last = Math.max(0, ...records.map(r => date(r.Collection_date)));
  return {
    total: records.length,
    preserved: records.filter(r => kinds[text(r.Release_Collect)] === 'preserved').length,
    toInsectary: records.filter(r => kinds[text(r.Release_Collect)] === 'insectary').length,
    marked: records.filter(r => kinds[text(r.Release_Collect)] === 'marked').length,
    species: new Set(named.map(r => text(r.SPECIES))).size,
    genera: new Set(named.map(r => text(r.SPECIES).split(' ')[0])).size,
    places: places.size,
    lastDate: iso(last || null),
    last12: records.filter(r => date(r.Collection_date) > today - 365).length,
    months,
    byMonth,
    byYear: [...years].sort().map(([year, n]) => ({ year, n })),
    topSpecies: tally(named, r => text(r.SPECIES), 12),
    topPlaces: [...places.values()]
      .sort((a, b) => b.n - a.n)
      .slice(0, 12)
      .map(p => ({ name: p.name, n: p.n, species: p.species.size, last: iso(p.last || null) })),
  };
}

/** Collector-days of Ikiam monitoring, as in the report (SamplingDay_data). */
function monitoringDayKeys(days) {
  const out = new Set();
  for (const r of days) {
    const d = date(r.Date);
    if (d && /^monitor/i.test(text(r.Purpose)) && /ikiam/i.test(text(r.Location)))
      out.add(`${d}|${text(r.Collectors_initials).toUpperCase()}`);
  }
  return out;
}

const initials = collector => text(collector).split(' - ')[0].trim().toUpperCase();

/** Rows counted by the monitoring report: Ikiam, Purpose "Monitoring…", or no Purpose on a monitoring day. */
export function isMonitoring(row, dayKeys) {
  if (!/^ikiam$/i.test(text(row.Collection_location))) return false;
  if (/^monitoring/i.test(text(row.Purpose))) return true;
  const d = date(row.Collection_date);
  return d !== null && /^(|na|n\/a|none)$/i.test(text(row.Purpose)) && dayKeys.has(`${d}|${initials(row.Collector)}`);
}

function monitoring(rows, days, today) {
  const dayKeys = monitoringDayKeys(days);
  const records = rows.filter(r => isMonitoring(r, dayKeys));
  const effort = new Set(dayKeys);
  for (const r of records) if (date(r.Collection_date)) effort.add(`${date(r.Collection_date)}|${initials(r.Collector)}`);
  const months = lastMonths(today);
  const perMonth = months.map(() => 0);
  const daysPerMonth = months.map(() => 0);
  for (const r of records) {
    const i = months.indexOf(month(date(r.Collection_date) ?? 0));
    if (i >= 0) perMonth[i]++;
  }
  for (const key of effort) {
    const i = months.indexOf(month(Number(key.split('|')[0])));
    if (i >= 0) daysPerMonth[i]++;
  }
  const dates = [...effort].map(k => Number(k.split('|')[0]));
  return {
    individuals: records.length,
    days: effort.size,
    species: new Set(records.filter(r => !blank(r.SPECIES)).map(r => text(r.SPECIES))).size,
    marked: records.filter(r => text(r.Release_Collect) === 'Mark_Released').length,
    preserved: records.filter(r => text(r.Release_Collect) === 'Collected_Preserved').length,
    first: iso(dates.length ? Math.min(...dates) : null),
    last: iso(dates.length ? Math.max(...dates) : null),
    months,
    perMonth,
    daysPerMonth,
    topSpecies: tally(
      records.filter(r => !blank(r.SPECIES)),
      r => text(r.SPECIES),
      8,
    ),
  };
}

function crispr(rows) {
  const eggs = rows.filter(r => !blank(r['CRISPR_No.']));
  const byStock = new Map();
  const experiments = new Set();
  let firstDate = Infinity,
    lastDate = 0;
  for (const r of eggs) {
    const stock = blank(r.Stock_of_origin) ? 'Sin stock' : text(r.Stock_of_origin);
    const s = byStock.get(stock) || { name: stock, experiments: new Set(), eggs: 0, hatched: 0, pupae: 0, adults: 0, mutants: 0, checked: 0 };
    s.experiments.add(text(r['CRISPR_No.']));
    s.eggs++;
    if (date(r.Hatch_date)) s.hatched++;
    if (date(r.Pupa_date)) s.pupae++;
    if (date(r.Emerge_date)) s.adults++;
    if (/^yes$/i.test(text(r.Mutant))) s.mutants++;
    if (/^(yes|no)$/i.test(text(r.Mutant))) s.checked++;
    byStock.set(stock, s);
    experiments.add(text(r['CRISPR_No.']));
    const d = date(r.CRISPR_date);
    if (d) {
      firstDate = Math.min(firstDate, d);
      lastDate = Math.max(lastDate, d);
    }
  }
  const stocks = [...byStock.values()]
    .map(s => ({ ...s, experiments: s.experiments.size }))
    .sort((a, b) => b.eggs - a.eggs);
  const sum = key => stocks.reduce((n, s) => n + s[key], 0);
  return {
    experiments: experiments.size,
    eggs: eggs.length,
    hatched: sum('hatched'),
    pupae: sum('pupae'),
    adults: sum('adults'),
    mutants: sum('mutants'),
    checked: sum('checked'),
    first: iso(Number.isFinite(firstDate) ? firstDate : null),
    last: iso(lastDate || null),
    stocks,
  };
}

function crosses(store) {
  const melinaea = rowsOf(store, 'Melinaea_crosses').filter(r => !blank(r.Female) || !blank(r['Cross direction']));
  const generations = rowsOf(store, 'F1/F2_MutationRate').filter(r => !blank(r.Insectary_ID));
  const matings = rowsOf(store, 'Stocks_Matings').filter(r => date(r.Mating_date));
  const lysPol = rowsOf(store, 'Crosses_Lys_x_Pol').filter(r => !blank(r['female Id']));
  const clutches = rowsOf(store, 'Insectary_stocks').filter(r => /^(F1|F2|Backcross)$/i.test(text(r.Generation)));

  // Individuals of each cross by generation (F1/F2 mutation rate).
  const ORDER = ['P', 'F1', 'F2', 'Backcross'];
  const byCross = new Map();
  for (const r of generations) {
    const cross = blank(r.species2) ? text(r.SPECIES) || 'Sin cruce' : text(r.species2);
    const c = byCross.get(cross) || { name: cross, P: 0, F1: 0, F2: 0, Backcross: 0, total: 0 };
    const g = ORDER.find(o => o.toLowerCase() === text(r.Generation).toLowerCase());
    if (g) c[g]++;
    c.total++;
    byCross.set(cross, c);
  }
  const directions = new Map();
  for (const r of melinaea) {
    const name = blank(r['Cross direction']) ? 'Sin dirección' : text(r['Cross direction']);
    const d = directions.get(name) || { name, couples: 0, mated: 0, clutches: 0 };
    d.couples++;
    if (num(r.Mating_started)) d.mated++;
    if (!blank(r['#clutch'])) d.clutches++;
    directions.set(name, d);
  }
  return {
    lines: [...byCross.values()].sort((a, b) => b.total - a.total),
    individuals: generations.length,
    melinaea: {
      couples: melinaea.length,
      mated: melinaea.filter(r => num(r.Mating_started)).length,
      clutches: melinaea.filter(r => !blank(r['#clutch'])).length,
      directions: [...directions.values()].sort((a, b) => b.couples - a.couples),
    },
    matings: { total: matings.length, bySpecies: tally(matings, r => text(r.Species)) },
    lysimniaPolymnia: {
      couples: lysPol.length,
      attempts: new Set(lysPol.map(r => text(r['Attempt N']))).size,
      mated: lysPol.filter(r => date(r['MATING DATE'])).length,
    },
    clutchesByGeneration: tally(clutches, r => text(r.Generation)),
  };
}

// ----------------------------------------------------------------- private

/** The date a butterfly entered the insectary: caught (wild) or its clutch's emergence (reared). */
function entered(row, clutchOf) {
  if (/wild/i.test(text(row.Wild_Reared))) return date(row.Intro2Insectary_date);
  const clutch = clutchOf.get(text(row['CLUTCH NUMBER']));
  if (!clutch) return null;
  return date(clutch['EMERGENCE DATE']) ?? date(clutch['PUPA DATE']) ?? (date(clutch['DATE LAID']) ? date(clutch['DATE LAID']) + 40 : null);
}

/** Stage of a clutch still in progress, and how many individuals it holds now. */
export function clutchStage(row) {
  const count = (...keys) => keys.map(k => num(row[k])).find(n => n !== null && n < 5000) ?? null;
  if (date(row['PUPA DATE'])) return { stage: 'pupa', n: count('NUMBER OF PUPA', 'NUMBER OF LARVAE', 'NUMBER OF EGGS') };
  if (date(row['HATCHING DATE']) || num(row['NUMBER OF LARVAE']))
    return { stage: 'larva', n: count('NUMBER OF LARVAE', 'NUMBER OF EGGS') };
  return { stage: 'egg', n: count('NUMBER OF EGGS') };
}

function insectary(store, today) {
  const stocks = rowsOf(store, 'Insectary_stocks');
  const clutchOf = new Map(stocks.map(r => [text(r['CLUTCH NUMBER']), r]));
  const individuals = rowsOf(store, 'Insectary_data').filter(r => !blank(r.Insectary_ID) && !blank(r.SPECIES));

  const withoutDeath = individuals.filter(r => !date(r.Death_date));
  const alive = [];
  let stale = 0;
  for (const r of withoutDeath) {
    const since = entered(r, clutchOf);
    if (since !== null && since > today - ALIVE_DAYS) alive.push(r);
    else stale++;
  }
  const bySpecies = new Map();
  for (const r of alive) {
    const name = text(r.SPECIES);
    const s = bySpecies.get(name) || { name, female: 0, male: 0, other: 0, wild: 0, reared: 0, total: 0 };
    const sex = text(r.Sex).toLowerCase();
    if (sex === 'female') s.female++;
    else if (sex === 'male') s.male++;
    else s.other++;
    if (/wild/i.test(text(r.Wild_Reared))) s.wild++;
    else s.reared++;
    s.total++;
    bySpecies.set(name, s);
  }

  const ongoing = stocks.filter(r => {
    const laid = date(r['DATE LAID']);
    return laid && laid > today - CLUTCH_DAYS && !date(r['EMERGENCE DATE']) && blank(r['NUMBER OF ADULTS']);
  });
  const stages = { egg: { clutches: 0, n: 0 }, larva: { clutches: 0, n: 0 }, pupa: { clutches: 0, n: 0 } };
  const clutchSpecies = new Map();
  for (const r of ongoing) {
    const { stage, n } = clutchStage(r);
    stages[stage].clutches++;
    stages[stage].n += n ?? 0;
    const name = blank(r.SPECIES) ? 'Sin especie' : text(r.SPECIES);
    const s = clutchSpecies.get(name) || { name, clutches: 0, egg: 0, larva: 0, pupa: 0 };
    s.clutches++;
    s[stage] += n ?? 0;
    clutchSpecies.set(name, s);
  }

  const months = lastMonths(today);
  const deaths = individuals.filter(r => date(r.Death_date));
  const recent = deaths.filter(r => date(r.Death_date) > today - 30);
  const deathsPerMonth = months.map(m => deaths.filter(r => month(date(r.Death_date)) === m).length);
  const preservedPerMonth = months.map(
    m => deaths.filter(r => month(date(r.Death_date)) === m && /preserved/i.test(text(r.Death_cause))).length,
  );
  const arrivals = individuals.filter(r => {
    const since = entered(r, clutchOf);
    return since !== null && since > today - 30;
  });
  return {
    aliveDays: ALIVE_DAYS,
    clutchDays: CLUTCH_DAYS,
    alive: alive.length,
    aliveFemale: alive.filter(r => /^female$/i.test(text(r.Sex))).length,
    aliveMale: alive.filter(r => /^male$/i.test(text(r.Sex))).length,
    aliveWild: alive.filter(r => /wild/i.test(text(r.Wild_Reared))).length,
    stale,
    aliveBySpecies: [...bySpecies.values()].sort((a, b) => b.total - a.total),
    clutches: ongoing.length,
    stages,
    clutchesBySpecies: [...clutchSpecies.values()].sort((a, b) => b.clutches - a.clutches),
    arrivals30: arrivals.length,
    deaths30: recent.length,
    deathCauses30: tally(recent, r => (blank(r.Death_cause) ? 'Sin causa' : text(r.Death_cause))),
    deathSpecies30: tally(recent, r => text(r.SPECIES), 10),
    months,
    deathsPerMonth,
    preservedPerMonth,
  };
}

// ----------------------------------------------------------------- entry points

const PUBLIC_SHEETS = ['Collection_data', 'SamplingDay_data', 'CRISPR', 'Melinaea_crosses', 'F1/F2_MutationRate', 'Stocks_Matings', 'Crosses_Lys_x_Pol', 'Insectary_stocks'];
const PRIVATE_SHEETS = ['Insectary_data', 'Insectary_stocks'];

export function createSummary(store) {
  const cache = new Map();
  function cached(key, sheets, build) {
    const revision = `${todaySerial()}:${sheets.map(s => tableRevision(store, s)).join(',')}`;
    const hit = cache.get(key);
    if (hit?.revision === revision) return hit.value;
    const value = build();
    cache.set(key, { revision, value });
    return value;
  }
  return {
    /** Everything for the home page; `insectary` only for signed-in people. */
    build({ signedIn }) {
      const today = todaySerial();
      const open = cached('public', PUBLIC_SHEETS, () => {
        const collection = rowsOf(store, 'Collection_data');
        return {
          collections: collections(collection, today),
          monitoring: monitoring(collection, rowsOf(store, 'SamplingDay_data'), today),
          crispr: crispr(rowsOf(store, 'CRISPR')),
          crosses: crosses(store),
        };
      });
      return {
        generatedAt: new Date().toISOString(),
        today: iso(today),
        ...open,
        insectary: signedIn ? cached('private', PRIVATE_SHEETS, () => insectary(store, today)) : null,
      };
    },
  };
}

/**
 * The sheets the monitoring report reads, for visitors without an account:
 * only the rows and columns its tables and charts use.
 */
export const PUBLIC_TABLES = {
  Collection_data: {
    rows: values => /^(ikiam|casa de lin)$/i.test(text(values.Collection_location)),
    columns: [
      'Release_Collect',
      'FieldMark_ID',
      'Family',
      'Subfamily',
      'Tribe',
      'Genus',
      'SPECIES',
      'Subspecies_Form',
      'Sex',
      'Collection_location',
      'Transect_section',
      'Collection_date',
      'Collection_time',
      'Collector',
      'Cloud_cover',
      'Flight_height',
      'Purpose',
    ],
  },
  SamplingDay_data: { rows: () => true, columns: ['Date', 'Location', 'Purpose', 'Collectors_initials'] },
};

export function publicTable(store, module) {
  const spec = PUBLIC_TABLES[module];
  if (!spec) throw Object.assign(new Error('Not available without an account'), { code: 'AUTH_REQUIRED', status: 401 });
  const full = tablePayload(store, module);
  const keys = full.columns.map(c => c.key);
  const keep = keys.map((k, i) => (spec.columns.includes(k) && keys.indexOf(k) === i ? i : -1)).filter(i => i >= 0);
  return {
    module,
    columns: keep.map(i => ({ ...full.columns[i], readonly: true })),
    headerProblems: [],
    rows: full.rows
      .filter(r => r.observed && spec.rows(Object.fromEntries(keys.map((k, i) => [k, r.v[i]]))))
      .map(r => ({ id: r.id, row: 0, version: r.version, observed: true, v: keep.map(i => r.v[i]), f: [] })),
  };
}
