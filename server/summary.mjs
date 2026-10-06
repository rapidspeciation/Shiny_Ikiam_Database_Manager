// The home page ("Inicio"). Anyone can read the natural-history summaries:
// rates and proportions (life-cycle times, when and where butterflies are
// found, weather, causes of death) that never say how many butterflies were
// collected or reared. The team's counts (insectary state, collections,
// monitoring, CRISPR, crosses) need a session.

import { moduleMap } from './schema.mjs';
import { idSuggestions, tableRevision } from './grid.mjs';

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

// ----------------------------------------------------------------- team (signed in)

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

/** How far ahead (and behind) the coming hatchings, pupations and emergences are listed. */
const AHEAD_DAYS = 7;
const LATE_DAYS = 3;

/**
 * What should happen soon in the insectary: clutches whose eggs should hatch,
 * whose larvae should pupate, or whose pupae should emerge, from the date of
 * their current stage plus their species' median time in it (the median of all
 * species when a species has fewer than 5 clutches measured). Clutches more
 * than a few days past the expected date go to `late`: the change was probably
 * not written down.
 */
export function upcoming(stocks, today) {
  const days = stageDays(stocks);
  const all = { egg: [], larva: [], pupa: [] };
  for (const s of days.values()) for (const k of Object.keys(all)) all[k].push(...s[k]);
  const medianFor = (species, stage) => {
    const own = days.get(binomial(species))?.[stage] ?? [];
    return own.length >= 5 ? median(own) : median(all[stage]);
  };
  const NEXT = { egg: 'hatch', larva: 'pupate', pupa: 'emerge' };
  const START = { egg: 'DATE LAID', larva: 'HATCHING DATE', pupa: 'PUPA DATE' };
  const items = [];
  const late = [];
  for (const r of stocks) {
    const laid = date(r['DATE LAID']);
    if (!laid || laid <= today - CLUTCH_DAYS || date(r['EMERGENCE DATE']) || !blank(r['NUMBER OF ADULTS'])) continue;
    const { stage, n } = clutchStage(r);
    const since = date(r[START[stage]]) ?? (stage === 'larva' ? laid + medianFor(r.SPECIES, 'egg') : null);
    const typical = medianFor(r.SPECIES, stage);
    if (since === null || typical === null) continue;
    const expected = Math.round(since + typical);
    const inDays = expected - today;
    const item = {
      clutch: text(r['CLUTCH NUMBER']),
      species: blank(r.SPECIES) ? null : text(r.SPECIES),
      event: NEXT[stage],
      n,
      since: iso(since),
      expected: iso(expected),
      inDays,
      where: blank(r['INSECTARY OR LABORATORY']) ? null : text(r['INSECTARY OR LABORATORY']),
    };
    if (inDays < -LATE_DAYS) late.push(item);
    else if (inDays <= AHEAD_DAYS) items.push(item);
  }
  const order = (a, b) => a.inDays - b.inDays || a.clutch.localeCompare(b.clutch, 'en', { numeric: true });
  return { aheadDays: AHEAD_DAYS, lateDays: LATE_DAYS, items: items.sort(order), late: late.sort(order).reverse() };
}

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

// ----------------------------------------------------------------- latest IDs (signed in)

const CAM = /^CAM(\d+)$/i;

/**
 * The CAM ID used last in some columns of a sheet: the latest date, then the
 * highest number. (By number alone, an old CAM from another booklet can win:
 * insectary rows also hold field CAMs.)
 */
function lastCam(rows, columns, dateOf) {
  let best = null;
  for (const r of rows)
    for (const key of columns) {
      const m = CAM.exec(text(r[key]));
      const d = dateOf(r);
      if (!m || d === null) continue;
      if (!best || d > best.date || (d === best.date && Number(m[1]) > best.number))
        best = { id: text(r[key]).toUpperCase(), number: Number(m[1]), width: m[1].length, date: d };
    }
  return best;
}
function nextCam(last, used) {
  if (!last) return null;
  for (let n = last.number + 1; ; n++) {
    const id = `CAM${String(n).padStart(last.width, '0')}`;
    if (!used.has(id)) return id;
  }
}

/** The last monitoring mark given in the series now in use, its date, and the next one (B68 → B69; after 99 the next letter). */
export function marks(rows) {
  const given = rows
    .map((r, i) => ({ r, i, m: /^([A-Z]+)(\d+)$/.exec(text(r.FieldMark_ID).toUpperCase()), date: date(r.Collection_date) }))
    .filter(x => x.m)
    .sort((a, b) => (a.date ?? 0) - (b.date ?? 0) || a.i - b.i);
  const latest = given.at(-1);
  if (!latest) return null;
  const series = latest.m[1];
  const max = Math.max(...given.filter(x => x.m[1] === series).map(x => Number(x.m[2])));
  // A recapture of an older mark can be the latest row: the last mark given is the highest in the series.
  const first = given.find(x => x.m[1] === series && Number(x.m[2]) === max);
  const next = max < 99 ? `${series}${max + 1}` : `${series === 'M' ? 'A' : String.fromCharCode(series.charCodeAt(0) + 1)}1`;
  return { last: `${series}${max}`, date: iso(first.date), next };
}

/**
 * The IDs the team labels with: the last CAM ID used in the
 * insectary and in field collections, the last monitoring mark, with their
 * dates, and the next free ones (plus the next Insectary ID and clutch number).
 */
function latestIds(store) {
  const insect = rowsOf(store, 'Insectary_data');
  const field = rowsOf(store, 'Collection_data');
  const used = new Set();
  for (const r of [...insect, ...field])
    for (const key of ['CAM_ID', 'CAM_ID_CollData', 'CAM_ID_insectary']) if (CAM.test(text(r[key]))) used.add(text(r[key]).toUpperCase());
  const insectaryCam = lastCam(insect, ['CAM_ID'], r => date(r.Preservation_date) ?? date(r.Death_date));
  const fieldCam = lastCam(field, ['CAM_ID'], r => date(r.Preservation_date) ?? date(r.Collection_date));
  const clutches = rowsOf(store, 'Insectary_stocks')
    .map(r => parseInt(text(r['CLUTCH NUMBER']), 10))
    .filter(Number.isFinite);
  let insectaryId = null;
  try {
    // Only a row after the last one used is "the next": an earlier empty row may already be on a butterfly's wings.
    const ids = idSuggestions(store, { kind: 'insectary', count: 1 });
    insectaryId = ids.tail ? (ids.sequence[0] ?? null) : null;
  } catch {
    /* No pre-filled Insectary IDs left. */
  }
  const cam = last => last && { last: last.id, date: iso(last.date), next: nextCam(last, used) };
  return {
    insectaryCam: cam(insectaryCam),
    collectionCam: cam(fieldCam),
    mark: marks(field.filter(r => /^mark_released$/i.test(text(r.Release_Collect)))),
    insectaryId,
    clutch: clutches.length ? String(Math.max(...clutches) + 1) : null,
  };
}

// ----------------------------------------------------------------- natural history (open)

function median(values) {
  const v = values.filter(Number.isFinite).sort((a, b) => a - b);
  if (!v.length) return null;
  const m = v.length >> 1;
  return v.length % 2 ? v[m] : (v[m - 1] + v[m]) / 2;
}
const quantile = (sorted, q) => sorted[Math.min(sorted.length - 1, Math.max(0, Math.round(q * (sorted.length - 1))))];
const round = (n, digits = 1) => (n === null || !Number.isFinite(n) ? null : Math.round(n * 10 ** digits) / 10 ** digits);
function mode(values) {
  const counts = new Map();
  for (const v of values) counts.set(v, (counts.get(v) || 0) + 1);
  return [...counts].sort((a, b) => b[1] - a[1])[0]?.[0] ?? null;
}

const CLOUDS = {
  'S_(cloudless_sunny)': 'Soleado, sin nubes',
  'S&C_(sun_&_cloud_patches)': 'Sol y nubes',
  'CL_(cloudy_light)': 'Nublado claro',
  'CD_(cloudy_dark)': 'Nublado oscuro',
};
const RAIN = { 'DY_(dry)': 'Sin lluvia', 'DZ_(drizzle)': 'Llovizna', 'WR_(weak_rain)': 'Lluvia débil' };
const DEATHS = {
  Unknown: 'Causa desconocida',
  Disappearance: 'Desaparecieron',
  Other: 'Otra causa',
  Eaten: 'Depredadas',
  Spider: 'Arañas',
  'Unknown - Only wings': 'Solo se hallaron las alas',
  Deformed: 'Deformidad',
  Ants: 'Hormigas',
  'Heat stroke': 'Golpe de calor',
};

/** Median days as egg, larva and pupa per species (subspecies together), from the clutch dates; hybrids are left out. */
const STAGES = [
  ['egg', 'DATE LAID', 'HATCHING DATE', 1, 30],
  ['larva', 'HATCHING DATE', 'PUPA DATE', 3, 60],
  ['pupa', 'PUPA DATE', 'EMERGENCE DATE', 3, 40],
];
/** Subspecies share their species' development times: "Mechanitis polymnia proceriformis" → "Mechanitis polymnia". */
const binomial = name => text(name).split(/\s+/).slice(0, 2).join(' ');

/** Days spent in each stage by every clutch with both dates, per species (hybrids left out). */
function stageDays(stocks) {
  const bySpecies = new Map();
  for (const r of stocks) {
    const full = text(r.SPECIES);
    if (blank(full) || /\sx\s|\bVS\b/i.test(full)) continue;
    const name = binomial(full);
    const s = bySpecies.get(name) || { name, egg: [], larva: [], pupa: [] };
    for (const [stage, from, to, min, max] of STAGES) {
      const a = date(r[from]),
        b = date(r[to]);
      if (a && b && b - a >= min && b - a <= max) s[stage].push(b - a);
    }
    bySpecies.set(name, s);
  }
  return bySpecies;
}

function lifeCycle(stocks) {
  return [...stageDays(stocks).values()]
    .filter(s => s.egg.length >= 5 && s.larva.length >= 5 && s.pupa.length >= 5)
    .map(s => ({ name: s.name, egg: median(s.egg), larva: median(s.larva), pupa: median(s.pupa) }))
    .map(s => ({ ...s, total: s.egg + s.larva + s.pupa }))
    .sort((a, b) => a.total - b.total);
}

/**
 * Collecting sessions (one person, one place, one day) with the hours searched:
 * the start and end written in SamplingDay_data, else the first and last
 * capture of the day. Sessions shorter than half an hour are left out.
 */
export function sessions(collection, days) {
  const planned = new Map();
  for (const r of days) {
    const d = date(r.Date);
    if (d && typeof r.Start_time === 'number' && typeof r.End_time === 'number' && r.End_time > r.Start_time)
      planned.set(`${d}|${text(r.Collectors_initials).toUpperCase()}`, [r.Start_time * 24, r.End_time * 24]);
  }
  const groups = new Map();
  for (const r of collection) {
    const d = date(r.Collection_date);
    const t = num(r.Collection_time);
    if (!d || t === null || t <= 0 || t >= 1 || blank(r.Collection_location)) continue;
    const key = `${d}|${initials(r.Collector)}|${text(r.Collection_location)}`;
    const g = groups.get(key) || { day: `${d}|${initials(r.Collector)}`, hours: [], clouds: [], rain: [] };
    g.hours.push(t * 24);
    if (CLOUDS[text(r.Cloud_cover)]) g.clouds.push(text(r.Cloud_cover));
    if (RAIN[text(r.Rainfall)]) g.rain.push(text(r.Rainfall));
    groups.set(key, g);
  }
  const out = [];
  for (const g of groups.values()) {
    const first = Math.min(...g.hours),
      last = Math.max(...g.hours);
    const plan = planned.get(g.day);
    const start = plan ? Math.min(plan[0], first) : first;
    const end = plan ? Math.max(plan[1], last) : last;
    if (end - start < 0.5 || (!plan && g.hours.length < 3)) continue;
    out.push({ start, end, hours: g.hours, cloud: mode(g.clouds), rain: mode(g.rain) });
  }
  return out;
}

/** Captures per hour of searching, for each hour of the day (the effort corrects for when people were out). */
export function activity(list, minEffort = 5) {
  const out = [];
  for (let h = 6; h < 18; h++) {
    let effort = 0,
      captures = 0;
    for (const s of list) {
      effort += Math.max(0, Math.min(s.end, h + 1) - Math.max(s.start, h));
      captures += s.hours.filter(t => t >= h && t < h + 1).length;
    }
    if (effort > 0 && effort >= minEffort)
      out.push({ hour: h, perHour: round(captures / effort), effortHours: round(effort, 0), share: captures });
  }
  const total = out.reduce((n, h) => n + h.share, 0) || 1;
  return out.map(h => ({ ...h, share: round((100 * h.share) / total) }));
}

/** Captures per hour of searching under each sky and rain condition (the day's most common record). */
function weather(list) {
  const rate = (key, labels) =>
    Object.entries(labels)
      .map(([code, label]) => {
        const chosen = list.filter(s => s[key] === code);
        const hours = chosen.reduce((n, s) => n + (s.end - s.start), 0);
        const captures = chosen.reduce((n, s) => n + s.hours.length, 0);
        return { code, label, sessions: chosen.length, perHour: hours ? round(captures / hours) : null };
      })
      .filter(r => r.sessions >= 5);
  return { clouds: rate('cloud', CLOUDS), rain: rate('rain', RAIN) };
}

/** Monitoring at Ikiam: butterflies per monitoring day and species seen, by month of the year (all years). */
function seasons(collection, days) {
  const dayKeys = monitoringDayKeys(days);
  const records = collection.filter(r => isMonitoring(r, dayKeys));
  const effort = new Set(dayKeys);
  for (const r of records) if (date(r.Collection_date)) effort.add(`${date(r.Collection_date)}|${initials(r.Collector)}`);
  const perMonth = Array.from({ length: 12 }, () => ({ days: 0, captures: 0, species: new Set() }));
  for (const key of effort) perMonth[Number(month(Number(key.split('|')[0])).slice(5)) - 1].days++;
  for (const r of records) {
    const m = perMonth[Number(month(date(r.Collection_date) ?? 0).slice(5)) - 1];
    m.captures++;
    if (!blank(r.SPECIES)) m.species.add(text(r.SPECIES));
  }
  return perMonth.map((m, i) => ({
    month: i + 1,
    perDay: m.days >= 3 ? round(m.captures / m.days) : null,
    species: m.days >= 3 ? m.species.size : null,
  }));
}

/**
 * The most often recorded Ithomiini: where they were found most often per day
 * of collecting at each place (places visited at least 3 days), the elevations
 * they were found at (10th to 90th percentile) and the share of females.
 */
function whereToFind(collection) {
  const records = collection.filter(r => date(r.Collection_date) && !blank(r.Collection_location));
  const siteDays = new Map();
  for (const r of records) {
    const site = text(r.Collection_location);
    const set = siteDays.get(site) || new Set();
    set.add(date(r.Collection_date));
    siteDays.set(site, set);
  }
  const ithomiini = records.filter(r => /^ithomiini$/i.test(text(r.Tribe)) && !blank(r.SPECIES));
  const common = tally(ithomiini, r => text(r.SPECIES), 12).map(s => s.name);
  return common.map(name => {
    const own = ithomiini.filter(r => text(r.SPECIES) === name);
    const bySite = tally(own, r => text(r.Collection_location))
      .map(s => ({ name: s.name, days: siteDays.get(s.name)?.size ?? 0, n: s.n }))
      .filter(s => s.days >= 3)
      .map(s => ({ name: s.name, perDay: round(s.n / s.days, 2) }))
      .sort((a, b) => b.perDay - a.perDay)
      .slice(0, 3);
    const elevations = own
      .map(r => num(r.ELEVATION))
      .filter(e => e !== null && e > 0 && e < 5000)
      .sort((a, b) => a - b);
    const sexed = own.filter(r => /^(female|male)$/i.test(text(r.Sex)));
    return {
      name,
      sites: bySite,
      elevation: elevations.length >= 5 ? [quantile(elevations, 0.1), quantile(elevations, 0.9)] : null,
      female: sexed.length >= 10 ? Math.round((100 * sexed.filter(r => /^female$/i.test(text(r.Sex))).length) / sexed.length) : null,
    };
  });
}

/** Places with the most species recorded, with the days spent there (richness grows with effort). */
function richestSites(collection) {
  const sites = new Map();
  for (const r of collection) {
    if (blank(r.Collection_location) || !date(r.Collection_date)) continue;
    const name = text(r.Collection_location);
    const s = sites.get(name) || { name, species: new Set(), ithomiini: new Set(), days: new Set(), elevation: [] };
    if (!blank(r.SPECIES) && !/NOT_FOUND/i.test(text(r.SPECIES))) {
      s.species.add(text(r.SPECIES));
      if (/^ithomiini$/i.test(text(r.Tribe))) s.ithomiini.add(text(r.SPECIES));
    }
    s.days.add(date(r.Collection_date));
    if (num(r.ELEVATION) > 0) s.elevation.push(num(r.ELEVATION));
    sites.set(name, s);
  }
  return [...sites.values()]
    .map(s => ({
      name: s.name,
      species: s.species.size,
      ithomiini: s.ithomiini.size,
      days: s.days.size,
      elevation: median(s.elevation),
    }))
    .sort((a, b) => b.ithomiini - a.ithomiini || b.species - a.species)
    .slice(0, 10);
}

/** How butterflies die in the insectary, as percentages (butterflies sacrificed for samples are left out). */
function deathCauses(individuals) {
  const deaths = individuals.filter(r => DEATHS[text(r.Death_cause)] && date(r.Death_date));
  return tally(deaths, r => DEATHS[text(r.Death_cause)]).map(c => ({
    name: c.name,
    percent: round((100 * c.n) / deaths.length),
  }));
}

function naturalHistory(store) {
  const collection = rowsOf(store, 'Collection_data');
  const days = rowsOf(store, 'SamplingDay_data');
  const list = sessions(collection, days);
  const named = collection.filter(r => !blank(r.SPECIES) && !/NOT_FOUND/i.test(text(r.SPECIES)));
  const years = collection.map(r => date(r.Collection_date)).filter(Boolean);
  const elevations = collection
    .map(r => num(r.ELEVATION))
    .filter(e => e !== null && e > 0 && e < 5000)
    .sort((a, b) => a - b);
  return {
    facts: {
      ithomiini: new Set(named.filter(r => /^ithomiini$/i.test(text(r.Tribe))).map(r => text(r.SPECIES))).size,
      species: new Set(named.map(r => text(r.SPECIES))).size,
      places: new Set(collection.filter(r => !blank(r.Collection_location)).map(r => text(r.Collection_location))).size,
      since: years.length ? Number(month(Math.min(...years)).slice(0, 4)) : null,
      elevation: elevations.length ? [quantile(elevations, 0.01), quantile(elevations, 0.99)] : null,
    },
    lifeCycle: lifeCycle(rowsOf(store, 'Insectary_stocks')),
    activity: activity(list),
    sessions: list.length,
    weather: weather(list),
    seasons: seasons(collection, days),
    species: whereToFind(collection),
    sites: richestSites(collection),
    deaths: deathCauses(rowsOf(store, 'Insectary_data')),
  };
}

// ----------------------------------------------------------------- entry points

const NATURE_SHEETS = ['Collection_data', 'SamplingDay_data', 'Insectary_stocks', 'Insectary_data'];
const TEAM_SHEETS = [...NATURE_SHEETS, 'CRISPR', 'Melinaea_crosses', 'F1/F2_MutationRate', 'Stocks_Matings', 'Crosses_Lys_x_Pol'];

const caches = new WeakMap();
function cached(store, key, sheets, build) {
  const cache = caches.get(store) ?? caches.set(store, new Map()).get(store);
  const revision = `${todaySerial()}:${sheets.map(s => tableRevision(store, s)).join(',')}`;
  const hit = cache.get(key);
  if (hit?.revision === revision) return hit.value;
  const value = build();
  cache.set(key, { revision, value });
  return value;
}

/**
 * Natural history for everyone; `team` only for signed-in people. Each part is computed again
 * only when a sheet it reads (or the day) changed, here in this thread: the app's requests ask
 * freshSummary (in the Revisión worker thread, server/checks-host.mjs).
 */
export function summaryHere(store, { signedIn }) {
  const today = todaySerial();
  const nature = cached(store, 'nature', NATURE_SHEETS, () => naturalHistory(store));
  const team = signedIn
    ? cached(store, 'team', TEAM_SHEETS, () => {
        const collection = rowsOf(store, 'Collection_data');
        return {
          latestIds: latestIds(store),
          insectary: insectary(store, today),
          upcoming: upcoming(rowsOf(store, 'Insectary_stocks'), today),
          monitoring: monitoring(collection, rowsOf(store, 'SamplingDay_data'), today),
          collections: collections(collection, today),
          crispr: crispr(rowsOf(store, 'CRISPR')),
          crosses: crosses(store),
        };
      })
    : null;
  return { generatedAt: new Date().toISOString(), today: iso(today), nature, team };
}

/** The state of the sheets the summaries read, and the day. */
export const summaryStamp = store => `${todaySerial()}:${TEAM_SHEETS.map(s => tableRevision(store, s)).join(',')}`;
/** Both parts with the state they were computed from: { stamp, today, nature, team } (the worker's answer). */
export function summaryEntry(store) {
  const stamp = summaryStamp(store);
  const { today, nature, team } = summaryHere(store, { signedIn: true });
  return { stamp, today, nature, team };
}
const kept = new WeakMap();
/** A summary computed in the worker: the cached answer while the sheets are as it was computed from. */
export function keepSummary(store, entry) {
  if (entry.stamp === summaryStamp(store)) kept.set(store, entry);
  return entry;
}
/** The kept summary when it is still up to date, else null. */
export function cachedSummary(store) {
  const hit = kept.get(store);
  return hit?.stamp === summaryStamp(store) ? hit : null;
}
/** Who computes the summaries for the app's requests (server/checks-host.mjs: a worker thread), by store. */
const runners = new WeakMap();
export function useSummaryRunner(store, runner) {
  if (runner) runners.set(store, runner);
  else runners.delete(store);
}
/** The home page's summaries as of now (summaryHere), computed in the worker where there is one. A promise. */
export async function freshSummary(store, { signedIn }) {
  const runner = runners.get(store);
  if (!runner) return summaryHere(store, { signedIn });
  const { today, nature, team } = await runner.fresh();
  return { generatedAt: new Date().toISOString(), today, nature, team: signedIn ? team : null };
}
