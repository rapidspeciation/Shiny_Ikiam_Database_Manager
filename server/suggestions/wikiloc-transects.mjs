// Corrections to the monitoring rows from the Wikiloc points stored in the app.
//
// Each capture of a walk on the map (monitoring_tracks) is linked to its
// Collection_data row; a walk still waiting for review (wikiloc_walks) is
// paired here with the shared matcher. The point's coordinates give the
// transect section (frontend/src/lib/transects.ts), which the sheet stopped
// recording in April 2026. Also: a note whose time disagrees with its row or
// with the walk's order, a mark on another collector's row, points without a
// row and rows without a point. Nothing is written.

import * as lib from '../../frontend/src/lib/monitoring.ts';
import { serialToIso } from '../../frontend/src/lib/dates.ts';
import { MAX_SECTION_DISTANCE, trailPosition } from '../../frontend/src/lib/transects.ts';
import { msg, textFields, tpl } from '../messages.mjs';
import { listTracks, listWalks, rematchTracks, tracksRevision } from '../monitoring.mjs';

export const id = 'wikiloc-transects';
export const title = tpl('Transecto y hora desde Wikiloc');
export const describe = tpl(
  'Sección del transecto (1–4) calculada con la posición del punto de Wikiloc de cada captura de monitoreo, y la hora de la nota cuando la fila tiene otra.',
);

const SHEET = 'Collection_data';
const clean = v => (v === null || v === undefined ? '' : String(v).trim());
const minuteOf = v => (typeof v === 'number' ? Math.round((v % 1) * 1440) : null);
const sectionOf = v => {
  const n = Number(clean(v));
  return Number.isInteger(n) && n >= 1 && n <= 4 ? n : null;
};
/** "9:05" for the reasons. */
const hhmm = m => (m === null || m === undefined ? '—' : lib.formatMinutes(m));
const round = n => Math.round(n);
const dateOf = r => (typeof r.values.Collection_date === 'number' ? r.values.Collection_date : null);
const isoSerial = iso => Math.round((Date.parse(`${iso}T00:00:00Z`) - Date.UTC(1899, 11, 30)) / 864e5);
/** 21/09/2026: dates are day first. */
const dmy = serial => serialToIso(serial).split('-').reverse().join('/');
const initials = v => clean(v).split(' - ')[0].trim().toUpperCase();

/*
 * How sure a section is. A point this close to the trail and this far (along
 * the trail) from a section boundary keeps its section under the usual GPS
 * error under the canopy (5–15 m) and the error of the reconstructed
 * boundaries (fitted to the QGIS report map). On the 619 points whose row has
 * a section, the point's section agrees 97 % of the time (Sep 2026).
 */
export const SURE = { distance: 15, margin: 20 };
export const FAIR = { distance: 30, margin: 8 };

/** 'strong', 'fair' or 'weak' geometry for a trail position. */
export function geometry(pos) {
  if (pos.distance <= SURE.distance && pos.margin >= SURE.margin) return 'strong';
  if (pos.distance <= FAIR.distance && pos.margin >= FAIR.margin) return 'fair';
  return 'weak';
}

/** Pairings of a point with its row that are not in doubt. */
const GOOD = new Set(['mark', 'manual', 'sure']);

/**
 * Certainty of a transect suggestion from how the point was paired with its
 * row and where it lies. A blank cell is filled; a value that disagrees is
 * only 'certain' when the point is well inside another section (the walker
 * chose the section on the trail, by landmarks, so a near miss is checked).
 */
export function transectCertainty({ pairing, pos, current }) {
  const geo = geometry(pos);
  const good = GOOD.has(pairing);
  if (current === null) {
    if (good && geo === 'strong') return 'certain';
    if ((good && geo === 'fair') || (!good && geo === 'strong')) return 'likely';
    return 'check';
  }
  if (good && geo === 'strong' && pos.margin >= 2 * SURE.margin) return 'certain';
  if (good && geo === 'strong') return 'likely';
  return 'check';
}

/** How a point was paired with its row, for the reasons (Spanish words the interface translates). */
const PAIRING = {
  mark: () => msg('marca'),
  manual: () => msg('una persona'),
  sure: () => msg('hora, especie y sexo'),
  tie: () => msg('empate'),
  order: () => msg('orden'),
  doubt: () => msg('dudoso'),
};

// ------------------------------------------------------------------ rows

function tableRows(store, sheet) {
  return store.db
    .prepare('SELECT id,row_num,values_json FROM records WHERE sheet=? AND observed=1 AND missing=0 AND row_num<2000000000 ORDER BY row_num')
    .all(sheet)
    .map(r => ({ id: r.id, row: r.row_num, version: 0, observed: true, values: JSON.parse(r.values_json), formulas: [] }));
}

/** Changes whenever the sheet copy, the stored walks or the waiting walks change. */
export function analyseState(store) {
  const rows = store.db
    .prepare("SELECT count(*) n, max(updated_at) u FROM records WHERE sheet IN ('Collection_data','SamplingDay_data')")
    .get();
  const walks = store.db
    .prepare("SELECT count(*) n, sum(status='waiting') w, sum(length(data_json)) s, max(created_at) c FROM wikiloc_walks")
    .get();
  return `${rows.n}:${rows.u}:${tracksRevision(store)}:${walks.n}:${walks.w}:${walks.s}:${walks.c}`;
}

// ------------------------------------------------------------------ timing

/** Indexes of the longest non-decreasing subsequence of `values` (the earliest such run on ties). */
export function longestRun(values) {
  const length = values.map(() => 1);
  const previous = values.map(() => -1);
  values.forEach((v, i) => {
    for (let j = 0; j < i; j++)
      if (values[j] <= v && length[j] + 1 > length[i]) {
        length[i] = length[j] + 1;
        previous[i] = j;
      }
  });
  const out = [];
  for (let end = length.indexOf(Math.max(0, ...length)); end >= 0; end = previous[end]) out.unshift(end);
  return out;
}

/**
 * Each point's time against its walk's order. Wikiloc lists a walk's points in
 * the order they were added; the longest run of points whose noted times go
 * forward is the walk's order, and each point should fall between the times of
 * the nearest points of that run before and after it. A note outside that
 * window by one hour ("10:12" between 9:10 and 9:14) has the wrong hour;
 * outside it otherwise, the point was added at another moment (and maybe
 * somewhere else on the trail). Only walks whose notes mostly follow their
 * order (`ordered`) are judged. `points` are { walk, capture, note } in walk
 * order; the same `walk` object groups them.
 */
export function walkTiming(points) {
  const out = new Map();
  const byWalk = new Map();
  for (const p of points) byWalk.set(p.walk, [...(byWalk.get(p.walk) || []), p]);
  for (const walk of byWalk.values()) {
    const noted = walk.map(p => (p.capture.timeFromTrack ? null : (p.note.minutes ?? null)));
    const timed = noted.flatMap((m, i) => (m === null ? [] : [i]));
    const spine = new Set(longestRun(timed.map(i => noted[i])).map(k => timed[k]));
    const ordered = timed.length >= 4 && spine.size / timed.length >= 0.8;
    walk.forEach((p, i) => {
      const before = timed.filter(j => j < i && spine.has(j)).at(-1);
      const after = timed.find(j => j > i && spine.has(j));
      const window = { before: before === undefined ? null : noted[before], after: after === undefined ? null : noted[after] };
      const fits = m =>
        (window.before === null || m >= window.before - lib.TIME_TOLERANCE) &&
        (window.after === null || m <= window.after + lib.TIME_TOLERANCE);
      const m = noted[i];
      const off = ordered && m !== null && !fits(m);
      // An hour off only between two points (the last point may simply have been added after the walk).
      const slip = off && window.before !== null && window.after !== null ? ([m - 60, m + 60].find(x => x >= 0 && fits(x)) ?? null) : null;
      out.set(p, { noted: m, ...window, ordered, fits, slip, outOfOrder: off && slip === null });
    });
  }
  return out;
}

/** Why a point's place is doubtful: its note's time is out of the walk's order. */
function outOfOrder(time) {
  const vars = { t: hhmm(time.noted), a: hhmm(time.before), b: hhmm(time.after) };
  if (time.before !== null && time.after !== null)
    return msg('. La hora de la nota ({t}) no cabe entre los puntos vecinos ({a}–{b}): el punto pudo añadirse en otro lugar', vars);
  if (time.before !== null)
    return msg('. La hora de la nota ({t}) es anterior a la del punto previo ({a}): el punto pudo añadirse después, en otro lugar', vars);
  return msg('. La hora de la nota ({t}) es posterior a la del punto siguiente ({b}): el punto pudo añadirse en otro lugar', vars);
}

// ------------------------------------------------------------------ analysis

/** One suggested cell edit; `reason` in Spanish and `reasonMsg` for the interface language. */
const edit = (row, field, current, suggested, certainty, reason, evidence) => ({
  sheet: SHEET,
  row: row.row,
  recordId: row.id,
  field,
  current,
  suggested,
  certainty,
  ...textFields('reason', reason),
  evidence,
});

const cache = new WeakMap();
/**
 * Every Wikiloc point with its row, and what disagrees: the transect section
 * from its coordinates, the time of its note, and its collector; points
 * without a row and rows without a point. Cached until the sheet, the stored
 * walks or the waiting walks change.
 */
export function analyse(store) {
  const state = analyseState(store);
  const hit = cache.get(store);
  if (hit?.state === state) return hit.result;
  const result = build(store);
  cache.set(store, { state, result });
  return result;
}

/**
 * Every point of the stored walks and of the walks waiting for review, with
 * its row (or none) and how sure that pairing is; and the walks (with their
 * GPS lines), also those without butterflies.
 */
export function walkPoints(store, { all, monitoring }) {
  const byId = new Map(all.map(r => [r.id, r]));
  const taxa = lib.taxaFrom(all);
  const local = lib.taxaFrom(all.filter(r => /^ikiam$/i.test(clean(r.values.Collection_location))));
  // Points whose pairing is doubtful (tie, order, a note that disagrees with its row), from the shared re-matching.
  const doubtful = new Map();
  for (const d of rematchTracks(store).doubts)
    if (d.source === 'track')
      for (const i of d.indexes) doubtful.set(`${d.trackId}|${i}`, d.confidence === 'tie' || d.confidence === 'order' ? d.confidence : 'doubt');

  const points = [];
  const walks = [];
  const imported = new Set();
  for (const t of listTracks(store)) {
    if (t.wikiloc?.id) imported.add(String(t.wikiloc.id));
    const walk = { id: t.id, date: t.date, collector: t.collector || null, name: t.name, url: t.wikiloc?.url || null, stored: true };
    walks.push({ walk, track: t.track || [] });
    t.captures.forEach((c, i) => {
      const row = c.recordId ? byId.get(c.recordId) || null : null;
      const note = lib.parseCapture(c.text, taxa, local);
      const mark = row && lib.hasMark(row) ? clean(row.values.FieldMark_ID).toUpperCase() : null;
      const pairing = !row
        ? c.link === 'none'
          ? 'none-chosen'
          : c.doubt
            ? 'doubt'
            : 'none'
        : c.link === 'manual'
          ? 'manual'
          : note.markId && note.markId.toUpperCase() === mark
            ? 'mark'
            : doubtful.get(`${t.id}|${i}`) || 'sure';
      points.push({ walk, index: i, capture: c, note, row, pairing });
    });
  }
  for (const w of listWalks(store)) {
    if (w.status !== 'waiting' || imported.has(String(w.wikilocId)) || !w.date || !w.collector) continue;
    const captures = w.waypoints.map(p => lib.locateCapture({ ...p, time: null }, taxa, local));
    const match = lib.matchWalk(monitoring, w.date, w.collector, captures);
    const walk = { id: w.id, date: match.date, collector: w.collector, name: w.name, url: w.url, stored: false };
    walks.push({ walk, track: w.track || [] });
    captures.forEach((c, i) => {
      const m = match.matches[i];
      const row = m.rows.length === 1 ? m.rows[0] : null;
      const pairing = !row ? 'none' : lib.doubtfulMatch(m) ? (m.confidence === 'tie' || m.confidence === 'order' ? m.confidence : 'doubt') : m.confidence;
      const capture = { ...c, text: w.waypoints[i].text, photos: w.waypoints[i].photos || [] };
      points.push({ walk, index: i, capture, note: c, row, pairing });
    });
  }
  return { points, walks };
}

function build(store) {
  const all = tableRows(store, SHEET);
  const days = lib.monitoringDays(tableRows(store, 'SamplingDay_data'));
  const monitoring = all.filter(r => lib.isMonitoringRow(r, days));
  const { points, walks } = walkPoints(store, { all, monitoring });

  const suggestions = [];
  const discrepancies = [];
  const counts = { points: points.length, linked: 0, agree: 0, filled: 0, differ: 0, far: 0 };
  const linkedIds = new Set();
  const evidence = (p, pos) => ({
    walk: p.walk,
    point: p.index,
    text: p.capture.text,
    lat: p.capture.lat,
    lon: p.capture.lon,
    photos: p.capture.photos || [],
    pairing: p.pairing,
    ...(pos ? { distance: round(pos.distance), margin: round(pos.margin), section: pos.section } : {}),
  });
  const rowInfo = r => ({
    row: r.row,
    recordId: r.id,
    date: dateOf(r),
    collector: clean(r.values.Collector) || null,
    species: clean(r.values.SPECIES) || null,
    sex: lib.sexOf(r.values.Sex),
    mark: lib.hasMark(r) ? clean(r.values.FieldMark_ID).toUpperCase() : null,
    minutes: minuteOf(r.values.Collection_time),
    section: clean(r.values.Transect_section) || null,
  });
  const issue = (kind, info, reason, ev) => ({ kind, certainty: 'check', ...info, ...textFields('reason', reason), evidence: ev });

  // Each point's place on the trail; per walk, how its rows' sections compare.
  const tallies = new Map();
  for (const p of points) {
    p.pos = trailPosition(p.capture.lat, p.capture.lon);
    if (!p.row) continue;
    p.current = sectionOf(p.row.values.Transect_section);
    if (p.current === null || p.pos.distance > MAX_SECTION_DISTANCE || geometry(p.pos) === 'weak') continue;
    const tally = tallies.get(p.walk) || { agree: 0, reversed: 0, other: 0 };
    if (p.current === p.pos.section) tally.agree++;
    else if (p.current === 5 - p.pos.section) tally.reversed++;
    else tally.other++;
    tallies.set(p.walk, tally);
  }
  /** A walk whose rows number the sections from the other end (T1 ↔ T4, T2 ↔ T3) on every clear point. */
  const reversedWalk = walk => {
    const tally = tallies.get(walk);
    return !!tally && tally.reversed >= 2 && !tally.agree && !tally.other;
  };
  const timing = walkTiming(points);

  for (const p of points) {
    const { row, pos, current } = p;
    if (!row) continue;
    counts.linked++;
    linkedIds.add(row.id);
    const time = timing.get(p);
    // ---- transect section
    const raw = row.values.Transect_section ?? null;
    if (pos.distance > MAX_SECTION_DISTANCE) {
      counts.far++;
      discrepancies.push(issue('far', rowInfo(row), msg('El punto está a {n} m del sendero: no se calcula el transecto', { n: round(pos.distance) }), evidence(p, pos)));
    } else if (current === pos.section) counts.agree++;
    else {
      counts[current === null ? 'filled' : 'differ']++;
      const reversed = current !== null && reversedWalk(p.walk);
      let certainty = transectCertainty({ pairing: p.pairing, pos, current });
      // The whole walk numbered from the other end: one consistent slip, not a GPS error.
      if (reversed && certainty === 'check' && geometry(pos) !== 'weak') certainty = 'likely';
      // Added out of the walk's order: the point may have been placed later, elsewhere on the trail.
      if (time.outOfOrder) certainty = 'check';
      const notes = [
        ...(reversed ? [msg('. Todas las filas de ese recorrido numeran los transectos al revés')] : []),
        ...(time.outOfOrder ? [outOfOrder(time)] : []),
      ];
      const vars = { s: pos.section, d: round(pos.distance), m: round(pos.margin), how: PAIRING[p.pairing](), notes: notes.length === 2 ? msg('{a}{b}', { a: notes[0], b: notes[1] }) : (notes[0] ?? '') };
      const reason =
        current === null
          ? msg('Punto de Wikiloc en T{s} ({d} m del sendero, {m} m del límite más cercano); emparejado por {how}{notes}', vars)
          : msg('La fila dice T{c}; el punto de Wikiloc está en T{s} ({d} m del sendero, {m} m del límite más cercano); emparejado por {how}{notes}', {
              ...vars,
              c: clean(raw),
            });
      suggestions.push(edit(row, 'Transect_section', raw, pos.section, certainty, reason, evidence(p, pos)));
    }
    // ---- time: the note's minute against the row's (the row was typed from the note) and the walk's order
    const typed = minuteOf(row.values.Collection_time);
    if (time.noted === null || typed === null) continue;
    const near = (a, b) => Math.abs(a - b) <= lib.TIME_TOLERANCE;
    const current_ = row.values.Collection_time ?? null;
    if (time.slip !== null) {
      // "10:12" between the points of 9:10 and 9:14: the note has the wrong hour, and the row copied it.
      if (time.fits(typed)) continue;
      const reason = msg('La nota dice {t}, pero el punto está entre los de {a} y {b} del recorrido: parece {s} (hora equivocada)', {
        t: hhmm(time.noted),
        a: hhmm(time.before),
        b: hhmm(time.after),
        s: hhmm(time.slip),
      });
      suggestions.push(edit(row, 'Collection_time', current_, time.slip / 1440, GOOD.has(p.pairing) ? 'likely' : 'check', reason, evidence(p, null)));
    } else if (!near(time.noted, typed)) {
      const fits = time.fits(time.noted);
      const rowFits = time.fits(typed);
      // The row's time follows the walk and the note's does not: the row was already corrected.
      if (!fits && rowFits) continue;
      const vars = { noted: hhmm(time.noted), typed: hhmm(typed), order: fits && !rowFits ? msg('; la hora de la nota sigue el orden del recorrido') : '' };
      const hour = near(Math.abs(time.noted - typed) % 60, 0) || near(Math.abs(time.noted - typed) % 60, 60);
      const reason = hour
        ? msg('La nota de Wikiloc dice {noted} y la fila {typed} (otra hora){order}', vars)
        : msg('La nota de Wikiloc dice {noted} y la fila {typed}{order}', vars);
      const certainty = (p.pairing === 'mark' || p.pairing === 'manual') && fits && !rowFits ? 'likely' : 'check';
      suggestions.push(edit(row, 'Collection_time', current_, time.noted / 1440, certainty, reason, evidence(p, null)));
    }
  }

  // ---- points without a row: the mark on another collector's row that day, on a nearby day, or nowhere
  const markRows = new Map();
  for (const r of monitoring)
    if (lib.hasMark(r)) {
      const k = clean(r.values.FieldMark_ID).toUpperCase();
      markRows.set(k, [...(markRows.get(k) || []), r]);
    }
  for (const p of points) {
    if (p.row || p.pairing === 'none-chosen') continue;
    const serial = isoSerial(p.walk.date);
    const mark = p.note.markId?.toUpperCase() || null;
    const same = mark ? (markRows.get(mark) || []).filter(r => !linkedIds.has(r.id)) : [];
    const otherCollector = same.find(r => dateOf(r) === serial && initials(r.values.Collector) !== initials(p.walk.collector));
    const otherDay = same.find(
      r => initials(r.values.Collector) === initials(p.walk.collector) && dateOf(r) !== serial && Math.abs((dateOf(r) ?? 0) - serial) <= 3,
    );
    const pos = p.pos;
    if (otherCollector) {
      const reason = msg('El punto {mark} es del recorrido de {walk}, pero la fila con esa marca ese día dice {who}', {
        mark,
        walk: p.walk.collector,
        who: clean(otherCollector.values.Collector),
      });
      suggestions.push(edit(otherCollector, 'Collector', otherCollector.values.Collector ?? null, p.walk.collector, 'check', reason, evidence(p, pos)));
    } else if (otherDay) {
      const reason = msg('El punto {mark} es del recorrido del {walk}, pero la fila con esa marca es del {day}', {
        mark,
        walk: dmy(serial),
        day: dmy(dateOf(otherDay)),
      });
      discrepancies.push(issue('date', rowInfo(otherDay), reason, evidence(p, pos)));
    } else {
      const reason = p.pairing === 'doubt' ? msg('Punto de Wikiloc sin fila: emparejamiento dudoso, ver Dudas') : msg('Punto de Wikiloc sin fila en la hoja');
      // What the note says (there is no row).
      const info = {
        row: null,
        recordId: null,
        date: serial,
        collector: p.walk.collector,
        species: p.note.species || null,
        sex: p.note.sex || null,
        mark,
        minutes: p.note.minutes ?? null,
        section: pos.distance <= MAX_SECTION_DISTANCE ? String(pos.section) : null,
      };
      discrepancies.push(issue('point-without-row', info, reason, evidence(p, pos)));
    }
  }

  // ---- rows without a point: monitoring rows of a collector-day that has a walk
  const walkDays = new Map();
  for (const { walk } of walks) walkDays.set(lib.dayKey(isoSerial(walk.date), walk.collector || ''), walk);
  for (const r of monitoring) {
    if (linkedIds.has(r.id)) continue;
    const d = dateOf(r);
    const walk = d === null ? null : walkDays.get(lib.dayKey(d, clean(r.values.Collector)));
    if (walk) discrepancies.push(issue('row-without-point', rowInfo(r), msg('Fila de monitoreo sin punto en el recorrido de Wikiloc de ese día'), { walk }));
  }

  // ---- collector-days with monitoring rows and no walk in the app
  const missing = new Map();
  for (const r of monitoring) {
    const d = dateOf(r);
    if (d === null) continue;
    const key = lib.dayKey(d, clean(r.values.Collector));
    if (walkDays.has(key)) continue;
    const entry = missing.get(key) || { date: d, collector: clean(r.values.Collector) || null, rows: 0, blankSections: 0 };
    entry.rows++;
    if (sectionOf(r.values.Transect_section) === null) entry.blankSections++;
    missing.set(key, entry);
  }

  const order = { certain: 0, likely: 1, check: 2 };
  suggestions.sort((a, b) => order[a.certainty] - order[b.certainty] || b.row - a.row);
  const tally = (list, key) => list.reduce((m, x) => ({ ...m, [x[key]]: (m[x[key]] || 0) + 1 }), {});
  return {
    suggestions,
    discrepancies,
    daysWithoutWalk: [...missing.values()].sort((a, b) => b.date - a.date),
    counts: {
      ...counts,
      monitoringRows: monitoring.length,
      blankSections: monitoring.filter(r => sectionOf(r.values.Transect_section) === null).length,
      byCertainty: { certain: 0, likely: 0, check: 0, ...tally(suggestions, 'certainty') },
      byField: tally(suggestions, 'field'),
      byKind: tally(discrepancies, 'kind'),
    },
    // For the downloads (not sent with the corrections).
    points,
    walks,
    monitoring,
  };
}

/** The edits for Revisión de datos (Suggested edits). */
export async function suggest(ctx) {
  return analyse(ctx.store).suggestions;
}
