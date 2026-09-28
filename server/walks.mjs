// A Wikiloc monitoring walk as preliminary Collection_data rows, for the
// assistant. The server cannot open Wikiloc (Cloudflare), so a link is queued
// for the worker on a home computer (server/monitoring.mjs, tools/wikiloc);
// once the walk arrives, its points are read and checked with the same code as
// Monitoreo → Importar recorrido (frontend/src/lib/monitoring.ts, loaded here
// with Node's type stripping), and the rows not yet in the sheet are returned
// ready for propose_changes. Nothing is written here.

import { moduleMap, parseDateText } from './schema.mjs';
import { listOptions } from './verify.mjs';
import { queueLink, trailUrl } from './monitoring.mjs';

const MODULE = 'Collection_data';
const fail = (code, message, status = 400) => Object.assign(new Error(message), { code, status });
const text = value => (value === null || value === undefined ? '' : String(value).trim());

let lib = null;
/** The monitoring logic of the app, shared with the browser. */
export function monitoringLib() {
  lib ??= import('../frontend/src/lib/monitoring.ts');
  return lib;
}

const workerOnline = store => {
  const seen = store.getSetting('wikilocWorkerSeen');
  return !!seen && Date.now() - Date.parse(seen) < 2 * 60_000;
};

/** Rows of a sheet shaped as the grids receive them (TableRow). */
function tableRows(store, sheet) {
  const mod = moduleMap.get(sheet);
  return store.db
    .prepare(
      'SELECT id,row_num,version,observed,values_json,formulas_json FROM records WHERE sheet=? AND missing=0 AND row_num>? AND row_num<2000000000 ORDER BY row_num',
    )
    .all(sheet, mod.headerRow)
    .map(r => ({
      id: r.id,
      row: r.row_num,
      version: r.version,
      observed: !!r.observed,
      values: JSON.parse(r.values_json),
      formulas: Object.keys(JSON.parse(r.formulas_json)),
    }));
}

/**
 * Species of Taxonomy_v18Jun25 (the SPECIES list of Collection_data) with their
 * subspecies and tribe. The sheet may hold binomials or only epithets.
 */
function taxonomy(store) {
  const out = new Map();
  for (const row of tableRows(store, 'Taxonomy_v18Jun25')) {
    const v = row.values;
    let species = text(v.species);
    if (species && !species.includes(' ') && text(v.genus)) species = `${text(v.genus)} ${species}`;
    if (!/^[A-Z][a-z]+ [a-z-]+$/.test(species)) continue;
    const entry = out.get(species) || { subspecies: new Set(), tribe: text(v.tribe) || null };
    const sub = text(v.subspecies);
    const word = sub.startsWith(`${species} `) ? sub.slice(species.length + 1).trim() : sub;
    if (word && !/\s/.test(word) && !/^(NA|N\/A)$/i.test(word)) entry.subspecies.add(word);
    out.set(species, entry);
  }
  return out;
}

/** Collection_data columns that are formulas in the next unused (pre-made) row: never typed. */
function createFormulas(rows) {
  let last = -1;
  rows.forEach((r, i) => {
    if (r.observed) last = i;
  });
  return new Set(rows.slice(last + 1).find(r => !r.observed)?.formulas || []);
}

/**
 * Queues a Wikiloc trail link for the worker. A walk already read is not read
 * again unless `refresh` (e.g. the notes were corrected in Wikiloc).
 */
export function queueWalk(store, { url, refresh = false }, user) {
  const found = trailUrl(url);
  if (!found) throw fail('INVALID_WALK', 'No Wikiloc trail link found; it looks like https://es.wikiloc.com/rutas-senderismo/…-12345678');
  const walk = store.db.prepare('SELECT id,status FROM wikiloc_walks WHERE wikiloc_id=?').get(found.wikilocId);
  if (walk && !refresh)
    return { status: 'ready', walkId: walk.id, url: found.url, next: 'Call get_walk with this walkId.' };
  const { job } = queueLink(store, { url: found.url }, user);
  const online = workerOnline(store);
  return {
    status: job.status,
    jobId: job.id,
    url: found.url,
    workerOnline: online,
    next: online
      ? 'The home computer reads it in a minute or two; then call get_walk with the url.'
      : 'The home computer that reads Wikiloc is offline: the link waits in the queue (Monitoreo → Importar shows it). Tell the person.',
  };
}

/** Where a walk not read yet stands: its job in the queue. */
function pending(store, found) {
  const job = store.db
    .prepare("SELECT status,message,updated_at FROM wikiloc_jobs WHERE kind='trail' AND target=? ORDER BY created_at DESC LIMIT 1")
    .get(found.url);
  return {
    status: job?.status ?? 'not_queued',
    message: job?.message ?? null,
    workerOnline: workerOnline(store),
    next: !job
      ? 'Queue it first with queue_wikiloc.'
      : job.status === 'failed'
        ? 'Reading the page failed; tell the person the message.'
        : 'Not read yet; try again in about a minute.',
  };
}

/**
 * The walk's points with the app's review, and the Collection_data rows to
 * propose for the captures that are not in the sheet yet. `date` and
 * `collector` override what the walk says (a title without a day, a profile
 * without its collector).
 */
export async function walkDraft(store, { walkId, url, date, collector } = {}) {
  let row = walkId ? store.db.prepare('SELECT * FROM wikiloc_walks WHERE id=?').get(String(walkId)) : null;
  if (!row && url) {
    const found = trailUrl(url);
    if (!found) throw fail('INVALID_WALK', 'No Wikiloc trail link found');
    row = store.db.prepare('SELECT * FROM wikiloc_walks WHERE wikiloc_id=?').get(found.wikilocId);
    if (!row) return pending(store, found);
  }
  if (!row) throw fail('WALK_NOT_FOUND', 'Give the walkId or url of a Wikiloc walk', 404);
  if (date && !/^\d{4}-\d{2}-\d{2}$/.test(String(date))) throw fail('INVALID_DATE', 'date must be YYYY-MM-DD');
  const m = await monitoringLib();
  const data = JSON.parse(row.data_json);

  const all = tableRows(store, MODULE);
  const observed = all.filter(r => r.observed);
  const days = m.monitoringDays(tableRows(store, 'SamplingDay_data'));
  const monitoring = all.filter(r => m.isMonitoringRow(r, days));
  const tax = taxonomy(store);
  // Names as the Monitoreo review knows them (the sheet's own, most used first), then Taxonomy's.
  const taxa = m.taxaFrom(all);
  for (const [species, entry] of tax) {
    const known = taxa.get(species) || [];
    taxa.set(species, [...known, ...[...entry.subspecies].filter(s => !known.includes(s))]);
  }
  const tribes = m.tribesFrom(all);
  const isIthomiini = species => !!species && (tribes.get(species) ?? tax.get(species)?.tribe) === 'Ithomiini';
  const collectors = [...new Set(observed.map(r => text(r.values.Collector)).filter(c => / - /.test(c)))];

  const problems = [];
  let who = text(collector) || data.collector || null;
  const source = text(collector) ? 'given' : data.collector ? 'profile' : null;
  if (!who) who = m.collectorFromName(row.name, collectors);
  if (who && !collectors.includes(who)) problems.push(`El colector «${who}» no aparece en Collection_data: revisa cómo se escribe`);
  let day = text(date) || row.date || null;
  if (!day) problems.push(`El título «${row.name}» no dice el día (${data.recorded || 'sin fecha'}): pregunta la fecha y llama get_walk con date`);
  if (!who) problems.push('No se sabe quién hizo el recorrido: pregunta el colector (p. ej. «FCH - Franz Chandi») y llama get_walk con collector');

  const captures = data.waypoints
    .map(p => m.locateCapture({ ...p, time: null }, taxa))
    .sort((a, b) => (a.seq ?? 1e9) - (b.seq ?? 1e9) || (a.minutes ?? 0) - (b.minutes ?? 0));
  // Points already entered: paired with the collector's rows of that day, as "Pasar al mapa" does.
  let paired = null;
  if (day && who) {
    const match = m.matchWalk(monitoring, day, who, captures);
    if (match.date !== day) {
      problems.push(`Las marcas coinciden con las filas del ${match.date}, no del ${day}: se usa ${match.date}`);
      day = match.date;
    }
    paired = new Map(match.pairs.map(p => [p.capture, p.row]));
  }

  const lists = listOptions(store, MODULE);
  const formulas = createFormulas(all);
  const marks = m.markIndex(monitoring);
  const preserved = m.preservedForRule(all, day ? parseDateText(day) : undefined);
  const points = [];
  const newRows = [];
  captures.forEach((c, i) => {
    const review = m.reviewCapture(c, i, {
      rows: monitoring,
      date: day || '',
      captures,
      marks,
      preserved,
      isIthomiini,
      ...(paired ? { existing: paired.get(c) ?? null } : {}),
    });
    const checks = review.list.map(k => `${k.kind}: ${k.text}`);
    if (c.species && lists.SPECIES && !lists.SPECIES.values.has(c.species))
      checks.push(`warn: ${c.species} no está en Taxonomy_v18Jun25 (la hoja lo rechazaría)`);
    const point = {
      index: i,
      text: c.text,
      species: c.species,
      subspecies: c.subspecies,
      sex: c.sex,
      time: m.formatMinutes(c.minutes) || null,
      height: c.height,
      cloud: c.cloud,
      rain: c.rain,
      markId: c.markId,
      recapture: review.recapture ? { row: review.recapture.row, recordId: review.recapture.id } : null,
      section: c.section,
      photos: c.photos.length,
      inSheet: review.existing ? { row: review.existing.row, recordId: review.existing.id } : null,
      checks,
    };
    points.push(point);
    if (review.existing || !day || !who) return;
    const values = m.captureValues(c, { date: day, collector: who, section: c.section });
    for (const field of formulas) delete values[field];
    // Readable for the model and the person; propose_changes turns them back into sheet values.
    values.Collection_date = day;
    if (typeof values.Collection_time === 'number') values.Collection_time = m.formatMinutes(c.minutes);
    for (const key of Object.keys(values)) if (values[key] === null) delete values[key];
    newRows.push({
      point: i,
      sheet: MODULE,
      values,
      note: `Wikiloc: ${c.text}`.slice(0, 300),
    });
  });
  const blocking = points.filter(p => !p.inSheet && p.checks.some(k => k.startsWith('warn:'))).length;
  return {
    status: 'ready',
    walk: {
      id: row.id,
      wikilocId: row.wikiloc_id,
      url: row.url,
      name: row.name,
      date: day,
      titleDate: row.date,
      collector: who,
      collectorFrom: source ?? (who ? 'title' : null),
      reviewStatus: row.status,
      points: points.length,
      inSheet: points.filter(p => p.inSheet).length,
    },
    problems,
    points,
    newRows,
    next: newRows.length
      ? `Propose newRows with propose_changes (one proposal for the walk). ${blocking} of them have warnings: fix what the note makes clear, otherwise say so in that row's note and tell the person.`
      : 'Nothing to propose.',
  };
}
