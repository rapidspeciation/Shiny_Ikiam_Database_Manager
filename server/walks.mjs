// A Wikiloc monitoring walk as preliminary Collection_data rows, for the
// assistant. A link is queued for the server's Wikiloc importer (Camoufox,
// tools/wikiloc, jobs in server/monitoring.mjs); once the walk arrives, its
// points are read and checked with the same code as Monitoreo → Importar
// recorrido (frontend/src/lib/monitoring.ts, loaded here with Node's type
// stripping), and the rows not yet in the sheet are returned ready for
// propose_changes. Nothing is written here.

import { moduleMap, parseDateText } from './schema.mjs';
import { msg, tpl } from './messages.mjs';
import { listOptions, listProblemMsg } from './verify.mjs';
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

/**
 * Collection_data columns that are formulas in the next unused (pre-made) row: never typed.
 * When those rows have run out the save makes more as copies of the last row with formulas.
 */
function createFormulas(rows) {
  let last = -1;
  rows.forEach((r, i) => {
    if (r.observed) last = i;
  });
  const next =
    rows.slice(last + 1).find(r => !r.observed) ?? (last >= 0 && rows[last].formulas.length ? rows[last] : null);
  return new Set(next?.formulas || []);
}

/**
 * Queues a Wikiloc trail link for the worker. A walk already read is not read
 * again unless `refresh` (e.g. the notes were corrected in Wikiloc).
 */
export function queueWalk(store, { url, refresh = false }, user) {
  const found = trailUrl(url);
  if (!found)
    throw fail('INVALID_WALK', 'No Wikiloc trail link found; it looks like https://es.wikiloc.com/rutas-senderismo/…-12345678 or https://loc.wiki/t/12345678');
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
      ? 'The Wikiloc importer reads it, usually within a minute or two; then call get_walk with the url.'
      : 'The Wikiloc importer is not responding: the link waits in the queue (Monitoreo → Importar shows it). Tell the person.',
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
      ? 'Queue it first with queue_walk.'
      : job.status === 'failed'
        ? 'Reading the page failed; tell the person the message.'
        : 'Not read yet; try again in about a minute.',
  };
}

/** The walk row by its id or Wikiloc link; { pending } while the link is not read yet. */
function walkRecord(store, { walkId, url }) {
  let row = walkId ? store.db.prepare('SELECT * FROM wikiloc_walks WHERE id=?').get(String(walkId)) : null;
  if (!row && url) {
    const found = trailUrl(url);
    if (!found) throw fail('INVALID_WALK', 'No Wikiloc trail link found');
    row = store.db.prepare('SELECT * FROM wikiloc_walks WHERE wikiloc_id=?').get(found.wikilocId);
    if (!row) return { pending: pending(store, found) };
  }
  if (!row) throw fail('WALK_NOT_FOUND', 'Give the walkId or url of a Wikiloc walk', 404);
  return { row };
}

/**
 * A walk read with the sheet: its captures in walk order, each with the
 * Monitoreo review (already in the sheet, new mark or recapture, the
 * 30-preserved rule, missing parts), the day and collector it belongs to, and
 * the sheet's lists and formula columns. Shared by get_walk and the walk's
 * proposal (Monitoreo → Importar).
 */
async function readWalk(store, row, { date, collector } = {}) {
  if (date && !/^\d{4}-\d{2}-\d{2}$/.test(String(date))) throw fail('INVALID_DATE', 'date must be YYYY-MM-DD');
  const m = await monitoringLib();
  const data = JSON.parse(row.data_json);

  const all = tableRows(store, MODULE);
  const observed = all.filter(r => r.observed);
  const dayRows = tableRows(store, 'SamplingDay_data');
  const days = m.monitoringDays(dayRows);
  const monitoring = all.filter(r => m.isMonitoringRow(r, days));
  const tax = taxonomy(store);
  // Names as the Monitoreo review knows them (the sheet's own, most used first), then Taxonomy's.
  const taxa = m.taxaFrom(all);
  for (const [species, entry] of tax) {
    const known = taxa.get(species) || [];
    taxa.set(species, [...known, ...[...entry.subspecies].filter(s => !known.includes(s))]);
  }
  // Abbreviations and a missing subspecies are resolved against the names seen at Ikiam.
  const local = m.taxaFrom(all.filter(r => /^ikiam$/i.test(text(r.values.Collection_location))));
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

  // A note without a time takes the time its point was saved in Wikiloc.
  const captures = data.waypoints
    .map(p => m.locateCapture({ ...p, time: p.time ?? null }, taxa, local))
    .sort((a, b) => (a.seq ?? 1e9) - (b.seq ?? 1e9) || (a.minutes ?? 0) - (b.minutes ?? 0));
  // Points already entered: paired with the collector's rows of that day, as "Pasar al mapa" does.
  let paired = null;
  let pairing = null;
  if (day && who) {
    const match = m.matchWalk(monitoring, day, who, captures);
    if (match.date !== day) {
      problems.push(`Las marcas coinciden con las filas del ${match.date}, no del ${day}: se usa ${match.date}`);
      day = match.date;
    }
    paired = new Map(match.pairs.map(p => [p.capture, p.row]));
    pairing = new Map(captures.map((c, i) => [c, match.matches[i]]));
  }
  // New mark, recapture (same mark, species and sex) or reused ID, from the marks before the walk.
  const roles = day ? m.walkMarkRoles(monitoring, day, captures) : null;
  const preserved = m.preservedForRule(all, day ? parseDateText(day) : undefined);
  const reviews = captures.map((c, i) =>
    m.reviewCapture(c, i, {
      rows: monitoring,
      date: day || '',
      captures,
      ...(roles ? { roles } : {}),
      preserved,
      isIthomiini,
      ...(paired ? { existing: paired.get(c) ?? null } : {}),
    }),
  );
  return {
    m,
    row,
    data,
    all,
    dayRows,
    monitoring,
    captures,
    reviews,
    roles,
    preserved,
    isIthomiini,
    pairing,
    problems,
    day,
    who,
    source,
    lists: listOptions(store, MODULE),
    formulas: createFormulas(all),
  };
}

/**
 * The walk's points with the app's review, and the Collection_data rows to
 * propose for the captures that are not in the sheet yet. `date` and
 * `collector` override what the walk says (a title without a day, a profile
 * without its collector).
 */
export async function walkDraft(store, { walkId, url, date, collector } = {}) {
  const found = walkRecord(store, { walkId, url });
  if (found.pending) return found.pending;
  const w = await readWalk(store, found.row, { date, collector });
  const { m, row, captures, reviews, pairing, lists, formulas, day, who, problems } = w;
  const points = [];
  const newRows = [];
  captures.forEach((c, i) => {
    const review = reviews[i];
    const checks = review.list.map(k => `${k.kind}: ${k.text}`);
    // How sure the pairing with its row is (mark, sure, tie, order, none); doubtful ones are listed in Monitoreo → Dudas.
    const how = pairing?.get(c);
    if (how && m.doubtfulMatch(how))
      checks.push(`warn: emparejada con la fila ${review.existing?.row ?? '—'} por ${how.confidence === 'tie' ? 'empate' : how.confidence === 'order' ? 'orden' : 'marca/hora'}${how.conflicts.length ? ` (no coincide: ${how.conflicts.join(', ')})` : ''}: revisar en Monitoreo → Dudas`);
    if (c.species && lists.SPECIES && !lists.SPECIES.values.has(c.species))
      checks.push(`warn: ${c.species} no está en Taxonomy_v18Jun25 (la hoja lo rechazaría)`);
    const twin = day && who ? teammateRow(w, c) : null;
    if (twin) checks.push(`warn: ${twinText(twin)}`);
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
      pairing: how ? { confidence: how.confidence, conflicts: how.conflicts } : null,
      checks,
    };
    points.push(point);
    if (review.existing || !day || !who) return;
    const values = m.captureValues(c, { date: day, collector: who, section: c.section });
    for (const field of formulas) delete values[field];
    // Readable for the model and the person; propose_changes turns them back into sheet values.
    values.Collection_date = day;
    for (const key of ['Death_date', 'Preservation_date']) if (typeof values[key] === 'number') values[key] = day;
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
      collectorFrom: w.source ?? (who ? 'title' : null),
      reviewStatus: row.status,
      points: points.length,
      inSheet: points.filter(p => p.inSheet).length,
      proposalId: row.proposal_id ?? null,
    },
    problems,
    points,
    newRows,
    next: newRows.length
      ? `Propose newRows with propose_changes (one proposal for the walk). ${blocking} of them have warnings: fix what the note makes clear, otherwise say so in that row's note and tell the person.`
      : 'Nothing to propose.',
  };
}

/**
 * The same butterfly already in the sheet that day under another collector (a
 * teammate's walk of the same morning, e.g. a newcomer trained on it): the same
 * mark, or for an unmarked one the same species and sex within 3 minutes.
 */
function teammateRow(w, c) {
  const serial = parseDateText(w.day);
  const mine = w.who.split(' - ')[0].trim().toUpperCase();
  const others = w.monitoring.filter(
    r => r.values.Collection_date === serial && text(r.values.Collector).split(' - ')[0].trim().toUpperCase() !== mine,
  );
  if (c.markId) return others.find(r => text(r.values.FieldMark_ID).toUpperCase() === c.markId.toUpperCase()) ?? null;
  if (!c.species || c.minutes === null) return null;
  return (
    others.find(r => {
      const t = r.values.Collection_time;
      return (
        text(r.values.SPECIES) === c.species &&
        (!c.sex || !text(r.values.Sex) || text(r.values.Sex).startsWith(c.sex)) &&
        typeof t === 'number' &&
        Math.abs(t * 1440 - c.minutes) <= 3
      );
    }) ?? null
  );
}
/** A row of the sheet in a note: its mark, species, sex (and collector) and row number. */
const rowText = (r, collector = false) =>
  `${[text(r.values.FieldMark_ID) !== 'NA' ? text(r.values.FieldMark_ID) : '', text(r.values.SPECIES) || '?', text(r.values.Sex)].filter(Boolean).join(' ')}${collector ? `, ${text(r.values.Collector).split(' - ')[0]}` : ''} (${r.row})`;
const twinText = r =>
  `ya en la hoja ese día: fila ${r.row} (${text(r.values.FieldMark_ID) || '—'} ${text(r.values.SPECIES)}, ${text(r.values.Collector).split(' - ')[0]}): ¿la misma mariposa, anotada por un compañero?`;

/** A date serial as the team reads it: dd/mm/yyyy. */
const serialDay = serial => new Date(Date.UTC(1899, 11, 30) + serial * 864e5).toISOString().slice(0, 10).split('-').reverse().join('/');
const minutesText = (m, minutes) => m.formatMinutes(minutes) || '';
/** A cell's note in the proposal's table: where its value comes from (hint), what to look at (doubt), what to type (unreadable). */
const hint = (key, vars) => {
  const m = msg(key, vars);
  return { text: m.text, msg: m.msg };
};
const doubt = (key, vars, extra = {}) => {
  const m = msg(key, vars);
  return { reason: m.text, reasonMsg: m.msg, ...extra };
};
const unread = doubt;

/**
 * A walk as the rows of its proposal (Monitoreo → Importar, the same review
 * table as the assistant's): one Collection_data row per capture not in the
 * sheet yet, in the walk's order, with sheet values, the Wikiloc photos of its
 * point, and per cell where its value comes from (hints), what to look at
 * (doubts) and what the note does not say (unreadable: empty until typed);
 * and the day's SamplingDay_data row (start and end from the recording).
 * `firstCam`, `firstTube`: the preserved butterflies get these and the next
 * ones in walk order. `skip`: point indexes left out (a butterfly that escaped).
 * Nothing is written here.
 */
export async function walkRows(store, { walkId, url, date, collector, firstCam, firstTube, skip = [] } = {}) {
  const found = walkRecord(store, { walkId, url });
  if (found.pending) return { status: 'pending', ...found.pending };
  const w = await readWalk(store, found.row, { date, collector });
  const { m, row, data, captures, reviews, roles, preserved, isIthomiini, formulas, day, who, problems } = w;
  if (!day || !who) return { status: 'needs', problems, walk: { id: row.id, name: row.name, date: day, collector: who } };
  const left = new Set((Array.isArray(skip) ? skip : []).map(Number));
  const serial = parseDateText(day);
  // Walk order for the IDs handed out: by time (the point order when there is none).
  const order = captures.map((c, i) => i).sort((a, b) => (captures[a].minutes ?? 1e9) - (captures[b].minutes ?? 1e9) || a - b);
  const takes = i => !reviews[i].existing && !captures[i].markId && !left.has(i);
  const fillOf = first => {
    if (!text(first)) return null;
    const ids = m.fillInOrder(order, i => takes(i), text(first));
    if (!ids) throw fail('INVALID_FIRST_ID', `«${text(first)}» does not end in a number`);
    return new Map(order.map((i, k) => [i, ids[k]]));
  };
  const cams = fillOf(firstCam);
  const tubes = fillOf(firstTube);
  const rows = [];
  const inSheet = [];
  for (const i of order) {
    const c = captures[i];
    const review = reviews[i];
    if (review.existing) {
      inSheet.push({ point: i, text: c.text, row: review.existing.row });
      continue;
    }
    if (left.has(i)) continue;
    const values = m.captureValues(c, {
      date: day,
      collector: who,
      section: c.section,
      cam: cams?.get(i) ?? null,
      tube: tubes?.get(i) ?? null,
    });
    for (const field of formulas) delete values[field];
    for (const key of Object.keys(values)) if (values[key] === null || values[key] === undefined) delete values[key];
    // Times as the team reads them (9:20); the proposal stores the sheet's day fraction.
    if (c.minutes !== null) values.Collection_time = m.formatMinutes(c.minutes);
    const hints = {};
    const doubts = {};
    const unreadable = {};
    const role = roles?.[i] ?? null;
    // Where the values come from.
    if (c.section !== null) hints.Transect_section = hint(tpl('T{section}: de la posición GPS del punto (a {m} m del sendero)'), { section: c.section, m: Math.round(c.sectionDistance) });
    else doubts.Transect_section = doubt(tpl('El punto está a {m} m del sendero: ¿qué tramo?'), { m: Math.round(c.sectionDistance) });
    if (c.timeFromPoint) hints.Collection_time = hint(tpl('{time}: cuando se guardó el punto en Wikiloc (la nota no dice la hora)'), { time: minutesText(m, c.minutes) });
    if (!c.rain) hints.Rainfall = hint(tpl('La nota no dice lluvia: seco (DY), como lo escribe el equipo'));
    if (c.subspeciesGuess === 'ikiam') hints.Subspecies_Form = hint(tpl('{sub}: la única subespecie de {species} vista en Ikiam'), { sub: c.subspecies, species: c.species });
    if (c.rest) hints.Notes_Collection_data = hint(tpl('Palabras de la nota no entendidas: «{words}»'), { words: c.rest });
    // What the note leaves out: empty, to type.
    if (!c.sex) unreadable.Sex = unread(tpl('No está en la nota de Wikiloc: mira la foto'));
    if (c.minutes === null) unreadable.Collection_time = unread(tpl('No está en la nota de Wikiloc'));
    if (c.height === null) unreadable.Flight_height = unread(tpl('No está en la nota de Wikiloc'));
    if (!c.cloud) unreadable.Cloud_cover = unread(tpl('No está en la nota de Wikiloc (los puntos de antes y después pueden decirlo)'));
    // The name.
    if (!c.species) unreadable.SPECIES = c.rest ? unread(tpl('La nota no nombra ninguna conocida («{words}»): mira la foto'), { words: c.rest }) : unread(tpl('No está en la nota de Wikiloc: mira la foto'));
    else if (!c.known) doubts.SPECIES = doubt(tpl('«{species}» no es un nombre visto en la hoja: revisa cómo se escribe con la foto'), { species: c.species });
    else if (c.unsure === 'species') doubts.SPECIES = doubt(tpl('La nota duda de la especie («?»): ID_status Complete_but_verify'));
    if (c.species && c.unsure === 'subspecies') doubts.Subspecies_Form = doubt(tpl('La nota duda de la subespecie («?»): ID_status Complete_but_verify'));
    // The mark.
    if (c.markId) {
      const since = role?.first?.values?.Collection_date;
      if (role?.role === 'recapture')
        hints.FieldMark_ID = hint(tpl('Recaptura de {mark} (marcada el {date}, fila {row}): una fila Mark_Released nueva con la misma marca, como dice el protocolo'), { mark: c.markId, date: typeof since === 'number' ? serialDay(since) : '—', row: role.first?.row ?? '—' });
      else if (role?.role === 'reused' && !role.known)
        doubts.FieldMark_ID = doubt(tpl('{mark} ya se usó para {rows}: ¿otro número? Pregunta al equipo'), { mark: c.markId, rows: role.others.slice(-2).map(r => rowText(r)) });
      else if (role?.role === 'unsure') doubts.FieldMark_ID = doubt(tpl('{mark} era {rows}: la especie dice si es recaptura'), { mark: c.markId, rows: role.others.slice(-1).map(r => rowText(r)) });
      if (role?.role !== 'recapture' && c.recaptureNote)
        doubts.FieldMark_ID = role?.others?.length ? doubt(tpl('La nota dice recaptura, pero {mark} era {row}'), { mark: c.markId, row: rowText(role.others.at(-1)) }) : doubt(tpl('La nota dice recaptura, pero {mark} no está en la hoja'), { mark: c.markId });
      const again = captures.filter((o, j) => j !== i && o.markId === c.markId);
      if (again.length)
        doubts.FieldMark_ID = doubt(tpl('{mark} también está en {points} de este recorrido: ¿dos mariposas con una marca? Pregunta al equipo'), { mark: c.markId, points: again.map(o => `${minutesText(m, o.minutes)} ${o.species ?? '?'} ${o.sex ?? ''}`.trim()) });
    } else {
      const n = c.species ? preserved.get(c.species) || 0 : 0;
      if (n >= m.MARK_THRESHOLD && isIthomiini(c.species))
        doubts.Release_Collect = doubt(tpl('{species} ya tiene {n} preservados (la regla de los 30): ¿marcar y soltar? La nota no tiene marca'), { species: c.species, n }, { alternatives: ['Mark_Released'] });
      else hints.Release_Collect = c.species ? hint(tpl('Sin marca en la nota: preservada ({n} {species} preservadas hasta ahora)'), { n, species: c.species }) : hint(tpl('Sin marca en la nota: preservada'));
      if (!cams) unreadable.CAM_ID = unread(tpl('Da el «Primer CAM_ID» arriba, o escríbelo del sobre'));
      else hints.CAM_ID = hint(tpl('En el orden del recorrido desde {first}'), { first: text(firstCam) });
      if (!tubes) unreadable.Tube_1_id = unread(tpl('Da el «Primer tubo» arriba, o escríbelo del tubo'));
      else hints.Tube_1_id = hint(tpl('En el orden del recorrido desde {first}'), { first: text(firstTube) });
    }
    // The same butterfly already entered by a teammate that day.
    const twin = teammateRow(w, c);
    if (twin)
      doubts.Collector = doubt(tpl('Ya en la hoja ese día: {row}: ¿la misma mariposa anotada en el recorrido de un compañero? Entonces quita esta fila'), { row: rowText(twin, true) });
    // A value the sheet's list would refuse (a name not in Taxonomy): left empty, what the note says offered to take.
    for (const [field, value] of Object.entries(values)) {
      if (!w.lists[field]?.strict) continue;
      const problem = listProblemMsg(w.lists, field, value);
      if (!problem) continue;
      delete values[field];
      delete doubts[field];
      unreadable[field] = { reason: problem.text, reasonMsg: problem.msg, partial: [String(value)] };
      if (field === 'SPECIES') {
        delete values.Subspecies_Form;
        delete values.Identifier;
        values.ID_status = 'To_identify';
      }
    }
    // The template's columns, not from the note: shown as inferred.
    const fromNote = new Set(['FieldMark_ID', 'SPECIES', 'Subspecies_Form', 'Sex', 'Collection_time', 'Flight_height', 'Cloud_cover', ...(c.rain ? ['Rainfall'] : [])]);
    rows.push({
      point: i,
      sheet: MODULE,
      values,
      note: `Wikiloc: ${c.text}`.slice(0, 300),
      photos: c.photos,
      ...(Object.keys(hints).length ? { hints } : {}),
      ...(Object.keys(doubts).length ? { doubts } : {}),
      ...(Object.keys(unreadable).length ? { unreadable } : {}),
      inferred: Object.keys(values).filter(f => !fromNote.has(f)),
    });
  }
  // SamplingDay_data: the day of effort, also for a walk without captures.
  const initials = who.split(' - ')[0].trim();
  const span = m.walkSpan(data, day);
  const existingDay = m.samplingDayRow(w.dayRows, day, initials, span);
  const remark = [row.name.length > 40 ? `«${row.name}»` : '', data.description ? `«${data.description}»` : ''].filter(Boolean).join(' · ');
  const took = typeof data.minutes === 'number' ? `${Math.floor(data.minutes / 60)} h ${data.minutes % 60} min` : null;
  let samplingDay = null;
  if (existingDay?.broken)
    problems.push(`SamplingDay_data row ${existingDay.row.row} looks like this day (${initials}) but its Date (${existingDay.row.values.Date}) is not a date: correct it there; no other row is proposed`);
  else if (existingDay) {
    const values = {};
    const hints = {};
    const blank = v => v === null || v === undefined || v === '' || v === 'NA';
    if (span && blank(existingDay.row.values.Start_time)) {
      values.Start_time = m.formatMinutes(span.start);
      hints.Start_time = hint(tpl('Cuando empezó la grabación en Wikiloc'));
    }
    if (span?.end != null && blank(existingDay.row.values.End_time)) {
      values.End_time = m.formatMinutes(span.end);
      hints.End_time = hint(tpl('Inicio + el «Tiempo» de Wikiloc ({took})'), { took });
    }
    if (Object.keys(values).length) samplingDay = { recordId: existingDay.row.id, row: existingDay.row.row, values, hints, note: 'Start and end from the Wikiloc recording' };
  } else {
    const values = { Date: serial, Location: 'Ikiam', Purpose: 'Monitoring', Collectors_initials: initials };
    const hints = {};
    const unreadable = {};
    if (span) {
      values.Start_time = m.formatMinutes(span.start);
      hints.Start_time = hint(tpl('Cuando empezó la grabación en Wikiloc'));
      if (span.end != null) {
        values.End_time = m.formatMinutes(span.end);
        hints.End_time = hint(tpl('Inicio + el «Tiempo» de Wikiloc ({took}): revísalo'), { took });
      } else unreadable.End_time = unread(tpl('Wikiloc no dice cuánto duró'));
    } else {
      unreadable.Start_time = unread(tpl('No se leyó de Wikiloc (trae el recorrido de nuevo)'));
      unreadable.End_time = unread(tpl('No se leyó de Wikiloc (trae el recorrido de nuevo)'));
    }
    if (remark) unreadable.Notes = unread(tpl('En Wikiloc: {remark}. El equipo escribe estas notas en inglés («{day} {initials}: …»)'), { remark, day: day.split('-').reverse().join('/'), initials });
    samplingDay = { sheet: 'SamplingDay_data', values, hints, unreadable, note: rows.length ? 'The walk\'s day' : 'The walk\'s day: no butterflies noted' };
  }
  return {
    status: 'ready',
    walk: { id: row.id, wikilocId: row.wikiloc_id, url: row.url, name: row.name, date: day, collector: who, collectorFrom: w.source, proposalId: row.proposal_id ?? null },
    problems,
    rows,
    inSheet,
    samplingDay,
  };
}
