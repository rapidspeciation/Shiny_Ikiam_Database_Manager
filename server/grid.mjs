// Read-side helpers for the spreadsheet grid: whole-sheet payloads and the
// next-identifier suggestions the original Shiny app offered.

import { REFERENCES } from './insectaryId.mjs';
import { moduleMap } from './schema.mjs';
import { msg, msgError, tpl } from './messages.mjs';
import { claimedValues } from './claims.mjs';

const blank = value => value === null || value === undefined || /^\s*(|NA|N\/A)\s*$/i.test(String(value));
const fail = (code, message, status = 400) => msgError(message, { code, status });

/**
 * One sheet as compact rows: values are arrays in column order and formula
 * cells are listed by column index. Unused pre-filled rows are included and
 * flagged, because new rows are written into them.
 */
export function tablePayload(store, module) {
  const mod = moduleMap.get(module);
  if (!mod) throw fail('MODULE_NOT_FOUND', 'Hoja desconocida', 404);
  const rows = store.db
    .prepare(
      'SELECT id,row_num,version,observed,values_json,formulas_json FROM records WHERE sheet=? AND missing=0 AND row_num>? AND row_num<2000000000 ORDER BY row_num',
    )
    .all(module, mod.headerRow);
  const keys = mod.fields.map(f => f.key);
  const layout = store.layouts.get(module);
  return {
    module,
    // A field whose column is missing from the sheet shows its last known values, read-only.
    columns: mod.fields.map(({ column, ...field }) =>
      layout && !layout.blocked && !layout.columns.has(field.key)
        ? { ...field, readonly: true, unavailable: true }
        : field,
    ),
    headerProblems: store.headerProblems.get(module) || [],
    rows: rows.map(r => wireRow(keys, r)),
    latest: latestUpdate(store, module),
  };
}

/** One row as an array of values in column order, plus the indexes of formula cells. */
export function wireRow(keys, r) {
  const values = JSON.parse(r.values_json);
  const formulas = JSON.parse(r.formulas_json);
  return {
    id: r.id,
    row: r.row_num,
    version: r.version,
    observed: Boolean(r.observed),
    v: keys.map(k => values[k] ?? null),
    f: keys.flatMap((k, i) => (formulas[k] ? [i] : [])),
  };
}

function latestUpdate(store, module) {
  return store.db.prepare('SELECT max(updated_at) u FROM records WHERE sheet=?').get(module).u || '';
}

/**
 * What changed in a sheet's local copy since `since` (the `latest` of an earlier
 * reply), so open pages can follow edits without reloading the whole sheet.
 * `count` lets the page check that it holds the same rows as the server.
 */
export function tableChanges(store, module, since) {
  const mod = moduleMap.get(module);
  if (!mod) throw fail('MODULE_NOT_FOUND', 'Hoja desconocida', 404);
  const keys = mod.fields.map(f => f.key);
  const visible = r => !r.missing && r.row_num > mod.headerRow && r.row_num < 2000000000;
  const recent = store.db
    .prepare(
      'SELECT id,row_num,version,observed,missing,values_json,formulas_json FROM records WHERE sheet=? AND updated_at>=?',
    )
    .all(module, String(since || ''));
  const count = store.db
    .prepare('SELECT count(*) n FROM records WHERE sheet=? AND missing=0 AND row_num>? AND row_num<2000000000')
    .get(module, mod.headerRow).n;
  return {
    module,
    revision: tableRevision(store, module),
    latest: latestUpdate(store, module),
    count,
    rows: recent.filter(visible).map(r => wireRow(keys, r)),
    removed: recent.filter(r => !visible(r)).map(r => r.id),
  };
}

/** Cheap fingerprint of a sheet's local copy, used for caching and ETags. */
export function tableRevision(store, module) {
  const r = store.db
    .prepare(
      'SELECT count(*) n, max(updated_at) u, total(version) v, total(row_num) r FROM records WHERE sheet=? AND missing=0',
    )
    .get(module);
  return `${r.n}-${r.u}-${r.v}-${r.r}`;
}

function rowsOf(store, sheet) {
  return store.db
    .prepare(
      'SELECT row_num,observed,values_json,formulas_json FROM records WHERE sheet=? AND missing=0 AND row_num>0 ORDER BY row_num',
    )
    .all(sheet)
    .map(r => ({
      row: r.row_num,
      observed: Boolean(r.observed),
      values: JSON.parse(r.values_json),
      formulas: JSON.parse(r.formulas_json || '{}') || {},
    }));
}

/**
 * Next identifiers, following the original Shiny app:
 *  - insectary: the pre-filled IDs of unused rows after the last recorded butterfly;
 *  - cam: the number after each consecutive run of CAM IDs, newest runs first;
 *  - tube: the number after each run of tube IDs, per prefix and preservation medium.
 * With `start`, returns `count` consecutive unused IDs beginning at `start`.
 * With `check` (cam or tube; IDs separated by commas), says which of them are
 * used already and where (the Tubos cards check typed and scanned tubes).
 */
export function idSuggestions(store, { kind, start, count, check } = {}) {
  const sheets = ID_SHEETS[kind]?.();
  if (!sheets) return computeIds(store, { kind, start, count });
  // Reading every row of these sheets takes up to a second (tube IDs): the answer is kept
  // until one of the sheets it reads changes.
  // Identifiers held by entries not in the sheet yet (server/claims.mjs) count as used too: those of
  // this kind (a tap holding an Insectary ID, server/holds.mjs, leaves the tubes' answers alone).
  // Each kind keeps its own answers, so asking for tubes and Insectary IDs in turn reads neither again.
  const stamp = [...sheets.map(sheet => tableRevision(store, sheet)), claimStamp(store, kind)].join('|');
  const kinds = idCache.get(store) ?? idCache.set(store, new Map()).get(store);
  let cache = kinds.get(kind);
  if (cache?.stamp !== stamp) kinds.set(kind, (cache = { stamp, answers: new Map() }));
  const key = `${kind}\u0000${start ?? ''}\u0000${count ?? ''}\u0000${check ?? ''}`;
  if (!cache.answers.has(key)) {
    const answer = computeIds(store, { kind, start, count, check });
    if (cache.answers.size >= 200) cache.answers.clear();
    cache.answers.set(key, answer);
  }
  return structuredClone(cache.answers.get(key));
}

const idCache = new WeakMap();
/** Changes whenever a claim of `kind` is taken or released. */
function claimStamp(store, kind) {
  try {
    const r = store.db.prepare('SELECT count(*) n, max(created_at) at, group_concat(value) v FROM claims WHERE kind = ?').get(kind);
    return `${r.n}-${r.at}-${r.v?.length ?? 0}`;
  } catch {
    return '';
  }
}
/** Where a claimed identifier is: an entry of `name` not in the sheet yet. */
const claimedHolder = holder => ({ sheet: null, row: null, label: null, claimedBy: holder.name });
/** The sheets each kind of suggestion reads. */
const ID_SHEETS = {
  insectary: () => ['Insectary_data', ...Object.keys(REFERENCES).filter(sheet => moduleMap.has(sheet))],
  cam: () => ['Insectary_data', 'Collection_data'],
  tube: () =>
    [...moduleMap.values()]
      .filter(mod => mod.fields.some(f => TUBE_COLUMN.test(f.key) && !NOT_TUBE_COLUMN.test(f.key)))
      .map(mod => mod.id),
};

function computeIds(store, { kind, start, count, check } = {}) {
  const n = Math.min(Math.max(Number(count) || 20, 1), 500);
  if (check !== undefined && (kind === 'cam' || kind === 'tube')) return usedAmong(kind === 'cam' ? usedCamIds(store) : usedTubeIds(store), check);
  // Every free pre-made row can be offered (earlier empty rows included).
  if (kind === 'insectary') return insectaryIds(store, start, Math.min(Math.max(Number(count) || 20, 1), 5000));
  if (kind === 'cam') return start ? fromStart(start, n, usedCamIds(store)) : camSuggestions(store);
  if (kind === 'tube') return start ? fromStart(start, n, usedTubeIds(store)) : tubeSuggestions(store);
  throw fail('INVALID_KIND', 'kind debe ser insectary, cam o tube');
}

/**
 * The pre-made rows free in the sheets (claims aside), the last row used and the
 * last pre-made ID: reading every row of Insectary_data and the sheets naming IDs
 * takes half a second, so it is kept until one of those sheets changes (a tap's
 * hold, server/holds.mjs, changes only the claims laid over it).
 */
const insectaryBases = new WeakMap();
function insectaryBase(store) {
  const stamp = ID_SHEETS.insectary().map(sheet => tableRevision(store, sheet)).join('|');
  const hit = insectaryBases.get(store);
  if (hit?.stamp === stamp) return hit;
  const rows = rowsOf(store, 'Insectary_data');
  const norm = value => String(value ?? '').trim().toUpperCase();
  const used = new Set(rows.filter(r => r.observed).map(r => norm(r.values.Insectary_ID)));
  for (const [sheet, fields] of Object.entries(REFERENCES))
    for (const r of moduleMap.has(sheet) ? rowsOf(store, sheet) : [])
      for (const field of fields) if (!blank(r.values[field])) used.add(norm(r.values[field]));
  const copies = new Map();
  for (const r of rows) copies.set(norm(r.values.Insectary_ID), (copies.get(norm(r.values.Insectary_ID)) || 0) + 1);
  const lastObserved = rows.reduce((max, r) => (r.observed ? Math.max(max, r.row) : max), 0);
  const typedIn = r =>
    Object.entries(r.values).some(
      ([field, value]) => field !== 'Insectary_ID' && !r.formulas[field] && String(value ?? '').trim() !== '',
    );
  const free = rows.filter(r => {
    const id = norm(r.values.Insectary_ID);
    return !r.observed && id && !blank(id) && !used.has(id) && copies.get(id) === 1 && !typedIn(r);
  });
  const round = r => /^[A-ZÑ]\d([A-Z])$/.exec(norm(r.values.Insectary_ID))?.[1] ?? '';
  const lastPremade = rows.findLast(r => round(r))?.values.Insectary_ID;
  const out = { stamp, rows: free, lastObserved, lastPremade };
  insectaryBases.set(store, out);
  return out;
}

/**
 * Free pre-made Insectary IDs. The letter at the end is the round of the
 * sheet's ID formula (A0A…Z9A, then A0B…Z9B, …, now D), not a kind of
 * butterfly. The team mostly goes on after the last row used, but also fills
 * earlier empty rows (backlogs, emergences typed later: H0B–H2B, L8D…), so
 * those count too, after the ones at the end. An ID is free when its row is
 * empty (nothing typed besides its formulas: a row of NA with a note "we skipt
 * this ID" is not) and no row of any sheet names it; an ID with two pre-made
 * rows is left out.
 * `tail` is how many come after the last row used (the usual suggestion).
 */
function insectaryIds(store, start, count) {
  const { rows: premade, lastObserved, lastPremade } = insectaryBase(store);
  const norm = value => String(value ?? '').trim().toUpperCase();
  // Held by an Emergidos entry, a card's tap (server/holds.mjs) or a save waiting for Google, of anyone: never offered again.
  const claimed = claimedValues(store.db, 'insectary');
  const free = premade.filter(r => !claimed.has(norm(r.values.Insectary_ID)));
  // Pre-made rows free but for a claim: the cards holding them keep their place in the order.
  const held = premade.filter(r => claimed.has(norm(r.values.Insectary_ID))).map(r => ({ value: String(r.values.Insectary_ID).trim(), row: r.row }));
  const tail = free.filter(r => r.row > lastObserved);
  // Earlier rows: the newest round first (D before C before B), in sheet order. IDs of older
  // forms (85Y, 6HQ) are not offered: they can still be typed when a wing carries one.
  const round = r => /^[A-ZÑ]\d([A-Z])$/.exec(norm(r.values.Insectary_ID))?.[1] ?? '';
  const earlier = free.filter(r => r.row < lastObserved && round(r));
  const newest = new Map();
  for (const r of earlier) newest.set(round(r), Math.max(newest.get(round(r)) ?? 0, r.row));
  const rank = r => (round(r) ? newest.get(round(r)) : -1);
  earlier.sort((a, b) => rank(b) - rank(a) || a.row - b.row);
  let pool = [...tail, ...earlier];
  if (start) {
    // From a chosen ID the rows follow in sheet order (H0B → H1B → H2B).
    const from = free.find(r => norm(r.values.Insectary_ID) === norm(start));
    if (!from) throw fail('ID_NOT_AVAILABLE', msg('{id} no es un Insectary ID preasignado libre', { id: start }), 409);
    pool = free.filter(r => r.row >= from.row && (r === from || round(r)));
  }
  const ids = pool.slice(0, count).map(r => ({ value: String(r.values.Insectary_ID).trim(), row: r.row }));
  return {
    suggestions: ids.slice(0, 1).map(i => ({ value: i.value, label: `${i.value} (fila ${i.row})` })),
    sequence: ids.map(i => i.value),
    rows: ids,
    tail: start ? ids.length : Math.min(tail.length, ids.length),
    freeAtEnd: tail.length,
    held,
    last: lastPremade ? String(lastPremade).trim() : null,
  };
}

const splitId = id => {
  const m = /^([A-Za-z]+)(\d+)$/.exec(String(id).trim());
  return m ? { prefix: m[1], number: Number(m[2]), width: m[2].length } : null;
};
const makeId = (prefix, number, width) => `${prefix}${String(number).padStart(width, '0')}`;

/**
 * Consecutive IDs from `start`, skipping any already used. When `start` itself
 * is used, `startUsed` says where, so the page can tell the person instead of
 * silently starting from another ID.
 */
function fromStart(start, count, used) {
  const value = String(start).trim().toUpperCase();
  const seq = sequence(value, count, used);
  const holder = used.get(value);
  return holder ? { sequence: seq, startUsed: { value, ...holder }, nextFree: seq[0] } : { sequence: seq };
}

function sequence(start, count, used) {
  const parts = splitId(start);
  if (!parts) throw fail('INVALID_ID', 'El ID inicial debe ser como CAM078277 o FS00001234');
  const out = [];
  for (let n = parts.number; out.length < count; n++) {
    const id = makeId(parts.prefix, n, parts.width);
    if (!used.has(id)) out.push(id);
  }
  return out;
}

/** Which of the IDs in `check` ("FS1,FS2", at most 200) are used, each with the first row holding it. */
function usedAmong(used, check) {
  const values = [...new Set(String(check).split(',').map(v => v.trim().toUpperCase()).filter(Boolean))].slice(0, 200);
  return { used: Object.fromEntries(values.filter(v => used.has(v)).map(v => [v, used.get(v)])) };
}

/** Where a used ID is: the first row holding it (sheet, row, the row's label). */
const holderOf = (sheet, r) => ({
  sheet,
  row: r.row,
  label: String(r.values.Insectary_ID ?? r.values.CAM_ID ?? r.values.FieldMark_ID ?? '').trim() || null,
});

/** CAM IDs already used, each with the first row holding it. */
function usedCamIds(store) {
  const used = new Map();
  for (const sheet of ['Insectary_data', 'Collection_data'])
    for (const r of rowsOf(store, sheet))
      for (const key of ['CAM_ID', 'CAM_ID_CollData', 'CAM_ID_insectary']) {
        const value = String(r.values[key] ?? '').trim();
        if (!blank(value) && !used.has(value)) used.set(value, holderOf(sheet, r));
      }
  for (const [value, holder] of claimedValues(store.db, 'cam')) if (!used.has(value)) used.set(value, claimedHolder(holder));
  return used;
}

const TUBE_ID = /^Tube_\d_id(?:_LEGS)?$/;
/** Columns that hold tube barcodes in any sheet (not their tissue, rack or manifest). */
const TUBE_COLUMN = /tube/i;
const NOT_TUBE_COLUMN = /tissue|rack|manifest|location|split|size|filter/i;
const BARCODE = /^[A-Z]{2}\d{7,9}$/;

/** Every tube barcode already used anywhere in the workbook (so a suggestion is never taken). */
function usedTubeIds(store) {
  const used = new Map();
  for (const mod of moduleMap.values()) {
    const keys = mod.fields.map(f => f.key).filter(k => TUBE_COLUMN.test(k) && !NOT_TUBE_COLUMN.test(k));
    if (!keys.length) continue;
    for (const r of rowsOf(store, mod.id))
      for (const key of keys) {
        const value = String(r.values[key] ?? '').trim();
        if (BARCODE.test(value) && !used.has(value)) used.set(value, holderOf(mod.id, r));
      }
  }
  for (const [value, holder] of claimedValues(store.db, 'tube')) if (!used.has(value)) used.set(value, claimedHolder(holder));
  return used;
}

/** Groups numbered IDs into runs of consecutive numbers and returns the ID after each run. */
function nextAfterRuns(items, used) {
  const byGroup = new Map();
  for (const item of items) {
    const parts = splitId(item.id);
    if (!parts) continue;
    const key = `${parts.prefix}\u0000${item.group || ''}`;
    byGroup.set(key, [...(byGroup.get(key) || []), { ...item, ...parts }]);
  }
  const runs = [];
  for (const list of byGroup.values()) {
    list.sort((a, b) => a.number - b.number);
    let run = [list[0]];
    const close = () => {
      const last = run.at(-1);
      let next = last.number + 1;
      while (used.has(makeId(last.prefix, next, last.width))) next++;
      const dates = run.map(x => x.date).filter(d => typeof d === 'number');
      runs.push({
        value: makeId(last.prefix, next, last.width),
        prefix: last.prefix,
        group: last.group || null,
        date: dates.length ? Math.max(...dates) : null,
        lastRow: Math.max(...run.map(x => x.row)),
      });
    };
    for (const item of list.slice(1)) {
      if (item.number === run.at(-1).number + 1 || item.number === run.at(-1).number) run.push(item);
      else {
        close();
        run = [item];
      }
    }
    close();
  }
  // Newest runs first: by date when known, otherwise by position in the sheet.
  return runs.sort((a, b) => (b.date ?? -1) - (a.date ?? -1) || b.lastRow - a.lastRow);
}

function camSuggestions(store) {
  const items = rowsOf(store, 'Insectary_data')
    .filter(r => r.observed && !blank(r.values.CAM_ID))
    .map(r => ({
      id: r.values.CAM_ID,
      row: r.row,
      date: numeric(r.values.Preservation_date) ?? numeric(r.values.Intro2Insectary_date),
    }));
  const runs = nextAfterRuns(items, usedCamIds(store));
  const perPrefix = new Map();
  const suggestions = [];
  for (const run of runs) {
    const n = perPrefix.get(run.prefix) || 0;
    if (n >= 2 || suggestions.some(s => s.value === run.value)) continue;
    perPrefix.set(run.prefix, n + 1);
    suggestions.push({ value: run.value, label: withDate(run.value, run.date) });
  }
  return { suggestions };
}

/**
 * Tube suggestions per rack. Several racks are in use at once: flash frozen
 * and ethanol go to different racks, and crosses (F1/F2, West × East) use other
 * racks than field collections and monitoring. Tubes are grouped by that work
 * and medium; the next free tube after each recently used run is suggested,
 * newest first.
 */
const CROSSES = /F1\/F2|WEST x EAST|cross|mutation/i;
const RACK_WORDS = {
  Cruces: tpl('Cruces'),
  Insectario: tpl('Insectario'),
  Colecta: tpl('Colecta'),
  Monitoreo: tpl('Monitoreo'),
  'Colecta (patas)': tpl('Colecta (patas)'),
  'Monitoreo (patas)': tpl('Monitoreo (patas)'),
  Patas: tpl('Patas'),
  'Medio sin indicar': tpl('Medio sin indicar'),
};
function tubeSuggestions(store) {
  const items = [];
  const add = (id, context, medium, date, row) => {
    const value = String(id ?? '').trim();
    if (!BARCODE.test(value)) return;
    const m = blank(medium) || /^(NA|NOT_COLLECTED)$/i.test(String(medium)) ? 'Medio sin indicar' : String(medium);
    items.push({ id: value, row, group: `${context} · ${m}`, context, medium: m, date: numeric(date) });
  };
  for (const r of rowsOf(store, 'Insectary_data')) {
    if (!r.observed) continue;
    const context = CROSSES.test(String(r.values.Research_purpose ?? '')) ? 'Cruces' : 'Insectario';
    for (const slot of [1, 2, 3, 4])
      add(
        r.values[`Tube_${slot}_id`],
        context,
        r.values[`T${Math.min(slot, 2)}_Preservation_medium`],
        r.values.Preservation_date,
        r.row,
      );
  }
  for (const r of rowsOf(store, 'Collection_data')) {
    if (!r.observed) continue;
    const context = /monitor/i.test(String(r.values.Purpose ?? '')) ? 'Monitoreo' : 'Colecta';
    const date = numeric(r.values.Preservation_date) ?? numeric(r.values.Collection_date);
    for (const slot of [1, 2, 3]) add(r.values[`Tube_${slot}_id`], context, r.values.Preservation_medium, date, r.row);
    add(r.values.Tube_4_id_LEGS, `${context} (patas)`, 'Patas', date, r.row);
  }
  const runs = nextAfterRuns(items, usedTubeIds(store));
  const byGroup = new Map();
  const suggestions = [];
  for (const run of runs) {
    const n = byGroup.get(run.group) || 0;
    // The two most recent runs of each rack group; old abandoned runs are left out.
    if (n >= 2 || suggestions.length >= 40 || suggestions.some(s => s.value === run.value)) continue;
    byGroup.set(run.group, n + 1);
    const [context, medium] = run.group.split(' · ');
    // The app's words (the rack's context, a medium it names) in the interface language; sheet media as they are.
    const word = w => (RACK_WORDS[w] ? msg(RACK_WORDS[w]) : w);
    const label = msg(run.date === null ? '{value} · {medium} · {context}' : '{value} · {medium} · {context} {date}', {
      value: run.value,
      medium: word(medium),
      context: word(context),
      ...(run.date === null ? {} : { date: withDate('', run.date).trim() }),
    });
    suggestions.push({ value: run.value, medium, context, date: run.date, label: label.text, labelMsg: label.msg });
  }
  return { suggestions };
}

const numeric = value => (typeof value === 'number' && Number.isFinite(value) ? value : null);
const MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
function withDate(text, serial) {
  if (serial === null) return text;
  const d = new Date(Date.UTC(1899, 11, 30) + Math.round(serial) * 86_400_000);
  return `${text} ${d.getUTCDate()}-${MONTHS[d.getUTCMonth()]}-${String(d.getUTCFullYear()).slice(2)}`;
}
