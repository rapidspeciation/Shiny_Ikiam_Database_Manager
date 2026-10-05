// Rows of the workbook as the assistant reads them (find_records, count_records,
// get_record, search_records): computed values of formula cells included, the
// formula text where it tells something (a count typed as =16+2-1), column
// filters, distance to a place, rows named by their ID in the sheet (W2B, a
// clutch number) and answers kept under a size budget so a wide query says
// "narrow it" instead of overflowing.

import { columnKeys, columnOf, moduleMap, parseDateText } from './schema.mjs';
import { RESULT_BUDGET, fitList } from './tool-budget.mjs';

const isoDate = serial => new Date(Date.UTC(1899, 11, 30) + serial * 864e5).toISOString().slice(0, 10);
const clip = (value, length) => String(value ?? '').slice(0, length);
/** A formula that is only arithmetic on typed numbers (a count kept as =16+2-1): its text is worth showing. */
const ARITHMETIC = /^=[\d\s+\-*/().]+$/;
/**
 * Characters of rows one find_records (or search_records) answer holds: the
 * rest of the answer (counts, formula columns, the note on what was cut) fits
 * in what is left of RESULT_BUDGET.
 */
export const FIND_BUDGET = RESULT_BUDGET - 6000;
const empty = value => value === null || value === undefined || value === '';
const textKey = value =>
  String(value ?? '')
    .toLowerCase()
    .normalize('NFD')
    .replace(/[̀-ͯ]/g, '')
    .replace(/\s+/g, ' ')
    .trim();
/** An identifier as the sheet's cells are compared with it: trimmed, any case. */
const idKey = value => String(value ?? '').trim().toLowerCase();
/** The database of a store (the tools are given either). */
const dbOf = source => source.db ?? source;

/**
 * A row as the assistant sees it: its app ID, sheet row, every non-empty value
 * (formula cells with their computed value; dates as YYYY-MM-DD) and `formulas`
 * = the formula text of arithmetic formulas (=16+2-1), or of every formula cell
 * with allFormulas. `fields` keeps only those columns. formulaColumns: false
 * leaves the list of formula columns out and inSheet: true the sheet (find_records
 * and describe_sheet give them once for all rows).
 */
export function compactRecord(record, { fields = null, allFormulas = false, formulaColumns = true, inSheet = false } = {}) {
  const mod = moduleMap.get(record.sheet);
  const dates = new Set(mod?.fields.filter(f => f.type === 'date').map(f => f.key));
  const values = {},
    formulas = {};
  for (const [key, value] of Object.entries(record.values ?? {})) {
    if (fields && !fields.has(key)) continue;
    if (!empty(value)) values[key] = dates.has(key) && typeof value === 'number' ? isoDate(value) : value;
  }
  for (const [key, formula] of Object.entries(record.formulas ?? {})) {
    if (fields && !fields.has(key)) continue;
    if (allFormulas || ARITHMETIC.test(String(formula))) formulas[key] = formula;
  }
  return {
    id: record.id,
    ...(inSheet ? {} : { sheet: record.sheet }),
    row: record.row,
    values,
    ...(Object.keys(formulas).length ? { formulas } : {}),
    ...(formulaColumns ? { formulaColumns: Object.keys(record.formulas ?? {}) } : {}),
  };
}

/*
 * Each sheet's rows, parsed once and kept while the sheet is unchanged: a query
 * (filters, 40 identifiers, a report's counts) reads them from memory instead
 * of parsing the sheet's JSON again. Per sheet, so an edit in Collection_data
 * leaves Insectary_data's rows as they are. Values only: a Collection_data row
 * holds some 6 kB of lookup formulas, read only for the rows returned.
 */
const parsed = new WeakMap();
/** A sheet's state: changes whenever one of its rows is written, synced or removed (both from indexes). */
const sheetStamp = (db, sheet) => {
  const s = db
    .prepare('SELECT (SELECT count(*) FROM records WHERE sheet = ? AND missing = 0) n, (SELECT max(updated_at) FROM records WHERE sheet = ?) u')
    .get(sheet, sheet);
  return `${s.n}:${s.u}`;
};
function sheetEntry(source, mod) {
  const db = dbOf(source);
  const sheets = parsed.get(db) ?? parsed.set(db, new Map()).get(db);
  const stamp = sheetStamp(db, mod.id);
  const hit = sheets.get(mod.id);
  if (hit?.stamp === stamp) return hit;
  clearTimeout(hit?.timer);
  const rows = db
    .prepare(
      'SELECT id, row_num, label, version, updated_at, observed, values_json FROM records WHERE sheet = ? AND missing = 0 AND row_num > ? AND row_num < 2000000000 ORDER BY row_num',
    )
    .all(mod.id, mod.headerRow)
    .map(r => ({
      id: r.id,
      sheet: mod.id,
      row: r.row_num,
      label: r.label,
      version: r.version,
      updatedAt: r.updated_at,
      observed: !!r.observed,
      values: JSON.parse(r.values_json),
    }));
  // A sheet nobody asks about for a while is let go.
  const timer = setTimeout(() => sheets.get(mod.id)?.rows === rows && sheets.delete(mod.id), 5 * 60_000);
  timer.unref?.();
  const entry = { stamp, rows, timer };
  sheets.set(mod.id, entry);
  return entry;
}
/**
 * Every row of a sheet below its header, the pre-made rows waiting to be
 * filled included (`observed: false`), parsed and shared: read them, never change them.
 */
export function cachedRows(source, sheet) {
  const mod = typeof sheet === 'string' ? moduleMap.get(sheet) : sheet;
  return mod ? sheetEntry(source, mod).rows : [];
}
/** The sheet's rows in use (observed: not the pre-made rows waiting to be filled, as the app's counts). */
const sheetRows = (source, mod) => cachedRows(source, mod).filter(r => r.observed);
const withFormulas = (db, record) =>
  record.formulas
    ? record
    : { ...record, formulas: JSON.parse(dbOf(db).prepare('SELECT formulas_json FROM records WHERE id = ?').get(record.id)?.formulas_json || '{}') };

// ------------------------------------------------------------------ rows by their ID

/**
 * The rows that hold each identifier (any case, trimmed) in an ID column
 * (schema identityFields: Insectary_ID, CLUTCH NUMBER…), in one pass:
 * Map(idKey → [{ id, sheet, row, label, primary }]); `primary`: in the sheet's
 * first ID column (Insectary_ID in Insectary_data, CAM_ID in Collection_data).
 * `sheet`: only that sheet's rows.
 */
function rowsWithIds(source, values, sheet = null) {
  const wanted = new Set(values.map(idKey).filter(Boolean));
  const out = new Map();
  if (!wanted.size) return out;
  const add = (key, hit) => {
    const list = out.get(key) ?? out.set(key, []).get(key);
    const known = list.find(h => h.id === hit.id);
    if (!known) list.push(hit);
    else known.primary ||= hit.primary;
  };
  if (sheet) {
    const mod = moduleMap.get(sheet);
    for (const r of cachedRows(source, mod))
      mod.identityFields.forEach((field, i) => {
        const key = idKey(r.values[field]);
        if (wanted.has(key)) add(key, { id: r.id, sheet: mod.id, row: r.row, label: r.label, primary: i === 0, observed: r.observed });
      });
    return out;
  }
  const rows = dbOf(source)
    .prepare(
      `SELECT r.id, r.sheet, r.row_num, r.label, r.observed, j.key field, lower(trim(CAST(j.value AS TEXT))) v
       FROM records r, json_each(r.identity_json) j
       WHERE r.missing = 0 AND r.row_num > 0 AND r.row_num < 2000000000
         AND lower(trim(CAST(j.value AS TEXT))) IN (SELECT value FROM json_each(?))
       ORDER BY r.sheet, r.row_num`,
    )
    .all(JSON.stringify([...wanted]));
  for (const r of rows) {
    const mod = moduleMap.get(r.sheet);
    if (!mod || r.row_num <= mod.headerRow) continue;
    add(r.v, { id: r.id, sheet: r.sheet, row: r.row_num, label: r.label, primary: mod.identityFields[0] === r.field, observed: !!r.observed });
  }
  return out;
}

const rowName = h => `${h.sheet} row ${h.row} (${h.observed ? h.label : `${h.label}, an empty pre-made row`}, recordId ${h.id})`;

/**
 * The rows a tool names: each by its app recordId, or as the sheet shows it:
 * its ID ("W2B", a clutch number; bare or { sheet, id }) or the values of some
 * columns ({ sheet, key: { column: value } }). `sheet`: the sheet the rows are
 * in when the tool knows it (bulk, show_rows). A bare ID found in several
 * sheets is the row whose first ID column holds it (W2B: Insectary_data, not
 * the Collection_data row of the same butterfly); of a filled row and an empty
 * pre-made row with one ID, the filled one. Returns { ids } (in the order
 * given; null where a ref fails) and problems [{ index, ref, error }], each
 * error saying what to give instead.
 */
export function resolveRows(source, refs, { sheet = null } = {}) {
  let byId;
  const recordOf = id => (source.getRecord ? source.getRecord(id) : (byId ??= source.prepare('SELECT id, sheet, label, missing FROM records WHERE id = ?')).get(id));
  const ids = new Array(refs.length).fill(null);
  const problems = [];
  const fail = (index, ref, error, kind = 'invalid') => problems.push({ index, ref, error, kind });
  const pending = new Map(); // sheet ('' = any) → [{ index, ref, value }]
  refs.forEach((ref, index) => {
    const object = !!ref && typeof ref === 'object' && !Array.isArray(ref);
    const inSheet = object && ref.sheet !== undefined && ref.sheet !== null ? String(ref.sheet) : sheet;
    if (inSheet && !moduleMap.has(inSheet)) return fail(index, ref, `Unknown sheet ${clip(inSheet, 60)}`);
    if (object && ref.key !== undefined) {
      if (!inSheet) return fail(index, ref, 'A key needs its sheet: {"sheet": …, "key": {column: value}}');
      if (!ref.key || typeof ref.key !== 'object' || Array.isArray(ref.key) || !Object.keys(ref.key).length)
        return fail(index, ref, 'key must be {column: value}');
      const columns = columnKeys(inSheet, Object.keys(ref.key));
      if (columns.error) return fail(index, ref, columns.error);
      const wanted = Object.values(ref.key).map(idKey);
      let hits = cachedRows(source, inSheet).filter(r => columns.keys.every((k, i) => idKey(r.values[k]) === wanted[i]));
      if (hits.length > 1 && hits.filter(r => r.observed).length === 1) hits = hits.filter(r => r.observed);
      const shown = clip(JSON.stringify(ref.key), 120);
      if (hits.length === 1) ids[index] = hits[0].id;
      else if (!hits.length) fail(index, ref, `No row of ${inSheet} with ${shown}; look it up with find_records`, 'missing');
      else fail(index, ref, `${hits.length} rows of ${inSheet} have ${shown}: ${hits.slice(0, 6).map(rowName).join('; ')}. Give the recordId of the one meant.`, 'ambiguous');
      return;
    }
    const value = object ? (ref.recordId ?? ref.id) : ref;
    if (value === null || value === undefined || String(value).trim() === '')
      return fail(index, ref, 'Name the row: its recordId, or its ID in the sheet ({"sheet": …, "id": …})');
    const text = clip(value, 120).trim();
    // An app recordId first, unless the ref says it is the sheet's ID.
    if (!(object && ref.id !== undefined && ref.recordId === undefined)) {
      const record = recordOf(text);
      if (record && !record.missing && (!inSheet || record.sheet === inSheet)) {
        ids[index] = record.id;
        return;
      }
      if (record?.missing) return fail(index, ref, `Row ${text} (${record.label}) is no longer in the sheet; look it up again with find_records`, 'missing');
      if (record) return fail(index, ref, `Row ${text} is in ${record.sheet}, not ${inSheet}`);
    }
    const list = pending.get(inSheet ?? '') ?? pending.set(inSheet ?? '', []).get(inSheet ?? '');
    list.push({ index, ref, value: text });
  });
  for (const [inSheet, items] of pending) {
    const found = rowsWithIds(source, items.map(i => i.value), inSheet || null);
    for (const { index, ref, value } of items) {
      const hits = found.get(idKey(value)) ?? [];
      const primary = hits.filter(h => h.primary);
      let pick = primary.length ? primary : hits;
      // The IDs come round again: a filled row over an empty pre-made row of the same ID.
      if (pick.length > 1 && pick.filter(h => h.observed).length === 1) pick = pick.filter(h => h.observed);
      if (pick.length === 1) ids[index] = pick[0].id;
      else if (!pick.length)
        fail(
          index,
          ref,
          `No row with ID ${value}${inSheet ? ` in ${inSheet} (ID columns: ${moduleMap.get(inSheet).identityFields.join(', ') || 'none'})` : ''}; give the row's recordId, or look it up with find_records`,
          'missing',
        );
      else fail(index, ref, `${value} is the ID of ${pick.length} rows: ${pick.slice(0, 6).map(rowName).join('; ')}. Give {"sheet", "id"} or the recordId of the one meant.`, 'ambiguous');
    }
  }
  return { ids, problems: problems.sort((a, b) => a.index - b.index) };
}

// ------------------------------------------------------------------ filters

const CONDITIONS = new Set(['contains', 'not', 'empty', 'from', 'to', 'min', 'max', 'in']);

/** A filter value as the sheet stores it: dates as serials, numbers as numbers, text compared loosely. */
function operand(field, value) {
  if (value === null || value === undefined) return { empty: true };
  if (field.type === 'date' && typeof value === 'string') {
    const serial = parseDateText(value.trim());
    if (serial !== null) return { number: serial };
  }
  if (typeof value === 'number') return { number: value };
  const text = String(value);
  if ((field.type === 'number' || field.type === 'date') && /^\s*-?\d+(?:\.\d+)?\s*$/.test(text)) return { number: Number(text) };
  return { text: textKey(text) };
}
const equals = (cell, op) => {
  if (op.empty) return empty(cell);
  if (empty(cell)) return false;
  if ('number' in op) return typeof cell === 'number' ? cell === op.number : textKey(cell) === String(op.number);
  return textKey(cell) === op.text;
};
const bound = (field, value) => {
  const op = operand(field, value);
  return 'number' in op ? op.number : null;
};

/**
 * The rows' test for filters { column: condition } (all must hold). A condition:
 * a value (equal; text ignores case and accents; dates YYYY-MM-DD), a list (any
 * of), { contains }, { not: value | list }, { empty: true | false }, { from, to }
 * (or min/max; dates or numbers, inclusive). Returns { test } or { error }.
 */
export function compileFilters(mod, filters) {
  if (filters === undefined || filters === null) return { test: () => true };
  if (typeof filters !== 'object' || Array.isArray(filters)) return { error: 'filters must be an object: column → condition' };
  const tests = [];
  for (const [name, condition] of Object.entries(filters)) {
    const column = columnOf(mod, name);
    if (column.error) return { error: `filters: ${column.error}` };
    const { key } = column;
    const field = mod.fields.find(f => f.key === key);
    const anyOf = list => {
      const ops = list.map(v => operand(field, v));
      return cell => ops.some(op => equals(cell, op));
    };
    if (Array.isArray(condition)) {
      const test = anyOf(condition);
      tests.push(values => test(values[key]));
      continue;
    }
    if (condition === null || typeof condition !== 'object') {
      const op = operand(field, condition);
      tests.push(values => equals(values[key], op));
      continue;
    }
    const unknown = Object.keys(condition).filter(k => !CONDITIONS.has(k));
    if (unknown.length) return { error: `${key}: unknown condition ${unknown.join(', ')} (use contains, not, empty, from, to, in)` };
    if ('in' in condition) {
      const test = anyOf(Array.isArray(condition.in) ? condition.in : [condition.in]);
      tests.push(values => test(values[key]));
    }
    if ('contains' in condition) {
      const part = textKey(condition.contains);
      tests.push(values => !empty(values[key]) && textKey(values[key]).includes(part));
    }
    if ('not' in condition) {
      const test = anyOf(Array.isArray(condition.not) ? condition.not : [condition.not]);
      tests.push(values => !test(values[key]));
    }
    if ('empty' in condition) tests.push(values => empty(values[key]) === Boolean(condition.empty));
    const low = bound(field, condition.from ?? condition.min);
    const high = bound(field, condition.to ?? condition.max);
    if ((condition.from ?? condition.min) !== undefined && low === null) return { error: `${key}: from must be a date (YYYY-MM-DD) or a number` };
    if ((condition.to ?? condition.max) !== undefined && high === null) return { error: `${key}: to must be a date (YYYY-MM-DD) or a number` };
    if (low !== null || high !== null)
      tests.push(values => {
        const cell = values[key];
        return typeof cell === 'number' && (low === null || cell >= low) && (high === null || cell <= high);
      });
  }
  return { test: values => tests.every(t => t(values)) };
}

// ------------------------------------------------------------------ places

const EARTH_KM = 6371.0088;
export function distanceKm(a, b) {
  const rad = d => (d * Math.PI) / 180;
  const dLat = rad(b.lat - a.lat),
    dLon = rad(b.lon - a.lon);
  const h = Math.sin(dLat / 2) ** 2 + Math.cos(rad(a.lat)) * Math.cos(rad(b.lat)) * Math.sin(dLon / 2) ** 2;
  return 2 * EARTH_KM * Math.asin(Math.min(1, Math.sqrt(h)));
}
const coordinate = (value, limit) => (typeof value === 'number' && Number.isFinite(value) && Math.abs(value) <= limit ? value : null);
const pointOf = (values, latKey, lonKey) => {
  const lat = coordinate(values[latKey], 90),
    lon = coordinate(values[lonKey], 180);
  return lat === null || lon === null ? null : { lat, lon };
};

/** Location_data: each Collection_location with its coordinates. */
function places(db) {
  const mod = moduleMap.get('Location_data');
  if (!mod) return new Map();
  const out = new Map();
  for (const r of sheetRows(db, mod)) {
    const name = r.values.Collection_location;
    const point = pointOf(r.values, 'Latitude', 'Longitude') ?? pointOf(r.values, 'DECIMAL_LATITUDE', 'DECIMAL_LONGITUDE');
    if (!empty(name) && point) out.set(textKey(name), { name: String(name), ...point });
  }
  return out;
}

/**
 * near = { location | lat + lon, km }: the centre and a function giving a row's
 * point (its DECIMAL_LATITUDE/LONGITUDE, else its Collection_location in
 * Location_data), or { error }.
 */
function compileNear(db, mod, near) {
  if (near === undefined || near === null) return {};
  if (typeof near !== 'object' || Array.isArray(near)) return { error: 'near must be { location or lat + lon, km }' };
  const km = Number(near.km);
  if (!Number.isFinite(km) || km <= 0 || km > 5000) return { error: 'near.km must be a distance in km (above 0)' };
  const known = places(db);
  let centre;
  if (!empty(near.location)) {
    const key = textKey(near.location);
    centre = known.get(key);
    if (!centre) {
      const hits = [...known.values()].filter(p => textKey(p.name).includes(key));
      if (hits.length === 1) centre = hits[0];
      else
        return {
          error: `No single Collection_location "${clip(near.location, 80)}" with coordinates in Location_data${
            hits.length ? `; did you mean: ${hits.slice(0, 8).map(p => p.name).join(', ')}` : ''
          }. Or give lat and lon.`,
        };
    }
  } else {
    const lat = coordinate(Number(near.lat), 90),
      lon = coordinate(Number(near.lon), 180);
    if (lat === null || lon === null) return { error: 'near needs location (a Collection_location) or lat and lon in decimal degrees' };
    centre = { name: null, lat, lon };
  }
  const has = key => mod.fields.some(f => f.key === key);
  const own = has('DECIMAL_LATITUDE') && has('DECIMAL_LONGITUDE');
  const plain = has('Latitude') && has('Longitude');
  const named = ['Collection_location', 'Location'].find(has);
  if (!own && !plain && !named)
    return { error: `${mod.id} has no coordinates or Collection_location; look for the rows in Collection_data` };
  const locate = values =>
    (own && pointOf(values, 'DECIMAL_LATITUDE', 'DECIMAL_LONGITUDE')) ||
    (plain && pointOf(values, 'Latitude', 'Longitude')) ||
    (named && !empty(values[named]) ? (known.get(textKey(values[named])) ?? null) : null);
  return { centre, km, locate };
}

// ------------------------------------------------------------------ the tools

/**
 * The rows matching a query: identifiers (field + values), filters and near.
 * Returns { mod, rows: [{ record, distance }], missing } or { error }.
 */
export function selectRecords(db, args) {
  const mod = moduleMap.get(String(args.module ?? args.sheet ?? ''));
  if (!mod) return { error: `Unknown sheet ${clip(args.module ?? args.sheet, 60)}` };
  const filter = compileFilters(mod, args.filters);
  if (filter.error) return filter;
  const near = compileNear(db, mod, args.near);
  if (near.error) return near;
  const hasValues = Array.isArray(args.values) && args.values.length;
  let rows, missing;
  if (hasValues || args.field) {
    const column = columnOf(mod, args.field ?? '');
    if (column.error) return { error: `field: ${column.error}` };
    if (!hasValues) return { error: 'Give at least one identifier in values (or leave field out and use filters)' };
    // One pass over the sheet's rows (pre-made rows included) for every identifier.
    const wanted = args.values.slice(0, 500).map(raw => String(raw).trim());
    const keys = new Set(wanted.map(idKey));
    const byValue = new Map();
    for (const r of cachedRows(db, mod)) {
      const key = idKey(r.values[column.key]);
      if (keys.has(key)) (byValue.get(key) ?? byValue.set(key, []).get(key)).push(r);
    }
    const seen = new Set();
    rows = [];
    missing = [];
    for (const value of wanted) {
      const hits = byValue.get(idKey(value)) ?? [];
      if (!hits.length) missing.push(value);
      for (const r of hits)
        if (!seen.has(r.id)) {
          seen.add(r.id);
          rows.push(r);
        }
    }
  } else if (args.filters || args.near) rows = sheetRows(db, mod);
  else return { error: 'Give field + values (identifiers), filters or near' };
  const out = [];
  let unplaced = 0;
  for (const record of rows) {
    if (!filter.test(record.values)) continue;
    if (near.locate) {
      const point = near.locate(record.values);
      if (!point) {
        unplaced++;
        continue;
      }
      const distance = distanceKm(near.centre, point);
      if (distance > near.km) continue;
      out.push({ record, distance: Math.round(distance * 100) / 100 });
    } else out.push({ record });
  }
  return {
    mod,
    rows: out,
    missing,
    ...(near.centre
      ? { near: { centre: near.centre, km: near.km, ...(unplaced ? { rowsWithoutPlace: unplaced } : {}) } }
      : {}),
  };
}

/**
 * The rows of one sheet a bulk change picks (propose_changes' `bulk`): `recordIds`
 * (app recordIds or the rows' IDs in the sheet, e.g. "W2B") and/or `filters` (as
 * find_records), all must hold. { mod, rows, missing } (ids not in the sheet) or
 * { error } (an ID of several rows among them).
 */
export function pickRows(db, { sheet, recordIds, filters }) {
  const mod = moduleMap.get(String(sheet ?? ''));
  if (!mod) return { error: `Unknown sheet ${clip(sheet, 60)}` };
  if (recordIds !== undefined && !Array.isArray(recordIds)) return { error: 'recordIds must be a list of row ids' };
  const ids = recordIds ? [...new Set(recordIds.map(id => String(id ?? '').trim()).filter(Boolean))] : [];
  const filtered = filters && typeof filters === 'object' && Object.keys(filters).length;
  if (!ids.length && !filtered) return { error: 'Give recordIds and/or filters' };
  const filter = compileFilters(mod, filters);
  if (filter.error) return filter;
  let rows;
  const missing = [];
  if (ids.length) {
    const resolved = resolveRows(db, ids, { sheet: mod.id });
    const ambiguous = resolved.problems.filter(p => p.kind === 'ambiguous');
    if (ambiguous.length) return { error: ambiguous.slice(0, 5).map(p => p.error).join(' ') };
    missing.push(...resolved.problems.map(p => ids[p.index]));
    const byId = new Map(cachedRows(db, mod).map(r => [r.id, r]));
    rows = [...new Set(resolved.ids.filter(Boolean))].map(id => byId.get(id)).filter(Boolean);
  } else rows = sheetRows(db, mod);
  return { mod, rows: rows.filter(r => filter.test(r.values)), missing };
}

/**
 * find_records: the rows within the size budget; only their ids (idsOnly), or
 * with `fields` as a table: `columns` (id, row, label, then the fields) and
 * `rows`, one list of values per row (null = empty cell), formula texts apart
 * by row id.
 */
export function findRecords(db, args, { budget = FIND_BUDGET } = {}) {
  const selected = selectRecords(db, args);
  if (selected.error) return selected;
  const { mod, rows, missing } = selected;
  const idsOnly = args.idsOnly === true;
  let columns = null;
  if (args.fields !== undefined && !idsOnly) {
    if (!Array.isArray(args.fields) || !args.fields.length) return { error: 'fields must be a list of columns' };
    const named = columnKeys(mod, args.fields);
    if (named.error) return { error: `fields: ${named.error}` };
    columns = [...new Set(named.keys)];
  }
  const fields = columns && new Set(columns);
  const placed = !!selected.near;
  const identifiers = Array.isArray(args.values) && args.values.length;
  const limit = Math.min(Math.max(Number(args.limit) || (idsOnly ? 300 : identifiers ? 150 : 50), 1), 500);
  const offset = Math.max(Number(args.offset) || 0, 0);
  const found = [];
  const formulas = {};
  const formulaColumns = new Set();
  let size = 0,
    cut = false;
  for (const item of rows.slice(offset, offset + limit)) {
    const { distance } = item;
    const record = idsOnly ? item.record : withFormulas(db, item.record);
    const near = placed ? { distanceKm: distance } : {};
    let row, extra;
    if (idsOnly) row = { id: record.id, row: record.row, label: record.label, ...near };
    else {
      const compact = compactRecord(record, { fields, allFormulas: args.formulas === true, formulaColumns: false, inSheet: true });
      if (!columns) row = { ...compact, ...near };
      else {
        row = [record.id, record.row, record.label, ...columns.map(k => compact.values[k] ?? null), ...(placed ? [distance] : [])];
        extra = compact.formulas;
      }
    }
    const length = JSON.stringify(row).length + 1 + (extra ? JSON.stringify(extra).length + record.id.length + 4 : 0);
    if (size + length > budget && found.length) {
      cut = true;
      break;
    }
    size += length;
    found.push(row);
    if (extra) formulas[record.id] = extra;
    for (const key of Object.keys(record.formulas ?? {})) if (!fields || fields.has(key)) formulaColumns.add(key);
  }
  const more = rows.length - offset - found.length;
  return {
    sheet: mod.id,
    total: rows.length,
    returned: found.length,
    ...(offset ? { offset } : {}),
    ...(columns
      ? { columns: ['id', 'row', 'label', ...columns, ...(placed ? ['distanceKm'] : [])], rows: found, ...(Object.keys(formulas).length ? { formulas } : {}) }
      : { found }),
    ...(missing ? { missing } : {}),
    ...(idsOnly ? {} : { formulaColumns: [...formulaColumns] }),
    ...(selected.near ? { near: selected.near } : {}),
    ...(more > 0
      ? {
          truncated: true,
          next: `${more} more row${more === 1 ? '' : 's'} not shown${cut ? ' (size limit)' : ''}: page with offset=${offset + found.length}, or narrow with filters, only the columns you need (fields) or only the ids (idsOnly); count_records for counts.`,
        }
      : {}),
  };
}

/** A value as a group name: dates as YYYY-MM-DD (or the year / month with "column:year", "column:month"). */
function groupValue(field, part, value) {
  if (empty(value)) return '(empty)';
  if (field.type === 'date' && typeof value === 'number') {
    const iso = isoDate(value);
    return part === 'year' ? iso.slice(0, 4) : part === 'month' ? iso.slice(0, 7) : iso;
  }
  return typeof value === 'string' ? value.trim() : value;
}

/** count_records: how many rows match, and per group (groupBy up to 3 columns). */
export function countRecords(db, args) {
  const selected = selectRecords(db, { ...args, module: args.sheet ?? args.module, field: undefined, values: undefined, filters: args.filters ?? {} });
  if (selected.error) return selected;
  const { mod, rows } = selected;
  const groupBy = args.groupBy === undefined || args.groupBy === null ? [] : Array.isArray(args.groupBy) ? args.groupBy : [args.groupBy];
  if (groupBy.length > 3) return { error: 'groupBy takes up to 3 columns' };
  const groups = [];
  for (const spec of groupBy) {
    const [name, part] = String(spec).split(':');
    const column = columnOf(mod, name);
    if (column.error) return { error: `groupBy: ${column.error}` };
    const { key } = column;
    const field = mod.fields.find(f => f.key === key);
    if (part && !(field.type === 'date' && ['year', 'month'].includes(part))) return { error: `${key}: only date columns take :year or :month` };
    groups.push({ key, part, field });
  }
  const out = { sheet: mod.id, total: rows.length, ...(selected.near ? { near: selected.near } : {}) };
  if (!groups.length) return out;
  const counts = new Map();
  for (const { record } of rows) {
    const values = groups.map(g => groupValue(g.field, g.part, record.values[g.key]));
    const id = JSON.stringify(values);
    const hit = counts.get(id) ?? { values, n: 0 };
    hit.n++;
    counts.set(id, hit);
  }
  const names = groups.map(g => (g.part ? `${g.key}:${g.part}` : g.key));
  const sorted = [...counts.values()].sort((a, b) => b.n - a.n || String(a.values).localeCompare(String(b.values)));
  const groupsOf = list => list.map(g => ({ ...Object.fromEntries(names.map((name, i) => [name, g.values[i]])), n: g.n }));
  const more = kept => (sorted.length > kept ? { truncated: true, next: `${sorted.length - kept} more groups not shown (the smallest): add filters` } : {});
  // Up to 300 groups, fewer when they are long texts (notes).
  const shown = Math.min(sorted.length, 300);
  return fitList({ ...out, groupBy: names, groups: groupsOf(sorted.slice(0, shown)), ...more(shown) }, 'groups', RESULT_BUDGET - 500, more).out;
}

export const FILTERS_DOC =
  'Column → condition, all must hold: a value (equal; text ignores case and accents; dates YYYY-MM-DD), a list (any of), {"contains": "text"}, {"not": value or list}, {"empty": true|false}, {"from", "to"} (dates or numbers, inclusive). Formula columns by their computed value. E.g. {"SPECIES": "Oleria onega", "Sex": "female"}';
const NEAR_DOC =
  'Rows within km of a place: {"location": a Collection_location of Location_data, e.g. "Ikiam"} or {"lat", "lon"}, plus "km". A row is placed by DECIMAL_LATITUDE/LONGITUDE, else its Collection_location; rows without a place are counted apart (rowsWithoutPlace).';

export const RECORD_TOOLS = [
  {
    type: 'function',
    function: {
      name: 'find_records',
      description:
        [
          'Rows of one sheet by identifiers (`field` + `values`; those not found come back in `missing`), `filters` and/or `near`.',
          '- Each row: id, row, values (non-empty cells; formulas computed; dates YYYY-MM-DD), `formulas`: the text of counts typed as sums (=16+2-1), of all with `formulas: true`.',
          '- `fields`: a table instead: `columns` (id, row, label, the fields) and `rows` (lists of values, null = empty).',
          '- `idsOnly`: id, row and label per row (300 by default). Counts: count_records.',
          '- A long answer ends with `truncated` and `next` (page with offset, or narrow it).',
        ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          module: { type: 'string', description: 'The sheet, e.g. Insectary_data' },
          field: { type: 'string', description: 'Column of the identifiers, e.g. Insectary_ID, CAM_ID, CLUTCH NUMBER' },
          values: { type: 'array', items: { type: 'string' }, description: 'Up to 500 identifiers' },
          filters: { type: 'object', description: FILTERS_DOC },
          near: {
            type: 'object',
            description: NEAR_DOC,
            properties: { location: { type: 'string' }, lat: { type: 'number' }, lon: { type: 'number' }, km: { type: 'number' } },
          },
          fields: { type: 'array', items: { type: 'string' } },
          idsOnly: { type: 'boolean' },
          formulas: { type: 'boolean' },
          limit: { type: 'integer', description: '1 to 500 (default 150 with values, else 50)' },
          offset: { type: 'integer' },
        },
        required: ['module'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'count_records',
      description: [
        'Count the rows of one sheet matching `filters` and/or `near` (as in find_records), in total and per group (`groupBy`: up to 3 columns; a date column as "Collection_date:year" or ":month"; empty cells as "(empty)"). Pre-made rows are not counted.',
        '- A text column as groupBy (e.g. Notes) gives each distinct text once with its count: a quick way to read and classify notes.',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          sheet: { type: 'string' },
          filters: { type: 'object', description: 'As in find_records' },
          near: { type: 'object', description: 'As in find_records' },
          groupBy: {
            description: 'e.g. "SPECIES" or ["SPECIES", "Sex"]',
            anyOf: [{ type: 'string' }, { type: 'array', items: { type: 'string' } }],
          },
        },
        required: ['sheet'],
      },
    },
  },
];
