// Rows of the workbook as the assistant reads them (find_records, count_records,
// get_record, search_records): computed values of formula cells included, the
// formula text where it tells something (a count typed as =16+2-1), column
// filters, distance to a place, and answers kept under a size budget so a wide
// query says "narrow it" instead of overflowing.

import { moduleMap, parseDateText } from './schema.mjs';

const isoDate = serial => new Date(Date.UTC(1899, 11, 30) + serial * 864e5).toISOString().slice(0, 10);
const clip = (value, length) => String(value ?? '').slice(0, length);
/** A formula that is only arithmetic on typed numbers (a count kept as =16+2-1): its text is worth showing. */
const ARITHMETIC = /^=[\d\s+\-*/().]+$/;
/** Characters of rows one find_records answer holds (the MCP answer itself is cut at 200k). */
export const FIND_BUDGET = 60000;
const empty = value => value === null || value === undefined || value === '';
const textKey = value =>
  String(value ?? '')
    .toLowerCase()
    .normalize('NFD')
    .replace(/[̀-ͯ]/g, '')
    .replace(/\s+/g, ' ')
    .trim();

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

/**
 * The sheet's rows in use (observed: not the pre-made rows waiting to be
 * filled, as the app's counts), parsed. Their formulas are read only for the
 * rows returned: a Collection_data row holds some 6 kB of lookup formulas.
 */
function sheetRows(db, mod) {
  return db
    .prepare(
      'SELECT id, row_num, label, version, values_json FROM records WHERE sheet = ? AND missing = 0 AND observed = 1 AND row_num > ? ORDER BY row_num',
    )
    .all(mod.id, mod.headerRow)
    .map(r => ({ id: r.id, sheet: mod.id, row: r.row_num, label: r.label, version: r.version, values: JSON.parse(r.values_json) }));
}
const withFormulas = (db, record) =>
  record.formulas
    ? record
    : { ...record, formulas: JSON.parse(db.prepare('SELECT formulas_json FROM records WHERE id = ?').get(record.id)?.formulas_json || '{}') };

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
  for (const [key, condition] of Object.entries(filters)) {
    const field = mod.fields.find(f => f.key === key);
    if (!field) return { error: `Unknown column ${clip(key, 60)} in ${mod.id}; see describe_sheet` };
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
function select(db, args) {
  const mod = moduleMap.get(String(args.module ?? args.sheet ?? ''));
  if (!mod) return { error: `Unknown sheet ${clip(args.module ?? args.sheet, 60)}` };
  const filter = compileFilters(mod, args.filters);
  if (filter.error) return filter;
  const near = compileNear(db, mod, args.near);
  if (near.error) return near;
  const hasValues = Array.isArray(args.values) && args.values.length;
  let rows, missing;
  if (hasValues || args.field) {
    const field = String(args.field ?? '');
    if (!mod.fields.some(f => f.key === field)) return { error: `Unknown column ${clip(field, 60)}` };
    if (!hasValues) return { error: 'Give at least one identifier in values (or leave field out and use filters)' };
    const query = db.prepare(
      'SELECT id FROM records WHERE sheet = ? AND missing = 0 AND row_num > 0 AND lower(trim(CAST(json_extract(values_json, ?) AS TEXT))) = ?',
    );
    const ids = [];
    missing = [];
    for (const raw of args.values.slice(0, 500)) {
      const value = String(raw).trim();
      const hits = query.all(mod.id, `$."${field.replaceAll('"', '')}"`, value.toLowerCase());
      if (!hits.length) missing.push(value);
      for (const { id } of hits) if (!ids.includes(id)) ids.push(id);
    }
    const byId = db.prepare('SELECT id, row_num, label, version, values_json, formulas_json FROM records WHERE id = ?');
    rows = ids.map(id => {
      const r = byId.get(id);
      return {
        id: r.id,
        sheet: mod.id,
        row: r.row_num,
        label: r.label,
        version: r.version,
        values: JSON.parse(r.values_json),
        formulas: JSON.parse(r.formulas_json || '{}'),
      };
    });
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

/** find_records: the rows, only some columns if asked, within the size budget. */
export function findRecords(db, args, { budget = FIND_BUDGET } = {}) {
  const selected = select(db, args);
  if (selected.error) return selected;
  const { mod, rows, missing } = selected;
  let fields = null;
  if (args.fields !== undefined) {
    if (!Array.isArray(args.fields) || !args.fields.length) return { error: 'fields must be a list of columns' };
    const unknown = args.fields.filter(f => !mod.fields.some(x => x.key === f));
    if (unknown.length) return { error: `Unknown columns in fields: ${unknown.slice(0, 10).join(', ')}` };
    fields = new Set(args.fields);
  }
  const identifiers = Array.isArray(args.values) && args.values.length;
  const limit = Math.min(Math.max(Number(args.limit) || (identifiers ? 150 : 50), 1), 500);
  const offset = Math.max(Number(args.offset) || 0, 0);
  const found = [];
  const formulaColumns = new Set();
  let size = 0,
    cut = false;
  for (const item of rows.slice(offset, offset + limit)) {
    const { distance } = item;
    const record = withFormulas(db, item.record);
    const row = {
      ...compactRecord(record, { fields, allFormulas: args.formulas === true, formulaColumns: false, inSheet: true }),
      ...(distance !== undefined ? { distanceKm: distance } : {}),
    };
    const length = JSON.stringify(row).length + 1;
    if (size + length > budget && found.length) {
      cut = true;
      break;
    }
    size += length;
    found.push(row);
    for (const key of Object.keys(record.formulas ?? {})) if (!fields || fields.has(key)) formulaColumns.add(key);
  }
  const more = rows.length - offset - found.length;
  return {
    sheet: mod.id,
    total: rows.length,
    returned: found.length,
    ...(offset ? { offset } : {}),
    found,
    ...(missing ? { missing } : {}),
    formulaColumns: [...formulaColumns],
    ...(selected.near ? { near: selected.near } : {}),
    ...(more > 0
      ? {
          truncated: `Truncated: ${more} more row${more === 1 ? '' : 's'} not shown${cut ? ' (size limit)' : ''}. Narrow with filters, ask only the columns you need (fields), use count_records for counts, or page with offset=${offset + found.length}.`,
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
  const selected = select(db, { ...args, module: args.sheet ?? args.module, field: undefined, values: undefined, filters: args.filters ?? {} });
  if (selected.error) return selected;
  const { mod, rows } = selected;
  const groupBy = args.groupBy === undefined || args.groupBy === null ? [] : Array.isArray(args.groupBy) ? args.groupBy : [args.groupBy];
  if (groupBy.length > 3) return { error: 'groupBy takes up to 3 columns' };
  const groups = [];
  for (const spec of groupBy) {
    const [key, part] = String(spec).split(':');
    const field = mod.fields.find(f => f.key === key);
    if (!field) return { error: `Unknown column ${clip(key, 60)} in groupBy` };
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
  const shown = sorted.slice(0, 300);
  return {
    ...out,
    groupBy: names,
    groups: shown.map(g => ({ ...Object.fromEntries(names.map((name, i) => [name, g.values[i]])), n: g.n })),
    ...(sorted.length > shown.length ? { truncated: `${sorted.length - shown.length} more groups not shown; add filters` } : {}),
  };
}

const FILTERS_DOC =
  'Column → condition, all must hold: a value (equal; text ignores case and accents; dates YYYY-MM-DD), a list (any of), {"contains": "text"}, {"not": value or list}, {"empty": true|false}, {"from": …, "to": …} (dates or numbers, inclusive). Formula columns are filtered on their computed value. E.g. {"SPECIES": "Oleria onega", "Preservation_medium": "Flash frozen"}';
const NEAR_DOC =
  'Only rows within km of a place: {"location": a Collection_location of Location_data, e.g. "Ikiam"} or {"lat": -0.95, "lon": -77.87}, plus "km". A row is placed by its DECIMAL_LATITUDE/DECIMAL_LONGITUDE, else by its Collection_location in Location_data; rows without a place are left out and counted (rowsWithoutPlace).';

export const RECORD_TOOLS = [
  {
    type: 'function',
    function: {
      name: 'find_records',
      description:
        [
          'Rows of one sheet, by exact identifiers (`field` + `values`, e.g. the Insectary_IDs of a notebook page; identifiers not found come back in `missing`) and/or by column `filters` and distance to a place (`near`).',
          '- Each row: id, row, values = every non-empty cell (formula cells with their computed value; dates YYYY-MM-DD) and formulas = the formula text of counts typed as sums (=16+2-1), or of every formula cell with `formulas: true`. formulaColumns lists the formula columns.',
          '- Ask only the columns you need (`fields`) and page with limit/offset. A cut answer says "Truncated: N more rows": narrow the query.',
          '- "How many": `count_records`.',
        ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          module: { type: 'string', description: 'Sheet, e.g. Insectary_data, Collection_data, Insectary_stocks (sheet also accepted)' },
          field: { type: 'string', description: 'Column the identifiers are in, e.g. Insectary_ID, CAM_ID, CLUTCH NUMBER' },
          values: { type: 'array', items: { type: 'string' }, description: 'Identifiers to look up (up to 500)' },
          filters: { type: 'object', description: FILTERS_DOC },
          near: {
            type: 'object',
            description: NEAR_DOC,
            properties: { location: { type: 'string' }, lat: { type: 'number' }, lon: { type: 'number' }, km: { type: 'number' } },
          },
          fields: { type: 'array', items: { type: 'string' }, description: 'Only these columns in each row' },
          formulas: { type: 'boolean', description: 'Also the formula text of every formula cell returned' },
          limit: { type: 'integer', description: 'Rows to return, 1 to 500 (default 150 with values, 50 otherwise)' },
          offset: { type: 'integer', description: 'Rows to skip, to page through a long answer' },
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
        'Count the rows of one sheet matching `filters` and/or `near` (as in `find_records`), in total and per group (`groupBy`: up to 3 columns; a date column as "Collection_date:year" or ":month"). Empty cells group as "(empty)". Pre-made rows that only hold formulas are not counted.',
        '- A text column as groupBy (e.g. Notes) gives each distinct text once with its count: a quick way to read and classify notes.',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          sheet: { type: 'string', description: 'e.g. Collection_data' },
          filters: { type: 'object', description: FILTERS_DOC },
          near: {
            type: 'object',
            description: NEAR_DOC,
            properties: { location: { type: 'string' }, lat: { type: 'number' }, lon: { type: 'number' }, km: { type: 'number' } },
          },
          groupBy: {
            description: 'A column or a list of up to 3, e.g. "SPECIES" or ["SPECIES", "Sex"]',
            anyOf: [{ type: 'string' }, { type: 'array', items: { type: 'string' } }],
          },
        },
        required: ['sheet'],
      },
    },
  },
];
