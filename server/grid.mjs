// Read-side helpers for the spreadsheet grid: whole-sheet payloads and the
// next-identifier suggestions the original Shiny app offered.

import { moduleMap } from './schema.mjs';

const blank = value => value === null || value === undefined || /^\s*(|NA|N\/A)\s*$/i.test(String(value));
const fail = (code, message, status = 400) => Object.assign(new Error(message), { code, status });

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
  return {
    module,
    columns: mod.fields.map(({ column, ...field }) => field),
    headerProblems: store.headerProblems.get(module) || [],
    rows: rows.map(r => wireRow(keys, r)),
    latest: latestUpdate(store, module),
  };
}

/** One row as an array of values in column order, plus the indexes of formula cells. */
function wireRow(keys, r) {
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
      'SELECT row_num,observed,values_json FROM records WHERE sheet=? AND missing=0 AND row_num>0 ORDER BY row_num',
    )
    .all(sheet)
    .map(r => ({ row: r.row_num, observed: Boolean(r.observed), values: JSON.parse(r.values_json) }));
}

/**
 * Next identifiers, following the original Shiny app:
 *  - insectary: the pre-filled IDs of unused rows after the last recorded butterfly;
 *  - cam: the number after each consecutive run of CAM IDs, newest runs first;
 *  - tube: the number after each run of tube IDs, per prefix and preservation medium.
 * With `start`, returns `count` consecutive unused IDs beginning at `start`.
 */
export function idSuggestions(store, { kind, start, count } = {}) {
  const n = Math.min(Math.max(Number(count) || 20, 1), 500);
  if (kind === 'insectary') return insectaryIds(store, start, n);
  if (kind === 'cam') return start ? fromStart(start, n, usedCamIds(store)) : camSuggestions(store);
  if (kind === 'tube') return start ? fromStart(start, n, usedTubeIds(store)) : tubeSuggestions(store);
  throw fail('INVALID_KIND', 'kind debe ser insectary, cam o tube');
}

function insectaryIds(store, start, count) {
  const rows = rowsOf(store, 'Insectary_data');
  const lastObserved = rows.reduce((max, r) => (r.observed ? Math.max(max, r.row) : max), 0);
  let free = rows.filter(r => !r.observed && r.row > lastObserved && !blank(r.values.Insectary_ID));
  if (start) {
    const at = free.findIndex(r => r.values.Insectary_ID === start);
    if (at < 0) throw fail('ID_NOT_AVAILABLE', `${start} is not an unused pre-filled Insectary ID`, 409);
    free = free.slice(at);
  }
  const ids = free.slice(0, count).map(r => ({ value: r.values.Insectary_ID, row: r.row }));
  return {
    suggestions: ids.slice(0, 1).map(i => ({ value: i.value, label: `${i.value} (fila ${i.row})` })),
    sequence: ids.map(i => i.value),
    rows: ids,
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
    suggestions.push({
      value: run.value,
      medium,
      context,
      date: run.date,
      label: withDate(`${run.value} · ${medium} · ${context}`, run.date),
    });
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
