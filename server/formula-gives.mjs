// What a row's formula cells will show once a proposal's values are written
// (the «Cambios propuestos» table shows them in their own colour, never
// written): each formula of the row evaluated with server/formula.mjs, its
// same-row references reading the proposed value where the proposal changes
// that cell, else the sheet's (a formula cell's own formula, evaluated in
// turn), and its lookups the other sheets as the app's copy has them.

import { moduleMap } from './schema.mjs';
import { evaluateFormula, lookupKey, parseFormula, relativeFormula, shownResult, Unsupported } from './formula.mjs';

/**
 * A reader for a store: `rowGives({ sheet, record, values })` → { gives, fallback }
 * for one row, `record` its sheet row ({ row, values, formulas }; a new row: the
 * pre-made row it goes into), `values` what the proposal writes there. `gives`:
 * formula column → the value it will give, for the formulas the proposal's
 * values reach (all of them in a new row); `fallback`: those it reaches that
 * cannot be evaluated (the sheet's value stays shown).
 */
export function createFormulaReader(store) {
  const indexes = new Map();
  /** Field ↔ column of a sheet, by the header read last (the profile's before any sync). */
  function columnsOf(sheet) {
    const layout = store.layouts?.get(sheet);
    const cached = indexes.get(`cols\u0000${sheet}`);
    if (cached && cached.layout === layout) return cached;
    const mod = moduleMap.get(sheet);
    if (!mod) return null;
    const byIndex = new Map();
    const byField = new Map();
    for (const f of mod.fields) {
      if (f.readonly) continue;
      const at = layout?.columns?.get(f.key) ?? (layout ? undefined : f.column);
      if (at === undefined || byIndex.has(at)) continue;
      byIndex.set(at, f.key);
      byField.set(f.key, at);
    }
    const out = { layout, byIndex, byField, headerRow: mod.headerRow };
    indexes.set(`cols\u0000${sheet}`, out);
    return out;
  }
  const stampOf = sheet =>
    store.db
      .prepare('SELECT (SELECT count(*) FROM records WHERE sheet = ? AND missing = 0) n, (SELECT max(updated_at) FROM records WHERE sheet = ?) u')
      .get(sheet, sheet);

  /** One proposal's reading: the lookups' indexes checked once against the sheets. */
  function session() {
    const checked = new Set();
    const fieldAt = (sheet, col) => {
      const cols = columnsOf(sheet);
      if (!cols) throw new Unsupported(`sheet ${sheet}`);
      const field = cols.byIndex.get(col);
      if (field === undefined) throw new Unsupported(`column ${col + 1} of ${sheet}`);
      return { field, cols };
    };
    /** The rows of a column by lookup key, in order (a sheet's header row first). */
    function index(sheet, col) {
      const { field, cols } = fieldAt(sheet, col);
      const key = `idx\u0000${sheet}\u0000${field}`;
      let hit = indexes.get(key);
      if (!checked.has(key)) {
        const stamp = stampOf(sheet);
        const s = `${stamp.n}:${stamp.u}:${cols.layout ? 'l' : ''}`;
        if (hit?.stamp !== s) {
          const map = new Map([[lookupKey(field), [cols.headerRow]]]);
          for (const r of store.db
            .prepare(
              `SELECT row_num, json_extract(values_json, '$."${field.replaceAll('"', '')}"') v FROM records WHERE sheet = ? AND missing = 0 AND row_num > ? AND row_num < 2000000000 ORDER BY row_num`,
            )
            .all(sheet, cols.headerRow)) {
            const k = lookupKey(r.v);
            if (!k) continue;
            const list = map.get(k);
            if (list) list.push(r.row_num);
            else map.set(k, [r.row_num]);
          }
          hit = { stamp: s, map };
          indexes.set(key, hit);
        }
        checked.add(key);
      }
      return hit.map;
    }
    const inRange = (rows, r1, r2) => (rows ?? []).filter(r => r >= r1 && r <= r2);
    const other = (sheet, col, row) => {
      const { field, cols } = fieldAt(sheet, col);
      if (row === cols.headerRow) return field;
      if (row < cols.headerRow) return null;
      return store.getRecordBySheetRow(sheet, row)?.values?.[field] ?? null;
    };
    return {
      other,
      find: (sheet, col, value, r1, r2) => (lookupKey(value) ? (inRange(index(sheet, col).get(lookupKey(value)), r1, r2)[0] ?? null) : null),
      count: (sheet, col, value, r1, r2) => (lookupKey(value) ? inRange(index(sheet, col).get(lookupKey(value)), r1, r2).length : 0),
    };
  }

  /** The own-row columns a formula reads (same row, this sheet), or null when it cannot be read. */
  const deps = new Map();
  function readsOf(sheet, formula, row) {
    const template = relativeFormula(formula, row);
    const key = `${sheet}\u0000${template}`;
    if (deps.has(key)) return deps.get(key);
    let out = null;
    try {
      const cols = columnsOf(sheet);
      const found = new Set();
      const walk = node => {
        if (!node || typeof node !== 'object') return;
        if (node.t === 'ref' && !node.sheet && node.row && ('rel' in node.row ? node.row.rel === 0 : node.row.abs === row))
          found.add(cols?.byIndex.get(node.col) ?? `#${node.col}`);
        if (node.t === 'range' && !node.sheet) for (let c = node.c1; c <= node.c2; c++) found.add(cols?.byIndex.get(c) ?? `#${c}`);
        for (const child of [node.a, node.b, ...(node.args ?? [])]) walk(child);
      };
      walk(parseFormula(template));
      out = [...found];
    } catch {
      out = null;
    }
    if (deps.size > 5000) deps.clear();
    deps.set(key, out);
    return out;
  }

  function rowGives({ sheet, record, values, all = false, force = [], ctx = session() }) {
    const gives = {};
    const fallback = [];
    if (!record) return { gives, fallback };
    const formulas = Object.fromEntries(Object.entries(record.formulas ?? {}).filter(([f]) => !(f in values)));
    // `force`: formulas the proposal writes: evaluated, and those reading them too.
    const changed = new Set([...Object.keys(values), ...force]);
    // Whether a formula column depends on what the proposal writes (through the row's other formulas too).
    const reached = new Map();
    const reaches = (field, seen = new Set()) => {
      if (reached.has(field)) return reached.get(field);
      if (seen.has(field)) return false;
      seen.add(field);
      const reads = readsOf(sheet, formulas[field], record.row);
      const out = reads === null || reads.some(f => changed.has(f) || (formulas[f] && reaches(f, seen)));
      reached.set(field, out);
      return out;
    };
    const cols = columnsOf(sheet);
    const memo = new Map();
    const evaluating = new Set();
    const own = field => {
      if (field in values) return plain(values[field]);
      if (!formulas[field]) return record.values?.[field] ?? null;
      if (memo.has(field)) return memo.get(field);
      if (evaluating.has(field)) throw new Unsupported(`circular reference in ${field}`);
      evaluating.add(field);
      try {
        const v = evaluateFormula(formulas[field], record.row, cellReader);
        memo.set(field, v);
        return v;
      } finally {
        evaluating.delete(field);
      }
    };
    const cellReader = {
      cell: (other, col, row) => {
        if ((other === null || other === sheet) && row === record.row) {
          const field = cols?.byIndex.get(col);
          if (field === undefined) throw new Unsupported(`column ${col + 1}`);
          return own(field);
        }
        return ctx.other(other ?? sheet, col, row);
      },
      find: (other, col, value, r1, r2) => ctx.find(other ?? sheet, col, value, r1, r2),
      count: (other, col, value, r1, r2) => ctx.count(other ?? sheet, col, value, r1, r2),
    };
    for (const field of Object.keys(record.formulas ?? {})) {
      // A formula typed over by the proposal (SPECIES): what it would have given, to tell them apart.
      const typed = field in values;
      if (!typed && !all && !force.includes(field) && !reaches(field)) continue;
      try {
        gives[field] = shownResult(typed ? evaluateFormula(record.formulas[field], record.row, cellReader) : own(field));
      } catch (e) {
        if (!(e instanceof Unsupported)) throw e;
        fallback.push(field);
      }
    }
    return { gives, fallback };
  }

  return { rowGives, session };
}

/**
 * Formulas worked out over rows already read (server/checks.mjs sheetRows():
 * Map sheet → [{ row, values }]), each sheet's columns by `columnsOf(sheet)`
 * (Map field → index) and `headerRow(sheet)`. `gives(formula, sheet, row)` →
 * what the cell would show (an error as its code, blank as null), or undefined
 * when it is not worked out here. `overlay` (Map "sheet\0row\0column" → value)
 * is read first: what formulas not written yet would give, so a formula reading
 * the row above (Data_entry_order) sees it.
 */
export function rowsReader(sheets, { columnsOf, headerRow }) {
  const overlay = new Map();
  const fields = new Map();
  const byRow = new Map();
  const indexes = new Map();
  const fieldsOf = sheet => {
    if (!fields.has(sheet)) {
      const out = [];
      for (const [field, column] of columnsOf(sheet) ?? []) out[column] ??= field;
      fields.set(sheet, out);
    }
    return fields.get(sheet);
  };
  const rowsOf = sheet => {
    if (!byRow.has(sheet)) byRow.set(sheet, new Map((sheets.get(sheet) ?? []).map(r => [r.row, r])));
    return byRow.get(sheet);
  };
  function cell(sheet, col, row) {
    const key = `${sheet}\u0000${row}\u0000${col}`;
    if (overlay.has(key)) return overlay.get(key);
    if (!sheets.has(sheet)) throw new Unsupported(`sheet ${sheet}`);
    const field = fieldsOf(sheet)[col];
    if (field === undefined) throw new Unsupported(`column ${col + 1} of ${sheet}`);
    if (row === headerRow(sheet)) return field;
    const value = rowsOf(sheet).get(row)?.values[field];
    return value === undefined || value === '' ? null : value;
  }
  /** The rows of a column by lookup key, in order (the header row too). */
  function index(sheet, col) {
    const key = `${sheet}\u0000${col}`;
    if (!indexes.has(key)) {
      const map = new Map();
      const add = (k, row) => k && (map.get(k) ?? map.set(k, []).get(k)).push(row);
      add(lookupKey(cell(sheet, col, headerRow(sheet))), headerRow(sheet));
      for (const r of sheets.get(sheet) ?? []) if (r.row !== headerRow(sheet)) add(lookupKey(cell(sheet, col, r.row)), r.row);
      for (const list of map.values()) list.sort((a, b) => a - b);
      indexes.set(key, map);
    }
    return indexes.get(key);
  }
  const within = (sheet, col, value, r1, r2) => (index(sheet, col).get(lookupKey(value)) ?? []).filter(r => r >= r1 && r <= r2);
  const contextOf = own => ({
    cell: (sheet, col, row) => cell(sheet ?? own, col, row),
    find: (sheet, col, value, r1, r2) => (lookupKey(value) ? (within(sheet ?? own, col, value, r1, r2)[0] ?? null) : null),
    count: (sheet, col, value, r1, r2) => (lookupKey(value) ? within(sheet ?? own, col, value, r1, r2).length : 0),
  });
  return {
    overlay,
    cell,
    gives(formula, sheet, row) {
      try {
        return shownResult(evaluateFormula(formula, row, contextOf(sheet)));
      } catch (e) {
        if (e instanceof Unsupported) return undefined;
        throw e;
      }
    },
  };
}

/** A value of the proposal as the sheet will hold it (a count kept as a sum: its total is not needed here). */
const plain = v => (v && typeof v === 'object' ? null : v === '' ? null : v);

/** A formula's error as a cell shows it (#N/A, #REF!…). */
export const isFormulaError = v => typeof v === 'string' && /^#(N\/A|REF!|VALUE!|DIV\/0!|NAME\?|NUM!|NULL!|ERROR!)$/.test(v);

/** Whether a formula's result is what the cell shows already (blank and empty text alike, 5 and "5" alike). */
export function sameResult(a, b) {
  const norm = v => (v === null || v === undefined || v === '' ? '' : typeof v === 'number' ? String(v) : String(v).trim());
  return norm(a) === norm(b);
}
