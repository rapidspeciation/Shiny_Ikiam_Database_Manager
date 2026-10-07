// Pre-made rows: empty rows at the end of a sheet that already hold its
// formulas, formats and dropdowns. The team makes them in Google Sheets by
// dragging the last one down. When they run out the app makes more the same
// way, with a copyPaste of the last pre-made row, so a new row is never a bare
// row without formulas.
// In Insectary_data the pre-made rows are the rows with an Insectary ID: its
// other formulas are filled down thousands of rows further, so more pre-made
// rows means the ID formula going on into the rows below the last ID. Rows are
// added only when none are left, and the app's account cannot add them there
// (protected columns): PAS does.

import { randomUUID } from 'node:crypto';
import { labelFor, moduleMap } from './schema.mjs';
import { columnLetter, describeProblems, headerLayout, headerText } from './columns.mjs';
import { protectionRefused, rowKey, rowValues, shiftFormula } from './sheets.mjs';
import { msg, msgError } from './messages.mjs';

export const MAX_EXTEND = 500;
/** Rows made at once when a save needs a row past the pre-made ones. */
export const AUTO_BLOCK = 20;
/** How far above the last formula row a column's formula is looked for. */
const LOOKBACK = 300;
/** …and for a formula typed over in the last pre-made row. */
const TYPED_OVER_LOOKBACK = 50;

const fail = (code, message, status = 400, details) => msgError(message, { code, status, details });
const kind = cell => {
  const value = cell?.userEnteredValue;
  return !value ? '.' : 'formulaValue' in value ? 'F' : 'c';
};
const text = cell => {
  const v = cell?.effectiveValue;
  return String(v?.stringValue ?? v?.numberValue ?? '').trim();
};
const range = (sheetId, r0, r1, c0, c1) => ({
  sheetId,
  startRowIndex: r0 - 1,
  endRowIndex: r1,
  startColumnIndex: c0,
  endColumnIndex: c1,
});

/**
 * The protected ranges of a sheet (as Google lists them) the app's account cannot
 * edit: Google leaves requestingUserCanEdit out when it is false; ranges that only
 * warn are editable.
 */
export const lockedRanges = ranges => (ranges || []).filter(p => p.requestingUserCanEdit !== true && !p.warningOnly);

/**
 * Whether column `column` (0-based) is locked for the app's account in sheet rows
 * row0–row1 (1-based): a locked range (lockedRanges) reaches them, and none of its
 * unprotected ranges leaves them all out.
 */
export function columnLocked(locked, column, row0, row1 = row0) {
  const covers = (r, a, b) =>
    (r.startRowIndex ?? 0) < b &&
    a - 1 < (r.endRowIndex ?? Infinity) &&
    (r.startColumnIndex ?? 0) <= column &&
    column < (r.endColumnIndex ?? Infinity);
  return locked.some(
    p =>
      covers(p.range ?? {}, row0, row1) &&
      !(p.unprotectedRanges || []).some(u => (u.startRowIndex ?? 0) <= row0 - 1 && row1 <= (u.endRowIndex ?? Infinity) && covers(u, row0, row1)),
  );
}

const PROTECTED_KEY = sheet => `protected:${sheet}`;
/**
 * Keeps the locked ranges of `sheet` (from Google's metadata, read with each sync and
 * before a save makes new rows) in the app's settings, where the proposal tables
 * (in the assistant's workers too) read them: protectedFields.
 */
export function noteProtection(db, sheet, ranges) {
  const lean = lockedRanges(ranges).map(p => ({
    range: p.range ?? {},
    ...(p.unprotectedRanges?.length ? { unprotectedRanges: p.unprotectedRanges } : {}),
  }));
  const value = JSON.stringify(lean);
  const was = db.prepare('SELECT value FROM settings WHERE key = ?').get(PROTECTED_KEY(sheet))?.value;
  if (was !== value)
    db.prepare('INSERT INTO settings(key,value) VALUES(?,?) ON CONFLICT(key) DO UPDATE SET value=excluded.value').run(PROTECTED_KEY(sheet), value);
}

/**
 * The fields of `sheet` the app's account cannot write at sheet row `row` (`columns`:
 * the sheet's header, field → column), as last noted (noteProtection); none when not
 * known yet.
 */
export function protectedFields(db, sheet, row, columns) {
  let locked;
  try {
    locked = JSON.parse(db.prepare('SELECT value FROM settings WHERE key = ?').get(PROTECTED_KEY(sheet))?.value ?? '[]');
  } catch {
    return new Set();
  }
  if (!locked.length || !columns) return new Set();
  const out = new Set();
  for (const [field, column] of columns) if (columnLocked(locked, column, row)) out.add(field);
  return out;
}

/** Where the sheet's rows end in the app's copy: last row with formulas, last used row, last row. */
function tail(store, sheet) {
  const q = where =>
    store.db
      .prepare(`SELECT max(row_num) n FROM records WHERE sheet=? AND missing=0 AND row_num<2000000000 ${where}`)
      .get(sheet).n || 0;
  return { formulaRow: q("AND formulas_json<>'{}'"), observedRow: q('AND observed=1'), lastRow: q('') };
}

/** The last Insectary_data row with an Insectary ID in the app's copy: the end of its pre-made rows. */
function lastIdRow(store) {
  return (
    store.db
      .prepare(
        "SELECT max(row_num) n FROM records WHERE sheet='Insectary_data' AND missing=0 AND row_num<2000000000 AND trim(coalesce(json_extract(values_json,'$.Insectary_ID'),''))<>''",
      )
      .get().n || 0
  );
}

/** Refused for Google's protection: the rows cannot be added by the app, nothing was written. */
function noRowsLeft(left, count) {
  return left > 0
    ? fail(
        'NO_ROWS_LEFT',
        msg('Quedan {n} filas al final de Insectary_data y hacen falta {count}: pide a PAS que añada filas', { n: left, count }),
        409,
      )
    : fail('NO_ROWS_LEFT', 'No quedan filas con fórmulas al final de Insectary_data: pide a PAS que añada filas', 409);
}

/**
 * Fields that are formulas in the row a new record will take: the next unused
 * pre-made row, or, when they have run out, the last row holding formulas (the
 * rows the save makes are copies of it). A new row never types over them.
 */
export function newRowFormulaFields(store, sheet) {
  const { formulaRow, observedRow } = tail(store, sheet);
  const pick = sql => store.db.prepare(sql).get(sheet, observedRow || moduleMap.get(sheet)?.headerRow || 1);
  const next =
    pick(
      'SELECT formulas_json f FROM records WHERE sheet=? AND missing=0 AND observed=0 AND row_num>? ORDER BY row_num LIMIT 1',
    ) ??
    (formulaRow && formulaRow >= observedRow
      ? store.db
          .prepare('SELECT formulas_json f FROM records WHERE sheet=? AND missing=0 AND row_num=?')
          .get(sheet, formulaRow)
      : null);
  return new Set(Object.keys(JSON.parse(next?.f || '{}') || {}));
}

/**
 * Before a save writes new rows at the end of sheets (`needs`: [{ sheet, lastRow }]),
 * makes pre-made rows where the save would go past them (in blocks of at most
 * MAX_EXTEND): in Insectary_data the next IDs up to the row the save needs, in
 * other sheets a block of AUTO_BLOCK rows at least. Only sheets kept with pre-made
 * rows (formulas at or after the last used row) get them; a sheet typed without
 * formulas keeps getting plain rows. Runs inside the write queue.
 */
export async function ensurePremadeRows(store, needs) {
  for (const { sheet, lastRow } of needs) {
    const byId = sheet === 'Insectary_data';
    for (;;) {
      const { formulaRow, observedRow } = tail(store, sheet);
      const premadeEnd = byId ? lastIdRow(store) : formulaRow;
      if (!formulaRow || !premadeEnd || (!byId && formulaRow < observedRow) || lastRow <= premadeEnd) break;
      const count = byId ? lastRow - premadeEnd : Math.max(AUTO_BLOCK, lastRow - formulaRow);
      try {
        await extendRows(store, sheet, Math.min(MAX_EXTEND, count), { actor: 'auto' });
      } catch (e) {
        if (e.code === 'NO_ROWS_LEFT') throw e;
        throw fail(
          'PREMADE_FAILED',
          `No se pudieron preparar filas nuevas en ${sheet} (con sus fórmulas y listas); no se guardó nada: ${e.message}`,
          e.status && e.status < 500 ? 409 : 503,
        );
      }
      if ((byId ? lastIdRow(store) : tail(store, sheet).formulaRow) <= premadeEnd) break;
    }
  }
}

/**
 * Where a new Insectary_data row named by its Insectary ID will go: the free
 * pre-made row whose ID formula gives it (`{ row }`), or, past the pre-made rows,
 * the row the formula will give it once they are made (`{ row, ahead: true }`):
 * the series goes on one ID per row from the last row with an ID, to the end of
 * its round (Z9). Null when the ID is used, held by more than one empty row, or
 * not in the series ahead.
 */
export function insectaryIdRow(store, id) {
  id = String(id ?? '').trim().toUpperCase();
  const rows = store.db
    .prepare(
      "SELECT row_num r, observed FROM records WHERE sheet='Insectary_data' AND missing=0 AND row_num<2000000000 AND upper(trim(json_extract(values_json,'$.Insectary_ID')))=?",
    )
    .all(id);
  if (rows.length) return rows.length === 1 && !rows[0].observed ? { row: rows[0].r } : null;
  const last = store.db
    .prepare(
      "SELECT row_num r, json_extract(values_json,'$.Insectary_ID') id FROM records WHERE sheet='Insectary_data' AND missing=0 AND row_num<2000000000 AND trim(coalesce(json_extract(values_json,'$.Insectary_ID'),''))<>'' ORDER BY row_num DESC LIMIT 1",
    )
    .get();
  let next = String(last?.id ?? '').trim().toUpperCase();
  next = suffixedId(next)?.base ?? next;
  for (let step = 1; (next = nextInSeries(next)); step++) if (next === id) return { row: last.r + step, ahead: true };
  return null;
}

/**
 * Where rows named by these Insectary IDs stand or will stand in Insectary_data,
 * to put a proposal's rows in the sheet's order (one read of the sheet for them
 * all): ID → { row } for the row holding it (the first, if several), { row,
 * ahead: true } past the pre-made rows (as insectaryIdRow), { row, below: true }
 * for a suffixed ID (W2B.2: its row goes below that row, the last of its ID's
 * group). IDs without a place are left out.
 */
export function insectaryIdPlaces(store, ids) {
  const wanted = [...new Set(ids.map(id => String(id ?? '').trim().toUpperCase()).filter(Boolean))];
  const out = new Map();
  if (!wanted.length) return out;
  const rows = store.db
    .prepare(
      "SELECT row_num r, upper(trim(json_extract(values_json,'$.Insectary_ID'))) v FROM records WHERE sheet='Insectary_data' AND missing=0 AND row_num>0 AND row_num<2000000000 AND trim(coalesce(json_extract(values_json,'$.Insectary_ID'),''))<>'' ORDER BY row_num",
    )
    .all();
  const first = new Map();
  const lastOfBase = new Map();
  for (const { r, v } of rows) {
    if (!first.has(v)) first.set(v, r);
    lastOfBase.set(suffixedId(v)?.base ?? v, r);
  }
  const last = rows.at(-1);
  let ahead = null;
  for (const id of wanted) {
    const suffixed = suffixedId(id);
    if (first.has(id)) out.set(id, { row: first.get(id) });
    else if (suffixed) {
      if (lastOfBase.has(suffixed.base)) out.set(id, { row: lastOfBase.get(suffixed.base), below: true });
    } else if (last) {
      if (!ahead) {
        ahead = new Map();
        let next = suffixedId(last.v)?.base ?? last.v;
        for (let step = 1; (next = nextInSeries(next)); step++) ahead.set(next, last.r + step);
      }
      if (ahead.has(id)) out.set(id, { row: ahead.get(id), ahead: true });
    }
  }
  return out;
}

/**
 * The rows of these base Insectary IDs (A0E for A0E.1): base → { row, empty }, the first
 * row holding it and whether it is still an empty pre-made row. One read for them all.
 */
export function insectaryBaseRows(store, bases) {
  const wanted = [...new Set(bases.map(id => String(id ?? '').trim().toUpperCase()).filter(Boolean))];
  const out = new Map();
  if (!wanted.length) return out;
  const rows = store.db
    .prepare(
      `SELECT row_num r, observed, upper(trim(json_extract(values_json,'$.Insectary_ID'))) v FROM records WHERE sheet='Insectary_data' AND missing=0 AND row_num>0 AND row_num<2000000000 AND upper(trim(json_extract(values_json,'$.Insectary_ID'))) IN (${wanted.map(() => '?').join(',')}) ORDER BY row_num`,
    )
    .all(...wanted);
  for (const { r, observed, v } of rows) if (!out.has(v)) out.set(v, { row: r, empty: !observed });
  return out;
}

/** `W2B.2` → { base: 'W2B', n: 2 }: an Insectary ID written on two butterflies, told apart by a suffix. Null otherwise. */
export function suffixedId(id) {
  const m = /^([A-Z0-9]+)\.([1-9]\d*)$/.exec(String(id ?? '').trim().toUpperCase());
  return m ? { id: `${m[1]}.${m[2]}`, base: m[1], n: Number(m[2]) } : null;
}

/**
 * Where a new Insectary_data row with a suffixed ID (`W2B.2`: the same ID written on
 * a second butterfly) goes, as the curators do it: a row inserted directly below the
 * last row of that ID's group (`W2B`, `W2B.1`, `W2B.2`…), never a pre-made row at
 * the end (a typed ID there would break the ID formula of every pre-made row after
 * it). Returns null when `id` has no suffix; { problem, … } when it cannot go
 * (`used`, `no_base`, `empty_base`, `repeated` with `rows`); else
 * { id, base, anchor: { id: record id, row, value: its Insectary ID } }.
 */
export function duplicateIdRow(store, id) {
  const parsed = suffixedId(id);
  if (!parsed) return null;
  const { base } = parsed;
  const group = store.db
    .prepare(
      "SELECT id, row_num r, observed, upper(trim(json_extract(values_json,'$.Insectary_ID'))) v FROM records WHERE sheet='Insectary_data' AND missing=0 AND row_num>0 AND row_num<2000000000 AND (upper(trim(json_extract(values_json,'$.Insectary_ID')))=? OR upper(trim(json_extract(values_json,'$.Insectary_ID'))) LIKE ?) ORDER BY row_num",
    )
    .all(base, `${base}.%`)
    .filter(r => r.v === base || suffixedId(r.v)?.base === base);
  if (group.some(r => r.v === parsed.id)) return { ...parsed, problem: 'used', rows: group.filter(r => r.v === parsed.id).map(r => r.r) };
  if (!group.length) return { ...parsed, problem: 'no_base' };
  // An ID of the group held by two rows: which one the new row follows is unclear.
  const repeated = group.find((r, i) => group.findIndex(o => o.v === r.v) !== i);
  if (repeated) return { ...parsed, problem: 'repeated', value: repeated.v, rows: group.filter(r => r.v === repeated.v).map(r => r.r) };
  // An empty pre-made row: the butterfly goes into it, no suffix needed.
  if (!group.some(r => r.observed)) return { ...parsed, problem: 'empty_base', rows: group.map(r => r.r) };
  const last = group.at(-1);
  return { ...parsed, anchor: { id: last.id, row: last.r, value: last.v } };
}

/** The endpoint's entry: `count` more pre-made rows in `sheet`, in the write queue. */
export function extendPremadeRows(store, sheet, count, user) {
  return store.runExclusive(() => extendRows(store, sheet, count, { actor: user?.id || user?.username || 'app' }));
}

/**
 * Makes `count` more pre-made rows. In most sheets they are appended after the
 * last row holding formulas (T):
 *  1. appendDimension when the grid is too short;
 *  2. copyPaste of row T over the new rows (formulas, whose relative references
 *     move with the row, number formats and data validation);
 *  3. the constants row T held (when it is already used) are cleared;
 *  4. a column whose fill-down stopped before T is continued from its last
 *     formula, over the empty pre-made rows too; a formula typed over in a used
 *     row T is taken from the rows just above.
 * In Insectary_data they are the `count` rows after the last row with an
 * Insectary ID, rows that mostly exist already with the other formulas: each gets
 * its ID (the formula of the ID above, moved to it; a new round's first ID typed),
 * and only rows without formulas get the copy of row T (2–3). Rows are appended
 * only past the grid's last row; when Google refuses them (protected columns),
 * nothing is written and PAS is asked to add rows (NO_ROWS_LEFT).
 * Protected ranges the credential cannot edit are left out, and reported so the
 * owner completes them. The rows are then read back and checked.
 * Must run inside the store's write queue.
 */
export async function extendRows(store, sheet, count, { actor = 'app' } = {}) {
  const mod = moduleMap.get(sheet);
  if (!mod) throw fail('MODULE_NOT_FOUND', 'Hoja desconocida', 404);
  count = Number(count);
  if (!Number.isInteger(count) || count < 1 || count > MAX_EXTEND)
    throw fail('INVALID_COUNT', msg('Indica entre 1 y {n} filas', { n: MAX_EXTEND }));
  const known = tail(store, sheet);
  if (!known.formulaRow) throw fail('NO_TEMPLATE', msg('{sheet} no tiene filas con fórmulas que copiar', { sheet }), 409);
  const byId = sheet === 'Insectary_data';
  const idRow = byId ? lastIdRow(store) : 0;
  if (byId && !idRow) throw fail('ID_SERIES', 'Insectary_data no tiene Insectary IDs que continuar', 409);
  const info = await store.sheets.sheetInfo(sheet);
  const first = Math.max(mod.headerRow + 1, (byId ? idRow : known.formulaRow) - LOOKBACK);
  const last = Math.min(info.rowCount, (byId ? idRow : Math.max(known.formulaRow, known.lastRow)) + count + 5);
  const read = await store.sheets.readRows([
    { sheet, rows: [mod.headerRow, ...Array.from({ length: Math.max(0, last - first + 1) }, (_, i) => first + i)] },
  ]);
  const layout = headerLayout(sheet, read.get(rowKey(sheet, mod.headerRow)));
  if (layout.blocked)
    throw fail(
      'HEADER_MISMATCH',
      `No se crean filas en ${sheet}: ${describeProblems(sheet, layout.problems.filter(p => p.blocking))}`,
      409,
    );
  const cellsAt = row => read.get(rowKey(sheet, row))?.cells || [];
  const kindAt = (row, column) => kind(cellsAt(row)[column]);
  const idColumn = byId ? layout.columns.get('Insectary_ID') : undefined;

  // The live end of the sheet: last formula row, last used row, last row with anything, last ID.
  let template = 0,
    lastUsed = 0,
    lastContent = 0,
    liveIdRow = 0,
    width = info.columnCount;
  for (let row = first; row <= last; row++) {
    const kinds = cellsAt(row).map(kind);
    width = Math.max(width, kinds.length);
    if (kinds.includes('F')) template = row;
    if (kinds.includes('c')) lastUsed = row;
    if (kinds.some(k => k !== '.')) lastContent = row;
    if (byId && text(cellsAt(row)[idColumn])) liveIdRow = row;
  }
  if (!template) throw fail('NO_TEMPLATE', msg('{sheet} no tiene filas con fórmulas que copiar', { sheet }), 409);
  if (byId && liveIdRow !== idRow)
    throw fail(
      'SHEET_CHANGED',
      msg('El último Insectary ID está ahora en la fila {row} de Google Sheets; vuelve a intentarlo en un minuto', {
        row: liveIdRow,
      }),
      409,
    );
  const start = (byId ? idRow : template) + 1;
  const end = start + count - 1;
  if (byId) {
    // A row below the last ID with something typed in it was used without an ID: it does not get one unseen.
    const typed = [];
    for (let row = start; row <= Math.min(end, last); row++) if (cellsAt(row).some(c => kind(c) === 'c')) typed.push(row);
    if (typed.length)
      throw fail(
        'BARE_ROWS',
        msg(
          'En Insectary_data hay filas escritas sin Insectary ID después del último ID (fila {row}): {rows}; revísalas en Google Sheets',
          { row: idRow, rows: typed.slice(0, 10).map(String) },
        ),
        409,
      );
  } else if (lastContent > template)
    throw fail(
      'BARE_ROWS',
      msg(
        'En {sheet} hay filas escritas sin fórmulas después de la última fila preasignada ({template}), hasta la {last}; revísalas en Google Sheets',
        { sheet, template, last: lastContent },
      ),
      409,
    );
  // The rows that get a copy of row T: all the new rows, in Insectary_data those without formulas.
  const pasted = [];
  for (let row = start; row <= end; row++) if (!byId || !cellsAt(row).some(c => kind(c) === 'F')) pasted.push(row);
  const pastedRuns = runs(pasted);
  const added = Math.max(0, end - info.rowCount);

  // Columns whose formula has to come from another row than the template.
  const clear = [];
  const fills = [];
  for (let column = 0; column < width; column++) {
    // Insectary IDs are written row by row (below).
    if (column === idColumn) continue;
    const k = kindAt(template, column);
    if (k === 'F') continue;
    if (k === 'c') {
      clear.push(column);
      const floor = Math.max(first, template - TYPED_OVER_LOOKBACK);
      let source = template - 1;
      while (source >= floor && kindAt(source, column) !== 'F') source--;
      if (source >= floor) fills.push({ column, source, from: start });
      continue;
    }
    // Insectary_data's rows filled down are not completed: only its new IDs are written there.
    if (byId) continue;
    // A filled-down formula (three rows at least) that stopped at or after the last used row.
    let source = template - 1;
    while (source >= first && kindAt(source, column) !== 'F') source--;
    if (
      source >= first &&
      source >= lastUsed &&
      kindAt(source - 1, column) === 'F' &&
      kindAt(source - 2, column) === 'F'
    )
      fills.push({ column, source, from: source + 1 });
  }

  // Protected ranges this credential cannot edit (e.g. columns only the owner edits).
  const locked = lockedRanges(info.protectedRanges);
  noteProtection(store.db, sheet, info.protectedRanges);
  const isLocked = (column, row0, row1) => columnLocked(locked, column, row0, row1);
  const fillFrom = Math.min(start, ...fills.map(f => f.from));
  const lockedColumns = new Set();
  for (let column = 0; column < width; column++) if (isLocked(column, fillFrom, end)) lockedColumns.add(column);
  if (byId && lockedColumns.has(idColumn))
    throw fail('ID_LOCKED', 'La columna Insectary_ID está protegida para la cuenta de la app: pide a PAS que escriba los IDs', 409);

  // Insectary IDs go on from the last one with the formula of the nearest ID cell that holds
  // one (a new round's first ID is typed): planIds.
  let ids = null;
  if (byId) {
    let source = idRow;
    while (source >= first && kindAt(source, idColumn) !== 'F') source--;
    ids =
      source < first
        ? { problem: msg('No hay una fórmula de Insectary ID encima de la fila {row}', { row: start }) }
        : planIds({
            previous: text(cellsAt(idRow)[idColumn]),
            source,
            formula: cellsAt(source)[idColumn].userEnteredValue.formulaValue,
            from: start,
            end,
            existing: new Set(
              store.db
                .prepare(
                  "SELECT upper(trim(json_extract(values_json,'$.Insectary_ID'))) id FROM records WHERE sheet=? AND missing=0 AND row_num<?",
                )
                .all(sheet, start)
                .map(r => r.id)
                .filter(Boolean),
            ),
          });
    if (ids.problem) throw fail('ID_SERIES', ids.problem, 409);
  }

  const { sheetId } = info;
  const requests = [];
  if (added) requests.push({ appendDimension: { sheetId, dimension: 'ROWS', length: added } });
  for (const [r0, r1] of pastedRuns) {
    for (const [c0, c1] of segments(width, lockedColumns))
      requests.push({
        copyPaste: {
          source: range(sheetId, template, template, c0, c1),
          destination: range(sheetId, r0, r1, c0, c1),
          pasteType: 'PASTE_NORMAL',
          pasteOrientation: 'NORMAL',
        },
      });
    for (const column of clear.filter(c => !lockedColumns.has(c)))
      requests.push({
        updateCells: { range: range(sheetId, r0, r1, column, column + 1), fields: 'userEnteredValue' },
      });
  }
  for (const f of fills.filter(f => !lockedColumns.has(f.column)))
    for (const [r0, r1] of f.from >= start ? pastedRuns : [[f.from, end]])
      requests.push({
        copyPaste: {
          source: range(sheetId, f.source, f.source, f.column, f.column + 1),
          destination: range(sheetId, r0, r1, f.column, f.column + 1),
          pasteType: 'PASTE_FORMULA',
          pasteOrientation: 'NORMAL',
        },
      });
  // Each ID cell: the formula moved to its row, or a new round's first ID.
  if (ids)
    requests.push({
      updateCells: {
        range: range(sheetId, start, end, idColumn, idColumn + 1),
        rows: ids.cells.map(c => ({
          values: [{ userEnteredValue: c.formula ? { formulaValue: c.formula } : { stringValue: c.id } }],
        })),
        fields: 'userEnteredValue',
      },
    });

  store.startWrite([sheet]);
  try {
    try {
      await store.sheets.batchUpdate(requests);
    } catch (e) {
      // Google applies a batchUpdate whole or not at all: refused, nothing was written.
      if (byId && added && protectionRefused(e)) throw noRowsLeft(info.rowCount - idRow, count);
      throw e;
    }
    store.sheets.gridRows?.set(sheet, Math.max(info.rowCount, end));
  } finally {
    store.endWrite([sheet]);
  }

  // Read back and check what a pre-made row must have.
  const checkFrom = Math.min(template, fillFrom);
  const grid = await store.sheets.readGrid(sheet, checkFrom, Math.max(template, end));
  const at = row => grid[row - checkFrom]?.cells || [];
  const templateCells = at(template);
  const problems = [];
  // Their descriptors, for the interface language (server/messages.mjs).
  const problemsMsg = [];
  const note = (m, count = 1) => {
    const full = count > 1 ? msg('{problem} ({n} filas)', { problem: m, n: count }) : m;
    problems.push(full.text);
    problemsMsg.push(full.msg);
  };
  const headerCells = read.get(rowKey(sheet, mod.headerRow))?.cells || [];
  const name = column => {
    const title = headerText(headerCells[column]);
    return title ? `${columnLetter(column)} (${title})` : columnLetter(column);
  };
  const span = (from, to) => Array.from({ length: Math.max(0, to - from + 1) }, (_, i) => from + i);
  // The first ID of a new round is typed, not a formula.
  const typedId = row => ids?.cells.some(c => c.row === row && !c.formula);
  const formulaRows = new Map(); // column → the rows that must hold a formula
  for (let column = 0; column < width; column++) {
    if (lockedColumns.has(column) || column === idColumn) continue;
    if (kind(templateCells[column]) === 'F') formulaRows.set(column, pasted);
  }
  for (const f of fills) if (!lockedColumns.has(f.column)) formulaRows.set(f.column, f.from >= start ? pasted : span(f.from, end));
  if (ids) formulaRows.set(idColumn, span(start, end).filter(row => !typedId(row)));
  const checks = { formulas: true, validation: true, formats: true, ids: null };
  for (const [column, rows] of formulaRows) {
    const bad = rows.filter(row => kind(at(row)[column]) !== 'F').length;
    if (bad) {
      checks.formulas = false;
      note(msg('Falta la fórmula en {column}', { column: name(column) }), bad);
    }
  }
  for (let column = 0; column < width; column++) {
    if (lockedColumns.has(column) || column === idColumn) continue;
    const want = templateCells[column];
    let constants = 0,
      validation = 0,
      format = 0;
    for (const row of pasted) {
      const cell = at(row)[column];
      if (kind(cell) === 'c') constants++;
      if (JSON.stringify(cell?.dataValidation ?? null) !== JSON.stringify(want?.dataValidation ?? null)) validation++;
      if (
        JSON.stringify(cell?.userEnteredFormat?.numberFormat ?? null) !==
        JSON.stringify(want?.userEnteredFormat?.numberFormat ?? null)
      )
        format++;
    }
    if (constants) {
      checks.formulas = false;
      note(msg('Quedó un valor copiado en {column}', { column: name(column) }), constants);
    }
    if (validation) {
      checks.validation = false;
      note(msg('La validación de {column} no coincide con la fila {row}', { column: name(column), row: template }), validation);
    }
    if (format) {
      checks.formats = false;
      note(msg('El formato de {column} no coincide con la fila {row}', { column: name(column), row: template }), format);
    }
  }

  // Insectary IDs: the series goes on with no gap and no repeat.
  let firstId = null,
    lastId = null,
    idCount = 0;
  if (ids) {
    checks.ids = true;
    const seen = new Set();
    for (const [i, expected] of ids.expected.entries()) {
      const row = ids.from + i;
      const id = text(at(row)[idColumn]);
      if (id !== expected) {
        checks.ids = false;
        note(
          id
            ? msg('Fila {row}: el Insectary ID es «{id}», se esperaba {expected}', { row, id, expected })
            : msg('Fila {row}: el Insectary ID es «vacío», se esperaba {expected}', { row, expected }),
        );
        break;
      }
      if (ids.existing.has(id) || seen.has(id)) {
        checks.ids = false;
        note(msg('Fila {row}: el Insectary ID {id} ya existe', { row, id }));
        break;
      }
      seen.add(id);
      firstId ??= id;
      lastId = id;
      idCount++;
    }
  }

  // The app's copy takes the rows as they are now.
  const now = new Date().toISOString();
  let touched = 0;
  for (let row = byId ? start : fillFrom; row <= end; row++) {
    if (!byId && row <= template && !fills.some(f => f.from <= row && !lockedColumns.has(f.column))) continue;
    const previous = store.getRecordBySheetRow(sheet, row);
    const kept = store.keepUnavailable(sheet, rowValues(sheet, { row, cells: at(row) }, layout), previous, layout);
    if (
      previous &&
      !previous.missing &&
      JSON.stringify(previous.values) === JSON.stringify(kept.values) &&
      JSON.stringify(previous.formulas) === JSON.stringify(kept.formulas)
    )
      continue;
    store.placeRecord({
      id: previous && !previous.missing ? previous.id : randomUUID(),
      sheet,
      row,
      values: kept.values,
      formulas: kept.formulas,
      label: labelFor(sheet, kept.values),
      version: (previous && !previous.missing ? previous.version : 0) + 1,
      updatedAt: now,
      missing: false,
    });
    touched++;
  }

  const ownerColumns = [...lockedColumns]
    .filter(c => kind(templateCells[c]) !== '.' || fills.some(f => f.column === c) || templateCells[c]?.dataValidation)
    .map(name);
  const completed = fills.filter(f => f.from <= template && !lockedColumns.has(f.column));
  const result = {
    sheet,
    template,
    // Rows appended to the sheet; in Insectary_data only past the grid's last row.
    added: byId ? (added ? { from: end - added + 1, to: end, count: added } : null) : { from: start, to: end, count },
    // Insectary_data: the rows that got the next IDs.
    filled: byId ? { from: start, to: end, count } : null,
    completed: completed.length
      ? { from: Math.min(...completed.map(f => f.from)), to: template, columns: completed.map(f => name(f.column)) }
      : null,
    firstId,
    lastId,
    ids: idCount,
    newRounds: ids?.cells.filter(c => !c.formula).map(({ row, id }) => ({ row, id })) ?? [],
    checks,
    ok: !problems.length,
    problems: problems.slice(0, 20),
    problemsMsg: problemsMsg.slice(0, 20),
    ownerColumns,
    protectedRanges: info.protectedRanges.map(({ range: r, ...p }) => ({
      columns:
        r.startColumnIndex === undefined && r.endColumnIndex === undefined
          ? 'todas'
          : `${columnLetter(r.startColumnIndex ?? 0)}–${columnLetter((r.endColumnIndex ?? width) - 1)}`,
      rows:
        r.startRowIndex === undefined && r.endRowIndex === undefined
          ? 'todas'
          : `${(r.startRowIndex ?? 0) + 1}–${r.endRowIndex ?? ''}`,
      canEdit: p.requestingUserCanEdit === true || !!p.warningOnly,
      ...(p.description ? { description: p.description } : {}),
    })),
    records: touched,
  };
  store.db
    .prepare('INSERT INTO audit(id,kind,detail_json,created_at) VALUES(?,?,?,?)')
    .run(randomUUID(), 'premade_rows', JSON.stringify({ actor, ...result, protectedRanges: undefined }), now);
  return result;
}

/** Row numbers (ascending) grouped into [first, last] runs. */
function runs(rows) {
  const out = [];
  for (const row of rows) {
    if (out.length && out.at(-1)[1] === row - 1) out.at(-1)[1] = row;
    else out.push([row, row]);
  }
  return out;
}

/** [c0, c1) runs of columns 0…width−1 that are not locked. */
function segments(width, locked) {
  const out = [];
  for (let c = 0; c < width; c++) {
    if (locked.has(c)) continue;
    if (out.length && out.at(-1)[1] === c) out.at(-1)[1] = c + 1;
    else out.push([c, c + 1]);
  }
  return out;
}

/**
 * The ID after `id` as the sheet's formula makes it: the digit goes up, and
 * after 9 the letter goes up (by character code) with digit 0; the last letter
 * (the round) stays. Null when `id` is not of that form, or after Z9 (a new round).
 */
export function nextInSeries(id) {
  const m = /^([A-Z])(\d)([A-Z])$/.exec(String(id ?? '').trim());
  if (!m || (m[1] === 'Z' && m[2] === '9')) return null;
  const digit = Number(m[2]);
  return digit === 9
    ? `${String.fromCharCode(m[1].charCodeAt(0) + 1)}0${m[3]}`
    : `${m[1]}${digit + 1}${m[3]}`;
}

const seriesIndex = id => (id.charCodeAt(0) - 65) * 10 + Number(id[1]);
const seriesId = (index, round) => `${String.fromCharCode(65 + Math.floor(index / 10))}${index % 10}${round}`;

/**
 * The Insectary IDs rows `from`–`end` will get, going on from `previous` (the ID
 * above; W2B.1 counts as W2B, as the formula reads it) with the formula of row
 * `source` moved to each row. After Z9 a new round starts with its first free ID
 * typed (after any of that round already in `existing`), and the formula goes on
 * from it: the older form, which writes its round letter ("0D"), with the new
 * letter; the newer one (LET) reads the round from the ID above. Returns { from,
 * expected: [id per row], cells: [{ row, id, formula }, or { row, id } typed], existing }
 * or { problem }.
 */
export function planIds({ previous, source, formula, from, end, existing }) {
  const expected = [];
  const cells = [];
  let id = String(previous ?? '').trim().toUpperCase();
  id = suffixedId(id)?.base ?? id;
  if (!/^[A-Z]\d[A-Z]$/.test(id))
    return {
      problem: id
        ? msg('La fila {row} no tiene un Insectary ID de la serie ({id})', { row: from - 1, id })
        : msg('La fila {row} no tiene un Insectary ID de la serie (vacío)', { row: from - 1 }),
    };
  let round = /"0([A-Z])"/.exec(formula)?.[1];
  const withRound = letter => {
    if (!round || round === letter) return;
    formula = formula.replaceAll(`"0${round}"`, `"0${letter}"`).replaceAll(`"${round}"`, `"${letter}"`);
    round = letter;
  };
  // A formula of an earlier round above a new round's first ID: it goes on with that round.
  withRound(id[2]);
  for (let row = from; row <= end; row++) {
    const next = nextInSeries(id);
    if (next) {
      id = next;
      cells.push({ row, id, formula: shiftFormula(formula, row - source) });
    } else {
      // Z9 reached: the next round, from the first ID not used yet in it.
      const letter = String.fromCharCode(id.charCodeAt(2) + 1);
      if (letter > 'Z') return { problem: msg('La serie de Insectary IDs llega a {id}: no hay más rondas', { id }) };
      const taken = [...existing].filter(x => x.length === 3 && x[2] === letter && /^[A-Z]\d$/.test(x.slice(0, 2)));
      const first = taken.length ? Math.max(...taken.map(seriesIndex)) + 1 : 0;
      if (first > seriesIndex('Z9A')) return { problem: msg('La ronda {letter} de Insectary IDs ya está usada', { letter }) };
      id = seriesId(first, letter);
      cells.push({ row, id });
      withRound(letter);
    }
    expected.push(id);
  }
  return { from, expected, cells, existing };
}
