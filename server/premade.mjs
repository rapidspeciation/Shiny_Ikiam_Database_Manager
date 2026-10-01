// Pre-made rows: empty rows at the end of a sheet that already hold its
// formulas, formats and dropdowns (in Insectary_data also the next Insectary
// IDs). The team makes them in Google Sheets by dragging the last one down.
// When they run out the app makes more the same way, with a copyPaste of the
// last pre-made row, so a new row is never a bare row without formulas.

import { randomUUID } from 'node:crypto';
import { labelFor, moduleMap } from './schema.mjs';
import { columnLetter, describeProblems, headerLayout, headerText } from './columns.mjs';
import { rowKey, rowValues, shiftFormula } from './sheets.mjs';
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

/** Where the sheet's rows end in the app's copy: last row with formulas, last used row, last row. */
function tail(store, sheet) {
  const q = where =>
    store.db
      .prepare(`SELECT max(row_num) n FROM records WHERE sheet=? AND missing=0 AND row_num<2000000000 ${where}`)
      .get(sheet).n || 0;
  return { formulaRow: q("AND formulas_json<>'{}'"), observedRow: q('AND observed=1'), lastRow: q('') };
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
 * makes pre-made rows where the save would go past them, as many as it needs (in
 * blocks of at most MAX_EXTEND). Only sheets kept with pre-made rows (formulas at
 * or after the last used row) get them; a sheet typed without formulas keeps
 * getting plain rows. Runs inside the write queue.
 */
export async function ensurePremadeRows(store, needs) {
  for (const { sheet, lastRow } of needs) {
    for (;;) {
      const { formulaRow, observedRow } = tail(store, sheet);
      if (!formulaRow || formulaRow < observedRow || lastRow <= formulaRow) break;
      try {
        await extendRows(store, sheet, Math.min(MAX_EXTEND, Math.max(AUTO_BLOCK, lastRow - formulaRow)), {
          actor: 'auto',
        });
      } catch (e) {
        throw fail(
          'PREMADE_FAILED',
          `No se pudieron preparar filas nuevas en ${sheet} (con sus fórmulas y listas); no se guardó nada: ${e.message}`,
          e.status && e.status < 500 ? 409 : 503,
        );
      }
      if (tail(store, sheet).formulaRow <= formulaRow) break;
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
  for (let step = 1; (next = nextInSeries(next)); step++) if (next === id) return { row: last.r + step, ahead: true };
  return null;
}

/** The endpoint's entry: `count` more pre-made rows at the end of `sheet`, in the write queue. */
export function extendPremadeRows(store, sheet, count, user) {
  return store.runExclusive(() => extendRows(store, sheet, count, { actor: user?.id || user?.username || 'app' }));
}

/**
 * Appends `count` pre-made rows after the last row holding formulas (T):
 *  1. appendDimension when the grid is too short;
 *  2. copyPaste of row T over the new rows (formulas, whose relative references
 *     move with the row, number formats and data validation);
 *  3. the constants row T held (when it is already used) are cleared;
 *  4. a column whose fill-down stopped before T (Insectary_data's ID column in
 *     the test workbook ends at the last used row) is continued from its last
 *     formula, over the empty pre-made rows too, so the ID series has no gap; a
 *     formula typed over in a used row T is taken from the rows just above.
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
  const info = await store.sheets.sheetInfo(sheet);
  const first = Math.max(mod.headerRow + 1, known.formulaRow - LOOKBACK);
  const last = Math.min(info.rowCount, Math.max(known.formulaRow, known.lastRow) + count + 5);
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

  // The live end of the sheet: last formula row, last used row, last row with anything.
  let template = 0,
    lastUsed = 0,
    lastContent = 0,
    width = info.columnCount;
  for (let row = first; row <= last; row++) {
    const kinds = cellsAt(row).map(kind);
    width = Math.max(width, kinds.length);
    if (kinds.includes('F')) template = row;
    if (kinds.includes('c')) lastUsed = row;
    if (kinds.some(k => k !== '.')) lastContent = row;
  }
  if (!template) throw fail('NO_TEMPLATE', msg('{sheet} no tiene filas con fórmulas que copiar', { sheet }), 409);
  if (lastContent > template)
    throw fail(
      'BARE_ROWS',
      msg(
        'En {sheet} hay filas escritas sin fórmulas después de la última fila preasignada ({template}), hasta la {last}; revísalas en Google Sheets',
        { sheet, template, last: lastContent },
      ),
      409,
    );
  const start = template + 1;
  const end = template + count;

  // Columns whose formula has to come from another row than the template.
  const clear = [];
  const fills = [];
  for (let column = 0; column < width; column++) {
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
  const locked = info.protectedRanges.filter(p => p.requestingUserCanEdit === false && !p.warningOnly);
  const covers = (r, row0, row1, column) =>
    (r.startRowIndex ?? 0) < row1 &&
    row0 - 1 < (r.endRowIndex ?? Infinity) &&
    (r.startColumnIndex ?? 0) <= column &&
    column < (r.endColumnIndex ?? Infinity);
  const isLocked = (column, row0, row1) =>
    locked.some(
      p =>
        covers(p.range, row0, row1, column) &&
        !(p.unprotectedRanges || []).some(
          u =>
            (u.startRowIndex ?? 0) <= row0 - 1 &&
            row1 <= (u.endRowIndex ?? Infinity) &&
            covers(u, row0, row1, column),
        ),
    );
  const fillFrom = Math.min(start, ...fills.map(f => f.from));
  const lockedColumns = new Set();
  for (let column = 0; column < width; column++) if (isLocked(column, fillFrom, end)) lockedColumns.add(column);

  // Insectary_data's ID formula adds one to the ID above and keeps the round letter, so
  // after Z9D it would give "[0D". A new round starts as the team starts one: its first ID
  // typed (A0E, or the one after the IDs of that round already in the sheet), then the
  // formula with the new letter.
  const idColumn = sheet === 'Insectary_data' ? layout.columns.get('Insectary_ID') : undefined;
  let ids = null;
  if (idColumn !== undefined && !lockedColumns.has(idColumn)) {
    const fill =
      kind(cellsAt(template)[idColumn]) === 'F'
        ? { source: template, from: start }
        : fills.find(f => f.column === idColumn);
    if (fill)
      ids = planIds({
        previous: text(cellsAt(fill.from - 1)[idColumn]),
        source: fill.source,
        formula: cellsAt(fill.source)[idColumn].userEnteredValue.formulaValue,
        from: fill.from,
        end,
        existing: new Set(
          store.db
            .prepare(
              "SELECT upper(trim(json_extract(values_json,'$.Insectary_ID'))) id FROM records WHERE sheet=? AND missing=0 AND row_num<?",
            )
            .all(sheet, fill.from)
            .map(r => r.id)
            .filter(Boolean),
        ),
      });
    if (ids?.problem) throw fail('ID_SERIES', ids.problem, 409);
  }

  const { sheetId } = info;
  const requests = [];
  if (end > info.rowCount)
    requests.push({ appendDimension: { sheetId, dimension: 'ROWS', length: end - info.rowCount } });
  for (const [c0, c1] of segments(width, lockedColumns))
    requests.push({
      copyPaste: {
        source: range(sheetId, template, template, c0, c1),
        destination: range(sheetId, start, end, c0, c1),
        pasteType: 'PASTE_NORMAL',
        pasteOrientation: 'NORMAL',
      },
    });
  for (const column of clear.filter(c => !lockedColumns.has(c)))
    requests.push({
      updateCells: { range: range(sheetId, start, end, column, column + 1), fields: 'userEnteredValue' },
    });
  for (const f of fills.filter(f => !lockedColumns.has(f.column)))
    requests.push({
      copyPaste: {
        source: range(sheetId, f.source, f.source, f.column, f.column + 1),
        destination: range(sheetId, f.from, end, f.column, f.column + 1),
        pasteType: 'PASTE_FORMULA',
        pasteOrientation: 'NORMAL',
      },
    });
  // From the first new round on, each ID cell is written: the round's first ID, then its formula.
  if (ids?.written.length)
    requests.push({
      updateCells: {
        range: range(sheetId, ids.written[0].row, end, idColumn, idColumn + 1),
        rows: ids.written.map(w => ({
          values: [{ userEnteredValue: w.formula ? { formulaValue: w.formula } : { stringValue: w.id } }],
        })),
        fields: 'userEnteredValue',
      },
    });

  store.startWrite([sheet]);
  try {
    await store.sheets.batchUpdate(requests);
    store.sheets.gridRows?.set(sheet, Math.max(info.rowCount, end));
  } finally {
    store.endWrite([sheet]);
  }

  // Read back and check what a pre-made row must have.
  const checkFrom = Math.min(template, fillFrom);
  const grid = await store.sheets.readGrid(sheet, checkFrom, end);
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
  const formulaColumns = new Map(); // column → first row that must hold a formula
  for (let column = 0; column < width; column++) {
    if (lockedColumns.has(column)) continue;
    if (kind(templateCells[column]) === 'F') formulaColumns.set(column, start);
  }
  for (const f of fills) if (!lockedColumns.has(f.column)) formulaColumns.set(f.column, f.from);
  const checks = { formulas: true, validation: true, formats: true, ids: null };
  // The first ID of a new round is typed, not a formula.
  const typedId = row => ids?.written.some(w => w.row === row && !w.formula);
  for (const [column, from] of formulaColumns) {
    let bad = 0;
    for (let row = from; row <= end; row++)
      if (kind(at(row)[column]) !== 'F' && !(column === idColumn && typedId(row))) bad++;
    if (bad) {
      checks.formulas = false;
      note(msg('Falta la fórmula en {column}', { column: name(column) }), bad);
    }
  }
  for (let column = 0; column < width; column++) {
    if (lockedColumns.has(column)) continue;
    const want = templateCells[column];
    let constants = 0,
      validation = 0,
      format = 0;
    for (let row = start; row <= end; row++) {
      const cell = at(row)[column];
      if (kind(cell) === 'c' && !(column === idColumn && typedId(row))) constants++;
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
  for (let row = fillFrom; row <= end; row++) {
    if (row <= template && !fills.some(f => f.from <= row && !lockedColumns.has(f.column))) continue;
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
    added: { from: start, to: end, count },
    completed: completed.length
      ? { from: Math.min(...completed.map(f => f.from)), to: template, columns: completed.map(f => name(f.column)) }
      : null,
    firstId,
    lastId,
    ids: idCount,
    newRounds: ids?.written.filter(w => !w.formula).map(w => ({ row: w.row, id: w.id })) ?? [],
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
      canEdit: p.requestingUserCanEdit !== false || !!p.warningOnly,
      ...(p.description ? { description: p.description } : {}),
    })),
    records: touched,
  };
  store.db
    .prepare('INSERT INTO audit(id,kind,detail_json,created_at) VALUES(?,?,?,?)')
    .run(randomUUID(), 'premade_rows', JSON.stringify({ actor, ...result, protectedRanges: undefined }), now);
  return result;
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
 * The Insectary IDs rows `from`–`end` will get: the formula of row `source`
 * continues `previous` (…Z9D); a new round starts with its first free ID typed
 * (after any of that round already in `existing`) and the same formula with the
 * new round letter. Returns { from, expected: [id per row], written: [{ row, id,
 * formula? }] for the rows from the first new round on, existing } or { problem }.
 */
export function planIds({ previous, source, formula, from, end, existing }) {
  const expected = [];
  const written = [];
  const round = /"0([A-Z])"/.exec(formula)?.[1];
  let id = String(previous ?? '').trim().toUpperCase();
  if (!/^[A-Z]\d[A-Z]$/.test(id))
    return {
      problem: id
        ? msg('La fila {row} no tiene un Insectary ID de la serie ({id})', { row: from - 1, id })
        : msg('La fila {row} no tiene un Insectary ID de la serie (vacío)', { row: from - 1 }),
    };
  let letter = round;
  for (let row = from; row <= end; row++) {
    const next = nextInSeries(id);
    if (next) id = next;
    else {
      // Z9 reached: the next round, from the first ID not used yet in it.
      if (!round) return { problem: msg('La serie de Insectary IDs llega a {id} y su fórmula no indica la ronda', { id }) };
      letter = String.fromCharCode(id.charCodeAt(2) + 1);
      if (letter > 'Z') return { problem: msg('La serie de Insectary IDs llega a {id}: no hay más rondas', { id }) };
      const taken = [...existing].filter(x => x.length === 3 && x[2] === letter && /^[A-Z]\d$/.test(x.slice(0, 2)));
      const first = taken.length ? Math.max(...taken.map(seriesIndex)) + 1 : 0;
      if (first > seriesIndex('Z9A')) return { problem: msg('La ronda {letter} de Insectary IDs ya está usada', { letter }) };
      id = seriesId(first, letter);
      written.push({ row, id });
      expected.push(id);
      continue;
    }
    if (written.length)
      written.push({
        row,
        id,
        // The formula of `source`, moved to this row, with the round's letter.
        formula: shiftFormula(formula, row - source)
          .replaceAll(`"0${round}"`, `"0${letter}"`)
          .replaceAll(`"${round}"`, `"${letter}"`),
      });
    expected.push(id);
  }
  return { from, expected, written, existing };
}
