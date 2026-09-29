import { readFileSync } from 'node:fs';
import { SANDBOX_ID, moduleMap, asCell, entered } from './schema.mjs';
import { columnLetter, headerLayout } from './columns.mjs';

const api = 'https://sheets.googleapis.com/v4/spreadsheets';

export class GoogleSheets {
  constructor(config = {}) {
    this.spreadsheetId = config.spreadsheetId || SANDBOX_ID;
    if (this.spreadsheetId !== SANDBOX_ID) throw new Error('Only the personal sandbox workbook is permitted');
    const file = config.googleCredentialsFile || process.env.GOOGLE_CREDENTIALS_FILE;
    if (!file) throw new Error('GOOGLE_CREDENTIALS_FILE is required for live mode');
    this.credentials = JSON.parse(readFileSync(file, 'utf8'));
    for (const key of ['client_id', 'client_secret', 'refresh_token'])
      if (!this.credentials[key]) throw new Error(`Google credential missing ${key}`);
    this.token = null;
    this.gridRows = new Map();
    this.metadata = new Map();
    this.metadataAt = 0;
    this.readTimes = [];
  }
  async accessToken() {
    if (this.token && this.token.expires > Date.now() + 60_000) return this.token.value;
    const c = this.credentials;
    const response = await fetch('https://oauth2.googleapis.com/token', {
      method: 'POST',
      headers: { 'content-type': 'application/x-www-form-urlencoded' },
      body: new URLSearchParams({
        client_id: c.client_id,
        client_secret: c.client_secret,
        refresh_token: c.refresh_token,
        grant_type: 'refresh_token',
      }),
    });
    if (!response.ok) throw new Error(`Google token refresh failed (${response.status})`);
    const body = await response.json();
    this.token = { value: body.access_token, expires: Date.now() + body.expires_in * 1000 };
    return this.token.value;
  }
  async request(path, options = {}) {
    const { background = false, ...fetchOptions } = options;
    const method = fetchOptions.method || 'GET';
    for (let attempt = 0; attempt < (method === 'GET' ? 5 : 1); attempt++) {
      if (method === 'GET') await this.readSlot(background);
      const response = await fetch(`${api}/${this.spreadsheetId}${path}`, {
        ...fetchOptions,
        headers: {
          authorization: `Bearer ${await this.accessToken()}`,
          'content-type': 'application/json',
          ...fetchOptions.headers,
        },
      });
      if (response.ok) return response.json();
      if (method === 'GET' && [429, 503].includes(response.status) && attempt < 4) {
        const delay = Math.max(Number(response.headers.get('retry-after') || 0) * 1000, 1000 * 2 ** attempt);
        await new Promise(resolve => setTimeout(resolve, delay));
        continue;
      }
      const body = await response.text();
      throw Object.assign(new Error(`Google Sheets ${response.status}: ${body.slice(0, 600)}`), {
        status: response.status,
      });
    }
  }
  async readSlot(background = false) {
    const windowMs = 60_000,
      limit = background ? 40 : 58;
    for (;;) {
      this.readTimes = this.readTimes.filter(t => Date.now() - t < windowMs);
      if (this.readTimes.length < limit) break;
      await new Promise(resolve => setTimeout(resolve, Math.max(1, windowMs - (Date.now() - this.readTimes[0]) + 100)));
    }
    this.readTimes.push(Date.now());
  }
  async revision() {
    const response = await fetch(
      `https://www.googleapis.com/drive/v3/files/${this.spreadsheetId}?fields=id,version,modifiedTime`,
      {
        headers: { authorization: `Bearer ${await this.accessToken()}` },
      },
    );
    if (response.status === 403) return null; // Sheets-only credentials cannot use Drive metadata.
    if (!response.ok) throw new Error(`Google Drive metadata failed (${response.status})`);
    const file = await response.json();
    return file.version || file.modifiedTime || null;
  }
  /** Sheet IDs, grid sizes and protected ranges of every tab, kept for 30 s. */
  async loadMetadata(force = false) {
    if (!force && this.metadataAt && Date.now() - this.metadataAt <= 30_000) return this.metadata;
    const metadata = await this.request(
      '?fields=sheets(properties(sheetId,title,gridProperties(rowCount,columnCount)),protectedRanges)',
      { background: true },
    );
    this.metadata = new Map((metadata.sheets || []).map(s => [s.properties?.title, s]));
    this.metadataAt = Date.now();
    return this.metadata;
  }
  async sheetInfo(sheet) {
    const source = (await this.loadMetadata(true)).get(sheet);
    if (!source) throw new Error(`Sheet ${sheet} missing from workbook`);
    const grid = source.properties.gridProperties || {};
    this.gridRows.set(sheet, grid.rowCount || 0);
    return {
      sheetId: source.properties.sheetId,
      rowCount: grid.rowCount || 0,
      columnCount: grid.columnCount || 0,
      protectedRanges: source.protectedRanges || [],
    };
  }
  sheetIdOf(sheet) {
    return this.metadata?.get(sheet)?.properties?.sheetId ?? moduleMap.get(sheet)?.sheetId;
  }
  // Whole rows are read ('Sheet'!1:500), never a fixed width: columns may have been
  // inserted or moved, and the header row read with them says where each field is.
  async readSheet(sheet) {
    const mod = moduleMap.get(sheet);
    if (!mod) throw new Error(`Unknown sheet: ${sheet}`);
    const source = (await this.loadMetadata()).get(sheet);
    if (!source) throw new Error(`Sheet ${sheet} missing from workbook`);
    const count = source.properties.gridProperties?.rowCount || 0;
    this.gridRows.set(sheet, count);
    const chunkSize = mod.fields.length > 60 ? 500 : mod.fields.length > 40 ? 800 : 2000;
    const rows = [];
    for (let start = 1; start <= count; start += chunkSize) {
      const end = Math.min(count, start + chunkSize - 1);
      const params = new URLSearchParams({
        ranges: `${quoteTitle(sheet)}!${start}:${end}`,
        fields: 'sheets(data(startRow,rowData(values(userEnteredValue,effectiveValue))))',
      });
      const result = await this.request(`?${params}`, { background: true });
      const data = result.sheets?.[0]?.data?.[0];
      for (const [i, row] of (data?.rowData || []).entries())
        rows.push({ row: (data.startRow || start - 1) + i + 1, cells: row.values || [] });
    }
    return rows;
  }
  async readRow(sheet, row) {
    return (await this.readRows([{ sheet, rows: [row] }])).get(rowKey(sheet, row));
  }
  /**
   * Reads several rows, possibly from several sheets, in a single request.
   * `targets` is a list of { sheet, rows: [rowNumber...] }. Returns a Map keyed
   * by rowKey(sheet, row). Rows beyond the data come back with no cells.
   * Callers read the header row too, to map the cells to fields.
   */
  async readRows(targets) {
    const ranges = [];
    for (const { sheet, rows } of targets) {
      if (!moduleMap.has(sheet)) throw new Error(`Unknown sheet: ${sheet}`);
      // Rows past the sheet's grid cannot be requested; they are empty by definition.
      const gridCount = this.gridRows.get(sheet);
      for (const [start, end] of consecutiveRuns(gridCount ? rows.filter(r => r <= gridCount) : rows))
        ranges.push({ sheet, start, end, a1: `${quoteTitle(sheet)}!${start}:${end}` });
    }
    const out = new Map();
    for (const { sheet, rows } of targets) for (const row of rows) out.set(rowKey(sheet, row), { row, cells: [] });
    if (!ranges.length) return out;
    const params = new URLSearchParams({
      fields:
        'sheets(properties(title,gridProperties(rowCount)),data(startRow,rowData(values(userEnteredValue,effectiveValue,userEnteredFormat(numberFormat)))))',
    });
    for (const range of ranges) params.append('ranges', range.a1);
    const result = await this.request(`?${params}`);
    for (const sheet of result.sheets || []) {
      const title = sheet.properties?.title;
      const rowCount = sheet.properties?.gridProperties?.rowCount;
      if (title && rowCount) this.gridRows.set(title, rowCount);
      for (const data of sheet.data || []) {
        const first = (data.startRow || 0) + 1;
        (data.rowData || []).forEach((row, i) =>
          out.set(rowKey(title, first + i), { row: first + i, cells: row.values || [] }),
        );
      }
    }
    for (const range of ranges)
      for (let row = range.start; row <= range.end; row++)
        if (!out.has(rowKey(range.sheet, row))) out.set(rowKey(range.sheet, row), { row, cells: [] });
    return out;
  }
  /** Rows `start`–`end` with what a pre-made row carries: formulas, number formats and data validation. */
  async readGrid(sheet, start, end) {
    const params = new URLSearchParams({
      ranges: `${quoteTitle(sheet)}!${start}:${end}`,
      fields:
        'sheets(data(startRow,rowData(values(userEnteredValue,effectiveValue,dataValidation,userEnteredFormat(numberFormat)))))',
    });
    const result = await this.request(`?${params}`);
    const data = result.sheets?.[0]?.data?.[0];
    const out = [];
    for (let row = start; row <= end; row++) out.push({ row, cells: [] });
    const first = data?.startRow ?? start - 1;
    for (const [i, row] of (data?.rowData || []).entries()) {
      const target = out[first + i + 1 - start];
      if (target) target.cells = row.values || [];
    }
    return out;
  }
  /** Raw batchUpdate; Google applies all its requests or none. */
  async batchUpdate(requests) {
    this.metadataAt = 0;
    return this.request(':batchUpdate', { method: 'POST', body: JSON.stringify({ requests }) });
  }
  /**
   * Writes cells of several rows (and sheets) in one batchUpdate, which Google
   * applies atomically. `writes` is a list of { sheet, row, changes, columns,
   * dateFormat, timeFormat }: `columns` gives each changed field's column, from
   * the header row read just before (never from the profile); dateFormat and
   * timeFormat list fields whose cell needs a date or time number format.
   */
  async writeBatch(writes) {
    const requests = [];
    const highest = new Map();
    for (const write of writes) highest.set(write.sheet, Math.max(highest.get(write.sheet) || 0, write.row));
    for (const [sheet, row] of highest) {
      const gridCount = this.gridRows.get(sheet);
      if (gridCount !== undefined && row > gridCount) {
        requests.push({
          appendDimension: { sheetId: this.sheetIdOf(sheet), dimension: 'ROWS', length: row - gridCount },
        });
        this.gridRows.set(sheet, row);
        this.metadataAt = 0;
      }
    }
    for (const write of writes) requests.push(...cellRequests(write, this.sheetIdOf(write.sheet)));
    if (!requests.length) throw new Error('No cells to write');
    return this.request(':batchUpdate', { method: 'POST', body: JSON.stringify({ requests }) });
  }
}

/**
 * The Sheets API on local rows, for tests and offline mode. Cells keep what
 * Google keeps (userEnteredValue, effectiveValue, userEnteredFormat,
 * dataValidation). `evaluate(formula, { row, column, value })` gives the
 * effective value of a pasted formula (Google computes it; tests supply the
 * formulas they need). `protectedRanges` lists, per sheet, ranges the
 * credential may not edit, as Google reports them.
 */
export class LocalSheets {
  constructor(seed = {}, { evaluate, protectedRanges } = {}) {
    this.rows = new Map();
    this.gridRows = new Map();
    this.spreadsheetId = SANDBOX_ID;
    this.evaluate = evaluate || defaultEvaluate;
    this.protectedRanges = protectedRanges || {};
    for (const [sheet, rows] of Object.entries(seed)) {
      const normalized = rows.map((r, i) => normalizeSeedRow(r, i + 1, sheet));
      const mod = moduleMap.get(sheet);
      if (mod && !normalized.some(r => r.row === mod.headerRow)) normalized.unshift(headerRow(mod));
      this.rows.set(sheet, normalized);
    }
  }
  async readSheet(sheet) {
    return structuredClone(this.rows.get(sheet) || []);
  }
  async readRow(sheet, row) {
    return structuredClone((this.rows.get(sheet) || []).find(r => r.row === row) || { row, cells: [] });
  }
  async readRows(targets) {
    const out = new Map();
    for (const { sheet, rows } of targets)
      for (const row of rows) out.set(rowKey(sheet, row), await this.readRow(sheet, row));
    return out;
  }
  async readGrid(sheet, start, end) {
    const out = [];
    for (let row = start; row <= end; row++) out.push(await this.readRow(sheet, row));
    return out;
  }
  rowCount(sheet) {
    return Math.max(this.gridRows.get(sheet) || 0, ...(this.rows.get(sheet) || []).map(r => r.row), 0);
  }
  async sheetInfo(sheet) {
    const mod = moduleMap.get(sheet);
    const sheetId = mod?.sheetId ?? 0;
    return {
      sheetId,
      rowCount: this.rowCount(sheet),
      columnCount: Math.max(0, ...(this.rows.get(sheet) || []).map(r => r.cells.length)),
      protectedRanges: (this.protectedRanges[sheet] || []).map(({ requestingUserCanEdit = false, ...range }) => ({
        range: { sheetId, ...range },
        requestingUserCanEdit,
      })),
    };
  }
  async writeBatch(writes) {
    if (this.failNextWrite) {
      const failure = this.failNextWrite;
      this.failNextWrite = null;
      throw failure;
    }
    for (const write of writes) await this.writeCells(write.sheet, write.row, write.changes, write.columns);
    return { replies: [] };
  }
  /** Writes by field name: at `columns` when given (as the app does), else where this sheet's header has the field. */
  async writeCells(sheet, row, changes, columns) {
    const rows = this.rows.get(sheet) || [];
    let target = rows.find(r => r.row === row);
    if (!target) {
      target = { row, cells: [] };
      rows.push(target);
      this.rows.set(sheet, rows);
    }
    const layout = columns ? null : headerLayout(sheet, rows.find(r => r.row === moduleMap.get(sheet).headerRow));
    for (const [key, value] of Object.entries(changes)) {
      const column = columns ? columns[key] : layout.columns.get(key);
      if (column === undefined) throw new Error(`Unknown field ${key}`);
      target.cells[column] = asCell(value);
    }
    return { replies: Object.keys(changes).map(() => ({})) };
  }
  async externalEdit(sheet, row, changes) {
    return this.writeCells(sheet, row, changes);
  }
  /**
   * The batchUpdate requests the app sends besides cell writes: appendDimension,
   * copyPaste (PASTE_NORMAL, PASTE_FORMULA) and updateCells. All or nothing, as in
   * Google: a request touching a protected range the credential cannot edit fails the batch.
   */
  async batchUpdate(requests) {
    for (const request of requests) {
      const [sheet, rect] = this.touched(request);
      if (rect && this.isProtected(sheet, rect))
        throw Object.assign(new Error('Google Sheets 400: You are trying to edit a protected cell or object.'), {
          status: 400,
        });
    }
    const pasted = new Map();
    for (const request of requests) {
      if (request.appendDimension) {
        const sheet = this.sheetById(request.appendDimension.sheetId);
        this.gridRows.set(sheet, this.rowCount(sheet) + request.appendDimension.length);
      } else if (request.copyPaste) this.copyPaste(request.copyPaste, pasted);
      else if (request.updateCells) this.updateCells(request.updateCells);
      else throw new Error(`LocalSheets cannot apply ${Object.keys(request)[0]}`);
    }
    // Formula results, in sheet order so a formula reading the row above sees its new value.
    for (const [sheet, cells] of pasted) {
      const sorted = [...cells].map(k => k.split(':').map(Number)).sort((a, b) => a[0] - b[0] || a[1] - b[1]);
      for (const [row, column] of sorted) {
        const cell = this.cell(sheet, row, column);
        const formula = cell?.userEnteredValue?.formulaValue;
        if (!formula) continue;
        const value = this.evaluate(formula, {
          row,
          column,
          value: (r, c) => effectiveOf(this.cell(sheet, r, c)),
        });
        if (value === undefined || value === null) delete cell.effectiveValue;
        else cell.effectiveValue = typeof value === 'number' ? { numberValue: value } : { stringValue: String(value) };
      }
    }
    return { replies: requests.map(() => ({})) };
  }
  sheetById(sheetId) {
    for (const sheet of this.rows.keys()) if ((moduleMap.get(sheet)?.sheetId ?? 0) === sheetId) return sheet;
    throw new Error(`No sheet with ID ${sheetId}`);
  }
  touched(request) {
    const range = request.copyPaste?.destination || request.updateCells?.range;
    if (range) return [this.sheetById(range.sheetId), range];
    if (request.appendDimension) return [this.sheetById(request.appendDimension.sheetId), null];
    return [null, null];
  }
  isProtected(sheet, rect) {
    const overlaps = (a0, a1, b0, b1) => (a0 ?? 0) < (b1 ?? Infinity) && (b0 ?? 0) < (a1 ?? Infinity);
    return (this.protectedRanges[sheet] || []).some(
      p =>
        !p.requestingUserCanEdit &&
        overlaps(p.startRowIndex, p.endRowIndex, rect.startRowIndex, rect.endRowIndex) &&
        overlaps(p.startColumnIndex, p.endColumnIndex, rect.startColumnIndex, rect.endColumnIndex),
    );
  }
  row(sheet, number) {
    const rows = this.rows.get(sheet) || [];
    let target = rows.find(r => r.row === number);
    if (!target) {
      target = { row: number, cells: [] };
      rows.push(target);
      rows.sort((a, b) => a.row - b.row);
      this.rows.set(sheet, rows);
    }
    return target;
  }
  cell(sheet, row, column) {
    return (this.rows.get(sheet) || []).find(r => r.row === row)?.cells[column];
  }
  copyPaste({ source, destination, pasteType = 'PASTE_NORMAL' }, pasted) {
    const sheet = this.sheetById(destination.sheetId);
    const from = this.sheetById(source.sheetId);
    const height = source.endRowIndex - source.startRowIndex;
    const width = source.endColumnIndex - source.startColumnIndex;
    const marks = pasted.get(sheet) || new Set();
    pasted.set(sheet, marks);
    // The source tiles the destination, as when pasting one row over many.
    const copies = [];
    for (let r = destination.startRowIndex; r < destination.endRowIndex; r++)
      for (let c = destination.startColumnIndex; c < destination.endColumnIndex; c++) {
        const sr = source.startRowIndex + ((r - destination.startRowIndex) % height);
        const sc = source.startColumnIndex + ((c - destination.startColumnIndex) % width);
        copies.push([r, c, structuredClone(this.cell(from, sr + 1, sc) || {}), r - sr, c - sc]);
      }
    for (const [r, c, original, dRows, dCols] of copies) {
      const target = this.row(sheet, r + 1);
      const formula = original.userEnteredValue?.formulaValue;
      const value = formula
        ? { formulaValue: shiftFormula(formula, dRows, dCols) }
        : original.userEnteredValue && structuredClone(original.userEnteredValue);
      if (pasteType === 'PASTE_FORMULA') {
        const cell = target.cells[c] || {};
        delete cell.userEnteredValue;
        delete cell.effectiveValue;
        if (value) cell.userEnteredValue = value;
        if (value && !formula) cell.effectiveValue = structuredClone(value);
        target.cells[c] = cell;
      } else if (pasteType === 'PASTE_NORMAL') {
        const cell = {};
        if (value) cell.userEnteredValue = value;
        if (value && !formula) cell.effectiveValue = structuredClone(value);
        if (original.userEnteredFormat) cell.userEnteredFormat = original.userEnteredFormat;
        if (original.dataValidation) cell.dataValidation = original.dataValidation;
        target.cells[c] = cell;
      } else throw new Error(`LocalSheets cannot paste ${pasteType}`);
      if (formula) marks.add(`${r + 1}:${c}`);
    }
  }
  updateCells({ range, rows, fields }) {
    const sheet = this.sheetById(range.sheetId);
    const keys = fields.split(',').map(f => f.trim().split('.')[0]);
    for (let r = range.startRowIndex; r < range.endRowIndex; r++)
      for (let c = range.startColumnIndex; c < range.endColumnIndex; c++) {
        const given = rows?.[r - range.startRowIndex]?.values?.[c - range.startColumnIndex] || {};
        const target = this.row(sheet, r + 1);
        const cell = target.cells[c] || {};
        for (const key of keys) {
          if (given[key] === undefined) delete cell[key];
          else cell[key] = structuredClone(given[key]);
          if (key === 'userEnteredValue') {
            delete cell.effectiveValue;
            if (given.userEnteredValue && !('formulaValue' in given.userEnteredValue))
              cell.effectiveValue = structuredClone(given.userEnteredValue);
          }
        }
        target.cells[c] = cell;
      }
  }
}

const effectiveOf = cell => {
  const v = cell?.effectiveValue;
  return v?.stringValue ?? v?.numberValue ?? v?.boolValue ?? null;
};
/** The formulas LocalSheets can work out on its own: ="text", =number and =ROW(). */
function defaultEvaluate(formula, { row }) {
  let m;
  if ((m = /^="([^"]*)"$/.exec(formula))) return m[1];
  if ((m = /^=(-?\d+(?:\.\d+)?)$/.exec(formula))) return Number(m[1]);
  if (/^=ROW\(\)$/i.test(formula)) return row;
  return undefined;
}

/**
 * A formula pasted `dRows` rows and `dCols` columns away, as Sheets does:
 * relative references (A12, $A12, A$12) move, absolute parts ($A$1) and
 * whole-column references (A:A) stay. Text in quotes and quoted sheet names are left alone.
 */
export function shiftFormula(formula, dRows, dCols = 0) {
  let out = '';
  for (let i = 0; i < formula.length; ) {
    const ch = formula[i];
    if (ch === '"' || ch === "'") {
      const end = formula.indexOf(ch, i + 1);
      const stop = end < 0 ? formula.length : end + 1;
      out += formula.slice(i, stop);
      i = stop;
      continue;
    }
    const m = /^(\$?)([A-Z]{1,3})(\$?)(\d+)(?![\w(])/.exec(formula.slice(i));
    const before = formula[i - 1];
    if (m && !(before && /[\w.]/.test(before))) {
      const [text, colAbs, letters, rowAbs, digits] = m;
      const column = colAbs ? letters : columnLetter(Math.max(0, columnIndex(letters) + dCols));
      const row = rowAbs ? digits : String(Math.max(1, Number(digits) + dRows));
      out += `${colAbs}${column}${rowAbs}${row}`;
      i += text.length;
      continue;
    }
    out += ch;
    i++;
  }
  return out;
}
function columnIndex(letters) {
  let n = 0;
  for (const ch of letters) n = n * 26 + (ch.charCodeAt(0) - 64);
  return n - 1;
}

function normalizeSeedRow(row, index, sheet) {
  if (row.cells) return { row: row.row || index, cells: structuredClone(row.cells) };
  const mod = moduleMap.get(sheet);
  const cells = [];
  const values = row.values || row;
  for (const [key, value] of Object.entries(values)) {
    const column = mod?.fields.find(f => f.key === key)?.column;
    if (column !== undefined) cells[column] = asCell(value);
  }
  return { row: row.row || index + (mod?.headerRow || 1), cells };
}

export const rowKey = (sheet, row) => `${sheet}\u0000${row}`;

function quoteTitle(sheet) {
  return `'${sheet.replaceAll("'", "''")}'`;
}
/** Groups row numbers into [start, end] runs so fewer ranges are requested. */
function consecutiveRuns(rows) {
  const sorted = [...new Set(rows)].sort((a, b) => a - b);
  const runs = [];
  for (const row of sorted) {
    const last = runs.at(-1);
    if (last && row === last[1] + 1) last[1] = row;
    else runs.push([row, row]);
  }
  return runs;
}
function cellRequests({ sheet, row, changes, columns, dateFormat = [], timeFormat = [] }, sheetId) {
  if (!sheetId) throw new Error(`Sheet ${sheet} has no verified sheet ID`);
  return Object.entries(changes).map(([field, value]) => {
    // Only the live header decides where a field is; a field it lacks is never written.
    const column = columns?.[field];
    if (column === undefined) throw new Error(`No live column for ${field} in ${sheet}`);
    const cell = asCell(value);
    const formatDate = dateFormat.includes(field) && typeof value === 'number';
    if (formatDate) cell.userEnteredFormat = { numberFormat: { type: 'DATE', pattern: 'd-mmm-yy' } };
    const formatTime = timeFormat.includes(field) && typeof value === 'number';
    if (formatTime) cell.userEnteredFormat = { numberFormat: { type: 'TIME', pattern: 'h:mm' } };
    return {
      updateCells: {
        range: {
          sheetId,
          startRowIndex: row - 1,
          endRowIndex: row,
          startColumnIndex: column,
          endColumnIndex: column + 1,
        },
        rows: [{ values: [cell] }],
        fields: formatDate || formatTime ? 'userEnteredValue,userEnteredFormat.numberFormat' : 'userEnteredValue',
      },
    };
  });
}
function headerRow(mod) {
  const cells = [];
  for (const field of mod.fields)
    cells[field.column] = { userEnteredValue: { stringValue: field.key }, effectiveValue: { stringValue: field.key } };
  return { row: mod.headerRow, cells };
}

/** True when the live cell already displays as a date. */
export function hasDateFormat(cell) {
  const type = cell?.userEnteredFormat?.numberFormat?.type;
  return type === 'DATE' || type === 'DATE_TIME';
}

/** True when the live cell already displays as a time. */
export function hasTimeFormat(cell) {
  const type = cell?.userEnteredFormat?.numberFormat?.type;
  return type === 'TIME' || type === 'DATE_TIME';
}

/**
 * The fields of a sheet row, by the live column `layout` (server/columns.mjs).
 * Fields whose column is missing are left out: callers keep their last known value.
 */
export function rowValues(sheet, row, layout) {
  if (!layout?.columns) throw new Error('rowValues needs the live column layout');
  const mod = moduleMap.get(sheet);
  const values = {},
    formulas = {};
  for (const field of mod.fields) {
    const column = layout.columns.get(field.key);
    if (column === undefined || Object.hasOwn(values, field.key)) continue;
    const cell = row?.cells?.[column];
    const value = entered(cell);
    if (value?.formula) {
      formulas[field.key] = value.formula;
      const effective = cell?.effectiveValue;
      values[field.key] = effective?.stringValue ?? effective?.numberValue ?? effective?.boolValue ?? null;
    } else values[field.key] = value;
  }
  return { values, formulas };
}
