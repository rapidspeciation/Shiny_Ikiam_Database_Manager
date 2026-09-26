import { readFileSync } from 'node:fs';
import { SANDBOX_ID, moduleMap, asCell, entered } from './schema.mjs';

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
  async readSheet(sheet) {
    const mod = moduleMap.get(sheet);
    if (!mod) throw new Error(`Unknown sheet: ${sheet}`);
    const title = `'${sheet.replaceAll("'", "''")}'`;
    const lastCol = columnName(Math.max(...mod.fields.map(f => f.column)) + 1);
    if (Date.now() - this.metadataAt > 30_000) {
      const metadata = await this.request('?fields=sheets(properties(sheetId,title,gridProperties(rowCount)))', {
        background: true,
      });
      this.metadata = new Map((metadata.sheets || []).map(s => [s.properties?.title, s]));
      this.metadataAt = Date.now();
    }
    const source = this.metadata.get(sheet);
    if (!source) throw new Error(`Sheet ${sheet} missing from workbook`);
    const count = source.properties.gridProperties?.rowCount || 0;
    this.gridRows.set(sheet, count);
    const chunkSize = mod.fields.length > 60 ? 500 : mod.fields.length > 40 ? 800 : 2000;
    const rows = [];
    for (let start = 1; start <= count; start += chunkSize) {
      const end = Math.min(count, start + chunkSize - 1);
      const params = new URLSearchParams({
        ranges: `${title}!A${start}:${lastCol}${end}`,
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
    const mod = moduleMap.get(sheet);
    const lastCol = columnName(Math.max(...mod.fields.map(f => f.column)) + 1);
    const title = `'${sheet.replaceAll("'", "''")}'`;
    const params = new URLSearchParams({
      ranges: `${title}!A${row}:${lastCol}${row}`,
      fields: 'sheets(data(startRow,rowData(values(userEnteredValue,effectiveValue))))',
    });
    const result = await this.request(`?${params}`);
    const data = result.sheets?.[0]?.data?.[0];
    return { row, cells: data?.rowData?.[0]?.values || [] };
  }
  /**
   * Reads several rows, possibly from several sheets, in a single request.
   * `targets` is a list of { sheet, rows: [rowNumber...] }. Returns a Map keyed
   * by rowKey(sheet, row). Rows beyond the data come back with no cells.
   */
  async readRows(targets) {
    const ranges = [];
    for (const { sheet, rows } of targets) {
      const mod = moduleMap.get(sheet);
      if (!mod) throw new Error(`Unknown sheet: ${sheet}`);
      // Rows past the sheet's grid cannot be requested; they are empty by definition.
      const gridCount = this.gridRows.get(sheet);
      for (const [start, end] of consecutiveRuns(gridCount ? rows.filter(r => r <= gridCount) : rows))
        ranges.push({ sheet, start, end, a1: `${quoteTitle(sheet)}!A${start}:${lastColumn(mod)}${end}` });
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
  /**
   * Writes cells of several rows (and sheets) in one batchUpdate, which Google
   * applies atomically. `writes` is a list of { sheet, row, changes, dateFormat }
   * where dateFormat lists fields whose cell needs a date number format.
   */
  async writeBatch(writes) {
    const requests = [];
    const highest = new Map();
    for (const write of writes) highest.set(write.sheet, Math.max(highest.get(write.sheet) || 0, write.row));
    for (const [sheet, row] of highest) {
      const gridCount = this.gridRows.get(sheet);
      if (gridCount !== undefined && row > gridCount) {
        requests.push({
          appendDimension: { sheetId: moduleMap.get(sheet).sheetId, dimension: 'ROWS', length: row - gridCount },
        });
        this.gridRows.set(sheet, row);
        this.metadataAt = 0;
      }
    }
    for (const write of writes) requests.push(...cellRequests(write));
    if (!requests.length) throw new Error('No cells to write');
    return this.request(':batchUpdate', { method: 'POST', body: JSON.stringify({ requests }) });
  }
  async writeCells(sheet, row, changes) {
    const mod = moduleMap.get(sheet);
    if (!mod?.sheetId) throw new Error(`Sheet ${sheet} has no verified sheet ID`);
    const requests = [];
    const gridCount = this.gridRows.get(sheet);
    if (gridCount !== undefined && row > gridCount) {
      requests.push({ appendDimension: { sheetId: mod.sheetId, dimension: 'ROWS', length: row - gridCount } });
      this.gridRows.set(sheet, row);
      this.metadataAt = 0;
    }
    requests.push(
      ...Object.entries(changes).map(([field, value]) => {
        const descriptor = mod.fields.find(f => f.key === field);
        const column = descriptor?.column;
        if (column === undefined) throw new Error(`Unknown field ${field}`);
        const cell = asCell(value);
        const dateNumber = descriptor.type === 'date' && typeof value === 'number';
        if (dateNumber) cell.userEnteredFormat = { numberFormat: { type: 'DATE', pattern: 'yyyy-mm-dd' } };
        return {
          updateCells: {
            range: {
              sheetId: mod.sheetId,
              startRowIndex: row - 1,
              endRowIndex: row,
              startColumnIndex: column,
              endColumnIndex: column + 1,
            },
            rows: [{ values: [cell] }],
            fields: dateNumber ? 'userEnteredValue,userEnteredFormat.numberFormat' : 'userEnteredValue',
          },
        };
      }),
    );
    if (!requests.length) throw new Error('No cells to write');
    return this.request(':batchUpdate', { method: 'POST', body: JSON.stringify({ requests }) });
  }
}

export class LocalSheets {
  constructor(seed = {}) {
    this.rows = new Map();
    this.spreadsheetId = SANDBOX_ID;
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
  async writeBatch(writes) {
    if (this.failNextWrite) {
      const failure = this.failNextWrite;
      this.failNextWrite = null;
      throw failure;
    }
    for (const write of writes) await this.writeCells(write.sheet, write.row, write.changes);
    return { replies: [] };
  }
  async writeCells(sheet, row, changes) {
    const rows = this.rows.get(sheet) || [];
    let target = rows.find(r => r.row === row);
    if (!target) {
      target = { row, cells: [] };
      rows.push(target);
      this.rows.set(sheet, rows);
    }
    const mod = moduleMap.get(sheet);
    for (const [key, value] of Object.entries(changes))
      target.cells[mod.fields.find(f => f.key === key).column] = asCell(value);
    return { replies: Object.keys(changes).map(() => ({})) };
  }
  async externalEdit(sheet, row, changes) {
    return this.writeCells(sheet, row, changes);
  }
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
function lastColumn(mod) {
  return columnName(Math.max(...mod.fields.map(f => f.column)) + 1);
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
function cellRequests({ sheet, row, changes, dateFormat = [] }) {
  const mod = moduleMap.get(sheet);
  if (!mod?.sheetId) throw new Error(`Sheet ${sheet} has no verified sheet ID`);
  return Object.entries(changes).map(([field, value]) => {
    const column = mod.fields.find(f => f.key === field)?.column;
    if (column === undefined) throw new Error(`Unknown field ${field}`);
    const cell = asCell(value);
    const formatDate = dateFormat.includes(field) && typeof value === 'number';
    if (formatDate) cell.userEnteredFormat = { numberFormat: { type: 'DATE', pattern: 'd-mmm-yy' } };
    return {
      updateCells: {
        range: {
          sheetId: mod.sheetId,
          startRowIndex: row - 1,
          endRowIndex: row,
          startColumnIndex: column,
          endColumnIndex: column + 1,
        },
        rows: [{ values: [cell] }],
        fields: formatDate ? 'userEnteredValue,userEnteredFormat.numberFormat' : 'userEnteredValue',
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

/**
 * Compares a sheet's live header row with the column map the app was built
 * from. Returns the list of mismatches; an empty list means writes are safe.
 */
export function headerMismatches(sheet, row) {
  const mod = moduleMap.get(sheet);
  const problems = [];
  for (const field of mod.fields) {
    const cell = row?.cells?.[field.column];
    const text = cell?.effectiveValue?.stringValue ?? cell?.userEnteredValue?.stringValue ?? '';
    if (String(text).trim() !== field.key.trim()) problems.push({ field: field.key, found: text || null });
  }
  return problems;
}

/** True when the live cell already displays as a date. */
export function hasDateFormat(cell) {
  const type = cell?.userEnteredFormat?.numberFormat?.type;
  return type === 'DATE' || type === 'DATE_TIME';
}

export function rowValues(sheet, row) {
  const mod = moduleMap.get(sheet);
  const values = {},
    formulas = {};
  for (const field of mod.fields) {
    const value = entered(row.cells[field.column]);
    if (value?.formula) {
      formulas[field.key] = value.formula;
      const effective = row.cells[field.column]?.effectiveValue;
      values[field.key] = effective?.stringValue ?? effective?.numberValue ?? effective?.boolValue ?? null;
    } else values[field.key] = value;
  }
  return { values, formulas };
}

function columnName(oneBased) {
  let n = oneBased,
    out = '';
  while (n) {
    n--;
    out = String.fromCharCode(65 + (n % 26)) + out;
    n = Math.floor(n / 26);
  }
  return out;
}
