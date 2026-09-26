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
    for (const key of ['client_id', 'client_secret', 'refresh_token']) if (!this.credentials[key]) throw new Error(`Google credential missing ${key}`);
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
      method: 'POST', headers: { 'content-type': 'application/x-www-form-urlencoded' },
      body: new URLSearchParams({ client_id: c.client_id, client_secret: c.client_secret, refresh_token: c.refresh_token, grant_type: 'refresh_token' })
    });
    if (!response.ok) throw new Error(`Google token refresh failed (${response.status})`);
    const body = await response.json();
    this.token = { value: body.access_token, expires: Date.now() + body.expires_in * 1000 };
    return this.token.value;
  }
  async request(path, options = {}) {
    const {background=false,...fetchOptions}=options;
    const method=fetchOptions.method||'GET';
    for(let attempt=0;attempt<(method==='GET'?5:1);attempt++){
      if(method==='GET')await this.readSlot(background);
      const response = await fetch(`${api}/${this.spreadsheetId}${path}`, {
        ...fetchOptions, headers: { authorization: `Bearer ${await this.accessToken()}`, 'content-type': 'application/json', ...fetchOptions.headers }
      });
      if(response.ok)return response.json();
      if(method==='GET'&&[429,503].includes(response.status)&&attempt<4){
        const delay=Math.max(Number(response.headers.get('retry-after')||0)*1000,1000*2**attempt);
        await new Promise(resolve=>setTimeout(resolve,delay));continue;
      }
      const body = await response.text();
      throw Object.assign(new Error(`Google Sheets ${response.status}: ${body.slice(0, 600)}`), { status: response.status });
    }
  }
  async readSlot(background=false) {
    const windowMs=60_000,limit=background?40:58;
    for(;;){
      this.readTimes=this.readTimes.filter(t=>Date.now()-t<windowMs);
      if(this.readTimes.length<limit)break;
      await new Promise(resolve=>setTimeout(resolve,Math.max(1,windowMs-(Date.now()-this.readTimes[0])+100)));
    }
    this.readTimes.push(Date.now());
  }
  async revision() {
    const response = await fetch(`https://www.googleapis.com/drive/v3/files/${this.spreadsheetId}?fields=id,version,modifiedTime`, {
      headers: { authorization: `Bearer ${await this.accessToken()}` }
    });
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
      const metadata = await this.request('?fields=sheets(properties(sheetId,title,gridProperties(rowCount)))',{background:true});
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
      const params = new URLSearchParams({ ranges: `${title}!A${start}:${lastCol}${end}`, fields: 'sheets(data(startRow,rowData(values(userEnteredValue,effectiveValue))))' });
      const result = await this.request(`?${params}`,{background:true});
      const data = result.sheets?.[0]?.data?.[0];
      for (const [i,row] of (data?.rowData || []).entries()) rows.push({ row: (data.startRow || start - 1) + i + 1, cells: row.values || [] });
    }
    return rows;
  }
  async readRow(sheet, row) {
    const mod = moduleMap.get(sheet);
    const lastCol = columnName(Math.max(...mod.fields.map(f => f.column)) + 1);
    const title = `'${sheet.replaceAll("'", "''")}'`;
    const params = new URLSearchParams({ ranges: `${title}!A${row}:${lastCol}${row}`, fields: 'sheets(data(startRow,rowData(values(userEnteredValue,effectiveValue))))' });
    const result = await this.request(`?${params}`);
    const data = result.sheets?.[0]?.data?.[0];
    return { row, cells: data?.rowData?.[0]?.values || [] };
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
    requests.push(...Object.entries(changes).map(([field, value]) => {
      const descriptor = mod.fields.find(f => f.key === field);
      const column = descriptor?.column;
      if (column === undefined) throw new Error(`Unknown field ${field}`);
      const cell = asCell(value);
      const dateNumber = descriptor.type === 'date' && typeof value === 'number';
      if (dateNumber) cell.userEnteredFormat = { numberFormat: { type: 'DATE', pattern: 'yyyy-mm-dd' } };
      return { updateCells: { range: { sheetId: mod.sheetId, startRowIndex: row - 1, endRowIndex: row, startColumnIndex: column, endColumnIndex: column + 1 }, rows: [{ values: [cell] }], fields: dateNumber?'userEnteredValue,userEnteredFormat.numberFormat':'userEnteredValue' } };
    }));
    if (!requests.length) throw new Error('No cells to write');
    return this.request(':batchUpdate', { method: 'POST', body: JSON.stringify({ requests }) });
  }
}

export class LocalSheets {
  constructor(seed = {}) { this.rows = new Map(); this.spreadsheetId = SANDBOX_ID; for (const [sheet, rows] of Object.entries(seed)) this.rows.set(sheet, rows.map((r, i) => normalizeSeedRow(r, i + 1, sheet))); }
  async readSheet(sheet) { return structuredClone(this.rows.get(sheet) || []); }
  async readRow(sheet, row) { return structuredClone((this.rows.get(sheet) || []).find(r => r.row === row) || { row, cells: [] }); }
  async writeCells(sheet, row, changes) {
    const rows = this.rows.get(sheet) || [];
    let target = rows.find(r => r.row === row);
    if (!target) { target = { row, cells: [] }; rows.push(target); this.rows.set(sheet, rows); }
    const mod = moduleMap.get(sheet);
    for (const [key, value] of Object.entries(changes)) target.cells[mod.fields.find(f => f.key === key).column] = asCell(value);
    return { replies: Object.keys(changes).map(() => ({})) };
  }
  async externalEdit(sheet, row, changes) { return this.writeCells(sheet, row, changes); }
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

export function rowValues(sheet, row) {
  const mod = moduleMap.get(sheet);
  const values = {}, formulas = {};
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
  let n = oneBased, out = '';
  while (n) { n--; out = String.fromCharCode(65 + n % 26) + out; n = Math.floor(n / 26); }
  return out;
}
