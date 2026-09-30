// Shared pieces of the local test lab (tools/lab): where its private files live,
// the workbook snapshot read with the app's own column logic, the benchmark
// cases (cases.json) and the value normalization used for scoring.
// Private data (photos, snapshot, cases, results, credentials) stays in the lab
// folder, never in the repository.
import { readFileSync, writeFileSync, existsSync, mkdirSync, chmodSync } from 'node:fs';
import { homedir } from 'node:os';
import { join } from 'node:path';
import { moduleMap, labelFor } from '../../server/schema.mjs';
import { headerLayout } from '../../server/columns.mjs';
import { rowValues } from '../../server/sheets.mjs';

export const LAB = process.env.ITHOMIINI_LAB_DIR || join(homedir(), '.cache', 'ithomiini-lab');
export const labPath = (...parts) => join(LAB, ...parts);

export function readJson(file, fallback) {
  if (!existsSync(file)) {
    if (fallback !== undefined) return fallback;
    throw new Error(`Missing ${file}`);
  }
  return JSON.parse(readFileSync(file, 'utf8'));
}
export function writePrivate(file, text) {
  mkdirSync(LAB, { recursive: true, mode: 0o700 });
  writeFileSync(file, text, { mode: 0o600 });
  chmodSync(file, 0o600);
}

/** The snapshot's sheets ({ sheet: [{ row, cells }] }). */
export function loadSnapshot(file = labPath('snapshot.json')) {
  const data = readJson(file);
  return data.sheets || data;
}

/**
 * A sheet of the snapshot as records in sheet order: { row, label, values, formulas }
 * (values as the app stores them: dates are day serials, sums keep their formula).
 */
export function sheetRecords(snapshot, sheet) {
  const mod = moduleMap.get(sheet);
  if (!mod) throw new Error(`Unknown sheet ${sheet}`);
  const rows = snapshot[sheet] || [];
  const header = rows.find(r => r.row === mod.headerRow);
  const layout = headerLayout(sheet, header);
  return rows
    .filter(r => r.row > mod.headerRow)
    .map(r => {
      const { values, formulas } = rowValues(sheet, r, layout);
      return { row: r.row, label: labelFor(sheet, values), values, formulas, layout };
    });
}

/**
 * The rows of a case in the snapshot. A case lists rows by label (the sheet's ID:
 * Insectary_ID, CLUTCH NUMBER…) — `labels` — or by ranges of labels in sheet
 * order — `ranges: [[first, last], …]` (every row between them, as on a notebook page).
 */
export function caseRows(snapshot, kase) {
  const records = sheetRecords(snapshot, kase.sheet).filter(r => r.label !== `${kase.sheet} record`);
  const same = (a, b) => String(a).trim().toLowerCase() === String(b).trim().toLowerCase();
  const out = [];
  for (const label of kase.labels || []) {
    const found = records.filter(r => same(r.label, label));
    if (found.length !== 1) throw new Error(`${kase.id}: ${found.length} rows with ${label} in ${kase.sheet}`);
    out.push(found[0]);
  }
  for (const [first, last] of kase.ranges || []) {
    const a = records.findIndex(r => same(r.label, first));
    const b = records.findIndex(r => same(r.label, last));
    if (a < 0 || b < a) throw new Error(`${kase.id}: range ${first}–${last} not found in ${kase.sheet}`);
    out.push(...records.slice(a, b + 1));
  }
  return out;
}

/** The fields scored in a case's rows: its `fields`, or every field but the ID and `skip`. */
export function caseFields(kase) {
  const mod = moduleMap.get(kase.sheet);
  if (kase.fields) return kase.fields;
  const skip = new Set([...(mod.identityFields.slice(0, 1)), ...(kase.skip || [])]);
  return [...new Set(mod.fields.map(f => f.key))].filter(k => !skip.has(k));
}

/**
 * The ground truth of a case: its rows' current values in the snapshot (the
 * human-corrected data). Cells calculated by a formula (not a typed sum like
 * =12+15) are left out: nobody transcribes them. `column` is where the cell is.
 */
export function groundTruth(snapshot, kase) {
  const fields = caseFields(kase);
  return caseRows(snapshot, kase).map(r => {
    const values = {};
    const columns = {};
    for (const f of fields) {
      if (!r.layout.columns.has(f)) continue;
      const formula = r.formulas[f];
      if (formula && !isSum(formula)) continue;
      values[f] = formula ?? r.values[f] ?? null;
      columns[f] = r.layout.columns.get(f);
    }
    return { row: r.row, label: r.label, values, columns };
  });
}
const isSum = f => /^=\s*\d+(\s*[+-]\s*\d+)*\s*$/.test(f);

export function loadCases(file = labPath('cases.json')) {
  return readJson(file).cases;
}

// ------------------------------------------------------------------ scoring
const NA = new Set(['', 'na', 'n/a', '-', '—', '–', 'none', 'null', 'nan']);
const SEX = { f: 'female', '♀': 'female', hembra: 'female', female: 'female', m: 'male', '♂': 'male', macho: 'male', male: 'male' };
const DAY0 = Date.UTC(1899, 11, 30);
const isDateField = field => /(^|[\s_])date([\s_]|$)|^date|date$/i.test(field) && !/^reason|date_time/i.test(field);

/**
 * A value in comparable form: NA and blanks alike, dates as YYYY-MM-DD (day serials,
 * ISO, dd/mm/yyyy), sums as their total (=12+15 → 27), whole numbers without decimals,
 * spaces collapsed, case ignored, sex words unified.
 */
export function norm(value, field = '') {
  if (value === null || value === undefined) return '';
  if (typeof value === 'boolean') return String(value);
  if (typeof value === 'number' && isDateField(field) && value > 20000 && value < 80000)
    return new Date(DAY0 + Math.round(value) * 86400000).toISOString().slice(0, 10);
  if (typeof value === 'number' && Number.isInteger(value)) return String(value);
  let s = String(value).trim().replace(/\s+/g, ' ').toLowerCase();
  if (NA.has(s)) return '';
  let m = /^(\d{4})-(\d{2})-(\d{2})/.exec(s);
  if (m) return s.slice(0, 10);
  m = /^(\d{1,2})\/(\d{1,2})\/(\d{4})$/.exec(s);
  if (m) return `${m[3]}-${m[2].padStart(2, '0')}-${m[1].padStart(2, '0')}`;
  if (/^=?\s*\d+(\s*[+-]\s*\d+)+$/.test(s)) {
    const total = [...s.matchAll(/([+-]?)\s*(\d+)/g)].reduce((t, x) => t + (x[1] === '-' ? -1 : 1) * Number(x[2]), 0);
    return String(total);
  }
  if (/^=?\d+(\.0+)?$/.test(s)) return String(Number(s.replace(/^=/, '')));
  if (/^sex$/i.test(field)) return SEX[s] ?? s;
  return s;
}

/** Levenshtein similarity (0–1) for free-text notes. */
function similarity(a, b) {
  if (a === b) return 1;
  const d = Array.from({ length: a.length + 1 }, (_, i) => [i]);
  for (let j = 1; j <= b.length; j++) d[0][j] = j;
  for (let i = 1; i <= a.length; i++)
    for (let j = 1; j <= b.length; j++)
      d[i][j] = Math.min(d[i - 1][j] + 1, d[i][j - 1] + 1, d[i - 1][j - 1] + (a[i - 1] === b[j - 1] ? 0 : 1));
  return 1 - d[a.length][b.length] / Math.max(a.length, b.length, 1);
}

/** A note without the "d/m/yy INI:" signatures the app adds to each entry. */
const unsigned = text => text.replace(/(^|\|)\s*\d{1,2}\/\d{1,2}\/\d{2,4}\s+[^\s:|]{1,12}:\s*/g, '$1').trim();

/**
 * A read value matches the truth. Notes: the text without the app's signatures,
 * 85 % similar to the whole note or to one of its entries (later entries people
 * added in the sheet are not on the page).
 */
export function same(read, truth, field) {
  const a = norm(read, field);
  const b = norm(truth, field);
  if (a === b) return true;
  if (/notes/i.test(field) && a && b) {
    const text = unsigned(a);
    const entries = [b, ...b.split('|')].map(unsigned).filter(Boolean);
    return entries.some(entry => similarity(text, entry) >= 0.85);
  }
  return false;
}
