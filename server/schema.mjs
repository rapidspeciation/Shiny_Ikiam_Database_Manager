import { readFileSync } from 'node:fs';
import { msg, msgError } from './messages.mjs';

const profile = JSON.parse(readFileSync(new URL('../docs/workbook-schema.json', import.meta.url)));
import { REAL_ID } from './workbook.mjs';

const groups = {
  Collection_data: 'field',
  SamplingDay_data: 'field',
  Location_data: 'field',
  Insectary_data: 'insectary',
  Insectary_stocks: 'insectary',
  Melinaea_crosses: 'breeding',
  Melinaea_eggs: 'breeding',
  Stocks_Matings: 'breeding',
  Crosses_Lys_x_Pol: 'breeding',
  Hybrid_Attempts: 'breeding',
  Pheromones_data: 'research',
  'F1/F2_MutationRate': 'research',
  CRISPR: 'research',
  Wing_tissue: 'research',
  Life_History: 'research',
  Sperm_dissections: 'research',
  Barcoding_DNA: 'research',
  ME_TempData: 'research',
  Ithomiini_tube_locations_18Jun26: 'samples',
  MEIER_manifests_23Jun26: 'samples',
  MEIER_manifests_30Jan25_OLD: 'samples',
  Photo_links: 'media',
  Taxonomy_v18Jun25: 'reference',
  Lists: 'reference',
  File_notes: 'reference',
};
const identity = {
  Collection_data: ['CAM_ID', 'FieldMark_ID', 'Insectary_ID'],
  Insectary_data: ['Insectary_ID', 'CAM_ID'],
  Insectary_stocks: ['CLUTCH NUMBER'],
  Pheromones_data: ['CAM_ID'],
  Melinaea_crosses: ['Female', 'Male'],
  Melinaea_eggs: ['Clutch_No.'],
  SamplingDay_data: ['Date', 'Location'],
  CRISPR: ['CRISPR_No.'],
  Ithomiini_tube_locations_18Jun26: ['Tube_ID'],
};
const integerFields =
  /(?:^#|NUMBER|Number|No\.|count|Eggs|eggs|PUPA|LARVAE|Latitude|Longitude|LATITUDE|LONGITUDE|ELEVATION|Elevation|_weight|_lag|concentration|260\/|Time_point|Flight_height)/i;
// A header naming a date ("Death_date", "PUPA DATE", "DATE_OF_COLLECTION"), but
// not free-text reasons or date-time columns.
const dateFields = /(?:^|[\s_])date(?:[\s_]|$)|^date|date$/i;
const notDateFields = /^reason|date_time/i;

export const modules = profile.sheets
  .filter(s => s.headers?.length && groups[s.name])
  .map(s => {
    const seen = new Set();
    const fields = s.headers.map(h => {
      const duplicate = seen.has(h.name);
      seen.add(h.name);
      return {
        key: h.name,
        label: h.name.replaceAll('_', ' '),
        column: h.column - 1,
        type:
          dateFields.test(h.name) && !notDateFields.test(h.name)
            ? 'date'
            : integerFields.test(h.name)
              ? 'number'
              : 'text',
        readonly: duplicate,
      };
    });
    return {
      id: s.name,
      label: s.name.replaceAll('_', ' '),
      labelEn: s.name.replaceAll('_', ' '),
      group: groups[s.name],
      sheet: s.name,
      sheetId: s.sheet_id,
      headerRow: s.header_row,
      fields,
      identityFields:
        identity[s.name] ||
        fields.filter(f => /(?:^CAM_ID$|^Tube_ID$|^Insectary_ID$|^Clutch_No\.$)/.test(f.key)).map(f => f.key),
      recordCount: 0,
    };
  });
export const moduleMap = new Map(modules.map(m => [m.id, m]));

export function fieldFor(module, key) {
  return moduleMap.get(module)?.fields.find(f => f.key === key);
}
/** A column name compared loosely: case, accents, spaces, underscores and dots aside ("pupa date" = "PUPA DATE"). */
const looseName = name =>
  String(name ?? '')
    .normalize('NFD')
    .replace(/[̀-ͯ]/g, '')
    .toLowerCase()
    .replace(/[\s_.]+/g, '');
/** Edits (insert, delete, change a character) from one name to another. */
function editDistance(a, b) {
  let row = Array.from({ length: b.length + 1 }, (_, j) => j);
  for (let i = 1; i <= a.length; i++) {
    const next = [i];
    for (let j = 1; j <= b.length; j++) next[j] = Math.min(row[j] + 1, next[j - 1] + 1, row[j - 1] + (a[i - 1] === b[j - 1] ? 0 : 1));
    row = next;
  }
  return row[b.length];
}
/**
 * A column of a sheet as the assistant names it: the exact name, else the one
 * column it names loosely (case, accents, spaces and underscores aside).
 * { key } or { error } naming the nearest columns.
 */
export function columnOf(module, name) {
  const mod = typeof module === 'string' ? moduleMap.get(module) : module;
  const text = String(name ?? '');
  if (!mod) return { error: `Unknown sheet ${String(typeof module === 'string' ? module : '').slice(0, 60)}` };
  if (mod.fields.some(f => f.key === text)) return { key: text };
  const keys = [...new Set(mod.fields.map(f => f.key))];
  const loose = looseName(text);
  const same = keys.filter(k => looseName(k) === loose);
  if (same.length === 1) return { key: same[0] };
  const near = (same.length ? same : keys)
    .map(k => {
      const other = looseName(k);
      const part = loose.length >= 3 && (other.includes(loose) || loose.includes(other));
      return { k, d: part ? 0.5 : editDistance(loose, other) / Math.max(loose.length, other.length, 1) };
    })
    .filter(x => same.length || x.d <= 0.5)
    .sort((a, b) => a.d - b.d)
    .slice(0, 5)
    .map(x => x.k);
  return {
    error: `Unknown column ${text.slice(0, 60)} in ${mod.id}${near.length ? `; did you mean ${near.join(', ')}?` : '; describe_sheet lists its columns'}`,
  };
}
/** Several columns (columnOf): { keys } in the order given, or the first { error }. */
export function columnKeys(module, names) {
  const keys = [];
  for (const name of names) {
    const out = columnOf(module, name);
    if (out.error) return out;
    keys.push(out.key);
  }
  return { keys };
}
/** A values object with its columns named as the sheet names them (columnOf): { values } or { error }. */
export function withColumnNames(module, values) {
  if (!values || typeof values !== 'object' || Array.isArray(values)) return { values };
  const out = {};
  for (const [name, value] of Object.entries(values)) {
    const found = columnOf(module, name);
    if (found.error) return found;
    out[found.key] = value;
  }
  return { values: out };
}
export function labelFor(module, values) {
  const mod = moduleMap.get(module);
  // Always text: a number (a clutch 1014) stored in the TEXT label column became "1014.0".
  return String(
    mod?.identityFields.map(k => values[k]).find(v => v !== null && v !== undefined && String(v).trim()) ||
      values.SPECIES ||
      values.Species ||
      `${module} record`,
  );
}
export function entered(cell) {
  const value = cell?.userEnteredValue;
  if (!value) return null;
  if ('formulaValue' in value) return { formula: value.formulaValue };
  if ('numberValue' in value) return value.numberValue;
  if ('boolValue' in value) return value.boolValue;
  if ('stringValue' in value) return value.stringValue;
  return null;
}
export function asCell(value) {
  if (value === null || value === undefined || value === '') return {};
  if (typeof value === 'object' && value.formula) return { userEnteredValue: { formulaValue: value.formula } };
  if (typeof value === 'number') return { userEnteredValue: { numberValue: value } };
  if (typeof value === 'boolean') return { userEnteredValue: { boolValue: value } };
  return { userEnteredValue: { stringValue: String(value) } };
}
export function comparable(value) {
  return JSON.stringify(value ?? null);
}
/**
 * Counts the team types as sums, one term per day or group of eggs (=12+15):
 * the app writes them as such formulas, keeping the terms.
 */
export const SUM_FIELDS = {
  Insectary_stocks: new Set([
    'NUMBER OF EGGS',
    'NUMBER OF LARVAE',
    'NUMBER OF PUPA',
    'NUMBER OF ADULTS',
    // Typed as =2 or =2+6 in the sheet too.
    'NUMBER OF PUPAE/LARVAE FOR DISECTIONS',
  ]),
};
/** A group of terms in parentheses (the box or plant part of a stage sits on): (6-2). */
const GROUP = String.raw`\(\s*\d+(?:\s*[+-]\s*\d+)*\s*\)`;
const SUM = new RegExp(String.raw`^=?\s*(?:\d+|${GROUP})(?:\s*(?:[+-]\s*\d+|\+\s*${GROUP}))*\s*$`);
/**
 * "=12+15", "12 + 15", "27-5" (27 larvae, 5 died) or "=27" as the formula
 * "=12+15" / "=27-5" / "=27"; with groups, one parenthesized sub-sum each
 * (box A, box B): "=(6-2)+(5+3)"; null when it is not a simple sum.
 */
export function simpleSum(text) {
  if (typeof text !== 'string' || !SUM.test(text)) return null;
  const trimmed = text.trim();
  // A plain number stays a number; only a sum (or an explicit "=") becomes a formula.
  if (!trimmed.startsWith('=') && !/[+\-(]/.test(trimmed)) return null;
  return '=' + trimmed.replace(/^=/, '').replace(/\s+/g, '');
}
/**
 * A simple sum's groups, each its terms: "=(6-2)+(5+3)" → [[6, -2], [5, 3]];
 * without parentheses one group ("=27-2" → [[27, -2]]); terms written outside
 * parentheses between groups make a group of their own. Null when not a sum.
 */
export function sumGroups(text) {
  const formula = typeof text === 'string' ? simpleSum(text) : null;
  if (!formula) return null;
  const groups = [];
  let bare = null;
  for (const m of formula.slice(1).matchAll(/\(([^)]*)\)|([+-]?\d+)/g)) {
    if (m[1] !== undefined) {
      bare = null;
      groups.push(m[1].match(/[+-]?\d+/g).map(Number));
    } else {
      if (!bare) groups.push((bare = []));
      bare.push(Number(m[2]));
    }
  }
  return groups;
}
export const isSumField = (module, field) => !!SUM_FIELDS[module]?.has(field);

// Formula cells that may be typed over, and only with a value different from what the
// formula predicts: the species of an insectary butterfly when what emerged is not what
// the clutch predicted. The formula is kept in history, so undo puts it back.
export const TYPED_OVER_FORMULA = { Insectary_data: new Set(['SPECIES', 'Collection_location']) };

export function validateValues(module, values, { allowFormula = false, normalize = true } = {}) {
  const mod = moduleMap.get(module);
  if (!mod) throw Object.assign(new Error('Hoja desconocida'), { status: 404, code: 'MODULE_NOT_FOUND' });
  if (!values || typeof values !== 'object' || Array.isArray(values))
    throw Object.assign(new Error('Los valores deben ser un objeto'), { status: 400, code: 'INVALID_VALUES' });
  const out = {};
  for (const [key, val] of Object.entries(values)) {
    const f = mod.fields.find(x => x.key === key);
    if (!f || f.readonly)
      throw msgError(msg('{field} no se puede editar', { field: key }), { status: 400, code: 'INVALID_FIELD', field: key });
    if (isSumField(module, key)) {
      const formula = simpleSum(typeof val === 'object' && val ? val.formula : val);
      if (formula) {
        out[key] = { formula };
        continue;
      }
    }
    if (val && typeof val === 'object') {
      if (!allowFormula || typeof val.formula !== 'string' || !val.formula.startsWith('=')) throw badValue(key);
      out[key] = { formula: val.formula };
      continue;
    }
    if (val !== null && !['string', 'number', 'boolean'].includes(typeof val)) throw badValue(key);
    if (!normalize || val === null || val === '') {
      out[key] = val === '' ? null : val;
      continue;
    }
    if (f.type === 'number') {
      // Numeric text becomes a number. Other text is kept: the workbook uses
      // values such as "994(6)" or "NA" in numeric-looking columns.
      const text = typeof val === 'string' ? val.trim() : null;
      out[key] = text !== null && /^-?\d+(?:\.\d+)?$/.test(text) ? Number(text) : typeof val === 'string' ? text : val;
      continue;
    }
    if (f.type === 'date') {
      if (typeof val === 'number' && Number.isFinite(val)) {
        if (!plausibleSerial(key, val)) throw badDate(key);
        out[key] = val;
        continue;
      }
      if (typeof val === 'string') {
        const candidate = val.trim();
        if (/^(NA|N\/A)$/i.test(candidate)) {
          out[key] = 'NA';
          continue;
        }
        const serial = parseDateText(candidate);
        if (serial !== null && plausibleSerial(key, serial)) {
          out[key] = serial;
          continue;
        }
      }
      throw badDate(key);
    }
    out[key] = val;
  }
  return out;
}

// A year typed wrong (92026 for 2026) makes a date the sheet would keep for ever.
const FIRST_SERIAL = 32874; // 1 Jan 1990
const LAST_SERIAL = 73051; // 1 Jan 2100
// "Days difference (…)" columns count days, not dates.
const plausibleSerial = (key, serial) =>
  /^days difference/i.test(key) || (serial >= FIRST_SERIAL && serial < LAST_SERIAL);
const badDate = key =>
  msgError(msg('Fecha no válida en {field}: usa 14-Aug-25 o 2025-08-14, entre 1990 y 2099', { field: key }), {
    status: 400,
    code: 'INVALID_DATE',
    field: key,
  });
const badValue = key =>
  msgError(msg('Valor no válido en {field}', { field: key }), { status: 400, code: 'INVALID_VALUE', field: key });

const MONTHS = ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'];
const MESES = ['ene', 'feb', 'mar', 'abr', 'may', 'jun', 'jul', 'ago', 'sep', 'oct', 'nov', 'dic'];
const SHEETS_EPOCH = Date.UTC(1899, 11, 30);

/**
 * Parses the date text people type or paste: 2025-08-14, 14-Aug-25 (the
 * original app's format), 14/08/2025 (day first, the workbook's locale) or a
 * Sheets serial number. Returns a Sheets serial day number, or null.
 */
export function parseDateText(text) {
  const s = String(text).trim();
  let m;
  if (/^\d{4,5}(?:\.\d+)?$/.test(s)) return Number(s);
  if ((m = /^(\d{4})-(\d{1,2})-(\d{1,2})$/.exec(s))) return serialFromParts(+m[1], +m[2], +m[3]);
  if ((m = /^(\d{1,2})[-/ ]([A-Za-z]{3})[A-Za-z]*[-/ ](\d{2}|\d{4})$/.exec(s))) {
    const abbr = m[2].toLowerCase();
    const month = (MONTHS.indexOf(abbr) + 1 || MESES.indexOf(abbr) + 1) | 0;
    return month ? serialFromParts(fullYear(+m[3]), month, +m[1]) : null;
  }
  if ((m = /^(\d{1,2})[/.-](\d{1,2})[/.-](\d{2}|\d{4})$/.exec(s)))
    return serialFromParts(fullYear(+m[3]), +m[2], +m[1]);
  return null;
}
function fullYear(year) {
  return year < 100 ? 2000 + year : year;
}
function serialFromParts(year, month, day) {
  if (month < 1 || month > 12 || day < 1 || day > 31) return null;
  const ms = Date.UTC(year, month - 1, day);
  if (new Date(ms).getUTCDate() !== day) return null;
  return Math.round((ms - SHEETS_EPOCH) / 86_400_000);
}

/** The letters of Insectary IDs: A to Z, no Ñ (the sheet's CHAR(CODE(letter)+1) goes from N to O). */
const alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
/**
 * The Insectary ID after `id`, as the sheet's formulas make them:
 * - since 30 Jun 2026, letter + digit + series letter: A0A … A9A, B0A … Z9A,
 *   then the next series A0B (=IF(MID(prev,2,1)="9", next letter&"0S", …));
 * - Jun 2023 – Jun 2026, digit + two letters: the digit runs fastest, then the
 *   last letter, then the middle one (9AZ → 0BA); after 9ZZ came A0A.
 * An Ñ (older app versions gave N9A → Ñ0A) is read as the N before O.
 * Anything else (2022's A7, 2023's 45M) starts the current form at A0A.
 */
export function nextInsectaryId(id) {
  const s = String(id ?? '')
    .trim()
    .toUpperCase()
    .replace(/Ñ/g, 'N');
  const letter = c => alphabet.indexOf(c);
  let m = /^([A-Z])(\d)([A-Z])$/.exec(s);
  if (m) {
    let [a, d, c] = [letter(m[1]), Number(m[2]), letter(m[3])];
    if (++d > 9) {
      d = 0;
      if (++a >= alphabet.length) {
        a = 0;
        c++;
      }
    }
    if (c >= alphabet.length)
      throw Object.assign(new Error('Insectary ID space exhausted'), { status: 409, code: 'ID_EXHAUSTED' });
    return `${alphabet[a]}${d}${alphabet[c]}`;
  }
  m = /^(\d)([A-Z])([A-Z])$/.exec(s);
  if (m) {
    let [d, middle, last] = [Number(m[1]), letter(m[2]), letter(m[3])];
    if (++d > 9) {
      d = 0;
      if (++last >= alphabet.length) {
        last = 0;
        if (++middle >= alphabet.length) return 'A0A';
      }
    }
    return `${d}${alphabet[middle]}${alphabet[last]}`;
  }
  return 'A0A';
}

export function makeSourceUrl(sheet, row, spreadsheetId = REAL_ID) {
  const mod = moduleMap.get(sheet);
  return `https://docs.google.com/spreadsheets/d/${spreadsheetId}/edit#gid=${mod?.sheetId || 0}&range=A${row}`;
}
