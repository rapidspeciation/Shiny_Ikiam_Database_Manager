import { readFileSync } from 'node:fs';

const profile = JSON.parse(readFileSync(new URL('../docs/workbook-schema.json', import.meta.url)));
export const SANDBOX_ID = '19FXrunwWKK1pbyHqWNPcytmaDmyBQoK7yabzIdRQQYM';
export const PRODUCTION_ID = '1QZj';

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
export function labelFor(module, values) {
  const mod = moduleMap.get(module);
  return (
    mod?.identityFields.map(k => values[k]).find(v => v !== null && v !== undefined && String(v).trim()) ||
    values.SPECIES ||
    values.Species ||
    `${module} record`
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
  Insectary_stocks: new Set(['NUMBER OF EGGS', 'NUMBER OF LARVAE', 'NUMBER OF PUPA', 'NUMBER OF ADULTS']),
};
const SUM = /^=?\s*\d+(?:\s*\+\s*\d+)*\s*$/;
/** "=12+15", "12 + 15" or "=27" as the formula "=12+15" / "=27"; null when it is not a simple sum. */
export function simpleSum(text) {
  if (typeof text !== 'string' || !SUM.test(text)) return null;
  const trimmed = text.trim();
  // A plain number stays a number; only a sum (or an explicit "=") becomes a formula.
  if (!trimmed.startsWith('=') && !trimmed.includes('+')) return null;
  return '=' + trimmed.replace(/^=/, '').split('+').map(t => t.trim()).join('+');
}
export const isSumField = (module, field) => !!SUM_FIELDS[module]?.has(field);

export function validateValues(module, values, { allowFormula = false, normalize = true } = {}) {
  const mod = moduleMap.get(module);
  if (!mod) throw Object.assign(new Error('Hoja desconocida'), { status: 404, code: 'MODULE_NOT_FOUND' });
  if (!values || typeof values !== 'object' || Array.isArray(values))
    throw Object.assign(new Error('Los valores deben ser un objeto'), { status: 400, code: 'INVALID_VALUES' });
  const out = {};
  for (const [key, val] of Object.entries(values)) {
    const f = mod.fields.find(x => x.key === key);
    if (!f || f.readonly)
      throw Object.assign(new Error(`${key} no se puede editar`), { status: 400, code: 'INVALID_FIELD', field: key });
    if (isSumField(module, key)) {
      const formula = simpleSum(typeof val === 'object' && val ? val.formula : val);
      if (formula) {
        out[key] = { formula };
        continue;
      }
    }
    if (val && typeof val === 'object') {
      if (!allowFormula || typeof val.formula !== 'string' || !val.formula.startsWith('='))
        throw Object.assign(new Error(`Valor no válido en ${key}`), { status: 400, code: 'INVALID_VALUE', field: key });
      out[key] = { formula: val.formula };
      continue;
    }
    if (val !== null && !['string', 'number', 'boolean'].includes(typeof val))
      throw Object.assign(new Error(`Valor no válido en ${key}`), { status: 400, code: 'INVALID_VALUE', field: key });
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
  Object.assign(new Error(`Fecha no válida en ${key}: usa 14-Aug-25 o 2025-08-14, entre 1990 y 2099`), {
    status: 400,
    code: 'INVALID_DATE',
    field: key,
  });

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

const alphabet = 'ABCDEFGHIJKLMNÑOPQRSTUVWXYZ';
export function nextInsectaryId(id) {
  if (!id || !/^[A-ZÑ]\d[A-ZÑ]$/.test(id)) return 'A0A';
  const [a, digit, c] = [...id];
  let ai = alphabet.indexOf(a),
    ci = alphabet.indexOf(c),
    d = Number(digit);
  if (ai < 0 || ci < 0) return 'A0A';
  if (++d > 9) {
    d = 0;
    if (++ai >= alphabet.length) {
      ai = 0;
      ci++;
    }
  }
  if (ci >= alphabet.length)
    throw Object.assign(new Error('Insectary ID space exhausted'), { status: 409, code: 'ID_EXHAUSTED' });
  return `${alphabet[ai]}${d}${alphabet[ci]}`;
}

export function makeSourceUrl(sheet, row, spreadsheetId = SANDBOX_ID) {
  const mod = moduleMap.get(sheet);
  return `https://docs.google.com/spreadsheets/d/${spreadsheetId}/edit#gid=${mod?.sheetId || 0}&range=A${row}`;
}
