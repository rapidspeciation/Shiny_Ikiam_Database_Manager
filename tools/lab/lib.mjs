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

/** The words of a text (3+ letters, no accents, lower case), to compare notes loosely. */
export function words(text) {
  return new Set(
    String(text ?? '')
      .normalize('NFD')
      .replace(/[̀-ͯ]/g, '')
      .toLowerCase()
      .match(/[a-z]{3,}/g) ?? [],
  );
}
/** The butterfly IDs in a text (U8A, C8B, 9HO: letters and digits, 2 to 4), for notes with no words. */
export function noteIds(text) {
  return new Set(String(text ?? '').toUpperCase().match(/\b(?=[A-Z0-9]*\d)(?=[A-Z0-9]*[A-Z])[A-Z0-9]{2,4}\b/g) ?? []);
}
/** A proposal's note cell as text ({ replace } and { value } forms too), without the app's signatures. */
const noteOf = cell => String((cell && typeof cell === 'object' ? (cell.replace ?? cell.value) : cell) ?? '');
const SIGNATURE = /\d{1,2}\/\d{1,2}\/\d{2,4}\s+[A-Z]{1,4}:/g;

/**
 * A case's proposals scored against its ground truth (`rows`, from
 * groundTruth), two ways:
 *
 * - legacy: as the bench always scored, comparable with the runs before
 *   doubtful cells went into proposals. Every truth cell counts; a cell the
 *   proposal marks doubtful (change.doubts) counts as left out, as the tool
 *   used to leave it. Notes apart, by words; a note without words is skipped.
 * - v2: cells whose truth is not on the photo (the case's `notOnPage`, label →
 *   columns) are not counted; doubtful cells count by their value and are
 *   counted apart (flagged right / flagged wrong; wrongUnflagged: wrong with no
 *   flag); notes without words (a parent couple, "U8A♀ + C8B♂") are compared by
 *   their IDs; a note proposed where the truth is blank is counted (extraNotes).
 *
 * `proposals`: [{ changes }] in the order they were made (later ones win).
 * Returns { legacy, v2, errors, rowsProposed, rowsOutside }.
 */
export function scoreCase(kase, rows, proposals) {
  const read = new Map(); // sheet row → { values, doubts }
  let outside = 0;
  for (const p of proposals)
    for (const c of p.changes ?? []) {
      if (c.context) continue;
      const target = c.create ? rows.find(r => same(r.label, c.label, '')) : rows.find(r => r.row === c.row && c.sheet === kase.sheet);
      if (!target) {
        outside++;
        continue;
      }
      const got = read.get(target.row) ?? { values: {}, doubts: {} };
      Object.assign(got.values, c.values);
      for (const field of Object.keys(c.values)) delete got.doubts[field];
      Object.assign(got.doubts, Object.fromEntries(Object.entries(c.doubts ?? {}).filter(([f]) => f in c.values)));
      read.set(target.row, got);
    }
  const skip = new Map(Object.entries(kase.notOnPage ?? {}).map(([label, fields]) => [String(label).toLowerCase(), new Set(fields)]));
  const legacy = { correct: 0, total: 0, wrong: 0, missing: 0, filledCorrect: 0, filledTotal: 0, notesMatch: 0, notesTotal: 0 };
  const v2 = {
    correct: 0,
    total: 0,
    wrong: 0,
    missing: 0,
    notOnPage: 0,
    flaggedRight: 0,
    flaggedWrong: 0,
    wrongUnflagged: 0,
    notesMatch: 0,
    notesTotal: 0,
    extraNotes: 0,
  };
  const errors = [];
  for (const r of rows) {
    const got = read.get(r.row);
    const off = skip.get(String(r.label).toLowerCase()) ?? new Set();
    for (const [field, value] of Object.entries(r.values)) {
      const proposed = got && Object.hasOwn(got.values, field) ? got.values[field] : undefined;
      const flagged = !!got?.doubts?.[field];
      const counted = !off.has(field);
      if (!counted) v2.notOnPage++;
      // Notes are scored apart: the sheet keeps each note's original date and author
      // ("10/6/26 MJS: …"), which a photo does not show; only the words are compared.
      if (/^notes?\b|^notes_/i.test(field)) {
        const mine = noteOf(proposed).replace(SIGNATURE, ' ');
        const truthWords = words(value);
        const mineWords = words(mine);
        const shared = [...mineWords].filter(w => truthWords.has(w)).length;
        const byWords = mineWords.size > 0 && shared / mineWords.size >= 0.5;
        if (truthWords.size) {
          legacy.notesTotal++;
          if (byWords && !flagged) legacy.notesMatch++;
        }
        if (!counted) continue;
        // The truth's own signatures ("29/9/26 FCH:") are not on the page either: their initials are no word.
        const truth = String(value ?? '').replace(SIGNATURE, ' ');
        const truthIds = noteIds(truth);
        const truthWords2 = words(truth);
        if (truthWords2.size) {
          const shared2 = [...mineWords].filter(w => truthWords2.has(w)).length;
          v2.notesTotal++;
          v2.notesMatch += mineWords.size > 0 && shared2 / mineWords.size >= 0.5;
        } else if (truthIds.size) {
          // A note of IDs only (the parent couple): right when it names the same butterflies.
          v2.notesTotal++;
          const mineIds = noteIds(mine);
          v2.notesMatch += [...truthIds].every(id => mineIds.has(id));
        } else if (norm(value, field) === '' && mine.trim()) v2.extraNotes++;
        continue;
      }
      const has = norm(value, field) !== '';
      const readValue = proposed?.formula ?? proposed;
      const right = proposed !== undefined && same(readValue, value, field);
      // Legacy: a doubtful cell was left out of the proposal.
      legacy.total++;
      legacy.filledTotal += has;
      if (proposed === undefined || flagged) {
        if (has) legacy.missing++;
        else legacy.correct++;
      } else if (right) {
        legacy.correct++;
        legacy.filledCorrect += has;
      } else legacy.wrong++;
      if (!counted) continue;
      v2.total++;
      if (proposed === undefined) {
        if (has) {
          v2.missing++;
          errors.push({ row: r.label, field, read: null, truth: String(value), kind: 'missing' });
        } else v2.correct++;
      } else if (right) {
        v2.correct++;
        v2.flaggedRight += flagged;
      } else {
        v2.wrong++;
        if (flagged) v2.flaggedWrong++;
        else v2.wrongUnflagged++;
        errors.push({ row: r.label, field, read: String(readValue), truth: String(value ?? ''), kind: flagged ? 'wrong (flagged)' : 'wrong' });
      }
    }
  }
  return { legacy, v2, errors, rowsProposed: read.size, rowsOutside: outside };
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
