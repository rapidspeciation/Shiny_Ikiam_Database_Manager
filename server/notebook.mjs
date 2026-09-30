// Matching a transcribed notebook page with its sheet: each line Claude read
// from the photo (the digitalizar-cuaderno skill, through the match_notebook
// tool) is compared with its row and the page becomes a proposal. Pure
// functions: the sheet is reached through the `lookup` given to buildReview
// (server/notebook-tool.mjs).

import { isSumField, parseDateText, simpleSum } from './schema.mjs';

/**
 * The notebooks that can be digitized, and the sheet columns each one fills.
 * How each notebook looks and is written is explained to Claude in the skill
 * assistant/skills/digitalizar-cuaderno/SKILL.md (a test checks it names every column).
 */
export const KINDS = {
  stocks: {
    label: 'Posturas',
    sheet: 'Insectary_stocks',
    keys: ['CLUTCH NUMBER'],
    // A clutch not yet in the sheet is a new row; an unknown butterfly ID is a misreading.
    newRows: true,
    fields: [
      'CLUTCH NUMBER',
      'SPECIES',
      'DATE LAID',
      'NUMBER OF EGGS',
      'INSECTARY OR LABORATORY',
      'HATCHING DATE',
      'NUMBER OF LARVAE',
      'PUPA DATE',
      'NUMBER OF PUPA',
      'EMERGENCE DATE',
      'NUMBER OF ADULTS',
      'NOTES',
    ],
    // Also read (not named in the skill yet): the generation, from "(F1)" after the species or
    // its own column, and the count kept for dissections (the notebook's "dissections" column).
    extra: ['Generation', 'NUMBER OF PUPAE/LARVAE FOR DISECTIONS'],
    aliases: {
      dissections: 'NUMBER OF PUPAE/LARVAE FOR DISECTIONS',
      disecciones: 'NUMBER OF PUPAE/LARVAE FOR DISECTIONS',
      'NUMBER OF PUPAE/LARVAE FOR DISSECTIONS': 'NUMBER OF PUPAE/LARVAE FOR DISECTIONS',
      generación: 'Generation',
    },
  },
  emergence: {
    label: 'Emergidos',
    sheet: 'Insectary_data',
    keys: ['Insectary_ID'],
    newRows: false,
    fields: [
      'Insectary_ID',
      'SPECIES',
      'Sex',
      'CLUTCH NUMBER',
      'Stock_of_origin',
      'Intro2Insectary_date',
      'Death_date',
      'Death_cause',
      'CAM_ID',
      'Tube_1_id',
      'Notes_Insectary_data',
    ],
  },
  deaths: {
    label: 'Muertes',
    sheet: 'Insectary_data',
    keys: ['Insectary_ID'],
    newRows: false,
    fields: [
      'Insectary_ID',
      'SPECIES',
      'Sex',
      'Death_date',
      'Death_cause',
      'CAM_ID',
      'Tube_1_id',
      'Notes_Insectary_data',
    ],
  },
  labels: {
    label: 'Sobres y etiquetas',
    sheet: 'Insectary_data',
    keys: ['Insectary_ID'],
    newRows: false,
    fields: ['Insectary_ID', 'SPECIES', 'Sex', 'CAM_ID', 'Tube_1_id'],
  },
  crispr: {
    label: 'CRISPR',
    sheet: 'CRISPR',
    keys: ['CRISPR_No.', 'Eggs_No.'],
    newRows: true,
    fields: [
      'CRISPR_No.',
      'Eggs_No.',
      'CRISPR_date',
      'Guide',
      'Stock_of_origin',
      'Hatch_date',
      'Pupa_date',
      'Emerge_date',
      'Mutant',
      'CAM_ID',
      'Notes',
    ],
  },
};
export const KIND_IDS = Object.keys(KINDS);
/** Every column a notebook fills: the skill's ones, then the extra ones. */
export const columnsOf = kind => [...kind.fields, ...(kind.extra ?? [])];

/** "Mechanitis lysimnia (F1)": the generation written with the species. */
const GENERATION = /\(\s*(F1|F2|BC|backcross)\s*\)|\s(F1|F2)\s*$/i;
/**
 * A generation written after the species goes to the Generation column (F1,
 * F2, Backcross), unless the line gives Generation itself; the species loses it.
 */
export function generationFromSpecies(text) {
  const species = text.SPECIES;
  const m = typeof species === 'string' ? GENERATION.exec(species) : null;
  if (!m) return text;
  const g = (m[1] ?? m[2]).toUpperCase();
  const rest = species.replace(m[0], ' ').replace(/\s+/g, ' ').trim();
  if (rest) text.SPECIES = rest;
  else delete text.SPECIES;
  if (isNone(text.Generation)) text.Generation = g === 'BC' || g === 'BACKCROSS' ? 'Backcross' : g;
  return text;
}

/** What a count kept as a sum adds up to (27 for "=12+15"), or null. */
const sumTotal = value => {
  const formula = typeof value === 'string' ? simpleSum(value) : null;
  return formula ? formula.slice(1).split('+').reduce((sum, term) => sum + Number(term), 0) : null;
};

const clip = (value, length) => String(value ?? '').slice(0, length);

/**
 * A page as Claude transcribed it (the match_notebook tool) in the form the
 * review reads: one entry per notebook line with its values, the confidence of
 * the doubtful cells and their other readings. Unknown columns are dropped and
 * reported; a cell with alternatives but no confidence counts as doubtful.
 */
export function checkTranscription({ kind, year = null, lines = [] }) {
  if (!KINDS[kind]) throw new Error(`Unknown notebook kind "${clip(kind, 40)}": use one of ${KIND_IDS.join(', ')}`);
  if (!Array.isArray(lines) || !lines.length) throw new Error('Give the lines of the page');
  const columns = columnsOf(KINDS[kind]);
  const fields = new Set(columns);
  // A column named as the notebook names it ("dissections") or in other case is the sheet's.
  const names = new Map([
    ...columns.map(f => [f.toLowerCase(), f]),
    ...Object.entries(KINDS[kind].aliases ?? {}).map(([alias, f]) => [alias.toLowerCase(), f]),
  ]);
  const rename = entries =>
    Object.fromEntries(Object.entries(entries ?? {}).map(([f, v]) => [fields.has(f) ? f : (names.get(String(f).trim().toLowerCase()) ?? f), v]));
  const ignored = new Set();
  const number = (value, low, high) => {
    const n = Number(value);
    return Number.isFinite(n) ? Math.min(high, Math.max(low, n)) : null;
  };
  const out = lines.slice(0, 150).map((line, i) => {
    const v = {},
      c = {},
      a = {};
    for (const [field, value] of Object.entries(rename(line?.values))) {
      if (!fields.has(field)) {
        ignored.add(field);
        continue;
      }
      v[field] = value === null || value === undefined ? null : clip(value, 300).trim();
      if (v[field] === null) c[field] = 0;
    }
    for (const [field, value] of Object.entries(rename(line?.alternatives))) {
      if (!fields.has(field)) continue;
      const options = (Array.isArray(value) ? value : [value])
        .filter(x => x !== null && x !== undefined)
        .map(x => clip(x, 120).trim())
        .filter(x => x && x !== v[field]);
      if (options.length) {
        a[field] = [...new Set(options)].slice(0, 3);
        c[field] = 0.5;
      }
    }
    for (const [field, value] of Object.entries(rename(line?.confidence))) {
      const n = number(value, 0, 1);
      if (fields.has(field) && n !== null) c[field] = n;
    }
    return {
      n: Number.isInteger(line?.n) && line.n > 0 ? line.n : i + 1,
      y: null,
      raw: clip(line?.raw, 300),
      crossed: Boolean(line?.crossedOut),
      v,
      c,
      a,
    };
  });
  const y = Number(year);
  return {
    transcription: {
      kind,
      rotate: 0,
      year: Number.isInteger(y) && y >= 1990 && y < 2100 ? y : null,
      lines: out,
    },
    ignored: [...ignored],
  };
}

// ---------------------------------------------------------------------------
// Values: what a notebook cell means in the sheet's terms.

const DATE_FIELD = /(?:^|[\s_])date(?:[\s_]|$)|^date|date$/i;
const NUMBER_FIELD = /^(NUMBER OF|Eggs_No\.|CRISPR_No\.)/;
export const typeOf = field => (DATE_FIELD.test(field) ? 'date' : NUMBER_FIELD.test(field) ? 'number' : 'text');
const NONE = /^\s*(|NA|N\/A|—|–|-|none)\s*$/i;
export const isNone = value => value === null || value === undefined || NONE.test(String(value));
const SERIAL_EPOCH = Date.UTC(1899, 11, 30);
export const serialYear = serial => new Date(SERIAL_EPOCH + serial * 864e5).getUTCFullYear();
const serialDayMonth = serial => new Date(SERIAL_EPOCH + serial * 864e5).toISOString().slice(5, 10);

/** "994 (7)", "994(7)" and 994 are the same clutch. */
export const clutchKey = value =>
  String(value ?? '')
    .toLowerCase()
    .replace(/\s+/g, '')
    .replace(/^0+(?=\d)/, '');
export const textKey = value =>
  String(value ?? '')
    .toLowerCase()
    .normalize('NFD')
    .replace(/[̀-ͯ]/g, '')
    .replace(/[.\s]+$/g, '')
    .replace(/\s+/g, ' ')
    .trim();

/**
 * A notebook date: "17/9" (day/month, the page's year), "19-6-23", "4/8/2025",
 * or what the grid shows ("17-Sep-25", 2025-09-17). Returns a Sheets serial,
 * whether the year was written, or null.
 */
export function readDate(text, year) {
  const s = String(text ?? '').trim();
  if (!s) return null;
  const short = /^(\d{1,2})\s*[/.\-]\s*(\d{1,2})$/.exec(s);
  if (short) {
    const serial = parseDateText(`${short[1]}/${short[2]}/${year}`);
    return serial === null ? null : { serial, yearWritten: false };
  }
  const serial = parseDateText(s.replace(/\s+/g, ''));
  return serial === null ? null : { serial, yearWritten: true };
}

/**
 * The terms of a count written as a sum: "12+15", "12 + 15", "=12+15", "27-5";
 * a worked sum "2+4=6+8=14" (a running total, then more terms) gives 2, 4, 8.
 * Null when the text is not a sum, or its steps do not add up.
 */
export function sumTerms(text) {
  const s = String(text ?? '').replace(/\s+/g, '').replace(/^=/, '');
  if (!/^\d+(?:[+-]\d+)*(?:=\d+(?:[+-]\d+)*)*$/.test(s)) return null;
  const parts = s.split('=').map(p => p.match(/[+-]?\d+/g).map(Number));
  let terms = parts[0];
  for (const part of parts.slice(1)) {
    const total = terms.reduce((a, b) => a + b, 0);
    if (part[0] !== total) return null;
    // The last step may be the total alone ("…=14").
    terms = [...terms, ...part.slice(1)];
  }
  return terms;
}
const formulaOf = terms => `=${terms.map((t, i) => (i && t >= 0 ? `+${t}` : String(t))).join('')}`;

/** The value a notebook cell gives a column, as the sheet stores it, or an error. */
export function readValue(field, text, { year, sheet = null }) {
  // A dash or NA written in a text column is the sheet's "NA" (e.g. no stock of origin, no CAM);
  // in dates, counts and notes it just means nothing to write.
  if (isNone(text))
    return { value: typeOf(field) === 'text' && !/^Notes|^NOTES$/.test(field) && String(text ?? '').trim() ? 'NA' : null };
  const s = String(text).trim();
  const type = typeOf(field);
  if (type === 'date') {
    const date = readDate(s, year);
    return date ? { value: date.serial, yearWritten: date.yearWritten } : { value: s, error: `Fecha no legible: «${s}»` };
  }
  if (field === 'CLUTCH NUMBER' || field === 'CRISPR_No.' || field === 'Eggs_No.') {
    const compact = s.replace(/\s+/g, '');
    return { value: /^\d+$/.test(compact) ? Number(compact) : compact };
  }
  if (type === 'number') {
    const terms = sumTerms(s);
    // Stock counts are typed as formulas keeping the notebook's terms (=12+15, =19).
    // Stock counts are kept as the notebook sums them (=12+15, =19); elsewhere the total.
    if (terms) return { value: (isSumField(sheet, field) && simpleSum(formulaOf(terms))) || terms.reduce((a, b) => a + b, 0) };
    // A worked sum whose steps do not add up counts what follows the last "=".
    const total = /^[\d\s+\-=]*=\s*(\d+)$/.exec(s);
    if (total) return { value: Number(total[1]) };
    return { value: /^\d+$/.test(s) ? Number(s) : s };
  }
  if (field === 'Sex') {
    const sex = { '♀': 'female', '♂': 'male', f: 'female', h: 'female', m: 'male' }[s.toLowerCase()];
    return { value: sex ?? s };
  }
  if (field === 'CAM_ID') {
    const m = /^cam\s*0*(\d{1,6})$/i.exec(s);
    return { value: m ? `CAM${m[1].padStart(6, '0')}` : s.toUpperCase() };
  }
  if (/^Tube_\d_id$/.test(field) || field === 'Insectary_ID') return { value: s.replace(/\s+/g, '').toUpperCase() };
  return { value: s };
}

/** Whether a notebook value and the sheet's say the same (format differences are not differences). */
export function sameValue(field, sheet, notebook) {
  if (isNone(sheet) && isNone(notebook)) return true;
  if (isNone(sheet) || isNone(notebook)) return false;
  const type = typeOf(field);
  if (type === 'date') return typeof sheet === 'number' && typeof notebook === 'number' && sheet === notebook;
  if (/CLUTCH|_No\./.test(field)) return clutchKey(sheet) === clutchKey(notebook);
  // Counts kept as sums: two sums compare their terms (=12+15 is not =14+13); a sum and a number, their total.
  if (type === 'number' && (sumTotal(sheet) !== null || sumTotal(notebook) !== null)) {
    if (sumTotal(sheet) !== null && sumTotal(notebook) !== null) return simpleSum(sheet) === simpleSum(notebook);
    return Number(sumTotal(sheet) ?? sheet) === Number(sumTotal(notebook) ?? notebook);
  }
  if (type === 'number' && Number.isFinite(Number(sheet)) && Number.isFinite(Number(notebook)))
    return Number(sheet) === Number(notebook);
  if (/^Notes|^NOTES$/.test(field)) return textKey(sheet).includes(textKey(notebook));
  return textKey(sheet) === textKey(notebook);
}

// ---------------------------------------------------------------------------
// The review: every notebook line against its sheet row.

const DOUBT = 0.8;

/** "d/m/yy INI: text", the form notes take in the workbook. */
export function noteText(text, { today, initials }) {
  const [y, m, d] = String(today).split('-');
  return `${Number(d)}/${Number(m)}/${String(y).slice(2)} ${initials}: ${text}`;
}

/**
 * The page's year when it is not written on a date: the one of the matching
 * sheet dates with the same day and month, else the commonest year among the
 * matched rows' dates in the page's columns, else the current year.
 */
function inferYear(lines, dateFields, fallback) {
  const exact = new Map(),
    near = new Map();
  const add = (map, year) => map.set(year, (map.get(year) ?? 0) + 1);
  for (const line of lines) {
    const record = line.record;
    if (!record) continue;
    for (const field of dateFields) {
      const sheet = record.values?.[field];
      if (typeof sheet !== 'number') continue;
      add(near, serialYear(sheet));
      const m = /^(\d{1,2})\s*[/.\-]\s*(\d{1,2})$/.exec(String(line.text[field] ?? '').trim());
      if (m && serialDayMonth(sheet) === `${m[2].padStart(2, '0')}-${m[1].padStart(2, '0')}`)
        add(exact, serialYear(sheet));
    }
  }
  const best = map => [...map].sort((a, b) => b[1] - a[1])[0]?.[0];
  return best(exact) ?? best(near) ?? fallback;
}

/**
 * CAMs and tubes written as runs: "cam505" or "72" under CAM076671 continue its
 * number (CAM076505, CAM076672), as "81" under FS50851380 is FS50851381.
 * Fills them in place; returns { line index: { field: as written } }.
 */
export function completeRuns(texts) {
  const done = {};
  const last = {};
  texts.forEach((text, i) => {
    for (const field of Object.keys(text)) {
      if (field !== 'CAM_ID' && !/^Tube_\d_id$/.test(field)) continue;
      const raw = String(text[field] ?? '').trim();
      const short = field === 'CAM_ID' ? /^(?:cam\s*)?(\d{1,4})$/i.exec(raw) : /^(\d{1,4})$/.exec(raw);
      const full = field === 'CAM_ID' ? /^cam\s*0*(\d{5,6})$/i.exec(raw) : /^[A-Z]{2}\d{8}$/i.exec(raw.replace(/\s+/g, ''));
      if (short && last[field]) {
        const prev = last[field];
        text[field] = prev.slice(0, prev.length - short[1].length) + short[1];
        (done[i] ??= {})[field] = raw;
        last[field] = text[field];
      } else if (full) last[field] = field === 'CAM_ID' ? `CAM${full[1].padStart(6, '0')}` : raw.replace(/\s+/g, '').toUpperCase();
    }
  });
  return done;
}

const LOOK_DIGIT = { O: '0', I: '1', L: '1', S: '5', B: '8', Z: '2', G: '6' };
const LOOK_LETTER = { 0: 'O', 1: 'I', 5: 'S', 8: 'B', 2: 'Z', 6: 'G' };
/** Insectary IDs that look like the one read (600 → 6OO, 10P → 1OP, 5OS ↔ 50S). */
export function lookAlikes(kind, keyValues) {
  if (kind.keys.length !== 1 || kind.keys[0] !== 'Insectary_ID') return [];
  const id = String(keyValues[0] ?? '').toUpperCase();
  if (id.length < 2 || id.length > 5) return [];
  let out = [''];
  for (const ch of id) {
    const options = [...new Set([ch, LOOK_DIGIT[ch], LOOK_LETTER[ch]].filter(Boolean))];
    out = out.flatMap(prefix => options.map(o => prefix + o)).slice(0, 64);
  }
  return out.filter(v => v !== id).map(v => [v]);
}

/** How many of a line's cells the sheet row already has (a date counts by day and month). */
function agreement(kind, record, text, year) {
  let same = 0;
  for (const field of columnsOf(kind)) {
    if (kind.keys.includes(field)) continue;
    const value = readValue(field, text[field], { year }).value;
    const before = record.values?.[field];
    if (isNone(value) || isNone(before)) continue;
    if (typeOf(field) === 'date') same += typeof value === 'number' && typeof before === 'number' && serialDayMonth(value) === serialDayMonth(before) ? 1 : 0;
    else same += sameValue(field, before, value) ? 1 : 0;
  }
  return same;
}

/**
 * Picks each line's row among its candidates. Consecutive notebook lines are
 * usually consecutive sheet rows (IDs are made in advance, in order), so the
 * choice that keeps the page's rows in step wins, then the one that agrees with
 * more cells, then the key as read. A line whose best rows tie is left for the person.
 */
function chooseRows(items) {
  const chain = items.filter(i => i.candidates.length && !i.line.crossed);
  const own = c => c.same + (c.exact ? 0.5 : 0);
  const step = (a, b, ca, cb) => (cb.record.row - ca.record.row === b.line.n - a.line.n ? 3 : 0);
  let previous = null;
  for (const item of chain) {
    item.best = item.candidates.map(c => {
      if (!previous) return { c, score: own(c), from: null };
      let from = null,
        score = -Infinity;
      for (const p of previous.best) {
        const s = p.score + step(previous, item, p.c, c);
        if (s > score) [score, from] = [s, p];
      }
      return { c, score: own(c) + score, from };
    });
    previous = item;
  }
  if (!previous) return;
  let node = previous.best.reduce((a, b) => (b.score > a.score ? b : a));
  for (let i = chain.length - 1; i >= 0 && node; i--) {
    chain[i].choice = node.c;
    node = node.from;
  }
  chain.forEach((item, i) => {
    if (item.candidates.length === 1) return void (item.record = item.candidates[0].record);
    // Several rows: the chosen one must do better here than any other, given its neighbours' choice.
    const before = chain[i - 1],
      after = chain[i + 1];
    const local = c =>
      own(c) + (before?.choice ? step(before, item, before.choice, c) : 0) + (after?.choice ? step(item, after, c, after.choice) : 0);
    const mine = local(item.choice);
    if (item.candidates.every(c => c === item.choice || local(c) < mine)) item.record = item.choice.record;
    item.readAs = item.record && !item.choice.exact ? item.keyValues.join(' ') : null;
  });
  for (const item of items) if (item.record && item.candidates.length === 1 && !item.candidates[0].exact) item.readAs = item.keyValues.join(' ');
}

/**
 * Compares a transcribed page with the sheet.
 *
 * lookup: {
 *   find(values) → records matching the line's key columns (sheet rows),
 *   clutch(text) → the clutch number as written in Insectary_stocks, or null,
 *   speciesOfClutch(clutch) → the species the SPECIES formula gives for a clutch,
 *   list(field) → { strict, values: Set } for dropdown columns,
 *   holder(field, value, recordId) → another row using this unique ID, or null,
 *   newRowFormulas: Set of columns that are formulas in a new row,
 *   typedOverFormula: Set of formula columns that may be typed over (SPECIES),
 * }
 * edits: { [line]: { [field]: text | null } } typed by the person (they win and are trusted).
 * picks: { [line]: boolean } rows the person ticked or unticked.
 */
export function buildReview({ transcription, edits = {}, picks = {}, year = null, today, initials = 'APP', lookup }) {
  const kind = KINDS[transcription.kind];
  const currentYear = Number(String(today).slice(0, 4));
  const todaySerial = parseDateText(today);
  const dateFields = columnsOf(kind).filter(f => typeOf(f) === 'date');

  // First the keys, to find each line's row (a corrected key finds another row).
  const texts = transcription.lines.map(line => {
    const text = { ...line.v };
    for (const [field, value] of Object.entries(edits[line.n] ?? {})) if (columnsOf(kind).includes(field)) text[field] = value;
    // "lys (F1)": the generation goes to its column, where the sheet has one.
    return columnsOf(kind).includes('Generation') ? generationFromSpecies(text) : text;
  });
  const completed = completeRuns(texts);
  const lines = transcription.lines.map((line, i) => {
    const edited = edits[line.n] ?? {};
    const text = texts[i];
    const keyValues = kind.keys.map(k => readValue(k, text[k], { year: currentYear }).value);
    const readable = keyValues.every(v => v !== null && v !== '');
    // The rows with this key, and with the look-alike keys (6OO read as 600: 0/O, 1/I, 5/S, 8/B).
    const candidates = [];
    if (readable) {
      for (const record of lookup.find(keyValues)) candidates.push({ record, exact: true });
      for (const variant of lookAlikes(kind, keyValues))
        for (const record of lookup.find(variant))
          if (!candidates.some(c => c.record.id === record.id)) candidates.push({ record, exact: false });
    }
    for (const c of candidates) c.same = agreement(kind, c.record, text, currentYear);
    return { line, edited, text, keyValues, candidates, record: null };
  });
  chooseRows(lines);
  const pageYear = year ?? transcription.year ?? inferYear(lines, dateFields, currentYear);
  const yearSource = year ? 'person' : transcription.year ? 'page' : 'inferred';

  // The same key on two lines of the page (as matched: 600 and 6OO are the same butterfly).
  const keyOf = item =>
    (item.record ? kind.keys.map(k => item.record.values?.[k]) : item.keyValues).map(clutchKey).join('|');
  const seen = new Map();
  for (const item of lines) {
    if (item.line.crossed || item.keyValues.some(v => v === null)) continue;
    seen.set(keyOf(item), [...(seen.get(keyOf(item)) ?? []), item.line.n]);
  }

  const out = lines.map((item, i) => {
    const { line, edited, text } = item;
    const twins = (seen.get(keyOf(item)) ?? []).filter(n => n !== line.n);
    const record = item.record;
    let status = line.crossed
      ? 'crossed'
      : item.keyValues.some(v => v === null || v === '')
        ? 'nokey'
        : twins.length
          ? 'duplicate'
          : record
            ? 'match'
            : item.candidates.length > 1
              ? 'ambiguous'
              : kind.newRows
                ? 'new'
                : 'missing';
    const message = {
      crossed: 'Tachada en el cuaderno: no se usa',
      nokey: `Sin ${kind.keys.join(' + ')} legible: escríbelo para buscar la fila`,
      duplicate: `El mismo ${kind.keys.join(' + ')} está también en la línea ${twins.join(', ')}`,
      ambiguous: `${item.candidates.length} filas de la hoja podrían ser esta (filas ${item.candidates.map(c => `${c.record.row} ${c.record.label ?? ''}`.trim()).join(', ')}): escribe el ${kind.keys.join(' + ')} correcto`,
      missing: `${item.keyValues.join(' ')} no está en ${kind.sheet}: ¿está bien leído?`,
      new: `Fila nueva en ${kind.sheet}`,
      // Read as a look-alike (600 for 6OO): the row was found by the others around it.
      match: item.readAs ? `Leído «${item.readAs}»; en la hoja es ${kind.keys.map(k => record.values?.[k]).join(' ')}` : '',
    }[status];
    const usable = status === 'match' || status === 'new';
    // A clutch changed on the page changes what the SPECIES formula will give.
    const clutchText = text['CLUTCH NUMBER'];
    const cells = {};
    let lastDate = null;
    for (const field of columnsOf(kind)) {
      const typed = field in edited;
      const confidence = typed ? 1 : (line.c[field] ?? (line.v[field] === null && field in line.v ? 0 : 1));
      const read = readValue(field, text[field], { year: pageYear, sheet: kind.sheet });
      let value = read.value;
      let error = read.error ?? null;
      // A count kept as a sum is compared (and shown) as its formula: =12+15.
      const sheetSum = isSumField(kind.sheet, field) ? simpleSum(record?.formulas?.[field]) : null;
      const before = record ? (sheetSum ?? record.values?.[field] ?? null) : null;
      // The key as the sheet writes it (6OO, not 600), once the row is found.
      if (kind.keys.includes(field) && record && status === 'match') value = before;
      // A day/month the sheet has in another year is the same date (the year was only inferred).
      if (typeOf(field) === 'date' && typeof value === 'number' && !read.yearWritten && typeof before === 'number') {
        if (serialDayMonth(before) === serialDayMonth(value)) value = before;
        else if (!year && value > todaySerial + 7) value = readValue(field, text[field], { year: pageYear - 1 }).value;
      } else if (typeOf(field) === 'date' && typeof value === 'number' && !read.yearWritten && !year && value > todaySerial + 7)
        value = readValue(field, text[field], { year: pageYear - 1 }).value;
      // The columns follow the stages (laid, hatched, pupa, emerged; emerged, died): a date without
      // its year that falls well before the one before it is in the next year (laid in December, emerged in January).
      if (typeOf(field) === 'date' && typeof value === 'number') {
        if (!read.yearWritten && lastDate !== null && value < lastDate - 30 && value !== before) {
          const next = readValue(field, text[field], { year: serialYear(value) + 1 }).value;
          if (typeof next === 'number' && next <= todaySerial + 7) value = next;
        }
        lastDate = value;
      }
      if (field === 'CLUTCH NUMBER' && kind.sheet !== 'Insectary_stocks' && !isNone(value)) {
        // Written as in Insectary_stocks (its list is strict): "685 (3)", not "685(3)".
        const known = lookup.clutch?.(value);
        if (known !== null && known !== undefined) value = known;
        else error ??= `El clutch ${value} no está en Insectary_stocks`;
      }
      // A shortened list value is completed when only one fits ("interme" → intermedia).
      const list = lookup.list?.(field);
      let unlisted = false;
      if (list && typeof value === 'string' && !isNone(value) && !list.values.has(value)) {
        const key = textKey(value);
        const hits = [...list.values].filter(o => textKey(o).startsWith(key));
        const exact = hits.find(o => textKey(o) === key);
        // A full name where the list holds its last word (Stock_of_origin: "messenoides").
        const tail = [...list.values].filter(o => textKey(o).length > 2 && key.endsWith(` ${textKey(o)}`));
        if (exact) value = exact;
        else if (hits.length === 1 && key.length >= 3) value = hits[0];
        else if (tail.length === 1) value = tail[0];
        else unlisted = true;
      }
      const alternatives = (line.a[field] ?? [])
        .map(a => readValue(field, a, { year: pageYear, sheet: kind.sheet }).value)
        .filter(a => !isNone(a) && a !== value);
      const cell = {
        // An explicit NA (from a dash in a text column) is kept; other "none" readings are nothing.
        value: value === 'NA' ? 'NA' : isNone(value) ? null : value,
        before,
        status: 'empty',
        confidence,
        // A value outside a list that is not strict (a species name) is shown but not written until confirmed.
        doubt: !typed && (confidence < DOUBT || (unlisted && !list?.strict)),
        alternatives: [...new Set(alternatives)],
        edited: typed,
        include: false,
        formula: false,
        message:
          error ??
          (unlisted && !list?.strict && !typed
            ? `«${value}» no está en la lista de ${field}`
            : completed[i]?.[field]
              ? `Escrito «${completed[i][field]}»: sigue el número de la línea de arriba`
              : kind.keys.includes(field) && item.readAs && record
                ? `Leído «${item.readAs}»`
                : null),
      };
      const isKey = kind.keys.includes(field);
      const sumField = isSumField(kind.sheet, field);
      const formulaHere = record ? Boolean(record.formulas?.[field]) : lookup.newRowFormulas?.has(field);
      if (line.v[field] === null && field in line.v && !typed) cell.status = 'unread';
      else if (cell.value === null) cell.status = isNone(before) ? 'empty' : 'keep';
      else if (status === 'new') cell.status = 'new';
      else if (!record) cell.status = 'empty';
      else if (field === 'SPECIES' && record.formulas?.SPECIES) {
        // The formula predicts the species from the clutch: type one only when what emerged
        // differs, from the clutch the row will have (the page may correct it).
        const nextClutch = readValue('CLUTCH NUMBER', clutchText, { year: pageYear }).value;
        const predicted =
          nextClutch !== null && !sameValue('CLUTCH NUMBER', record.values?.['CLUTCH NUMBER'], nextClutch)
            ? (lookup.speciesOfClutch?.(lookup.clutch?.(nextClutch) ?? nextClutch) ?? before)
            : before;
        cell.formula = true;
        // A formula that gives nothing (a wild butterfly, no clutch) is filled with the typed species.
        cell.status = sameValue(field, predicted, cell.value) ? 'same' : isNone(predicted) ? 'fill' : 'conflict';
        if (cell.status !== 'same')
          cell.message ??= `La fórmula da «${predicted ?? 'vacío'}»; se escribirá encima`;
      } else if (cell.value === 'NA' && (before === null || before === undefined || before === '')) cell.status = 'fill';
      else if (sameValue(field, before, cell.value)) cell.status = 'same';
      // A count still at the new row's =0 is not filled in yet.
      else if (isNone(before) || (sumField && record.formulas?.[field] === '=0')) cell.status = 'fill';
      else if (/^Notes|^NOTES$/.test(field)) cell.status = 'fill';
      else cell.status = 'conflict';
      if (isKey && status === 'match') cell.status = 'same';
      const formula = record?.formulas?.[field];
      if (['fill', 'conflict', 'new'].includes(cell.status) && formulaHere && !(field === 'SPECIES' && cell.formula)) {
        // A count typed as a sum (=12+15) is replaced by the notebook's sum; other formulas are kept.
        const allowed =
          (lookup.typedOverFormula?.has(field) && record) ||
          (sumField && (!record || sheetSum !== null) && (typeof cell.value === 'number' || simpleSum(cell.value) !== null));
        if (!allowed) {
          // Not written (the app never replaces a formula), but a sum that disagrees with the
          // notebook is pointed out, to correct in Google Sheets.
          const typedSum = typeof formula === 'string' && /^=[\d\s+\-*/().]+$/.test(formula);
          cell.status = 'formula';
          cell.mismatch = Boolean(record) && !isNone(before);
          cell.message ??= typedSum
            ? `La hoja tiene ${formula} (${show(field, before)}); el cuaderno dice ${show(field, cell.value)}: corrígelo en Google Sheets`
            : record
              ? 'Columna con fórmula en la hoja: no se escribe'
              : 'Columna con fórmula en las filas nuevas: no se escribe';
        }
      }
      // What the save would refuse: a value outside a strict list, an ID used by another row, an unreadable date.
      if (['fill', 'conflict', 'new'].includes(cell.status)) {
        if (!error && list?.strict && !list.values.has(String(cell.value).trim()))
          error = `«${cell.value}» no está en la lista de ${field}`;
        const holder = error ? null : lookup.holder?.(field, cell.value, record?.id ?? null);
        if (holder) error = `${cell.value} ya está en ${holder.sheet} fila ${holder.row}`;
        if (error) Object.assign(cell, { status: 'error', message: error });
      }
      // Notes are written with the date and initials, after what the cell already holds.
      if (/^Notes|^NOTES$/.test(field) && ['fill', 'new'].includes(cell.status)) {
        const note = noteText(cell.value, { today, initials });
        cell.write = isNone(before) ? note : `${before} | ${note}`;
      }
      cell.include = usable && ['fill', 'conflict', 'new'].includes(cell.status) && !cell.doubt;
      cells[field] = cell;
    }
    const changes = Object.values(cells).filter(c => c.include).length;
    const picked = usable && (picks[line.n] ?? changes > 0);
    return {
      n: line.n,
      y: line.y,
      raw: line.raw,
      crossed: line.crossed,
      status,
      message,
      recordId: record?.id ?? null,
      row: record?.row ?? null,
      version: record?.version ?? null,
      label: record?.label ?? item.keyValues.join(' '),
      cells,
      changes,
      picked: picked && changes > 0,
    };
  });

  const count = test => out.reduce((n, l) => n + Object.values(l.cells).filter(test).length, 0);
  return {
    kind: transcription.kind,
    sheet: kind.sheet,
    keys: kind.keys,
    fields: columnsOf(kind),
    year: pageYear,
    yearSource,
    rotate: transcription.rotate,
    lines: out,
    counts: {
      lines: out.length,
      rows: out.filter(l => l.changes).length,
      fills: count(c => c.include && c.status === 'fill'),
      conflicts: count(c => c.status === 'conflict'),
      doubts: count(c => c.doubt && ['fill', 'conflict', 'new'].includes(c.status)),
      errors: count(c => c.status === 'error') + out.filter(l => ['missing', 'ambiguous', 'duplicate', 'nokey'].includes(l.status)).length,
      created: out.filter(l => l.status === 'new' && l.changes).length,
      same: count(c => c.status === 'same'),
    },
  };
}

/**
 * The ticked rows of a review in propose_changes form: `changes` for rows in
 * the sheet, `newRows` for new ones. Each carries its notebook line.
 */
export function proposalRows(review) {
  const changes = [],
    newRows = [];
  for (const line of review.lines) {
    if (!line.picked || !line.changes) continue;
    const values = {};
    const notes = [];
    for (const [field, cell] of Object.entries(line.cells)) {
      if (!cell.include) continue;
      values[field] = cell.write ?? cell.value;
      if (cell.status === 'conflict') notes.push(`${field}: hoja ${show(field, cell.before)} → cuaderno ${show(field, cell.value)}`);
    }
    // Doubtful readings stay out of the proposal, but the row says so.
    for (const [field, cell] of Object.entries(line.cells))
      if (cell.doubt && ['fill', 'conflict', 'new'].includes(cell.status))
        notes.push(`${field} dudoso: ${[cell.value, ...cell.alternatives].map(v => show(field, v)).join(' / ')} (no incluido)`);
    const note = clip([`Línea ${line.n}: «${line.raw}»`, ...notes].join(' · '), 300);
    if (line.status === 'new') newRows.push({ sheet: review.sheet, values, note, line: line.n });
    else changes.push({ recordId: line.recordId, values, note, line: line.n });
  }
  return { changes, newRows };
}

const MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
/** A value as the grids show it (dates as 14-Aug-25). */
export function show(field, value) {
  if (value === null || value === undefined || value === '') return 'vacío';
  if (typeOf(field) === 'date' && typeof value === 'number') {
    const d = new Date(SERIAL_EPOCH + value * 864e5);
    return `${d.getUTCDate()}-${MONTHS[d.getUTCMonth()]}-${String(d.getUTCFullYear()).slice(2)}`;
  }
  return String(value);
}
