// Reading a photographed notebook page: the prompt that asks the AI for one
// entry per notebook line (values, confidence, alternatives, position), the
// parser for its answer, and the comparison of each line with the sheet that
// turns the page into a proposal. Pure functions: the sheet is reached
// through the `lookup` given to buildReview (server/notebook-jobs.mjs).

import { parseDateText } from './schema.mjs';

/** The notebooks that can be digitized, and the sheet columns each one fills. */
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
    looks:
      'the clutch (stocks) notebook: one line per clutch of eggs; headers like Clutch/#, Species, Date laid/Fecha, # Eggs/Huevos, Hatch date/Eclosión, # Larvae, Pupa date, # Pupae, Emerge date, # Adults, Notes',
    hints: [
      'CLUTCH NUMBER as written: 994, or 994(7) for another batch from the same couple.',
      'Counts are whole numbers. INSECTARY OR LABORATORY is Insectary or Laboratory (Ins./Lab.).',
    ],
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
    looks:
      'the insectary butterfly notebook: one line per butterfly; headers like # | ID | Species | Sex | # Clutch | Stock origin | Emerge date | Dead date | Notes (highlighted lines are usually dead butterflies)',
    hints: [
      'The first "#" column is a running count (e.g. 3096): ignore it. Insectary_ID is the butterfly ID written on its wing: a digit then letters (5VB, 0NX) or letters then digits (H79, V36).',
      'Intro2Insectary_date is the emerge date; Death_date the dead date.',
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
    looks:
      'the daily round of dead butterflies: one line per dead butterfly; headers like Date | ID | Species | Sex | Cause | CAM | Notes',
    hints: [
      'Insectary_ID is the butterfly ID written on its wing (5VB, H79). A date written once above several lines applies to all of them.',
    ],
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
    looks:
      'the CRISPR notebook: one line per injected egg; headers like CRISPR | # Eggs | CRISPR date | Guide | Specie | Hatch date | Pupa date | Emerge date | Mutant yes/no | CAM ID | Notes',
    hints: [
      'CRISPR_No. is the experiment number (e.g. 50), Eggs_No. the egg number (1, 2, 3…). The "Specie" column is Stock_of_origin (e.g. Inter = Mechanitis messenoides intermedia).',
      'Guide as written (2B, 2A-2D, No guide). Mutant: Yes, No or Check.',
    ],
  },
};
export const KIND_IDS = Object.keys(KINDS);

const SPECIES_HINT =
  '`messen.`/`messenoid` = Mechanitis messenoides messenoides, `interm.`/`inter` = Mechanitis messenoides intermedia, `decept` = Mechanitis messenoides deceptus, `pol. p.`/`polymnia p.` = Mechanitis polymnia proceriformis, `pol. e.`/`eurydice` = Mechanitis polymnia eurydice, `wer x pro` = Mechanitis polymnia werneri x proceriformis, `zaneka` = Melinaea menophilus zaneka, `mothone` = Melinaea mothone, `lysimnia` = Mechanitis lysimnia, `hibrido` = hybrid (zaneka x menophilus).';

/** One line of the answer, per notebook, so the example never names another notebook's columns. */
const EXAMPLES = {
  stocks: {
    raw: '994(7) lys 20/9 12',
    x: false,
    v: { 'CLUTCH NUMBER': '994(7)', SPECIES: 'Mechanitis lysimnia', 'DATE LAID': '20/9', 'NUMBER OF EGGS': '12' },
    c: { 'NUMBER OF EGGS': 0.6 },
    a: { 'NUMBER OF EGGS': ['17'] },
  },
  emergence: {
    raw: '5VB decept ♀ 838 interm. 4/8',
    x: false,
    v: { Insectary_ID: '5VB', SPECIES: 'Mechanitis messenoides deceptus', Sex: 'female' },
    c: { Sex: 0.6 },
    a: { Sex: ['male'] },
  },
  deaths: {
    raw: '17/9 5VB decept ♀ unk',
    x: false,
    v: { Insectary_ID: '5VB', Death_date: '17/9', Death_cause: 'Unknown' },
    c: { Insectary_ID: 0.6 },
    a: { Insectary_ID: ['5VD'] },
  },
  crispr: {
    raw: '50 9 19-6-23 2B Inter 27/6',
    x: false,
    v: { 'CRISPR_No.': '50', 'Eggs_No.': '9', CRISPR_date: '19-6-23', Guide: '2B', Hatch_date: '27/6' },
    c: { Hatch_date: 0.6 },
    a: { Hatch_date: ['29/6'] },
  },
};

export const SYSTEM_PROMPT =
  'You transcribe photographed pages of the Ikiam insectary notebooks (Tena, Ecuador) into JSON for a database. You read handwriting carefully and never guess: a doubtful reading gets a low confidence and alternatives. Reply with the JSON object only: no prose, no code fences.';

/**
 * The instructions sent with the photo. `kind` 'auto' describes every notebook
 * and lets the model say which one it sees (from the headers).
 */
export function transcriptionPrompt({ kind = 'auto', species = [], lists = {}, today = '' } = {}) {
  const kinds = kind === 'auto' ? KIND_IDS : [kind];
  const parts = [
    `Today is ${today}. Transcribe this notebook page.`,
    kind === 'auto'
      ? 'First decide from the headers and content which notebook it is:'
      : `It is ${KINDS[kind].looks.replace(/^the /, 'the ')}.`,
  ];
  for (const id of kinds) {
    const k = KINDS[id];
    parts.push(
      `- kind "${id}": ${k.looks}. Columns (keys of "v"): ${k.fields.map(f => JSON.stringify(f)).join(', ')}. ${k.hints.join(' ')}`,
    );
  }
  const listLines = Object.entries(lists)
    .filter(([, values]) => values?.length && values.length <= 40)
    .map(([field, values]) => `${field}: ${values.join(' | ')}`);
  parts.push(
    '',
    'Rules:',
    '- One entry per written line of the table, top to bottom. Skip lines that hold only a pre-written ID or number and nothing else.',
    '- Crossed-out lines, or lines marked "no se usó el ID": include them with "x": true.',
    '- Repeat marks (", ll, 〃, ||, a wavy line down a column) and a brace } spanning lines: write the repeated value in every line it covers.',
    '- `—` or `-` alone means none: write "NA".',
    '- Dates exactly as written, day first: "17/9", "4-8", "19-6-23". Do not convert them. A date written once for several lines applies to all of them.',
    '- Sex: ♀ = "female", ♂ = "male", NA when written so.',
    `- Species: write the full name. Abbreviations: ${SPECIES_HINT}${species.length ? ` Names in use: ${species.join('; ')}.` : ''}`,
    '- Notes (right-hand column or facing page, matched to lines by position or by a bracket): a CAM (CAM + 6 digits; "cam505" continues the prefix of the CAM above, e.g. CAM076505) goes in CAM_ID; a wing clip tube ("wc", 2 letters + 8 digits, a short "553" continues the tube above) in Tube_1_id; the cause of death (unk = Unknown, eaten, spider, ants, disapp = Disappearance, deformed, heat shock = Heat stroke, preserved = Killed_Preserved, only wings = Unknown - Only wings) in Death_cause; any other note text in the notes column.',
    ...(listLines.length ? ['- Allowed values:', ...listLines.map(l => `  ${l}`)] : []),
    '- Confidence: for every cell you are not sure of, give "c" (0 to 1) and up to 3 other readings in "a". Omit cells you are sure of. A cell you cannot read: value null, c 0.',
    '- Position: "y" is the vertical centre of the line on the upright page (0 = top edge, 1 = bottom edge of the photo). "rotate" is how many degrees the photo must turn clockwise to read the text upright (0, 90, 180 or 270); y refers to the upright page.',
    '- "raw" is the line as written, short (keep abbreviations and symbols).',
    '',
    'Answer with this JSON:',
    JSON.stringify({
      kind: kind === 'auto' ? KIND_IDS.join('|') : kind,
      rotate: 0,
      year: null,
      headers: ['column headers as written'],
      lines: [{ n: 1, y: 0.12, ...EXAMPLES[kind === 'auto' ? 'emergence' : kind] }],
      other: 'text outside the table, if any',
    }),
    '"year": the year of the page if written anywhere (a header, a sticky note, a full date), else null.',
  );
  return parts.join('\n');
}

const clip = (value, length) => String(value ?? '').slice(0, length);

/** The JSON object in the model's answer (it may add a fence or a sentence despite the instructions). */
function jsonIn(text) {
  const s = String(text ?? '').replace(/```(?:json)?/g, '');
  const start = s.indexOf('{');
  const end = s.lastIndexOf('}');
  if (start < 0 || end <= start) return null;
  try {
    return JSON.parse(s.slice(start, end + 1));
  } catch {
    return null;
  }
}

/**
 * The model's answer as a checked transcription. Unknown columns are dropped,
 * positions and confidences are clamped, lines are numbered.
 */
export function parseTranscription(text, requested = 'auto') {
  const data = jsonIn(text);
  if (!data || !Array.isArray(data.lines)) throw new Error('La respuesta de la IA no tiene líneas legibles');
  const kind = KINDS[requested] ? requested : KINDS[data.kind] ? data.kind : null;
  if (!kind) throw new Error('No se reconoció el tipo de cuaderno; elígelo y vuelve a leer la página');
  const fields = new Set(KINDS[kind].fields);
  const number = (value, low, high) => {
    const n = Number(value);
    return Number.isFinite(n) ? Math.min(high, Math.max(low, n)) : null;
  };
  const lines = data.lines.slice(0, 120).map((line, i) => {
    const v = {},
      c = {},
      a = {};
    for (const [field, value] of Object.entries(line?.v ?? {})) {
      if (!fields.has(field)) continue;
      v[field] = value === null || value === undefined ? null : clip(value, 300).trim();
    }
    for (const [field, value] of Object.entries(line?.c ?? {})) {
      const n = number(value, 0, 1);
      if (fields.has(field) && n !== null) c[field] = n;
    }
    for (const [field, value] of Object.entries(line?.a ?? {})) {
      if (!fields.has(field) || !Array.isArray(value)) continue;
      const options = value.map(x => clip(x, 120).trim()).filter(x => x && x !== v[field]);
      if (options.length) a[field] = [...new Set(options)].slice(0, 3);
    }
    return {
      n: i + 1,
      y: number(line?.y, 0, 1),
      raw: clip(line?.raw, 300),
      crossed: Boolean(line?.x),
      v,
      c,
      a,
    };
  });
  const rotate = [0, 90, 180, 270].includes(Number(data.rotate)) ? Number(data.rotate) : 0;
  const year = Number.isInteger(Number(data.year)) && data.year >= 1990 && data.year < 2100 ? Number(data.year) : null;
  return {
    kind,
    detected: KINDS[data.kind] ? data.kind : null,
    rotate,
    year,
    headers: Array.isArray(data.headers) ? data.headers.slice(0, 30).map(h => clip(h, 60)) : [],
    other: clip(data.other, 2000),
    lines,
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

/** The value a notebook cell gives a column, as the sheet stores it, or an error. */
export function readValue(field, text, { year }) {
  if (isNone(text)) return { value: null };
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
  if (type === 'number') return { value: /^\d+$/.test(s) ? Number(s) : s };
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
  const dateFields = kind.fields.filter(f => typeOf(f) === 'date');

  // First the keys, to find each line's row (a corrected key finds another row).
  const lines = transcription.lines.map(line => {
    const edited = edits[line.n] ?? {};
    const text = { ...line.v };
    for (const [field, value] of Object.entries(edited)) if (kind.fields.includes(field)) text[field] = value;
    const keyValues = kind.keys.map(k => readValue(k, text[k], { year: currentYear }).value);
    const found = keyValues.every(v => v !== null && v !== '') ? lookup.find(keyValues) : [];
    return { line, edited, text, keyValues, found, record: found.length === 1 ? found[0] : null };
  });
  const pageYear = year ?? transcription.year ?? inferYear(lines, dateFields, currentYear);
  const yearSource = year ? 'person' : transcription.year ? 'page' : 'inferred';

  // The same key on two lines of the page.
  const seen = new Map();
  for (const item of lines) {
    if (item.line.crossed || item.keyValues.some(v => v === null)) continue;
    const key = item.keyValues.map(clutchKey).join('|');
    seen.set(key, [...(seen.get(key) ?? []), item.line.n]);
  }

  const out = lines.map(item => {
    const { line, edited, text } = item;
    const key = item.keyValues.map(clutchKey).join('|');
    const twins = (seen.get(key) ?? []).filter(n => n !== line.n);
    // Several sheet rows with this key: the one that agrees with the most cells.
    if (!item.record && item.found.length > 1) {
      const score = record =>
        kind.fields.filter(f => {
          const value = readValue(f, text[f], { year: pageYear }).value;
          return !isNone(value) && sameValue(f, record.values?.[f], value);
        }).length;
      const ranked = item.found.map(r => [r, score(r)]).sort((a, b) => b[1] - a[1]);
      if (ranked[0][1] > ranked[1][1]) item.record = ranked[0][0];
    }
    const record = item.record;
    let status = line.crossed
      ? 'crossed'
      : item.keyValues.some(v => v === null || v === '')
        ? 'nokey'
        : twins.length
          ? 'duplicate'
          : record
            ? 'match'
            : item.found.length > 1
              ? 'ambiguous'
              : kind.newRows
                ? 'new'
                : 'missing';
    const message = {
      crossed: 'Tachada en el cuaderno: no se usa',
      nokey: `Sin ${kind.keys.join(' + ')} legible: escríbelo para buscar la fila`,
      duplicate: `El mismo ${kind.keys.join(' + ')} está también en la línea ${twins.join(', ')}`,
      ambiguous: `${item.found.length} filas de la hoja tienen este ${kind.keys.join(' + ')} (filas ${item.found.map(r => r.row).join(', ')})`,
      missing: `${item.keyValues.join(' ')} no está en ${kind.sheet}: ¿está bien leído?`,
      new: `Fila nueva en ${kind.sheet}`,
      match: '',
    }[status];
    const usable = status === 'match' || status === 'new';
    // A clutch changed on the page changes what the SPECIES formula will give.
    const clutchText = text['CLUTCH NUMBER'];
    const cells = {};
    for (const field of kind.fields) {
      const typed = field in edited;
      const confidence = typed ? 1 : (line.c[field] ?? (line.v[field] === null && field in line.v ? 0 : 1));
      const read = readValue(field, text[field], { year: pageYear });
      let value = read.value;
      let error = read.error ?? null;
      const before = record ? (record.values?.[field] ?? null) : null;
      // A day/month the sheet has in another year is the same date (the year was only inferred).
      if (typeOf(field) === 'date' && typeof value === 'number' && !read.yearWritten && typeof before === 'number') {
        if (serialDayMonth(before) === serialDayMonth(value)) value = before;
        else if (!year && value > todaySerial + 7) value = readValue(field, text[field], { year: pageYear - 1 }).value;
      } else if (typeOf(field) === 'date' && typeof value === 'number' && !read.yearWritten && !year && value > todaySerial + 7)
        value = readValue(field, text[field], { year: pageYear - 1 }).value;
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
        if (exact) value = exact;
        else if (hits.length === 1 && key.length >= 3) value = hits[0];
        else unlisted = true;
      }
      const alternatives = (line.a[field] ?? [])
        .map(a => readValue(field, a, { year: pageYear }).value)
        .filter(a => !isNone(a) && a !== value);
      const cell = {
        value: isNone(value) ? null : value,
        before,
        status: 'empty',
        confidence,
        // A value outside a list that is not strict (a species name) is shown but not written until confirmed.
        doubt: !typed && (confidence < DOUBT || (unlisted && !list?.strict)),
        alternatives: [...new Set(alternatives)],
        edited: typed,
        include: false,
        formula: false,
        message: error ?? (unlisted && !list?.strict && !typed ? `«${value}» no está en la lista de ${field}` : null),
      };
      const isKey = kind.keys.includes(field);
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
        cell.status = sameValue(field, predicted, cell.value) ? 'same' : 'conflict';
        if (cell.status === 'conflict')
          cell.message ??= `La fórmula da «${predicted ?? 'vacío'}»; se escribirá encima`;
      } else if (sameValue(field, before, cell.value)) cell.status = 'same';
      else if (isNone(before)) cell.status = 'fill';
      else if (/^Notes|^NOTES$/.test(field)) cell.status = 'fill';
      else cell.status = 'conflict';
      if (isKey && status === 'match') cell.status = 'same';
      if (['fill', 'conflict', 'new'].includes(cell.status) && formulaHere && !(field === 'SPECIES' && cell.formula)) {
        const allowed = lookup.typedOverFormula?.has(field) && record;
        if (!allowed) {
          cell.status = 'formula';
          cell.message ??= 'Columna con fórmula en la hoja: no se escribe';
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
    fields: kind.fields,
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

/** The page as text for the chat: one numbered line each, so "¿qué dice la línea 5?" can be answered. */
export function pageText(review) {
  return review.lines
    .map(l => {
      const values = Object.entries(l.cells)
        .filter(([, c]) => c.value !== null)
        .map(([f, c]) => `${f}=${show(f, c.value)}${c.doubt ? '?' : ''}`)
        .join(', ');
      return `${l.n}. «${l.raw}» → ${values || '—'}${l.message ? ` (${l.message})` : ''}`;
    })
    .join('\n');
}
