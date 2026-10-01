// Matching a transcribed notebook page with its sheet: each line Claude read
// from the photo (the digitalizar-cuaderno skill, through the match_notebook
// tool) is compared with its row and the page becomes a proposal. Pure
// functions: the sheet is reached through the `lookup` given to buildReview
// (server/notebook-tool.mjs).

import { isSumField, parseDateText, simpleSum } from './schema.mjs';
import { msg } from './messages.mjs';

/**
 * What the team types in Insectary_data for a butterfly that died and was not
 * preserved (Unknown, Disappearance, Eaten…): the block DeathsView writes, plus
 * Research_purpose NA (insectary.md A8). Only empty cells take it.
 */
export const NOT_PRESERVED = {
  Research_purpose: 'NA',
  Preservation_date: 'NA',
  CAM_ID: 'NA',
  Tube_1_id: 'NA',
  Tube_1_tissue: 'NA',
  T1_Preservation_medium: 'NOT_COLLECTED',
  Tube_2_id: 'NA',
  Tube_2_tissue: 'NA',
  T2_Preservation_medium: 'NOT_COLLECTED',
  Tube_3_id: 'NA',
  Tube_3_tissue: 'NA',
  Tube_4_id: 'NA',
  Tube_4_tissue: 'NA',
  Preservation_medium: 'NOT_COLLECTED',
  Preserved_Dead_Alive: 'NA',
  Location_body: 'NA',
};
/** The tissue of a wing clip, exactly as the ORGANISM_PART list has it. */
export const WING_CLIP = '**OTHER_SOMATIC_ANIMAL_TISSUE** | WING CLIP';
/** The columns of a death and its preservation that a line implies (templates, note words). */
const DEATH_EXTRA = [
  'Research_purpose',
  'Preservation_date',
  'Tube_1_tissue',
  'T1_Preservation_medium',
  'Tube_2_id',
  'Tube_2_tissue',
  'T2_Preservation_medium',
  'Tube_3_id',
  'Tube_3_tissue',
  'Tube_4_id',
  'Tube_4_tissue',
  'Preservation_medium',
  'Preserved_Dead_Alive',
  'Location_body',
];
/** The columns impliedValues fills: after the dates in every notebook's column order. */
const IMPLIED_FIELDS = new Set(['Death_cause', 'CAM_ID', 'Tube_1_id', ...DEATH_EXTRA]);

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
    // Reared when the line gives a clutch; Wild-caught when Claude says so (no clutch, a collector's note).
    // The death and preservation columns come from the notes' words and the templates (impliedValues).
    extra: ['Wild_Reared', ...DEATH_EXTRA],
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
    extra: DEATH_EXTRA,
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
const CLUTCH_GENERATION = /^\s*(\d+\s*(?:\(\s*\d+\s*\))?)\s*\(\s*(F1|F2|BC|backcross)\s*\)\s*$/i;
const generationName = g => (/^(bc|backcross)$/i.test(g) ? 'Backcross' : g.toUpperCase());
const GENERATION = /\(\s*(F1|F2|BC|backcross)\s*\)|\s(F1|F2)\s*$/i;
/**
 * A generation written after the species goes to the Generation column (F1,
 * F2, Backcross), unless the line gives Generation itself; the species loses it.
 */
export function generationFromSpecies(text) {
  // "994(F1)": the generation written after the clutch number (a batch, "992(2)", stays).
  const clutch = typeof text['CLUTCH NUMBER'] === 'string' ? CLUTCH_GENERATION.exec(text['CLUTCH NUMBER']) : null;
  if (clutch) {
    text['CLUTCH NUMBER'] = clutch[1].trim();
    if (isNone(text.Generation)) text.Generation = generationName(clutch[2]);
  }
  const species = text.SPECIES;
  const m = typeof species === 'string' ? GENERATION.exec(species) : null;
  if (!m) return text;
  const g = (m[1] ?? m[2]).toUpperCase();
  const rest = species.replace(m[0], ' ').replace(/\s+/g, ' ').trim();
  if (rest) text.SPECIES = rest;
  else delete text.SPECIES;
  if (isNone(text.Generation)) text.Generation = generationName(g);
  return text;
}

/** The terms of a count kept as a sum ([7, -3] for "=7-3"), or null. */
const sumParts = value => {
  const formula = typeof value === 'string' ? simpleSum(value) : null;
  return formula ? formula.slice(1).match(/[+-]?\d+/g).map(Number) : null;
};
/** What a count kept as a sum adds up to (27 for "=12+15", 4 for "=7-3"), or null. */
const sumTotal = value => sumParts(value)?.reduce((sum, term) => sum + term, 0) ?? null;

/**
 * "ins/oda", "ins/este", "ins ESTEBAN", "in-Oda": the clutch is in the Insectary and the
 * butterflies are that person's. The note says it as the team wrote it in the workbook.
 */
const OWNERS = { oda: 'Oda', este: 'Esteban', esteban: 'Esteban' };
export const ownerNote = name => `Butterflies of ${name}`;
/** The note as it was written before notes switched to English (a row that says it already). */
const ownerNoteEs = name => `mariposas de ${name}`;
const OWNER_WORDS = Object.keys(OWNERS).join('|');
// In a note: "ins/este", "in-Oda", "ins ESTEBAN" (a bare "in" needs its slash or dash).
const OWNER_CODE = new RegExp(
  String.raw`\b(?:in(?:s(?:ect(?:ary)?)?)?\.?\s*[/\\\-–—_:,+]\s*|ins(?:ect(?:ary)?)?\.?\s+)(${OWNER_WORDS})\b`,
  'gi',
);
const OWNER_SAID = new RegExp(String.raw`\b(?:mariposas|butterflies)\s+(?:de|of|from)\s+(${OWNER_WORDS})\b`, 'gi');
const tidy = s =>
  s
    .replace(/\s*([;,|/])\s*(?=[;,|/]|$)/g, '')
    .replace(/^[\s;,|/.:-]+|[\s;,|/:-]+$/g, '')
    .replace(/\s{2,}/g, ' ');
/**
 * Whose butterflies a clutch line says (from its INSECTARY OR LABORATORY cell, or a code
 * copied into its notes): the column becomes "ins", the notes lose the code and the owner's
 * note is returned apart. "ins/lab" is read as "ins/oda" (the sheet has no "ins/lab", and the
 * codes look alike in the notebooks), a doubt on the note. Changes `text` in place.
 */
export function insectaryOwner(text) {
  const out = { owner: null, doubt: false };
  const column = String(text['INSECTARY OR LABORATORY'] ?? '').trim();
  const code = /^in(?:s(?:ect(?:ary)?)?)?\.?\s*(?:[/\\\-–—_:,+]\s*|\s+)([a-záéíóúñ]+)\.?$/i.exec(column);
  if (code) {
    const word = code[1].toLowerCase();
    if (OWNERS[word]) [out.owner, text['INSECTARY OR LABORATORY']] = [OWNERS[word], 'ins'];
    else if (/^lab/.test(word)) [out.owner, out.doubt, text['INSECTARY OR LABORATORY']] = ['Oda', true, 'ins'];
  }
  if (typeof text.NOTES === 'string' && text.NOTES.trim()) {
    let note = text.NOTES;
    for (const pattern of [OWNER_CODE, OWNER_SAID])
      note = note.replace(pattern, (_, word) => {
        out.owner ??= OWNERS[word.toLowerCase()];
        return '';
      });
    note = tidy(note);
    if (note) text.NOTES = note;
    else delete text.NOTES;
    if (out.owner && isNone(column)) text['INSECTARY OR LABORATORY'] = 'ins';
  }
  return out;
}

const clip = (value, length) => String(value ?? '').slice(0, length);

/**
 * A page as Claude transcribed it (the match_notebook tool) in the form the
 * review reads: one entry per notebook line with its values, the confidence of
 * the doubtful cells and their other readings. Unknown columns are dropped and
 * reported; a cell with alternatives but no confidence counts as doubtful.
 */
export function checkTranscription({ kind, year = null, lines = [] }) {
  if (!KINDS[kind]) throw new Error(`Unknown notebook kind "${clip(kind, 40)}": use one of ${KIND_IDS.join(', ')}`);
  // The lines sent as JSON text (some clients pass an array argument as a string).
  if (typeof lines === 'string')
    try {
      lines = JSON.parse(lines);
    } catch {
      throw new Error('lines must be a list of lines (it came as text that is not JSON)');
    }
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
    // Why a cell is doubtful, in a few words ("7 or 1: this hand's 1 has a flag"), shown to the person.
    const r = {};
    for (const [field, value] of Object.entries(rename(line?.reasons)))
      if (fields.has(field) && typeof value === 'string' && value.trim()) r[field] = clip(value, 160).trim();
    return {
      n: Number.isInteger(line?.n) && line.n > 0 ? line.n : i + 1,
      y: null,
      raw: clip(line?.raw, 300),
      crossed: Boolean(line?.crossedOut),
      v,
      c,
      a,
      ...(Object.keys(r).length ? { r } : {}),
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
/**
 * The terms of a count corrected on the page, as the team types it: the first sum as
 * written, then each new total written after "=" (the crossed-out value replaced) as the
 * difference from the running total. "31+4=1" → 31, 4, -34; "12=9=4" (12 crossed out, then
 * 9, then 4) → 12, -3, -5; "2+4=6+8=14" (steps that add up) → 2, 4, 8. Null when not a sum.
 */
export function correctedTerms(text) {
  const s = String(text ?? '').replace(/\s+/g, '').replace(/^=/, '');
  if (!/^\d+(?:[+-]\d+)*(?:=\d+(?:[+-]\d+)*)*$/.test(s)) return null;
  const parts = s.split('=').map(p => p.match(/[+-]?\d+/g).map(Number));
  const terms = [...parts[0]];
  for (const part of parts.slice(1)) {
    const total = terms.reduce((a, b) => a + b, 0);
    if (part[0] !== total) terms.push(part[0] - total);
    terms.push(...part.slice(1));
  }
  return terms;
}
const formulaOf = terms => `=${terms.map((t, i) => (i && t >= 0 ? `+${t}` : String(t))).join('')}`;

/** The value a notebook cell gives a column, as the sheet stores it, or an error. */
export function readValue(field, text, { year, sheet = null }) {
  // A dash or NA written in a text column is the sheet's "NA" (e.g. no stock of origin, no CAM);
  // so is one in a clutch's dates and counts (a stage that never came: the team types NA there).
  // Elsewhere, in dates, counts and notes, it just means nothing to write (a living butterfly).
  if (isNone(text)) {
    const na = (typeOf(field) === 'text' || sheet === 'Insectary_stocks') && !/^Notes|^NOTES$/.test(field);
    return { value: na && String(text ?? '').trim() ? 'NA' : null };
  }
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
    // Stock counts are kept as the notebook sums them (=12+15, =19; a corrected count keeps
    // its first terms and the corrections: =31+4-34); elsewhere the total.
    const terms = sumTerms(s) ?? (isSumField(sheet, field) ? correctedTerms(s) : null);
    if (terms) return { value: (isSumField(sheet, field) && simpleSum(formulaOf(terms))) || terms.reduce((a, b) => a + b, 0) };
    // A worked sum whose steps do not add up counts what follows the last "=".
    const total = /^[\d\s+\-=]*=\s*(\d+)$/.exec(s);
    if (total) return { value: Number(total[1]) };
    return { value: /^\d+$/.test(s) ? Number(s) : s };
  }
  // The notebook's "ins", "ins/oda", "ins ESTEBAN" is the Insectary ("lab…" the Laboratory); the part
  // after it (whose butterflies, which room) belongs in the notes, not in this list column.
  if (field === 'INSECTARY OR LABORATORY') {
    if (/^ins/i.test(s)) return { value: 'Insectary' };
    if (/^lab/i.test(s)) return { value: 'Laboratory' };
    return { value: s };
  }
  if (field === 'Sex') {
    const sex = { '♀': 'female', '♂': 'male', f: 'female', h: 'female', m: 'male' }[s.toLowerCase()];
    return { value: sex ?? s };
  }
  if (field === 'CAM_ID') {
    // Seven digits (CAM0770540): a digit too many, kept as written for the checks to point out.
    const long = /^cam\s*(\d{7,})$/i.exec(s);
    if (long) return { value: `CAM${long[1]}` };
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
  // Counts kept as sums: two sums compare their terms (=12+15 is not =14+13); a sum and a single
  // number (the notebook's final count, 4 for the sheet's =7-3), their total.
  if (type === 'number' && (sumTotal(sheet) !== null || sumTotal(notebook) !== null)) {
    if ((sumParts(sheet)?.length ?? 1) > 1 && (sumParts(notebook)?.length ?? 1) > 1)
      return simpleSum(sheet) === simpleSum(notebook);
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

/**
 * Why a cell is doubtful: a check's reason, the reader's own words, a value
 * outside a list, or its confidence. { reason, reasonMsg? } (reasonMsg: the
 * descriptor the interface translates; the reader's words go as they are).
 */
function doubtReason(check, said, unlisted, confidence) {
  if (check?.reason) return { reason: check.reason, ...(check.reasonMsg ? { reasonMsg: check.reasonMsg } : {}) };
  if (said) return { reason: said };
  const m = unlisted
    ? msg('«{value}» no está en la lista de {field}', { value: String(unlisted.value), field: unlisted.field })
    : msg('Lectura dudosa (confianza {confidence})', { confidence: Math.round(confidence * 100) / 100 });
  return { reason: m.text, reasonMsg: m.msg };
}

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

// ---------------------------------------------------------------------------
// Checks of a line against the lines around it. They make doubts (the value
// goes in highlighted, with the other readings), never silent changes.

/** Digits one hand writes alike: 848 for 843, 17 for 11, 6 for 5. */
const LOOK_DIGITS = new Set(['38', '17', '56', '08', '68', '49', '27', '06', '35', '89', '09'].flatMap(p => [p, p[1] + p[0]]));
/** Two texts of one length that differ in one character, a digit one hand writes like the other. */
const oneLookAlike = (a, b) => {
  if (a.length !== b.length || a === b) return false;
  const diff = [...a].map((ch, i) => [ch, b[i]]).filter(([x, y]) => x !== y);
  return diff.length === 1 && LOOK_DIGITS.has(diff[0].join(''));
};

/**
 * A run of Emergidos lines read with a clutch that looks like the run next to
 * it (848 after 843s, the same emerge date): the smaller run, or the one whose
 * clutch could not give butterflies emerging then, gets the other clutch as an
 * alternative. `plausible(clutch, emergeText)`: true when the clutch was laid 20
 * to 90 days before, false when it has no laid date or is not in the sheet, null
 * when it cannot tell. Returns { [line index]: { value, alternatives, confidence, reason } }.
 */
export function clutchRuns(texts, crossed, plausible = () => null) {
  const runs = [];
  texts.forEach((text, i) => {
    const clutch = crossed[i] || isNone(text['CLUTCH NUMBER']) ? null : String(text['CLUTCH NUMBER']).trim();
    const last = runs.at(-1);
    if (clutch && last && last.end === i - 1 && clutchKey(last.clutch) === clutchKey(clutch)) {
      last.end = i;
      last.lines.push(i);
    } else if (clutch) runs.push({ clutch, key: clutchKey(clutch), end: i, lines: [i], emerge: String(text.Intro2Insectary_date ?? '').trim() });
  });
  const out = {};
  runs.forEach((run, k) => {
    const near = [runs[k - 1], runs[k + 1]].filter(o => o && oneLookAlike(run.key, o.key));
    for (const other of near.sort((a, b) => b.lines.length - a.lines.length)) {
      const sameDay = run.emerge && run.emerge === other.emerge;
      const mine = plausible(run.clutch, run.emerge);
      const theirs = plausible(other.clutch, run.emerge);
      const swap = mine === false && theirs === true;
      if (!swap && (!sameDay || other.lines.length <= run.lines.length || (mine === true && theirs === false))) continue;
      const why = swap
        ? msg('Leído {read}, entre líneas del {other}: el {read} no tiene una puesta 20–90 días antes', { read: run.clutch, other: other.clutch })
        : msg('Clutch {read} entre líneas del {other} (misma emergencia)', { read: run.clutch, other: other.clutch });
      for (const i of run.lines)
        out[i] = {
          value: swap ? other.clutch : run.clutch,
          alternatives: [swap ? run.clutch : other.clutch],
          confidence: swap ? 0.4 : 0.6,
          reason: why.text,
          reasonMsg: why.msg,
        };
      return;
    }
  });
  return out;
}

/** The number of a CAM or tube as written: { prefix, digits } ("CAM", "0770540"; "FS", "5848961"), or null. */
const idParts = (field, value) => {
  const m = field === 'CAM_ID' ? /^(cam)\s*(\d+)$/i.exec(String(value ?? '').trim()) : /^([A-Z]{2})\s*(\d+)$/i.exec(String(value ?? '').trim());
  return m ? { prefix: m[1].toUpperCase(), digits: m[2] } : null;
};
/** How far a number is from the run of the lines around it (their numbers plus the lines between), or Infinity. */
const runDistance = (n, i, around) => Math.min(Infinity, ...around.map(o => Math.abs(n - (o.n + (i - o.i)))));

/**
 * CAMs and tubes that do not fit: a CAM with seven digits (CAM0770540, a digit
 * too many), a tube with seven or nine (FS5848961, one dropped from FS50848961),
 * or a CAM far from the run of the lines around it. The value becomes the
 * reading that continues the run, when one does; the written one stays as an
 * alternative. Returns { [line index]: { [field]: { value, alternatives, confidence, reason } } }.
 */
export function idChecks(texts, crossed) {
  const out = {};
  const fields = ['CAM_ID', 'Tube_1_id', 'Tube_2_id'];
  for (const field of fields) {
    const size = field === 'CAM_ID' ? 6 : 8;
    const parts = texts.map((t, i) => (crossed[i] ? null : idParts(field, t[field])));
    parts.forEach((p, i) => {
      if (!p) return;
      // The lines around with a well-formed ID of the same prefix (up to 3 each side).
      const around = [];
      for (const step of [-1, 1])
        for (let j = i + step, k = 0; j >= 0 && j < parts.length && k < 3; j += step) {
          const o = parts[j];
          if (o && o.prefix === p.prefix && o.digits.length === size) {
            around.push({ i: j, n: Number(o.digits) });
            k++;
          }
        }
      const put = check => ((out[i] ??= {})[field] = check);
      const written = `${p.prefix}${p.digits}`;
      if (p.digits.length !== size && Math.abs(p.digits.length - size) === 1) {
        // Every reading one digit away: a digit dropped (inserted back) or one too many (taken out).
        const options = new Set();
        if (p.digits.length < size)
          for (let at = 0; at <= p.digits.length; at++) for (let d = 0; d <= 9; d++) options.add(p.digits.slice(0, at) + d + p.digits.slice(at));
        else for (let at = 0; at < p.digits.length; at++) options.add(p.digits.slice(0, at) + p.digits.slice(at + 1));
        const ranked = [...options]
          .filter(o => o.length === size && (field !== 'CAM_ID' || /^0/.test(o)))
          .map(o => ({ o, far: runDistance(Number(o), i, around) }))
          .sort((a, b) => a.far - b.far || (b.o.startsWith('07') ? 1 : 0) - (a.o.startsWith('07') ? 1 : 0));
        const fits = ranked.filter(r => r.far <= 20);
        const best = fits[0]?.far <= 2 ? fits[0] : null;
        const why =
          p.digits.length < size
            ? msg('{value} tiene {n} cifras (son {size}): ¿falta una?', { value: written, n: p.digits.length, size })
            : msg('{value} tiene {n} cifras (son {size}): ¿una de más?', { value: written, n: p.digits.length, size });
        put({
          value: best ? `${p.prefix}${best.o}` : written,
          alternatives: [...(best ? [written] : []), ...fits.filter(r => r !== best).slice(0, 2).map(r => `${p.prefix}${r.o}`)],
          confidence: 0.3,
          reason: why.text,
          reasonMsg: why.msg,
        });
        return;
      }
      // A CAM far from a tight run of the lines around it.
      if (field !== 'CAM_ID' || p.digits.length !== size) return;
      const before = around.filter(o => o.i < i).sort((a, b) => b.i - a.i)[0];
      const after = around.filter(o => o.i > i).sort((a, b) => a.i - b.i)[0];
      const n = Number(p.digits);
      if (!before || !after || Math.abs(after.n - before.n) > 20 || Math.min(Math.abs(n - before.n), Math.abs(n - after.n)) <= 50) return;
      const expected = before.n + (i - before.i);
      const cam = n => `CAM${String(n).padStart(6, '0')}`;
      const why = msg('Fuera de la serie de las líneas vecinas ({from} … {to})', { from: cam(before.n), to: cam(after.n) });
      put({
        value: written,
        alternatives: expected < after.n || expected === after.n - (after.i - i) ? [cam(expected)] : [],
        confidence: 0.5,
        reason: why.text,
        reasonMsg: why.msg,
      });
    });
  }
  return out;
}

/**
 * Words of an Emergidos or Muertes note that belong in columns (ai-errors.md
 * R11): "ethanol" / "flash frozen" (the tube's medium), "wc" (a wing clip),
 * "pheromone" (killed for pheromones), "preserved", "unk" (cause unknown), and
 * CAMs and tubes. They leave the note (the rest stays, and a note left empty is
 * not written) and are returned: { medium, wingClip, pheromone, preserved,
 * unknown, cams, tubes }. Changes `text` in place.
 */
export function noteColumns(text, field = 'Notes_Insectary_data') {
  const out = { cams: [], tubes: [] };
  if (typeof text[field] !== 'string' || !text[field].trim()) return out;
  let note = ` ${text[field]} `;
  const take = (pattern, found) => {
    note = note.replace(pattern, (...m) => {
      found(m);
      return ' ';
    });
  };
  take(/\bcam\s*0?(\d{5,7})\b/gi, m => out.cams.push(`CAM${m[1].padStart(6, '0')}`));
  take(/\b([A-Z]{2})\s?(\d{7,9})\b/gi, m => out.tubes.push(`${m[1].toUpperCase()}${m[2]}`));
  take(/\b(?:ethanol|etanol|alcohol|etoh)\b/gi, () => (out.medium ??= 'Ethanol'));
  take(/\b(?:flash[\s-]*froz(?:en)?|flash[\s-]*frozen|ultracongelad[oa]s?|nitr[oó]geno(?:\s+l[ií]quido)?)\b/gi, () => (out.medium ??= 'Flash frozen'));
  take(/(?<![\w/])(?:w\.?\s?c\.?|wing[\s-]*clip(?:ped)?|clip\s+de\s+ala)(?![\w/])/gi, () => (out.wingClip = true));
  take(/\b(?:pheromon\w*|feromon\w*)\b/gi, () => (out.pheromone = true));
  take(/(?<![\w/])(?:unk\.?|unknown|desconocid[oa])(?=[\s,;:.)|]|$)/gi, () => (out.unknown = true));
  // "preserved" is a column's word only when nothing but IDs and media follow it ("preserved in
  // ultrafridge at -80ºC" stays a note).
  if (/\bpreserv|preservad|killed/i.test(note)) out.preserved = true;
  take(/\b(?:killed\s*(?:[&y+]|and)\s*)?(?:preserv\w*|preservad[oa]s?)\.?(?=[\s,;|(){}\[\]-]*$)/gi, () => {});
  // What is left once the column words are out: brackets, joining words and punctuation are not a note.
  const rest = note
    .replace(/[{}[\]()]/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();
  const empty = /^(?:[\s,;:.|+&/\\\-–—→↗↑"']|\b(?:in|en|and|y|with|con|de|of|to|a|body)\b)*$/i.test(rest);
  const kept = empty ? '' : tidy(rest);
  if (kept) text[field] = kept;
  else delete text[field];
  return out;
}

/**
 * What a death line implies in Insectary_data's other columns, filling only
 * empty cells (insectary.md A8, A9; ai-errors.md T3): a butterfly that died and
 * was not preserved takes the NA / NOT_COLLECTED block; a preserved one its
 * Preservation_date (= the death date), Preserved_Dead_Alive, Location_body
 * Ikiam, its tube's tissue and medium and the unused tubes NA; the note's words
 * give the medium, a wing clip, Research_purpose Pheromones and the cause.
 * `text`: the line's values as written (after noteColumns); `row`: the sheet
 * row's values; `death`/`intro`: the dates as serials (line or row), or null.
 * Returns { values: { field: value as the sheet stores it }, reasons: { field: why, a msg() } }.
 */
export function impliedValues({ text, row = {}, note = {}, death = null, intro = null }) {
  const values = {};
  const reasons = {};
  const has = f => !isNone(text[f]) || !isNone(row[f]);
  const set = (field, value, reason) => {
    if (values[field] === undefined && isNone(text[field])) [values[field], reasons[field]] = [value, reason];
  };
  const said = [note.medium && `«${note.medium === 'Ethanol' ? 'ethanol' : 'flash frozen'}»`, note.wingClip && '«wc»', note.pheromone && '«pheromone»', note.preserved && '«preserved»', note.unknown && '«unk»']
    .filter(Boolean)
    .join(', ');
  const fromNote = msg('De la nota: {words}', { words: said });
  const written = String(text.Death_cause ?? row.Death_cause ?? '').trim();
  const sample = has('CAM_ID') || has('Tube_1_id');
  const died = death !== null || !isNone(written) || note.unknown;
  // The cause the line does not write: "unk" is Unknown; preserved, for pheromones, or a CAM on the
  // day it emerged is Killed_Preserved (a same-day death with a CAM is a butterfly killed to keep).
  let cause = isNone(written) ? null : written;
  if (!cause && note.unknown) set('Death_cause', (cause = 'Unknown'), fromNote);
  else if (!cause && death !== null && (note.preserved || note.pheromone || (sample && (note.medium || (intro !== null && death === intro)))))
    set('Death_cause', (cause = 'Killed_Preserved'), note.preserved || note.pheromone || note.medium ? fromNote : msg('Con CAM y muerta el día que emergió'));
  if (note.pheromone) set('Research_purpose', 'Pheromones', fromNote);
  const killed = /^killed/i.test(cause ?? '');
  const year = death ?? intro;
  const recent = year !== null && new Date(Date.UTC(1899, 11, 30) + year * 864e5).getUTCFullYear() >= 2025;
  const tube1 = has('Tube_1_id');
  const usual = msg('Lo habitual desde 2025');
  if (died && !sample && !killed && cause) {
    for (const [field, value] of Object.entries(NOT_PRESERVED)) set(field, value, msg('Muerte sin preservar: como Muertes (NA / NOT_COLLECTED)'));
  } else if (sample && died && (killed || note.preserved || note.medium || note.tubes?.length)) {
    const why = msg('Individuo preservado: lo que el equipo escribe siempre');
    if (death !== null) set('Preservation_date', death, msg('La fecha de muerte (preservado ese día)'));
    if (killed) set('Preserved_Dead_Alive', 'Alive', msg('Killed_Preserved: preservado vivo'));
    set('Location_body', 'Ikiam', why);
    if (tube1) {
      set('Tube_1_tissue', note.wingClip ? WING_CLIP : 'WHOLE_ORGANISM', note.wingClip ? fromNote : why);
      if (note.medium || recent) set('T1_Preservation_medium', note.medium ?? 'Flash frozen', note.medium ? fromNote : usual);
    }
    if (has('Tube_2_id')) {
      set('Tube_2_tissue', 'WHOLE_ORGANISM', why);
      if (note.medium || recent) set('T2_Preservation_medium', note.medium ?? 'Flash frozen', note.medium ? fromNote : usual);
    } else if (!note.wingClip) {
      set('Tube_2_id', 'NA', why);
      set('Tube_2_tissue', 'NA', why);
      set('T2_Preservation_medium', 'NOT_COLLECTED', why);
    }
    for (const field of ['Tube_3_id', 'Tube_3_tissue', 'Tube_4_id', 'Tube_4_tissue']) set(field, 'NA', why);
  } else if (tube1 && (note.wingClip || note.medium)) {
    // A wing clip taken from a living butterfly: its tube's tissue and medium.
    if (note.wingClip) set('Tube_1_tissue', WING_CLIP, fromNote);
    if (note.medium || recent) set('T1_Preservation_medium', note.medium ?? 'Flash frozen', note.medium ? fromNote : usual);
  }
  return { values, reasons };
}

/** Existing IDs that differ from one read by a character or two swapped (9NM for 9MN): "did you mean". */
export function nearIds(read, ids) {
  const a = String(read ?? '').toUpperCase();
  const out = [];
  for (const id of ids) {
    const b = String(id).toUpperCase();
    if (b.length !== a.length || b === a) continue;
    const diff = [...a].map((ch, i) => i).filter(i => a[i] !== b[i]);
    if (diff.length === 1 || (diff.length === 2 && diff[1] === diff[0] + 1 && a[diff[0]] === b[diff[1]] && a[diff[1]] === b[diff[0]]))
      out.push(id);
  }
  return out;
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
 *   adultsOfClutch(clutch) → how many Insectary_data rows have that clutch (null: unknown),
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
  const owners = [];
  const texts = transcription.lines.map(line => {
    const text = { ...line.v };
    for (const [field, value] of Object.entries(edits[line.n] ?? {})) if (columnsOf(kind).includes(field)) text[field] = value;
    // "ins/oda": Insectary, and a note saying whose butterflies they are.
    owners.push(columnsOf(kind).includes('INSECTARY OR LABORATORY') ? insectaryOwner(text) : {});
    // "lys (F1)": the generation goes to its column, where the sheet has one.
    return columnsOf(kind).includes('Generation') ? generationFromSpecies(text) : text;
  });
  // A clutch page writes "ins" on some lines only: the others are in the same room when every
  // line that says it agrees (the team types it on every row).
  const rooms = new Set(
    transcription.lines
      .map((line, i) => (line.crossed ? null : texts[i]['INSECTARY OR LABORATORY']))
      .filter(room => !isNone(room))
      .map(room => readValue('INSECTARY OR LABORATORY', room, {}).value)
      .filter(v => v === 'Insectary' || v === 'Laboratory'),
  );
  const pageRoom = columnsOf(kind).includes('INSECTARY OR LABORATORY') && rooms.size === 1 ? [...rooms][0] : null;
  // An Emergidos or Muertes note: its words that belong in columns leave it ("ethanol", "wc",
  // "pheromone", "unk", CAMs and tubes); a CAM or tube goes to its column when the line has none.
  const deathKind = kind.sheet === 'Insectary_data' && columnsOf(kind).includes('Death_cause');
  const notes = texts.map((text, i) => {
    if (!deathKind || edits[transcription.lines[i].n]?.Notes_Insectary_data !== undefined) return {};
    const said = noteColumns(text);
    if (said.cams[0] && isNone(text.CAM_ID)) text.CAM_ID = said.cams[0];
    for (const tube of said.tubes) {
      if (isNone(text.Tube_1_id)) text.Tube_1_id = tube;
      else if (tube !== String(text.Tube_1_id).toUpperCase() && isNone(text.Tube_2_id)) text.Tube_2_id = tube;
    }
    return said;
  });
  const completed = completeRuns(texts);
  const crossedLines = transcription.lines.map(line => line.crossed);
  // CAMs with a digit too many, tubes with one dropped, CAMs out of the page's run.
  const checks = idChecks(texts, crossedLines);
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
  const serialOf = (text, fallback) => (isNone(text) ? (typeof fallback === 'number' ? fallback : null) : (readDate(text, pageYear)?.serial ?? null));

  // A clutch read unlike the run next to it (848 among 843s), judged by the laid dates in Insectary_stocks.
  if (kind.sheet === 'Insectary_data' && columnsOf(kind).includes('CLUTCH NUMBER')) {
    const plausible = (clutch, emerge) => {
      const known = lookup.clutch?.(readValue('CLUTCH NUMBER', clutch, {}).value);
      if (!lookup.laidOfClutch) return null;
      if (known === null || known === undefined) return false;
      const laid = lookup.laidOfClutch(known);
      if (typeof laid !== 'number') return false;
      const day = serialOf(emerge);
      return day === null ? true : day - laid >= 20 && day - laid <= 90;
    };
    for (const [i, check] of Object.entries(clutchRuns(texts, crossedLines, plausible))) {
      if (edits[transcription.lines[i].n]?.['CLUTCH NUMBER'] !== undefined) continue;
      (checks[i] ??= {})['CLUTCH NUMBER'] = check;
      // The species the formula will give follows the clutch the row gets.
      texts[i]['CLUTCH NUMBER'] = check.value;
    }
  }

  // An ID not in the sheet: the IDs one character away, nearest first to where its neighbours are.
  for (const [i, item] of lines.entries()) {
    if (item.record || item.candidates.length || kind.newRows || !lookup.nearIds || item.keyValues.some(v => v === null || v === '')) continue;
    const found = lines
      .map((o, j) => (o.record ? { at: o.record.row - (o.line.n - item.line.n), gap: Math.abs(j - i) } : null))
      .filter(Boolean)
      .sort((a, b) => a.gap - b.gap)[0];
    item.near = lookup
      .nearIds(item.keyValues)
      .sort((a, b) => (found ? Math.abs(a.row - found.at) - Math.abs(b.row - found.at) : a.row - b.row))
      .slice(0, 3);
  }

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
      missing:
        `${item.keyValues.join(' ')} no está en ${kind.sheet}: ¿está bien leído?` +
        (item.near?.length ? ` ¿Quisiste decir ${item.near.map(n => `${n.value} (fila ${n.row})`).join(' o ')}?` : ''),
      new: `Fila nueva en ${kind.sheet}`,
      // Read as a look-alike (600 for 6OO): the row was found by the others around it.
      match: item.readAs ? `Leído «${item.readAs}»; en la hoja es ${kind.keys.map(k => record.values?.[k]).join(' ')}` : '',
    }[status];
    const usable = status === 'match' || status === 'new';
    // A clutch changed on the page changes what the SPECIES formula will give.
    const clutchText = text['CLUTCH NUMBER'];
    const cells = {};
    let lastDate = null;
    // A new tube where the row already has a first one (a wing clip): one CAM per individual, each
    // sample its own tube, so it goes to the next free Tube_n (with the tissue and medium given for it).
    const said = notes[i] ?? {};
    const first = record?.values?.Tube_1_id;
    if (deathKind && record && !isNone(text.Tube_1_id) && !isNone(first) && !('Tube_1_id' in edited) && String(first).toUpperCase() !== String(text.Tube_1_id).toUpperCase()) {
      const free = [2, 3, 4].find(n => isNone(record.values?.[`Tube_${n}_id`]) && isNone(text[`Tube_${n}_id`]));
      if (free) {
        for (const [from, to] of [['Tube_1_id', `Tube_${free}_id`], ['Tube_1_tissue', `Tube_${free}_tissue`], ['T1_Preservation_medium', `T${free}_Preservation_medium`]])
          if (!isNone(text[from])) [text[to], text[from]] = [text[from], undefined];
      }
    }
    // What a death line implies in the other columns (computed once its dates are read).
    let implied = null;
    const impliedNow = () =>
      (implied ??=
        deathKind && usable
          ? impliedValues({
              text,
              row: record?.values ?? {},
              note: said,
              death: typeof cells.Death_date?.value === 'number' ? cells.Death_date.value : serialOf(null, record?.values?.Death_date),
              intro:
                typeof cells.Intro2Insectary_date?.value === 'number'
                  ? cells.Intro2Insectary_date.value
                  : serialOf(null, record?.values?.Intro2Insectary_date),
            })
          : { values: {}, reasons: {} });
    for (const field of columnsOf(kind)) {
      const typed = field in edited;
      const unreadable = line.v[field] === null && field in line.v;
      const owner = owners[i] ?? {};
      let confidence = typed ? 1 : (line.c[field] ?? (unreadable ? 0 : 1));
      // A check against the lines around (a clutch unlike its run, a CAM or tube with a digit more or less).
      const check = typed ? null : (checks[i]?.[field] ?? null);
      if (check) confidence = Math.min(confidence, check.confidence);
      // What the page implies where the line writes nothing; it only fills an empty cell.
      let inferred = null;
      let hint = null;
      if (usable && !typed && !unreadable && isNone(text[field])) {
        if (field === 'INSECTARY OR LABORATORY' && pageRoom) inferred = pageRoom;
        // "(F1)" not written after the species: no generation (the team types NA).
        else if (field === 'Generation' && kind.sheet === 'Insectary_stocks' && !isNone(text.SPECIES)) inferred = 'NA';
        // A butterfly with a clutch was reared.
        else if (field === 'Wild_Reared' && !isNone(text['CLUTCH NUMBER'])) inferred = 'Reared';
        // A death's other columns (the not-preserved block, a preserved butterfly's), the note's words.
        else if (deathKind && IMPLIED_FIELDS.has(field) && impliedNow().values[field] !== undefined)
          [inferred, hint] = [impliedNow().values[field], impliedNow().reasons[field] ?? null];
      }
      let source = check ? check.value : (inferred ?? text[field]);
      // "ins/lab" read as "ins/oda": the room is sure, whose butterflies they are is not.
      const ownerGuess = field === 'NOTES' && owner.doubt && !typed;
      if (ownerGuess) confidence = Math.min(confidence, 0.5);
      // One CAM per individual, for life: another CAM for a row that has one is a misreading or the wrong row.
      const rowCam = field === 'CAM_ID' && record ? record.values?.CAM_ID : null;
      const camClash =
        !typed && /^CAM\d+$/i.test(String(rowCam ?? '')) && !isNone(text.CAM_ID) && String(text.CAM_ID).replace(/\s+/g, '').toUpperCase() !== String(rowCam).toUpperCase();
      if (camClash) confidence = Math.min(confidence, 0.3);
      // The note says whose butterflies they are, unless the row's note already does.
      if (field === 'NOTES' && owner.owner && !typed) {
        const parts = [text.NOTES, ownerNote(owner.owner)].filter(p => !isNone(p));
        const has = textKey(record?.values?.NOTES ?? '');
        const said = p => has.includes(textKey(p)) || (p === ownerNote(owner.owner) && has.includes(textKey(ownerNoteEs(owner.owner))));
        const fresh = parts.filter(p => !said(p));
        source = fresh.length ? fresh.join('; ') : has.includes(textKey(parts.at(-1))) ? parts.at(-1) : ownerNoteEs(owner.owner);
      }
      // What the page only implies is already as the sheet stores it.
      const read = inferred !== null ? { value: inferred, yearWritten: true } : readValue(field, source, { year: pageYear, sheet: kind.sheet });
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
      // The reading as written when it is not in a non-strict list (false when it is, or there is none).
      let unlisted = false;
      // The list values a reading outside the list could be ("salapia": the listed salapias), offered as its alternatives.
      let guesses = [];
      if (list && typeof value === 'string' && !isNone(value) && !list.values.has(value)) {
        const key = textKey(value);
        const hits = [...list.values].filter(o => textKey(o).startsWith(key));
        const exact = hits.find(o => textKey(o) === key);
        // A full name where the list holds its last word (Stock_of_origin: "messenoides").
        const tail = [...list.values].filter(o => textKey(o).length > 2 && key.endsWith(` ${textKey(o)}`));
        // A short name that is a word of one value only ("salapia" → Ithomia salapia salapia).
        const words = key.length >= 4 ? [...list.values].filter(o => ` ${textKey(o)} `.includes(` ${key} `)) : [];
        // Among several, the one the row already has or its clutch gives ("Ithomia salapia").
        const clutchSpecies =
          field === 'SPECIES' && !isNone(clutchText)
            ? lookup.speciesOfClutch?.(lookup.clutch?.(clutchText) ?? clutchText)
            : null;
        const known = [before, clutchSpecies].filter(v => typeof v === 'string').map(textKey);
        const own = [...hits, ...words].find(o => known.includes(textKey(o)));
        if (exact) value = exact;
        else if (hits.length === 1 && key.length >= 3) value = hits[0];
        else if (tail.length === 1) value = tail[0];
        else if (own) value = own;
        else if (!hits.length && words.length === 1) value = words[0];
        else {
          unlisted = String(value);
          guesses = [...new Set([...hits, ...words, ...tail])].slice(0, 3);
          // The species' epithet alone ("salapia"): its nominate subspecies is the best reading, still to check.
          const nominate = words.filter(o => textKey(o).endsWith(` ${key} ${key}`));
          if (nominate.length === 1) [value, guesses] = [nominate[0], guesses.filter(o => o !== nominate[0])];
        }
      }
      const alternatives = [
        ...(check?.alternatives ?? []),
        ...(line.a[field] ?? []),
        ...(ownerGuess && !isNone(text.NOTES) ? [text.NOTES] : []),
        ...(camClash ? [rowCam] : []),
        ...guesses,
      ]
        .map(a => readValue(field, a, { year: pageYear, sheet: kind.sheet }).value)
        // A clutch as Insectary_stocks writes it ("685 (3)").
        .map(a => (field === 'CLUTCH NUMBER' && kind.sheet !== 'Insectary_stocks' ? (lookup.clutch?.(a) ?? a) : a))
        .filter(a => !isNone(a) && a !== value);
      const doubt = !typed && (confidence < DOUBT || (unlisted && !list?.strict));
      const cell = {
        // An explicit NA (from a dash in a text column) is kept; other "none" readings are nothing.
        value: value === 'NA' ? 'NA' : isNone(value) ? null : value,
        before,
        status: 'empty',
        confidence,
        // A doubtful reading (or a value outside a list that is not strict, a species name) goes into the
        // proposal highlighted, with its other readings, for the person to check before applying.
        doubt,
        ...(doubt ? doubtReason(check, line.r?.[field] ?? (ownerGuess ? 'read "ins/lab": most likely "ins/oda"' : camClash ? `the row already has ${rowCam}: one CAM per individual (a new sample takes a new tube)` : undefined), unlisted && !list?.strict ? { value: unlisted, field } : null, confidence) : { reason: null }),
        alternatives: [...new Set(alternatives)],
        edited: typed,
        include: false,
        formula: false,
        // Not written on the line: the page's room, a template, a word of the note.
        inferred: inferred !== null,
        message:
          error ??
          hint?.text ??
          (unlisted && !list?.strict && !typed
            ? `«${unlisted}» no está en la lista de ${field}`
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
      // A dash in a count where the sheet has 0 (or the new row's =0): the same nothing.
      const zero = !isNone(before) && Number(sumTotal(before) ?? before) === 0;
      if (['fill', 'conflict'].includes(cell.status) && cell.value === 'NA' && zero) cell.status = 'same';
      if (cell.status === 'conflict') {
        const sheetTerms = sumField ? sumParts(before) : null;
        const pageTerms = sumField ? (sumParts(cell.value) ?? [Number(cell.value)]) : null;
        // What the page only implies (the room, no generation, reared, a dash) never replaces a value.
        if (inferred !== null)
          Object.assign(cell, {
            status: 'keep',
            // A template's cell is only said when the row holds something else (NOT_COLLECTED for NA is not news).
            message: hint ? null : `La hoja tiene ${show(field, before)}; la línea no lo escribe: se deja`,
          });
        else if (cell.value === 'NA' && isNone(text[field]) && typeOf(field) !== 'text')
          Object.assign(cell, { status: 'keep', message: `La hoja tiene ${show(field, before)}; el cuaderno pone «—»: se deja` });
        // The sheet already has the page's terms and more (added after the page was written).
        else if (sheetTerms && pageTerms.length < sheetTerms.length && pageTerms.every((t, k) => t === sheetTerms[k]))
          Object.assign(cell, { status: 'keep', message: `La hoja tiene ${before}: los términos del cuaderno y más; se deja` });
      }
      const formula = record?.formulas?.[field];
      // What the page only implies never goes over a formula (T2_Preservation_medium often is one).
      if (inferred !== null && hint && formulaHere && ['fill', 'conflict', 'new'].includes(cell.status))
        Object.assign(cell, { status: 'keep', message: null });
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
        // The butterfly's own Collection_data row may hold its tube too (the twin of a wild-caught one).
        const own = { Insectary_ID: record?.values?.Insectary_ID ?? null, CAM_ID: cells.CAM_ID?.value ?? record?.values?.CAM_ID ?? null };
        const holder = error ? null : lookup.holder?.(field, cell.value, record?.id ?? null, own);
        if (holder) error = `${cell.value} ya está en ${holder.sheet} fila ${holder.row}`;
        if (error) Object.assign(cell, { status: 'error', message: error });
      }
      // Notes are written with the date and initials, after what the cell already holds.
      if (/^Notes|^NOTES$/.test(field) && ['fill', 'new'].includes(cell.status)) {
        const note = noteText(cell.value, { today, initials });
        cell.write = isNone(before) ? note : `${before} | ${note}`;
      }
      // Doubtful cells go in too (highlighted, never left out); an unreadable one (null) is never written.
      cell.include = usable && ['fill', 'conflict', 'new'].includes(cell.status);
      // A cell the reader could not read (null): no value, why (the reader's words) and what of it was
      // read (its "alternatives", as written). It goes into the proposal as a cell for the person to fill
      // when the row has nothing there yet and the column is not a formula the sheet computes.
      if (cell.status === 'unread') {
        delete cell.reasonMsg;
        Object.assign(cell, {
          doubt: false,
          reason: line.r?.[field] ?? null,
          alternatives: [],
          partial: line.a[field] ?? [],
          toFill:
            usable &&
            !isKey &&
            (!formulaHere || sumField) &&
            (isNone(before) || (sumField && record?.formulas?.[field] === '=0')),
        });
      }
      if (hint) cell.hintMsg = hint.msg;
      cells[field] = cell;
    }
    const changes = Object.values(cells).filter(c => c.include).length;
    const toFill = Object.values(cells).filter(c => c.toFill).length;
    const picked = usable && (picks[line.n] ?? changes > 0);
    // The adults a clutch line counts against the butterflies of that clutch typed in Insectary_data
    // (the two notebooks are filled apart): a difference is said, never corrected.
    const warnings = [];
    const adults = cells['NUMBER OF ADULTS']?.value ?? null;
    const counted = adults === null || adults === 'NA' ? null : (sumTotal(adults) ?? (Number.isFinite(Number(adults)) ? Number(adults) : null));
    const clutchHere = cells['CLUTCH NUMBER']?.value ?? record?.values?.['CLUTCH NUMBER'] ?? null;
    if (usable && counted !== null && clutchHere !== null && lookup.adultsOfClutch) {
      const typed = lookup.adultsOfClutch(clutchHere);
      if (typed !== null && typed !== counted)
        warnings.push(`NUMBER OF ADULTS: the page says ${counted}; Insectary_data has ${typed} butterflies of clutch ${clutchHere}`);
    }
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
      // Unreadable cells the person fills in the table.
      ...(toFill ? { toFill } : {}),
      picked: picked && changes > 0,
      ...(item.near?.length ? { near: item.near } : {}),
      ...(warnings.length ? { warnings } : {}),
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
      doubts: count(c => c.doubt && c.include),
      unreadable: count(c => c.toFill),
      errors: count(c => c.status === 'error') + out.filter(l => ['missing', 'ambiguous', 'duplicate', 'nokey'].includes(l.status)).length,
      created: out.filter(l => l.status === 'new' && l.changes).length,
      same: count(c => c.status === 'same'),
    },
  };
}

/**
 * The cells of a line the reader could not read and the person fills in the
 * proposal's table: { field: { reason?, partial? } } (reason: the reader's
 * words; partial: what of it was read, as written), or undefined. Never written
 * unless the person (or the assistant, on their word) gives a value.
 */
export function unreadableOf(line) {
  const out = {};
  for (const [field, cell] of Object.entries(line.cells))
    if (cell.toFill)
      out[field] = {
        ...(cell.reason ? { reason: clip(cell.reason, 200) } : {}),
        ...(cell.partial?.length ? { partial: cell.partial.slice(0, 3) } : {}),
      };
  return Object.keys(out).length ? out : undefined;
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
    // Per cell, what the person sees beside the value: a doubt (how sure, the other readings, why)
    // and, for cells the line does not write, where they come from.
    const doubts = {};
    const hints = {};
    const inferred = [];
    for (const [field, cell] of Object.entries(line.cells)) {
      if (!cell.include) continue;
      values[field] = cell.write ?? cell.value;
      if (cell.status === 'conflict') notes.push(`${field}: hoja ${show(field, cell.before)} → cuaderno ${show(field, cell.value)}`);
      if (cell.doubt)
        doubts[field] = {
          confidence: Math.round(cell.confidence * 100) / 100,
          // A note's other readings would be written with its date and initials: only the value's.
          alternatives: /^Notes|^NOTES$/.test(field) ? [] : cell.alternatives.slice(0, 3),
          reason: clip(cell.reason, 200),
          ...(cell.reasonMsg ? { reasonMsg: cell.reasonMsg } : {}),
        };
      if (cell.inferred) inferred.push(field);
      // Where an implied value comes from (a template, the note's words), for the person.
      if (cell.hintMsg && !cell.doubt) hints[field] = { text: clip(cell.message, 200), msg: cell.hintMsg };
    }
    const note = clip([`Línea ${line.n}: «${line.raw}»`, ...notes, ...(line.warnings ?? [])].join(' · '), 300);
    const unreadable = unreadableOf(line);
    const meta = {
      ...(Object.keys(doubts).length ? { doubts } : {}),
      ...(unreadable ? { unreadable } : {}),
      ...(Object.keys(hints).length ? { hints } : {}),
      ...(inferred.length ? { inferred } : {}),
    };
    if (line.status === 'new') newRows.push({ sheet: review.sheet, values, note, line: line.n, ...meta });
    else changes.push({ recordId: line.recordId, values, note, line: line.n, ...meta });
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
