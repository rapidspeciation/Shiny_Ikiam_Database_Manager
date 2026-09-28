// "Revisión de datos": inconsistencies across the workbook, found in the local
// copy (never by asking Google). Each issue names the sheet row, the column and
// the value, says what is wrong and, when the right value is obvious, carries a
// fix shaped like a propose_changes change ({ recordId, values }), so the
// assistant can draft the correction and the person confirms it.
//
// The whole scan runs once per state of the local copy (and per day, for
// future dates) and is cached, so paging and filtering are free.

import { moduleMap, parseDateText } from './schema.mjs';
import { TUBE_FIELD, UNIQUE, blankOrNA, isIdValue } from './verifications.mjs';
import { listOptions } from './verify.mjs';

/** Kinds of issue, in the order they are listed, with their Spanish names for the app. */
export const CHECK_KINDS = {
  repeat: 'ID o tubo repetido',
  cam_cross: 'CAM de dos mariposas',
  list: 'Fuera de la lista de la hoja',
  insectary_link: 'Colecta sin insectario (o al revés)',
  link_mismatch: 'Colecta e insectario no coinciden',
  date_order: 'Fechas en orden imposible',
  future_date: 'Fecha en el futuro',
  bad_date: 'Fecha que no es una fecha',
  missing_sample: 'Preservada sin CAM o tubo',
  mark_reuse: 'Marca usada en dos especies',
};
const KIND_ORDER = Object.keys(CHECK_KINDS);

const EPOCH = Date.UTC(1899, 11, 30);
const iso = serial => new Date(EPOCH + Math.round(serial) * 864e5).toISOString().slice(0, 10);
const todaySerial = () => {
  const today = new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' }).format(new Date());
  return Math.round((Date.parse(`${today}T00:00:00Z`) - EPOCH) / 864e5);
};
const text = value => (value === null || value === undefined ? '' : String(value).trim());
const isDate = value => typeof value === 'number' && Number.isFinite(value) && value > 0;
const binomial = value =>
  text(value)
    .toLowerCase()
    .split(/\s+/)
    .slice(0, 2)
    .join(' ');
/** "female ?", "Female", "female_?" → "female"; blanks and NOT_COLLECTED say nothing. */
const sexOf = value => {
  const s = text(value)
    .toLowerCase()
    .replace(/[_\s]*\?$/, '')
    .trim();
  return s === 'female' || s === 'male' ? s : null;
};
const hasMark = value => !!text(value) && !/^(NA|N\/A|not given)$/i.test(text(value));

/**
 * CAM columns that give a butterfly its own CAM (one pool each, see Lists).
 * Other sheets (Barcoding_DNA, Pheromones_data…) cite the CAM of a butterfly
 * recorded here, so their CAMs are expected to repeat these.
 */
const CAM_OWNERS = { Collection_data: 'CAM_ID', Insectary_data: 'CAM_ID', Wing_tissue: 'CAM_ID' };

/** Loads every recorded row once; placeholders (pre-made rows) are kept apart. */
function load(store) {
  const bySheet = new Map();
  const rows = store.db
    .prepare(
      'SELECT id,sheet,row_num,observed,label,values_json,formulas_json FROM records WHERE missing=0 AND row_num>0 AND row_num<2000000000',
    )
    .all();
  for (const r of rows) {
    const mod = moduleMap.get(r.sheet);
    if (!mod || r.row_num <= mod.headerRow) continue;
    const list = bySheet.get(r.sheet) || bySheet.set(r.sheet, []).get(r.sheet);
    list.push({
      id: r.id,
      sheet: r.sheet,
      row: r.row_num,
      observed: !!r.observed,
      label: r.label,
      values: JSON.parse(r.values_json),
      formulas: JSON.parse(r.formulas_json),
    });
  }
  return bySheet;
}

const ref = (row, field) => ({
  sheet: row.sheet,
  row: row.row,
  recordId: row.id,
  label: row.label,
  ...(field ? { field, value: shown(row.sheet, field, row.values[field]) } : {}),
});
function shown(sheet, field, value) {
  const type = moduleMap.get(sheet)?.fields.find(f => f.key === field)?.type;
  return type === 'date' && isDate(value) ? iso(value) : (value ?? null);
}

/** A date one year off that lands inside [min, max]: the usual slip when typing the year. */
function yearSlip(serial, min, max) {
  const d = new Date(EPOCH + serial * 864e5);
  const found = [-1, 1]
    .map(shift => {
      const moved = new Date(Date.UTC(d.getUTCFullYear() + shift, d.getUTCMonth(), d.getUTCDate()));
      return Math.round((moved.getTime() - EPOCH) / 864e5);
    })
    .filter(s => s >= min && s <= max);
  return found.length === 1 ? found[0] : null;
}

/**
 * What is wrong with a typed value of a date column, or null: a serial before
 * 2000 or more than two years ahead (375004 typed for 21/9/2026), or text.
 */
function badDate(value, today) {
  if (typeof value === 'number') {
    if (!Number.isFinite(value)) return `tiene «${value}», que no es una fecha`;
    if (value >= 36526 && value <= today + 731) return null;
    const year = value > 0 && value < 2958466 ? new Date(EPOCH + value * 864e5).getUTCFullYear() : null;
    return `tiene el número ${value}${year ? ` (año ${year})` : ''}, que no es una fecha`;
  }
  if (typeof value !== 'string') return null;
  const t = value.trim();
  if (!t || /^(NA|N\/A|NOT_COLLECTED|NOT_PROVIDED|unknown|-)$/i.test(t)) return null;
  return `es el texto «${t}», no una fecha`;
}

/** The right day of a broken date, when a note starts with it ("21/9/2026 AA: …") or the text reads as a date. */
function dayFromNotes(row, noteFields, field) {
  const value = row.values[field];
  const parsed = typeof value === 'string' ? parseDateText(value) : null;
  if (parsed && parsed >= 36526) return { fix: { recordId: row.id, values: { [field]: iso(parsed) } }, fixNote: 'fecha escrita como texto' };
  for (const key of noteFields) {
    const m = /^\s*(\d{1,2})\/(\d{1,2})\/(\d{2}|\d{4})\b/.exec(text(row.values[key]));
    const day = m && parseDateText(`${m[1]}/${m[2]}/${m[3]}`);
    if (day) return { fix: { recordId: row.id, values: { [field]: iso(day) } }, fixNote: `día de ${key}` };
  }
  return {};
}

/** Same text once case, underscores, spaces and accents are ignored ("female_?" = "female ?"). */
const loose = value =>
  text(value)
    .normalize('NFD')
    .replace(/\p{M}/gu, '')
    .toLowerCase()
    .replace(/[_\s]+/g, ' ')
    .replace(/\s*\?$/, ' ?')
    .trim();

function scan(store) {
  const sheets = load(store);
  const issues = [];
  const seen = new Map();
  const add = (kind, row, field, problem, extra = {}) => {
    // A stable key for the app's list; a row can have the same kind of problem twice in one column.
    const key = `${kind}:${row.id}:${field}`;
    seen.set(key, (seen.get(key) || 0) + 1);
    issues.push({
      id: seen.get(key) > 1 ? `${key}:${seen.get(key)}` : key,
      kind,
      ...ref(row, field),
      problem,
      ...extra,
    });
  };
  const observed = sheet => (sheets.get(sheet) || []).filter(r => r.observed);
  const collection = observed('Collection_data');
  const insectary = observed('Insectary_data');
  const today = todaySerial();

  // ---- IDs that must not repeat (the sheet's conditional formats): per sheet, and tubes across the workbook.
  const holders = new Map();
  for (const [sheet, rows] of sheets) {
    const mod = moduleMap.get(sheet);
    const fields = mod.fields.map(f => f.key).filter(k => UNIQUE[sheet]?.includes(k) || TUBE_FIELD.test(k));
    if (!fields.length) continue;
    for (const row of rows) {
      if (!row.observed) continue;
      for (const field of fields) {
        const value = row.values[field];
        if (!isIdValue(value) || typeof value === 'object') continue;
        const key = `${TUBE_FIELD.test(field) ? 'tube' : `${sheet}:${field}`}\u0000${text(value)}`;
        (holders.get(key) || holders.set(key, []).get(key)).push({ row, field });
      }
    }
  }
  for (const [key, list] of holders) {
    if (list.length < 2) continue;
    const value = key.split('\u0000')[1];
    for (const h of list) {
      const others = list.filter(o => o !== h);
      add(
        'repeat',
        h.row,
        h.field,
        `${value} también está en ${others
          .slice(0, 3)
          .map(o =>
            o.row === h.row ? o.field : `${o.row.sheet === h.row.sheet ? '' : `${o.row.sheet} `}fila ${o.row.row}`,
          )
          .join(', ')}${others.length > 3 ? ` y ${others.length - 3} más` : ''}`,
        { related: others.slice(0, 5).map(o => ref(o.row, o.field)) },
      );
    }
  }

  // ---- A CAM given to two butterflies in different sheets (the same butterfly in Collection and Insectary is fine).
  const cams = new Map();
  for (const [sheet, field] of Object.entries(CAM_OWNERS))
    for (const row of observed(sheet)) {
      const value = text(row.values[field]);
      if (isIdValue(value) && !row.formulas[field]) (cams.get(value) || cams.set(value, []).get(value)).push(row);
    }
  const sameButterfly = (a, b) =>
    isIdValue(a.values.Insectary_ID) && text(a.values.Insectary_ID).toUpperCase() === text(b.values.Insectary_ID).toUpperCase();
  for (const [value, list] of cams) {
    if (new Set(list.map(r => r.sheet)).size < 2) continue;
    for (const row of list) {
      const others = list.filter(o => o.sheet !== row.sheet && !sameButterfly(o, row));
      if (!others.length) continue;
      add(
        'cam_cross',
        row,
        CAM_OWNERS[row.sheet],
        `${value} también es el CAM de ${others.map(o => `${o.sheet} fila ${o.row} (${o.label})`).join(', ')}`,
        { related: others.slice(0, 5).map(o => ref(o, CAM_OWNERS[o.sheet])) },
      );
    }
  }

  // ---- Values outside a strict dropdown list (typed values only; formulas are the sheet's business).
  for (const [sheet, rows] of sheets) {
    const lists = Object.entries(listOptions(store, sheet)).filter(([, o]) => o.strict);
    if (!lists.length) continue;
    const byLoose = new Map(
      lists.map(([field, o]) => {
        const index = new Map();
        for (const v of o.values) index.set(loose(v), index.has(loose(v)) ? null : v);
        return [field, index];
      }),
    );
    for (const row of rows) {
      if (!row.observed) continue;
      for (const [field, o] of lists) {
        const value = row.values[field];
        if (row.formulas[field] || value === null || value === undefined || typeof value === 'object') continue;
        const t = text(value);
        if (!t || o.values.has(t)) continue;
        const match = byLoose.get(field).get(loose(t));
        add('list', row, field, `«${t}» no está en la lista de ${field} (${o.source})`, {
          ...(match ? { fix: { recordId: row.id, values: { [field]: match } } } : {}),
        });
      }
    }
  }

  // ---- Wild butterflies sent to the insectary, and their two rows.
  const insectaryById = new Map();
  for (const row of sheets.get('Insectary_data') || []) {
    const id = text(row.values.Insectary_ID).toUpperCase();
    if (id) (insectaryById.get(id) || insectaryById.set(id, []).get(id)).push(row);
  }
  const collectionById = new Map();
  for (const row of collection) {
    const id = text(row.values.Insectary_ID).toUpperCase();
    if (isIdValue(id)) (collectionById.get(id) || collectionById.set(id, []).get(id)).push(row);
  }
  for (const row of collection) {
    if (text(row.values.Release_Collect) !== 'Collected_Sent2Insectary') continue;
    const id = text(row.values.Insectary_ID).toUpperCase();
    if (!isIdValue(id)) {
      add('insectary_link', row, 'Insectary_ID', 'Enviada al insectario sin Insectary_ID');
      continue;
    }
    const rows = insectaryById.get(id) || [];
    const filled = rows.filter(r => r.observed);
    if (!filled.length)
      add(
        'insectary_link',
        row,
        'Insectary_ID',
        rows.length
          ? `La fila de ${id} en Insectary_data (fila ${rows[0].row}) está vacía: falta registrar la mariposa`
          : `${id} no tiene fila en Insectary_data`,
        rows.length ? { related: [ref(rows[0], 'Insectary_ID')] } : {},
      );
  }
  for (const row of insectary) {
    const id = text(row.values.Insectary_ID).toUpperCase();
    if (!isIdValue(id) || !/^wild/i.test(text(row.values.Wild_Reared)) || collectionById.has(id)) continue;
    add('insectary_link', row, 'Insectary_ID', `Mariposa silvestre ${id} sin fila en Collection_data`);
  }

  // Species, sex, CAMs and dates of the same butterfly in both sheets.
  for (const [id, cRows] of collectionById) {
    const iRows = (insectaryById.get(id) || []).filter(r => r.observed);
    for (const c of cRows)
      for (const i of iRows) {
        const cs = binomial(c.values.SPECIES);
        const is = binomial(i.values.SPECIES);
        if (cs && is && !blankOrNA(cs) && !blankOrNA(is) && cs !== is)
          add(
            'link_mismatch',
            i,
            'SPECIES',
            `${id}: Insectary_data dice ${text(i.values.SPECIES)}, Collection_data (fila ${c.row}) dice ${text(c.values.SPECIES)}`,
            { related: [ref(c, 'SPECIES')] },
          );
        const [csx, isx] = [sexOf(c.values.Sex), sexOf(i.values.Sex)];
        if (csx && isx && csx !== isx)
          add(
            'link_mismatch',
            i,
            'Sex',
            `${id}: Insectary_data dice ${text(i.values.Sex)}, Collection_data (fila ${c.row}) dice ${text(c.values.Sex)}`,
            { related: [ref(c, 'Sex')] },
          );
        // The copy of the other sheet's CAM, when typed (it is a formula in most rows).
        for (const [row, field, other, source] of [
          [c, 'CAM_ID_insectary', i, 'CAM_ID'],
          [i, 'CAM_ID_CollData', c, 'CAM_ID'],
        ]) {
          const [mine, theirs] = [text(row.values[field]), text(other.values[source])];
          if (row.formulas[field] || !isIdValue(mine) || !isIdValue(theirs) || mine === theirs) continue;
          add(
            'link_mismatch',
            row,
            field,
            `${id}: ${field} dice ${mine}, pero el CAM_ID de ${other.sheet} (fila ${other.row}) es ${theirs}`,
            { related: [ref(other, source)], fix: { recordId: row.id, values: { [field]: theirs } } },
          );
        }
        const [caught, intro] = [c.values.Collection_date, i.values.Intro2Insectary_date];
        if (isDate(caught) && isDate(intro) && intro < caught)
          add(
            'date_order',
            i,
            'Intro2Insectary_date',
            `${id} entró al insectario (${iso(intro)}) antes de ser colectada (${iso(caught)}, Collection_data fila ${c.row})`,
            { related: [ref(c, 'Collection_date')] },
          );
      }
  }

  // ---- Events before the butterfly was caught or entered the insectary.
  const order = [
    ['Collection_data', 'Death_date', 'Collection_date', 'murió', 'ser colectada'],
    ['Collection_data', 'Preservation_date', 'Collection_date', 'se preservó', 'ser colectada'],
    ['Insectary_data', 'Death_date', 'Intro2Insectary_date', 'murió', 'entrar al insectario'],
    ['Insectary_data', 'Preservation_date', 'Intro2Insectary_date', 'se preservó', 'entrar al insectario'],
    ['Insectary_data', 'Preservation_date', 'Death_date', 'se preservó', 'morir'],
  ];
  for (const [sheet, later, earlier, did, before] of order)
    for (const row of observed(sheet)) {
      const [a, b] = [row.values[later], row.values[earlier]];
      if (!isDate(a) || !isDate(b) || a >= b || row.formulas[later]) continue;
      const slip = yearSlip(a, b, today);
      add('date_order', row, later, `${row.label} ${did} (${iso(a)}) antes de ${before} (${iso(b)})`, {
        related: [ref(row, earlier)],
        ...(slip ? { fix: { recordId: row.id, values: { [later]: iso(slip) } }, fixNote: 'año mal escrito' } : {}),
      });
    }

  // ---- Dates after today (typed ones), and values in a date column that are no date at all.
  for (const [sheet, rows] of sheets) {
    const dates = moduleMap
      .get(sheet)
      .fields.filter(f => f.type === 'date')
      .map(f => f.key);
    if (!dates.length) continue;
    const noteFields = moduleMap
      .get(sheet)
      .fields.map(f => f.key)
      .filter(k => /notes?/i.test(k));
    for (const row of rows) {
      if (!row.observed) continue;
      for (const field of dates) {
        const value = row.values[field];
        if (row.formulas[field] || value === null || value === undefined || typeof value === 'object') continue;
        const broken = badDate(value, today);
        if (broken) {
          add('bad_date', row, field, `${field} ${broken}`, dayFromNotes(row, noteFields, field));
          continue;
        }
        if (!isDate(value) || value <= today) continue;
        const slip = yearSlip(value, today - 366, today);
        add(
          'future_date',
          row,
          field,
          `${field} es ${iso(value)}, después de hoy`,
          slip ? { fix: { recordId: row.id, values: { [field]: iso(slip) } }, fixNote: 'año mal escrito' } : {},
        );
      }
    }
  }

  // ---- Preserved butterflies without their CAM or first tube (monitoring rows get them later, in Colecta or Tubos).
  const preserved = [
    ['Collection_data', row => text(row.values.Release_Collect) === 'Collected_Preserved'],
    ['Insectary_data', row => isDate(row.values.Preservation_date)],
  ];
  for (const [sheet, isPreserved] of preserved)
    for (const row of observed(sheet)) {
      if (!isPreserved(row)) continue;
      if (!isIdValue(row.values.CAM_ID) && !row.formulas.CAM_ID)
        add('missing_sample', row, 'CAM_ID', 'Preservada sin CAM_ID');
      if (
        !isIdValue(row.values.Tube_1_id) &&
        !row.formulas.Tube_1_id &&
        text(row.values.Tube_1_tissue).toUpperCase() !== 'NOT_COLLECTED'
      )
        add('missing_sample', row, 'Tube_1_id', 'Preservada sin Tube_1_id');
    }

  // ---- A field mark on two species: an ID given twice, or a wrong species (docs/monitoring.md).
  const marks = new Map();
  for (const row of collection) {
    if (!hasMark(row.values.FieldMark_ID)) continue;
    const mark = text(row.values.FieldMark_ID).toUpperCase();
    (marks.get(mark) || marks.set(mark, []).get(mark)).push(row);
  }
  const dateOf = row => (isDate(row.values.Collection_date) ? row.values.Collection_date : Infinity);
  for (const [mark, list] of marks) {
    const named = list.filter(r => binomial(r.values.SPECIES));
    if (new Set(named.map(r => binomial(r.values.SPECIES))).size < 2) continue;
    named.sort((a, b) => dateOf(a) - dateOf(b) || a.row - b.row);
    const first = binomial(named[0].values.SPECIES);
    const owners = named.filter(r => binomial(r.values.SPECIES) === first);
    // Once per other species (its first row): its recaptures are the same reused ID, not new problems.
    const firstOfEach = named.filter(
      (r, i) => binomial(r.values.SPECIES) !== first && named.findIndex(o => binomial(o.values.SPECIES) === binomial(r.values.SPECIES)) === i,
    );
    for (const row of firstOfEach)
      add(
        'mark_reuse',
        row,
        'FieldMark_ID',
        `${mark} ya se usó para ${text(owners[0].values.SPECIES)} (fila ${owners[0].row}${
          isDate(owners[0].values.Collection_date) ? `, ${iso(owners[0].values.Collection_date)}` : ''
        }); aquí es ${text(row.values.SPECIES)}`,
        { related: owners.slice(0, 5).map(o => ref(o, 'SPECIES')) },
      );
  }

  // Newest rows first inside each kind and sheet: recent mistakes are the ones people remember.
  return issues.sort(
    (a, b) =>
      KIND_ORDER.indexOf(a.kind) - KIND_ORDER.indexOf(b.kind) || a.sheet.localeCompare(b.sheet) || b.row - a.row,
  );
}

const cache = new WeakMap();
/** Every issue, recomputed only when the local copy (or the day) changed. */
export function allIssues(store) {
  const state = store.db.prepare('SELECT count(*) n, max(updated_at) u FROM records').get();
  const stamp = `${state.n}:${state.u}:${todaySerial()}`;
  const hit = cache.get(store);
  if (hit?.stamp === stamp) return hit;
  const started = Date.now();
  const entry = { stamp, issues: scan(store), checkedAt: new Date().toISOString(), ms: Date.now() - started };
  cache.set(store, entry);
  return entry;
}

/**
 * One page of issues, optionally only some kinds (comma-separated), one sheet,
 * or the rows of one record. Counts per kind and per sheet come with it.
 */
export function checkData(store, { sheet, kind, recordId, limit = 50, offset = 0 } = {}) {
  if (sheet && !moduleMap.has(String(sheet)))
    throw Object.assign(new Error(`Unknown sheet ${String(sheet).slice(0, 60)}`), { status: 404, code: 'MODULE_NOT_FOUND' });
  const kinds = kind
    ? String(kind)
        .split(',')
        .map(k => k.trim())
        .filter(Boolean)
    : [];
  const unknown = kinds.filter(k => !CHECK_KINDS[k]);
  if (unknown.length)
    throw Object.assign(new Error(`Unknown kind ${unknown[0]}; use ${KIND_ORDER.join(', ')}`), {
      status: 400,
      code: 'INVALID_KIND',
    });
  const { issues, checkedAt } = allIssues(store);
  const inSheet = issues.filter(i => (!sheet || i.sheet === sheet) && (!recordId || i.recordId === recordId));
  const counts = Object.fromEntries(KIND_ORDER.map(k => [k, 0]));
  for (const i of inSheet) counts[i.kind]++;
  const sheets = {};
  for (const i of issues) if (!kinds.length || kinds.includes(i.kind)) sheets[i.sheet] = (sheets[i.sheet] || 0) + 1;
  const chosen = kinds.length ? inSheet.filter(i => kinds.includes(i.kind)) : inSheet;
  const size = Math.min(Math.max(Number(limit) || 50, 1), 500);
  const start = Math.max(Number(offset) || 0, 0);
  return {
    checkedAt,
    total: chosen.length,
    offset: start,
    limit: size,
    counts,
    sheets,
    kinds: CHECK_KINDS,
    issues: chosen.slice(start, start + size),
  };
}
