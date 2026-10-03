// "Revisión de datos": inconsistencies across the workbook, found in the local
// copy (never by asking Google). Each issue names the sheet row, the column and
// the value, says what is wrong and, when the right value is obvious, carries a
// fix shaped like a propose_changes change ({ recordId, values }), so the
// assistant can draft the correction and the person confirms it. Wikiloc points
// stored without a row (walk_doubt) name the row they most likely are, if any.
//
// The whole scan runs once per state of the local copy and of the stored walks
// (and per day, for future dates) and is cached, so paging and filtering are free.
//
// Problem texts are Spanish (the assistant reads them); each also goes as a
// descriptor (problemMsg, fixNoteMsg) the interface shows in its language
// (server/messages.mjs). The descriptor does not depend on the language, so the
// cache holds one list for everyone.

import { msg, textFields, tpl } from './messages.mjs';
import { moduleMap, parseDateText } from './schema.mjs';
import { TUBE_FIELD, UNIQUE, blankOrNA, isIdValue } from './verifications.mjs';
import { listOptions } from './verify.mjs';
import { pendingPoints, tracksRevision } from './monitoring.mjs';
import { photoContext, photoIndex, reviewData, reviewRevision } from './photodata.mjs';
import { photoIssues } from './photo-checks.mjs';
import { trackFindings } from './findings.mjs';
import { askFor, sampleGap } from './preserved.mjs';

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
  preserved_na: 'Causa preservada, celdas sin preservar',
  mark_reuse: 'Marca usada en dos especies',
  walk_doubt: 'Punto de Wikiloc sin emparejar',
  // From the photos (server/photo-checks.mjs).
  photo_camid: 'Sobre con otro CAM que la foto',
  photo_extra: 'Fotos de otra mariposa en la carpeta',
  envelope_sex: 'Sexo del sobre distinto',
  envelope_species: 'Especie del sobre distinta',
  photo_missing: 'Preservada sin fotos',
  ai_species: 'La IA ve otra especie',
};
const KIND_ORDER = Object.keys(CHECK_KINDS);

const EPOCH = Date.UTC(1899, 11, 30);
export const iso = serial => new Date(EPOCH + Math.round(serial) * 864e5).toISOString().slice(0, 10);
/** Today in Ecuador as a sheet date serial. */
export const todaySerial = () => {
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
/** Who collected or identified the butterfly, and its day, for the filters of the Revisión tab. */
const WHO_FIELDS = ['Collector', 'Identifier', 'COLLECTED_BY', 'IDENTIFIED_BY', 'Collectors_initials'];
const DATE_FIELDS = ['Collection_date', 'Intro2Insectary_date', 'Preservation_date', 'Death_date', 'Date'];
const hasMark = value => !!text(value) && !/^(NA|N\/A|not given)$/i.test(text(value));

/**
 * CAM columns that give a butterfly its own CAM (one pool each, see Lists).
 * Other sheets (Barcoding_DNA, Pheromones_data…) cite the CAM of a butterfly
 * recorded here, so their CAMs are expected to repeat these.
 */
const CAM_OWNERS = { Collection_data: 'CAM_ID', Insectary_data: 'CAM_ID', Wing_tissue: 'CAM_ID' };

/** The state of the local copy: changes whenever any row is written, synced or removed. */
export function recordsStamp(store) {
  const state = store.db.prepare('SELECT count(*) n, max(updated_at) u FROM records').get();
  return `${state.n}:${state.u}`;
}

/** Loads every recorded row once, in sheet order; placeholders (pre-made rows) are kept apart. */
function load(store) {
  const bySheet = new Map();
  const rows = store.db
    .prepare(
      'SELECT id,sheet,row_num,observed,label,values_json,formulas_json FROM records WHERE missing=0 AND row_num>0 AND row_num<2000000000 ORDER BY sheet,row_num',
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

/**
 * Every row of the local copy by sheet (read-only: shared), as load() gives
 * them. The checks, the suggested edits (server/suggestions/) and the alerts
 * (server/alerts.mjs) all read the whole copy when it changes: the rows are
 * parsed once and kept a minute for the others, then let go (about 100k rows).
 */
let loaded = null;
export function sheetRows(store) {
  const stamp = recordsStamp(store);
  if (loaded?.store === store && loaded.stamp === stamp) return loaded.sheets;
  clearTimeout(loaded?.timer);
  const sheets = load(store);
  loaded = { store, stamp, sheets, timer: setTimeout(() => (loaded = null), 60_000) };
  loaded.timer.unref?.();
  return sheets;
}

export const ref = (row, field) => ({
  sheet: row.sheet,
  row: row.row,
  recordId: row.id,
  label: row.label,
  ...(field ? { field, value: shown(row.sheet, field, row.values[field]) } : {}),
});
/** A cell as people read it: dates of date columns as YYYY-MM-DD. */
export function shown(sheet, field, value) {
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
function badDate(field, value, today) {
  if (typeof value === 'number') {
    if (!Number.isFinite(value)) return msg('{field} tiene «{value}», que no es una fecha', { field, value });
    if (value >= 36526 && value <= today + 731) return null;
    const year = value > 0 && value < 2958466 ? new Date(EPOCH + value * 864e5).getUTCFullYear() : null;
    return year
      ? msg('{field} tiene el número {value} (año {year}), que no es una fecha', { field, value, year })
      : msg('{field} tiene el número {value}, que no es una fecha', { field, value });
  }
  if (typeof value !== 'string') return null;
  const t = value.trim();
  if (!t || /^(NA|N\/A|NOT_COLLECTED|NOT_PROVIDED|unknown|-)$/i.test(t)) return null;
  return msg('{field} es el texto «{value}», no una fecha', { field, value: t });
}

/** The right day of a broken date, when a note starts with it ("21/9/2026 AA: …") or the text reads as a date. */
function dayFromNotes(row, noteFields, field) {
  const value = row.values[field];
  const parsed = typeof value === 'string' ? parseDateText(value) : null;
  if (parsed && parsed >= 36526)
    return { fix: { recordId: row.id, values: { [field]: iso(parsed) } }, fixNote: msg('fecha escrita como texto') };
  for (const key of noteFields) {
    const m = /^\s*(\d{1,2})\/(\d{1,2})\/(\d{2}|\d{4})\b/.exec(text(row.values[key]));
    const day = m && parseDateText(`${m[1]}/${m[2]}/${m[3]}`);
    if (day) return { fix: { recordId: row.id, values: { [field]: iso(day) } }, fixNote: msg('día de {field}', { field: key }) };
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
  const sheets = sheetRows(store);
  const issues = [];
  const seen = new Map();
  /** `problem` and `extra.fixNote`: a msg() (text and descriptor) or a plain text. */
  const add = (kind, row, field, problem, { fixNote, ...extra } = {}) => {
    // A stable key for the app's list; a row can have the same kind of problem twice in one column.
    const key = `${kind}:${row?.id}:${field}`;
    seen.set(key, (seen.get(key) || 0) + 1);
    issues.push({
      id: seen.get(key) > 1 ? `${key}:${seen.get(key)}` : key,
      kind,
      // Photos filed under a CAM that has no row yet: no row to point to.
      ...(row ? ref(row, field) : { sheet: 'Photo_links', row: null, recordId: null, label: extra.label ?? '', field, value: null }),
      ...textFields('problem', problem),
      ...(fixNote ? textFields('fixNote', fixNote) : {}),
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
  // A tube named in two sheets for the same butterfly (Barcoding_DNA or F1/F2 listing a specimen's
  // tubes, the same CAM in both rows) is a reference, not a repeat.
  const camOf = row => text(row.values.CAM_ID || row.values.CAM_ID_CollData || row.values.CAM_ID_insectary);
  const sameSpecimen = (a, b) => a.row.sheet !== b.row.sheet && isIdValue(camOf(a.row)) && camOf(a.row) === camOf(b.row);
  for (const [key, list] of holders) {
    if (list.length < 2) continue;
    const value = key.split('\u0000')[1];
    for (const h of list) {
      const others = list.filter(o => o !== h && !sameSpecimen(o, h));
      if (!others.length) continue;
      const places = others
        .slice(0, 3)
        .map(o =>
          o.row === h.row
            ? o.field
            : o.row.sheet === h.row.sheet
              ? msg('fila {row}', { row: o.row.row })
              : msg('{sheet} fila {row}', { sheet: o.row.sheet, row: o.row.row }),
        );
      add(
        'repeat',
        h.row,
        h.field,
        others.length > 3
          ? msg('{value} también está en {places} y {more} más', { value, places, more: others.length - 3 })
          : msg('{value} también está en {places}', { value, places }),
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
        msg('{value} también es el CAM de {rows}', {
          value,
          rows: others.map(o => msg('{sheet} fila {row} ({label})', { sheet: o.sheet, row: o.row, label: o.label })),
        }),
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
        // The source is a sheet range, or verify.mjs's words for a list typed in the validation rule.
        const source = o.source === 'lista fija de la hoja' ? msg('lista fija de la hoja') : o.source;
        add('list', row, field, msg('«{value}» no está en la lista de {field} ({source})', { value: t, field, source }), {
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
      add('insectary_link', row, 'Insectary_ID', msg('Enviada al insectario sin Insectary_ID'));
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
          ? msg('La fila de {id} en Insectary_data (fila {row}) está vacía: falta registrar la mariposa', {
              id,
              row: rows[0].row,
            })
          : msg('{id} no tiene fila en Insectary_data', { id }),
        rows.length ? { related: [ref(rows[0], 'Insectary_ID')] } : {},
      );
  }
  for (const row of insectary) {
    const id = text(row.values.Insectary_ID).toUpperCase();
    if (!isIdValue(id) || !/^wild/i.test(text(row.values.Wild_Reared)) || collectionById.has(id)) continue;
    add('insectary_link', row, 'Insectary_ID', msg('Mariposa silvestre {id} sin fila en Collection_data', { id }));
  }

  // Species, sex, CAMs and dates of the same butterfly in both sheets.
  const SAYS_BOTH = tpl('{id}: Insectary_data dice {insectary}, Collection_data (fila {row}) dice {collection}');
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
            msg(SAYS_BOTH, { id, insectary: text(i.values.SPECIES), row: c.row, collection: text(c.values.SPECIES) }),
            { related: [ref(c, 'SPECIES')] },
          );
        const [csx, isx] = [sexOf(c.values.Sex), sexOf(i.values.Sex)];
        if (csx && isx && csx !== isx)
          add(
            'link_mismatch',
            i,
            'Sex',
            msg(SAYS_BOTH, { id, insectary: text(i.values.Sex), row: c.row, collection: text(c.values.Sex) }),
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
            msg('{id}: {field} dice {mine}, pero el CAM_ID de {sheet} (fila {row}) es {theirs}', {
              id,
              field,
              mine,
              sheet: other.sheet,
              row: other.row,
              theirs,
            }),
            { related: [ref(other, source)], fix: { recordId: row.id, values: { [field]: theirs } } },
          );
        }
        const [caught, intro] = [c.values.Collection_date, i.values.Intro2Insectary_date];
        if (isDate(caught) && isDate(intro) && intro < caught)
          add(
            'date_order',
            i,
            'Intro2Insectary_date',
            msg('{id} entró al insectario ({intro}) antes de ser colectada ({caught}, Collection_data fila {row})', {
              id,
              intro: iso(intro),
              caught: iso(caught),
              row: c.row,
            }),
            { related: [ref(c, 'Collection_date')] },
          );
      }
  }

  // ---- Events before the butterfly was caught or entered the insectary.
  const order = [
    ['Collection_data', 'Death_date', 'Collection_date', tpl('{label} murió ({later}) antes de ser colectada ({earlier})')],
    ['Collection_data', 'Preservation_date', 'Collection_date', tpl('{label} se preservó ({later}) antes de ser colectada ({earlier})')],
    ['Insectary_data', 'Death_date', 'Intro2Insectary_date', tpl('{label} murió ({later}) antes de entrar al insectario ({earlier})')],
    [
      'Insectary_data',
      'Preservation_date',
      'Intro2Insectary_date',
      tpl('{label} se preservó ({later}) antes de entrar al insectario ({earlier})'),
    ],
    ['Insectary_data', 'Preservation_date', 'Death_date', tpl('{label} se preservó ({later}) antes de morir ({earlier})')],
  ];
  for (const [sheet, later, earlier, problem] of order)
    for (const row of observed(sheet)) {
      const [a, b] = [row.values[later], row.values[earlier]];
      if (!isDate(a) || !isDate(b) || a >= b || row.formulas[later]) continue;
      const slip = yearSlip(a, b, today);
      add('date_order', row, later, msg(problem, { label: row.label, later: iso(a), earlier: iso(b) }), {
        related: [ref(row, earlier)],
        ...(slip ? { fix: { recordId: row.id, values: { [later]: iso(slip) } }, fixNote: msg('año mal escrito') } : {}),
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
        const broken = badDate(field, value, today);
        if (broken) {
          add('bad_date', row, field, broken, dayFromNotes(row, noteFields, field));
          continue;
        }
        if (!isDate(value) || value <= today) continue;
        const slip = yearSlip(value, today - 366, today);
        add(
          'future_date',
          row,
          field,
          msg('{field} es {date}, después de hoy', { field, date: iso(value) }),
          slip ? { fix: { recordId: row.id, values: { [field]: iso(slip) } }, fixNote: msg('año mal escrito') } : {},
        );
      }
    }
  }

  // ---- Preserved butterflies without their CAM or first tube (monitoring rows get them when typed, like the others).
  for (const row of collection) {
    if (text(row.values.Release_Collect) !== 'Collected_Preserved') continue;
    if (!isIdValue(row.values.CAM_ID) && !row.formulas.CAM_ID)
      add('missing_sample', row, 'CAM_ID', msg('Preservada sin CAM_ID'));
    if (
      !isIdValue(row.values.Tube_1_id) &&
      !row.formulas.Tube_1_id &&
      text(row.values.Tube_1_tissue).toUpperCase() !== 'NOT_COLLECTED'
    )
      add('missing_sample', row, 'Tube_1_id', msg('Preservada sin Tube_1_id'));
  }
  // Insectary rows: preserved by their preservation cells, and whom to ask (server/preserved.mjs).
  for (const row of insectary) {
    const gap = sampleGap(row.values, row.formulas);
    if (!gap) continue;
    const ask = askFor(store.db, row);
    const vars = { why: gap.why, ...(ask.length ? { who: ask } : {}) };
    if (gap.kind === 'preserved_na')
      add(
        'preserved_na',
        row,
        'Death_cause',
        ask.length
          ? msg('Death_cause dice preservada ({why}), pero CAM_ID y los tubos dicen NA (no preservada); pregunta a {who}', vars)
          : msg('Death_cause dice preservada ({why}), pero CAM_ID y los tubos dicen NA (no preservada)', vars),
        { ask, related: [ref(row, 'CAM_ID'), ref(row, 'Tube_1_id')] },
      );
    else
      for (const field of gap.missing)
        add(
          'missing_sample',
          row,
          field,
          ask.length
            ? msg('Preservada ({why}) sin {field}; pregunta a {who}', { ...vars, field })
            : msg('Preservada ({why}) sin {field}', { ...vars, field }),
          { ask },
        );
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
        isDate(owners[0].values.Collection_date)
          ? msg('{mark} ya se usó para {species} (fila {row}, {date}); aquí es {here}', {
              mark,
              species: text(owners[0].values.SPECIES),
              row: owners[0].row,
              date: iso(owners[0].values.Collection_date),
              here: text(row.values.SPECIES),
            })
          : msg('{mark} ya se usó para {species} (fila {row}); aquí es {here}', {
              mark,
              species: text(owners[0].values.SPECIES),
              row: owners[0].row,
              here: text(row.values.SPECIES),
            }),
        { related: owners.slice(0, 5).map(o => ref(o, 'SPECIES')) },
      );
  }

  // ---- Wikiloc points stored on the map without a row because their pairing was doubtful (docs/monitoring.md).
  // No fix: which row it is needs the photos, or the collector.
  const byId = new Map(collection.map(r => [r.id, r]));
  const day = date => date.split('-').reverse().join('/');
  const clock = m => (m === null ? '' : `${Math.floor(m / 60)}:${String(m % 60).padStart(2, '0')}`);
  const sexWord = s => (s === 'female' ? msg('hembra') : s === 'male' ? msg('macho') : '');
  const rowText = r =>
    msg('fila {row} ({details})', {
      row: r.row,
      details: [r.species || msg('sin especie'), sexWord(r.sex), clock(r.minutes), r.markId].filter(Boolean),
    });
  const disagree = tpl('la nota no coincide del todo con su fila');
  const WHY = {
    tie: tpl('empate con otra fila'),
    order: tpl('solo por el orden del recorrido'),
    sure: disagree,
    mark: disagree,
    none: tpl('ninguna fila encaja'),
  };
  const CONFLICT = { especie: tpl('especie'), sexo: tpl('sexo'), marca: tpl('marca') };
  const MAYBE = [
    tpl('No hay filas libres de ese día.'),
    tpl('Puede ser la {a}.'),
    tpl('Puede ser la {a} o la {b}.'),
    tpl('Puede ser la {a} o la {b} o la {c}.'),
  ];
  for (const d of pendingPoints(store)) {
    const proposed = d.proposed.find(Boolean) || null;
    const options = [proposed, ...d.candidates.filter(r => r.recordId !== proposed?.recordId)].filter(Boolean).slice(0, 3);
    const conflicts = d.conflicts.filter(c => c !== 'hora');
    const vars = {
      text: d.text,
      day: day(d.date),
      collector: d.collector || msg('sin colector'),
      why: msg(WHY[d.confidence] ?? String(d.confidence)),
      options: msg(MAYBE[options.length], Object.fromEntries(options.map((r, i) => ['abc'[i], rowText(r)]))),
    };
    const problem = conflicts.length
      ? msg(
          'Punto «{text}» del recorrido del {day} ({collector}) guardado sin fila: {why} (no coincide: {conflicts}). {options} Emparéjalo en Monitoreo → Dudas.',
          { ...vars, conflicts: conflicts.map(c => (CONFLICT[c] ? msg(CONFLICT[c]) : c)) },
        )
      : msg('Punto «{text}» del recorrido del {day} ({collector}) guardado sin fila: {why}. {options} Emparéjalo en Monitoreo → Dudas.', vars);
    issues.push({
      id: `walk_doubt:${d.trackId}:${d.indexes[0]}`,
      kind: 'walk_doubt',
      sheet: 'Collection_data',
      // The row it would most likely be; none when nothing fits.
      row: proposed?.row ?? null,
      recordId: proposed?.recordId ?? null,
      label: `Wikiloc ${day(d.date)} ${text(d.collector).split(' - ')[0]}`,
      field: 'Wikiloc',
      value: d.text,
      ...textFields('problem', problem),
      link: '#/monitoreo?vista=dudas',
      walk: { trackId: d.trackId, index: d.indexes[0], date: d.date, collector: d.collector, name: d.name, wikiloc: d.wikiloc },
      related: options.flatMap(r => (byId.has(r.recordId) ? [ref(byId.get(r.recordId), 'SPECIES')] : [])),
    });
  }

  photoIssues(store, { sheets, add, ref, today });

  // Who and when, for the Revisión tab's filters; and the photos of every issue about a butterfly with a CAM.
  const rows = new Map([...sheets.values()].flat().map(r => [r.id, r]));
  const data = reviewData(store);
  const index = photoIndex(store);
  for (const issue of issues) {
    const row = issue.recordId ? rows.get(issue.recordId) : null;
    if (!row) continue;
    const who = WHO_FIELDS.map(k => text(row.values[k])).filter(v => v && !/^(NA|N\/A|NOT_PROVIDED)$/i.test(v));
    if (who.length) issue.who = [...new Set(who)];
    const day = DATE_FIELDS.map(k => row.values[k]).find(isDate);
    if (day) issue.date = iso(day);
    const cam = text(row.values.CAM_ID).toUpperCase();
    if (issue.photos || !/^CAM\d+$/.test(cam)) continue;
    const found = photoContext(data, index, cam);
    if (found) Object.assign(issue, { cam, ...found });
  }

  // Newest rows first inside each kind and sheet: recent mistakes are the ones people remember (newest walks, for walk points).
  return issues.sort(
    (a, b) =>
      KIND_ORDER.indexOf(a.kind) - KIND_ORDER.indexOf(b.kind) ||
      a.sheet.localeCompare(b.sheet) ||
      (a.group && b.group ? b.group.size - a.group.size || a.group.key.localeCompare(b.group.key) : 0) ||
      (a.walk && b.walk ? b.walk.date.localeCompare(a.walk.date) || a.walk.index - b.walk.index : (b.row ?? 0) - (a.row ?? 0)),
  );
}

const cache = new WeakMap();
/** Every issue, recomputed only when the local copy (or the day) changed. */
export function allIssues(store) {
  // The stored walks and the imported photo readings too (walk_doubt, the photo kinds).
  const stamp = `${recordsStamp(store)}:${todaySerial()}:${tracksRevision(store)}:${reviewRevision(store.db)}`;
  const hit = cache.get(store);
  if (hit?.stamp === stamp) return hit;
  const started = Date.now();
  const entry = { stamp, issues: scan(store), checkedAt: new Date().toISOString(), ms: Date.now() - started };
  // First seen / solved (the Revisión tab's «Resueltos»): issues no longer found are solved.
  trackFindings(store, 'check', entry.issues.map(checkFinding), { at: entry.checkedAt });
  cache.set(store, entry);
  return entry;
}

/** What the solved list keeps of an issue (server/findings.mjs). */
const checkFinding = issue => ({
  key: issue.id,
  kind: issue.kind,
  sheet: issue.sheet,
  row: issue.row,
  recordId: issue.recordId,
  field: issue.field,
  label: issue.label,
  value: issue.value,
  text: issue.problem,
  textMsg: issue.problemMsg,
  // The other rows involved: fixing one of them can solve it too (a repeated tube).
  others: (issue.related ?? []).map(r => r.recordId).filter(Boolean),
});

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
