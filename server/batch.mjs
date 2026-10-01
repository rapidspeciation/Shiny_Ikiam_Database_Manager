// Every write to Google Sheets goes through applyBatch.
//
// A batch holds edits to existing rows and new rows, possibly across sheets.
// It is checked against the live Sheet, written with one atomic batchUpdate,
// read back to verify, and recorded as a single history action so it can be
// undone as a unit. Google is contacted three times regardless of batch size:
// one read, one write, one verification read.

import { randomUUID } from 'node:crypto';
import { comparable, isSumField, labelFor, moduleMap, simpleSum, validateValues } from './schema.mjs';
import { hasDateFormat, hasTimeFormat, rowKey, rowValues } from './sheets.mjs';
import { describeProblems, headerLayout, sameLayout } from './columns.mjs';
import { ensurePremadeRows, insectaryIdRow } from './premade.mjs';
import { cleanPurpose, inferPurpose } from './history.mjs';
import { TUBE_FIELD, UNIQUE, isIdValue, isUnique, twinRows } from './verifications.mjs';
import { listOptions, listProblemMsg } from './verify.mjs';
import { msg, msgError, textFields } from './messages.mjs';

/** Where a write came from. Chosen by the server, never by the client. */
export const SOURCES = new Set(['app', 'undo', 'ai_approved', 'import']);

export const MAX_BATCH = 500;

// Identifiers that must not repeat (server/verifications.mjs, as in the Google
// Sheet's conditional formats). Tube IDs are unique across the whole workbook.

// Formula cells that may be typed over, and only with a value different from what the
// formula predicts: the species of an insectary butterfly when what emerged is not what
// the clutch predicted. The formula is kept in history, so undo puts it back.
export const TYPED_OVER_FORMULA = { Insectary_data: new Set(['SPECIES', 'Collection_location']) };

/** What the SPECIES formula of an insectary row will give: the species of its clutch in Insectary_stocks. */
function predictedSpecies(store, sheet, field, values) {
  if (sheet !== 'Insectary_data' || field !== 'SPECIES' || values['CLUTCH NUMBER'] == null) return undefined;
  return store.db
    .prepare(
      `SELECT json_extract(values_json,'$.SPECIES') s FROM records WHERE sheet='Insectary_stocks' AND missing=0 AND trim(CAST(json_extract(values_json,'$."CLUTCH NUMBER"') AS TEXT))=?`,
    )
    .get(String(values['CLUTCH NUMBER']).trim())?.s;
}

const blank = value => value === null || value === undefined || /^\s*(|NA|N\/A)\s*$/i.test(String(value));
const cellValue = (values, formulas, field) => (formulas[field] ? { formula: formulas[field] } : values[field]);
/** `message`: a text, or a msg() when it has values in it (its descriptor goes to the app, server/messages.mjs). */
const fail = (code, message, status = 400, details) => msgError(message, { code, status, details });

/**
 * `purpose`: the flow a save belongs to (history.mjs PURPOSES). A save from the
 * app may declare its tab in `body.purpose`; otherwise it is inferred from what it changes.
 */
export async function applyBatch(store, body, user, { source = 'app', reverses = null, purpose = null } = {}) {
  store.validateRole(user);
  store.requireRequestId(body.requestId);
  if (!SOURCES.has(source)) throw fail('INVALID_SOURCE', 'Origen de escritura desconocido');
  const edits = Array.isArray(body.edits) ? body.edits : [];
  const creates = Array.isArray(body.creates) ? body.creates : [];
  if (edits.length + creates.length > MAX_BATCH)
    throw fail('BATCH_TOO_LARGE', msg('Guarda como máximo {n} filas a la vez', { n: MAX_BATCH }));

  return store.runExclusive(async () => {
    // A retried request returns the original outcome instead of writing twice.
    const prior = store.actionByRequest(body.requestId);
    if (prior) {
      if (prior.action.actor !== (user.id || user.username))
        throw fail('REQUEST_ID_CONFLICT', 'Este ID de solicitud pertenece a otra persona', 409);
      if (prior.status === 'failed') {
        // Nothing was written last time, so the same request may be tried again.
        store.db.prepare('UPDATE actions SET request_id=NULL WHERE id=?').run(prior.action.id);
      } else if (prior.status !== 'verified') {
        throw fail(
          'WRITE_UNCERTAIN',
          'El intento anterior aún se está confirmando; vuelve a intentarlo en un momento',
          409,
          {
            actionId: prior.action.id,
          },
        );
      } else
        return { ...prior, actions: [prior.action], records: prior.records || (prior.record ? [prior.record] : []) };
    }
    if (!edits.length && !creates.length) throw fail('INVALID_VALUES', 'No hay nada que guardar');
    // With `partial`, a change that cannot be saved (a repeated CAM, a cell changed by
    // someone else…) is left out and reported in `skipped`, and everything else is saved.
    // Without it (undo, the assistant) the batch stays all or nothing.
    const partial = body.partial === true && source === 'app';
    const skipped = [];
    let input = { edits, creates };
    let plan = planFor(store, source, input);
    for (let round = 0; partial && plan.conflicts.length && round < 5; round++) {
      const rest = withoutConflicts(input, plan.conflicts);
      if (!rest) break;
      skipped.push(...plan.conflicts);
      input = rest;
      plan = planFor(store, source, input);
    }
    throwIfConflicts(plan, skipped);
    // New rows past the sheet's pre-made rows would be bare (no formulas, no dropdowns):
    // make more pre-made rows first, as the team would by dragging the last one down.
    await ensurePremadeRows(store, plan.newRowNeeds());

    const live = await store.sheets.readRows(plan.readTargets());
    await plan.resolve(live);
    for (let round = 0; partial && plan.conflicts.length && round < 5; round++) {
      const rest = withoutConflicts(input, plan.conflicts);
      if (!rest) break;
      skipped.push(...plan.conflicts);
      input = rest;
      plan = planFor(store, source, input);
      if (plan.conflicts.length) continue;
      // The smaller batch touches the same rows or fewer; read any row not read yet.
      const missing = plan
        .readTargets()
        .map(({ sheet, rows }) => ({ sheet, rows: rows.filter(row => !live.has(rowKey(sheet, row))) }))
        .filter(t => t.rows.length);
      if (missing.length) for (const [key, row] of await store.sheets.readRows(missing)) live.set(key, row);
      await plan.resolve(live);
    }
    throwIfConflicts(plan, skipped);
    plan.skipped = skipped;
    if (!plan.writes.length)
      return { status: 'unchanged', action: null, actions: [], records: [], created: [], skipped };

    const declared = source === 'app' ? purpose || cleanPurpose(body.purpose) : null;
    const actionId = beginAction(
      store,
      {
        requestId: body.requestId,
        user,
        source,
        reason: body.reason,
        reverses,
        purpose: declared || inferPurpose({ source, reason: body.reason }, plannedChanges(plan)),
      },
      plan,
    );
    const sheets = [...new Set(plan.writes.map(w => w.sheet))];
    store.startWrite(sheets);
    try {
      try {
        await store.sheets.writeBatch(plan.writes);
      } catch (e) {
        // Google applies batchUpdate atomically: a rejected request wrote nothing.
        const rejected = e.status >= 400 && e.status < 500;
        store.finishAction(actionId, rejected ? 'failed' : 'uncertain', null);
        if (!rejected) scheduleRecovery(store);
        throw fail(
          rejected ? 'WRITE_REJECTED' : 'WRITE_UNCERTAIN',
          rejected
            ? 'Google Sheets rechazó el cambio; no se guardó nada'
            : 'No se pudo confirmar la escritura en Google Sheets',
          rejected ? 502 : 503,
          { actionId, cause: e.message?.slice(0, 300) },
        );
      }
      let check;
      try {
        check = await store.sheets.readRows(plan.writeTargets());
      } catch {
        store.finishAction(actionId, 'uncertain', null);
        scheduleRecovery(store);
        throw fail('WRITE_UNCERTAIN', 'No se pudo confirmar la escritura en Google Sheets', 503, { actionId });
      }
      const records = plan.verifyAndPersist(check);
      if (!records) {
        scheduleRecovery(store);
        store.finishAction(actionId, 'uncertain', null);
        throw fail('WRITE_UNCERTAIN', 'No se pudo verificar la escritura en Google Sheets', 503, { actionId });
      }
      const created = plan.targets.filter(t => t.clientId).map(t => ({ clientId: t.clientId, recordId: t.record.id }));
      const action = store.action(store.db.prepare('SELECT * FROM actions WHERE id=?').get(actionId));
      action.status = 'verified';
      const result = { status: 'verified', action, actions: [action], records, created, skipped };
      // `skipped` is kept with the outcome, so a retried request reports the same cells left out.
      store.finishAction(actionId, 'verified', { records, record: records[0], created, status: 'verified', skipped });
      return result;
    } finally {
      store.endWrite(sheets);
    }
  });
}

function planFor(store, source, { edits, creates }) {
  const plan = new Plan(store, source);
  plan.addEdits(edits);
  plan.addCreates(creates);
  return plan;
}

function throwIfConflicts(plan, skipped) {
  if (plan.conflicts.length || (skipped.length && !plan.targets.length))
    throw fail('BATCH_CONFLICT', 'Algunos cambios necesitan revisión; no se guardó nada', 409, {
      items: [...skipped, ...plan.conflicts],
    });
}

/**
 * The batch without the changes that conflict: the field of an edit when the
 * conflict names one, else the whole row. New rows are saved together or not at
 * all: rows typed together are often linked (a field butterfly's Collection_data
 * and Insectary_data rows). Null when a conflict belongs to no single change
 * (e.g. the sheet's columns changed).
 */
export function withoutConflicts({ edits, creates }, conflicts) {
  if (conflicts.some(c => !c.id && !c.clientId)) return null;
  const rows = new Set(conflicts.filter(c => c.id && !c.field).map(c => c.id));
  const fields = new Set(conflicts.filter(c => c.id && c.field).map(c => `${c.id}\u0000${c.field}`));
  const keptEdits = [];
  edits.forEach(edit => {
    if (rows.has(edit?.id)) return;
    const values = Object.fromEntries(
      Object.entries(edit?.values || {}).filter(([field]) => !fields.has(`${edit.id}\u0000${field}`)),
    );
    if (Object.keys(values).length) keptEdits.push({ ...edit, values });
  });
  return { edits: keptEdits, creates: conflicts.some(c => c.clientId) ? [] : creates };
}

/** Collects, validates and resolves the rows a batch touches. */
class Plan {
  constructor(store, source) {
    this.store = store;
    this.source = source;
    this.conflicts = [];
    /** One entry per affected row: { sheet, row, record, clean, expected, clientId?, candidates? } */
    this.targets = [];
    this.writes = [];
    /** Live column map of each sheet, from the header row read with the batch. */
    this.layouts = new Map();
    this.pools = [];
  }

  /** `message`: a text or a msg() (message and messageMsg). */
  conflict(target, code, message, extra = {}) {
    this.conflicts.push({
      id: target?.editId ?? target?.record?.id ?? null,
      clientId: target?.clientId ?? null,
      index: target?.index ?? null,
      code,
      ...textFields('message', message),
      ...extra,
    });
  }

  validate(target, module, values) {
    try {
      return validateValues(module, values, {
        allowFormula: this.source === 'undo',
        normalize: this.source !== 'undo',
      });
    } catch (e) {
      this.conflict(target, e.code || 'INVALID_VALUES', e.messageMsg ? { text: e.message, msg: e.messageMsg } : e.message, {
        field: e.field ?? null,
      });
      return null;
    }
  }

  addEdits(edits) {
    const seen = new Set();
    edits.forEach((edit, index) => {
      const target = { index, editId: edit?.id, expected: edit?.expected || null, raw: edit?.values || {} };
      const record = typeof edit?.id === 'string' ? this.store.getRecord(edit.id) : null;
      const allowed = record && TYPED_OVER_FORMULA[record.sheet];
      target.replaceFormula = new Set(
        Array.isArray(edit?.replaceFormula) ? edit.replaceFormula.filter(f => allowed?.has(f)) : [],
      );
      if (!record || record.missing || record.row <= 0)
        return this.conflict(target, 'RECORD_NOT_FOUND', 'La fila ya no está disponible; recarga la tabla');
      if (seen.has(record.id))
        return this.conflict(target, 'DUPLICATE_EDIT', 'La misma fila aparece dos veces en un guardado');
      seen.add(record.id);
      Object.assign(target, { sheet: record.sheet, row: record.row, record });
      if (!target.expected && edit.expectedVersion !== undefined && Number(edit.expectedVersion) !== record.version)
        return this.conflict(target, 'VERSION_CONFLICT', 'La fila cambió desde que se abrió');
      target.clean = this.validate(target, record.sheet, edit.values);
      if (target.clean && !Object.keys(target.clean).length)
        return this.conflict(target, 'INVALID_VALUES', 'No hay columnas que actualizar');
      if (target.clean) this.targets.push(target);
    });
  }

  addCreates(creates) {
    const byModule = new Map();
    const reach = new Map(); // sheet → the furthest row an Insectary ID ahead of the pre-made rows needs
    creates.forEach((create, index) => {
      const target = { index, clientId: create?.clientId || `new-${index}`, sheet: create?.module };
      const allowed = TYPED_OVER_FORMULA[create?.module];
      target.replaceFormula = new Set(
        Array.isArray(create?.replaceFormula) ? create.replaceFormula.filter(f => allowed?.has(f)) : [],
      );
      if (!moduleMap.has(create?.module)) return this.conflict(target, 'MODULE_NOT_FOUND', 'Hoja desconocida');
      // If an earlier save to this sheet may or may not have landed, a new row could
      // duplicate it. Wait until that save is confirmed (this happens automatically).
      const unsettled = this.store.db
        .prepare(
          "SELECT 1 FROM actions a WHERE a.status IN ('pending','uncertain') AND EXISTS(SELECT 1 FROM changes c WHERE c.action_id=a.id AND c.sheet=?) LIMIT 1",
        )
        .get(create.module);
      if (unsettled)
        return this.conflict(
          target,
          'WRITE_UNCERTAIN',
          msg('Un guardado anterior en {sheet} aún se está confirmando; vuelve a intentarlo en un minuto', {
            sheet: create.module,
          }),
        );
      target.clean = this.validate(target, create.module, create.values);
      if (!target.clean) return;
      if (!Object.values(target.clean).some(v => !blank(v)))
        return this.conflict(target, 'INVALID_VALUES', 'Una fila nueva necesita al menos un valor');
      const placeholderId = create.module === 'Insectary_data' ? target.clean.Insectary_ID : null;
      if (placeholderId) {
        const matches = this.store.db
          .prepare(
            "SELECT * FROM records WHERE sheet='Insectary_data' AND missing=0 AND json_extract(values_json,'$.Insectary_ID')=?",
          )
          .all(placeholderId);
        if (matches.some(r => r.observed))
          return this.conflict(target, 'DUPLICATE_ID', msg('Insectary_ID {id} ya está registrado', { id: placeholderId }), {
            field: 'Insectary_ID',
          });
        if (matches.length > 1)
          return this.conflict(target, 'IDENTITY_CONFLICT', msg('Hay más de una fila sin usar con el ID {id}', { id: placeholderId }));
        if (matches.length === 1) target.candidates = [matches[0].row_num];
        // An ID past the pre-made rows (a notebook page ahead of them): they are made up to its row.
        else {
          const ahead = insectaryIdRow(this.store, placeholderId);
          if (ahead) reach.set(create.module, Math.max(reach.get(create.module) ?? 0, ahead.row));
        }
      }
      if (!target.candidates) byModule.set(create.module, [...(byModule.get(create.module) || []), target]);
      this.targets.push(target);
    });
    // New rows go into the first unused rows after the last recorded one, which
    // may already hold pre-filled formulas. A few spare rows are read in case
    // someone has just typed into one of them in Google Sheets.
    for (const [module, targets] of byModule) {
      const reserved = new Set(this.targets.filter(t => t.sheet === module && t.candidates).flatMap(t => t.candidates));
      const last =
        this.store.db
          .prepare('SELECT max(row_num) n FROM records WHERE sheet=? AND missing=0 AND observed=1')
          .get(module).n || moduleMap.get(module).headerRow;
      const pool = [];
      const end = reach.get(module) ?? 0;
      for (let row = last + 1; pool.length < targets.length + 5 || row <= end; row++) if (!reserved.has(row)) pool.push(row);
      for (const target of targets) target.candidates = pool;
      this.pools.push({ sheet: module, lastRow: Math.max(pool[targets.length - 1], end) });
    }
  }

  /** For each sheet getting new rows at the end: the last row they may need. */
  newRowNeeds() {
    return this.pools;
  }

  readTargets() {
    const bySheet = new Map();
    for (const t of this.targets) {
      const rows = bySheet.get(t.sheet) || new Set([moduleMap.get(t.sheet).headerRow]);
      for (const row of t.candidates || [t.row]) rows.add(row);
      bySheet.set(t.sheet, rows);
    }
    return [...bySheet].map(([sheet, rows]) => ({ sheet, rows: [...rows] }));
  }

  writeTargets() {
    const bySheet = new Map();
    for (const w of this.writes)
      bySheet.set(w.sheet, [...(bySheet.get(w.sheet) || [moduleMap.get(w.sheet).headerRow]), w.row]);
    return [...bySheet].map(([sheet, rows]) => ({ sheet, rows }));
  }

  async resolve(live) {
    const brokenSheets = new Set();
    this.layouts = new Map();
    for (const sheet of new Set(this.targets.map(t => t.sheet))) {
      // Fields are mapped to columns by the header read now, just before writing.
      const layout = headerLayout(sheet, live.get(rowKey(sheet, moduleMap.get(sheet).headerRow)));
      this.layouts.set(sheet, layout);
      if (layout.blocked) {
        const problems = layout.problems.filter(p => p.blocking);
        brokenSheets.add(sheet);
        this.conflict(
          null,
          'HEADER_MISMATCH',
          `No se guarda en ${sheet}: ${describeProblems(sheet, problems)}. Corrige los encabezados en Google Sheets`,
          { sheet, problems: problems.slice(0, 10) },
        );
      }
    }
    // A field whose column is gone cannot be saved; the rest of the row can.
    const unavailable = target => {
      const layout = this.layouts.get(target.sheet);
      const missing = Object.keys(target.clean).filter(field => !layout.columns.has(field));
      for (const field of missing)
        this.conflict(
          target,
          'COLUMN_MISSING',
          msg('Falta la columna {field} en {sheet} (Google Sheets); ese valor no se puede guardar', { field, sheet: target.sheet }),
          { field },
        );
      return missing.length > 0;
    };
    // Rows being edited are resolved first and reserved, so a new row never lands on them.
    const used = new Set();
    for (const target of this.targets.filter(t => t.record && !brokenSheets.has(t.sheet))) {
      if (unavailable(target)) continue;
      await this.resolveEdit(target, live);
      used.add(`${target.sheet}:${target.row}`);
    }
    for (const target of this.targets.filter(t => !t.record && !brokenSheets.has(t.sheet)))
      if (!unavailable(target)) this.resolveCreate(target, live, used);
    const written = new Set();
    for (const write of this.writes) {
      const key = `${write.sheet}:${write.row}`;
      if (written.has(key))
        this.conflict(
          null,
          'ROW_COLLISION',
          msg('Dos cambios de este guardado van a {sheet} fila {row}', { sheet: write.sheet, row: write.row }),
        );
      written.add(key);
    }
    this.checkUniqueIds();
    this.checkLists();
  }

  async resolveEdit(target, live) {
    const { record } = target;
    const layout = this.layouts.get(record.sheet);
    let liveRow = live.get(rowKey(record.sheet, record.row));
    const expectedIdentity = this.store.identity(record.sheet, record.values);
    const current = rowValues(record.sheet, liveRow, layout);
    // Columns missing from the sheet keep their last known value in the app: compare the rest.
    const present = values => Object.fromEntries(Object.entries(values).filter(([k]) => layout.columns.has(k)));
    if (Object.keys(expectedIdentity).length) {
      if (comparable(this.store.identity(record.sheet, current.values)) !== comparable(expectedIdentity)) {
        liveRow = await this.findMovedRow(record, expectedIdentity);
        if (!liveRow)
          return this.conflict(
            target,
            'ROW_MOVED',
            'La fila se movió o su identificador cambió en Google Sheets; recarga la tabla',
          );
        target.row = liveRow.row;
      }
    } else if (
      comparable(present(record.values)) !== comparable(current.values) ||
      comparable(present(record.formulas)) !== comparable(current.formulas)
    ) {
      return this.conflict(
        target,
        'ROW_CHANGED',
        'Esta fila cambió en Google Sheets; recarga la tabla antes de editarla',
      );
    }
    const before = rowValues(record.sheet, liveRow, layout);
    const changes = [];
    for (const [field, after] of Object.entries(target.clean)) {
      const replacing = !!before.formulas[field] && target.replaceFormula.has(field);
      // A count kept as a sum (=12+15) may be rewritten; any other formula stays the sheet's.
      const sumCell = isSumField(record.sheet, field) && !!simpleSum(before.formulas[field]);
      if (before.formulas[field] && this.source !== 'undo' && !replacing && !sumCell)
        return this.conflict(target, 'FORMULA_CELL', msg('{field} se calcula con una fórmula de la hoja', { field }), { field });
      if (replacing) {
        const predicted = before.values[field] ?? null;
        if (comparable(predicted) === comparable(after))
          return this.conflict(target, 'MATCHES_FORMULA', msg('{field} ya da {value}; no hace falta escribirlo', { field, value: after }), {
            field,
          });
        if (
          target.expected &&
          Object.hasOwn(target.expected, field) &&
          comparable(target.expected[field]) !== comparable(predicted)
        )
          return this.conflict(target, 'EXTERNAL_CONFLICT', msg('Otra persona cambió {field} en la hoja', { field }), {
            field,
            expected: target.expected[field],
            actual: predicted,
          });
        changes.push({ field, before: { formula: before.formulas[field] }, after });
        continue;
      }
      const actual = cellValue(before.values, before.formulas, field);
      let expected =
        target.expected && Object.hasOwn(target.expected, field)
          ? target.expected[field]
          : cellValue(record.values, record.formulas, field);
      // The sum a person saw may be sent as its text ("=12+15").
      if (sumCell && typeof expected === 'string' && simpleSum(expected)) expected = { formula: simpleSum(expected) };
      // …or as the number it shows (27).
      if (sumCell && typeof expected === 'number' && comparable(expected) === comparable(before.values[field] ?? null))
        expected = cellValue(before.values, before.formulas, field);
      // Typing the text a cell already holds (e.g. "944" stored as text) is not a change.
      if (typeof actual === 'string' && target.raw[field] === actual) continue;
      if (comparable(actual) !== comparable(expected ?? null))
        this.conflict(target, 'EXTERNAL_CONFLICT', msg('Otra persona cambió {field} en la hoja', { field }), {
          field,
          expected: expected ?? null,
          actual: actual ?? null,
        });
      else if (comparable(actual) !== comparable(after)) changes.push({ field, before: actual ?? null, after });
    }
    this.addWrite(target, liveRow, before, changes);
  }

  async findMovedRow(record, identity) {
    this.movedCache ??= new Map();
    if (!this.movedCache.has(record.sheet))
      this.movedCache.set(record.sheet, await this.store.sheets.readSheet(record.sheet));
    const rows = this.movedCache.get(record.sheet);
    const headerRow = moduleMap.get(record.sheet).headerRow;
    const layout = headerLayout(record.sheet, rows.find(r => r.row === headerRow));
    // Columns rearranged since the batch's own read: the row found would be written with a stale map.
    if (!sameLayout(layout, this.layouts.get(record.sheet))) return null;
    const matches = rows.filter(
      r =>
        r.row > headerRow &&
        comparable(this.store.identity(record.sheet, rowValues(record.sheet, r, layout).values)) ===
          comparable(identity),
    );
    return matches.length === 1 ? matches[0] : null;
  }

  resolveCreate(target, live, used) {
    for (const row of target.candidates) {
      if (used.has(`${target.sheet}:${row}`)) continue;
      const liveRow = live.get(rowKey(target.sheet, row));
      const before = rowValues(target.sheet, liveRow, this.layouts.get(target.sheet));
      if (this.store.hasObservation(target.sheet, before.values, before.formulas)) continue;
      // With a column missing, the row may be in use through that column: trust the app's last read.
      if (this.layouts.get(target.sheet).missing.length && this.store.getRecordBySheetRow(target.sheet, row)?.observed)
        continue;
      // Someone may have typed into this row in Google Sheets: never overwrite a value.
      const typedOver = Object.entries(target.clean).some(
        ([field, after]) =>
          !before.formulas[field] &&
          !blank(before.values[field]) &&
          comparable(before.values[field]) !== comparable(after),
      );
      if (typedOver) continue;
      if (
        target.sheet === 'Insectary_data' &&
        target.clean.Insectary_ID &&
        before.values.Insectary_ID !== target.clean.Insectary_ID
      )
        continue;
      used.add(`${target.sheet}:${row}`);
      target.row = row;
      const existing = this.store.getRecordBySheetRow(target.sheet, row);
      target.recordId = existing && !existing.observed ? existing.id : randomUUID();
      target.version = existing && !existing.observed ? existing.version : 0;
      const changes = [];
      for (const [field, after] of Object.entries(target.clean)) {
        if (comparable(before.values[field] ?? null) === comparable(after)) continue;
        if (before.formulas[field]) {
          // A pre-made row's count kept as a sum takes the notebook's sum.
          if (isSumField(target.sheet, field) && simpleSum(before.formulas[field])) {
            if (comparable({ formula: before.formulas[field] }) !== comparable(after))
              changes.push({ field, before: { formula: before.formulas[field] }, after });
            continue;
          }
          // e.g. a butterfly of another subspecies than its clutch predicts.
          if (target.replaceFormula.has(field)) {
            // The new row's formula has no clutch to work from yet, so compare with the clutch's species.
            if (comparable(predictedSpecies(this.store, target.sheet, field, target.clean)) === comparable(after))
              continue;
            changes.push({ field, before: { formula: before.formulas[field] }, after });
            continue;
          }
          return this.conflict(target, 'FORMULA_CELL', msg('{field} se calcula con una fórmula en la fila nueva', { field }), {
            field,
          });
        }
        // "NA" is written on purpose (the workbook uses it); only empty values are skipped.
        if (after !== null && after !== '') changes.push({ field, before: before.values[field] ?? null, after });
      }
      if (!changes.length) return this.conflict(target, 'INVALID_VALUES', 'La fila nueva no tiene nada que guardar');
      return this.addWrite(target, liveRow, before, changes);
    }
    this.conflict(
      target,
      'NO_FREE_ROW',
      target.sheet === 'Insectary_data' && target.clean.Insectary_ID
        ? msg('La fila sin usar de {id} ya no está libre; recarga y elige otro ID', { id: target.clean.Insectary_ID })
        : 'No se encontró una fila libre; recarga la tabla y vuelve a intentarlo',
    );
  }

  addWrite(target, liveRow, before, changes) {
    if (!changes.length) return;
    const mod = moduleMap.get(target.sheet);
    // Each field goes to the column the live header gives it.
    const layout = this.layouts.get(target.sheet);
    const columns = Object.fromEntries(changes.map(c => [c.field, layout.columns.get(c.field)]));
    const cellOf = field => liveRow?.cells?.[columns[field]];
    const dateFormat = changes
      .filter(c => mod.fields.find(f => f.key === c.field)?.type === 'date' && typeof c.after === 'number')
      .filter(c => !hasDateFormat(cellOf(c.field)))
      .map(c => c.field);
    // Times of day are stored as day fractions; a new cell needs a time format to show "9:20".
    const timeFormat = changes
      .filter(c => /(^|_)time$/i.test(c.field) && typeof c.after === 'number' && c.after >= 0 && c.after < 1)
      .filter(c => !hasTimeFormat(cellOf(c.field)))
      .map(c => c.field);
    target.changes = changes;
    target.before = before;
    this.writes.push({
      sheet: target.sheet,
      row: target.row,
      changes: Object.fromEntries(changes.map(c => [c.field, c.after])),
      columns,
      dateFormat,
      timeFormat,
    });
  }

  /**
   * Rejects a new value that is not in a strict list of the sheet (Google Sheets
   * would refuse it when typed there). Undo and imports put back what was there.
   */
  checkLists() {
    if (this.source === 'undo' || this.source === 'import') return;
    const bySheet = new Map();
    for (const t of this.targets)
      for (const c of t.changes || []) {
        const options = bySheet.get(t.sheet) ?? bySheet.set(t.sheet, listOptions(this.store, t.sheet)).get(t.sheet);
        if (!options[c.field]?.strict) continue;
        const problem = listProblemMsg(options, c.field, c.after);
        if (problem) this.conflict(t, 'NOT_IN_LIST', problem, { field: c.field, value: c.after });
      }
  }

  /** A tube's other holder is the same butterfly's other row (Insectary_data and its Collection_data twin). */
  isTwin(target, holder) {
    const mine = { ...(target.record?.values ?? {}), ...(target.clean ?? {}) };
    const theirs = this.store.getRecord(holder.id)?.values;
    if (!theirs) return false;
    if (target.sheet === 'Insectary_data' && holder.sheet === 'Collection_data') return twinRows(mine, theirs);
    if (target.sheet === 'Collection_data' && holder.sheet === 'Insectary_data') return twinRows(theirs, mine);
    return false;
  }

  /** Rejects IDs that another row already uses, or that repeat within the batch. */
  checkUniqueIds() {
    // Undo puts back what was there before (repeats included): refusing it would leave the data half restored.
    if (this.source === 'undo') return;
    const proposed = [];
    for (const t of this.targets)
      for (const c of t.changes || []) {
        if (isUnique(t.sheet, c.field) && isIdValue(c.after) && typeof c.after !== 'object')
          proposed.push({ target: t, field: c.field, value: String(c.after).trim() });
      }
    if (!proposed.length) return;
    const owners = uniqueIdIndex(this.store);
    const inBatch = new Map();
    for (const p of proposed) {
      const scope = TUBE_FIELD.test(p.field) ? 'tube' : `${p.target.sheet}:${p.field}`;
      const key = `${scope}\u0000${p.value}`;
      const ownId = p.target.record?.id || p.target.recordId;
      const holders = (owners.get(key) || []).filter(h => h.id !== ownId && !(scope === 'tube' && this.isTwin(p.target, h)));
      // A value being moved away from its current holder in this same batch is free.
      const stillHeld = holders.filter(
        h =>
          !this.targets.some(
            t =>
              (t.record?.id || t.recordId) === h.id &&
              t.changes?.some(c => c.field === h.field && comparable(c.after) !== comparable(p.value)),
          ),
      );
      if (stillHeld.length)
        this.conflict(
          p.target,
          'DUPLICATE_ID',
          stillHeld[0].label
            ? msg('{value} ya está usado en {sheet} fila {row} ({label})', {
                value: p.value,
                sheet: stillHeld[0].sheet,
                row: stillHeld[0].row,
                label: stillHeld[0].label,
              })
            : msg('{value} ya está usado en {sheet} fila {row}', { value: p.value, sheet: stillHeld[0].sheet, row: stillHeld[0].row }),
          {
            field: p.field,
            value: p.value,
          },
        );
      if (inBatch.has(key) && inBatch.get(key) !== ownId)
        this.conflict(p.target, 'DUPLICATE_ID', msg('{value} está dos veces en este guardado', { value: p.value }), {
          field: p.field,
          value: p.value,
        });
      inBatch.set(key, ownId);
    }
  }

  /** Confirms Google holds exactly what was written, then updates the local copy. */
  verifyAndPersist(check) {
    const records = [];
    // The check is read with its own header: were columns moved meanwhile, the cells moved with them.
    const layouts = new Map();
    const layoutOf = sheet => {
      if (!layouts.has(sheet))
        layouts.set(sheet, headerLayout(sheet, check.get(rowKey(sheet, moduleMap.get(sheet).headerRow))));
      return layouts.get(sheet);
    };
    for (const target of this.targets.filter(t => t.changes?.length)) {
      const liveRow = check.get(rowKey(target.sheet, target.row));
      const layout = layoutOf(target.sheet);
      if (layout.blocked) return null;
      const previous = target.record || this.store.getRecordBySheetRow(target.sheet, target.row);
      const now = this.store.keepUnavailable(target.sheet, rowValues(target.sheet, liveRow, layout), previous, layout);
      const matches = target.changes.every(
        c => comparable(cellValue(now.values, now.formulas, c.field)) === comparable(c.after),
      );
      if (!matches) return null;
      const record = {
        id: target.record?.id || target.recordId,
        sheet: target.sheet,
        row: target.row,
        values: now.values,
        formulas: now.formulas,
        label: labelFor(target.sheet, now.values),
        version: (target.record?.version ?? target.version ?? 0) + 1,
        updatedAt: new Date().toISOString(),
        missing: false,
      };
      this.store.placeRecord(record);
      target.record = this.store.getRecord(record.id);
      records.push(target.record);
    }
    return records;
  }
}

/** The changes of a plan as inferPurpose reads them. */
function plannedChanges(plan) {
  return plan.targets.flatMap(t =>
    (t.changes || []).map(c => ({
      sheet: t.sheet,
      field: c.field,
      before: c.before,
      after: c.after,
      isNew: !t.record,
      rowPurpose: t.sheet === 'Collection_data' ? (t.record?.values?.Purpose ?? t.clean?.Purpose) : undefined,
    })),
  );
}

function beginAction(store, { requestId, user, source, reason, reverses, purpose }, plan) {
  const id = randomUUID();
  const now = new Date().toISOString();
  store.db.exec('BEGIN IMMEDIATE');
  try {
    store.db
      .prepare(
        'INSERT INTO actions(id,request_id,actor,source,created_at,status,reason,reverses,result_json,purpose) VALUES(?,?,?,?,?,?,?,?,?,?)',
      )
      .run(
        id,
        requestId,
        user.id || user.username,
        source,
        now,
        'pending',
        reason || null,
        reverses || null,
        plan.skipped?.length ? JSON.stringify({ skipped: plan.skipped }) : null,
        purpose || null,
      );
    const insert = store.db.prepare(
      'INSERT INTO changes(id,action_id,record_id,sheet,row_num,field,before_json,after_json) VALUES(?,?,?,?,?,?,?,?)',
    );
    for (const target of plan.targets)
      for (const c of target.changes || [])
        insert.run(
          randomUUID(),
          id,
          target.record?.id || target.recordId,
          target.sheet,
          target.row,
          c.field,
          JSON.stringify(c.before ?? null),
          JSON.stringify(c.after ?? null),
        );
    store.db.exec('COMMIT');
  } catch (e) {
    store.db.exec('ROLLBACK');
    throw e;
  }
  return id;
}

/** Map of "scope\0value" → rows holding that identifier in the local copy. */
/** The sheets that hold IDs checked for repeats: those in UNIQUE, and any with tube columns. */
const UNIQUE_SHEETS = () =>
  [...moduleMap.values()].filter(m => UNIQUE[m.id] || m.fields.some(f => TUBE_FIELD.test(f.key))).map(m => m.id);

export function uniqueIdIndex(store) {
  const index = new Map();
  for (const sheet of UNIQUE_SHEETS()) {
    const unique = field => isUnique(sheet, field);
    const rows = store.db
      .prepare('SELECT id,row_num,values_json FROM records WHERE sheet=? AND missing=0 AND observed=1')
      .all(sheet);
    for (const r of rows) {
      const values = JSON.parse(r.values_json);
      for (const [field, value] of Object.entries(values)) {
        if (!unique(field) || !isIdValue(value)) continue;
        const scope = TUBE_FIELD.test(field) ? 'tube' : `${sheet}:${field}`;
        const key = `${scope}\u0000${String(value).trim()}`;
        index.set(key, [
          ...(index.get(key) || []),
          { id: r.id, sheet, row: r.row_num, field, label: labelFor(sheet, values) },
        ]);
      }
    }
  }
  return index;
}

/** Re-checks unconfirmed writes shortly after one happens, outside the write queue. */
function scheduleRecovery(store) {
  if (store.recoveryTimer) return;
  store.recoveryTimer = setTimeout(() => {
    store.recoveryTimer = null;
    store.runExclusive(() => store.recoverPending()).catch(() => {});
  }, 3000);
  store.recoveryTimer.unref?.();
}
