// Every write to Google Sheets goes through applyBatch.
//
// A batch holds edits to existing rows and new rows, possibly across sheets.
// It is checked against the live Sheet, written with one atomic batchUpdate,
// read back to verify, and recorded as a single history action so it can be
// undone as a unit. Google is contacted three times regardless of batch size:
// one read, one write, one verification read.

import { randomUUID } from 'node:crypto';
import { TYPED_OVER_FORMULA, comparable, isSumField, labelFor, moduleMap, simpleSum, validateValues } from './schema.mjs';
import { isFormulaValue, sameCell } from './formula-write.mjs';
import { hasDateFormat, hasTimeFormat, protectionRefused, rowKey, rowValues } from './sheets.mjs';
import { describeProblems, headerLayout, sameLayout } from './columns.mjs';
import { columnLocked, duplicateIdRow, ensurePremadeRows, insectaryIdRow, lockedRanges, noteProtection, suffixedId } from './premade.mjs';
import { isPlaceholder, newRowFormulas } from './formula-patterns.mjs';
import { cleanPurpose, inferPurpose } from './history.mjs';
import { TUBE_FIELD, UNIQUE, isIdValue, isUnique, twinRows } from './verifications.mjs';
import { listOptions, listProblemMsg } from './verify.mjs';
import { msg, msgError, textFields } from './messages.mjs';
import { claimIndex, claimKind, claimValue } from './claims.mjs';
import { busyError } from './workbook-health.mjs';

/** Where a write came from. Chosen by the server, never by the client. */
export const SOURCES = new Set(['app', 'undo', 'ai_approved', 'import']);

export const MAX_BATCH = 500;

// Identifiers that must not repeat (server/verifications.mjs, as in the Google
// Sheet's conditional formats). Tube IDs are unique across the whole workbook.

// Formula cells that may be typed over (server/schema.mjs). A value the formula already
// gives is not written: the formula stays.
export { TYPED_OVER_FORMULA };

/** The same text as a formula gives, whatever the spacing or capitals ("mechanitis  m. intermedia " is not, "Mechanitis M. intermedia" is). */
const formulaText = value => String(value ?? '').trim().replace(/\s+/g, ' ').toLowerCase();
export const sameAsFormula = (a, b) => formulaText(a) === formulaText(b);

/**
 * Insectary_data's Insectary_ID formula may be typed over only to tell apart two
 * butterflies given the same ID: the row's own ID with a suffix (`W2B` → `W2B.1`).
 * The next row's formula reads only the first two characters, so the series goes on.
 */
export const renamesWithSuffix = (current, next) => {
  const parsed = suffixedId(next);
  return !!parsed && parsed.base === String(current ?? '').trim().toUpperCase() && String(next).trim().toUpperCase() === parsed.id;
};
const mayReplace = (sheet, field) => TYPED_OVER_FORMULA[sheet]?.has(field) || (sheet === 'Insectary_data' && field === 'Insectary_ID');

/** What the SPECIES formula of an insectary row will give: the species of its clutch in Insectary_stocks. */
export function predictedSpecies(store, sheet, field, values) {
  if (sheet !== 'Insectary_data' || field !== 'SPECIES' || values['CLUTCH NUMBER'] == null) return undefined;
  return store.db
    .prepare(
      `SELECT json_extract(values_json,'$.SPECIES') s FROM records WHERE sheet='Insectary_stocks' AND missing=0 AND trim(CAST(json_extract(values_json,'$."CLUTCH NUMBER"') AS TEXT))=?`,
    )
    .get(String(values['CLUTCH NUMBER']).trim())?.s;
}

const blank = value => value === null || value === undefined || /^\s*(|NA|N\/A)\s*$/i.test(String(value));
const cellValue = (values, formulas, field) => (formulas[field] ? { formula: formulas[field] } : values[field]);

/**
 * Whether a sheet cell (`current`: { values, formulas } of its row) still holds
 * what an edit expects there (`expected`: what was read when it was drafted), as
 * the save checks it: a formula typed over (`replacing`) by the value it gives, a
 * count kept as a sum by its text (=12+15) or the number it shows (27), any other
 * cell by its value or formula.
 */
export function cellHolds(sheet, field, expected, current, replacing = false) {
  const values = current?.values ?? {};
  const formulas = current?.formulas ?? {};
  if (replacing) return comparable(expected) === comparable(values[field] ?? null);
  const actual = cellValue(values, formulas, field);
  const sumCell = isSumField(sheet, field) && !!simpleSum(formulas[field]);
  let want = expected;
  if (sumCell && typeof want === 'string' && simpleSum(want)) want = { formula: simpleSum(want) };
  if (sumCell && typeof want === 'number' && comparable(want) === comparable(values[field] ?? null)) want = actual;
  return sameCell(actual ?? null, want ?? null);
}
/** `message`: a text, or a msg() when it has values in it (its descriptor goes to the app, server/messages.mjs). */
const fail = (code, message, status = 400, details) => msgError(message, { code, status, details });
const cellKind = cell => {
  const value = cell?.userEnteredValue;
  return !value ? '.' : 'formulaValue' in value ? 'F' : 'c';
};
/** How many rows above a new row are looked at for a formula typed over in the row it copies. */
const FORMULA_LOOKBACK = 3;

/** Why a suffixed Insectary ID cannot get its row (premade.mjs duplicateIdRow): { code, message }, or null. */
export function duplicateProblem(found) {
  if (!found?.problem) return null;
  const { id, base } = found;
  if (found.problem === 'used') return { code: 'DUPLICATE_ID', message: msg('Insectary_ID {id} ya está registrado', { id }) };
  if (found.problem === 'no_base')
    return {
      code: 'ID_NOT_FOUND',
      message: msg('{base} no está en Insectary_data: {id} es para una segunda mariposa con el ID {base}', { id, base }),
    };
  if (found.problem === 'empty_base')
    return { code: 'ID_FREE', message: msg('La fila de {base} está sin usar: la mariposa va en ella, sin sufijo', { base }) };
  return {
    code: 'IDENTITY_CONFLICT',
    message: msg('{value} está en más de una fila ({rows}): corrígelo antes de añadir {id}', {
      value: found.value,
      rows: found.rows.map(String),
      id,
    }),
  };
}

/**
 * `purpose`: the flow a save belongs to (history.mjs PURPOSES). A save from the
 * app may declare its tab in `body.purpose`; otherwise it is inferred from what it changes.
 *
 * A save never waits for a workbook that does not answer: while Google is busy or
 * slow (server/workbook-health.mjs), while earlier saves wait (the outbox), or
 * while the app restarts, it is kept in the app's database and written in order
 * when the workbook answers (server/outbox.mjs); the answer is then
 * { status: 'queued', outboxId }. A save waiting its turn behind another is kept
 * there too as soon as that one is held too long or Google stops answering. `outbox`: { kind, ref, claimOwners } of the
 * queued item (an applied proposal, the staged entries). `fromOutbox`: the
 * outbox writing it now (the claims of `claimOwners` are its own).
 */
export async function applyBatch(
  store,
  body,
  user,
  { source = 'app', reverses = null, purpose = null, outbox = null, fromOutbox = null } = {},
) {
  store.validateRole(user);
  store.requireRequestId(body.requestId);
  if (!SOURCES.has(source)) throw fail('INVALID_SOURCE', 'Origen de escritura desconocido');
  const edits = Array.isArray(body.edits) ? body.edits : [];
  const creates = Array.isArray(body.creates) ? body.creates : [];
  // Rows a save inserted, deleted when it is undone (Store.undo): only an undo deletes rows.
  const deletes = source === 'undo' && Array.isArray(body.deletes) ? body.deletes : [];
  if (edits.length + creates.length + deletes.length > MAX_BATCH)
    throw fail('BATCH_TOO_LARGE', msg('Guarda como máximo {n} filas a la vez', { n: MAX_BATCH }));

  const declared = source === 'app' ? purpose || cleanPurpose(body.purpose) : null;
  const queue = () => store.outbox.enqueue({ body, user, source, purpose: declared, reverses, ...(outbox ?? {}) });
  if (!fromOutbox && store.outbox) {
    // A retried request: what became of it in the outbox.
    const queued = store.outbox.byRequest(body.requestId);
    if (queued) return store.outbox.answer(queued, user);
    if (store.outbox.shouldQueue()) return queue();
  }
  // The app is stopping (a deploy): saves in progress finish, new ones wait for the new process.
  if (store.draining) throw fail('SHUTTING_DOWN', 'La app se está reiniciando; vuelve a guardar en un minuto', 503);
  // `started`: its turn came; `kept`: kept in the outbox while it waited for it (below), so its turn does nothing.
  let started = false;
  let kept = false;
  const turn = store.runExclusive(async () => {
    started = true;
    if (kept) return null;
    // Google stopped answering while this save waited for the one before it.
    if (!fromOutbox && store.outbox?.shouldQueue({ waiting: true })) return queue();
    store.writesInFlight = (store.writesInFlight ?? 0) + 1;
    store.writeStartedAt = Date.now();
    try {
      return await writeBatchNow();
    } catch (e) {
      // Nothing was written yet (the rows were being read): the save waits for the workbook instead.
      if (!fromOutbox && store.outbox && busyError(e) && !e.code) return queue();
      throw e;
    } finally {
      store.writesInFlight--;
    }
  });
  if (fromOutbox || !store.outbox) return turn;
  // While it waits for the save before it: kept in the outbox once that one is held too long
  // (or Google stops answering), instead of waiting for it.
  return new Promise((resolve, reject) => {
    const look = setInterval(() => {
      if (started || !store.outbox.shouldQueue()) return;
      clearInterval(look);
      kept = true;
      try {
        resolve(queue());
      } catch (e) {
        reject(e);
      }
    }, store.outbox.lookEveryMs);
    look.unref?.();
    turn.then(
      out => {
        clearInterval(look);
        if (!kept) resolve(out);
      },
      e => {
        clearInterval(look);
        if (!kept) reject(e);
      },
    );
  });
  async function writeBatchNow() {
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
    if (!edits.length && !creates.length && !deletes.length) throw fail('INVALID_VALUES', 'No hay nada que guardar');
    // With `partial`, a change that cannot be saved (a repeated CAM, a cell changed by
    // someone else…) is left out and reported in `skipped`, and everything else is saved.
    // Without it (undo, the assistant) the batch stays all or nothing.
    const partial = body.partial === true && source === 'app';
    const skipped = [];
    let input = { edits, creates, deletes };
    let plan = planFor(store, source, input, { claimOwners: fromOutbox });
    for (let round = 0; partial && plan.conflicts.length && round < 5; round++) {
      const rest = withoutConflicts(input, plan.conflicts);
      if (!rest) break;
      skipped.push(...plan.conflicts);
      input = rest;
      plan = planFor(store, source, input, { claimOwners: fromOutbox });
    }
    throwIfConflicts(plan, skipped);
    // New rows past the sheet's pre-made rows would be bare (no formulas, no dropdowns):
    // make more pre-made rows first, as the team would by dragging the last one down.
    await ensurePremadeRows(store, plan.newRowNeeds());

    const live = await store.sheets.readRows(plan.readTargets(), { failFast: true });
    await plan.resolve(live);
    for (let round = 0; partial && plan.conflicts.length && round < 5; round++) {
      const rest = withoutConflicts(input, plan.conflicts);
      if (!rest) break;
      skipped.push(...plan.conflicts);
      input = rest;
      plan = planFor(store, source, input, { claimOwners: fromOutbox });
      if (plan.conflicts.length) continue;
      // The smaller batch touches the same rows or fewer; read any row not read yet.
      const missing = plan
        .readTargets()
        .map(({ sheet, rows }) => ({ sheet, rows: rows.filter(row => !live.has(rowKey(sheet, row))) }))
        .filter(t => t.rows.length);
      if (missing.length) for (const [key, row] of await store.sheets.readRows(missing, { failFast: true })) live.set(key, row);
      await plan.resolve(live);
    }
    throwIfConflicts(plan, skipped);
    plan.skipped = skipped;
    // Rows inserted or deleted move the rows below them: everything is numbered as after the write.
    plan.finalizeRows();
    if (!plan.writes.length)
      return { status: 'unchanged', action: null, actions: [], records: [], created: [], skipped };

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
        // Rows may have been inserted or deleted: the next sync compares the whole sheet.
        if (!rejected) for (const sheet of plan.structuralSheets()) store.sheetDigests.delete(sheet);
        // The app's account may not insert rows where protected columns reach them: PAS does.
        const inserted = plan.writes.filter(w => w.insert);
        if (rejected && inserted.length && protectionRefused(e))
          throw fail(
            'ROWS_PROTECTED',
            msg('Google Sheets no deja a la cuenta de la app insertar la fila de {id}: pide a PAS que inserte la fila; no se guardó nada', {
              id: inserted.map(w => w.changes.Insectary_ID ?? `${w.sheet} ${w.insert.at}`),
            }),
            409,
            { actionId },
          );
        // A cell the app's account may not edit (a column only the owner edits): which, in plain words.
        const locked = rejected && protectionRefused(e) ? await protectedCells(store, plan.writes) : [];
        if (locked.length)
          throw fail(
            'CELLS_PROTECTED',
            msg('Google rechazó la escritura: celda protegida en {fields}; no se guardó nada', {
              fields: [...new Set(locked.map(c => c.field))],
            }),
            409,
            { actionId, protected: locked.slice(0, 20), cause: e.message?.slice(0, 300) },
          );
        throw fail(
          rejected ? 'WRITE_REJECTED' : 'WRITE_UNCERTAIN',
          rejected
            ? 'Google Sheets rechazó el cambio; no se guardó nada'
            : 'No se pudo confirmar la escritura en Google Sheets',
          rejected ? 502 : 503,
          { actionId, cause: e.message?.slice(0, 300) },
        );
      }
      // The local copy follows the rows the write inserted or deleted.
      plan.moveStoredRows();
      let check;
      try {
        check = await store.sheets.readRows(plan.writeTargets(), { failFast: true });
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
  }
}

/** The cells of `writes` in ranges the app's account cannot edit, as Google has them now: [{ sheet, row, field }]. */
async function protectedCells(store, writes) {
  const out = [];
  const bySheet = new Map();
  for (const write of writes) {
    if (!write.columns) continue;
    if (!bySheet.has(write.sheet))
      try {
        const ranges = await store.sheets.protectedRangesOf?.(write.sheet);
        if (ranges) noteProtection(store.db, write.sheet, ranges);
        bySheet.set(write.sheet, lockedRanges(ranges));
      } catch {
        bySheet.set(write.sheet, []);
      }
    const locked = bySheet.get(write.sheet);
    for (const field of Object.keys(write.changes || {}))
      if (write.columns[field] !== undefined && columnLocked(locked, write.columns[field], write.row))
        out.push({ sheet: write.sheet, row: write.row, field });
  }
  return out;
}

/**
 * `claimOwners`: the claims (server/claims.mjs) this save may use, its own;
 * `staging`: checked against the app's copy for Emergidos and Clutches entries
 * kept in the app (server/staged.mjs), not written now.
 */
function planFor(store, source, { edits, creates, deletes = [] }, { claimOwners = null, staging = false } = {}) {
  const plan = new Plan(store, source, { claimOwners, staging });
  plan.addEdits(edits);
  plan.addCreates(creates);
  plan.addDeletes(deletes);
  return plan;
}

/**
 * The checks of a save (the same as applyBatch's) against rows given instead
 * of Google's: `live` maps rowKey(sheet, row) to a row as Google returns it
 * (cells by column), for every row asked. What can't be saved is left out and
 * returned in `skipped`, a new row alone (not every new row, as a save does).
 * Used when Emergidos and Clutches entries are kept in the app (server/staged.mjs).
 */
export async function checkAgainst(store, input, { live, claimOwners = null, source = 'app' }) {
  const skipped = [];
  let current = { edits: input.edits ?? [], creates: input.creates ?? [] };
  for (let round = 0; round < 8; round++) {
    const plan = planFor(store, source, current, { claimOwners, staging: true });
    if (!plan.conflicts.length) await plan.resolve(live);
    if (!plan.conflicts.length) return { plan, input: current, skipped };
    const rest = withoutConflicts(current, plan.conflicts, { createsAlone: true });
    if (!rest) return { plan: null, input: { edits: [], creates: [] }, skipped: [...skipped, ...plan.conflicts] };
    skipped.push(...plan.conflicts);
    current = rest;
    if (!current.edits.length && !current.creates.length) return { plan: null, input: current, skipped };
  }
  return { plan: null, input: { edits: [], creates: [] }, skipped };
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
export function withoutConflicts({ edits, creates }, conflicts, { createsAlone = false } = {}) {
  if (conflicts.some(c => !c.id && !c.clientId)) return null;
  // Checking entries kept in the app (checkAgainst): only the new rows refused are left out.
  if (createsAlone) {
    const refused = new Set(conflicts.filter(c => c.clientId).map(c => c.clientId));
    const rest = withoutConflicts({ edits, creates: [] }, conflicts.filter(c => !c.clientId));
    return rest && { edits: rest.edits, creates: creates.filter((c, i) => !refused.has(c?.clientId || `new-${i}`)) };
  }
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
  constructor(store, source, { claimOwners = null, staging = false } = {}) {
    this.store = store;
    this.source = source;
    this.claimOwners = new Set(claimOwners ?? []);
    this.staging = staging;
    this.conflicts = [];
    /** One entry per affected row: { sheet, row, record, clean, expected, clientId?, candidates? } */
    this.targets = [];
    this.writes = [];
    /** Live column map of each sheet, from the header row read with the batch. */
    this.layouts = new Map();
    this.pools = [];
  }

  /**
   * Formula cells a save may type over: SPECIES (what emerged differs from its clutch), a suffixed
   * Insectary_ID, and in a reviewed proposal any formula cell it marks so (a value its formula would
   * not give, shown to the person as doubtful: server/assistant.mjs weighFormulaCells).
   */
  mayReplace(sheet, field) {
    return mayReplace(sheet, field) || (this.source === 'ai_approved' && !(sheet === 'Insectary_data' && field === 'Insectary_ID'));
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
        // The assistant's proposals write formulas given as {"formula"} (a plain "=..." stays text).
        allowFormula: this.source === 'undo' || this.source === 'ai_approved',
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
      target.replaceFormula = new Set(
        Array.isArray(edit?.replaceFormula) && record ? edit.replaceFormula.filter(f => this.mayReplace(record.sheet, f)) : [],
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
      target.replaceFormula = new Set(
        Array.isArray(create?.replaceFormula)
          ? create.replaceFormula.filter(f => TYPED_OVER_FORMULA[create?.module]?.has(f) || (this.source === 'ai_approved' && f !== 'Insectary_ID'))
          : [],
      );
      if (!moduleMap.has(create?.module)) return this.conflict(target, 'MODULE_NOT_FOUND', 'Hoja desconocida');
      // If an earlier save to this sheet may or may not have landed, a new row could
      // duplicate it. Wait until that save is confirmed (this happens automatically).
      const unsettled = this.store.db
        .prepare(
          "SELECT 1 FROM actions a WHERE a.status IN ('pending','uncertain') AND EXISTS(SELECT 1 FROM changes c WHERE c.action_id=a.id AND c.sheet=?) LIMIT 1",
        )
        .get(create.module);
      if (unsettled && !this.staging)
        return this.conflict(
          target,
          'WRITE_UNCERTAIN',
          msg('Un guardado anterior en {sheet} aún se está confirmando; vuelve a intentarlo en un minuto', {
            sheet: create.module,
          }),
        );
      target.clean = this.validate(target, create.module, create.values);
      if (!target.clean) return;
      // An ID, CAM, tube or clutch number someone holds in a change not in the sheet yet (server/claims.mjs).
      if (this.claimed(target, Object.entries(target.clean))) return;
      if (!Object.values(target.clean).some(v => !blank(v)))
        return this.conflict(target, 'INVALID_VALUES', 'Una fila nueva necesita al menos un valor');
      // The same ID on a second butterfly (W2B.2): a row inserted below its group, not a pre-made row.
      const duplicate = create.module === 'Insectary_data' ? duplicateIdRow(this.store, target.clean.Insectary_ID) : null;
      if (duplicate) {
        const problem = duplicateProblem(duplicate);
        if (problem) return this.conflict(target, problem.code, problem.message, { field: 'Insectary_ID' });
        if (this.targets.some(t => t.insert?.anchorId === duplicate.anchor.id))
          return this.conflict(
            target,
            'ROW_COLLISION',
            msg('Las filas nuevas de {base} se guardan de una en una', { base: duplicate.base }),
            { field: 'Insectary_ID' },
          );
        target.clean.Insectary_ID = duplicate.id;
        target.insert = {
          base: duplicate.base,
          anchorId: duplicate.anchor.id,
          anchorRow: duplicate.anchor.row,
          anchorValue: duplicate.anchor.value,
        };
        this.targets.push(target);
        return;
      }
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

  /**
   * Rows a save inserted, deleted again by its undo (Store.undo): { id, expected }
   * where `expected` holds what that save wrote in the row. The row is deleted only
   * while it holds nothing else.
   */
  addDeletes(deletes) {
    deletes.forEach((del, index) => {
      const target = { index, editId: del?.id, deleteRow: true, expected: del?.expected || {} };
      const record = typeof del?.id === 'string' ? this.store.getRecord(del.id) : null;
      if (!record || record.missing || record.row <= 0)
        return this.conflict(target, 'RECORD_NOT_FOUND', 'La fila ya no está disponible; recarga la tabla');
      if (this.targets.some(t => t.record?.id === record.id))
        return this.conflict(target, 'DUPLICATE_EDIT', 'La misma fila aparece dos veces en un guardado');
      Object.assign(target, { sheet: record.sheet, row: record.row, record });
      this.targets.push(target);
    });
  }

  /** For each sheet getting new rows at the end: the last row they may need. */
  newRowNeeds() {
    return this.pools;
  }

  readTargets() {
    const bySheet = new Map();
    for (const t of this.targets) {
      const rows = bySheet.get(t.sheet) || new Set([moduleMap.get(t.sheet).headerRow]);
      const header = moduleMap.get(t.sheet).headerRow;
      // An inserted row: the row it copies, a few rows above (formulas typed over in it) and the row below.
      const around = t.insert
        ? Array.from({ length: FORMULA_LOOKBACK + 2 }, (_, i) => t.insert.anchorRow - FORMULA_LOOKBACK + i).filter(r => r > header)
        : null;
      for (const row of around || t.candidates || [t.row]) rows.add(row);
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
    for (const target of this.targets.filter(t => t.record && !t.deleteRow && !brokenSheets.has(t.sheet))) {
      if (unavailable(target)) continue;
      await this.resolveEdit(target, live);
      used.add(`${target.sheet}:${target.row}`);
    }
    for (const target of this.targets.filter(t => t.deleteRow && !brokenSheets.has(t.sheet))) this.resolveDelete(target, live);
    // New rows leave the columns the app's account cannot edit to the sheet (their owner fills them).
    const creating = this.targets.filter(t => !t.record && !t.insert && !brokenSheets.has(t.sheet)).map(t => t.sheet);
    if (!this.staging && this.source !== 'undo') await this.loadProtection(new Set(creating));
    for (const target of this.targets.filter(t => !t.record && !brokenSheets.has(t.sheet)))
      if (!unavailable(target)) {
        if (target.insert) this.resolveInsert(target, live);
        else this.resolveCreate(target, live, used);
      }
    const written = new Set();
    for (const write of this.writes) {
      // A row inserted at a row number goes in before the row there, which may be written too.
      const key = write.insert ? `${write.sheet}:+${write.insert.at}` : `${write.sheet}:${write.row}`;
      if (written.has(key))
        this.conflict(
          null,
          'ROW_COLLISION',
          msg('Dos cambios de este guardado van a {sheet} fila {row}', { sheet: write.sheet, row: write.row }),
        );
      written.add(key);
    }
    this.checkUniqueIds();
    this.checkClaims();
    this.checkLists();
  }

  /**
   * Refuses an identifier held by a change not in the sheet yet (an Emergidos or
   * Clutches entry kept in the app, a save waiting for Google: server/claims.mjs)
   * unless this save is that change. Undo puts back what was there.
   */
  checkClaims() {
    if (this.source === 'undo') return;
    for (const t of this.targets) {
      // New rows were looked at when added (addCreates); typing into a pre-made row takes its ID too.
      if (!t.record) continue;
      const cells = (t.changes || []).map(c => [c.field, c.after]);
      if (!t.record.observed && t.sheet === 'Insectary_data' && t.changes?.length) cells.push(['Insectary_ID', t.record.values?.Insectary_ID]);
      this.claimed(t, cells);
    }
  }
  /** Conflicts for `cells` ([field, value]) of target `t` held by another change not in the sheet yet; whether any was. */
  claimed(t, cells) {
    if (this.source === 'undo') return false;
    let found = false;
    for (const [field, value] of cells) {
      const kind = claimKind(t.sheet, field);
      if (!kind || !isIdValue(value) || typeof value === 'object') continue;
      this.claimIndex ??= claimIndex(this.store.db);
      const holder = this.claimIndex.get(`${kind}\u0000${claimValue(value)}`);
      if (!holder || this.claimOwners.has(holder.owner)) continue;
      found = true;
      this.conflict(
        t,
        'CLAIMED',
        msg('{value} ya lo tiene {name} en cambios aún no guardados en Google Sheets', { value: claimValue(value), name: holder.name }),
        { field, value, holder: { name: holder.name, owner: holder.owner } },
      );
    }
    return found;
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
      // What the formula gives once this edit is in (its clutch may change with it): left to the formula.
      if (before.formulas[field] && TYPED_OVER_FORMULA[record.sheet]?.has(field) && this.source !== 'undo') {
        const clutch = target.clean['CLUTCH NUMBER'];
        const moved = clutch !== undefined && comparable(clutch) !== comparable(before.values['CLUTCH NUMBER'] ?? null);
        const predicted = moved
          ? predictedSpecies(this.store, record.sheet, field, { ...before.values, ...target.clean })
          : (before.values[field] ?? null);
        if (predicted !== undefined && sameAsFormula(predicted, after)) continue;
      }
      const replacing = !!before.formulas[field] && target.replaceFormula.has(field);
      // A count kept as a sum (=12+15) may be rewritten; any other formula stays the sheet's.
      const sumCell = isSumField(record.sheet, field) && !!simpleSum(before.formulas[field]);
      // A formula written over a formula (a proposal's {"formula"}) is checked and written as any value.
      const formulaWrite = this.source === 'ai_approved' && isFormulaValue(after) && !isSumField(record.sheet, field);
      if (before.formulas[field] && this.source !== 'undo' && !replacing && !sumCell && !formulaWrite)
        return this.conflict(target, 'FORMULA_CELL', msg('{field} se calcula con una fórmula de la hoja', { field }), { field });
      if (replacing) {
        const predicted = before.values[field] ?? null;
        // Two butterflies with one ID: only the row's own ID with a suffix (W2B → W2B.1).
        if (field === 'Insectary_ID' && !renamesWithSuffix(predicted, after))
          return this.conflict(
            target,
            'FORMULA_CELL',
            msg('El Insectary ID {id} se calcula con una fórmula: solo se cambia añadiéndole un sufijo ({id}.1)', { id: predicted ?? '' }),
            { field },
          );
        if (target.expected && Object.hasOwn(target.expected, field) && !cellHolds(record.sheet, field, target.expected[field], before, true))
          return this.conflict(target, 'EXTERNAL_CONFLICT', msg('Otra persona cambió {field} en la hoja', { field }), {
            field,
            expected: target.expected[field],
            actual: predicted,
          });
        changes.push({ field, before: { formula: before.formulas[field] }, after });
        continue;
      }
      const actual = cellValue(before.values, before.formulas, field);
      // The sum a person saw may be sent as its text ("=12+15") or as the number it shows (27): cellHolds.
      const expected =
        target.expected && Object.hasOwn(target.expected, field)
          ? target.expected[field]
          : cellValue(record.values, record.formulas, field);
      // Typing the text a cell already holds (e.g. "944" stored as text) is not a change.
      if (typeof actual === 'string' && target.raw[field] === actual) continue;
      if (!cellHolds(record.sheet, field, expected, before))
        this.conflict(target, 'EXTERNAL_CONFLICT', msg('Otra persona cambió {field} en la hoja', { field }), {
          field,
          expected: expected ?? null,
          actual: actual ?? null,
        });
      else if (!sameCell(actual, after)) changes.push({ field, before: actual ?? null, after });
    }
    this.addWrite(target, liveRow, before, changes);
  }

  /** The ranges of `sheets` the app's account cannot edit (Google's metadata, kept 30 s): this.locked. */
  async loadProtection(sheets) {
    this.locked = new Map();
    for (const sheet of sheets)
      try {
        const ranges = await this.store.sheets.protectedRangesOf?.(sheet);
        if (!ranges) continue;
        noteProtection(this.store.db, sheet, ranges);
        this.locked.set(sheet, lockedRanges(ranges));
      } catch {
        // Not known now: Google refuses a protected cell, and the save says which.
      }
  }

  async findMovedRow(record, identity) {
    // Checked against the app's copy (staging): the row is where the copy has it.
    if (this.staging) return null;
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
        // The species the clutch gives: the new row's formula will show it once the clutch is in.
        if (
          before.formulas[field] &&
          TYPED_OVER_FORMULA[target.sheet]?.has(field) &&
          sameAsFormula(predictedSpecies(this.store, target.sheet, field, target.clean) ?? before.values[field], after)
        )
          continue;
        if (before.formulas[field]) {
          // A pre-made row's count kept as a sum takes the notebook's sum.
          if (isSumField(target.sheet, field) && simpleSum(before.formulas[field])) {
            if (comparable({ formula: before.formulas[field] }) !== comparable(after))
              changes.push({ field, before: { formula: before.formulas[field] }, after });
            continue;
          }
          // e.g. a butterfly of another subspecies than its clutch predicts.
          if (target.replaceFormula.has(field)) {
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
      // The formulas rows of this kind keep (server/formula-patterns.mjs: the Collected_Sent2Insectary
      // lookups, CAM_ID_insectary, Data_entry_order…), moved to this row, where the pre-made row has
      // none and nothing but a placeholder (NA) was typed: new rows do not widen the gap.
      if (this.source !== 'undo') {
        const layout = this.layouts.get(target.sheet);
        const fills = newRowFormulas(this.store, target.sheet, row, { ...before.values, ...target.clean });
        // Columns the app's account cannot edit (Data_entry_order…) are left to the sheet, a placeholder typed there too.
        const locked = this.locked?.get(target.sheet) ?? [];
        const isLocked = field => locked.length > 0 && columnLocked(locked, layout.columns.get(field), row);
        for (const [field, formula] of Object.entries(fills)) {
          const typed = target.clean[field];
          if (before.formulas[field] || !layout.columns.has(field) || !blank(before.values[field])) continue;
          if (typed !== undefined && typed !== null && typed !== '' && !isPlaceholder(typed)) continue;
          const at = changes.findIndex(c => c.field === field);
          if (at >= 0) changes.splice(at, 1);
          if (!isLocked(field)) changes.push({ field, before: before.values[field] ?? null, after: { formula } });
        }
        for (let i = changes.length - 1; i >= 0; i--)
          if (isPlaceholder(changes[i].after) && isLocked(changes[i].field)) changes.splice(i, 1);
        if (!changes.length) return this.conflict(target, 'INVALID_VALUES', 'La fila nueva no tiene nada que guardar');
      }
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

  /**
   * A new butterfly whose ID is suffixed (W2B.2): a row inserted directly below the
   * last row of its ID's group, as the curators insert it. It copies that row
   * (formulas, formats, dropdowns) without its values; the row below keeps its ID
   * formula, which still reads the row above the inserted one.
   */
  resolveInsert(target, live) {
    const { sheet, insert } = target;
    const layout = this.layouts.get(sheet);
    const header = moduleMap.get(sheet).headerRow;
    const cellsAt = row => live.get(rowKey(sheet, row))?.cells || [];
    const idAt = row => String(rowValues(sheet, live.get(rowKey(sheet, row)), layout).values.Insectary_ID ?? '').trim().toUpperCase();
    const anchor = insert.anchorRow;
    if (idAt(anchor) !== insert.anchorValue)
      return this.conflict(
        target,
        'ROW_MOVED',
        msg('La fila {row} ya no es {id} en Google Sheets; recarga y vuelve a intentarlo', { row: anchor, id: insert.anchorValue }),
      );
    const below = idAt(anchor + 1);
    if (below === insert.base || suffixedId(below)?.base === insert.base)
      return this.conflict(
        target,
        'ROW_MOVED',
        msg('Debajo de {id} (fila {row}) hay otra fila de {base} en Google Sheets; recarga y vuelve a intentarlo', {
          id: insert.anchorValue,
          row: anchor,
          base: insert.base,
        }),
      );
    const idColumn = layout.columns.get('Insectary_ID');
    const width = Math.max(cellsAt(anchor).length, cellsAt(header).length, ...[...layout.columns.values()].map(c => c + 1));
    // Every column that is not a formula in the row copied is emptied; a formula typed over
    // there (a species other than its clutch's) comes back from the nearest row above that has it.
    const clear = [];
    const formulas = [];
    for (let column = 0; column < width; column++) {
      const kind = cellKind(cellsAt(anchor)[column]);
      if (kind === 'F') continue;
      clear.push(column);
      if (kind !== 'c' || column === idColumn) continue;
      for (let row = anchor - 1; row >= Math.max(header + 1, anchor - FORMULA_LOOKBACK); row--)
        if (cellKind(cellsAt(row)[column]) === 'F') {
          formulas.push({ column, from: row });
          break;
        }
    }
    const isFormula = field => {
      const column = layout.columns.get(field);
      return field !== 'Insectary_ID' && (cellKind(cellsAt(anchor)[column]) === 'F' || formulas.some(f => f.column === column));
    };
    const changes = [];
    for (const [field, after] of Object.entries(target.clean)) {
      if (after === null || after === '') continue;
      if (isFormula(field)) {
        // The species the clutch gives: the inserted row's formula shows it once the clutch is in.
        const predicted = TYPED_OVER_FORMULA[sheet]?.has(field) ? predictedSpecies(this.store, sheet, field, target.clean) : undefined;
        if (predicted !== undefined && sameAsFormula(predicted, after)) continue;
        if (!target.replaceFormula.has(field) && !isSumField(sheet, field))
          return this.conflict(target, 'FORMULA_CELL', msg('{field} se calcula con una fórmula en la fila nueva', { field }), {
            field,
          });
      }
      changes.push({ field, before: null, after });
    }
    target.row = anchor + 1;
    target.recordId = randomUUID();
    target.version = 0;
    // Formats (dates, times) come with the copy of the row above.
    const write = this.addWrite(target, live.get(rowKey(sheet, anchor)), { values: {}, formulas: {} }, changes);
    write.insert = { at: anchor + 1, source: anchor, width, clear, formulas };
  }

  /** A row a save inserted, deleted by its undo while it holds only what that save wrote. */
  resolveDelete(target, live) {
    const { record } = target;
    const layout = this.layouts.get(record.sheet);
    const liveRow = live.get(rowKey(record.sheet, record.row));
    const current = rowValues(record.sheet, liveRow, layout);
    const identity = this.store.identity(record.sheet, record.values);
    if (!Object.keys(identity).length || comparable(this.store.identity(record.sheet, current.values)) !== comparable(identity))
      return this.conflict(target, 'ROW_MOVED', 'La fila se movió o su identificador cambió en Google Sheets; recarga la tabla');
    for (const [field, value] of Object.entries(current.values)) {
      if (current.formulas[field] || value === null || value === '') continue;
      if (!Object.hasOwn(target.expected, field) || comparable(target.expected[field]) !== comparable(value))
        return this.conflict(
          target,
          'ROW_CHANGED',
          msg('La fila {label} tiene datos que no puso ese guardado ({field}); no se borra', { label: record.label, field }),
          { field },
        );
    }
    target.changes = Object.keys(target.expected)
      .filter(field => layout.columns.has(field))
      .map(field => ({ field, before: cellValue(current.values, current.formulas, field) ?? null, after: null }));
    target.before = current;
    // Kept in the history at the row it had.
    target.historyRow = record.row;
    target.write = { sheet: record.sheet, row: record.row, deleteRow: { at: record.row }, changes: {}, columns: {} };
    this.writes.push(target.write);
  }

  /** Sheets where this batch inserts or deletes rows. */
  structuralSheets() {
    return [...new Set(this.writes.filter(w => w.insert || w.deleteRow).map(w => w.sheet))];
  }

  /**
   * Rows inserted or deleted move the rows below them. The writes and targets are
   * numbered as the sheet will be after the write (the rows inserted and deleted
   * keep, in `insert.at`/`deleteRow.at`, where they go in the sheet as it is now).
   */
  finalizeRows() {
    const ops = this.writes.filter(w => w.insert || w.deleteRow);
    if (!ops.length) return;
    const final = (sheet, row, self = null) => {
      let out = row;
      for (const op of ops) {
        if (op === self || op.sheet !== sheet) continue;
        if (op.insert && (self?.insert ? op.insert.at < row : op.insert.at <= row)) out++;
        if (op.deleteRow && op.deleteRow.at < row) out--;
      }
      return out;
    };
    for (const target of this.targets) {
      if (!target.write || target.write.row !== target.row) continue;
      const own = target.write.insert || target.write.deleteRow ? target.write : null;
      target.row = target.write.row = final(target.sheet, target.row, own);
    }
  }

  /** The local copy after the write: rows below an inserted row move down, below a deleted one up. */
  moveStoredRows() {
    const ops = this.writes
      .filter(w => w.insert || w.deleteRow)
      .sort((a, b) => (b.insert ?? b.deleteRow).at - (a.insert ?? a.deleteRow).at);
    if (!ops.length) return;
    const db = this.store.db;
    db.exec('BEGIN IMMEDIATE');
    try {
      for (const op of ops) {
        if (op.insert) this.store.shiftRows(op.sheet, op.insert.at, 1);
        else {
          const target = this.targets.find(t => t.write === op);
          db.prepare('UPDATE records SET missing=1,row_num=?,updated_at=? WHERE id=?').run(
            this.store.displacedRow(op.sheet),
            new Date().toISOString(),
            target.record.id,
          );
          this.store.shiftRows(op.sheet, op.deleteRow.at + 1, -1);
        }
      }
      db.exec('COMMIT');
    } catch (e) {
      db.exec('ROLLBACK');
      throw e;
    }
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
    target.write = {
      sheet: target.sheet,
      row: target.row,
      changes: Object.fromEntries(changes.map(c => [c.field, c.after])),
      columns,
      dateFormat,
      timeFormat,
    };
    this.writes.push(target.write);
    return target.write;
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
        // A formula's value is the sheet's to give.
        if (c.after && typeof c.after === 'object') continue;
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
      // A deleted row: the row now in its place is the one that was below it.
      if (target.deleteRow) {
        const identity = this.store.identity(target.sheet, target.record.values);
        const there = this.store.identity(target.sheet, rowValues(target.sheet, liveRow, layout).values);
        if (comparable(there) === comparable(identity)) return null;
        continue;
      }
      const previous = target.record || this.store.getRecordBySheetRow(target.sheet, target.row);
      const now = this.store.keepUnavailable(target.sheet, rowValues(target.sheet, liveRow, layout), previous, layout);
      // A formula as Google keeps it may differ in spacing or the case of names.
      const matches = target.changes.every(c => sameCell(cellValue(now.values, now.formulas, c.field), c.after));
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
          target.historyRow ?? target.row,
          c.field,
          JSON.stringify(c.before ?? null),
          JSON.stringify(c.after ?? null),
        );
    // Rows this save inserts: undoing it deletes them (Store.previewUndo).
    for (const target of plan.targets.filter(t => t.insert && t.changes?.length))
      store.db
        .prepare('INSERT INTO inserted_rows(action_id,record_id,sheet) VALUES(?,?,?)')
        .run(id, target.recordId, target.sheet);
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
