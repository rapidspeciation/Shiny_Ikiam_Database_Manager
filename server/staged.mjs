// Emergidos and Clutches entries kept in the app until someone saves them to
// Google Sheets. Every save to Insectary_data makes the team's workbook
// recalculate for minutes (5 Oct 2026), and four people work in the insectary at
// once: what they enter in those two tabs is checked as a save is (IDs, CAMs,
// tubes, lists, formula cells, a cell changed meanwhile) against the app's copy
// with everyone's entries on it, and stored here, seen by everyone at once,
// marked as not yet in the sheet, with who entered it. «Guardar en Google Sheets»
// writes all of them, both tabs, as one save through the outbox
// (server/outbox.mjs), so a busy workbook only makes them wait.
//
// Each entry (one press of a tab's Save) holds items: a new row (`create`, its
// row shown as `staged:<clientId>`) or an existing row's cells (`edit`). An edit
// keeps, per cell, what the person saw (`expected`: another person's entry on the
// same cell comes first, so a second one must have seen it) and what the sheet
// held before the first entry on it (`base`: what the save checks the sheet
// still holds). New rows' identifiers (Insectary ID, CAM, tube, clutch number)
// are claimed in the same transaction that stores them (server/claims.mjs): two
// people never get the same one. Undoing an entry releases them; once written
// they are the sheet's.

import { randomUUID } from 'node:crypto';
import { checkAgainst } from './batch.mjs';
import { claimsOf, releaseClaims, takeClaims } from './claims.mjs';
import { idSuggestions } from './grid.mjs';
import { asCell, comparable, labelFor, moduleMap } from './schema.mjs';
import { msg, msgError, tpl } from './messages.mjs';
import { rowKey } from './sheets.mjs';

/** The tabs whose saves are kept in the app until «Guardar en Google Sheets». */
export const STAGED_PURPOSES = new Set(['emergidos', 'clutches']);
const parse = text => (text ? JSON.parse(text) : null);
const json = value => JSON.stringify(value);
const now = () => new Date().toISOString();
const fail = (code, message, status = 400, details) => msgError(message, { code, status, details });
const STAGED_ID = /^staged:(.+)$/;
/** What a sum (=12+15-2) shows. */
export const sumTotal = formula => (String(formula).match(/[+-]?\s*\d+(?:\.\d+)?/g) ?? []).reduce((n, t) => n + Number(t.replace(/\s+/g, '')), 0);

export function initStaged(db) {
  db.exec(`CREATE TABLE IF NOT EXISTS staged(id TEXT PRIMARY KEY, entry_id TEXT NOT NULL, request_id TEXT, purpose TEXT NOT NULL,
      kind TEXT NOT NULL, sheet TEXT NOT NULL, record_id TEXT, client_id TEXT, label TEXT, values_json TEXT NOT NULL,
      expected_json TEXT, base_json TEXT, replace_formula_json TEXT, actor TEXT NOT NULL, edited_by TEXT, created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL, status TEXT NOT NULL DEFAULT 'staged', outbox_id TEXT, error_json TEXT);
    CREATE INDEX IF NOT EXISTS staged_record ON staged(record_id);
    CREATE INDEX IF NOT EXISTS staged_entry ON staged(entry_id);
    CREATE INDEX IF NOT EXISTS staged_request ON staged(request_id);`);
}

/** A stored value as a cell holds it: a sum ({ formula }) or a value. */
const cellOf = (values, formulas, field) => (formulas?.[field] ? { formula: formulas[field] } : (values?.[field] ?? null));
/** Whether a value an entry holds is what a person saw: a sum may be seen as its text (=12+15) or its total (27). */
function sameCell(stored, seen) {
  if (comparable(stored ?? null) === comparable(seen ?? null)) return true;
  const formula = stored && typeof stored === 'object' ? stored.formula : null;
  if (!formula) return false;
  const text = seen && typeof seen === 'object' ? seen.formula : seen;
  return String(text ?? '').replace(/\s+/g, '') === formula || Number(text) === sumTotal(formula);
}

export class Staged {
  constructor(store) {
    this.store = store;
    this.db = store.db;
    this.queue = Promise.resolve();
    store.outbox.watch(item => {
      if (item.kind === 'staged') this.settled(item);
    });
  }
  /** One staging at a time (they claim identifiers); never behind a write to Google. */
  serial(fn) {
    const result = this.queue.then(fn);
    this.queue = result.catch(() => {});
    return result;
  }
  rows(where = "status IN ('staged','sent')", ...args) {
    return this.db.prepare(`SELECT * FROM staged WHERE ${where} ORDER BY rowid`).all(...args);
  }
  shape(r, people) {
    return {
      id: r.id,
      entryId: r.entry_id,
      purpose: r.purpose,
      kind: r.kind,
      sheet: r.sheet,
      recordId: r.record_id,
      clientId: r.client_id,
      rowId: r.kind === 'create' ? `staged:${r.client_id}` : r.record_id,
      label: r.label,
      values: parse(r.values_json) ?? {},
      ...(r.kind === 'edit' ? { expected: parse(r.expected_json) ?? {}, base: parse(r.base_json) ?? {} } : {}),
      actor: r.actor,
      actorName: people.get(r.actor) ?? r.actor,
      ...(r.edited_by && r.edited_by !== r.actor ? { editedBy: r.edited_by, editedByName: people.get(r.edited_by) ?? r.edited_by } : {}),
      createdAt: r.created_at,
      updatedAt: r.updated_at,
      status: r.status,
      ...(r.outbox_id ? { outboxId: r.outbox_id } : {}),
      ...(r.error_json ? { error: parse(r.error_json) } : {}),
    };
  }
  people() {
    return new Map(this.db.prepare('SELECT id, display_name FROM users').all().map(u => [u.id, u.display_name]));
  }
  /** Everyone's entries not in the sheet yet, and what they claim. */
  list() {
    const people = this.people();
    const items = this.rows().map(r => this.shape(r, people));
    const claims = this.db
      .prepare("SELECT kind, value, owner, actor FROM claims WHERE owner LIKE 'staged:%' ORDER BY created_at")
      .all()
      .map(c => ({ kind: c.kind, value: c.value, itemId: c.owner.slice(7), actor: c.actor, actorName: people.get(c.actor) ?? c.actor }));
    return { items, claims, count: this.count(), revision: this.store.liveRevision?.() ?? null };
  }
  /** Changes waiting for «Guardar en Google Sheets» (cells of edits, plus new rows), and those being written. */
  count() {
    let staged = 0,
      sent = 0;
    for (const r of this.rows()) {
      const n = r.kind === 'create' ? 1 : Object.keys(parse(r.values_json) ?? {}).length;
      if (r.status === 'sent') sent += n;
      else staged += n;
    }
    return { staged, sent, rows: this.rows("status = 'staged'").length };
  }

  /**
   * The sheet rows as they will be with everyone's entries (the checks run against
   * them): rowKey → a row as Google returns it. `exclude`: items being changed now.
   */
  live(exclude = new Set()) {
    const store = this.store;
    const edits = new Map();
    const premade = new Map();
    for (const r of this.rows()) {
      if (exclude.has(r.id)) continue;
      const values = parse(r.values_json) ?? {};
      if (r.kind === 'edit') edits.set(r.record_id, { ...(edits.get(r.record_id) ?? {}), ...values });
      else if (r.sheet === 'Insectary_data' && values.Insectary_ID) {
        // A new butterfly goes into the pre-made row of its ID: that row is taken.
        const row = store.db
          .prepare(
            "SELECT row_num FROM records WHERE sheet='Insectary_data' AND missing=0 AND observed=0 AND upper(trim(json_extract(values_json,'$.Insectary_ID')))=?",
          )
          .get(String(values.Insectary_ID).trim().toUpperCase());
        if (row) premade.set(rowKey('Insectary_data', row.row_num), values);
      }
    }
    return {
      get: key => {
        const [sheet, text] = key.split('\u0000');
        const row = Number(text);
        const mod = moduleMap.get(sheet);
        const columns = store.layouts.get(sheet)?.columns ?? new Map(mod.fields.map(f => [f.key, f.column]));
        const cells = [];
        if (row === mod.headerRow) {
          for (const [field, column] of columns) cells[column] = { userEnteredValue: { stringValue: field }, effectiveValue: { stringValue: field } };
          return { row, cells };
        }
        const record = store.getRecordBySheetRow(sheet, row);
        const values = { ...(record?.missing ? {} : (record?.values ?? {})) };
        const formulas = { ...(record?.missing ? {} : (record?.formulas ?? {})) };
        for (const [field, value] of Object.entries((record && edits.get(record.id)) ?? {})) {
          if (value && typeof value === 'object' && value.formula) {
            formulas[field] = value.formula;
            values[field] = sumTotal(value.formula);
          } else {
            delete formulas[field];
            values[field] = value;
          }
        }
        // A pre-made row taken by a new butterfly: its typed cells (its formulas stay).
        for (const [field, value] of Object.entries(premade.get(key) ?? {})) if (!formulas[field]) values[field] = value;
        for (const [field, column] of columns) {
          const formula = formulas[field];
          const value = values[field];
          if (formula) {
            const shown = value === null || value === undefined || value === '' ? {} : asCell(value);
            cells[column] = { userEnteredValue: { formulaValue: formula }, ...(shown.userEnteredValue ? { effectiveValue: shown.userEnteredValue } : {}) };
          } else {
            const cell = asCell(value);
            if (cell.userEnteredValue) cell.effectiveValue = cell.userEnteredValue;
            cells[column] = cell;
          }
        }
        return { row, cells };
      },
    };
  }

  /** A cell's latest entry not written yet: { value, item } or null. */
  latestOn(recordId, field, exclude = new Set()) {
    const rows = this.db.prepare("SELECT * FROM staged WHERE kind='edit' AND record_id=? AND status IN ('staged','sent') ORDER BY rowid DESC").all(recordId);
    for (const r of rows) {
      if (exclude.has(r.id)) continue;
      const values = parse(r.values_json) ?? {};
      if (Object.hasOwn(values, field)) return { value: values[field], item: r };
    }
    return null;
  }

  /**
   * Keeps a tab's save in the app (POST /api/staged): `edits` of sheet rows
   * (id: a record) or of rows entered here (id: staged:<clientId>), `creates`
   * of new rows, `purpose` emergidos or clutches. What fails a check is left
   * out and listed in `skipped` (as a partial save); the rest is stored.
   */
  async stage(body, user) {
    this.store.validateRole(user);
    this.store.requireRequestId(body.requestId);
    if (!STAGED_PURPOSES.has(body.purpose)) throw fail('INVALID_PURPOSE', 'Solo Emergidos y Clutches guardan en la app');
    const edits = Array.isArray(body.edits) ? body.edits : [];
    const creates = Array.isArray(body.creates) ? body.creates : [];
    if (!edits.length && !creates.length) throw fail('INVALID_VALUES', 'No hay nada que guardar');
    if (edits.length + creates.length > 500) throw fail('BATCH_TOO_LARGE', msg('Guarda como máximo {n} filas a la vez', { n: 500 }));
    return this.serial(() => this.stageNow(body, user, edits, creates));
  }
  async stageNow(body, user, edits, creates) {
    const actor = user.id || user.username;
    const prior = this.db.prepare('SELECT entry_id FROM staged WHERE request_id = ? LIMIT 1').get(body.requestId);
    if (prior) return { status: 'staged', entryId: prior.entry_id, duplicate: true, records: [], created: [], skipped: [], ...this.summary() };
    const skipped = [];
    // Rows entered here and not written yet, edited again: their new row as a whole.
    const again = [];
    for (const edit of edits.filter(e => STAGED_ID.test(String(e?.id ?? '')))) {
      const clientId = STAGED_ID.exec(edit.id)[1];
      const item = this.db.prepare("SELECT * FROM staged WHERE kind='create' AND client_id=? AND status IN ('staged','sent')").get(clientId);
      if (!item) {
        skipped.push({ id: edit.id, code: 'STAGED_GONE', message: 'Esa fila ya no está entre los cambios sin guardar (se guardó o se deshizo)' });
        continue;
      }
      if (item.status === 'sent') {
        skipped.push({ id: edit.id, code: 'STAGED_SENT', message: 'Esa fila se está escribiendo en Google Sheets; cámbiala cuando esté guardada' });
        continue;
      }
      const values = parse(item.values_json) ?? {};
      const changed = Object.entries(edit.expected ?? {}).find(([field, seen]) => !sameCell(values[field], seen));
      if (changed) {
        skipped.push({
          id: edit.id,
          field: changed[0],
          code: 'EXTERNAL_CONFLICT',
          ...textOf(msg('Otra persona cambió {field} en esa fila sin guardar', { field: changed[0] })),
        });
        continue;
      }
      again.push({ item, values: { ...values, ...(edit.values ?? {}) } });
    }
    const exclude = new Set(again.map(a => a.item.id));
    const sheetEdits = edits.filter(e => !STAGED_ID.test(String(e?.id ?? '')));
    const newRows = creates.map(c => ({ ...c, clientId: typeof c?.clientId === 'string' && c.clientId ? c.clientId : randomUUID() }));
    const input = {
      edits: sheetEdits,
      creates: [
        ...newRows,
        ...again.map(a => ({ module: a.item.sheet, clientId: a.item.client_id, values: a.values, replaceFormula: parse(a.item.replace_formula_json) ?? [] })),
      ],
    };
    const owners = [...exclude].map(id => `staged:${id}`);
    const checked = await checkAgainst(this.store, input, { live: this.live(exclude), claimOwners: owners });
    const againIds = new Set(again.map(a => a.item.client_id));
    // A refusal of a row entered here is said on that row (staged:<clientId>).
    for (const s of checked.skipped)
      skipped.push(s.clientId && againIds.has(s.clientId) ? { ...s, id: `staged:${s.clientId}`, clientId: null } : this.explain(s));
    // All or nothing (the Emergidos cards: the butterflies and their clutches' counts go together).
    const whole = body.partial === false;
    const refuse = () => {
      throw fail('BATCH_CONFLICT', 'Algunos cambios necesitan revisión; no se guardó nada', 409, { items: skipped });
    };
    if (whole && skipped.length) refuse();
    const plan = checked.plan;
    const entryId = randomUUID();
    const at = now();
    const created = [];
    const stored = [];
    if (plan) {
      const insert = this.db.prepare(
        `INSERT INTO staged(id, entry_id, request_id, purpose, kind, sheet, record_id, client_id, label, values_json, expected_json, base_json,
           replace_formula_json, actor, created_at, updated_at) VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
      );
      this.db.exec('BEGIN IMMEDIATE');
      try {
        for (const target of plan.targets) {
          if (target.record) {
            // An existing row's cells: what the person saw, and what the sheet holds under everyone's entries.
            const fields = (target.changes ?? []).map(c => c.field);
            if (!fields.length) continue;
            const id = randomUUID();
            const raw = sheetEdits.find(e => e?.id === target.record.id);
            const base = {};
            const expected = {};
            for (const field of fields) {
              const latest = this.latestOn(target.record.id, field);
              base[field] = latest ? latest.value : cellOf(target.record.values, target.record.formulas, field);
              expected[field] = raw?.expected && Object.hasOwn(raw.expected, field) ? raw.expected[field] : base[field];
            }
            insert.run(
              id, entryId, body.requestId, body.purpose, 'edit', target.sheet, target.record.id, null, target.record.label,
              json(Object.fromEntries(fields.map(f => [f, target.clean[f]]))), json(expected), json(base), null, actor, at, at,
            );
            stored.push(id);
            continue;
          }
          const redo = again.find(a => a.item.client_id === target.clientId);
          this.db.exec('SAVEPOINT row');
          const id = redo?.item.id ?? randomUUID();
          const owner = `staged:${id}`;
          if (redo) {
            releaseClaims(this.db, owner);
            this.db
              .prepare('UPDATE staged SET values_json = ?, label = ?, edited_by = ?, updated_at = ?, error_json = NULL WHERE id = ?')
              .run(json(target.clean), labelFor(target.sheet, target.clean), actor, at, id);
          } else {
            const raw = newRows.find(c => c.clientId === target.clientId);
            insert.run(
              id, entryId, body.requestId, body.purpose, 'create', target.sheet, null, target.clientId, labelFor(target.sheet, target.clean),
              json(target.clean), null, null, json(raw?.replaceFormula ?? []), actor, at, at,
            );
          }
          const refused = takeClaims(this.db, owner, actor, claimsOf(target.sheet, target.clean), { now: at });
          if (refused.length) {
            // Taken a moment ago by someone else's entry: the next free one is offered.
            this.db.exec('ROLLBACK TO row');
            this.db.exec('RELEASE row');
            skipped.push(this.claimedItem(redo ? { id: `staged:${target.clientId}` } : { clientId: target.clientId }, refused[0]));
            if (whole) break;
            continue;
          }
          this.db.exec('RELEASE row');
          if (!redo) created.push({ clientId: target.clientId, recordId: `staged:${target.clientId}` });
          stored.push(id);
        }
        if (whole && skipped.length) refuse();
        this.db.exec('COMMIT');
      } catch (e) {
        this.db.exec('ROLLBACK');
        throw e;
      }
    }
    if (stored.length) this.store.bumpLive?.('staged');
    return { status: stored.length ? 'staged' : 'unchanged', entryId: stored.length ? entryId : null, records: [], created, skipped, ...this.summary() };
  }
  summary() {
    return { staged: this.count() };
  }
  /** A refusal about an identifier someone else's entry holds: who, and the next free one. */
  explain(item) {
    if (item.code !== 'CLAIMED' || !item.holder) return item;
    const next = this.nextFree(claimKindOfField(item.field), item.value);
    return {
      ...item,
      ...(next ? { next } : {}),
      ...textOf(
        next
          ? msg('{value} ya lo tiene {name} en cambios aún no guardados en Google Sheets; el siguiente libre es {next}', { value: String(item.value).toUpperCase(), name: item.holder.name, next })
          : msg('{value} ya lo tiene {name} en cambios aún no guardados en Google Sheets', { value: String(item.value).toUpperCase(), name: item.holder.name }),
      ),
    };
  }
  claimedItem(where, refused) {
    return this.explain({ ...where, code: 'CLAIMED', field: refused.field, value: refused.value, holder: { name: refused.holder?.name ?? '?', owner: refused.holder?.owner } });
  }
  /** The next free identifier of a kind (sheet, entries and waiting saves counted), from `value` on. */
  nextFree(kind, value) {
    try {
      if (kind === 'insectary') return idSuggestions(this.store, { kind: 'insectary', count: 1 }).sequence?.[0] ?? null;
      if (kind === 'cam' || kind === 'tube') return idSuggestions(this.store, { kind, start: value, count: 1 }).sequence?.[0] ?? null;
      if (kind === 'clutch') return nextClutchNumber(this.store);
    } catch {
      return null;
    }
    return null;
  }

  /**
   * Undoes entries not written yet: one item, or a whole entry (`entryId`). An
   * item a later entry builds on (the same cell) waits for that one to go first.
   */
  remove({ itemId = null, entryId = null }, user) {
    this.store.validateRole(user);
    return this.serial(() => {
      const items = itemId
        ? this.rows('id = ?', itemId)
        : entryId
          ? this.db.prepare('SELECT * FROM staged WHERE entry_id = ? ORDER BY rowid').all(entryId)
          : [];
      if (!items.length) throw fail('STAGED_NOT_FOUND', 'Ese cambio ya no está entre los cambios sin guardar', 404);
      if (items.some(i => i.status === 'sent'))
        throw fail('STAGED_SENT', 'Se está escribiendo en Google Sheets; deshazlo desde Historial cuando esté guardado', 409);
      const ids = new Set(items.map(i => i.id));
      for (const item of items.filter(i => i.kind === 'edit')) {
        const fields = Object.keys(parse(item.values_json) ?? {});
        const later = this.db
          .prepare("SELECT * FROM staged WHERE kind='edit' AND record_id=? AND rowid > (SELECT rowid FROM staged WHERE id=?) AND status IN ('staged','sent')")
          .all(item.record_id, item.id)
          .find(r => !ids.has(r.id) && fields.some(f => Object.hasOwn(parse(r.values_json) ?? {}, f)));
        if (later) {
          const name = this.people().get(later.actor) ?? later.actor;
          throw fail('STAGED_LATER', msg('{name} cambió después la misma celda de {label}: deshaz primero ese cambio', { name, label: item.label ?? '' }), 409);
        }
      }
      this.db.exec('BEGIN IMMEDIATE');
      try {
        for (const item of items) {
          releaseClaims(this.db, `staged:${item.id}`);
          this.db.prepare('DELETE FROM staged WHERE id = ?').run(item.id);
        }
        this.db.exec('COMMIT');
      } catch (e) {
        this.db.exec('ROLLBACK');
        throw e;
      }
      this.store.bumpLive?.('staged');
      return { removed: items.map(i => i.id), ...this.summary() };
    });
  }

  /**
   * «Guardar en Google Sheets»: every entry of both tabs as one save, through the
   * outbox (written now, or when the workbook answers). Waits up to `waitMs` for
   * the outcome; then says it is waiting.
   */
  async flush(body, user, { waitMs = 20_000 } = {}) {
    this.store.validateRole(user);
    this.store.requireRequestId(body.requestId);
    const outbox = this.store.outbox;
    let item = outbox.byRequest(body.requestId);
    if (!item) {
      item = await this.serial(() => {
        const items = this.rows("status = 'staged'");
        if (!items.length) return null;
        const { save, purpose, reason } = this.merge(items);
        let id = null;
        outbox.enqueue({
          body: { requestId: body.requestId, reason, partial: true, ...save },
          user,
          source: 'app',
          purpose,
          kind: 'staged',
          ref: json(items.map(i => i.id)),
          claimOwners: items.map(i => `staged:${i.id}`),
          // In the transaction that stores the save: the entries are now being written.
          also: outboxId => {
            id = outboxId;
            const mark = this.db.prepare("UPDATE staged SET status = 'sent', outbox_id = ?, error_json = NULL WHERE id = ?");
            for (const i of items) mark.run(outboxId, i.id);
          },
        });
        this.store.bumpLive?.('staged');
        return outbox.get(id);
      });
      if (!item) return { status: 'empty', ...this.summary() };
    }
    // While Google is busy nothing is written: the answer says so at once (the entries show as being written).
    const settled = (await outbox.wait(item.id, this.store.sheets.health?.state === 'busy' ? 0 : waitMs)) ?? item;
    return { ...outbox.view(settled), status: settled.status, ...this.summary() };
  }
  /** The entries as one save: new rows, and each sheet row's cells (expected: what the sheet held before the first entry). */
  merge(items) {
    const creates = [];
    const edits = new Map();
    const purposes = new Map();
    const people = new Map();
    const names = this.people();
    for (const item of items) {
      purposes.set(item.purpose, (purposes.get(item.purpose) ?? 0) + 1);
      const who = names.get(item.actor) ?? item.actor;
      people.set(who, (people.get(who) ?? 0) + 1);
      const values = parse(item.values_json) ?? {};
      if (item.kind === 'create') {
        const replaceFormula = parse(item.replace_formula_json) ?? [];
        creates.push({ clientId: item.client_id, module: item.sheet, values, ...(replaceFormula.length ? { replaceFormula } : {}) });
        continue;
      }
      const edit = edits.get(item.record_id) ?? { id: item.record_id, values: {}, expected: {} };
      const base = parse(item.base_json) ?? {};
      for (const [field, value] of Object.entries(values)) {
        if (!Object.hasOwn(edit.expected, field)) edit.expected[field] = base[field] ?? null;
        edit.values[field] = value;
      }
      edits.set(item.record_id, edit);
    }
    const purpose = [...purposes].sort((a, b) => b[1] - a[1])[0][0];
    const reason = msg(tpl('Registrado en la app por {people}'), { people: [...people].map(([who, n]) => `${who} (${n})`).join(', ') }).text;
    return { save: { edits: [...edits.values()], creates }, purpose, reason };
  }

  /** A save of the entries settled (server/outbox.mjs): what was written leaves; the rest comes back, with why. */
  settled(outboxItem) {
    const ids = parse(outboxItem.ref) ?? [];
    const items = ids.map(id => this.db.prepare('SELECT * FROM staged WHERE id = ?').get(id)).filter(Boolean);
    const result = parse(outboxItem.result_json) ?? {};
    const error = parse(outboxItem.error_json);
    const refusals = outboxItem.status === 'done' ? (result.skipped ?? []) : (error?.details?.items ?? []);
    const general = refusals.filter(r => !r.id && !r.clientId);
    const created = new Set((result.created ?? []).map(c => c.clientId));
    const at = now();
    const back = (item, why, values = null) =>
      this.db
        .prepare("UPDATE staged SET status = 'staged', outbox_id = NULL, error_json = ?, updated_at = ?, values_json = coalesce(?, values_json) WHERE id = ?")
        .run(json(why), at, values ? json(values) : null, item.id);
    const done = item => {
      releaseClaims(this.db, `staged:${item.id}`);
      this.db.prepare('DELETE FROM staged WHERE id = ?').run(item.id);
    };
    const reason = r => ({ code: r.code, message: r.message, ...(r.messageMsg ? { messageMsg: r.messageMsg } : {}), ...(r.field ? { field: r.field } : {}) });
    const fallback =
      outboxItem.status === 'done'
        ? { code: 'NOT_SAVED', message: 'No se guardó: otra fila nueva de este guardado necesita revisión' }
        : reason(general[0] ?? error ?? { code: 'NOT_SAVED', message: 'No se guardó' });
    const entries = new Set();
    this.db.exec('BEGIN IMMEDIATE');
    try {
      for (const item of items) {
        if (outboxItem.status !== 'done') {
          const own = refusals.find(r => (item.kind === 'create' ? r.clientId === item.client_id : r.id === item.record_id));
          back(item, own ? reason(own) : fallback);
          continue;
        }
        if (item.kind === 'create') {
          if (created.has(item.client_id)) {
            done(item);
            entries.add(item.entry_id);
          } else {
            const own = refusals.find(r => r.clientId === item.client_id);
            back(item, own ? reason(own) : general[0] ? reason(general[0]) : fallback);
          }
          continue;
        }
        const values = parse(item.values_json) ?? {};
        const own = refusals.filter(r => r.id === item.record_id && (!r.field || Object.hasOwn(values, r.field)));
        if (!own.length) {
          done(item);
          entries.add(item.entry_id);
          continue;
        }
        // Only the refused cells stay to look at: the others are in the sheet now.
        const whole = own.some(r => !r.field);
        const left = whole ? values : Object.fromEntries(Object.entries(values).filter(([field]) => own.some(r => r.field === field)));
        back(item, reason(own[0]), left);
      }
      // The clutches marked as checked with these entries: their save is this one now.
      if (result.actionId && entries.size)
        for (const table of ['clutch_checks', 'clutch_events'])
          this.db
            .prepare(`UPDATE ${table} SET action_id = ? WHERE staged_entry IN (${[...entries].map(() => '?').join(',')}) AND action_id IS NULL`)
            .run(result.actionId, ...entries);
      this.db.exec('COMMIT');
    } catch (e) {
      this.db.exec('ROLLBACK');
      console.error('Staged entries after a save:', e.message);
    }
    this.store.bumpLive?.('staged');
  }

  /**
   * The entries of one sheet as the AI and the clutch lists see them:
   * { edits: Map recordId → [{ field, before, after, actor, at, item }], creates: [item] }.
   */
  ofSheet(sheet) {
    const edits = new Map();
    const creates = [];
    const people = this.people();
    for (const r of this.rows('sheet = ? AND status IN (\'staged\',\'sent\')', sheet)) {
      const item = this.shape(r, people);
      if (r.kind === 'create') {
        creates.push(item);
        continue;
      }
      const list = edits.get(r.record_id) ?? [];
      for (const [field, after] of Object.entries(item.values)) list.push({ field, before: item.base[field] ?? null, after, actor: item.actorName, at: item.updatedAt, item });
      edits.set(r.record_id, list);
    }
    return { edits, creates };
  }
}

/** Fields of a refusal's text, with the descriptor (server/messages.mjs). */
const textOf = m => ({ message: m.text, messageMsg: m.msg });
const claimKindOfField = field =>
  field === 'Insectary_ID' ? 'insectary' : field === 'CLUTCH NUMBER' ? 'clutch' : /^CAM_ID/.test(field ?? '') ? 'cam' : /^Tube_/.test(field ?? '') ? 'tube' : null;

/** The clutch number after the highest one in the sheet or held by an entry. */
export function nextClutchNumber(store) {
  let max = 0;
  for (const r of store.db
    .prepare("SELECT json_extract(values_json,'$.\"CLUTCH NUMBER\"') n FROM records WHERE sheet='Insectary_stocks' AND missing=0")
    .all()) {
    const n = parseInt(String(r.n ?? ''), 10);
    if (Number.isFinite(n)) max = Math.max(max, n);
  }
  for (const c of store.db.prepare("SELECT value FROM claims WHERE kind='clutch'").all()) {
    const n = parseInt(c.value, 10);
    if (Number.isFinite(n)) max = Math.max(max, n);
  }
  return String(max + 1);
}
