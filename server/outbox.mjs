// Saves waiting for Google. While the workbook recalculates (minutes after an
// edit to Insectary_data) Google answers slowly, with 503, or not at all
// (server/workbook-health.mjs). A save then never hangs: applyBatch keeps it
// here, in the app's database, with who made it, when and what, and the values
// each cell was expected to hold (the same conflict checks as any save), and
// answers { status: 'queued', outboxId }. When the workbook answers again the
// waiting saves are written in the order they came, consecutive saves of one
// person and tab together as one batch (fewer recalculations), each through
// applyBatch: a cell someone changed in the sheet meanwhile is refused and
// reported (never overwritten), as in any save. A restart loses nothing: the
// items live in SQLite and the next process writes them.
//
// Statuses: queued → writing → done | conflict (refused: the person looks at it,
// Historial/Revisión) | failed (Google refused the write). An item whose write
// Google did not confirm stays `writing` with its action (status uncertain)
// until the app's recovery settles that action; the items after it wait.

import { randomUUID } from 'node:crypto';
import { applyBatch, MAX_BATCH } from './batch.mjs';
import { claimsOf, releaseClaims, takeClaims } from './claims.mjs';
import { busyError } from './workbook-health.mjs';
import { msgError } from './messages.mjs';

const parse = text => (text ? JSON.parse(text) : null);
const json = value => JSON.stringify(value);
const now = () => new Date().toISOString();
/** A write still running after this long makes later saves wait in the outbox instead of behind it. */
export const HELD_WRITE_MS = 10_000;
/** How often a save waiting behind another looks whether that one is held too long. */
export const LOOK_EVERY_MS = 1_000;
/** How often the outbox looks again on its own (Google may answer without anyone saving). */
export const DRAIN_EVERY_MS = 30_000;
const SETTLED = new Set(['done', 'conflict', 'failed']);

export function initOutbox(db) {
  db.exec(`CREATE TABLE IF NOT EXISTS outbox(id TEXT PRIMARY KEY, request_id TEXT UNIQUE, actor TEXT NOT NULL, user_json TEXT NOT NULL,
      source TEXT NOT NULL, purpose TEXT, reverses TEXT, kind TEXT NOT NULL DEFAULT 'save', ref TEXT, claim_owners_json TEXT,
      body_json TEXT NOT NULL, status TEXT NOT NULL, created_at TEXT NOT NULL, updated_at TEXT NOT NULL, attempts INTEGER NOT NULL DEFAULT 0,
      batch_request_id TEXT, action_id TEXT, result_json TEXT, error_json TEXT);
    CREATE INDEX IF NOT EXISTS outbox_status ON outbox(status);`);
}

/** The person as the save will be recorded (what applyBatch needs of them). */
const userOf = user => ({ id: user.id, username: user.username, displayName: user.displayName ?? null, role: user.role });
/** The changes of a save body: edits' record ids and new rows' client ids. */
const touchedIds = body => new Set([...(body.edits ?? []).map(e => e?.id), ...(body.deletes ?? []).map(d => d?.id)].filter(Boolean));
const size = body => (body.edits?.length ?? 0) + (body.creates?.length ?? 0) + (body.deletes?.length ?? 0);

export class Outbox {
  constructor(store) {
    this.store = store;
    this.db = store.db;
    this.watchers = new Set();
    this.running = null;
    this.again = false;
    this.timer = null;
    // Tests set them shorter (config.heldWriteMs).
    this.heldMs = store.config?.heldWriteMs ?? HELD_WRITE_MS;
    this.lookEveryMs = Math.max(10, Math.min(LOOK_EVERY_MS, Math.floor(this.heldMs / 4)));
  }
  get health() {
    return this.store.sheets?.health ?? null;
  }
  /** Saves not written yet (queued, or being written). */
  waiting() {
    return this.db.prepare("SELECT count(*) n FROM outbox WHERE status IN ('queued','writing')").get().n;
  }
  /**
   * Whether a save must wait here: the workbook is not answering normally, saves
   * wait already (order), the app is restarting, or a write has been held for long
   * (`waiting`: asked again by a save that waited behind it).
   */
  shouldQueue({ waiting = false } = {}) {
    const store = this.store;
    if (store.draining) return true;
    if (this.health && this.health.state !== 'ok') return true;
    if (this.waiting()) return true;
    if (!waiting && store.writesInFlight && Date.now() - (store.writeStartedAt ?? 0) > this.heldMs) return true;
    return false;
  }
  byRequest(requestId) {
    if (!requestId) return null;
    return this.db.prepare('SELECT * FROM outbox WHERE request_id = ?').get(requestId) ?? null;
  }
  get(id) {
    return this.db.prepare('SELECT * FROM outbox WHERE id = ?').get(id) ?? null;
  }
  /**
   * Keeps a save until the workbook answers. Its identifiers (new rows' IDs, CAMs,
   * tubes) are claimed where free, so nobody is offered them meanwhile; the write
   * checks them again. `claimOwners`: claims the save holds already (the staged entries it writes);
   * `also(id)`: run in the same transaction (the entries marked as being written).
   */
  enqueue({ body, user, source = 'app', purpose = null, reverses = null, kind = 'save', ref = null, claimOwners = [], also = null }) {
    const id = randomUUID();
    const at = now();
    const owners = [`outbox:${id}`, ...claimOwners];
    this.db.exec('BEGIN IMMEDIATE');
    try {
      this.db
        .prepare(
          `INSERT INTO outbox(id, request_id, actor, user_json, source, purpose, reverses, kind, ref, claim_owners_json, body_json, status, created_at, updated_at)
           VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'queued', ?, ?)`,
        )
        .run(id, body.requestId, user.id || user.username, json(userOf(user)), source, purpose, reverses, kind, ref, json(owners), json(body), at, at);
      for (const change of [...(body.creates ?? []).map(c => [c.module, c.values]), ...(body.edits ?? []).map(e => [this.store.getRecord(e.id)?.sheet, e.values])])
        if (change[0]) takeClaims(this.db, `outbox:${id}`, user.id || user.username, claimsOf(change[0], change[1]), { now: at });
      also?.(id);
      this.db.exec('COMMIT');
    } catch (e) {
      this.db.exec('ROLLBACK');
      throw e;
    }
    this.changed();
    this.kick();
    return this.answer(this.get(id), user);
  }
  /** What a request (or a retry of it) gets back about its item. */
  answer(item, user) {
    if (user && item.actor !== (user.id || user.username))
      throw msgError('Este ID de solicitud pertenece a otra persona', { code: 'REQUEST_ID_CONFLICT', status: 409 });
    const view = this.view(item);
    if (item.status === 'done') {
      const result = parse(item.result_json) ?? {};
      const action = result.actionId ? { id: result.actionId } : null;
      return { ...view, ...result, action, actions: action ? [action] : [], status: result.status ?? 'verified' };
    }
    if (item.status === 'conflict' || item.status === 'failed') {
      const error = parse(item.error_json) ?? {};
      throw Object.assign(new Error(error.message || 'No se guardó'), {
        code: error.code || 'BATCH_CONFLICT',
        status: error.status || 409,
        ...(error.messageMsg ? { messageMsg: error.messageMsg } : {}),
        details: { ...(error.details ?? {}), outboxId: item.id },
      });
    }
    return { ...view, status: 'queued', action: null, actions: [], records: [], created: [], skipped: [] };
  }
  /** An item as the app shows it: its state, its place in the queue, the workbook's state. */
  view(item) {
    const result = parse(item.result_json);
    const position =
      item.status === 'queued' || item.status === 'writing'
        ? this.db.prepare("SELECT count(*) n FROM outbox WHERE status IN ('queued','writing') AND rowid <= (SELECT rowid FROM outbox WHERE id = ?)").get(item.id).n
        : 0;
    return {
      outboxId: item.id,
      outbox: {
        id: item.id,
        status: item.status,
        kind: item.kind,
        ref: item.ref,
        purpose: item.purpose,
        actor: item.actor,
        createdAt: item.created_at,
        updatedAt: item.updated_at,
        attempts: item.attempts,
        position,
        waiting: this.waiting(),
        ...(item.action_id ? { actionId: item.action_id } : {}),
        ...(item.error_json ? { error: parse(item.error_json) } : {}),
      },
      ...(result ? { result } : {}),
      workbook: this.health?.snapshot() ?? { state: 'ok' },
    };
  }
  /** The items not written yet, and the latest settled ones (the banner, the History). */
  list({ settled = 20 } = {}) {
    const people = new Map(this.db.prepare('SELECT id, display_name FROM users').all().map(u => [u.id, u.display_name]));
    const shape = r => {
      const body = parse(r.body_json) ?? {};
      return {
        id: r.id,
        status: r.status,
        kind: r.kind,
        ref: r.ref,
        purpose: r.purpose,
        source: r.source,
        actor: r.actor,
        actorName: people.get(r.actor) ?? r.actor,
        createdAt: r.created_at,
        updatedAt: r.updated_at,
        attempts: r.attempts,
        edits: body.edits?.length ?? 0,
        creates: body.creates?.length ?? 0,
        records: (body.edits ?? []).map(e => e.id).slice(0, 200),
        ...(r.action_id ? { actionId: r.action_id } : {}),
        ...(r.error_json ? { error: parse(r.error_json) } : {}),
      };
    };
    return {
      waiting: this.waiting(),
      items: this.db.prepare("SELECT * FROM outbox WHERE status IN ('queued','writing') ORDER BY rowid").all().map(shape),
      settled: this.db
        .prepare("SELECT * FROM outbox WHERE status IN ('done','conflict','failed') ORDER BY updated_at DESC LIMIT ?")
        .all(Math.min(Math.max(Number(settled) || 0, 0), 100))
        .map(shape),
    };
  }
  /** Calls `fn(item)` when an item is settled (done, conflict, failed). Returns the call that stops it. */
  watch(fn) {
    this.watchers.add(fn);
    return () => this.watchers.delete(fn);
  }
  changed() {
    this.store.bumpLive?.('outbox');
  }
  /** Writes what waits soon (without waiting for it). */
  kick() {
    setImmediate(() => {
      if (!this.stopped) void this.drain().catch(e => console.error('Outbox:', e.message));
    });
  }
  /** Looks again every DRAIN_EVERY_MS while items wait (Google answers again without anyone saving). */
  start(every = DRAIN_EVERY_MS) {
    if (this.timer || !every) return;
    this.timer = setInterval(() => {
      if (this.waiting()) this.kick();
    }, every);
    this.timer.unref?.();
  }
  stop() {
    this.stopped = true;
    if (this.timer) clearInterval(this.timer);
    this.timer = null;
  }
  /** Resolves once item `id` is settled, or after `ms` (with the item as it is then). */
  async wait(id, ms = 20_000) {
    const until = Date.now() + ms;
    for (;;) {
      const item = this.get(id);
      if (!item || SETTLED.has(item.status) || Date.now() >= until) return item;
      await new Promise(resolve => setTimeout(resolve, 100));
    }
  }

  /**
   * Writes the waiting saves in order while the workbook answers (not busy) and
   * the app is not stopping. One run at a time; a call during a run makes it look again.
   */
  drain() {
    if (this.running) {
      this.again = true;
      return this.running;
    }
    this.running = (async () => {
      try {
        do {
          this.again = false;
          if (!(await this.settleWriting())) return;
          for (;;) {
            if (this.stopped || this.store.draining || this.health?.state === 'busy') return;
            // A save whose outcome is unknown: new rows could repeat it; the recovery settles it first.
            if (this.store.unconfirmedCount()) {
              await this.store.runExclusive(() => this.store.recoverPending()).catch(() => {});
              if (this.store.unconfirmedCount()) return;
              if (!(await this.settleWriting())) return;
            }
            const group = this.nextGroup();
            if (!group.length) break;
            if (!(await this.write(group))) return;
          }
        } while (this.again);
      } finally {
        this.running = null;
      }
    })();
    return this.running;
  }
  /**
   * Items left `writing` (a write Google did not confirm, a restart): settled from
   * their save's outcome. False while one is still unknown (the items after it wait).
   */
  async settleWriting() {
    const writing = this.db.prepare("SELECT * FROM outbox WHERE status = 'writing' ORDER BY rowid").all();
    const batches = Map.groupBy(writing, r => r.batch_request_id ?? r.id);
    for (const items of batches.values()) {
      const batchId = items[0].batch_request_id;
      const action = batchId ? this.db.prepare('SELECT * FROM actions WHERE request_id = ?').get(batchId) : null;
      if (!action || action.status === 'failed') {
        // Nothing reached the sheet: written again.
        this.setStatus(items, 'queued');
        continue;
      }
      if (action.status === 'verified') {
        const result = parse(action.result_json) ?? {};
        this.distribute(items, { ...result, action: { id: action.id }, status: 'verified' });
        continue;
      }
      return false;
    }
    return true;
  }
  /**
   * The next saves to write together: the first waiting one, then the following
   * ones of the same person, tab and kind of save, while they touch other rows and
   * at most one of them adds rows (new rows are saved all or none).
   */
  nextGroup() {
    const rows = this.db.prepare("SELECT * FROM outbox WHERE status IN ('queued','writing') ORDER BY rowid LIMIT 200").all();
    if (!rows.length || rows[0].status === 'writing') return [];
    const first = rows[0];
    const group = [first];
    const body = parse(first.body_json);
    // Proposals and the staged entries are written alone: their own outcome goes back to them.
    if (first.kind !== 'save' || first.source !== 'app' || body.partial !== true) return group;
    const touched = touchedIds(body);
    let count = size(body);
    let creating = (body.creates?.length ?? 0) > 0;
    for (const r of rows.slice(1)) {
      const b = parse(r.body_json);
      const same = r.status === 'queued' && r.kind === 'save' && r.source === 'app' && r.actor === first.actor && r.purpose === first.purpose && b.partial === true;
      if (!same) break;
      const ids = touchedIds(b);
      const creates = (b.creates?.length ?? 0) > 0;
      if ([...ids].some(id => touched.has(id)) || (creating && creates) || count + size(b) > MAX_BATCH) break;
      group.push(r);
      for (const id of ids) touched.add(id);
      count += size(b);
      creating ||= creates;
    }
    return group;
  }
  setStatus(items, status, extra = {}) {
    const at = now();
    const update = this.db.prepare(
      'UPDATE outbox SET status = ?, updated_at = ?, result_json = coalesce(?, result_json), error_json = ?, action_id = coalesce(?, action_id) WHERE id = ?',
    );
    for (const item of items)
      update.run(
        status,
        at,
        extra.result ? json(extra.result) : null,
        extra.error ? json(extra.error) : null,
        extra.actionId ?? null,
        item.id,
      );
    this.changed();
  }
  /** Writes one group as one save. False when the workbook stopped answering (the rest waits). */
  async write(group) {
    const first = group[0];
    const attempt = first.attempts + 1;
    const batchRequestId = `outbox:${first.id}:${attempt}`;
    const at = now();
    for (const item of group)
      this.db
        .prepare("UPDATE outbox SET status = 'writing', attempts = attempts + 1, batch_request_id = ?, updated_at = ? WHERE id = ?")
        .run(batchRequestId, at, item.id);
    this.changed();
    const bodies = group.map(item => parse(item.body_json));
    // New rows' client ids are told apart per item (two saves may both have "new-0").
    const tag = (item, clientId) => (group.length > 1 ? `${item.id}:${clientId}` : clientId);
    const merged = {
      requestId: batchRequestId,
      reason: bodies.find(b => b.reason)?.reason ?? null,
      partial: bodies[0].partial === true,
      edits: bodies.flatMap(b => b.edits ?? []),
      creates: group.flatMap((item, i) => (bodies[i].creates ?? []).map((c, k) => ({ ...c, clientId: tag(item, c.clientId || `new-${k}`) }))),
      ...(bodies.some(b => b.deletes?.length) ? { deletes: bodies.flatMap(b => b.deletes ?? []) } : {}),
    };
    const user = parse(first.user_json);
    const owners = [...new Set(group.flatMap(item => parse(item.claim_owners_json) ?? [`outbox:${item.id}`]))];
    const options = { source: first.source, purpose: first.purpose, reverses: first.reverses, fromOutbox: owners };
    let result;
    try {
      result = await applyBatch(this.store, merged, user, options);
      // New rows left out only because another one of the batch was refused: written now, on their own.
      const created = new Set((result.created ?? []).map(c => c.clientId));
      const refused = new Set((result.skipped ?? []).map(s => s.clientId).filter(Boolean));
      const dropped = merged.creates.filter(c => !created.has(c.clientId) && !refused.has(c.clientId));
      if (merged.partial && dropped.length && refused.size) {
        try {
          const more = await applyBatch(this.store, { requestId: `${batchRequestId}:rows`, reason: merged.reason, partial: true, edits: [], creates: dropped }, user, options);
          result = {
            ...result,
            records: [...(result.records ?? []), ...(more.records ?? [])],
            created: [...(result.created ?? []), ...(more.created ?? [])],
            skipped: [...(result.skipped ?? []), ...(more.skipped ?? [])],
            actions: [...(result.actions ?? []), ...(more.actions ?? [])],
          };
        } catch (e) {
          result = { ...result, skipped: [...(result.skipped ?? []), ...(e.details?.items ?? [])] };
        }
      }
    } catch (e) {
      if (busyError(e) && !e.code) {
        // The rows could not be read: nothing was written. They wait for the workbook.
        this.setStatus(group, 'queued');
        return false;
      }
      if (e.code === 'WRITE_UNCERTAIN') {
        // Maybe written: the recovery reads the cells again, then these items are settled (settleWriting).
        if (e.details?.actionId) for (const item of group) this.db.prepare('UPDATE outbox SET action_id = ? WHERE id = ?').run(e.details.actionId, item.id);
        this.changed();
        return false;
      }
      if (e.code === 'SHUTTING_DOWN') {
        this.setStatus(group, 'queued');
        return false;
      }
      const error = { code: e.code || 'SERVER_ERROR', message: e.message, ...(e.messageMsg ? { messageMsg: e.messageMsg } : {}), status: Number(e.status) || 500 };
      if (e.code === 'BATCH_CONFLICT') {
        const items = e.details?.items ?? [];
        for (const [i, item] of group.entries()) {
          const own =
            group.length === 1
              ? items
              : ownItems(items, bodies[i], clientId => tag(item, clientId), clientId => untag(item, clientId, group.length));
          this.settle(item, 'conflict', { error: { ...error, details: { items: own.length ? own : items.filter(x => !x.id && !x.clientId) } } });
        }
        return true;
      }
      for (const item of group) this.settle(item, 'failed', { error: { ...error, ...(e.details ? { details: e.details } : {}) } });
      return true;
    }
    this.distribute(group, result, tag);
    return true;
  }
  /** Gives each item of a written group its part of the outcome. */
  distribute(group, result, tag = null) {
    const bodies = group.map(item => parse(item.body_json));
    const tagOf = tag ?? ((item, clientId) => (group.length > 1 ? `${item.id}:${clientId}` : clientId));
    const actionId = result.action?.id ?? result.actions?.[0]?.id ?? null;
    for (const [i, item] of group.entries()) {
      const body = bodies[i];
      const ids = touchedIds(body);
      const mine = new Map((body.creates ?? []).map((c, k) => [tagOf(item, c.clientId || `new-${k}`), c.clientId || `new-${k}`]));
      const created = (result.created ?? []).filter(c => mine.has(c.clientId)).map(c => ({ ...c, clientId: mine.get(c.clientId) }));
      const recordIds = new Set([...ids, ...created.map(c => c.recordId)]);
      const records = (result.records ?? []).filter(r => recordIds.has(r.id));
      const skipped = ownItems(result.skipped ?? [], body, clientId => tagOf(item, clientId), clientId => mine.get(clientId) ?? clientId);
      const wrote = records.length > 0 || created.length > 0;
      const own = { status: result.status ?? 'verified', records, created, skipped, ...(actionId && wrote ? { actionId } : {}) };
      this.settle(item, !wrote && skipped.length ? 'conflict' : 'done', {
        result: own,
        actionId: wrote ? actionId : null,
        ...(!wrote && skipped.length
          ? { error: { code: 'BATCH_CONFLICT', message: 'Algunos cambios necesitan revisión; no se guardó nada', status: 409, details: { items: skipped } } }
          : {}),
      });
    }
  }
  /** An item settled: its claims released, the watchers told (a proposal, the staged entries). */
  settle(item, status, { result = null, error = null, actionId = null } = {}) {
    this.setStatus([item], status, { result, error, actionId });
    releaseClaims(this.db, `outbox:${item.id}`);
    const settled = this.get(item.id);
    for (const fn of this.watchers) {
      try {
        fn(settled);
      } catch (e) {
        console.error('Outbox watcher:', e.message);
      }
    }
  }
}

/** The refused changes of a batch that belong to one item's body (`tagged`: its client ids as sent). */
function ownItems(items, body, tagged, untagged) {
  const ids = touchedIds(body);
  const clients = new Set((body.creates ?? []).map((c, k) => tagged(c.clientId || `new-${k}`)));
  return items
    .filter(x => (x.id && ids.has(x.id)) || (x.clientId && clients.has(x.clientId)))
    .map(x => (x.clientId ? { ...x, clientId: untagged(x.clientId) } : x));
}
const untag = (item, clientId, size) => (size > 1 && clientId.startsWith(`${item.id}:`) ? clientId.slice(item.id.length + 1) : clientId);
