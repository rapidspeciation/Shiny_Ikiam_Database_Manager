// Identifiers held by changes not yet in Google Sheets: the Emergidos and
// Clutches entries kept in the app until someone saves them (server/staged.mjs)
// and the saves waiting for a busy workbook (server/outbox.mjs). Several people
// register at once (one reviews a clutch, others the emerged butterflies): an
// Insectary ID, a CAM, a tube or a clutch number one of them holds is never
// offered to, nor accepted from, another. A claim is taken in the same SQLite
// transaction that stores the change (the table's key refuses a second holder),
// released when the change is undone, and dropped once Google holds the rows.

import { moduleMap } from './schema.mjs';
import { TUBE_FIELD, isIdValue } from './verifications.mjs';

export function initClaims(db) {
  db.exec(`CREATE TABLE IF NOT EXISTS claims(kind TEXT NOT NULL, value TEXT NOT NULL, owner TEXT NOT NULL, actor TEXT NOT NULL,
      created_at TEXT NOT NULL, PRIMARY KEY(kind, value));
    CREATE INDEX IF NOT EXISTS claims_owner ON claims(owner);`);
}

/** The kind of identifier a sheet's field holds, as claims count them; null for other fields. */
export function claimKind(sheet, field) {
  if (sheet === 'Insectary_data' && field === 'Insectary_ID') return 'insectary';
  if (sheet === 'Insectary_stocks' && field === 'CLUTCH NUMBER') return 'clutch';
  if (TUBE_FIELD.test(field)) return 'tube';
  // CAM_ID, CAM_ID_CollData, CAM_ID_insectary: one series across the sheets (server/grid.mjs usedCamIds).
  if (/^CAM_ID/.test(field) && moduleMap.has(sheet)) return 'cam';
  return null;
}

/** An identifier as claims compare it (994 and "994", " a4e" and "A4E" are one). */
export const claimValue = value => String(value ?? '').trim().toUpperCase();

/** The identifiers a change writes: [{ kind, value, field }], each once. */
export function claimsOf(sheet, values = {}) {
  const out = [];
  for (const [field, value] of Object.entries(values ?? {})) {
    const kind = claimKind(sheet, field);
    if (!kind || value === null || typeof value === 'object' || !isIdValue(value)) continue;
    const text = claimValue(value);
    if (text && !out.some(c => c.kind === kind && c.value === text)) out.push({ kind, value: text, field });
  }
  return out;
}

/** Who holds each claimed identifier: Map "kind\0VALUE" → { kind, value, owner, actor, name, createdAt }. */
export function claimIndex(db) {
  const out = new Map();
  let rows = [];
  try {
    rows = db
      .prepare('SELECT c.*, u.display_name name, u.username FROM claims c LEFT JOIN users u ON u.id = c.actor')
      .all();
  } catch {
    return out;
  }
  for (const r of rows)
    out.set(`${r.kind}\u0000${r.value}`, {
      kind: r.kind,
      value: r.value,
      owner: r.owner,
      actor: r.actor,
      name: r.name ?? r.username ?? r.actor,
      createdAt: r.created_at,
    });
  return out;
}

/** The values of one kind held by changes not in the sheet yet: Map VALUE → holder. */
export function claimedValues(db, kind) {
  const out = new Map();
  for (const holder of claimIndex(db).values()) if (holder.kind === kind) out.set(holder.value, holder);
  return out;
}

/** The holder of an identifier, or null. */
export function claimHolder(db, kind, value) {
  let r;
  try {
    r = db
      .prepare('SELECT c.*, u.display_name name, u.username FROM claims c LEFT JOIN users u ON u.id = c.actor WHERE c.kind = ? AND c.value = ?')
      .get(kind, claimValue(value));
  } catch {
    return null; // a database without the app's tables (tests of the assistant alone)
  }
  return r ? { kind: r.kind, value: r.value, owner: r.owner, actor: r.actor, name: r.name ?? r.username ?? r.actor, createdAt: r.created_at } : null;
}

/**
 * Takes `claims` for `owner` (in the caller's transaction). Those another owner
 * holds are not taken and are returned, with their holder; `force` keeps
 * whatever is free and never fails (a save waiting for Google: the write checks again).
 */
export function takeClaims(db, owner, actor, claims, { now = new Date().toISOString() } = {}) {
  const refused = [];
  const insert = db.prepare('INSERT INTO claims(kind, value, owner, actor, created_at) VALUES(?, ?, ?, ?, ?) ON CONFLICT(kind, value) DO NOTHING');
  const holder = db.prepare('SELECT owner FROM claims WHERE kind = ? AND value = ?');
  for (const c of claims) {
    const r = insert.run(c.kind, c.value, owner, actor, now);
    if (r.changes) continue;
    const held = holder.get(c.kind, c.value);
    if (held?.owner !== owner) refused.push({ ...c, holder: claimHolder(db, c.kind, c.value) });
  }
  return refused;
}

/** Releases what `owner` holds. */
export function releaseClaims(db, owner) {
  db.prepare('DELETE FROM claims WHERE owner = ?').run(owner);
}
