// The Insectary ID of a butterfly being registered in Emergidos, held from the tap
// that gives it (+♀, +♂, + larva) until its card is saved or taken away. Four people
// work in the insectary at once and the ID goes on the wing at that moment: nobody
// else is offered it, nor can save it, meanwhile. A hold is a claim
// (server/claims.mjs) owned by `hold:<card key>`; saving the card (server/staged.mjs)
// passes it to the entry, taking the card away releases it, and a hold left by a
// card nobody saved goes after HOLD_HOURS.

import { claimHolder, claimValue, releaseClaims, takeClaims } from './claims.mjs';
import { idSuggestions } from './grid.mjs';
import { msgError } from './messages.mjs';

export const HOLD = 'hold:';
export const HOLD_HOURS = 36;
const KEY = /^[A-Za-z0-9_-]{6,80}$/;
const ID = /^[A-Z0-9]{2,8}(\.[1-9]\d?)?$/;
const fail = (code, message, status = 400) => msgError(message, { code, status });

/** Holds older than HOLD_HOURS (a card left unsaved on a phone) are let go. */
export function dropStaleHolds(db, now = Date.now()) {
  try {
    db.prepare("DELETE FROM claims WHERE owner LIKE 'hold:%' AND created_at < ?").run(new Date(now - HOLD_HOURS * 3_600_000).toISOString());
  } catch {
    /* a database without the app's tables */
  }
}

/** The holds `actor` has (their owners), which a save of theirs may use as its own. */
export function holdOwners(db, actor) {
  return db.prepare("SELECT DISTINCT owner FROM claims WHERE owner LIKE 'hold:%' AND actor = ?").all(actor).map(r => r.owner);
}

/**
 * Holds `value` for the card `key` (POST /api/ids/hold): the card's earlier hold
 * (its ID changed) is let go in the same step. Answers { held: true, value } or,
 * when the ID is not free (`code` CLAIMED: someone else holds it, with who; USED:
 * a butterfly of the sheet, or not a free pre-made row), { held: false, next }:
 * the next free one, for the app to take instead.
 */
export function holdId(store, body, user) {
  store.validateRole(user);
  const key = String(body?.key ?? '');
  if (!KEY.test(key)) throw fail('INVALID_KEY', 'Falta la tarjeta');
  if ((body?.kind ?? 'insectary') !== 'insectary') throw fail('INVALID_KIND', 'Solo se reservan Insectary IDs');
  const value = claimValue(body?.value);
  if (!ID.test(value)) throw fail('INVALID_VALUES', 'Insectary ID no válido');
  const db = store.db;
  const owner = HOLD + key;
  const actor = user.id || user.username;
  dropStaleHolds(db);
  const holder = claimHolder(db, 'insectary', value);
  if (holder && holder.owner === owner) return { held: true, value };
  const next = () => idSuggestions(store, { kind: 'insectary', count: 1 }).sequence?.[0] ?? null;
  if (holder) return { held: false, value, code: 'CLAIMED', holder: holder.name, next: next() };
  // A suffixed ID (W0B.1: the same ID on a second butterfly) has no pre-made row of its own.
  const suffixed = value.includes('.');
  if (!suffixed && !idSuggestions(store, { kind: 'insectary', count: 5000 }).sequence?.some(id => claimValue(id) === value))
    return { held: false, value, code: 'USED', next: next() };
  db.exec('BEGIN IMMEDIATE');
  try {
    releaseClaims(db, owner);
    const refused = takeClaims(db, owner, actor, [{ kind: 'insectary', value, field: 'Insectary_ID' }]);
    if (refused.length) {
      db.exec('ROLLBACK');
      return { held: false, value, code: 'CLAIMED', holder: refused[0].holder?.name ?? '?', next: next() };
    }
    db.exec('COMMIT');
  } catch (e) {
    db.exec('ROLLBACK');
    throw e;
  }
  store.bumpLive?.('staged');
  return { held: true, value };
}

/** Lets go the hold of card `key` (DELETE /api/ids/hold/<key>): taken away, or saved. */
export function releaseHold(store, key, user) {
  store.validateRole(user);
  if (!KEY.test(String(key ?? ''))) throw fail('INVALID_KEY', 'Falta la tarjeta');
  const r = store.db.prepare('DELETE FROM claims WHERE owner = ?').run(HOLD + key);
  if (r.changes) store.bumpLive?.('staged');
  return { released: r.changes };
}
