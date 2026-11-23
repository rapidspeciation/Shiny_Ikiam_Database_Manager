// Identificar's «En línea (servidor)» for visitors without a sign-in (Franz, 16 Nov 2026: «not logged in people could
// still use the identifier app … at most one image at a time so that if multiple people want to use it they would have
// to wait on a queue»). Off unless ANON_SERVER_IDENTIFY=1 (server/index.mjs); the team's own path
// (POST /api/ai/identify-photo) is unchanged.
//
// No queue of its own: a visitor's photo goes to the app's one identifier and its one queue (server/identifier.mjs),
// the same the team's photos and a walk's use, so the server never reads two photos at once and holds the models
// once. There a visitor's photo (priority 1) comes after the team's waiting photos and before the background ones.
// This module adds what a visitor needs on top of it, as tickets:
// - their place (1: being read) and an estimated wait (the identifier's mean of its last photos' times);
// - leaving cancels: the page sends DELETE when it closes, and a ticket nobody asked about for `staleMs` is given up
//   (a closed tab, a lost signal); a ticket still waiting leaves the identifier's queue at once;
// - at most `maxWaiting` visitors' photos at once (waiting or being read); one more gets 503 BUSY;
// - a visitor's answer within `timeoutMs` of sending, else 504 (the identifier ends its own work by its time limits);
// - an answer nobody picks up goes after `keepMs`.
// Privacy: the photo is only in the request's memory and the identifier's temp file, which identifyUpload removes when
// the reading ends whatever happens (also cancelled or timed out; server/identify-photos.mjs). Nothing goes to the
// database, the photo store or the log; a ticket keeps no photo, only its answer until picked up.

import { randomUUID } from 'node:crypto';

const fail = (code, message, status = 400) => Object.assign(new Error(message), { code, status });

/**
 * `read({ photo, side, signal, ticket })` → { status, body }: the photo read by the identifier (identifyUpload's
 * answer), given up when `signal` aborts while it waits. `standing(ticket)` → the identifier's { state, ahead, waitMs }
 * or null; `queued()` → { ahead, waitMs } for one more visitor's photo.
 */
export function createVisitorIdentify({
  read,
  standing = () => null,
  queued = () => ({ ahead: 0, waitMs: 5000 }),
  maxWaiting = 20,
  maxPolls = 600,
  timeoutMs = 180_000,
  staleMs = 30_000,
  keepMs = 120_000,
  now = () => Date.now(),
  sweepMs = 5_000,
} = {}) {
  const tickets = new Map();
  let closed = false;
  const pending = new Set();
  // Count every admitted photo until its underlying work settles (including one cancelled or timed out while running).
  const active = () => pending.size;

  function view(t) {
    if (t.state === 'queued') {
      const s = standing(t);
      // Not in the identifier's queue yet (its temp file being written): after everyone there.
      const at = s ?? { state: 'waiting', ...queued() };
      return { ticket: t.id, state: at.state, place: at.ahead + 1, ahead: at.ahead, waitS: Math.max(1, Math.ceil(at.waitMs / 1000)) };
    }
    return { ticket: t.id, state: t.state, ...(t.state === 'done' ? { answer: t.answer } : { status: t.status, error: t.error }) };
  }

  function giveUp(t) {
    t.ctl.abort();
    tickets.delete(t.id);
  }

  function sweep() {
    const at = now();
    for (const t of [...tickets.values()]) {
      if (t.state === 'queued' && at - t.seen > staleMs) giveUp(t);
      else if (t.state !== 'queued' && at - t.ended > keepMs) tickets.delete(t.id);
    }
  }
  let sweeper = null;
  function startSweeper() {
    if (sweeper || closed) return;
    sweeper = setInterval(() => {
      sweep();
      if (!tickets.size) {
        clearInterval(sweeper);
        sweeper = null;
      }
    }, sweepMs);
    sweeper.unref?.();
  }

  async function follow(t, photo, side) {
    const work = Promise.resolve()
      .then(() => read({ photo, side, signal: t.ctl.signal, ticket: t }))
      .catch(() => ({ status: 503, body: { error: { code: 'identifier_failed', message: 'The identifier did not answer.' } } }));
    let timer;
    const late = new Promise(resolve => {
      timer = setTimeout(() => resolve({ status: 504, body: { error: { code: 'timeout', message: 'The identifier took too long.' } } }), timeoutMs);
      timer.unref?.();
    });
    const out = await Promise.race([work, late]);
    clearTimeout(timer);
    // Past its time: out of the queue if it still waits (being read, it ends by the identifier's own limit).
    if (out.status === 504) t.ctl.abort();
    t.ended = now();
    if (out?.status === 200) {
      t.state = 'done';
      t.answer = out.body;
    } else {
      t.state = 'failed';
      t.status = out?.status ?? 503;
      t.error = out?.body?.error ?? { code: 'identifier_failed', message: 'The identifier did not answer.' };
    }
    // Keep the slot (and idle()) until the underlying reading has really ended (its temp file removed).
    if (out.status === 504) await work;
  }

  return {
    /** A photo for the identifier: its ticket and place, or 503 BUSY when `maxWaiting` visitors' photos are in already. */
    submit({ photo, side = null }) {
      if (closed) throw fail('UNAVAILABLE', 'The identifier is not available.', 503);
      sweep();
      if (active() >= maxWaiting) throw fail('BUSY', 'The identifier is busy; try again in a minute.', 503);
      const t = { id: randomUUID(), state: 'queued', ctl: new AbortController(), seen: now(), polls: 0 };
      tickets.set(t.id, t);
      startSweeper();
      const p = follow(t, photo, side).finally(() => pending.delete(p));
      pending.add(p);
      return view(t);
    },
    /** A ticket's place and wait, or its answer (given once: the ticket then goes); null when unknown or gone. */
    status(id) {
      const t = tickets.get(id);
      if (!t) return null;
      if (++t.polls > maxPolls) throw fail('RATE_LIMITED', 'Too many requests; try again later', 429);
      t.seen = now();
      // The others who stopped asking go now, so this place is right.
      sweep();
      const out = view(t);
      if (t.state !== 'queued') tickets.delete(id);
      return out;
    },
    /** The visitor left: out of the identifier's queue if it still waits; its answer is not kept. */
    cancel(id) {
      const t = tickets.get(id);
      if (!t) return false;
      giveUp(t);
      return true;
    },
    /** What the page reads before offering it: visitors' photos in, and the wait for one more. */
    info() {
      sweep();
      const q = queued();
      const n = active();
      return { visitors: n, full: n >= maxWaiting, ahead: q.ahead, waitS: Math.max(1, Math.ceil(q.waitMs / 1000)) };
    },
    /** Resolves when every visitor's photo has ended (tests). */
    idle: () => Promise.all([...pending]),
    close() {
      closed = true;
      if (sweeper) clearInterval(sweeper);
      sweeper = null;
      for (const t of tickets.values()) t.ctl.abort();
      tickets.clear();
    },
  };
}
