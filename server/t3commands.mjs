// T3 Code's own commands, sent by the app with its admin token (a chat made, a message
// sent: server/t3projects.mjs, server/t3tell.mjs). T3 is a stock install and takes them
// one of two ways, by version:
// - up to 0.0.45: POST /api/orchestration/dispatch, one command per request;
// - from 0.0.46 (its new orchestrator, protocol 2) that route is gone (404). Commands go
//   over T3's socket (/ws, Effect RPC as JSON text frames), opened with a one-time ticket
//   the token gets (POST /api/auth/websocket-ticket); and two of them changed shape
//   (orchestrator2 below).
// Which way a T3 takes is learnt from its answer and kept while commands go through; a
// failed command forgets it, so an updated T3 is asked again.
import { randomUUID } from 'node:crypto';
import { readFile } from 'node:fs/promises';

const TIMEOUT_MS = 10_000;
/** The orchestration protocol of the socket's commands (T3 refuses others with 426). */
const PROTOCOL = 2;
/** How each T3 (by its local address) took commands last: 'http' or 'socket'. */
const ways = new Map();

/**
 * A command as 0.0.46 takes it; the app writes them as 0.0.45 does. thread.create
 * says who made the chat (the person, as from T3's page); a message is
 * message.dispatch, with the chat's own modes (set when it was made). Pure, for tests.
 */
export function orchestrator2(command) {
  const { createdAt: _, ...rest } = command;
  if (command.type === 'thread.create') return { ...rest, createdBy: 'user', creationSource: 'web' };
  if (command.type !== 'thread.turn.start') return rest;
  const { message } = command;
  return {
    type: 'message.dispatch',
    commandId: command.commandId,
    createdBy: 'user',
    creationSource: 'web',
    threadId: command.threadId,
    messageId: message.messageId,
    text: message.text,
    attachments: message.attachments ?? [],
    ...(command.modelSelection ? { modelSelection: command.modelSelection } : {}),
    // The app only writes to a chat that is not answering.
    dispatchMode: { type: 'start_immediately' },
  };
}

/** Why T3 refused a command over the socket (its error's message, or the start of a defect). */
function failure(exit) {
  const cause = Array.isArray(exit?.cause) ? exit.cause[0] : null;
  const why = cause?.error?.message ?? cause?.error?._tag ?? (typeof cause?.defect === 'string' ? cause.defect : cause?._tag);
  return new Error(`T3 refused the command: ${String(why ?? 'unknown').slice(0, 200)}`);
}

/** The commands over HTTP, in order; false when T3 has no such route (0.0.46 on), before any was sent. */
async function overHttp({ local, token, fetchImpl, timeout }, commands) {
  for (const [i, command] of commands.entries()) {
    const response = await fetchImpl(`${local}/api/orchestration/dispatch`, {
      method: 'POST',
      headers: { authorization: `Bearer ${token}`, 'content-type': 'application/json' },
      body: JSON.stringify({ ...command, createdAt: new Date().toISOString() }),
      signal: AbortSignal.timeout(timeout),
    });
    if (response.status === 404 && i === 0) return false;
    if (!response.ok) throw new Error(`T3 answered ${response.status}`);
  }
  return true;
}

/** The commands over one socket, each answered before the next; the socket is closed after. */
async function overSocket({ local, token, fetchImpl, WebSocketImpl, timeout }, commands) {
  const signal = AbortSignal.timeout(timeout);
  const response = await fetchImpl(`${local}/api/auth/websocket-ticket`, {
    method: 'POST',
    headers: { authorization: `Bearer ${token}` },
    signal,
  });
  if (!response.ok) throw new Error(`T3 answered ${response.status} for a socket ticket`);
  const { ticket } = await response.json();
  const url = new URL('/ws', local);
  url.protocol = url.protocol === 'https:' ? 'wss:' : 'ws:';
  url.searchParams.set('orchestrationProtocol', String(PROTOCOL));
  url.searchParams.set('wsTicket', ticket);
  const socket = new WebSocketImpl(url.href);
  const waiting = new Map();
  let opened;
  let ended = null;
  const end = error => {
    ended ??= error;
    opened?.reject(error);
    for (const pending of waiting.values()) pending.reject(error);
    waiting.clear();
  };
  socket.addEventListener('open', () => opened?.resolve());
  socket.addEventListener('error', () => end(new Error('T3 socket failed')));
  socket.addEventListener('close', () => end(new Error('T3 closed the socket')));
  socket.addEventListener('message', event => {
    let frames;
    try {
      frames = JSON.parse(String(event.data));
    } catch {
      return;
    }
    for (const frame of [frames].flat()) {
      if (frame?._tag === 'Defect') end(new Error('T3 failed the request'));
      if (frame?._tag !== 'Exit') continue;
      const pending = waiting.get(String(frame.requestId));
      waiting.delete(String(frame.requestId));
      if (frame.exit?._tag === 'Success') pending?.resolve(frame.exit.value);
      else pending?.reject(failure(frame.exit));
    }
  });
  const onAbort = () => end(new Error('T3 took too long'));
  signal.addEventListener('abort', onAbort);
  try {
    if (socket.readyState !== 1) await new Promise((resolve, reject) => (opened = { resolve, reject }));
    for (const [i, command] of commands.entries()) {
      if (ended) throw ended;
      const id = String(i + 1);
      const answered = new Promise((resolve, reject) => waiting.set(id, { resolve, reject }));
      socket.send(JSON.stringify({ _tag: 'Request', id, tag: 'orchestration.dispatchCommand', payload: orchestrator2(command), headers: [] }));
      await answered;
    }
  } finally {
    signal.removeEventListener('abort', onAbort);
    socket.close();
  }
}

/**
 * send(...commands) for the T3 at `local` with the admin token in `tokenFile`: each command
 * (0.0.45's shape, without commandId) gets its id and goes, in order, the way that T3 takes
 * it. Resolves once T3 accepted them all; null without T3 or its token.
 */
export function t3Commands({ local, tokenFile, fetchImpl = fetch, WebSocketImpl = globalThis.WebSocket, timeout = TIMEOUT_MS } = {}) {
  if (!local || !tokenFile) return null;
  return async (...list) => {
    const commands = list.map(command => ({ commandId: randomUUID(), ...command }));
    const t3 = { local, token: (await readFile(tokenFile, 'utf8')).trim(), fetchImpl, WebSocketImpl, timeout };
    try {
      if (ways.get(local) !== 'socket' && (await overHttp(t3, commands))) return void ways.set(local, 'http');
      await overSocket(t3, commands);
      ways.set(local, 'socket');
    } catch (e) {
      ways.delete(local);
      throw e;
    }
  };
}
