import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { orchestrator2, t3Commands } from '../server/t3commands.mjs';
import { chatStarter } from '../server/t3projects.mjs';
import { tellChat } from '../server/t3tell.mjs';

// T3's commands from the app: over HTTP up to 0.0.45, over its socket (new shapes) from 0.0.46.

const tokenFile = (() => {
  const dir = mkdtempSync(join(tmpdir(), 't3commands-'));
  writeFileSync(join(dir, 'token'), 'admin-token\n');
  return join(dir, 'token');
})();

/** T3 as the app reaches it: `dispatch` answers POST /api/orchestration/dispatch (404 from 0.0.46), `answer` each socket request. */
function fakeT3({ dispatch = 200, answer = () => ({ _tag: 'Success', value: { sequence: 1 } }) } = {}) {
  const posts = [];
  const sockets = [];
  const fetchImpl = async (url, init) => {
    posts.push({ url, auth: init.headers.authorization, body: init.body ? JSON.parse(init.body) : null });
    if (url.endsWith('/api/auth/websocket-ticket')) return { ok: true, status: 200, json: async () => ({ ticket: 'ticket 1' }) };
    return { ok: dispatch < 300, status: dispatch };
  };
  class WebSocketImpl {
    constructor(url) {
      Object.assign(this, { url, readyState: 0, sent: [], closed: false, listeners: {} });
      sockets.push(this);
      setTimeout(() => ((this.readyState = 1), this.emit('open')), 0);
    }
    addEventListener(name, fn) {
      (this.listeners[name] ??= []).push(fn);
    }
    emit(name, event = {}) {
      for (const fn of this.listeners[name] ?? []) fn(event);
    }
    send(text) {
      const request = JSON.parse(text);
      this.sent.push(request);
      const exit = answer(request);
      // T3 answers each request with its Exit (as a batch, sometimes).
      if (exit) setTimeout(() => this.emit('message', { data: JSON.stringify([{ _tag: 'Exit', requestId: request.id, exit }]) }), 0);
    }
    close() {
      this.closed = true;
    }
  }
  return { posts, sockets, fetchImpl, WebSocketImpl };
}

test('up to T3 0.0.45: each command is POSTed to its dispatch route, in order, with the admin token', async () => {
  const t3 = fakeT3();
  const start = chatStarter({ local: 'http://t3-045.test', tokenFile, ...t3 });
  const id = await start({ id: 'p-ana' }, { title: 'Revisión', text: 'Revisa el lote' });
  assert.deepEqual(t3.posts.map(p => [p.url, p.auth, p.body.type]), [
    ['http://t3-045.test/api/orchestration/dispatch', 'Bearer admin-token', 'thread.create'],
    ['http://t3-045.test/api/orchestration/dispatch', 'Bearer admin-token', 'thread.turn.start'],
  ]);
  const [made, turn] = t3.posts.map(p => p.body);
  assert.deepEqual([made.threadId, turn.threadId, turn.message.text, turn.runtimeMode], [id, id, 'Revisa el lote', 'full-access']);
  assert.ok(made.commandId && made.createdAt && made.commandId !== turn.commandId);
  assert.equal(t3.sockets.length, 0);
});

test("from T3 0.0.46 (no dispatch route): the commands go over its socket with a ticket, in the new shapes; that T3's way is kept", async () => {
  const t3 = fakeT3({ dispatch: 404 });
  const start = chatStarter({ local: 'http://t3-046.test:3773', tokenFile, ...t3 });
  const id = await start({ id: 'p-ana' }, { title: 'Revisión', text: 'Revisa el lote' });
  assert.deepEqual(t3.posts.map(p => [p.url, p.auth]), [
    ['http://t3-046.test:3773/api/orchestration/dispatch', 'Bearer admin-token'],
    ['http://t3-046.test:3773/api/auth/websocket-ticket', 'Bearer admin-token'],
  ]);
  const [socket] = t3.sockets;
  assert.equal(socket.url, 'ws://t3-046.test:3773/ws?orchestrationProtocol=2&wsTicket=ticket+1');
  assert.deepEqual(socket.sent.map(r => [r._tag, r.id, r.tag]), [
    ['Request', '1', 'orchestration.dispatchCommand'],
    ['Request', '2', 'orchestration.dispatchCommand'],
  ]);
  const [made, message] = socket.sent.map(r => r.payload);
  assert.deepEqual(
    [made.type, made.threadId, made.projectId, made.title, made.createdBy, made.creationSource, made.runtimeMode, made.createdAt],
    ['thread.create', id, 'p-ana', 'Revisión', 'user', 'web', 'full-access', undefined],
  );
  assert.equal(made.modelSelection.model, 'claude-opus-5-5');
  assert.deepEqual(
    { ...message, commandId: 'c', messageId: 'm', modelSelection: undefined },
    {
      type: 'message.dispatch',
      commandId: 'c',
      createdBy: 'user',
      creationSource: 'web',
      threadId: id,
      messageId: 'm',
      text: 'Revisa el lote',
      attachments: [],
      modelSelection: undefined,
      dispatchMode: { type: 'start_immediately' },
    },
  );
  assert.equal(socket.closed, true);
  // The next command goes straight to the socket.
  await start({ id: 'p-ana' });
  assert.deepEqual(t3.posts.slice(2).map(p => p.url), ['http://t3-046.test:3773/api/auth/websocket-ticket']);
  assert.equal(t3.sockets[1].sent.length, 1, 'an empty chat: one command');
});

test('a message into an idle chat on 0.0.46 is message.dispatch, with the chat keeping its own modes', async () => {
  const t3 = fakeT3({ dispatch: 404 });
  const chats = {
    session: () => ({ projectId: 'p-franz', runtimeMode: 'approval-required', interactionMode: 'plan', busy: false }),
    projectsOf: () => ['p-franz'],
  };
  const out = await tellChat({
    t3: { local: 'http://t3-tell.test', tokenFile },
    chats,
    threadId: 'th-1',
    username: 'franz',
    text: 'La hoja cambió.',
    fetchImpl: t3.fetchImpl,
    WebSocketImpl: t3.WebSocketImpl,
  });
  assert.deepEqual(out, { sent: true });
  const { payload } = t3.sockets[0].sent[0];
  assert.deepEqual([payload.type, payload.threadId, payload.text, payload.runtimeMode], ['message.dispatch', 'th-1', 'La hoja cambió.', undefined]);
});

test('T3 refusing a command over its socket: its reason, the socket closed, and the next command tries HTTP again', async () => {
  let refuse = true;
  const t3 = fakeT3({
    dispatch: 404,
    answer: () =>
      refuse
        ? { _tag: 'Failure', cause: [{ _tag: 'Fail', error: { _tag: 'OrchestrationV2DispatchCommandError', message: 'Thread not found.' } }] }
        : { _tag: 'Success', value: { sequence: 2 } },
  });
  const send = t3Commands({ local: 'http://t3-refuses.test', tokenFile, ...t3 });
  await assert.rejects(send({ type: 'thread.turn.start', threadId: 't', message: { messageId: 'm', text: 'x' } }), /T3 refused the command: Thread not found\./);
  assert.equal(t3.sockets[0].closed, true);
  refuse = false;
  await send({ type: 'thread.create', threadId: 't2' });
  assert.deepEqual(
    t3.posts.map(p => p.url.split('/api/')[1]),
    ['orchestration/dispatch', 'auth/websocket-ticket', 'orchestration/dispatch', 'auth/websocket-ticket'],
  );
  // T3 0.0.45 refusing over HTTP: its status.
  const old = t3Commands({ local: 'http://t3-045-refuses.test', tokenFile, ...fakeT3({ dispatch: 403 }) });
  await assert.rejects(old({ type: 'thread.create', threadId: 't' }), /T3 answered 403/);
  assert.equal(t3Commands({ local: 'http://x' }), null, 'no token, no commands');
});

test('a socket that never answers fails within the time given, and is closed', async () => {
  const t3 = fakeT3({ dispatch: 404, answer: () => null });
  const send = t3Commands({ local: 'http://t3-silent.test', tokenFile, timeout: 50, ...t3 });
  const started = Date.now();
  await assert.rejects(send({ type: 'thread.create', threadId: 't' }), /T3 took too long/);
  assert.ok(Date.now() - started < 1000);
  assert.equal(t3.sockets[0].closed, true);
});

test("0.0.46's shapes: other commands as they are, without 0.0.45's createdAt", () => {
  assert.deepEqual(orchestrator2({ type: 'thread.archive', commandId: 'c', threadId: 't', createdAt: 'x' }), {
    type: 'thread.archive',
    commandId: 'c',
    threadId: 't',
  });
  const turn = orchestrator2({
    type: 'thread.turn.start',
    commandId: 'c',
    threadId: 't',
    message: { messageId: 'm', role: 'user', text: 'hola', attachments: [{ type: 'image', id: 'a' }] },
    modelSelection: { instanceId: 'claudeAgent', model: 'claude-sonnet-5-5' },
    runtimeMode: 'full-access',
  });
  assert.deepEqual([turn.attachments, turn.modelSelection.model, 'runtimeMode' in turn], [[{ type: 'image', id: 'a' }], 'claude-sonnet-5-5', false]);
});
