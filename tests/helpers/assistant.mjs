// What the assistant tests share: a person in the app's users, the token T3 calls the MCP
// tools with, and those calls. Not a test file itself (node --test runs tests/*.test.mjs).
import { createHash } from 'node:crypto';

/** A person in the app's users table; returns them as the server passes a user around. */
export function addUser(store, { id = 'u1', username = 'franz', displayName = 'Franz', role = 'editor' } = {}) {
  store.db
    .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES(?,?,?,?,'s','h',1,'2026-01-01')")
    .run(id, username, displayName, role);
  return { id, username, displayName, role };
}

/** A T3 token for `userId` (only its hash is kept, as the app does). */
export function addToken(store, userId, token) {
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update(token).digest('hex'), userId);
}

/**
 * MCP calls with `token`, as T3 makes them. `call` parses the tool's text as JSON, `raw` returns
 * it as text, `result` the whole result (isError, content). `meta` is the call's _meta, e.g.
 * { 'claudecode/toolUseId': 'toolu_1' } (`toolUseId` sets it for every call).
 */
export function mcpClient(assistant, token, { toolUseId } = {}) {
  const mcp = (method, params) => assistant.mcp({ authorization: `Bearer ${token}` }, { jsonrpc: '2.0', id: 1, method, params });
  const result = async (name, args, meta = toolUseId ? { 'claudecode/toolUseId': toolUseId } : undefined) =>
    (await mcp('tools/call', { name, arguments: args, ...(meta ? { _meta: meta } : {}) })).body.result;
  const raw = async (name, args, meta) => (await result(name, args, meta)).content[0].text;
  const call = async (name, args, meta) => JSON.parse(await raw(name, args, meta));
  return { mcp, result, raw, call };
}

/**
 * A person signed in to the assistant: the user (see addUser), their token (default
 * `<username>-token`) and their MCP calls (see mcpClient), plus the app's API as them:
 * `http(method, path, { body, query, headers, page })` and `get(path, query)`.
 */
export function signIn(store, assistant, { token, toolUseId, ...person } = {}) {
  const user = addUser(store, person);
  const key = token ?? `${user.username}-token`;
  addToken(store, user.id, key);
  const http = (method, path, { body = {}, query = {}, headers = {}, page } = {}) =>
    assistant.handle({ method, path, body, user, query, headers, ...(page !== undefined ? { page } : {}) });
  const get = (path, query = {}) => http('GET', path, { query });
  return { user, token: key, ...mcpClient(assistant, key, { toolUseId }), http, get };
}

/** Waits until `check()` is true (polled every 5 ms), failing after `ms`. */
export async function until(check, ms = 2000) {
  const end = Date.now() + ms;
  while (!(await check())) {
    if (Date.now() > end) throw new Error('timed out waiting');
    await new Promise(resolve => setTimeout(resolve, 5));
  }
}
