#!/usr/bin/env node
// Standalone stdio-to-Streamable-HTTP MCP proxy for Antigravity ACP threads in T3 Code.
// Zero external dependencies and no relative imports so it can be copied outside release dirs.
// Reads join(cwd, '.mcp.json') at session start so each workspace keeps strict per-user token
// isolation even though Antigravity shares one profile config/mcp_config.json across workspaces.
import { readFileSync, realpathSync } from 'node:fs';
import { join } from 'node:path';
import { pathToFileURL } from 'node:url';

const PROTOCOL_VERSION = '2025-06-18';
const SERVER_INFO = { name: 'ithomiini-mcp-proxy', version: '1.0.0' };
const DEFAULT_TIMEOUT_MS = 120_000;

/**
 * Reads the workspace's .mcp.json (`mcpServers.db`, falling back to `mcpServers.ithomiini`
 * if `db` is absent or not an HTTP(S) entry). Strict to `cwd` (never walks parent dirs).
 * Returns `{ url, headers }` for an HTTP(S) MCP server, or `null` when missing/unconfigured.
 */
export function readWorkspaceMcp(cwd = process.cwd()) {
  let raw;
  try {
    raw = readFileSync(join(cwd, '.mcp.json'), 'utf8').trim();
  } catch {
    return null;
  }
  if (!raw) return null;
  let data;
  try {
    data = JSON.parse(raw);
  } catch {
    return null;
  }
  const servers = data?.mcpServers;
  if (!servers || typeof servers !== 'object' || Array.isArray(servers)) return null;
  const isHttpEntry = entry =>
    Boolean(
      entry &&
        typeof entry === 'object' &&
        !Array.isArray(entry) &&
        typeof entry.url === 'string' &&
        /^https?:\/\//i.test(entry.url.trim()),
    );
  const entry = [servers.db, servers.ithomiini].find(isHttpEntry);
  if (!entry) return null;
  const url = entry.url.trim();
  const headers = {};
  if (entry.headers && typeof entry.headers === 'object' && !Array.isArray(entry.headers)) {
    for (const [k, v] of Object.entries(entry.headers)) {
      if (v != null) headers[k] = String(v);
    }
  }
  return { url, headers };
}

/**
 * Extracts a JSON-RPC response object from a `text/event-stream` body.
 * Collects multi-line `data:` blocks per SSE event and prefers the response matching `expectedId`.
 * SSE server notifications ahead of the reply are dropped (stdio emits one reply per request).
 */
export function parseSseJsonRpc(text, expectedId) {
  const events = [];
  let dataLines = [];
  const flush = () => {
    if (!dataLines.length) return;
    const payload = dataLines.join('\n').trim();
    dataLines = [];
    if (!payload) return;
    const parsed = JSON.parse(payload);
    if (Array.isArray(parsed)) events.push(...parsed.filter(e => e && typeof e === 'object' && !Array.isArray(e)));
    else if (parsed && typeof parsed === 'object') events.push(parsed);
  };
  for (const rawLine of String(text ?? '').split(/\r?\n/)) {
    if (rawLine === '') {
      flush();
      continue;
    }
    if (rawLine.startsWith(':')) continue;
    if (rawLine.startsWith('data:')) {
      const value = rawLine.slice(5);
      dataLines.push(value.startsWith(' ') ? value.slice(1) : value);
    }
  }
  flush();
  const responses = events.filter(e => Object.hasOwn(e, 'result') || Object.hasOwn(e, 'error'));
  if (!responses.length) throw new Error('SSE stream contained no JSON-RPC response');
  const exact = responses.find(e => Object.hasOwn(e, 'id') && e.id === expectedId);
  return exact ?? responses[responses.length - 1];
}

function isNotificationMessage(msg) {
  return !Object.hasOwn(msg, 'id') || msg.id === undefined || String(msg.method ?? '').startsWith('notifications/');
}

function rpcError(id, code, message) {
  return { jsonrpc: '2.0', id, error: { code, message: String(message || 'Unknown error') } };
}

function extractErrorDetail(text) {
  const trimmed = String(text ?? '').trim();
  if (!trimmed) return '';
  try {
    const parsed = JSON.parse(trimmed);
    if (parsed?.error?.message) return String(parsed.error.message);
  } catch {
    /* plain text body */
  }
  return trimmed.slice(0, 200);
}

function handleEmptyServer(msg) {
  if (isNotificationMessage(msg)) return null;
  const method = String(msg.method ?? '');
  if (method === 'initialize') {
    return {
      jsonrpc: '2.0',
      id: msg.id,
      result: {
        protocolVersion: msg.params?.protocolVersion || PROTOCOL_VERSION,
        capabilities: { tools: {} },
        serverInfo: SERVER_INFO,
      },
    };
  }
  if (method === 'ping') return { jsonrpc: '2.0', id: msg.id, result: {} };
  if (method === 'tools/list') return { jsonrpc: '2.0', id: msg.id, result: { tools: [] } };
  return rpcError(msg.id, -32601, `Method not found: ${method}`);
}

/**
 * Creates a JSON-RPC message/line handler bound to one workspace `cwd`.
 * Serializes only until `initialize` completes (so `mcp-session-id` is established),
 * then dispatches subsequent messages concurrently with an upstream fetch timeout.
 */
export function createProxyHandler({ cwd = process.cwd(), fetchImpl = globalThis.fetch, timeoutMs = DEFAULT_TIMEOUT_MS } = {}) {
  const config = readWorkspaceMcp(cwd);
  let sessionId = null;
  let initGate = Promise.resolve();

  async function handleMessage(msg) {
    if (!msg || typeof msg !== 'object' || Array.isArray(msg)) {
      return rpcError(null, -32600, 'Invalid Request');
    }
    // Method-less JSON-RPC messages (e.g. client responses with id/result or id/error) need no reply.
    if (!Object.hasOwn(msg, 'method') || msg.method === undefined) {
      return null;
    }
    if (!config) return handleEmptyServer(msg);

    const isInit = String(msg.method ?? '') === 'initialize';
    const gate = initGate;
    let releaseInit = null;
    if (isInit) {
      initGate = new Promise(resolve => {
        releaseInit = resolve;
      });
    }

    await gate;
    try {
      const notification = isNotificationMessage(msg);
      const headers = {
        ...config.headers,
        'Content-Type': 'application/json',
        Accept: 'application/json, text/event-stream',
        ...(sessionId ? { 'mcp-session-id': sessionId } : {}),
      };

      const signal = timeoutMs > 0 ? AbortSignal.timeout(timeoutMs) : undefined;
      let res;
      try {
        res = await fetchImpl(config.url, {
          method: 'POST',
          headers,
          body: JSON.stringify(msg),
          ...(signal ? { signal } : {}),
        });
      } catch (err) {
        return notification ? null : rpcError(msg.id, -32000, `MCP upstream request failed: ${err?.message || err}`);
      }

      const nextSession = res?.headers?.get?.('mcp-session-id');
      if (nextSession) sessionId = nextSession;

      if (notification) {
        if (typeof res?.text === 'function') await res.text().catch(() => {});
        return null;
      }

      let text = '';
      try {
        text = typeof res?.text === 'function' ? await res.text() : '';
      } catch (err) {
        return rpcError(msg.id, -32000, `Failed to read MCP upstream body: ${err?.message || err}`);
      }

      if (!res?.ok) {
        const detail = extractErrorDetail(text);
        return rpcError(msg.id, -32000, `MCP upstream HTTP ${res?.status ?? 0}${detail ? `: ${detail}` : ''}`);
      }

      if (!text.trim()) {
        return rpcError(msg.id, -32000, `MCP upstream returned empty body (HTTP ${res.status})`);
      }

      const contentType = String(res.headers?.get?.('content-type') ?? '').toLowerCase();
      try {
        if (contentType.includes('text/event-stream')) {
          return parseSseJsonRpc(text, msg.id);
        }
        const parsed = JSON.parse(text);
        if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) {
          return rpcError(msg.id, -32000, 'MCP upstream returned non-object JSON');
        }
        return parsed;
      } catch (err) {
        return rpcError(msg.id, -32000, `Failed to parse MCP upstream response: ${err?.message || err}`);
      }
    } finally {
      releaseInit?.();
    }
  }

  async function handleLine(line) {
    const trimmed = String(line ?? '').trim();
    if (!trimmed) return null;
    let msg;
    try {
      msg = JSON.parse(trimmed);
    } catch (err) {
      return rpcError(null, -32700, `Parse error: ${err?.message || err}`);
    }
    return handleMessage(msg);
  }

  return {
    config,
    get sessionId() {
      return sessionId;
    },
    handleMessage,
    handleLine,
  };
}

/** Runs the newline-delimited JSON-RPC stdio loop until stdin closes and all in-flight replies finish. */
export function runStdio({
  cwd = process.cwd(),
  fetchImpl = globalThis.fetch,
  timeoutMs = DEFAULT_TIMEOUT_MS,
  stdin = process.stdin,
  stdout = process.stdout,
} = {}) {
  const handler = createProxyHandler({ cwd, fetchImpl, timeoutMs });
  let buffer = '';
  const inFlight = new Set();

  stdout.on('error', () => {
    /* Parent closed pipe. */
  });

  const dispatch = line => {
    const p = handler
      .handleLine(line)
      .then(reply => {
        if (reply !== null && reply !== undefined) {
          stdout.write(JSON.stringify(reply) + '\n');
        }
      })
      .catch(() => {
        /* Never crash the stdio loop on an unexpected write/handler error. */
      });
    inFlight.add(p);
    p.finally(() => inFlight.delete(p));
  };

  return new Promise(resolve => {
    stdin.setEncoding('utf8');
    stdin.on('data', chunk => {
      buffer += chunk;
      let idx;
      while ((idx = buffer.indexOf('\n')) !== -1) {
        const line = buffer.slice(0, idx).replace(/\r$/, '');
        buffer = buffer.slice(idx + 1);
        dispatch(line);
      }
    });
    stdin.on('end', () => {
      const tail = buffer.replace(/\r$/, '');
      buffer = '';
      if (tail.trim()) dispatch(tail);
      Promise.allSettled([...inFlight]).then(() => resolve());
    });
    stdin.on('error', () => {
      Promise.allSettled([...inFlight]).then(() => resolve());
    });
  });
}

function isScript() {
  try {
    return (
      Boolean(process.argv[1]) &&
      pathToFileURL(realpathSync(process.argv[1])).href === pathToFileURL(realpathSync(new URL(import.meta.url))).href
    );
  } catch {
    return false;
  }
}

if (isScript()) {
  runStdio();
}
