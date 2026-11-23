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

/**
 * Reads the workspace's .mcp.json (`mcpServers.db`, falling back to `mcpServers.ithomiini`).
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
  const entry = servers.db ?? servers.ithomiini;
  const url = typeof entry?.url === 'string' ? entry.url.trim() : '';
  if (!/^https?:\/\//i.test(url)) return null;
  const headers = {};
  if (entry?.headers && typeof entry.headers === 'object' && !Array.isArray(entry.headers)) {
    for (const [k, v] of Object.entries(entry.headers)) {
      if (v != null) headers[k] = String(v);
    }
  }
  return { url, headers };
}

/**
 * Extracts a JSON-RPC response object from a `text/event-stream` body.
 * Collects multi-line `data:` blocks per SSE event and prefers the message matching `expectedId`.
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
    if (Array.isArray(parsed)) events.push(...parsed);
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
  if (!events.length) throw new Error('Empty SSE stream');
  const exact = events.find(e => Object.hasOwn(e, 'id') && e.id === expectedId);
  if (exact) return exact;
  const reply = [...events].reverse().find(e => Object.hasOwn(e, 'result') || Object.hasOwn(e, 'error'));
  return reply ?? events[events.length - 1];
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
 * Tracks `mcp-session-id` across requests and never crashes on upstream errors.
 */
export function createProxyHandler({ cwd = process.cwd(), fetchImpl = globalThis.fetch } = {}) {
  const config = readWorkspaceMcp(cwd);
  let sessionId = null;

  async function handleMessage(msg) {
    if (!msg || typeof msg !== 'object' || Array.isArray(msg)) {
      return rpcError(null, -32600, 'Invalid Request');
    }
    if (!config) return handleEmptyServer(msg);

    const notification = isNotificationMessage(msg);
    const headers = {
      'Content-Type': 'application/json',
      Accept: 'application/json, text/event-stream',
      ...(sessionId ? { 'mcp-session-id': sessionId } : {}),
      ...config.headers,
    };

    let res;
    try {
      res = await fetchImpl(config.url, {
        method: 'POST',
        headers,
        body: JSON.stringify(msg),
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
      if (!parsed || typeof parsed !== 'object') {
        return rpcError(msg.id, -32000, 'MCP upstream returned non-object JSON');
      }
      return parsed;
    } catch (err) {
      return rpcError(msg.id, -32000, `Failed to parse MCP upstream response: ${err?.message || err}`);
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

/** Runs the newline-delimited JSON-RPC stdio loop until stdin closes. */
export function runStdio({ cwd = process.cwd(), fetchImpl = globalThis.fetch, stdin = process.stdin, stdout = process.stdout } = {}) {
  const handler = createProxyHandler({ cwd, fetchImpl });
  let buffer = '';
  let chain = Promise.resolve();

  stdout.on('error', () => {
    /* Parent closed pipe. */
  });

  const enqueue = line => {
    chain = chain
      .then(async () => {
        const reply = await handler.handleLine(line);
        if (reply !== null && reply !== undefined) {
          stdout.write(JSON.stringify(reply) + '\n');
        }
      })
      .catch(() => {
        /* Never break the stdio processing chain on an unexpected write/handler error. */
      });
  };

  return new Promise(resolve => {
    stdin.setEncoding('utf8');
    stdin.on('data', chunk => {
      buffer += chunk;
      let idx;
      while ((idx = buffer.indexOf('\n')) !== -1) {
        const line = buffer.slice(0, idx).replace(/\r$/, '');
        buffer = buffer.slice(idx + 1);
        enqueue(line);
      }
    });
    stdin.on('end', () => {
      const tail = buffer.replace(/\r$/, '');
      buffer = '';
      if (tail.trim()) enqueue(tail);
      chain.finally(() => resolve());
    });
    stdin.on('error', () => {
      chain.finally(() => resolve());
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
