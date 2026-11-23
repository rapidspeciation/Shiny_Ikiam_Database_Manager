import test from 'node:test';
import assert from 'node:assert/strict';
import http from 'node:http';
import { spawn } from 'node:child_process';
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { createProxyHandler, parseSseJsonRpc, readWorkspaceMcp } from '../scripts/mcp-stdio-proxy.mjs';

const proxyScript = new URL('../scripts/mcp-stdio-proxy.mjs', import.meta.url).pathname;

function makeWorkspace(mcpJson) {
  const dir = mkdtempSync(join(tmpdir(), 'mcp-stdio-proxy-'));
  if (mcpJson !== undefined) {
    writeFileSync(join(dir, '.mcp.json'), typeof mcpJson === 'string' ? mcpJson : JSON.stringify(mcpJson));
  }
  return dir;
}

test('empty workspace fallback: missing or non-HTTP .mcp.json serves empty MCP without leaking tokens or calling fetch', async () => {
  const emptyDir = makeWorkspace(undefined);
  const stdioOnlyDir = makeWorkspace({ mcpServers: { db: { command: 'echo', args: ['hi'] } } });
  const unrelatedDir = makeWorkspace({ mcpServers: { other: { type: 'http', url: 'http://127.0.0.1:9999/mcp' } } });
  try {
    for (const dir of [emptyDir, stdioOnlyDir, unrelatedDir]) {
      assert.equal(readWorkspaceMcp(dir), null);
      let fetched = 0;
      const proxy = createProxyHandler({
        cwd: dir,
        fetchImpl: async () => {
          fetched++;
          throw new Error('must not fetch');
        },
      });
      const init = await proxy.handleMessage({ jsonrpc: '2.0', id: 1, method: 'initialize', params: { protocolVersion: '2025-06-18' } });
      assert.equal(init.jsonrpc, '2.0');
      assert.equal(init.id, 1);
      assert.equal(init.result.protocolVersion, '2025-06-18');
      assert.deepEqual(init.result.capabilities, { tools: {} });

      assert.deepEqual(await proxy.handleMessage({ jsonrpc: '2.0', id: 2, method: 'ping' }), { jsonrpc: '2.0', id: 2, result: {} });
      assert.deepEqual(await proxy.handleMessage({ jsonrpc: '2.0', id: 3, method: 'tools/list' }), { jsonrpc: '2.0', id: 3, result: { tools: [] } });
      assert.equal(await proxy.handleMessage({ jsonrpc: '2.0', method: 'notifications/initialized' }), null);
      assert.equal(await proxy.handleMessage({ jsonrpc: '2.0', id: 4, method: 'notifications/progress' }), null);

      const unknown = await proxy.handleMessage({ jsonrpc: '2.0', id: 5, method: 'tools/call', params: { name: 'query' } });
      assert.equal(unknown.id, 5);
      assert.equal(unknown.error.code, -32601);
      assert.equal(fetched, 0);
    }
  } finally {
    for (const d of [emptyDir, stdioOnlyDir, unrelatedDir]) rmSync(d, { recursive: true, force: true });
  }
});

test('HTTP JSON forwarding: sends Bearer header, tracks mcp-session-id, falls back to ithomiini, and handles 202 notifications silently', async () => {
  const dir = makeWorkspace({
    mcpServers: {
      ithomiini: { type: 'http', url: 'http://127.0.0.1:9/api/ai/mcp', headers: { Authorization: 'Bearer tok-legacy' } },
    },
  });
  try {
    const seen = [];
    const proxy = createProxyHandler({
      cwd: dir,
      fetchImpl: async (url, opts) => {
        seen.push({ url, method: opts.method, headers: opts.headers, body: JSON.parse(opts.body) });
        const body = JSON.parse(opts.body);
        if (body.method === 'initialize') {
          return new Response(JSON.stringify({ jsonrpc: '2.0', id: body.id, result: { protocolVersion: '2025-06-18', capabilities: { tools: {} } } }), {
            status: 200,
            headers: { 'content-type': 'application/json', 'mcp-session-id': 'sess-42' },
          });
        }
        if (String(body.method).startsWith('notifications/') || body.id === undefined) {
          return new Response(null, { status: 202 });
        }
        return new Response(JSON.stringify({ jsonrpc: '2.0', id: body.id, result: { tools: [{ name: 'query' }] } }), {
          status: 200,
          headers: { 'content-type': 'application/json' },
        });
      },
    });

    const init = await proxy.handleMessage({ jsonrpc: '2.0', id: 0, method: 'initialize', params: {} });
    assert.equal(init.id, 0);
    assert.equal(proxy.sessionId, 'sess-42');
    assert.equal(seen[0].url, 'http://127.0.0.1:9/api/ai/mcp');
    assert.equal(seen[0].headers.Authorization, 'Bearer tok-legacy');
    assert.equal(seen[0].headers['Content-Type'], 'application/json');
    assert.equal(seen[0].headers.Accept, 'application/json, text/event-stream');
    assert.equal(seen[0].headers['mcp-session-id'], undefined);

    // Notification: forwarded with session id, returns null on 202 empty body.
    assert.equal(await proxy.handleMessage({ jsonrpc: '2.0', method: 'notifications/initialized' }), null);
    assert.equal(seen[1].headers['mcp-session-id'], 'sess-42');
    assert.equal(seen[1].headers.Authorization, 'Bearer tok-legacy');

    // Notification without notifications/ prefix (missing id): still forwarded and silent.
    assert.equal(await proxy.handleMessage({ jsonrpc: '2.0', method: 'initialized' }), null);
    assert.equal(seen[2].headers['mcp-session-id'], 'sess-42');

    // Subsequent request carries tracked session id.
    const list = await proxy.handleMessage({ jsonrpc: '2.0', id: 2, method: 'tools/list' });
    assert.deepEqual(list.result.tools, [{ name: 'query' }]);
    assert.equal(seen[3].headers['mcp-session-id'], 'sess-42');
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test('SSE responses, upstream HTTP/network/parse errors, and malformed stdin lines return valid JSON-RPC errors', async () => {
  const dir = makeWorkspace({
    mcpServers: {
      db: { type: 'http', url: 'http://127.0.0.1:9/api/ai/mcp', headers: { Authorization: 'Bearer tok-1' } },
    },
  });
  try {
    let mode = 'sse';
    const proxy = createProxyHandler({
      cwd: dir,
      fetchImpl: async (_url, opts) => {
        const body = JSON.parse(opts.body);
        if (mode === 'sse') {
          const sse = [
            ': keepalive',
            'event: message',
            'data: {"jsonrpc":"2.0","method":"notifications/progress","params":{"progress":1}}',
            '',
            'event: message',
            `data: {"jsonrpc":"2.0",`,
            `data: "id":${JSON.stringify(body.id)},"result":{"ok":true}}`,
            '',
          ].join('\r\n');
          return new Response(sse, { status: 200, headers: { 'content-type': 'text/event-stream; charset=utf-8' } });
        }
        if (mode === 'http-500') return new Response('upstream boom', { status: 500, headers: { 'content-type': 'text/plain' } });
        if (mode === 'http-401') {
          return new Response(JSON.stringify({ jsonrpc: '2.0', id: body.id, error: { code: -32001, message: 'Unauthorized' } }), {
            status: 401,
            headers: { 'content-type': 'application/json' },
          });
        }
        if (mode === 'bad-json') return new Response('{not json', { status: 200, headers: { 'content-type': 'application/json' } });
        if (mode === 'network') throw new TypeError('fetch failed: ECONNREFUSED');
        throw new Error('unexpected');
      },
    });

    // SSE response with preceding notification + multi-line data.
    const sseReply = await proxy.handleMessage({ jsonrpc: '2.0', id: 7, method: 'tools/call' });
    assert.deepEqual(sseReply, { jsonrpc: '2.0', id: 7, result: { ok: true } });

    // Upstream HTTP 500 -> JSON-RPC -32000 error.
    mode = 'http-500';
    const err500 = await proxy.handleMessage({ jsonrpc: '2.0', id: 8, method: 'ping' });
    assert.equal(err500.jsonrpc, '2.0');
    assert.equal(err500.id, 8);
    assert.equal(err500.error.code, -32000);
    assert.match(err500.error.message, /HTTP 500.*upstream boom/);

    // Upstream HTTP 401 -> JSON-RPC -32000 error preserving detail.
    mode = 'http-401';
    const err401 = await proxy.handleMessage({ jsonrpc: '2.0', id: 9, method: 'ping' });
    assert.equal(err401.id, 9);
    assert.equal(err401.error.code, -32000);
    assert.match(err401.error.message, /HTTP 401.*Unauthorized/);

    // Unparseable upstream JSON -> JSON-RPC -32000 error.
    mode = 'bad-json';
    const errJson = await proxy.handleMessage({ jsonrpc: '2.0', id: 10, method: 'ping' });
    assert.equal(errJson.id, 10);
    assert.equal(errJson.error.code, -32000);

    // Network failure -> JSON-RPC -32000 for request, null for notification.
    mode = 'network';
    const errNet = await proxy.handleMessage({ jsonrpc: '2.0', id: 11, method: 'ping' });
    assert.equal(errNet.id, 11);
    assert.equal(errNet.error.code, -32000);
    assert.match(errNet.error.message, /ECONNREFUSED/);
    assert.equal(await proxy.handleMessage({ jsonrpc: '2.0', method: 'notifications/initialized' }), null);

    // Line parsing: blank line ignored, malformed JSON returns -32700.
    assert.equal(await proxy.handleLine('   \t '), null);
    const parseErr = await proxy.handleLine('{broken');
    assert.equal(parseErr.jsonrpc, '2.0');
    assert.equal(parseErr.id, null);
    assert.equal(parseErr.error.code, -32700);

    // Direct SSE helper check.
    assert.deepEqual(parseSseJsonRpc('data: {"jsonrpc":"2.0","id":3,"result":{"n":1}}\n\n', 3), { jsonrpc: '2.0', id: 3, result: { n: 1 } });
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test('per-workspace isolation: two workspaces send only their own Bearer tokens, never cross-leaking', async () => {
  const wsAna = makeWorkspace({
    mcpServers: { db: { type: 'http', url: 'http://127.0.0.1:9/api/ai/mcp', headers: { Authorization: 'Bearer tok-ana' } } },
  });
  const wsLuis = makeWorkspace({
    mcpServers: { db: { type: 'http', url: 'http://127.0.0.1:9/api/ai/mcp', headers: { Authorization: 'Bearer tok-luis' } } },
  });
  try {
    const auths = [];
    const fetchImpl = async (_url, opts) => {
      auths.push(opts.headers.Authorization);
      const body = JSON.parse(opts.body);
      return new Response(JSON.stringify({ jsonrpc: '2.0', id: body.id, result: { who: opts.headers.Authorization } }), {
        status: 200,
        headers: { 'content-type': 'application/json' },
      });
    };
    const pAna = createProxyHandler({ cwd: wsAna, fetchImpl });
    const pLuis = createProxyHandler({ cwd: wsLuis, fetchImpl });
    const rAna = await pAna.handleMessage({ jsonrpc: '2.0', id: 1, method: 'ping' });
    const rLuis = await pLuis.handleMessage({ jsonrpc: '2.0', id: 1, method: 'ping' });
    assert.equal(rAna.result.who, 'Bearer tok-ana');
    assert.equal(rLuis.result.who, 'Bearer tok-luis');
    assert.deepEqual(auths, ['Bearer tok-ana', 'Bearer tok-luis']);
  } finally {
    rmSync(wsAna, { recursive: true, force: true });
    rmSync(wsLuis, { recursive: true, force: true });
  }
});

test('spawned stdio round-trip against local HTTP mock server across chunked stdin, SSE, 202 notifications, and empty workspace', async () => {
  const received = [];
  const server = http.createServer((req, res) => {
    let raw = '';
    req.on('data', c => (raw += c));
    req.on('end', () => {
      const msg = JSON.parse(raw);
      received.push({ auth: req.headers.authorization, session: req.headers['mcp-session-id'], method: msg.method, id: msg.id });
      if (msg.method === 'initialize') {
        res.writeHead(200, { 'content-type': 'application/json', 'mcp-session-id': 'live-sess-9' });
        return res.end(JSON.stringify({ jsonrpc: '2.0', id: msg.id, result: { protocolVersion: '2025-06-18', capabilities: { tools: {} } } }));
      }
      if (String(msg.method).startsWith('notifications/')) {
        res.writeHead(202);
        return res.end();
      }
      if (msg.method === 'tools/list') {
        res.writeHead(200, { 'content-type': 'text/event-stream' });
        return res.end(`event: message\ndata: ${JSON.stringify({ jsonrpc: '2.0', id: msg.id, result: { tools: [{ name: 'query' }] } })}\n\n`);
      }
      if (msg.method === 'boom') {
        res.writeHead(502, { 'content-type': 'text/plain' });
        return res.end('bad gateway');
      }
      res.writeHead(200, { 'content-type': 'application/json' });
      res.end(JSON.stringify({ jsonrpc: '2.0', id: msg.id, result: { echo: msg.params ?? null } }));
    });
  });

  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  const { port } = server.address();
  const wsConfigured = makeWorkspace({
    mcpServers: {
      db: { type: 'http', url: `http://127.0.0.1:${port}/api/ai/mcp`, headers: { Authorization: 'Bearer tok-roundtrip' } },
    },
  });
  const wsEmpty = makeWorkspace(undefined);

  const runProxy = (cwd, chunks) =>
    new Promise((resolve, reject) => {
      const child = spawn(process.execPath, [proxyScript], { cwd, stdio: ['pipe', 'pipe', 'pipe'] });
      let stdout = '';
      let stderr = '';
      child.stdout.on('data', d => (stdout += d));
      child.stderr.on('data', d => (stderr += d));
      child.on('error', reject);
      child.on('close', code => resolve({ code, stdout, stderr }));
      (async () => {
        for (const chunk of chunks) {
          child.stdin.write(chunk);
          await new Promise(r => setTimeout(r, 5));
        }
        child.stdin.end();
      })();
    });

  try {
    const initLine = JSON.stringify({ jsonrpc: '2.0', id: 1, method: 'initialize', params: { protocolVersion: '2025-06-18' } });
    const res1 = await runProxy(wsConfigured, [
      // Split a single JSON line across two chunks, plus blank line & malformed JSON.
      initLine.slice(0, 20),
      initLine.slice(20) + '\n\n',
      '{not-json\n',
      JSON.stringify({ jsonrpc: '2.0', method: 'notifications/initialized' }) + '\n',
      JSON.stringify({ jsonrpc: '2.0', id: 2, method: 'tools/list' }) + '\n',
      JSON.stringify({ jsonrpc: '2.0', id: 3, method: 'boom' }) + '\n',
      JSON.stringify({ jsonrpc: '2.0', id: 4, method: 'ping', params: { ok: 1 } }), // no trailing newline
    ]);
    assert.equal(res1.code, 0, res1.stderr);
    assert.equal(res1.stderr, '');
    const lines = res1.stdout.trim().split('\n').map(l => JSON.parse(l));
    assert.equal(lines.length, 5, 'notification emits nothing, 5 responses emitted');
    assert.equal(lines[0].id, 1);
    assert.equal(lines[0].result.protocolVersion, '2025-06-18');
    assert.equal(lines[1].error.code, -32700);
    assert.equal(lines[2].id, 2);
    assert.deepEqual(lines[2].result.tools, [{ name: 'query' }]);
    assert.equal(lines[3].id, 3);
    assert.equal(lines[3].error.code, -32000);
    assert.match(lines[3].error.message, /HTTP 502/);
    assert.equal(lines[4].id, 4);
    assert.deepEqual(lines[4].result, { echo: { ok: 1 } });

    assert.equal(received.length, 5);
    assert.ok(received.every(r => r.auth === 'Bearer tok-roundtrip'));
    assert.equal(received[0].session, undefined);
    assert.deepEqual(received.slice(1).map(r => r.session), ['live-sess-9', 'live-sess-9', 'live-sess-9', 'live-sess-9']);

    // Unconfigured workspace served locally with zero HTTP requests to the mock server.
    const beforeCount = received.length;
    const res2 = await runProxy(wsEmpty, [
      JSON.stringify({ jsonrpc: '2.0', id: 1, method: 'initialize', params: {} }) + '\n',
      JSON.stringify({ jsonrpc: '2.0', method: 'notifications/initialized' }) + '\n',
      JSON.stringify({ jsonrpc: '2.0', id: 2, method: 'tools/list' }) + '\n',
    ]);
    assert.equal(res2.code, 0, res2.stderr);
    const emptyLines = res2.stdout.trim().split('\n').map(l => JSON.parse(l));
    assert.equal(emptyLines.length, 2);
    assert.deepEqual(emptyLines[1], { jsonrpc: '2.0', id: 2, result: { tools: [] } });
    assert.equal(received.length, beforeCount, 'empty workspace made zero upstream requests');
  } finally {
    server.close();
    rmSync(wsConfigured, { recursive: true, force: true });
    rmSync(wsEmpty, { recursive: true, force: true });
  }
});
