import test from 'node:test';
import assert from 'node:assert/strict';
import http from 'node:http';
import { PassThrough } from 'node:stream';
import { spawn } from 'node:child_process';
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { createProxyHandler, parseSseJsonRpc, readWorkspaceMcp, runStdio } from '../scripts/mcp-stdio-proxy.mjs';

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
      // Client responses (no method) are silently ignored instead of fabricating an error.
      assert.equal(await proxy.handleMessage({ jsonrpc: '2.0', id: 99, result: { roots: [] } }), null);

      const unknown = await proxy.handleMessage({ jsonrpc: '2.0', id: 5, method: 'tools/call', params: { name: 'query' } });
      assert.equal(unknown.id, 5);
      assert.equal(unknown.error.code, -32601);
      assert.equal(fetched, 0);
    }
  } finally {
    for (const d of [emptyDir, stdioOnlyDir, unrelatedDir]) rmSync(d, { recursive: true, force: true });
  }
});

test('HTTP JSON forwarding: sends Bearer header, tracks mcp-session-id, falls back from non-HTTP db to HTTP ithomiini, and handles 202 notifications silently', async () => {
  const dir = makeWorkspace({
    mcpServers: {
      db: { command: 'node', args: ['stdio-only.mjs'] },
      ithomiini: {
        type: 'http',
        url: 'http://127.0.0.1:9/api/ai/mcp',
        headers: { Authorization: 'Bearer tok-legacy', 'Content-Type': 'text/plain', Accept: 'text/plain', 'mcp-session-id': 'spoofed' },
      },
    },
  });
  try {
    const seen = [];
    const proxy = createProxyHandler({
      cwd: dir,
      fetchImpl: async (url, opts) => {
        seen.push({ url, method: opts.method, headers: opts.headers, body: JSON.parse(opts.body), hasSignal: Boolean(opts.signal) });
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
    assert.equal(seen[0].headers['Content-Type'], 'application/json', 'protocol Content-Type wins over config headers');
    assert.equal(seen[0].headers.Accept, 'application/json, text/event-stream', 'protocol Accept wins over config headers');
    assert.equal(seen[0].hasSignal, true);

    // Client response (method-less message with id) is a silent no-op and does not call fetch.
    assert.equal(await proxy.handleMessage({ jsonrpc: '2.0', id: 77, result: {} }), null);
    assert.equal(await proxy.handleMessage({ jsonrpc: '2.0', id: 78, error: { code: -32601, message: 'no' } }), null);
    assert.equal(seen.length, 1);

    // Notification: forwarded with session id, returns null on 202 empty body.
    assert.equal(await proxy.handleMessage({ jsonrpc: '2.0', method: 'notifications/initialized' }), null);
    assert.equal(seen[1].headers['mcp-session-id'], 'sess-42', 'tracked session id wins over config headers');
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

test('SSE responses, upstream array / notification-only SSE / HTTP / timeout / network errors, and malformed stdin lines return valid JSON-RPC errors', async () => {
  const dir = makeWorkspace({
    mcpServers: {
      db: { type: 'http', url: 'http://127.0.0.1:9/api/ai/mcp', headers: { Authorization: 'Bearer tok-1' } },
    },
  });
  try {
    let mode = 'sse';
    const proxy = createProxyHandler({
      cwd: dir,
      timeoutMs: 40,
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
        if (mode === 'sse-notif-only') {
          const sse = [
            'event: message',
            'data: {"jsonrpc":"2.0","method":"notifications/progress","params":{"progress":1}}',
            '',
            'event: message',
            `data: {"jsonrpc":"2.0","id":${JSON.stringify(body.id)},"method":"sampling/createMessage"}`,
            '',
          ].join('\n');
          return new Response(sse, { status: 200, headers: { 'content-type': 'text/event-stream' } });
        }
        if (mode === 'array') {
          return new Response(JSON.stringify([{ jsonrpc: '2.0', id: body.id, result: {} }]), {
            status: 200,
            headers: { 'content-type': 'application/json' },
          });
        }
        if (mode === 'timeout') {
          return await new Promise((_, reject) => {
            opts.signal.addEventListener('abort', () => reject(opts.signal.reason), { once: true });
          });
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

    // Notification-only / request-only SSE stream -> JSON-RPC -32000 error.
    mode = 'sse-notif-only';
    const errSse = await proxy.handleMessage({ jsonrpc: '2.0', id: 70, method: 'tools/call' });
    assert.equal(errSse.id, 70);
    assert.equal(errSse.error.code, -32000);
    assert.match(errSse.error.message, /SSE stream contained no JSON-RPC response/);

    // Upstream top-level JSON array -> JSON-RPC -32000 error.
    mode = 'array';
    const errArr = await proxy.handleMessage({ jsonrpc: '2.0', id: 71, method: 'ping' });
    assert.equal(errArr.id, 71);
    assert.equal(errArr.error.code, -32000);
    assert.match(errArr.error.message, /non-object JSON/);

    // Upstream timeout -> JSON-RPC -32000 error (never hangs).
    mode = 'timeout';
    const errTimeout = await proxy.handleMessage({ jsonrpc: '2.0', id: 72, method: 'tools/call' });
    assert.equal(errTimeout.id, 72);
    assert.equal(errTimeout.error.code, -32000);
    assert.match(errTimeout.error.message, /timeout|abort/i);

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

    // Direct SSE helper checks.
    assert.deepEqual(parseSseJsonRpc('data: {"jsonrpc":"2.0","id":3,"result":{"n":1}}\n\n', 3), { jsonrpc: '2.0', id: 3, result: { n: 1 } });
    assert.throws(() => parseSseJsonRpc('data: {"jsonrpc":"2.0","method":"notifications/log"}\n\n', 3), /SSE stream contained no JSON-RPC response/);
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test('per-workspace isolation and strict non-traversal: two workspaces send only their own Bearer tokens, and subdirectories do not inherit parent .mcp.json', async () => {
  const wsAna = makeWorkspace({
    mcpServers: { db: { type: 'http', url: 'http://127.0.0.1:9/api/ai/mcp', headers: { Authorization: 'Bearer tok-ana' } } },
  });
  const wsLuis = makeWorkspace({
    mcpServers: { db: { type: 'http', url: 'http://127.0.0.1:9/api/ai/mcp', headers: { Authorization: 'Bearer tok-luis' } } },
  });
  const subdir = join(wsAna, 'subdir');
  mkdirSync(subdir, { recursive: true });
  try {
    assert.equal(readWorkspaceMcp(subdir), null, 'strict non-traversal: subdirectory does not inherit parent .mcp.json');
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

test('concurrent post-initialize dispatch: slow tools/call does not block a concurrent ping while initialize still gates sessionId', async () => {
  const dir = makeWorkspace({
    mcpServers: { db: { type: 'http', url: 'http://127.0.0.1:9/api/ai/mcp', headers: { Authorization: 'Bearer tok-conc' } } },
  });
  try {
    let releaseSlow;
    const slowStarted = new Promise(resolve => {
      releaseSlow = resolve;
    });
    let resolveSlowFetch;
    const seenSessions = {};
    const fetchImpl = async (_url, opts) => {
      const body = JSON.parse(opts.body);
      seenSessions[body.id] = opts.headers['mcp-session-id'];
      if (body.method === 'initialize') {
        await new Promise(r => setTimeout(r, 20));
        return new Response(JSON.stringify({ jsonrpc: '2.0', id: body.id, result: { protocolVersion: '2025-06-18' } }), {
          status: 200,
          headers: { 'content-type': 'application/json', 'mcp-session-id': 'sess-conc' },
        });
      }
      if (body.method === 'tools/call') {
        releaseSlow();
        await new Promise(resolve => {
          resolveSlowFetch = resolve;
        });
        return new Response(JSON.stringify({ jsonrpc: '2.0', id: body.id, result: { slow: true } }), {
          status: 200,
          headers: { 'content-type': 'application/json' },
        });
      }
      return new Response(JSON.stringify({ jsonrpc: '2.0', id: body.id, result: { fast: true } }), {
        status: 200,
        headers: { 'content-type': 'application/json' },
      });
    };

    const stdin = new PassThrough();
    const stdout = new PassThrough();
    const replies = [];
    let pingArrived;
    const gotPing = new Promise(resolve => {
      pingArrived = resolve;
    });
    let outBuf = '';
    stdout.setEncoding('utf8');
    stdout.on('data', chunk => {
      outBuf += chunk;
      let idx;
      while ((idx = outBuf.indexOf('\n')) !== -1) {
        const msg = JSON.parse(outBuf.slice(0, idx));
        outBuf = outBuf.slice(idx + 1);
        replies.push(msg);
        if (msg.id === 3) pingArrived();
      }
    });

    const done = runStdio({ cwd: dir, fetchImpl, stdin, stdout });
    stdin.write(
      [
        JSON.stringify({ jsonrpc: '2.0', id: 1, method: 'initialize', params: {} }),
        JSON.stringify({ jsonrpc: '2.0', id: 2, method: 'tools/call', params: { name: 'slow' } }),
        JSON.stringify({ jsonrpc: '2.0', id: 3, method: 'ping' }),
      ].join('\n') + '\n',
    );
    await slowStarted;
    await gotPing;
    // Ping (id: 3) completed while slow tools/call (id: 2) is still blocked!
    assert.deepEqual(replies.map(r => r.id), [1, 3]);
    // Close stdin while slow tools/call is still in flight; runStdio must wait for it before resolving.
    stdin.end();
    resolveSlowFetch();
    await done;
    assert.deepEqual(replies.map(r => r.id), [1, 3, 2]);
    assert.equal(seenSessions[1], undefined);
    assert.equal(seenSessions[2], 'sess-conc');
    assert.equal(seenSessions[3], 'sess-conc');
  } finally {
    rmSync(dir, { recursive: true, force: true });
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
      JSON.stringify({ jsonrpc: '2.0', id: 99, result: {} }) + '\n', // client response: no reply
      JSON.stringify({ jsonrpc: '2.0', id: 2, method: 'tools/list' }) + '\n',
      JSON.stringify({ jsonrpc: '2.0', id: 3, method: 'boom' }) + '\n',
      JSON.stringify({ jsonrpc: '2.0', id: 4, method: 'ping', params: { ok: 1 } }), // no trailing newline
    ]);
    assert.equal(res1.code, 0, res1.stderr);
    assert.equal(res1.stderr, '');
    const lines = res1.stdout.trim().split('\n').map(l => JSON.parse(l));
    assert.equal(lines.length, 5, 'notification and client response emit nothing, 5 responses emitted');
    const byId = new Map(lines.map(l => [l.id, l]));
    assert.equal(byId.get(1).result.protocolVersion, '2025-06-18');
    assert.equal(byId.get(null).error.code, -32700);
    assert.deepEqual(byId.get(2).result.tools, [{ name: 'query' }]);
    assert.equal(byId.get(3).error.code, -32000);
    assert.match(byId.get(3).error.message, /HTTP 502/);
    assert.deepEqual(byId.get(4).result, { echo: { ok: 1 } });

    assert.equal(received.length, 5);
    assert.ok(received.every(r => r.auth === 'Bearer tok-roundtrip'));
    const initReq = received.find(r => r.method === 'initialize');
    assert.equal(initReq.session, undefined);
    for (const r of received.filter(x => x.method !== 'initialize')) assert.equal(r.session, 'live-sess-9');

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
