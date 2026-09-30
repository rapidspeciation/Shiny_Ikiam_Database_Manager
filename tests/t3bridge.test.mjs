import test from 'node:test';
import assert from 'node:assert/strict';
import http from 'node:http';
import vm from 'node:vm';
import { createHash } from 'node:crypto';
import { gzipSync } from 'node:zlib';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createApp } from '../server/index.mjs';
import {
  BRIDGE_PATH,
  bridgeScript,
  bridgeTag,
  canInject,
  createT3Bridge,
  injectBridge,
  isPageLoad,
} from '../server/t3bridge.mjs';

// The T3 bridge: T3's page gets a script that tells the Asistente tab which chat it shows.

const APP = 'https://app.example.org';
const ENV = '4e6c4765-8cfa-4adc-b761-3c3bae2ae7e0';
const A = '5c8de89d-bbaf-4328-b354-74733e099781';
const PAGE =
  '<!doctype html><html><head><meta charset="UTF-8" /><script>theme()</script><script type="module" src="/assets/index.js"></script></head><body></body></html>';

test('the tag goes before the first script of <head>, or at its end; once', () => {
  const tag = bridgeTag(APP);
  assert.equal(tag, `<script src="${BRIDGE_PATH}" data-app-origin="${APP}"></script>`);
  const out = injectBridge(PAGE, APP);
  assert.ok(out.indexOf(tag) < out.indexOf('<script>theme()'), 'before T3 sets up its router');
  assert.equal(out.replace(`${tag}\n`, ''), PAGE, 'nothing else changes');
  assert.equal(injectBridge(out, APP), out, 'not twice');
  const noScript = '<html><head><title>T3</title></head><body><script src="/x.js"></script></body></html>';
  assert.equal(
    injectBridge(noScript, APP),
    `<html><head><title>T3</title>${tag}\n</head><body><script src="/x.js"></script></body></html>`,
  );
  assert.equal(injectBridge('<p>no head</p>', APP), '<p>no head</p>');
  assert.equal(injectBridge(PAGE, ''), PAGE, 'no app origin, no script');
  assert.ok(bridgeTag('https://a"><script>').includes('data-app-origin="https://a&quot;>&lt;script>"'));
});

test('which requests and answers get it', () => {
  const req = (url, accept = 'text/html,application/xhtml+xml', method = 'GET') => ({
    url,
    method,
    headers: { accept },
  });
  assert.equal(isPageLoad(req('/')), true);
  assert.equal(isPageLoad(req(`/${ENV}/${A}?x=1`)), true);
  assert.equal(isPageLoad(req('/pair')), true);
  assert.equal(isPageLoad(req('/api/auth/session')), false);
  assert.equal(isPageLoad(req('/assets/index.js')), false);
  assert.equal(isPageLoad(req('/', '*/*')), false);
  assert.equal(isPageLoad(req('/', 'text/html', 'POST')), false);
  assert.equal(isPageLoad(req(BRIDGE_PATH)), false);
  assert.equal(canInject(200, { 'content-type': 'text/html; charset=utf-8' }), true);
  assert.equal(canInject(200, { 'content-type': 'text/html', 'content-security-policy': "script-src 'self'" }), false);
  assert.equal(canInject(200, { 'content-type': 'text/html', 'content-encoding': 'br' }), false);
  assert.equal(canInject(200, { 'content-type': 'application/json' }), false);
  assert.equal(canInject(304, { 'content-type': 'text/html' }), false);
});

/** Runs the bridge in a fake T3 page: its posts, and a way to navigate and to say hello. */
function runBridge({ framed = true, path = '/', appOrigin = APP } = {}) {
  const posts = [];
  const listeners = {};
  // Copied out of the script's context (its objects have another Object.prototype).
  const parent = { postMessage: (data, origin) => posts.push({ data: JSON.parse(JSON.stringify(data)), origin }) };
  const window = {
    location: { pathname: path },
    history: {
      pushState(_s, _t, url) {
        window.location.pathname = url;
      },
      replaceState(_s, _t, url) {
        window.location.pathname = url;
      },
    },
    document: {
      currentScript: { dataset: { appOrigin } },
      visibilityState: 'visible',
      hasFocus: () => true,
      addEventListener() {},
    },
    addEventListener: (name, fn) => (listeners[name] ??= []).push(fn),
    setInterval: () => 0,
    queueMicrotask: fn => fn(),
  };
  window.window = window;
  window.parent = framed ? parent : window;
  vm.runInNewContext(bridgeScript, window);
  const say = (name, event) => (listeners[name] ?? []).forEach(fn => fn(event));
  return { posts, window, parent, say };
}

test('the script tells the app (only) which chat the frame shows', () => {
  const { posts, window, parent, say } = runBridge({ path: `/${ENV}/${A}` });
  assert.deepEqual(posts, [
    {
      origin: APP,
      data: {
        type: 'ithomiini-t3',
        v: 1,
        path: `/${ENV}/${A}`,
        environmentId: ENV,
        threadId: A,
        draftId: null,
        visible: true,
        focused: true,
      },
    },
  ]);
  window.history.pushState({}, '', '/draft/abc123');
  assert.deepEqual([posts.at(-1).data.threadId, posts.at(-1).data.draftId], [null, 'abc123']);
  window.history.replaceState({}, '', '/draft/abc123');
  assert.equal(posts.length, 2, 'the same place again: nothing sent');
  window.history.pushState({}, '', '/settings/general');
  assert.deepEqual([posts.at(-1).data.threadId, posts.at(-1).data.draftId], [null, null]);
  // Hello: answered only when it comes from the app's page itself.
  say('message', { origin: 'https://evil.example', source: parent, data: { type: 'ithomiini-t3-hello' } });
  say('message', { origin: APP, source: {}, data: { type: 'ithomiini-t3-hello' } });
  assert.equal(posts.length, 3);
  say('message', { origin: APP, source: parent, data: { type: 'ithomiini-t3-hello' } });
  assert.equal(posts.length, 4);
  assert.ok(posts.every(p => p.origin === APP));
  // A T3 tab of its own (not in a frame), or no app origin: silent.
  assert.deepEqual(runBridge({ framed: false }).posts, []);
  assert.deepEqual(runBridge({ appOrigin: '' }).posts, []);
});

/** A tiny T3: a page, a compressed script, an API echo, a page with its own CSP, and a websocket that echoes. */
async function fakeT3() {
  const seen = [];
  const server = http.createServer((req, res) => {
    seen.push({ url: req.url, headers: req.headers });
    if (req.url === '/assets/index.js') {
      res.writeHead(200, { 'content-type': 'text/javascript', 'content-encoding': 'gzip' });
      return res.end(gzipSync('console.log(1)'));
    }
    if (req.url.startsWith('/api/')) {
      let body = '';
      req
        .on('data', c => (body += c))
        .on('end', () => {
          res.writeHead(201, { 'content-type': 'application/json', 'set-cookie': ['a=1; Path=/', 'b=2; Path=/'] });
          res.end(JSON.stringify({ method: req.method, body, cookie: req.headers.cookie ?? null }));
        });
      return;
    }
    const headers = {
      'content-type': 'text/html; charset=utf-8',
      etag: '"t3"',
      'cache-control': 'no-cache',
      'set-cookie': ['s=1; Path=/'],
    };
    if (req.url === '/csp') headers['content-security-policy'] = "script-src 'self'";
    res.writeHead(200, headers);
    res.end(PAGE);
  });
  server.on('upgrade', (req, socket) => {
    const accept = createHash('sha1')
      .update(`${req.headers['sec-websocket-key']}258EAFA5-E914-47DA-95CA-C5AB0DC85B11`)
      .digest('base64');
    socket.write(
      `HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: ${accept}\r\n\r\n`,
    );
    const frame = text => Buffer.concat([Buffer.from([0x81, Buffer.byteLength(text)]), Buffer.from(text)]);
    socket.write(frame(`hello ${req.url}`));
    socket.on('data', data => {
      // One small masked text frame from the browser.
      if ((data[0] & 0x0f) !== 1) return socket.end();
      const length = data[1] & 0x7f;
      const mask = data.subarray(2, 6);
      const text = Buffer.from(data.subarray(6, 6 + length).map((b, i) => b ^ mask[i % 4])).toString();
      socket.write(frame(`echo ${text}`));
    });
    socket.on('error', () => {});
  });
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  return { server, seen, local: `http://127.0.0.1:${server.address().port}` };
}

/** A request with the headers given (Host included), its answer as text. */
function request(port, path, headers = {}, { method = 'GET', body } = {}) {
  return new Promise((resolve, reject) => {
    const req = http.request({ host: '127.0.0.1', port, path, method, headers }, res => {
      const chunks = [];
      res
        .on('data', c => chunks.push(c))
        .on('end', () => resolve({ status: res.statusCode, headers: res.headers, body: Buffer.concat(chunks) }));
    });
    req.on('error', reject);
    req.end(body);
  });
}

test('the full proxy (the lab): all of T3 passes, its pages with the tag, websockets too', async () => {
  const t3 = await fakeT3();
  const bridge = createT3Bridge({ t3: { url: 'https://t3.example.org:8510', local: t3.local }, appOrigin: `${APP}/` });
  const proxy = await bridge.listen(0);
  const { port } = proxy.address();
  try {
    const page = await request(port, `/${ENV}/${A}`, {
      accept: 'text/html',
      'accept-encoding': 'gzip, br',
      'if-none-match': '"t3"',
    });
    assert.equal(page.status, 200);
    assert.ok(page.body.toString().includes(bridgeTag(APP)), 'the app origin, without its trailing slash');
    assert.equal(page.headers.etag, undefined);
    assert.equal(Number(page.headers['content-length']), page.body.length);
    assert.deepEqual(page.headers['set-cookie'], ['s=1; Path=/']);
    const asked = t3.seen.at(-1).headers;
    assert.equal(asked['accept-encoding'], 'identity', 'asked uncompressed');
    assert.equal(asked['if-none-match'], undefined, 'never "not modified" to a page without the tag');
    // T3's own CSP: left alone.
    assert.ok(!(await request(port, '/csp', { accept: 'text/html' })).body.toString().includes(BRIDGE_PATH));
    // Other files and the API pass as they are (compressed, cookies both ways, bodies).
    const script = await request(port, '/assets/index.js', { accept: '*/*', 'accept-encoding': 'gzip' });
    assert.equal(script.headers['content-encoding'], 'gzip');
    const api = await request(
      port,
      '/api/auth/x',
      { cookie: 't3=abc', 'content-type': 'application/json' },
      { method: 'POST', body: '{"a":1}' },
    );
    assert.equal(api.status, 201);
    assert.deepEqual(JSON.parse(api.body), { method: 'POST', body: '{"a":1}', cookie: 't3=abc' });
    assert.deepEqual(api.headers['set-cookie'], ['a=1; Path=/', 'b=2; Path=/']);
    // The script itself, cacheable for a few minutes.
    const script2 = await request(port, BRIDGE_PATH);
    assert.equal(script2.headers['cache-control'], 'public, max-age=300');
    assert.equal(script2.body.toString(), bridgeScript);

    // A websocket through the proxy.
    const ws = new WebSocket(`ws://127.0.0.1:${port}/ws?x=1`);
    const got = [];
    await new Promise((resolve, reject) => {
      ws.onmessage = e => {
        got.push(e.data);
        if (got.length === 1) ws.send('ping');
        else resolve();
      };
      ws.onerror = () => reject(new Error('websocket failed'));
    });
    assert.deepEqual(got, ['hello /ws?x=1', 'echo ping']);
    ws.close();

    // T3 down (restarting): a page that says so and tries again; other requests a plain 502.
    await new Promise(resolve => {
      t3.server.closeAllConnections();
      t3.server.close(resolve);
    });
    const down = await request(port, '/', { accept: 'text/html' });
    assert.equal(down.status, 502);
    assert.match(
      down.body.toString(),
      /T3 Code is not answering[\s\S]*http-equiv="refresh"|http-equiv="refresh"[\s\S]*T3 Code is not answering/,
    );
    assert.equal((await request(port, '/api/x')).status, 502);
    const refused = new WebSocket(`ws://127.0.0.1:${port}/ws`);
    await new Promise(resolve => (refused.onerror = resolve));
  } finally {
    proxy.closeAllConnections();
    await new Promise(resolve => proxy.close(resolve));
    bridge.close();
    t3.server.close();
  }
});

test("production: the app answers T3's page loads for T3's host (Caddy sends them), its own pages as before", async () => {
  const t3 = await fakeT3();
  const store = new Store({ localMode: true }, { sheets: new LocalSheets({}) });
  const app = await createApp(
    {
      localMode: true,
      secureCookies: false,
      syncIntervalMs: 0,
      publicUrl: APP,
      t3: { url: 'https://t3.example.org', local: t3.local },
    },
    { store, skipInitialSync: true },
  );
  const { port } = await app.listen(0, '127.0.0.1');
  try {
    const page = await request(port, `/${ENV}/${A}`, { host: 't3.example.org', accept: 'text/html' });
    assert.equal(page.status, 200);
    assert.ok(page.body.toString().includes(bridgeTag(APP)));
    assert.equal((await request(port, BRIDGE_PATH, { host: 'T3.example.org' })).body.toString(), bridgeScript);
    // The app's own host: its health, not T3.
    const health = await request(port, '/health', { host: 'app.example.org' });
    assert.equal(JSON.parse(health.body).status, 'ok');
    assert.equal(t3.seen.length, 1);
  } finally {
    await app.close();
    t3.server.close();
  }
});
