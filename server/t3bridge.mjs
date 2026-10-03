// The T3 bridge: tells the Asistente tab which chat its T3 Code frame shows.
// T3 is a stock install on its own address, with no embedding API; its page has
// no Content-Security-Policy and no service worker. So the page is fetched from
// T3 and a small script is added to it (bridgeScript): inside a frame, it posts
// the chat on screen ({ type: 'ithomiini-t3', path, environmentId, threadId,
// draftId, visible, focused }) to the app's origin only, on every navigation and
// when the app says hello. A T3 page open in its own browser tab sends nothing.
// Both ways, for links: the app asks it to open a chat ({ type: 'ithomiini-t3-open',
// path }, in T3's own router, no reload), and a click in T3 on a link to the app's
// Asistente tab (a proposal the assistant gives) goes to the app's page instead of
// a new tab ({ type: 'ithomiini-t3-link', hash }); without the app's answer the
// link opens in a new tab as before.
// - Production (Caddy): requests to T3's host reach this app only for page loads
//   and /__ithomiini/bridge.js (deploy/Caddyfile.fragment); T3's API, assets and
//   websockets go from Caddy straight to T3, and so does everything if the app is down.
// - Full proxy (the lab, ITHOMIINI_T3_PROXY_PORT): a listener of its own proxies
//   all of T3 (HTTP and websockets) and adds the script to its page loads.
// Without the script (an old route, a T3 update that broke it) the app guesses
// the open chat from T3's trace log as before (server/t3chats.mjs).
import http from 'node:http';

export const BRIDGE_PATH = '/__ithomiini/bridge.js';

/* The script added to T3's page. Runs in T3's page: nothing from this module in scope. */
function bridge() {
  if (window.parent === window) return;
  const app = document.currentScript && document.currentScript.dataset.appOrigin;
  if (!app) return;
  const UUID = '[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}';
  // A chat is /<environmentId>/<threadId>; a new chat is /draft/<draftId> until its first message.
  const CHAT = new RegExp(`^/(${UUID})/(${UUID})(?:/|$)`, 'i');
  const DRAFT = /^\/draft\/([^/]+)/;
  let last = '';
  const state = () => {
    const path = location.pathname;
    const chat = CHAT.exec(path);
    return {
      type: 'ithomiini-t3',
      v: 1,
      path,
      environmentId: chat ? chat[1] : null,
      threadId: chat ? chat[2] : null,
      draftId: (DRAFT.exec(path) || [])[1] || null,
      visible: document.visibilityState === 'visible',
      focused: document.hasFocus(),
    };
  };
  const send = force => {
    const now = state();
    const key = JSON.stringify(now);
    if (!force && key === last) return;
    last = key;
    try {
      window.parent.postMessage(now, app);
    } catch {
      /* the frame's parent went away */
    }
  };
  // T3's router moves with history.pushState / replaceState (and back / forward).
  for (const name of ['pushState', 'replaceState']) {
    const original = history[name];
    history[name] = function (...args) {
      const out = original.apply(this, args);
      queueMicrotask(() => send());
      return out;
    };
  }
  for (const event of ['popstate', 'hashchange', 'focus', 'blur']) addEventListener(event, () => send());
  document.addEventListener('visibilitychange', () => send());
  if (window.navigation && navigation.addEventListener) navigation.addEventListener('navigatesuccess', () => send());
  // A link the app opened (#/asistente?chat=…): T3's router moves to the chat as after a back/forward.
  const open = path => {
    if (!CHAT.test(path) || location.pathname === path) return;
    const index = (history.state && history.state.__TSR_index) || 0;
    const key = Math.random().toString(36).slice(2, 10);
    history.pushState({ __TSR_index: index + 1, key, __TSR_key: key }, '', path);
    dispatchEvent(new PopStateEvent('popstate', { state: history.state }));
  };
  let unanswered;
  addEventListener('message', e => {
    if (e.origin !== app || e.source !== window.parent || !e.data) return;
    // The app asks again when its panel starts listening (the frame loaded before it).
    if (e.data.type === 'ithomiini-t3-hello') send(true);
    if (e.data.type === 'ithomiini-t3-open' && typeof e.data.path === 'string') open(e.data.path);
    if (e.data.type === 'ithomiini-t3-link-ok') clearTimeout(unanswered);
  });
  // A link to the app's Asistente tab: shown beside this frame, not in a new tab.
  document.addEventListener(
    'click',
    e => {
      if (e.defaultPrevented || e.button !== 0 || e.metaKey || e.ctrlKey || e.shiftKey || e.altKey) return;
      const a = e.target && e.target.closest ? e.target.closest('a[href]') : null;
      if (!a) return;
      let url;
      try {
        url = new URL(a.href, location.href);
      } catch {
        return;
      }
      if (url.origin !== app || !/^#\/asistente(?:[?/]|$)/.test(url.hash)) return;
      e.preventDefault();
      e.stopPropagation();
      clearTimeout(unanswered);
      unanswered = setTimeout(() => window.open(url.href, '_blank', 'noopener'), 400);
      try {
        window.parent.postMessage({ type: 'ithomiini-t3-link', v: 1, hash: url.hash.slice(0, 1000) }, app);
      } catch {
        /* the fallback opens it */
      }
    },
    true,
  );
  // In case a later T3 moves some other way.
  setInterval(() => send(), 1000);
  send(true);
}
export const bridgeScript = `// Ithomiini: tells the app embedding T3 Code which chat is on screen (server/t3bridge.mjs).\n(${bridge})();\n`;

const escapeAttribute = text => String(text).replace(/&/g, '&amp;').replace(/"/g, '&quot;').replace(/</g, '&lt;');
export const bridgeTag = appOrigin =>
  `<script src="${BRIDGE_PATH}" data-app-origin="${escapeAttribute(appOrigin)}"></script>`;

/**
 * T3's page with the bridge's script tag, before its first script in <head>
 * (so it sees T3's router start), or at the end of <head>. Unchanged without a
 * <head>, without an app origin, or when the tag is there already. Pure, for tests.
 */
export function injectBridge(html, appOrigin) {
  if (!appOrigin || html.includes(BRIDGE_PATH)) return html;
  const end = html.search(/<\/head\s*>/i);
  if (end < 0) return html;
  const script = html.slice(0, end).search(/<script\b/i);
  const at = script >= 0 ? script : end;
  return `${html.slice(0, at)}${bridgeTag(appOrigin)}\n${html.slice(at)}`;
}

/** A browser loading a page of T3 (not its API, built files or the bridge itself). */
export function isPageLoad(req) {
  const path = String(req.url ?? '').split('?')[0];
  return (
    req.method === 'GET' &&
    /text\/html/i.test(String(req.headers.accept ?? '')) &&
    !/^\/(?:api|assets)(?:\/|$)/.test(path) &&
    path !== BRIDGE_PATH
  );
}

/** Whether T3's answer can take the script: an HTML page, not compressed, without a CSP of its own. */
export function canInject(statusCode, headers) {
  return (
    statusCode === 200 &&
    /^text\/html/i.test(String(headers['content-type'] ?? '')) &&
    !headers['content-security-policy'] &&
    (!headers['content-encoding'] || headers['content-encoding'] === 'identity')
  );
}

// Connection-level headers, not passed on (RFC 9110 §7.6.1).
const HOP = new Set(['connection', 'keep-alive', 'proxy-connection', 'transfer-encoding', 'te', 'trailer', 'upgrade']);
const pass = headers => Object.fromEntries(Object.entries(headers).filter(([name]) => !HOP.has(name)));

/** The page shown in the frame when T3 does not answer (restarting, updating); it tries again by itself. */
const unavailablePage = `<!doctype html><meta charset="utf-8"><meta http-equiv="refresh" content="5">
<title>T3 Code</title><body style="font:15px system-ui,sans-serif;color:#44403c;padding:2rem">
<p>T3 Code is not answering (it may be restarting). This page tries again in a few seconds.</p>
<p lang="es">T3 Code no responde (puede que se esté reiniciando). Esta página vuelve a intentarlo en unos segundos.</p>`;

/**
 * The bridge for one T3: `t3` is config.t3 ({ url: its public address, local:
 * where this server reaches it }), `appOrigin` the app's public origin (the
 * only one the script talks to; no script without it).
 */
export function createT3Bridge({ t3, appOrigin }) {
  const local = new URL(t3.local);
  const upstream = { host: local.hostname, port: Number(local.port) || 80 };
  const publicHost = new URL(t3.url).host.toLowerCase();
  const origin = appOrigin ? new URL(appOrigin).origin : '';
  const agent = new http.Agent({ keepAlive: true, maxSockets: 64 });

  /** Whether a request to the app's server is for T3's host (production: Caddy sends T3's page loads here). */
  const owns = req => String(req.headers.host ?? '').toLowerCase() === publicHost;

  function fail(req, res, page) {
    if (res.headersSent) return void res.destroy();
    res.writeHead(502, {
      'content-type': page ? 'text/html; charset=utf-8' : 'text/plain; charset=utf-8',
      'cache-control': 'no-store',
    });
    res.end(page ? unavailablePage : 'T3 Code is not answering');
  }

  /** One HTTP request, passed to T3; page loads get the script. */
  function handle(req, res) {
    if (String(req.url ?? '').split('?')[0] === BRIDGE_PATH) {
      res.writeHead(200, {
        'content-type': 'text/javascript; charset=utf-8',
        'cache-control': 'public, max-age=300',
        'x-content-type-options': 'nosniff',
      });
      return void res.end(req.method === 'HEAD' ? undefined : bridgeScript);
    }
    const page = !!origin && isPageLoad(req);
    const headers = pass(req.headers);
    if (page) {
      // Plain text to add the tag to, and never "not modified" (the browser may hold T3's page without it).
      headers['accept-encoding'] = 'identity';
      delete headers['if-none-match'];
      delete headers['if-modified-since'];
    }
    const up = http.request({ ...upstream, agent, method: req.method, path: req.url, headers }, answer => {
      if (!page || !canInject(answer.statusCode, answer.headers)) {
        res.writeHead(answer.statusCode, answer.statusMessage, pass(answer.headers));
        return void answer.pipe(res);
      }
      const chunks = [];
      answer.on('data', chunk => chunks.push(chunk));
      answer.on('error', () => fail(req, res, true));
      answer.on('end', () => {
        const body = Buffer.from(injectBridge(Buffer.concat(chunks).toString('utf8'), origin));
        const out = pass(answer.headers);
        for (const name of ['content-length', 'etag', 'last-modified']) delete out[name];
        res.writeHead(answer.statusCode, answer.statusMessage, { ...out, 'content-length': body.length });
        res.end(body);
      });
    });
    // A page load is short; other requests may be long-lived (T3's streams), so only pages time out.
    if (page) up.setTimeout(15000, () => up.destroy(new Error('T3 took too long')));
    up.on('error', () => fail(req, res, page));
    res.on('close', () => res.writableFinished || up.destroy());
    req.pipe(up);
  }

  /** A websocket (or other upgrade) passed to T3 as is. */
  function upgrade(req, socket, head) {
    socket.on('error', () => {});
    const up = http.request({ ...upstream, agent: false, method: req.method, path: req.url, headers: req.headers });
    const statusLine = answer =>
      `HTTP/1.1 ${answer.statusCode} ${answer.statusMessage}\r\n` +
      answer.rawHeaders.reduce((text, value, i) => (i % 2 ? `${text}: ${value}\r\n` : `${text}${value}`), '') +
      '\r\n';
    up.on('upgrade', (answer, t3Socket, t3Head) => {
      t3Socket.on('error', () => socket.destroy());
      socket.on('close', () => t3Socket.destroy());
      t3Socket.on('close', () => socket.destroy());
      socket.write(statusLine(answer));
      if (t3Head.length) socket.write(t3Head);
      if (head.length) t3Socket.write(head);
      t3Socket.pipe(socket).pipe(t3Socket);
    });
    // T3 refused the upgrade: its answer, then the connection closes.
    up.on('response', answer => {
      socket.write(statusLine(answer));
      answer.pipe(socket);
    });
    up.on('error', () => {
      if (socket.writable) socket.end('HTTP/1.1 502 Bad Gateway\r\nconnection: close\r\ncontent-length: 0\r\n\r\n');
    });
    up.end();
  }

  /** The full proxy (the lab): all of T3 on its own port, websockets included. */
  function listen(port, host = '127.0.0.1') {
    const server = http.createServer(handle);
    server.on('upgrade', upgrade);
    return new Promise((resolve, reject) => {
      server.once('error', reject);
      server.listen(port, host, () => resolve(server));
    });
  }

  return { owns, handle, upgrade, listen, close: () => agent.destroy() };
}
