import { t3Admin } from './t3admin.mjs';
import { createT3Bridge } from './t3bridge.mjs';
import http from 'node:http';
import { readFileSync, statSync, createReadStream, existsSync, chmodSync } from 'node:fs';
import { join, resolve, extname, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createHash, randomUUID } from 'node:crypto';
import { backup } from 'node:sqlite';
import { gzipSync } from 'node:zlib';
import { Store } from './store.mjs';
import { applyBatch } from './batch.mjs';
import { historyGroup, historyGroups, previewEdits, undoEdits } from './history.mjs';
import {
  addProfile,
  attachWalkPhotos,
  claimJob,
  deleteTrack,
  deleteWalk,
  getPhoto,
  finishJob,
  listJobs,
  listProfiles,
  listTracks,
  listWalks,
  queueLink,
  queueSync,
  removeProfile,
  reopenWalk,
  saveTrack,
  saveWalk,
  rematchTracks,
  applyRematch,
  linkCapture,
  storeReviewedWalk,
} from './monitoring.mjs';
import { monitoringRowsCsv, pointsCsv, walksGpx, wikilocCorrections } from './monitoring-export.mjs';
import { idSuggestions, tableChanges, tablePayload, tableRevision } from './grid.mjs';
import { extendPremadeRows } from './premade.mjs';
import { addClutchCheck, clutchDay, clutchState, removeClutchCheck } from './clutches.mjs';
import { createSheetHook } from './hooks.mjs';
import { createInvitations, mailerFromEnv } from './invitations.mjs';
import { createPasswordResets } from './passwordReset.mjs';
import { createSummary } from './summary.mjs';
import { applyIdChange, planIdChange } from './insectaryId.mjs';
import { UNIQUE, TUBE_FIELD } from './verifications.mjs';
import { CHECK_KINDS, allIssues, checkData } from './checks.mjs';
import { reviewPage, setVerdicts, trainingLabels, verdictHistory } from './review.mjs';
import { allSuggestions, suggestionPage, suggestionSources, suggestionsCsv } from './suggestions/index.mjs';
import { solvedFindings } from './findings.mjs';
import { alerts } from './alerts.mjs';
import { createInstructions } from './instructions.mjs';
import { createPhotoService, photoCacheDir } from './photos.mjs';
import { listOptions } from './verify.mjs';
import { moduleMap, validateValues } from './schema.mjs';
import { REAL_ID, checkWorkbookId, workbookFromEnv, workbookUrl } from './workbook.mjs';
import {
  setup,
  login,
  getSession,
  checkCsrf,
  revoke,
  cookie,
  publicUser,
  requireAdmin,
  resolveAccount,
  createUser,
  updateUser,
  csrfForSession,
} from './auth.mjs';

const here = dirname(fileURLToPath(import.meta.url));
const webRoot = resolve(here, '../web');
const now = () => new Date().toISOString();
const fail = (code, message, status = 400, details) => Object.assign(new Error(message), { code, status, details });
const json = (res, status, value, headers = {}) => send(res, status, JSON.stringify(value), headers);
/** Sends JSON, gzip-compressed when the client accepts it and the body is large. */
function send(res, status, text, headers = {}, gzipped = null) {
  const accepts = /\bgzip\b/.test(res.req?.headers['accept-encoding'] || '');
  const compress = accepts && (gzipped || text.length > 8192);
  res.writeHead(status, {
    'content-type': 'application/json; charset=utf-8',
    'cache-control': 'no-store',
    'x-content-type-options': 'nosniff',
    vary: 'accept-encoding',
    ...(compress ? { 'content-encoding': 'gzip' } : {}),
    ...headers,
  });
  res.end(compress ? gzipped || gzipSync(text) : text);
}
/**
 * A large answer that is often asked again unchanged (the walks and tracks of
 * Monitoreo, a sheet's lists): tagged with a hash of its body, so the browser
 * revalidates and an unchanged answer comes back as a 304 without the body.
 */
function sendTagged(res, value) {
  const text = JSON.stringify(value);
  const etag = `"${createHash('sha1').update(text).digest('base64url')}"`;
  const headers = { etag, 'cache-control': 'private, no-cache' };
  if (res.req?.headers['if-none-match'] === etag) {
    res.writeHead(304, headers);
    return res.end();
  }
  return send(res, 200, text, headers);
}
function decodePart(part) {
  try {
    return decodeURIComponent(part);
  } catch {
    throw fail('INVALID_PATH', 'Malformed URL');
  }
}

/**
 * Failed sign-ins are limited per account and per client address. Behind the
 * reverse proxy every request comes from 127.0.0.1, so the forwarded address
 * is used when the connection itself is local.
 */
class LoginLimiter {
  constructor({ limit = 8, windowMs = 15 * 60_000 } = {}) {
    this.limit = limit;
    this.windowMs = windowMs;
    this.failures = new Map();
  }
  keys(req, username) {
    const socket = req.socket.remoteAddress || 'unknown';
    const local = /^(::1|127\.|::ffff:127\.)/.test(socket);
    const forwarded = local
      ? String(req.headers['x-forwarded-for'] || '')
          .split(',')[0]
          .trim()
      : '';
    return [`user:${String(username || '').toLowerCase()}`, `ip:${forwarded || socket}`];
  }
  recent(key) {
    const cutoff = Date.now() - this.windowMs;
    const list = (this.failures.get(key) || []).filter(t => t > cutoff);
    if (list.length) this.failures.set(key, list);
    else this.failures.delete(key);
    return list;
  }
  /** An address gets more attempts than an account, since several people may share one. */
  accountBlocked(req, username) {
    return this.recent(this.keys(req, username)[0]).length >= this.limit;
  }
  addressBlocked(req) {
    return this.recent(this.keys(req, '')[1]).length >= this.limit * 3;
  }
  check(req, username) {
    if (this.accountBlocked(req, username) || this.addressBlocked(req))
      throw fail('RATE_LIMITED', 'Too many attempts; wait 15 minutes and try again', 429);
  }
  failed(req, username) {
    for (const key of this.keys(req, username)) this.failures.set(key, [...this.recent(key), Date.now()]);
  }
  succeeded(req, username) {
    this.failures.delete(this.keys(req, username)[0]);
  }
}
const mime = {
  '.html': 'text/html; charset=utf-8',
  '.js': 'text/javascript; charset=utf-8',
  '.mjs': 'text/javascript; charset=utf-8',
  '.css': 'text/css; charset=utf-8',
  '.svg': 'image/svg+xml',
  '.png': 'image/png',
  '.webp': 'image/webp',
  '.jpg': 'image/jpeg',
  '.ico': 'image/x-icon',
  '.json': 'application/json',
  '.webmanifest': 'application/manifest+json',
};

export function configFromEnv(env = process.env) {
  const workbook = workbookFromEnv(env);
  return {
    host: env.APP_HOST || '127.0.0.1',
    port: Number(env.APP_PORT || 8794),
    basePath: normalizeBase(env.APP_BASE_PATH || '/ithomiini'),
    databasePath: env.DATABASE_PATH || join(here, '../.local/app.sqlite'),
    googleCredentialsFile: env.GOOGLE_CREDENTIALS_FILE,
    setupToken: env.SETUP_TOKEN,
    localMode: env.LOCAL_MODE === '1',
    seedFile: env.SEED_FILE,
    secureCookies: env.SECURE_COOKIES !== '0',
    // The Google Sheets workbook (WORKBOOK_ID, the team's workbook by default).
    spreadsheetId: workbook.id,
    syncIntervalMs: Number(env.SYNC_INTERVAL_MS || 300000),
    sheetHookSecret: env.SHEET_HOOK_SECRET,
    // The app's address for links the assistant gives (e.g. to a save in the Historial).
    publicUrl: (env.APP_PUBLIC_URL || env.APP_ORIGIN || '').replace(/\/+$/, ''),
    // Specimen photos fetched from Drive for the Revisión tab (server/photos.mjs).
    photoCacheDir: env.PHOTO_CACHE_DIR,
    photoCacheMb: Number(env.PHOTO_CACHE_MB || 1024),
    // T3 Code (stock install on its own host), shown inside the Asistente tab.
    t3: env.ITHOMIINI_T3_URL
      ? {
          url: env.ITHOMIINI_T3_URL.replace(/\/+$/, ''),
          local: env.ITHOMIINI_T3_LOCAL || 'http://127.0.0.1:3773',
          tokenFile: env.ITHOMIINI_T3_ADMIN_TOKEN_FILE,
          home: env.ITHOMIINI_T3_HOME || '/home/ubuntu/.t3',
          // The lab: all of T3 through this app on 127.0.0.1:<port>, its pages with the bridge (server/t3bridge.mjs).
          proxyPort: Number(env.ITHOMIINI_T3_PROXY_PORT) || null,
        }
      : null,
    aiApiKey: env.AI_API_KEY || env.OPENAI_API_KEY,
    aiModel: env.AI_MODEL || env.OPENAI_MODEL,
    aiBaseUrl: env.AI_BASE_URL,
    geminiApiKey: env.GEMINI_API_KEY,
    knowledgeRoots: env.KNOWLEDGE_DIR ? [env.KNOWLEDGE_DIR] : [],
    ai: {
      baseUrl: env.ITHOMIINI_AI_BASE_URL || env.AI_BASE_URL,
      model: env.ITHOMIINI_AI_MODEL || env.AI_MODEL,
      apiKeyFile: env.ITHOMIINI_AI_API_KEY_FILE,
      apiKey: env.ITHOMIINI_AI_API_KEY || env.AI_API_KEY,
      transcriptionModel: env.ITHOMIINI_AI_TRANSCRIPTION_MODEL,
      transcriptionMode: env.ITHOMIINI_AI_TRANSCRIPTION_MODE,
      visionModel: env.ITHOMIINI_AI_VISION_MODEL,
    },
  };
}
function normalizeBase(value) {
  const raw = '/' + String(value).replace(/^\/+|\/+$/g, '');
  return raw === '/' ? '/' : raw;
}
function routePath(url, base) {
  const path = new URL(url, 'http://localhost').pathname;
  return base !== '/' && path.startsWith(base + '/') ? path.slice(base.length) : path === base ? '/' : path;
}
async function bodyOf(req) {
  const pieces = [];
  let size = 0;
  for await (const chunk of req) {
    size += chunk.length;
    if (size > 15 * 1024 * 1024) throw fail('BODY_TOO_LARGE', 'Request body is too large', 413);
    pieces.push(chunk);
  }
  if (!size) return {};
  try {
    return JSON.parse(Buffer.concat(pieces).toString('utf8'));
  } catch {
    throw fail('INVALID_JSON', 'Request body must be valid JSON');
  }
}
function parseFilters(text) {
  if (!text) return {};
  try {
    const filters = JSON.parse(text);
    if (filters && typeof filters === 'object' && !Array.isArray(filters)) return filters;
  } catch {
    /* fall through */
  }
  throw fail('INVALID_FILTER', 'filters must be a JSON object');
}
function requireId(body) {
  if (typeof body.requestId !== 'string' || body.requestId.length < 8)
    throw fail('REQUEST_ID_REQUIRED', 'A unique requestId is required');
}
function checkOrigin(req) {
  if (req.headers['sec-fetch-site'] === 'cross-site')
    throw fail('ORIGIN_FORBIDDEN', 'Cross-site requests are not allowed', 403);
  if (!req.headers.origin) return;
  let origin;
  try {
    origin = new URL(req.headers.origin);
  } catch {
    throw fail('ORIGIN_FORBIDDEN', 'Invalid Origin header', 403);
  }
  const host = String(req.headers['x-forwarded-host'] || req.headers.host || '')
    .split(',')[0]
    .trim();
  if (origin.host !== host) throw fail('ORIGIN_FORBIDDEN', 'Cross-origin requests are not allowed', 403);
}
function requireEditor(user) {
  if (!['editor', 'reviewer', 'admin'].includes(user.role)) throw fail('FORBIDDEN', 'Editor role required', 403);
}
function requireReviewer(user) {
  if (!['reviewer', 'admin'].includes(user.role)) throw fail('FORBIDDEN', 'Reviewer role required', 403);
}
function cleanTask(body, old = {}) {
  const title = body.title === undefined ? old.title : String(body.title).trim();
  if (!title || title.length > 200) throw fail('INVALID_TASK', 'Task title is required');
  const status = body.status ?? old.status ?? 'open';
  if (!['open', 'in_progress', 'blocked', 'done', 'cancelled'].includes(status))
    throw fail('INVALID_TASK', 'Invalid task status');
  return {
    title,
    description: body.description === undefined ? (old.description ?? null) : String(body.description).slice(0, 5000),
    dueDate: body.dueDate === undefined ? (old.dueDate ?? null) : body.dueDate,
    assignee: body.assignee === undefined ? (old.assignee ?? null) : body.assignee,
    status,
    recordId: body.recordId === undefined ? (old.recordId ?? null) : body.recordId,
  };
}
function csvRows(text) {
  const rows = [];
  let row = [],
    cell = '',
    quoted = false;
  for (let i = 0; i < text.length; i++) {
    const c = text[i];
    if (quoted) {
      if (c === '"' && text[i + 1] === '"') {
        cell += '"';
        i++;
      } else if (c === '"') quoted = false;
      else cell += c;
    } else if (c === '"') quoted = true;
    else if (c === ',') {
      row.push(cell);
      cell = '';
    } else if (c === '\n') {
      row.push(cell.replace(/\r$/, ''));
      rows.push(row);
      row = [];
      cell = '';
    } else cell += c;
  }
  if (quoted) throw fail('INVALID_CSV', 'CSV has an unclosed quote');
  if (cell || row.length) {
    row.push(cell.replace(/\r$/, ''));
    rows.push(row);
  }
  return rows;
}
function csvEscape(value) {
  if (typeof value === 'number' || typeof value === 'boolean') return String(value);
  const raw = value == null ? '' : String(value);
  const text = /^[=+\-@\t]/.test(raw) ? `'${raw}` : raw;
  return /[",\r\n]/.test(text) ? `"${text.replaceAll('"', '""')}"` : text;
}

export async function createApp(config = {}, options = {}) {
  config = { ...configFromEnv({}), ...config };
  config.basePath = normalizeBase(config.basePath || '/ithomiini');
  config.spreadsheetId = checkWorkbookId(config.spreadsheetId || REAL_ID);
  let seed = options.seed;
  if (!seed && config.localMode && config.seedFile) {
    seed = JSON.parse(readFileSync(config.seedFile, 'utf8'));
    seed = seed.sheets || seed;
  }
  const store = options.store || new Store(config, { sheets: options.sheets, seed });
  const assistantFile = new URL('./assistant.mjs', import.meta.url);
  let assistant = null;
  if (existsSync(fileURLToPath(assistantFile))) {
    const { createAssistant } = await import(assistantFile.href);
    assistant = createAssistant({ store, config });
  }
  const loginLimiter = new LoginLimiter();
  const tableCache = new Map();
  const sheetHook = createSheetHook(store, { secret: config.sheetHookSecret });
  const summary = createSummary(store);
  // The AI instructions page: the assistant's brief, skills, subagents and tools with their history.
  const instructions = createInstructions({ tools: () => assistant?.tools?.() ?? [], ...options.instructions });
  const photos = createPhotoService(store, {
    // Next to the database file the store really opened; in memory for an in-memory database.
    dir: config.photoCacheDir || photoCacheDir({}, store.db.location?.() ?? null),
    maxBytes: (config.photoCacheMb || 1024) * 1024 * 1024,
    ...(options.fetchPhoto ? { fetchImpl: options.fetchPhoto } : {}),
  });
  const mailer = options.mailer ?? mailerFromEnv();
  const invitations = createInvitations(store, mailer, options.mail ? { send: options.mail } : {});
  const resets = createPasswordResets(store, mailer, options.mail ? { send: options.mail } : {});
  // Reset links asked for from the sign-in page: 3 per account and 9 per address every 15 minutes.
  const resetLimiter = new LoginLimiter({ limit: 3 });
  // T3's pages with the script that tells the Asistente tab which chat they show (server/t3bridge.mjs).
  const t3Bridge = config.t3?.url && config.t3?.local ? createT3Bridge({ t3: config.t3, appOrigin: config.publicUrl }) : null;
  let t3Proxy = null;
  const server = http.createServer(async (req, res) => {
    // Production: Caddy sends T3's page loads here (deploy/Caddyfile.fragment).
    if (t3Bridge?.owns(req)) return t3Bridge.handle(req, res);
    const requestId = randomUUID();
    res.setHeader('x-request-id', requestId);
    try {
      const url = new URL(req.url, 'http://localhost'),
        path = routePath(req.url, config.basePath),
        method = req.method;
      if (method === 'GET' && path === '/health') return json(res, 200, { status: 'ok', sync: store.syncStatus.state });
      // Shares are normally caught by the service worker; if it was not active yet,
      // open the import screen and let the person share again.
      if (method === 'POST' && path === '/share-target') {
        req.resume();
        res.writeHead(303, { location: `${config.basePath === '/' ? '' : config.basePath}/#/monitoreo?vista=importar&compartido=0` });
        return res.end();
      }
      if (!path.startsWith('/api/')) return serveStatic(req, res, path, config.basePath, config.t3?.url);
      if (method !== 'GET' && method !== 'HEAD') checkOrigin(req);
      const session = getSession(store, req.headers.cookie);
      if (method === 'GET' && path === '/api/auth/session')
        return json(res, 200, {
          user: session?.user || null,
          csrf: session ? csrfForSession(store, session) : null,
          setupRequired: !store.db.prepare("SELECT 1 FROM users WHERE role='admin' AND active=1").get(),
        });
      const body = ['POST', 'PATCH', 'PUT', 'DELETE'].includes(method) ? await bodyOf(req) : {};
      // T3 Code's chats reach the assistant's tools here with the person's token (scripts/t3-provision.mjs) instead of a session.
      if (path === '/api/ai/mcp') {
        if (!assistant?.mcp) throw fail('NOT_FOUND', 'Not found', 404);
        if (method !== 'POST') {
          res.writeHead(405, { allow: 'POST' });
          return res.end();
        }
        const out = await assistant.mcp(req.headers, body);
        if (out.status === 202) {
          res.writeHead(202);
          return res.end();
        }
        return json(res, out.status, out.body);
      }
      // Called by the Apps Script trigger, which has no session; it sends a shared secret instead.
      if (method === 'POST' && path === '/api/hooks/sheet-edit')
        return json(res, 202, sheetHook.receive(req.headers, body));
      if (method === 'POST' && path === '/api/auth/setup') {
        const user = setup(store, body, config);
        const auth = login(store, { username: user.username, password: body.password });
        return json(
          res,
          201,
          { user: auth.user, csrf: auth.csrf },
          { 'set-cookie': cookie(auth.token, { path: config.basePath, secure: config.secureCookies }) },
        );
      }
      if (method === 'POST' && path === '/api/auth/login') {
        // Username or email: failures count against the account, however it was named.
        const account = resolveAccount(store, body.username)?.username ?? String(body.username ?? '').trim();
        loginLimiter.check(req, account);
        try {
          const auth = login(store, body);
          loginLimiter.succeeded(req, account);
          return json(
            res,
            200,
            { user: auth.user, csrf: auth.csrf },
            { 'set-cookie': cookie(auth.token, { path: config.basePath, secure: config.secureCookies }) },
          );
        } catch (e) {
          loginLimiter.failed(req, account);
          throw e;
        }
      }
      // Forgotten password: the answer is the same whether or not the account exists
      // (and the email is sent in the background, so the timing does not tell either).
      if (method === 'POST' && path === '/api/auth/reset/request') {
        const identifier = String(body.identifier ?? '').trim();
        if (!identifier) throw fail('IDENTIFIER_REQUIRED', 'Enter your username or email');
        if (resetLimiter.addressBlocked(req))
          throw fail('RATE_LIMITED', 'Too many requests; wait 15 minutes and try again', 429);
        const account = resolveAccount(store, identifier)?.username ?? identifier.toLowerCase();
        const blocked = resetLimiter.accountBlocked(req, account);
        resetLimiter.failed(req, account);
        if (!blocked) resets.request(identifier)?.catch(() => {});
        return json(res, 202, { ok: true });
      }
      if (method === 'GET' && path === '/api/auth/reset/lookup')
        return json(res, 200, { reset: resets.lookup(url.searchParams.get('t')) });
      if (method === 'POST' && path === '/api/auth/reset') {
        // Guessed links count against the address (with failed sign-ins); an expired or used one does not.
        if (loginLimiter.addressBlocked(req))
          throw fail('RATE_LIMITED', 'Too many attempts; wait 15 minutes and try again', 429);
        let changed;
        try {
          changed = resets.use(body.token, body.password);
        } catch (e) {
          if (e.code === 'RESET_INVALID') loginLimiter.failed(req, 'password-reset');
          throw e;
        }
        const auth = login(store, { username: changed.username, password: body.password });
        return json(
          res,
          200,
          { user: auth.user, csrf: auth.csrf },
          { 'set-cookie': cookie(auth.token, { path: config.basePath, secure: config.secureCookies }) },
        );
      }
      // The home page is open to visitors: natural-history summaries only (the team's
      // counts are added for signed-in people).
      if (method === 'GET' && path === '/api/summary') return json(res, 200, summary.build({ signedIn: !!session }));
      // The invitation page is used before the person has an account.
      if (method === 'GET' && path === '/api/invitations/lookup')
        return json(res, 200, { invitation: invitations.lookup(url.searchParams.get('t')) });
      if (method === 'POST' && path === '/api/invitations/accept') {
        loginLimiter.check(req, 'invitation');
        let created;
        try {
          created = invitations.accept(body.token, body);
        } catch (e) {
          loginLimiter.failed(req, 'invitation');
          throw e;
        }
        const auth = login(store, { username: created.username, password: body.password });
        return json(
          res,
          201,
          { user: auth.user, csrf: auth.csrf },
          { 'set-cookie': cookie(auth.token, { path: config.basePath, secure: config.secureCookies }) },
        );
      }
      if (!session) throw fail('AUTH_REQUIRED', 'Sign in required', 401);
      if (method !== 'GET' && method !== 'HEAD') checkCsrf(session, req.headers['x-csrf-token']);
      const user = session.user,
        query = Object.fromEntries(url.searchParams.entries());
      if (method === 'POST' && path === '/api/auth/logout') {
        revoke(store, session);
        return json(
          res,
          200,
          { ok: true },
          { 'set-cookie': cookie('', { path: config.basePath, secure: config.secureCookies }) },
        );
      }
      if (method === 'GET' && path === '/api/bootstrap')
        return json(res, 200, {
          user,
          csrf: csrfForSession(store, session),
          modules: store.listModules(),
          stats: store.getStats(),
          options: {},
          sync: store.syncStatus,
          settings: {
            language: 'es',
            // The lab's offline copy (LOCAL_MODE) has no sheet of its own: no link to the team's.
            sheetUrl: store.localMode ? null : workbookUrl(store.sheets.spreadsheetId),
            localMode: store.localMode,
            basePath: config.basePath,
          },
        });
      if (method === 'GET' && path === '/api/records')
        return json(res, 200, store.searchRecords({ ...query, filters: parseFilters(query.filters) }));
      if (method === 'GET' && /^\/api\/records\/[^/]+$/.test(path)) {
        const record = store.getRecord(decodePart(path.split('/')[3]));
        if (!record) throw fail('RECORD_NOT_FOUND', 'Record not found', 404);
        const related = relatedRecords(store, record);
        return json(res, 200, {
          record,
          related,
          history: store.getHistory({ recordId: record.id, limit: 50 }).actions,
        });
      }
      if (method === 'POST' && path === '/api/records') {
        requireEditor(user);
        return json(res, 201, await store.createRecord(body, user));
      }
      if (method === 'PATCH' && /^\/api\/records\/[^/]+$/.test(path)) {
        requireEditor(user);
        return json(res, 200, await store.updateRecord(decodePart(path.split('/')[3]), body, user));
      }
      if (method === 'DELETE' && /^\/api\/records\/[^/]+$/.test(path)) {
        requireEditor(user);
        requireId(body);
        const record = store.getRecord(decodePart(path.split('/')[3]));
        if (!record) throw fail('RECORD_NOT_FOUND', 'Record not found', 404);
        const event = addEvent(
          store,
          {
            kind: 'withdrawal',
            recordId: record.id,
            values: { reason: body.reason || 'Withdrawn after review' },
            requestId: body.requestId,
          },
          user,
        );
        return json(res, 200, { record, event, status: 'recorded_in_app' });
      }
      if (method === 'POST' && path === '/api/records/batch') {
        requireEditor(user);
        requireId(body);
        return json(res, 200, await applyBatch(store, body, user, { source: 'app' }));
      }
      if (method === 'GET' && path === '/api/table') {
        // Whole sheets are cached compressed until the local copy changes.
        const module = String(query.module || '');
        const revision = tableRevision(store, module);
        const etag = `"${Buffer.from(`${module}:${revision}`).toString('base64url')}"`;
        if (req.headers['if-none-match'] === etag) {
          res.writeHead(304, { etag, 'cache-control': 'no-cache' });
          return res.end();
        }
        let cached = tableCache.get(module);
        if (cached?.revision !== revision) {
          const text = JSON.stringify({ ...tablePayload(store, module), revision });
          cached = { revision, text, gzipped: gzipSync(text) };
          tableCache.set(module, cached);
        }
        return send(res, 200, cached.text, { etag, 'cache-control': 'no-cache' }, cached.gzipped);
      }
      if (method === 'GET' && path === '/api/table/changes')
        return json(res, 200, tableChanges(store, String(query.module || ''), query.since));
      if (method === 'GET' && path === '/api/ids') return json(res, 200, idSuggestions(store, query));
      // Clutches (cards): the counts' sum formulas and last changes; the day's checks and changes; marking a check.
      if (method === 'GET' && path === '/api/clutches/state') return sendTagged(res, clutchState(store));
      if (method === 'GET' && path === '/api/clutches/day') return json(res, 200, clutchDay(store, query));
      if (method === 'POST' && path === '/api/clutches/checks') {
        requireEditor(user);
        requireId(body);
        const saved = addClutchCheck(store, body, user);
        return json(res, saved.duplicate ? 200 : 201, { check: saved.check });
      }
      if (method === 'DELETE' && /^\/api\/clutches\/checks\/[^/]+$/.test(path)) {
        requireEditor(user);
        return json(res, 200, removeClutchCheck(store, decodePart(path.split('/')[4]), user));
      }
      // More pre-made rows (formulas, formats, dropdowns; Insectary IDs) at the end of a sheet.
      if (method === 'POST' && /^\/api\/sheets\/[^/]+\/extend$/.test(path)) {
        requireReviewer(user);
        const sheet = decodePart(path.split('/')[3]);
        if (!moduleMap.has(sheet)) throw fail('MODULE_NOT_FOUND', 'Hoja desconocida', 404);
        return json(res, 200, await extendPremadeRows(store, sheet, body.count, user));
      }
      // The sheet's own checks for one sheet (repeated IDs, dropdown lists), for the grids to colour.
      if (method === 'GET' && path === '/api/verifications') {
        const module = String(query.module || '');
        const mod = moduleMap.get(module);
        if (!mod) throw fail('MODULE_NOT_FOUND', 'Unknown module', 404);
        const lists = Object.fromEntries(
          Object.entries(listOptions(store, module)).map(([field, o]) => [field, { strict: o.strict, source: o.source, values: [...o.values] }]),
        );
        const unique = mod.fields.map(f => f.key).filter(k => UNIQUE[module]?.includes(k) || TUBE_FIELD.test(k));
        return sendTagged(res, { module, unique, lists });
      }
      // Revisión de datos: inconsistencies across the workbook (the assistant's check_data tool).
      if (method === 'GET' && path === '/api/checks') return json(res, 200, checkData(store, query));
      // The Revisión tab: the same issues with people's verdicts, and the specimen photos they show.
      if (method === 'GET' && path === '/api/review') {
        requireEditor(user);
        return json(res, 200, reviewPage(store, query));
      }
      if (method === 'GET' && path === '/api/review/verdicts') {
        requireEditor(user);
        return json(res, 200, { history: verdictHistory(store, query.issueId) });
      }
      if (method === 'POST' && path === '/api/review/verdicts') {
        requireEditor(user);
        return json(res, 200, setVerdicts(store, body, user));
      }
      // Suggested edits (server/suggestions/): read-only, for people to look at and copy; nothing applies them.
      if (method === 'GET' && path === '/api/suggested-edits') {
        requireEditor(user);
        return json(res, 200, await suggestionPage(store, query));
      }
      if (method === 'GET' && path === '/api/suggested-edits/csv') {
        requireEditor(user);
        // format=tsv: the same list to copy and paste into a sheet.
        const tsv = query.format === 'tsv';
        const csv = await suggestionsCsv(store, query, { tsv });
        if (tsv)
          return send(res, 200, csv, { 'content-type': 'text/tab-separated-values; charset=utf-8', 'cache-control': 'no-store' });
        res.writeHead(200, {
          'content-type': 'text/csv; charset=utf-8',
          'content-disposition': `attachment; filename="suggested-edits-${new Date().toISOString().slice(0, 10)}.csv"`,
          'cache-control': 'no-store',
        });
        return res.end(`\ufeff${csv}`);
      }
      // Problems and suggestions the sheet no longer has (server/findings.mjs), after looking again.
      if (method === 'GET' && path === '/api/solved') {
        requireEditor(user);
        allIssues(store);
        await allSuggestions(store);
        const titles = { check: CHECK_KINDS, suggestion: Object.fromEntries(suggestionSources().map(s => [s.id, s.title])) };
        return json(res, 200, { ...solvedFindings(store, query), titles });
      }
      // CAM pools running out and the 30-preserved rule (server/alerts.mjs): for the team (Inicio, Revisión).
      if (method === 'GET' && path === '/api/alerts') return json(res, 200, alerts(store));
      // Verdicts on what models read from the photos, kept as training labels.
      if (method === 'GET' && path === '/api/review/labels') {
        requireEditor(user);
        res.writeHead(200, {
          'content-type': 'application/x-ndjson; charset=utf-8',
          'content-disposition': 'attachment; filename="review-labels.jsonl"',
          'cache-control': 'no-store',
        });
        return res.end(trainingLabels(store));
      }
      if (method === 'GET' && /^\/api\/photo\/[\w-]+$/.test(path)) {
        const photo = await photos.get(path.split('/')[3], Number(query.w || 400));
        if (req.headers['if-none-match'] === photo.etag) {
          res.writeHead(304, { etag: photo.etag, 'cache-control': 'private, max-age=2592000, immutable' });
          return res.end();
        }
        res.writeHead(200, {
          'content-type': photo.mime,
          'content-security-policy': 'sandbox; default-src none',
          'x-content-type-options': 'nosniff',
          'cache-control': 'private, max-age=2592000, immutable',
          etag: photo.etag,
        });
        return res.end(photo.data);
      }
      // Correcting an Insectary ID after saving: preview, then one undoable save.
      if (method === 'GET' && path === '/api/insectary-ids/plan') {
        const plan = planIdChange(store, query.from, query.to);
        return json(res, 200, { ...plan, edits: plan.edits.length });
      }
      if (method === 'POST' && path === '/api/insectary-ids/change') {
        requireEditor(user);
        requireId(body);
        return json(res, 200, await applyIdChange(store, body, user));
      }
      if (method === 'POST' && path === '/api/actions') {
        requireEditor(user);
        requireId(body);
        return json(res, 200, await domainAction(store, body, user));
      }
      if (method === 'GET' && path === '/api/history') return json(res, 200, store.getHistory(query));
      // Saves grouped by person, purpose and time (Historial tab); a group with all its changes.
      if (method === 'GET' && path === '/api/history/groups') return json(res, 200, historyGroups(store, query));
      if (method === 'GET' && /^\/api\/history\/groups\/[^/]+$/.test(path))
        return json(res, 200, { group: historyGroup(store, decodePart(path.split('/')[4])) });
      // Undo whole groups (groupIds), saves (actionIds) or single changes (changeIds).
      if (method === 'POST' && path === '/api/history/preview') return json(res, 200, previewEdits(store, body));
      if (method === 'POST' && path === '/api/history/undo') return json(res, 200, await undoEdits(store, body, user));
      if (method === 'GET' && path === '/api/tasks') return json(res, 200, { tasks: store.listTasks() });
      if (method === 'POST' && path === '/api/tasks') {
        requireEditor(user);
        requireId(body);
        const prior = store.db.prepare('SELECT value FROM settings WHERE key=?').get(`request:${body.requestId}`);
        if (prior) return json(res, 200, JSON.parse(prior.value));
        const task = cleanTask(body),
          id = randomUUID(),
          stamp = now();
        store.db
          .prepare('INSERT INTO tasks VALUES(?,?,?,?,?,?,?,?,?,?)')
          .run(
            id,
            task.title,
            task.description,
            task.dueDate,
            task.assignee,
            task.status,
            task.recordId,
            user.id,
            stamp,
            stamp,
          );
        const result = { task: store.task(store.db.prepare('SELECT * FROM tasks WHERE id=?').get(id)) };
        store.setSetting(`request:${body.requestId}`, JSON.stringify(result));
        return json(res, 201, result);
      }
      if (method === 'PATCH' && /^\/api\/tasks\/[^/]+$/.test(path)) {
        requireEditor(user);
        requireId(body);
        const id = path.split('/')[3],
          old = store.db.prepare('SELECT * FROM tasks WHERE id=?').get(id);
        if (!old) throw fail('TASK_NOT_FOUND', 'Task not found', 404);
        const task = cleanTask(body, store.task(old));
        store.db
          .prepare(
            'UPDATE tasks SET title=?,description=?,due_date=?,assignee=?,status=?,record_id=?,updated_at=? WHERE id=?',
          )
          .run(task.title, task.description, task.dueDate, task.assignee, task.status, task.recordId, now(), id);
        return json(res, 200, { task: store.task(store.db.prepare('SELECT * FROM tasks WHERE id=?').get(id)) });
      }
      if (method === 'GET' && path === '/api/events') return json(res, 200, { events: store.listEvents(query) });
      if (method === 'POST' && path === '/api/events') {
        requireEditor(user);
        requireId(body);
        return json(res, 201, { event: addEvent(store, body, user) });
      }
      if (method === 'GET' && path === '/api/options')
        return json(res, 200, { options: optionsFor(store, query.module, query.field, query.q, query.species) });
      if (method === 'GET' && path === '/api/suggestions') return json(res, 200, suggestionsFor(store, query.module));
      if (method === 'GET' && path === '/api/sync')
        return json(res, 200, { ...store.syncStatus, hook: sheetHook.status });
      if (method === 'POST' && path === '/api/sync') {
        requireEditor(user);
        return json(res, 200, await store.sync({ force: true }));
      }
      if (method === 'POST' && path === '/api/import/preview') {
        requireEditor(user);
        return json(res, 200, previewImport(store, body, user));
      }
      if (method === 'POST' && path === '/api/import/apply') {
        requireEditor(user);
        requireId(body);
        return json(res, 200, await applyImport(store, body, user));
      }
      if (method === 'GET' && path === '/api/export') {
        const mod = moduleMap.get(query.module);
        if (!mod) throw fail('MODULE_NOT_FOUND', 'Unknown module', 404);
        if (query.format && query.format !== 'csv') throw fail('INVALID_FORMAT', 'Only CSV is supported');
        const records = store.searchRecords({ module: mod.id, limit: 500000 }).records;
        const fields = mod.fields.map(f => f.key);
        const csv = [
          fields.map(csvEscape).join(','),
          ...records.map(r => fields.map(k => csvEscape(r.values[k])).join(',')),
        ].join('\r\n');
        res.writeHead(200, {
          'content-type': 'text/csv; charset=utf-8',
          'content-disposition': `attachment; filename="${mod.id.replaceAll('/', '_')}.csv"`,
          'cache-control': 'no-store',
        });
        return res.end(csv);
      }
      if (method === 'GET' && path === '/api/monitoring/tracks') return sendTagged(res, { tracks: listTracks(store, user) });
      // Monitoreo → Wikiloc: what the app holds, the corrections its points suggest, and the downloads (anyone signed in).
      if (method === 'GET' && path === '/api/monitoring/wikiloc-data') return json(res, 200, wikilocCorrections(store));
      if (method === 'GET' && /^\/api\/monitoring\/export\/(rows\.csv|points\.csv|walks\.gpx)$/.test(path)) {
        const kind = path.split('/')[4];
        const walk = query.walk ? String(query.walk) : null;
        const file =
          kind === 'rows.csv' ? monitoringRowsCsv(store) : kind === 'points.csv' ? pointsCsv(store, walk) : walksGpx(store, walk);
        res.writeHead(200, {
          'content-type': file.type,
          'content-disposition': `attachment; filename="${file.name}"`,
          'cache-control': 'no-store',
          'x-content-type-options': 'nosniff',
        });
        return res.end(file.body);
      }
      if (method === 'POST' && path === '/api/monitoring/tracks') {
        requireEditor(user);
        requireId(body);
        const saved = saveTrack(store, body, user);
        return json(res, saved.duplicate ? 200 : 201, saved);
      }
      // Pairing of walk points with sheet rows: what matching again would change, and the doubts.
      if (method === 'GET' && path === '/api/monitoring/rematch') {
        requireEditor(user);
        return json(res, 200, rematchTracks(store, user));
      }
      if (method === 'POST' && path === '/api/monitoring/rematch') {
        requireReviewer(user);
        return json(res, 200, applyRematch(store, body));
      }
      if (method === 'POST' && /^\/api\/monitoring\/tracks\/[^/]+\/link$/.test(path)) {
        requireEditor(user);
        return json(res, 200, linkCapture(store, decodePart(path.split('/')[4]), body));
      }
      if (method === 'POST' && /^\/api\/monitoring\/wikiloc\/[^/]+\/store$/.test(path)) {
        requireEditor(user);
        return json(res, 201, storeReviewedWalk(store, decodePart(path.split('/')[4]), body, user));
      }
      if (method === 'POST' && /^\/api\/monitoring\/tracks\/[^/]+\/photos$/.test(path)) {
        requireEditor(user);
        return json(res, 200, attachWalkPhotos(store, decodePart(path.split('/')[4]), String(body.walkId || '')));
      }
      if (method === 'GET' && path === '/api/monitoring/wikiloc/jobs') return json(res, 200, listJobs(store));
      if (method === 'POST' && path === '/api/monitoring/wikiloc/links') {
        requireEditor(user);
        return json(res, 201, queueLink(store, body, user));
      }
      if (method === 'POST' && path === '/api/monitoring/wikiloc/sync') {
        requireEditor(user);
        return json(res, 201, queueSync(store, user));
      }
      if (method === 'POST' && path === '/api/monitoring/wikiloc/jobs/claim') {
        requireEditor(user);
        return json(res, 200, claimJob(store));
      }
      if (method === 'POST' && /^\/api\/monitoring\/wikiloc\/jobs\/[^/]+$/.test(path)) {
        requireEditor(user);
        return json(res, 200, finishJob(store, decodePart(path.split('/')[5]), body));
      }
      if (method === 'GET' && path === '/api/monitoring/wikiloc/profiles')
        return json(res, 200, { profiles: listProfiles(store) });
      if (method === 'POST' && path === '/api/monitoring/wikiloc/profiles') {
        requireEditor(user);
        return json(res, 201, addProfile(store, body, user));
      }
      if (method === 'DELETE' && /^\/api\/monitoring\/wikiloc\/profiles\/[^/]+$/.test(path)) {
        requireEditor(user);
        return json(res, 200, removeProfile(store, decodePart(path.split('/')[5])));
      }
      if (method === 'GET' && path === '/api/monitoring/wikiloc') return sendTagged(res, { walks: listWalks(store) });
      if (method === 'POST' && path === '/api/monitoring/wikiloc') {
        requireEditor(user);
        const saved = await saveWalk(store, body, user);
        return json(res, saved.updated ? 200 : 201, saved);
      }
      if (method === 'POST' && /^\/api\/monitoring\/wikiloc\/[^/]+\/reopen$/.test(path)) {
        requireEditor(user);
        return json(res, 200, reopenWalk(store, decodePart(path.split('/')[4])));
      }
      if (method === 'DELETE' && /^\/api\/monitoring\/wikiloc\/[^/]+$/.test(path)) {
        requireEditor(user);
        return json(res, 200, deleteWalk(store, decodePart(path.split('/')[4]), user));
      }
      if (method === 'GET' && /^\/api\/monitoring\/photos\/\d+$/.test(path)) {
        const photo = getPhoto(store, path.split('/')[4]);
        if (!photo) throw fail('PHOTO_NOT_FOUND', 'Photo not found', 404);
        res.writeHead(200, {
          'content-type': photo.mime_type,
          'content-security-policy': 'sandbox; default-src none',
          'x-content-type-options': 'nosniff',
          'cache-control': 'private, max-age=86400',
        });
        return res.end(photo.data);
      }
      if (method === 'DELETE' && /^\/api\/monitoring\/tracks\/[^/]+$/.test(path)) {
        requireEditor(user);
        return json(res, 200, deleteTrack(store, decodePart(path.split('/')[4]), user));
      }
      if (method === 'GET' && path === '/api/attachments')
        return json(res, 200, { attachments: listAttachments(store, query.recordId) });
      if (method === 'POST' && path === '/api/attachments') {
        requireEditor(user);
        requireId(body);
        return json(res, 201, { attachment: addAttachment(store, body, user) });
      }
      if (method === 'GET' && /^\/api\/attachments\/[^/]+\/content$/.test(path)) {
        const attachment = store.getAttachment(path.split('/')[3]);
        if (!attachment) throw fail('ATTACHMENT_NOT_FOUND', 'Attachment not found', 404);
        res.writeHead(200, {
          'content-type': attachment.mimeType,
          'content-disposition': `${attachment.mimeType.startsWith('image/') || attachment.mimeType.startsWith('audio/') ? 'inline' : 'attachment'}; filename="${attachment.name.replace(/["\r\n]/g, '_')}"`,
          'content-security-policy': 'sandbox; default-src none',
          'x-content-type-options': 'nosniff',
          'cache-control': 'private, no-store',
        });
        return res.end(attachment.data);
      }
      if (path === '/api/admin/users' && method === 'GET') {
        requireAdmin(user);
        return json(res, 200, {
          users: store.db.prepare('SELECT * FROM users ORDER BY username').all().map(publicUser),
        });
      }
      if (path === '/api/admin/users' && method === 'POST') {
        requireAdmin(user);
        requireId(body);
        return json(res, 201, { user: createUser(store, body) });
      }
      if (/^\/api\/admin\/users\/[^/]+$/.test(path) && method === 'PATCH') {
        requireAdmin(user);
        requireId(body);
        return json(res, 200, { user: updateUser(store, path.split('/')[4], body, user) });
      }
      const resetLink = /^\/api\/admin\/users\/([^/]+)\/reset-link$/.exec(path);
      if (resetLink && method === 'POST') {
        requireAdmin(user);
        return json(res, 201, await resets.adminLink(resetLink[1], user));
      }
      // environmentId: T3's chats are at <url>/<environmentId>/<threadId> (links that open a chat in the frame).
      if (method === 'GET' && path === '/api/t3/status')
        return json(res, 200, { url: config.t3?.url ?? null, environmentId: t3EnvironmentId(config.t3) });
      // What the assistant is told (any signed-in person): every file, the tools, each one's history.
      if (method === 'GET' && path === '/api/instructions') return json(res, 200, await instructions.list());
      if (method === 'GET' && path === '/api/instructions/diff') {
        const found = await instructions.diff(String(query.id ?? ''), String(query.commit ?? ''));
        if (!found) throw fail('NOT_FOUND', 'No such change', 404);
        return json(res, 200, found);
      }
      // Admins update T3 Code from the Asistente tab (server/t3admin.mjs).
      if (path === '/api/admin/t3' && (method === 'GET' || method === 'POST')) {
        requireAdmin(user);
        if (!config.t3) throw fail('T3_DISABLED', 'T3 Code is not configured', 404);
        const admin = t3Admin({ home: config.t3.home });
        return json(res, 200, method === 'GET' ? await admin.status() : await admin.update());
      }
      if (method === 'POST' && path === '/api/t3/pair') {
        requireEditor(user);
        if (!config.t3?.tokenFile) throw fail('T3_DISABLED', 'T3 Code is not configured', 404);
        return json(res, 200, await t3Pairing(config.t3, user));
      }
      if (path === '/api/admin/invitations' && method === 'GET') {
        requireAdmin(user);
        return json(res, 200, { invitations: invitations.list() });
      }
      if (path === '/api/admin/invitations' && method === 'POST') {
        requireAdmin(user);
        return json(res, 201, await invitations.create(body, user));
      }
      const invitationAction = /^\/api\/admin\/invitations\/([0-9a-f]{24})\/(resend|revoke)$/.exec(path);
      if (invitationAction && method === 'POST') {
        requireAdmin(user);
        const [, id, action] = invitationAction;
        return json(
          res,
          200,
          action === 'resend' ? await invitations.resend(id, user) : { invitation: invitations.revoke(id) },
        );
      }
      if (path === '/api/admin/status' && method === 'GET') {
        requireAdmin(user);
        return json(res, 200, {
          sync: store.syncStatus,
          stats: store.getStats(),
          pending: store.db.prepare("SELECT count(*) n FROM actions WHERE status IN ('pending','uncertain')").get().n,
          databasePath: config.databasePath,
          workbookId: store.sheets.spreadsheetId,
        });
      }
      if (path === '/api/admin/recover' && method === 'POST') {
        requireAdmin(user);
        return json(res, 200, await store.recoverPending());
      }
      if (/^\/api\/admin\/actions\/[^/]+\/resolve$/.test(path) && method === 'POST') {
        requireAdmin(user);
        return json(res, 200, { action: store.resolveAction(path.split('/')[4], body.status) });
      }
      if (path === '/api/admin/backup' && method === 'POST') {
        requireAdmin(user);
        requireId(body);
        if (config.databasePath === ':memory:')
          throw fail('BACKUP_UNAVAILABLE', 'In-memory database cannot be backed up', 409);
        const path = resolve(
          dirname(config.databasePath),
          `backup-${new Date().toISOString().replace(/[:.]/g, '-')}.sqlite`,
        );
        await backup(store.db, path);
        chmodSync(path, 0o600);
        return json(res, 200, { path, createdAt: now() });
      }
      if (assistant) {
        // The page asking (lib/api.ts): Cambios propuestos skips the list it already has after its own edits.
        const page = String(req.headers['x-ithomiini-page'] ?? '').slice(0, 64) || null;
        const answer = await assistant.handle({ method, path, body, user, query, page, headers: req.headers });
        if (answer?.tagged) return sendTagged(res, answer.body);
        // Bytes (a proposal's notebook photo), or JSON.
        if (answer && 'raw' in answer) {
          res.writeHead(answer.status || 200, answer.headers);
          return res.end(answer.raw ?? undefined);
        }
        if (answer) return json(res, answer.status || 200, answer.body, answer.headers);
      }
      throw fail('NOT_FOUND', 'Route not found', 404);
    } catch (e) {
      const status = Number(e.status) || 500;
      if (status >= 500) console.error(`[${requestId}]`, e.stack || e.message);
      return json(res, status, {
        error: {
          code: e.code || 'SERVER_ERROR',
          message: status >= 500 && !e.code ? 'Server error' : e.message,
          // Its descriptor, for the interface language (server/messages.mjs).
          ...(e.messageMsg && (status < 500 || e.code) ? { messageMsg: e.messageMsg } : {}),
          ...(e.details ? { details: e.details } : {}),
        },
      });
    }
  });
  const ready = options.skipInitialSync
    ? Promise.resolve(store.syncStatus)
    : new Promise(resolve => setImmediate(resolve))
        .then(() => store.sync())
        .then(() => store.recoverPending())
        .catch(e => {
          console.error('Initial sync failed:', e.message);
          return store.syncStatus;
        });
  const interval =
    config.syncIntervalMs > 0
      ? setInterval(
          () => store.sync().catch(e => console.error('Scheduled sync failed:', e.message)),
          config.syncIntervalMs,
        )
      : null;
  interval?.unref();
  return {
    server,
    store,
    ready,
    listen: async (port = config.port, host = config.host) => {
      if (t3Bridge && config.t3.proxyPort && !t3Proxy)
        t3Proxy = await t3Bridge.listen(config.t3.proxyPort).then(
          proxy => (console.log(`T3 Code (${config.t3.local}) with the bridge on 127.0.0.1:${config.t3.proxyPort}`), proxy),
          e => console.error('T3 proxy:', e.message),
        );
      return new Promise(resolve => server.listen(port, host, () => resolve(server.address())));
    },
    close: async () => {
      if (interval) clearInterval(interval);
      if (t3Proxy) {
        t3Proxy.closeAllConnections();
        await new Promise(resolve => t3Proxy.close(resolve));
      }
      t3Bridge?.close();
      await new Promise(resolve => server.close(resolve));
      store.close();
    },
  };
}

/** The id of T3's environment (its chats' addresses start with it), read once it exists. */
const t3Environments = new Map();
function t3EnvironmentId(t3) {
  if (!t3?.home) return null;
  if (t3Environments.has(t3.home)) return t3Environments.get(t3.home);
  let id = null;
  try {
    id = readFileSync(join(t3.home, 'userdata', 'environment-id'), 'utf8').trim();
  } catch {
    return null;
  }
  if (!/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(id)) return null;
  t3Environments.set(t3.home, id);
  return id;
}

/**
 * A one-time T3 Code sign-in link for the person, made with the broker's admin
 * token (T3 is a stock install: its own pairing flow, nothing patched).
 */
async function t3Pairing(t3, user) {
  const token = readFileSync(t3.tokenFile, 'utf8').trim();
  const response = await fetch(`${t3.local}/api/auth/pairing-token`, {
    method: 'POST',
    headers: { authorization: `Bearer ${token}`, 'content-type': 'application/json' },
    body: JSON.stringify({
      label: `ithomiini:${user.username}`,
      scopes: ['orchestration:read', 'orchestration:operate', 'terminal:operate', 'review:write', 'relay:read'],
    }),
    signal: AbortSignal.timeout(10000),
  });
  if (!response.ok) throw fail('T3_UNAVAILABLE', `T3 Code answered ${response.status}`, 502);
  const { credential, expiresAt } = await response.json();
  return { url: `${t3.url}/pair#token=${encodeURIComponent(credential)}`, expiresAt };
}

function serveStatic(req, res, path, base, frameSrc = '') {
  if (req.method !== 'GET' && req.method !== 'HEAD') throw fail('METHOD_NOT_ALLOWED', 'Method not allowed', 405);
  const clean = decodeURIComponent(path).replace(/^\/+/, ''),
    target = resolve(webRoot, clean || 'index.html');
  if (!target.startsWith(webRoot + '/') && target !== webRoot) throw fail('NOT_FOUND', 'File not found', 404);
  const found = existsSync(target) && statSync(target).isFile();
  // A missing script or style (a page opened before a deploy asks for the old build's files) is a 404,
  // not the page itself: served as HTML it broke the page instead of letting it reload.
  if (!found && /^assets\//.test(clean)) throw fail('NOT_FOUND', 'File not found', 404);
  const file = found ? target : join(webRoot, 'index.html');
  if (!existsSync(file)) throw fail('NOT_FOUND', 'Frontend is not built', 404);
  res.writeHead(200, {
    'content-type': mime[extname(file)] || 'application/octet-stream',
    'x-content-type-options': 'nosniff',
    'content-security-policy': `default-src 'self'; object-src 'none'; base-uri 'self'; frame-ancestors 'none'; img-src 'self' data: blob: https:; connect-src 'self'; font-src 'self'; style-src 'self' 'unsafe-inline'; script-src 'self'; worker-src 'self'${frameSrc ? `; frame-src ${frameSrc}` : ''}`,
    // Built scripts and styles carry a content hash in their name: a new build gets new names.
    'cache-control': file.endsWith('index.html')
      ? 'no-cache'
      : found && /^assets\//.test(clean)
        ? 'public, max-age=31536000, immutable'
        : 'public, max-age=3600',
  });
  if (req.method === 'HEAD') return res.end();
  createReadStream(file).pipe(res);
}
function relatedRecords(store, record) {
  const identityField =
    /^(CAM_ID(?:_.*)?|Insectary_ID|FieldMark_ID|Tube_(?:[1-5]_)?id|CLUTCH NUMBER|Clutch_No\.|Female|Male|Mother ID|Father ID|female Id|male Id|male_ID|female_ID)$/i;
  const normalize = value =>
    String(value ?? '')
      .trim()
      .toUpperCase();
  const ids = [
    ...new Set(
      Object.entries(record.values)
        .filter(
          ([key, value]) =>
            identityField.test(key) &&
            value != null &&
            normalize(value).length >= 2 &&
            !['NA', 'N/A'].includes(normalize(value)),
        )
        .map(([, value]) => normalize(value)),
    ),
  ].slice(0, 8);
  const seen = new Set([record.id]),
    related = [];
  for (const value of ids) {
    for (const hit of store.searchRecords({ q: value, limit: 150 }).records) {
      const exact = Object.entries(hit.values).some(
        ([key, candidate]) => identityField.test(key) && normalize(candidate) === value,
      );
      if (exact && !seen.has(hit.id)) {
        seen.add(hit.id);
        related.push(hit);
      }
      if (related.length >= 30) return related;
    }
  }
  return related;
}
function addEvent(store, body, user) {
  if (typeof body.kind !== 'string' || !body.kind.trim()) throw fail('INVALID_EVENT', 'Event kind required');
  const old = store.db.prepare('SELECT * FROM events WHERE request_id=?').get(body.requestId);
  if (old)
    return {
      id: old.id,
      kind: old.kind,
      recordId: old.record_id,
      values: JSON.parse(old.values_json),
      actor: old.actor,
      createdAt: old.created_at,
      source: 'app',
    };
  if (body.recordId && !store.getRecord(body.recordId)) throw fail('RECORD_NOT_FOUND', 'Record not found', 404);
  const id = randomUUID(),
    stamp = now();
  store.db
    .prepare('INSERT INTO events(id,kind,record_id,values_json,actor,created_at,request_id) VALUES(?,?,?,?,?,?,?)')
    .run(id, body.kind, body.recordId || null, JSON.stringify(body.values || {}), user.id, stamp, body.requestId);
  return {
    id,
    kind: body.kind,
    recordId: body.recordId || null,
    values: body.values || {},
    actor: user.id,
    createdAt: stamp,
    source: 'app',
  };
}
const ACTION_TYPES = new Set(['death', 'preservation', 'collection', 'emergence', 'tubes', 'correction', 'edit']);

/**
 * Named record actions from older clients. Every value must be a real sheet
 * column: unknown fields are rejected instead of being kept only in the app.
 * The history source is always "app"; the action type is kept in the reason.
 */
async function domainAction(store, body, user) {
  const type = String(body.type || '').trim();
  if (!ACTION_TYPES.has(type)) throw fail('INVALID_ACTION', `Unknown action type: ${type || '(empty)'}`);
  const reason = body.reason ? `${type}: ${body.reason}` : type;
  if (body.recordId && ['death', 'preservation'].includes(type)) {
    const source = store.getRecord(body.recordId);
    if (!source) throw fail('RECORD_NOT_FOUND', 'Record not found', 404);
    if (source.sheet === 'Collection_data') {
      // Death and preservation cells in Collection_data are formulas reading
      // Insectary_data, so the event is written to the linked insectary row.
      const link = source.values.Insectary_ID;
      const matches = link
        ? store
            .searchRecords({ module: 'Insectary_data', q: String(link), limit: 100 })
            .records.filter(r => r.values.Insectary_ID === link)
        : [];
      if (matches.length !== 1)
        throw fail('IDENTITY_CONFLICT', 'Choose the linked insectary record before recording this event', 409, {
          matches: matches.map(r => ({ id: r.id, row: r.row, label: r.label })),
        });
      const map = {
        Death_date: 'Death_date',
        Death_cause: 'Death_cause',
        Preservation_date: 'Preservation_date',
        Preservation_medium: 'Preservation_medium',
        Preserved_dead_alive: 'Preserved_Dead_Alive',
        Preserved_Dead_Alive: 'Preserved_Dead_Alive',
      };
      const values = {};
      for (const [key, value] of Object.entries(body.values || {})) {
        if (!map[key]) throw fail('INVALID_FIELD', `${key} has no sheet-backed insectary destination`);
        values[map[key]] = value;
      }
      const result = await applyBatch(
        store,
        {
          requestId: body.requestId,
          reason: `${reason} (linked from Collection_data row ${source.row})`,
          edits: [{ id: matches[0].id, values, expectedVersion: body.expectedVersion }],
        },
        user,
      );
      return { ...result, sourceRecordId: source.id };
    }
  }
  const items = Array.isArray(body.records)
    ? body.records
    : [body.recordId ? { id: body.recordId, values: body.values } : { module: body.module, values: body.values }];
  return applyBatch(
    store,
    {
      requestId: body.requestId,
      reason,
      edits: items
        .filter(item => item.id)
        .map(item => ({
          id: item.id,
          values: item.values,
          expectedVersion: item.expectedVersion ?? body.expectedVersion,
        })),
      creates: items
        .filter(item => !item.id)
        .map(item => ({ module: item.module || body.module, values: item.values })),
    },
    user,
  );
}
function optionsFor(store, module, field, q = '', species = '') {
  const mod = moduleMap.get(module);
  if (!mod || !mod.fields.some(f => f.key === field)) throw fail('INVALID_FIELD', 'Known module and field required');
  const rows = store.db
    .prepare('SELECT values_json FROM records WHERE sheet=? AND missing=0 AND observed=1 ORDER BY row_num DESC')
    .all(module);
  const counts = new Map();
  const add = value => {
    if (
      value != null &&
      value !== '' &&
      !['NA', 'N/A'].includes(String(value).trim().toUpperCase()) &&
      String(value).toLowerCase().includes(String(q).toLowerCase())
    )
      counts.set(String(value), (counts.get(String(value)) || 0) + 1);
  };
  for (const row of rows) {
    const values = JSON.parse(row.values_json);
    if (field === 'Subspecies_Form' && species && values.SPECIES !== species) continue;
    add(values[field]);
  }
  const listField = { Identifier: 'Abbr_name', Collector: 'Abbr_name', CAM_ID_insectary: 'InsectaryWild&Reared_CAMid' }[
    field
  ];
  if (listField)
    for (const row of store.db
      .prepare('SELECT values_json FROM records WHERE sheet=? AND missing=0 ORDER BY row_num')
      .all('Lists'))
      add(JSON.parse(row.values_json)[listField]);
  if (field === 'Collection_location')
    for (const row of store.db
      .prepare('SELECT values_json FROM records WHERE sheet=? AND missing=0 ORDER BY row_num')
      .all('Location_data'))
      add(JSON.parse(row.values_json).Collection_location);
  if (field === 'Sex') for (const value of ['male', 'female', 'unknown']) add(value);
  let options = [...counts].map(([value, count]) => ({ value, count }));
  if (field === 'CAM_ID_insectary') {
    const numeric = value => Number(/^CAM(\d+)$/i.exec(String(value))?.[1] ?? -1);
    const last = Math.max(-1, ...rows.map(row => numeric(JSON.parse(row.values_json)[field])));
    options.sort(
      (a, b) =>
        (numeric(a.value) > last ? 0 : 1) - (numeric(b.value) > last ? 0 : 1) || numeric(a.value) - numeric(b.value),
    );
  }
  return options.slice(0, 100);
}
function suggestionsFor(store, module) {
  const mod = moduleMap.get(module);
  if (!mod) throw fail('MODULE_NOT_FOUND', 'Unknown module', 404);
  const recent = store.db
    .prepare(
      'SELECT values_json FROM records WHERE sheet=? AND missing=0 AND observed=1 ORDER BY row_num DESC LIMIT 500',
    )
    .all(module)
    .map(r => JSON.parse(r.values_json));
  const defaults = {
    Collection_data: ['Country', 'Side_Andes', 'Collection_location', 'Transect_section', 'Collector', 'Identifier'],
    Insectary_data: ['Collection_location'],
    SamplingDay_data: ['Location', 'Collectors_initials', 'DataLogger'],
  };
  const values = {};
  for (const key of defaults[module] || []) {
    const latest = recent.find(r => r[key] != null && r[key] !== '');
    if (latest) values[key] = latest[key];
  }
  if (module === 'Insectary_data') values.Insectary_ID = store.suggestInsectaryId();
  if (['Collection_data', 'Insectary_data'].includes(module)) {
    const camIds = store.db
      .prepare('SELECT values_json FROM records WHERE sheet=? AND missing=0 AND observed=1 ORDER BY row_num DESC')
      .all(module)
      .map(r => JSON.parse(r.values_json).CAM_ID);
    const latestCam = camIds.find(id => /^CAM\d+$/i.test(id || ''));
    if (latestCam) {
      const used = new Set(camIds);
      let n = Number(latestCam.slice(3)),
        candidate;
      do {
        candidate = `CAM${String(++n).padStart(latestCam.length - 3, '0')}`;
      } while (used.has(candidate));
      values.CAM_ID = candidate;
    }
  }
  const options = Object.fromEntries(
    mod.fields
      .filter(f =>
        /(?:species|subspecies|location|collector|identifier|CAM_ID_insectary|rainfall|cloud_cover|sex|medium|purpose|stock|cage)/i.test(
          f.key,
        ),
      )
      .map(f => [f.key, optionsFor(store, module, f.key)]),
  );
  if (['Collection_data', 'Insectary_data'].includes(module)) {
    const recentTube = recent.flatMap(r =>
      Object.entries(r)
        .filter(([k, v]) => /^Tube_[1-5]_id$/.test(k) && /^FS\d+$/i.test(v || ''))
        .map(([, v]) => v),
    )[0];
    if (recentTube) {
      const used = new Set();
      for (const sheet of ['Collection_data', 'Insectary_data'])
        for (const row of store.db
          .prepare('SELECT values_json FROM records WHERE sheet=? AND missing=0 AND observed=1')
          .all(sheet))
          for (const [k, v] of Object.entries(JSON.parse(row.values_json)))
            if (/^Tube_[1-5]_id$/.test(k) && /^FS\d+$/i.test(v || '')) used.add(v);
      let n = Number(recentTube.slice(2));
      const tubeIds = [];
      while (tubeIds.length < 100) {
        const candidate = `FS${String(++n).padStart(recentTube.length - 2, '0')}`;
        if (!used.has(candidate)) tubeIds.push(candidate);
      }
      options.tubeIds = tubeIds;
      for (const field of mod.fields.filter(f => /^Tube_[1-5]_id$/.test(f.key))) options[field.key] = tubeIds;
    }
  }
  let target = null;
  const identityKey = mod.identityFields.find(key => values[key] != null);
  if (identityKey) {
    const matches = store.db
      .prepare(
        'SELECT * FROM records WHERE sheet=? AND missing=0 AND observed=0 AND json_extract(values_json,?)=? LIMIT 2',
      )
      .all(module, `$.${JSON.stringify(identityKey)}`, values[identityKey]);
    if (matches.length === 1) target = store.hydrate(matches[0]);
  }
  const formulas = target?.formulas || {};
  for (const key of Object.keys(formulas)) if (key !== 'Insectary_ID' && key !== 'CAM_ID') delete values[key];
  return { values, options, readonlyFields: Object.keys(formulas), formulas, targetRow: target?.row || null };
}
function previewImport(store, body, user) {
  const mod = moduleMap.get(body.module);
  if (!mod) throw fail('MODULE_NOT_FOUND', 'Unknown module', 404);
  if (typeof body.csv !== 'string' || body.csv.length > 5_000_000) throw fail('INVALID_CSV', 'CSV must be under 5 MB');
  const data = csvRows(body.csv),
    headers = data.shift() || [],
    errors = [],
    rows = [],
    seen = new Set();
  if (new Set(headers).size !== headers.length)
    errors.push({ row: 1, message: 'CSV header contains duplicate fields' });
  for (const [i, cells] of data.entries()) {
    if (cells.length !== headers.length) {
      errors.push({ row: i + 2, message: 'Column count differs from header' });
      continue;
    }
    const values = {};
    for (let c = 0; c < headers.length; c++) {
      const field = mod.fields.find(f => f.key === headers[c]);
      if (!field) errors.push({ row: i + 2, field: headers[c], message: 'Unknown field' });
      else if (cells[c] !== '') {
        const n = field.type === 'number' ? Number(cells[c]) : null;
        if (field.type === 'number' && !Number.isFinite(n))
          errors.push({ row: i + 2, field: headers[c], message: 'Number is invalid' });
        else if (field.type === 'date') {
          const text = cells[c].trim();
          if (/^\d{4}-\d{2}-\d{2}$/.test(text)) {
            const ms = Date.parse(`${text}T00:00:00Z`);
            if (!Number.isFinite(ms) || new Date(ms).toISOString().slice(0, 10) !== text)
              errors.push({ row: i + 2, field: headers[c], message: 'Date must be a real YYYY-MM-DD date' });
            else values[headers[c]] = (ms - Date.UTC(1899, 11, 30)) / 86_400_000;
          } else if (Number.isFinite(Number(text))) values[headers[c]] = Number(text);
          else errors.push({ row: i + 2, field: headers[c], message: 'Date must be YYYY-MM-DD or a Sheets serial' });
        } else values[headers[c]] = field.type === 'number' ? n : cells[c];
      }
    }
    try {
      validateValues(body.module, values);
    } catch (e) {
      errors.push({ row: i + 2, message: e.message });
    }
    const key = mod.identityFields.find(k => values[k]);
    if (key) {
      const marker = `${key}:${values[key]}`;
      const duplicate = store.db
        .prepare('SELECT 1 FROM records WHERE sheet=? AND missing=0 AND json_extract(values_json,?)=? LIMIT 1')
        .get(mod.id, `$.${JSON.stringify(key)}`, values[key]);
      if (duplicate || seen.has(marker))
        errors.push({ row: i + 2, field: key, message: 'Possible existing identifier; review before import' });
      seen.add(marker);
    }
    if (Object.keys(values).length) rows.push(values);
  }
  const id = randomUUID();
  store.db
    .prepare('INSERT INTO import_previews(id,module,rows_json,errors_json,actor,created_at) VALUES(?,?,?,?,?,?)')
    .run(id, body.module, JSON.stringify(rows), JSON.stringify(errors), user.id, now());
  return { previewId: id, rows: rows.slice(0, 50), rowCount: rows.length, errors };
}
async function applyImport(store, body, user) {
  const preview = store.db.prepare('SELECT * FROM import_previews WHERE id=?').get(body.previewId);
  if (!preview || preview.actor !== user.id) throw fail('PREVIEW_NOT_FOUND', 'Import preview not found', 404);
  if (preview.applied) throw fail('IMPORT_APPLIED', 'Import was already applied', 409);
  const errors = JSON.parse(preview.errors_json);
  if (errors.length) throw fail('IMPORT_INVALID', 'Import has validation errors', 409, { errors });
  const rows = JSON.parse(preview.rows_json),
    results = [];
  for (let i = 0; i < rows.length; i++) {
    try {
      results.push(
        await store.createRecord(
          { module: preview.module, values: rows[i], requestId: `${body.requestId}:${i}`, reason: 'CSV import' },
          user,
          'import',
        ),
      );
    } catch (e) {
      if (results.length)
        throw fail('PARTIAL_IMPORT', 'Some rows were saved; review them before retrying', 409, {
          applied: results.map(x => ({ recordId: x.record?.id, actionId: x.action?.id, status: x.status })),
          failedIndex: i,
          cause: e.code || 'ERROR',
        });
      throw e;
    }
  }
  store.db.prepare('UPDATE import_previews SET applied=1 WHERE id=?').run(preview.id);
  return { created: results.length, records: results.map(r => r.record), status: 'verified' };
}
function listAttachments(store, recordId) {
  const rows = recordId
    ? store.db.prepare('SELECT * FROM attachments WHERE record_id=? ORDER BY created_at DESC').all(recordId)
    : store.db.prepare('SELECT * FROM attachments ORDER BY created_at DESC LIMIT 100').all();
  return rows.map(r => ({
    id: r.id,
    recordId: r.record_id,
    name: r.name,
    mimeType: r.mime_type,
    size: r.data.length,
    createdBy: r.created_by,
    createdAt: r.created_at,
  }));
}
function addAttachment(store, body, user) {
  if (body.recordId && !store.getRecord(body.recordId)) throw fail('RECORD_NOT_FOUND', 'Record not found', 404);
  const allowed = new Set([
    'image/jpeg',
    'image/png',
    'image/webp',
    'application/pdf',
    'audio/mpeg',
    'audio/mp4',
    'audio/webm',
    'audio/ogg',
    'text/plain',
  ]);
  if (!allowed.has(body.mimeType)) throw fail('INVALID_MIME', 'File type is not supported');
  if (
    typeof body.dataBase64 !== 'string' ||
    body.dataBase64.length > 14_000_000 ||
    !/^[-A-Za-z0-9+/=\s]+$/.test(body.dataBase64)
  )
    throw fail('INVALID_ATTACHMENT', 'Invalid attachment data');
  const data = Buffer.from(body.dataBase64, 'base64');
  if (!data.length || data.length > 10 * 1024 * 1024)
    throw fail('INVALID_ATTACHMENT', 'Attachment must be under 10 MB');
  const name = String(body.name || 'attachment')
      .replace(/[\\/\0\r\n]/g, '_')
      .slice(0, 160),
    id = randomUUID(),
    stamp = now();
  store.db
    .prepare('INSERT INTO attachments VALUES(?,?,?,?,?,?,?)')
    .run(id, body.recordId || null, name, body.mimeType, data, user.id, stamp);
  return {
    id,
    recordId: body.recordId || null,
    name,
    mimeType: body.mimeType,
    size: data.length,
    createdBy: user.id,
    createdAt: stamp,
  };
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const app = await createApp(configFromEnv());
  const address = await app.listen();
  console.log(
    `Ithomiini app listening on ${address.address}:${address.port}${app.store.config.basePath || '/ithomiini'}`,
  );
}
