import { createHash, randomBytes, randomUUID } from 'node:crypto';
import { readFile, readdir, stat } from 'node:fs/promises';
import { basename, extname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createReports } from './reports.mjs';
import { TYPED_OVER_FORMULA } from './batch.mjs';
import { comparable, moduleMap, validateValues } from './schema.mjs';
import { claudeAllowed, claudeConfig, prepareWorkspace, runClaude } from './claude.mjs';

const here = fileURLToPath(new URL('.', import.meta.url));
const bad = (status, code, message) => ({ status, body: { error: { code, message } } });
const now = () => new Date().toISOString();
const clip = (value, length = 1200) => String(value ?? '').slice(0, length);
const owner = user => String(user?.id ?? user?.username ?? '');
const json = value => JSON.stringify(value);
const EDITORS = ['editor', 'reviewer', 'admin'];
const isoDate = serial => new Date(Date.UTC(1899, 11, 30) + serial * 864e5).toISOString().slice(0, 10);
const parse = value => {
  try {
    return JSON.parse(value);
  } catch {
    return null;
  }
};

const TOOLS = [
  {
    type: 'function',
    function: {
      name: 'search_records',
      description: 'Search workbook rows by free text. Returns a small list with sheet, row and app ID.',
      parameters: {
        type: 'object',
        properties: { query: { type: 'string' }, module: { type: 'string' } },
        required: ['query'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'find_records',
      description:
        'Fetch many rows at once by exact identifier, e.g. the Insectary_IDs read from a notebook page. Returns found rows (non-empty typed values; dates as YYYY-MM-DD) and the identifiers not found.',
      parameters: {
        type: 'object',
        properties: {
          module: { type: 'string', description: 'Sheet, e.g. Insectary_data, Collection_data, Insectary_stocks' },
          field: { type: 'string', description: 'Column to match, e.g. Insectary_ID, CAM_ID, CLUTCH NUMBER' },
          values: { type: 'array', items: { type: 'string' }, description: 'Up to 150 identifiers' },
        },
        required: ['module', 'field', 'values'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'get_record',
      description: 'Fetch one row by app ID, including its sheet row and version.',
      parameters: { type: 'object', properties: { id: { type: 'string' } }, required: ['id'] },
    },
  },
  {
    type: 'function',
    function: {
      name: 'describe_sheet',
      description:
        'Columns of a sheet with their type, which ones are formulas, the values in use for short-list columns, and the latest rows.',
      parameters: { type: 'object', properties: { module: { type: 'string' } }, required: ['module'] },
    },
  },
  {
    type: 'function',
    function: {
      name: 'search_knowledge',
      description: 'Search approved local protocols and meeting notes.',
      parameters: { type: 'object', properties: { query: { type: 'string' } }, required: ['query'] },
    },
  },
  {
    type: 'function',
    function: {
      name: 'run_report',
      description: 'Produce a sourced count, stage, cross, sample, quality or weekly report.',
      parameters: {
        type: 'object',
        properties: {
          kind: { type: 'string', enum: ['overview', 'counts', 'stages', 'crosses', 'samples', 'quality', 'weekly'] },
          module: { type: 'string' },
          field: { type: 'string' },
        },
        required: ['kind'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'propose_changes',
      description:
        'Draft edits to existing rows. They appear to the person as a table with the changed cells highlighted and are only written when the person confirms. Formula cells cannot be changed, except SPECIES in Insectary_data when what emerged differs from the formula prediction. Give a short note per row saying where the value comes from.',
      parameters: {
        type: 'object',
        properties: {
          changes: {
            type: 'array',
            items: {
              type: 'object',
              properties: {
                recordId: { type: 'string' },
                values: { type: 'object', description: 'Column → new value; dates as YYYY-MM-DD' },
                note: { type: 'string' },
              },
              required: ['recordId', 'values'],
            },
          },
          reason: { type: 'string' },
        },
        required: ['changes', 'reason'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'apply_proposal',
      description:
        "Write a pending proposal to Google Sheets. Only call this when the person's latest message explicitly approves it (e.g. 'sí, aplícalo', 'está correcto'). Optionally only some rows, by their index.",
      parameters: {
        type: 'object',
        properties: { proposalId: { type: 'string' }, indexes: { type: 'array', items: { type: 'integer' } } },
        required: ['proposalId'],
      },
    },
  },
];

function init(db) {
  db.exec(`CREATE TABLE IF NOT EXISTS ai_threads (
    id TEXT PRIMARY KEY, owner_id TEXT NOT NULL, title TEXT NOT NULL, created_at TEXT NOT NULL, updated_at TEXT NOT NULL
  );
  CREATE INDEX IF NOT EXISTS ai_threads_owner ON ai_threads(owner_id, updated_at);
  CREATE TABLE IF NOT EXISTS ai_messages (
    id TEXT PRIMARY KEY, thread_id TEXT NOT NULL REFERENCES ai_threads(id) ON DELETE CASCADE,
    role TEXT NOT NULL, content TEXT NOT NULL, sources_json TEXT NOT NULL DEFAULT '[]',
    results_json TEXT NOT NULL DEFAULT '[]', proposals_json TEXT NOT NULL DEFAULT '[]', created_at TEXT NOT NULL
  );
  CREATE INDEX IF NOT EXISTS ai_messages_thread ON ai_messages(thread_id, created_at);
  CREATE TABLE IF NOT EXISTS ai_proposals (
    id TEXT PRIMARY KEY, thread_id TEXT NOT NULL REFERENCES ai_threads(id) ON DELETE CASCADE,
    owner_id TEXT NOT NULL, changes_json TEXT NOT NULL, reason TEXT, status TEXT NOT NULL,
    created_at TEXT NOT NULL, applied_at TEXT
  );`);
  const has = (table, column) =>
    db
      .prepare(`PRAGMA table_info(${table})`)
      .all()
      .some(c => c.name === column);
  if (!has('ai_threads', 'claude_session')) db.exec('ALTER TABLE ai_threads ADD COLUMN claude_session TEXT');
  if (!has('ai_messages', 'attachments_json'))
    db.exec("ALTER TABLE ai_messages ADD COLUMN attachments_json TEXT NOT NULL DEFAULT '[]'");
  if (!has('ai_proposals', 'applied_json')) db.exec('ALTER TABLE ai_proposals ADD COLUMN applied_json TEXT');
  // Personal tokens for agents outside the app (T3 Code projects) to use the same tools.
  db.exec(`CREATE TABLE IF NOT EXISTS ai_tokens (
    token_hash TEXT PRIMARY KEY, user_id TEXT NOT NULL, label TEXT NOT NULL, created_at TEXT NOT NULL, revoked_at TEXT
  )`);
  // A restarted process cannot know whether an in-flight source write completed.
  db.prepare("UPDATE ai_proposals SET status = 'needs_review' WHERE status = 'applying'").run();
}

async function keyFor(ai) {
  if (ai.apiKeyFile) return (await readFile(ai.apiKeyFile, 'utf8')).trim();
  return ai.apiKey ?? '';
}

function providerConfig(config) {
  const ai = config.ai ?? {};
  return {
    baseUrl: ai.baseUrl ?? process.env.ITHOMIINI_AI_BASE_URL ?? 'https://api.openai.com/v1',
    model: ai.model ?? process.env.ITHOMIINI_AI_MODEL ?? '',
    apiKeyFile: ai.apiKeyFile ?? process.env.ITHOMIINI_AI_API_KEY_FILE,
    apiKey: ai.apiKey ?? process.env.ITHOMIINI_AI_API_KEY,
    transcriptionModel: ai.transcriptionModel ?? process.env.ITHOMIINI_AI_TRANSCRIPTION_MODEL,
    transcriptionMode:
      ai.transcriptionMode ??
      process.env.ITHOMIINI_AI_TRANSCRIPTION_MODE ??
      (String(ai.baseUrl ?? process.env.ITHOMIINI_AI_BASE_URL ?? '').includes('openrouter.ai') ? 'chat' : 'endpoint'),
    visionModel: ai.visionModel ?? process.env.ITHOMIINI_AI_VISION_MODEL,
  };
}

function endpoint(ai, path) {
  const base = new URL(ai.baseUrl.endsWith('/') ? ai.baseUrl : `${ai.baseUrl}/`);
  if (
    base.protocol !== 'https:' &&
    !(base.protocol === 'http:' && ['localhost', '127.0.0.1', '[::1]'].includes(base.hostname))
  )
    throw new Error('AI endpoint must use HTTPS or local HTTP');
  return new URL(path, base).toString();
}

async function providerFetch(ai, path, body, multipart = false) {
  const key = await keyFor(ai);
  if (!key || !ai.model) throw new Error('AI provider is not configured');
  const response = await fetch(endpoint(ai, path), {
    method: 'POST',
    headers: { Authorization: `Bearer ${key}`, ...(multipart ? {} : { 'Content-Type': 'application/json' }) },
    body: multipart ? body : json(body),
    signal: AbortSignal.timeout(45000),
  });
  if (!response.ok) throw new Error(`AI provider returned HTTP ${response.status}`);
  const payload = await response.json();
  return payload;
}

async function complete(ai, messages, tools = TOOLS, model = ai.model) {
  const payload = await providerFetch({ ...ai, model }, 'chat/completions', {
    model,
    messages,
    ...(tools?.length ? { tools, tool_choice: 'auto' } : {}),
  });
  const message = payload?.choices?.[0]?.message;
  if (!message || (typeof message.content !== 'string' && !Array.isArray(message.tool_calls)))
    throw new Error('AI provider returned no message');
  return message;
}

function recordSource(record) {
  return {
    id: record.id,
    type: 'record',
    sheet: record.sheet,
    row: record.row,
    version: record.version,
    label: record.label,
    sourceUrl: record.sourceUrl ?? null,
  };
}

async function knowledgeFiles(config) {
  const roots =
    config.knowledgeRoots ??
    (config.knowledgeRoot
      ? [config.knowledgeRoot]
      : process.env.KNOWLEDGE_DIR
        ? [process.env.KNOWLEDGE_DIR]
        : [join(here, '..', 'docs', 'meetings.md'), join(here, '..', 'docs', 'workflows.md')]);
  const paths = [];
  for (const configured of roots) {
    const root = resolve(configured);
    let info;
    try {
      info = await stat(root);
    } catch {
      continue;
    }
    if (info.isFile()) {
      paths.push(root);
      continue;
    }
    if (!info.isDirectory()) continue;
    for (const item of (await readdir(root, { withFileTypes: true })).slice(0, 300)) {
      if (item.isFile() && ['.md', '.txt'].includes(extname(item.name).toLowerCase()))
        paths.push(join(root, item.name));
    }
  }
  return paths.slice(0, 400);
}

async function documents(config) {
  const entries = [];
  for (const path of await knowledgeFiles(config)) {
    let info;
    try {
      info = await stat(path);
    } catch {
      continue;
    }
    if (!info.isFile() || info.size > 150000) continue;
    const text = await readFile(path, 'utf8');
    const front = /^---\n([\s\S]*?)\n---\n/.exec(text);
    const metadata = Object.fromEntries(
      (front?.[1] ?? '')
        .split('\n')
        .map(line => /^([A-Za-z][\w-]*):\s*(.*)$/.exec(line))
        .filter(Boolean)
        .map(match => [match[1], match[2].replace(/^['"]|['"]$/g, '')]),
    );
    const content = front ? text.slice(front[0].length) : text;
    const id = createHash('sha256').update(path).digest('hex').slice(0, 20);
    const name = basename(path);
    const driveId = /^([A-Za-z0-9_-]{25,})\.txt$/.exec(name)?.[1];
    entries.push({
      id,
      title: metadata.title ?? content.match(/^#\s+(.+)$/m)?.[1] ?? name,
      text: content,
      sourceUrl:
        metadata.sourceUrl ?? (driveId ? `https://docs.google.com/document/d/${driveId}/edit` : `/api/knowledge/${id}`),
    });
  }
  return entries;
}

function searchDocs(docs, query) {
  const terms = clip(query, 120)
    .toLowerCase()
    .split(/[^\p{L}\p{N}]+/u)
    .filter(term => term.length > 2)
    .slice(0, 8);
  if (!terms.length) return [];
  return docs
    .map(doc => {
      const content = doc.text.toLowerCase();
      const hits = terms.map(term => content.indexOf(term)).filter(index => index >= 0);
      const first = Math.min(...hits);
      return {
        doc,
        score: hits.length,
        snippet: Number.isFinite(first) ? clip(doc.text.slice(Math.max(0, first - 180), first + 800), 1000) : '',
      };
    })
    .filter(item => item.score)
    .sort((a, b) => b.score - a.score)
    .slice(0, 8)
    .map(({ doc, snippet }) => ({ id: doc.id, type: 'document', title: doc.title, snippet, sourceUrl: doc.sourceUrl }));
}

function publicMessage(row) {
  return {
    id: row.id,
    role: row.role,
    content: row.content,
    sources: parse(row.sources_json) ?? [],
    results: parse(row.results_json) ?? [],
    proposals: parse(row.proposals_json) ?? [],
    attachments: parse(row.attachments_json) ?? [],
    createdAt: row.created_at,
  };
}

export function createAssistant({ store, config = {} }) {
  if (!store?.db) throw new Error('Assistant requires store.db');
  const db = store.db;
  init(db);
  const ai = providerConfig(config);
  const claude = config.claude ?? claudeConfig();
  const mcpUrl =
    config.mcpUrl ??
    `http://127.0.0.1:${config.port ?? 8794}${config.basePath && config.basePath !== '/' ? config.basePath : ''}/api/ai/mcp`;
  if (claude.bin && claude.workspace)
    prepareWorkspace(claude, join(here, '..')).catch(e => console.error('Claude workspace:', e.message));
  const reports = createReports({ store, config });
  const thread = (id, user) =>
    db.prepare('SELECT * FROM ai_threads WHERE id = ? AND owner_id = ?').get(id, owner(user));
  const insertMessage = (threadId, role, content, sources = [], results = [], proposals = [], attachments = []) => {
    const message = {
      id: randomUUID(),
      thread_id: threadId,
      role,
      content,
      sources_json: json(sources),
      results_json: json(results),
      proposals_json: json(proposals),
      attachments_json: json(attachments),
      created_at: now(),
    };
    db.prepare(
      'INSERT INTO ai_messages (id,thread_id,role,content,sources_json,results_json,proposals_json,attachments_json,created_at) VALUES (?,?,?,?,?,?,?,?,?)',
    ).run(...Object.values(message));
    db.prepare('UPDATE ai_threads SET updated_at = ? WHERE id = ?').run(message.created_at, threadId);
    return publicMessage(message);
  };

  /** A row as the model sees it: typed values only (formula results are omitted), dates readable. */
  function compact(record) {
    const mod = moduleMap.get(record.sheet);
    const dates = new Set(mod?.fields.filter(f => f.type === 'date').map(f => f.key));
    const keepFormula = TYPED_OVER_FORMULA[record.sheet] ?? new Set();
    const values = {};
    for (const [key, value] of Object.entries(record.values ?? {})) {
      if (value === null || value === '') continue;
      if (record.formulas?.[key] && !keepFormula.has(key)) continue;
      values[key] = dates.has(key) && typeof value === 'number' ? isoDate(value) : value;
    }
    return {
      id: record.id,
      sheet: record.sheet,
      row: record.row,
      label: record.label,
      version: record.version,
      values,
      formulaColumns: Object.keys(record.formulas ?? {}),
    };
  }

  function findRecords(args, context) {
    const mod = moduleMap.get(String(args.module ?? ''));
    if (!mod) return { error: `Unknown sheet ${clip(args.module, 60)}` };
    const field = String(args.field ?? '');
    if (!mod.fields.some(f => f.key === field)) return { error: `Unknown column ${clip(field, 60)}` };
    if (!Array.isArray(args.values) || !args.values.length) return { error: 'Give at least one identifier' };
    const query = db.prepare(
      'SELECT id FROM records WHERE sheet = ? AND missing = 0 AND row_num > 0 AND lower(trim(CAST(json_extract(values_json, ?) AS TEXT))) = ?',
    );
    const found = [],
      missing = [];
    for (const raw of args.values.slice(0, 150)) {
      const value = String(raw).trim();
      const ids = query.all(mod.id, `$."${field.replaceAll('"', '')}"`, value.toLowerCase());
      if (!ids.length) missing.push(value);
      for (const { id } of ids) {
        const record = store.getRecord(id);
        context.records.set(record.id, record);
        context.sources.set(record.id, recordSource(record));
        found.push(compact(record));
      }
    }
    return { found, missing };
  }

  function describeSheet(args) {
    const mod = moduleMap.get(String(args.module ?? ''));
    if (!mod) return { error: `Unknown sheet ${clip(args.module, 60)}` };
    const recent = db
      .prepare(
        'SELECT id FROM records WHERE sheet = ? AND missing = 0 AND observed = 1 AND row_num > 0 ORDER BY row_num DESC LIMIT 1500',
      )
      .all(mod.id)
      .map(r => store.getRecord(r.id));
    const formulas = new Map(),
      options = new Map();
    for (const record of recent) {
      for (const key of Object.keys(record.formulas ?? {})) formulas.set(key, (formulas.get(key) ?? 0) + 1);
      for (const [key, value] of Object.entries(record.values ?? {}))
        if (typeof value === 'string' && value && !record.formulas?.[key]) {
          const seen = options.get(key) ?? new Map();
          seen.set(value, (seen.get(value) ?? 0) + 1);
          options.set(key, seen);
        }
    }
    return {
      sheet: mod.id,
      columns: mod.fields.map(f => {
        const seen = options.get(f.key);
        return {
          key: f.key,
          type: f.type,
          formula: (formulas.get(f.key) ?? 0) > recent.length / 2,
          ...(seen && seen.size <= 30 ? { values: [...seen.keys()] } : {}),
        };
      }),
      latestRows: recent.slice(0, 3).map(compact),
    };
  }

  function proposeChanges(args, context) {
    if (!EDITORS.includes(context.user.role)) return { error: 'Your role cannot propose edits' };
    if (!Array.isArray(args.changes) || !args.changes.length || args.changes.length > 100)
      return { error: 'Provide 1 to 100 changes' };
    const changes = [];
    for (const candidate of args.changes) {
      const old = store.getRecord(String(candidate?.recordId ?? ''));
      if (!old || old.missing) return { error: `Row ${clip(candidate?.recordId, 60)} not found; use find_records` };
      const raw = candidate.values;
      if (
        !raw ||
        typeof raw !== 'object' ||
        Array.isArray(raw) ||
        !Object.keys(raw).length ||
        Object.keys(raw).length > 20
      )
        return { error: `Invalid values for ${old.label}` };
      let values;
      try {
        values = validateValues(old.sheet, raw);
      } catch (e) {
        return { error: `${old.label}: ${e.message}` };
      }
      const before = {},
        replaceFormula = [];
      for (const key of Object.keys(values)) {
        if (old.formulas?.[key]) {
          if (!TYPED_OVER_FORMULA[old.sheet]?.has(key))
            return { error: `${old.label}: ${key} is calculated by a formula and cannot be changed` };
          if (comparable(old.values?.[key] ?? null) === comparable(values[key]))
            return { error: `${old.label}: the ${key} formula already gives ${values[key]}; leave it` };
          replaceFormula.push(key);
        }
        before[key] = old.values?.[key] ?? null;
      }
      if (Object.keys(values).every(key => comparable(before[key]) === comparable(values[key]))) continue;
      changes.push({
        recordId: old.id,
        sheet: old.sheet,
        row: old.row,
        label: old.label,
        expectedVersion: old.version,
        before,
        values,
        replaceFormula,
        note: clip(candidate.note, 300),
      });
    }
    if (!changes.length) return { error: 'Every proposed value is already in the sheet' };
    const id = randomUUID();
    db.prepare(
      'INSERT INTO ai_proposals (id,thread_id,owner_id,changes_json,reason,status,created_at) VALUES (?,?,?,?,?,?,?)',
    ).run(id, context.threadId, owner(context.user), json(changes), clip(args.reason, 500), 'pending', now());
    const proposal = { id, changes, reason: clip(args.reason, 500), status: 'pending' };
    context.proposals.push(proposal);
    return { proposalId: id, rows: changes.length, status: 'waiting for the person to confirm' };
  }

  /** Writes the chosen rows of a proposal as one save (undoable in Historial). */
  async function applyProposal(proposal, user, { requestId, indexes, reason }) {
    if (proposal.status !== 'pending')
      throw Object.assign(new Error('Proposal has already been applied.'), { status: 409, code: 'proposal_used' });
    if (!EDITORS.includes(user.role))
      throw Object.assign(new Error('Your role cannot apply changes.'), { status: 403, code: 'forbidden' });
    const all = parse(proposal.changes_json) ?? [];
    const chosen =
      Array.isArray(indexes) && indexes.length
        ? [...new Set(indexes.map(Number))].filter(i => all[i])
        : all.map((_, i) => i);
    if (!chosen.length) throw Object.assign(new Error('No rows selected.'), { status: 400, code: 'nothing_selected' });
    const claimed = db
      .prepare("UPDATE ai_proposals SET status = 'applying' WHERE id = ? AND status = 'pending'")
      .run(proposal.id);
    if (!claimed.changes)
      throw Object.assign(new Error('Proposal is already being applied.'), { status: 409, code: 'proposal_used' });
    try {
      const result = await store.applyProposal(
        chosen.map(i => all[i]),
        { user, requestId, reason: clip(reason || proposal.reason, 500) },
      );
      const status = ['verified', 'unchanged'].includes(result?.status) ? 'applied' : 'needs_review';
      db.prepare('UPDATE ai_proposals SET status = ?, applied_at = ?, applied_json = ? WHERE id = ?').run(
        status,
        status === 'applied' ? now() : null,
        json(chosen),
        proposal.id,
      );
      return { proposalId: proposal.id, status, applied: chosen, result };
    } catch (cause) {
      db.prepare("UPDATE ai_proposals SET status = 'needs_review' WHERE id = ?").run(proposal.id);
      throw cause;
    }
  }

  /** A proposal as the review table shows it: each row with the current values of the changed columns. */
  function proposalView(proposal, row) {
    const fields = [...new Set(proposal.changes.flatMap(c => Object.keys(c.values)))];
    const sheet = proposal.changes[0]?.sheet;
    const types = Object.fromEntries(
      fields.map(f => [f, moduleMap.get(sheet)?.fields.find(x => x.key === f)?.type ?? 'text']),
    );
    return {
      ...proposal,
      status: row?.status ?? proposal.status,
      appliedAt: row?.applied_at ?? null,
      applied: parse(row?.applied_json ?? 'null'),
      fields,
      types,
      changes: proposal.changes.map((change, index) => {
        const record = store.getRecord(change.recordId);
        return {
          ...change,
          index,
          row: record?.row ?? change.row,
          label: change.label ?? record?.label,
          current: Object.fromEntries(fields.map(f => [f, record?.values?.[f] ?? null])),
        };
      }),
    };
  }

  async function executeTool(name, args, context) {
    if (name === 'search_records') {
      const query = clip(args.query, 100).trim();
      if (!query) return { error: 'Search query required' };
      const page = await store.searchRecords({
        module: clip(args.module, 100) || undefined,
        q: query,
        limit: 12,
        offset: 0,
      });
      for (const record of page.records ?? []) {
        context.records.set(record.id, record);
        context.sources.set(record.id, recordSource(record));
      }
      return { records: (page.records ?? []).map(compact), total: page.total };
    }
    if (name === 'find_records') return findRecords(args, context);
    if (name === 'describe_sheet') return describeSheet(args);
    if (name === 'get_record') {
      const record = store.getRecord(clip(args.id, 120));
      if (!record) return { error: 'Record not found' };
      context.records.set(record.id, record);
      context.sources.set(record.id, recordSource(record));
      return compact(record);
    }
    if (name === 'search_knowledge') {
      const found = searchDocs(await documents(config), args.query);
      for (const item of found) context.sources.set(item.id, item);
      return { documents: found };
    }
    if (name === 'run_report') {
      const response = await reports.build({ kind: args.kind, module: args.module, field: args.field });
      if (response.status !== 200) return response.body;
      for (const item of response.body.sources) context.sources.set(item.id, { ...item, type: 'record' });
      const result = { ...response.body, sources: response.body.sources.slice(0, 30) };
      context.results.push(result);
      return result;
    }
    if (name === 'propose_changes') return proposeChanges(args, context);
    if (name === 'apply_proposal') {
      const proposal = db
        .prepare('SELECT * FROM ai_proposals WHERE id = ? AND thread_id = ?')
        .get(String(args.proposalId ?? ''), context.threadId);
      if (!proposal) return { error: 'Proposal not found in this conversation' };
      try {
        const out = await applyProposal(proposal, context.user, {
          requestId: `ai-${randomUUID()}`,
          indexes: args.indexes,
          reason: 'Confirmado en el chat',
        });
        context.applied.push(proposal.id);
        return { status: out.status, rows: out.applied.length };
      } catch (e) {
        return { error: clip(e.message, 300), details: e.details?.items?.slice(0, 10) };
      }
    }
    return { error: 'Unknown tool' };
  }

  function initialsFor(user) {
    const name = String(user.displayName || user.username || '').trim();
    const words = name.toLowerCase().split(/\s+/).filter(Boolean);
    let known = [];
    try {
      known = db
        .prepare(
          "SELECT DISTINCT json_extract(values_json, '$.Collector') c FROM records WHERE sheet = 'Collection_data' AND json_extract(values_json, '$.Collector') LIKE '% - %'",
        )
        .all()
        .map(r => String(r.c));
    } catch {
      /* No mirrored rows (tests). */
    }
    const match = known.find(c => words.length && words.every(w => c.toLowerCase().includes(w)));
    return match ? match.split(' - ')[0].trim() : words.map(w => w[0].toUpperCase()).join('') || 'APP';
  }

  function systemPrompt(user) {
    return [
      `Today is ${now().slice(0, 10)}. You are talking with ${user.displayName || user.username} (initials ${initialsFor(user)}, role ${user.role}).`,
      'Reply in Spanish, briefly. Refer to rows by their identifier (e.g. 5VB, CAM078038) and sheet row, never by internal app IDs.',
      'Use the tools to read exact rows before answering. Only claim what the tools show. Never infer survival, fertility, mating, genotype or identity from counts.',
      'Changes are drafted with propose_changes; the person reviews them in a table and confirms. Use apply_proposal only when their latest message explicitly approves a proposal. Never say a change was written unless apply_proposal returned applied.',
    ].join('\n');
  }

  async function loadImages(attachmentIds) {
    if (!attachmentIds.length) return [];
    if (attachmentIds.length > 6 || typeof store.getAttachment !== 'function')
      throw new Error('Attachments are unavailable');
    const images = [];
    for (const id of attachmentIds) {
      const attachment = await store.getAttachment(String(id));
      // SQLite returns blobs as Uint8Array.
      const data = attachment?.data instanceof Uint8Array ? Buffer.from(attachment.data) : null;
      if (!data || !/^image\/(png|jpeg|webp)$/.test(attachment.mimeType) || data.length > 10_000_000)
        throw new Error('Invalid image attachment');
      images.push({ ...attachment, data });
    }
    return images;
  }

  // Tokens that let one Claude turn call the app's tools through /api/ai/mcp.
  const turns = new Map();
  const busy = new Set();

  async function replyWithClaude(threadId, user, prompt, images, context) {
    const thread = db.prepare('SELECT claude_session FROM ai_threads WHERE id = ?').get(threadId);
    const token = randomBytes(24).toString('hex');
    turns.set(token, { context, expires: Date.now() + claude.timeoutMs + 60000 });
    const content = [
      ...images.map(image => ({
        type: 'image',
        source: { type: 'base64', media_type: image.mimeType, data: image.data.toString('base64') },
      })),
      { type: 'text', text: prompt },
    ];
    const run = (resume, text = prompt) =>
      runClaude(claude, {
        content: [...content.slice(0, -1), { type: 'text', text }],
        system: systemPrompt(user),
        mcpUrl,
        token,
        resume,
        sessionId: resume ? null : randomUUID(),
        docsDir: join(here, '..', 'docs'),
      });
    try {
      let out;
      try {
        out = await run(thread?.claude_session ?? null);
      } catch (e) {
        if (!e.missingSession) throw e;
        // The saved session is gone (e.g. a new server): start again with the recent messages as context.
        const recent = db
          .prepare(
            'SELECT role,content FROM ai_messages WHERE thread_id = ? ORDER BY created_at DESC, rowid DESC LIMIT 11',
          )
          .all(threadId)
          .reverse()
          .slice(0, -1)
          .map(m => `${m.role === 'user' ? 'Persona' : 'Asistente'}: ${clip(m.content, 1500)}`)
          .join('\n\n');
        out = await run(null, recent ? `Conversación anterior:\n${recent}\n\n${prompt}` : prompt);
      }
      db.prepare('UPDATE ai_threads SET claude_session = ? WHERE id = ?').run(out.sessionId, threadId);
      return out.text;
    } finally {
      turns.delete(token);
    }
  }

  async function replyWithApi(threadId, user, prompt, images, context) {
    const history = db
      .prepare('SELECT role,content FROM ai_messages WHERE thread_id = ? ORDER BY created_at DESC, rowid DESC LIMIT 10')
      .all(threadId)
      .reverse();
    const messages = [
      { role: 'system', content: systemPrompt(user) },
      ...history.map(row => ({ role: row.role, content: row.content })),
    ];
    if (images.length)
      messages[messages.length - 1] = {
        role: 'user',
        content: [
          { type: 'text', text: prompt },
          ...images.map(image => ({
            type: 'image_url',
            image_url: { url: `data:${image.mimeType};base64,${image.data.toString('base64')}` },
          })),
        ],
      };
    for (let round = 0; round < 6; round++) {
      const response = await complete(ai, messages);
      if (!response.tool_calls?.length) return clip(response.content, 12000);
      if (response.tool_calls.length > 8) throw new Error('AI requested too many tools');
      messages.push({ role: 'assistant', content: response.content ?? null, tool_calls: response.tool_calls });
      for (const call of response.tool_calls) {
        const result = await executeTool(call.function?.name, parse(call.function?.arguments ?? '{}') ?? {}, context);
        messages.push({ role: 'tool', tool_call_id: call.id, content: json(result).slice(0, 18000) });
      }
    }
    throw new Error('AI did not produce an answer');
  }

  async function reply(threadId, user, prompt, attachments = []) {
    if (busy.has(threadId)) throw Object.assign(new Error('busy'), { busy: true });
    busy.add(threadId);
    try {
      const images = await loadImages(attachments.map(a => a.id));
      const context = {
        threadId,
        user,
        records: new Map(),
        sources: new Map(),
        results: [],
        proposals: [],
        applied: [],
      };
      let answer = claudeAllowed(claude, user)
        ? await replyWithClaude(threadId, user, prompt, images, context)
        : await replyWithApi(threadId, user, prompt, images, context);
      answer = clip(answer, 12000);
      const cited = [...answer.matchAll(/\[([A-Za-z0-9_-]{2,120})\]/g)].map(match => match[1]);
      answer = answer.replace(/\[([A-Za-z0-9_-]{2,120})\]/g, (full, id) => (context.sources.has(id) ? full : ''));
      const sources = cited.filter(id => context.sources.has(id)).map(id => context.sources.get(id));
      const unique = [...new Map(sources.map(item => [item.id, item])).values()];
      const views = context.proposals.map(p =>
        proposalView(p, db.prepare('SELECT status,applied_at,applied_json FROM ai_proposals WHERE id = ?').get(p.id)),
      );
      const message = insertMessage(threadId, 'assistant', answer, unique, context.results, context.proposals);
      return {
        message: { ...message, proposals: views },
        sources: unique,
        results: context.results,
        proposals: views,
        applied: context.applied,
      };
    } finally {
      busy.delete(threadId);
    }
  }

  /**
   * An agent outside the app (T3 Code) using a personal token: it acts as that
   * person, and its proposals go to their "T3 Code" conversation for review.
   */
  const agents = new Map();
  function agentTurn(token) {
    const hash = createHash('sha256').update(token).digest('hex');
    const row = db
      .prepare(
        'SELECT u.* FROM ai_tokens t JOIN users u ON u.id = t.user_id WHERE t.token_hash = ? AND t.revoked_at IS NULL AND u.active = 1',
      )
      .get(hash);
    if (!row) return null;
    const user = { id: row.id, username: row.username, displayName: row.display_name, role: row.role };
    let thread = db.prepare("SELECT id FROM ai_threads WHERE owner_id = ? AND title = 'T3 Code'").get(owner(user));
    if (!thread) {
      thread = { id: randomUUID() };
      db.prepare('INSERT INTO ai_threads (id,owner_id,title,created_at,updated_at) VALUES (?,?,?,?,?)').run(
        thread.id,
        owner(user),
        'T3 Code',
        now(),
        now(),
      );
    }
    const cached = agents.get(hash);
    if (cached?.context.threadId === thread.id) return cached;
    const turn = {
      expires: Infinity,
      agent: true,
      context: {
        threadId: thread.id,
        user,
        records: new Map(),
        sources: new Map(),
        results: [],
        proposals: [],
        applied: [],
      },
    };
    agents.set(hash, turn);
    return turn;
  }

  /** MCP (streamable HTTP, JSON replies) for the Claude CLI; one token per turn. */
  async function mcp(headers, body) {
    const token = /^Bearer\s+(\S+)$/.exec(String(headers.authorization ?? ''))?.[1];
    const turn = token && (turns.get(token) ?? agentTurn(token));
    const id = body?.id ?? null;
    if (!turn || turn.expires < Date.now())
      return { status: 401, body: { jsonrpc: '2.0', id, error: { code: -32001, message: 'Unauthorized' } } };
    const result = value => ({ status: 200, body: { jsonrpc: '2.0', id, result: value } });
    const method = String(body?.method ?? '');
    if (method.startsWith('notifications/')) return { status: 202, body: null };
    if (method === 'initialize')
      return result({
        protocolVersion: body.params?.protocolVersion ?? '2025-06-18',
        capabilities: { tools: { listChanged: false } },
        serverInfo: { name: 'ithomiini', version: '1.0.0' },
      });
    if (method === 'ping') return result({});
    if (method === 'tools/list')
      return result({
        tools: TOOLS.map(t => ({
          name: t.function.name,
          description: t.function.description,
          inputSchema: t.function.parameters,
        })),
      });
    if (method === 'tools/call') {
      let out;
      const before = turn.context.proposals.length;
      try {
        out = await executeTool(String(body.params?.name ?? ''), body.params?.arguments ?? {}, turn.context);
      } catch (e) {
        out = { error: clip(e.message, 300) };
      }
      // Proposals from T3 Code are shown in the app for review (Asistente → Cambios propuestos).
      if (turn.agent && turn.context.proposals.length > before) {
        const fresh = turn.context.proposals.splice(before);
        insertMessage(turn.context.threadId, 'assistant', 'Propuesta desde T3 Code', [], [], fresh);
        out = {
          ...out,
          review: 'The person reviews it in the app: Asistente → Cambios propuestos, or tells you to apply it.',
        };
      }
      return result({ content: [{ type: 'text', text: json(out).slice(0, 200000) }], isError: Boolean(out?.error) });
    }
    return { status: 200, body: { jsonrpc: '2.0', id, error: { code: -32601, message: 'Method not found' } } };
  }

  async function handle({ method, path, body = {}, user, query = {} }) {
    if (!/^\/api\/(chat|reports|knowledge|ai)(?:\/|$)/.test(path)) return null;
    if (!user || !owner(user)) return bad(401, 'unauthorized', 'Sign in to use the assistant.');
    if (path === '/api/reports') return reports.handle({ method, path, query, user });

    if (path === '/api/ai/status' && method === 'GET') {
      let key = '';
      try {
        key = await keyFor(ai);
      } catch {
        /* Status must not reveal file paths. */
      }
      const useClaude = claudeAllowed(claude, user);
      return {
        status: 200,
        body: {
          configured: useClaude || Boolean(ai.model && key),
          provider: useClaude
            ? 'Claude'
            : String(ai.baseUrl).includes('openrouter.ai')
              ? 'OpenRouter'
              : 'OpenAI-compatible',
          model: useClaude ? claude.model : ai.model || null,
          transcription: Boolean(ai.transcriptionModel && key),
          vision: useClaude || Boolean(ai.visionModel && key),
        },
      };
    }
    if (path === '/api/knowledge' && method === 'GET')
      return { status: 200, body: { documents: searchDocs(await documents(config), query.q) } };
    const docMatch = /^\/api\/knowledge\/([a-f0-9]{20})$/.exec(path);
    if (docMatch && method === 'GET') {
      const doc = (await documents(config)).find(item => item.id === docMatch[1]);
      return doc
        ? { status: 200, body: { id: doc.id, title: doc.title, text: doc.text, sourceUrl: doc.sourceUrl } }
        : bad(404, 'not_found', 'Document not found.');
    }
    if (path === '/api/chat/threads' && method === 'GET') {
      const threads = db
        .prepare(
          'SELECT id,title,created_at AS createdAt,updated_at AS updatedAt FROM ai_threads WHERE owner_id = ? ORDER BY updated_at DESC LIMIT 100',
        )
        .all(owner(user));
      return { status: 200, body: { threads } };
    }
    if (path === '/api/chat/threads' && method === 'POST') {
      const id = randomUUID();
      const title = clip(body.title || 'New conversation', 120);
      const time = now();
      db.prepare('INSERT INTO ai_threads (id,owner_id,title,created_at,updated_at) VALUES (?,?,?,?,?)').run(
        id,
        owner(user),
        title,
        time,
        time,
      );
      return { status: 201, body: { thread: { id, title, createdAt: time, updatedAt: time } } };
    }
    const threadMatch = /^\/api\/chat\/threads\/([0-9a-f-]{36})(\/messages)?$/.exec(path);
    if (threadMatch) {
      const record = thread(threadMatch[1], user);
      if (!record) return bad(404, 'not_found', 'Conversation not found.');
      if (!threadMatch[2] && method === 'GET') {
        const rows = new Map(
          db
            .prepare('SELECT id,status,applied_at,applied_json FROM ai_proposals WHERE thread_id = ?')
            .all(record.id)
            .map(item => [item.id, item]),
        );
        const messages = db
          .prepare('SELECT * FROM ai_messages WHERE thread_id = ? ORDER BY created_at, rowid')
          .all(record.id)
          .map(row => {
            const message = publicMessage(row);
            message.proposals = message.proposals.map(p => proposalView(p, rows.get(p.id)));
            return message;
          });
        return {
          status: 200,
          body: {
            thread: { id: record.id, title: record.title, createdAt: record.created_at, updatedAt: record.updated_at },
            messages,
          },
        };
      }
      if (!threadMatch[2] && method === 'DELETE') {
        db.prepare('DELETE FROM ai_proposals WHERE thread_id = ?').run(record.id);
        db.prepare('DELETE FROM ai_messages WHERE thread_id = ?').run(record.id);
        db.prepare('DELETE FROM ai_threads WHERE id = ?').run(record.id);
        return { status: 200, body: { deleted: true } };
      }
      if (threadMatch[2] && method === 'POST') {
        const message = typeof body.message === 'string' ? body.message.trim() : '';
        if (!message || message.length > 6000)
          return bad(400, 'invalid_message', 'Message must contain 1 to 6000 characters.');
        if (
          body.attachmentIds !== undefined &&
          (!Array.isArray(body.attachmentIds) ||
            body.attachmentIds.length > 6 ||
            body.attachmentIds.some(id => typeof id !== 'string' || id.length > 120))
        )
          return bad(400, 'invalid_attachments', 'Use at most six photos.');
        const attachments = (body.attachmentIds ?? []).map(id => {
          const found = store.getAttachment?.(id);
          return { id, name: found?.name ?? 'foto', mimeType: found?.mimeType ?? null };
        });
        if (busy.has(record.id)) return bad(409, 'busy', 'The assistant is still answering in this conversation.');
        insertMessage(record.id, 'user', message, [], [], [], attachments);
        try {
          return { status: 200, body: await reply(record.id, user, message, attachments) };
        } catch (e) {
          console.error('Assistant turn failed:', e.message);
          return bad(502, 'provider_error', 'The assistant could not complete this message. Try again.');
        }
      }
    }
    if (path === '/api/chat/proposals' && method === 'GET') {
      const rows = db
        .prepare(
          "SELECT m.proposals_json, m.created_at FROM ai_messages m JOIN ai_threads t ON t.id = m.thread_id WHERE t.owner_id = ? AND m.proposals_json <> '[]' ORDER BY m.created_at DESC LIMIT 50",
        )
        .all(owner(user));
      const status = new Map(
        db
          .prepare('SELECT id,status,applied_at,applied_json FROM ai_proposals WHERE owner_id = ?')
          .all(owner(user))
          .map(r => [r.id, r]),
      );
      const proposals = rows
        .flatMap(r =>
          (parse(r.proposals_json) ?? []).map(p => ({ ...proposalView(p, status.get(p.id)), createdAt: r.created_at })),
        )
        .filter(p => (query.all ? true : p.status === 'pending'));
      return { status: 200, body: { proposals } };
    }
    const discardMatch = /^\/api\/chat\/proposals\/([0-9a-f-]{36})\/discard$/.exec(path);
    if (discardMatch && method === 'POST') {
      const done = db
        .prepare("UPDATE ai_proposals SET status = 'discarded' WHERE id = ? AND owner_id = ? AND status = 'pending'")
        .run(discardMatch[1], owner(user));
      return done.changes
        ? { status: 200, body: { proposalId: discardMatch[1], status: 'discarded' } }
        : bad(409, 'proposal_used', 'Proposal is no longer pending.');
    }
    const proposalMatch = /^\/api\/chat\/proposals\/([0-9a-f-]{36})\/apply$/.exec(path);
    if (proposalMatch && method === 'POST') {
      const proposal = db
        .prepare('SELECT * FROM ai_proposals WHERE id = ? AND owner_id = ?')
        .get(proposalMatch[1], owner(user));
      if (!proposal) return bad(404, 'not_found', 'Proposal not found.');
      if (proposal.status !== 'pending') return bad(409, 'proposal_used', 'Proposal has already been applied.');
      const requestId = typeof body.requestId === 'string' ? body.requestId : '';
      if (requestId.length < 8 || requestId.length > 120)
        return bad(400, 'request_id_required', 'A unique requestId of 8 to 120 characters is required.');
      if (body.indexes !== undefined && (!Array.isArray(body.indexes) || body.indexes.some(i => !Number.isInteger(i))))
        return bad(400, 'invalid_indexes', 'indexes must be a list of row numbers.');
      try {
        const out = await applyProposal(proposal, user, { requestId, indexes: body.indexes, reason: body.reason });
        return { status: out.status === 'applied' ? 200 : 409, body: out };
      } catch (cause) {
        const current = (parse(proposal.changes_json) ?? []).map(change => {
          const record = store.getRecord(change.recordId);
          return {
            recordId: change.recordId,
            version: record?.version ?? null,
            values: record ? Object.fromEntries(Object.keys(change.values).map(f => [f, record.values?.[f]])) : null,
          };
        });
        const status = db.prepare('SELECT status FROM ai_proposals WHERE id = ?').get(proposal.id)?.status;
        return {
          status: cause.status ?? 409,
          body: {
            error: {
              code: cause.code ?? 'apply_failed',
              message: clip(cause.message, 300),
              details: { proposalId: proposal.id, status, current, items: cause.details?.items?.slice(0, 20) ?? [] },
            },
          },
        };
      }
    }
    if (path === '/api/ai/transcribe' && method === 'POST') {
      if (!ai.transcriptionModel)
        return bad(501, 'unsupported', 'Audio transcription is not configured. Use text entry.');
      const mime = String(body.mimeType ?? '');
      if (!/^audio\/(mpeg|mp3|mp4|m4a|ogg|wav|webm|flac)$/.test(mime))
        return bad(400, 'invalid_audio', 'Unsupported audio type.');
      const data = String(body.dataBase64 ?? '');
      if (!/^[A-Za-z0-9+/]+={0,2}$/.test(data) || data.length > 14_000_000)
        return bad(400, 'invalid_audio', 'Audio data is missing or too large.');
      const format = mime === 'audio/mpeg' ? 'mp3' : mime.split('/')[1];
      try {
        let text;
        if (ai.transcriptionMode === 'chat') {
          const response = await complete(
            ai,
            [
              {
                role: 'system',
                content:
                  'Transcribe the audio verbatim in its original language. Return only the transcript. Do not invent unclear words.',
              },
              {
                role: 'user',
                content: [
                  { type: 'text', text: 'Transcribe this field voice note verbatim.' },
                  { type: 'input_audio', input_audio: { data, format } },
                ],
              },
            ],
            [],
            ai.transcriptionModel,
          );
          text = response.content;
        } else if (String(ai.baseUrl).includes('openrouter.ai')) {
          const response = await providerFetch(ai, 'audio/transcriptions', {
            model: ai.transcriptionModel,
            input_audio: { data, format },
          });
          text = response.text;
        } else {
          const form = new FormData();
          form.set('model', ai.transcriptionModel);
          form.set('file', new Blob([Buffer.from(data, 'base64')], { type: mime }), `recording.${format}`);
          const response = await providerFetch(ai, 'audio/transcriptions', form, true);
          text = response.text;
        }
        if (typeof text !== 'string' || !text.trim()) throw new Error('Missing transcription');
        return { status: 200, body: { text, draft: true, requiresReview: true } };
      } catch {
        return bad(502, 'provider_error', 'Audio transcription failed. Try again or enter text.');
      }
    }
    if (path === '/api/ai/extract' && method === 'POST') {
      if (!ai.visionModel)
        return bad(501, 'unsupported', 'Image extraction is not configured. Enter the observation manually.');
      const mime = String(body.mimeType ?? '');
      if (!/^image\/(png|jpeg|webp)$/.test(mime)) return bad(400, 'invalid_image', 'Use PNG, JPEG, or WebP.');
      const data = String(body.dataBase64 ?? '');
      if (!/^[A-Za-z0-9+/]+={0,2}$/.test(data) || data.length > 14_000_000)
        return bad(400, 'invalid_image', 'Image data is missing or too large.');
      try {
        const answer = await complete(
          ai,
          [
            {
              role: 'system',
              content:
                'Transcribe visible text from this field note or label as a draft. Return JSON with keys text and uncertain. Do not guess specimen identity, taxon, sex, or biological outcome. If unclear, say so in uncertain. No record write is permitted.',
            },
            {
              role: 'user',
              content: [
                { type: 'text', text: clip(body.prompt || 'Extract visible text.', 500) },
                { type: 'image_url', image_url: { url: `data:${mime};base64,${data}` } },
              ],
            },
          ],
          [],
          ai.visionModel,
        );
        const raw = String(answer.content ?? '');
        const parsed = parse(raw.replace(/^```(?:json)?\s*|\s*```$/g, ''));
        return {
          status: 200,
          body: {
            text: clip(parsed?.text ?? raw, 12000),
            uncertain: parsed?.uncertain ?? [],
            draft: true,
            requiresReview: true,
          },
        };
      } catch {
        return bad(502, 'provider_error', 'Image extraction failed. Enter the observation manually.');
      }
    }
    return bad(404, 'not_found', 'Assistant route not found.');
  }
  return { handle, mcp };
}
