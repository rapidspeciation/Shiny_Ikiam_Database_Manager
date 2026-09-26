import { createHash, randomUUID } from 'node:crypto';
import { readFile, readdir, stat } from 'node:fs/promises';
import { basename, extname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createReports } from './reports.mjs';

const here = fileURLToPath(new URL('.', import.meta.url));
const bad = (status, code, message) => ({ status, body: { error: { code, message } } });
const now = () => new Date().toISOString();
const clip = (value, length = 1200) => String(value ?? '').slice(0, length);
const owner = user => String(user?.id ?? user?.username ?? '');
const json = value => JSON.stringify(value);
const parse = value => { try { return JSON.parse(value); } catch { return null; } };

const TOOLS = [
  { type: 'function', function: { name: 'search_records', description: 'Search exact workbook records. Returns a small list with source sheet and row.', parameters: { type: 'object', properties: { query: { type: 'string' }, module: { type: 'string' } }, required: ['query'] } } },
  { type: 'function', function: { name: 'get_record', description: 'Fetch one exact record by app ID, including its source and version.', parameters: { type: 'object', properties: { id: { type: 'string' } }, required: ['id'] } } },
  { type: 'function', function: { name: 'search_knowledge', description: 'Search approved local protocols and meeting notes.', parameters: { type: 'object', properties: { query: { type: 'string' } }, required: ['query'] } } },
  { type: 'function', function: { name: 'run_report', description: 'Produce a sourced count, stage, cross, sample, quality or weekly report.', parameters: { type: 'object', properties: { kind: { type: 'string', enum: ['overview', 'counts', 'stages', 'crosses', 'samples', 'quality', 'weekly'] }, module: { type: 'string' }, field: { type: 'string' } }, required: ['kind'] } } },
  { type: 'function', function: { name: 'propose_changes', description: 'Draft exact record edits for human review. Never applies them. First fetch each record with get_record.', parameters: { type: 'object', properties: { changes: { type: 'array', items: { type: 'object', properties: { recordId: { type: 'string' }, values: { type: 'object' } }, required: ['recordId', 'values'] } }, reason: { type: 'string' } }, required: ['changes'] } } },
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
    transcriptionMode: ai.transcriptionMode ?? process.env.ITHOMIINI_AI_TRANSCRIPTION_MODE ?? (String(ai.baseUrl ?? process.env.ITHOMIINI_AI_BASE_URL ?? '').includes('openrouter.ai') ? 'chat' : 'endpoint'),
    visionModel: ai.visionModel ?? process.env.ITHOMIINI_AI_VISION_MODEL,
  };
}

function endpoint(ai, path) {
  const base = new URL(ai.baseUrl.endsWith('/') ? ai.baseUrl : `${ai.baseUrl}/`);
  if (base.protocol !== 'https:' && !(base.protocol === 'http:' && ['localhost', '127.0.0.1', '[::1]'].includes(base.hostname))) throw new Error('AI endpoint must use HTTPS or local HTTP');
  return new URL(path, base).toString();
}

async function providerFetch(ai, path, body, multipart = false) {
  const key = await keyFor(ai);
  if (!key || !ai.model) throw new Error('AI provider is not configured');
  const response = await fetch(endpoint(ai, path), {
    method: 'POST', headers: { Authorization: `Bearer ${key}`, ...(multipart ? {} : { 'Content-Type': 'application/json' }) },
    body: multipart ? body : json(body), signal: AbortSignal.timeout(45000),
  });
  if (!response.ok) throw new Error(`AI provider returned HTTP ${response.status}`);
  const payload = await response.json();
  return payload;
}

async function complete(ai, messages, tools = TOOLS, model = ai.model) {
  const payload = await providerFetch({ ...ai, model }, 'chat/completions', { model, messages, ...(tools?.length ? { tools, tool_choice: 'auto' } : {}) });
  const message = payload?.choices?.[0]?.message;
  if (!message || (typeof message.content !== 'string' && !Array.isArray(message.tool_calls))) throw new Error('AI provider returned no message');
  return message;
}

function recordSource(record) {
  return { id: record.id, type: 'record', sheet: record.sheet, row: record.row, version: record.version, label: record.label, sourceUrl: record.sourceUrl ?? null };
}

async function knowledgeFiles(config) {
  const roots = config.knowledgeRoots ?? (config.knowledgeRoot ? [config.knowledgeRoot] : process.env.KNOWLEDGE_DIR ? [process.env.KNOWLEDGE_DIR] : [join(here, '..', 'docs', 'meetings.md'), join(here, '..', 'docs', 'workflows.md')]);
  const paths = [];
  for (const configured of roots) {
    const root = resolve(configured);
    let info;
    try { info = await stat(root); } catch { continue; }
    if (info.isFile()) { paths.push(root); continue; }
    if (!info.isDirectory()) continue;
    for (const item of (await readdir(root, { withFileTypes: true })).slice(0, 300)) {
      if (item.isFile() && ['.md', '.txt'].includes(extname(item.name).toLowerCase())) paths.push(join(root, item.name));
    }
  }
  return paths.slice(0, 400);
}

async function documents(config) {
  const entries = [];
  for (const path of await knowledgeFiles(config)) {
    let info;
    try { info = await stat(path); } catch { continue; }
    if (!info.isFile() || info.size > 150000) continue;
    const text = await readFile(path, 'utf8');
    const front = /^---\n([\s\S]*?)\n---\n/.exec(text);
    const metadata = Object.fromEntries((front?.[1] ?? '').split('\n').map(line => /^([A-Za-z][\w-]*):\s*(.*)$/.exec(line)).filter(Boolean).map(match => [match[1], match[2].replace(/^['"]|['"]$/g, '')]));
    const content = front ? text.slice(front[0].length) : text;
    const id = createHash('sha256').update(path).digest('hex').slice(0, 20);
    const name = basename(path);
    const driveId = /^([A-Za-z0-9_-]{25,})\.txt$/.exec(name)?.[1];
    entries.push({ id, title: metadata.title ?? content.match(/^#\s+(.+)$/m)?.[1] ?? name, text: content, sourceUrl: metadata.sourceUrl ?? (driveId ? `https://docs.google.com/document/d/${driveId}/edit` : `/api/knowledge/${id}`) });
  }
  return entries;
}

function searchDocs(docs, query) {
  const terms = clip(query, 120).toLowerCase().split(/[^\p{L}\p{N}]+/u).filter(term => term.length > 2).slice(0, 8);
  if (!terms.length) return [];
  return docs.map(doc => {
    const content = doc.text.toLowerCase();
    const hits = terms.map(term => content.indexOf(term)).filter(index => index >= 0);
    const first = Math.min(...hits);
    return { doc, score: hits.length, snippet: Number.isFinite(first) ? clip(doc.text.slice(Math.max(0, first - 180), first + 800), 1000) : '' };
  }).filter(item => item.score).sort((a, b) => b.score - a.score).slice(0, 8)
    .map(({ doc, snippet }) => ({ id: doc.id, type: 'document', title: doc.title, snippet, sourceUrl: doc.sourceUrl }));
}

function publicMessage(row) {
  return { id: row.id, role: row.role, content: row.content, sources: parse(row.sources_json) ?? [], results: parse(row.results_json) ?? [], proposals: parse(row.proposals_json) ?? [], createdAt: row.created_at };
}

export function createAssistant({ store, config = {} }) {
  if (!store?.db) throw new Error('Assistant requires store.db');
  const db = store.db;
  init(db);
  const ai = providerConfig(config);
  const reports = createReports({ store, config });
  const thread = (id, user) => db.prepare('SELECT * FROM ai_threads WHERE id = ? AND owner_id = ?').get(id, owner(user));
  const insertMessage = (threadId, role, content, sources = [], results = [], proposals = []) => {
    const message = { id: randomUUID(), thread_id: threadId, role, content, sources_json: json(sources), results_json: json(results), proposals_json: json(proposals), created_at: now() };
    db.prepare('INSERT INTO ai_messages (id,thread_id,role,content,sources_json,results_json,proposals_json,created_at) VALUES (?,?,?,?,?,?,?,?)').run(...Object.values(message));
    db.prepare('UPDATE ai_threads SET updated_at = ? WHERE id = ?').run(message.created_at, threadId);
    return publicMessage(message);
  };

  async function executeTool(call, context) {
    const args = parse(call.function?.arguments ?? '{}') ?? {};
    const name = call.function?.name;
    if (name === 'search_records') {
      const query = clip(args.query, 100).trim();
      if (!query) return { error: 'Search query required' };
      const page = await store.searchRecords({ module: clip(args.module, 100) || undefined, q: query, limit: 12, offset: 0 });
      const found = (page.records ?? []).map(record => ({ ...recordSource(record), values: record.values }));
      for (const record of page.records ?? []) context.records.set(record.id, record);
      for (const item of found) context.sources.set(item.id, recordSource(item));
      return { records: found, total: page.total, truncated: page.total > found.length };
    }
    if (name === 'get_record') {
      const id = clip(args.id, 120);
      if (!id) return { error: 'Record ID required' };
      const record = await store.getRecord(id);
      if (!record) return { error: 'Record not found' };
      context.records.set(record.id, record);
      context.sources.set(record.id, recordSource(record));
      return { ...recordSource(record), values: record.values, formulas: record.formulas };
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
    if (name === 'propose_changes') {
      if (!['editor', 'reviewer', 'admin'].includes(context.user.role)) return { error: 'Your role cannot propose edits' };
      if (!Array.isArray(args.changes) || !args.changes.length || args.changes.length > 20) return { error: 'Provide 1 to 20 changes' };
      const changes = [];
      for (const candidate of args.changes) {
        const old = context.records.get(candidate.recordId);
        if (!old) return { error: `Fetch record ${clip(candidate.recordId, 60)} before proposing it` };
        const values = candidate.values;
        if (!values || typeof values !== 'object' || Array.isArray(values) || !Object.keys(values).length || Object.keys(values).length > 20) return { error: 'Invalid field patch' };
        const before = {};
        for (const key of Object.keys(values)) {
          if (!Object.hasOwn(old.values ?? {}, key) || Object.hasOwn(old.formulas ?? {}, key)) return { error: `Field ${clip(key, 60)} is absent or formula based` };
          before[key] = old.values[key];
        }
        changes.push({ recordId: old.id, expectedVersion: old.version, before, values });
      }
      const id = randomUUID();
      db.prepare('INSERT INTO ai_proposals (id,thread_id,owner_id,changes_json,reason,status,created_at) VALUES (?,?,?,?,?,?,?)').run(id, context.threadId, owner(context.user), json(changes), clip(args.reason, 500), 'pending', now());
      const proposal = { id, changes, reason: clip(args.reason, 500), status: 'pending' };
      context.proposals.push(proposal);
      return { proposal };
    }
    return { error: 'Unknown tool' };
  }

  async function reply(threadId, user, prompt, attachmentIds = []) {
    const history = db.prepare('SELECT role,content FROM ai_messages WHERE thread_id = ? ORDER BY created_at DESC, rowid DESC LIMIT 10').all(threadId).reverse();
    const messages = [
      { role: 'system', content: 'You are the Ithomiini research assistant. Use tools to retrieve exact records, approved documents, and reports before answering factual questions. Cite source IDs in square brackets, e.g. [record-id] or [document-id]. Only claim facts supported by tool output; explain missing data and report methods. Never infer survival, fertility, mating, genotype, current life status, or biological identity from simple counts, eggs, or repeated marks. For edits, fetch each exact record, then use propose_changes. Proposals require human review. Never say an edit was applied. No SQL, shell, or external browsing is available.' },
      ...history.map(row => ({ role: row.role, content: row.content })),
    ];
    if (attachmentIds.length) {
      if (attachmentIds.length > 3 || typeof store.getAttachment !== 'function') throw new Error('Attachments are unavailable');
      const parts = [{ type: 'text', text: prompt }];
      for (const id of attachmentIds) {
        const attachment = await store.getAttachment(String(id));
        if (!attachment || !/^image\/(png|jpeg|webp)$/.test(attachment.mimeType) || !Buffer.isBuffer(attachment.data) || attachment.data.length > 10_000_000) throw new Error('Invalid image attachment');
        parts.push({ type: 'image_url', image_url: { url: `data:${attachment.mimeType};base64,${attachment.data.toString('base64')}` } });
      }
      messages[messages.length - 1] = { role: 'user', content: parts };
    }
    const context = { threadId, user, records: new Map(), sources: new Map(), results: [], proposals: [] };
    let answer = '';
    for (let round = 0; round < 4; round++) {
      const response = await complete(ai, messages);
      if (!response.tool_calls?.length) { answer = clip(response.content, 12000); break; }
      if (response.tool_calls.length > 8) throw new Error('AI requested too many tools');
      messages.push({ role: 'assistant', content: response.content ?? null, tool_calls: response.tool_calls });
      for (const call of response.tool_calls) {
        const result = await executeTool(call, context);
        messages.push({ role: 'tool', tool_call_id: call.id, content: json(result).slice(0, 18000) });
      }
    }
    if (!answer) throw new Error('AI did not produce an answer');
    const cited = [...answer.matchAll(/\[([A-Za-z0-9_-]{2,120})\]/g)].map(match => match[1]);
    answer = answer.replace(/\[([A-Za-z0-9_-]{2,120})\]/g, (full, id) => context.sources.has(id) ? full : '');
    const sources = cited.filter(id => context.sources.has(id)).map(id => context.sources.get(id));
    const unique = [...new Map(sources.map(item => [item.id, item])).values()];
    const message = insertMessage(threadId, 'assistant', answer, unique, context.results, context.proposals);
    return { message, sources: unique, results: context.results, proposals: context.proposals };
  }

  async function handle({ method, path, body = {}, user, query = {} }) {
    if (!/^\/api\/(chat|reports|knowledge|ai)(?:\/|$)/.test(path)) return null;
    if (!user || !owner(user)) return bad(401, 'unauthorized', 'Sign in to use the assistant.');
    if (path === '/api/reports') return reports.handle({ method, path, query, user });

    if (path === '/api/ai/status' && method === 'GET') {
      let key = '';
      try { key = await keyFor(ai); } catch { /* Status must not reveal file paths. */ }
      return { status: 200, body: { configured: Boolean(ai.model && key), provider: String(ai.baseUrl).includes('openrouter.ai') ? 'OpenRouter' : 'OpenAI-compatible', model: ai.model || null, transcription: Boolean(ai.transcriptionModel && key), vision: Boolean(ai.visionModel && key) } };
    }
    if (path === '/api/knowledge' && method === 'GET') return { status: 200, body: { documents: searchDocs(await documents(config), query.q) } };
    const docMatch = /^\/api\/knowledge\/([a-f0-9]{20})$/.exec(path);
    if (docMatch && method === 'GET') {
      const doc = (await documents(config)).find(item => item.id === docMatch[1]);
      return doc ? { status: 200, body: { id: doc.id, title: doc.title, text: doc.text, sourceUrl: doc.sourceUrl } } : bad(404, 'not_found', 'Document not found.');
    }
    if (path === '/api/chat/threads' && method === 'GET') {
      const threads = db.prepare('SELECT id,title,created_at AS createdAt,updated_at AS updatedAt FROM ai_threads WHERE owner_id = ? ORDER BY updated_at DESC LIMIT 100').all(owner(user));
      return { status: 200, body: { threads } };
    }
    if (path === '/api/chat/threads' && method === 'POST') {
      const id = randomUUID();
      const title = clip(body.title || 'New conversation', 120);
      const time = now();
      db.prepare('INSERT INTO ai_threads (id,owner_id,title,created_at,updated_at) VALUES (?,?,?,?,?)').run(id, owner(user), title, time, time);
      return { status: 201, body: { thread: { id, title, createdAt: time, updatedAt: time } } };
    }
    const threadMatch = /^\/api\/chat\/threads\/([0-9a-f-]{36})(\/messages)?$/.exec(path);
    if (threadMatch) {
      const record = thread(threadMatch[1], user);
      if (!record) return bad(404, 'not_found', 'Conversation not found.');
      if (!threadMatch[2] && method === 'GET') {
        const statusById = new Map(db.prepare('SELECT id,status,applied_at FROM ai_proposals WHERE thread_id = ?').all(record.id).map(item => [item.id, item]));
        const messages = db.prepare('SELECT * FROM ai_messages WHERE thread_id = ? ORDER BY created_at, rowid').all(record.id).map(row => {
          const message = publicMessage(row);
          message.proposals = message.proposals.map(proposal => ({ ...proposal, status: statusById.get(proposal.id)?.status ?? proposal.status, appliedAt: statusById.get(proposal.id)?.applied_at ?? null }));
          return message;
        });
        return { status: 200, body: { thread: { id: record.id, title: record.title, createdAt: record.created_at, updatedAt: record.updated_at }, messages } };
      }
      if (!threadMatch[2] && method === 'DELETE') {
        db.prepare('DELETE FROM ai_proposals WHERE thread_id = ?').run(record.id);
        db.prepare('DELETE FROM ai_messages WHERE thread_id = ?').run(record.id);
        db.prepare('DELETE FROM ai_threads WHERE id = ?').run(record.id);
        return { status: 200, body: { deleted: true } };
      }
      if (threadMatch[2] && method === 'POST') {
        const message = typeof body.message === 'string' ? body.message.trim() : '';
        if (!message || message.length > 6000) return bad(400, 'invalid_message', 'Message must contain 1 to 6000 characters.');
        if (body.attachmentIds !== undefined && (!Array.isArray(body.attachmentIds) || body.attachmentIds.length > 3 || body.attachmentIds.some(id => typeof id !== 'string' || id.length > 120))) return bad(400, 'invalid_attachments', 'Use at most three image attachments.');
        insertMessage(record.id, 'user', message);
        try { return { status: 200, body: await reply(record.id, user, message, body.attachmentIds ?? []) }; }
        catch { return bad(502, 'provider_error', 'The assistant could not complete this message. Try again.'); }
      }
    }
    const proposalMatch = /^\/api\/chat\/proposals\/([0-9a-f-]{36})\/apply$/.exec(path);
    if (proposalMatch && method === 'POST') {
      const proposal = db.prepare('SELECT * FROM ai_proposals WHERE id = ? AND owner_id = ?').get(proposalMatch[1], owner(user));
      if (!proposal) return bad(404, 'not_found', 'Proposal not found.');
      if (proposal.status !== 'pending') return bad(409, 'proposal_used', 'Proposal has already been applied.');
      if (!['editor', 'reviewer', 'admin'].includes(user.role)) return bad(403, 'forbidden', 'Your role cannot apply changes.');
      if (typeof store.applyProposal !== 'function') return bad(503, 'apply_unavailable', 'Reviewed changes are unavailable.');
      const requestId = typeof body.requestId === 'string' ? body.requestId : '';
      if (requestId.length < 8 || requestId.length > 120) return bad(400, 'request_id_required', 'A unique requestId of 8 to 120 characters is required.');
      const claimed = db.prepare("UPDATE ai_proposals SET status = 'applying' WHERE id = ? AND owner_id = ? AND status = 'pending'").run(proposal.id, owner(user));
      if (!claimed.changes) return bad(409, 'proposal_used', 'Proposal is already being applied or reviewed.');
      try {
        const result = await store.applyProposal(parse(proposal.changes_json), { user, requestId, reason: clip(body.reason || proposal.reason, 500) });
        const status = result?.status === 'verified' ? 'applied' : 'needs_review';
        db.prepare('UPDATE ai_proposals SET status = ?, applied_at = ? WHERE id = ?').run(status, status === 'applied' ? now() : null, proposal.id);
        return { status: status === 'applied' ? 200 : 409, body: { proposalId: proposal.id, status, result } };
      } catch (cause) {
        db.prepare("UPDATE ai_proposals SET status = 'needs_review' WHERE id = ?").run(proposal.id);
        const current = [];
        for (const change of parse(proposal.changes_json) ?? []) {
          const record = await store.getRecord(change.recordId);
          current.push({ recordId: change.recordId, version: record?.version ?? null, values: record ? Object.fromEntries(Object.keys(change.values).map(field => [field, record.values?.[field]])) : null });
        }
        return { status: cause.status ?? 409, body: { error: { code: cause.code ?? 'apply_failed', message: clip(cause.message, 180), details: { proposalId: proposal.id, status: 'needs_review', current, note: 'Some source changes may have applied. Inspect record history before drafting another change.' } } } };
      }
    }
    if (path === '/api/ai/transcribe' && method === 'POST') {
      if (!ai.transcriptionModel) return bad(501, 'unsupported', 'Audio transcription is not configured. Use text entry.');
      const mime = String(body.mimeType ?? '');
      if (!/^audio\/(mpeg|mp3|mp4|m4a|ogg|wav|webm|flac)$/.test(mime)) return bad(400, 'invalid_audio', 'Unsupported audio type.');
      const data = String(body.dataBase64 ?? '');
      if (!/^[A-Za-z0-9+/]+={0,2}$/.test(data) || data.length > 14_000_000) return bad(400, 'invalid_audio', 'Audio data is missing or too large.');
      const format = mime === 'audio/mpeg' ? 'mp3' : mime.split('/')[1];
      try {
        let text;
        if (ai.transcriptionMode === 'chat') {
          const response = await complete(ai, [
            { role: 'system', content: 'Transcribe the audio verbatim in its original language. Return only the transcript. Do not invent unclear words.' },
            { role: 'user', content: [{ type: 'text', text: 'Transcribe this field voice note verbatim.' }, { type: 'input_audio', input_audio: { data, format } }] },
          ], [], ai.transcriptionModel);
          text = response.content;
        } else if (String(ai.baseUrl).includes('openrouter.ai')) {
          const response = await providerFetch(ai, 'audio/transcriptions', { model: ai.transcriptionModel, input_audio: { data, format } });
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
      } catch { return bad(502, 'provider_error', 'Audio transcription failed. Try again or enter text.'); }
    }
    if (path === '/api/ai/extract' && method === 'POST') {
      if (!ai.visionModel) return bad(501, 'unsupported', 'Image extraction is not configured. Enter the observation manually.');
      const mime = String(body.mimeType ?? '');
      if (!/^image\/(png|jpeg|webp)$/.test(mime)) return bad(400, 'invalid_image', 'Use PNG, JPEG, or WebP.');
      const data = String(body.dataBase64 ?? '');
      if (!/^[A-Za-z0-9+/]+={0,2}$/.test(data) || data.length > 14_000_000) return bad(400, 'invalid_image', 'Image data is missing or too large.');
      try {
        const answer = await complete(ai, [
          { role: 'system', content: 'Transcribe visible text from this field note or label as a draft. Return JSON with keys text and uncertain. Do not guess specimen identity, taxon, sex, or biological outcome. If unclear, say so in uncertain. No record write is permitted.' },
          { role: 'user', content: [{ type: 'text', text: clip(body.prompt || 'Extract visible text.', 500) }, { type: 'image_url', image_url: { url: `data:${mime};base64,${data}` } }] },
        ], [], ai.visionModel);
        const raw = String(answer.content ?? '');
        const parsed = parse(raw.replace(/^```(?:json)?\s*|\s*```$/g, ''));
        return { status: 200, body: { text: clip(parsed?.text ?? raw, 12000), uncertain: parsed?.uncertain ?? [], draft: true, requiresReview: true } };
      } catch { return bad(502, 'provider_error', 'Image extraction failed. Enter the observation manually.'); }
    }
    return bad(404, 'not_found', 'Assistant route not found.');
  }
  return { handle };
}
