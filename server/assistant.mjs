import { createHash, randomBytes, randomUUID } from 'node:crypto';
import { readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createReports } from './reports.mjs';
import { TYPED_OVER_FORMULA, uniqueIdIndex } from './batch.mjs';
import { allIssues, checkData } from './checks.mjs';
import { agreedFixes, markApplied } from './review.mjs';
import { tpl, withoutMsgs } from './messages.mjs';
import { comparable, isSumField, labelFor, moduleMap, simpleSum, validateValues } from './schema.mjs';
import { TUBE_FIELD, isIdValue, isUnique } from './verifications.mjs';
import { listOptions, listProblem } from './verify.mjs';
import { queueWalk, walkDraft } from './walks.mjs';
import { claudeAllowed, claudeConfig, prepareWorkspace, runClaude } from './claude.mjs';
import { KINDS, isNone, noteText } from './notebook.mjs';
import { RECORD_TOOLS, compactRecord, countRecords, findRecords } from './records-tool.mjs';
import { MATCH_NOTEBOOK_TOOL, createNotebookMatcher, matchSummary } from './notebook-tool.mjs';
import { newRowFormulaFields } from './premade.mjs';
import { KNOWLEDGE_TOOLS, createKnowledge, runKnowledgeTool } from './knowledge.mjs';
import { HISTORY_TOOLS, HISTORY_TOOL_NAMES, runHistoryTool } from './history.mjs';
import { createT3Chats } from './t3chats.mjs';

const here = fileURLToPath(new URL('.', import.meta.url));
const bad = (status, code, message) => ({ status, body: { error: { code, message } } });
const now = () => new Date().toISOString();
const clip = (value, length = 1200) => String(value ?? '').slice(0, length);
const owner = user => String(user?.id ?? user?.username ?? '');
const json = value => JSON.stringify(value);
const EDITORS = ['editor', 'reviewer', 'admin'];
const isoDate = serial => new Date(Date.UTC(1899, 11, 30) + serial * 864e5).toISOString().slice(0, 10);
const TIME_FIELD = /(^|_)time$/i;
/** "9:20" in a time column becomes the day fraction Sheets stores (the grids show it as 9:20). */
const withSheetTimes = values =>
  !values || typeof values !== 'object' || Array.isArray(values)
    ? values
    : Object.fromEntries(
        Object.entries(values).map(([key, value]) => {
          const m = TIME_FIELD.test(key) && typeof value === 'string' && /^\s*([01]?\d|2[0-3]):([0-5]\d)\s*$/.exec(value);
          return [key, m ? (Number(m[1]) * 60 + Number(m[2])) / 1440 : value];
        }),
      );
const parse = value => {
  try {
    return JSON.parse(value);
  } catch {
    return null;
  }
};

/*
 * What the assistant's values mean in a proposal. Emptying a cell is never
 * implicit: null (or leaving the column out) is "no change there", and only
 * { clear: true } empties a cell. A note the assistant adds keeps the team's
 * "d/m/yy INI: " form after what the cell holds; { replace } rewrites it.
 */
const VALUES_DOC =
  'Column → value; dates as YYYY-MM-DD, times as H:MM. null (or leaving the column out) = no change there; {"clear": true} = empty the cell; in a notes column your text is added after the existing note ({"replace": "…"} rewrites it).';
const VALUES_RULES =
  'Values: null never empties a cell (it means no change); to empty one give {"clear": true}, only when the person wants it emptied. Notes columns (NOTES, Notes, Notes_…): give only the new text; it is written as "d/m/yy INI: text" (today, the person\'s initials) after the existing note with " | ", never over it, unless you give {"replace": "the whole note"} because the person asked to rewrite it.';
/**
 * A value the assistant dropped (null in update_proposal), or a cell the person
 * set back to the sheet's value ("Valor de la hoja"): the cell goes back to no change.
 */
const DROP = Symbol('drop');
/** The person took the assistant's value again ("Valor de la IA"): the one kept with their mark. */
const AI_VALUE = Symbol('ai value');
const NOTE_FIELD = /^notes?(?:_|$)/i;
/** A note already in the team's form: "29/9/26 FCH:", "16/06/2023 AA:", "23Ago26 PAS". */
const NOTE_PREFIX = /^\s*(?:\d{1,2}\s*[/.-]\s*\d{1,2}\s*[/.-]\s*\d{2,4}|\d{1,2}\s*[A-Za-z]{3}\s*\d{2,4})\s+[A-ZÑ]{2,4}\b/;
const ecuadorDay = () => new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' }).format(new Date());

const TOOLS = [
  {
    type: 'function',
    function: {
      name: 'search_records',
      description:
        'Search workbook rows by free text. Returns a small list (12) with sheet, row and app ID. For exact identifiers or column conditions use find_records; for counts, count_records.',
      parameters: {
        type: 'object',
        properties: { query: { type: 'string' }, module: { type: 'string' } },
        required: ['query'],
      },
    },
  },
  ...RECORD_TOOLS,
  {
    type: 'function',
    function: {
      name: 'get_record',
      description:
        'Fetch one row by app ID, including its sheet row and version: every non-empty value (formula cells with their computed value) and formulas = the formula text of each formula cell.',
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
  ...KNOWLEDGE_TOOLS,
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
      name: 'check_data',
      description:
        'Scan the workbook for inconsistencies: repeated IDs/tubes, a CAM given to two butterflies, values outside strict dropdown lists, Collection_data rows sent to the insectary without a filled Insectary_data row (and wild insectary butterflies without a collection row), species/sex/CAM mismatches between the two rows of one butterfly, deaths or preservations dated before collection or entry, future dates, preserved rows without CAM or Tube_1_id, field marks used for two species, and Wikiloc monitoring points stored on the map without a row because their pairing was doubtful (walk_doubt: row/recordId is the likeliest row or null, value the note, walk = {date, collector, trackId}; they are paired by a person in Monitoreo → Dudas, never with propose_changes). From the specimen photos: photo_camid (the envelope in the photo shows another CAM than the file name: a Drive rename, task), photo_extra (photos of another butterfly in a CAM folder: task), envelope_sex / envelope_species (the envelope says another sex/species than the sheet; ocr = what was read), photo_missing (preserved without photos in Photo_links), ai_species (the Wings Gallery model sees another species; ai = {predicted, confidence}). Photo issues carry cam, strength (fuerte/media/baja/dudosa), curation (earlier decision), photos, envelopeText, envelopeCamid, prediction; tasks carry task.text and are never sheet changes. People judge issues in the Revisión tab; use list_agreed_fixes for the fixes they accepted. Each issue has sheet, row, recordId, field, value, problem and, when the right value is obvious, fix = {recordId, values} ready for propose_changes. Paginated; filter by sheet and kind (comma-separated). Call without kind first to see the counts.',
      parameters: {
        type: 'object',
        properties: {
          sheet: { type: 'string', description: 'Only this sheet, e.g. Collection_data' },
          kind: {
            type: 'string',
            description:
              'repeat, cam_cross, list, insectary_link, link_mismatch, date_order, future_date, bad_date, missing_sample, mark_reuse, walk_doubt, photo_camid, photo_extra, envelope_sex, envelope_species, photo_missing, ai_species (comma-separated)',
          },
          recordId: { type: 'string', description: 'Only the issues of this row' },
          limit: { type: 'integer', description: '1 to 200, default 50' },
          offset: { type: 'integer' },
        },
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'queue_wikiloc',
      description:
        'Queue a Wikiloc monitoring walk (trail URL) to be read by the home computer (the server cannot open Wikiloc). If the walk was already read it returns its walkId at once. Then call get_walk.',
      parameters: {
        type: 'object',
        properties: {
          url: { type: 'string', description: 'Wikiloc trail link, e.g. https://es.wikiloc.com/rutas-senderismo/…-123456789' },
          refresh: { type: 'boolean', description: 'Read it again even if already read (notes corrected in Wikiloc)' },
        },
        required: ['url'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'get_walk',
      description:
        'A Wikiloc walk read from its page: every point with the parsed note (species matched to the sheet and Taxonomy, subspecies, sex, time, height, weather codes, mark, transect section), whether it is already in Collection_data, the review checks of Monitoreo (recapture, mark used for another species, 30-preserved rule, missing parts), and newRows: Collection_data values for the points not in the sheet yet, ready for propose_changes. While the walk is still being read it returns its queue status. Give date or collector when the walk lacks them.',
      parameters: {
        type: 'object',
        properties: {
          url: { type: 'string' },
          walkId: { type: 'string' },
          date: { type: 'string', description: 'YYYY-MM-DD, when the title has no day' },
          collector: { type: 'string', description: 'As in Collection_data, e.g. "FCH - Franz Chandi"' },
        },
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'propose_changes',
      description:
        'Draft edits to existing rows (changes) and/or new rows (newRows, e.g. from get_walk). They appear to the person as a table with the changed cells highlighted and are only written when the person confirms. Formula cells cannot be changed, except SPECIES in Insectary_data when what emerged differs from the formula prediction. Give a short note per row saying where the value comes from. Use one proposal per task (e.g. one per walk or per kind of fix). ' +
        VALUES_RULES,
      parameters: {
        type: 'object',
        properties: {
          changes: {
            type: 'array',
            items: {
              type: 'object',
              properties: {
                recordId: { type: 'string' },
                values: { type: 'object', description: VALUES_DOC },
                note: { type: 'string' },
              },
              required: ['recordId', 'values'],
            },
          },
          newRows: {
            type: 'array',
            items: {
              type: 'object',
              properties: {
                sheet: { type: 'string' },
                values: { type: 'object', description: `${VALUES_DOC} In a new row, null and {"clear": true} just leave the cell empty.` },
                note: { type: 'string' },
              },
              required: ['sheet', 'values'],
            },
          },
          reason: { type: 'string' },
          issueIds: {
            type: 'array',
            items: { type: 'string' },
            description:
              'The issueId of every fix from list_agreed_fixes used in this proposal: once the person applies it, those issues show as applied in the Revisión tab',
          },
        },
        required: ['reason'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'list_agreed_fixes',
      description:
        'The corrections people agreed on in the Revisión tab (verdict accepted, or another value they gave), ready to propose: fixes = {issueId, recordId, sheet, row, label, values, note, decidedBy} for propose_changes; tasks = work that is no sheet change (Drive renames and merges of specimen photos) to explain as a checklist; needsValue = accepted without a value (ask); stale = the data changed since the verdict. When the person says "aplica las correcciones acordadas": call this, make ONE propose_changes with all fixes (merge values per recordId, keep notes) and issueIds, tell them what it changes and list the tasks, and wait for their confirmation before apply_proposal. Never apply on your own.',
      parameters: {
        type: 'object',
        properties: {
          kind: { type: 'string', description: 'Only these kinds (comma-separated), e.g. envelope_sex' },
          limit: { type: 'integer', description: '1 to 100, default 100' },
        },
      },
    },
  },
  MATCH_NOTEBOOK_TOOL,
  // Historial: find a save, link to it, preview and undo (server/history.mjs).
  ...HISTORY_TOOLS,
  {
    type: 'function',
    function: {
      name: 'apply_proposal',
      description:
        "Write a pending proposal to Google Sheets. Only call this when the person's latest message explicitly approves it (e.g. 'sí, aplícalo', 'está correcto'). It writes what the table shows: your values, the cells the person typed, and not the cells the person set back to the sheet value (a row left with nothing to write is skipped). Optionally only some rows, by their index.",
      parameters: {
        type: 'object',
        properties: { proposalId: { type: 'string' }, indexes: { type: 'array', items: { type: 'integer' } } },
        required: ['proposalId'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'update_proposal',
      description:
        `Revise a pending proposal in place (the person sees the table change live): when the person corrects something ('la especie es X', 'quita la fila 3', 'falta el colector'), update the SAME proposal instead of making a new one. rows = cells of rows already in it, by their index: a value replaces what you proposed there; null drops your proposed change to that cell (an existing row keeps the sheet's value, a new row's cell stays empty) and never empties a cell; {"clear": true} empties the sheet's cell (only when the person wants it emptied; the table shows it in red as vaciar). changes / newRows = more rows (a recordId already in it is merged into its row); removeRows = indexes to take out. Every value is checked as in propose_changes (nothing is saved if one fails). Notes columns: your text is added after the existing note with the "d/m/yy INI: " prefix ({"replace": "…"} rewrites the whole note, only when asked). Cells the person edited in the table are theirs: they come back as conflicts and are kept; tell the person, and set overridePersonEdits only when they ask you to replace them. Returns the proposal's rows with their index (a cell to be emptied shows as {"clear": true}; context rows of match_notebook are marked context and never written).`,
      parameters: {
        type: 'object',
        properties: {
          proposalId: { type: 'string' },
          rows: {
            type: 'array',
            items: {
              type: 'object',
              properties: {
                index: { type: 'integer', description: 'The row index in the proposal (propose_changes / get_proposal)' },
                values: {
                  type: 'object',
                  description:
                    'Column → new value; dates as YYYY-MM-DD, times as H:MM. null = drop your change to that cell (it does NOT empty it); {"clear": true} = empty the cell',
                },
                note: { type: 'string' },
              },
              required: ['index'],
            },
          },
          changes: {
            type: 'array',
            items: {
              type: 'object',
              properties: { recordId: { type: 'string' }, values: { type: 'object', description: VALUES_DOC }, note: { type: 'string' } },
              required: ['recordId', 'values'],
            },
          },
          newRows: {
            type: 'array',
            items: {
              type: 'object',
              properties: { sheet: { type: 'string' }, values: { type: 'object' }, note: { type: 'string' } },
              required: ['sheet', 'values'],
            },
          },
          removeRows: { type: 'array', items: { type: 'integer' } },
          reason: { type: 'string', description: 'A new title for the proposal, only if its subject changed' },
          overridePersonEdits: {
            type: 'boolean',
            description: 'Replace cells the person edited by hand; only when the person asked for it',
          },
        },
        required: ['proposalId'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'get_proposal',
      description:
        "A proposal as the person sees it now: each row with its index, values (dates YYYY-MM-DD), note and personEdits (cells the person corrected by hand in the table, or set back to the sheet value with the table's «Valor de la hoja» button, with what you had proposed; those set back are not written). Read it when the person says they changed the table, before update_proposal on a proposal you did not just make, and before apply_proposal if they edited it.",
      parameters: { type: 'object', properties: { proposalId: { type: 'string' } }, required: ['proposalId'] },
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
  // New rows of a proposal, once written: their record IDs (to show their sheet rows).
  if (!has('ai_proposals', 'created_json')) db.exec('ALTER TABLE ai_proposals ADD COLUMN created_json TEXT');
  // Issues of the Revisión tab a proposal fixes (list_agreed_fixes): marked applied when it is written.
  if (!has('ai_proposals', 'issues_json')) db.exec('ALTER TABLE ai_proposals ADD COLUMN issues_json TEXT');
  // Proposals are revised in place (update_proposal, the person's edits in the table): a revision per
  // proposal, when and by whom ('ai' or 'person') it last changed.
  if (!has('ai_proposals', 'revision')) db.exec('ALTER TABLE ai_proposals ADD COLUMN revision INTEGER NOT NULL DEFAULT 1');
  if (!has('ai_proposals', 'updated_at')) db.exec('ALTER TABLE ai_proposals ADD COLUMN updated_at TEXT');
  if (!has('ai_proposals', 'last_by')) db.exec('ALTER TABLE ai_proposals ADD COLUMN last_by TEXT');
  // The T3 Code chat a proposal comes from (server/t3chats.mjs): its thread id ('' = looked for, not
  // found), its title then, and the tool-use id of the call that drafted it (to find the chat later).
  if (!has('ai_proposals', 't3_thread')) db.exec('ALTER TABLE ai_proposals ADD COLUMN t3_thread TEXT');
  if (!has('ai_proposals', 't3_title')) db.exec('ALTER TABLE ai_proposals ADD COLUMN t3_title TEXT');
  if (!has('ai_proposals', 't3_tool_use')) db.exec('ALTER TABLE ai_proposals ADD COLUMN t3_tool_use TEXT');
  db.exec('CREATE INDEX IF NOT EXISTS ai_proposals_owner_status ON ai_proposals(owner_id, status, created_at)');
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

async function providerFetch(ai, path, body, multipart = false, timeoutMs = 45000) {
  const key = await keyFor(ai);
  if (!key || !ai.model) throw new Error('AI provider is not configured');
  const response = await fetch(endpoint(ai, path), {
    method: 'POST',
    headers: { Authorization: `Bearer ${key}`, ...(multipart ? {} : { 'Content-Type': 'application/json' }) },
    body: multipart ? body : json(body),
    signal: AbortSignal.timeout(timeoutMs),
  });
  if (!response.ok) throw new Error(`AI provider returned HTTP ${response.status}`);
  const payload = await response.json();
  return payload;
}

async function complete(ai, messages, tools = TOOLS, model = ai.model, timeoutMs) {
  const payload = await providerFetch(
    { ...ai, model },
    'chat/completions',
    {
      model,
      messages,
      ...(tools?.length ? { tools, tool_choice: 'auto' } : {}),
    },
    false,
    timeoutMs,
  );
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
  const knowledge = createKnowledge(config);
  // The chats of T3 Code (its state and trace log, read-only): which one made a proposal, which one is open.
  const t3 = config.t3Chats ?? (config.t3?.home ? createT3Chats({ home: config.t3.home }) : null);
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

  /** A row as the model sees it: every value (formula cells computed), dates readable, the formulas worth reading. */
  const compact = (record, options) => compactRecord(record, options);

  /** find_records (server/records-tool.mjs): the rows it returns can be cited. */
  function findRows(args, context) {
    // Models reached through the API get tool answers cut at 18000 characters: a smaller budget.
    const out = findRecords(db, args, context.findBudget ? { budget: context.findBudget } : undefined);
    for (const row of out.found ?? []) {
      const record = store.getRecord(row.id);
      if (!record) continue;
      context.records.set(record.id, record);
      context.sources.set(record.id, recordSource(record));
    }
    return out;
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
      latestRows: recent.slice(0, 3).map(r => compact(r)),
    };
  }

  /** Columns that are formulas in the next unused (pre-made) row of a sheet: a new row leaves them. */
  function createFormulaFields(sheet) {
    return newRowFormulaFields(store, sheet);
  }

  /**
   * A new row for a proposal, checked now as the save will check it (strict lists,
   * IDs already used), so the assistant can correct it before the person sees it.
   */
  function proposedRow(candidate, index, ids, clientId = randomUUID()) {
    const at = `newRows[${index}]`;
    const sheet = String(candidate?.sheet ?? '');
    if (!moduleMap.has(sheet)) return { error: `${at}: unknown sheet ${clip(sheet, 60)}` };
    const raw = candidate.values;
    if (!raw || typeof raw !== 'object' || Array.isArray(raw) || !Object.keys(raw).length || Object.keys(raw).length > 80)
      return { error: `${at}: invalid values` };
    let values;
    try {
      values = validateValues(sheet, withSheetTimes(raw));
    } catch (e) {
      return { error: `${at}: ${e.message}` };
    }
    // A count kept as a sum (=12+15) goes into the new row over its pre-made formula; other formulas stay.
    for (const [key, value] of Object.entries(values)) if (value?.formula && isSumField(sheet, key)) values[key] = value.formula;
    const formulas = createFormulaFields(sheet);
    const kept = key => isSumField(sheet, key) && values[key] !== null;
    const dropped = Object.keys(values).filter(key => formulas.has(key) && !kept(key));
    for (const key of Object.keys(values)) if ((formulas.has(key) && !kept(key)) || values[key] === null) delete values[key];
    if (!Object.keys(values).length) return { error: `${at}: the new row has no values` };
    const lists = listOptions(store, sheet);
    for (const [field, value] of Object.entries(values)) {
      const problem = lists[field]?.strict && listProblem(lists, field, value);
      if (problem) return { error: `${at}: ${problem}` };
    }
    for (const [field, value, key] of uniqueKeys(sheet, values)) {
      const holder = ids.used().get(key)?.[0];
      if (holder) return { error: `${at}: ${value} is already used in ${holder.sheet} row ${holder.row}` };
      if (ids.proposed.has(key)) return { error: `${at}: ${value} appears twice in this proposal` };
      ids.proposed.add(key);
    }
    const identity = moduleMap.get(sheet).identityFields.map(key => values[key]).find(isIdValue);
    const time = Object.entries(raw).find(([key]) => TIME_FIELD.test(key))?.[1];
    return {
      change: {
        create: true,
        sheet,
        clientId,
        recordId: null,
        row: null,
        label: clip(identity ?? ([values.SPECIES, time].filter(Boolean).join(' ') || labelFor(sheet, values)), 80),
        before: {},
        values,
        replaceFormula: [],
        note: clip(candidate.note, 300),
        ...(dropped.length ? { dropped } : {}),
      },
    };
  }

  /** The IDs of a new row that must not be used elsewhere: [field, value, key] (tubes across the workbook). */
  const uniqueKeys = (sheet, values) =>
    Object.entries(values)
      .filter(([field, value]) => isUnique(sheet, field) && isIdValue(value))
      .map(([field, value]) => [field, value, `${TUBE_FIELD.test(field) ? 'tube' : `${sheet}:${field}`}\u0000${String(value).trim()}`]);

  const idsFor = () => {
    let index;
    return { proposed: new Set(), used: () => (index ??= uniqueIdIndex(store)) };
  };

  /**
   * The checked rows of a proposal (as the save will check them), or { error }.
   * `ids` is shared when a page is checked one row at a time (IDs repeated between rows).
   */
  function draftChanges(args, ids = idsFor()) {
    const edits = Array.isArray(args.changes) ? args.changes : [];
    const creates = Array.isArray(args.newRows) ? args.newRows : [];
    if (!edits.length && !creates.length) return { error: 'Provide changes to existing rows or newRows' };
    if (edits.length + creates.length > 100) return { error: 'Provide at most 100 rows per proposal' };
    const changes = [];
    for (const [i, candidate] of creates.entries()) {
      const out = proposedRow(candidate, i, ids);
      if (out.error) return out;
      changes.push(out.change);
    }
    for (const candidate of edits) {
      const old = store.getRecord(String(candidate?.recordId ?? ''));
      if (!old || old.missing) return { error: `Row ${clip(candidate?.recordId, 60)} not found; use find_records` };
      const raw = withSheetTimes(candidate.values);
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
        // A count kept as a sum is shown and written as its formula text (=12+15), over the old sum.
        const sum = isSumField(old.sheet, key) ? simpleSum(old.formulas?.[key]) : null;
        if (values[key]?.formula && isSumField(old.sheet, key)) values[key] = values[key].formula;
        if (sum) {
          before[key] = sum;
          continue;
        }
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
    return { changes };
  }

  /** A drafted proposal saved for review: it shows at once in Cambios propuestos (and the chat). */
  function saveProposal(changes, reason, context, issueIds = []) {
    const id = randomUUID();
    const time = now();
    const chat = context.t3 ? chatOfCall(context) : null;
    db.prepare(
      'INSERT INTO ai_proposals (id,thread_id,owner_id,changes_json,reason,status,created_at,issues_json,updated_at,last_by,t3_thread,t3_title,t3_tool_use) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)',
    ).run(
      id,
      context.threadId,
      owner(context.user),
      json(changes),
      clip(reason, 500),
      'pending',
      time,
      issueIds.length ? json(issueIds) : null,
      time,
      'ai',
      chat?.id ?? null,
      chat?.title ?? null,
      context.t3?.toolUseId ?? null,
    );
    const proposal = { id, changes, reason: clip(reason, 500), status: 'pending' };
    context.proposals.push(proposal);
    changed(owner(context.user));
    return proposal;
  }

  const initialsCache = new Map();
  const initialsOf = user => {
    const key = owner(user) || user?.username || '';
    const hit = initialsCache.get(key);
    if (hit && hit.until > Date.now()) return hit.value;
    const value = initialsFor(user);
    initialsCache.set(key, { value, until: Date.now() + 600000 });
    return value;
  };

  /**
   * A note the assistant writes, in the team's form: "d/m/yy INI: text" after
   * what the cell already holds (" | " between them), never over it. Text that
   * already starts with the old note (the assistant kept it) only gets its new
   * part prefixed; a note already dated keeps its own prefix.
   */
  function addedNote(before, text, user) {
    const note = String(text).trim();
    const dated = s => (NOTE_PREFIX.test(s) ? s : noteText(s, { today: ecuadorDay(), initials: initialsOf(user) }));
    if (isNone(before)) return dated(note);
    const old = String(before).trim();
    if (note === old) return old;
    if (note.startsWith(old)) {
      const tail = note.slice(old.length).replace(/^\s*\|\s*/, '').trim();
      return tail ? `${old} | ${dated(tail)}` : old;
    }
    return `${old} | ${dated(note)}`;
  }

  /**
   * The values the assistant gave for one row, as a proposal keeps them. null,
   * "" or a missing value is no change (listed in `ignored`, or DROP with
   * keepDrops, for update_proposal to take back a change it proposed);
   * { clear: true } empties an existing row's cell (null, the only way to);
   * { replace: text } is taken as it is; a note is added after the existing one.
   */
  function fromAssistant(record, raw, user, { keepDrops = false } = {}) {
    if (!raw || typeof raw !== 'object' || Array.isArray(raw)) return { values: raw, ignored: [] };
    const values = {},
      ignored = [];
    const nothing = field => (keepDrops ? (values[field] = DROP) : ignored.push(field));
    for (const [field, value] of Object.entries(raw)) {
      const object = value && typeof value === 'object' && !Array.isArray(value);
      if (value === null || value === undefined || value === '') nothing(field);
      else if (object && (value.clear === true || ('replace' in value && isNone(value.replace) && value.replace !== 'NA'))) {
        if (record) values[field] = null;
        else if (keepDrops) values[field] = DROP;
      } else if (object && 'replace' in value) values[field] = value.replace;
      else if (NOTE_FIELD.test(field) && typeof value === 'string') values[field] = addedNote(record?.values?.[field], value, user);
      else values[field] = value;
    }
    return { values, ignored };
  }

  /** propose_changes' arguments with the assistant's values read as fromAssistant says. */
  function assistantArgs(args, user) {
    const ignored = [];
    const changes = [];
    for (const change of Array.isArray(args.changes) ? args.changes : []) {
      const record = store.getRecord(String(change?.recordId ?? ''));
      const out = fromAssistant(record && !record.missing ? record : null, change?.values, user);
      ignored.push(...out.ignored.map(field => `${record?.label ?? clip(change?.recordId, 60)}: ${field}`));
      // Only nulls: nothing to change in that row.
      if (out.ignored.length && out.values && !Object.keys(out.values).length) continue;
      changes.push({ ...change, values: out.values });
    }
    const newRows = (Array.isArray(args.newRows) ? args.newRows : []).map(row => ({
      ...row,
      values: fromAssistant(null, row?.values, user).values,
    }));
    return { args: { ...args, changes, newRows }, ignored };
  }

  /**
   * propose_changes as the assistant calls it; `literal` for fixes built by the
   * app (the Revisión tab), whose values are taken as they are.
   */
  function proposeChanges(input, context, { literal = false } = {}) {
    if (!EDITORS.includes(context.user.role)) return { error: 'Your role cannot propose edits' };
    const { args, ignored } = literal ? { args: input, ignored: [] } : assistantArgs(input, context.user);
    if (ignored.length && !args.changes.length && !args.newRows.length)
      return { error: 'Every value was null, and null means no change. To empty a cell give {"clear": true}.' };
    const drafted = draftChanges(args);
    if (drafted.error) return drafted;
    const { changes } = drafted;
    const issueIds = Array.isArray(args.issueIds) ? args.issueIds.slice(0, 500).map(i => clip(i, 200)) : [];
    const { id } = saveProposal(changes, args.reason, context, issueIds);
    const dropped = [...new Set(changes.flatMap(c => c.dropped ?? []))];
    return {
      proposalId: id,
      rows: changes.length,
      status: 'waiting for the person to confirm',
      ...(dropped.length ? { leftOut: `Formula columns left out of the new rows: ${dropped.join(', ')}` } : {}),
      ...(ignored.length ? { noChange: ignored.slice(0, 50), noChangeNote: 'null means no change: these cells keep the sheet value. To empty one give {"clear": true}.' } : {}),
      // Row indexes for update_proposal (new rows first, then edits of existing rows).
      table: changes.map((c, index) => ({ index, sheet: c.sheet, label: c.label, ...(c.create ? { create: true } : { row: c.row }) })),
    };
  }

  // ------------------------------------------------------------ proposals revised in place
  /*
   * A pending proposal changes while the person reviews it: the assistant
   * revises it (update_proposal, a notebook page matched again) and the person
   * corrects cells in the table (POST …/edit). Each row keeps its key (the
   * clientId of a new row, the recordId of an edited one) and the cells the
   * person typed: personEdits = field → { ai: what the assistant had proposed
   * (absent: no change to that cell), by, at }. The assistant does not
   * overwrite those unless asked; it gets them back as conflicts. A cell the
   * person set back to the sheet's value (or emptied, in a new row) keeps its
   * mark with the assistant's value and is left out of `values`: the table shows
   * the suggestion aside, and applying writes only `values` (a row left without
   * any is not written).
   */
  const rowKey = change => change.clientId ?? change.recordId;
  const proposedOf = (change, field) => (field in change.values ? change.values[field] : undefined);
  const same = (a, b) => comparable(a) === comparable(b);
  /** A value as the save will store it (dates as serials, times as day fractions), to compare it. */
  function normal(sheet, field, value) {
    try {
      const out = validateValues(sheet, withSheetTimes({ [field]: value }))[field];
      return out?.formula ?? out;
    } catch {
      return value;
    }
  }

  /** A cell of a row set (not yet checked). In an existing row, the sheet's own value means no change there. */
  function setCell(change, field, value) {
    const values = { ...change.values };
    // The assistant dropped its change: the cell goes back to the sheet's value (or empty, in a new row).
    if (value === DROP) {
      delete values[field];
      return { ...change, values };
    }
    if (!change.create && !isSumField(change.sheet, field)) {
      const record = store.getRecord(change.recordId);
      if (same(normal(change.sheet, field, value), record?.values?.[field] ?? null)) {
        delete values[field];
        return { ...change, values };
      }
    }
    if (value === null && change.create) delete values[field];
    else values[field] = value;
    return { ...change, values };
  }

  /** One row checked again as propose_changes checks it: { change, dropped } or { error }. */
  function redraftRow(change, index, others, used) {
    const personEdits = change.personEdits && Object.keys(change.personEdits).length ? change.personEdits : undefined;
    const keep = fresh => ({ ...change, ...fresh, personEdits });
    if (!Object.keys(change.values).length)
      return { change: keep(change.create ? { values: {} } : { values: {}, before: {}, replaceFormula: [] }), dropped: [] };
    if (change.create) {
      const proposed = new Set(others.filter(c => c.create).flatMap(c => uniqueKeys(c.sheet, c.values).map(k => k[2])));
      const out = proposedRow({ sheet: change.sheet, values: change.values, note: change.note }, index, { proposed, used }, change.clientId);
      // Only formula columns: they are left out, as an empty row.
      if (/the new row has no values$/.test(out.error ?? ''))
        return { change: keep({ values: {} }), dropped: Object.keys(change.values) };
      if (out.error) return { error: out.error.replace(/^newRows\[\d+\]: /, '') };
      const { dropped = [], ...fresh } = out.change;
      // A row whose ID (and species) was set back or emptied keeps the name it was shown with, not "Insectary".
      const named =
        !!fresh.values.SPECIES || moduleMap.get(change.sheet).identityFields.some(key => isIdValue(fresh.values[key]));
      return { change: { ...keep(fresh), ...(named || !change.label ? {} : { label: change.label }), dropped: undefined }, dropped };
    }
    const out = draftChanges({ changes: [{ recordId: change.recordId, values: change.values, note: change.note }] });
    if (out.error === 'Every proposed value is already in the sheet')
      return { change: keep({ values: {}, before: {}, replaceFormula: [] }), dropped: [] };
    if (out.error) return { error: out.error };
    return { change: keep(out.changes[0]), dropped: [] };
  }

  /**
   * Revises a proposal's rows. `by`: 'ai' or 'person'. ops: set [{ ref (index or
   * key), values, note, before }], remove [ref], add { changes, newRows } (the
   * assistant's new rows), addEmpty [{ sheet }] (an empty new row the person fills).
   * Each cell is checked as it is set: a refused cell keeps its value (for the
   * assistant the caller refuses the whole revision).
   */
  function reviseChanges(changes, ops, { by, force = false, user } = {}) {
    let rows = changes.map(c => ({ ...c, values: { ...c.values }, ...(c.personEdits ? { personEdits: { ...c.personEdits } } : {}) }));
    const find = ref => (typeof ref === 'number' ? (rows[ref] ? ref : -1) : rows.findIndex(c => rowKey(c) === ref));
    const out = { conflicts: [], rejected: [], overrode: [], leftOut: [] };
    let index;
    const used = () => (index ??= uniqueIdIndex(store));
    const where = i => ({ index: i, key: rowKey(rows[i]), label: rows[i].label, sheet: rows[i].sheet });
    const who = user ? clip(user.displayName || user.username || owner(user), 80) : 'person';

    for (const op of ops.set ?? []) {
      const i = find(op.ref);
      if (i < 0) {
        out.rejected.push({ ref: op.ref, message: `Row ${clip(op.ref, 60)} is not in the proposal` });
        continue;
      }
      if (typeof op.note === 'string' && by === 'ai') rows[i] = { ...rows[i], note: clip(op.note, 300) };
      const values = op.values && typeof op.values === 'object' && !Array.isArray(op.values) ? op.values : {};
      for (const [field, raw] of Object.entries(values)) {
        const row = rows[i];
        const current = proposedOf(row, field);
        const mark = row.personEdits?.[field];
        // "Valor de la IA" where the assistant proposed nothing (or it is already there): nothing to do.
        if (raw === AI_VALUE && !(mark && 'ai' in mark)) continue;
        const value = raw === AI_VALUE ? mark.ai : raw === '' || raw === undefined ? null : raw;
        if (by === 'ai' && mark && !force) {
          if (value === DROP ? current !== undefined : !same(normal(row.sheet, field, value), current))
            out.conflicts.push({ ...where(i), field, person: current ?? null, yours: value === DROP ? 'no change' : value });
          continue;
        }
        const drafted = redraftRow(setCell(row, field, value), i, rows.filter((_, j) => j !== i), used);
        if (drafted.error || drafted.dropped.includes(field)) {
          const message = drafted.error ?? `${field} is a formula in the new row; it is left empty`;
          if (drafted.error || by === 'person') out.rejected.push({ ...where(i), field, message });
          else out.leftOut.push(field);
          if (drafted.error) continue;
        }
        const next = drafted.change;
        const after = proposedOf(next, field);
        const marks = { ...next.personEdits };
        if (by === 'ai') delete marks[field];
        else {
          const ai = mark ? mark.ai : current;
          // What the person saw when they started typing was replaced by the assistant meanwhile.
          if (op.before && field in op.before && !same(op.before[field], current) && !same(current, after))
            out.overrode.push({ ...where(i), field, ai: current ?? null });
          // Back to what the assistant proposed (or, where it proposed nothing, to no change): no longer theirs.
          // A cell set back to the sheet keeps the assistant's value aside, even one that emptied it (null).
          const back = after === undefined || ai === undefined ? after === ai : same(after, ai);
          if (back) delete marks[field];
          else marks[field] = { ...(ai === undefined ? {} : { ai }), by: who, at: now() };
        }
        rows[i] = { ...next, personEdits: Object.keys(marks).length ? marks : undefined };
        // A notebook line shown only for context becomes a real change once someone gives it a value.
        if (rows[i].context && Object.keys(rows[i].values).length) rows[i] = { ...rows[i], context: undefined };
      }
    }

    const removing = new Set();
    for (const ref of ops.remove ?? []) {
      const i = find(ref);
      if (i < 0) continue;
      if (by === 'ai' && !force && Object.keys(rows[i].personEdits ?? {}).length) {
        out.conflicts.push({ ...where(i), field: null, message: 'The person edited this row in the table; it was kept' });
        continue;
      }
      removing.add(i);
    }
    rows = rows.filter((_, i) => !removing.has(i));

    const add = ops.add;
    if (add && ((add.changes ?? []).length || (add.newRows ?? []).length)) {
      const proposed = new Set(rows.filter(c => c.create).flatMap(c => uniqueKeys(c.sheet, c.values).map(k => k[2])));
      const drafted = draftChanges(add, { proposed, used });
      if (drafted.error) out.rejected.push({ message: drafted.error });
      else {
        rows.push(...drafted.changes.map(({ dropped, ...c }) => c));
        out.leftOut.push(...drafted.changes.flatMap(c => c.dropped ?? []));
      }
    }
    for (const { sheet } of ops.addEmpty ?? []) {
      if (!moduleMap.has(sheet)) continue;
      rows.push({ create: true, sheet, clientId: randomUUID(), recordId: null, row: null, label: '', before: {}, values: {}, replaceFormula: [], note: '' });
    }
    if (rows.length > 100) out.rejected.push({ message: 'At most 100 rows per proposal' });
    out.leftOut = [...new Set(out.leftOut)];
    return { changes: rows, ...out };
  }

  /** Saves a revised proposal (only while pending): its revision goes up and the Asistente tab follows it at once. */
  function saveRevision(proposal, changes, by, reason = null) {
    const row = db
      .prepare(
        "UPDATE ai_proposals SET changes_json = ?, reason = coalesce(?, reason), revision = revision + 1, updated_at = ?, last_by = ? WHERE id = ? AND status = 'pending' RETURNING revision",
      )
      .get(json(changes), reason, now(), by, proposal.id);
    if (row) changed(proposal.owner_id);
    return row?.revision ?? null;
  }

  const ownProposal = (id, user) =>
    db.prepare('SELECT * FROM ai_proposals WHERE id = ? AND owner_id = ?').get(String(id ?? ''), owner(user));
  const ownProposalListed = id =>
    db.prepare('SELECT p.*, t.title FROM ai_proposals p JOIN ai_threads t ON t.id = p.thread_id WHERE p.id = ?').get(id);
  /** A proposal as Cambios propuestos lists it (with the conversation it comes from: its T3 chat, if known). */
  const listedView = (r, titles = new Map()) => ({
    ...proposalView({ id: r.id, changes: [], reason: r.reason, status: r.status }, r),
    createdAt: r.created_at,
    source: (r.t3_thread && (titles.get(r.t3_thread)?.title ?? r.t3_title)) || r.title,
    chat: r.t3_thread || null,
  });

  /** The rows of a proposal as the assistant reads them: index, values with readable dates, the person's edits. */
  function proposalTable(changes) {
    const readable = (sheet, field, value) =>
      moduleMap.get(sheet)?.fields.find(f => f.key === field)?.type === 'date' && typeof value === 'number' ? isoDate(value) : value;
    return changes.map((c, index) => ({
      index,
      sheet: c.sheet,
      label: c.label,
      ...(c.create ? { create: true } : { row: c.row, recordId: c.recordId }),
      ...(c.context ? { context: 'already in the sheet: shown for context, never written' } : {}),
      // An existing row's cell to be emptied reads as it is given: { clear: true } (null means no change).
      values: Object.fromEntries(
        Object.entries(c.values).map(([f, v]) => [f, v === null && !c.create ? { clear: true } : readable(c.sheet, f, v)]),
      ),
      ...(c.note ? { note: c.note } : {}),
      ...(c.personEdits
        ? {
            personEdits: Object.fromEntries(
              Object.entries(c.personEdits).map(([f, m]) => [
                f,
                {
                  // A cell the person set back: an existing row keeps the sheet's value, a new row's stays empty.
                  value:
                    f in c.values
                      ? readable(c.sheet, f, c.values[f])
                      : c.create
                        ? 'left empty (not written)'
                        : 'no change (keep the sheet value)',
                  youProposed: 'ai' in m ? readable(c.sheet, f, m.ai) : 'no change',
                },
              ]),
            ),
          }
        : {}),
    }));
  }

  function getProposal(args, context) {
    const proposal = ownProposal(args.proposalId, context.user);
    if (!proposal) return { error: 'Proposal not found' };
    return {
      proposalId: proposal.id,
      status: proposal.status,
      revision: proposal.revision,
      reason: proposal.reason,
      lastChangedBy: proposal.last_by ?? 'ai',
      rows: proposalTable(parse(proposal.changes_json) ?? []),
    };
  }

  /** update_proposal: the assistant revises a pending proposal the person is looking at. */
  function updateProposal(args, context) {
    if (!EDITORS.includes(context.user.role)) return { error: 'Your role cannot propose edits' };
    const proposal = ownProposal(args.proposalId, context.user);
    if (!proposal) return { error: 'Proposal not found' };
    if (proposal.status !== 'pending')
      return { error: `The proposal is ${proposal.status}; draft a new one with propose_changes` };
    const changes = parse(proposal.changes_json) ?? [];
    // The assistant's values: null takes back its change to a cell, { clear: true } empties it, notes are added.
    const own = (i, values) => {
      const row = changes[i];
      const record = row && !row.create ? store.getRecord(row.recordId) : null;
      return fromAssistant(record, values, context.user, { keepDrops: true }).values;
    };
    const set = (Array.isArray(args.rows) ? args.rows : []).map(r => {
      const ref = Number.isInteger(r?.index) ? r.index : -1;
      return { ref, values: own(ref, r?.values), note: r?.note };
    });
    const extra = assistantArgs({ changes: [], newRows: args.newRows }, context.user).args;
    const add = { changes: [], newRows: extra.newRows };
    // A row already in the proposal is revised, not added twice.
    for (const c of Array.isArray(args.changes) ? args.changes : []) {
      const i = changes.findIndex(r => !r.create && r.recordId === String(c?.recordId ?? ''));
      if (i >= 0) set.push({ ref: i, values: own(i, c.values), note: c.note });
      else add.changes.push(...assistantArgs({ changes: [c] }, context.user).args.changes);
    }
    const remove = (Array.isArray(args.removeRows) ? args.removeRows : []).filter(Number.isInteger);
    if (!set.length && !remove.length && !add.changes.length && !add.newRows.length && !args.reason)
      return { error: 'Give rows, changes, newRows or removeRows' };
    const out = reviseChanges(changes, { set, remove, add }, { by: 'ai', force: !!args.overridePersonEdits, user: context.user });
    if (out.rejected.length) return { error: 'Nothing was changed', problems: out.rejected.slice(0, 20) };
    const reason = args.reason ? clip(args.reason, 500) : null;
    const unchanged = json(out.changes) === json(changes) && !reason;
    const revision = unchanged ? proposal.revision : saveRevision(proposal, out.changes, 'ai', reason);
    if (revision === null) return { error: 'The proposal is no longer pending' };
    return {
      proposalId: proposal.id,
      revision,
      ...(unchanged ? { unchanged: true } : {}),
      rows: proposalTable(out.changes),
      ...(out.leftOut.length ? { leftOut: `Formula columns left out of the new rows: ${out.leftOut.join(', ')}` } : {}),
      ...(out.conflicts.length
        ? {
            conflicts: out.conflicts,
            note: 'The person edited these cells by hand; they were kept. Tell the person, and only replace them (overridePersonEdits) if they ask.',
          }
        : {}),
    };
  }

  /**
   * A notebook page matched again (match_notebook with replaceProposalId) takes
   * the place of the page's proposal, keeping its id; the person's cells stay,
   * and are conflicts where the new match reads something else.
   */
  function carryPersonEdits(old, fresh, user) {
    const sameRow = (a, b) =>
      a.create ? b.create && a.sheet === b.sheet && !!a.label && a.label === b.label : !b.create && a.recordId === b.recordId;
    // New rows keep their key, so the table keeps its ticks.
    const rows = fresh.map(c => {
      const before = c.create && old.find(o => sameRow(o, c));
      return before ? { ...c, clientId: before.clientId } : c;
    });
    const conflicts = [];
    const set = [];
    for (const o of old) {
      const edits = Object.entries(o.personEdits ?? {});
      if (!edits.length) continue;
      const i = rows.findIndex(c => sameRow(o, c));
      if (i < 0) {
        conflicts.push({ label: o.label, sheet: o.sheet, field: null, message: 'A row the person edited is no longer in the match; their edits were dropped' });
        continue;
      }
      for (const [field, mark] of edits) {
        const person =
          field in o.values ? o.values[field] : o.create ? null : (store.getRecord(o.recordId)?.values?.[field] ?? null);
        const ai = proposedOf(rows[i], field);
        if (ai !== undefined && !same(ai, person) && !same(ai, mark.ai))
          conflicts.push({ index: i, label: rows[i].label, field, person, yours: ai });
        set.push({ ref: i, values: { [field]: person } });
      }
    }
    const out = reviseChanges(rows, { set }, { by: 'person', user });
    for (const r of out.rejected) conflicts.push({ ...r, message: `The person's value no longer fits: ${r.message}` });
    return { changes: out.changes, conflicts };
  }

  /** Writes the chosen rows of a proposal as one save (undoable in Historial). */
  async function applyProposal(proposal, user, { requestId, indexes, reason }) {
    if (proposal.status !== 'pending')
      throw Object.assign(new Error('Proposal has already been applied.'), { status: 409, code: 'proposal_used' });
    if (!EDITORS.includes(user.role))
      throw Object.assign(new Error('Your role cannot apply changes.'), { status: 403, code: 'forbidden' });
    const all = parse(proposal.changes_json) ?? [];
    // A row left without values (the person emptied it in the table) has nothing to write, and a
    // notebook line shown only for context (match_notebook includeUnchanged) is never written.
    const chosen = (
      Array.isArray(indexes) && indexes.length ? [...new Set(indexes.map(Number))].filter(i => all[i]) : all.map((_, i) => i)
    ).filter(i => Object.keys(all[i].values ?? {}).length && !all[i].context);
    if (!chosen.length) throw Object.assign(new Error('No rows selected.'), { status: 400, code: 'nothing_selected' });
    const claimed = db
      .prepare("UPDATE ai_proposals SET status = 'applying' WHERE id = ? AND status = 'pending'")
      .run(proposal.id);
    if (!claimed.changes)
      throw Object.assign(new Error('Proposal is already being applied.'), { status: 409, code: 'proposal_used' });
    changed(proposal.owner_id);
    try {
      const result = await store.applyProposal(
        chosen.map(i => all[i]),
        { user, requestId, reason: clip(reason || proposal.reason, 500) },
      );
      const status = ['verified', 'unchanged'].includes(result?.status) ? 'applied' : 'needs_review';
      const created = Object.fromEntries((result?.created ?? []).map(c => [c.clientId, c.recordId]));
      db.prepare(
        'UPDATE ai_proposals SET status = ?, applied_at = ?, applied_json = ?, created_json = ? WHERE id = ?',
      ).run(status, status === 'applied' ? now() : null, json(chosen), json(created), proposal.id);
      // Agreed issues of the Revisión tab whose rows were written: now applied.
      const issueIds = parse(proposal.issues_json ?? 'null');
      if (status === 'applied' && issueIds?.length)
        markApplied(store, issueIds, { recordIds: new Set(chosen.map(i => all[i].recordId)), proposalId: proposal.id, user });
      return { proposalId: proposal.id, status, applied: chosen, result };
    } catch (cause) {
      db.prepare("UPDATE ai_proposals SET status = 'needs_review' WHERE id = ?").run(proposal.id);
      throw cause;
    } finally {
      changed(proposal.owner_id);
    }
  }

  /**
   * A proposal as the review table shows it: each row with the current values of
   * the changed columns. New rows have no current values (and, once written, their row).
   * `row` is its ai_proposals row: its rows as last revised, revision, status.
   * A pending one also gives what the table needs to edit it: every current value
   * of the edited rows (columns can be added), their formula columns, and those
   * of the pre-made rows new rows go into.
   */
  function proposalView(proposal, row) {
    const changes = (row?.changes_json && parse(row.changes_json)) || proposal.changes;
    const status = row?.status ?? proposal.status;
    const open = status === 'pending';
    const fields = [...new Set(changes.flatMap(c => [...Object.keys(c.values), ...Object.keys(c.personEdits ?? {})]))];
    const sheets = [...new Set(changes.map(c => c.sheet))];
    const typeOf = f =>
      sheets.map(s => moduleMap.get(s)?.fields.find(x => x.key === f)?.type).find(Boolean) ?? 'text';
    const created = parse(row?.created_json ?? 'null') ?? {};
    const locked = (sheet, keys) => keys.filter(f => !TYPED_OVER_FORMULA[sheet]?.has(f) && !isSumField(sheet, f));
    const newRowFormulas = open
      ? Object.fromEntries(
          sheets.filter(s => changes.some(c => c.create && c.sheet === s)).map(s => [s, locked(s, [...createFormulaFields(s)])]),
        )
      : {};
    return {
      ...proposal,
      status,
      revision: row?.revision ?? 1,
      updatedAt: row?.updated_at ?? null,
      lastBy: row?.last_by ?? null,
      appliedAt: row?.applied_at ?? null,
      applied: parse(row?.applied_json ?? 'null'),
      sheets,
      fields,
      types: Object.fromEntries(fields.map(f => [f, typeOf(f)])),
      newRowFormulas,
      changes: changes.map((change, index) => {
        const recordId = change.create ? (created[change.clientId] ?? null) : change.recordId;
        const record = recordId ? store.getRecord(recordId) : null;
        return {
          ...change,
          key: rowKey(change),
          recordId,
          index,
          row: record?.row ?? change.row,
          label: change.label || record?.label || '',
          current: change.create ? {} : Object.fromEntries(fields.map(f => [f, record?.values?.[f] ?? null])),
          // The rest of the row, for columns the person adds to the table.
          ...(open && !change.create
            ? {
                rowValues: Object.fromEntries(Object.entries(record?.values ?? {}).filter(([, v]) => v !== null && v !== '')),
                formulas: locked(change.sheet, Object.keys(record?.formulas ?? {})),
              }
            : {}),
        };
      }),
    };
  }

  /*
   * Live list of proposals for the Asistente tab: a revision per person that
   * changes whenever one of their proposals is added, applied or discarded, and
   * requests that wait (long polling) until it changes.
   */
  const boot = randomUUID().slice(0, 8);
  const revisions = new Map();
  const waiters = new Map();
  const revisionOf = ownerId => `${boot}.${revisions.get(ownerId) ?? 0}`;
  function changed(ownerId) {
    revisions.set(ownerId, (revisions.get(ownerId) ?? 0) + 1);
    for (const wake of waiters.get(ownerId) ?? []) wake();
    waiters.delete(ownerId);
  }
  /** Waits for a change of the person's proposals, or until `moved()` says the chat to show changed (checked every 2 s). */
  function waitForChange(ownerId, seen, ms, moved = null) {
    if (seen !== revisionOf(ownerId)) return Promise.resolve();
    return new Promise(resolve => {
      const list = waiters.get(ownerId) ?? waiters.set(ownerId, new Set()).get(ownerId);
      const wake = () => {
        clearTimeout(timer);
        clearInterval(watch);
        list.delete(wake);
        resolve();
      };
      const timer = setTimeout(wake, ms);
      const watch = moved ? setInterval(() => moved() && wake(), 2000) : undefined;
      list.add(wake);
    });
  }

  // ------------------------------------------------------------ proposals by T3 chat
  const ago = (iso, ms) => new Date(Date.parse(iso) - ms).toISOString();
  /** The T3 chat of a tool call, if T3 has recorded the call already (by its tool-use id; Codex: the only chat answering). */
  function chatOfCall(context) {
    if (!t3) return null;
    const { toolUseId } = context.t3;
    const id = toolUseId ? t3.threadOfToolUse(toolUseId, ago(now(), 10 * 60_000)) : t3.onlyRunning(context.user.username);
    return id ? { id, title: t3.threads([id]).get(id)?.title ?? null } : null;
  }
  /*
   * Proposals from T3 Code whose chat is not known yet (T3 had not recorded the
   * call, or they were made before chats were recorded) are linked when the
   * list is asked for: by tool-use id at once (before the answer), and by the
   * chats' tool results naming the proposal at most every 5 s per person,
   * without holding the answer up. Not found 10 minutes after it was made:
   * left as a proposal outside the chats.
   */
  const unlinked = (ownerId, withToolUse = false) =>
    db
      .prepare(
        `SELECT p.id, p.created_at, p.t3_tool_use FROM ai_proposals p JOIN ai_threads t ON t.id = p.thread_id
         WHERE p.owner_id = ? AND p.t3_thread IS NULL AND t.title = 'T3 Code'${withToolUse ? ' AND p.t3_tool_use IS NOT NULL' : ''}`,
      )
      .all(ownerId);
  function link(ownerId, found) {
    if (!found.size) return;
    const titles = t3.threads([...found.values()]);
    const set = db.prepare('UPDATE ai_proposals SET t3_thread = ?, t3_title = ? WHERE id = ? AND t3_thread IS NULL');
    for (const [id, thread] of found) set.run(thread, titles.get(thread)?.title ?? null, id);
    changed(ownerId);
  }
  function linkByToolUse(ownerId) {
    if (!t3) return;
    const found = new Map();
    for (const r of unlinked(ownerId, true)) {
      const id = t3.threadOfToolUse(r.t3_tool_use, ago(r.created_at, 10 * 60_000));
      if (id) found.set(r.id, id);
    }
    link(ownerId, found);
  }
  const scannedAt = new Map();
  async function linkByResult(ownerId) {
    if (!t3 || Date.now() - (scannedAt.get(ownerId) ?? 0) < 5000) return;
    scannedAt.set(ownerId, Date.now());
    const rows = unlinked(ownerId);
    if (!rows.length || !t3.available) return;
    const since = ago(rows.map(r => r.created_at).sort()[0], 10 * 60_000);
    link(ownerId, await t3.findProposals(rows.map(r => r.id), since));
    const none = db.prepare("UPDATE ai_proposals SET t3_thread = '' WHERE id = ? AND t3_thread IS NULL");
    for (const r of rows) if (Date.now() - Date.parse(r.created_at) > 10 * 60_000) none.run(r.id);
  }

  /**
   * The chat whose proposals the panel shows when it follows T3 ("auto"): the
   * one open in T3 now (see t3chats.mjs), else the one most recently active (a
   * message in it, or one of its proposals changed; 'app' = the proposals made
   * outside T3 chats), else all of them (no T3 chats at all).
   */
  function followed(user, groups, current) {
    const open = t3?.open(user.username, current) ?? null;
    if (open) return { chat: open, how: 'open' };
    const latest = t3?.chatsOf(user.username, 1)[0];
    let best = latest ? { chat: latest.id, at: latest.lastUserAt ?? '' } : null;
    for (const g of groups.values()) if (!best || g.at > best.at) best = { chat: g.id, at: g.at };
    if (!best || (best.chat === 'app' && !latest && groups.size === 1)) return { chat: 'all', how: 'all' };
    return { chat: best.chat, how: 'recent' };
  }
  /** The chats with pending proposals (the panel's chat selector), most recently changed first. */
  function chatGroups(ownerId) {
    const rows = db
      .prepare(
        "SELECT t3_thread, t3_title, coalesce(updated_at, created_at) at FROM ai_proposals WHERE owner_id = ? AND status IN ('pending', 'applying')",
      )
      .all(ownerId);
    const groups = new Map();
    for (const r of rows) {
      const id = r.t3_thread || 'app';
      const g = groups.get(id) ?? groups.set(id, { id, title: r.t3_title ?? null, pending: 0, at: '' }).get(id);
      g.pending++;
      if (r.at > g.at) g.at = r.at;
    }
    return groups;
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
      return { records: (page.records ?? []).map(r => compact(r)), total: page.total };
    }
    if (name === 'find_records') return findRows(args, context);
    if (name === 'count_records') return countRecords(db, args);
    if (name === 'describe_sheet') return describeSheet(args);
    if (name === 'get_record') {
      const record = store.getRecord(clip(args.id, 120));
      if (!record) return { error: 'Record not found' };
      context.records.set(record.id, record);
      context.sources.set(record.id, recordSource(record));
      return compact(record, { allFormulas: true });
    }
    if (['search_knowledge', 'read_document', 'list_documents', 'sync_documents'].includes(name))
      return runKnowledgeTool(knowledge, name, args, context);
    if (name === 'run_report') {
      const response = await reports.build({ kind: args.kind, module: args.module, field: args.field });
      if (response.status !== 200) return response.body;
      for (const item of response.body.sources) context.sources.set(item.id, { ...item, type: 'record' });
      const result = { ...response.body, sources: response.body.sources.slice(0, 30) };
      context.results.push(result);
      return result;
    }
    if (name === 'check_data') {
      const out = checkData(store, {
        sheet: args.sheet ? clip(args.sheet, 100) : undefined,
        kind: args.kind ? clip(args.kind, 300) : undefined,
        recordId: args.recordId ? clip(args.recordId, 120) : undefined,
        limit: Math.min(Number(args.limit) || 50, 200),
        offset: args.offset,
      });
      for (const issue of out.issues.filter(i => i.recordId))
        context.sources.set(issue.recordId, { id: issue.recordId, type: 'record', sheet: issue.sheet, row: issue.row, label: issue.label });
      return { ...out, issues: withoutMsgs(out.issues) };
    }
    if (name === 'queue_wikiloc') {
      if (!EDITORS.includes(context.user.role)) return { error: 'Your role cannot queue walks' };
      return queueWalk(store, { url: clip(args.url, 500), refresh: !!args.refresh }, context.user);
    }
    if (name === 'get_walk')
      return walkDraft(store, {
        walkId: args.walkId ? clip(args.walkId, 60) : undefined,
        url: args.url ? clip(args.url, 500) : undefined,
        date: args.date ? clip(args.date, 10) : undefined,
        collector: args.collector ? clip(args.collector, 120) : undefined,
      });
    if (name === 'propose_changes') return proposeChanges(args, context);
    if (name === 'update_proposal') return updateProposal(args, context);
    if (name === 'get_proposal') return getProposal(args, context);
    if (name === 'list_agreed_fixes') return agreedFixes(store, { kind: args.kind ? clip(args.kind, 300) : undefined, limit: args.limit });
    if (name === 'match_notebook') return matchNotebook(args, context);
    if (HISTORY_TOOL_NAMES.has(name)) return runHistoryTool(store, name, args, context, { publicUrl: config.publicUrl });
    if (name === 'apply_proposal') {
      const proposal = db
        .prepare('SELECT * FROM ai_proposals WHERE id = ? AND thread_id = ?')
        .get(String(args.proposalId ?? ''), context.threadId);
      if (!proposal) return { error: 'Proposal not found in this conversation' };
      try {
        const out = await applyProposal(proposal, context.user, {
          requestId: `ai-${randomUUID()}`,
          indexes: args.indexes,
          reason: tpl('Confirmado en el chat'),
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
    const letters = words.map(w => w.replace(/[^\p{L}]/gu, '')[0]).filter(Boolean);
    return match ? match.split(' - ')[0].trim() : letters.join('').toUpperCase() || 'APP';
  }

  function systemPrompt(user) {
    return [
      `Today is ${now().slice(0, 10)}. You are talking with ${user.displayName || user.username} (initials ${initialsFor(user)}, role ${user.role}).`,
      'Reply briefly, in the language the person writes in (Spanish or English); sheet names, column names, codes and values stay exactly as they are in the workbook. Refer to rows by their identifier (e.g. 5VB, CAM078038) and sheet row, never by internal app IDs.',
      'Use the tools to read exact rows before answering. Only claim what the tools show. Never infer survival, fertility, mating, genotype or identity from counts.',
      'Questions over many rows: find_records with filters, near (distance to a place) and only the fields you need; count_records for counts. When an answer is truncated, narrow it; never read the database or the server files instead.',
      'In proposals, null means no change; empty a cell only with {"clear": true} when the person asks. Notes you add go after the existing note as "d/m/yy INI: text".',
      'Changes are drafted with propose_changes; the person reviews them in a table and confirms. Use apply_proposal only when their latest message explicitly approves a proposal. Never say a change was written unless apply_proposal returned applied.',
      'When the person corrects a pending proposal ("la especie es X", "quita esa fila"), revise the same one with update_proposal (rows by index) instead of drafting a new one. The person can also edit cells in the table: get_proposal shows their edits (personEdits); never overwrite them unless they ask (update_proposal returns them as conflicts).',
      'check_data lists inconsistencies with ready fixes; queue_wikiloc and get_walk turn a Wikiloc monitoring walk into newRows for propose_changes.',
      '"Aplica las correcciones acordadas": list_agreed_fixes, then ONE propose_changes with its fixes and issueIds, list the tasks (Drive work), and wait for the person to confirm.',
      'A photo of a notebook page, envelope or label: transcribe every line as the digitalizar-cuaderno instructions say, then match_notebook compares it with the sheet and drafts one proposal per page.',
      'Meetings, protocols, reports and presentations of the project Drive: search_knowledge, list_documents (e.g. the last meeting) and read_document; sync_documents brings them up to date with Drive when asked (not automatic). When you answer from a document, name it and give its Drive link (sourceUrl).',
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
    const run = (resume, text = prompt, earlier = []) =>
      runClaude(claude, {
        content: [...earlier, ...content.slice(0, -1), { type: 'text', text }],
        system: systemPrompt(user),
        mcpUrl,
        token,
        resume,
        sessionId: resume ? null : randomUUID(),
        docsDir: join(here, '..', 'docs'),
      });
    /**
     * A new session starts with the conversation so far: a notebook page's
     * conversation begins with its photo and transcription, written by the app.
     */
    const fresh = async () => {
      const recent = db
        .prepare(
          'SELECT role,content,attachments_json FROM ai_messages WHERE thread_id = ? ORDER BY created_at DESC, rowid DESC LIMIT 11',
        )
        .all(threadId)
        .reverse()
        .slice(0, -1);
      const text = recent.map(m => `${m.role === 'user' ? 'Persona' : 'Asistente'}: ${clip(m.content, 6000)}`).join('\n\n');
      const photos = images.length ? [] : recent.flatMap(m => (parse(m.attachments_json) ?? []).map(a => a.id)).slice(-2);
      const earlier = (await loadImages(photos).catch(() => [])).map(image => ({
        type: 'image',
        source: { type: 'base64', media_type: image.mimeType, data: image.data.toString('base64') },
      }));
      return run(null, text ? `Conversación anterior:\n${text}\n\n${prompt}` : prompt, earlier);
    };
    try {
      let out;
      if (!thread?.claude_session) out = await fresh();
      else
        try {
          out = await run(thread.claude_session);
        } catch (e) {
          if (!e.missingSession) throw e;
          // The saved session is gone (e.g. a new server): start again with the recent messages as context.
          out = await fresh();
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
    // Models without Claude Code's skills get the notebook instructions with the photo.
    const skill = images.length
      ? await readFile(join(here, '..', 'assistant', 'skills', 'digitalizar-cuaderno', 'SKILL.md'), 'utf8')
          .then(text => text.replace(/^---[\s\S]*?---\s*/, ''))
          .catch(() => '')
      : '';
    const messages = [
      { role: 'system', content: [systemPrompt(user), skill].filter(Boolean).join('\n\n') },
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
    context.findBudget = 15000;
    for (let round = 0; round < 6; round++) {
      const response = await complete(ai, messages);
      if (!response.tool_calls?.length) return clip(response.content, 12000);
      if (response.tool_calls.length > 8) throw new Error('AI requested too many tools');
      messages.push({ role: 'assistant', content: response.content ?? null, tool_calls: response.tool_calls });
      for (const call of response.tool_calls) {
        let result;
        try {
          result = await executeTool(call.function?.name, parse(call.function?.arguments ?? '{}') ?? {}, context);
        } catch (e) {
          result = { error: clip(e.message, 300) };
        }
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
        proposalView(p, db.prepare('SELECT * FROM ai_proposals WHERE id = ?').get(p.id)),
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
  /** A person's conversation with a fixed title (T3 Code, Revisión de datos), created on first use. */
  function namedThread(user, title) {
    const found = db.prepare('SELECT id FROM ai_threads WHERE owner_id = ? AND title = ?').get(owner(user), title);
    if (found) return found.id;
    const id = randomUUID();
    db.prepare('INSERT INTO ai_threads (id,owner_id,title,created_at,updated_at) VALUES (?,?,?,?,?)').run(
      id,
      owner(user),
      title,
      now(),
      now(),
    );
    return id;
  }
  function agentTurn(token) {
    const hash = createHash('sha256').update(token).digest('hex');
    const row = db
      .prepare(
        'SELECT u.* FROM ai_tokens t JOIN users u ON u.id = t.user_id WHERE t.token_hash = ? AND t.revoked_at IS NULL AND u.active = 1',
      )
      .get(hash);
    if (!row) return null;
    const user = { id: row.id, username: row.username, displayName: row.display_name, role: row.role };
    const thread = { id: namedThread(user, 'T3 Code') };
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
      // A T3 Code chat's call: its own list of proposals (chats call at the same time), and Claude's
      // tool-use id, which T3 records with the call: the proposals it drafts are shown with that chat.
      const context = turn.agent
        ? { ...turn.context, proposals: [], t3: { toolUseId: clip(body.params?._meta?.['claudecode/toolUseId'], 100) || null } }
        : turn.context;
      const before = context.proposals.length;
      try {
        out = await executeTool(String(body.params?.name ?? ''), body.params?.arguments ?? {}, context);
      } catch (e) {
        out = { error: clip(e.message, 300) };
      }
      // Proposals from T3 Code are shown in the app for review (Asistente → Cambios propuestos).
      if (turn.agent && context.proposals.length > before) {
        const fresh = context.proposals.splice(before);
        insertMessage(turn.context.threadId, 'assistant', 'Propuesta desde T3 Code', [], [], fresh);
        out = {
          ...out,
          review: 'The person reviews it in the app: Asistente → Cambios propuestos, or tells you to apply it.',
        };
      }
      // An answer cut in the middle is no JSON at all: say so instead, so the query is narrowed.
      let text = json(out);
      if (text.length > 200000) {
        out = { error: `The answer was too long (${text.length} characters). Narrow the query: filters, fields, limit, or count_records for counts.` };
        text = json(out);
      }
      return result({ content: [{ type: 'text', text }], isError: Boolean(out?.error) });
    }
    return { status: 200, body: { jsonrpc: '2.0', id, error: { code: -32601, message: 'Method not found' } } };
  }

  const notebooks = createNotebookMatcher({ store, db, newIds: idsFor, draftChanges, initialsFor });

  /**
   * A transcribed notebook page matched with its sheet (match_notebook): one
   * proposal for the page, replacing the page's earlier one when Claude matches
   * it again after a correction.
   */
  function matchNotebook(args, context) {
    let matched;
    try {
      matched = notebooks.match(args, context.user);
    } catch (e) {
      return { error: clip(e.message, 300) };
    }
    const editor = EDITORS.includes(context.user.role);
    const replaced = args.replaceProposalId
      ? db
          .prepare("SELECT * FROM ai_proposals WHERE id = ? AND owner_id = ? AND status = 'pending'")
          .get(String(args.replaceProposalId), owner(context.user))
      : null;
    const { review } = matched;
    // The same rows in another pending proposal: the page matched again, maybe in another conversation.
    // Context rows (includeUnchanged) write nothing: they neither overlap nor make a proposal alone.
    const writes = matched.changes.some(c => !c.context);
    const rows = new Set(matched.changes.filter(c => !c.context).map(c => c.recordId).filter(Boolean));
    const overlaps = rows.size
      ? db
          .prepare("SELECT id, reason, changes_json FROM ai_proposals WHERE owner_id = ? AND status = 'pending' AND id != ?")
          .all(owner(context.user), replaced?.id ?? '')
          .map(p => ({
            id: p.id,
            reason: p.reason,
            rows: (parse(p.changes_json) ?? []).filter(c => !c.context && rows.has(c.recordId)).map(c => c.label),
          }))
          .filter(p => p.rows.length)
          .map(p => ({ proposalId: p.id, reason: p.reason, rows: p.rows.slice(0, 10), count: p.rows.length }))
      : [];
    const reason = `Cuaderno ${KINDS[review.kind].label} (${review.sheet})${args.title ? `: ${clip(args.title, 120)}` : ''}`;
    let proposal = null;
    let conflicts = [];
    if (editor && replaced && writes) {
      // The corrected page takes the place of its proposal (same id): the table beside the chat changes in place.
      const carried = carryPersonEdits(parse(replaced.changes_json) ?? [], matched.changes, context.user);
      conflicts = carried.conflicts;
      if (saveRevision(replaced, carried.changes, 'ai', reason) !== null) proposal = { id: replaced.id };
    } else if (editor && replaced) {
      db.prepare("UPDATE ai_proposals SET status = 'discarded' WHERE id = ? AND status = 'pending'").run(replaced.id);
      changed(owner(context.user));
    }
    if (editor && writes && !proposal) proposal = saveProposal(matched.changes, reason, context);
    return {
      ...matchSummary(matched, proposal?.id),
      ...(replaced ? { replaced: replaced.id } : {}),
      ...(conflicts.length
        ? {
            conflicts,
            conflictNote: 'Cells the person corrected by hand in the table were kept. Tell the person where your new reading differs.',
          }
        : {}),
      ...(overlaps.length ? { overlaps } : {}),
      ...(!editor ? { note: 'This person can only read the workbook: nothing was proposed' } : {}),
    };
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
    if (path === '/api/knowledge' && method === 'GET') {
      const passages = await knowledge.search({ query: query.q, kind: query.kind, from: query.from, to: query.to, perDoc: 1 });
      return { status: 200, body: { documents: passages } };
    }
    const docMatch = /^\/api\/knowledge\/([A-Za-z0-9_-]{20,80})$/.exec(path);
    if (docMatch && method === 'GET') {
      const doc = await knowledge.get(docMatch[1]);
      return doc
        ? {
            status: 200,
            body: { id: doc.id, title: doc.title, kind: doc.kind, date: doc.date, text: doc.text, sourceUrl: doc.sourceUrl },
          }
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
            .prepare('SELECT * FROM ai_proposals WHERE thread_id = ?')
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
        changed(owner(user));
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
      const me = owner(user);
      // chat: 'all' (default), 'app' (made outside T3 chats), a T3 thread id, or 'auto': the chat
      // T3 shows (see followed()); follow = that chat as the page last got it.
      const asked = String(query.chat ?? 'all');
      const follow = String(query.follow ?? '') || null;
      // wait=1 with the revision the page holds: answer when a proposal is added, applied or
      // discarded (or after 20 s), so the Asistente tab shows edits as the assistant drafts them;
      // with T3, also when another chat is opened there.
      if (query.wait)
        await waitForChange(me, String(query.revision ?? ''), 20000, t3 && !query.only ? () => followed(user, chatGroups(me), follow).chat !== follow : null);
      linkByToolUse(me);
      void linkByResult(me).catch(e => console.error('Proposals by chat:', e.message));
      const revision = revisionOf(me);
      const groups = chatGroups(me);
      const followNow = followed(user, groups, follow);
      const scope = query.only ? { chat: 'all', how: 'only' } : asked === 'auto' ? followNow : { chat: asked, how: 'chosen' };
      const select = `SELECT p.*, t.title FROM ai_proposals p JOIN ai_threads t ON t.id = p.thread_id WHERE p.owner_id = ?`;
      const where =
        query.only ? ' AND p.id = ?' : scope.chat === 'all' ? '' : scope.chat === 'app' ? " AND coalesce(p.t3_thread, '') = ''" : ' AND p.t3_thread = ?';
      const args = [me, ...(query.only ? [String(query.only)] : scope.chat === 'all' || scope.chat === 'app' ? [] : [scope.chat])];
      const order = 'ORDER BY p.created_at DESC, p.rowid DESC';
      // all=1: the pending ones and the last few reviewed (the panel shows five), not every old proposal on each change.
      const rows = [
        ...db.prepare(`${select}${where} AND p.status IN ('pending', 'applying') ${order} LIMIT 200`).all(...args),
        ...(query.all
          ? db
              .prepare(`${select}${where} AND p.status NOT IN ('pending', 'applying') ${order} LIMIT ?`)
              .all(...args, Math.min(Number(query.reviewed) || 5, 50))
          : []),
      ];
      // Titles as T3 shows them now (T3 names a chat after its first message, and it can be renamed).
      const threadIds = [...groups.keys(), scope.chat, followNow.chat, ...rows.map(r => r.t3_thread)];
      const titles = t3 ? t3.threads(threadIds.filter(id => id && id !== 'app' && id !== 'all')) : new Map();
      const titleOf = id => (id === 'all' || id === 'app' ? null : (titles.get(id)?.title ?? groups.get(id)?.title ?? null));
      const chats = [...groups.values()]
        .sort((a, b) => (a.at < b.at ? 1 : -1))
        .map(g => ({ id: g.id, title: titleOf(g.id), pending: g.pending }));
      return {
        status: 200,
        body: {
          revision,
          scope: { ...scope, title: titleOf(scope.chat) },
          follow: { ...followNow, title: titleOf(followNow.chat) },
          chats,
          proposals: rows.map(r => listedView(r, titles)),
        },
      };
    }
    // A cell edited by the person in the table (Asistente → Cambios propuestos), checked as a save checks it.
    const editMatch = /^\/api\/chat\/proposals\/([0-9a-f-]{36})\/edit$/.exec(path);
    if (editMatch && method === 'POST') {
      if (!EDITORS.includes(user.role)) return bad(403, 'forbidden', 'Your role cannot edit proposals.');
      const proposal = ownProposal(editMatch[1], user);
      if (!proposal) return bad(404, 'not_found', 'Proposal not found.');
      if (proposal.status !== 'pending') return bad(409, 'proposal_used', 'La propuesta ya no está pendiente.');
      const cells = body.cells ?? [];
      const scalar = v => v === null || ['string', 'number', 'boolean'].includes(typeof v);
      if (
        !Array.isArray(cells) ||
        cells.length > 2000 ||
        cells.some(
          c =>
            typeof c?.key !== 'string' ||
            typeof c.field !== 'string' ||
            c.field.length > 120 ||
            !scalar(c.value ?? null) ||
            (c.use !== undefined && c.use !== 'sheet' && c.use !== 'ai'),
        )
      )
        return bad(400, 'invalid_cells', 'cells must be a list of { key, field, value, use? }.');
      const remove = Array.isArray(body.remove) ? body.remove.filter(k => typeof k === 'string').slice(0, 100) : [];
      const addEmpty = Array.isArray(body.add) ? body.add.filter(a => typeof a?.sheet === 'string').slice(0, 20) : [];
      const out = reviseChanges(
        parse(proposal.changes_json) ?? [],
        {
          // use: the buttons for the selected cells, "Valor de la hoja" (back to the sheet's value,
          // the assistant's kept aside) and "Valor de la IA" (the assistant's value again).
          set: cells.map(c => ({
            ref: c.key,
            values: {
              [c.field]:
                c.use === 'sheet'
                  ? DROP
                  : c.use === 'ai'
                    ? AI_VALUE
                    : typeof c.value === 'string'
                      ? clip(c.value, 2000)
                      : (c.value ?? null),
            },
            ...(c.before !== undefined && !c.use && scalar(c.before) ? { before: { [c.field]: c.before } } : {}),
          })),
          remove,
          addEmpty,
        },
        { by: 'person', user },
      );
      if (out.changes.length > 100) return bad(409, 'too_many_rows', 'Una propuesta tiene como máximo 100 filas.');
      if (json(out.changes) !== proposal.changes_json && saveRevision(proposal, out.changes, 'person') === null)
        return bad(409, 'proposal_used', 'La propuesta ya no está pendiente.');
      return {
        status: 200,
        body: { proposal: listedView(ownProposalListed(proposal.id)), rejected: out.rejected, overrode: out.overrode },
      };
    }
    // The obvious fixes of chosen issues as one proposal to confirm (the old Tablas → Revisión de datos list; kept for links and tools).
    if (path === '/api/chat/proposals/from-checks' && method === 'POST') {
      if (!Array.isArray(body.ids) || !body.ids.length || body.ids.length > 100)
        return bad(400, 'invalid_ids', 'Choose 1 to 100 issues.');
      const wanted = new Set(body.ids.map(String));
      const merged = new Map();
      for (const issue of allIssues(store).issues) {
        if (!wanted.has(issue.id) || !issue.fix) continue;
        const change = merged.get(issue.fix.recordId) ?? { recordId: issue.fix.recordId, values: {}, notes: [] };
        Object.assign(change.values, issue.fix.values);
        change.notes.push(issue.problem);
        merged.set(issue.fix.recordId, change);
      }
      if (!merged.size) return bad(409, 'no_fixes', 'Those issues have no obvious fix, or were already fixed.');
      const threadId = namedThread(user, 'Revisión de datos');
      const context = { threadId, user, records: new Map(), sources: new Map(), results: [], proposals: [], applied: [] };
      const out = proposeChanges(
        {
          reason: tpl('Arreglos de la Revisión de datos'),
          changes: [...merged.values()].map(c => ({ recordId: c.recordId, values: c.values, note: c.notes.join(' · ') })),
        },
        context,
        { literal: true },
      );
      if (out.error) return bad(409, 'invalid_fix', out.error);
      insertMessage(threadId, 'assistant', 'Arreglos propuestos desde Revisión de datos', [], [], context.proposals);
      return { status: 201, body: out };
    }
    // "Preparar propuesta" in the Revisión tab: every agreed fix as one proposal to confirm.
    if (path === '/api/chat/proposals/from-review' && method === 'POST') {
      const agreed = agreedFixes(store, { kind: body.kind ? clip(body.kind, 300) : undefined, limit: 100 });
      if (!agreed.fixes.length) return bad(409, 'no_fixes', 'No accepted fixes are waiting.');
      const merged = new Map();
      for (const f of agreed.fixes) {
        const change = merged.get(f.recordId) ?? { recordId: f.recordId, values: {}, notes: [] };
        Object.assign(change.values, f.values);
        change.notes.push(f.note);
        merged.set(f.recordId, change);
      }
      const threadId = namedThread(user, 'Revisión de datos');
      const context = { threadId, user, records: new Map(), sources: new Map(), results: [], proposals: [], applied: [] };
      const out = proposeChanges(
        {
          reason: tpl('Correcciones acordadas en Revisión'),
          changes: [...merged.values()].map(c => ({ recordId: c.recordId, values: c.values, note: c.notes.join(' · ') })),
          issueIds: agreed.fixes.map(f => f.issueId),
        },
        context,
        { literal: true },
      );
      if (out.error) return bad(409, 'invalid_fix', out.error);
      insertMessage(threadId, 'assistant', 'Correcciones acordadas en Revisión', [], [], context.proposals);
      return { status: 201, body: { ...out, fixes: agreed.fixes.length, tasks: agreed.tasks.length } };
    }
    const discardMatch = /^\/api\/chat\/proposals\/([0-9a-f-]{36})\/discard$/.exec(path);
    if (discardMatch && method === 'POST') {
      const done = db
        .prepare("UPDATE ai_proposals SET status = 'discarded' WHERE id = ? AND owner_id = ? AND status = 'pending'")
        .run(discardMatch[1], owner(user));
      if (done.changes) changed(owner(user));
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
      // The rows were chosen on the revision the table showed: if the assistant changed it since, look again.
      if (body.revision !== undefined && Number(body.revision) !== proposal.revision)
        return bad(409, 'proposal_changed', 'La propuesta cambió mientras la revisabas: mira la tabla y vuelve a aplicar.');
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
