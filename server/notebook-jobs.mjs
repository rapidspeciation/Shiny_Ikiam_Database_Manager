// "Digitalizar cuaderno": each photographed page is a job that is read in the
// background (the person keeps photographing), compared with the sheet and
// turned into a proposal of the assistant, so it also shows in Cambios
// propuestos and its own conversation. Nothing is written until the person
// applies it. Jobs and photos live in the app database and survive a reload.

import { createHash, randomUUID } from 'node:crypto';
import { moduleMap } from './schema.mjs';
import { TUBE_FIELD, isIdValue, isUnique } from './verifications.mjs';
import { listOptions } from './verify.mjs';
import {
  KINDS,
  KIND_IDS,
  SYSTEM_PROMPT,
  buildReview,
  clutchKey,
  pageText,
  parseTranscription,
  proposalRows,
  transcriptionPrompt,
  typeOf,
} from './notebook.mjs';

const now = () => new Date().toISOString();
const json = value => JSON.stringify(value);
const clip = (value, length) => String(value ?? '').slice(0, length);
const bad = (status, code, message) => ({ status, body: { error: { code, message } } });
const parse = (value, fallback) => {
  try {
    return JSON.parse(value) ?? fallback;
  } catch {
    return fallback;
  }
};
const ecuadorDay = () => new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' }).format(new Date());
const clock = () =>
  new Intl.DateTimeFormat('es-EC', {
    timeZone: 'America/Guayaquil',
    day: '2-digit',
    month: '2-digit',
    hour: '2-digit',
    minute: '2-digit',
    hour12: false,
  }).format(new Date());

export function initNotebookJobs(db) {
  db.exec(`CREATE TABLE IF NOT EXISTS notebook_jobs (
    id TEXT PRIMARY KEY, owner_id TEXT NOT NULL, owner_json TEXT NOT NULL, attachment_id TEXT NOT NULL,
    name TEXT, photo_hash TEXT, source_hash TEXT, kind TEXT NOT NULL, year INTEGER,
    status TEXT NOT NULL, error TEXT, transcription_json TEXT, edits_json TEXT NOT NULL DEFAULT '{}',
    picks_json TEXT NOT NULL DEFAULT '{}', keys_json TEXT NOT NULL DEFAULT '[]', thread_id TEXT,
    proposal_id TEXT, message_id TEXT, proposals_json TEXT NOT NULL DEFAULT '[]', model TEXT,
    duration_ms INTEGER, reused_from TEXT, created_at TEXT NOT NULL, updated_at TEXT NOT NULL
  );
  CREATE INDEX IF NOT EXISTS notebook_jobs_owner ON notebook_jobs(owner_id, created_at);
  CREATE INDEX IF NOT EXISTS notebook_jobs_hash ON notebook_jobs(photo_hash);`);
  // The page's counts (rows with changes, doubts…), for the strip and the history without reviewing each page.
  if (!db.prepare('PRAGMA table_info(notebook_jobs)').all().some(c => c.name === 'counts_json'))
    db.exec('ALTER TABLE notebook_jobs ADD COLUMN counts_json TEXT');
}

/**
 * deps (from the assistant): store, db, draftChanges(args, ids), newIds(),
 * applyProposal(row, user, options), changed(ownerId), waitForChange, revisionOf,
 * insertMessage, namedThread-like newThread(user, title), transcribe(user, request), initialsFor(user).
 */
export function createNotebookJobs(deps) {
  const { store, db } = deps;
  initNotebookJobs(db);
  const owner = user => String(user?.id ?? user?.username ?? '');
  const get = id => db.prepare('SELECT * FROM notebook_jobs WHERE id = ?').get(id);
  const touch = (id, fields) => {
    const keys = Object.keys(fields);
    db.prepare(`UPDATE notebook_jobs SET ${keys.map(k => `${k} = ?`).join(', ')}, updated_at = ? WHERE id = ?`).run(
      ...keys.map(k => fields[k]),
      now(),
      id,
    );
  };

  // ---- The sheet, as the review reads it --------------------------------
  const indexes = new Map();
  /** Rows by their key columns (normalized: "685 (3)" = "685(3)"), rebuilt when the sheet changes. */
  function keyIndex(sheet, keys) {
    const mod = moduleMap.get(sheet);
    // Both from indexes (a max() filtered on `missing` read every row: 100 ms a call).
    const stamp = db
      .prepare(
        'SELECT (SELECT count(*) FROM records WHERE sheet = ? AND missing = 0) n, (SELECT max(updated_at) FROM records WHERE sheet = ?) u',
      )
      .get(sheet, sheet);
    const cacheKey = `${sheet}\u0000${keys.join('|')}`;
    const hit = indexes.get(cacheKey);
    if (hit?.stamp === `${stamp.n}:${stamp.u}`) return hit.map;
    const map = new Map();
    const columns = keys.map((k, i) => `json_extract(values_json, '$."${k.replaceAll('"', '')}"') k${i}`).join(', ');
    for (const r of db
      .prepare(`SELECT id, ${columns} FROM records WHERE sheet = ? AND missing = 0 AND row_num > ?`)
      .all(sheet, mod.headerRow)) {
      const values = keys.map((_, i) => r[`k${i}`]);
      if (values.some(v => v === null || v === '')) continue;
      const key = values.map(clutchKey).join('|');
      map.set(key, [...(map.get(key) ?? []), { id: r.id, value: values[0] }]);
    }
    indexes.set(cacheKey, { stamp: `${stamp.n}:${stamp.u}`, map });
    return map;
  }

  /** Columns that are formulas in the next unused row of a sheet (a new row leaves them). */
  function newRowFormulas(sheet) {
    const last =
      db.prepare('SELECT max(row_num) n FROM records WHERE sheet=? AND missing=0 AND observed=1').get(sheet).n ??
      moduleMap.get(sheet).headerRow;
    const next = db
      .prepare(
        'SELECT formulas_json FROM records WHERE sheet=? AND missing=0 AND observed=0 AND row_num>? ORDER BY row_num LIMIT 1',
      )
      .get(sheet, last);
    return new Set(Object.keys(parse(next?.formulas_json ?? '{}', {})));
  }

  /** A page is reviewed on every correction: the slower lookups are kept for a few seconds. */
  const memo = new Map();
  const remembered = (key, ms, make) => {
    const hit = memo.get(key);
    if (hit && hit.until > Date.now()) return hit.value;
    const value = make();
    memo.set(key, { until: Date.now() + ms, value });
    return value;
  };
  const listsOf = sheet => remembered(`lists:${sheet}`, 3000, () => listOptions(store, sheet));
  // IDs used anywhere guide the review only (the save checks them again), so a short-lived copy will do.
  const usedIds = () => remembered('ids', 30000, () => deps.newIds().used());
  const initials = user => remembered(`ini:${user.id ?? user.username}`, 600000, () => deps.initialsFor(user));

  function lookupFor(sheet, keys) {
    const lists = listsOf(sheet);
    let own, stocks;
    const mine = () => (own ??= keyIndex(sheet, keys));
    const clutches = () => (stocks ??= keyIndex('Insectary_stocks', ['CLUTCH NUMBER']));
    const record = id => {
      const r = store.getRecord(id);
      return r && { id: r.id, row: r.row, version: r.version, label: r.label, values: r.values, formulas: r.formulas };
    };
    return {
      find: values => (mine().get(values.map(clutchKey).join('|')) ?? []).map(h => record(h.id)).filter(Boolean),
      clutch: value => clutches().get(clutchKey(value))?.[0]?.value ?? null,
      speciesOfClutch: value => {
        const hit = clutches().get(clutchKey(value))?.[0];
        return hit ? (store.getRecord(hit.id)?.values?.SPECIES ?? null) : null;
      },
      list: field => lists[field],
      holder: (field, value, recordId) => {
        if (!(isUnique(sheet, field) || TUBE_FIELD.test(field)) || !isIdValue(value)) return null;
        const unique = usedIds();
        const key = `${TUBE_FIELD.test(field) ? 'tube' : `${sheet}:${field}`}\u0000${String(value).trim()}`;
        return (unique.get(key) ?? []).find(h => h.id !== recordId) ?? null;
      },
      newRowFormulas: newRowFormulas(sheet),
      typedOverFormula: new Set(sheet === 'Insectary_data' ? ['SPECIES'] : []),
    };
  }

  /** Species names in use, commonest first, for the prompt (the notebooks abbreviate them). */
  function speciesNames(sheets) {
    const counts = new Map();
    for (const sheet of sheets) {
      const field = sheet === 'CRISPR' ? 'Stock_of_origin' : 'SPECIES';
      for (const r of db
        .prepare(
          `SELECT json_extract(values_json, '$."${field}"') s FROM records WHERE sheet = ? AND missing = 0 AND observed = 1 ORDER BY row_num DESC LIMIT 2000`,
        )
        .all(sheet)) {
        const name = String(r.s ?? '').trim();
        if (name && !/^(NA|#N\/A)$/i.test(name) && name.length < 90) counts.set(name, (counts.get(name) ?? 0) + 1);
      }
    }
    return [...counts]
      .sort((a, b) => b[1] - a[1])
      .slice(0, 45)
      .map(([name]) => name);
  }
  /** Short dropdown lists of the page's columns, so the model writes their exact values. */
  function promptLists(kinds) {
    const out = {};
    for (const id of kinds) {
      const lists = listsOf(KINDS[id].sheet);
      for (const field of KINDS[id].fields) {
        const values = lists[field]?.values;
        if (values && values.size <= 40 && !['CLUTCH NUMBER', 'SPECIES', 'CAM_ID'].includes(field))
          out[field] = [...new Set([...(out[field] ?? []), ...values])];
      }
    }
    return out;
  }

  // ---- Reading pages, two at a time -------------------------------------
  const queue = [];
  let running = 0;
  const LIMIT = 2;
  function enqueue(id) {
    if (!queue.includes(id)) queue.push(id);
    pump();
  }
  function pump() {
    while (running < LIMIT && queue.length) {
      const id = queue.shift();
      running++;
      read(id)
        .catch(e => console.error('Notebook page failed:', e.message))
        .finally(() => {
          running--;
          pump();
        });
    }
  }

  async function read(id) {
    const job = get(id);
    if (!job || job.status !== 'queued') return;
    const user = parse(job.owner_json, {});
    touch(id, { status: 'reading', error: null });
    deps.changed(job.owner_id);
    const started = Date.now();
    try {
      const attachment = store.getAttachment(job.attachment_id);
      const data = attachment?.data instanceof Uint8Array ? Buffer.from(attachment.data) : null;
      if (!data) throw new Error('La foto ya no está en el servidor');
      const kinds = job.kind === 'auto' ? KIND_IDS : [job.kind];
      const prompt = transcriptionPrompt({
        kind: job.kind,
        species: speciesNames([...new Set(kinds.map(k => KINDS[k].sheet))]),
        lists: promptLists(kinds),
        today: ecuadorDay(),
      });
      const out = await deps.transcribe(user, {
        image: { mimeType: attachment.mimeType, data },
        prompt,
        system: SYSTEM_PROMPT,
      });
      const transcription = parseTranscription(out.text, job.kind);
      if (!get(id) || get(id).status !== 'reading') return; // discarded meanwhile
      touch(id, {
        status: 'ready',
        transcription_json: json(transcription),
        model: clip(out.model, 80),
        duration_ms: Date.now() - started,
      });
      settle(id, { announce: true });
    } catch (e) {
      if (get(id)?.status === 'reading')
        touch(id, { status: 'error', error: clip(e.message, 400), duration_ms: Date.now() - started });
    } finally {
      deps.changed(job.owner_id);
    }
  }

  // ---- The review and its proposal ---------------------------------------
  function reviewOf(job) {
    const transcription = parse(job.transcription_json, null);
    if (!transcription) return null;
    const kind = KINDS[transcription.kind];
    const user = parse(job.owner_json, {});
    return buildReview({
      transcription,
      edits: parse(job.edits_json, {}),
      picks: parse(job.picks_json, {}),
      year: job.year ?? null,
      today: ecuadorDay(),
      initials: initials(user),
      lookup: lookupFor(kind.sheet, kind.keys),
    });
  }

  /**
   * Brings the page's proposal in line with its review: rows are checked one by one
   * (a bad row is marked, the rest still go), the pending proposal is updated in
   * place, or a new one is made (after a partial apply, or when rows appear).
   */
  function settle(id, { announce = false } = {}) {
    const job = get(id);
    const review = reviewOf(job);
    if (!review) return null;
    const user = parse(job.owner_json, {});
    const rows = proposalRows(review);
    const ids = deps.newIds();
    const changes = [];
    for (const [kind, list] of [
      ['newRows', rows.newRows],
      ['changes', rows.changes],
    ])
      for (const row of list) {
        const out = deps.draftChanges({ [kind]: [row] }, ids);
        const line = review.lines.find(l => l.n === row.line);
        if (out.error) {
          if (!/already in the sheet/.test(out.error)) {
            line.rowError = clip(out.error.replace(/^newRows\[0\]: /, ''), 300);
            line.picked = false;
          }
          continue;
        }
        changes.push(...out.changes.map(c => ({ ...c, line: row.line })));
      }
    // As written (994(7), 5VB, "50 / 9"); compared normalized in warningsOf.
    const keys = review.lines
      .filter(l => !l.crossed && l.status !== 'nokey')
      .map(l => review.keys.map(k => String(l.cells[k]?.value ?? '').trim()).join(' / '));
    const fields = { keys_json: json([...new Set(keys)]), counts_json: json(review.counts) };
    const current = job.proposal_id ? db.prepare('SELECT * FROM ai_proposals WHERE id = ?').get(job.proposal_id) : null;
    let proposalTouched = false;
    const reason = `Cuaderno ${KINDS[review.kind].label} (${review.sheet}): página del ${job.created_at.slice(0, 10)}`;
    if (current?.status === 'discarded' && job.status === 'ready') {
      // Discarded in Cambios propuestos or the chat: the page is closed too.
      Object.assign(fields, { status: appliedLines(job).size ? 'done' : 'discarded', proposal_id: null });
    } else if (current?.status === 'pending') {
      if (!changes.length || job.status !== 'ready') {
        db.prepare("UPDATE ai_proposals SET status = 'discarded' WHERE id = ? AND status = 'pending'").run(current.id);
        fields.proposal_id = null;
        proposalTouched = true;
      } else if (current.changes_json !== json(changes)) {
        proposalTouched = true;
        db.prepare('UPDATE ai_proposals SET changes_json = ? WHERE id = ?').run(json(changes), current.id);
        // The conversation shows the proposal as saved in its message: keep it the same.
        if (job.message_id)
          db.prepare('UPDATE ai_messages SET proposals_json = ? WHERE id = ?').run(
            json([{ id: current.id, changes, reason, status: 'pending' }]),
            job.message_id,
          );
      }
    } else if (changes.length && job.status === 'ready' && EDITORS.includes(user.role)) {
      const proposalId = randomUUID();
      db.prepare(
        'INSERT INTO ai_proposals (id,thread_id,owner_id,changes_json,reason,status,created_at) VALUES (?,?,?,?,?,?,?)',
      ).run(proposalId, job.thread_id, job.owner_id, json(changes), reason, 'pending', now());
      const proposal = { id: proposalId, changes, reason, status: 'pending' };
      const text = announce ? summaryText(job, review) : `Quedan ${changes.length} filas de la página por aplicar.`;
      const message = deps.insertMessage(job.thread_id, 'assistant', text, [], [], [proposal]);
      Object.assign(fields, {
        proposal_id: proposalId,
        message_id: message.id,
        proposals_json: json([...parse(job.proposals_json, []), proposalId]),
      });
    } else if (announce) {
      deps.insertMessage(job.thread_id, 'assistant', summaryText(job, review));
      proposalTouched = true;
    }
    // Only a real change wakes the pages following the list (opening a page must not loop).
    const changedFields = Object.fromEntries(Object.entries(fields).filter(([k, v]) => job[k] !== v));
    if (Object.keys(changedFields).length || proposalTouched) {
      if (Object.keys(changedFields).length) touch(id, changedFields);
      deps.changed(job.owner_id);
    }
    return review;
  }

  /** What the conversation of the page says once it is read (the chat can then answer about any line). */
  function summaryText(job, review) {
    const c = review.counts;
    const seconds = Math.round((get(job.id)?.duration_ms ?? 0) / 1000);
    return clip(
      [
        `Página leída en ${seconds} s: cuaderno de ${KINDS[review.kind].label} (${review.sheet}), año ${review.year}${review.yearSource === 'inferred' ? ' (deducido de la hoja)' : ''}. Página ${job.id}.`,
        `${c.lines} líneas: ${c.rows} con cambios (${c.fills} celdas por llenar, ${c.conflicts} diferencias con la hoja${c.created ? `, ${c.created} filas nuevas` : ''}), ${c.doubts} dudosas, ${c.errors} con problemas.`,
        'Revísala en Asistente → Digitalizar cuaderno, o pregúntame por una línea.',
        '',
        'Transcripción:',
        pageText(review),
      ].join('\n'),
      11000,
    );
  }

  const EDITORS = ['editor', 'reviewer', 'admin'];

  /** Lines applied by any of the page's proposals. */
  function appliedLines(job) {
    const lines = new Set();
    for (const pid of parse(job.proposals_json, [])) {
      const row = db.prepare('SELECT changes_json, applied_json, status FROM ai_proposals WHERE id = ?').get(pid);
      if (row?.status !== 'applied') continue;
      const changes = parse(row.changes_json, []);
      for (const i of parse(row.applied_json, []) ?? []) if (changes[i]?.line) lines.add(changes[i].line);
    }
    return lines;
  }

  function warningsOf(job) {
    const out = [];
    const others = db
      .prepare(
        "SELECT id, owner_json, created_at, status, photo_hash, source_hash, keys_json, transcription_json IS NOT NULL read FROM notebook_jobs WHERE id <> ? AND status <> 'discarded'",
      )
      .all(job.id);
    const by = row => parse(row.owner_json, {}).displayName || parse(row.owner_json, {}).username || '';
    for (const other of others)
      if ((job.photo_hash && other.photo_hash === job.photo_hash) || (job.source_hash && other.source_hash === job.source_hash))
        out.push({ kind: 'photo', jobId: other.id, at: other.created_at, by: by(other), status: other.status });
    const norm = key => key.split(' / ').map(clutchKey).join('|');
    const mine = new Set(parse(job.keys_json, []).map(norm));
    if (mine.size)
      for (const other of others) {
        const shared = parse(other.keys_json, []).filter(k => mine.has(norm(k)));
        if (shared.length >= Math.min(3, mine.size) && !out.some(w => w.jobId === other.id))
          out.push({ kind: 'keys', jobId: other.id, at: other.created_at, by: by(other), keys: shared.slice(0, 8), count: shared.length });
      }
    if (job.reused_from) out.push({ kind: 'reused', jobId: job.reused_from });
    return out;
  }

  function summary(job, review = null) {
    const transcription = parse(job.transcription_json, null);
    const kind = transcription?.kind ?? (job.kind === 'auto' ? null : job.kind);
    const proposal = job.proposal_id
      ? db.prepare('SELECT status FROM ai_proposals WHERE id = ?').get(job.proposal_id)
      : null;
    return {
      id: job.id,
      status: job.status,
      requestedKind: job.kind,
      kind,
      label: kind ? KINDS[kind].label : 'Detectando…',
      sheet: kind ? KINDS[kind].sheet : null,
      attachmentId: job.attachment_id,
      name: job.name,
      owner: parse(job.owner_json, {}).displayName || '',
      error: job.error,
      createdAt: job.created_at,
      updatedAt: job.updated_at,
      durationMs: job.duration_ms,
      model: job.model,
      threadId: job.thread_id,
      proposalId: job.proposal_id,
      proposalStatus: proposal?.status ?? null,
      lines: transcription?.lines.length ?? 0,
      keys: parse(job.keys_json, []),
      appliedLines: [...appliedLines(job)],
      ...(review || job.counts_json ? { counts: review?.counts ?? parse(job.counts_json, undefined) } : {}),
      warnings: warningsOf(job),
    };
  }

  function detail(job) {
    const review = job.status === 'ready' || job.status === 'done' ? settle(job.id) : null;
    job = get(job.id);
    const out = summary(job, review);
    if (!review) return out;
    const applied = new Set(out.appliedLines);
    const sheet = moduleMap.get(review.sheet);
    const lists = listsOf(review.sheet);
    const transcription = parse(job.transcription_json, {});
    return {
      ...out,
      year: review.year,
      yearSource: review.yearSource,
      rotate: review.rotate,
      headers: transcription.headers ?? [],
      other: transcription.other ?? '',
      keyFields: review.keys,
      fields: review.fields,
      types: Object.fromEntries(
        review.fields.map(f => [f, sheet?.fields.find(x => x.key === f)?.type ?? typeOf(f)]),
      ),
      options: Object.fromEntries(
        review.fields
          .filter(f => lists[f]?.values.size && lists[f].values.size <= 200)
          .map(f => [f, [...lists[f].values]]),
      ),
      reviewLines: review.lines.map(l => ({ ...l, applied: applied.has(l.n) })),
    };
  }

  // ---- Jobs survive a restart: pages being read are read again ----------
  db.prepare("UPDATE notebook_jobs SET status = 'queued' WHERE status = 'reading'").run();
  for (const r of db.prepare("SELECT id FROM notebook_jobs WHERE status = 'queued' ORDER BY created_at").all())
    queue.push(r.id);
  setTimeout(pump, 2000).unref?.();

  function create(body, user) {
    if (!EDITORS.includes(user.role)) return bad(403, 'forbidden', 'Tu usuario no puede digitalizar páginas.');
    const attachment = store.getAttachment(String(body.attachmentId ?? ''));
    if (!attachment || !/^image\/(png|jpeg|webp)$/.test(attachment.mimeType))
      return bad(400, 'invalid_photo', 'Sube primero la foto de la página.');
    const kind = ['auto', ...KIND_IDS].includes(body.kind) ? body.kind : 'auto';
    const year = Number.isInteger(body.year) && body.year >= 1990 && body.year < 2100 ? body.year : null;
    const photoHash = createHash('sha256').update(Buffer.from(attachment.data)).digest('hex');
    const sourceHash = /^[a-f0-9]{64}$/.test(String(body.sourceHash ?? '')) ? body.sourceHash : null;
    const id = randomUUID();
    // The same photo was read before: its reading is used again (no wait, no cost), with a warning.
    const earlier = db
      .prepare(
        "SELECT id, transcription_json FROM notebook_jobs WHERE transcription_json IS NOT NULL AND (photo_hash = ? OR (? IS NOT NULL AND source_hash = ?)) ORDER BY created_at DESC LIMIT 1",
      )
      .get(photoHash, sourceHash, sourceHash);
    const reused =
      earlier && (kind === 'auto' || parse(earlier.transcription_json, {}).kind === kind) ? earlier : null;
    const label = kind === 'auto' ? 'página' : KINDS[kind].label;
    const threadId = deps.newThread(user, `Cuaderno · ${label} · ${clock()}`);
    deps.insertMessage(threadId, 'user', `Página del cuaderno para digitalizar (${label}).`, [], [], [], [
      { id: attachment.id, name: attachment.name, mimeType: attachment.mimeType },
    ]);
    const time = now();
    db.prepare(
      `INSERT INTO notebook_jobs (id, owner_id, owner_json, attachment_id, name, photo_hash, source_hash, kind, year, status,
        transcription_json, thread_id, reused_from, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)`,
    ).run(
      id,
      owner(user),
      json({ id: user.id, username: user.username, displayName: user.displayName, role: user.role }),
      attachment.id,
      clip(body.name || attachment.name, 160),
      photoHash,
      sourceHash,
      kind,
      year,
      reused ? 'ready' : 'queued',
      reused?.transcription_json ?? null,
      threadId,
      reused?.id ?? null,
      time,
      time,
    );
    if (reused) settle(id, { announce: true });
    else enqueue(id);
    deps.changed(owner(user));
    return { status: 201, body: { job: summary(get(id)) } };
  }

  /** The person's corrections: typed cells, ticked rows, the page's year. */
  function saveEdits(job, body) {
    const fields = {};
    if (body.edits && typeof body.edits === 'object') {
      const edits = parse(job.edits_json, {});
      for (const [line, cells] of Object.entries(body.edits)) {
        if (!/^\d{1,3}$/.test(line) || !cells || typeof cells !== 'object') continue;
        for (const [field, value] of Object.entries(cells)) {
          if (typeof field !== 'string' || field.length > 80) continue;
          (edits[line] ??= {})[field] = value === null || value === undefined ? null : clip(value, 300);
        }
      }
      fields.edits_json = json(edits);
    }
    if (body.picks && typeof body.picks === 'object') {
      const picks = parse(job.picks_json, {});
      for (const [line, on] of Object.entries(body.picks)) if (/^\d{1,3}$/.test(line)) picks[line] = Boolean(on);
      fields.picks_json = json(picks);
    }
    if ('year' in body)
      fields.year = Number.isInteger(body.year) && body.year >= 1990 && body.year < 2100 ? body.year : null;
    if (Object.keys(fields).length) touch(job.id, fields);
  }

  async function apply(job, body, user) {
    if (!EDITORS.includes(user.role)) return bad(403, 'forbidden', 'Tu usuario no puede aplicar cambios.');
    if (job.status !== 'ready') return bad(409, 'not_ready', 'La página no está lista para aplicar.');
    const requestId = typeof body.requestId === 'string' ? body.requestId : '';
    if (requestId.length < 8 || requestId.length > 120) return bad(400, 'request_id_required', 'Falta requestId.');
    saveEdits(job, body);
    settle(job.id);
    job = get(job.id);
    const proposal = job.proposal_id ? db.prepare('SELECT * FROM ai_proposals WHERE id = ?').get(job.proposal_id) : null;
    if (proposal?.status !== 'pending') return bad(409, 'nothing_to_apply', 'No hay filas marcadas con cambios.');
    const changes = parse(proposal.changes_json, []);
    const wanted = Array.isArray(body.lines) ? new Set(body.lines.map(Number)) : null;
    const indexes = changes.map((c, i) => (!wanted || wanted.has(c.line) ? i : -1)).filter(i => i >= 0);
    if (!indexes.length) return bad(409, 'nothing_to_apply', 'Las filas elegidas no tienen cambios.');
    let result;
    try {
      result = await deps.applyProposal(proposal, user, {
        requestId,
        indexes,
        reason: `${proposal.reason} · aplicado desde Digitalizar cuaderno`,
      });
    } catch (e) {
      return {
        status: e.status ?? 409,
        body: {
          error: {
            code: e.code ?? 'apply_failed',
            message: clip(e.message, 300),
            details: { items: e.details?.items?.slice(0, 20) ?? [] },
          },
        },
      };
    }
    // What is left (rows not chosen) becomes a new proposal; a page with nothing left is done.
    touch(job.id, { proposal_id: null });
    memo.delete('ids');
    const review = settle(job.id);
    if (!get(job.id).proposal_id && !review?.lines.some(l => l.picked && l.changes)) touch(job.id, { status: 'done' });
    deps.changed(job.owner_id);
    return { status: 200, body: { job: detail(get(job.id)), result: { status: result.status, applied: result.applied.length } } };
  }

  function discard(job) {
    if (job.proposal_id)
      db.prepare("UPDATE ai_proposals SET status = 'discarded' WHERE id = ? AND status = 'pending'").run(job.proposal_id);
    touch(job.id, { status: appliedLines(job).size ? 'done' : 'discarded', proposal_id: null });
    const i = queue.indexOf(job.id);
    if (i >= 0) queue.splice(i, 1);
    deps.changed(job.owner_id);
  }

  /** The chat's and T3's view of a page: its lines as read and compared. */
  function pageForTool(args, user) {
    const job = args.jobId
      ? db.prepare('SELECT * FROM notebook_jobs WHERE id = ? AND owner_id = ?').get(String(args.jobId), owner(user))
      : db
          .prepare(
            "SELECT * FROM notebook_jobs WHERE owner_id = ? AND transcription_json IS NOT NULL ORDER BY created_at DESC LIMIT 1",
          )
          .get(owner(user));
    if (!job) return { error: 'No digitized notebook page found' };
    const review = reviewOf(job);
    if (!review) return { jobId: job.id, status: job.status, error: job.error ?? 'Not read yet' };
    const wanted = Number.isInteger(args.line) ? [args.line] : null;
    return {
      jobId: job.id,
      status: job.status,
      kind: review.kind,
      sheet: review.sheet,
      year: review.year,
      proposalId: job.proposal_id,
      lines: review.lines
        .filter(l => !wanted || wanted.includes(l.n))
        .map(l => ({
          n: l.n,
          raw: l.raw,
          status: l.status,
          row: l.row,
          message: l.message || undefined,
          cells: Object.fromEntries(
            Object.entries(l.cells)
              .filter(([, c]) => c.value !== null || c.before !== null)
              .map(([f, c]) => [
                f,
                {
                  notebook: typeOf(f) === 'date' && typeof c.value === 'number' ? isoOf(c.value) : c.value,
                  sheet: typeOf(f) === 'date' && typeof c.before === 'number' ? isoOf(c.before) : c.before,
                  status: c.status,
                  ...(c.doubt ? { doubtful: true, alternatives: c.alternatives } : {}),
                },
              ]),
          ),
        })),
    };
  }
  const isoOf = serial => new Date(Date.UTC(1899, 11, 30) + serial * 864e5).toISOString().slice(0, 10);

  async function handle({ method, path, body = {}, user, query = {} }) {
    if (path === '/api/notebook/jobs' && method === 'GET') {
      if (query.wait) await deps.waitForChange(owner(user), String(query.revision ?? ''), 20000);
      const revision = deps.revisionOf(owner(user));
      const jobs = db
        .prepare('SELECT * FROM notebook_jobs WHERE owner_id = ? ORDER BY created_at DESC LIMIT 150')
        .all(owner(user))
        .map(job => summary(job));
      return { status: 200, body: { revision, jobs } };
    }
    if (path === '/api/notebook/jobs' && method === 'POST') return create(body, user);
    const match = /^\/api\/notebook\/jobs\/([0-9a-f-]{36})(?:\/(apply|discard|retry))?$/.exec(path);
    if (!match) return null;
    const job = db.prepare('SELECT * FROM notebook_jobs WHERE id = ? AND owner_id = ?').get(match[1], owner(user));
    if (!job) return bad(404, 'not_found', 'Página no encontrada.');
    const action = match[2];
    if (!action && method === 'GET') return { status: 200, body: { job: detail(job) } };
    if (!action && method === 'PATCH') {
      saveEdits(job, body);
      return { status: 200, body: { job: detail(get(job.id)) } };
    }
    if (action === 'apply' && method === 'POST') return apply(job, body, user);
    if (action === 'discard' && method === 'POST') {
      discard(job);
      return { status: 200, body: { job: summary(get(job.id)) } };
    }
    if (action === 'retry' && method === 'POST') {
      if (job.status === 'reading' || job.status === 'queued')
        return bad(409, 'busy', 'La página se está leyendo todavía.');
      if (job.proposal_id)
        db.prepare("UPDATE ai_proposals SET status = 'discarded' WHERE id = ? AND status = 'pending'").run(job.proposal_id);
      const kind = ['auto', ...KIND_IDS].includes(body.kind) ? body.kind : job.kind;
      touch(job.id, {
        status: 'queued',
        kind,
        error: null,
        transcription_json: null,
        edits_json: '{}',
        picks_json: '{}',
        proposal_id: null,
        reused_from: null,
      });
      enqueue(job.id);
      deps.changed(job.owner_id);
      return { status: 200, body: { job: summary(get(job.id)) } };
    }
    return bad(405, 'method_not_allowed', 'Método no permitido.');
  }

  return { handle, pageForTool };
}
