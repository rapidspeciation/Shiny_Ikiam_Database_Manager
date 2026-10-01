// When each problem was first seen and when it was solved: the Revisión tab's
// «Resueltos». Every issue of the checks (server/checks.mjs) and every suggested
// edit (server/suggestions/) is remembered under a stable key (check: the issue
// id, kind:recordId:field; suggestion: source:recordId:field). Each time the
// checks or the suggestions are computed again (after the local copy changed),
// keys seen for the first time are stored with the time, and stored keys that are
// no longer found are solved: the sheet changed so the check does not find them.
// Who solved it and when come from the history (the last change of that cell, or
// of the row, after the problem was first seen); edits read from Google Sheets
// have no person ("Google Sheets"). A problem that comes back is open again.

const parse = text => {
  try {
    return text ? JSON.parse(text) : null;
  } catch {
    return null;
  }
};
const json = value => (value === undefined ? null : JSON.stringify(value));

const ready = new WeakSet();
export function initFindings(db) {
  if (ready.has(db)) return;
  db.exec(`CREATE TABLE IF NOT EXISTS findings(
      type TEXT NOT NULL, key TEXT NOT NULL, kind TEXT NOT NULL, sheet TEXT, row_num INTEGER, record_id TEXT,
      field TEXT, label TEXT, value_json TEXT, text TEXT, text_msg TEXT, extra_json TEXT,
      first_seen TEXT NOT NULL, solved_at TEXT, solved_json TEXT, PRIMARY KEY(type, key));
    CREATE INDEX IF NOT EXISTS findings_solved ON findings(type, solved_at);`);
  ready.add(db);
}

/**
 * The last saved change that can have solved a finding: of its cell, else of
 * its row, else of a related row (the other half of a repeat), after it was
 * first seen. Null when the history has none (e.g. the row was removed).
 */
function solvingChange(db, finding, since) {
  const query = db.prepare(
    `SELECT a.id, a.actor, a.purpose, a.created_at, c.field, c.before_json, c.after_json
     FROM changes c JOIN actions a ON a.id = c.action_id
     WHERE c.record_id = ? AND a.created_at >= ? AND a.status != 'failed'
     ORDER BY (c.field = ?) DESC, a.created_at DESC LIMIT 1`,
  );
  for (const recordId of [finding.record_id, ...(parse(finding.extra_json)?.others ?? [])]) {
    if (!recordId) continue;
    const change = query.get(recordId, since, finding.field ?? '');
    if (change) return { ...change, recordId };
  }
  return null;
}

/**
 * Stores what a fresh run of the checks (type 'check') or the suggestions
 * ('suggestion') found: { key, kind, sheet, row, recordId, field, label, value,
 * text, textMsg, others } each. Returns how many were new and how many solved.
 */
export function trackFindings(store, type, list, { at = new Date().toISOString() } = {}) {
  const db = store.db;
  initFindings(db);
  const open = new Map(
    db
      .prepare('SELECT key, record_id, field, first_seen, extra_json FROM findings WHERE type = ? AND solved_at IS NULL')
      .all(type)
      .map(r => [r.key, r]),
  );
  const current = new Set();
  const upsert = db.prepare(
    `INSERT INTO findings(type,key,kind,sheet,row_num,record_id,field,label,value_json,text,text_msg,extra_json,first_seen)
     VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?)
     ON CONFLICT(type, key) DO UPDATE SET kind=excluded.kind, sheet=excluded.sheet, row_num=excluded.row_num,
       record_id=excluded.record_id, field=excluded.field, label=excluded.label, value_json=excluded.value_json,
       text=excluded.text, text_msg=excluded.text_msg, extra_json=excluded.extra_json, solved_at=NULL, solved_json=NULL`,
  );
  const solve = db.prepare('UPDATE findings SET solved_at = ?, solved_json = ? WHERE type = ? AND key = ?');
  const recordNow = db.prepare('SELECT values_json, missing FROM records WHERE id = ?');
  const users = db.prepare('SELECT display_name FROM users WHERE id = ?');
  let added = 0,
    solved = 0;
  db.exec('BEGIN');
  try {
    for (const f of list) {
      if (current.has(f.key)) continue;
      current.add(f.key);
      if (open.has(f.key)) continue;
      upsert.run(
        type,
        f.key,
        f.kind,
        f.sheet ?? null,
        f.row ?? null,
        f.recordId ?? null,
        f.field ?? null,
        f.label ?? null,
        json(f.value ?? null),
        f.text ?? null,
        json(f.textMsg),
        f.others?.length ? json({ others: f.others.slice(0, 5) }) : null,
        at,
      );
      added++;
    }
    for (const [key, f] of open) {
      if (current.has(key)) continue;
      const change = solvingChange(db, f, f.first_seen);
      const record = f.record_id ? recordNow.get(f.record_id) : null;
      const now = record && !record.missing ? (parse(record.values_json)?.[f.field] ?? null) : null;
      const detail = {
        // The time of the change that solved it; else when the checks noticed.
        at: change?.created_at ?? at,
        ...(change
          ? {
              actionId: change.id,
              purpose: change.purpose,
              user: change.actor === 'unknown' ? null : (users.get(change.actor)?.display_name ?? change.actor),
              field: change.field,
              before: parse(change.before_json),
              after: parse(change.after_json),
              ...(change.recordId !== f.record_id ? { recordId: change.recordId } : {}),
            }
          : {}),
        now,
        ...(f.record_id && (!record || record.missing) ? { rowGone: true } : {}),
      };
      solve.run(detail.at, JSON.stringify(detail), type, key);
      solved++;
    }
    db.exec('COMMIT');
  } catch (e) {
    db.exec('ROLLBACK');
    throw e;
  }
  return { added, solved };
}

/** When each open finding of a type was first seen (key → ISO time). */
export function firstSeen(store, type, keys) {
  initFindings(store.db);
  const out = new Map();
  const query = store.db.prepare('SELECT first_seen FROM findings WHERE type = ? AND key = ?');
  for (const key of keys) {
    const r = query.get(type, key);
    if (r) out.set(key, r.first_seen);
  }
  return out;
}

const publicFinding = r => {
  const solved = parse(r.solved_json) ?? {};
  return {
    type: r.type,
    key: r.key,
    kind: r.kind,
    sheet: r.sheet,
    row: r.row_num,
    recordId: r.record_id,
    field: r.field,
    label: r.label,
    value: parse(r.value_json),
    text: r.text,
    ...(r.text_msg ? { textMsg: parse(r.text_msg) } : {}),
    firstSeen: r.first_seen,
    solvedAt: r.solved_at,
    solved,
  };
};

/**
 * Solved findings, newest first: of one type (check / suggestion) or both, one
 * kind, a text in the label, row or problem; with the counts per type and kind.
 */
export function solvedFindings(store, { type, kind, q, limit = 50, offset = 0 } = {}) {
  const db = store.db;
  initFindings(db);
  const clauses = ['solved_at IS NOT NULL'];
  const args = [];
  if (type) {
    clauses.push('type = ?');
    args.push(String(type));
  }
  const text = String(q ?? '').trim();
  if (text) {
    const like = `%${text.replace(/[\\%_]/g, m => '\\' + m)}%`;
    clauses.push("(label LIKE ? ESCAPE '\\' OR text LIKE ? ESCAPE '\\' OR value_json LIKE ? ESCAPE '\\')");
    args.push(like, like, like);
  }
  const where = clauses.join(' AND ');
  const kinds = {};
  for (const r of db.prepare(`SELECT type, kind, count(*) n FROM findings WHERE ${where} GROUP BY type, kind`).all(...args))
    kinds[`${r.type}:${r.kind}`] = r.n;
  const chosen = kind ? `${where} AND kind = ?` : where;
  const chosenArgs = kind ? [...args, String(kind)] : args;
  const size = Math.min(Math.max(Number(limit) || 50, 1), 500);
  const start = Math.max(Number(offset) || 0, 0);
  const total = db.prepare(`SELECT count(*) n FROM findings WHERE ${chosen}`).get(...chosenArgs).n;
  const items = db
    .prepare(`SELECT * FROM findings WHERE ${chosen} ORDER BY solved_at DESC, key LIMIT ? OFFSET ?`)
    .all(...chosenArgs, size, start)
    .map(publicFinding);
  const open = Object.fromEntries(
    db
      .prepare('SELECT type, count(*) n FROM findings WHERE solved_at IS NULL GROUP BY type')
      .all()
      .map(r => [r.type, r.n]),
  );
  return { total, offset: start, limit: size, kinds, open, items };
}
