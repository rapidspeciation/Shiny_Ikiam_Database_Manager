// The Revisión tab: every issue of the checks (server/checks.mjs) with the
// verdict people gave it. A verdict is accepted (the proposed fix is right),
// rejected (not a problem, or the reading is wrong) or other (the right value
// is another one). The last verdict of an issue counts; all are kept. Accepted
// fixes wait until someone asks for them (the T3 tool list_agreed_fixes, or
// "Preparar propuesta" in the tab): they become one ordinary proposal that a
// person confirms, and once it is written its issues move to "applied".
// Verdicts on issues read from photos by a model are also training labels.

import { allIssues, CHECK_KINDS } from './checks.mjs';
import { MODEL_KINDS, TASK_KINDS } from './photo-checks.mjs';
import { moduleMap } from './schema.mjs';
import { firstSeen } from './findings.mjs';

export const VERDICTS = ['accepted', 'rejected', 'other', 'pending', 'applied'];
/** The tab's status of an issue, from its last verdict. */
export const STATUS = {
  pending: 'pendiente',
  accepted: 'aceptado',
  rejected: 'rechazado',
  other: 'otro',
  applied: 'aplicado',
};
const EPOCH = Date.UTC(1899, 11, 30);
const iso = serial => new Date(EPOCH + Math.round(serial) * 864e5).toISOString().slice(0, 10);
const fail = (code, message, status = 400) => Object.assign(new Error(message), { code, status });
const clip = (value, n) => String(value ?? '').slice(0, n);
const parse = text => {
  try {
    return JSON.parse(text);
  } catch {
    return null;
  }
};

const ready = new WeakSet();
export function initVerdicts(db) {
  if (ready.has(db)) return;
  db.exec(`CREATE TABLE IF NOT EXISTS issue_verdicts(
      id INTEGER PRIMARY KEY AUTOINCREMENT, issue_id TEXT NOT NULL, kind TEXT NOT NULL, verdict TEXT NOT NULL,
      value TEXT, comment TEXT, user_id TEXT NOT NULL, user_name TEXT NOT NULL, at TEXT NOT NULL,
      proposal_id TEXT, snapshot_json TEXT NOT NULL);
    CREATE INDEX IF NOT EXISTS issue_verdicts_issue ON issue_verdicts(issue_id, id);`);
  ready.add(db);
}

/** The last verdict of every issue that has one. */
export function latestVerdicts(db) {
  initVerdicts(db);
  const rows = db
    .prepare(
      'SELECT v.* FROM issue_verdicts v JOIN (SELECT issue_id, max(id) id FROM issue_verdicts GROUP BY issue_id) l ON l.id = v.id',
    )
    .all();
  return new Map(rows.map(r => [r.issue_id, r]));
}
const publicVerdict = v =>
  v && {
    verdict: v.verdict,
    status: STATUS[v.verdict],
    value: v.value,
    comment: v.comment,
    user: v.user_name,
    at: v.at,
    ...(v.proposal_id ? { proposalId: v.proposal_id } : {}),
  };

/** What is kept of an issue with each verdict: enough to show it, and to learn from it, after it is gone. */
function snapshot(issue) {
  const keep = [
    'kind',
    'sheet',
    'row',
    'recordId',
    'label',
    'field',
    'value',
    'problem',
    'problemMsg',
    'fix',
    'fixNote',
    'fixNoteMsg',
    'task',
    'cam',
  ];
  const more = ['photos', 'envelopeCamid', 'envelopeText', 'prediction', 'ocr', 'ai', 'strength', 'curation', 'group'];
  return Object.fromEntries([...keep, ...more].filter(k => issue[k] !== undefined).map(k => [k, issue[k]]));
}

/**
 * Records one verdict for each issue (or each issue of a batch). Only issues
 * listed now can be judged; `applied` is for tasks done by hand (Drive), the
 * sheet fixes get it when their proposal is written.
 */
export function setVerdicts(store, body, user) {
  const db = store.db;
  initVerdicts(db);
  const verdict = String(body.verdict ?? '');
  if (!VERDICTS.includes(verdict)) throw fail('INVALID_VERDICT', `verdict must be one of ${VERDICTS.join(', ')}`);
  const { issues } = allIssues(store);
  const byId = new Map(issues.map(i => [i.id, i]));
  const chosen = body.group
    ? issues.filter(i => i.group?.key === String(body.group))
    : (Array.isArray(body.ids) ? body.ids : []).map(id => byId.get(String(id)));
  if (!chosen.length || chosen.length > 500) throw fail('INVALID_IDS', 'Choose 1 to 500 issues');
  if (chosen.some(i => !i))
    throw fail('ISSUE_NOT_FOUND', 'An issue is no longer listed (it may be fixed already); reload', 409);
  const value = verdict === 'other' ? clip(body.value, 300).trim() : null;
  if (verdict === 'other' && !value) throw fail('VALUE_REQUIRED', 'Give the right value');
  if (verdict === 'applied' && chosen.some(i => !i.task))
    throw fail('NOT_A_TASK', 'Only tasks done by hand are marked done here; sheet fixes are applied with a proposal');
  const at = new Date().toISOString();
  const insert = db.prepare(
    'INSERT INTO issue_verdicts(issue_id,kind,verdict,value,comment,user_id,user_name,at,snapshot_json) VALUES(?,?,?,?,?,?,?,?,?)',
  );
  const name = user.displayName || user.username || String(user.id);
  db.exec('BEGIN');
  try {
    for (const issue of chosen)
      insert.run(
        issue.id,
        issue.kind,
        verdict,
        value,
        clip(body.comment, 500).trim() || null,
        String(user.id),
        name,
        at,
        JSON.stringify(snapshot(issue)),
      );
    db.exec('COMMIT');
  } catch (e) {
    db.exec('ROLLBACK');
    throw e;
  }
  return {
    saved: chosen.length,
    ids: chosen.map(i => i.id),
    verdict: publicVerdict({ verdict, value, comment: body.comment || null, user_name: name, at }),
  };
}

export function verdictHistory(store, issueId) {
  initVerdicts(store.db);
  return store.db
    .prepare('SELECT * FROM issue_verdicts WHERE issue_id=? ORDER BY id DESC LIMIT 50')
    .all(String(issueId))
    .map(publicVerdict);
}

const statusOf = (issue, verdicts) => verdicts.get(issue.id)?.verdict ?? 'pending';
const inRange = (issue, from, to) =>
  (!from || (issue.date && issue.date >= from)) && (!to || (issue.date && issue.date <= to));
const matchesPerson = (issue, person) => {
  if (!person) return true;
  const p = person.toLowerCase();
  return (issue.who ?? []).some(w => w.toLowerCase() === p || w.toLowerCase().split(' - ')[0] === p);
};

/** Values of the rows of an issue, side by side: its row and the related ones, the compared columns first. */
const CONTEXT = [
  'CAM_ID',
  'Insectary_ID',
  'SPECIES',
  'Sex',
  'Collection_date',
  'Intro2Insectary_date',
  'Preservation_date',
  'Collector',
];
function sideBySide(store, issue) {
  const ids = [issue.recordId, ...(issue.related ?? []).map(r => r.recordId)].filter(Boolean);
  const records = [...new Set(ids)].map(id => store.getRecord(id)).filter(r => r && !r.missing);
  if (!records.length) return null;
  const compare = [
    ...new Set(
      [issue.field, ...(issue.related ?? []).map(r => r.field), ...Object.keys(issue.fix?.values ?? {})].filter(
        Boolean,
      ),
    ),
  ];
  const has = f => records.some(r => moduleMap.get(r.sheet)?.fields.some(x => x.key === f));
  const fields = [...new Set([...compare, ...CONTEXT])].filter(has).slice(0, 9);
  const dateType = (sheet, f) => moduleMap.get(sheet)?.fields.find(x => x.key === f)?.type === 'date';
  return {
    fields,
    compare: compare.filter(has),
    rows: records.map(r => ({
      sheet: r.sheet,
      row: r.row,
      recordId: r.id,
      label: r.label,
      values: Object.fromEntries(
        fields.map(f => {
          const v = r.values?.[f] ?? null;
          return [f, dateType(r.sheet, f) && typeof v === 'number' ? iso(v) : v];
        }),
      ),
    })),
  };
}

/**
 * One page of the tab: the issues after the filters (kind, sheet, person,
 * date range, status, batch, text), with their verdict and rows side by side;
 * counts per kind and status; and how many accepted fixes wait to be applied.
 */
export function reviewPage(store, query = {}) {
  const db = store.db;
  const verdicts = latestVerdicts(db);
  const { issues, checkedAt } = allIssues(store);
  const kinds = String(query.kind ?? '')
    .split(',')
    .map(k => k.trim())
    .filter(Boolean);
  const unknown = kinds.find(k => !CHECK_KINDS[k]);
  if (unknown) throw fail('INVALID_KIND', `Unknown kind ${clip(unknown, 40)}`);
  const status = String(query.status ?? '');
  if (status && status !== 'all' && !STATUS[status])
    throw fail('INVALID_STATUS', `status must be one of ${Object.keys(STATUS).join(', ')}, all`);
  const text = String(query.q ?? '')
    .trim()
    .toLowerCase();
  const base = issues.filter(
    i =>
      (!query.sheet || i.sheet === query.sheet) &&
      matchesPerson(i, String(query.person ?? '').trim()) &&
      inRange(i, query.from, query.to) &&
      (!query.group || i.group?.key === query.group) &&
      (!text || `${i.label} ${i.cam ?? ''} ${i.problem}`.toLowerCase().includes(text)),
  );
  const counts = Object.fromEntries(Object.keys(CHECK_KINDS).map(k => [k, 0]));
  const statuses = Object.fromEntries(Object.keys(STATUS).map(s => [s, 0]));
  const people = new Map();
  for (const i of base) {
    const s = statusOf(i, verdicts);
    if (!status || status === 'all' || s === status) counts[i.kind]++;
    if (!kinds.length || kinds.includes(i.kind)) statuses[s]++;
    for (const w of i.who ?? []) people.set(w, (people.get(w) ?? 0) + 1);
  }
  let chosen = base.filter(
    i => (!kinds.length || kinds.includes(i.kind)) && (!status || status === 'all' || statusOf(i, verdicts) === status),
  );
  // Applied fixes leave the checks (the sheet is right now): shown from what was kept with their verdict.
  const listed = new Set(issues.map(i => i.id));
  const gone = [...verdicts.values()]
    .filter(v => v.verdict === 'applied' && !listed.has(v.issue_id))
    .map(v => ({ id: v.issue_id, ...parse(v.snapshot_json), resolved: true }))
    .filter(
      i =>
        (!query.sheet || i.sheet === query.sheet) &&
        (!text || `${i.label} ${i.cam ?? ''} ${i.problem}`.toLowerCase().includes(text)),
    );
  for (const i of gone) if (!kinds.length || kinds.includes(i.kind)) statuses.applied++;
  if (status === 'applied' || status === 'all') {
    for (const i of gone) counts[i.kind] = (counts[i.kind] ?? 0) + 1;
    chosen = [...chosen, ...gone.filter(i => !kinds.length || kinds.includes(i.kind))];
  }
  // Newest first by default (recent problems are the easiest to fix); "old", or "kind" for the checks' own order.
  const order = String(query.sort ?? 'recent');
  if (order === 'recent' || order === 'old') {
    const dir = order === 'recent' ? -1 : 1;
    chosen = [...chosen].sort(
      (a, b) => (a.date ? 0 : 1) - (b.date ? 0 : 1) || (a.date && b.date ? dir * String(a.date).localeCompare(String(b.date)) : 0),
    );
  }
  const size = Math.min(Math.max(Number(query.limit) || 25, 1), 200);
  const start = Math.max(Number(query.offset) || 0, 0);
  const slice = chosen.slice(start, start + size);
  // Since when the checks find it (server/findings.mjs).
  const seen = firstSeen(store, 'check', slice.map(i => i.id));
  const page = slice.map(i => ({
    ...i,
    verdict: publicVerdict(verdicts.get(i.id)) ?? null,
    ...(seen.has(i.id) && !i.resolved ? { firstSeen: seen.get(i.id) } : {}),
    ...(i.resolved ? {} : { table: sideBySide(store, i) }),
  }));
  return {
    checkedAt,
    total: chosen.length,
    offset: start,
    limit: size,
    counts,
    statuses,
    kinds: CHECK_KINDS,
    taskKinds: [...TASK_KINDS],
    sheets: [...new Set(issues.map(i => i.sheet))].sort(),
    people: [...people]
      .sort((a, b) => b[1] - a[1])
      .slice(0, 80)
      .map(([name, n]) => ({ name, n })),
    agreed: agreedCounts(issues, verdicts),
    issues: page,
  };
}

function agreedCounts(issues, verdicts) {
  let fixes = 0,
    tasks = 0;
  for (const i of issues) {
    const v = verdicts.get(i.id);
    if (!v || !['accepted', 'other'].includes(v.verdict)) continue;
    if (i.task) tasks++;
    else if (i.fix || v.verdict === 'other') fixes++;
  }
  return { fixes, tasks };
}

/** The change an agreed issue asks for, or why there is none. */
function agreedChange(issue, v) {
  if (v.verdict === 'other') {
    const field = issue.fix ? Object.keys(issue.fix.values)[0] : issue.field;
    const recordId = issue.fix?.recordId ?? issue.recordId;
    if (!recordId || !field) return { missing: 'no row to change' };
    return { recordId, values: { [field]: v.value } };
  }
  if (!issue.fix) return { missing: 'accepted, but there is no proposed value: ask the person for it' };
  // The data changed after the verdict: the fix now proposed was not the one accepted.
  const accepted = parse(v.snapshot_json)?.fix;
  if (JSON.stringify(accepted ?? null) !== JSON.stringify(issue.fix)) return { stale: true };
  return issue.fix;
}

/**
 * Accepted issues ready to be proposed (list_agreed_fixes): sheet fixes with
 * their issue ids, and tasks that are no sheet change (Drive renames and merges)
 * as a checklist. Issues accepted but gone from the checks are counted apart.
 */
export function agreedFixes(store, { kind, limit = 100 } = {}) {
  const verdicts = latestVerdicts(store.db);
  const { issues } = allIssues(store);
  const kinds = kind
    ? String(kind)
        .split(',')
        .map(k => k.trim())
    : null;
  const listed = new Set(issues.map(i => i.id));
  const fixes = [],
    tasks = [],
    needsValue = [],
    stale = [];
  for (const issue of issues) {
    const v = verdicts.get(issue.id);
    if (!v || !['accepted', 'other'].includes(v.verdict) || (kinds && !kinds.includes(issue.kind))) continue;
    const who = `${v.verdict === 'other' ? `valor ${v.value} dado` : 'aceptado'} por ${v.user_name}${v.comment ? `: ${v.comment}` : ''}`;
    const base = {
      issueId: issue.id,
      kind: issue.kind,
      sheet: issue.sheet,
      row: issue.row,
      label: issue.label,
      decidedBy: v.user_name,
    };
    if (issue.task) {
      tasks.push({
        ...base,
        cam: issue.cam,
        task: v.verdict === 'other' ? `${issue.task.text} (otro valor: ${v.value})` : issue.task.text,
        comment: v.comment,
      });
      continue;
    }
    const change = agreedChange(issue, v);
    if (change.missing) needsValue.push({ ...base, problem: issue.problem, why: change.missing });
    else if (change.stale) stale.push({ ...base, problem: issue.problem });
    else fixes.push({ ...base, recordId: change.recordId, values: change.values, note: `${issue.problem} (${who})` });
  }
  const gone = [...verdicts.values()].filter(
    v => ['accepted', 'other'].includes(v.verdict) && !listed.has(v.issue_id),
  ).length;
  const size = Math.min(Math.max(Number(limit) || 100, 1), 100);
  return {
    total: fixes.length,
    fixes: fixes.slice(0, size),
    tasks,
    needsValue,
    stale,
    goneAlready: gone,
    howTo:
      'Make ONE propose_changes with these fixes (values merged per recordId, the note of each) and issueIds = every issueId used; the person confirms it; call apply_proposal only after their explicit yes. Tasks are not sheet changes: explain them as a checklist (they are marked done in the Revisión tab). needsValue: ask the person. stale: the data changed after the verdict, ask for a new look in the Revisión tab.',
  };
}

/** After a proposal made from agreed issues is written: their verdicts move to applied. */
export function markApplied(store, issueIds, { recordIds, proposalId, user }) {
  const db = store.db;
  const verdicts = latestVerdicts(db);
  const insert = db.prepare(
    'INSERT INTO issue_verdicts(issue_id,kind,verdict,value,comment,user_id,user_name,at,proposal_id,snapshot_json) VALUES(?,?,?,?,?,?,?,?,?,?)',
  );
  const at = new Date().toISOString();
  let marked = 0;
  for (const id of new Set(issueIds.map(String))) {
    const v = verdicts.get(id);
    if (!v || !['accepted', 'other'].includes(v.verdict)) continue;
    const snap = parse(v.snapshot_json) ?? {};
    const recordId = snap.fix?.recordId ?? snap.recordId;
    if (recordIds && !recordIds.has(recordId)) continue;
    insert.run(
      id,
      v.kind,
      'applied',
      v.value,
      null,
      String(user.id),
      user.displayName || user.username || String(user.id),
      at,
      proposalId,
      v.snapshot_json,
    );
    marked++;
  }
  return marked;
}

/**
 * Verdicts on what a model read from the photos, as training labels (JSON
 * lines): the last verdict of each issue with the reading it judged.
 */
export function trainingLabels(store) {
  const lines = [];
  for (const v of latestVerdicts(store.db).values()) {
    if (!MODEL_KINDS.has(v.kind) || v.verdict === 'pending') continue;
    const s = parse(v.snapshot_json) ?? {};
    lines.push(
      JSON.stringify({
        issueId: v.issue_id,
        kind: v.kind,
        verdict: v.verdict,
        value: v.value,
        comment: v.comment,
        user: v.user_name,
        at: v.at,
        cam: s.cam ?? null,
        envelope: s.photos?.envelope ?? null,
        envelopeCamid: s.envelopeCamid ?? null,
        envelopeText: s.envelopeText ?? null,
        ocr: s.ocr ?? null,
        ai: s.ai ?? null,
        prediction: s.prediction ?? null,
        sheetValue: s.value ?? null,
        curation: s.curation ?? null,
      }),
    );
  }
  return lines.join('\n') + (lines.length ? '\n' : '');
}
