// Moving the local database from one workbook to another (the test copy to the
// team's workbook), for scripts/switch-workbook.mjs.
//
// The local copy of the sheets is read again from the new workbook with the
// normal sync matching, so a record keeps its id when the same specimen is
// found (same identifiers at the same row, or at another row: remapped), and
// monitoring links, attachments and notebook matches keep pointing at it. The
// thousands of differences are not logged as edits made in Google Sheets: the
// Historial of the old workbook is archived (archived_actions,
// archived_changes) and the Historial starts empty. Pending AI proposals were
// made against the old rows and are discarded; Revisión verdicts judged the old
// rows and are archived. Users, sessions, monitoring walks and photos,
// envelope and photo curation, Wikiloc profiles and tokens are kept.

import { randomUUID } from 'node:crypto';
import { modules } from './schema.mjs';
import { describeProblems, headerLayout } from './columns.mjs';
import { bumpReviewRevision } from './photodata.mjs';

const now = () => new Date().toISOString();
const fail = (code, message) => Object.assign(new Error(message), { code });
const parse = value => {
  try {
    return value ? JSON.parse(value) : null;
  } catch {
    return null;
  }
};
const tableExists = (db, name) => !!db.prepare("SELECT 1 FROM sqlite_master WHERE type='table' AND name=?").get(name);
const count = (db, sql, ...args) => db.prepare(sql).get(...args).n;

/**
 * Reads every sheet of the target workbook once, before anything is changed:
 * a sheet whose header cannot be read (a missing identity column, a repeated
 * header) stops the switch with nothing written. Returns an adapter that
 * serves these reads to the sync and refuses every write.
 */
export async function readWorkbook(sheets, { log = () => {} } = {}) {
  const rows = new Map();
  const problems = [];
  for (const mod of modules) {
    const read = await sheets.readSheet(mod.id);
    const layout = headerLayout(
      mod.id,
      read.find(r => r.row === mod.headerRow),
    );
    if (layout.blocked) problems.push({ sheet: mod.id, problems: layout.problems });
    rows.set(mod.id, read);
    log(`  read ${mod.id}: ${read.length} rows`);
  }
  if (problems.length)
    throw fail(
      'HEADER_PROBLEMS',
      `Sheets of the new workbook that cannot be read (nothing was changed): ${problems
        .map(p => describeProblems(p.sheet, p.problems))
        .join('; ')}`,
    );
  const revision = sheets.revision ? await sheets.revision() : null;
  const readOnly = () => {
    throw new Error('The workbook switch never writes to Google Sheets');
  };
  return {
    spreadsheetId: sheets.spreadsheetId,
    revision: async () => revision,
    readSheet: async sheet => rows.get(sheet) || [],
    readRows: readOnly,
    readGrid: readOnly,
    writeBatch: readOnly,
    batchUpdate: readOnly,
  };
}

/** The record ids other tables point at, to report which survive the switch. */
function references(db) {
  const refs = [];
  const add = (kind, id, extra = {}) => id && refs.push({ kind, id: String(id), ...extra });
  for (const t of db.prepare('SELECT id, data_json FROM monitoring_tracks').all())
    for (const c of parse(t.data_json)?.captures || [])
      add(c.link === 'manual' ? 'monitoring (manual link)' : 'monitoring (automatic link)', c.recordId);
  for (const table of ['attachments', 'tasks', 'events'])
    if (tableExists(db, table))
      for (const r of db.prepare(`SELECT record_id FROM ${table} WHERE record_id IS NOT NULL`).all())
        add(table, r.record_id);
  return refs;
}

function archive(db, table, archived, stamp, from) {
  if (!tableExists(db, table)) return 0;
  if (!tableExists(db, archived))
    db.exec(`CREATE TABLE ${archived} AS SELECT *, '' AS archived_workbook, '' AS archived_at FROM ${table} WHERE 0`);
  const columns = db
    .prepare(`PRAGMA table_info(${table})`)
    .all()
    .map(c => `"${c.name}"`)
    .join(',');
  db.prepare(
    `INSERT INTO ${archived}(${columns},archived_workbook,archived_at) SELECT ${columns},?,? FROM ${table}`,
  ).run(from, stamp);
  const moved = count(db, `SELECT count(*) n FROM ${table}`);
  db.exec(`DELETE FROM ${table}`);
  return moved;
}

/**
 * Switches `store` (opened with { switching: true } on the new workbook's
 * adapter from readWorkbook) to that workbook. Returns a report of what
 * changed; `store.db` holds the result.
 */
export async function switchWorkbook(store, { from = store.cachedWorkbook(), user = 'workbook-switch' } = {}) {
  const db = store.db;
  const to = store.sheets.spreadsheetId;
  if (from === to) throw fail('SAME_WORKBOOK', `The database already caches workbook ${to}`);
  const stamp = now();
  const before = {
    records: Object.fromEntries(
      db
        .prepare('SELECT sheet, count(*) n FROM records WHERE missing=0 GROUP BY sheet')
        .all()
        .map(r => [r.sheet, r.n]),
    ),
    refs: references(db),
  };
  const openProposals = !tableExists(db, 'ai_proposals')
    ? []
    : db
        .prepare(
          "SELECT id, reason, status, created_at FROM ai_proposals WHERE status IN ('pending','applying','needs_review') ORDER BY created_at",
        )
        .all()
        .map(p => ({ id: p.id, status: p.status, createdAt: p.created_at, reason: p.reason }));
  const unsettled = count(db, "SELECT count(*) n FROM actions WHERE status IN ('pending','uncertain')");

  // The rows, matched by identity then row, with no history of the differences.
  const sync = await store.sync({ force: true, history: false });
  if (sync.skipped) throw fail('SYNC_INCOMPLETE', `${sync.skipped} sheets could not be read; nothing else was changed`);

  const alive = db.prepare('SELECT 1 FROM records WHERE id=? AND missing=0');
  const refs = {};
  for (const r of before.refs) {
    const entry = (refs[r.kind] ||= { total: 0, kept: 0, lost: 0 });
    entry.total++;
    if (alive.get(r.id)) entry.kept++;
    else entry.lost++;
  }

  db.exec('BEGIN IMMEDIATE');
  let archived;
  try {
    // Changes first: they reference their action.
    const changes = archive(db, 'changes', 'archived_changes', stamp, from);
    archived = {
      actions: archive(db, 'actions', 'archived_actions', stamp, from),
      changes,
      undoPlans: archive(db, 'undo_plans', 'archived_undo_plans', stamp, from),
      issueVerdicts: archive(db, 'issue_verdicts', 'archived_issue_verdicts', stamp, from),
    };
    if (openProposals.length)
      db.prepare(
        "UPDATE ai_proposals SET status='discarded', updated_at=?, last_by=? WHERE status IN ('pending','applying','needs_review')",
      ).run(stamp, user);
    db.prepare('DELETE FROM import_previews WHERE applied=0').run();
    store.setSetting('workbookId', to);
    store.setSetting('previousWorkbookId', from || '');
    store.setSetting('workbookSwitchedAt', stamp);
    db.prepare('INSERT INTO audit(id,kind,detail_json,created_at) VALUES(?,?,?,?)').run(
      randomUUID(),
      'workbook_switch',
      JSON.stringify({
        from,
        to,
        sync: { ...sync, headerProblems: undefined },
        archived,
        discarded: openProposals.map(p => p.id),
      }),
      stamp,
    );
    db.exec('COMMIT');
  } catch (e) {
    db.exec('ROLLBACK');
    throw e;
  }
  // Revisión de datos and photo checks are cached on this revision; the app rebuilds its other caches on start.
  bumpReviewRevision(db);

  const after = Object.fromEntries(
    db
      .prepare('SELECT sheet, count(*) n FROM records WHERE missing=0 GROUP BY sheet')
      .all()
      .map(r => [r.sheet, r.n]),
  );
  const sheets = modules.map(m => {
    const s = sync.bySheet?.[m.id] || {};
    return {
      sheet: m.id,
      before: before.records[m.id] || 0,
      after: after[m.id] || 0,
      kept: (after[m.id] || 0) - (s.added || 0) - (s.moved || 0),
      moved: s.moved || 0,
      added: s.added || 0,
      retired: s.missing || 0,
      changedRows: s.changed || 0,
      changedCells: s.cells || 0,
    };
  });
  return {
    from,
    to,
    at: stamp,
    revision: store.getSetting('sourceRevision'),
    sheets,
    totals: {
      kept: sheets.reduce((n, s) => n + s.kept, 0),
      moved: sync.moved,
      added: sync.added,
      retired: sync.missing,
      changedRows: sync.changed,
      changedCells: sync.cells,
    },
    references: refs,
    archived,
    unsettledActions: unsettled,
    discardedProposals: openProposals,
    headerProblems: sync.headerProblems,
  };
}

/** The report as lines of text for the console. */
export function formatReport(report) {
  const lines = [];
  lines.push(`Workbook ${report.from || '(none)'} -> ${report.to}  (Drive revision ${report.revision ?? '?'})`);
  lines.push('');
  lines.push('sheet                              before   after    kept   moved   added retired  changed rows / cells');
  for (const s of report.sheets)
    lines.push(
      `${s.sheet.padEnd(34)} ${String(s.before).padStart(6)} ${String(s.after).padStart(7)} ${String(s.kept).padStart(7)} ${String(s.moved).padStart(7)} ${String(s.added).padStart(7)} ${String(s.retired).padStart(7)}  ${s.changedRows} / ${s.changedCells}`,
    );
  const t = report.totals;
  lines.push(
    `TOTAL kept ${t.kept}, moved (remapped by identity) ${t.moved}, added ${t.added}, retired ${t.retired}, changed rows ${t.changedRows} (${t.changedCells} cells, not logged as edits)`,
  );
  lines.push('');
  lines.push('Links to records:');
  for (const [kind, r] of Object.entries(report.references))
    lines.push(`  ${kind}: ${r.total} (${r.kept} still point at a row, ${r.lost} to a row gone from the new workbook)`);
  if (!Object.keys(report.references).length) lines.push('  none');
  const a = report.archived;
  lines.push('');
  lines.push(
    `Historial archived: ${a.actions} saves, ${a.changes} cell changes, ${a.undoPlans} undo plans; ${a.issueVerdicts} Revisión verdicts archived.` +
      (report.unsettledActions ? ` ${report.unsettledActions} of the saves were still pending/uncertain.` : ''),
  );
  lines.push(`AI proposals discarded (${report.discardedProposals.length}):`);
  for (const p of report.discardedProposals)
    lines.push(`  ${p.id}  ${p.status}  ${p.createdAt}  ${String(p.reason ?? '').slice(0, 90)}`);
  const problems = Object.entries(report.headerProblems || {});
  if (problems.length) {
    lines.push('');
    lines.push('Header notes (not blocking):');
    for (const [sheet, list] of problems) lines.push(`  ${sheet}: ${describeProblems(sheet, list)}`);
  }
  return lines;
}
