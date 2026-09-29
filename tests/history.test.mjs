import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { mkdtempSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { DatabaseSync } from 'node:sqlite';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { applyBatch } from '../server/batch.mjs';
import { createAssistant } from '../server/assistant.mjs';
import {
  backfillPurposes,
  compressLabels,
  historyGroup,
  historyGroups,
  inferPurpose,
  previewEdits,
  summaryText,
  undoEdits,
} from '../server/history.mjs';

const ana = { id: 'u-ana', username: 'ana', displayName: 'Ana Pérez', role: 'editor' };
const bob = { id: 'u-bob', username: 'bob', displayName: 'Bob Díaz', role: 'editor' };
const viewer = { id: 'u-val', username: 'val', displayName: 'Val', role: 'viewer' };
const formulaCell = (formula, value) => ({ userEnteredValue: { formulaValue: formula }, effectiveValue: { stringValue: value } });
const BASE = Date.parse('2026-09-20T15:00:00.000Z');
const time = minutes => new Date(BASE + minutes * 60_000).toISOString();

async function fixture() {
  const sheets = new LocalSheets({
    Insectary_data: [
      ...['A0A', 'A1A', 'A2A', 'A3A'].map((id, i) => ({ row: i + 2, values: { Insectary_ID: id, SPECIES: 'Melinaea menophilus', Sex: 'female' } })),
      { row: 6, cells: [formulaCell('="A4A"', 'A4A')] },
    ],
    Collection_data: [{ row: 2, values: { CAM_ID: 'CAM000001', SPECIES: 'Species', Notes_Collection_data: 'first' } }],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Collection_data'] });
  for (const u of [ana, bob, viewer])
    store.db
      .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES(?,?,?,?,'s','h',1,'2026-01-01')")
      .run(u.id, u.username, u.displayName, u.role);
  let n = 0;
  /** One save, dated `minutes` after the base time. */
  const save = async (who, purpose, minutes, body) => {
    const out = await applyBatch(store, { requestId: `history-${++n}-request`, purpose, ...body }, who);
    store.db.prepare('UPDATE actions SET created_at=? WHERE id=?').run(time(minutes), out.action.id);
    return out.action.id;
  };
  const row = (sheet, r) => store.getRecordBySheetRow(sheet, r);
  return { store, sheets, save, row };
}

/** The scene of the Historial tests: interleaved saves by two people, then two syncs of edits made in Sheets. */
async function scene() {
  const f = await fixture();
  const { store, save, row } = f;
  const ids = {};
  const newCam = cam => ({ module: 'Collection_data', values: { CAM_ID: cam, SPECIES: 'Oleria gunilla' } });
  ids.c0 = await save(ana, 'colecta', 0, { creates: [newCam('CAM000101'), newCam('CAM000102')] });
  ids.c10 = await save(ana, 'colecta', 10, { edits: [{ id: row('Collection_data', 2).id, values: { Notes_Collection_data: 'second' } }] });
  ids.m12 = await save(bob, 'muertes', 12, {
    edits: [
      { id: row('Insectary_data', 2).id, values: { Death_date: '2026-09-19' } },
      { id: row('Insectary_data', 5).id, values: { Death_date: '2026-09-18' } },
    ],
  });
  ids.c15 = await save(ana, 'colecta', 15, { creates: [newCam('CAM000103')] });
  ids.c60 = await save(ana, 'colecta', 60, { edits: [{ id: row('Collection_data', 2).id, values: { Notes_Collection_data: 'third' } }] });
  ids.t61 = await save(ana, 'tablas', 61, {
    edits: [{ id: row('Insectary_data', 3).id, values: { Sex: 'male', Notes_Insectary_data: 'sexo corregido' } }],
  });
  // Edits read from Google Sheets: three rows in one sync, one in the next (6 minutes later).
  const external = async (r, note, minutes) => {
    await f.sheets.externalEdit('Insectary_data', r, { Notes_Insectary_data: note });
    await store.refreshRows('Insectary_data', [r]);
    const id = store.db.prepare("SELECT id FROM actions WHERE source='sheet_reconciliation' ORDER BY rowid DESC LIMIT 1").get().id;
    store.db.prepare('UPDATE actions SET created_at=? WHERE id=?').run(time(minutes), id);
    return id;
  };
  ids.s100 = await external(2, 'uno', 100);
  await external(4, 'dos', 100.2);
  await external(5, 'tres', 100.4);
  ids.s106 = await external(4, 'cuatro', 106);
  return { ...f, ids };
}

test('saves group by person and purpose within 30 minutes, even when others save in between; syncs from Sheets are one group each', async () => {
  const { store, ids } = await scene();
  const { groups, next } = historyGroups(store, {});
  assert.equal(next, null);
  assert.deepEqual(
    groups.map(g => [g.purpose, g.actorName, g.counts.actions]),
    [
      ['sheets', null, 1],
      ['sheets', null, 3],
      ['tablas', 'Ana Pérez', 1],
      ['colecta', 'Ana Pérez', 1],
      ['colecta', 'Ana Pérez', 3],
      ['muertes', 'Bob Díaz', 1],
    ],
  );
  const colecta = groups[4];
  // The group's id is its first save; any of its saves finds it.
  assert.equal(colecta.id, ids.c0);
  assert.deepEqual(colecta.actionIds, [ids.c15, ids.c10, ids.c0]);
  assert.equal(colecta.start, time(0));
  assert.equal(colecta.end, time(15));
  assert.deepEqual(colecta.counts, { actions: 3, rows: 4, newRows: 3, cells: 7 });
  assert.equal(colecta.summary, '3 filas nuevas y 1 editada: CAM000001, CAM000101–CAM000103');
  assert.equal(colecta.link, `#/historial?grupo=${ids.c0}`);
  assert.equal(groups[5].summary, '2 muertes registradas: A0A, A3A');
  assert.equal(groups[1].summary, '3 filas cambiadas en Google Sheets: A0A, A2A, A3A');
  assert.equal(historyGroup(store, ids.c10).id, ids.c0);
  assert.equal(historyGroup(store, ids.s106).counts.actions, 1);

  // Filters pick whole groups.
  assert.deepEqual(historyGroups(store, { user: 'bob' }).groups.map(g => g.id), [ids.m12]);
  assert.deepEqual(historyGroups(store, { user: 'Ana' }).groups.length, 3);
  assert.deepEqual(historyGroups(store, { purpose: 'colecta' }).groups.map(g => g.id), [ids.c60, ids.c0]);
  const found = historyGroups(store, { text: 'CAM000102' }).groups;
  assert.deepEqual(found.map(g => g.id), [ids.c0]);
  assert.deepEqual(found[0].matched, [ids.c0]);
  assert.equal(found[0].counts.actions, 3, 'the whole save is shown, not only the matching part');
  assert.deepEqual(historyGroups(store, { sheet: 'Collection_data' }).groups.map(g => g.id), [ids.c60, ids.c0]);
  assert.equal(historyGroups(store, { from: '2026-09-20', to: '2026-09-20' }).groups.length, 6);
  assert.equal(historyGroups(store, { to: '2026-09-19' }).groups.length, 0);
  assert.equal(historyGroups(store, { from: '2026-09-21' }).groups.length, 0);

  // Pages, and loading down to a linked save.
  const first = historyGroups(store, { limit: 2 });
  assert.equal(first.groups.length, 2);
  assert.equal(first.next, 2);
  const second = historyGroups(store, { limit: 2, offset: first.next });
  assert.deepEqual(second.groups.map(g => g.purpose), ['tablas', 'colecta']);
  const linked = historyGroups(store, { limit: 1, until: ids.m12 });
  assert.equal(linked.found, true);
  assert.equal(linked.groups.at(-1).id, ids.m12);
  assert.equal(linked.next, null);
  store.close();
});

test('purposes: declared by the app, inferred for older saves, added to an old database', async () => {
  assert.equal(inferPurpose({ source: 'undo' }), 'deshacer');
  assert.equal(inferPurpose({ source: 'ai_approved' }), 'asistente');
  assert.equal(inferPurpose({ source: 'sheet_reconciliation' }), 'sheets');
  assert.equal(inferPurpose({ source: 'app', reason: 'Cambiar Insectary ID A0A → A1A' }), 'cambio_id');
  assert.equal(inferPurpose({ source: 'app', reason: 'death: linked' }), 'muertes');
  const change = (sheet, field, extra = {}) => ({ sheet, field, before: null, after: 'x', ...extra });
  assert.equal(inferPurpose({ source: 'app' }, [change('Collection_data', 'CAM_ID', { isNew: true })]), 'colecta');
  assert.equal(inferPurpose({ source: 'app' }, [change('Collection_data', 'Purpose', { after: 'Monitoring', isNew: true })]), 'monitoreo');
  assert.equal(inferPurpose({ source: 'app' }, [change('Collection_data', 'Sex', { rowPurpose: 'Monitoring_Ikiam' })]), 'monitoreo');
  assert.equal(inferPurpose({ source: 'app' }, [change('Collection_data', 'Tube_1_id', { before: 'NA' })]), 'tubos');
  assert.equal(
    inferPurpose({ source: 'app' }, [change('Insectary_data', 'Death_date'), change('Insectary_data', 'CAM_ID')]),
    'muertes',
  );
  assert.equal(
    inferPurpose({ source: 'app' }, [change('Insectary_data', 'CLUTCH NUMBER', { isNew: true }), change('Insectary_data', 'Sex', { isNew: true })]),
    'emergidos',
  );
  assert.equal(inferPurpose({ source: 'app' }, [change('Insectary_data', 'Tube_2_id'), change('Insectary_data', 'T2_Preservation_medium')]), 'tubos');
  assert.equal(inferPurpose({ source: 'app' }, [change('Insectary_stocks', 'NUMBER OF EGGS', { before: 3 })]), 'clutches');
  assert.equal(inferPurpose({ source: 'app' }, [change('Insectary_data', 'Sex', { before: 'male' })]), 'tablas');

  // A save from a tab keeps its purpose; one without it is inferred when written.
  const { store, save, row } = await fixture();
  const declared = await save(ana, 'tablas', 0, { edits: [{ id: row('Insectary_data', 2).id, values: { Death_date: '2026-09-19' } }] });
  const inferred = await save(ana, undefined, 1, { edits: [{ id: row('Insectary_data', 3).id, values: { Death_date: '2026-09-19' } }] });
  const bogus = await save(ana, 'hackeo', 2, { edits: [{ id: row('Insectary_data', 4).id, values: { Death_date: '2026-09-19' } }] });
  const purposeOf = id => store.db.prepare('SELECT purpose FROM actions WHERE id=?').get(id).purpose;
  assert.equal(purposeOf(declared), 'tablas');
  assert.equal(purposeOf(inferred), 'muertes');
  assert.equal(purposeOf(bogus), 'muertes', 'an unknown purpose from a client is ignored');
  assert.equal(store.getHistory({ purpose: 'tablas' }).actions[0].purpose, 'tablas');

  // Saves from before the purpose existed get one.
  store.db.prepare('UPDATE actions SET purpose=NULL').run();
  assert.equal(backfillPurposes(store.db), 3);
  assert.equal(purposeOf(declared), 'muertes');
  store.close();

  // An old database: the column is added and its saves inferred when the app starts.
  const dir = mkdtempSync(join(tmpdir(), 'history-migration-'));
  try {
    const path = join(dir, 'old.sqlite');
    const old = new DatabaseSync(path);
    old.exec(`CREATE TABLE actions(id TEXT PRIMARY KEY, request_id TEXT UNIQUE, actor TEXT NOT NULL, source TEXT NOT NULL, created_at TEXT NOT NULL, status TEXT NOT NULL, reason TEXT, reverses TEXT, result_json TEXT);
      CREATE TABLE changes(id TEXT PRIMARY KEY, action_id TEXT NOT NULL, record_id TEXT NOT NULL, sheet TEXT NOT NULL, row_num INTEGER NOT NULL, field TEXT NOT NULL, before_json TEXT, after_json TEXT);
      INSERT INTO actions VALUES('a1','r1','u-ana','app','2026-01-01T00:00:00Z','verified',NULL,NULL,'{"created":[{"clientId":"c","recordId":"rec1"}]}');
      INSERT INTO changes VALUES('c1','a1','rec1','Insectary_data',9,'CLUTCH NUMBER','null','944');
      INSERT INTO actions VALUES('a2',NULL,'unknown','sheet_reconciliation','2026-01-02T00:00:00Z','observed',NULL,NULL,NULL);`);
    old.close();
    const migrated = new Store({ localMode: true, databasePath: path }, { sheets: new LocalSheets({}) });
    assert.deepEqual(
      migrated.db.prepare('SELECT id, purpose FROM actions ORDER BY id').all().map(r => [r.id, r.purpose]),
      [
        ['a1', 'emergidos'],
        ['a2', 'sheets'],
      ],
    );
    assert.ok(migrated.db.prepare("SELECT 1 FROM sqlite_master WHERE name='changes_action'").get());
    migrated.close();
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test('undo a whole group, one save or one change, after a preview; the undo is a save that can be undone', async () => {
  const { store, ids, row } = await scene();
  const notes = () => row('Collection_data', 2).values.Notes_Collection_data;

  // The colecta group edited a cell that a later save changed again: the preview says so and nothing is written.
  const blocked = previewEdits(store, { groupIds: [ids.c0] });
  assert.equal(blocked.eligible, false);
  assert.equal(blocked.conflicts[0].reason, 'later_field_edit');
  assert.equal(blocked.conflicts[0].label, 'CAM000001');
  await assert.rejects(undoEdits(store, { groupIds: [ids.c0], requestId: 'undo-blocked-1' }, ana), { code: 'UNDO_CONFLICT' });

  // The later save first (by any of its action ids), then the whole group.
  const later = previewEdits(store, { actionIds: [ids.c60] });
  assert.deepEqual(
    later.changes.map(c => [c.label, c.field, c.before, c.after]),
    [['CAM000001', 'Notes_Collection_data', 'third', 'second']],
  );
  await undoEdits(store, { actionIds: [ids.c60], requestId: 'undo-later-1' }, ana);
  assert.equal(notes(), 'second');
  const group = previewEdits(store, { groupIds: [ids.c0] });
  assert.equal(group.eligible, true);
  assert.equal(group.changes.length, 7);
  const undone = await undoEdits(store, { groupIds: [ids.c0], requestId: 'undo-group-1', reason: 'colecta repetida' }, ana);
  assert.equal(notes(), 'first');
  assert.equal(row('Collection_data', 3).values.CAM_ID ?? null, null);
  // A retried request returns the first outcome.
  assert.equal((await undoEdits(store, { groupIds: [ids.c0], requestId: 'undo-group-1' }, ana)).action.id, undone.action.id);

  // The undo is a save of its own (purpose deshacer); the group shows as undone, with its rows still named.
  const detail = historyGroup(store, ids.c0);
  assert.equal(detail.undone, 'all');
  assert.equal(detail.undoable, false);
  assert.ok(detail.actions.every(a => a.reversedBy === undone.action.id));
  assert.ok(detail.actions.flatMap(a => a.changes).every(c => c.undone));
  assert.ok(detail.actions.flatMap(a => a.changes).some(c => c.label === 'CAM000102' && c.isNew));
  const undoGroup = historyGroup(store, undone.action.id);
  assert.equal(undoGroup.purpose, 'deshacer');
  assert.match(undoGroup.summary, /filas restauradas/);
  await assert.rejects(undoEdits(store, { groupIds: [ids.c0], requestId: 'undo-group-2' }, ana), { code: 'NOTHING_TO_UNDO' });
  // Undoing the undo puts the save back.
  await undoEdits(store, { actionIds: [undone.action.id], requestId: 'redo-group-1' }, ana);
  assert.equal(notes(), 'second');
  assert.equal(row('Collection_data', 3).values.CAM_ID, 'CAM000101');
  assert.equal(historyGroup(store, ids.c0).undone, null, 'an undone undo does not count');

  // One change of a save: only that cell.
  const tablas = historyGroup(store, ids.t61);
  const sex = tablas.actions[0].changes.find(c => c.field === 'Sex');
  await undoEdits(store, { changeIds: [sex.id], requestId: 'undo-change-1' }, ana);
  assert.equal(row('Insectary_data', 3).values.Sex, 'female');
  assert.equal(row('Insectary_data', 3).values.Notes_Insectary_data, 'sexo corregido');
  assert.equal(historyGroup(store, ids.t61).undone, 'all', 'the save counts as undone (in part)');
  assert.equal(historyGroup(store, ids.t61).undoable, false);

  // One change of a group, then "Deshacer todo" undoes the rest.
  const deaths = historyGroup(store, ids.m12).actions[0].changes;
  await undoEdits(store, { changeIds: [deaths[0].id], requestId: 'undo-death-1' }, bob);
  const rest = previewEdits(store, { groupIds: [ids.m12] });
  assert.deepEqual(rest.changes.map(c => c.label), ['A3A']);
  await undoEdits(store, { groupIds: [ids.m12], requestId: 'undo-death-2' }, bob);
  assert.equal(row('Insectary_data', 5).values.Death_date ?? null, null);

  // Edits made directly in Google Sheets can be undone too.
  const sync = previewEdits(store, { groupIds: [ids.s106] });
  assert.equal(sync.eligible, true);
  assert.deepEqual(sync.changes.map(c => [c.field, c.before, c.after]), [['Notes_Insectary_data', 'cuatro', 'dos']]);

  // Only people who edit may undo.
  await assert.rejects(undoEdits(store, { groupIds: [ids.s106], requestId: 'undo-viewer-1' }, viewer), { code: 'FORBIDDEN' });
  store.close();
});

test('labels and summaries: runs of identifiers become ranges', () => {
  assert.deepEqual(compressLabels(['A2D', 'A0D', 'A1D', 'A3D', 'CAM079891', 'CAM079892', 'CAM079894', 'B0D', 'x']), [
    'A0D–A3D',
    'B0D',
    'CAM079891, CAM079892',
    'CAM079894',
    'x',
  ]);
  assert.equal(
    summaryText({ purpose: 'colecta', rows: 12, newRows: 12, labels: Array.from({ length: 12 }, (_, i) => `CAM0${79890 + i}`) }),
    '12 mariposas de colecta: CAM079890–CAM079901',
  );
  assert.equal(summaryText({ purpose: 'emergidos', rows: 1, newRows: 1, labels: ['5VB'] }), '1 emergido: 5VB');
  assert.equal(
    summaryText({ purpose: 'tablas', rows: 8, newRows: 0, labels: ['1', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] }),
    '8 filas editadas: 1, b, c, d, e, f y 2 más',
  );
  assert.equal(summaryText({ purpose: 'tablas', rows: 0 }), 'Sin cambios guardados');
});

test('assistant tools: find a save with its link, show it, preview and undo only once confirmed, as the person', async () => {
  const { store, ids, row } = await scene();
  const assistant = createAssistant({ store, config: { claude: {}, publicUrl: 'https://app.example/' } });
  const tokens = {};
  for (const u of [ana, viewer]) {
    tokens[u.id] = `token-${u.username}`;
    store.db
      .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
      .run(createHash('sha256').update(tokens[u.id]).digest('hex'), u.id);
  }
  const call = async (who, name, args) => {
    const out = await assistant.mcp(
      { authorization: `Bearer ${tokens[who.id]}` },
      { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } },
    );
    return JSON.parse(out.body.result.content[0].text);
  };
  const listed = await assistant.mcp({ authorization: `Bearer ${tokens[ana.id]}` }, { jsonrpc: '2.0', id: 1, method: 'tools/list' });
  const names = listed.body.result.tools.map(t => t.name);
  for (const name of ['list_history', 'get_history_group', 'preview_undo', 'undo_edits']) assert.ok(names.includes(name), name);

  const found = await call(ana, 'list_history', { text: 'CAM000102' });
  assert.equal(found.groups.length, 1);
  assert.equal(found.groups[0].url, `https://app.example/#/historial?grupo=${ids.c0}`);
  assert.equal(found.groups[0].user, 'Ana Pérez');
  assert.equal((await call(ana, 'list_history', { user: 'bob', purpose: 'muertes' })).groups[0].id, ids.m12);

  const group = await call(ana, 'get_history_group', { id: ids.m12 });
  assert.equal(group.url, `https://app.example/#/historial?grupo=${ids.m12}`);
  assert.deepEqual(
    group.actions[0].changes.map(c => [c.label, c.field, c.before, c.after]),
    [
      ['A0A', 'Death_date', null, '2026-09-19'],
      ['A3A', 'Death_date', null, '2026-09-18'],
    ],
  );

  const change = group.actions[0].changes[1].id;
  const preview = await call(ana, 'preview_undo', { changeIds: [change] });
  assert.equal(preview.eligible, true);
  assert.deepEqual(preview.changes, [{ label: 'A3A', sheet: 'Insectary_data', row: 5, field: 'Death_date', now: '2026-09-18', back: null }]);
  // Without the person's confirmation nothing is written.
  assert.match((await call(ana, 'undo_edits', { changeIds: [change] })).error, /Not confirmed/);
  assert.equal(typeof row('Insectary_data', 5).values.Death_date, 'number');
  // A viewer cannot undo, even when confirmed.
  assert.ok((await call(viewer, 'undo_edits', { changeIds: [change], confirmed: true })).error);
  const done = await call(ana, 'undo_edits', { changeIds: [change], confirmed: true });
  assert.equal(done.status, 'verified');
  assert.equal(done.undone, 1);
  assert.match(done.url, /^https:\/\/app\.example\/#\/historial\?grupo=/);
  assert.equal(row('Insectary_data', 5).values.Death_date ?? null, null);
  assert.equal(typeof row('Insectary_data', 2).values.Death_date, 'number');
  const undo = store.db.prepare("SELECT actor, purpose, reason FROM actions WHERE source='undo'").get();
  assert.deepEqual({ ...undo }, { actor: ana.id, purpose: 'deshacer', reason: 'Deshecho desde el asistente' });
  store.close();
});
