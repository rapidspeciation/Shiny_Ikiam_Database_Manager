import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { DatabaseSync } from 'node:sqlite';
import { createAssistant, mcpTools } from '../server/assistant.mjs';
import { ACCEPTED_KEYS, closestKey, needsText, unknownKeys, withInputNotes } from '../server/tool-input.mjs';

// Retro 16 Nov 2026: chats called db tools before their schema was loaded and guessed keys (failed calls in its
// week: host_plants 73, show_photos 22, row_history 16, tubes_lookup 10, send_sheet_photos 9). The real schemas, so a renamed key fails here.
const SCHEMAS = Object.fromEntries(mcpTools().map(t => [t.name, t.inputSchema]));
const notes = (name, args, out) => withInputNotes(name, SCHEMAS[name], args, out);

test('a guessed key is named with the closest key the tool has (the retro calls)', () => {
  assert.equal(
    notes('host_plants', { species: 'Mechanitis', area: 'Napo', photo_sheet: true }, { error: "Name a 'butterfly' or a 'plant'." }).error,
    "Unknown key 'species': use 'butterfly'. Name a 'butterfly' or a 'plant'.",
  );
  assert.match(notes('host_plants', { taxon: 'x', country: 'EC' }, { error: 'e' }).error, /^Unknown key 'taxon': use 'butterfly'\./);
  assert.match(notes('show_photos', { cams: ['CAM079891'] }, { error: 'ids: the CAM IDs to show' }).error, /^Unknown key 'cams': use 'ids'\. Missing key: 'ids', e\.g\. /);
  assert.match(notes('show_photos', { cam_ids: ['CAM079891'] }, { error: 'e' }).error, /Unknown key 'cam_ids': use 'ids'/);
  assert.match(notes('tubes_lookup', { ids: ['FS1'] }, { error: 'e' }).error, /^Unknown key 'ids': use 'tubes'/);
  assert.match(notes('tubes_lookup', { query: 'rack 3' }, { error: 'e' }).error, /^Unknown key 'query': use 'reading'/);
  assert.match(notes('maps_snapshot', { taxon: ['x'], sex: 'male' }, { error: 'e' }).error, /^Unknown key 'taxon': use 'species'/);
  assert.match(notes('show_rows', { ids: ['CAM079891'] }, { error: 'e' }).error, /^Unknown key 'ids': use 'recordIds'/);
  // A key of another meaning is not offered: a proposal is not a row's id.
  assert.doesNotMatch(notes('list_proposals', { proposal: 'p1' }, { error: 'e' }).error, /use 'id'/);
  assert.match(notes('send_sheet_photos', { lang: 'es', sheets: ['s1'] }, { error: 'e' }).error, /^Missing key: 'tiles', e\.g\. \{"tiles":\[1,4\]\}\. e$/);
  // No close key: the tool's keys are listed.
  assert.equal(
    notes('show_photos', { query: 'x' }, { error: 'e' }).error,
    "Unknown key 'query' (show_photos's keys: ids, view, notes, title). Missing key: 'ids', e.g. {\"ids\":[\"CAM079891\"]}. e",
  );
});

test('an empty input that fails says the key the tool needs, with an example, before its own error', () => {
  // Required keys: the input cannot be right without them.
  assert.equal(notes('show_photos', {}, { error: 'ids: the CAM IDs to show' }).error, 'show_photos needs ids, e.g. {"ids":["CAM079891"]}. ids: the CAM IDs to show');
  assert.match(notes('send_sheet_photos', {}, { error: 'e' }).error, /^send_sheet_photos needs sheets and tiles, e\.g\. \{"sheets":\[.*\],"tiles":\[1,4\]\}\. e$/);
  assert.match(notes('send_sheet_photos', { sheets: ['s1'] }, { error: 'e' }).error, /^Missing key: 'tiles', e\.g\. \{"tiles":\[1,4\]\}\. e$/);
  // No example where a key means something else in each tool, none for `confirmed`; the tool's own condition stays.
  const undo = notes('undo_edits', {}, { error: 'Not confirmed: show preview_undo to the person and call again with confirmed: true only after they approve' });
  assert.match(undo.error, /^undo_edits needs [^,]*confirmed[^,]*\. Not confirmed: .*only after they approve$/);
  assert.doesNotMatch(undo.error, /e\.g\./);
  assert.doesNotMatch(notes('search_text', {}, { error: 'search_text needs `query`: the text to find' }).error, /SELECT/);
  // No required key: {} may be a whole input (list_invitations, review_issues), failing for another reason
  // (only administrators), so the tool's own error stays; those tools say what they need themselves.
  assert.deepEqual(notes('list_invitations', {}, { error: 'Only administrators see invitations' }), { error: 'Only administrators see invitations' });
  assert.deepEqual(notes('tubes_lookup', {}, { error: "Give 'tubes' (IDs) or a 'reading'." }), { error: "Give 'tubes' (IDs) or a 'reading'." });
  assert.deepEqual(notes('sync_documents', {}, { error: 'own' }), { error: 'own' });
  assert.deepEqual(notes('list_proposals', {}, { proposals: [] }), { proposals: [] });
});

test('a key a tool reads in place of a schema key is no unknown key and stands for a required one (critics r1)', () => {
  // find_records and describe_sheet read `sheet` for `module`, count_records `module` for `sheet`, get_record `recordId`/`key` for `id`.
  assert.deepEqual(notes('find_records', { sheet: 'Collection_data' }, { records: [1] }), { records: [1] });
  assert.deepEqual(notes('find_records', { sheet: 'Colection_data' }, { error: 'Unknown sheet Colection_data' }), { error: 'Unknown sheet Colection_data' });
  assert.deepEqual(notes('describe_sheet', { sheet: 'Collection_data' }, { columns: [] }), { columns: [] });
  assert.deepEqual(notes('count_records', { module: 'Collection_data' }, { count: 3 }), { count: 3 });
  assert.deepEqual(notes('get_record', { recordId: 'r1' }, { error: 'Record not found' }), { error: 'Record not found' });
  assert.deepEqual(notes('maps_snapshot', { taxa: ['Mechanitis'] }, { ok: 1 }), { ok: 1 });
  // Only for the key it stands for (critic r2): another required key is still named, and an optional accepted key stands for none.
  assert.match(notes('send_sheet_photos', { sheet: 'S1' }, { error: 'e' }).error, /^Missing key: 'tiles'/);
  assert.match(notes('find_records', { groupBy: 'Sex' }, { error: 'Unknown sheet ' }).error, /^Missing key: 'module'\. Unknown sheet$/);
  assert.match(notes('maps_snapshot', { near: { lat: 0, lng: 0 } }, { error: 'e' }).error, /^Missing key: 'species'/);
});

test('an answer that worked keeps its content; keys outside the schema get a note, keys a tool reads on purpose none', () => {
  assert.deepEqual(notes('find_records', { module: 'Collection_data', limit: 2 }, { records: [] }), { records: [] });
  const out = notes('find_records', { modul: 'Collection_data', bogus: 1 }, { records: [1] });
  assert.deepEqual(out.records, [1]);
  assert.equal(out.inputWarning, "Not in find_records's schema: 'modul' (use 'module'), 'bogus'. Check the answer is what you asked for.");
  // query answers in text: the note goes after it.
  assert.equal(notes('query', { sql: 'SELECT 1', limt: 5 }, '1 row'), "1 row\n\nNote: Not in query's schema: 'limt' (use 'limit'). Check the answer is what you asked for.");
  for (const [name, keys] of Object.entries(ACCEPTED_KEYS)) {
    assert.ok(SCHEMAS[name], `${name} is a tool`);
    assert.deepEqual(unknownKeys(name, SCHEMAS[name], Object.fromEntries(keys.map(k => [k, 'x']))), [], name);
  }
});

test('closestKey: same meaning first, then spelling; nothing when nothing is close', () => {
  assert.equal(closestKey('Species', ['butterfly', 'plant']), 'butterfly');
  assert.equal(closestKey('camIds', ['ids', 'view']), 'ids');
  assert.equal(closestKey('recordid', ['recordId', 'id']), 'recordId');
  assert.equal(closestKey('sheet', ['sheets', 'tiles']), 'sheets');
  assert.equal(closestKey('sheet', ['module', 'fields']), 'module');
  assert.equal(closestKey('proposal', ['proposalId', 'full']), 'proposalId');
  assert.equal(closestKey('proposal', ['id', 'status']), null);
  assert.equal(closestKey('columns', ['fields', 'module']), 'fields');
  assert.equal(closestKey('xyzzy', ['ids', 'view']), null);
  assert.equal(needsText('sync_documents', SCHEMAS.sync_documents), null);
  assert.equal(needsText('row_history', SCHEMAS.row_history), null, 'no required key: row_history says it itself');
});

test('every db tool call goes through the check: row_history {} and host_plants {species} over MCP', async () => {
  const db = new DatabaseSync(':memory:');
  try {
    db.exec('CREATE TABLE users (id TEXT PRIMARY KEY, username TEXT, display_name TEXT, role TEXT, active INTEGER)');
    db.prepare("INSERT INTO users VALUES ('u1','lab','Lab','editor',1)").run();
    const assistant = createAssistant({ store: { db, getRecord: () => null }, config: { knowledgeRoots: [] } });
    const token = 'token-for-lab';
    db.prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')").run(
      createHash('sha256').update(token).digest('hex'),
      'u1',
    );
    const call = async (name, args) => {
      const reply = await assistant.mcp(
        { authorization: `Bearer ${token}` },
        { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } },
      );
      return { isError: reply.body.result.isError, out: JSON.parse(reply.body.result.content[0].text) };
    };
    const history = await call('row_history', {});
    assert.equal(history.isError, true);
    // row_history says it itself (server/history.mjs), not "No row found for ".
    assert.match(history.out.error, /^row_history needs id, e\.g\. \{"id":"CAM079891"\}/);
    assert.doesNotMatch(history.out.error, /No row found/);
    const plants = await call('host_plants', { species: 'Mechanitis messenoides' });
    assert.equal(plants.isError, true);
    assert.match(plants.out.error, /^Unknown key 'species': use 'butterfly'\./);
  } finally {
    db.close();
  }
});

test('the small tools chats called before loading them load with the chat (anthropic/alwaysLoad)', () => {
  const loaded = new Set(mcpTools().filter(t => t._meta?.['anthropic/alwaysLoad']).map(t => t.name));
  for (const name of ['row_history', 'show_photos', 'tubes_lookup', 'send_sheet_photos', 'query', 'find_records']) assert.ok(loaded.has(name), name);
  // The large ones stay behind tool search (their schemas are kB each; the check above answers a guess).
  for (const name of ['host_plants', 'maps_snapshot']) assert.ok(!loaded.has(name), name);
});
