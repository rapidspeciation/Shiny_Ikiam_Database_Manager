import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { allIssues, checkData } from '../server/checks.mjs';
import { LOOK_ITEMS, lookAt } from '../server/look-at.mjs';

// IDs typed with a digit more or less than their series (the id_format check), and what the
// proposals' answers point out about their rows (lookAt): the checks, the notes, the gaps.

const fs = n => `FS${String(58489600 + n)}`;
/**
 * 60 preserved larvae (rows 2–61) with their CAM and tubes, but: L7E's Tube_1_id lacks a digit,
 * L9E has no CAM_ID, L12E no Tube_2_id; notes on L3E and L4E (the same) and L5E.
 * Collection_data: FS tubes too (one series with Insectary_data's), a short FF series with one
 * 7-digit tube, an FD series of mixed lengths, a field mark counter (B5 among B10–B99) and a
 * 5-digit CAM among 6-digit ones.
 */
function seed() {
  const larvae = Array.from({ length: 60 }, (_, i) => ({
    row: i + 2,
    values: {
      Insectary_ID: `L${i}E`,
      SPECIES: 'Mechanitis lysimnia',
      LIFESTAGE: '4th instar larva',
      Preserved_Dead_Alive: 'Dead',
      ...(i === 9 ? {} : { CAM_ID: `CAM0790${String(i).padStart(2, '0')}` }),
      Tube_1_id: i === 7 ? 'FS5848967' : fs(i),
      ...(i === 12 ? {} : { Tube_2_id: fs(100 + i) }),
      ...(i === 3 || i === 4 ? { Notes_Insectary_data: '16/9/2026 AA: Larvae 4th instar' } : {}),
      ...(i === 5 ? { Notes_Insectary_data: `16/9/2026 AA: ${'found on the leaf, '.repeat(10)}` } : {}),
    },
  }));
  const caught = Array.from({ length: 90 }, (_, i) => ({
    row: i + 2,
    values: {
      Release_Collect: 'Mark_Released',
      FieldMark_ID: i === 0 ? 'B5' : `B${i + 10}`,
      SPECIES: 'Ithomia salapia',
      ...(i < 30 ? { Tube_1_id: fs(300 + i) } : {}),
      ...(i === 30 ? { Tube_1_id: 'FS5849001' } : {}),
      ...(i >= 40 && i < 60 ? { Tube_2_id: i === 40 ? 'FF1161954' : `FF${10161900 + i}` } : {}),
      ...(i >= 30 && i < 90 ? { Tube_3_id: i < 34 ? `FD${308880300 + i}` : `FD${30888000 + i}` } : {}),
      ...(i >= 30 ? { CAM_ID_insectary: i === 31 ? 'CAM07901' : `CAM0720${i}` } : {}),
    },
  }));
  return { Insectary_data: larvae, Collection_data: caught };
}

async function setup() {
  const sheets = new LocalSheets(seed());
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Collection_data'] });
  const assistant = createAssistant({ store, config: {} });
  store.db
    .prepare(
      "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u-franz','franz','Franz Chandi','editor','s','h',1,'2026-01-01')",
    )
    .run();
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update('franz-token').digest('hex'), 'u-franz');
  const call = async (name, args) => {
    const out = await assistant.mcp(
      { authorization: 'Bearer franz-token' },
      { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } },
    );
    return JSON.parse(out.body.result.content[0].text);
  };
  const user = { id: 'u-franz', username: 'franz', displayName: 'Franz Chandi', role: 'editor' };
  return { store, assistant, call, user, row: n => store.getRecordBySheetRow('Insectary_data', n) };
}

test('id_format: an ID with a digit more or less than the rest of its series, only where the series is clear', async () => {
  const { store } = await setup();
  try {
    const out = checkData(store, { kind: 'id_format' });
    const said = out.issues.map(i => [i.sheet, i.row, i.field, i.value, i.problem]);
    assert.deepEqual(said, [
      ['Collection_data', 33, 'CAM_ID_insectary', 'CAM07901', 'Los CAM de CAM_ID_insectary tienen 6 dígitos; este tiene 5'],
      // Tubes are one series across the sheets: Collection_data's 31 FS tubes count with Insectary_data's 120.
      ['Collection_data', 32, 'Tube_1_id', 'FS5849001', 'Los tubos FS tienen 8 dígitos; este tiene 7'],
      ['Insectary_data', 9, 'Tube_1_id', 'FS5848967', 'Los tubos FS tienen 8 dígitos; este tiene 7'],
    ]);
    // Not flagged: FF (20 tubes, too few to tell), FD (4 of 60 with 9 digits: a mixed series), B5 among B10–B99 (a counter).
    assert.ok(!out.issues.some(i => /^(FF|FD|B)/.test(String(i.value))));
    assert.equal(out.kinds.id_format, 'ID con un dígito de más o de menos');
    // With English for the interface (server/messages.mjs descriptors).
    assert.deepEqual(out.issues.find(i => i.label === 'L7E').problemMsg, {
      key: 'Los tubos {prefix} tienen {expected} dígitos; este tiene {digits}',
      vars: { prefix: 'FS', expected: 8, digits: 7 },
    });
  } finally {
    store.close();
  }
});

test('lookAt of a bulk proposal: the checks on its rows, their notes and the key cells left empty, each said once', async () => {
  const { store, call } = await setup();
  try {
    const out = await call('propose_changes', {
      reason: 'Larvas preservadas: Sex NOT_COLLECTED',
      bulk: [{ sheet: 'Insectary_data', filters: { LIFESTAGE: '4th instar larva' }, set: { Sex: 'NOT_COLLECTED' } }],
    });
    assert.ok(!out.error, out.error);
    assert.equal(out.rows, 60);
    assert.equal(out.bulk[0].preview.length, 5);
    const look = out.lookAt;
    // In the proposal's order (L7E is row 9, L9E row 11).
    assert.deepEqual(look.issues, [
      { kind: 'id_format', field: 'Tube_1_id', problem: 'Los tubos FS tienen 8 dígitos; este tiene 7', rows: ['L7E: FS5848967'] },
      { kind: 'missing_sample', field: 'CAM_ID', problem: 'Preservada (Preserved_Dead_Alive Dead) sin CAM_ID', rows: ['L9E'] },
    ]);
    // The same note on two rows comes once; a long one clipped.
    assert.deepEqual(look.notes[0], { field: 'Notes_Insectary_data', note: '16/9/2026 AA: Larvae 4th instar', rows: ['L3E', 'L4E'] });
    assert.equal(look.notes[1].rows[0], 'L5E');
    assert.ok(look.notes[1].note.length <= 120 && look.notes[1].note.endsWith('…'));
    // L12E lacks the Tube_2_id the other 59 have; L9E's CAM_ID is already among the issues.
    assert.deepEqual(look.gaps, [{ field: 'Tube_2_id', filledIn: '59 of 60 rows', rows: ['L12E'] }]);
    assert.ok(!look.rest, 'nothing left out');

    // get_proposal's summary says the same; update_proposal only about the rows it changed.
    const got = await call('get_proposal', { proposalId: out.proposalId });
    assert.deepEqual(got.lookAt, look);
    const i7 = got.labels.indexOf('L7E');
    const i20 = got.labels.indexOf('L20E');
    const revised = await call('update_proposal', { proposalId: out.proposalId, rows: [{ index: i7, values: { Sex: 'NA' } }] });
    assert.deepEqual(revised.lookAt, { issues: [look.issues.find(i => i.kind === 'id_format')] });
    const quiet = await call('update_proposal', { proposalId: out.proposalId, rows: [{ index: i20, values: { Sex: 'NA' } }] });
    assert.equal(quiet.lookAt, undefined, 'nothing to say about L20E');
    // A cell the proposal writes: it deals with it already.
    const fixed = await call('update_proposal', { proposalId: out.proposalId, rows: [{ index: i7, values: { Tube_1_id: 'FS58489607' } }] });
    assert.ok(!fixed.error, fixed.error);
    assert.equal(fixed.lookAt, undefined);
  } finally {
    store.close();
  }
});

test('lookAt caps its lists and says how to see the rest', async () => {
  // 40 rows with notes, all different; the last 12 preserved without CAM_ID.
  const rows = Array.from({ length: 40 }, (_, i) => ({
    row: i + 2,
    values: {
      Insectary_ID: `N${i}E`,
      SPECIES: 'Oleria onega',
      Notes_Insectary_data: `nota ${i}`,
      ...(i >= 28 ? { Death_cause: 'Killed_Preserved', Tube_1_id: `FS${70000000 + i}` } : {}),
    },
  }));
  const store = new Store({ localMode: true }, { sheets: new LocalSheets({ Insectary_data: rows }) });
  await store.sync({ sheets: ['Insectary_data'] });
  try {
    const changes = rows.map(r => {
      const record = store.getRecordBySheetRow('Insectary_data', r.row);
      return { sheet: 'Insectary_data', recordId: record.id, label: record.label, values: { Sex: 'NOT_COLLECTED' } };
    });
    const look = lookAt(store, changes);
    assert.equal(look.notes.length, LOOK_ITEMS);
    assert.equal(look.notesMore, 40 - LOOK_ITEMS);
    // The same issue on 12 rows: the first ten named.
    const [missing] = look.issues;
    assert.equal(missing.problem, 'Preservada (Death_cause Killed_Preserved) sin CAM_ID');
    assert.equal(missing.rows.length, 10);
    assert.equal(missing.moreRows, 2);
    assert.match(look.rest, /check_data with a row's recordId.*`query`/);
    // Only some rows (those a revision changed).
    assert.deepEqual(lookAt(store, changes, { only: new Set([0]) }), { notes: [{ field: 'Notes_Insectary_data', note: 'nota 0', rows: ['N0E'] }] });
  } finally {
    store.close();
  }
});

test('the proposals table shows what the checks say about a cell while it keeps the sheet value', async () => {
  const { store, assistant, call, user } = await setup();
  try {
    allIssues(store);
    const out = await call('propose_changes', {
      reason: 'Sex',
      changes: [
        { sheet: 'Insectary_data', id: 'L7E', values: { Sex: 'NOT_COLLECTED' } },
        { sheet: 'Insectary_data', id: 'L8E', values: { Sex: 'NOT_COLLECTED' } },
      ],
    });
    const listed = await assistant.handle({ method: 'GET', path: '/api/chat/proposals', user, query: {} });
    const view = listed.body.proposals.find(p => p.id === out.proposalId);
    const l7e = view.changes.find(c => c.label === 'L7E');
    // As an index into the proposal's hints (sent once), and its column shown.
    assert.deepEqual(Object.keys(l7e.checks), ['Tube_1_id']);
    assert.equal(view.hintTable[l7e.checks.Tube_1_id[0]].msg.key, 'Los tubos {prefix} tienen {expected} dígitos; este tiene {digits}');
    assert.ok(view.fields.includes('Tube_1_id'));
    assert.ok(!view.changes.find(c => c.label === 'L8E').checks);
  } finally {
    store.close();
  }
});

test('match_notebook answers with lookAt about the rows of its page', async () => {
  const { store, call } = await setup();
  try {
    const out = await call('match_notebook', {
      kind: 'emergence',
      year: 2026,
      lines: [
        { raw: 'L3E ♀', values: { Insectary_ID: 'L3E', Sex: 'female' } },
        { raw: 'L7E ♂', values: { Insectary_ID: 'L7E', Sex: 'male' } },
      ],
    });
    assert.ok(out.proposalId, JSON.stringify(out).slice(0, 300));
    assert.deepEqual(out.lookAt.notes, [{ field: 'Notes_Insectary_data', note: '16/9/2026 AA: Larvae 4th instar', rows: ['L3E'] }]);
    assert.deepEqual(out.lookAt.issues.map(i => [i.kind, i.rows]), [['id_format', ['L7E: FS5848967']]]);
  } finally {
    store.close();
  }
});
