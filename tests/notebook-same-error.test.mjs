// A page that corrects a typing slip (FS5848967 typed for FS50848967) makes
// match_notebook read the same column of the rows around the page, not on the
// photo, with the same correction: added to the proposal as doubtful cells.

import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { applySlip, sameErrorRows, typingSlip } from '../server/notebook.mjs';

test('typingSlip names one slip between the sheet and the page, and applySlip repeats it', () => {
  assert.deepEqual(typingSlip('FS5848994', 'FS50848994'), { kind: 'missing', at: 3, char: '0', anchor: 'FS5', length: 9 });
  assert.equal(typingSlip('FS508484994', 'FS50848994').kind, 'extra');
  assert.equal(typingSlip('FS508848994', 'FS50848994').kind, 'doubled');
  assert.deepEqual(typingSlip('FS05848994', 'FS50848994'), { kind: 'swap', at: 2, anchor: 'FS05', length: 10 });
  assert.deepEqual(typingSlip('FR50848994', 'FS50848994'), { kind: 'prefix', from: 'FR', to: 'FS' });
  assert.deepEqual(typingSlip('CAM12345', 'CAM012345'), { kind: 'padding', letters: 'CAM', from: 5, to: 6 });
  // Not one slip: two digits apart, another number, text.
  assert.equal(typingSlip('FS50848961', 'FS50848999'), null);
  assert.equal(typingSlip('FS50848961', 'FS63886694'), null);
  assert.equal(typingSlip('NA', 'FS50848961'), null);
  assert.equal(typingSlip('FS50848961', 'FS50848961'), null);

  assert.equal(applySlip(typingSlip('FS5848994', 'FS50848994'), 'FS5848961'), 'FS50848961');
  assert.equal(applySlip(typingSlip('FS5848994', 'FS50848994'), 'FS6388669'), null, 'another start');
  assert.equal(applySlip(typingSlip('FS5848994', 'FS50848994'), 'FS50848961'), null, 'already right');
  assert.equal(applySlip(typingSlip('FS508848994', 'FS50848994'), 'FS508848961'), 'FS50848961');
  assert.equal(applySlip(typingSlip('FS05848994', 'FS50848994'), 'FS05848961'), 'FS50848961');
  assert.equal(applySlip(typingSlip('FR50848994', 'FS50848994'), 'FR50848961'), 'FS50848961');
  assert.equal(applySlip(typingSlip('CAM12345', 'CAM012345'), 'CAM12340'), 'CAM012340');
  assert.equal(applySlip(typingSlip('CAM0012345', 'CAM012345'), 'CAM0012340'), 'CAM012340');
});

/** A column of tubes by row: { row: value }. */
const columnOf = (values, field = 'Tube_1_id') =>
  Object.entries(values).map(([row, value]) => ({ recordId: `r${row}`, row: Number(row), label: `ID${row}`, field, value }));
// The workbook: FS + 8 digits is the usual form, FS + 7 a slip.
const counts = { 'FS:8': 5000, 'FS:7': 40, 'FR:8': 3, 'CAM:6': 3000, 'CAM:5': 10 };
const familyCount = (field, family) => counts[family] ?? 0;
const fix = (row, wrong, right, line = 1) => ({ field: 'Tube_1_id', line, row, wrong, right });

test('a missing digit: the rows above the page typed with the same slip, and only those', () => {
  const rows = columnOf({
    100: 'FS50848960', // right
    101: 'FS5848961',
    102: 'FS5848962',
    103: 'FS5848963',
    104: 'FS63886694', // another rack, right
    105: 'FS6388669', // another slip: not the page's
    106: 'FS5848999', // its correction is used elsewhere
    170: 'FS5848970', // far from the page and from the others
  });
  const out = sameErrorRows({
    fixes: [fix(110, 'FS5848967', 'FS50848967'), fix(111, 'FS5848968', 'FS50848968', 2)],
    rows,
    familyCount,
    taken: (field, value) => value === 'FS50848999',
  });
  assert.deepEqual(
    out.map(o => [o.row, o.value, o.suggested, o.slip, o.line]),
    [
      [101, 'FS5848961', 'FS50848961', 'missing', 1],
      [102, 'FS5848962', 'FS50848962', 'missing', 1],
      [103, 'FS5848963', 'FS50848963', 'missing', 1],
    ],
  );
  assert.match(out[0].reason.text, /Mismo error que la línea 1 de la página \(FS5848967 → FS50848967: falta un 0\); esta fila no está en la foto/);
  assert.deepEqual(out[0].reason.msg.vars.what, { key: 'falta un {char}', vars: { char: '0' } });
});

test('a run typed with the slip is followed past the 50 rows, row by row', () => {
  const values = {};
  for (let row = 140; row <= 175; row++) values[row] = `FS5848${row + 760}`; // FS5848900 … FS5848935
  values[260] = 'FS5848936'; // a slip far away, after a gap
  const out = sameErrorRows({ fixes: [fix(100, 'FS5848899', 'FS50848899')], rows: columnOf(values), familyCount });
  // 140–150 are within 50 rows; 151–175 follow the run; 260 is too far.
  assert.equal(out.length, 36);
  assert.equal(out.at(-1).row, 175);
  assert.equal(out.at(-1).suggested, 'FS50848935');
});

test('the corrected value must be free, join the run and fix a rare form', () => {
  const page = [fix(110, 'FS5848967', 'FS50848967')];
  // Used elsewhere in the workbook, or twice here.
  assert.equal(sameErrorRows({ fixes: page, rows: columnOf({ 109: 'FS5848966' }), familyCount, taken: () => true }).length, 0);
  assert.equal(sameErrorRows({ fixes: page, rows: columnOf({ 109: 'FS5848967' }), familyCount }).length, 0, 'the page already gives FS50848967');
  // Out of the run: FS5841234 → FS50841234 is far from every tube around.
  assert.equal(sameErrorRows({ fixes: page, rows: columnOf({ 109: 'FS5841234' }), familyCount }).length, 0);
  // FS + 7 digits common in the workbook: not a slip there.
  assert.equal(sameErrorRows({ fixes: page, rows: columnOf({ 109: 'FS5848966' }), familyCount: (field, f) => (f === 'FS:8' ? 100 : 50) }).length, 0);
  // Another column is not read.
  assert.equal(sameErrorRows({ fixes: page, rows: columnOf({ 109: 'FS5848966' }, 'Tube_2_id'), familyCount }).length, 0);
});

test('a prefix: fixed where the wrong letters are rare, left where they are a rack of their own', () => {
  const page = [fix(110, 'FR50848967', 'FS50848967')];
  const rows = columnOf({ 105: 'FR50848962', 106: 'FS50848963' });
  assert.deepEqual(
    sameErrorRows({ fixes: page, rows, familyCount }).map(o => [o.row, o.suggested, o.slip]),
    [[105, 'FS50848962', 'prefix']],
  );
  assert.equal(sameErrorRows({ fixes: page, rows, familyCount: (field, f) => ({ 'FS:8': 5000, 'FR:8': 800 })[f] ?? 0 }).length, 0);
});

test('a swap: fixed when the value as typed is out of the run, never a value that fits it', () => {
  const page = [fix(110, 'FS05848967', 'FS50848967')];
  const rows = columnOf({ 104: 'FS50848960', 105: 'FS05848962', 106: 'FS05123456', 107: 'FS50848961' });
  assert.deepEqual(
    sameErrorRows({ fixes: page, rows, familyCount }).map(o => [o.row, o.suggested, o.slip]),
    [[105, 'FS50848962', 'swap']],
  );
  // A swap whose typed value is itself in the run (FS50848976 among FS508489xx) stays.
  const late = [fix(110, 'FS50848967', 'FS50848976')];
  assert.equal(sameErrorRows({ fixes: late, rows: columnOf({ 104: 'FS50848960', 105: 'FS50848962' }), familyCount }).length, 0);
});

test('padding and doubled characters: CAMs and tubes', () => {
  const cams = sameErrorRows({
    fixes: [{ field: 'CAM_ID', line: 4, row: 50, wrong: 'CAM12345', right: 'CAM012345' }],
    rows: columnOf({ 48: 'CAM12344', 49: 'CAM012343', 52: 'CAM012347' }, 'CAM_ID'),
    familyCount,
  });
  assert.deepEqual(cams.map(o => [o.row, o.suggested, o.slip]), [[48, 'CAM012344', 'padding']]);
  const doubled = sameErrorRows({ fixes: [fix(110, 'FS508848967', 'FS50848967')], rows: columnOf({ 111: 'FS508848968' }), familyCount: (field, f) => ({ 'FS:8': 5000, 'FS:9': 2 })[f] ?? 0 });
  assert.deepEqual(doubled.map(o => [o.row, o.suggested, o.slip]), [[111, 'FS50848968', 'doubled']]);
});

test('match_notebook adds the rows around the page with the same slip to its proposal, as doubtful cells', async () => {
  // 260 butterflies of another rack (FS + 8: the usual form), then the case of 1 Oct 2026:
  // Q4C–Q9C and the page's R0C–R2C typed without the 0 of FS508489xx.
  const rows = [];
  for (let i = 0; i < 260; i++) rows.push({ row: 2 + i, values: { Insectary_ID: `F${i}X`, Sex: 'female', Tube_1_id: `FS63${880000 + i}` } });
  const at = 262;
  const ids = ['P0C', 'Q4C', 'Q5C', 'Q6C', 'Q7C', 'Q8C', 'Q9C', 'R0C', 'R1C', 'R2C', 'R3C', 'R4C', 'R5C'];
  const tubes = ['FS50848960', 'FS5848961', 'FS5848962', 'FS5848963', 'FS5848964', 'FS5848965', 'FS5848966', 'FS5848967', 'FS5848968', 'FS5848969', 'FS50848970', 'FS5848971', 'FS63886999'];
  ids.forEach((id, i) => rows.push({ row: at + i, values: { Insectary_ID: id, Sex: 'male', Tube_1_id: tubes[i] } }));
  // R4C's correction is already another butterfly's tube.
  rows.push({ row: at + 20, values: { Insectary_ID: 'Z9Z', Sex: 'male', Tube_1_id: 'FS50848971' } });
  const store = new Store({ localMode: true }, { sheets: new LocalSheets({ Insectary_data: rows }) });
  await store.sync({ sheets: ['Insectary_data'] });
  const assistant = createAssistant({ store, config: {} });
  store.db
    .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u-franz','franz','Franz Chandi','editor','s','h',1,'2026-01-01')")
    .run();
  store.db.prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')").run(createHash('sha256').update('tok').digest('hex'), 'u-franz');
  const call = async (name, args) =>
    JSON.parse(
      (await assistant.mcp({ authorization: 'Bearer tok' }, { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } })).body.result
        .content[0].text,
    );
  const user = { id: 'u-franz', username: 'franz', displayName: 'Franz Chandi', role: 'editor' };
  const page = {
    kind: 'emergence',
    year: 2026,
    title: 'emergidos R0C–R2C',
    lines: ['R0C', 'R1C', 'R2C'].map((id, i) => ({ raw: `${id} FS5084896${7 + i}`, values: { Insectary_ID: id, Tube_1_id: `FS5084896${7 + i}` } })),
  };
  try {
    const out = await call('match_notebook', page);
    assert.equal(out.counts.rowsInProposal, 3);
    // The page's tube is the row's first tube typed without its 0, not a second tube.
    assert.deepEqual(out.lines[0].differs, { Tube_1_id: { sheet: 'FS5848967', notebook: 'FS50848967' } });
    assert.deepEqual(
      out.sameErrorNearby.map(s => [s.row, s.id, s.value, s.suggested, s.inProposal]),
      ['Q4C', 'Q5C', 'Q6C', 'Q7C', 'Q8C', 'Q9C'].map((id, i) => [at + 1 + i, id, `FS584896${1 + i}`, `FS5084896${1 + i}`, true]),
    );
    assert.match(out.sameErrorNearby[0].reason, /Mismo error que la línea 1 de la página \(FS5848967 → FS50848967: falta un 0\)/);

    const proposal = (await assistant.handle({ method: 'GET', path: '/api/chat/proposals', body: {}, user, query: {} })).body.proposals[0];
    assert.equal(proposal.id, out.proposalId);
    const nearby = proposal.changes.filter(c => c.sameErrorAs);
    assert.deepEqual(
      nearby.map(c => [c.label, c.values.Tube_1_id]),
      ['Q4C', 'Q5C', 'Q6C', 'Q7C', 'Q8C', 'Q9C'].map((id, i) => [id, `FS5084896${1 + i}`]),
    );
    // After the page's rows, each a doubtful cell with the value as typed to keep, and why.
    assert.ok(proposal.changes.findIndex(c => c.sameErrorAs) > proposal.changes.findIndex(c => c.label === 'R2C'));
    assert.equal(nearby[0].doubts.Tube_1_id.confidence, 0.6);
    assert.deepEqual(nearby[0].doubts.Tube_1_id.alternatives, ['FS5848961']);
    assert.equal(nearby[0].doubts.Tube_1_id.reasonMsg.key, 'Mismo error que la línea {line} de la página ({wrong} → {right}: {what}); esta fila no está en la foto');
    assert.match(nearby[0].note, /^Mismo error que en la página, no está en esta foto: Tube_1_id FS5848961 → FS50848961 \(como la línea 1\)/);

    // The same page matched again in another proposal: those rows are listed with the first one, not proposed twice.
    const again = await call('match_notebook', page);
    assert.ok(again.sameErrorNearby.every(s => s.alreadyIn === out.proposalId && !s.inProposal));
  } finally {
    store.close?.();
  }
});
