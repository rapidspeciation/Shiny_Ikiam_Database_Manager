// A proposal with rows of two sheets (a notebook page's Insectary_data rows and new Collection_data
// rows for its wild-caught butterflies) applied one sheet at a time: each sheet's table has its own
// «Aplicar». The first apply writes its sheet's rows as a save of its own; the proposal stays pending
// with the other sheet's rows (those written can no longer be edited) until they are applied too.
import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { moduleMap } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { signIn } from './helpers/assistant.mjs';

const col = (sheet, key) => moduleMap.get(sheet).fields.find(f => f.key === key).column;
const SENT = 'Collected_Sent2Insectary';

async function fixture() {
  const collection = [
    { row: 2, values: { Release_Collect: SENT, Insectary_ID: 'A0T', SPECIES: 'Oleria onega', Sex: 'male' } },
    { row: 3, values: {} },
    { row: 4, values: {} },
    { row: 5, values: {} },
    { row: 6, values: {} },
  ];
  const sheets = new LocalSheets({
    Collection_data: collection,
    Insectary_data: [
      { row: 2, values: { Insectary_ID: 'A0T', Wild_Reared: 'Wild-caught', SPECIES: 'Oleria onega' } },
      { row: 3, values: { Insectary_ID: 'A1T', Wild_Reared: 'Wild-caught', SPECIES: 'Oleria onega' } },
      { row: 4, values: { Insectary_ID: 'A2T', Wild_Reared: 'Wild-caught', SPECIES: 'Oleria onega' } },
      { row: 5, values: { Insectary_ID: 'A3T', Wild_Reared: 'Wild-caught', SPECIES: 'Oleria onega' } },
    ],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Collection_data', 'Insectary_data'] });
  const assistant = createAssistant({ store, config: {} });
  const { call, user: ana } = signIn(store, assistant, { id: 'u-ana', username: 'ana', displayName: 'Ana' });
  const listed = async id =>
    (await assistant.handle({ method: 'GET', path: '/api/chat/proposals', user: ana, query: { all: '1', only: id } })).body.proposals[0];
  const post = (path, body) => assistant.handle({ method: 'POST', path, user: ana, body });
  const cell = (sheet, row, key) => sheets.cell(sheet, row, col(sheet, key))?.userEnteredValue;
  const saves = () => store.db.prepare("SELECT count(*) AS n FROM actions WHERE status != 'failed'").get().n;
  return { store, assistant, call, listed, post, cell, saves };
}

/** The page's two Insectary_data rows and a new Collection_data row for a wild-caught butterfly. */
async function propose(call, store, wildId = 'A1T', sexes = ['female', 'male']) {
  const row = r => store.db.prepare("SELECT id FROM records WHERE sheet='Insectary_data' AND row_num=?").get(r).id;
  const proposed = await call('propose_changes', {
    reason: 'Cuaderno: página con silvestres',
    changes: [
      { recordId: row(2), values: { Sex: sexes[0] } },
      { recordId: row(3), values: { Sex: sexes[1] } },
    ],
    newRows: [{ sheet: 'Collection_data', values: { Release_Collect: SENT, Insectary_ID: wildId, SPECIES: 'Oleria onega', Sex: 'male' } }],
  });
  assert.ok(proposed.proposalId, JSON.stringify(proposed));
  return proposed.proposalId;
}

test('one sheet applied: its rows written as a save, the other sheet still pending, then the rest applies it', async () => {
  const { store, call, listed, post, cell, saves } = await fixture();
  try {
    const id = await propose(call, store);
    const before = saves();
    const first = await post(`/api/chat/proposals/${id}/apply`, { requestId: randomUUID(), sheets: ['Insectary_data'] });
    assert.equal(first.status, 200, JSON.stringify(first.body));
    assert.equal(first.body.status, 'pending');
    assert.deepEqual(first.body.left, ['Collection_data']);
    assert.equal(first.body.applied.length, 2);
    assert.equal(cell('Insectary_data', 2, 'Sex')?.stringValue, 'female');
    assert.equal(cell('Insectary_data', 3, 'Sex')?.stringValue, 'male');
    assert.equal(cell('Collection_data', 3, 'Insectary_ID'), undefined, 'the other sheet waits');
    assert.equal(saves(), before + 1);

    // The table: still pending, its Insectary_data rows marked written, one apply in its history.
    let view = await listed(id);
    assert.equal(view.status, 'pending');
    assert.deepEqual(
      view.changes.filter(c => c.applied).map(c => c.sheet),
      ['Insectary_data', 'Insectary_data'],
    );
    assert.ok(!view.changes.find(c => c.sheet === 'Collection_data').applied);
    assert.equal(view.applies.length, 1);
    assert.deepEqual(view.applies[0].sheets, ['Insectary_data']);
    assert.equal(view.applies[0].rows, 2);
    assert.equal(view.applies[0].status, 'applied');
    const got = await call('get_proposal', { proposalId: id });
    assert.deepEqual(got.writtenSheets, ['Insectary_data']);

    // Rows written stay as written; the rest can still be corrected.
    const at = sheet => view.changes.find(c => c.sheet === sheet).index;
    const edit = await call('update_proposal', { proposalId: id, rows: [{ index: at('Insectary_data'), values: { Sex: 'male' } }] });
    assert.match(JSON.stringify(edit), /Already written to the sheet/);
    const fixed = await call('update_proposal', { proposalId: id, rows: [{ index: at('Collection_data'), values: { Sex: 'female' } }] });
    assert.ok(!fixed.error, JSON.stringify(fixed));
    // The same sheet again: nothing left there.
    const again = await post(`/api/chat/proposals/${id}/apply`, { requestId: randomUUID(), sheets: ['Insectary_data'] });
    assert.equal(again.body.error.code, 'nothing_selected');
    assert.equal((await listed(id)).status, 'pending');

    // The rest (the assistant's tool, by sheet): written as a second save, the proposal applied.
    const second = await call('apply_proposal', { proposalId: id, sheet: 'Collection_data' });
    assert.equal(second.status, 'applied', JSON.stringify(second));
    assert.equal(second.rows, 1);
    assert.equal(cell('Collection_data', 3, 'Insectary_ID')?.stringValue, 'A1T');
    assert.equal(cell('Collection_data', 3, 'Sex')?.stringValue, 'female');
    assert.equal(cell('Insectary_data', 2, 'Sex')?.stringValue, 'female', 'written once, as it was');
    assert.equal(saves(), before + 2);
    view = await listed(id);
    assert.equal(view.status, 'applied');
    assert.equal(view.applied.length, 3);
    assert.deepEqual(
      view.applies.map(a => [a.sheets, a.rows, a.status]),
      [
        [['Insectary_data'], 2, 'applied'],
        [['Collection_data'], 1, 'applied'],
      ],
    );
    assert.ok(view.changes.find(c => c.create).recordId, 'the new row has its sheet row');
  } finally {
    store.close();
  }
});

test('«Aplicar todo» after one sheet writes only the rest; discarding the rest keeps what was written', async () => {
  const { store, call, listed, post, cell } = await fixture();
  try {
    const id = await propose(call, store);
    await post(`/api/chat/proposals/${id}/apply`, { requestId: randomUUID(), sheets: ['Insectary_data'] });
    const all = await post(`/api/chat/proposals/${id}/apply`, { requestId: randomUUID() });
    assert.equal(all.body.status, 'applied', JSON.stringify(all.body));
    const view = await listed(id);
    assert.deepEqual(all.body.applied, [view.changes.find(c => c.create).index], 'the rows written before are not written again');
    assert.equal(cell('Collection_data', 3, 'Insectary_ID')?.stringValue, 'A1T');

    const other = await propose(call, store, 'A2T', ['male', 'female']);
    await post(`/api/chat/proposals/${other}/apply`, { requestId: randomUUID(), sheets: ['Insectary_data'] });
    const discarded = await post(`/api/chat/proposals/${other}/discard`, {});
    assert.equal(discarded.body.status, 'applied', 'what was written stays applied; the rest is left out');
    assert.equal((await listed(other)).applies.length, 1);
    // A sheet the proposal does not have: refused.
    const third = await propose(call, store, 'A3T');
    const bad = await post(`/api/chat/proposals/${third}/apply`, { requestId: randomUUID(), sheets: ['Insectary_stocks'] });
    assert.equal(bad.body.error.code, 'unknown_sheet');
    assert.equal((await listed(third)).status, 'pending');
  } finally {
    store.close();
  }
});

test('one sheet refused as a whole stays to apply; a partly written one compared with the sheet goes back to pending', async () => {
  const { store, call, listed, post } = await fixture();
  try {
    const id = await propose(call, store);
    // Google takes no write (the save refused as a whole): nothing marked written, still pending.
    const originalApply = store.applyProposal.bind(store);
    store.applyProposal = async () => {
      throw Object.assign(new Error('Google rechazó la escritura'), { code: 'CELLS_PROTECTED', status: 409 });
    };
    const refused = await post(`/api/chat/proposals/${id}/apply`, { requestId: randomUUID(), sheets: ['Insectary_data'] });
    assert.equal(refused.body.error.details.status, 'pending');
    let view = await listed(id);
    assert.ok(!view.changes.some(c => c.applied));
    store.applyProposal = originalApply;

    // Sent but not confirmed (needs_review): once the sheet holds its rows, pending again for the rest.
    const uncertain = store.applyProposal.bind(store);
    store.applyProposal = async (...args) => ({ ...(await uncertain(...args)), status: 'uncertain' });
    const sent = await post(`/api/chat/proposals/${id}/apply`, { requestId: randomUUID(), sheets: ['Insectary_data'] });
    assert.equal(sent.body.status, 'needs_review', JSON.stringify(sent.body));
    store.applyProposal = originalApply;
    await store.sync({ sheets: ['Insectary_data', 'Collection_data'] });
    view = await listed(id);
    assert.equal(view.status, 'pending', JSON.stringify(view.applies));
    assert.equal(view.applies.at(-1).status, 'applied');
    const rest = await post(`/api/chat/proposals/${id}/apply`, { requestId: randomUUID(), sheets: ['Collection_data'] });
    assert.equal(rest.body.status, 'applied', JSON.stringify(rest.body));
  } finally {
    store.close();
  }
});

test('someone else on the team finishes a handed-over chat: applies its proposal as themselves', async () => {
  const { store, assistant, call } = await fixture();
  const a0 = store.getRecordBySheetRow('Insectary_data', 2);
  const { proposalId } = await call('propose_changes', {
    reason: 'Página 12',
    changes: [{ recordId: a0.id, values: { Notes_Insectary_data: 'ala rota' } }],
  });
  const luis = { id: 'u-luis', username: 'luis', displayName: 'Luis', role: 'editor' };
  const as = who => body => assistant.handle({ method: 'POST', path: `/api/chat/proposals/${proposalId}/apply`, body, user: who });
  // Someone who only looks can't.
  assert.equal((await as({ ...luis, role: 'observer' })({ requestId: 'apply-observer-1' })).status, 404);
  const out = await as(luis)({ requestId: 'apply-luis-12345' });
  assert.equal(out.status, 200, JSON.stringify(out.body));
  // The note was written by the assistant for Ana (Ana's chat); the save is Luis's.
  assert.match(store.getRecord(a0.id).values.Notes_Insectary_data, /^\d+\/\d+\/\d+ A: ala rota$/);
  const actors = store.db.prepare("SELECT DISTINCT actor FROM actions WHERE request_id LIKE '%apply-luis-12345%'").all();
  assert.deepEqual(actors.map(a => a.actor), ['u-luis']);
  store.close();
});
