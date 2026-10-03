// Cells of a proposal edited in the sheet after the assistant read them
// (server/sheet-edits.mjs): told apart in the table with what was read and what
// the sheet has now, the sheet's value kept unless the person chooses the
// proposal's, a choice that holds only while the sheet keeps that value.
import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { moduleMap } from '../server/schema.mjs';
import { nextInSeries } from '../server/premade.mjs';

const user = { id: 'u1', username: 'franz', displayName: 'Franz', role: 'editor' };

async function fixture(seed, { evaluate, config = {} } = {}) {
  const sheets = new LocalSheets(seed, evaluate ? { evaluate } : {});
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: Object.keys(seed) });
  const assistant = createAssistant({ store, config: { proposalWaitMs: 300, ...config } });
  store.db
    .prepare(
      "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u1','franz','Franz','editor','s','h',1,'2026-01-01')",
    )
    .run();
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update('franz-token').digest('hex'), 'u1');
  const call = async (name, args) => {
    const out = await assistant.mcp(
      { authorization: 'Bearer franz-token' },
      { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } },
    );
    return JSON.parse(out.body.result.content[0].text);
  };
  const listed = async id =>
    (await assistant.handle({ method: 'GET', path: '/api/chat/proposals', body: {}, user, query: { all: '1', only: id } })).body.proposals[0];
  const post = (id, action, body) => assistant.handle({ method: 'POST', path: `/api/chat/proposals/${id}/${action}`, body, user });
  let n = 0;
  const apply = (id, body = {}) => post(id, 'apply', { requestId: `apply-${++n}-${id}`, ...body });
  /** Someone types in Google Sheets; the sheet hook reports the row. */
  const typeInSheet = async (sheet, row, changes) => {
    await sheets.externalEdit(sheet, row, changes);
    await store.refreshRows(sheet, [row]);
  };
  const close = () => store.close?.();
  return { sheets, store, assistant, call, listed, post, apply, typeInSheet, close };
}

const insectary = () => ({
  Insectary_data: [
    { row: 2, values: { Insectary_ID: 'A1A', SPECIES: 'Oleria onega', Sex: 'male', Notes_Insectary_data: '' } },
    { row: 3, values: { Insectary_ID: 'A2A', SPECIES: 'Oleria onega', Sex: 'female' } },
  ],
});

test('a cell edited in the sheet after the proposal is told apart, and its value is kept when applying', async () => {
  const f = await fixture(insectary());
  try {
    const a1 = f.store.getRecordBySheetRow('Insectary_data', 2);
    const { proposalId } = await f.call('propose_changes', {
      reason: 'Página 12',
      changes: [{ recordId: a1.id, values: { Sex: 'female', Notes_Insectary_data: 'ala rota' } }],
    });
    assert.equal((await f.listed(proposalId)).changes[0].sheetChanged, undefined);
    await f.typeInSheet('Insectary_data', 2, { Sex: 'NA' });
    const row = (await f.listed(proposalId)).changes[0];
    assert.deepEqual(Object.keys(row.sheetChanged), ['Sex']);
    assert.equal(row.sheetChanged.Sex.read, 'male');
    assert.equal(row.sheetChanged.Sex.now, 'NA');
    // Seen by the sheet's edit trigger, nobody named.
    assert.equal(row.sheetChanged.Sex.source, 'sheets');
    assert.ok(row.sheetChanged.Sex.at);
    assert.equal(row.sheetChanged.Sex.use, undefined);
    // The assistant reads it too.
    const got = await f.call('get_proposal', { proposalId });
    assert.equal(got.rows[0].sheetChanged.Sex.now, 'NA');
    assert.match(got.rows[0].sheetChanged.Sex.applying, /keeps the sheet/);

    const out = await f.apply(proposalId);
    assert.equal(out.status, 200, JSON.stringify(out.body));
    assert.deepEqual(
      out.body.keptFromSheet.map(k => [k.field, k.read, k.now]),
      [['Sex', 'male', 'NA']],
    );
    const after = f.store.getRecord(a1.id).values;
    assert.equal(after.Sex, 'NA');
    assert.match(after.Notes_Insectary_data, /ala rota/);
  } finally {
    f.close();
  }
});

test("the person chooses the proposal's value: it goes over the sheet's; edited again after that, applying waits", async () => {
  const f = await fixture(insectary());
  try {
    const a1 = f.store.getRecordBySheetRow('Insectary_data', 2);
    const { proposalId } = await f.call('propose_changes', { changes: [{ recordId: a1.id, values: { Sex: 'female' } }] });
    await f.typeInSheet('Insectary_data', 2, { Sex: 'NA' });
    const choose = await f.post(proposalId, 'edit', { cells: [], sheet: [{ key: a1.id, field: 'Sex', use: 'proposal' }] });
    assert.equal(choose.status, 200);
    const chosen = choose.body.proposal.changes[0].sheetChanged.Sex;
    assert.equal(chosen.use, 'proposal');
    assert.equal(chosen.decidedBy, 'Franz');

    // Someone edits it again: the choice no longer holds, and the save does not guess.
    await f.typeInSheet('Insectary_data', 2, { Sex: 'unknown' });
    const again = (await f.listed(proposalId)).changes[0].sheetChanged.Sex;
    assert.equal(again.again, true);
    assert.equal(again.use, undefined);
    const refused = await f.apply(proposalId);
    assert.equal(refused.status, 409);
    assert.equal(refused.body.error.code, 'sheet_changed_again');
    assert.equal((await f.listed(proposalId)).status, 'pending');
    assert.equal(f.store.getRecord(a1.id).values.Sex, 'unknown');

    // Chosen again: written over what the sheet has now.
    await f.post(proposalId, 'edit', { cells: [], sheet: [{ key: a1.id, field: 'Sex', use: 'proposal' }] });
    const out = await f.apply(proposalId);
    assert.equal(out.status, 200, JSON.stringify(out.body));
    assert.equal(out.body.keptFromSheet, undefined);
    assert.equal(f.store.getRecord(a1.id).values.Sex, 'female');
  } finally {
    f.close();
  }
});

test('a value the person types in such a cell is a choice; the assistant setting it reads the sheet again', async () => {
  const f = await fixture(insectary());
  try {
    const [a1, a2] = [2, 3].map(r => f.store.getRecordBySheetRow('Insectary_data', r));
    const { proposalId } = await f.call('propose_changes', {
      changes: [
        { recordId: a1.id, values: { Sex: 'female' } },
        { recordId: a2.id, values: { Sex: 'male' } },
      ],
    });
    await f.typeInSheet('Insectary_data', 2, { Sex: 'NA' });
    await f.typeInSheet('Insectary_data', 3, { Sex: 'NA' });
    // Typed by the person: theirs goes over the sheet's.
    await f.post(proposalId, 'edit', { cells: [{ key: a1.id, field: 'Sex', value: 'male', before: 'female' }] });
    const typed = (await f.listed(proposalId)).changes.find(c => c.recordId === a1.id);
    assert.equal(typed.sheetChanged.Sex.use, 'proposal');
    // Set again by the assistant after it looked: a fresh proposal for that cell, over what the sheet has now.
    const updated = await f.call('update_proposal', { proposalId, rows: [{ index: 1, values: { Sex: 'female' } }] });
    assert.ok(!updated.error, JSON.stringify(updated));
    assert.equal(updated.rows[1].sheetChanged, undefined);
    const shown = (await f.listed(proposalId)).changes.find(c => c.recordId === a2.id);
    assert.equal(shown.sheetChanged, undefined);
    // An edit of another cell of the row does not hide the sheet's edit (what was read stays).
    await f.post(proposalId, 'edit', { cells: [{ key: a1.id, field: 'Notes_Insectary_data', value: 'x', before: null }] });
    assert.equal((await f.listed(proposalId)).changes.find(c => c.recordId === a1.id).sheetChanged.Sex.use, 'proposal');
    const out = await f.apply(proposalId);
    assert.equal(out.status, 200, JSON.stringify(out.body));
    assert.equal(f.store.getRecord(a1.id).values.Sex, 'male');
    assert.equal(f.store.getRecord(a2.id).values.Sex, 'female');
  } finally {
    f.close();
  }
});

test('counts kept as sums compare by their formula: the same sum is no edit, another is', async () => {
  const mod = moduleMap.get('Insectary_stocks');
  const col = key => mod.fields.find(x => x.key === key).column;
  const cells = [];
  cells[col('CLUTCH NUMBER')] = { userEnteredValue: { numberValue: 121 } };
  cells[col('NUMBER OF EGGS')] = { userEnteredValue: { formulaValue: '=41+36' }, effectiveValue: { numberValue: 77 } };
  cells[col('NUMBER OF LARVAE')] = { userEnteredValue: { formulaValue: '=50' }, effectiveValue: { numberValue: 50 } };
  const f = await fixture({ Insectary_stocks: [{ row: 2, cells }] });
  try {
    const clutch = f.store.getRecordBySheetRow('Insectary_stocks', 2);
    const { proposalId } = await f.call('propose_changes', {
      changes: [{ recordId: clutch.id, values: { 'NUMBER OF EGGS': '=41+30', 'NUMBER OF LARVAE': '=50+2' } }],
    });
    // Read again with nothing changed (its total, not its formula, is what a row holds as value): no edit.
    await f.typeInSheet('Insectary_stocks', 2, { 'CLUTCH NUMBER': 121 });
    assert.equal((await f.listed(proposalId)).changes[0].sheetChanged, undefined);
    await f.typeInSheet('Insectary_stocks', 2, { 'NUMBER OF EGGS': { formula: '=41+37' } });
    const row = (await f.listed(proposalId)).changes[0];
    assert.deepEqual(Object.keys(row.sheetChanged), ['NUMBER OF EGGS']);
    assert.deepEqual([row.sheetChanged['NUMBER OF EGGS'].read, row.sheetChanged['NUMBER OF EGGS'].now], ['=41+36', '=41+37']);
    const out = await f.apply(proposalId);
    assert.equal(out.status, 200, JSON.stringify(out.body));
    const after = f.store.getRecord(clutch.id).formulas;
    assert.equal(after['NUMBER OF EGGS'], '=41+37');
    assert.equal(after['NUMBER OF LARVAE'], '=50+2');
  } finally {
    f.close();
  }
});

test('a species typed over its formula compares by what the formula gave', async () => {
  const mod = moduleMap.get('Insectary_data');
  const col = key => mod.fields.find(x => x.key === key).column;
  const cells = [];
  cells[col('Insectary_ID')] = { userEnteredValue: { stringValue: 'A1A' } };
  cells[col('SPECIES')] = { userEnteredValue: { formulaValue: '="Oleria onega"' }, effectiveValue: { stringValue: 'Oleria onega' } };
  cells[col('Sex')] = { userEnteredValue: { stringValue: 'male' } };
  const f = await fixture({ Insectary_data: [{ row: 2, cells }] });
  try {
    const a1 = f.store.getRecordBySheetRow('Insectary_data', 2);
    const { proposalId } = await f.call('propose_changes', { changes: [{ recordId: a1.id, values: { SPECIES: 'Oleria baizana' } }] });
    assert.equal((await f.listed(proposalId)).changes[0].sheetChanged, undefined);
    // The formula now gives another species (its clutch changed).
    f.sheets.rows.get('Insectary_data').find(r => r.row === 2).cells[col('SPECIES')] = {
      userEnteredValue: { formulaValue: '="Oleria onega"' },
      effectiveValue: { stringValue: 'Oleria gunilla' },
    };
    await f.store.sync({ sheets: ['Insectary_data'], force: true });
    const row = (await f.listed(proposalId)).changes[0];
    assert.deepEqual([row.sheetChanged.SPECIES.read, row.sheetChanged.SPECIES.now], ['Oleria onega', 'Oleria gunilla']);
  } finally {
    f.close();
  }
});

test("a sheet edit the app had not read yet: the save refuses, the proposal stays pending and shows it", async () => {
  const f = await fixture(insectary());
  try {
    const a1 = f.store.getRecordBySheetRow('Insectary_data', 2);
    const { proposalId } = await f.call('propose_changes', { changes: [{ recordId: a1.id, values: { Sex: 'female' } }] });
    // Typed in Google Sheets; no hook report, no sync yet.
    await f.sheets.externalEdit('Insectary_data', 2, { Sex: 'NA' });
    const out = await f.apply(proposalId);
    assert.equal(out.status, 409);
    assert.equal(out.body.error.code, 'BATCH_CONFLICT');
    const listed = await f.listed(proposalId);
    assert.equal(listed.status, 'pending');
    assert.equal(listed.changes[0].sheetChanged.Sex.now, 'NA');
    // Applying again keeps the sheet's value: nothing is left to write.
    const again = await f.apply(proposalId);
    assert.equal(again.status, 400);
    assert.equal(again.body.error.code, 'nothing_selected');
  } finally {
    f.close();
  }
});

test("a sheet edit to a proposal's row wakes the person's list (with a new stamp)", async () => {
  const f = await fixture(insectary());
  try {
    const a1 = f.store.getRecordBySheetRow('Insectary_data', 2);
    const { proposalId } = await f.call('propose_changes', { changes: [{ recordId: a1.id, values: { Sex: 'female' } }] });
    const ask = query => f.assistant.handle({ method: 'GET', path: '/api/chat/proposals', body: {}, user, query });
    const first = (await ask({})).body;
    // Nothing new: the wait runs out and answers "unchanged".
    const idle = (await ask({ wait: '1', revision: first.revision, stamp: first.stamp })).body;
    assert.equal(idle.unchanged, true);
    const waiting = ask({ wait: '1', revision: idle.revision, stamp: idle.stamp });
    await f.typeInSheet('Insectary_data', 2, { Sex: 'NA' });
    const woken = (await waiting).body;
    assert.ok(!woken.unchanged);
    const shown = woken.proposals.find(p => p.id === proposalId);
    assert.notEqual(shown.sheetStamp, first.proposals.find(p => p.id === proposalId).sheetStamp);
    assert.equal(shown.changes[0].sheetChanged.Sex.now, 'NA');
  } finally {
    f.close();
  }
});

test('a new row whose pre-made row was typed into meanwhile is flagged, and left out when applying', async () => {
  const mod = moduleMap.get('Insectary_data');
  const col = key => mod.fields.find(x => x.key === key).column;
  const idFormula = row => `=NEXT(A${row - 1})`;
  const evaluate = (formula, { value }) => {
    const ref = /^=NEXT\(A(\d+)\)$/.exec(formula);
    return ref ? (nextInSeries(value(Number(ref[1]), 0)) ?? '#VALUE!') : undefined;
  };
  const rows = [];
  let id = 'Q0D';
  for (let row = 2; row <= 6; row++) {
    const cells = [];
    cells[col('Insectary_ID')] =
      row === 2 ? { userEnteredValue: { stringValue: id } } : { userEnteredValue: { formulaValue: idFormula(row) }, effectiveValue: { stringValue: id } };
    if (row === 2) cells[col('Sex')] = { userEnteredValue: { stringValue: 'male' } };
    rows.push({ row, cells });
    id = nextInSeries(id);
  }
  const f = await fixture({ Insectary_data: rows }, { evaluate });
  try {
    const premade = f.store.getRecordBySheetRow('Insectary_data', 4);
    const target = premade.values.Insectary_ID;
    const { proposalId } = await f.call('propose_changes', {
      newRows: [
        { sheet: 'Insectary_data', values: { Insectary_ID: target, Sex: 'female' } },
        { sheet: 'Insectary_data', values: { Insectary_ID: f.store.getRecordBySheetRow('Insectary_data', 5).values.Insectary_ID, Sex: 'male' } },
      ],
    });
    assert.equal((await f.listed(proposalId)).changes[0].rowTaken, undefined);
    // Someone used that pre-made row in Google Sheets.
    await f.typeInSheet('Insectary_data', 4, { Sex: 'male' });
    const listed = await f.listed(proposalId);
    assert.equal(listed.changes[0].rowTaken.row, 4);
    assert.equal(listed.changes[1].rowTaken, undefined);
    const out = await f.apply(proposalId);
    assert.equal(out.status, 200, JSON.stringify(out.body));
    assert.deepEqual(out.body.applied, [1]);
    assert.equal(out.body.keptFromSheet[0].rowTaken.row, 4);
    assert.equal(f.store.getRecordBySheetRow('Insectary_data', 4).values.Sex, 'male');
    assert.equal(f.store.getRecordBySheetRow('Insectary_data', 5).values.Sex, 'male');
  } finally {
    f.close();
  }
});

test("“Tell the assistant” sends to the proposal's own T3 chat when it is idle; otherwise the page copies it", async () => {
  const thread = '11111111-1111-4111-8111-111111111111';
  const sessions = { [thread]: { projectId: 'p-franz', runtimeMode: 'full-access', interactionMode: 'default', busy: false } };
  const sent = [];
  const t3Chats = {
    available: true,
    session: id => sessions[id] ?? null,
    projectsOf: name => (name === 'franz' ? ['p-franz'] : []),
    threads: () => new Map(),
    chatsOf: () => [],
    threadOfToolUse: () => null,
    onlyRunning: () => null,
    findProposals: async () => new Map(),
    open: () => null,
  };
  const t3Fetch = async (url, options) => {
    sent.push({ url, auth: options.headers.authorization, body: JSON.parse(options.body) });
    return { ok: true, json: async () => ({ sequence: 1 }) };
  };
  const { mkdtemp, writeFile, rm } = await import('node:fs/promises');
  const { join } = await import('node:path');
  const { tmpdir } = await import('node:os');
  const dir = await mkdtemp(join(tmpdir(), 't3tell-'));
  await writeFile(join(dir, 'token'), 'admin-token\n');
  const f = await fixture(insectary(), { config: { t3Chats, t3Fetch, t3: { local: 'http://t3.test', tokenFile: join(dir, 'token') } } });
  try {
    const a1 = f.store.getRecordBySheetRow('Insectary_data', 2);
    const { proposalId } = await f.call('propose_changes', { changes: [{ recordId: a1.id, values: { Sex: 'female' } }] });
    const text = 'La hoja cambió: Sex (A1A). Revísalo y actualiza la propuesta.';
    // Not linked to a chat yet.
    assert.deepEqual((await f.post(proposalId, 'tell', { text })).body, { sent: false, reason: 'no_chat', chat: null });
    f.store.db.prepare('UPDATE ai_proposals SET t3_thread = ? WHERE id = ?').run(thread, proposalId);
    sessions[thread].busy = true;
    assert.equal((await f.post(proposalId, 'tell', { text })).body.reason, 'busy');
    sessions[thread].busy = false;
    const out = await f.post(proposalId, 'tell', { text });
    assert.deepEqual(out.body, { sent: true, chat: thread });
    assert.equal(sent.length, 1);
    assert.equal(sent[0].url, 'http://t3.test/api/orchestration/dispatch');
    assert.equal(sent[0].auth, 'Bearer admin-token');
    assert.equal(sent[0].body.type, 'thread.turn.start');
    assert.equal(sent[0].body.threadId, thread);
    assert.equal(sent[0].body.message.text, text);
    // Another person's chat is never written to.
    sessions[thread].projectId = 'p-other';
    assert.equal((await f.post(proposalId, 'tell', { text })).body.reason, 'no_chat');
    assert.equal((await f.post(proposalId, 'tell', { text: '' })).status, 400);
  } finally {
    f.close();
    await rm(dir, { recursive: true, force: true });
  }
});
