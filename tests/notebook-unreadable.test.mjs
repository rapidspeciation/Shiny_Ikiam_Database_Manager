import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { moduleMap, parseDateText } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { unfilledUnreadable } from '../server/doubts.mjs';

// Cells match_notebook could not read (null): in the proposal as empty cells
// marked unreadable, for the person to fill; never written unless filled.

const d = text => parseDateText(text);

async function setup() {
  const species = moduleMap.get('Insectary_data').fields.find(f => f.key === 'SPECIES').column;
  const sheets = new LocalSheets({
    Insectary_stocks: [{ row: 2, values: { 'CLUTCH NUMBER': 848, SPECIES: 'Mechanitis messenoides messenoides' } }],
    Insectary_data: [
      { row: 2, values: { Insectary_ID: '5VB', 'CLUTCH NUMBER': 848, Wild_Reared: 'Reared', Stock_of_origin: 'messenoides', Sex: 'female', Intro2Insectary_date: d('2025-08-04') } },
      { row: 3, values: { Insectary_ID: '8VD', 'CLUTCH NUMBER': 848, Wild_Reared: 'Reared', Stock_of_origin: 'messenoides' } },
      { row: 4, values: { Insectary_ID: '9VD', 'CLUTCH NUMBER': 848, Wild_Reared: 'Reared', Stock_of_origin: 'messenoides', Sex: 'male' } },
    ],
  });
  for (const row of [2, 3, 4])
    sheets.rows.get('Insectary_data').find(r => r.row === row).cells[species] = {
      userEnteredValue: { formulaValue: '=XLOOKUP(C2,Insectary_stocks!A:A,Insectary_stocks!C:C,"")' },
      effectiveValue: { stringValue: 'Mechanitis messenoides messenoides' },
    };
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_stocks', 'Insectary_data'] });
  const assistant = createAssistant({ store, config: {} });
  store.db
    .prepare(
      "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u-franz','franz','Franz Chandi','editor','s','h',1,'2026-01-01')",
    )
    .run();
  const token = 'token-for-franz';
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update(token).digest('hex'), 'u-franz');
  const mcp = (method, params) => assistant.mcp({ authorization: `Bearer ${token}` }, { jsonrpc: '2.0', id: 1, method, params });
  const call = async (name, args) => JSON.parse((await mcp('tools/call', { name, arguments: args })).body.result.content[0].text);
  const user = { id: 'u-franz', username: 'franz', displayName: 'Franz Chandi', role: 'editor' };
  const http = (method, path, body) => assistant.handle({ method, path, body, user, query: {} });
  return { store, mcp, call, http };
}

const PAGE = {
  kind: 'emergence',
  year: 2025,
  title: 'emergidos con celdas ilegibles',
  lines: [
    // A death date to write, a CAM cut off by the photo's edge (part of it read), an entry date the sheet already has.
    {
      raw: '5VB ♀ 848 ?/8 dead 7/8 disappeared CAM0765..',
      values: { Insectary_ID: '5VB', Sex: 'female', 'CLUTCH NUMBER': '848', Intro2Insectary_date: null, Death_date: '7/8', Death_cause: 'Disappearance', CAM_ID: null },
      reasons: { CAM_ID: 'cut off by the photo edge', Intro2Insectary_date: 'smudged' },
      alternatives: { CAM_ID: ['CAM0765??'] },
    },
    // Only the sex is news, and it cannot be read: the row still shows, to fill.
    { raw: '8VD ? 848', values: { Insectary_ID: '8VD', Sex: null, 'CLUTCH NUMBER': '848' }, reasons: { Sex: 'smudged' } },
    // The species is the sheet's formula: nothing to fill.
    { raw: '9VD ?? ♂ 848', values: { Insectary_ID: '9VD', SPECIES: null, Sex: 'male', 'CLUTCH NUMBER': '848' } },
    // Not in the sheet: stays in the not-found list.
    { raw: '7ZZ ?', values: { Insectary_ID: '7ZZ', Sex: null } },
  ],
};

test('match_notebook puts unreadable cells in the proposal for the person to fill, with why and what was read', async () => {
  const { store, mcp, call, http } = await setup();
  try {
    const tool = (await mcp('tools/list')).body.result.tools.find(t => t.name === 'match_notebook');
    assert.match(tool.inputSchema.properties.lines.items.properties.values.description, /null: unreadable/);
    assert.match(tool.inputSchema.properties.lines.items.properties.reasons.description, /unreadable/);

    const out = await call('match_notebook', PAGE);
    const line = n => out.lines.find(l => l.n === n);
    assert.equal(out.counts.unreadableToFill, 2);
    assert.deepEqual(line(1).unreadable, {
      Intro2Insectary_date: { reason: 'smudged', toFill: false, sheet: '2025-08-04' },
      CAM_ID: { reason: 'cut off by the photo edge', partial: ['CAM0765??'], toFill: true },
    });
    assert.ok(!line(1).doubtful, 'an unreadable cell is not a doubtful one');
    assert.deepEqual(line(2).unreadable, { Sex: { reason: 'smudged', toFill: true } });
    assert.equal(line(2).inProposal, true, 'a row whose only news is unreadable still shows');
    assert.deepEqual(line(3).unreadable, { SPECIES: { toFill: false, sheet: 'Mechanitis messenoides messenoides' } }, 'the sheet has it (a formula)');
    assert.equal(line(4).status, 'missing');
    assert.equal(line(4).inProposal, false);

    // The table: the cells have no value, their reason and partial reading go with them.
    const listed = (await http('GET', '/api/chat/proposals')).body.proposals[0];
    // The whole page shows: the rows to write, then the line already in the sheet and the one not found.
    assert.deepEqual(listed.changes.map(c => [c.label, c.index >= 0, c.page?.status ?? null]), [
      ['5VB', true, null],
      ['8VD', true, null],
      ['9VD', false, 'match'],
      ['7ZZ', false, 'missing'],
    ]);
    const [first, second] = listed.changes;
    assert.ok(!('CAM_ID' in first.values));
    assert.deepEqual(first.unreadable, { CAM_ID: { reason: 'cut off by the photo edge', partial: ['CAM0765??'] } });
    assert.deepEqual(second.values, {});
    assert.deepEqual(second.unreadable, { Sex: { reason: 'smudged' } });
    assert.ok(listed.fields.includes('CAM_ID') && listed.fields.includes('Sex'), 'their columns show');

    const table = await call('get_proposal', { proposalId: out.proposalId });
    assert.deepEqual(table.attention.find(r => r.index === 1).unreadable, { Sex: { reason: 'smudged', filled: false } });

    // The person types the sex in the table: from then on it is written like any other cell.
    const typed = await http('POST', `/api/chat/proposals/${out.proposalId}/edit`, { cells: [{ key: second.key, field: 'Sex', value: 'male' }] });
    assert.equal(typed.status, 200);
    assert.equal(typed.body.proposal.changes[1].values.Sex, 'male');
    assert.deepEqual(unfilledUnreadable(typed.body.proposal.changes).map(u => [u.label, u.field]), [['5VB', 'CAM_ID']]);

    // Matched again: the person's value stays.
    const again = await call('match_notebook', { ...PAGE, replaceProposalId: out.proposalId });
    assert.equal(again.proposalId, out.proposalId);
    const kept = (await http('GET', '/api/chat/proposals')).body.proposals[0];
    assert.equal(kept.changes.find(c => c.label === '8VD').values.Sex, 'male');

    // Applied: the empty unreadable cell is left as the sheet has it, and listed for the assistant to ask.
    const applied = await call('apply_proposal', { proposalId: out.proposalId });
    assert.equal(applied.status, 'applied', JSON.stringify(applied));
    assert.deepEqual(applied.unreadable, [
      { index: 0, label: '5VB', row: 2, field: 'CAM_ID', reason: 'cut off by the photo edge', partial: ['CAM0765??'] },
    ]);
    assert.match(applied.unreadableNote, /left as the sheet has them/);
    const row5 = store.getRecordBySheetRow('Insectary_data', 2).values;
    assert.equal(row5.Death_date, d('2025-08-07'));
    assert.equal(row5.CAM_ID ?? null, null, 'never written');
    assert.equal(store.getRecordBySheetRow('Insectary_data', 3).values.Sex, 'male');
  } finally {
    store.close();
  }
});

test('a proposal of unreadable cells only writes nothing until someone fills them', async () => {
  const { store, call } = await setup();
  try {
    const out = await call('match_notebook', { kind: 'emergence', year: 2025, lines: [PAGE.lines[1]] });
    assert.ok(out.proposalId);
    const refused = await call('apply_proposal', { proposalId: out.proposalId });
    assert.match(refused.error ?? '', /only unreadable cells/, JSON.stringify(refused));
    assert.deepEqual(refused.unreadable, [{ index: 0, label: '8VD', row: 3, field: 'Sex', reason: 'smudged' }]);

    // The person tells the assistant: its value fills the cell.
    const updated = await call('update_proposal', { proposalId: out.proposalId, rows: [{ index: 0, values: { Sex: 'female' } }] });
    assert.deepEqual(updated.changed[0].unreadable, { Sex: { reason: 'smudged', filled: true } });
    const applied = await call('apply_proposal', { proposalId: out.proposalId });
    assert.equal(applied.status, 'applied', JSON.stringify(applied));
    assert.ok(!applied.unreadable);
    assert.equal(store.getRecordBySheetRow('Insectary_data', 3).values.Sex, 'female');
  } finally {
    store.close();
  }
});

test('doubtful and unreadable together: the refusal lists both', async () => {
  const { store, call } = await setup();
  try {
    const out = await call('match_notebook', {
      kind: 'emergence',
      year: 2025,
      lines: [
        { raw: '5VB dead 7/8 cam?', values: { Insectary_ID: '5VB', Death_date: '7/8', CAM_ID: null }, confidence: { Death_date: 0.5 }, alternatives: { Death_date: ['1/8'] } },
      ],
    });
    const refused = await call('apply_proposal', { proposalId: out.proposalId });
    assert.match(refused.error, /doubtful cells not checked/);
    assert.deepEqual(refused.unreadable.map(u => u.field), ['CAM_ID']);
    assert.match(refused.unreadableNote, /ask the person/);
  } finally {
    store.close();
  }
});
