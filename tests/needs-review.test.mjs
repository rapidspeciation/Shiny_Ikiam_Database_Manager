// A proposal left in needs_review (a save cut by a restart) is compared with the sheet after
// each sync: all its cells there → applied; else it stays, with the cells that differ.
import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { moduleMap } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';

test('needs_review: applied when the sheet holds every cell, else the cells that differ', async () => {
  const sheets = new LocalSheets({
    Insectary_data: [2, 3].map(row => ({ row, values: { Insectary_ID: `K${row}B`, Sex: 'NA' } })),
  });
  const store = new Store({ localMode: true }, { sheets });
  try {
    await store.sync({ sheets: ['Insectary_data'] });
    const assistant = createAssistant({ store, config: {} });
    store.db
      .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u1','franz','Franz','editor','s','h',1,'2026-01-01')")
      .run();
    store.db.prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')").run(createHash('sha256').update('franz-token').digest('hex'), 'u1');
    const call = async (name, args) =>
      JSON.parse(
        (await assistant.mcp({ authorization: 'Bearer franz-token' }, { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } }))
          .body.result.content[0].text,
      );
    const at = row => store.getRecordBySheetRow('Insectary_data', row).id;
    const out = await call('propose_changes', {
      reason: 'Sexos',
      changes: [
        { recordId: at(2), values: { Sex: 'male', Death_date: '2026-10-05' } },
        { recordId: at(3), values: { Sex: 'female' } },
      ],
    });
    // A restart cut its save.
    store.db.prepare("UPDATE ai_proposals SET status = 'needs_review' WHERE id = ?").run(out.proposalId);
    // Someone typed part of it by hand in the sheet.
    const sex = moduleMap.get('Insectary_data').fields.find(f => f.key === 'Sex').column;
    const death = moduleMap.get('Insectary_data').fields.find(f => f.key === 'Death_date').column;
    const row = n => sheets.rows.get('Insectary_data').find(r => r.row === n);
    row(2).cells[sex] = { userEnteredValue: { stringValue: 'male' } };
    row(2).cells[death] = { userEnteredValue: { numberValue: 46300 } };
    await store.sync({ sheets: ['Insectary_data'], force: true });
    let p = await call('get_proposal', { proposalId: out.proposalId });
    assert.equal(p.status, 'needs_review');
    assert.equal(p.sheetCheck.matched, 2);
    assert.deepEqual(
      p.sheetCheck.differ.map(d => [d.label, d.field, d.proposal, d.sheet]),
      [['K3B', 'Sex', 'female', 'NA']],
    );
    // The rest typed too: applied on its own.
    row(3).cells[sex] = { userEnteredValue: { stringValue: 'female' } };
    await store.sync({ sheets: ['Insectary_data'], force: true });
    p = await call('get_proposal', { proposalId: out.proposalId });
    assert.equal(p.status, 'applied');
    assistant.close();
  } finally {
    store.close();
  }
});
