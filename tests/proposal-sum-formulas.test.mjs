import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { moduleMap } from '../server/schema.mjs';

// The review table shows a count kept as a sum as its formula (=41+36), not the total it computes to (77):
// a person who types the sheet's sum back, or sets the cell back to the sheet, sees the sum, not a number.

test('a proposal shows the sheet sums of its counts as formulas', async () => {
  const sheets = new LocalSheets({
    Insectary_stocks: [
      { row: 2, values: { 'CLUTCH NUMBER': 120, SPECIES: 'Mechanitis polymnia proceriformis' } },
      { row: 3, cells: [] },
    ],
  });
  const mod = moduleMap.get('Insectary_stocks');
  const col = key => mod.fields.find(f => f.key === key).column;
  const cells = [];
  cells[col('CLUTCH NUMBER')] = { userEnteredValue: { numberValue: 121 } };
  cells[col('NUMBER OF EGGS')] = { userEnteredValue: { formulaValue: '=41+36' }, effectiveValue: { numberValue: 77 } };
  cells[col('NUMBER OF LARVAE')] = { userEnteredValue: { formulaValue: '=50' }, effectiveValue: { numberValue: 50 } };
  sheets.rows.get('Insectary_stocks').find(r => r.row === 3).cells = cells;
  const store = new Store({ localMode: true }, { sheets });
  try {
    await store.sync({ sheets: ['Insectary_stocks'] });
    const assistant = createAssistant({ store, config: {} });
    store.db
      .prepare(
        "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u1','franz','Franz','editor','s','h',1,'2026-01-01')",
      )
      .run();
    store.db
      .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
      .run(createHash('sha256').update('franz-token').digest('hex'), 'u1');
    const clutch = store.getRecordBySheetRow('Insectary_stocks', 3);
    const out = await assistant.mcp(
      { authorization: 'Bearer franz-token' },
      {
        jsonrpc: '2.0',
        id: 1,
        method: 'tools/call',
        params: { name: 'propose_changes', arguments: { reason: 'Posturas', changes: [{ recordId: clutch.id, values: { 'NUMBER OF EGGS': '=41+30' } }] } },
      },
    );
    assert.ok(!out.body.result.isError, out.body.result.content[0].text);
    const user = { id: 'u1', username: 'franz', displayName: 'Franz', role: 'editor' };
    const list = await assistant.handle({ method: 'GET', path: '/api/chat/proposals', body: {}, user, query: { all: '1' } });
    const shown = list.body.proposals[0].changes[0];
    assert.equal(shown.current['NUMBER OF EGGS'], '=41+36');
    assert.equal(shown.rowValues['NUMBER OF EGGS'], '=41+36');
    // A single number typed as =50 is a sum too: the same as the person would type it.
    assert.equal(shown.rowValues['NUMBER OF LARVAE'], '=50');
    assert.equal(shown.rowValues['CLUTCH NUMBER'], 121);
  } finally {
    store.close?.();
  }
});
