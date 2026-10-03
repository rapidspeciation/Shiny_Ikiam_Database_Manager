import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant } from '../server/assistant.mjs';

// The review table reads like the sheet: rows by row number, and the sheet's rows between them that the
// proposal leaves alone shown greyed for context (never written), so nothing is hidden in between.

test('a pending proposal shows its rows in sheet order, with the rows in between for context', async () => {
  const sheets = new LocalSheets({
    Insectary_data: [2, 3, 4, 5, 6].map(row => ({ row, values: { Insectary_ID: `K${row}B`, Sex: 'NA' } })),
  });
  const store = new Store({ localMode: true }, { sheets });
  try {
    await store.sync({ sheets: ['Insectary_data'] });
    const assistant = createAssistant({ store, config: {} });
    store.db
      .prepare(
        "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u1','franz','Franz','editor','s','h',1,'2026-01-01')",
      )
      .run();
    store.db
      .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
      .run(createHash('sha256').update('franz-token').digest('hex'), 'u1');
    const at = row => store.getRecordBySheetRow('Insectary_data', row).id;
    const out = await assistant.mcp(
      { authorization: 'Bearer franz-token' },
      {
        jsonrpc: '2.0',
        id: 1,
        method: 'tools/call',
        params: {
          name: 'propose_changes',
          arguments: {
            reason: 'Emergidos',
            changes: [
              { recordId: at(6), values: { Sex: 'female' } },
              { recordId: at(3), values: { Sex: 'male' } },
            ],
          },
        },
      },
    );
    assert.ok(!out.body.result.isError, out.body.result.content[0].text);
    const user = { id: 'u1', username: 'franz', displayName: 'Franz', role: 'editor' };
    const list = await assistant.handle({ method: 'GET', path: '/api/chat/proposals', body: {}, user, query: { all: '1' } });
    const shown = list.body.proposals[0].changes;
    assert.deepEqual(
      shown.map(c => [c.row, !!c.gap]),
      [
        [3, false],
        [4, true],
        [5, true],
        [6, false],
      ],
    );
    const gap = shown[1];
    assert.equal(gap.context, true);
    assert.deepEqual(gap.values, {});
    // A pending proposal sends a row's sheet values once, as rowValues (as for its own rows).
    assert.equal(gap.rowValues.Sex, 'NA');
    // The proposal's own rows keep their index (what applying and editing refer to).
    assert.deepEqual(
      shown.filter(c => !c.gap).map(c => c.index),
      [1, 0],
    );
  } finally {
    store.close?.();
  }
});
