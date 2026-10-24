import test from 'node:test';
import assert from 'node:assert/strict';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { importBundle, readCuration } from '../server/envelope-import.mjs';

test('Fotos archive routes require an editor and show reviewable cards without calling AI', async () => {
  const store = new Store({ localMode: true }, { sheets: new LocalSheets({
    Collection_data: [{ row: 2, values: { CAM_ID: 'CAM000101', SPECIES: 'Oleria gunilla', Release_Collect: 'Collected_Preserved' } }],
  }) });
  await store.sync({ sheets: ['Collection_data'] });
  store.db.prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('lab','lab','Lab','editor','s','h',1,'2026-01-01')").run();
  const dir = fileURLToPath(new URL('./fixtures/envelope/', import.meta.url));
  importBundle(store.db, readCuration(dir, join(dir, 'manifest.csv')));
  const assistant = createAssistant({ store });
  const user = { id: 'lab', username: 'lab', displayName: 'Lab', role: 'editor' };
  const http = (method, path, body = {}, who = user, query = {}) => assistant.handle({ method, path, body, user: who, query });
  try {
    assert.equal((await http('GET', '/api/photo-curation/summary', {}, null)).status, 401);
    assert.equal((await http('GET', '/api/photo-curation/summary', {}, { ...user, role: 'viewer' })).status, 403);
    assert.equal((await http('POST', '/api/photo-curation/show', {}, { ...user, role: 'viewer' })).status, 403);
    const summary = await http('GET', '/api/photo-curation/summary');
    assert.equal(summary.status, 200);
    assert.ok(summary.body.total > 0);
    assert.equal((await http('GET', '/api/photo-curation/summary', {}, user, { tier: 'invalid' })).status, 400);
    const shown = await http('POST', '/api/photo-curation/show', { kind: 'camid', limit: 2 });
    assert.equal(shown.status, 201);
    assert.ok(shown.body.proposalId);
    const proposal = store.db.prepare('SELECT * FROM ai_proposals WHERE id=?').get(shown.body.proposalId);
    const card = JSON.parse(proposal.sync_json);
    assert.equal(card.type, 'curation');
    assert.ok(card.entries.every(e => !e.checked), 'a person selects what to apply');
    // An empty choice cannot write anything or close the pending card.
    const applied = await http('POST', `/api/chat/proposals/${proposal.id}/curation/apply`, {});
    assert.ok([200, 409].includes(applied.status));
    assert.equal(store.db.prepare("SELECT count(*) AS n FROM actions WHERE source='ai_approved'").get().n, 0);
    assert.notEqual(store.db.prepare('SELECT status FROM ai_proposals WHERE id=?').get(proposal.id).status, 'applied');
  } finally {
    assistant.close();
    store.close();
  }
});
