import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { TOOL_ALIASES, createAssistant, currentTool, mcpTools } from '../server/assistant.mjs';
import { MAIN_TOOLS } from '../server/assistant-host.mjs';
import { allIssues } from '../server/checks.mjs';
import { setVerdicts } from '../server/review.mjs';
import { signIn } from './helpers/assistant.mjs';

// Renamed and merged tools: a chat that loaded the earlier list still calls them by their old
// names (TOOL_ALIASES), and gets what the tool they became answers.

const editor = { id: 'u-ana', username: 'ana', displayName: 'Ana', role: 'editor' };

async function setup() {
  const seed = {
    Insectary_data: [{ row: 2, values: { Insectary_ID: 'A0A', SPECIES: 'Oleria onega', Sex: 'female' } }],
    // One CAM on two rows: a `repeat` issue.
    Collection_data: [
      { row: 2, values: { CAM_ID: 'CAM000001', SPECIES: 'Oleria onega', Sex: 'female' } },
      { row: 3, values: { CAM_ID: 'CAM000001', SPECIES: 'Ithomia salapia', Sex: 'male' } },
    ],
  };
  const store = new Store({ localMode: true }, { sheets: new LocalSheets(seed) });
  await store.sync({ sheets: Object.keys(seed) });
  const assistant = createAssistant({ store, config: {} });
  return { store, assistant, ...signIn(store, assistant, editor) };
}

test('tools/list gives only the new names; every old name maps to a listed tool', () => {
  const names = new Set(mcpTools().map(t => t.name));
  for (const [old, alias] of Object.entries(TOOL_ALIASES)) {
    assert.ok(!names.has(old), `${old} is not listed`);
    assert.ok(names.has(alias.name), `${old} → ${alias.name} is listed`);
  }
  // Loaded with every chat: the identifier and SQL readers stay, counting and free text are found on demand.
  const always = mcpTools()
    .filter(t => t._meta?.['anthropic/alwaysLoad'])
    .map(t => t.name);
  assert.ok(always.includes('query') && always.includes('find_records'));
  assert.ok(!always.includes('count_records') && !always.includes('search_text'));
  // A renamed tool runs where it ran: the Wikiloc queue in the app's thread.
  assert.equal(currentTool('queue_wikiloc').name, 'queue_walk');
  assert.ok(MAIN_TOOLS.has(currentTool('queue_wikiloc').name));
  assert.deepEqual(currentTool('find_records', { a: 1 }), { name: 'find_records', args: { a: 1 } });
});

test('old names answer as the tools they became', async () => {
  const { store, call, raw } = await setup();
  try {
    const a0a = store.getRecordBySheetRow('Insectary_data', 2).id;
    const same = async (old, now, args = {}, nowArgs = args) => assert.equal(await raw(old, args), await raw(now, nowArgs), `${old} = ${now}`);
    await same('search_records', 'search_text', { query: 'Oleria' });
    await same('record_history', 'row_history', { recordId: a0a });
    await same('check_data', 'review_issues', { kind: 'repeat' });
    await same('list_suggested_edits', 'review_suggestions');
    await same('queue_wikiloc', 'queue_walk', { url: 'https://example.com/123' });
    assert.match((await call('queue_wikiloc', { url: 'https://example.com/123' })).error, /No Wikiloc trail link/);
    await same('get_alerts', 'review_issues', {}, { show: 'alerts' });
    assert.ok(Array.isArray((await call('get_alerts', {})).camPools));
  } finally {
    store.close();
  }
});

test('review_issues: the agreed fixes (show "agreed") and the alerts (show "alerts") as the removed tools gave them, counted in the overview', async () => {
  const { store, call, raw } = await setup();
  try {
    const issue = allIssues(store).issues.find(i => i.kind === 'repeat' && i.recordId);
    assert.ok(issue, 'the fixture has a repeated CAM');
    setVerdicts(store, { ids: [issue.id], verdict: 'other', value: 'CAM000002' }, editor);

    const agreed = await call('review_issues', { show: 'agreed' });
    assert.equal(agreed.total, 1);
    assert.equal(agreed.fixes[0].issueId, issue.id);
    assert.deepEqual(Object.values(agreed.fixes[0].values), ['CAM000002']);
    assert.match(agreed.howTo, /propose_changes/);
    // list_agreed_fixes took kind and limit: the same answer.
    assert.equal(await raw('list_agreed_fixes', { kind: 'repeat', limit: 5 }), await raw('review_issues', { show: 'agreed', kind: 'repeat', limit: 5 }));
    assert.equal((await call('list_agreed_fixes', { kind: 'date_order' })).total, 0);

    const alerts = await call('review_issues', { show: 'alerts' });
    assert.ok(['alerts', 'camPools', 'preserveRule', 'missingSamples'].every(k => k in alerts), Object.keys(alerts).join());
    assert.ok(!JSON.stringify(alerts).includes('Msg"'), 'the texts, without their translation keys');

    // The overview counts the tab's other parts; an answer about one kind leaves them (and the kinds) out.
    const overview = await call('review_issues', {});
    assert.deepEqual(overview.agreed, { fixes: 1, tasks: 0, needsValue: 0, stale: 0 });
    assert.equal(overview.alerts, alerts.alerts.length);
    assert.ok(overview.kinds.repeat);
    const one = await call('review_issues', { kind: 'repeat' });
    assert.equal(one.agreed, undefined);
    assert.equal(one.kinds, undefined);
    assert.ok(one.issues.some(i => i.id === issue.id));
    assert.match((await call('review_issues', { show: 'everything' })).error, /issues .*agreed or alerts/);
  } finally {
    store.close();
  }
});
