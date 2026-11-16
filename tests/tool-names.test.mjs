import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant, mcpTools } from '../server/assistant.mjs';
import { allIssues } from '../server/checks.mjs';
import { setVerdicts } from '../server/review.mjs';
import { signIn } from './helpers/assistant.mjs';

// The tools' names: the earlier names of tools renamed or merged on 9 Oct 2026 (search_records, record_history,
// queue_wikiloc, check_data, list_suggested_edits, list_agreed_fixes, get_alerts) answered as the tools they became
// until 16 Nov 2026, when they were taken out: no chat had called one since at least 12 Oct.
const OLD_NAMES = ['search_records', 'record_history', 'queue_wikiloc', 'check_data', 'list_suggested_edits', 'list_agreed_fixes', 'get_alerts'];

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

test('tools/list gives the current names only', () => {
  const names = new Set(mcpTools().map(t => t.name));
  for (const old of OLD_NAMES) assert.ok(!names.has(old), `${old} is not listed`);
  for (const now of ['search_text', 'row_history', 'queue_walk', 'review_issues', 'review_suggestions']) assert.ok(names.has(now), now);
  // Loaded with every chat: the identifier and SQL readers stay, counting and free text are found on demand.
  const always = mcpTools()
    .filter(t => t._meta?.['anthropic/alwaysLoad'])
    .map(t => t.name);
  assert.ok(always.includes('query') && always.includes('find_records'));
  assert.ok(!always.includes('count_records') && !always.includes('search_text'));
});

test('an old name is an unknown tool now', async () => {
  const { store, call } = await setup();
  try {
    for (const old of OLD_NAMES) assert.equal((await call(old, {})).error, 'Unknown tool', old);
  } finally {
    store.close();
  }
});

test('review_issues: the agreed fixes (show "agreed") and the alerts (show "alerts"), counted in the overview', async () => {
  const { store, call } = await setup();
  try {
    const issue = allIssues(store).issues.find(i => i.kind === 'repeat' && i.recordId);
    assert.ok(issue, 'the fixture has a repeated CAM');
    setVerdicts(store, { ids: [issue.id], verdict: 'other', value: 'CAM000002' }, editor);

    const agreed = await call('review_issues', { show: 'agreed' });
    assert.equal(agreed.total, 1);
    assert.equal(agreed.fixes[0].issueId, issue.id);
    assert.deepEqual(Object.values(agreed.fixes[0].values), ['CAM000002']);
    assert.match(agreed.howTo, /propose_changes/);
    assert.equal((await call('review_issues', { show: 'agreed', kind: 'date_order' })).total, 0);

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
