import { test } from 'node:test';
import assert from 'node:assert/strict';
import { progressUpdates } from '../tools/whatsapp/t3-progress.mjs';
const row = (ordinal, type, payload) => ({ ordinal, type, payload_json: JSON.stringify(payload) });
test('personal progress follows updates to the same assistant row', () => {
  const seen = new Map();
  const first = row(1, 'assistant_message', { text: 'Checking the files' });
  assert.deepEqual(progressUpdates([first], seen, { personal: true }), [{ kind: 'assistant', text: 'Checking the files' }]);
  assert.deepEqual(progressUpdates([first], seen, { personal: true }), []);
  assert.equal(progressUpdates([row(1, 'assistant_message', { text: 'Checking the files now' })], seen, { personal: true })[0].text, 'Checking the files now');
});
test('groups receive tools only and reasoning never crosses the bridge', () => {
  const rows = [row(1, 'reasoning', { text: 'private' }), row(2, 'assistant_message', { text: 'public' }), row(3, 'dynamic_tool', { toolName: 'Read', input: 'secret path', title: 'sensitive title', output: 'secret result' })];
  assert.deepEqual(progressUpdates(rows, new Map()), [{ tool: 'Read', title: '' }]);
  const personal = progressUpdates(rows, new Map(), { personal: true });
  assert.equal(personal.length, 2);
  assert.ok(!JSON.stringify(personal).includes('private'));
  assert.ok(!JSON.stringify(personal).includes('secret'));
});
test('malformed rows are ignored, tool events are deduplicated, text is bounded', () => {
  const seen = new Map();
  const tool = row(3, 'command_execution', { input: 'secret' });
  assert.equal(progressUpdates([tool], seen).length, 1);
  assert.deepEqual(progressUpdates([tool, { ordinal: 4, type: 'dynamic_tool', payload_json: '{' }], seen), []);
  assert.equal(progressUpdates([row(5, 'assistant_message', { text: 'a'.repeat(900) })], seen, { personal: true })[0].text.length, 600);
});
