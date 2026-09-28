import test from 'node:test';
import assert from 'node:assert/strict';
import { allowedModel, claudeConfig } from '../server/claude.mjs';

test('the assistant only runs Sonnet or Opus 5.5, never Fable or older models', () => {
  assert.equal(allowedModel('sonnet'), 'sonnet');
  assert.equal(allowedModel('opus'), 'opus');
  assert.equal(allowedModel('claude-opus-5-5'), 'claude-opus-5-5');
  assert.equal(allowedModel('fable'), 'sonnet');
  assert.equal(allowedModel('claude-fable-5-1'), 'sonnet');
  assert.equal(allowedModel('claude-sonnet-4-5'), 'sonnet');
  assert.equal(allowedModel(''), 'sonnet');
  assert.equal(claudeConfig({ ITHOMIINI_CLAUDE_MODEL: 'fable' }).model, 'sonnet');
});

test('"*" gives the shared Claude account to everyone', async () => {
  const { claudeAllowed } = await import('../server/claude.mjs');
  const base = { ITHOMIINI_CLAUDE_BIN: '/bin/claude', ITHOMIINI_CLAUDE_WORKSPACE: '/tmp/ws' };
  const everyone = claudeConfig({ ...base, ITHOMIINI_CLAUDE_USERS: '*' });
  assert.equal(claudeAllowed(everyone, { username: 'angel' }), true);
  const listed = claudeConfig({ ...base, ITHOMIINI_CLAUDE_USERS: 'franz.chandi' });
  assert.equal(claudeAllowed(listed, { username: 'angel' }), false);
  assert.equal(claudeAllowed(everyone, null), false);
});
