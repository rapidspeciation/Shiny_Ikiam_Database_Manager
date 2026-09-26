import test from 'node:test';
import assert from 'node:assert/strict';
import { scryptSync } from 'node:crypto';
import { passwordFields, validatePassword, verifyPassword } from '../server/auth.mjs';

test('new passwords accept 6 through 16 characters and verify correctly', () => {
  for (const password of ['abcdef', 'abcdefghijklmnop']) {
    const { salt, hash } = passwordFields(password);
    assert.equal(verifyPassword(password, { salt, password_hash: hash }), true);
  }
  for (const password of ['', 'abcde', 'abcdefghijklmnopq', null, 123456]) {
    assert.throws(() => validatePassword(password), {
      code: 'WEAK_PASSWORD', message: 'Password must have 6 to 16 characters',
    });
  }
});

test('existing passwords longer than 16 characters can still sign in', () => {
  const password = 'existing-long-password';
  const salt = 'legacy-test-salt';
  const user = { salt, password_hash: scryptSync(password, salt, 64).toString('hex') };
  assert.equal(verifyPassword(password, user), true);
  assert.equal(verifyPassword('wrong-password', user), false);
});
