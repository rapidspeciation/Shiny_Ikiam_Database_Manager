import test from 'node:test';
import assert from 'node:assert/strict';
import { code128Symbols, code128Svg } from '../web/barcode.js';

test('labels encode Code 128B with weighted checksum, stop symbol, and quiet zones', () => {
  assert.deepEqual(code128Symbols('AB'), [104, 33, 34, 102, 106]);
  assert.match(code128Svg('AB'), /viewBox="0 0 77 64"/);
  assert.match(code128Svg('AB'), /<rect x="10"/);
  assert.throws(() => code128Svg('NÑD'));
  assert.throws(() => code128Svg(''));
});
