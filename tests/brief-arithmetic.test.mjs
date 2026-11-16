// The arithmetic rule every chat reads (16 Nov 2026: a reply added 10+13+9+12+4 in its head and wrote 41).
import test from 'node:test';
import assert from 'node:assert/strict';
import { readdirSync } from 'node:fs';
import { join } from 'node:path';
import { DatabaseSync } from 'node:sqlite';
import { composeBrief, GENERIC_PERSON, SERVER_PATHS } from '../server/brief.mjs';
import { runQuery, sqlProblem } from '../server/query-tool.mjs';

test('the app brief and the WhatsApp brief both say arithmetic is done with a tool, a worked-out total after its list', () => {
  const wa = join(import.meta.dirname, '..', 'assistant/voice/whatsapp-d3l');
  for (const voice of [null, readdirSync(wa).filter(f => f.endsWith('.md')).map(f => join(wa, f))]) {
    const brief = composeBrief(GENERIC_PERSON, { ...SERVER_PATHS, voice }).replace(/\s+/g, ' ');
    const where = voice ? 'WhatsApp' : 'app';
    assert.match(brief, /Any arithmetic, even a sum of two numbers, is done with a tool, never in your head/, where);
    assert.match(brief, /A total you work out goes after the list it sums/, where);
  }
});

test('the calculator the rule names: query takes the sum it shows (every chat has query, the group chat too)', () => {
  const sql = 'SELECT 10+13+9+12+4';
  assert.equal(sqlProblem(sql), null);
  const db = new DatabaseSync(':memory:');
  assert.deepEqual(runQuery(db, sql, 5).rows, [[48]]);
  db.close();
});
