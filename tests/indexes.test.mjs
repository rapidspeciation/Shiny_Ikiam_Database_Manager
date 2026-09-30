import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';

const plan = (store, sql, ...args) =>
  store.db
    .prepare(`EXPLAIN QUERY PLAN ${sql}`)
    .all(...args)
    .map(r => r.detail)
    .join(' ');

test('sheet fingerprints and row counts read an index, not the rows', () => {
  const store = new Store({ localMode: true }, { sheets: new LocalSheets({}) });
  // grid.mjs tableRevision: every table poll, the summary's cache and the tables' ETag.
  assert.match(
    plan(store, 'SELECT count(*) n, max(updated_at) u, total(version) v, total(row_num) r FROM records WHERE sheet=? AND missing=0', 'Insectary_data'),
    /COVERING INDEX records_state/,
  );
  // Store.listModules and getStats (bootstrap).
  assert.match(plan(store, 'SELECT count(*) n FROM records WHERE sheet=? AND missing=0 AND observed=1', 'Insectary_data'), /COVERING INDEX/);
  assert.match(plan(store, 'SELECT sheet,count(*) count FROM records WHERE missing=0 AND observed=1 GROUP BY sheet'), /COVERING INDEX/);
  store.close();
});
