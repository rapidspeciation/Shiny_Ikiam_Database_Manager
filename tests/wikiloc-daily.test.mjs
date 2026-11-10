// The daily Wikiloc check's switch: WIKILOC_DAILY_CHECK in service.env (on by default, off in LOCAL_MODE, 0: off) and
// what /health says of it. The check itself (fake clock, fake worker): tests/monitoring.test.mjs.
import test from 'node:test';
import assert from 'node:assert/strict';
import { configFromEnv, createApp } from '../server/index.mjs';

test('WIKILOC_DAILY_CHECK: 0 off, 1 on, unset auto', () => {
  assert.equal(configFromEnv({ WIKILOC_DAILY_CHECK: '0' }).wikilocDaily, false);
  assert.equal(configFromEnv({ WIKILOC_DAILY_CHECK: '1' }).wikilocDaily, true);
  assert.equal(configFromEnv({}).wikilocDaily, 'auto');
});

test("/health says whether the daily check is on", async t => {
  for (const [wikilocDaily, expected] of [[true, 'on'], [false, 'off'], ['auto', 'off']]) {
    const app = await createApp({ databasePath: ':memory:', localMode: true, secureCookies: false, syncIntervalMs: 0, wikilocDaily });
    await app.ready;
    const { port } = await app.listen(0, '127.0.0.1');
    try {
      const health = await (await fetch(`http://127.0.0.1:${port}/health`)).json();
      assert.equal(health.wikiloc.daily, expected, String(wikilocDaily));
    } finally {
      await app.close();
    }
  }
});
