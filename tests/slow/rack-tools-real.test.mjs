// The slow suite: tests/rack-tools.test.mjs again, on Franz's rack photo read for real (tests/helpers/rack-reading.mjs).
process.env.ITHOMIINI_TEST_REAL_RACK = '1';
await import('../rack-tools.test.mjs');
