// The server tests' guard against using a model CLI, an AI host or T3 (tests/helpers/no-ai-core.mjs says what it refuses):
// `node --test --import ./tests/helpers/no-ai.mjs tests/*.test.mjs` (package.json, scripts/deploy.sh). In a test file's
// process an attempt fails the test it happened in; one outside a test (or in a node process the test started, reported
// here) fails the file at exit. tests/no-ai-guard.test.mjs fails when a run does not load this file.
import { installNoAiGuard } from './no-ai-core.mjs';

const first = !process.env.NOAI_GUARD_PARENT;
const guard = installNoAiGuard({ owner: first });
if (first && (process.env.NODE_TEST_CONTEXT || /\.test\.[cm]?js$/.test(process.argv[1] ?? ''))) {
  const { afterEach, beforeEach } = await import('node:test');
  const file = process.argv[1] ?? '?';
  beforeEach(t => guard.testing(`${file} › ${t.name}`));
  afterEach(() => {
    const fresh = guard.take();
    if (fresh.length) throw new Error(`${fresh.join('\n')}\nTests use fakes, never a model CLI, an AI host or T3 (tests/helpers/no-ai-core.mjs).`);
  });
}
