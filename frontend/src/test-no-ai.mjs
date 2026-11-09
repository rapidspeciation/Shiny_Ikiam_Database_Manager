// The frontend tests' guard against using a model CLI, an AI host (Gemini's API, which the app calls from the browser,
// among them) or T3: tests/helpers/no-ai-core.mjs, the same as the server tests'. An attempt fails the test it happened in.
import { afterEach, beforeEach } from 'vitest'
import { installNoAiGuard } from '../../tests/helpers/no-ai-core.mjs'

const guard = installNoAiGuard({ owner: true })
beforeEach(ctx => guard.testing(`${ctx.task.file?.filepath ?? '?'} › ${ctx.task.name}`))
afterEach(() => {
  const fresh = guard.take()
  if (fresh.length) throw new Error(`${fresh.join('\n')}\nTests use fakes, never a model CLI, an AI host or T3 (tests/helpers/no-ai-core.mjs).`)
})
