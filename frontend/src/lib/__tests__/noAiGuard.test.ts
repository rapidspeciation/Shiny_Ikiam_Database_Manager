// The guard of frontend/src/test-no-ai.mjs (tests/helpers/no-ai-core.mjs) is loaded and refuses an AI host, offline.
import { describe, expect, it } from 'vitest'

const guard = (globalThis as Record<symbol, { take(): string[] } | undefined>)[Symbol.for('ithomiini.noAiGuard')]

describe('no-AI guard', () => {
  it('is loaded by the setup files', () => {
    expect(guard, 'frontend/vite.config.ts setupFiles must list ./src/test-no-ai.mjs').toBeTruthy()
  })

  it.skipIf(!guard)("refuses Gemini's API and T3 before any lookup, and writes it down", async () => {
    await expect(fetch('https://generativelanguage.googleapis.com/v1beta/models')).rejects.toThrow()
    await expect(fetch('http://127.0.0.1:3774/')).rejects.toThrow()
    const got = guard!.take()
    expect(got).toHaveLength(2)
    expect(got[0]).toMatch(/refused connection to AI host generativelanguage\.googleapis\.com:443/)
    expect(got[1]).toMatch(/refused connection to T3 port 127\.0\.0\.1:3774/)
  })
})
