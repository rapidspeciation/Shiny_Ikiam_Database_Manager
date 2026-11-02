import { vi } from 'vitest'
import { locale } from './lib/i18n'

// A test's time limit: vitest's 5 s is often taken by a file's first mount alone (the components' compile) when the
// deploy runs the server tests, the type check and the build beside these on a busy machine. No test here waits on
// real time for what it checks, so a longer limit only stops a stuck test later.
vi.setConfig({ testTimeout: 60_000, hookTimeout: 60_000 })

// Tests assert the Spanish texts (the keys of the translations); i18n.test.ts sets the language it needs.
locale.value = 'es'
