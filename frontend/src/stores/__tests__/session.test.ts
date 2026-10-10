import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { createPinia, setActivePinia } from 'pinia'
import { useSession } from '../session'

const user = { id: 'u1', username: 'ana', role: 'editor', displayName: 'Ana' }
const bootstrap = { user, csrf: 'c2', modules: [], settings: {}, sync: null }

/** The server: who is signed in (null: a visitor). Answers and the order asked are kept. */
function server(signedIn: boolean) {
  const asked: string[] = []
  const fetch = vi.fn(async (url: string) => {
    asked.push(url)
    if (url === 'api/auth/session')
      return new Response(JSON.stringify({ user: signedIn ? user : null, csrf: 'c1', setupRequired: false }))
    if (url === 'api/bootstrap')
      return signedIn
        ? new Response(JSON.stringify(bootstrap))
        : new Response(JSON.stringify({ error: { code: 'UNAUTHORIZED', message: 'no' } }), { status: 401 })
    throw new Error(url)
  })
  vi.stubGlobal('fetch', fetch)
  return asked
}

describe('session.init', () => {
  beforeEach(() => {
    setActivePinia(createPinia())
    localStorage.clear()
  })
  afterEach(() => vi.unstubAllGlobals())

  it('a visitor asks only for the session', async () => {
    const asked = server(false)
    const session = useSession()
    await session.init()
    expect(asked).toEqual(['api/auth/session'])
    expect(session.user).toBeNull()
    expect(session.ready).toBe(true)
  })

  it('the first sign-in on a device asks the bootstrap after the session, later starts ask both at once', async () => {
    let asked = server(true)
    await useSession().init()
    expect(asked).toEqual(['api/auth/session', 'api/bootstrap'])

    setActivePinia(createPinia())
    asked = server(true)
    const session = useSession()
    await session.init()
    expect(asked).toEqual(['api/bootstrap', 'api/auth/session'])
    expect(session.user?.username).toBe('ana')
  })

  it('a session that expired on a device that was signed in: a visitor, and the next start asks only for the session', async () => {
    server(true)
    await useSession().init()

    setActivePinia(createPinia())
    let asked = server(false)
    const session = useSession()
    await session.init()
    expect(asked).toEqual(['api/bootstrap', 'api/auth/session'])
    expect(session.user).toBeNull()

    setActivePinia(createPinia())
    asked = server(false)
    await useSession().init()
    expect(asked).toEqual(['api/auth/session'])
  })
})
