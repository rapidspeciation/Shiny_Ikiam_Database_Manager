import { defineStore } from 'pinia'
import { api, setCsrf } from '../lib/api'
import type { Module, Settings, SyncStatus, User } from '../lib/types'
import { setStagedSaving } from '../lib/stagedSwitch'

interface Bootstrap {
  user: User
  csrf: string
  modules: Module[]
  settings: Settings
  sync: SyncStatus
}

// Whether someone was signed in on this device when the app last started (no one's name, just yes or no).
const SIGNED_IN = 'session:signed-in'
const signedInHere = () => {
  try {
    return localStorage.getItem(SIGNED_IN) === '1'
  } catch {
    return false
  }
}
const rememberSignedIn = (yes: boolean) => {
  try {
    if (yes) localStorage.setItem(SIGNED_IN, '1')
    else localStorage.removeItem(SIGNED_IN)
  } catch {
    /* private mode */
  }
}

export const useSession = defineStore('session', {
  state: () => ({
    ready: false,
    setupRequired: false,
    user: null as User | null,
    modules: [] as Module[],
    settings: null as Settings | null,
    sync: null as SyncStatus | null,
  }),
  getters: {
    canEdit: s => !!s.user && ['editor', 'reviewer', 'admin'].includes(s.user.role),
    isAdmin: s => s.user?.role === 'admin',
    /** Reviewers and admins: may also make pre-made rows in the workbook. */
    isReviewer: s => !!s.user && ['reviewer', 'admin'].includes(s.user.role),
    module: s => (id: string) => s.modules.find(m => m.id === id),
  },
  actions: {
    async init() {
      // On a device that was signed in, the bootstrap is asked together with the session (one round
      // trip less on a phone's network); it is only used when the session says who is signed in.
      const early = signedInHere() ? api<Bootstrap>('bootstrap').catch(() => null) : null
      const session = await api<{ user: User | null; csrf: string | null; setupRequired: boolean }>('auth/session')
      this.setupRequired = session.setupRequired
      setCsrf(session.csrf)
      if (session.user) await this.load(await early)
      else {
        rememberSignedIn(false)
        // Signed out or expired: cached sheets must not stay readable on this device.
        navigator.serviceWorker?.controller?.postMessage('clear')
      }
      this.ready = true
    },
    async load(early: Bootstrap | null = null) {
      const data = early ?? (await api<Bootstrap>('bootstrap'))
      rememberSignedIn(true)
      setCsrf(data.csrf)
      this.user = data.user
      this.modules = data.modules
      this.settings = data.settings
      setStagedSaving(data.settings?.stagedSaving !== false)
      this.sync = data.sync
    },
    async login(username: string, password: string) {
      const data = await api<{ csrf: string }>('auth/login', { method: 'POST', body: { username, password } })
      setCsrf(data.csrf)
      await this.load()
    },
    async setup(token: string, username: string, displayName: string, password: string) {
      const data = await api<{ csrf: string }>('auth/setup', { method: 'POST', body: { token, username, displayName, password } })
      setCsrf(data.csrf)
      this.setupRequired = false
      await this.load()
    },
    async logout() {
      await api('auth/logout', { method: 'POST', body: {} })
      setCsrf(null)
      this.user = null
      rememberSignedIn(false)
      navigator.serviceWorker?.controller?.postMessage('clear')
    },
  },
})
