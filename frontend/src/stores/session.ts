import { defineStore } from 'pinia'
import { api, setCsrf } from '../lib/api'
import type { Module, Settings, SyncStatus, User } from '../lib/types'

interface Bootstrap {
  user: User
  csrf: string
  modules: Module[]
  settings: Settings
  sync: SyncStatus
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
      const session = await api<{ user: User | null; csrf: string | null; setupRequired: boolean }>('auth/session')
      this.setupRequired = session.setupRequired
      setCsrf(session.csrf)
      if (session.user) await this.load()
      // Signed out or expired: cached sheets must not stay readable on this device.
      else navigator.serviceWorker?.controller?.postMessage('clear')
      this.ready = true
    },
    async load() {
      const data = await api<Bootstrap>('bootstrap')
      setCsrf(data.csrf)
      this.user = data.user
      this.modules = data.modules
      this.settings = data.settings
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
      navigator.serviceWorker?.controller?.postMessage('clear')
    },
  },
})
