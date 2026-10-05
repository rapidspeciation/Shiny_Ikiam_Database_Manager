import { defineStore } from 'pinia'
import { api } from '../lib/api'
import { changeCount, type StagedClaim, type StagedItem } from '../lib/staged'

/** Whether Google answers (server/workbook-health.mjs): ok, slow, or busy (503, no answer). */
export interface WorkbookState {
  state: 'ok' | 'slow' | 'busy'
  since?: string
  lastError?: string
  checkedAt?: string | null
  nextProbeAt?: string
}
interface Pulse {
  revision: string
  workbook: WorkbookState
  outbox: { waiting: number }
  staged: { staged: number; sent: number; rows: number }
  /** This person's saves kept in the app recently (waiting for Google, or settled). */
  mine: { id: string; status: string; kind: string; ref: string | null; updated_at: string }[]
  /** The lab's simulated busy workbook (LOCAL_MODE). */
  simulated?: { mode: string; until: string } | null
}

let stopped = true
let generation = 0

/**
 * What every open page follows at once (GET /api/pulse, a long poll): how
 * Google answers, how many saves wait for it, and everyone's Emergidos and
 * Clutches entries kept in the app (loaded again whenever they change).
 */
export const useLive = defineStore('live', {
  state: () => ({
    revision: '',
    workbook: { state: 'ok' } as WorkbookState,
    waiting: 0,
    counts: { staged: 0, sent: 0, rows: 0 },
    mine: [] as Pulse['mine'],
    simulated: null as Pulse['simulated'],
    items: [] as StagedItem[],
    claims: [] as StagedClaim[],
    stagedLoaded: false,
    /** Bumped on every change (pages that follow something else, e.g. the pending saves). */
    ticks: 0,
  }),
  getters: {
    /** Google is not answering normally. */
    busy: s => s.workbook.state !== 'ok',
    /** Changes waiting for «Guardar en Google Sheets» (a new row counts one). */
    stagedCount: s => changeCount(s.items),
    /** Changes being written now (or waiting for Google). */
    sendingCount: s => changeCount(s.items, 'sent'),
  },
  actions: {
    apply(p: Pulse) {
      const stagedChanged = p.staged.staged !== this.counts.staged || p.staged.sent !== this.counts.sent || p.revision !== this.revision
      this.revision = p.revision
      this.workbook = p.workbook
      this.waiting = p.outbox.waiting
      this.counts = p.staged
      this.mine = p.mine ?? []
      this.simulated = p.simulated ?? null
      this.ticks++
      if (stagedChanged || !this.stagedLoaded) void this.loadStaged()
    },
    async loadStaged() {
      try {
        const out = await api<{ items: StagedItem[]; claims: StagedClaim[] }>('staged')
        this.items = out.items
        this.claims = out.claims
        this.stagedLoaded = true
      } catch {
        /* the next pulse asks again */
      }
    },
    /** Asks again at once (after this page's own save, staging or undo). */
    async refresh() {
      try {
        this.apply(await api<Pulse>('pulse'))
      } catch {
        /* offline */
      }
    },
    /** Follows the server until stop() (signed out). */
    start() {
      if (!stopped) return
      stopped = false
      const mine = ++generation
      void (async () => {
        while (!stopped && mine === generation) {
          try {
            const query = this.revision ? `?wait=1&revision=${encodeURIComponent(this.revision)}` : ''
            const p = await api<Pulse>(`pulse${query}`)
            if (stopped || mine !== generation) return
            if (p.revision !== this.revision || !this.stagedLoaded) this.apply(p)
          } catch {
            await new Promise(resolve => setTimeout(resolve, 5000))
          }
        }
      })()
    },
    stop() {
      stopped = true
      generation++
      this.$reset()
    },
  },
})
