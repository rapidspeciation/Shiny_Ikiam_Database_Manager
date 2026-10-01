import { computed, onActivated, onBeforeUnmount, onDeactivated, onMounted, ref, watch } from 'vue'
import { api, requestId } from '../lib/api'
import { MODULE, type DayChange } from '../lib/clutches'
import { useTables } from '../stores/tables'

/** A clutch marked as checked (server/clutches.mjs). */
export interface ClutchCheck {
  id: string
  recordId: string
  clutch: string | null
  day: string
  actor: string
  username: string | null
  name: string | null
  /** The fields this check changed; none: "checked, no change". */
  fields: string[]
  actionId: string | null
  createdAt: string
}
export interface ServerDayChange extends DayChange {
  actorIds: string[]
  at: string
}
interface Day {
  day: string
  checks: ClutchCheck[]
  changes: ServerDayChange[]
}
/** What the cards show of a clutch today. */
export interface ClutchToday {
  checks: ClutchCheck[]
  changes: ServerDayChange[]
  checked: boolean
  changed: boolean
  /** Who checked or changed it today (names). */
  who: string[]
}
const POLL_MS = 20_000
const NONE: ClutchToday = { checks: [], changes: [], checked: false, changed: false, who: [] }

/**
 * The clutches' sum formulas and last changes (followed when the sheet
 * changes), and today's checks and changes of everyone (asked again every 20 s
 * while the page is visible, so parallel rounds see each other).
 */
export function useClutchDay() {
  const tables = useTables()
  const sums = ref<Record<string, Record<string, string>>>({})
  const last = ref<Record<string, { at: string; actor: string; name: string | null }>>({})
  const day = ref<Day>({ day: '', checks: [], changes: [] })
  const loaded = ref(false)
  const error = ref('')

  async function loadState() {
    try {
      const state = await api<{ sums: typeof sums.value; last: typeof last.value }>('clutches/state')
      sums.value = state.sums
      last.value = state.last
      loaded.value = true
    } catch (e) {
      error.value = e instanceof Error ? e.message : String(e)
    }
  }
  async function loadDay() {
    try {
      day.value = await api<Day>('clutches/day')
    } catch {
      /* offline: keep the last answer */
    }
  }
  // A save (or another person's edit followed by the table) changes the formulas.
  watch(
    () => tables.versions[MODULE],
    () => {
      void loadState()
      void loadDay()
    },
  )
  let timer: ReturnType<typeof setInterval> | undefined
  /** The tab is on screen (tabs stay alive in the background: no asking then). */
  let active = true
  const onVisible = () => {
    if (active && document.visibilityState === 'visible') void loadDay()
  }
  onMounted(() => {
    void loadState()
    void loadDay()
    timer = setInterval(() => {
      if (active && document.visibilityState === 'visible' && navigator.onLine) void loadDay()
    }, POLL_MS)
    document.addEventListener('visibilitychange', onVisible)
  })
  onActivated(() => {
    if (!active) void loadDay()
    active = true
  })
  onDeactivated(() => (active = false))
  onBeforeUnmount(() => {
    clearInterval(timer)
    document.removeEventListener('visibilitychange', onVisible)
  })

  const byRecord = computed(() => {
    const out = new Map<string, ClutchToday>()
    const get = (id: string) => {
      let v = out.get(id)
      if (!v) out.set(id, (v = { checks: [], changes: [], checked: false, changed: false, who: [] }))
      return v
    }
    for (const c of day.value.checks) {
      const v = get(c.recordId)
      v.checks.push(c)
      v.checked = true
      if (c.fields.length) v.changed = true
      const who = c.name || c.username || ''
      if (who && !v.who.includes(who)) v.who.push(who)
    }
    for (const c of day.value.changes) {
      const v = get(c.recordId)
      v.changes.push(c)
      v.checked = v.changed = true
      for (const who of c.actors) if (!v.who.includes(who)) v.who.push(who)
    }
    return out
  })
  const today = (recordId: string) => byRecord.value.get(recordId) ?? NONE

  /** Marks a clutch as checked by this person today, with the fields changed (none: no change). */
  async function markChecked(recordId: string, fields: string[] = [], actionId?: string | null) {
    const { check } = await api<{ check: ClutchCheck }>('clutches/checks', {
      method: 'POST',
      body: { requestId: requestId(), recordId, fields, ...(actionId ? { actionId } : {}) },
    })
    day.value = { ...day.value, checks: [...day.value.checks.filter(c => c.id !== check.id), check] }
    return check
  }
  async function unmark(id: string) {
    await api(`clutches/checks/${encodeURIComponent(id)}`, { method: 'DELETE', body: {} })
    day.value = { ...day.value, checks: day.value.checks.filter(c => c.id !== id) }
  }
  return { sums, last, day, loaded, error, today, byRecord, loadDay, loadState, markChecked, unmark }
}
export type ClutchDay = ReturnType<typeof useClutchDay>
