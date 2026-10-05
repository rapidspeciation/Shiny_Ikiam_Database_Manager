import { computed, onActivated, onBeforeUnmount, onDeactivated, onMounted, ref, watch } from 'vue'
import { api, requestId } from '../lib/api'
import { MODULE, reviewState, type ClutchEvent, type ClutchTallies, type DayChange, type EventKind, type ReviewState, type Stage } from '../lib/clutches'
import { applyClutchSettings, clutchSettings } from '../lib/clutchSettings'
import { overlaySums, overlayTable } from '../lib/staged'
import { useLive } from '../stores/live'
import { useTables } from '../stores/tables'

/** A clutch marked as checked (server/clutches.mjs): only in the app, for one day, seen by everyone. */
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
  /** Checked, or checked but someone should look again (with a short reason, `note`). */
  state: 'checked' | 'verify'
  note: string | null
  actionId: string | null
  /** The entry kept in the app it went with, until that is written to Google Sheets. */
  stagedEntry?: string | null
  createdAt: string
}
export interface ServerDayChange extends DayChange {
  actorIds: string[]
  at: string
  /** The changes still standing (not undone) behind this before → after, to undo them. */
  parts: { changeId: string; actionId: string; actor: string }[]
}
export interface Day {
  day: string
  checks: ClutchCheck[]
  changes: ServerDayChange[]
  /** The day's app-only events (hatched, died, disappeared, preserved). */
  events?: ClutchEvent[]
}
/** What the cards show of a clutch today. */
export interface ClutchToday {
  checks: ClutchCheck[]
  changes: ServerDayChange[]
  events: ClutchEvent[]
  checked: boolean
  changed: boolean
  /** Who checked or changed it today (names). */
  who: string[]
  /** Who marked it as checked today (names): the marks only, kept in the app. */
  checkedBy: string[]
  /** Where its review stands today: not looked at, checked, or to verify (the latest mark counts). */
  review: ReviewState
  /** The latest mark, if any. */
  latest: ClutchCheck | null
}
const POLL_MS = 20_000
const NONE: ClutchToday = { checks: [], changes: [], events: [], checked: false, changed: false, who: [], checkedBy: [], review: 'none', latest: null }

/**
 * The clutches' sum formulas, last changes, what their events add up to and
 * the team's settings (followed when the sheet changes), and today's checks,
 * changes and events of everyone (asked again every 20 s while the page is
 * visible, so parallel rounds see each other).
 */
export function useClutchDay() {
  const tables = useTables()
  const live = useLive()
  const sheetSums = ref<Record<string, Record<string, string>>>({})
  /** The counts' sums as the sheet has them, with everyone's entries kept in the app on top (lib/staged.ts). */
  const sums = computed(() => {
    void tables.versions[MODULE]
    return overlaySums(sheetSums.value, overlayTable(tables.tables[MODULE], live.items).sums)
  })
  const last = ref<Record<string, { at: string; actor: string; name: string | null }>>({})
  const tallies = ref<Record<string, ClutchTallies>>({})
  const day = ref<Day>({ day: '', checks: [], changes: [], events: [] })
  const loaded = ref(false)
  const error = ref('')

  async function loadState() {
    try {
      const state = await api<{ sums: typeof sums.value; last: typeof last.value; tallies?: typeof tallies.value; settings?: { subtractPreserved: boolean } }>(
        'clutches/state',
      )
      sheetSums.value = state.sums
      last.value = state.last
      tallies.value = state.tallies ?? {}
      applyClutchSettings(state.settings)
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
  // Someone's entries kept in the app came or went (or were written): the day's list has them.
  watch(
    () => live.items.filter(i => i.sheet === MODULE).map(i => `${i.id}:${i.status}:${i.updatedAt}`).join(),
    () => void loadDay(),
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
      if (!v) out.set(id, (v = { ...NONE, checks: [], changes: [], events: [], who: [], checkedBy: [] }))
      return v
    }
    for (const c of day.value.checks) {
      const v = get(c.recordId)
      v.checks.push(c)
      v.checked = true
      if (c.fields.length) v.changed = true
      const who = c.name || c.username || ''
      if (who && !v.who.includes(who)) v.who.push(who)
      if (who && !v.checkedBy.includes(who)) v.checkedBy.push(who)
    }
    for (const v of out.values()) {
      v.review = reviewState(v.checks)
      v.latest = [...v.checks].sort((a, b) => a.createdAt.localeCompare(b.createdAt)).at(-1) ?? null
    }
    for (const c of day.value.changes) {
      const v = get(c.recordId)
      v.changes.push(c)
      // Changed today counts as looked at (`checked`), though only a mark sets its review.
      v.checked = v.changed = true
      for (const who of c.actors) if (!v.who.includes(who)) v.who.push(who)
    }
    for (const e of day.value.events ?? []) get(e.recordId).events.push(e)
    return out
  })
  const today = (recordId: string) => byRecord.value.get(recordId) ?? NONE

  /**
   * Marks a clutch as checked by this person today, with the fields changed
   * (none: no change); `verify` when someone should look again, with why.
   */
  async function markChecked(
    recordId: string,
    fields: string[] = [],
    actionId?: string | null,
    { state = 'checked', note = '', stagedEntry = null }: { state?: 'checked' | 'verify'; note?: string; stagedEntry?: string | null } = {},
  ) {
    const { check } = await api<{ check: ClutchCheck }>('clutches/checks', {
      method: 'POST',
      body: {
        requestId: requestId(),
        recordId,
        fields,
        state,
        ...(note.trim() ? { note: note.trim() } : {}),
        ...(actionId ? { actionId } : {}),
        ...(stagedEntry ? { stagedEntry } : {}),
      },
    })
    day.value = { ...day.value, checks: [...day.value.checks.filter(c => c.id !== check.id), check] }
    return check
  }
  async function unmark(id: string) {
    await api(`clutches/checks/${encodeURIComponent(id)}`, { method: 'DELETE', body: {} })
    day.value = { ...day.value, checks: day.value.checks.filter(c => c.id !== id) }
  }

  /** Bumped by every event recorded or taken back here, for the timelines to load again. */
  const eventsVersion = ref(0)
  /** Records what happened to some eggs, larvae or pupae today (only in the app). */
  async function addEvent(body: { recordId: string; stage: Stage; kind: EventKind; count: number; ids?: string[]; note?: string; actionId?: string | null }) {
    const { event } = await api<{ event: ClutchEvent }>('clutches/events', {
      method: 'POST',
      body: { requestId: requestId(), ...body },
    })
    day.value = { ...day.value, events: [...(day.value.events ?? []).filter(e => e.id !== event.id), event] }
    eventsVersion.value++
    void loadState()
    return event
  }
  async function removeEvent(id: string) {
    await api(`clutches/events/${encodeURIComponent(id)}`, { method: 'DELETE', body: {} })
    day.value = { ...day.value, events: (day.value.events ?? []).filter(e => e.id !== id) }
    eventsVersion.value++
    void loadState()
  }
  return {
    sums,
    last,
    tallies,
    settings: clutchSettings,
    day,
    loaded,
    error,
    today,
    byRecord,
    loadDay,
    loadState,
    markChecked,
    unmark,
    eventsVersion,
    addEvent,
    removeEvent,
  }
}
export type ClutchDay = ReturnType<typeof useClutchDay>
