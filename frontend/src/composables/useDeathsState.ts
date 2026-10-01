import { reactive, ref, type Ref } from 'vue'
import { todayIso } from '../lib/dates'
import type { OwnChoices } from '../lib/deaths'
import { persistentRef } from '../lib/persist'

/**
 * What Muertes is registering, shared by its two modes (the cards and the
 * table, see useEntryMode): the butterflies chosen, the death date, the cause,
 * preserved or not, the medium and the CAM and tube typed for each. Switching
 * mode (or turning a phone) keeps all of it. The IDs, cause, preserved and the
 * cards' own values live as long as the browser tab (sessionStorage), the
 * medium in this browser.
 */
export interface DeathsState {
  /** The Insectary IDs chosen, in the order they were added. */
  picked: Ref<string[]>
  /** The death date (ISO), today by default. */
  date: Ref<string>
  cause: Ref<string>
  /** Preserved (CAM and tube for each) or not (the NA / NOT_COLLECTED block). */
  preserved: Ref<boolean>
  medium: Ref<string>
  /** The CAM and tube typed (or suggested) for each butterfly, by Insectary ID. */
  samples: Record<string, { cam: string; tube: string }>
  /** What the app suggested, so a value the person typed is never replaced. */
  suggested: Record<string, { cam: string; tube: string }>
  /**
   * The cards' own date, cause or preservation, by Insectary ID (the cards
   * only: set with the card selected; the rest come from date, cause and
   * preserved above, which apply to all).
   */
  own: Ref<OwnChoices>
  /** The cards selected (tapped): what the panel sets while any is selected. */
  selected: Ref<string[]>
}

const PREFIX = 'ithomiini:'
const read = (storage: Storage, key: string): unknown => {
  try {
    const saved = storage.getItem(PREFIX + key)
    return saved === null ? undefined : JSON.parse(saved)
  } catch {
    return undefined
  }
}
const write = (storage: Storage, key: string, value: unknown) => storage.setItem(PREFIX + key, JSON.stringify(value))
const drop = (storage: Storage, key: string) => storage.removeItem(PREFIX + key)

/**
 * Moves what the two separate screens kept before (until 1 Oct 2026) into the
 * shared keys: the computer's list ('deaths:picked', and its older
 * 'deaths:loaded') and the phone's ('deaths:phone-picked') become one list
 * without repeats; the phone's cause fills an empty one; the phone's
 * "preserved" (or the computer's "not preserved", reversed) and medium carry over.
 */
export function migrateDeathKeys(session: Storage = sessionStorage, local: Storage = localStorage) {
  const lists = ['deaths:ids', 'deaths:picked', 'deaths:loaded', 'deaths:phone-picked']
  const old = lists.slice(1).filter(key => read(session, key) !== undefined)
  if (old.length) {
    const seen = new Set<string>()
    const ids: string[] = []
    for (const key of lists) {
      const list = read(session, key)
      if (!Array.isArray(list)) continue
      for (const id of list) {
        const text = String(id).trim()
        if (text && !seen.has(text.toUpperCase())) {
          seen.add(text.toUpperCase())
          ids.push(text)
        }
      }
    }
    write(session, 'deaths:ids', ids)
    for (const key of old) drop(session, key)
  }
  const phoneCause = read(session, 'deaths:phone-cause')
  if (phoneCause !== undefined) {
    if (typeof phoneCause === 'string' && phoneCause && !read(session, 'deaths:cause')) write(session, 'deaths:cause', phoneCause)
    drop(session, 'deaths:phone-cause')
  }
  const phonePreserved = read(session, 'deaths:phone-preserved')
  const notPreserved = read(session, 'deaths:not-preserved')
  if (read(session, 'deaths:preserved') === undefined) {
    if (typeof phonePreserved === 'boolean') write(session, 'deaths:preserved', phonePreserved)
    else if (typeof notPreserved === 'boolean') write(session, 'deaths:preserved', !notPreserved)
  }
  drop(session, 'deaths:phone-preserved')
  drop(session, 'deaths:not-preserved')
  const medium = read(local, 'deaths:phone-medium')
  if (medium !== undefined) {
    if (typeof medium === 'string' && read(local, 'deaths:medium') === undefined) write(local, 'deaths:medium', medium)
    drop(local, 'deaths:phone-medium')
  }
}

/** A fresh state from the browser's storage (tests; the app uses the one shared state below). */
export function createDeathsState(): DeathsState {
  migrateDeathKeys()
  return {
    picked: persistentRef<string[]>('deaths:ids', []),
    // Deaths are usually entered the same day.
    date: ref(todayIso()),
    cause: persistentRef('deaths:cause', ''),
    preserved: persistentRef('deaths:preserved', false),
    medium: persistentRef('deaths:medium', 'Flash frozen', { lasting: true }),
    samples: reactive({}),
    suggested: reactive({}),
    own: persistentRef<OwnChoices>('deaths:own', {}),
    selected: ref<string[]>([]),
  }
}

let shared: DeathsState | null = null
/** The one state both Muertes modes read and write. */
export function useDeathsState(): DeathsState {
  return (shared ??= createDeathsState())
}
