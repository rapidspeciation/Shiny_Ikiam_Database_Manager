import { computed, reactive, ref, type Ref, type WritableComputedRef } from 'vue'
import { todayIso } from '../lib/dates'
import type { DeathChoice } from '../lib/deaths'
import { addCards, cardsForIds, type DeathCard } from '../lib/deathsCart'
import { persistentRef } from '../lib/persist'

/**
 * What Muertes is recording, shared by its two modes (the cards and the
 * table, see useEntryMode), so switching mode (or turning a phone) keeps it:
 * the panel's values for the next butterflies (date, cause, preserved, note),
 * «Seleccionadas» (each butterfly picked with its own values), the one the
 * panel shows, the medium, and the CAM and tube typed for each. They live as
 * long as the browser tab (sessionStorage), the medium in this browser; the
 * date starts at today on every load.
 */
export interface DeathsState {
  /** «Para las próximas mariposas»: the values each butterfly picked starts with. */
  defaults: Ref<DeathChoice>
  /** «Seleccionadas», in the order picked. */
  cards: Ref<DeathCard[]>
  /** The card the panel shows (its Insectary ID); null: the values for the next butterflies. */
  focus: Ref<string | null>
  /** «Seleccionar varias»: each ID tapped is added (or taken out) and the search stays for the next. */
  several: Ref<boolean>
  medium: Ref<string>
  /** The CAM and tube typed (or suggested) for each butterfly, by Insectary ID. */
  samples: Record<string, { cam: string; tube: string }>
  /** What the app suggested, so a value the person typed is never replaced. */
  suggested: Record<string, { cam: string; tube: string }>
  // The table mode's view of the same: the IDs, and the panel's date, cause and preservation.
  picked: WritableComputedRef<string[]>
  date: WritableComputedRef<string>
  cause: WritableComputedRef<string>
  preserved: WritableComputedRef<boolean>
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

/**
 * Moves what Muertes kept from 1 to 6 Oct 2026 (one butterfly at a time or a
 * group, with the panel's values and each one's own) into «Seleccionadas»:
 * the IDs chosen and those left unfinished become cards with their values.
 */
export function migrateGroupKeys(session: Storage = sessionStorage) {
  const ids = read(session, 'deaths:ids')
  const unfinished = read(session, 'deaths:unfinished')
  const old = ['deaths:ids', 'deaths:unfinished', 'deaths:own', 'deaths:cause', 'deaths:preserved', 'deaths:note', 'deaths:several', 'deaths:touched']
  if (!old.some(key => read(session, key) !== undefined)) return
  const text = (v: unknown) => (typeof v === 'string' ? v : '')
  const defaults: DeathChoice = {
    date: todayIso(),
    cause: text(read(session, 'deaths:cause')),
    preserved: read(session, 'deaths:preserved') === true,
    note: text(read(session, 'deaths:note')),
  }
  if (read(session, 'deaths:defaults') === undefined) {
    const { date: _today, ...kept } = defaults
    write(session, 'deaths:defaults', kept)
  }
  const own = (read(session, 'deaths:own') ?? {}) as Record<string, Partial<DeathChoice>>
  const list = [...(Array.isArray(ids) ? ids : []), ...(Array.isArray(unfinished) ? unfinished : [])].map(String)
  let cards = (read(session, 'deaths:cards') as DeathCard[] | undefined) ?? []
  for (const id of list) {
    const mine = own && typeof own === 'object' ? own[id] : undefined
    cards = addCards(cards, [id], { ...defaults, ...(mine ?? {}) }).cards
  }
  write(session, 'deaths:cards', cards)
  for (const key of old) drop(session, key)
}

/** A fresh state from the browser's storage (tests; the app uses the one shared state below). */
export function createDeathsState(): DeathsState {
  migrateDeathKeys()
  migrateGroupKeys()
  // The panel's cause, preservation and note are kept; the date is today's on every load.
  const kept = persistentRef<Omit<DeathChoice, 'date'>>('deaths:defaults', { cause: '', preserved: false, note: '' })
  const date = ref(todayIso())
  const defaults = computed<DeathChoice>({
    get: () => ({ date: date.value, cause: kept.value.cause ?? '', preserved: !!kept.value.preserved, note: kept.value.note ?? '' }),
    set: v => {
      date.value = v.date
      kept.value = { cause: v.cause, preserved: v.preserved, note: v.note }
    },
  })
  const cards = persistentRef<DeathCard[]>('deaths:cards', [])
  const field = <F extends keyof DeathChoice>(key: F) =>
    computed<DeathChoice[F]>({
      get: () => defaults.value[key],
      set: v => (defaults.value = { ...defaults.value, [key]: v }),
    })
  return {
    defaults,
    cards,
    focus: persistentRef<string | null>('deaths:focus', null),
    several: persistentRef('deaths:several-pick', false),
    medium: persistentRef('deaths:medium', 'Flash frozen', { lasting: true }),
    samples: reactive({}),
    suggested: reactive({}),
    picked: computed({
      get: () => cards.value.map(c => c.id),
      set: ids => (cards.value = cardsForIds(cards.value, ids, defaults.value)),
    }),
    date: field('date'),
    cause: field('cause'),
    preserved: field('preserved'),
  }
}

let shared: DeathsState | null = null
/** The one state both Muertes modes read and write. */
export function useDeathsState(): DeathsState {
  return (shared ??= createDeathsState())
}
