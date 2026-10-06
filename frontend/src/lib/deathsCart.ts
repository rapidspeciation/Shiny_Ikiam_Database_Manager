import { searchKey, type ChoiceField, type DeathChoice } from './deaths'
import type { CellValue } from './types'

/**
 * Muertes in two levels, like a shopping cart: «Seleccionadas», the
 * butterflies picked by ID, each with its own date, cause, preservation and
 * note (copied from the panel's values for the next butterflies when it was
 * picked, then changed on its own); and «Registradas hoy», the deaths saved
 * from Muertes today (read from the history), sorted to copy them into the
 * paper notebook.
 */

/** A butterfly in «Seleccionadas»: its Insectary ID and the death it will be recorded with. */
export interface DeathCard {
  id: string
  choice: DeathChoice
}

const same = (a: string, b: string) => searchKey(a) === searchKey(b)
export const hasCard = (cards: DeathCard[], id: string) => cards.some(c => same(c.id, id))
export const cardOf = (cards: DeathCard[], id: string) => cards.find(c => same(c.id, id))

/**
 * IDs picked, after the cards there, each with a copy of the panel's values
 * (`defaults`); an ID already there keeps its card and values. `added`: the new ones.
 */
export function addCards(cards: DeathCard[], ids: string[], defaults: DeathChoice): { cards: DeathCard[]; added: string[] } {
  const out = [...cards]
  const added: string[] = []
  for (const id of ids) {
    if (hasCard(out, id)) continue
    out.push({ id, choice: { ...defaults } })
    added.push(id)
  }
  return { cards: out, added }
}

export const removeCards = (cards: DeathCard[], ids: string[]) => cards.filter(c => !ids.some(id => same(c.id, id)))

/** One card's value of a field (the panel with that card selected). */
export function setCardField<F extends ChoiceField>(cards: DeathCard[], id: string, field: F, value: DeathChoice[F]): DeathCard[] {
  return cards.map(c => (same(c.id, id) ? { ...c, choice: { ...c.choice, [field]: value } } : c))
}

/** The cards whose value of a field is not the panel's (to offer giving them the panel's). */
export const differing = (cards: DeathCard[], defaults: DeathChoice, field: ChoiceField) =>
  cards.filter(c => c.choice[field] !== defaults[field]).map(c => c.id)

/** The panel's value of a field given to every card («Aplicar a las seleccionadas»). */
export function applyToAll<F extends ChoiceField>(cards: DeathCard[], field: F, value: DeathChoice[F]): DeathCard[] {
  return cards.map(c => (c.choice[field] === value ? c : { ...c, choice: { ...c.choice, [field]: value } }))
}

/**
 * The cards for a list of IDs (the table mode's picker): in that order, those
 * already there with their values, the new ones with the panel's.
 */
export function cardsForIds(cards: DeathCard[], ids: string[], defaults: DeathChoice): DeathCard[] {
  const out: DeathCard[] = []
  for (const id of ids) {
    if (hasCard(out, id)) continue
    out.push(cardOf(cards, id) ?? { id, choice: { ...defaults } })
  }
  return out
}

/**
 * Which card the panel shows after picking IDs (null: the panel's values for
 * the next butterflies). One ID picked that was there already, or that is now
 * the only card: that one, one at a time as before. Several IDs, «Seleccionar
 * varias», or one more beside others: none, so the next ones keep the panel's values.
 */
export function focusAfterPick(before: DeathCard[], after: DeathCard[], picked: string[], several: boolean): string | null {
  if (several || picked.length !== 1) return null
  const there = cardOf(before, picked[0])
  if (there) return there.id
  const card = cardOf(after, picked[0])
  return card && after.length === 1 ? card.id : null
}

// --- Registradas hoy

/** A save in the history (GET /api/history): who, when, and each cell before → after. */
export interface HistoryAction {
  id: string
  actor: string
  actorName: string | null
  createdAt: string
  status: string
  purpose: string | null
  changes: { id: string; recordId: string; label: string | null; sheet: string; field: string; before: CellValue; after: CellValue }[]
}

/** A butterfly whose death was saved from Muertes on a day: its changes (to undo them), who and when. */
export interface Recorded {
  recordId: string
  label: string
  /** The last save of it that day. */
  at: string
  actors: { id: string; name: string }[]
  changeIds: string[]
}

/** The day (ISO) a moment falls on in Ecuador, as `todayIso` counts days. */
export const ecuadorDay = (iso: string) =>
  Number.isNaN(Date.parse(iso)) ? '' : new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' }).format(new Date(iso))

/**
 * The Insectary_data rows that Muertes' saves of `day` wrote in, one each,
 * with all those changes (an edit after the first save too), the people who
 * saved them and the last time. Only saves confirmed in Google Sheets.
 */
export function recordedOn(actions: HistoryAction[], day: string, dayOf: (iso: string) => string = ecuadorDay): Recorded[] {
  const out = new Map<string, Recorded>()
  const ordered = [...actions].sort((a, b) => a.createdAt.localeCompare(b.createdAt))
  for (const action of ordered) {
    if (action.purpose !== 'muertes' || action.status !== 'verified' || dayOf(action.createdAt) !== day) continue
    for (const change of action.changes) {
      if (change.sheet !== 'Insectary_data') continue
      let item = out.get(change.recordId)
      if (!item) {
        item = { recordId: change.recordId, label: change.label ?? '', at: action.createdAt, actors: [], changeIds: [] }
        out.set(change.recordId, item)
      }
      item.at = action.createdAt
      if (change.label) item.label = change.label
      item.changeIds.push(change.id)
      if (!item.actors.some(a => a.id === action.actor)) item.actors.push({ id: action.actor, name: action.actorName || action.actor })
    }
  }
  return [...out.values()]
}

/** How «Registradas hoy» is sorted: by Insectary ID, emergence date or sheet row, up or down. */
export type RecordedSort = 'id' | 'emergence' | 'row'
export interface RecordedOrder {
  by: RecordedSort
  desc: boolean
}
/** What a recorded death is sorted by. */
export interface SortFacts {
  id: string
  /** Intro2Insectary_date (emerged or brought in), null when unknown. */
  emergence: number | null
  row: number
}

/** Insectary IDs as people read them: A2E before A10E, capitals ignored. */
export const compareIds = (a: string, b: string) => a.localeCompare(b, 'en', { numeric: true, sensitivity: 'base' })

/**
 * The recorded deaths in the order chosen; the same emergence date or ID by
 * sheet row, and those without an emergence date last either way.
 */
export function sortRecorded<T>(items: T[], order: RecordedOrder, facts: (item: T) => SortFacts): T[] {
  const sign = order.desc ? -1 : 1
  const keyed = items.map(item => ({ item, f: facts(item) }))
  keyed.sort((x, y) => {
    const a = x.f
    const b = y.f
    if (order.by === 'emergence') {
      if (a.emergence === null || b.emergence === null) {
        if (a.emergence !== b.emergence) return a.emergence === null ? 1 : -1
      } else if (a.emergence !== b.emergence) return sign * (a.emergence - b.emergence)
    } else if (order.by === 'id') {
      const c = compareIds(a.id, b.id)
      if (c) return sign * c
    }
    return sign * (a.row - b.row)
  })
  return keyed.map(k => k.item)
}

export const DEFAULT_ORDER: RecordedOrder = { by: 'row', desc: false }
/** A sort read back from storage, or the default when it is not one. */
export function readOrder(value: unknown): RecordedOrder {
  const v = value as Partial<RecordedOrder> | null
  return v && (v.by === 'id' || v.by === 'emergence' || v.by === 'row') ? { by: v.by, desc: !!v.desc } : DEFAULT_ORDER
}
