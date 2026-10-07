import { isBlank } from './cells'
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

/** The value of a field of one card, or of each card selected in the panel. */
export function setCardField<F extends ChoiceField>(cards: DeathCard[], ids: string | string[], field: F, value: DeathChoice[F]): DeathCard[] {
  const list = typeof ids === 'string' ? [ids] : ids
  return cards.map(c => (list.some(id => same(c.id, id)) ? { ...c, choice: { ...c.choice, [field]: value } } : c))
}

/** A quick phrase after what a note says ("Head eaten; With fungi"). */
export const withPhrase = (note: string, phrase: string) => (note.trim() ? `${note.trim()}; ${phrase}` : phrase)

/** A quick phrase added to each selected card's own note. */
export const addPhraseTo = (cards: DeathCard[], ids: string[], phrase: string): DeathCard[] =>
  cards.map(c => (ids.some(id => same(c.id, id)) ? { ...c, choice: { ...c.choice, note: withPhrase(c.choice.note, phrase) } } : c))

// --- Several cards open in the panel at once

/** The cards open in the panel (their IDs, in the cards' order), and the one a Shift+click counts from. */
export interface CardSelection {
  ids: string[]
  anchor: string | null
}
/**
 * A click on a card, with the cards' IDs as shown (`order`). Plain: that card
 * alone (`again` when it was already the one open, to make it pulse). Ctrl or
 * ⌘ (`toggle`): it joins or leaves the others. Shift (`range`): the cards from
 * the last one clicked to this one (with Ctrl too, beside those already there).
 */
export function clickCard(
  selection: CardSelection,
  order: string[],
  id: string,
  how: { toggle?: boolean; range?: boolean } = {},
): CardSelection & { again: boolean } {
  const at = order.findIndex(o => same(o, id))
  if (at < 0) return { ...selection, again: false }
  const card = order[at]
  const sorted = (ids: string[]) => order.filter(o => ids.some(i => same(i, o)))
  const from = selection.anchor === null ? -1 : order.findIndex(o => same(o, selection.anchor!))
  if (how.range && from >= 0) {
    const span = order.slice(Math.min(from, at), Math.max(from, at) + 1)
    return { ids: sorted(how.toggle ? [...selection.ids, ...span] : span), anchor: order[from], again: false }
  }
  if (how.toggle) {
    const there = selection.ids.some(i => same(i, card))
    return { ids: sorted(there ? selection.ids.filter(i => !same(i, card)) : [...selection.ids, card]), anchor: card, again: false }
  }
  return { ids: [card], anchor: card, again: selection.ids.length === 1 && same(selection.ids[0], card) }
}

/**
 * What the panel shows for the cards selected: each field's value when they
 * all have it, and the fields where they differ («varios»; shown empty).
 */
export function commonChoice(cards: DeathCard[], ids: string[]): { choice: DeathChoice; mixed: ChoiceField[] } {
  const chosen = cards.filter(c => ids.some(id => same(c.id, id))).map(c => c.choice)
  const choice: DeathChoice = { date: '', cause: '', preserved: false, note: '' }
  const mixed: ChoiceField[] = []
  if (!chosen.length) return { choice, mixed }
  for (const field of ['date', 'cause', 'preserved', 'note'] as const) {
    const first = chosen[0][field]
    if (chosen.every(c => c[field] === first)) (choice as Record<ChoiceField, unknown>)[field] = first
    else mixed.push(field)
  }
  return { choice, mixed }
}

/** The IDs named in the panel's title: the first `max`, and how many more («G7D, G8D y 3 más (5)»). */
export const namedIds = (ids: string[], max = 2) => ({ shown: ids.slice(0, max), more: Math.max(0, ids.length - max) })

// --- The values for the next butterflies

/**
 * After recording, the next butterflies start with that death (a run of the
 * same cause needs one tap): the last card recorded's values, or the panel's
 * as they were when nothing was recorded.
 */
export const nextDefaults = (recorded: DeathChoice[], current: DeathChoice): DeathChoice =>
  recorded.length ? { ...recorded[recorded.length - 1] } : current

/** The values for the next butterflies as kept in the browser: the date with the day it was chosen on. */
export interface KeptDefaults {
  cause: string
  preserved: boolean
  note: string
  date?: string
  day?: string
}
/** The values kept, read back: the date only the same day (else today's); anything missing, empty. */
export function readDefaults(kept: unknown, today: string): DeathChoice {
  const k = (kept && typeof kept === 'object' ? kept : {}) as Partial<KeptDefaults>
  const text = (v: unknown) => (typeof v === 'string' ? v : '')
  return {
    date: k.day === today && typeof k.date === 'string' ? k.date : today,
    cause: text(k.cause),
    preserved: k.preserved === true,
    note: text(k.note),
  }
}
/** The values for the next butterflies to keep, stamped with today. */
export const keepDefaults = (choice: DeathChoice, today: string): KeptDefaults => ({ ...choice, day: today })

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
  /** Death_date and Death_cause before that day's first save of it (null: alive; a death replaced: the old one). */
  before: Partial<Record<'Death_date' | 'Death_cause', CellValue>>
}

const DEATH_FIELDS = ['Death_date', 'Death_cause'] as const
const sameCell = (a: CellValue | undefined, b: CellValue | undefined) => (isBlank(a) && isBlank(b)) || String(a ?? '') === String(b ?? '')
/**
 * The death recorded still holds: its date and cause are not back to what
 * they were before (an undo, also of a death that replaced another).
 */
export function stillRecorded(item: Recorded, current: (field: string) => CellValue): boolean {
  const fields = DEATH_FIELDS.filter(f => f in item.before)
  return !fields.length || !fields.every(f => sameCell(current(f), item.before[f]))
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
        item = { recordId: change.recordId, label: change.label ?? '', at: action.createdAt, actors: [], changeIds: [], before: {} }
        out.set(change.recordId, item)
      }
      for (const f of DEATH_FIELDS) if (change.field === f && !(f in item.before)) item.before[f] = change.before
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
