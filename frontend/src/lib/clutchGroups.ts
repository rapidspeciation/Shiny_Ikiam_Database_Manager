import type { ClutchEvent, CountField, EventKind, Stage } from './clutches'

/**
 * A stage's count written as the sheet keeps it, in groups: one parenthesized
 * sub-sum per group (box A, box B: =(6-2)+(5+3)), each holding its dated terms
 * (+6 hatched, −2 died); one group is the plain sum (=27-2-11-3). What the
 * parentheses cannot hold (each group's label, the group it came from, its
 * photos) lives in the app by position (server/clutches.mjs clutch_groups), and
 * each term has its event (date, cause, note). Everything here is pure: the
 * clutch editor applies the plans (the new formula, the events, the groups) and
 * the tests check them.
 */
export type Groups = number[][]

export const groupTotal = (g: number[]) => g.reduce((a, b) => a + b, 0)
export const groupsTotal = (groups: Groups) => groups.reduce((a, g) => a + groupTotal(g), 0)
export const flatTerms = (groups: Groups) => groups.flat()

/** One group's terms as written: [6, −2] → "6-2". */
const termsWritten = (terms: number[]) => terms.map((t, i) => (i === 0 ? String(t) : t < 0 ? `-${-t}` : `+${t}`)).join('')
/** The formula for groups: one group plain (=27-2), several in parentheses (=(6-2)+(5+3)); nothing for none. */
export function formulaOfGroups(groups: Groups): string | null {
  const kept = groups.filter(g => g.length)
  if (!kept.length) return null
  if (kept.length === 1) return `=${termsWritten(kept[0])}`
  return '=' + kept.map(g => `(${termsWritten(g)})`).join('+')
}

/**
 * A simple sum's groups ("=(6-2)+(5+3)" → [[6, −2], [5, 3]]; "=27-2" → [[27, −2]]);
 * terms between parentheses make a group of their own. Null when the text is
 * not such a sum (the same grammar as lib/sums.ts simpleSum).
 */
export function groupsOf(formula: string): Groups | null {
  const s = formula.trim().replace(/^=/, '').replace(/\s+/g, '')
  if (!/^(?:\d+|\(\d+(?:[+-]\d+)*\))(?:[+-]\d+|\+\(\d+(?:[+-]\d+)*\))*$/.test(s)) return null
  const out: Groups = []
  let bare: number[] | null = null
  for (const m of s.matchAll(/\(([^)]*)\)|([+-]?\d+)/g)) {
    if (m[1] !== undefined) {
      bare = null
      out.push((m[1].match(/[+-]?\d+/g) ?? []).map(Number))
    } else {
      if (!bare) out.push((bare = []))
      bare.push(Number(m[2]))
    }
  }
  return out
}

// --- What a person types: a space is a plus («27 3 5» → 27+3+5), «-» a minus

export type ParseResult = { ok: true; terms: number[] } | { ok: false; reason: 'empty' | 'invalid' | 'first' }
/**
 * Terms typed in a count or formula box: "27 3 5" → [27, 3, 5], "27 -3" or
 * "27-3" → [27, −3], "=12+15" → [12, 15]; the minus may be −, – or -. A phone
 * may turn two spaces into ". ": a lone dot counts as a space. The first term
 * cannot be a loss.
 */
export function parseTerms(text: string): ParseResult {
  const s = text
    .replace(/[−–—]/g, '-')
    .replace(/^\s*=/, '')
    .replace(/\.(\s|$)/g, ' $1')
    .trim()
  if (!s) return { ok: false, reason: 'empty' }
  if (!/^[-+]?\s*\d{1,4}(?:\s*[-+]?\s*\d{1,4})*$/.test(s)) return { ok: false, reason: 'invalid' }
  const terms = [...s.matchAll(/([-+]?)\s*(\d{1,4})/g)].map(m => (m[1] === '-' ? -Number(m[2]) : Number(m[2])))
  if (terms[0] < 0) return { ok: false, reason: 'first' }
  return { ok: true, terms }
}
/** Numbers typed one after another, all counts: "6 5" or "6+5" → [6, 5] (a minus is not a count). */
export function parseCounts(text: string): number[] | null {
  const r = parseTerms(text)
  if (!r.ok || r.terms.some(t => t < 0)) return null
  return r.terms
}
export type FormulaResult = { ok: true; groups: Groups } | { ok: false; reason: 'empty' | 'invalid' | 'first' | 'negative' }
/**
 * A whole formula typed by hand: "=(6-2)+(5+3)", "(6 -2) (5 3)" or "27 -2 -11";
 * inside a group a space is a plus; between groups a space or a plus. Each
 * group starts with a gain and never adds up below 0.
 */
export function parseFormula(text: string): FormulaResult {
  const s = text.replace(/[−–—]/g, '-').replace(/^\s*=/, '').trim()
  if (!s) return { ok: false, reason: 'empty' }
  const parts: string[] = []
  let rest = s
  if (s.includes('(')) {
    while (rest.length) {
      const m = /^\s*\+?\s*\(([^()]*)\)\s*/.exec(rest)
      if (!m) return { ok: false, reason: 'invalid' }
      parts.push(m[1])
      rest = rest.slice(m[0].length)
    }
  } else parts.push(s)
  const groups: Groups = []
  for (const p of parts) {
    const r = parseTerms(p)
    if (!r.ok) return { ok: false, reason: r.reason }
    if (groupTotal(r.terms) < 0) return { ok: false, reason: 'negative' }
    groups.push(r.terms)
  }
  return { ok: true, groups }
}

// --- The groups of a count as the person sees them: label, totals, the app's record of each

/** What the app keeps of an open group (server/clutches.mjs clutch_groups), by position. */
export interface GroupRow {
  id: string
  recordId?: string
  field: string
  stage: Stage
  position: number
  label: string | null
  originId: string | null
  createdAt?: string
  endedAt?: string | null
}
/** A count's state for planning: its groups as written and what the app keeps of each, by position. */
export interface CountState {
  field: CountField
  stage: Stage
  groups: Groups
  meta: (GroupRow | null)[]
}
/**
 * The app's groups matched to a formula's parentheses by position. `mismatch`
 * when they do not agree (someone added or took away parentheses in Sheets, or
 * this person's formula is not saved yet): the labels stay by position.
 */
export function countState(field: CountField, stage: Stage, groups: Groups, rows: GroupRow[]): CountState & { mismatch: boolean } {
  const open = rows.filter(r => r.field === field && !r.endedAt).sort((a, b) => a.position - b.position)
  const n = Math.max(groups.length, 0)
  const meta = Array.from({ length: n }, (_, i) => open[i] ?? null)
  return { field, stage, groups, meta, mismatch: open.length > 0 && open.length !== n }
}
/** "A", "B"… the first letter no group uses. */
export function nextLabel(used: (string | null | undefined)[]): string {
  const taken = new Set(used.filter(Boolean).map(s => String(s).trim().toUpperCase()))
  for (let i = 0; i < 26; i++) {
    const l = String.fromCharCode(65 + i)
    if (!taken.has(l)) return l
  }
  return String(used.length + 1)
}
/** A group's name: its label, else its letter by position. */
export const groupName = (meta: GroupRow | null | undefined, index: number) => meta?.label || String.fromCharCode(65 + Math.min(index, 25))

/** Groups selected by taps: one tap toggles a group; `only` keeps just that one. */
export function toggleSelection(selected: number[], index: number, only = false): number[] {
  if (only) return selected.length === 1 && selected[0] === index ? [] : [index]
  return selected.includes(index) ? selected.filter(i => i !== index) : [...selected, index].sort((a, b) => a - b)
}

// --- Which event each term is

type Termed = Pick<ClutchEvent, 'id' | 'term' | 'groupId' | 'createdAt'> & { field?: string | null }
/**
 * The event behind each term of each group (its id, or null for a term written
 * before the app, by hand or in Sheets). A group's terms are matched with that
 * group's events (the first group also takes events without a group) of the
 * same value, from the newest term and event back, each event once.
 */
export function alignTerms(groups: Groups, events: Termed[], groupIds: (string | null)[]): (string | null)[][] {
  const used = new Set<string>()
  const sorted = [...events].filter(e => e.term !== null && e.term !== undefined).sort((a, b) => b.createdAt.localeCompare(a.createdAt))
  return groups.map((terms, p) => {
    const id = groupIds[p] ?? null
    const pool = sorted.filter(e => (id !== null && e.groupId === id) || (p === 0 && (e.groupId === null || e.groupId === undefined)))
    const out: (string | null)[] = terms.map(() => null)
    for (let i = terms.length - 1; i >= 0; i--) {
      const e = pool.find(x => !used.has(x.id) && x.term === terms[i])
      if (!e) continue
      used.add(e.id)
      out[i] = e.id
    }
    return out
  })
}

// --- Plans: what an action does to the formula, the groups and the events

/** An event to record with a step (server/clutches.mjs addClutchStep): a group by its id or by the key of a new one. */
export interface StepEvent {
  stage: Stage
  kind: EventKind
  count: number
  term: number | null
  groupId?: string | null
  groupKey?: string
  fromGroupId?: string | null
  fromGroupKey?: string
  day?: string
  dayKnown?: boolean
  ids?: string[]
  note?: string | null
}
export interface StepGroupItem {
  id?: string
  key?: string
  label: string | null
  originId?: string | null
}
export interface Plan {
  /** The counts' groups after it (only those that change). */
  counts: Partial<Record<CountField, Groups>>
  /** The open groups of each count it touches, by position (for the server). */
  groups: { field: CountField; list: StepGroupItem[] }[]
  events: StepEvent[]
  log: { kind: 'regroup' | 'formula'; field: CountField; before: string | null; after: string | null; note?: string | null }[]
}
export type PlanResult = { ok: true; plan: Plan } | { ok: false; reason: 'empty' | 'first' | 'negative' | 'sum' | 'unchanged' }

let keys = 0
const newKey = () => `g${Date.now().toString(36)}${++keys}`
/** The groups of a count as a list for the server, with a label for those without (by letter). */
function listFor(meta: (GroupRow | null)[], extra: Map<number, { key: string; label: string | null; originId: string | null }> = new Map()): StepGroupItem[] {
  const used = meta.map((m, i) => extra.get(i)?.label ?? m?.label ?? null)
  return meta.map((m, i) => {
    const x = extra.get(i)
    if (x) return { key: x.key, label: x.label, originId: x.originId }
    if (m) return { id: m.id, label: m.label }
    const label = nextLabel(used)
    used[i] = label
    return { key: newKey(), label, originId: null }
  })
}
const refOf = (item: StepGroupItem | undefined, meta: GroupRow | null | undefined) =>
  item?.id ? { groupId: item.id } : item?.key ? { groupKey: item.key } : meta ? { groupId: meta.id } : { groupId: null }

/** A lone 0 (=0) is replaced by what comes next. */
const base = (groups: Groups) => (groups.length === 1 && groups[0].length === 1 && groups[0][0] === 0 ? [[]] : groups.map(g => [...g]))

/**
 * One more term in one group (the last when none is said): +5 hatched into box
 * A, −2 died in box B. A group starts with a gain and never goes below 0.
 */
export function appendInGroup(groups: Groups, index: number | null, term: number): { ok: true; groups: Groups } | { ok: false; reason: 'empty' | 'first' | 'negative' } {
  if (!Number.isInteger(term) || term === 0) return { ok: false, reason: 'empty' }
  const out = base(groups.length ? groups : [[]])
  const at = index === null || index < 0 || index >= out.length ? out.length - 1 : index
  if (!out[at].length && term < 0) return { ok: false, reason: 'first' }
  if (groupTotal(out[at]) + term < 0) return { ok: false, reason: 'negative' }
  out[at].push(term)
  return { ok: true, groups: out }
}

/** Whether a stage's count holds groups the app knows (a list to send) or only the one implicit group. */
const grouped = (s: CountState) => s.groups.length > 1 || s.meta.some(Boolean)

/**
 * A gain, a loss or a correction in one group of a count (`index`: the group;
 * the last when none): the term goes inside that group's parentheses and the
 * event names the group. `term` null: an event without a term (preserved ones
 * kept counted, eggs that did not hatch).
 */
export function planTerm(
  s: CountState,
  index: number | null,
  e: Omit<StepEvent, 'term' | 'groupId' | 'groupKey'> & { term: number | null },
): PlanResult {
  const at = s.groups.length ? (index === null || index < 0 || index >= s.groups.length ? s.groups.length - 1 : index) : 0
  const meta = s.meta[at] ?? null
  const event: StepEvent = { ...e, ...(meta ? { groupId: meta.id } : { groupId: null }) }
  if (e.term === null) return { ok: true, plan: { counts: {}, groups: [], events: [event], log: [] } }
  const r = appendInGroup(s.groups, at, e.term)
  if (!r.ok) return r
  return { ok: true, plan: { counts: { [s.field]: r.groups }, groups: [], events: [event], log: [] } }
}

/**
 * The total typed over the count's total (counted 10, not 11): the difference
 * as its own term, a correction, in the group chosen (the last when none).
 */
export function planCorrection(s: CountState, index: number | null, counted: number, note: string | null = null): PlanResult {
  if (!Number.isInteger(counted) || counted < 0) return { ok: false, reason: 'empty' }
  const diff = counted - groupsTotal(s.groups)
  if (diff === 0) return { ok: false, reason: 'unchanged' }
  if (!s.groups.length || flatTerms(s.groups).length === 0 || (s.groups.length === 1 && s.groups[0].length === 1 && s.groups[0][0] === 0))
    return planTerm({ ...s, groups: [[]] }, 0, { stage: s.stage, kind: 'correction', count: counted, term: counted, note })
  return planTerm(s, index, { stage: s.stage, kind: 'correction', count: Math.abs(diff), term: diff, note })
}

/**
 * A term taken out of the sum (its event deleted): the group keeps its other
 * terms; a group left without terms goes (its parentheses and its place).
 */
export function planRemove(s: CountState, group: number, index: number): { ok: true; groups: Groups; list: StepGroupItem[] | null } | { ok: false; reason: 'first' | 'negative' } {
  const out = s.groups.map(g => [...g])
  if (!out[group] || index < 0 || index >= out[group].length) return { ok: true, groups: out, list: null }
  out[group].splice(index, 1)
  if (out[group].length && out[group][0] < 0) return { ok: false, reason: 'first' }
  if (groupTotal(out[group]) < 0) return { ok: false, reason: 'negative' }
  if (out[group].length) return { ok: true, groups: out, list: null }
  const meta = s.meta.filter((_, i) => i !== group)
  return { ok: true, groups: out.filter((_, i) => i !== group), list: grouped(s) ? listFor(meta) : null }
}

/**
 * Regrouping, written as transfers so the history and the total stay: each
 * group (by position) to its new count, groups after the last count merged
 * away, new ones added at the end. A group that gives keeps its parentheses
 * with a −N; one merged away gives all it has and its parentheses go; a group
 * that receives gets +N (a new one starts with it). "6+5" from one group of 11
 * (=27-2-11-3) → =(27-2-11-3-5)+(5). Refused unless the counts add up to the total.
 */
export function planRegroup(s: CountState, targets: number[], labels: (string | null)[] = []): PlanResult {
  if (!targets.length || targets.some(t => !Number.isInteger(t) || t < 0)) return { ok: false, reason: 'empty' }
  const totals = s.groups.map(groupTotal)
  if (targets.reduce((a, b) => a + b, 0) !== groupsTotal(s.groups)) return { ok: false, reason: 'sum' }
  const n = s.groups.length
  // New groups start with something; a group kept may be emptied only by merging it away.
  if (targets.slice(n).some(t => t === 0)) return { ok: false, reason: 'empty' }
  const want = Array.from({ length: Math.max(n, targets.length) }, (_, i) => (i < targets.length ? targets[i] : 0))
  const dropped = new Set<number>()
  for (let i = 0; i < n; i++) if (i >= targets.length) dropped.add(i)
  const same = want.length === n && want.every((t, i) => t === totals[i])
  if (same && labels.every((l, i) => !l || l === (s.meta[i]?.label ?? null))) return { ok: false, reason: 'unchanged' }
  const have = want.map((_, i) => (i < n ? totals[i] : 0))
  const givers = want.map((t, i) => have[i] - t).map((d, i) => ({ i, d })).filter(x => x.d > 0)
  const takers = want.map((t, i) => t - have[i]).map((d, i) => ({ i, d })).filter(x => x.d > 0)
  const transfers: { from: number; to: number; count: number }[] = []
  for (const g of givers) {
    let left = g.d
    for (const k of takers) {
      if (!left) break
      const x = Math.min(left, k.d)
      if (!x) continue
      transfers.push({ from: g.i, to: k.i, count: x })
      k.d -= x
      left -= x
    }
  }
  // The groups' labels and app records after it: kept by position, new ones keyed.
  const used = s.meta.map(m => m?.label ?? null)
  const items: StepGroupItem[] = want.map((_, i) => {
    const typed = labels[i]?.trim() || null
    if (i < n && s.meta[i]) return { id: s.meta[i]!.id, label: typed ?? s.meta[i]!.label }
    const label = typed ?? nextLabel(used)
    used[i] = label
    return { key: newKey(), label, originId: null }
  })
  const out: Groups = want.map((_, i) => (i < n ? [...s.groups[i]] : []))
  const events: StepEvent[] = []
  for (const t of transfers) {
    if (!dropped.has(t.from)) {
      out[t.from].push(-t.count)
      events.push({ stage: s.stage, kind: 'transfer', count: t.count, term: -t.count, ...refOf(items[t.from], s.meta[t.from]), ...fromRef(items[t.to]) })
    }
    out[t.to].push(t.count)
    events.push({ stage: s.stage, kind: 'transfer', count: t.count, term: t.count, ...refOf(items[t.to], s.meta[t.to]), ...fromRef(items[t.from]) })
  }
  const keep = want.map((_, i) => !dropped.has(i))
  const groups = out.filter((_, i) => keep[i])
  const list = items.filter((_, i) => keep[i])
  return {
    ok: true,
    plan: {
      counts: { [s.field]: groups },
      groups: [{ field: s.field, list }],
      events,
      log: [{ kind: 'regroup', field: s.field, before: formulaOfGroups(s.groups), after: formulaOfGroups(groups) }],
    },
  }
}
/**
 * Part of one group moved to a new group right after it (2 of box A's larvae
 * put in a box of their own): A keeps its parentheses with a −2, the new group
 * starts with (2). =(6-2)+(5+3), 2 from A → =(6-2-2)+(2)+(5+3).
 */
export function planSplit(s: CountState, index: number, count: number, label: string | null = null): PlanResult {
  const g = s.groups[index]
  if (!g || !Number.isInteger(count) || count < 1) return { ok: false, reason: 'empty' }
  if (count > groupTotal(g)) return { ok: false, reason: 'negative' }
  const groups = s.groups.map(x => [...x])
  groups[index].push(-count)
  groups.splice(index + 1, 0, [count])
  const meta = [...s.meta]
  while (meta.length < s.groups.length) meta.push(null)
  const used = meta.map(m => m?.label ?? null)
  const key = newKey()
  const extra = new Map([[index + 1, { key, label: label?.trim() || null, originId: null }]])
  meta.splice(index + 1, 0, null)
  if (!extra.get(index + 1)!.label) extra.get(index + 1)!.label = nextLabel([...used, ...meta.map((m, i) => (i === index + 1 ? null : m?.label ?? null))])
  const list = listFor(meta, extra)
  const source = list[index]
  return {
    ok: true,
    plan: {
      counts: { [s.field]: groups },
      groups: [{ field: s.field, list }],
      events: [
        { stage: s.stage, kind: 'transfer', count, term: -count, ...refOf(source, s.meta[index]), fromGroupKey: key },
        { stage: s.stage, kind: 'transfer', count, term: count, groupKey: key, ...fromRef(source) },
      ],
      log: [{ kind: 'regroup', field: s.field, before: formulaOfGroups(s.groups), after: formulaOfGroups(groups) }],
    },
  }
}
const fromRef = (item: StepGroupItem | undefined) => (item?.id ? { fromGroupId: item.id } : item?.key ? { fromGroupKey: item.key } : {})

/** What is left of each group of a stage that has not moved on: the group's total less those hatched (pupated) from it and, eggs, those that never hatched. */
export function leftOf(
  s: CountState,
  events: Pick<ClutchEvent, 'stage' | 'kind' | 'count' | 'groupId' | 'fromGroupId'>[],
  next: Stage | null,
): number[] {
  const ids = s.meta.map(m => m?.id ?? null)
  const position = (id: string | null | undefined) => {
    const at = id ? ids.indexOf(id) : -1
    return at >= 0 ? at : 0
  }
  const out = s.groups.map(groupTotal)
  for (const e of events) {
    if (next && e.stage === next && (e.kind === 'hatched' || e.kind === 'pupated')) out[position(e.fromGroupId)] -= e.count
    else if (e.stage === s.stage && e.kind === 'not_hatched') out[position(e.groupId)] -= e.count
  }
  return out.map(n => Math.max(0, n))
}

/**
 * Hatched (or pupated) from groups of the stage before: each group chosen gives
 * its count to the next stage, in a group of its own that mirrors it (box A's
 * eggs → box A's larvae; the same one again on a later day), with one +N term
 * and its event each. Eggs that did not hatch (`notHatched`) are recorded on
 * their group, without a term (the eggs laid stay counted). A count without
 * groups the app knows of, from the implicit group, just gets its +N.
 */
export function planMoveOn(
  from: CountState,
  to: CountState,
  items: { index: number; count: number; notHatched?: number }[],
  { day, dayKnown = true }: { day?: string; dayKnown?: boolean } = {},
): PlanResult {
  const kind: EventKind = to.stage === 'larva' ? 'hatched' : 'pupated'
  if (items.some(x => !Number.isInteger(x.count) || x.count < 0 || !Number.isInteger(x.notHatched ?? 0) || (x.notHatched ?? 0) < 0))
    return { ok: false, reason: 'empty' }
  const wanted = items.filter(x => x.count > 0 || (x.notHatched ?? 0) > 0)
  if (!wanted.length) return { ok: false, reason: 'empty' }
  const groups: Groups = to.groups.length === 1 && to.groups[0].length === 1 && to.groups[0][0] === 0 ? [] : to.groups.map(g => [...g])
  const meta: (GroupRow | null)[] = groups.map((_, i) => to.meta[i] ?? null)
  const named = meta.some(Boolean)
  const added = new Map<number, { key: string; label: string | null; originId: string | null }>()
  const events: (StepEvent & { at?: number })[] = []
  const labels = () => [...meta.map(m => m?.label ?? null), ...[...added.values()].map(v => v.label)]
  for (const x of wanted) {
    const source = from.meta[x.index] ?? null
    if (x.count > 0) {
      // The group that came from the same one before (box A's eggs → box A's larvae), on any day.
      let at = source ? meta.findIndex(m => m?.originId === source.id) : -1
      if (at < 0 && source) at = [...added.entries()].find(([, v]) => v.originId === source.id)?.[0] ?? -1
      if (at < 0 && (source || groups.length > 1 || named)) {
        // A group of its own, named as the one it came from (or the next letter).
        at = groups.length
        groups.push([])
        meta.push(null)
        added.set(at, { key: newKey(), label: source ? groupName(source, x.index) : nextLabel(labels()), originId: source?.id ?? null })
      }
      if (at < 0) {
        if (!groups.length) {
          groups.push([])
          meta.push(null)
        }
        at = groups.length - 1
      }
      groups[at].push(x.count)
      events.push({ stage: to.stage, kind, count: x.count, term: x.count, at, fromGroupId: source?.id ?? null, ...(day ? { day } : {}), dayKnown })
    }
    if ((x.notHatched ?? 0) > 0)
      events.push({ stage: from.stage, kind: 'not_hatched', count: x.notHatched!, term: null, groupId: source?.id ?? null })
  }
  const list = added.size ? listFor(meta, added) : null
  for (const e of events) {
    if (e.at === undefined) continue
    const item = list?.[e.at]
    if (item?.key) e.groupKey = item.key
    else e.groupId = item?.id ?? meta[e.at]?.id ?? null
    delete e.at
  }
  return {
    ok: true,
    plan: { counts: { [to.field]: groups }, groups: list ? [{ field: to.field, list }] : [], events, log: [] },
  }
}

/** Every count of a step in one place: plans one after another (a hatch and its eggs' not-hatched). */
export function mergePlans(a: Plan, b: Plan): Plan {
  return { counts: { ...a.counts, ...b.counts }, groups: [...a.groups, ...b.groups], events: [...a.events, ...b.events], log: [...a.log, ...b.log] }
}

/** The groups a regroup's text gives: "6+5" or "6 5" → [6, 5]; with the total they must reach. */
export function regroupTargets(text: string, total: number): { ok: true; targets: number[] } | { ok: false; reason: 'empty' | 'sum'; sum?: number } {
  const counts = parseCounts(text)
  if (!counts || !counts.length) return { ok: false, reason: 'empty' }
  const sum = counts.reduce((a, b) => a + b, 0)
  if (sum !== total) return { ok: false, reason: 'sum', sum }
  return { ok: true, targets: counts }
}
