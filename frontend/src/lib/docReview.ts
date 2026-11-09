/**
 * Comments on the instructions and Claude's changes to them (#/instrucciones, server/doc-review.mjs):
 * the page's state from GET api/doc-review, and the DOM work on the rendered text — where a selection is
 * (quote, 32 characters before and after, offset: Franz's md-review scheme), placing a thread again,
 * highlighting it, and stacking the margin's cards beside their highlights.
 */
import { reactive } from 'vue'
import { api } from './api'

export interface DocComment {
  id: string
  body: string
  createdAt: string
  editedAt: string | null
  deleted: boolean
  kind: 'person' | 'claude' | 'system'
  author: string | null
  own: boolean
}
export interface DocThread {
  id: string
  path: string
  quote: string
  prefix: string
  suffix: string
  offset: number
  heading: string
  status: 'open' | 'resolved'
  createdAt: string
  own: boolean
  resolvedAt: string | null
  resolvedBy: string | null
  resolvedByChange: string | null
  comments: DocComment[]
}
export interface DocRevision {
  id: number
  kind: 'apply' | 'undo'
  gitState: 'pending' | 'committed' | 'pushed' | 'failed' | 'off'
  gitCommit: string | null
  gitError: string | null
  appliedAt: string
  appliedBy: string | null
  undoneAt: string | null
}
export interface DocCheck {
  code: 'front_matter' | 'placeholder' | 'guide_link' | 'route' | 'stale' | 'check_failed'
  file?: string
  text?: string
  route?: string
}
export interface DocChangeVersion {
  id: string
  version: number
  before: string
  after: string
  diff: string | null
  createdAt: string
  status: string
}
export interface DocChange {
  id: string
  seriesId: string
  version: number
  path: string
  heading: string
  before: string
  after: string
  diff: string | null
  threadIds: string[]
  status: 'pending' | 'applied' | 'undone' | 'discarded' | 'superseded'
  stale: boolean
  lost: boolean
  chat: string | null
  createdAt: string
  decidedAt: string | null
  decidedBy: string | null
  revision: DocRevision | null
  budget: { part: string; bytes: number }[]
  checks: DocCheck[]
  versions: DocChangeVersion[]
  comments: DocComment[]
}
export interface DocAsk {
  state: 'idle' | 'running' | 'finished' | 'gone'
  chat: string | null
  at: string | null
}
export interface DocReviewState {
  admin: boolean
  threads: DocThread[]
  changes: DocChange[]
  counts: Record<string, number>
  ask: DocAsk
}
export interface Anchor {
  quote: string
  prefix: string
  suffix: string
  offset: number
  heading: string
}

/** Characters kept before and after a quote. */
export const CONTEXT = 32
/** The brief as the page shows it is AGENTS.md and the app's voice (server/brief.mjs). */
export const sourcesOf = (id: string) => (id === 'assistant/AGENTS.md' ? [id, 'assistant/voice/app.md', 'assistant/voice/core.md'] : [id])

// ------------------------------------------------------------------ the state

export const review = reactive({
  state: null as DocReviewState | null,
  failed: '',
})
let loading: Promise<void> | null = null
/** Reads the state again (one request at a time). */
export function loadReview(): Promise<void> {
  loading ??= api<DocReviewState>('doc-review')
    .then(s => {
      review.state = s
      review.failed = ''
    })
    .catch(e => {
      review.failed = String((e as Error)?.message ?? e)
    })
    .finally(() => (loading = null))
  return loading
}
/** A call that changes something, then the state again. */
export async function act<T>(path: string, method = 'POST', body: unknown = {}): Promise<T> {
  const out = await api<T>(`doc-review/${path}`, { method, body })
  await loadReview()
  return out
}

/** What a file shows: its threads, and its changes (the brief's include the voice's). */
export function forEntry(state: DocReviewState | null, id: string) {
  const sources = sourcesOf(id)
  return {
    threads: (state?.threads ?? []).filter(t => t.path === id),
    changes: (state?.changes ?? []).filter(c => sources.includes(c.path)),
  }
}
/** The quiet count beside a file in the list: its open threads and pending changes. */
export const countFor = (state: DocReviewState | null, id: string) => sourcesOf(id).reduce((n, p) => n + (state?.counts?.[p] ?? 0), 0)
/** Open threads across every file (the bar's «Pedir a Claude · n»). */
export const openCount = (state: DocReviewState | null) => (state?.threads ?? []).filter(t => t.status === 'open').length

/** A change still to decide on, or one decided in the last day (its card stays in the margin for Deshacer). */
export function shownInMargin(c: DocChange, now = Date.now()) {
  if (c.status === 'pending' || c.lost || c.revision?.gitState === 'failed') return true
  return (c.status === 'applied' || c.status === 'undone') && !!c.decidedAt && now - Date.parse(c.decidedAt) < 24 * 3600_000
}

// ------------------------------------------------------------------ the text

/** Text nodes of the rendered text, leaving out what the page adds to it (pins, data-doc-ui). */
export function textNodes(root: Node): Text[] {
  const out: Text[] = []
  const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT, {
    acceptNode: n => ((n.parentElement?.closest('[data-doc-ui]') ? NodeFilter.FILTER_REJECT : NodeFilter.FILTER_ACCEPT)),
  })
  for (let n = walker.nextNode(); n; n = walker.nextNode()) out.push(n as Text)
  return out
}
export const fullText = (root: Node) => textNodes(root).map(n => n.data).join('')
function offsetOf(root: Node, node: Node, offset: number) {
  let at = 0
  for (const n of textNodes(root)) {
    if (n === node) return at + offset
    at += n.data.length
  }
  return -1
}

/** Where a thread's quote is in the text: matches scored by prefix and suffix, ties to the nearest offset (-1: gone). */
export function locate(text: string, a: Pick<Anchor, 'quote' | 'prefix' | 'suffix' | 'offset'>): number {
  if (!a.quote) return -1
  let best = -1
  let score = -Infinity
  for (let i = text.indexOf(a.quote); i >= 0; i = text.indexOf(a.quote, i + 1)) {
    let s = 0
    if (a.prefix && text.slice(Math.max(0, i - a.prefix.length), i) === a.prefix) s += 2
    if (a.suffix && text.slice(i + a.quote.length, i + a.quote.length + a.suffix.length) === a.suffix) s += 2
    s -= Math.abs(i - (a.offset ?? 0)) / 1e6
    if (s > score) {
      score = s
      best = i
    }
  }
  return best
}

const HEADINGS = 'h1, h2, h3, h4, h5, h6'
/** The heading the node is under (the last one before it), as its text. */
export function headingBefore(root: Element, node: Node): string {
  let found = ''
  for (const h of root.querySelectorAll(HEADINGS)) {
    const pos = h.compareDocumentPosition(node)
    if (h === node || h.contains(node) || pos & Node.DOCUMENT_POSITION_FOLLOWING) found = h.textContent?.trim() ?? ''
    else break
  }
  return found
}

/** The selection inside `root` as an anchor (null: none, or outside it). */
export function selectionAnchor(root: Element, selection: Selection | null = window.getSelection()): (Anchor & { rect: DOMRect | null }) | null {
  if (!selection || !selection.rangeCount || selection.isCollapsed) return null
  const range = selection.getRangeAt(0)
  if (!root.contains(range.commonAncestorContainer)) return null
  const raw = selection.toString()
  const quote = raw.trim()
  if (!quote) return null
  const text = fullText(root)
  let start = range.startContainer.nodeType === Node.TEXT_NODE ? offsetOf(root, range.startContainer, range.startOffset) : -1
  if (start >= 0) start += raw.length - raw.trimStart().length
  if (start < 0 || text.slice(start, start + quote.length) !== quote) start = text.indexOf(quote)
  // A selection across blocks: the browser's text has line breaks the text nodes may not.
  if (start < 0) {
    const flat = quote.replace(/\s+/g, ' ')
    start = text.replace(/\s+/g, ' ').indexOf(flat)
    if (start < 0) return null
  }
  const rect = typeof range.getBoundingClientRect === 'function' ? range.getBoundingClientRect() : null
  return {
    quote: text.slice(start, start + quote.length) === quote ? quote : quote.replace(/\s+/g, ' '),
    offset: start,
    prefix: text.slice(Math.max(0, start - CONTEXT), start),
    suffix: text.slice(start + quote.length, start + quote.length + CONTEXT),
    heading: headingBefore(root, range.startContainer),
    rect,
  }
}

/** Unwraps every highlight the page put in the text. */
export function clearMarks(root: Element) {
  for (const m of root.querySelectorAll('mark[data-thread]')) m.replaceWith(...m.childNodes)
  for (const p of root.querySelectorAll('[data-doc-ui]')) p.remove()
  root.normalize()
}
/** Wraps the text from `start` (length `length`) in marks for a thread; the marks made. */
export function markRange(root: Element, start: number, length: number, id: string): HTMLElement[] {
  const end = start + length
  const marks: HTMLElement[] = []
  let at = 0
  for (const n of textNodes(root)) {
    const a = at
    const b = at + n.data.length
    at = b
    if (b <= start || a >= end) continue
    const s = Math.max(start, a) - a
    const e = Math.min(end, b) - a
    if (e <= s || !n.data.slice(s, e).trim()) continue
    const range = document.createRange()
    range.setStart(n, s)
    range.setEnd(n, e)
    const mark = document.createElement('mark')
    mark.className = 'doc-mark'
    mark.dataset.thread = id
    try {
      range.surroundContents(mark)
      marks.push(mark)
    } catch {
      /* a range across elements: that piece stays unmarked */
    }
  }
  return marks
}

/** The heading elements by their text, and the section each one opens (to the next heading of its level or higher). */
export function sectionElements(root: Element, heading: string): { head: Element | null; end: Element | null } {
  const all = [...root.querySelectorAll(HEADINGS)]
  const plain = (s: string) => s.replace(/[`*_]/g, '').replace(/\s+/g, ' ').trim()
  const head = all.find(h => plain(h.textContent ?? '') === plain(heading)) ?? null
  if (!head) return { head: null, end: null }
  const level = Number(head.tagName[1])
  const end = all.slice(all.indexOf(head) + 1).find(h => Number(h.tagName[1]) <= level) ?? null
  return { head, end }
}

/**
 * The margin's cards: each at its wanted top, pushed down past the one above it (8 px apart); the active one
 * stays at its own top and those above it move up. `items`: in the order of their tops.
 */
export function stackCards(items: { id: string; top: number; height: number }[], active: string | null = null, gap = 8): Record<string, number> {
  const sorted = [...items].sort((a, b) => a.top - b.top)
  const out: Record<string, number> = {}
  const pivot = active ? sorted.findIndex(i => i.id === active) : -1
  if (pivot < 0) {
    let bottom = -Infinity
    for (const i of sorted) {
      const top = Math.max(i.top, bottom + gap)
      out[i.id] = top
      bottom = top + i.height
    }
    return out
  }
  out[sorted[pivot].id] = sorted[pivot].top
  let bottom = sorted[pivot].top + sorted[pivot].height
  for (const i of sorted.slice(pivot + 1)) {
    const top = Math.max(i.top, bottom + gap)
    out[i.id] = top
    bottom = top + i.height
  }
  let ceiling = sorted[pivot].top
  for (const i of sorted.slice(0, pivot).reverse()) {
    const top = Math.min(i.top, ceiling - gap - i.height)
    out[i.id] = top
    ceiling = top
  }
  return out
}
