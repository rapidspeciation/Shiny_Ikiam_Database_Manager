/**
 * A commit's change to an instructions file, as git's word diff gives it
 * (`git log --word-diff=porcelain`, server/instructions.mjs): each line of the
 * new text with the words added and removed in it, so a reworded paragraph
 * shows only its changed words. Long unchanged stretches fold into one row.
 */

export interface Segment {
  kind: 'ctx' | 'add' | 'del'
  text: string
}
export type DiffRow =
  | { type: 'line'; change: 'ctx' | 'add' | 'del' | 'mod'; segments: Segment[] }
  | { type: 'hunk'; line: number; section: string }
  | { type: 'skip'; count: number }
  | { type: 'note'; text: string }
export interface DiffFile {
  path: string | null
  rows: DiffRow[]
}

/** Unchanged lines kept around a change. */
const CONTEXT = 3

function lineChange(segments: Segment[]): 'ctx' | 'add' | 'del' | 'mod' {
  const kinds = new Set(segments.filter(s => s.kind !== 'ctx' || s.text.trim()).map(s => s.kind))
  if (!kinds.has('add') && !kinds.has('del')) return 'ctx'
  if (kinds.size === 1) return kinds.has('add') ? 'add' : 'del'
  return 'mod'
}

/** Folds unchanged runs longer than 2 × CONTEXT + 1 (keeping CONTEXT lines next to each change). */
export function fold(rows: DiffRow[], context = CONTEXT): DiffRow[] {
  const out: DiffRow[] = []
  let run: DiffRow[] = []
  const flush = (before: boolean, after: boolean) => {
    const head = before ? context : 0
    const tail = after ? context : 0
    if (run.length > head + tail + 1) out.push(...run.slice(0, head), { type: 'skip', count: run.length - head - tail }, ...run.slice(run.length - tail))
    else out.push(...run)
    run = []
  }
  let changed = false // a change right before the current run
  for (const row of rows) {
    if (row.type === 'line' && row.change === 'ctx') {
      run.push(row)
      continue
    }
    const change = row.type === 'line'
    flush(changed, change)
    out.push(row)
    changed = change
  }
  flush(changed, false)
  return out
}

/** Parses a word diff (one or several files) into rows to show. */
export function parseWordDiff(text: string, context = CONTEXT): DiffFile[] {
  const files: DiffFile[] = []
  let file: DiffFile | null = null
  let header = false
  let segments: Segment[] = []
  const current = () => {
    if (!file) {
      file = { path: null, rows: [] }
      files.push(file)
    }
    return file
  }
  const endLine = () => {
    current().rows.push({ type: 'line', change: lineChange(segments), segments })
    segments = []
  }
  for (const raw of text.split('\n')) {
    const diffHead = /^diff --git a\/(.*) b\/(.*)$/.exec(raw)
    if (diffHead) {
      if (segments.length) endLine()
      file = { path: diffHead[2], rows: [] }
      files.push(file)
      header = true
      continue
    }
    const hunk = /^@@ -\d+(?:,\d+)? \+(\d+)(?:,\d+)? @@ ?(.*)$/.exec(raw)
    if (hunk) {
      if (segments.length) endLine()
      header = false
      current().rows.push({ type: 'hunk', line: Number(hunk[1]), section: hunk[2] })
      continue
    }
    if (header) {
      if (/^new file mode/.test(raw)) current().rows.push({ type: 'note', text: 'new' })
      else if (/^deleted file mode/.test(raw)) current().rows.push({ type: 'note', text: 'deleted' })
      else if (/^rename from /.test(raw)) current().rows.push({ type: 'note', text: `renamed:${raw.slice(12)}` })
      continue
    }
    if (raw === '~') endLine()
    else if (raw.startsWith('\\')) {
      if (!/No newline at end of file/.test(raw)) current().rows.push({ type: 'note', text: raw.slice(1).trim() })
    } else if (raw[0] === ' ' || raw[0] === '+' || raw[0] === '-')
      segments.push({ kind: raw[0] === ' ' ? 'ctx' : raw[0] === '+' ? 'add' : 'del', text: raw.slice(1) })
  }
  if (segments.length) endLine()
  return files.map(f => ({ ...f, rows: fold(f.rows, context) }))
}

/** A block of a section's diff read as text (ProseDiff): a paragraph whose source lines are joined, or a code line. */
export type ProseBlock =
  | { type: 'para'; segments: Segment[]; changed: boolean; marker: 'bullet' | 'heading' | string | null }
  | { type: 'code'; segments: Segment[]; changed: boolean }
  | { type: 'skip'; count: number }
  | { type: 'note'; text: string }

/** A line that starts a block of its own in Markdown: a heading, a list item, a table row, a quote. */
const BLOCK_START = /^\s*(?:#{1,6}\s|[-*+]\s|\d+[.)]\s|\||>)/
const FENCE = /^\s*(?:```|~~~)/
/** The Markdown mark a block starts with: a heading's #, a list item's -, * or 1. (taken out of the text). */
const MARKER = /^\s*(#{1,6}\s+|[-*+]\s+|\d+[.)]\s+)/

/** Takes a block's Markdown mark out of its first words: 'heading', 'bullet', the item's number ('2.') or null. */
function takeMarker(segments: Segment[]): string | null {
  const first = segments.find(s => s.text.trim())
  const m = first && MARKER.exec(first.text)
  if (!first || !m) return null
  first.text = first.text.slice(m[0].length)
  const mark = m[1].trim()
  return mark.startsWith('#') ? 'heading' : /^\d/.test(mark) ? mark : 'bullet'
}

/**
 * The rows of a word diff as prose: lines wrapped in the source join into one paragraph (with a space), blank
 * lines and Markdown block starts begin a new one, fenced code stays line by line. Hunk headers are dropped, and
 * headings and list items lose their Markdown mark (`marker` says which it was).
 */
export function proseBlocks(rows: DiffRow[]): ProseBlock[] {
  const out: ProseBlock[] = []
  let para: Extract<ProseBlock, { type: 'para' }> | null = null
  let fence = false
  for (const row of rows) {
    if (row.type === 'hunk') continue
    if (row.type !== 'line') {
      para = null
      out.push(row)
      continue
    }
    const text = row.segments.map(s => s.text).join('')
    const changed = row.change !== 'ctx'
    if (FENCE.test(text) || fence) {
      if (FENCE.test(text)) fence = !fence
      para = null
      out.push({ type: 'code', segments: row.segments, changed })
      continue
    }
    if (!text.trim()) {
      para = null
      continue
    }
    if (!para || BLOCK_START.test(text)) {
      const segments = row.segments.map(s => ({ ...s }))
      para = { type: 'para', segments, changed, marker: takeMarker(segments) }
      out.push(para)
      continue
    }
    para.segments.push({ kind: 'ctx', text: ' ' }, ...row.segments)
    para.changed ||= changed
  }
  return out
}

export interface InlinePiece {
  text: string
  code: boolean
  bold: boolean
}
/**
 * Each segment's text as the doc shows it: `code` spans without their backticks and **bold** without its stars
 * (a span may run across segments: the word diff splits at spaces), links as their text.
 */
export function inlinePieces(segments: Segment[]): InlinePiece[][] {
  let code = false
  let bold = false
  return segments.map(seg => {
    const pieces: InlinePiece[] = []
    const text = seg.text.replace(/\[([^\]]*)\]\([^)\s]*\)/g, '$1')
    for (const part of text.split(/(`|\*\*)/)) {
      if (part === '`') code = !code
      else if (part === '**' && !code) bold = !bold
      else if (part) pieces.push({ text: part, code, bold })
    }
    return pieces
  })
}
