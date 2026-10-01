/**
 * A small Markdown renderer for the AI instructions page: headings (with ids
 * for a table of contents), paragraphs, nested lists, pipe tables, fenced code,
 * block quotes, rules, and inline code, bold, italics and links. Every text is
 * escaped: the output is safe for v-html. Relative links (a skill's
 * reference/…md) go through `link`, so they open inside the page.
 */

export interface Heading {
  level: number
  text: string
  id: string
}
export interface Rendered {
  html: string
  headings: Heading[]
}
export interface MarkdownOptions {
  /** A relative link → the address to open (inside the app), or null to show its text only. */
  link?: (href: string) => string | null
}

export const escapeHtml = (text: string) =>
  text.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;')

/** A heading's id: its words in lowercase joined by dashes ("Death, preserved" → death-preserved). */
export function slug(text: string): string {
  return (
    text
      .toLowerCase()
      .normalize('NFD')
      .replace(/[̀-ͯ]/g, '')
      .replace(/[`*]/g, '')
      .replace(/[^a-z0-9]+/g, '-')
      .replace(/^-+|-+$/g, '') || 'section'
  )
}

// ------------------------------------------------------------------ inline

const EXTERNAL = /^(https?:|mailto:)/i

/** Inline Markdown → HTML: `code`, [text](url), **bold**, *italic* / _italic_, bare https:// links. */
export function inline(text: string, options: MarkdownOptions = {}): string {
  const held: string[] = []
  const hold = (html: string) => `\u0000${held.push(html) - 1}\u0000`
  // Code spans and links first (whichever starts first): nothing inside code is formatted.
  let out = text.replace(/(`+)([\s\S]*?[^`])\1(?!`)|\[([^\]\n]+)\]\(([^)\s]+)\)/g, (_, _ticks, code?: string, label?: string, href?: string) => {
    if (code !== undefined) return hold(`<code>${escapeHtml(code.trim() || code)}</code>`)
    const inner = inline(label!, { ...options, link: () => null })
    if (EXTERNAL.test(href!)) return hold(`<a href="${escapeHtml(href!)}" target="_blank" rel="noopener">${inner}</a>`)
    const target = href!.startsWith('#') ? href! : (options.link?.(href!) ?? null)
    return hold(target ? `<a href="${escapeHtml(target)}">${inner}</a>` : inner)
  })
  out = escapeHtml(out)
  out = out.replace(/\*\*(?=\S)([\s\S]*?\S)\*\*/g, '<strong>$1</strong>')
  out = out.replace(/(^|[^\w*])\*(?=\S)([^*]*?\S)\*(?![\w*])/g, '$1<em>$2</em>')
  // _italic_ only between spaces or punctuation: Insectary_ID and Tube_1_id stay as they are.
  out = out.replace(/(^|[\s(«"])_(?=\S)([^_]*?\S)_(?=$|[\s).,;:!?»"])/g, '$1<em>$2</em>')
  out = out.replace(/https?:\/\/[^\s()\u0000]+?(?=[.,;:!?»]*(?:\s|$|&lt;|&gt;|\)|\u0000))/g, url => hold(`<a href="${url}" target="_blank" rel="noopener">${url}</a>`))
  return out.replace(/\u0000(\d+)\u0000/g, (_, i: string) => held[Number(i)])
}

// ------------------------------------------------------------------ blocks

const FENCE = /^\s*(```+|~~~+)\s*([\w-]*)\s*$/
const HEADING = /^(#{1,6})\s+(.*?)\s*#*\s*$/
const RULE = /^\s*([-*_])(\s*\1){2,}\s*$/
const ITEM = /^(\s*)([-*+]|\d{1,4}[.)])\s+(.*)$/
const QUOTE = /^\s*>\s?/
const TABLE_RULE = /^\s*\|?\s*:?-{2,}:?\s*(\|\s*:?-{2,}:?\s*)*\|?\s*$/

/** A table row's cells: split on | outside code spans (`a | b` stays one cell); \| is a bar. */
function cells(line: string): string[] {
  const row = line.trim().replace(/^\|/, '').replace(/\|$/, '')
  const out: string[] = []
  let cell = ''
  let code = false
  for (let i = 0; i < row.length; i++) {
    const c = row[i]
    if (c === '\\' && row[i + 1] === '|') {
      cell += '|'
      i++
    } else if (c === '`') {
      code = !code
      cell += c
    } else if (c === '|' && !code) {
      out.push(cell.trim())
      cell = ''
    } else cell += c
  }
  out.push(cell.trim())
  return out
}

interface Item {
  indent: number
  ordered: boolean
  start: number
  text: string[]
  children: Item[]
}
const indentOf = (line: string) => line.match(/^\s*/)![0].replace(/\t/g, '    ').length

/** Renders Markdown; `headings` lists the headings with their ids, for a table of contents. */
export function renderMarkdown(source: string, options: MarkdownOptions = {}): Rendered {
  const headings: Heading[] = []
  const used = new Map<string, number>()
  const idFor = (text: string) => {
    const base = slug(text)
    const n = used.get(base) ?? 0
    used.set(base, n + 1)
    return n ? `${base}-${n + 1}` : base
  }
  const html = blocks(source.replace(/\r\n?/g, '\n').split('\n'))
  return { html, headings }

  function blocks(lines: string[]): string {
    const out: string[] = []
    let i = 0
    const startsBlock = (line: string, next = '') =>
      FENCE.test(line) || HEADING.test(line) || RULE.test(line) || ITEM.test(line) || QUOTE.test(line) || (line.trim().startsWith('|') && TABLE_RULE.test(next))
    while (i < lines.length) {
      const line = lines[i]
      if (!line.trim()) {
        i++
        continue
      }
      const fence = FENCE.exec(line)
      if (fence) {
        const body: string[] = []
        i++
        while (i < lines.length && !lines[i].trim().startsWith(fence[1])) body.push(lines[i++])
        i++
        out.push(`<pre><code${fence[2] ? ` data-lang="${escapeHtml(fence[2])}"` : ''}>${escapeHtml(body.join('\n'))}</code></pre>`)
        continue
      }
      const heading = HEADING.exec(line)
      if (heading) {
        const level = heading[1].length
        const id = idFor(heading[2])
        headings.push({ level, text: heading[2].replace(/[`*]/g, ''), id })
        out.push(`<h${level} id="${id}">${inline(heading[2], options)}</h${level}>`)
        i++
        continue
      }
      if (RULE.test(line) && !ITEM.test(line)) {
        out.push('<hr>')
        i++
        continue
      }
      if (line.trim().startsWith('|') && TABLE_RULE.test(lines[i + 1] ?? '')) {
        const head = cells(line)
        const align = cells(lines[i + 1]).map(c => (c.endsWith(':') ? (c.startsWith(':') ? 'center' : 'right') : ''))
        i += 2
        const rows: string[][] = []
        while (i < lines.length && lines[i].trim().startsWith('|')) rows.push(cells(lines[i++]))
        const cell = (tag: string, text: string, k: number) =>
          `<${tag}${align[k] ? ` style="text-align:${align[k]}"` : ''}>${inline(text, options)}</${tag}>`
        out.push(
          `<div class="md-table"><table><thead><tr>${head.map((c, k) => cell('th', c, k)).join('')}</tr></thead><tbody>${rows
            .map(r => `<tr>${head.map((_, k) => cell('td', r[k] ?? '', k)).join('')}</tr>`)
            .join('')}</tbody></table></div>`,
        )
        continue
      }
      if (QUOTE.test(line)) {
        const body: string[] = []
        while (i < lines.length && QUOTE.test(lines[i])) body.push(lines[i++].replace(QUOTE, ''))
        out.push(`<blockquote>${blocks(body)}</blockquote>`)
        continue
      }
      if (ITEM.test(line)) {
        const [html, next] = list(lines, i)
        out.push(html)
        i = next
        continue
      }
      const para: string[] = []
      while (i < lines.length && lines[i].trim() && !(para.length && startsBlock(lines[i], lines[i + 1]))) para.push(lines[i++].trim())
      out.push(`<p>${inline(para.join(' '), options)}</p>`)
    }
    return out.join('\n')
  }

  /** A list from line `from` (nested by indentation), and the line after it. */
  function list(lines: string[], from: number): [string, number] {
    const items: Item[] = []
    let i = from
    while (i < lines.length) {
      const line = lines[i]
      const item = ITEM.exec(line)
      if (item && !(RULE.test(line) && !item[3].trim())) {
        items.push({ indent: indentOf(line), ordered: /\d/.test(item[2]), start: parseInt(item[2]) || 1, text: [item[3]], children: [] })
        i++
        continue
      }
      if (!line.trim()) {
        // A blank line ends the list unless the next line is indented under it, or another item.
        const next = lines.slice(i + 1).find(l => l.trim())
        if (next !== undefined && (ITEM.test(next) ? indentOf(next) >= items[0].indent : indentOf(next) > items[0].indent)) {
          i++
          continue
        }
        break
      }
      // A line under an item continues its text: indented under it, or right after it (a lazy continuation).
      const other = HEADING.test(line) || FENCE.test(line) || QUOTE.test(line) || RULE.test(line) || line.trim().startsWith('|')
      if (indentOf(line) <= items[0].indent && (other || !lines[i - 1].trim())) break
      items.at(-1)!.text.push(line.trim())
      i++
    }
    // Nest by indentation.
    const roots: Item[] = []
    const stack: Item[] = []
    for (const item of items) {
      while (stack.length && stack.at(-1)!.indent >= item.indent) stack.pop()
      ;(stack.length ? stack.at(-1)!.children : roots).push(item)
      stack.push(item)
    }
    const render = (group: Item[]): string => {
      const runs: Item[][] = []
      for (const item of group) {
        const last = runs.at(-1)
        if (last && last[0].ordered === item.ordered) last.push(item)
        else runs.push([item])
      }
      return runs
        .map(run => {
          const tag = run[0].ordered ? 'ol' : 'ul'
          const start = run[0].ordered && run[0].start !== 1 ? ` start="${run[0].start}"` : ''
          return `<${tag}${start}>${run
            .map(item => `<li>${inline(item.text.join(' '), options)}${item.children.length ? render(item.children) : ''}</li>`)
            .join('')}</${tag}>`
        })
        .join('')
    }
    return [render(roots), i]
  }
}
