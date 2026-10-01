import { describe, expect, it } from 'vitest'
import { inline, renderMarkdown, slug } from '../markdown'
import { fold, parseWordDiff } from '../wordDiff'
import { commitTime, resolveLink, toolParams } from '../instructions'

describe('instructions page helpers', () => {
  it('relative links open the file they name, if the page has it', () => {
    const ids = new Set([
      'assistant/skills/app-guide/SKILL.md',
      'assistant/skills/app-guide/reference/monitoreo.md',
      'assistant/skills/data-rules/reference/clutches.md',
    ])
    expect(resolveLink('assistant/skills/app-guide/SKILL.md', 'reference/monitoreo.md', ids)).toBe(
      'assistant/skills/app-guide/reference/monitoreo.md',
    )
    expect(resolveLink('assistant/skills/data-rules/reference/crosses.md', 'clutches.md#counts', ids)).toBe(
      'assistant/skills/data-rules/reference/clutches.md',
    )
    expect(resolveLink('assistant/skills/app-guide/reference/monitoreo.md', '../SKILL.md', ids)).toBe('assistant/skills/app-guide/SKILL.md')
    expect(resolveLink('assistant/skills/app-guide/SKILL.md', 'reference/missing.md', ids)).toBeNull()
    expect(resolveLink('assistant/skills/app-guide/SKILL.md', 'https://example.org/x.md', ids)).toBeNull()
  })

  it('tool parameters as rows, nested ones under their parent', () => {
    const rows = toolParams({
      type: 'object',
      required: ['kind', 'lines'],
      properties: {
        kind: { type: 'string', enum: ['stocks', 'labels'], description: 'Which notebook' },
        lines: { type: 'array', items: { type: 'object', required: ['raw'], properties: { raw: { type: 'string' }, values: { type: 'object' } } } },
        groupBy: { anyOf: [{ type: 'string' }, { type: 'array', items: { type: 'string' } }] },
      },
    })
    expect(rows.map(r => [r.name, r.type, r.required, r.depth])).toEqual([
      ['kind', '"stocks" | "labels"', true, 0],
      ['lines', 'object[]', true, 0],
      ['lines[].raw', 'string', true, 1],
      ['lines[].values', 'object', false, 1],
      ['groupBy', 'string | string[]', false, 0],
    ])
  })

  it('commit times day first, in their own zone', () => {
    expect(commitTime('2026-09-30T20:55:45-05:00')).toBe('30/09/2026 20:55')
  })
})

describe('markdown', () => {
  it('headings get ids for the table of contents', () => {
    const { html, headings } = renderMarkdown('# Data rules\n\n## Death, preserved\n\ntext\n\n## Death, preserved\n### `Mark_Released`')
    expect(headings.map(h => [h.level, h.id])).toEqual([
      [1, 'data-rules'],
      [2, 'death-preserved'],
      [2, 'death-preserved-2'],
      [3, 'mark-released'],
    ])
    expect(html).toContain('<h2 id="death-preserved">Death, preserved</h2>')
    expect(html).toContain('<h3 id="mark-released"><code>Mark_Released</code></h3>')
    expect(slug('Pre-made row — «Corregir»')).toBe('pre-made-row-corregir')
  })

  it('lists nest by indentation; wrapped lines continue the item', () => {
    const { html } = renderMarkdown('Intro\n- one\n  wrapped\n  - inner\n- two\n\n3. third\n4. fourth\n\nAfter')
    expect(html).toBe(
      '<p>Intro</p>\n<ul><li>one wrapped<ul><li>inner</li></ul></li><li>two</li></ul><ol start="3"><li>third</li><li>fourth</li></ol>\n<p>After</p>',
    )
  })

  it('tables keep a bar inside code in its cell', () => {
    const { html } = renderMarkdown('| Column | Value |\n|---|---|\n| Tube_1_tissue | `**OTHER** | WING CLIP` |\n| Sex | `NA` |')
    expect(html).toContain('<th>Column</th><th>Value</th>')
    expect(html).toContain('<td>Tube_1_tissue</td><td><code>**OTHER** | WING CLIP</code></td>')
    expect(html.match(/<tr>/g)).toHaveLength(3)
  })

  it('escapes HTML everywhere, code blocks included', () => {
    const { html } = renderMarkdown('<script>alert(1)</script>\n\n```sh\necho "<b>"\n```')
    expect(html).not.toContain('<script>')
    expect(html).toContain('&lt;script&gt;')
    expect(html).toContain('<pre><code data-lang="sh">echo &quot;&lt;b&gt;&quot;</code></pre>')
  })

  it('inline: bold, italics, code, links; snake_case stays', () => {
    expect(inline('**Never** *M. messenoides* in Insectary_ID and Tube_1_id, _really_')).toBe(
      '<strong>Never</strong> <em>M. messenoides</em> in Insectary_ID and Tube_1_id, <em>really</em>',
    )
    expect(inline('`a*b*c` and `x`')).toBe('<code>a*b*c</code> and <code>x</code>')
    expect(inline('see https://ithomiini-ikiam.com/#/tubos.')).toBe(
      'see <a href="https://ithomiini-ikiam.com/#/tubos" target="_blank" rel="noopener">https://ithomiini-ikiam.com/#/tubos</a>.',
    )
    // Relative links open inside the page; a link the page cannot open shows its text.
    const link = (href: string) => (href.endsWith('.md') ? `#/instrucciones?archivo=${href}` : null)
    expect(inline('[`notes.md`](reference/notes.md) or [x](javascript:void)', { link })).toBe(
      '<a href="#/instrucciones?archivo=reference/notes.md"><code>notes.md</code></a> or x',
    )
  })

  it('block quotes and rules', () => {
    expect(renderMarkdown('> **Lab copy.** Here:\n>\n> - step').html).toBe(
      '<blockquote><p><strong>Lab copy.</strong> Here:</p>\n<ul><li>step</li></ul></blockquote>',
    )
    expect(renderMarkdown('a\n\n---\n\nb').html).toBe('<p>a</p>\n<hr>\n<p>b</p>')
  })
})

describe('word diff', () => {
  const diff = [
    'diff --git a/assistant/AGENTS.md b/assistant/AGENTS.md',
    '--- a/assistant/AGENTS.md',
    '+++ b/assistant/AGENTS.md',
    '@@ -3,4 +3,4 @@ Intro',
    ' Keep answers ',
    '-short',
    '+brief',
    ' ; use tables.',
    '~',
    '+A new line.',
    '~',
    '-An old line.',
    '~',
    ' Unchanged.',
    '~',
  ].join('\n')

  it('lines with their added and removed words', () => {
    const [file] = parseWordDiff(diff)
    expect(file.path).toBe('assistant/AGENTS.md')
    expect(file.rows[0]).toEqual({ type: 'hunk', line: 3, section: 'Intro' })
    expect(file.rows.slice(1).map(r => (r.type === 'line' ? r.change : r.type))).toEqual(['mod', 'add', 'del', 'ctx'])
    const first = file.rows[1]
    expect(first.type === 'line' && first.segments.map(s => `${s.kind}:${s.text}`)).toEqual([
      'ctx:Keep answers ',
      'del:short',
      'add:brief',
      'ctx:; use tables.',
    ])
  })

  it('new and renamed files are noted; several files are kept apart', () => {
    const files = parseWordDiff(
      'diff --git a/a.md b/a.md\nnew file mode 100644\n@@ -0,0 +1 @@\n+x\n~\ndiff --git a/old.md b/new.md\nsimilarity index 90%\nrename from old.md\nrename to new.md\n',
    )
    expect(files.map(f => f.path)).toEqual(['a.md', 'new.md'])
    expect(files[0].rows[0]).toEqual({ type: 'note', text: 'new' })
    expect(files[1].rows).toEqual([{ type: 'note', text: 'renamed:old.md' }])
  })

  it('long unchanged stretches fold, keeping three lines around each change', () => {
    const ctx = (n: number) => ({ type: 'line' as const, change: 'ctx' as const, segments: [{ kind: 'ctx' as const, text: String(n) }] })
    const add = { type: 'line' as const, change: 'add' as const, segments: [{ kind: 'add' as const, text: '+' }] }
    const rows = [...Array.from({ length: 10 }, (_, i) => ctx(i)), add, ...Array.from({ length: 10 }, (_, i) => ctx(10 + i)), add, ctx(20)]
    const folded = fold(rows)
    expect(folded.map(r => (r.type === 'skip' ? `…${r.count}` : r.type === 'line' && r.change === 'add' ? '+' : r.type === 'line' ? r.segments[0].text : ''))).toEqual([
      '…7', '7', '8', '9', '+', '10', '11', '12', '…4', '17', '18', '19', '+', '20',
    ])
  })
})
