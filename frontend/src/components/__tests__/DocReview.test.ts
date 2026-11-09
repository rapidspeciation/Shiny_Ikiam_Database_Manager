import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { createApp, h, nextTick, reactive } from 'vue'
import DocThreadCard from '../instructions/DocThreadCard.vue'
import DocChangeCard from '../instructions/DocChangeCard.vue'
import DocReview from '../instructions/DocReview.vue'
import ProseDiff from '../instructions/ProseDiff.vue'
import { review, type DocChange, type DocReviewState, type DocThread } from '../../lib/docReview'
import { renderMarkdown } from '../../lib/markdown'
import { t, tn } from '../../lib/i18n'

let unmount = () => {}
let calls: { method: string; url: string; body: unknown }[] = []
let state: DocReviewState
beforeEach(() => {
  calls = []
  vi.stubGlobal('fetch', async (url: string, init: RequestInit = {}) => {
    const method = init.method ?? 'GET'
    calls.push({ method, url, body: init.body ? JSON.parse(String(init.body)) : null })
    const data = method === 'GET' ? state : url.endsWith('/threads') ? { thread: { id: 'new-1' } } : { ok: true }
    return new Response(JSON.stringify(data), { status: 200, headers: { 'content-type': 'application/json' } })
  })
})
afterEach(() => {
  unmount()
  vi.unstubAllGlobals()
  document.body.innerHTML = ''
  review.state = null
})
function mount(component: object, props: Record<string, unknown>) {
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp({ render: () => h(component, props) })
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  app.mount(host)
  unmount = () => app.unmount()
  return host
}
const flush = () => new Promise(r => setTimeout(r, 0))
const posts = () => calls.filter(c => c.method !== 'GET')
const button = (host: Element, text: string) => [...host.querySelectorAll('button')].find(b => b.textContent?.trim() === text) as HTMLButtonElement | undefined

const PATH = 'assistant/skills/demo/SKILL.md'
const thread = (over: Partial<DocThread> = {}): DocThread => ({
  id: 't1',
  path: PATH,
  quote: 'Usa NA para el sexo',
  prefix: '',
  suffix: '',
  offset: 0,
  heading: 'Datos',
  status: 'open',
  createdAt: '2026-11-08T10:00:00Z',
  own: false,
  resolvedAt: null,
  resolvedBy: null,
  resolvedByChange: null,
  comments: [
    { id: 'c1', body: '¿NA o NOT_COLLECTED?', createdAt: '2026-11-08T10:00:00Z', editedAt: null, deleted: false, kind: 'person', author: 'Ana Pérez', own: true },
    { id: 'c2', body: 'Propuesto.', createdAt: '2026-11-08T10:05:00Z', editedAt: null, deleted: false, kind: 'claude', author: null, own: false },
    { id: 'c3', body: '', createdAt: '2026-11-08T10:06:00Z', editedAt: null, deleted: true, kind: 'person', author: 'Luis', own: false },
  ],
  ...over,
})
const DIFF = '@@ -1,3 +1,3 @@\n ## Datos\n~\n~\n Usa \n-NA\n+NOT_COLLECTED\n  para el sexo.\n~'
const change = (over: Partial<DocChange> = {}): DocChange => ({
  id: 'ch1',
  seriesId: 's1',
  version: 2,
  path: PATH,
  heading: 'Datos',
  before: '## Datos\n\nUsa NA para el sexo.\n',
  after: '## Datos\n\nUsa NOT_COLLECTED para el sexo.\n',
  diff: DIFF,
  threadIds: ['t1'],
  status: 'pending',
  stale: false,
  lost: false,
  revising: false,
  chat: 'chat-1',
  createdAt: '2026-11-08T10:10:00Z',
  decidedAt: null,
  decidedBy: null,
  revision: null,
  budget: [],
  checks: [],
  versions: [
    { id: 'v1', version: 1, before: 'x', after: '## Datos\n\nUsa NC.\n', diff: '@@ -1 +1 @@\n-x\n+y\n~', createdAt: '', status: 'superseded' },
    { id: 'ch1', version: 2, before: 'x', after: '## Datos\n\nUsa NOT_COLLECTED para el sexo.\n', diff: DIFF, createdAt: '', status: 'pending' },
  ],
  comments: [],
  ...over,
})

describe('DocThreadCard', () => {
  it("shows people by name, Claude's tag, a deleted comment; edits only one's own; replies and resolves", async () => {
    const host = mount(DocThreadCard, { thread: thread(), active: true })
    expect(host.textContent).toContain('Ana Pérez')
    expect(host.querySelector('.doc-claude-tag')?.textContent).toBe('Claude')
    expect(host.textContent).toContain('Comentario borrado')
    expect(host.querySelectorAll('button').length).toBeGreaterThan(0)
    // Edit and delete only on the person's own comment.
    expect([...host.querySelectorAll('button')].filter(b => b.textContent === 'Editar').length).toBe(1)
    const box = host.querySelector('textarea')!
    box.value = 'NOT_COLLECTED'
    box.dispatchEvent(new Event('input'))
    await nextTick()
    button(host, 'Responder')!.click()
    await flush()
    expect(posts()[0]).toMatchObject({ method: 'POST', url: 'api/doc-review/threads/t1/comments', body: { body: 'NOT_COLLECTED' } })
    button(host, 'Resolver')!.click()
    await flush()
    expect(posts()[1].url).toBe('api/doc-review/threads/t1/resolve')
  })
  it('a thread a pending change answers links to it with «Ver cambio»', async () => {
    let shown = ''
    const host = mount(DocThreadCard, { thread: thread(), answeredBy: 's1', onChange: (id: string) => (shown = id) })
    button(host, 'Ver cambio')!.click()
    expect(shown).toBe('s1')
    unmount()
    expect(button(mount(DocThreadCard, { thread: thread() }), 'Ver cambio')).toBeUndefined()
  })
  it('a resolved thread offers Reabrir', async () => {
    const host = mount(DocThreadCard, { thread: thread({ status: 'resolved', resolvedBy: 'Luis' }), active: true })
    expect(host.textContent).toContain('Resuelto por Luis')
    button(host, 'Reabrir')!.click()
    await flush()
    expect(posts()[0].url).toBe('api/doc-review/threads/t1/reopen')
  })
})

describe('ProseDiff', () => {
  it('reads as the doc: lines joined, headings and list items without their marks, code without backticks, mono only for fenced code', () => {
    const diff =
      '@@ -1,4 +1,4 @@\n ## The site\n~\n \n~\n nombre y coordenadas.» \n-A partial\n+When the\n  name or a\n~\n nearby town with `place`.\n~\n - Meeting notes go in **the\n~\n folder**.\n~\n \n~\n ```sh\n~\n-gog --readonly\n+gog\n~\n ```\n~'
    const host = mount(ProseDiff, { diff })
    const blocks = [...host.querySelectorAll('.doc-prose-block')]
    expect(blocks.map(b => b.textContent)).toEqual([
      'The site',
      'nombre y coordenadas.» A partial When the name or a nearby town with place.',
      '•Meeting notes go in the folder.',
      '```sh',
      'gog --readonly gog',
      '```',
    ])
    expect(blocks[0].className).toContain('font-semibold')
    expect(blocks[1].className).not.toContain('font-mono')
    expect(blocks[1].querySelector('code')?.textContent).toBe('place')
    expect(blocks[2].querySelector('strong')?.textContent).toBe('the')
    expect(blocks[4].className).toContain('font-mono')
    // A changed paragraph is marked in Claude's green (the class), not amber.
    expect(blocks[1].className).toContain('is-changed')
    expect(blocks[1].className).not.toContain('amber')
    expect(host.firstElementChild!.className).not.toContain('font-mono')
  })
})

describe('DocChangeCard', () => {
  it('a word diff of the section, Cambios | Después, v1 one click away; admins apply and discard', async () => {
    const host = mount(DocChangeCard, { change: change(), admin: true, active: true })
    expect(host.querySelector('del')?.textContent).toBe('NA')
    expect(host.querySelector('ins')?.textContent).toBe('NOT_COLLECTED')
    button(host, 'Después')!.click()
    await nextTick()
    expect(host.querySelector('.md')?.textContent).toContain('Usa NOT_COLLECTED para el sexo.')
    button(host, 'Aplicar')!.click()
    await flush()
    expect(posts()[0]).toMatchObject({ url: 'api/doc-review/changes/s1/apply', body: { anyway: false, version: 2 } })
    button(host, 'Descartar')!.click()
    await flush()
    expect(posts()[1].url).toBe('api/doc-review/changes/s1/discard')
  })
  it('an earlier version is read only: «Versión anterior», no status or decisions, «Ver v2» goes back', async () => {
    const applied = { status: 'applied' as const, decidedBy: 'Franz', decidedAt: '2026-11-08T11:00:00Z' }
    let host = mount(DocChangeCard, { change: change(), admin: true, active: true })
    button(host, 'Después')!.click()
    button(host, 'v1')!.click()
    await nextTick()
    expect(host.querySelector('.md')?.textContent).toContain('Usa NC.')
    expect(host.textContent).toContain('Versión anterior')
    for (const label of ['Aplicar', 'Descartar', 'Comentar', 'Pedir versión nueva']) expect(button(host, label), label).toBeUndefined()
    button(host, 'Ver v2')!.click()
    await nextTick()
    expect(host.textContent).not.toContain('Versión anterior')
    expect(button(host, 'Aplicar')).toBeDefined()
    unmount()
    host = mount(DocChangeCard, { change: change(applied), admin: true })
    expect(host.textContent).toContain('Aplicado por Franz')
    button(host, 'v1')!.click()
    await nextTick()
    expect(host.textContent).not.toContain('Aplicado por')
    expect(button(host, 'Deshacer')).toBeUndefined()
  })
  it('shows the comment it answers (a click shows that thread), says when Claude is answering, and why a version exists', async () => {
    let shown = ''
    let host = mount(DocChangeCard, { change: change(), admin: true, quotes: [{ id: 't1', quote: 'Usa NA para el sexo' }, { id: 't2', quote: 'otra' }], onThread: (id: string) => (shown = id) })
    const quote = host.querySelector<HTMLButtonElement>('.doc-quote-btn')!
    expect(quote.textContent).toContain('Usa NA para el sexo')
    expect(quote.textContent).toContain('+1')
    quote.click()
    expect(shown).toBe('t1')
    expect(host.querySelector('header')!.textContent).not.toContain('§')
    unmount()
    const system = { id: 'x', body: 'version_after_stale:2', createdAt: '', editedAt: null, deleted: false, kind: 'system' as const, author: null, own: false }
    host = mount(DocChangeCard, { change: change({ revising: true, comments: [system] }), admin: true })
    expect(host.textContent).toContain('Claude está respondiendo…')
    expect(button(host, 'Aplicar')!.disabled).toBe(true)
    expect(host.textContent).toContain('v2: el texto había cambiado')
  })
  it('a double tap on the decision button never applies and then undoes', async () => {
    const live = reactive(change())
    const host = mount(DocChangeCard, { change: live, admin: true })
    button(host, 'Aplicar')!.click()
    await flush()
    Object.assign(live, { status: 'applied', decidedBy: 'Franz', decidedAt: '2026-11-08T12:00:00Z' })
    await nextTick()
    button(host, 'Deshacer')!.click()
    await flush()
    expect(posts().map(p => p.url)).toEqual(['api/doc-review/changes/s1/apply'])
  })
  it('a click anywhere on the card, its diff included, makes it the active one', async () => {
    let activated = 0
    const host = mount(DocChangeCard, { change: change(), admin: true, onActivate: () => activated++ })
    host.querySelector('del')!.dispatchEvent(new MouseEvent('click', { bubbles: true }))
    expect(activated).toBe(1)
  })
  it('for others: Comentar, no Aplicar; a comment for admins can ask for a new version', async () => {
    let host = mount(DocChangeCard, { change: change(), admin: false, active: true })
    expect(button(host, 'Aplicar')).toBeUndefined()
    expect(button(host, 'Pedir versión nueva')).toBeUndefined()
    expect(button(host, 'Comentar')).toBeDefined()
    unmount()
    host = mount(DocChangeCard, { change: change(), admin: true, active: true })
    const box = host.querySelector('textarea')!
    box.value = 'Más corto'
    box.dispatchEvent(new Event('input'))
    await nextTick()
    button(host, 'Pedir versión nueva')!.click()
    await flush()
    expect(posts()[0]).toMatchObject({ url: 'api/doc-review/changes/s1/revise', body: { body: 'Más corto', why: 'comment' } })
  })
  it('a growth of the fixed context shows its bytes and needs «Aplicar igualmente»; a failed check blocks', async () => {
    let host = mount(DocChangeCard, { change: change({ budget: [{ part: 'brief', bytes: 42 }] }), admin: true })
    expect(host.textContent).toContain('Crece 42 bytes: las instrucciones generales')
    expect(host.textContent).not.toContain('brief')
    expect(button(host, 'Aplicar')).toBeUndefined()
    button(host, 'Aplicar igualmente')!.click()
    await flush()
    expect(posts().at(-1)!.body).toMatchObject({ anyway: true })
    unmount()
    host = mount(DocChangeCard, { change: change({ checks: [{ code: 'placeholder', text: '{{x}}' }] }), admin: true })
    expect(host.textContent).toContain('Marcador desconocido: {{x}}')
    expect(button(host, 'Aplicar')!.disabled).toBe(true)
  })
  it('stale: Desactualizada and «Pedir a Claude que la rehaga»; applied: git state and Deshacer', async () => {
    let host = mount(DocChangeCard, { change: change({ stale: true }), admin: true })
    expect(host.textContent).toContain('Desactualizada')
    button(host, 'Pedir a Claude que la rehaga')!.click()
    await flush()
    expect(posts()[0].body).toMatchObject({ why: 'stale' })
    unmount()
    const revision = { id: 7, kind: 'apply' as const, gitState: 'failed' as const, gitCommit: null, gitError: 'push', appliedAt: '', appliedBy: 'Franz', undoneAt: null }
    host = mount(DocChangeCard, { change: change({ status: 'applied', decidedBy: 'Franz', decidedAt: '2026-11-08T11:00:00Z', revision: { ...revision, gitState: 'committed', gitCommit: 'abcdef1234' } }), admin: true })
    // A git sync that worked shows nothing (the commit only in a tooltip).
    expect(host.textContent).not.toMatch(/git|abcdef1/)
    expect(host.querySelector('[title="abcdef1"]')).not.toBe(null)
    unmount()
    host = mount(DocChangeCard, { change: change({ status: 'applied', decidedBy: 'Franz', decidedAt: '2026-11-08T11:00:00Z', revision }), admin: true })
    expect(host.textContent).toContain('Sin subir a git')
    button(host, 'Reintentar')!.click()
    await flush()
    expect(posts().at(-1)!.url).toBe('api/doc-review/revisions/7/retry')
    button(host, 'Deshacer')!.click()
    await flush()
    expect(posts().at(-1)!.url).toBe('api/doc-review/changes/s1/undo')
    unmount()
    host = mount(DocChangeCard, { change: change({ status: 'undone', decidedBy: 'Franz', decidedAt: '2026-11-08T12:00:00Z' }), admin: true })
    expect(host.textContent).toContain('Deshecho por Franz')
    expect(host.querySelector('[data-diff]')!.className).toContain('opacity-70')
  })
})

describe('DocReview', () => {
  const html = renderMarkdown('# Demo\n\n## Datos\n\nUsa NA para el sexo.\n\n## Fotos\n\nDos fotos.\n').html
  const viewport = (width: number) => (window as unknown as { happyDOM: { setViewport: (v: { width: number; height: number }) => void } }).happyDOM.setViewport({ width, height: 800 })
  afterEach(() => viewport(1024))
  const withState = (over: Partial<DocReviewState> = {}) => {
    state = { admin: true, threads: [thread(), thread({ id: 't2', quote: 'texto que ya no está' })], changes: [change()], counts: {}, ask: { state: 'idle', chat: null, at: null }, ...over }
    review.state = state
  }
  it('computers: highlights in the text, a bar beside a changed section, cards in the margin; a thread whose text is gone below', async () => {
    withState()
    const host = mount(DocReview, { entryId: PATH, html })
    await flush()
    await flush()
    const marks = host.querySelectorAll('mark[data-thread="t1"]')
    expect([...marks].map(m => m.textContent).join('')).toBe('Usa NA para el sexo')
    expect(host.querySelector('mark[data-thread="t2"]')).toBe(null)
    expect(host.querySelectorAll('.doc-change-bar').length).toBe(1)
    const margin = host.querySelector('aside')!
    expect(margin.querySelector('[data-card="t1"]')).not.toBe(null)
    expect(margin.querySelector('[data-card="s1"]')).not.toBe(null)
    expect(margin.textContent).toContain('El texto cambió')
    expect(margin.textContent).toContain('Comentarios · 3')
    // «Ver cambio» on the thread the change answers makes the change the active card.
    button(margin.querySelector('[data-card="t1"]')!, 'Ver cambio')!.click()
    await flush()
    expect(margin.querySelector('[data-card="s1"]')!.className).toContain('doc-card-active')
    // The change card quotes the question it answers, by its author.
    expect(margin.querySelector('[data-card="s1"] .doc-quote-btn')!.textContent).toContain('Ana Pérez: ¿NA o NOT_COLLECTED?')
    // A highlight clicked: its card is the active one, and the highlight darkens.
    ;(marks[0] as HTMLElement).click()
    await flush()
    expect(margin.querySelector('[data-card="t1"]')!.className).toContain('doc-card-active')
    expect(marks[0].classList.contains('is-active')).toBe(true)
  })
  it('computers: a selection shows the Comentar chip; the comment is saved with its anchor', async () => {
    withState({ threads: [], changes: [] })
    const host = mount(DocReview, { entryId: PATH, html })
    await flush()
    const p = host.querySelector('.md p')!
    const range = document.createRange()
    range.setStart(p.firstChild!, 4)
    range.setEnd(p.firstChild!, 6)
    const sel = window.getSelection()!
    sel.removeAllRanges()
    sel.addRange(range)
    document.dispatchEvent(new Event('selectionchange'))
    await nextTick()
    button(host, 'Comentar')!.click()
    await nextTick()
    const box = host.querySelector<HTMLTextAreaElement>('[data-new-comment]')!
    box.value = '¿Por qué NA?'
    box.dispatchEvent(new Event('input'))
    await nextTick()
    host.querySelector<HTMLFormElement>('form[data-card="new"]')!.dispatchEvent(new Event('submit'))
    await flush()
    const saved = posts().find(c => c.url === 'api/doc-review/threads')!
    expect(saved.body).toMatchObject({ path: PATH, quote: 'NA', heading: 'Datos', body: '¿Por qué NA?' })
    expect((saved.body as { prefix: string }).prefix.endsWith('Usa ')).toBe(true)
  })
  it('phones: no margin; the bar has the count; a highlight opens its thread in a bottom sheet; the list', async () => {
    viewport(390)
    withState()
    const host = mount(DocReview, { entryId: PATH, html })
    await flush()
    await flush()
    expect(host.querySelector('aside')).toBe(null)
    expect(button(host, 'Comentarios · 3')).toBeDefined()
    ;(host.querySelector('mark[data-thread="t1"]') as HTMLElement).click()
    await nextTick()
    expect(host.querySelector('[role="dialog"]')?.textContent).toContain('¿NA o NOT_COLLECTED?')
    button(host, 'Comentarios · 3')!.click()
    await nextTick()
    expect(host.querySelector('[role="dialog"]')?.textContent).toContain('Claude Datos')
    // From the list, a thread opens with «‹ Comentarios» back to the list.
    ;[...host.querySelectorAll<HTMLButtonElement>('[role="dialog"] button')].find(b => b.textContent?.includes('¿NA o NOT_COLLECTED?'))!.click()
    await nextTick()
    expect(button(host, 'Comentarios')).toBeDefined()
    button(host, 'Comentarios')!.click()
    await nextTick()
    expect(host.querySelector('[role="dialog"]')?.textContent).toContain('Claude Datos')
    // Resolving a thread opened from the list goes back to the list.
    ;[...host.querySelectorAll<HTMLButtonElement>('[role="dialog"] button')].find(b => b.textContent?.includes('¿NA o NOT_COLLECTED?'))!.click()
    await nextTick()
    button(host, 'Resolver')!.click()
    await flush()
    expect(posts().at(-1)!.url).toBe('api/doc-review/threads/t1/resolve')
    expect(host.querySelector('[role="dialog"]')?.textContent).toContain('Claude Datos')
    // Escape closes the sheet.
    host.querySelector('[role="dialog"]')!.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }))
    await nextTick()
    expect(host.querySelector('[role="dialog"]')).toBe(null)
    // The «Cambio» tag beside the heading opens the change's sheet.
    const tag = host.querySelector<HTMLButtonElement>('h2 .doc-change-tag')!
    expect(tag.textContent).toBe('Cambio')
    tag.click()
    await nextTick()
    expect(host.querySelector('[role="dialog"] [data-card="s1"]')).not.toBe(null)
  })
  it('phones: the quote in a thread sheet closes the sheet and shows its passage', async () => {
    viewport(390)
    withState()
    const host = mount(DocReview, { entryId: PATH, html })
    await flush()
    await flush()
    ;(host.querySelector('mark[data-thread="t1"]') as HTMLElement).click()
    await nextTick()
    host.querySelector<HTMLButtonElement>('[role="dialog"] .doc-quote-btn')!.click()
    await flush()
    expect(host.querySelector('[role="dialog"]')).toBe(null)
    expect(host.querySelector('mark[data-thread="t1"]')!.classList.contains('is-active')).toBe(true)
  })
  it('highlights and the change bar take the keyboard: a name, Enter opens', async () => {
    withState()
    const host = mount(DocReview, { entryId: PATH, html })
    await flush()
    await flush()
    const mark = host.querySelector<HTMLElement>('mark[data-thread="t1"]')!
    expect(mark.tabIndex).toBe(0)
    expect(mark.getAttribute('aria-label')).toBe('Comentario de Ana Pérez')
    mark.dispatchEvent(new KeyboardEvent('keydown', { key: 'Enter', bubbles: true }))
    await flush()
    expect(host.querySelector('aside [data-card="t1"]')!.className).toContain('doc-card-active')
    const bar = host.querySelector<HTMLElement>('.doc-change-bar')!
    expect(bar.getAttribute('tabindex')).toBe('0')
    expect(bar.getAttribute('aria-label')).toBe('Cambio propuesto: § Datos')
    bar.dispatchEvent(new KeyboardEvent('keydown', { key: 'Enter', bubbles: true }))
    await flush()
    expect(host.querySelector('aside [data-card="s1"]')!.className).toContain('doc-card-active')
  })
})
