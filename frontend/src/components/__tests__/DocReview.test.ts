import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { createApp, h, nextTick } from 'vue'
import DocThreadCard from '../instructions/DocThreadCard.vue'
import DocChangeCard from '../instructions/DocChangeCard.vue'
import DocReview from '../instructions/DocReview.vue'
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
  it('a resolved thread offers Reabrir', async () => {
    const host = mount(DocThreadCard, { thread: thread({ status: 'resolved', resolvedBy: 'Luis' }), active: true })
    expect(host.textContent).toContain('Resuelto por Luis')
    button(host, 'Reabrir')!.click()
    await flush()
    expect(posts()[0].url).toBe('api/doc-review/threads/t1/reopen')
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
    button(host, 'v1')!.click()
    await nextTick()
    expect(host.querySelector('.md')?.textContent).toContain('Usa NC.')
    button(host, 'Aplicar')!.click()
    await flush()
    expect(posts()[0]).toMatchObject({ url: 'api/doc-review/changes/s1/apply', body: { anyway: false } })
    button(host, 'Descartar')!.click()
    await flush()
    expect(posts()[1].url).toBe('api/doc-review/changes/s1/discard')
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
    expect(host.textContent).toContain('+42 bytes · brief')
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
    host = mount(DocChangeCard, { change: change({ status: 'applied', decidedBy: 'Franz', decidedAt: '2026-11-08T11:00:00Z', revision }), admin: true })
    expect(host.textContent).toContain('Sin subir a git')
    button(host, 'Reintentar')!.click()
    await flush()
    expect(posts().at(-1)!.url).toBe('api/doc-review/revisions/7/retry')
    button(host, 'Deshacer')!.click()
    await flush()
    expect(posts().at(-1)!.url).toBe('api/doc-review/changes/s1/undo')
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
    expect(host.querySelector('[role="dialog"]')?.textContent).toContain('§ Datos')
  })
})
