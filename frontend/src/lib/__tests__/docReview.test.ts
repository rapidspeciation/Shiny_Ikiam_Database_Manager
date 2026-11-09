import { afterEach, describe, expect, it } from 'vitest'
import { clearMarks, countFor, openCount, forEntry, fullText, headingBefore, locate, markRange, sectionElements, selectionAnchor, shownInMargin, stackCards, type DocChange, type DocReviewState } from '../docReview'

afterEach(() => {
  document.body.innerHTML = ''
})

const doc = () => {
  const root = document.createElement('div')
  root.innerHTML = '<h1>Demo</h1><p>Intro.</p><h2>Datos</h2><p>Usa <code>NA</code> para el sexo.</p><h3>Detalle</h3><p>Usa NA otra vez.</p><h2>Fotos</h2><p>Dos.</p>'
  document.body.append(root)
  return root
}

describe('doc review: anchors in the rendered text', () => {
  it('places a quote by its prefix and suffix, ties to the nearest offset', () => {
    const text = 'el sexo NA aquí. Otra vez el sexo NA allá.'
    expect(locate(text, { quote: 'sexo NA', prefix: 'Otra vez el ', suffix: '', offset: 0 })).toBe(text.lastIndexOf('sexo NA'))
    expect(locate(text, { quote: 'sexo NA', prefix: '', suffix: '', offset: 40 })).toBe(text.lastIndexOf('sexo NA'))
    expect(locate(text, { quote: 'nada', prefix: '', suffix: '', offset: 0 })).toBe(-1)
  })

  it('a selection gives quote, 32 characters around it, its offset and the heading it is under', () => {
    const root = doc()
    const p = root.querySelectorAll('p')[1]
    const range = document.createRange()
    range.setStart(p.firstChild!, 0)
    range.setEnd(p.lastChild!, ' para el sexo.'.length - 1)
    const sel = window.getSelection()!
    sel.removeAllRanges()
    sel.addRange(range)
    const a = selectionAnchor(root, sel)!
    expect(a.quote).toBe('Usa NA para el sexo')
    expect(a.heading).toBe('Datos')
    const text = fullText(root)
    expect(text.slice(a.offset, a.offset + a.quote.length)).toBe(a.quote)
    expect(a.prefix).toBe(text.slice(Math.max(0, a.offset - 32), a.offset))
    expect(a.suffix.startsWith('.')).toBe(true)
    // Outside the text: nothing.
    const other = document.createElement('p')
    other.textContent = 'fuera'
    document.body.append(other)
    const r2 = document.createRange()
    r2.selectNodeContents(other)
    sel.removeAllRanges()
    sel.addRange(r2)
    expect(selectionAnchor(root, sel)).toBe(null)
  })

  it('marks a quote across inline elements and clears the marks again', () => {
    const root = doc()
    const text = fullText(root)
    const at = text.indexOf('Usa NA para')
    const marks = markRange(root, at, 'Usa NA para el sexo'.length, 't1')
    expect(marks.length).toBe(3)
    expect([...root.querySelectorAll('mark[data-thread="t1"]')].map(m => m.textContent).join('')).toBe('Usa NA para el sexo')
    expect(fullText(root)).toBe(text)
    clearMarks(root)
    expect(root.querySelectorAll('mark').length).toBe(0)
    expect(fullText(root)).toBe(text)
  })

  it('the heading a node is under, and the section a heading opens', () => {
    const root = doc()
    expect(headingBefore(root, root.querySelectorAll('p')[2].firstChild!)).toBe('Detalle')
    expect(headingBefore(root, root.querySelector('p')!.firstChild!)).toBe('Demo')
    const { head, end } = sectionElements(root, 'Datos')
    expect(head?.textContent).toBe('Datos')
    expect(end?.textContent).toBe('Fotos')
    expect(sectionElements(root, 'Nada').head).toBe(null)
  })
})

describe('doc review: the margin', () => {
  it('stacks cards that collide; the active one stays at its highlight and those above move up', () => {
    const items = [
      { id: 'a', top: 0, height: 100 },
      { id: 'b', top: 50, height: 100 },
      { id: 'c', top: 400, height: 50 },
    ]
    expect(stackCards(items)).toEqual({ a: 0, b: 108, c: 400 })
    expect(stackCards(items, 'b')).toEqual({ a: -58, b: 50, c: 400 })
  })

  it('counts per file (the brief counts its voice), and which changes stay in the margin', () => {
    const state = { counts: { 'assistant/AGENTS.md': 1, 'assistant/voice/app.md': 2 }, threads: [], changes: [{ path: 'assistant/voice/app.md' }] } as unknown as DocReviewState
    expect(countFor(state, 'assistant/AGENTS.md')).toBe(3)
    const asked = { threads: [{ id: 'a', status: 'open' }, { id: 'b', status: 'open' }, { id: 'c', status: 'resolved' }], changes: [{ status: 'pending', threadIds: ['a'] }] } as unknown as DocReviewState
    expect(openCount(asked)).toBe(1)
    expect(forEntry(state, 'assistant/AGENTS.md').changes.length).toBe(1)
    const now = Date.parse('2026-11-08T12:00:00Z')
    const c = (over: Partial<DocChange>) => ({ status: 'pending', lost: false, revision: null, decidedAt: null, ...over }) as DocChange
    expect(shownInMargin(c({}), now)).toBe(true)
    expect(shownInMargin(c({ status: 'applied', decidedAt: '2026-11-08T10:00:00Z' }), now)).toBe(true)
    expect(shownInMargin(c({ status: 'applied', decidedAt: '2026-11-01T10:00:00Z' }), now)).toBe(false)
    expect(shownInMargin(c({ status: 'applied', decidedAt: '2026-11-01T10:00:00Z', lost: true }), now)).toBe(true)
  })
})
