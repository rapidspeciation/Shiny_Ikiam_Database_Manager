import { afterEach, describe, expect, it } from 'vitest'
import { effectScope, nextTick, ref } from 'vue'
import { useDismiss } from '../useDismiss'

function setup() {
  document.body.innerHTML = '<div id="holder"><button id="trigger">menu</button><div id="menu"><a href="#x" id="item">x</a></div></div><button id="elsewhere">o</button><iframe id="frame"></iframe>'
  const open = ref(true)
  const page = ref('/a')
  const holder = ref(document.getElementById('holder'))
  const trigger = ref(document.getElementById('trigger'))
  const scope = effectScope()
  scope.run(() => useDismiss(open, holder, { trigger, closeOn: () => page.value }))
  return { open, page, scope, $: (id: string) => document.getElementById(id)! }
}
const press = (el: Element) => el.dispatchEvent(new PointerEvent('pointerdown', { bubbles: true }))
let scope: ReturnType<typeof effectScope> | undefined
afterEach(() => scope?.stop())

describe('useDismiss', () => {
  it('closes on a click outside, not on one inside', async () => {
    const s = setup(); scope = s.scope
    press(s.$('item'))
    expect(s.open.value).toBe(true)
    press(s.$('elsewhere'))
    expect(s.open.value).toBe(false)
  })
  it('closes even when the click outside is stopped on its way up (a grid)', () => {
    const s = setup(); scope = s.scope
    s.$('elsewhere').addEventListener('pointerdown', e => e.stopPropagation())
    press(s.$('elsewhere'))
    expect(s.open.value).toBe(false)
  })
  it('closes on Escape and puts the focus back on its button', () => {
    const s = setup(); scope = s.scope
    s.$('item').focus()
    document.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }))
    expect(s.open.value).toBe(false)
    expect(document.activeElement).toBe(s.$('trigger'))
  })
  it('closes when the focus moves out of it, and on a change of page', async () => {
    const s = setup(); scope = s.scope
    s.$('elsewhere').focus()
    expect(s.open.value).toBe(false)
    s.open.value = true
    await nextTick()
    s.page.value = '/b'
    await nextTick()
    expect(s.open.value).toBe(false)
  })
  it('stops listening once closed', async () => {
    const s = setup(); scope = s.scope
    s.open.value = false
    await nextTick()
    s.$('trigger').focus()
    document.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }))
    // Escape on a closed menu leaves the focus where it was (the listener is off).
    s.$('elsewhere').focus()
    document.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }))
    expect(document.activeElement).toBe(s.$('elsewhere'))
  })
})

describe('useDismiss without a trigger', () => {
  it('gives the focus back to what had it when the menu opened', async () => {
    document.body.innerHTML = '<div id="box"><button id="opener">⋯</button><a href="#y" id="link">y</a></div>'
    const open = ref(false)
    const box = ref(document.getElementById('box'))
    scope = effectScope()
    scope.run(() => useDismiss(open, box))
    document.getElementById('opener')!.focus()
    open.value = true
    await nextTick()
    document.getElementById('link')!.focus()
    document.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }))
    expect(open.value).toBe(false)
    expect(document.activeElement?.id).toBe('opener')
  })
})
