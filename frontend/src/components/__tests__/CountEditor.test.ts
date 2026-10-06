import { afterEach, describe, expect, it } from 'vitest'
import { createApp, h, nextTick, ref } from 'vue'
import CountEditor, { type CountEvent } from '../clutches/CountEditor.vue'
import { appendNote, eventNote, type Stage } from '../../lib/clutches'
import { isoToSerial } from '../../lib/dates'
import { locale, t, tn } from '../../lib/i18n'
import type { CellValue } from '../../lib/types'

// A count of a clutch on its card: what happened to the larvae, the day it happened, and the note shown before saving.

let unmount = () => {}
afterEach(() => {
  unmount()
  document.body.innerHTML = ''
})

const TODAY = isoToSerial('2026-10-05')
async function mount(start: CellValue, { stage = 'larva' as Stage, subtractPreserved = false, canRegister = false } = {}) {
  const value = ref<CellValue>(start)
  const events: CountEvent[] = []
  const registers: { count: number; lifestage: string; day: number; done: (ids: string[]) => void }[] = []
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp({
    render: () =>
      h(CountEditor, {
        field: 'NUMBER OF LARVAE',
        value: value.value,
        saved: start,
        dirty: false,
        editable: true,
        locked: false,
        more: 'hatched',
        stage,
        subtractPreserved,
        today: TODAY,
        canRegister,
        noteFor: (e: { kind: CountEvent['kind']; count: number; ids: string[]; day: number; lifestage?: string }) =>
          appendNote(null, eventNote({ stage, ...e }, TODAY), TODAY, 'FCH'),
        onSet: (v: CellValue) => (value.value = v),
        onEvent: (e: CountEvent) => events.push(e),
        onRegister: (r: (typeof registers)[number]) => registers.push(r),
      }),
  })
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  locale.value = 'en'
  app.mount(host)
  unmount = () => app.unmount()
  await nextTick()
  const button = (text: string) => [...host.querySelectorAll('button')].find(b => b.textContent?.trim().startsWith(text))!
  const type = async (n: string) => {
    const box = host.querySelector<HTMLInputElement>('input[placeholder="N"]')!
    box.value = n
    box.dispatchEvent(new Event('input'))
    await nextTick()
  }
  const click = async (b: HTMLElement) => {
    b.click()
    await nextTick()
  }
  return { host, value, events, registers, button, type, click }
}

describe('a count on a clutch card', () => {
  it('−5 died: the count takes them off, the event is today, and the note for NOTES is shown', async () => {
    const m = await mount('=20')
    await m.type('5')
    await m.click(m.button('−5'))
    await m.click(m.button('Died'))
    expect(m.value.value).toBe('=20-5')
    expect(m.events.map(e => [e.kind, e.count, e.day])).toEqual([['died', 5, TODAY]])
    expect(m.host.textContent).toContain('Added to NOTES (saved with the clutch): 5/10/26 FCH: 5 larvae died')
  })
  it('preserved larvae stay counted by default, at the 3rd instar unless another is chosen; the note is shown while choosing', async () => {
    const m = await mount('=20')
    await m.type('10')
    await m.click(m.button('−10'))
    await m.click(m.button('Preserved'))
    expect(m.host.textContent).toContain('5/10/26 FCH: 10 larvae preserved as 3rd instar')
    await m.click(m.button('4th instar larva'))
    expect(m.host.textContent).toContain('10 larvae preserved as 4th instar')
    await m.click(m.button('Set'))
    expect(m.value.value).toBe('=20')
    expect(m.events.map(e => [e.kind, e.count, e.lifestage])).toEqual([['preserved', 10, '4th instar larva']])
  })
  it('+3 hatched yesterday: the event and its note carry the day', async () => {
    const m = await mount('=12', { stage: 'larva' })
    await m.click(m.button('Yesterday'))
    await m.type('3')
    await m.click(m.button('+3'))
    expect(m.value.value).toBe('=12+3')
    expect(m.events.map(e => [e.kind, e.count, e.day])).toEqual([['hatched', 3, TODAY - 1]])
    expect(m.host.textContent).toContain('3 larvae hatched on 4/10/26')
    // The next one is today again.
    await m.type('1')
    await m.click(m.button('+1'))
    expect(m.events.at(-1)?.day).toBe(TODAY)
  })
  it('registered in Insectary_data: the IDs the cards took come back with the event', async () => {
    const m = await mount('=20', { canRegister: true })
    await m.type('2')
    await m.click(m.button('−2'))
    await m.click(m.button('Preserved'))
    await m.click(m.button('Register 2 in Insectary_data'))
    expect(m.registers.map(r => [r.count, r.lifestage, r.day])).toEqual([[2, '3rd instar larva', TODAY]])
    m.registers[0].done(['R0C', 'R1C'])
    await nextTick()
    expect(m.events.map(e => [e.kind, e.count, e.ids])).toEqual([['preserved', 2, ['R0C', 'R1C']]])
    expect(m.host.textContent).toContain('2 larvae preserved as 3rd instar (R0C, R1C)')
  })
})
