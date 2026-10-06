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
async function mount(start: CellValue, { stage = 'larva' as Stage, subtractPreserved = false, canRegister = false, startOfDay = undefined as CellValue | undefined } = {}) {
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
        startOfDay,
        dirty: value.value !== start,
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
  it('− without a number: the cause, then how many (1 to start, + and − to change it)', async () => {
    const m = await mount('=20')
    await m.click(m.button('−'))
    expect(m.host.textContent).toContain('What happened?')
    await m.click(m.button('Disappeared'))
    const box = m.host.querySelector<HTMLInputElement>('input[aria-label="How many"]')!
    expect(box.value).toBe('1')
    await m.click(m.host.querySelector<HTMLButtonElement>('button[aria-label="One more"]')!)
    await m.click(m.button('Subtract 2'))
    expect(m.value.value).toBe('=20-2')
    expect(m.events.map(e => [e.kind, e.count])).toEqual([['disappeared', 2]])
  })
  it('a chip tapped is struck out of the sum, and back in when tapped again', async () => {
    const m = await mount('=2+3+9+1+8+23')
    const chip = () => [...m.host.querySelectorAll<HTMLButtonElement>('button[aria-pressed]')].find(b => b.textContent?.trim() === '+23')!
    await m.click(chip())
    expect(m.value.value).toBe('=2+3+9+1+8')
    await nextTick()
    expect(chip().getAttribute('aria-pressed')).toBe('true')
    expect(m.host.textContent).toContain('Struck out = not in the sum')
    await m.click(chip())
    expect(m.value.value).toBe('=2+3+9+1+8+23')
    // No separate "remove the last term" button.
    expect(m.host.textContent).not.toMatch(/Remove \+23/)
  })
  it("this morning against now, today's chips apart, and one tap takes today's changes back", async () => {
    const m = await mount('=11-1+4+5', { startOfDay: '=11-1+4' })
    expect(m.host.textContent).toMatch(/This morning\s*14\s*→\s*now\s*19/)
    expect(m.host.textContent).toMatch(/today\s*\+5/)
    await m.click(m.button("Undo today's changes"))
    expect(m.value.value).toBe('=11-1+4')
  })
  it('−5 died: the cause first, then how many; the count takes them off, the event is today, and the note for NOTES is shown', async () => {
    const m = await mount('=20')
    await m.type('5')
    await m.click(m.button('−5'))
    // Nothing taken off before the cause is said.
    expect(m.value.value).toBe('=20')
    await m.click(m.button('Died'))
    expect(m.host.textContent).toContain('5/10/26 FCH: 5 larvae died')
    await m.click(m.button('Subtract 5'))
    expect(m.value.value).toBe('=20-5')
    expect(m.events.map(e => [e.kind, e.count, e.day])).toEqual([['died', 5, TODAY]])
    expect(m.host.textContent).toContain('Added to NOTES (saved with the clutch): 5/10/26 FCH: 5 larvae died')
  })
  it('preserved larvae stay counted by default, at the 3rd instar unless another is chosen; the note is shown while choosing', async () => {
    const m = await mount('=20')
    await m.type('10')
    await m.click(m.button('−10'))
    await m.click(m.button('Preserved'))
    expect(m.host.querySelector<HTMLInputElement>('input[aria-label="How many"]')!.value).toBe('10')
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
