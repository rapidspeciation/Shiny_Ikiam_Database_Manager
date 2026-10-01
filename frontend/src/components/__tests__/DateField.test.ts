import { afterEach, describe, expect, it } from 'vitest'
import { createApp, h, nextTick, ref } from 'vue'
import DateField from '../DateField.vue'
import { calendarDays, shiftMonth } from '../../lib/dates'
import { locale, t, tn } from '../../lib/i18n'

let unmount = () => {}
afterEach(() => {
  unmount()
  document.body.innerHTML = ''
})

async function mount(start = '2026-09-28') {
  const value = ref(start)
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp({
    render: () =>
      h('label', [h(DateField, { modelValue: value.value, 'onUpdate:modelValue': (v: string) => (value.value = v), class: 'field-input' })]),
  })
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  locale.value = 'en'
  app.mount(host)
  unmount = () => app.unmount()
  await nextTick()
  const button = host.querySelector('button')!
  const calendar = () => document.body.querySelector<HTMLElement>('[role=dialog]')
  const open = async () => {
    button.click()
    await nextTick()
    expect(calendar()).not.toBeNull()
  }
  return { value, button, calendar, open, host }
}

describe('the calendar of a date box', () => {
  it('months Monday first, six weeks, the days around greyed', () => {
    const october = calendarDays(2026, 10)
    expect(october).toHaveLength(42)
    // 1 Oct 2026 is a Thursday: Monday 28 Sep starts the grid.
    expect(october[0]).toEqual({ iso: '2026-09-28', day: 28, inMonth: false })
    expect(october[3]).toEqual({ iso: '2026-10-01', day: 1, inMonth: true })
    expect(october.filter(d => d.inMonth)).toHaveLength(31)
    expect(shiftMonth(2026, 1, -1)).toEqual({ year: 2025, month: 12 })
    expect(shiftMonth(2026, 12, 1)).toEqual({ year: 2027, month: 1 })
  })
  it('opens on the date chosen; choosing a day sets it and closes', async () => {
    const { value, calendar, open } = await mount()
    await open()
    expect(calendar()!.querySelector('[data-chosen=true]')!.getAttribute('data-iso')).toBe('2026-09-28')
    calendar()!.querySelector<HTMLButtonElement>('[data-iso="2026-09-15"]')!.click()
    await nextTick()
    expect(value.value).toBe('2026-09-15')
    expect(calendar()).toBeNull()
  })
  it('closes with Escape, ✕, Cancel, a tap outside, and its button again, changing nothing', async () => {
    const { value, button, calendar, open, host } = await mount()
    await open()
    document.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }))
    await nextTick()
    expect(calendar()).toBeNull()

    await open()
    calendar()!.querySelector<HTMLButtonElement>('button[aria-label="Close"]')!.click()
    await nextTick()
    expect(calendar()).toBeNull()

    await open()
    const cancel = [...calendar()!.querySelectorAll('button')].find(b => /Cancel/.test(b.textContent || ''))!
    cancel.click()
    await nextTick()
    expect(calendar()).toBeNull()

    await open()
    host.querySelector('input')!.dispatchEvent(new Event('pointerdown', { bubbles: true }))
    await nextTick()
    expect(calendar()).toBeNull()

    await open()
    // Its own button toggles it (the pointerdown on it is not "outside").
    button.dispatchEvent(new Event('pointerdown', { bubbles: true }))
    button.click()
    await nextTick()
    expect(calendar()).toBeNull()
    expect(value.value).toBe('2026-09-28')
  })
  it('months go back and forward; what is typed still works', async () => {
    const { value, calendar, open, host } = await mount('')
    const input = host.querySelector('input')!
    input.value = '3/2/2026'
    input.dispatchEvent(new Event('input'))
    input.dispatchEvent(new Event('change'))
    await nextTick()
    expect(value.value).toBe('2026-02-03')
    await open()
    const prev = calendar()!.querySelector<HTMLButtonElement>('button')!
    prev.click()
    await nextTick()
    expect(calendar()!.querySelector('[data-iso="2026-01-15"]')).not.toBeNull()
  })
})
