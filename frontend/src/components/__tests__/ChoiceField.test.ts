import { afterEach, describe, expect, it } from 'vitest'
import { createApp, h, nextTick, ref } from 'vue'
import ChoiceField from '../ChoiceField.vue'
import type { ChoiceOptions } from '../../lib/choices'

const species = ['Oleria onega', 'Mechanitis messenoides', 'Mechanitis polymnia', 'Ithomia salapia']
const fates = [
  { value: 'insectario', label: 'Collected_Sent2Insectary' },
  { value: 'preservada', label: 'Collected_Preserved' },
]

let unmount = () => {}
afterEach(() => {
  unmount()
  document.body.innerHTML = ''
})

/** Mounts a ChoiceField inside a form; returns its text box, the v-model and what the form saw. */
async function mount(options: ChoiceOptions, start = '', props: Record<string, unknown> = {}) {
  const value = ref(start)
  const submitted = ref(0)
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp({
    render: () =>
      h('form', { onSubmit: (e: Event) => (e.preventDefault(), submitted.value++) }, [
        h(ChoiceField, {
          options,
          modelValue: value.value,
          'onUpdate:modelValue': (v: string) => (value.value = v),
          class: 'field-input',
          ...props,
        }),
      ]),
  })
  app.mount(host)
  unmount = () => app.unmount()
  await nextTick()
  const input = host.querySelector('input')!
  return { input, value, submitted }
}
const items = () => [...document.querySelectorAll<HTMLElement>('[role=option]')]
const picked = () => document.querySelector('[role=option].is-pick')?.textContent?.trim()
async function type(input: HTMLInputElement, text: string) {
  input.dispatchEvent(new Event('focus'))
  input.value = text
  input.dispatchEvent(new Event('input'))
  await nextTick()
  await nextTick()
}
async function key(input: HTMLInputElement, name: string) {
  const event = new KeyboardEvent('keydown', { key: name, bubbles: true, cancelable: true })
  input.dispatchEvent(event)
  await nextTick()
  return event
}

describe('ChoiceField', () => {
  it('filters as you type, prefix matches first, and marks the first', async () => {
    const { input } = await mount(species)
    await type(input, 'm')
    expect(items().map(el => el.textContent?.trim())).toEqual([
      'Mechanitis messenoides',
      'Mechanitis polymnia',
      'Ithomia salapia',
    ])
    expect(picked()).toBe('Mechanitis messenoides')
    expect(input.getAttribute('aria-expanded')).toBe('true')
    expect(input.getAttribute('aria-activedescendant')).toBe(items()[0].id)
  })
  it('Enter takes the marked suggestion without submitting the form', async () => {
    const { input, value } = await mount(species)
    await type(input, 'mess')
    const enter = await key(input, 'Enter')
    expect(enter.defaultPrevented).toBe(true)
    expect(value.value).toBe('Mechanitis messenoides')
    expect(items()).toHaveLength(0)
  })
  it('↓ moves the mark and Tab takes it', async () => {
    const { input, value } = await mount(species)
    await type(input, 'mech')
    await key(input, 'ArrowDown')
    expect(picked()).toBe('Mechanitis polymnia')
    const tab = await key(input, 'Tab')
    expect(tab.defaultPrevented).toBe(false)
    expect(value.value).toBe('Mechanitis polymnia')
  })
  it('free text keeps a new value', async () => {
    const { input, value } = await mount(species)
    await type(input, 'Greta andromica')
    await key(input, 'Enter')
    expect(value.value).toBe('Greta andromica')
  })
  it('Escape closes and restores the value', async () => {
    const { input, value } = await mount(species, 'Oleria onega')
    await type(input, 'mech')
    await key(input, 'Escape')
    expect(items()).toHaveLength(0)
    expect(input.value).toBe('Oleria onega')
    expect(value.value).toBe('Oleria onega')
  })
  it('select-like lists show labels, store values and revert what is not an option', async () => {
    const { input, value } = await mount(fates, 'insectario', { freetext: false })
    expect(input.value).toBe('Collected_Sent2Insectary')
    await type(input, 'pres')
    await key(input, 'Enter')
    expect(value.value).toBe('preservada')
    expect(input.value).toBe('Collected_Preserved')
    await type(input, 'nada')
    await key(input, 'Enter')
    expect(value.value).toBe('preservada')
    await nextTick()
    expect(input.value).toBe('Collected_Preserved')
  })
  it('a click on an option takes it', async () => {
    const { input, value } = await mount(fates, 'insectario', { freetext: false, allowEmpty: true })
    input.dispatchEvent(new Event('focus'))
    await nextTick()
    await nextTick()
    expect(items().map(el => el.textContent?.trim())).toEqual(['—', 'Collected_Sent2Insectary', 'Collected_Preserved'])
    expect(picked()).toBe('Collected_Sent2Insectary')
    items()[0].click()
    await nextTick()
    expect(value.value).toBe('')
  })
})
