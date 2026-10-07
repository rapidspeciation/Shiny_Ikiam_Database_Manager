import { afterEach, describe, expect, it } from 'vitest'
import { createApp, h, nextTick, ref } from 'vue'
import StagePanel, { type StageAct, type TermRef } from '../clutches/StagePanel.vue'
import type { ClutchEvent } from '../../lib/clutches'
import type { GroupRow } from '../../lib/clutchGroups'
import { locale, t, tn } from '../../lib/i18n'
import type { CellValue } from '../../lib/types'

// A stage of a clutch: the sum's terms as chips in their groups, the total typed over, what happened.

let unmount = () => {}
afterEach(() => {
  unmount()
  document.body.innerHTML = ''
})

const group = (id: string, position: number, label: string): GroupRow => ({ id, field: 'NUMBER OF LARVAE', stage: 'larva', position, label, originId: null })
const event = (id: string, term: number, groupId: string | null, kind: ClutchEvent['kind'] = term > 0 ? 'hatched' : 'died'): ClutchEvent => ({
  id,
  recordId: 'r',
  clutch: '1',
  day: '2026-10-05',
  stage: 'larva',
  kind,
  count: Math.abs(term),
  ids: [],
  note: null,
  field: 'NUMBER OF LARVAE',
  term,
  groupId,
  actor: 'u',
  username: null,
  name: null,
  actionId: null,
  createdAt: `2026-10-05T1${Math.abs(term) % 10}:00:00Z`,
})

async function mount(value: CellValue, { meta = [] as (GroupRow | null)[], events = [] as ClutchEvent[] } = {}) {
  const acts: StageAct[] = []
  const opened: TermRef[] = []
  const said: string[] = []
  const activeTerm = ref<{ group: number; index: number } | null>(null)
  const inlineOpen = ref(false)
  const selected = ref<number[]>([])
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp({
    render: () =>
      h(StagePanel, {
        field: 'NUMBER OF LARVAE',
        stage: 'larva',
        value,
        saved: value,
        dirty: false,
        editable: true,
        locked: false,
        more: 'hatched',
        meta,
        events,
        selected: selected.value,
        'onUpdate:selected': (s: number[]) => (selected.value = s),
        activeTerm: activeTerm.value,
        inlineOpen: inlineOpen.value,
        onAct: (a: StageAct) => acts.push(a),
        onOpen: (r: TermRef) => opened.push(r),
        onPanelOpened: () => said.push('panelOpened'),
        onEmptyTap: () => said.push('emptyTap'),
      }),
  })
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  locale.value = 'en'
  app.mount(host)
  unmount = () => app.unmount()
  await nextTick()
  const buttons = () => [...host.querySelectorAll('button')]
  const button = (text: string | RegExp) => buttons().find(b => (typeof text === 'string' ? b.textContent?.trim().startsWith(text) : text.test(b.textContent ?? '')))!
  const click = async (b: HTMLElement) => {
    b.click()
    await nextTick()
  }
  const type = async (input: HTMLInputElement, text: string) => {
    input.value = text
    input.dispatchEvent(new Event('input'))
    await nextTick()
  }
  return { host, acts, opened, said, activeTerm, inlineOpen, selected, button, click, type }
}

describe('a stage of a clutch', () => {
  it('the total is the only one: tapped, the count typed (a space adds) becomes a correction', async () => {
    const m = await mount('=27-2-11-3')
    await m.click(m.host.querySelector<HTMLButtonElement>('button[aria-label^="Type the total"]')!)
    await m.type(m.host.querySelector<HTMLInputElement>('input[aria-label^="New total"]')!, '6 4')
    expect(m.host.textContent).toContain('11 → 10 · −1 correction')
    await m.click(m.button('Set'))
    expect(m.acts).toEqual([{ type: 'correction', total: 10, groupIndex: null, reason: null }])
    // No separate box for the total, and no "tap a number to take it out".
    expect(m.host.textContent).not.toMatch(/Tap a number/)
  })
  it('each term a chip with its event; tapped, it opens the event', async () => {
    const m = await mount('=(6-2)+(5)', { meta: [group('A', 0, 'A'), group('B', 1, 'box B')], events: [event('h6', 6, 'A'), event('d2', -2, 'A'), event('h5', 5, 'B')] })
    expect(m.host.textContent).toMatch(/4 · A/)
    expect(m.host.textContent).toMatch(/5 · box B/)
    await m.click(m.button('−2 died'))
    expect(m.opened.map(o => [o.group, o.index, o.term, o.event?.id])).toEqual([[0, 1, -2, 'd2']])
  })
  it('groups are selected by a tap; − then goes to the one selected, the cause first', async () => {
    const m = await mount('=(6)+(5)', { meta: [group('A', 0, 'A'), group('B', 1, 'B')] })
    await m.click(m.host.querySelectorAll<HTMLButtonElement>('button[title="Tap to select the group"]')[1])
    expect(m.selected.value).toEqual([1])
    await nextTick()
    await m.click(m.button(/what happened/))
    await m.click(m.button('Died'))
    await m.type(m.host.querySelector<HTMLInputElement>('input[aria-label="How many"]')!, '2')
    await m.click(m.button('Subtract 2'))
    expect(m.acts).toEqual([{ type: 'loss', kind: 'died', count: 2, groupIndex: 1, ids: [] }])
  })
  it('+ hatched: a space adds; the day can be unknown (already big)', async () => {
    const m = await mount('=10')
    await m.click(m.button('hatched'))
    await m.type(m.host.querySelector<HTMLInputElement>('input[aria-label="How many"]')!, '3 2')
    await m.click(m.button('Unknown (already big)'))
    await m.click(m.button('+5'))
    expect(m.acts).toEqual([expect.objectContaining({ type: 'gain', count: 5, dayKnown: false, groupIndex: null, fromIndex: null })])
  })
  it('regroup: the counts must add up to the total', async () => {
    const m = await mount('=11')
    await m.click(m.button('Regroup'))
    const box = m.host.querySelector<HTMLInputElement>('input[id^="regroup-"]')!
    await m.type(box, '6 4')
    expect(m.host.textContent).toContain('= 10 / 11')
    await m.type(box, '6 5')
    await m.click([...m.host.querySelectorAll<HTMLButtonElement>('button.btn-primary')].find(b => b.textContent?.includes('Regroup'))!)
    expect(m.acts).toEqual([{ type: 'regroup', targets: [6, 5], labels: ['A', 'B'] }])
  })
  it('a chip tapped is shown selected (its sheet is the editor’s, under the chips); a tap outside the chips closes it', async () => {
    const m = await mount('=27-2', { events: [] })
    await m.click(m.button('+27'))
    expect(m.opened.map(o => [o.group, o.index, o.term, o.event])).toEqual([[0, 0, 27, null]])
    m.activeTerm.value = { group: 0, index: 0 }
    await nextTick()
    expect(m.button('+27').getAttribute('aria-pressed')).toBe('true')
    expect(m.button('−2').getAttribute('aria-pressed')).toBe('false')
    await m.click(m.host.querySelector<HTMLElement>('[aria-label="History of the sum"]')!)
    expect(m.said).toEqual(['emptyTap'])
  })
  it('one area at a time, inline with its title: opening one tells the editor; the editor opening its own closes it; Esc closes it', async () => {
    const m = await mount('=11')
    await m.click(m.button('Regroup'))
    expect(m.said).toEqual(['panelOpened'])
    expect(m.host.querySelector('[role="group"][aria-label="Regroup"] h3')?.textContent).toBe('Regroup')
    expect(document.querySelector('.fixed')).toBeNull()
    m.inlineOpen.value = true
    await nextTick()
    await nextTick()
    expect(m.host.querySelector('input[id^="regroup-"]')).toBeNull()
    m.inlineOpen.value = false
    await nextTick()
    await m.click(m.host.querySelector<HTMLButtonElement>('button[aria-label^="Type the total"]')!)
    expect(m.host.textContent).toContain("Today's count")
    window.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape' }))
    await nextTick()
    expect(m.host.querySelector('input[aria-label^="New total"]')).toBeNull()
  })
})
